(defpackage :genodb
  (:use :common-lisp)
  (:import-from :alexandria :iota :once-only :with-gensyms)
  (:import-from :ironclad :with-octet-input-stream :with-octet-output-stream)
  (:import-from :listopia :all :any :split-at)
  (:import-from :str
   :concat :contains? :join :s-rest :split :starts-with?
   :trim-right :words)
  (:import-from :trivia :lambda-match :match)
  (:import-from :trivial-utf-8 :string-to-utf-8-bytes)
  (:export :main))

(in-package :genodb)

;;;
;;; Utilities
;;;

(defun matrix-row (matrix n)
  "Return the Nth row of MATRIX."
  (let ((ncols (array-dimension matrix 1)))
    (make-array ncols
                :displaced-to matrix
                :displaced-index-offset (* n ncols))))

(defun matrix-column (matrix n)
  "Return the Nth column of MATRIX."
  (let ((column (make-array (array-dimension matrix 0))))
    (dotimes (i (length column))
      (setf (aref column i)
            (aref matrix i n)))
    column))

(defmacro save-excursion (stream &body body)
  "Evaluate BODY, and restore STREAM to the position it was in before
evaluation of BODY."
  (with-gensyms (position)
    (once-only (stream)
      `(let* ((,position (file-position ,stream)))
         (unwind-protect
              (progn ,@body)
           (file-position ,stream ,position))))))

(defun unget-line (line stream)
  "Unget LINE to STREAM."
  (file-position stream (- (file-position stream)
                           (1+ (length line)))))

(defun for-each-indexed (function list &optional (start 0))
  "Apply FUNCTION successively on every element of LIST. FUNCTION is
invoked as (FUNCTION INDEX ELEMENT) where ELEMENT is an element of
LIST and INDEX is its index. START is the index to use for the first
element."
  (match list
    ((list* head tail)
     (funcall function start head)
     (for-each-indexed function tail (1+ start)))))

(defun assoc-ref (alist key &key (test #'equalp))
  "Return the value associated with KEY in ALIST. KEYS are compared
using TEST."
  (match (assoc key alist :test test)
    ((cons _ value) value)))

(defun count-lines (stream)
  "Return the number of lines in STREAM starting from the current
position."
  (labels ((count-lines-loop (result)
             (if (read-line stream nil)
                 (count-lines-loop (1+ result))
                 result)))
    (save-excursion stream
      (count-lines-loop 0))))

(defun repeat (thunk n)
  "Run THUNK N times and return the result as a list."
  (labels ((repeat-tail (thunk n result)
             (if (zerop n)
                 result
                 (repeat-tail thunk
                              (1- n)
                              (cons (funcall thunk)
                                    result)))))
    (reverse (repeat-tail thunk n (list)))))

(defun repeat-indexed (function n)
  "Run FUNCTION N times and return the result as a list. FUNCTION is
invoked as (FUNCTION INDEX) for INDEX = 0, 1, 2, ..., n-1."
  (labels ((repeat-tail (function i n result)
             (if (= i n)
                 result
                 (repeat-tail function (1+ i) n (cons (funcall function i)
                                                      result)))))
    (reverse (repeat-tail function 0 n (list)))))

;;;
;;; Genotype database and matrix
;;;

(defstruct genotype-matrix
  matrix metadata)

(defstruct genotype-db-matrix
  db hash nrows ncols)

(defmacro with-genotype-db ((db database-directory &key write) &body body)
  (with-gensyms (env)
    (once-only (database-directory write)
      `(lmdb:with-env (,env ,database-directory
                            :if-does-not-exist :create
                            :map-size (* 100 1024 1024))
         (let ((,db (lmdb:get-db nil :env ,env)))
           (lmdb:with-txn (:env ,env :write ,write)
             ,@body))))))

(defun metadata-key (hash key)
  "Return the database key to retrieve metadata KEY associated with
blob of HASH."
  (concatenate '(vector (unsigned-byte 8))
               hash
               (string-to-utf-8-bytes (concat ":" key))))

(defvar *blob-hash-digest*
  :sha256)

(defun write-bytevector-with-length (bv stream)
  "Write length of BV followed by BV itself to STREAM. The length is
written as a little endian 64-bit unsigned integer."
  (write-sequence (lmdb:uint64-to-octets (length bv)) stream)
  (write-sequence bv stream))

(defun bv-hash (bv &optional metadata)
  "Return hash of BV + METADATA. METADATA is an association list mapping
string keys to string, uint64 or bytevector values."
  (ironclad:with-digesting-stream (stream *blob-hash-digest*)
    ;; Write bytevector.
    (write-bytevector-with-length bv stream)
    ;; Write metadata.
    (mapc (lambda-match
            ((cons key value)
             (write-bytevector-with-length (string-to-utf-8-bytes key)
                                           stream)
             (write-bytevector-with-length
              (etypecase value
                (string (string-to-utf-8-bytes value))
                ((unsigned-byte 64) (lmdb:uint64-to-octets value))
                ((vector (unsigned-byte 8)) value))
              stream)))
          metadata)))

(defun genotype-db-get (db hash)
  "Get bytevector with HASH from genotype DB."
  (lmdb:g3t db hash))

(defun genotype-db-put (db bv &optional metadata)
  "Put BV, a bytevector, into DB. Associate METADATA, an association
list of metadata, with BV. Return the hash."
  (let ((hash (bv-hash bv metadata)))
    ;; Put bytevector and metadata into db. Do nothing if it is
    ;; already in db.
    (unless (genotype-db-get db hash)
      (lmdb:put db hash bv)
      (mapc (lambda-match
              ((cons key value)
               (lmdb:put db (metadata-key hash key) value)))
            metadata))
    hash))

(defun genotype-db-metadata-get (db hash key)
  "Get metadata associated with KEY, HASH from genotype DB."
  (lmdb:g3t db (metadata-key hash key)))

(defun genotype-db-current-matrix (db)
  "Return the hash of the current matrix in genotype matrix DB."
  (let ((hash-length (ironclad:digest-length *blob-hash-digest*)))
    (make-array hash-length
                :element-type '(unsigned-byte 8)
                :displaced-to (lmdb:g3t db (string-to-utf-8-bytes "versions")))))

(defun (setf genotype-db-current-matrix) (hash db)
  "Set HASH as the current matrix in genotype matrix DB."
  (let ((versions (string-to-utf-8-bytes "versions")))
    (lmdb:put db versions
              (concatenate '(vector (unsigned-byte 8))
                           hash
                           (lmdb:g3t db versions)))))

(defun genotype-db-all-matrices (db)
  "Return a list of all matrices in DB, newest first."
  (let ((hash-length (ironclad:digest-length *blob-hash-digest*))
        (all-matrix-hashes (lmdb:g3t db (string-to-utf-8-bytes "versions"))))
    (mapcar (lambda (i)
              (genotype-db-matrix db
                                  (make-array hash-length
                                              :element-type '(unsigned-byte 8)
                                              :displaced-to all-matrix-hashes
                                              :displaced-index-offset (* i hash-length))))
            (iota (/ (length all-matrix-hashes)
                     hash-length)))))

(defun genotype-db-matrix (db hash)
  "Return the matrix identified by HASH from genotype matrix DB."
  (make-genotype-db-matrix
   :db db
   :hash hash
   :nrows (lmdb:octets-to-uint64
           (genotype-db-metadata-get db hash "nrows"))
   :ncols (lmdb:octets-to-uint64
           (genotype-db-metadata-get db hash "ncols"))))

(defun encode-genotype-vector (vector)
  "Encode genotype VECTOR to a bytevector."
  (map '(vector (unsigned-byte 8))
       (lambda (genotype)
         (case genotype
           ((maternal) 0)
           ((paternal) 1)
           ((heterozygous) 2)
           ((unknown) 3)
           (t (error 'unknown-genotype-matrix-data))))
       vector))

(defun genotype-db-matrix-put (db matrix)
  "Put genotype MATRIX into DB and return the hash."
  (let ((matrix (genotype-matrix-matrix matrix)))
    (match (array-dimensions matrix)
      ((list nrows ncols)
       (genotype-db-put
        db
        (with-octet-output-stream (stream)
          (dotimes (i nrows)
            (write-sequence
             (genotype-db-put
              db (encode-genotype-vector (matrix-row matrix i)))
             stream))
          (dotimes (j ncols)
            (write-sequence
             (genotype-db-put
              db (encode-genotype-vector (matrix-column matrix j)))
             stream)))
        `(("nrows" . ,nrows)
          ("ncols" . ,ncols)))))))

(defun decode-genotype-vector (bv)
  "Decode BV to genotype vector."
  (map 'vector
       (lambda (integer)
         (case integer
           ((0) 'maternal)
           ((1) 'paternal)
           ((2) 'heterozygous)
           ((3) 'unknown)
           (t (error 'unknown-genotype-matrix-data))))
       bv))

(defun genotype-db-matrix-row-ref (matrix i)
  "Return the Ith row of genotype db MATRIX."
  (let ((db (genotype-db-matrix-db matrix)))
    (decode-genotype-vector
     (genotype-db-get
      db
      (let ((hash-length (ironclad:digest-length *blob-hash-digest*)))
        (make-array hash-length
                    :element-type '(unsigned-byte 8)
                    :displaced-to (genotype-db-get db (genotype-db-matrix-hash matrix))
                    :displaced-index-offset (* i hash-length)))))))

(defun genotype-db-matrix-column-ref (matrix j)
  "Return the Jth column of genotype db MATRIX."
  (let ((db (genotype-db-matrix-db matrix)))
    (decode-genotype-vector
     (genotype-db-get
      db
      (let ((hash-length (ironclad:digest-length *blob-hash-digest*)))
        (make-array hash-length
                    :element-type '(unsigned-byte 8)
                    :displaced-to (genotype-db-get db (genotype-db-matrix-hash matrix))
                    :displaced-index-offset (* (+ (genotype-db-matrix-nrows matrix) j)
                                               hash-length)))))))

;;;
;;; Geno files
;;;

(defun read-geno-file (file)
  "Read geno FILE and return a genotype-matrix object."
  (with-open-file (stream file)
    (let ((file-metadata
            ;; Read file metadata.
            (labels ((read-geno-metadata ()
                       (let ((line (read-line stream)))
                         (cond
                           ;; Comment line
                           ((starts-with? "#" line)
                            (read-geno-metadata))
                           ;; Metadata line
                           ((starts-with? "@" line)
                            (match (split ":" (trim-right (s-rest line)))
                              ((list key value)
                               (acons key value
                                      (read-geno-metadata)))))
                           ;; Some other line
                           (t (unget-line line stream)
                              (list))))))
              (read-geno-metadata))))
      ;; Extract metadata column names from table header.
      (multiple-value-bind (metadata-columns individuals)
          (match (words (read-line stream))
            ((list* "Chr" "Locus" "cM" "Mb" individuals)
             (values (list "Chr" "Locus" "cM" "Mb")
                     individuals))
            ((list* "Chr" "Locus" "cM" individuals)
             (values (list "Chr" "Locus" "cM" "Mb")
                     individuals)))
        ;; Read data.
        (let* ((nrows (count-lines stream))
               (ncols (length individuals))
               (matrix (make-array (list nrows ncols)))
               (maternal (assoc-ref file-metadata "mat"))
               (paternal (assoc-ref file-metadata "pat"))
               (heterozygous (assoc-ref file-metadata "het"))
               (unknown (assoc-ref file-metadata "unk")))
          (make-genotype-matrix
           :matrix matrix
           :metadata
           ;; Write matrix data by mutation. Return metadata.
           (repeat-indexed (lambda (i)
                             (match (split-at (length metadata-columns)
                                              (words (read-line stream)))
                               ((list metadata data)
                                (for-each-indexed (lambda (j element)
                                                    (setf (aref matrix i j)
                                                          (cond
                                                            ((string= element maternal)
                                                             'maternal)
                                                            ((string= element paternal)
                                                             'paternal)
                                                            ((string= element heterozygous)
                                                             'heterozygous)
                                                            ((string= element unknown)
                                                             'unknown))))
                                                  data)
                                (mapcar #'cons metadata-columns metadata))))
                           nrows)))))))

(defun live-hashes (db)
  "Return all live hashes in DB."
  (let ((current-matrix-hash (genotype-db-current-matrix db)))
    (and current-matrix-hash
         (let* ((hash-length (ironclad:digest-length *blob-hash-digest*))
                (current-matrix (genotype-db-matrix db current-matrix-hash))
                (hashes (genotype-db-get db current-matrix-hash)))
           (cons (genotype-db-matrix-hash current-matrix)
                 ;; Hashes of all rows and columns
                 (mapcar (lambda (i)
                           (make-array hash-length
                                       :element-type '(unsigned-byte 8)
                                       :displaced-to hashes
                                       :displaced-index-offset (* i hash-length)))
                         (iota (+ (genotype-db-matrix-nrows current-matrix)
                                  (genotype-db-matrix-ncols current-matrix)))))))))

(defun collect-garbage (db)
  "Delete all keys in DB that are not associated with a live hash."
  (let ((live-hashes (live-hashes db)))
    (lmdb:with-cursor (cursor db)
      (lmdb:cursor-first cursor)
      (lmdb:do-cursor (key value cursor)
        (unless (or (equalp key (string-to-utf-8-bytes "versions"))
                    (any (lambda (hash)
                           (equalp hash
                                   (make-array (length hash)
                                               :element-type '(unsigned-byte 8)
                                               :displaced-to key)))
                         live-hashes))
          (lmdb:cursor-del cursor))))))

(defun import-into-genotype-db (geno-file genotype-database)
  "Import GENO-FILE into GENOTYPE-DATABASE."
  (let ((matrix (read-geno-file geno-file)))
    ;; Write genotype matrix into genotype database.
    (with-genotype-db (db genotype-database :write t)
      (let* ((hash (genotype-db-matrix-put db matrix))
             (db-matrix (genotype-db-matrix db hash)))
        ;; Read written data back and verify.
        (unless (and (all (lambda (i)
                            (equalp (matrix-row (genotype-matrix-matrix matrix) i)
                                    (genotype-db-matrix-row-ref db-matrix i)))
                          (iota (genotype-db-matrix-nrows db-matrix)))
                     (all (lambda (i)
                            (equalp (matrix-column (genotype-matrix-matrix matrix) i)
                                    (genotype-db-matrix-column-ref db-matrix i)))
                          (iota (genotype-db-matrix-ncols db-matrix))))
          ;; Roll back database updates.
          (collect-garbage db)
          ;; Exit with error message.
          (format *error-output*
                  "Rereading and verifying genotype matrix written to \"~a\" failed.
This is a bug. Please report it.
"
                  genotype-database)
          (uiop:quit 1))
        ;; Set the current matrix.
        (setf (genotype-db-current-matrix db)
              hash)))))

(defun print-genotype-db-info (database-directory)
  (with-genotype-db (db database-directory)
    (format t
            "Path: ~a~%Versions: ~a~%Keys: ~a~%~%"
            database-directory
            (length (genotype-db-all-matrices db))
            (getf (lmdb:db-statistics db)
                  :entries))
    (for-each-indexed (lambda (i matrix)
                        (format t "Version ~a
  Dimensions: ~a × ~a~%"
                                (1+ i)
                                (genotype-db-matrix-nrows matrix)
                                (genotype-db-matrix-ncols matrix)))
                      (genotype-db-all-matrices db))))

(defun main ()
  (match (uiop:command-line-arguments)
    ((list "import" geno-file genotype-database)
     (import-into-genotype-db
      geno-file
      (fad:pathname-as-directory genotype-database)))
    ((list "info" genotype-database)
     (print-genotype-db-info
      (fad:pathname-as-directory genotype-database)))
    (_ (format t "Usage:

Import GENO-FILE into GENOTYPE-DATABASE:
  genodb import GENO-FILE GENOTYPE-DATABASE

Print info about GENOTYPE-DATABASE:
  genodb info GENOTYPE-DATABASE
")
       (uiop:quit 1))))
