(defpackage :genodb
  (:use :common-lisp)
  (:import-from :alexandria :iota :once-only :with-gensyms)
  (:import-from :ironclad :with-octet-input-stream :with-octet-output-stream)
  (:import-from :listopia :split-at)
  (:import-from :str
   :concat :contains? :join :s-rest :split :starts-with?
   :trim-right :words)
  (:import-from :trivia :lambda-match :match)
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
  db nrows ncols)

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
               (lmdb:string-to-octets (concat ":" key))))

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
             (write-bytevector-with-length (lmdb:string-to-octets key)
                                           stream)
             (write-bytevector-with-length
              (etypecase value
                (string (lmdb:string-to-octets value))
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
  (lmdb:g3t db "current"))

(defun (setf genotype-db-current-matrix) (hash db)
  "Set HASH as the current matrix in genotype matrix DB."
  (lmdb:put db "current" hash))

(defun genotype-db-matrix (db)
  "Return the current matrix from genotype matrix DB."
  (let ((hash (genotype-db-current-matrix db)))
    (make-genotype-db-matrix
     :db db
     :nrows (lmdb:octets-to-uint64
             (genotype-db-metadata-get db hash "nrows"))
     :ncols (lmdb:octets-to-uint64
             (genotype-db-metadata-get db hash "ncols")))))

(defun (setf genotype-db-matrix) (matrix db)
  "Set genotype MATRIX as the current matrix in genotype matrix DB."
  (let ((matrix (genotype-matrix-matrix matrix)))
    (match (array-dimensions matrix)
      ((list nrows ncols)
       (setf (genotype-db-current-matrix db)
             (genotype-db-put
              db
              (with-octet-output-stream (stream)
                (dotimes (i nrows)
                  (write-sequence
                   (genotype-db-put
                    db
                    (map '(vector (unsigned-byte 8))
                         (lambda (genotype)
                           (case genotype
                             ((maternal) 0)
                             ((paternal) 1)
                             ((heterozygous) 2)
                             ((unknown) 3)
                             (t (error 'unknown-genotype-matrix-data))))
                         (matrix-row matrix i)))
                   stream)))
              `(("nrows" . ,nrows)
                ("ncols" . ,ncols))))))))

(defun genotype-db-matrix-row-ref (matrix i)
  "Return the Ith row of genotype db MATRIX."
  (let ((db (genotype-db-matrix-db matrix)))
    (map 'vector
         (lambda (integer)
           (case integer
             ((0) 'maternal)
             ((1) 'paternal)
             ((2) 'heterozygous)
             ((3) 'unknown)
             (t (error 'unknown-genotype-matrix-data))))
         (genotype-db-get
          db
          (let ((hash-length (ironclad:digest-length *blob-hash-digest*)))
            (make-array hash-length
                        :element-type '(unsigned-byte 8)
                        :displaced-to (genotype-db-get db (genotype-db-current-matrix db))
                        :displaced-index-offset (* i hash-length)))))))

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

(defun print-genotype-db-info (database-directory)
  (with-genotype-db (db database-directory)
    (let ((matrix (genotype-db-matrix db)))
      (format t
              "Path: ~a~%Dimensions: ~a × ~a~%"
              database-directory
              (genotype-db-matrix-nrows matrix)
              (genotype-db-matrix-ncols matrix)))))

(defun main ()
  (match (uiop:command-line-arguments)
    ((list "import" geno-file genotype-database)
     (with-genotype-db (db
                        (fad:pathname-as-directory genotype-database)
                        :write t)
       (setf (genotype-db-matrix db)
             (read-geno-file geno-file))))
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
