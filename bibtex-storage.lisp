(defpackage :bibtex-manager/bibtex-storage
  (:nicknames :bibtex-storage)
  (:use :cl :ol
        :bibtex-runtime
        :mathscinet)
  (:export
   #:associated-entry
   #:associate
   #:save-manager-data
   #:load-manager-data))

(in-package :bibtex-manager/bibtex-storage)

;; the bib files used to store bibtex entries. We read in from all of
;; them, but write out only to the first. We expect paths relative to
;; `library-root'.
(defpar bib-files '("topics.bib"))
(defpar mrnumber-database "mrnumbers.sexp")

(defun library-file (file)
  (merge-pathnames file library:library-root))

;; we store all bib-entries in a hash-table, indexed by the mrnumber
(defvar *mrnumber-bibtex-table*
  (make-hash-table))

;; additionally, we maintain a table of pathnames to bib-entries
(defvar *document-bibtex-table*
  (make-hash-table :test 'equal))

;;; loading bibtex data from the file
(defun map-bib-file (function &rest files)
  "Read in all bib-entries from `files' and map `function' over them."
  (let ((*bib-database* (make-hash-table :test 'equal)))
    (dolist (file files)
      (with-open-file (stream file :if-does-not-exist nil)
        (when stream (read-bib-database stream))))
    (maphash-values function *bib-database*)))

(defun file->bib-entries (&rest files)
  "Read in all bib-entries from `files' and return a list of them."
  (nprog1 entries
    (apply #'map-bib-file (lambda (entry) (push entry entries)) files)))

(defun save-bib-entry (entry)
  "Store `entry' by mrnumber in the global table."
  (when entry
    (check-type entry bibtex-runtime::bib-entry)
    (setf (gethash (bib-entry-mrnumber entry)
                   *mrnumber-bibtex-table*)
          entry)))

(defun load-bib-file (&rest files)
  "Read in all bib-entries from `files' and store them in
`*mrnumber-bibtex-table*'."
  (apply #'map-bib-file #'save-bib-entry files))

(defun bib-entry-by-mr (mrnumber)
  (check-type mrnumber integer)
  (or (gethash mrnumber *mrnumber-bibtex-table*)
      (save-bib-entry (first (search-bibtex-entries :mrnumber mrnumber)))
      (error "No bibtex-entry found for MR=~A" mrnumber)))

(defun save-bib-table (file)
  ;; todo protect against external modifications
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (maphash-values (clambda (write-bib-entry x!entry stream)) *mrnumber-bibtex-table*)))

;;; loading the pathname -> MR map
(defun load-document-mr-table (file)
  (with-open-file (stream file :if-does-not-exist nil)
    (when stream
      ;; fix use safe-read
      (setf *document-bibtex-table* (make-hash-table :test 'equal))
      (dolist (pair (assoc1 :document-mr-table (read stream)))
        (setf (gethash (car pair) *document-bibtex-table*)
              (bib-entry-by-mr (cdr pair)))))))

(defun save-document-mr-table (file)
  ;; todo protect against external modifications
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (le1 (table)
      (maphash (lambda (k v)
                 (push (cons k ; todo transform path
                             (bib-entry-mrnumber v))
                       table))
               *document-bibtex-table*)
      (push :document-mr-table table)
      (print table stream))))

(defun load-manager-data ()
  ;; we reset the bibtex-table
  (setf *document-bibtex-table* (make-hash-table :test 'eql))
  ;; we read in all bib files
  (apply #'load-bib-file (mapcar #'library-file bib-files))
  ;; then, we load the pathname mappings
  (load-document-mr-table (library-file mrnumber-database)))

(defun save-manager-data ()
  ;; first store the documents table
  (save-document-mr-table (library-file mrnumber-database))
  ;; then write out to the first bib file
  (save-bib-table (library-file (first bib-files))))

(defgeneric associate (obj-1 obj-2))

(defmethod associate ((path pathname) (entry bibtex-runtime::bib-entry))
  ;; make sure to put this entry into our database
  (save-bib-entry entry)
  ;; then associate the path with it
  (setf (gethash path *document-bibtex-table*) entry))

(defmethod associate ((path pathname) (mrnumber integer))
  ;; retrieve entry from local storage or from mathscinet, if
  ;; necessary
  (setf (gethash path *document-bibtex-table*)
        (bib-entry-by-mr mrnumber)))

(defun associated-entry (path)
  (gethash path *document-bibtex-table*))
