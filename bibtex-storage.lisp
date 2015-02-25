(defpackage :bibtex-manager/bibtex-storage
  (:nicknames :bibtex-storage)
  (:use :cl :ol
        :bibtex-runtime
        :mathscinet)
  (:export
   #:associated-entry
   #:associate))

(in-package :bibtex-manager/bibtex-storage)

;; compute the required file names
(defpar relative-bib-library-path "topics.bib")
(defpar relative-bib-map-path "topics.sexp")

(defun bib-library-path ()
  (merge-pathnames relative-bib-library-path library:library-root))

(defun bib-doi-map-path ()
  (merge-pathnames relative-bib-map-path library:library-root))

(defvar *path-doi-map* nil)

(defvar *doi-bibtex-cache*
  (make-hash-table :test 'equal))

(defun save-bib ()
  ;; todo protect against external changes
  )

(defun load-bib ()
  ;; todo protect against unsaved changes
  ;; reset loaded data
  (setf *path-doi-map* nil
        *doi-bibtex-cache* (make-hash-table :test 'equal))
  ;; load bibtex cache-store first
  (let ((*bib-database* (make-hash-table :test 'equal)))
    (with-open-file (stream (bib-library-path) :if-does-not-exist nil)
      (when stream (read-bib-database stream)))
    (maphash-values (lambda (entry) (cache-store entry :replace t)) *bib-database*))
  ;; then load the doi-map
  (with-open-file (stream (bib-doi-map-path) :if-does-not-exist nil)
    (when stream
      ;; fix use safe-read
      (setf *path-doi-map* (assoc1 :path-doi-map (read stream))))))


(defgeneric associate (obj-1 obj-2))

(defmethod associate ((path pathname) (entry bibtex-runtime::bib-entry))
  (let1 (cons (or (assoc path *path-doi-map*)
                  (push (cons path nil) *path-doi-map*)))
    (setf (cdr cons) (mathscinet:bib-entry-doi entry))
    (cache-store entry)))

(defgeneric cache-store (obj &key replace))

(defmethod cache-store ((entry bibtex-runtime::bib-entry) &key (replace t))
  (when (or replace (not #1=(gethash (mathscinet:bib-entry-doi entry) *doi-bibtex-cache*)))
    (setf #1# entry)
    (values entry (mathscinet:bib-entry-doi entry))))

(defmethod cache-store ((obj (eql nil)) &key replace)
  (declare (ignore replace))
  nil)

(defun doi->entry (doi)
  (or (gethash doi *doi-bibtex-cache*)
      (cache-store (first (search-bibtex-entries :doi doi)))))

(defmethod associate ((path pathname) (doi string))
  (associate path (doi->entry doi)))

(defun associated-entry (path)
  (aif (assoc path *path-doi-map*)
       (doi->entry (cdr it))))
