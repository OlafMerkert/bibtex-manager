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

(defun bib-mrnum-map-path ()
  (merge-pathnames relative-bib-map-path library:library-root))

(defvar *path-mrnum-map* nil)

(defvar *mrnum-bibtex-cache*
  (make-hash-table))

(defun save-bib ()
  ;; todo protect against external changes
  )

(defun load-bib ()
  ;; todo protect against unsaved changes
  ;; reset loaded data
  (setf *path-mrnum-map* nil
        *mrnum-bibtex-cache* (make-hash-table))
  ;; load bibtex cache-store first
  (let ((*bib-database* (make-hash-table :test 'equal)))
    (with-open-file (stream (bib-library-path) :if-does-not-exist nil)
      (when stream (read-bib-database stream)))
    (maphash-values (lambda (entry) (cache-store entry :replace t)) *bib-database*))
  ;; then load the mrnum-map
  (with-open-file (stream (bib-mrnum-map-path) :if-does-not-exist nil)
    (when stream
      ;; fix use safe-read
      (setf *path-mrnum-map* (assoc1 :path-mrnum-map (read stream))))))


(defgeneric associate (obj-1 obj-2))

(defmethod associate ((path pathname) (entry bibtex-runtime::bib-entry))
  (let1 (cons (or (assoc path *path-mrnum-map*)
                  (aprog1 (cons path nil)
                    (push it *path-mrnum-map*))))
    (setf (cdr cons) (mathscinet:bib-entry-mrnumber entry))
    (cache-store entry)))

(defgeneric cache-store (obj &key replace))

(defmethod cache-store ((entry bibtex-runtime::bib-entry) &key (replace t))
  (when (or replace (not #1=(gethash (mathscinet:bib-entry-mrnumber entry) *doi-bibtex-cache*)))
    (setf #1# entry)
    (values entry (mathscinet:bib-entry-doi entry))))

(defmethod cache-store ((obj (eql nil)) &key replace)
  (declare (ignore replace))
  nil)

(defun mrnum->entry (mrnum)
  (or (gethash mrnum *mrnum-bibtex-cache*)
      (cache-store (first (search-bibtex-entries :mrnumber mrnum)))
      ))

(defmethod associate ((path pathname) (mrnum integer))
  (associate path (mrnum->entry mrnum)))

(defun associated-entry (path)
  (aif (assoc path *path-mrnum-map*)
       (mrnum->entry (cdr it))))

;; maybe it would be smarter to use MR number instead of doi? this
;; would also allow to use integers for identifying stuff!
