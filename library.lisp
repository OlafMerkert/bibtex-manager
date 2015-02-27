(defpackage :bibtex-manager/library
  (:nicknames :library)
  (:use :cl :ol)
  (:export
   #:ensure-pathname-relative
   #:list-head-match
   #:filename->metadata
   #:regex-separation-filter
   #:all-files-in-library
   #:library-root
   #:library-files
   #:all-folders-in-library
   #:pathname-folder
   #:files-in-folders
   #:library-path))

(in-package :bibtex-manager/library)

(defpar library-root (first (uiop:directory* "~/Perfezionamento/topics/")))

(defun library-path (file)
  (merge-pathnames file library:library-root))

(defpar excluded-file-endings '("bib" "tex"))

(defun all-files-in-library ()
  (nprog1 files
    (fad:walk-directory
     library-root
     (lambda (path) (push path files))
     :directories :depth-first
     :test (lambda (path)
             (not (or (fad:directory-pathname-p path) ; only files
                      ;; ignore some file endings
                      (member (pathname-type path) excluded-file-endings
                              :test #'string-equal))))
     :follow-symlinks nil)))

;;; implement diverse heuristics for detecting author and title
(defun regex-separation-filter (regex &optional (limit 2))
  "Return a function that filters out string which are split into at
least `limit' parts by `regex'."
  (lambda (name) (let1 (parts (ppcre:split regex name :limit limit))
              (when (<= limit (length parts))
                parts))))

(defpar name-filters
    (list (regex-separation-filter "\\s*;\\s*")
          (regex-separation-filter "\\s*-\\s*")
          (regex-separation-filter "\\s*,\\s*")
          #'identity))

(defun filename->metadata (pathname)
  "Try to extract author and title information from the basename,
using `name-filters'."
  (let* ((name (pathname-name pathname))
         (split-name
          (some (lambda (filter-fun) (funcall filter-fun name))
                name-filters)))
    (if (listp split-name)
        (progn
          (assert (= 2 (length split-name)))
          (list :author (first split-name) :title (second split-name)))
        (list :title split-name))))

;; extract the relative part of a given file
(defun list-head-match (short-list long-list &optional (test #'equal))
  "Verify that `long-list' starts with `short-list', comparing
elements with `test', and return the remainder of `long-list'."
  (cond ((null short-list) long-list)
        ((null long-list) (error "long-list too short, remains ~A" short-list))
        ((or (atom short-list) (atom long-list))
         (error "cannot compare atoms: ~A ~A" short-list long-list))
        ((funcall test (car short-list) (car long-list) )
         (list-head-match (cdr short-list) (cdr long-list)))
        (t (error "lists do not match: ~A != ~A"
                  (car short-list) (car long-list)))))

(defun ensure-pathname-relative (pathname relative-to-dir)
  "For files contained in `relative-to-dir', convert to a pathname
relative to it."
  (let ((rltv-dir (pathname-directory relative-to-dir))
        (path-dir (pathname-directory pathname)))
    (if (fad:pathname-relative-p pathname)
        pathname
        (make-pathname :directory (list* :relative (list-head-match rltv-dir path-dir))
                       :defaults pathname))))

(defun all-files-in-library/relative ()
  (mapcar (lambda (path) (ensure-pathname-relative path library-root))
          (all-files-in-library)))

(defpar library-files (all-files-in-library/relative))

(defun pathname-folder (pathname)
  (or (cdr (pathname-directory pathname))
      (list "/")))

(defun all-folders-in-library (&optional (files library-files))
  (sort (remove-duplicates (mapcar #'pathname-folder files) :test 'equal)
        #'string<
        :key (lambda (f) (string-join f " "))))

(defun files-in-folders (folder &optional (files library-files))
  (remove folder files :test-not 'equal :key #'pathname-folder ))
