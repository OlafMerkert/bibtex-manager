(defpackage :bibtex-manager/clim-ui
  (:use :clim-lisp :clim :ol)
  (:export))

(in-package :bibtex-manager/clim-ui)

(define-application-frame manager-ui ()
  ()
  (:panes (main :application
                :width 800 :height 600
                :scroll-bars t
                :incremental-redisplay t)
          (int :interactor
               :width 800 :height 80
               :scroll-bars t))
  (:layouts (default (vertically () main int))))

(defun manager-ui ()
  (run-frame-top-level (make-instance 'manager-ui)))

(define-manager-ui-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

;; a command for testing code
(define-manager-ui-command (com-go :name "Go" :menu t) ()
  (with-pane))

(define-manager-ui-command (com-list-all :name "List All") ()
  (with-pane
    (dolist (file library:library-files)
      (present file 'document :stream pane :single-box t
               :allow-sensitive-inferiors nil)
      (terpri))))

(defmacro with-pane (&body body)
  `(let1 (pane (get-frame-pane *application-frame* 'main))
     ,@body))

(defpar colour-table
    `((:title . ,+red+)
      (:author . ,+blue+)
      (:folder . ,+black+)
      (:type  . ,+gray+)))

(defmacro in-colour (colour &body body)
  `(with-drawing-options (pane :ink (assoc1 ,colour colour-table))
    ,@body))

;;; define presentations for pathnames and bib-entries
(define-presentation-type document ())

(define-presentation-method presentation-typep (object (type document))
  (pathnamep object))

(define-presentation-method present (relative-path (type document) pane view &key)
  (let1 (data (library:filename->metadata relative-path))
    (awhen (getf data :author)
      (in-colour :author
        (princ it pane))
      (princ ": " pane))
    (awhen (getf data :title)
      (in-colour :title
        (princ it pane)))
    (in-colour :type
      (format pane ".~A " (string-downcase (pathname-type relative-path))))
    (awhen (cdr (pathname-directory relative-path))
      (in-colour :folder
        (format pane "~{~A~^ ~}" it)))))

;; (define-presentation-method accept ((type document) pane view &key default default-type))
