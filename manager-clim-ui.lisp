(defpackage :bibtex-manager/clim-ui
  (:use :clim-lisp :clim :ol)
  (:export))

(in-package :bibtex-manager/clim-ui)

(defpar ui-width 800)

(define-application-frame manager-ui ()
  ((current-object :initform nil
                   :accessor current-object))
  (:panes (main :application
                :width ui-width :height 600
                :scroll-bars t
                :incremental-redisplay t)
          (current-object-pane :application
                               :width ui-width :height 10
                               :display-function #'show-current-object)
          (int :interactor
               :width ui-width :height 160
               :scroll-bars t))
  (:layouts (default (vertically () main current-object-pane int))))

(defun manager-ui ()
  (run-frame-top-level (make-instance 'manager-ui)))

(defun show-current-object (frame pane)
  (with-slots (current-object) frame
    (when current-object
      (present current-object
               (cond ((pathnamep current-object) 'document)
                     ((bibtex-runtime::bib-entry-p current-object) 'bib-entry)
                     (t (presentation-type-of current-object)))
               :stream pane
               :single-box t
               :allow-sensitive-inferiors nil))))

(define-manager-ui-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

(defmacro with-pane (&body body)
  `(let1 (pane (get-frame-pane *application-frame* 'main))
     ,@body))

(defpar colour-table
    `((:title  . ,+red+)
      (:author . ,+blue+)
      (:folder . ,+black+)
      (:type   . ,+gray+)
      (:year   . ,+black+)
      (:warning . ,+orange+)))

(defmacro in-colour (colour &body body)
  `(with-drawing-options (pane :ink (assoc1 ,colour colour-table))
     ,@body))

(define-manager-ui-command (com-clear :name "Clear") ()
  (with-pane (window-clear pane)))

;; a command for testing code
(define-manager-ui-command (com-go :name "Go" :menu t) ()
  ;; (with-pane)
  )

(define-manager-ui-command (com-list-all :name "List All") ()
  (with-pane
    (dolist (file library:library-files)
      (present file 'document :stream pane :single-box t
               :allow-sensitive-inferiors nil)
      (terpri))))

(define-manager-ui-command (com-show-bib :name "Show Bib")
    ((file 'document))
  (let1 (entries (apply #'mathscinet:search-bibtex-entries (library:filename->metadata file)))
    (with-pane
      (if entries
          (dolist (entry entries)
            (present (car entry) 'bib-entry :stream pane :single-box t
                     :allow-sensitive-inferiors nil)
            (terpri))
          (in-colour :warning (format pane "No Bib entries found!~%"))))))

;;; define presentations for pathnames and bib-entries
(define-presentation-type document ())

(define-presentation-method presentation-typep (object (type document))
  (pathnamep object))

(defun print-document (pane &key author title type year folder)
  (macrolet ((print-part (name &key prefix infix suffix)
               `(when ,name
                  ,(when prefix `(princ ,prefix pane))
                  (in-colour ,(keyw name)
                    ,(if infix
                         `(format pane ,(concatenate 'string "~{~A~^" infix "~}") ,name)
                         `(princ ,name pane)))
                  ,(when suffix `(princ ,suffix pane)))))
    (print-part author :suffix ": ")
    (print-part title)
    (print-part type :prefix ".")
    (print-part year :prefix " ")
    (print-part folder :prefix " " :infix " ")))


(define-presentation-method present (relative-path (type document) pane view &key)
  (let1 (data (library:filename->metadata relative-path))
    (apply #'print-document pane
           :type (string-downcase (pathname-type relative-path))
           :folder (cdr (pathname-directory relative-path))
           data)))

(define-presentation-method present (relative-path (type document) pane (view textual-dialog-view) &key)
  (princ relative-path pane))

(define-presentation-method accept ((type document) stream (view textual-dialog-view) &key)
  ;; FIX don't use read
  (pathname (read-line stream)))

;; (define-presentation-method accept ((type document) pane view &key default default-type))

(define-presentation-type bib-entry ())

(define-presentation-method presentation-typep (object (type bib-entry))
  (bibtex-runtime::bib-entry-p object))

(define-presentation-method present (entry (type bib-entry) pane view &key)
  (print-document pane
                  :author (mathscinet:bib-entry-author entry)
                  :title (mathscinet:bib-entry-title entry)
                  :type (bibtex-runtime:bib-entry-type entry)
                  :year (mathscinet:bib-entry-year entry)))
