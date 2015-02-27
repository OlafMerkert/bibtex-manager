(defpackage :bibtex-manager/clim-ui
  (:use :clim-lisp :clim :ol)
  (:export))

(in-package :bibtex-manager/clim-ui)

(defpar ui-width 800)

(define-application-frame manager-ui ()
  ((current-object :initform nil
                   :accessor current-object)
   (result-count :initform 10
                 :accessor result-count))
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
  ;; before starting, we read in the library data
  (bibtex-storage:load-manager-data)
  (run-frame-top-level (make-instance 'manager-ui))
  ;; and after closing, we automatically save the data
  (bibtex-storage:save-manager-data))

(defun show-current-object (frame pane)
  (with-slots (current-object) frame
    (when current-object
      (present current-object
               (cond ((pathnamep current-object) 'document)
                     ((bibtex-runtime::bib-entry-p current-object) 'bib-entry)
                     ((listp current-object) 'folder)
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
    `((:title  . ,+blue+)
      (:author . ,+black+)
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
;; (define-manager-ui-command (com-go :name "Go" :menu t) ()
;;   ;; (with-pane)
;;   )

(defun new-section (pane &optional force)
  (declare (ignore force))
  (terpri pane))

(defun display-documents (pane files)
  (new-section pane)
  (dolist (file files)
    (present file 'document :stream pane :single-box t)
    (terpri pane)))

(defun display-folders (pane folders)
  (new-section pane)
  (dolist (folder folders)
    (present folder 'folder :stream pane :single-box t)
    (terpri pane)))

(defun display-bib-entries (pane entries)
  (new-section pane)
  (if entries
      (dolist (entry entries)
        (present entry 'bib-entry :stream pane :single-box t)
        (terpri pane))
      (in-colour :warning (format pane "No Bib entries found!~%"))))

;;; setting app options
(define-manager-ui-command (com-set-result-count :name "Set Result Count") ((count 'integer :prompt "Count"))
  (with-pane
    (format pane "Display at most ~D results.~%"
            (setf (result-count *application-frame*) (max 50 count)))))

;;; file listing
(define-manager-ui-command (com-list-all :name "List All") ()
  (with-pane (display-documents pane library:library-files)))

(define-manager-ui-command (com-list-folders :name "List Folders") ()
  (with-pane (display-folders pane (library:all-folders-in-library))))

(define-manager-ui-command (com-list-folder :name "List Folder")
    ((folder 'folder))
  (setf (current-object *application-frame*) folder)
  (with-pane (display-documents pane (library:files-in-folders folder))))

;;; search command
(define-manager-ui-command (com-bib-search :name "Search All" :menu t)
    ((anywhere 'string :prompt "All fields"))
  (with-pane
    (display-bib-entries pane (take (result-count *application-frame*)
                                    (mathscinet:search-bibtex-entries :anywhere anywhere )))))

(define-manager-ui-command (com-bib-search-author :name "Search Author" :menu t)
    ((author 'string :prompt "Author"))
  (with-pane
    (display-bib-entries pane (take (result-count *application-frame*)
                                    (mathscinet:search-bibtex-entries :author author)))))

(define-manager-ui-command (com-bib-search-title :name "Search Title" :menu t)
    ((title 'string :prompt "Title"))
  (with-pane
    (display-bib-entries pane (take (result-count *application-frame*)
                                    (mathscinet:search-bibtex-entries :title title)))))

(define-manager-ui-command (com-bib-search-mr :name "Search MR")
    ((mr-number 'integer :prompt "MR"))
  (with-pane
    (display-bib-entries pane (take (result-count *application-frame*)
                                    (mathscinet:search-bibtex-entries :mrnumber mr-number)))))

;;; 
(define-manager-ui-command (com-bib-lookup :name "Bib Lookup")
    ((file 'document))
  (setf (current-object *application-frame*) file)
  (with-pane
    (display-bib-entries pane (take (result-count *application-frame*)
                                    (apply #'mathscinet:search-bibtex-entries/fallbacks
                                           (library:filename->metadata file))))))


;;; association
(define-manager-ui-command (com-bib-associate :name "Bib Associate" :menu t) ((bib-entry 'bib-entry))
  ;; use the current object (if it is a pathname)
  (with-slots (current-object) *application-frame*
    (when (pathnamep current-object)
      (bibtex-storage:associate current-object bib-entry))))

(define-manager-ui-command (com-list-associated :name "List Associated") ()
  (with-pane (display-documents pane (remove-if-not #'bibtex-storage:associated-entry
                                                    library:library-files ))))

(define-manager-ui-command (com-list-unassociated :name "List UnAssociated") ()
  (with-pane (display-documents pane (remove-if #'bibtex-storage:associated-entry
                                                library:library-files ))))

(define-manager-ui-command (com-list-library :name "List Library") ()
  (with-pane (display-bib-entries pane (table-values bibtex-storage:*mrnumber-bibtex-table*))))

;;; bib library management
(define-manager-ui-command (com-bib-store :name "Bib Store") ((bib-entry 'bib-entry))
  (bibtex-storage:save-bib-entry bib-entry))

(define-manager-ui-command (com-bib-show :name "Bib Show" :menu t) ((entry 'bib-entry))
  (with-pane
    (new-section pane)
    (bibtex-runtime:write-bib-entry entry pane)))

(define-manager-ui-command (com-open-file :name "Open File") ((document 'document))
  (run-program "/usr/bin/xdg-open" (mkstr (library:library-path document))))

(define-manager-ui-command (com-bib-entry :name "Bib Entry" :menu t)
    ((document 'document))
  (setf (current-object *application-frame*) document)
  (awhen (bibtex-storage:associated-entry document)
    (with-pane (new-section pane)
               (display-bib-entries pane (list it)))))

;;; define presentations for pathnames and bib-entries
(define-presentation-type document ())

(define-presentation-method presentation-typep (object (type document))
  (pathnamep object))

(defun print-document (pane &key author title type year folder associated)
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
    ;; (print-part folder :prefix " " :infix " ")
    (when folder
      (princ " " pane)
      (in-colour :folder
        (present folder 'folder :stream pane)))
    (when associated (princ " *" pane))))

(define-presentation-method present (relative-path (type document) pane view &key)
  (let1 (data (library:filename->metadata relative-path))
    (apply #'print-document pane
           :type (string-downcase (pathname-type relative-path))
           :folder (library:pathname-folder relative-path)
           :associated (bibtex-storage:associated-entry relative-path )
           data)))

(define-presentation-method present (relative-path (type document) pane (view textual-dialog-view) &key)
  (princ relative-path pane))

(define-presentation-method accept ((type document) stream (view textual-dialog-view) &key)
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

(define-presentation-type folder ())

(define-presentation-method presentation-typep (object (type folder))
  (and (listp object)
       (every #'stringp object)))

(define-presentation-method present (folder (type folder) pane view &key)
  (princ (string-join folder " ") pane))

;; use slashes in the dialog
(define-presentation-method present (folder (type folder) pane (view textual-dialog-view) &key)
  (princ (string-join folder "/") pane))

(define-presentation-method accept ((type folder) stream (view textual-dialog-view) &key)
  (split-sequence:split-sequence #\/ (read-line stream)))

;;; translate clicks to commands
(define-presentation-to-command-translator translate-bib-show
    (bib-entry com-bib-show manager-ui)
    (object) (list object))

(define-presentation-to-command-translator translate-open-file
    (document com-open-file manager-ui)
    (object) (list object))

(define-presentation-to-command-translator translate-list-folder
    (folder com-list-folder manager-ui)
    (object) (list object))

;; Local Variables:
;; clim-application: manager-ui
;; End:
