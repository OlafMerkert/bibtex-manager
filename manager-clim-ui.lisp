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
  (with-pane (princ "Hello" pane)))

(defmacro with-pane (&body body)
  `(let1 (pane (get-frame-pane *application-frame* 'main))
     ,@body))
