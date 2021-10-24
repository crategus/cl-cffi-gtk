(defpackage :application-1
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:example-app-1))

(in-package :application-1)

;;; --- example-app-window -----------------------------------------------------

(defclass example-app-window (gtk-application-window)
  ()
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((win example-app-window) &key &allow-other-keys)
  ;; Nothing to do
)

(defun example-app-window-new (app)
  (make-instance 'example-app-window
                 :application app))

(defun example-app-window-open (win filename)
  (format t "in EXAMPLE-APP-WINDOW-OPEN~%")
  (format t "    window : ~a~%" win)
  (format t "  filename : ~a~%" filename))

;;; --- example-app ------------------------------------------------------------

(defclass example-app (gtk-application)
  ()
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((app example-app) &key &allow-other-keys)
  ;; Connect signal "activate" to the application
  (g-signal-connect app "activate"
      (lambda (app)
        (format t "Application in ACTIVATE~%")
        (gtk-window-present (example-app-window-new app))))
    ;; Connect signal "open" to the application
    (g-signal-connect app "open"
        (lambda (application files n-files hint)
          (declare (ignore hint))
          (let ((win (first (gtk-application-windows application))))
            (unless win
              (setf win (example-app-window-new application)))
            (dotimes (i n-files)
              (let* ((file (mem-aref files '(g-object g-file) i))
                     (filename (g-file-basename file)))
                (example-app-window-open win filename)))
            (gtk-window-present win)))))

(defun example-app-new ()
  (make-instance 'example-app
                 :application-id "com.crategus.exampleapp1"
                 :flags :handles-open))

(defun example-app-1 (&rest argv)
  (let ((argv (cons "exampleapp"
                    (if argv argv (uiop:command-line-arguments)))))
    (g-application-run (example-app-new) argv)))

;;; 2021-10-16
