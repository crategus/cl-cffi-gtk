(defpackage :application-1
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:example-app))

(in-package :application-1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *debug-subclass* nil))

;;; --- example-app-window -----------------------------------------------------

(defclass example-app-window (gtk-application-window)
  ()
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((win example-app-window) &key &allow-other-keys)
  (g-signal-connect win "delete-event"
                    (lambda (widget event)
                      (declare (ignore event))
                      (format t "Application in DELETE-EVENT~%")
                      (let ((app (gtk-window-application widget)))
                        (g-application-quit app)))))

(defun example-app-window-new (app)
  (make-instance 'example-app-window
                 :application app))

(defun example-app-window-open (win file)
  (declare (ignore win file))
  ;; Nothing to do.
)

;;; --- example-app ------------------------------------------------------------

(defclass example-app (gtk-application)
  ()
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((app example-app) &key &allow-other-keys)
  (g-signal-connect app "activate"
                    (lambda (app)
                      (format t "Application in ACTIVATE~%")
                      (gtk-window-present (example-app-window-new app))))
  (g-signal-connect app "open"
                    (lambda (app files n-files hint)
                      (declare (ignore hint))
                      (format t "Application in OPEN~%")
                      (let ((win (first (gtk-application-windows app))))
                        (unless win
                          (setf win (example-app-window-new app)))
                        (dotimes (i n-files)
                          (example-app-window-open win
                                                   (mem-aref files
                                                             'g-object
                                                             i)))
                          (gtk-window-present win))))
  (g-signal-connect app "shutdown"
                    (lambda (app)
                      (declare (ignore app))
                      (format t "Application in SHUTDOWN~%")
                      (leave-gtk-main))))

(defun example-app-new ()
  (unless (string= "Example Application" (g-application-name))
    (setf (g-application-name) "Example Application"))
  (make-instance 'example-app
                 :application-id "com.crategus.example-app"
                 :flags :handles-open
                 :inactivity-timeout 10000
                 :register-session t))

(defun example-app (&optional (argv nil))
  (within-main-loop
    (g-application-run (example-app-new) argv)))

;;; 2020-12-19
