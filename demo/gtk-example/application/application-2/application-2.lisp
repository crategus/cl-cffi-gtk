(defpackage :application-2
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:example-app))

(in-package :application-2)

(trace gobject::INITIALIZE-GOBJECT-CLASS-G-TYPE)
(trace gobject::register-object-type)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *debug-subclass* t))

;;; --- app-window -------------------------------------------------------------

(defclass app-window (gtk-application-window)
  ((template-p :initform nil
               :accessor app-window-template-p
               :allocation :class))
  (:g-type-name . "AppWindow")
  (:metaclass gobject-class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type-implementation
      "AppWindow" app-window "GtkApplicationWindow" nil nil))

(defmethod initialize-instance :after
    ((window app-window) &key &allow-other-keys)
  (format t "~&in INITIALIZE-INSTANCE :after for AppWindow~%")

  (unless (app-window-template-p window)
    (let ((resource (g-resource-load "application.gresource")))
      (format t "~&Register template from resource~%")
      (g-resources-register resource)
      (gtk-widget-class-set-template-from-resource
                                          "AppWindow"
                                          "/com/crategus/application/window.ui")
      (g-resources-unregister resource)
      (setf (app-window-template-p window) t)))

  (gtk-widget-init-template window)

  (g-signal-connect window "delete-event"
                    (lambda (widget event)
                      (declare (ignore event))
                      (format t "Application in DELETE-EVENT~%")
                      (let ((app (gtk-window-application widget)))
                        (g-application-quit app)))))

(defun app-window-new (app)
  (make-instance 'app-window
                 :application app))

(defun app-window-open (win file)
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
                      (gtk-window-present (app-window-new app))))
  (g-signal-connect app "open"
                    (lambda (app files n-files hint)
                      (declare (ignore hint))
                      (format t "Application in OPEN~%")
                      (let ((win (first (gtk-application-windows app))))
                        (unless win
                          (setf win (app-window-new app)))
                         (dotimes (i n-files)
                           (app-window-open win (mem-aref files
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

    (g-application-run (example-app-new) argv)

))

;;; 2020-12-19
