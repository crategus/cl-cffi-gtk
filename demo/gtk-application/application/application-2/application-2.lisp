(defpackage :application-2
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:example-app-2))

(in-package :application-2)

(defparameter *window-ui*
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
  <template class='ExampleAppWindow' parent='GtkApplicationWindow'>
    <property name='title' translatable='yes'>Example Application</property>
    <property name='default-width'>600</property>
    <property name='default-height'>400</property>
    <child>
      <object class='GtkBox' id='content_box'>
        <property name='visible'>True</property>
        <property name='orientation'>vertical</property>
        <child>
          <object class='GtkHeaderBar' id='header'>
            <property name='visible'>True</property>
            <child type='title'>
              <object class='GtkStackSwitcher' id='tabs'>
                <property name='visible'>True</property>
                <property name='stack'>stack</property>
              </object>
            </child>
          </object>
        </child>
        <child>
          <object class='GtkStack' id='stack'>
            <property name='visible'>True</property>
          </object>
        </child>
      </object>
    </child>
  </template>
</interface>")

;;; --- example-app-window -----------------------------------------------------

(defclass example-app-window (gtk-application-window)
  ((template-p :initform nil
               :accessor app-window-template-p
               :allocation :class))
  (:g-type-name . "ExampleAppWindow")
  (:metaclass gobject-class))

(register-object-type-implementation
    "ExampleAppWindow" example-app-window "GtkApplicationWindow" nil nil)

;(defmethod initialize-instance :after
;    ((class example-app-window) &key &allow-other-keys)
;  (format t "in INITIAlIZE-INSTANCE for class : ~a~%" class)
;)

(defmethod initialize-instance :after
    ((window example-app-window) &key &allow-other-keys)
  (format t "~&in INITIALIZE-INSTANCE for ExampleAppWindow~%")

;  (unless (app-window-template-p window)
;    (let ((resource (g-resource-load "application.gresource")))
;      (format t "~&Register template from resource~%")
;      (g-resources-register resource)
;      (gtk-widget-class-set-template-from-resource
;                                          "ExampleAppWindow"
;                                          "/com/crategus/application/window.ui")
;      (g-resources-unregister resource)
;      (setf (app-window-template-p window) t)))

  (unless (app-window-template-p window)
    (gtk-widget-class-set-template "ExampleAppWindow" *window-ui*)
    (setf (app-window-template-p window) t))

  (gtk-widget-init-template window))

(defun example-app-window-new (app)
  (make-instance 'example-app-window
                 :application app))

(defun example-app-window-open (win filename)
  (declare (ignore win filename))
  ;; Nothing to do.
)

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
                 :application-id "com.crategus.exampleapp2"
                 :flags :handles-open))

(defun example-app-2 (&rest argv)
  (let ((argv (cons "exampleapp"
                    (if argv argv (uiop:command-line-arguments)))))
    (g-application-run (example-app-new) argv)))

;;; 2021-10-16
