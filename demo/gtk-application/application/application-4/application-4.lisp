(defpackage :application-4
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:example-app-4))

(in-package :application-4)

(defparameter *appmenu*
"<?xml version='1.0'?>
<interface>
  <!-- interface-requires gtk+ 3.0 -->
  <menu id='appmenu'>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_Preferences</attribute>
        <attribute name='action'>app.preferences</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_Quit</attribute>
        <attribute name='action'>app.quit</attribute>
      </item>
    </section>
  </menu>
</interface>")

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-application)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk-text-buffer-bounds buffer)
    (gtk-text-buffer-delete buffer start end)))

(defun load-file-into-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk-text-buffer-insert buffer line)
      (gtk-text-buffer-insert buffer (format nil "~%")))))

;;; --- example-app-window -----------------------------------------------------

(defclass example-app-window (gtk-application-window)
  ((template-p :initform nil
               :accessor app-window-template-p
               :allocation :class)
   (stack :initform nil
          :accessor example-app-window-stack))
  (:g-type-name . "ExampleAppWindow")
  (:metaclass gobject-class))

(register-object-type-implementation "ExampleAppWindow"      ; name
                                     example-app-window      ; class
                                     "GtkApplicationWindow"  ; parent
                                     nil                     ; interfaces
                                     (("stack"               ; prop-name
                                       "GtkWidget"           ; prop-type
                                       example-app-window-stack ; prop-accessor
                                       t                     ; prop-readable
                                       t)))                  ; prop-writable

(defmethod initialize-instance :after
    ((window example-app-window) &key &allow-other-keys)
  (format t "~&in INITIALIZE-INSTANCE for ExampleAppWindow~%")
  (let* ((box (make-instance 'gtk-box
                             :orientation :vertical))
         (header (make-instance 'gtk-header-bar))
         (stack (make-instance 'gtk-stack))
         (switcher (make-instance 'gtk-stack-switcher
                                  :stack stack)))

    (setf (example-app-window-stack window) stack)

    (gtk-container-add header switcher)
    (gtk-box-pack-start box header :expand nil :fill nil)
    (gtk-box-pack-start box stack)
    (gtk-container-add window box)
    (gtk-widget-show-all window)))

(defun example-app-window-new (app)
  (make-instance 'example-app-window
                 :title "Example Application"
                 :application app
                 :show-menubar nil
                 :default-width 480
                 :default-height 600))

(defun example-app-window-open (win filename)

  (format t "~&in EXAMPLE-APP-WINDOW-OPEN~%")
  (format t "         win : ~a~%" win)
  (format t "    filename : ~a~%" filename)

  (let ((scrolled (make-instance 'gtk-scrolled-window
                                 :hexpand t
                                 :vexpand t))
        (view (make-instance 'gtk-text-view
                             :editable nil
                             :cursor-visible nil)))

     (gtk-container-add scrolled view)
     (gtk-stack-add-titled (example-app-window-stack win)
                           scrolled
                           filename
                           filename)

     (load-file-into-buffer (gtk-text-view-buffer view) (sys-path filename))

     (gtk-widget-show scrolled)
     (gtk-widget-show view)
))

;;; --- example-app ------------------------------------------------------------

(defclass example-app (gtk-application)
  ()
  (:metaclass gobject-class))

(defmethod initialize-instance :after
    ((app example-app) &key &allow-other-keys)

  ;; Connect signal "startup" to the application
  (g-signal-connect app "startup"
    (lambda (app)
      (format t "Application in STARTUP~%")

      (setf (gtk-application-accels-for-action app "app.quit") "<Primary>q")

      ;; Add action "quit" to the application
      (let ((action (g-simple-action-new "quit" nil)))
        ;; Connect a handler to the signal activate
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (format t "~&in action QUIT~%")
             ;; Destroy all windows of the application. When the last window is
             ;; destroyed, the application is shutdown. The C example calls the
             ;; g_application_quit function. In the Lisp library this will not
             ;; close the open windows and the windows do not react.
             (dolist (window (gtk-application-windows app))
               (gtk-widget-destroy window))))
        ;; Add the action to action map of the application
        (g-action-map-add-action app action))

      ;; Add action "preferences" to the application
      (let ((action (g-simple-action-new "preferences" nil)))
        ;; Connect a handler to the signal activate
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
        ))
        ;; Add the action to action map of the application
        (g-action-map-add-action app action))

      (let ((builder (make-instance 'gtk-builder)))
        ;; Read the menus from a string
        (gtk-builder-add-from-string builder *appmenu*)
        ;; Set the application menu
        (setf (gtk-application-app-menu app)
              (gtk-builder-object builder "appmenu"))

  )))

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

(defun example-app-4 (&rest argv)
  (let ((argv (cons "exampleapp"
                    (if argv argv (uiop:command-line-arguments)))))
    (g-application-run (example-app-new) argv)))

;;; 2021-10-30
