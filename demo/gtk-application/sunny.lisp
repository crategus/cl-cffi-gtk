;;;; Example Sunny (2021-9-18)

(in-package :gtk-application)

(defvar *sunny-application* nil)

(defparameter *sunny-menus*
"<interface>
  <menu id='app-menu'>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_New Window</attribute>
        <attribute name='action'>app.new</attribute>
      </item>
      <item>
        <attribute name='label' translatable='yes'>_About Sunny</attribute>
        <attribute name='action'>app.about</attribute>
      </item>
      <item>
        <attribute name='label' translatable='yes'>_Quit</attribute>
        <attribute name='action'>app.quit</attribute>
        <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
    </section>
  </menu>
</interface>")

(defclass sunny (gtk-application)
  ()
  (:metaclass gobject-class))

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk-application)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk-text-buffer-bounds buffer)
    (gtk-text-buffer-delete buffer start end)))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk-text-buffer-insert buffer line)
      (gtk-text-buffer-insert buffer (format nil "~%")))))

(defun new-application-window (application filename)
  (let ((window (make-instance 'gtk-application-window
                               :application application
                               :title "Sunny"
                               :show-menu-bar t
                               :icon-name "sunny"
                               :default-width 480
                               :default-height 320))
        (header (make-instance 'gtk-header-bar
                               :title "Sunny"
                               :show-close-button t))
        (scrolled (make-instance 'gtk-scrolled-window
                                 :hexpand t
                                 :vexpand t))
        (textview (make-instance 'gtk-text-view))
        (overlay (make-instance 'gtk-overlay)))

    (format t "New Application window.~%")
    (format t "   application : ~a~%" application)
    (format t "      filename : ~a~%" filename)
    (format t "       windows : ~a~%" (gtk-application-windows application))

;    (setf (gtk-window-titlebar window) header)

    ;; Connect signal "destroy" to the application window
    (g-signal-connect window "destroy"
        (lambda (widget)
          (declare (ignore widget))
          (let ((windows (gtk-application-windows *sunny-application*)))
            (format t "Signal destroy.~%")
            (format t "  windows : ~a~%" windows)
;          (unless windows
;            ;; Quit the application
;            (g-application-quit application))
)))

    (when filename
      (let ((buffer (gtk-text-view-buffer textview)))
        (format t "    buffer : ~a~%" buffer)
        (load-file-buffer buffer (sys-path filename))
    ))

    (gtk-container-add scrolled textview)
    (gtk-container-add overlay scrolled)
    (gtk-container-add window overlay)

    (gtk-widget-show-all window)))

(defun new-activated (action parameter)
  (declare (ignore action parameter))
  (format t "New Activated.~%")
  (g-application-activate *sunny-application*))

(defun about-activated (action parameter)
  (declare (ignore action parameter))
  (format t "About Activated.~&")
  (gtk-show-about-dialog nil
                         :program-name "Sunny"
                         :title "About Sunny"
                         :logo-icon-name "sunny"
                         :comments "A cheap Bloatpad clone."))

(defun quit-activated (action parameter)
  (declare (ignore action parameter))
  (let* ((application *sunny-application*)
         (windows (gtk-application-windows application)))
    (format t "Quit activated.~%")
    (format t "  application : ~a~%" application)
    (format t "      windows : ~a~%" windows)
    (dolist (window windows)
      (gtk-widget-destroy window))))
;    (g-application-quit application)))

(defun sunny (&rest argv)
  (within-main-loop
    (let (;; Create the application
          (app (make-instance 'sunny
                              :application-id "com.crategus.sunny"
                              :flags '(:handles-open))))
      (setf *sunny-application* app)
      ;; Connect signal "activate" to the application
      (g-signal-connect app "activate"
                            (lambda (application)
                              (format t "Application is in activate.~%")
                              (new-application-window application nil)))

      ;; Connect signal "startup" to the application
      (g-signal-connect app "startup"
          (lambda (application)
            (format t "Application is in startup.~%")
            (let ((entries (list (list "about" #'about-activated nil nil nil)
                                 (list "quit" #'quit-activated nil nil nil)
                                 (list "new" #'new-activated nil nil)))
                  (builder (make-instance 'gtk-builder)))

            (setf (g-application-name) "Sunny")
            (g-action-map-add-action-entries application entries)

            ;; Read the menus from a string
            (gtk-builder-add-from-string builder *sunny-menus*)
            ;; Set the application menu
            (setf (gtk-application-app-menu application)
                  (gtk-builder-object builder "app-menu")))))

      ;; Connect signal "open" to the application
      (g-signal-connect app "open"
          (lambda (application files n-files hint)
            (declare (ignore hint))
            (format t "Application is in open.~%")

            (dotimes (i n-files)
              (let* ((file (mem-aref files '(g-object g-file) i))
                     (filename (g-file-basename file)))
                (format t "  filename : ~a~%" filename)
                (new-application-window application filename)))

      ))

      ;; Connect signal "shutdown" to the application
      (g-signal-connect app "shutdown"
                        (lambda (application)
                          (declare (ignore application))
                          (format t "Application in shutdown.~%")
                          ;; Leave the main loop on shutdown
                          (leave-gtk-main)))
      ;; Run the application
      (g-application-run app argv)))
  (join-gtk-main))
