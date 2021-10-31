;;;; Example Sunny - 2021-10-13

(in-package :gtk-application)

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

(defun new-sunny-window (application filename)
  (let ((window (make-instance 'gtk-application-window
                               :application application
                               :title "Sunny"
                               :show-menubar nil
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
    (declare (ignorable header))
    ;; TODO: Does not work as expected. The application window has no menu, when
    ;; setting the titlebar.
    (gtk-widget-show header)
    (setf (gtk-window-titlebar window) header)
    ;; Load the file into the buffer
    (when filename
      (let ((buffer (gtk-text-view-buffer textview)))
        (load-file-into-buffer buffer (sys-path filename))))
    ;; Pack and show the widgets
    (gtk-container-add scrolled textview)
    (gtk-container-add overlay scrolled)
    (gtk-container-add window overlay)
    (gtk-widget-show-all window)))

(defun sunny-action-new (application action parameter)
  (declare (ignore action parameter))
  (g-application-activate application))

(defun sunny-action-about (application action parameter)
  (declare (ignore application action parameter))
  (gtk-show-about-dialog nil
                         :modal t
                         :program-name "Sunny"
                         :title "About Sunny"
                         :logo-icon-name "sunny"
                         :comments "A cheap Bloatpad clone."))

(defun sunny-action-quit (application action parameter)
  (declare (ignore action parameter))
  (let ((windows (gtk-application-windows application)))
    (dolist (window windows)
      (gtk-widget-destroy window))))

(defun sunny (&rest argv)
  (let ((argv (cons "sunny" (if argv argv (uiop:command-line-arguments))))
        ;; Create the application
        (app (make-instance 'sunny
                            :application-id "com.crategus.sunny"
                            :flags :handles-open)))
    ;; Connect signal "startup" to the application
    (g-signal-connect app "startup"
        (lambda (application)
          (let ((actions (list (list "about"
                                     #'(lambda (action parameter)
                                         (sunny-action-about application
                                                             action
                                                             parameter))
                                     nil nil nil)
                               (list "quit"
                                     #'(lambda (action parameter)
                                         (sunny-action-quit application
                                                            action
                                                            parameter))
                                     nil nil nil)
                               (list "new"
                                     #'(lambda (action parameter)
                                         (sunny-action-new application
                                                           action
                                                           parameter))
                                     nil nil)))
                (builder (make-instance 'gtk-builder)))
          ;; Set the application name
          (unless (g-application-name)
            (setf (g-application-name) "Sunny"))
          ;; Add the actions
          (g-action-map-add-action-entries application actions)
          ;; Read the menus from a string
          (gtk-builder-add-from-string builder *sunny-menus*)
          ;; Set the application menu
          (setf (gtk-application-app-menu application)
                (gtk-builder-object builder "app-menu")))))
    ;; Connect signal "open" to the application
    (g-signal-connect app "open"
        (lambda (application files n-files hint)
          (declare (ignore hint))
          (dotimes (i n-files)
            (let* ((file (mem-aref files '(g-object g-file) i))
                   (filename (g-file-basename file)))
              (new-sunny-window application filename)))))
    ;; Connect signal "activate" to the application
    (g-signal-connect app "activate"
                          (lambda (application)
                            (new-sunny-window application nil)))
    ;; Run the application
    (g-application-run app argv)))
