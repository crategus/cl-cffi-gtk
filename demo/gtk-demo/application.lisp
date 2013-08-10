;;;; Simple Application

(defclass bloat-pad (gtk-application)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "BloatPad"))

(register-object-type-implementation "BloatPad"
                                     bloat-pad
                                     "GtkApplication"
                                     nil
                                     nil)

(defun new-window (application file)
  (declare (ignore file))
    (let (;; Create the application window
          (window (make-instance 'gtk-application-window
                                 :application application
                                 :title "Bloatpad"
                                 :border-width 12
                                 :default-width 500
                                 :default-height 400))
          (grid (make-instance 'gtk-grid))
          (toolbar (make-instance 'gtk-toolbar)))

      ;; Connect signal "destroy" to the application window
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)
                          (if (zerop gtk::*main-thread-level*)
                              (g-application-quit application))))

      ;; Add action "copy" to the application window
      (let ((action (g-simple-action-new "copy" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window "bloatpad-text"))))
               (gtk-text-buffer-copy-clipboard
                                  (gtk-text-view-get-buffer view)
                                  (gtk-widget-get-clipboard view
                                                            "CLIPBOARD"))))))

      ;; Add action "paste" to the application window
      (let ((action (g-simple-action-new "paste" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window "bloatpad-text"))))
               (gtk-text-buffer-paste-clipboard
                                       (gtk-text-view-get-buffer view)
                                       (gtk-widget-get-clipboard view
                                                                 "CLIPBOARD")
                                       :default-editable t)))))

      ;; Add action "fullscreen" to the application window
      (let ((action (g-simple-action-new-stateful
                                               "fullscreen"
                                               nil
                                               (g-variant-new-boolean nil))))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore parameter))
             (let* ((state (g-action-get-state action))
                    (value (g-variant-get-boolean state)))
               (g-action-change-state action
                                      (g-variant-new-boolean (not value))))))
        (g-signal-connect action "change-state"
           (lambda (action parameter)
             (if (g-variant-get-boolean parameter)
                 (gtk-window-fullscreen window)
                 (gtk-window-unfullscreen window))
             (g-simple-action-set-state action parameter))))

      ;; Add action "justify" to the application window
      (let ((action (g-simple-action-new-stateful
                                             "justify"
                                             (g-variant-type-new "s")
                                             (g-variant-new-string "left"))))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (g-action-change-state action parameter)))
        (g-signal-connect action "change-state"
           (lambda (action parameter)
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-get-data window "bloatpad-text")))
                   (str (g-variant-get-string parameter)))
               (cond ((equal str "left")
                      (gtk-text-view-set-justification view :left))
                     ((equal str "center")
                      (gtk-text-view-set-justification view :center))
                     (t
                      (gtk-text-view-set-justification view :right)))
               (g-simple-action-set-state action parameter)))))

      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-left")))
        (gtk-actionable-set-detailed-action-name button "win.justify::left")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-center")))
        (gtk-actionable-set-detailed-action-name button "win.justify::center")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :stock-id "gtk-justify-right")))
        (gtk-actionable-set-detailed-action-name button "win.justify::right")
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-separator-tool-item
                                   :draw nil)))
        (gtk-tool-item-set-expand button t)
        (gtk-container-add toolbar button))
      (let ((button (make-instance 'gtk-tool-item))
            (box (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6))
            (label (make-instance 'gtk-label
                                  :label "Fullscreen:"))
            (switch (make-instance 'gtk-switch)))
        (gtk-actionable-set-action-name switch "win.fullscreen")
        (gtk-container-add box label)
        (gtk-container-add box switch)
        (gtk-container-add button box)
        (gtk-container-add toolbar button))
      (gtk-grid-attach grid toolbar 0 0 1 1)
      (let ((scrolled (make-instance 'gtk-scrolled-window
                                     :hexpand t
                                     :vexpand t))
            (view (make-instance 'gtk-text-view)))
        (g-object-set-data window "bloatpad-text" (pointer view))
        (gtk-container-add scrolled view)
        (gtk-grid-attach grid scrolled 0 1 1 1))
      (gtk-container-add window grid)
      (gtk-widget-show-all window)))

(defun bloat-pad-activate (application)
  ;; Start a main loop and create an application window
  (within-main-loop
    (new-window application nil))
  ;; Wait until the main loop has finished
  (join-gtk-main))

(defun create-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk-about-dialog
                               :program-name "Example Dialog"
                               :version "0.00"
                               :copyright "(c) Dieter Kaiser"
                               :website
                               "github.com/crategus/cl-cffi-gtk"
                               :website-label "Project web site"
                               :license "LLGPL"
                               :authors '("Dieter Kaiser")
                               :documenters '("Dieter Kaiser")
                               :artists '("None")
                               :logo-icon-name
                               "applications-development"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk-dialog-run dialog)
    ;; Destroy the about dialog
    (gtk-widget-destroy dialog)))

(defvar *menu*
  "<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>")

(defun bloat-pad-startup (application)
  ;; Add action "new" to the application
  (let ((action (g-simple-action-new "new" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; ensure-gtk-main increases the thread level for the new window
         (ensure-gtk-main)
         (new-window application nil)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action "about" to the application
  (let ((action (g-simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (create-about-dialog)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))

  ;; Add action "quit" to the application
  (let ((action (g-simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk-application-get-windows application))
           (gtk-widget-destroy window))
         ;; Quit the main loop
         (leave-gtk-main)
         ;; Quit the application
         (g-application-quit application)))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action))

  ;; Intitialize the application menu and the menubar
  (let ((builder (make-instance 'gtk-builder)))
    ;; Read the menus from a string
    (gtk-builder-add-from-string builder *menu*)
    ;; Set the application menu
    (gtk-application-set-app-menu application
                                  (gtk-builder-get-object builder
                                                          "app-menu"))
    ;; Set the menubar
    (gtk-application-set-menubar application
                                 (gtk-builder-get-object builder
                                                         "menubar"))))

(defun bloat-pad-open (application)
  (declare (ignore application))
  ;; Executed when the application is opened
  nil)

(defun bloat-pad-shutdown (application)
  (declare (ignore application))
  ;; Executed when the application is shut down
  nil)

(defmethod initialize-instance :after
    ((app bloat-pad) &key &allow-other-keys)
  (g-signal-connect app "activate" #'bloat-pad-activate)
  (g-signal-connect app "startup" #'bloat-pad-startup)
  (g-signal-connect app "open" #'bloat-pad-open)
  (g-signal-connect app "shutdown" #'bloat-pad-shutdown))

(defun bloat-pad-new ()
  (g-set-application-name "Bloatpad")
  (format t "~A~%" (g-get-application-name))
  (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-get-default))
        t)
  (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
        t)
  (make-instance 'bloat-pad
                 :application-id "org.gtk.Test.bloatpad"
                 :flags :handles-open
                 :inactivity-timeout 30000
                 :register-session t))

(defun example-application (&optional (argc 0) (argv (null-pointer)))
  (let (;; Create an instance of the application Bloat Pad
        (bloat-pad (bloat-pad-new)))
    (format t "call G-APPLICATION-RUN.~%")
    ;; Run the application
    (g-application-run bloat-pad argc argv)
    (format t "back from G-APPLICATION-RUN.~%")
    ;; Destroy the application
    (g-object-unref (pointer bloat-pad))))

