;;;; Application Bloatpad (2021-9-8)

(in-package #:gtk-application)

(defparameter *menu*
"<interface>
  <menu id='app-menu'>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_New Window</attribute>
        <attribute name='action'>app.new</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
        <attribute name='hidden-when'>macos-menubar</attribute>
        <attribute name='action'>app.about</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name='label' translatable='yes'>_Quit</attribute>
        <attribute name='hidden-when'>macos-menubar</attribute>
        <attribute name='action'>app.quit</attribute>
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
        </item>
        <item>
          <attribute name='label' translatable='yes'>_Paste</attribute>
          <attribute name='action'>win.paste</attribute>
        </item>
      </section>
      <section>
        <item>
          <!-- action should never be missing (so always shown) -->
          <attribute name='label'>Clear (always shown)</attribute>
          <attribute name='action'>win.clear</attribute>
          <attribute name='hidden-when'>action-missing</attribute>
        </item>
        <item>
          <attribute name='label'>Clear (hidden when no text)</attribute>
          <attribute name='hidden-when'>action-disabled</attribute>
          <attribute name='action'>win.clear</attribute>
        </item>
        <item>
          <attribute name='label'>Spell check (does nothing, hides)</attribute>
          <attribute name='hidden-when'>action-missing</attribute>
          <attribute name='action'>win.spell-check</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>Accelerators...</attribute>
          <attribute name='action'>app.edit-accels</attribute>
        </item>
      </section>
      <section>
        <item>
          <attribute name='label' translatable='yes'>Shortcuts...</attribute>
          <attribute name='action'>win.show-help-overlay</attribute>
        </item>
      </section>
    </submenu>
    <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
        <item>
          <attribute name='label' translatable='yes'>_Fullscreen</attribute>
          <attribute name='action'>win.fullscreen</attribute>
        </item>
        <item>
          <attribute name='label' translatable='yes'>_Look Busy</attribute>
          <attribute name='action'>win.busy</attribute>
        </item>
      </section>
    </submenu>
    <submenu id='icon-menu'>
      <attribute name='label' translatable='yes'>_Icons</attribute>
    </submenu>
    <submenu id='time-menu'>
      <attribute name='label' translatable='yes'>Time</attribute>
      <attribute name='submenu-action'>app.time-active</attribute>
    </submenu>
  </menu>
</interface>")


(defclass bloat-pad (gtk-application)
  ((quit-inhibit :initarg :quit-inhibit
                 :accessor bloat-pad-quit-inhibit)
   (time :initarg :time
         :initform nil
         :accessor bloat-pad-time)
   (timeout :initarg :timeout
            :initform 0
            :accessor bloat-pad-timeout))
  (:g-type-name . "BloatPad")
  (:metaclass gobject-class))

(register-object-type-implementation "BloatPad"              ; name
                                     bloat-pad               ; class
                                     "GtkApplication"        ; parent
                                     nil                     ; interfaces
                                     nil)                    ; properties

;                                     (("quit-inhibit"
;                                       "guint"
;                                       bloat-pad-quit-inhibit
;                                       t
;                                       t)
;                                      ("time"                ; prop-name
;                                       "GMenu"               ; prop-type
;                                       bloat-pad-time        ; prop-accessor
;                                       t                     ; prop-reader
;                                       t)                    ; prop-writer
;                                      ("timeout"
;                                       "guint"
;                                       bloat-pad-timeout
;                                       t
;                                       t)))

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
                           (g-object-data window "bloatpad-text"))))
               (gtk-text-buffer-copy-clipboard
                                  (gtk-text-view-buffer view)
                                  (gtk-widget-clipboard view "CLIPBOARD"))))))

      ;; Add action "paste" to the application window
      (let ((action (g-simple-action-new "paste" nil)))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore action parameter))
             (let ((view (gobject::get-g-object-for-pointer
                           (g-object-data window "bloatpad-text"))))
               (gtk-text-buffer-paste-clipboard
                                       (gtk-text-view-buffer view)
                                       (gtk-widget-clipboard view "CLIPBOARD")
                                       :editable t)))))

      ;; Add action "fullscreen" to the application window
      (let ((action (g-simple-action-new-stateful
                                               "fullscreen"
                                               nil
                                               (g-variant-new-boolean nil))))
        (g-action-map-add-action window action)
        (g-signal-connect action "activate"
           (lambda (action parameter)
             (declare (ignore parameter))
             (let* ((state (g-action-state action))
                    (value (g-variant-boolean state)))
               (g-action-change-state action
                                      (g-variant-new-boolean (not value))))))
        (g-signal-connect action "change-state"
           (lambda (action parameter)
             (if (g-variant-boolean parameter)
                 (gtk-window-fullscreen window)
                 (gtk-window-unfullscreen window))
             (setf (g-action-state action) parameter))))

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
                           (g-object-data window "bloatpad-text")))
                   (str (g-variant-string parameter)))
               (cond ((equal str "left")
                      (setf (gtk-text-view-justification view) :left))
                     ((equal str "center")
                      (setf (gtk-text-view-justification view) :center))
                     (t
                      (setf (gtk-text-view-justification view) :right)))
               (setf (g-action-state action) parameter)))))

      ;; Left justify toggle tool button for the toolbar
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-justify-left")))
        (gtk-actionable-set-detailed-action-name button "win.justify::left")
        (gtk-container-add toolbar button))

      ;; Center justify toggle tool button for the toolbar
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-justify-center")))
        (gtk-actionable-set-detailed-action-name button "win.justify::center")
        (gtk-container-add toolbar button))

      ;; Right justify toggle tool button for the toolbar
      (let ((button (make-instance 'gtk-toggle-tool-button
                                   :icon-name "format-justify-right")))
        (gtk-actionable-set-detailed-action-name button "win.justify::right")
        (gtk-container-add toolbar button))

      ;; Invisible separator which shift the next tool item to the right
      (let ((button (make-instance 'gtk-separator-tool-item
                                   :draw nil)))
        (setf (gtk-tool-item-expand button) t)
        (gtk-container-add toolbar button))

      ;; A label and a switch on the right of the toolbar
      (let ((button (make-instance 'gtk-tool-item))
            (box (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 6))
            (label (make-instance 'gtk-label
                                  :label "Fullscreen:"))
            (switch (make-instance 'gtk-switch)))
        (setf (gtk-actionable-action-name switch) "win.fullscreen")
        (gtk-container-add box label)
        (gtk-container-add box switch)
        (gtk-container-add button box)
        (gtk-container-add toolbar button))

      ;; Place the toolbar in the grid
      (gtk-grid-attach grid toolbar 0 0 1 1)


      (let ((scrolled (make-instance 'gtk-scrolled-window
                                     :hexpand t
                                     :vexpand t))
            (view (make-instance 'gtk-text-view)))
        (setf (g-object-data window "bloatpad-text") (pointer view))
        (gtk-container-add scrolled view)
        (gtk-grid-attach grid scrolled 0 1 1 1))

      (gtk-container-add window grid)
      (gtk-widget-show-all window)))

(defun bloat-pad-activate (application)
  ;; Create a new application window
  (new-window application nil))

(defun create-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk-about-dialog
                               :program-name "Simple Application"
                               :version "0.9"
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

(defun bloat-pad-startup (application)

  ;; Add more accelerators
  (let ((accels '(("app.new" "<Primary>n")
                  ("app.quit" "<Primary>q")
                  ("win.copy" "<Primary>c")
                  ("win.paste" "<Primary>p")
                  ("win.justify::left" "<Primary>l")
                  ("win.justify::center" "<Primary>m")
                  ("win.justify::right" "<Primary>r"))))
    (loop for (name accel) in accels
          do (setf (gtk-application-accels-for-action application name) accel)))

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
         (dolist (window (gtk-application-windows application))
           (gtk-widget-destroy window))
         ;; Quit the main loop
         (leave-gtk-main)
         ;; Quit the application
         (g-application-quit application)))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action))

  ;; Add action "edit-accels" to the application
  (let ((action (g-simple-action-new "edit-accels" nil)))

    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Implement the functionality
    ))
    (g-action-map-add-action application action))

  ;; Add action "time-active" to the application
  (let ((action (g-simple-action-new-stateful "time-active"
                                              nil
                                              (g-variant-new-boolean nil))))

    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Implement the functionality
    ))
    (g-action-map-add-action application action))

  ;; Add action "clear-all" to the application
  (let ((action (g-simple-action-new "clear-all" nil)))

    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Implement the functionality
    ))
    (g-action-map-add-action application action))

  ;; Intitialize the application menu and the menubar
  (let ((builder (make-instance 'gtk-builder)))
    ;; Read the menus from a string
    (gtk-builder-add-from-string builder *menu*)
    ;; Set the application menu
    (setf (gtk-application-app-menu application)
          (gtk-builder-object builder "app-menu"))
    ;; Set the menubar
    (setf (gtk-application-menubar application)
          (gtk-builder-object builder "menubar"))

    ;; Populate the icon menu with icons

    ;; The C example uses the function gtk-application-menu-by-id to get
    ;; the menu from automatic loaded resources. This does not work in the
    ;; Lisp example. TODO: Implement automatic loading of resources.
    (let ((menu (gtk-builder-object builder "icon-menu")))

;      (let* ((file (g-file-new-for-path (sys-path "color-select.png")))
;             (icon (g-file-icon-new file))
;             (item (g-menu-item-new "File Icon" nil)))

;        (format t "file : ~a~%" file)
;        (format t "icon : ~a~%" icon)

;        (g-menu-item-set-icon item icon)
;        (g-menu-append-item menu item))

;      (let ((icon (gdk-pixbuf-new-from-resource
;                            "/com/crategus/application/preferences-system.png"))
;            (item (g-menu-item-new "Pixbuf" nil)))
;        (g-menu-item-set-icon item icon)
;        (g-menu-append-item menu item))

      (let ((icon (g-themed-icon-new "edit-find"))
            (item (g-menu-item-new "Themed Icon" nil)))
        (g-menu-item-set-icon item icon)
        (g-menu-append-item menu item))

      (let ((icon (g-themed-icon-new "weather-severe-alert-symbolic"))
            (item (g-menu-item-new "Symbolic Icon" nil)))
        (g-menu-item-set-icon item icon)
        (g-menu-append-item menu item))

    )

))

(defun bloat-pad-open (application)
  (declare (ignore application))
  ;; Executed when the application is opened
  +gdk-event-propagate+)

(defun bloat-pad-shutdown (application)
  ;; Executed when the application is shut down
  (unless (= 0 (bloat-pad-timeout application))
    (g-source-remove (bloat-pad-timeout application))
    (setf (bloat-pad-timeout application) 0))
  +gdk-event-propagate+)

(defmethod initialize-instance :after
    ((app bloat-pad) &key &allow-other-keys)
  (g-signal-connect app "activate" #'bloat-pad-activate)
  (g-signal-connect app "startup" #'bloat-pad-startup)
  (g-signal-connect app "open" #'bloat-pad-open)
  (g-signal-connect app "shutdown" #'bloat-pad-shutdown))

(defun bloatpad-new ()
  (unless (string= "Bloatpad" (g-application-name))
      (setf (g-application-name) "Bloatpad"))
  (make-instance 'bloat-pad
                 :application-id "com.crategus.bloatpad"
                 :flags :handles-open
                 :inactivity-timeout 30000
                 :register-session t))

(defun bloatpad (&optional (argv nil))
  (within-main-loop
    (let (;; Create an instance of the application Bloat Pad
          (bloatpad (bloatpad-new))
          ;; Load resources
          (resource (g-resource-load (sys-path "bloatpad.gresource"))))
      ;; Register the resources
      (g-resources-register resource)
      ;; Set an accelerator to toggle fullscreen
      (setf (gtk-application-accels-for-action bloatpad "win.fullscreen") "F11")
      ;; Run the application
      (g-application-run bloatpad argv)
      ;; Unregister the resources
      (g-resources-unregister resource)))
  (join-gtk-main))
