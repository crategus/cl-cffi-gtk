;;;; Application Bloatpad - 2021-10-24

(in-package #:gtk-application)

(defparameter *menu*
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
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
      <attribute name='submenu-action'>win.time-active</attribute>
    </submenu>
  </menu>
</interface>")

(defclass bloatpad (gtk-application)
  ((inhibit :initarg :inhibit
            :initform 0
            :accessor bloatpad-inhibit)
   (time :initarg :time
         :initform nil
         :accessor bloatpad-time)
   (timeout :initarg :timeout
            :initform 0
            :accessor bloatpad-timeout))
  (:g-type-name . "BloatPad")
  (:metaclass gobject-class))

(register-object-type-implementation "BloatPad"              ; name
                                     bloatpad                ; class
                                     "GtkApplication"        ; parent
                                     nil                     ; interfaces
                                     nil)                    ; properties

(defun text-buffer-changed-cb (window buffer)
  (let ((application (gtk-window-application window))
        (count (gtk-text-buffer-char-count buffer)))
    (if (> count 0)
        (when (= 0 (bloatpad-inhibit application))
          (format t "inhibit application~%")
          (setf (bloatpad-inhibit application)
                (gtk-application-inhibit
                                    application
                                    (gtk-application-active-window application)
                                    :logout
                                    "Bloatpad cannot save, you cannot logout")))
        (when (> (bloatpad-inhibit application) 0)
          (format t "uninhibit application~%")
          (gtk-application-uninhibit application (bloatpad-inhibit application))
          (setf (bloatpad-inhibit application) 0)))
    (setf (g-action-enabled (g-action-map-lookup-action window "clear"))
          (> count 0))
    (if (> count 0)
        (let ((action (g-simple-action-new "spell-check" nil)))
          (g-action-map-add-action window action))
        (g-action-map-remove-action window "spell-check"))
    (let ((line-count-old (pointer-address (g-object-data buffer "line-count")))
          (line-count (gtk-text-buffer-line-count buffer)))
      (setf (g-object-data buffer "line-count") (make-pointer line-count))
      (when (and (< line-count-old 3) (= 3 line-count))
        (format t "Create and send a notification~%")
        (let ((notification (g-notification-new "Three lines of text")))
          (g-notification-set-body notification "Keep up the good work.")
          (g-notification-add-button notification "Start over" "app.clear-all")
          (g-application-send-notification application
                                           "three-lines"
                                           notification))))))

(defun new-bloatpad-window (application filename)
  (let ((window (make-instance 'gtk-application-window
                               :application application
                               :title "Bloatpad"
                               :default-width 640
                               :default-height 480))
        (grid (make-instance 'gtk-grid))
        (toolbar (make-instance 'gtk-toolbar))
        (scrolled (make-instance 'gtk-scrolled-window
                                   :hexpand t
                                   :vexpand t))
        (view (make-instance 'gtk-text-view
                             :monospace t)))

    ;; Add action "copy" to the application window
    (let ((action (g-simple-action-new "copy" nil)))
      (g-action-map-add-action window action)
      (g-signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore action parameter))
            (gtk-text-buffer-copy-clipboard
                                          (gtk-text-view-buffer view)
                                          (gtk-widget-clipboard view
                                                                "CLIPBOARD")))))
    ;; Add action "paste" to the application window
    (let ((action (g-simple-action-new "paste" nil)))
      (g-action-map-add-action window action)
      (g-signal-connect action "activate"
          (lambda (action parameter)
            (declare (ignore action parameter))
            (gtk-text-buffer-paste-clipboard (gtk-text-view-buffer view)
                                             (gtk-widget-clipboard view
                                                                   "CLIPBOARD")
                                             :editable t))))

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
    ;; Add action "busy" to the application window
    (let ((action (g-simple-action-new-stateful
                                             "busy"
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
               (let* ((display (gtk-widget-display window))
                      (cursor (gdk-cursor-new-from-name display "wait"))
                      (window (gtk-widget-window window)))
                 (setf (gdk-window-cursor window) cursor)
                 (g-application-mark-busy application))
               (let ((window (gtk-widget-window window)))
                 (setf (gdk-window-cursor window) nil)
                 (g-application-unmark-busy application)))
           (setf (g-action-state action) parameter))))
    ;; Add action "justify" to the application window
    (let ((action (g-simple-action-new-stateful "justify"
                                                (g-variant-type-new "s")
                                                (g-variant-new-string "left"))))
      (g-action-map-add-action window action)
      (g-signal-connect action "activate"
         (lambda (action parameter)
           (g-action-change-state action parameter)))
      (g-signal-connect action "change-state"
         (lambda (action parameter)
           (let ((str (g-variant-string parameter)))
             (cond ((equal str "left")
                    (setf (gtk-text-view-justification view) :left))
                   ((equal str "center")
                    (setf (gtk-text-view-justification view) :center))
                   (t
                    (setf (gtk-text-view-justification view) :right)))
             (setf (g-action-state action) parameter)))))

    ;; Add action "clear" to the application window
    (let ((action (g-simple-action-new "clear" nil)))
      (g-action-map-add-action window action)
      (g-signal-connect action "activate"
         (lambda (action parameter)
           (declare (ignore action parameter))
             (setf (gtk-text-buffer-text (gtk-text-view-buffer view)) ""))))

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

    ;; Load file into the buffer
    (when filename
      (let ((buffer (gtk-text-view-buffer view)))
        (format t "    buffer : ~a~%" buffer)
        (load-file-into-buffer buffer (sys-path filename))))
    ;; Connect a signal handler for the text buffer
    (g-signal-connect (gtk-text-view-buffer view) "changed"
        (lambda (buffer)
          (text-buffer-changed-cb window buffer)))
    (text-buffer-changed-cb window (gtk-text-view-buffer view))
    (gtk-container-add scrolled view)
    (gtk-grid-attach grid scrolled 0 1 1 1)

    (gtk-container-add window grid)
    (gtk-widget-show-all window)))

(defun action-about (action parameter)
  (declare (ignore action parameter))
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

(defun action-edit-accels (application action parameter)
  (format t "in action EDIT-ACCELS~%")
  (format t "  application : ~a~%" application)
  (format t "       action : ~a~%" action)
  (format t "    parameter : ~a~%" parameter)
  (let ((dialog (make-instance 'gtk-dialog
                               :application application))
        (combo (make-instance 'gtk-combo-box-text))
        (entry (make-instance 'gtk-entry))
        (actions (gtk-application-list-action-descriptions application)))
    (format t "      actions : ~a~%" actions)
    (dolist (action actions)
      (gtk-combo-box-text-append combo action action))
    (g-signal-connect combo "changed"
        (lambda (combo)
          (format t "in signal CHANGED~%")
          (format t "   combo : ~a~%" combo)
          (let ((action (gtk-combo-box-active-id combo)))
            (format t "  action : ~a~%" action)
            (when action
              (let ((accels (gtk-application-accels-for-action application
                                                               action)))
                (format t "  accels : ~a~%" accels)
                (setf (gtk-entry-text entry)
                      (string-right-trim ","
                                         (format nil "~{~a,~}" accels))))))))
    ;; Add buttons to the dialog
    (gtk-dialog-add-button dialog "Close" :close)
    (gtk-dialog-add-button dialog "Set" :apply)
    (g-signal-connect dialog "response"
        (lambda (dialog id)
          (format t "in signal RESPONSE~%")
          (format t "  dialog : ~a~%" dialog)
          (format t "      id : ~a~%" id)

          (if (= (foreign-enum-value 'gtk-response-type :close) id)
              (gtk-widget-destroy dialog)
              (let ((action (gtk-combo-box-active-id combo)))
                (when action
                  (let ((accels (gtk-entry-text entry)))
                    (setf (gtk-application-accels-for-action application action)
                          (split-sequence:split-sequence ","
                                                         accels
                                                         :test #'string=))))))))
    ;; Pack and show the widgets
    (gtk-container-add (gtk-dialog-content-area dialog) combo)
    (gtk-container-add (gtk-dialog-content-area dialog) entry)
    (gtk-widget-show-all dialog)))

(defun update-time (application)
  (g-menu-remove-all (bloatpad-time application))
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (let ((time (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
      (g-menu-append (bloatpad-time application) time nil)))
  +g-source-continue+)

(defun action-time-active (application action state)
  (format t "in action TIME-ACTIVE~%")
  (format t "  application : ~a~%" application)
  (format t "       action : ~a~%" action)
  (format t "        state : ~a~%" state)
  (if (g-variant-boolean state)
      (when (= 0 (bloatpad-timeout application))
        (setf (bloatpad-timeout application)
              (g-timeout-add 1000
                             (lambda ()
                               (update-time application))))
        (update-time application))
      (when (> 0 (bloatpad-timeout application))
        (g-source-remove (bloatpad-timeout application))
        (setf (bloatpad-timeout application) 0))))

(defun bloatpad-startup (application)
  (format t "Application in STARTUP.~%")
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
         (g-application-activate application)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))
  ;; Add action "about" to the application
  (let ((action (g-simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (action-about action parameter)))
    ;; Add the action to the action map of the application
    (g-action-map-add-action application action))
  ;; Add action "quit" to the application
  (let ((action (g-simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application. When the last window is
         ;; destroyed, the application is shutdown.
         ;; The C example calls the g_application_quit function. In the Lisp
         ;; library this will not close the open windows and the windows
         ;; do not react.
         (dolist (window (gtk-application-windows application))
           (gtk-widget-destroy window))))
    ;; Add the action to action map of the application
    (g-action-map-add-action application action))
  ;; Add action "edit-accels" to the application
  (let ((action (g-simple-action-new "edit-accels" nil)))
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (action-edit-accels application action parameter)))
    (g-action-map-add-action application action))
  ;; TODO: No menu item to activate this action available
  ;; Add action "clear-all" to the application
  (let ((action (g-simple-action-new "clear-all" nil)))
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (format t "in action CLEAR-ALL~%")
         (let ((windows (gtk-application-windows application)))
           (dolist (window windows)
             (g-action-group-activate-action window "clear" nil)))))
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
        (g-menu-append-item menu item)))
    (let ((menu (gtk-builder-object builder "time-menu")))
      (setf (bloatpad-time application) menu)
      (format t "  bloatpad-time : ~a~%" (bloatpad-time application))))
  ;; Add action "time-active" to the application
  (let ((action (g-simple-action-new-stateful "time-active"
                                              nil
                                              (g-variant-new-boolean nil))))
    (g-signal-connect action "activate"
       (lambda (action parameter)
         (format t "in action activate for TIME-ACTIVE~%")
         (action-time-active application action parameter)))
    (g-action-map-add-action application action))
  ;; Start the timer for the time menu
  (action-time-active application "time-active" (g-variant-new-boolean t)))

(defun bloatpad-open (application files n-files hint)
  (declare (ignore hint))
  (format t "Application in OPEN.~%")
  ;; Executed when the application is opened
  (dotimes (i n-files)
    (let* ((file (mem-aref files '(g-object g-file) i))
           (filename (g-file-basename file)))
      (format t "  filename : ~a~%" filename)
      (new-bloatpad-window application filename))))

(defun bloatpad-activate (application)
  (format t "Application in ACTIVATE.~%")
  ;; Create a new application window
  (new-bloatpad-window application nil))

(defun bloatpad-shutdown (application)
  ;; Executed when the application is shut down
  (format t "Application in SHUTDOWN.~%")
  (unless (= 0 (bloatpad-timeout application))
    (g-source-remove (bloatpad-timeout application))
    (setf (bloatpad-timeout application) 0)))

(defmethod initialize-instance :after
    ((app bloatpad) &key &allow-other-keys)
  (g-signal-connect app "startup" #'bloatpad-startup)
  (g-signal-connect app "open" #'bloatpad-open)
  (g-signal-connect app "activate" #'bloatpad-activate)
  (g-signal-connect app "shutdown" #'bloatpad-shutdown))

(defun bloatpad-new ()
  (unless (string= "Bloatpad" (g-application-name))
      (setf (g-application-name) "Bloatpad"))
  (make-instance 'bloatpad
                 :application-id "com.crategus.bloatpad"
                 :flags :handles-open
                 :register-session t))

(defun bloatpad (&rest argv)
    (let ((argv (cons "bloatpad" (if argv argv (uiop:command-line-arguments))))
          ;; Create an instance of the application Bloat Pad
          (bloatpad (bloatpad-new))
          ;; Load resources
          (resource (g-resource-load (sys-path "bloatpad.gresource"))))
      (format t "Start BLOATPAD with argv : ~a~%" argv)
      ;; Register the resources
      (g-resources-register resource)
      ;; Set an accelerator to toggle fullscreen
      (setf (gtk-application-accels-for-action bloatpad "win.fullscreen") "F11")
      ;; Run the application
      (g-application-run bloatpad argv)
      (format t "Application back from G-APPLICATION-RUN~%")
      ;; Unregister the resources
      (g-resources-unregister resource)))
