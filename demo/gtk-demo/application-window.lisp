;;;; Application window
;;;;
;;;; Demonstrates a typical application window with menubar, toolbar, statusbar.
;;;; This example uses GtkUIManager and GtkActionGroup.

(in-package #:gtk-demo)

(defstruct app
  window
  message
  infobar)

(defvar *app* (make-app))

(defun activate-action (action)
  (let* ((name (gtk-action-name action))
         (type (g-object-type-name action))
         (dialog (gtk-message-dialog-new (app-window *app*)
                                         '(:destroy-with-parent)
                                         :info
                                         :close
                                         "You activated action ~S of type ~S."
                                         name
                                         type)))
    (cond ((string= name "DarkTheme")
           (let ((value (gtk-toggle-action-active action))
                 (settings (gtk-settings-default)))
             (setf (g-object-property settings
                                      "gtk-application-prefer-dark-theme")
                   value)))
          ((string= name "HideTitlebar")
           (let ((value (gtk-toggle-action-active action)))
             (setf (gtk-window-hide-titlebar-when-maximized (app-window *app*))
                   value)))
          (t
           (let ((response (gtk-dialog-run dialog)))
             (gtk-widget-destroy dialog)
             response)))))

(defun activate-radio-action (action current)
  (declare (ignore action))
  (let ((name (gtk-action-name current))
        (type (g-object-type-name current))
        (active (gtk-toggle-action-active current))
        (value (gtk-radio-action-current-value current)))
    (when active
      (setf (gtk-label-text (app-message *app*))
            (format nil
                    "You activated radio action ~S of type ~S.~% ~
                     Current value ~D."
                    name type value))
      (setf (gtk-info-bar-message-type (app-infobar *app*)) value)
      (gtk-widget-show (app-infobar *app*)))))

(defun activate-about (action)
  (declare (ignore action))
  (gtk-show-about-dialog (app-window *app*)
                         :program-name "GTK+ Lisp Code Demos"
                         :version (format nil "Running against GTK+ ~D.~D.~D"
                                              (gtk-major-version)
                                              (gtk-minor-version)
                                              (gtk-micro-version))
                         :copyright "(C) 2014 Dieter Kaiser"
                         :license-type :lgpl-2-1
                         :website "http://www.gtk.org"
                         :comments "Program to demonstrate GTK+ Lisp functions."
                         :logo (gdk-pixbuf-new-from-file (rel-path "gtk-logo-old.png"))
                         :title "About GTK+ Lisp Code Demos"))

(defvar *entries*
        (list
          (list "FileMenu" nil "_File")               ; name, stock id, label
          (list "OpenMenu" nil "_Open")               ; name, stock id, label
          (list "PreferencesMenu" nil "_Preferences") ; name, stock id, label
          (list "ColorMenu" nil "_Color")             ; name, stock id, label
          (list "ShapeMenu" nil "_Shape")             ; name, stock id, label
          (list "HelpMenu" nil "_Help")               ; name, stock id, label
          (list "New" "gtk-new"                       ; name, stock id
                "_New" "<control>N"                   ; label, accelerator
                "Create a new file"                   ; tooltip
                (lambda (action)
                  (activate-action action)))
          (list "File1" nil                           ; name, stock id
                "File1" nil                           ; label, accelerator
                "Open first file"                     ; tooltip
                #'activate-action)
          (list "Save" "gtk-save"                     ; name, stock id
                "_Save" "<control>S"                  ; label, accelerator
                "Save current file"                   ; tooltip
                #'activate-action)
          (list "SaveAs" "gtk-save"                   ; name, stock id
                "Save _As..." nil                     ; label, accelerator
                "Save to a file"                      ; tooltip
                #'activate-action)
          (list "Quit" "gtk-quit"                     ; name, stock id
                "_Quit" "<control>Q"                  ; label, accelerator
                "Quit"                                ; tooltip
                #'activate-action)
          (list "About" nil                           ; name, stock id
                "_About" "<control>A"                 ; label, accelerator
                "About"                               ; tooltip
                #'activate-about)
          (list "Logo" "demo-gtk-logo"                ; name, stock id
                nil nil                               ; label, accelerator
                "GTK+"                                ; tooltip
                #'activate-action)))

(defvar *toggle-entries*
        (list
          (list "Bold" "gtk-bold"                     ; name, stock id
                "_Bold" "<control>B"                  ; label, accelerator
                "Bold"                                ; tooltip
                #'activate-action
                t)                                    ; is_active
          (list "DarkTheme" nil                       ; name, stock id
                "_Prefer Dark Theme" nil              ; label, accelerator
                "Prefer Dark Theme"                   ; tooltip
                #'activate-action
                nil)                                  ; is_active
          (list "HideTitlebar" nil
                "_Hide Titlebar when maximized" nil
                "Hide Titlebar when maximized"
                #'activate-action
                nil)))

(defvar *color-entries*
        (list
          (list "Red" nil                             ; name, stock id
                "_Red" "<control>R"                   ; label, accelerator
                "Blood" 0)                            ; tooltip, value
          (list "Green" nil                           ; name, stock id
                "_Green" "<control>G"                 ; label, accelerator
                "Grass" 1)                            ; tooltip, value
          (list "Blue" nil                            ; name, stock id
                "_Blue" "<control>B"                  ; label, accelerator
                "Sky" 2)))                            ; tooltip, value

(defvar *shape-entries*
        (list
          (list "Square" nil                          ; name, stock id
                "_Square" "<control>S"                ; label, accelerator
                "Square" 0)                           ; tooltip, value
          (list "Rectangle" nil                       ; name, stock id
                "_Rectangle" "<control>R"             ; label, accelerator
                "Rectangle" 1)                        ; tooltip, value
          (list "Oval" nil                            ; name, stock id
                "_Oval" "<control>O"                  ; label, accelerator
                "Egg" 2)))                            ; tooltip, value

(defvar *ui-info*
"<ui>
   <menubar name='MenuBar'>
     <menu action='FileMenu'>
       <menuitem action='New'/>
       <menuitem action='Open'/>
       <menuitem action='Save'/>
       <menuitem action='SaveAs'/>
       <separator/>
       <menuitem action='Quit'/>
     </menu>
     <menu action='PreferencesMenu'>
       <menuitem action='DarkTheme'/>
       <menuitem action='HideTitlebar'/>
       <menu action='ColorMenu'>
        <menuitem action='Red'/>
        <menuitem action='Green'/>
        <menuitem action='Blue'/>
       </menu>
       <menu action='ShapeMenu'>
         <menuitem action='Square'/>
         <menuitem action='Rectangle'/>
         <menuitem action='Oval'/>
       </menu>
       <menuitem action='Bold'/>
     </menu>
     <menu action='HelpMenu'>
       <menuitem action='About'/>
     </menu>
   </menubar>
   <toolbar name='ToolBar'>
     <toolitem action='Open'/>
     <toolitem action='Quit'/>
     <separator action='Sep1'/>
     <toolitem action='Logo'/>
   </toolbar>
 </ui>")

(defun update-statusbar (buffer statusbar)
  (let* ((count (gtk-text-buffer-char-count buffer))
         (iter (gtk-text-buffer-iter-at-mark
                   buffer
                   (gtk-text-buffer-get-insert buffer)))
         (row (gtk-text-iter-line iter))
         (col (gtk-text-iter-line-offset iter))
         (msg (format nil "Row: ~A Col: ~A | Chars: ~A" row col count)))
    (gtk-statusbar-pop statusbar 0)
    (gtk-statusbar-push statusbar 0 msg)))

(defun register-stock-icons ()
  (let* ((factory (make-instance 'gtk-icon-factory))
         (pixbuf (gdk-pixbuf-new-from-file (rel-path "gtk-logo-old.png")))
         (icon-set (gtk-icon-set-new-from-pixbuf pixbuf)))
    (gtk-icon-factory-add-default factory)
    (gtk-icon-factory-add factory "demo-gtk-logo" icon-set)))

(defun demo-application-window ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Demo Application Window"
                                   :icon-name "document-open"
                                   :default-width 300
                                   :default-height 250))
           (table (make-instance 'gtk-grid))
           (action-group (make-instance 'gtk-action-group
                                        :name "AppWindowActions"))
           (ui-info (make-instance 'gtk-ui-manager))
           (scrolled (make-instance 'gtk-scrolled-window
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :automatic
                                    :shadow-type :in
                                    :halign :fill
                                    :valign :fill
                                    :hexpand t
                                    :vexpand t))
           (contents (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-buffer contents))
           (statusbar (make-instance 'gtk-statusbar
                                     :halign :fill))
           (infobar (make-instance 'gtk-info-bar
                                   :no-show-all t
                                   :halign :fill))
           (message (make-instance 'gtk-label)))
      (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-default))
            nil)
      (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-default))
            nil)
      (register-stock-icons)
      ;; Store global widgets
      (setf (app-window  *app*) window
            (app-message *app*) message
            (app-infobar *app*) infobar)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create the actions
      (let ((open-action (make-instance 'gtk-action
                                        :name "Open"
                                        :label "_Open"
                                        :tooltip "Open a file"
                                        :stock-id "gtk-open")))
        (gtk-action-group-add-action action-group open-action))
      (gtk-action-group-add-actions action-group *entries*)
      (gtk-action-group-add-toggle-actions action-group *toggle-entries*)
      (gtk-action-group-add-radio-actions action-group
                                          *color-entries*
                                          1
                                          #'activate-radio-action)
      (gtk-action-group-add-radio-actions action-group
                                          *shape-entries*
                                          0
                                          #'activate-radio-action)

      (gtk-ui-manager-insert-action-group ui-info action-group 0)

      (gtk-window-add-accel-group window
        (gtk-ui-manager-get-accel-group ui-info))

      (let ((ui-id (gtk-ui-manager-add-ui-from-string ui-info *ui-info*)))
        (when (= 0 ui-id)
          (error "building menus failed")))

      (let ((bar (gtk-ui-manager-get-widget ui-info "/MenuBar")))
        (gtk-widget-show bar)
        (setf (gtk-widget-halign bar) :fill)
        (gtk-grid-attach table bar 0 0 1 1))

      (let ((bar (gtk-ui-manager-get-widget ui-info "/ToolBar")))
        (gtk-widget-show bar)
        (setf (gtk-widget-halign bar) :fill)
        (gtk-grid-attach table bar 0 1 1 1))

      ;; Add infobar
      (gtk-widget-show message)
      (gtk-box-pack-start (gtk-info-bar-content-area infobar) message)
      (gtk-info-bar-add-button infobar "gtk-ok" -5)
      (g-signal-connect infobar "response"
                        (lambda (infobar response-id)
                          (declare (ignore response-id))
                          (gtk-widget-hide infobar)))
      (gtk-grid-attach table infobar 0 2 1 1)

      ;; Add text view
      (gtk-grid-attach table scrolled 0 3 1 1)
      (gtk-widget-grab-focus contents)
      (gtk-container-add scrolled contents)

      ;; Add statusbar
      (gtk-grid-attach table statusbar 0 4 1 1)

      ;; Show text widget info in the statusbar
      (g-signal-connect buffer "changed"
                               (lambda (buffer)
                                 (update-statusbar buffer statusbar)))

      (g-signal-connect buffer "mark-set"
                               (lambda (buffer location mark)
                                 (declare (ignore location mark))
                                 (update-statusbar buffer statusbar)))

      (update-statusbar buffer statusbar)

      ;; Add the Grid to the window
      (gtk-container-add window table)
      ;; Show the window.
      (gtk-widget-show-all window))))

