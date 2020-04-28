(in-package #:gtk-widget-factory)

(defvar *application* nil)
(defvar *application-window* nil)
(defvar *toplevel-stack* nil)

(defclass my-text-view (gtk-text-view)
  ((surface :initarg :surface
            :accessor my-text-view-surface))
  (:metaclass gobject-class)
  (:g-type-name . "MyTextView"))

(register-object-type-implementation "MyTextView"            ; name
                                     my-text-view            ; class
                                     "GtkTextView"           ; parent
                                     nil                     ; interfaces
                                     (("value"               ; prop-name
                                       "gboolean"            ; prop-type
                                       my-text-view-value    ; prop-accessor
                                       t                     ; prop-reader
                                       t                     ; prop-writer
                                      )))

;;; ----------------------------------------------------------------------------

(defun activate-change-theme-state (action state)
  (let ((settings (gtk-settings-get-default)))
    (setf (gtk-settings-gtk-application-prefer-dark-theme settings)
          (g-variant-get-boolean state))
    (setf (g-simple-action-state action) state)))

(defun activate-change-transition-state (action state)
  (setf (gtk-stack-transition-type *toplevel-stack*)
        (if (g-variant-get-boolean state)
            :slide-left-right
            :none))
  (setf (g-simple-action-state action) state))


(defun activate-search ())
(defun activate-delete ())


(defun activate-get-busy (action parameter)

  (format t "in activate-get-busy~%")

  (g-application-mark-busy *application*)
  (let ((cursor (gdk-cursor-new-from-name (gtk-widget-display *application-window*) "wait"))
        (window (gtk-widget-window *application-window*)))

    (setf (gdk-window-cursor window) cursor)
    (g-timeout-add 5000
                   (lambda ()
                     (format t "in timeout callback~%")
                     (setf (gtk-widget-sensitive *application-window*) t)
                     (setf (gdk-window-cursor window) nil)
                     (g-application-unmark-busy *application*)
                     +g-source-remove+)))

    (setf (gtk-widget-sensitive *application-window*) nil))

(defun activate-background (action parameter))
(defun activate-open (action parameter))
(defun activate-record (action parameter))
(defun activate-lock (action parameter))

;;; ----------------------------------------------------------------------------

(defun activate-quit (action parameter)
  (format t "in action activate-quit: ~a, ~a~%" action parameter)
  ;; Destroy all windows of the application
  (dolist (window (gtk-application-get-windows *application*))
    (gtk-widget-destroy window)))

(defun activate-about (action parameter)
  (format t "in action activate-about: ~a, ~a~%" action parameter)

  (gtk-show-about-dialog (gtk-application-active-window *application*)
                         :program-name "GTK Widget Factory"
                         :version (format nil "Running against GTK+ ~d.~d.~d"
                                          (gtk-get-major-version)
                                          (gtk-get-minor-version)
                                          (gtk-get-micro-version))
                         :copyright "© 2020 Dieter Kaiser"
                         :license-type :lgpl-2-1
                         :website "http://www.gtk.org"
                         :authors '("Dieter Kaiser")
                         :logo-icon-name "gtk3-widget-factory"
                         :title "About GTK Widget Factory"))

(defun activate-inspector (action parameter)
  (format t "in action activate-inspector: ~a, ~a~%" action parameter)
  (gtk-window-interactive-debugging t))

;;; ----------------------------------------------------------------------------

(defun activate (application)
  (format t "in signal activate for ~a~%" application)
  ;; Load CSS file
  (let ((provider (gtk-css-provider-new)))
    (gtk-css-provider-load-from-path provider
                                     (rel-path "gtk-widget-factory.css"))
    (gtk-style-context-add-provider-for-screen (gdk-screen-get-default)
                                               provider
                                               +gtk-style-provider-priority-application+))

  ;; Load UI file
  (let* ((builder (gtk-builder-new-from-file (rel-path "gtk-widget-factory.ui")))
         (entries (list (list "dark" nil nil "false" #'activate-change-theme-state)
                        (list "transition" nil nil "false" #'activate-change-transition-state)
                        (list "search" #'activate-search nil nil nil)
                        (list "delete" #'activate-delete nil nil nil)
                        (list "busy" #'activate-get-busy nil nil nil)
                        (list "background" #'activate-background nil nil nil)
                        (list "open" #'activate-open nil nil nil)
                        (list "record" #'activate-record nil nil nil)
                        (list "lock" #'activate-lock nil nil nil))))

    (setf *application-window*
          (gtk-builder-get-object builder "window"))

    ;; Connect signal "destroy" to the application window
    (g-signal-connect *application-window* "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        ;; Quit the application
                        (g-application-quit application)))

;  gtk_builder_add_callback_symbol (builder, "on_entry_icon_release", (GCallback)on_entry_icon_release);
;  gtk_builder_add_callback_symbol (builder, "on_scale_button_value_changed", (GCallback)on_scale_button_value_changed);
;  gtk_builder_add_callback_symbol (builder, "on_scale_button_query_tooltip", (GCallback)on_scale_button_query_tooltip);
;  gtk_builder_add_callback_symbol (builder, "on_record_button_toggled", (GCallback)on_record_button_toggled);
;  gtk_builder_add_callback_symbol (builder, "on_page_combo_changed", (GCallback)on_page_combo_changed);
;  gtk_builder_add_callback_symbol (builder, "on_range_from_changed", (GCallback)on_range_from_changed);
;  gtk_builder_add_callback_symbol (builder, "on_range_to_changed", (GCallback)on_range_to_changed);
;  gtk_builder_add_callback_symbol (builder, "osd_frame_button_press", (GCallback)osd_frame_button_press);
;  gtk_builder_add_callback_symbol (builder, "tab_close_cb", (GCallback)tab_close_cb);
;  gtk_builder_add_callback_symbol (builder, "increase_icon_size", (GCallback)increase_icon_size);
;  gtk_builder_add_callback_symbol (builder, "decrease_icon_size", (GCallback)decrease_icon_size);
;  gtk_builder_add_callback_symbol (builder, "reset_icon_size", (GCallback)reset_icon_size);
;  gtk_builder_add_callback_symbol (builder, "scale_format_value", (GCallback)scale_format_value);
;  gtk_builder_add_callback_symbol (builder, "scale_format_value_blank", (GCallback)scale_format_value_blank);
;  gtk_builder_connect_signals (builder, NULL);

      ;; Add the application window to the application
      (gtk-application-add-window application *application-window*)
      ;; Add actions to the action map of the applicatin window
      (g-action-map-add-action-entries *application-window* entries)

;  struct {
;    const gchar *action_and_target;
;    const gchar *accelerators[2];
;  } accels[] = {
;    { "app.about", { "F1", NULL } },
;    { "app.quit", { "<Primary>q", NULL } },
;    { "win.dark", { "<Primary>d", NULL } },
;    { "win.search", { "<Primary>s", NULL } },
;    { "win.delete", { "Delete", NULL } },
;    { "win.background", { "<Primary>b", NULL } },
;    { "win.open", { "<Primary>o", NULL } },
;    { "win.record", { "<Primary>r", NULL } },
;    { "win.lock", { "<Primary>l", NULL } },
;  };

;  for (i = 0; i < G_N_ELEMENTS (accels); i++)
;    gtk_application_set_accels_for_action (GTK_APPLICATION (app), accels[i].action_and_target, accels[i].accelerators);

      (gtk-application-set-accels-for-action application "app.about" '("F1"))
      (gtk-application-set-accels-for-action application "app.quit" '("<Primary>q"))
      (gtk-application-set-accels-for-action application "win.dark" '("<Primary>d"))
      (gtk-application-set-accels-for-action application "win.search" '("<Primary>s"))
      (gtk-application-set-accels-for-action application "win.delete" '("Delete"))
      (gtk-application-set-accels-for-action application "win.background" '("<Primary>b"))
      (gtk-application-set-accels-for-action application "win.open" '("<Primary>o"))
      (gtk-application-set-accels-for-action application "win.record" '("<Primary>r"))
      (gtk-application-set-accels-for-action application "win.lock" '("<Primary>l"))

      ;; Save the toplevel stack in a global variable
      (setf *toplevel-stack*
            (gtk-builder-get-object builder "toplevel_stack"))

      ;; Set text on the statusbar
      (let ((statusbar (gtk-builder-get-object builder "statusbar")))
        (gtk-statusbar-push statusbar 0 "All systems are operating normally.")

      )

;  widget = (GtkWidget *)gtk_builder_get_object (builder, "statusbar");
;  gtk_statusbar_push (GTK_STATUSBAR (widget), 0, "All systems are operating normally.");
;  action = G_ACTION (g_property_action_new ("statusbar", widget, "visible"));
;  g_action_map_add_action (G_ACTION_MAP (window), action);



      ;; Show the application window
      (gtk-widget-show-all *application-window*)))

(defun startup (application)
  (format t "in signal startup for ~a~%" application))

(defun shutdown (application)
  (format t "in signal shutdown for ~a~%" application)
  ;; Quit the main loop
  (leave-gtk-main))

;;; ----------------------------------------------------------------------------

(defun gtk-widget-factory (&optional (argv nil))
  (within-main-loop
    (let ((entries `(("about" ,#'activate-about nil nil nil)
                     ("quit" ,#'activate-quit nil nil nil)
                     ("inspector" ,#'activate-inspector nil nil nil)
                     ;("main" nil "s" "'steak'" nil)
                     ;("wine" nil nil "false" nil)
                     ;("beer" nil nil "false" nil)
                     ;("water" nil nil "true" nil)
                     ;("dessert" nil "s" "'bars'" nil)
                     ;("pay" nil "s" nil nil)
                    )))
      ;; Create the application
      (setf *application*
            (make-instance 'gtk-application
                           :application-id "com.crategus.gtk-widget-factory"
                           :flags :none))

      ;; Set actions für the application
      (g-action-map-add-action-entries *application* entries)
      ;; Set enabled to "false" for action "wine"
;      (setf (g-simple-action-enabled (g-action-map-lookup-action app "wine")) nil)

      ;; Connect signal "activate" to the applicaton
      (g-signal-connect *application* "activate" #'activate)
      ;; Connect signal "startup" to the applicaton
      (g-signal-connect *application* "startup" #'startup)
      ;; Connect signal "shutdown" to the applicaton
      (g-signal-connect *application* "shutdown" #'shutdown)

      ;; Run the application
      (g-application-run *application* argv))))

