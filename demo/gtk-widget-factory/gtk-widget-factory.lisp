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

;; Control the pulse mode of "progressbar3" and "entry1"

(let ((*pulse-time* 250)
      (*pulse-entry-mode* 0))

  ;; Callback function for the destroy notify handler
  (defcallback pulse-remove :void ((data :pointer))
    (g-source-remove (pointer-address data)))

  ;; Function called by the timeout handler
  (defun pulse-it (widget)
    ;; Pulse the widget which is an entry or a progress bar
    (if (eq 'gtk-entry (type-of widget))
        (gtk-entry-progress-pulse widget)
        (gtk-progress-bar-pulse widget))
    ;; Set a timeout handler and store the handler on the property list
    (g-object-set-data-full widget
                            "pulse-id"
                            (make-pointer (g-timeout-add *pulse-time*
                                                         (lambda ()
                                                           (pulse-it widget))))
                            (callback pulse-remove))
    ;; Remove the source
    +g-source-remove+)

  (defun pulse-update (adjustment widget)
    (let ((value (gtk-adjustment-value adjustment))
          (pulse-id (g-object-data widget "pulse-id")))
      (setf *pulse-time* (truncate (+ 50 (* 4 value))))
      (if (= 100 value)
          (setf (g-object-data widget "pulse-id") (null-pointer))
          (when (and (null-pointer-p pulse-id)
                     (or (eq 'gtk-progress-bar (type-of widget))
                         (and (eq 'gtk-entry (type-of widget))
                              (= 3 (mod *pulse-entry-mode* 3)))))
            (g-object-set-data-full widget
                                    "pulse-id"
                                    (make-pointer (g-timeout-add *pulse-time*
                                                                 (lambda ()
                                                                   (pulse-it widget))))
                                    (callback pulse-remove))))))

  (defun on-entry-icon-release (entry icon-pos event)
    (declare (ignore event))
    (when (eq :secondary icon-pos)
      (setf *pulse-entry-mode* (1+ *pulse-entry-mode*))
      (cond ((= 0 (mod *pulse-entry-mode* 3))
             (setf (g-object-data entry "pulse-id") (null-pointer))
             (setf (gtk-entry-progress-fraction entry) 0.0d0))
            ((= 1 (mod *pulse-entry-mode* 3))
             (setf (gtk-entry-progress-fraction entry) 0.25d0))
            (t
             (when (< (- *pulse-time* 50) 400)
               (setf (gtk-entry-progress-pulse-step entry) 0.1d0)
               (pulse-it entry))))))
)

;;; ----------------------------------------------------------------------------

(defun activate-change-theme-state (action state)
  (let ((settings (gtk-settings-default)))
    (setf (gtk-settings-gtk-application-prefer-dark-theme settings)
          (g-variant-boolean state))
    (setf (g-action-state action) state)))

(defun activate-change-transition-state (action state)
  (setf (gtk-stack-transition-type *toplevel-stack*)
        (if (g-variant-boolean state)
            :slide-left-right
            :none))
  (setf (g-action-state action) state))


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

(defun activate-background (action parameter) (declare (ignore action parameter)))
(defun activate-open (action parameter) (declare (ignore action parameter)))
(defun activate-record (action parameter) (declare (ignore action parameter)))
(defun activate-lock (action parameter) (declare (ignore action parameter)))

;;; ----------------------------------------------------------------------------

(defun activate-quit (action parameter)
  (format t "in action activate-quit: ~a, ~a~%" action parameter)
  ;; Destroy all windows of the application
  (dolist (window (gtk-application-windows *application*))
    (gtk-widget-destroy window)))

(defun activate-about (action parameter)
  (format t "in action activate-about: ~a, ~a~%" action parameter)

  (gtk-show-about-dialog (gtk-application-active-window *application*)
                         :program-name "GTK Widget Factory"
                         :version (format nil "Running against GTK+ ~d.~d.~d"
                                          (gtk-major-version)
                                          (gtk-minor-version)
                                          (gtk-micro-version))
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
    (gtk-style-context-add-provider-for-screen (gdk-screen-default)
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
          (gtk-builder-object builder "window"))

    ;; Connect signal "destroy" to the application window
    (g-signal-connect *application-window* "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        ;; Quit the application
                        (g-application-quit application)))

    ;; Connect signal "icon-release" to "entry1"
    (g-signal-connect (gtk-builder-object builder "entry1")
                      "icon-release"
                      #'on-entry-icon-release)

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

      ;; Set accels for some actions
      (setf (gtk-application-accels-for-action application "app.about") '("F1"))
      (setf (gtk-application-accels-for-action application "app.quit") '("<Primary>q"))
      (setf (gtk-application-accels-for-action application "win.dark") '("<Primary>d"))
      (setf (gtk-application-accels-for-action application "win.search") '("<Primary>s"))
      (setf (gtk-application-accels-for-action application "win.delete") '("Delete"))
      (setf (gtk-application-accels-for-action application "win.background") '("<Primary>b"))
      (setf (gtk-application-accels-for-action application "win.open") '("<Primary>o"))
      (setf (gtk-application-accels-for-action application "win.record") '("<Primary>r"))
      (setf (gtk-application-accels-for-action application "win.lock") '("<Primary>l"))

      ;; Save the toplevel stack in a global variable
      (setf *toplevel-stack*
            (gtk-builder-object builder "toplevel_stack"))

      ;; Set text and an action on the statusbar
      (let ((statusbar (gtk-builder-object builder "statusbar")))
        (gtk-statusbar-push statusbar 0 "All systems are operating normally.")
        (g-action-map-add-action *application-window*
                                 (g-property-action-new "statusbar"
                                                        statusbar
                                                        "visible")))
      ;; Set an action on the toolbar
      (let ((toolbar (gtk-builder-object builder "toolbar")))
        (g-action-map-add-action *application-window*
                                 (g-property-action-new "toolbar"
                                                        toolbar
                                                        "visible")))

      ;; Connect entry1 and progressbar3 to adjustment1
      (let ((adjustment (gtk-builder-object builder "adjustment1"))
            (progressbar (gtk-builder-object builder "progressbar3"))
            (entry (gtk-builder-object builder "entry1")))
        (g-signal-connect adjustment "value-changed"
                          (lambda (adj)
                            (pulse-update adj progressbar)))
        (pulse-update adjustment progressbar)
        (g-signal-connect adjustment "value-changed"
                          (lambda (adj)
                            (pulse-update adj entry)))
        (pulse-update adjustment entry))


      ;; Show the application window
      (gtk-widget-show-all *application-window*)))

(defun startup (application)
  (format t "in signal startup for ~a~%" application))

(defun shutdown (application)
  (format t "in signal shutdown for ~a~%" application)
  ;; Quit the main loop
  (leave-gtk-main))

;;; ----------------------------------------------------------------------------

(defun gtk-widget-factory (&rest argv)
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
;      (setf (g-action-enabled (g-action-map-lookup-action app "wine")) nil)

      ;; Connect signal "activate" to the applicaton
      (g-signal-connect *application* "activate" #'activate)
      ;; Connect signal "startup" to the applicaton
      (g-signal-connect *application* "startup" #'startup)
      ;; Connect signal "shutdown" to the applicaton
      (g-signal-connect *application* "shutdown" #'shutdown)

      ;; Run the application
      (g-application-run *application* argv))))

