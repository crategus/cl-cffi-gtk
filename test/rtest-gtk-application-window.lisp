(def-suite gtk-application-window :in gtk-suite)
(in-suite gtk-application-window)

;;; --- GtkApplicationWindow ---------------------------------------------------

(test gtk-application-window-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkApplicationWindow"))
  (is-false (g-type-is-abstract "GtkApplicationWindow"))
  (is-true  (g-type-is-derived "GtkApplicationWindow"))
  (is-false (g-type-is-fundamental "GtkApplicationWindow"))
  (is-true  (g-type-is-value-type "GtkApplicationWindow"))
  (is-true  (g-type-has-value-table "GtkApplicationWindow"))
  (is-true  (g-type-is-classed "GtkApplicationWindow"))
  (is-true  (g-type-is-instantiatable "GtkApplicationWindow"))
  (is-true  (g-type-is-derivable "GtkApplicationWindow"))
  (is-true  (g-type-is-deep-derivable "GtkApplicationWindow"))
  (is-false (g-type-is-interface "GtkApplicationWindow"))

  ;; Check the registered name
  (is (eq 'gtk-application-window
          (registered-object-type-by-name "GtkApplicationWindow")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkApplicationWindow"))))
    (is (equal (gtype "GtkApplicationWindow") (g-type-from-class class)))
    (is (equal (gtype "GtkApplicationWindow") (g-object-class-type class)))
    (is (equal "GtkApplicationWindow" (g-object-class-name class)))
    (is (equal (gtype "GtkApplicationWindow")
               (g-type-from-class  (g-type-class-peek "GtkApplicationWindow"))))
    (is (equal (gtype "GtkApplicationWindow")
               (g-type-from-class  (g-type-class-peek-static "GtkApplicationWindow"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-application-window)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-application-window (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkApplicationWindow" (gobject-class-g-type-name class)))
    (is (equal "GtkApplicationWindow" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_application_window_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkWindow") (g-type-parent "GtkApplicationWindow")))
  (is (= 7 (g-type-depth "GtkApplicationWindow")))
  (is (equal (gtype "GtkContainer")
             (g-type-next-base "GtkApplicationWindow" "GtkWidget")))
  (is-true  (g-type-is-a "GtkApplicationWindow" "GObject"))
  (is-true  (g-type-is-a "GtkApplicationWindow" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkApplicationWindow" "gboolean"))
  (is-true  (g-type-is-a "GtkApplicationWindow" "GtkWindow"))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkApplicationWindow" query)
    (is (equal (gtype "GtkApplicationWindow")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkApplicationWindow"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 1184
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 64
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkApplicationWindow"))))
             
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GActionGroup" "GActionMap")
             (mapcar #'gtype-name (g-type-interfaces "GtkApplicationWindow"))))

  ;; Check the class properties
  (is (equal '("accept-focus" "app-paintable" "application" "attached-to" "border-width"
 "can-default" "can-focus" "child" "composite-child" "decorated"
 "default-height" "default-width" "deletable" "destroy-with-parent"
 "double-buffered" "events" "expand" "focus-on-click" "focus-on-map"
 "focus-visible" "gravity" "halign" "has-default" "has-focus" "has-resize-grip"
 "has-tooltip" "has-toplevel-focus" "height-request" "hexpand" "hexpand-set"
 "hide-titlebar-when-maximized" "icon" "icon-name" "is-active" "is-focus"
 "is-maximized" "margin" "margin-bottom" "margin-end" "margin-left"
 "margin-right" "margin-start" "margin-top" "mnemonics-visible" "modal" "name"
 "no-show-all" "opacity" "parent" "receives-default" "resizable"
 "resize-grip-visible" "resize-mode" "role" "scale-factor" "screen" "sensitive"
 "show-menubar" "skip-pager-hint" "skip-taskbar-hint" "startup-id" "style"
 "title" "tooltip-markup" "tooltip-text" "transient-for" "type" "type-hint"
 "urgency-hint" "valign" "vexpand" "vexpand-set" "visible" "width-request"
 "window" "window-position")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkApplicationWindow"))
                          #'string-lessp)))

  ;; Check the style properties
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
 "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
 "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
 "separator-width" "text-handle-height" "text-handle-width"
 "visited-link-color" "wide-separators" "window-dragging"
 "decoration-button-layout" "decoration-resize-handle")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkApplicationWindow"))))

  ;; Check the names of the child properties
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkApplicationWindow"))))

  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkApplicationWindow" GTK-APPLICATION-WINDOW
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GActionGroup" "GActionMap"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_application_window_get_type")
                       ((SHOW-MENUBAR GTK-APPLICATION-WINDOW-SHOW-MENUBAR
                         "show-menubar" "gboolean" T T)))
             (get-g-type-definition "GtkApplicationWindow")))
)

;;; ----------------------------------------------------------------------------
;;; Properties and Accessors
;;; ----------------------------------------------------------------------------

;;; --- gtk-application-window-show-menubar ------------------------------------

(test gtk-application-window-show-menubar
  (let ((window (make-instance 'gtk-application-window)))
    ;; Default value is true
    (is-true  (gtk-application-window-show-menubar window))
    ;; Set show-menubar property to nil
    (setf (gtk-application-window-show-menubar window) nil)
    (is-false (gtk-application-window-show-menubar window))
))

;;; ----------------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------------

;;; --- gtk_application_window_new ---------------------------------------------

(test gtk-application-window-new
  (let ((application (make-instance 'gtk-application)))
    (is (eq 'gtk-application (type-of application)))
    ;; Works only for a registered application
;    (is (eq 'gtk-application-window (type-of (gtk-application-window-new application))))
    ;; Create a window with make-instance
    (is (eq 'gtk-application-window (type-of (make-instance 'gtk-application-window))))
))

;;; --- gtk_application_window_get_id ------------------------------------------

(test gtk-application-window-get-id
  (let ((window (make-instance 'gtk-application-window)))
    ;; Zero if the window is not added to a GtkApplication
    (is (= 0 (gtk-application-window-get-id window)))
))

;;; --- gtk_application_window_set_help_overlay --------------------------------
;;; --- gtk_application_window_get_help_overlay --------------------------------

(test gtk-application-window-help-overlay
  (let ((window (make-instance 'gtk-application-window))
        (help-overlay (make-instance 'gtk-shortcuts-window)))
    ;; Default value is nil
    (is-false (gtk-application-window-get-help-overlay window))
    ;; Set a GtkShortcutsWindow
    (gtk-application-window-set-help-overlay window help-overlay)
    ;; Retrieve the GtkShortcutsWindow
    (is (eq 'gtk-shortcuts-window (type-of (gtk-application-window-get-help-overlay window))))
))

