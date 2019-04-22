(def-suite gtk-window :in gtk-suite)
(in-suite gtk-window)

;;; --- GtkWindow --------------------------------------------------------------

(test gtk-window-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkWindow"))
  (is-false (g-type-is-abstract "GtkWindow"))
  (is-true  (g-type-is-derived "GtkWindow"))
  (is-false (g-type-is-fundamental "GtkWindow"))
  (is-true  (g-type-is-value-type "GtkWindow"))
  (is-true  (g-type-has-value-table "GtkWindow"))
  (is-true  (g-type-is-classed "GtkWindow"))
  (is-true  (g-type-is-instantiatable "GtkWindow")) ; Why is this true?
  (is-true  (g-type-is-derivable "GtkWindow"))
  (is-true  (g-type-is-deep-derivable "GtkWindow"))
  (is-false (g-type-is-interface "GtkWindow"))

  ;; Check the registered name
  (is (eq 'gtk-window
          (registered-object-type-by-name "GtkWindow")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkWindow"))))
    (is (equal (gtype "GtkWindow") (g-type-from-class class)))
    (is (equal (gtype "GtkWindow") (g-object-class-type class)))
    (is (equal "GtkWindow" (g-object-class-name class)))
    (is (equal (gtype "GtkWindow")
               (g-type-from-class  (g-type-class-peek "GtkWindow"))))
    (is (equal (gtype "GtkWindow")
               (g-type-from-class  (g-type-class-peek-static "GtkWindow"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-window)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-window (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkWindow" (gobject-class-g-type-name class)))
    (is (equal "GtkWindow" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_window_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkBin") (g-type-parent "GtkWindow")))
  (is (= 6 (g-type-depth "GtkWindow")))
  (is (equal (gtype "GInitiallyUnowned")
             (g-type-next-base "GtkWindow" "GObject")))
  (is-true  (g-type-is-a "GtkWindow" "GObject"))
  (is-true  (g-type-is-a "GtkWindow" "GInitiallyUnowned"))
  (is-false (g-type-is-a "GtkWindow" "gboolean"))
  (is-true (g-type-is-a "GtkWindow" "GtkWindow"))

  ;; Check the children
  (is (equal '("GtkDialog" "GtkAssistant" "GtkOffscreenWindow" "GtkPlug" "GtkShortcutsWindow"
 "GtkApplicationWindow")
             (mapcar #'gtype-name (g-type-children "GtkWindow"))))
             
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkWindow"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkWindow" query)
    (is (equal (gtype "GtkWindow")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkWindow"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 1072
           (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (= 56
           (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the class properties.
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
 "skip-pager-hint" "skip-taskbar-hint" "startup-id" "style" "title"
 "tooltip-markup" "tooltip-text" "transient-for" "type" "type-hint"
 "urgency-hint" "valign" "vexpand" "vexpand-set" "visible" "width-request"
 "window" "window-position")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkWindow"))
                          #'string-lessp)))

  ;; Get the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
 "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
 "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
 "separator-width" "text-handle-height" "text-handle-width"
 "visited-link-color" "wide-separators" "window-dragging"
 "decoration-button-layout" "decoration-resize-handle")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkWindow"))))

  ;; Get the names of the child properties
  ;; to add

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWindow" GTK-WINDOW
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_window_get_type")
                       ((ACCEPT-FOCUS GTK-WINDOW-ACCEPT-FOCUS "accept-focus"
                         "gboolean" T T)
                        (APPLICATION GTK-WINDOW-APPLICATION "application"
                         "GtkApplication" T T)
                        (ATTACHED-TO GTK-WINDOW-ATTACHED-TO "attached-to"
                         "GtkWidget" T T)
                        (DECORATED GTK-WINDOW-DECORATED "decorated" "gboolean"
                         T T)
                        (DEFAULT-HEIGHT GTK-WINDOW-DEFAULT-HEIGHT
                         "default-height" "gint" T T)
                        (DEFAULT-WIDTH GTK-WINDOW-DEFAULT-WIDTH "default-width"
                         "gint" T T)
                        (DELETABLE GTK-WINDOW-DELETABLE "deletable" "gboolean"
                         T T)
                        (DESTROY-WITH-PARENT GTK-WINDOW-DESTROY-WITH-PARENT
                         "destroy-with-parent" "gboolean" T T)
                        (FOCUS-ON-MAP GTK-WINDOW-FOCUS-ON-MAP "focus-on-map"
                         "gboolean" T T)
                        (FOCUS-VISIBLE GTK-WINDOW-FOCUS-VISIBLE "focus-visible"
                         "gboolean" T T)
                        (GRAVITY GTK-WINDOW-GRAVITY "gravity" "GdkGravity" T T)
                        (HAS-RESIZE-GRIP GTK-WINDOW-HAS-RESIZE-GRIP
                         "has-resize-grip" "gboolean" T T)
                        (HAS-TOPLEVEL-FOCUS GTK-WINDOW-HAS-TOPLEVEL-FOCUS
                         "has-toplevel-focus" "gboolean" T NIL)
                        (HIDE-TITLEBAR-WHEN-MAXIMIZED
                         GTK-WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED
                         "hide-titlebar-when-maximized" "gboolean" T T)
                        (ICON GTK-WINDOW-ICON "icon" "GdkPixbuf" T T)
                        (ICON-NAME GTK-WINDOW-ICON-NAME "icon-name"
                         "gchararray" T T)
                        (IS-ACTIVE GTK-WINDOW-IS-ACTIVE "is-active" "gboolean"
                         T NIL)
                        (IS-MAXIMIZED GTK-WINDOW-IS-MAXIMIZED "is-maximized"
                         "gboolean" T NIL)
                        (MNEMONICS-VISIBLE GTK-WINDOW-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (MODAL GTK-WINDOW-MODAL "modal" "gboolean" T T)
                        (RESIZABLE GTK-WINDOW-RESIZABLE "resizable" "gboolean"
                         T T)
                        (RESIZE-GRIP-VISIBLE GTK-WINDOW-RESIZE-GRIP-VISIBLE
                         "resize-grip-visible" "gboolean" T NIL)
                        (ROLE GTK-WINDOW-ROLE "role" "gchararray" T T)
                        (SCREEN GTK-WINDOW-SCREEN "screen" "GdkScreen" T T)
                        (SKIP-PAGER-HINT GTK-WINDOW-SKIP-PAGER-HINT
                         "skip-pager-hint" "gboolean" T T)
                        (SKIP-TASKBAR-HINT GTK-WINDOW-SKIP-TASKBAR-HINT
                         "skip-taskbar-hint" "gboolean" T T)
                        (STARTUP-ID GTK-WINDOW-STARTUP-ID "startup-id"
                         "gchararray" NIL T)
                        (TITLE GTK-WINDOW-TITLE "title" "gchararray" T T)
                        (TRANSIENT-FOR GTK-WINDOW-TRANSIENT-FOR "transient-for"
                         "GtkWindow" T T)
                        (TYPE GTK-WINDOW-TYPE "type" "GtkWindowType" T NIL)
                        (TYPE-HINT GTK-WINDOW-TYPE-HINT "type-hint"
                         "GdkWindowTypeHint" T T)
                        (URGENCY-HINT GTK-WINDOW-URGENCY-HINT "urgency-hint"
                         "gboolean" T T)
                        (WINDOW-POSITION GTK-WINDOW-WINDOW-POSITION
                         "window-position" "GtkWindowPosition" T T)))
             (get-g-type-definition "GtkWindow"))))

