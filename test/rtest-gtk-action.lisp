
(def-suite gtk-action :in gtk-suite)
(in-suite gtk-action)

(defvar *verbose-gtk-action* nil)

;;;   GtkAction

(test gtk-action-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkAction"))
  (is-false (g-type-is-abstract "GtkAction"))
  (is-true  (g-type-is-derived "GtkAction"))
  (is-false (g-type-is-fundamental "GtkAction"))
  (is-true  (g-type-is-value-type "GtkAction"))
  (is-true  (g-type-has-value-table "GtkAction"))
  (is-true  (g-type-is-classed "GtkAction"))
  (is-true  (g-type-is-instantiatable "GtkAction"))
  (is-true  (g-type-is-derivable "GtkAction"))
  (is-true  (g-type-is-deep-derivable "GtkAction"))
  (is-false (g-type-is-interface "GtkAction"))

  ;; Check the registered name
  (is (eq 'gtk-action
          (registered-object-type-by-name "GtkAction")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkAction"))))
    (is (equal (gtype "GtkAction") (g-type-from-class class)))
    (is (equal (gtype "GtkAction") (g-object-class-type class)))
    (is (equal "GtkAction" (g-object-class-name class)))
    (is (equal (gtype "GtkAction")
               (g-type-from-class  (g-type-class-peek "GtkAction"))))
    (is (equal (gtype "GtkAction")
               (g-type-from-class  (g-type-class-peek-static "GtkAction"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-action)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-action (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkAction" (gobject-class-g-type-name class)))
    (is (equal "GtkAction" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_action_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GObject") (g-type-parent "GtkAction")))
  (is (= 2 (g-type-depth "GtkAction")))
  (is (equal (gtype "GtkAction")
             (g-type-next-base "GtkAction" "GObject")))
  (is-false (g-type-is-a "GtkAction" "GtkWidget"))
  (is-false (g-type-is-a "GtkAction" "GtkContainer"))
  (is-false (g-type-is-a "GtkAction" "gboolean"))
  (is-false (g-type-is-a "GtkAction" "GtkWindow"))

  ;; Check the children
  (is (equal '("GtkToggleAction" "GtkRecentAction")
             (mapcar #'gtype-name (g-type-children "GtkAction"))))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkAction"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkAction" query)
    (is (equal (gtype "GtkAction")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkAction"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 232 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  32 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal ' ("name" "label" "short-label" "tooltip" "stock-id" "icon-name"
                "gicon" "visible-horizontal" "visible-vertical"
                "visible-overflown" "is-important" "hide-if-empty" "sensitive"
                "visible" "action-group" "always-show-image")
             (mapcar #'param-spec-name
                     (g-object-class-list-properties "GtkAction"))))

  ;; Get the names of the style properties.
  (is (equal '()
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkAction"))))

  ;; Get the names to the child properties

  ;; * GtkAction is not a container *

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAction" GTK-ACTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_action_get_type")
                       ((ACTION-GROUP GTK-ACTION-ACTION-GROUP "action-group"
                         "GtkActionGroup" T T)
                        (ALWAYS-SHOW-IMAGE GTK-ACTION-ALWAYS-SHOW-IMAGE
                         "always-show-image" "gboolean" T T)
                        (GICON GTK-ACTION-GICON "gicon" "GIcon" T T)
                        (HIDE-IF-EMPTY GTK-ACTION-HIDE-IF-EMPTY "hide-if-empty"
                         "gboolean" T T)
                        (ICON-NAME GTK-ACTION-ICON-NAME "icon-name"
                         "gchararray" T T)
                        (IS-IMPORTANT GTK-ACTION-IS-IMPORTANT "is-important"
                         "gboolean" T T)
                        (LABEL GTK-ACTION-LABEL "label" "gchararray" T T)
                        (NAME GTK-ACTION-NAME "name" "gchararray" T NIL)
                        (SENSITIVE GTK-ACTION-SENSITIVE "sensitive" "gboolean"
                         T T)
                        (SHORT-LABEL GTK-ACTION-SHORT-LABEL "short-label"
                         "gchararray" T T)
                        (STOCK-ID GTK-ACTION-STOCK-ID "stock-id" "gchararray" T
                         T)
                        (TOOLTIP GTK-ACTION-TOOLTIP "tooltip" "gchararray" T T)
                        (VISIBLE GTK-ACTION-VISIBLE "visible" "gboolean" T T)
                        (VISIBLE-HORIZONTAL GTK-ACTION-VISIBLE-HORIZONTAL
                         "visible-horizontal" "gboolean" T T)
                        (VISIBLE-OVERFLOWN GTK-ACTION-VISIBLE-OVERFLOWN
                         "visible-overflown" "gboolean" T T)
                        (VISIBLE-VERTICAL GTK-ACTION-VISIBLE-VERTICAL
                         "visible-vertical" "gboolean" T T)))
             (get-g-type-definition "GtkAction"))))

;;;   gtk_action_new

(test gtk-action-new.1
  (let ((action (gtk-action-new "action")))
    (is (equal "action" (gtk-action-name action)))
    (is-false (gtk-action-label action))
    (is-false (gtk-action-tooltip action))
    (is-false (gtk-action-stock-id action))))

(test gtk-action-new.2
  (let ((action (gtk-action-new "action" "label" "tooltip" "stock-id")))
    (is (equal "action" (gtk-action-name action)))
    (is (equal "label" (gtk-action-label action)))
    (is (equal "tooltip" (gtk-action-tooltip action)))
    (is (equal "stock-id" (gtk-action-stock-id action)))))

;;;   gtk_action_get_name

(test gtk-action-get-name
  (let ((action (gtk-action-new "action")))
    (is (equal "action" (gtk-action-get-name action)))))

;;;   gtk_action_is_sensitive
;;;   gtk_action_get_sensitive
;;;   gtk_action_set_sensitive

(test gtk-action-sensitive
  (let ((action (gtk-action-new "action")))
    (is-true (gtk-action-get-sensitive action))
    (is-true (gtk-action-is-sensitive action))
    (gtk-action-set-sensitive action nil)
    (is-false (gtk-action-get-sensitive action))
    (is-false (gtk-action-is-sensitive action))))

;;;   gtk_action_is_visible
;;;   gtk_action_get_visible
;;;   gtk_action_set_visible

(test gtk-action-visible
  (let ((action (gtk-action-new "action")))
    (is-true (gtk-action-get-visible action))
    (is-true (gtk-action-is-visible action))
    (gtk-action-set-visible action nil)
    (is-false (gtk-action-get-visible action))
    (is-false (gtk-action-is-visible action))))

;;;   gtk_action_activate

(test gtk-action-activate
  (let ((action (gtk-action-new "action"))
        (message nil))

    (g-signal-connect action "activate"
       (lambda (action)
         (setf message "ACTIVATE CALLED")
         (when *verbose-gtk-action*
           (format t "~&Signal ACTIVATE for ~A~%" (gtk-action-name action)))))

    (gtk-action-activate action)
    (is (equal "ACTIVATE CALLED" message))))

;;;   gtk_action_create_icon

(test gtk-action-create-icon
  (let ((action (gtk-action-new "action")))
    ;; Check for a stock-id, also check for icon-name and gicon
    (gtk-action-set-stock-id action "gtk-ok")
    (is (eq 'gtk-image (type-of (gtk-action-create-icon action :dialog))))))

;;;   gtk_action_create_menu_item

(test gtk-action-create-menu-item
  (let ((action (gtk-action-new "action")))
    (is (eq 'gtk-image-menu-item
            (type-of (gtk-action-create-menu-item action))))))

;;;   gtk_action_create_tool_item

(test gtk-action-create-tool-item
  (let ((action (gtk-action-new "action")))
    (is (eq 'gtk-tool-button
            (type-of (gtk-action-create-tool-item action))))))

;;;   gtk_action_create_menu

(test gtk-action-create-menu
  (let ((action (gtk-action-new "action")))
    ;; Create an test for an result not nil
    (is-false (gtk-action-create-menu action))))

;;;   gtk_action_get_proxies

(test gtk-action-get-proxies
  (let ((action (gtk-action-new "action")))
    (is-false (gtk-action-get-proxies action))
    ;; Add a too item to list of proxies
    (gtk-action-create-tool-item action)
    (is (eq 'gtk-tool-button (type-of (first (gtk-action-get-proxies action)))))
    ;; Add a menu item to the proxies
    (gtk-action-create-menu-item action)
    (is (eq 'gtk-image-menu-item
            (type-of (first (gtk-action-get-proxies action)))))))

;;;   gtk_action_connect_accelerator

(test gtk-action-connect-accelerator
  (let ((accel-group (gtk-accel-group-new))
        (action (gtk-action-new "action")))
    (gtk-action-set-accel-path action "<test>/File/Exit")
    (gtk-action-set-accel-group action accel-group)
    (gtk-action-connect-accelerator action)))

;;;    gtk_action_disconnect_accelerator
;;;    gtk_action_block_activate
;;;    gtk_action_unblock_activate
;;;    gtk_action_get_always_show_image
;;;    gtk_action_set_always_show_image
;;;    gtk_action_get_accel_path
;;;    gtk_action_set_accel_path
;;;    gtk_action_get_accel_closure
;;;    gtk_action_set_accel_group
;;;    gtk_action_set_label
;;;    gtk_action_get_label
;;;    gtk_action_set_short_label
;;;    gtk_action_get_short_label
;;;    gtk_action_set_tooltip
;;;    gtk_action_get_tooltip
;;;    gtk_action_set_stock_id
;;;    gtk_action_get_stock_id
;;;    gtk_action_set_gicon
;;;    gtk_action_get_gicon
;;;    gtk_action_set_icon_name
;;;    gtk_action_get_icon_name
;;;    gtk_action_set_visible_horizontal
;;;    gtk_action_get_visible_horizontal
;;;    gtk_action_set_visible_vertical
;;;    gtk_action_get_visible_vertical
;;;    gtk_action_set_is_important
;;;    gtk_action_get_is_important
