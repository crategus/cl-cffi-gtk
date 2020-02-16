(def-suite gtk-action :in gtk-suite)
(in-suite gtk-action)

(defvar *verbose-gtk-action* nil)

;;;   GtkAction

(test gtk-action-class
  ;; Type check
  (is-true  (g-type-is-object "GtkAction"))
  ;; Check the registered name
  (is (eq 'gtk-action
          (registered-object-type-by-name "GtkAction")))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkAction")))
  ;; Check the children
  (is (equal '("GtkToggleAction" "GtkRecentAction")
             (mapcar #'gtype-name (g-type-children "GtkAction"))))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkAction"))))
  ;; Check the class properties
  (is (equal '("action-group" "always-show-image" "gicon" "hide-if-empty" "icon-name"
 "is-important" "label" "name" "sensitive" "short-label" "stock-id" "tooltip"
 "visible" "visible-horizontal" "visible-overflown" "visible-vertical")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkAction"))
                          #'string-lessp)))
  ;; Check the class definition
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

;;;   gtk-action-name

(test gtk-action-name
  (let ((action (gtk-action-new "action")))
    (is (equal "action" (gtk-action-name action)))))

;;;   gtk-action-is-sensitive
;;;   gtk-action-sensitive

(test gtk-action-sensitive
  (let ((action (gtk-action-new "action")))
    (is-true (gtk-action-sensitive action))
    (is-true (gtk-action-is-sensitive action))
    (setf (gtk-action-sensitive action) nil)
    (is-false (gtk-action-sensitive action))
    (is-false (gtk-action-is-sensitive action))))

;;;   gtk-action-is-visible
;;;   gtk-action-visible

(test gtk-action-visible
  (let ((action (gtk-action-new "action")))
    (is-true (gtk-action-visible action))
    (is-true (gtk-action-is-visible action))
    (setf (gtk-action-visible action) nil)
    (is-false (gtk-action-visible action))
    (is-false (gtk-action-is-visible action))))

;;;   gtk-action-activate

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

;;;   gtk-action-create-icon

(test gtk-action-create-icon
  (let ((action (gtk-action-new "action")))
    ;; Check for a stock-id, also check for icon-name and gicon
    (setf (gtk-action-stock-id action) "gtk-ok")
    (is (eq 'gtk-image (type-of (gtk-action-create-icon action :dialog))))))

;;;   gtk-action-create-menu-item

(test gtk-action-create-menu-item
  (let ((action (gtk-action-new "action")))
    (is (eq 'gtk-image-menu-item
            (type-of (gtk-action-create-menu-item action))))))

;;;   gtk-action-create-tool-item

(test gtk-action-create-tool-item
  (let ((action (gtk-action-new "action")))
    (is (eq 'gtk-tool-button
            (type-of (gtk-action-create-tool-item action))))))

;;;   gtk-action-create-menu

(test gtk-action-create-menu
  (let ((action (gtk-action-new "action")))
    ;; Create an test for an result not nil
    (is-false (gtk-action-create-menu action))))

;;;   gtk-action-get-proxies

(test gtk-action-get-proxies
  (let ((action (gtk-action-new "action")))
    (is-false (gtk-action-get-proxies action))
    ;; Add a tool item to list of proxies
    (gtk-action-create-tool-item action)
    (is (eq 'gtk-tool-button (type-of (first (gtk-action-get-proxies action)))))
    ;; Add a menu item to list of proxies
    (gtk-action-create-menu-item action)
    (is (eq 'gtk-image-menu-item
            (type-of (first (gtk-action-get-proxies action)))))))

;;;   gtk-action-connect-accelerator

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

