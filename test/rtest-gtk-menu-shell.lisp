(def-suite gtk-menu-shell :in gtk-suite)
(in-suite gtk-menu-shell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMenuDirectionType

(test gtk-menu-direction-type
  ;; Check the type
  (is-true (g-type-is-enum "GtkMenuDirectionType"))
  ;; Check the type initializer
  (is (eq (gtype "GtkMenuDirectionType")
          (gtype (foreign-funcall "gtk_menu_direction_type_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-menu-direction-type (gobject::registered-enum-type "GtkMenuDirectionType")))
  ;; Check the names
  (is (equal '("GTK_MENU_DIR_PARENT" "GTK_MENU_DIR_CHILD" "GTK_MENU_DIR_NEXT"
               "GTK_MENU_DIR_PREV")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkMenuDirectionType"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkMenuDirectionType"))))
  ;; Check the nick names
  (is (equal '("parent" "child" "next" "prev")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkMenuDirectionType"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkMenuDirectionType"
                             GTK-MENU-DIRECTION-TYPE
                             (:EXPORT T :TYPE-INITIALIZER "gtk_menu_direction_type_get_type")
                             (:PARENT 0)
                             (:CHILD 1)
                             (:NEXT 2)
                             (:PREV 3))
             (gobject::get-g-type-definition "GtkMenuDirectionType"))))

;;;     GtkMenuShell

(test gtk-menu-shell-class
  ;; Type check
  (is (g-type-is-object "GtkMenuShell"))
  ;; Check the registered name
  (is (eq 'gtk-menu-shell
          (registered-object-type-by-name "GtkMenuShell")))
  ;; Check the type initializer
  (is (eq (gtype "GtkMenuShell")
          (gtype (foreign-funcall "gtk_menu_shell_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer") (g-type-parent "GtkMenuShell")))
  ;; Check the children
  (is (equal '("GtkMenu" "GtkMenuBar")
             (mapcar #'g-type-name (g-type-children "GtkMenuShell"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkMenuShell"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "events" "expand" "focus-on-click"
               "halign" "has-default" "has-focus" "has-tooltip" "height-request" "hexpand"
               "hexpand-set" "is-focus" "margin" "margin-bottom" "margin-end" "margin-left"
               "margin-right" "margin-start" "margin-top" "name" "no-show-all" "opacity"
               "parent" "receives-default" "resize-mode" "scale-factor" "sensitive" "style"
               "take-focus" "tooltip-markup" "tooltip-text" "valign" "vexpand" "vexpand-set"
               "visible" "width-request" "window")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkMenuShell"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkMenuShell"))))
  ;; Get the names of the child properties
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (gtk-container-class-list-child-properties "GtkMenuShell"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMenuShell" GTK-MENU-SHELL
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_menu_shell_get_type")
                       ((TAKE-FOCUS GTK-MENU-SHELL-TAKE-FOCUS "take-focus"
                         "gboolean" T T)))
             (get-g-type-definition "GtkMenuShell"))))

;;; --- Properties -------------------------------------------------------------

;;;     gboolean    take-focus          Read / Write

(test gtk-menu-shell-properties
  (let ((menu (make-instance 'gtk-menu))) ; gtk-menu implements gtk-menu-shell
    (is-true (gtk-menu-shell-take-focus menu))))

;;; --- Signals ----------------------------------------------------------------

;;;         void    activate-current    Action
;;;         void    cancel              Action
;;;         void    cycle-focus         Action
;;;         void    deactivate          Run First
;;;         void    insert              Run First
;;;         void    move-current        Action
;;;     gboolean    move-selected       Run Last
;;;         void    selection-done      Run First

;;; --- Functions --------------------------------------------------------------

;;;     gtk_menu_shell_append

(test gtk-menu-shell-append
  (let ((menu (make-instance 'gtk-menu))
        (item1 (make-instance 'gtk-menu-item))
        (item2 (make-instance 'gtk-menu-item))
        (item3 (make-instance 'gtk-menu-item)))

    (is-false (gtk-menu-shell-append menu item1))
    (is (equal item1 (first (gtk-container-children menu))))

    (is-false (gtk-menu-shell-append menu item2))
    (is (equal item2 (second (gtk-container-children menu))))

    (is-false (gtk-menu-shell-append menu item3))
    (is (equal item3 (third (gtk-container-children menu))))))

;;;     gtk_menu_shell_prepend

(test gtk-menu-shell-prepend
  (let ((menu (make-instance 'gtk-menu))
        (item1 (make-instance 'gtk-menu-item))
        (item2 (make-instance 'gtk-menu-item))
        (item3 (make-instance 'gtk-menu-item)))

    (is-false (gtk-menu-shell-prepend menu item1))
    (is (equal item1 (first (gtk-container-children menu))))

    (is-false (gtk-menu-shell-prepend menu item2))
    (is (equal item2 (first (gtk-container-children menu))))

    (is-false (gtk-menu-shell-prepend menu item3))
    (is (equal item3 (first (gtk-container-children menu))))))

;;;     gtk_menu_shell_insert

(test gtk-menu-shell-insert
  (let ((menu (make-instance 'gtk-menu))
        (item1 (make-instance 'gtk-menu-item))
        (item2 (make-instance 'gtk-menu-item))
        (item3 (make-instance 'gtk-menu-item)))

    (is-false (gtk-menu-shell-insert menu item1 1))
    (is (equal item1 (first (gtk-container-children menu))))

    (is-false (gtk-menu-shell-insert menu item2 1))
    (is (equal item2 (second (gtk-container-children menu))))

    (is-false (gtk-menu-shell-insert menu item3 1))
    (is (equal item3 (second (gtk-container-children menu))))))

;;;     gtk_menu_shell_deactivate
;;;     gtk_menu_shell_select_item
;;;     gtk_menu_shell_select_first
;;;     gtk_menu_shell_deselect
;;;     gtk_menu_shell_activate_item
;;;     gtk_menu_shell_cancel
;;;     gtk_menu_shell_get_selected_item
;;;     gtk_menu_shell_get_parent_shell
;;;     gtk_menu_shell_bind_model

