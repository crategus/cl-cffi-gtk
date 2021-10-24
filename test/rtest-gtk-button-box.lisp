(def-suite gtk-button-box :in gtk-suite)
(in-suite gtk-button-box)

;;; Types and Values

;;;     GtkButtonBoxStyle

(test gtk-button-box-style
  ;; Check the type
  (is (g-type-is-enum "GtkButtonBoxStyle"))
  ;; Check the type initializer
  (is (eq (gtype "GtkButtonBoxStyle")
          (gtype (foreign-funcall "gtk_button_box_style_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-button-box-style
          (registered-enum-type "GtkButtonBoxStyle")))
  ;; Check the names
  (is (equal '("GTK_BUTTONBOX_SPREAD" "GTK_BUTTONBOX_EDGE" "GTK_BUTTONBOX_START"
               "GTK_BUTTONBOX_END" "GTK_BUTTONBOX_CENTER" "GTK_BUTTONBOX_EXPAND")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the values
  (is (equal '(1 2 3 4 5 6)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the nick names
  (is (equal '("spread" "edge" "start" "end" "center" "expand")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkButtonBoxStyle"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkButtonBoxStyle"
                             GTK-BUTTON-BOX-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_button_box_style_get_type")
                             (:SPREAD 1)
                             (:EDGE 2)
                             (:START 3)
                             (:END 4)
                             (:CENTER 5)
                             (:EXPAND 6))
             (get-g-type-definition "GtkButtonBoxStyle"))))

;;;     GtkButtonBox

(foreign-funcall "gtk_hbutton_box_get_type" g-size)
(foreign-funcall "gtk_vbutton_box_get_type" g-size)

(test gtk-button-box-class
  ;; Type check
  (is (g-type-is-object "GtkButtonBox"))
  ;; Check the registered name
  (is (eq 'gtk-button-box
          (registered-object-type-by-name "GtkButtonBox")))
  ;; Check the type initializer
  (is (eq (gtype "GtkButtonBox")
          (gtype (foreign-funcall "gtk_button_box_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBox") (g-type-parent "GtkButtonBox")))
  ;; Check the children
  (is (equal '("GtkHButtonBox" "GtkVButtonBox")
             (mapcar #'g-type-name (g-type-children "GtkButtonBox"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'g-type-name (g-type-interfaces "GtkButtonBox"))))
  ;; Check the class properties
  (is (equal '("layout-style")
             (list-class-property-names "GtkButtonBox")))
  ;; Get the names of the style properties.
  (is (equal '("child-internal-pad-x" "child-internal-pad-y" "child-min-height"
               "child-min-width")
             (list-class-style-property-names "GtkButtonBox")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "non-homogeneous" "pack-type" "padding"
               "position" "secondary")
             (list-class-child-property-names "GtkButtonBox")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButtonBox" GTK-BUTTON-BOX
                       (:SUPERCLASS GTK-BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_button_box_get_type")
                       ((LAYOUT-STYLE GTK-BUTTON-BOX-LAYOUT-STYLE
                         "layout-style" "GtkButtonBoxStyle" T T)))
             (get-g-type-definition "GtkButtonBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkButtonBoxStyle  layout-style          Read / Write

;;; --- Child Properties -------------------------------------------------------

;;;              gboolean  non-homogeneous       Read / Write
;;;              gboolean  secondary             Read / Write

;;; --- Style Properties -------------------------------------------------------

;;;                  gint  child-internal-pad-x  Read
;;;                  gint  child-internal-pad-y  Read
;;;                  gint  child-min-height      Read
;;;                  gint  child-min-width       Read

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_box_new
;;;     gtk_button_box_get_layout
;;;     gtk_button_box_get_child_secondary
;;;     gtk_button_box_get_child_non_homogeneous
;;;     gtk_button_box_set_layout
;;;     gtk_button_box_set_child_secondary
;;;     gtk_button_box_set_child_non_homogeneous

;;; 2021-10-19
