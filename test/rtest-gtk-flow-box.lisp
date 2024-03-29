(def-suite gtk-flow-box :in gtk-suite)
(in-suite gtk-flow-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBoxChild

(test gtk-flow-box-child-class
  ;; Type check
  (is (g-type-is-object "GtkFlowBoxChild"))
  ;; Check the registered name
  (is (eq 'gtk-flow-box-child
          (registered-object-type-by-name "GtkFlowBoxChild")))
  ;; Check the type initializer
  (is (eq (gtype "GtkFlowBoxChild")
          (gtype (foreign-funcall "gtk_flow_box_child_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkBin") (g-type-parent "GtkFlowBoxChild")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkFlowBoxChild"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkFlowBoxChild"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkFlowBoxChild")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-class-style-property-names "GtkFlowBoxChild")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkFlowBoxChild")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFlowBoxChild" GTK-FLOW-BOX-CHILD
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_flow_box_child_get_type")
                       NIL)
             (get-g-type-definition "GtkFlowBoxChild"))))

;;;     GtkFlowBox

(test gtk-flow-box-class
  ;; Type check
  (is (g-type-is-object "GtkFlowBox"))
  ;; Check the registered name
  (is (eq 'gtk-flow-box
          (registered-object-type-by-name "GtkFlowBox")))
  ;; Check the type initializer
  (is (eq (gtype "GtkFlowBox")
          (gtype (foreign-funcall "gtk_flow_box_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer") (g-type-parent "GtkFlowBox")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkFlowBox"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'g-type-name (g-type-interfaces "GtkFlowBox"))))
  ;; Check the class properties
  (is (equal '("activate-on-single-click" "column-spacing" "homogeneous"
               "max-children-per-line" "min-children-per-line" "orientation"
               "row-spacing" "selection-mode")
             (list-class-property-names "GtkFlowBox")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-class-style-property-names "GtkFlowBox")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkFlowBox")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFlowBox" GTK-FLOW-BOX
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_flow_box_get_type")
                       ((ACTIVATE-ON-SINGLE-CLICK
                         GTK-FLOW-BOX-ACTIVATE-ON-SINGLE-CLICK
                         "activate-on-single-click" "gboolean" T T)
                        (COLUMN-SPACING GTK-FLOW-BOX-COLUMN-SPACING
                         "column-spacing" "guint" T T)
                        (HOMOGENEOUS GTK-FLOW-BOX-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (MAX-CHILDREN-PER-LINE
                         GTK-FLOW-BOX-MAX-CHILDREN-PER-LINE
                         "max-children-per-line" "guint" T T)
                        (MIN-CHILDREN-PER-LINE
                         GTK-FLOW-BOX-MIN-CHILDREN-PER-LINE
                         "min-children-per-line" "guint" T T)
                        (ROW-SPACING GTK-FLOW-BOX-ROW-SPACING "row-spacing"
                         "guint" T T)
                        (SELECTION-MODE GTK-FLOW-BOX-SELECTION-MODE
                         "selection-mode" "GtkSelectionMode" T T)))
             (get-g-type-definition "GtkFlowBox"))))

;;; --- Properties -------------------------------------------------------------

(test gkt-flow-box-properties
  (let ((flowbox (make-instance 'gtk-flow-box)))
    (is-true (gtk-flow-box-activate-on-single-click flowbox))
    (is (= 0 (gtk-flow-box-column-spacing flowbox)))
    (is-false (gtk-flow-box-homogeneous flowbox))
    (is (= 7 (gtk-flow-box-max-children-per-line flowbox)))
    (is (= 0 (gtk-flow-box-min-children-per-line flowbox)))
    (is (= 0 (gtk-flow-box-row-spacing flowbox)))
    (is (eq :single (gtk-flow-box-selection-mode flowbox)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment
;;;     gtk_flow_box_set_homogeneous
;;;     gtk_flow_box_get_homogeneous
;;;     gtk_flow_box_set_row_spacing
;;;     gtk_flow_box_get_row_spacing
;;;     gtk_flow_box_set_column_spacing
;;;     gtk_flow_box_get_column_spacing
;;;     gtk_flow_box_set_min_children_per_line
;;;     gtk_flow_box_get_min_children_per_line
;;;     gtk_flow_box_set_max_children_per_line
;;;     gtk_flow_box_get_max_children_per_line
;;;     gtk_flow_box_set_activate_on_single_click
;;;     gtk_flow_box_get_activate_on_single_click
;;;     (*GtkFlowBoxForeachFunc)
;;;     gtk_flow_box_selected_foreach
;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all
;;;     gtk_flow_box_set_selection_mode
;;;     gtk_flow_box_get_selection_mode
;;;     (*GtkFlowBoxFilterFunc)
;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter
;;;     (*GtkFlowBoxSortFunc)
;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort
;;;     (*GtkFlowBoxCreateWidgetFunc)
;;;     gtk_flow_box_bind_model

;;;     gtk_flow_box_child_new
;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed

;;; 2021-10-18
