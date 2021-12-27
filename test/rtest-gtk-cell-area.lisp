(def-suite gtk-cell-area :in gtk-suite)
(in-suite gtk-cell-area)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellArea

(test gtk-cell-area-class
  ;; Type check
  (is (g-type-is-object "GtkCellArea"))
  ;; Check the registered name
  (is (eq 'gtk-cell-area
          (registered-object-type-by-name "GtkCellArea")))
  ;; Check the type initializer
  (is (eq (gtype "GtkCellArea")
          (gtype (foreign-funcall "gtk_cell_area_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GInitiallyUnowned")
          (g-type-parent "GtkCellArea")))
  ;; Check the children
  (is (equal '("GtkCellAreaBox")
             (mapcar #'g-type-name (g-type-children "GtkCellArea"))))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkCellArea"))))
  ;; Check the class properties
  (is (equal '("edit-widget" "edited-cell" "focus-cell")
             (list-class-property-names "GtkCellArea")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellArea" GTK-CELL-AREA
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_cell_area_get_type")
                       ((EDIT-WIDGET GTK-CELL-AREA-EDIT-WIDGET "edit-widget"
                         "GtkCellEditable" T NIL)
                        (EDITED-CELL GTK-CELL-AREA-EDITED-CELL "edited-cell"
                         "GtkCellRenderer" T NIL)
                        (FOCUS-CELL GTK-CELL-AREA-FOCUS-CELL "focus-cell"
                         "GtkCellRenderer" T T)))
             (get-g-type-definition "GtkCellArea"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkCellEditable*   edit-widget         Read
;;;     GtkCellRenderer*   edited-cell         Read
;;;     GtkCellRenderer*   focus-cell          Read / Write

(test gtk-cell-area-properties
  ;; Create a GtkCellAreaBox object, GtkCellArea is a abstract class.
  (let ((area (make-instance 'gtk-cell-area-box)))

    (is-false (gtk-cell-area-edit-widget area))
    (is-false (gtk-cell-area-edited-cell area))
    (is-false (gtk-cell-area-focus-cell area))
))

;;; --- Signals ----------------------------------------------------------------

;;;                void    add-editable        Run First
;;;                void    apply-attributes    Run First
;;;                void    focus-changed       Run First
;;;                void    remove-editable     Run First

;;; --- Functions --------------------------------------------------------------

;;;     GtkCellCallback
;;;     GtkCellAllocCallback

;;;     GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID

;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer
;;;     gtk_cell_area_foreach
;;;     gtk_cell_area_foreach_alloc
;;;     gtk_cell_area_event
;;;     gtk_cell_area_render
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position
;;;     gtk_cell_area_create_context
;;;     gtk_cell_area_copy_context
;;;     gtk_cell_area_get_request_mode
;;;     gtk_cell_area_get_preferred_width
;;;     gtk_cell_area_get_preferred_height_for_width
;;;     gtk_cell_area_get_preferred_height
;;;     gtk_cell_area_get_preferred_width_for_height
;;;     gtk_cell_area_get_current_path_string
;;;     gtk_cell_area_apply_attributes
;;;     gtk_cell_area_attribute_connect
;;;     gtk_cell_area_attribute_disconnect
;;;     gtk_cell_area_attribute_get_column ()
;;;     gtk_cell_area_class_install_cell_property

;;;     gtk_cell_area_class_find_cell_property

(test gtk-cell-area-class-find-cell-property
  (let ((area (make-instance 'gtk-cell-area-box))
        (renderer (make-instance 'gtk-cell-renderer-pixbuf)))
    (is-false (gtk-cell-area-add area renderer))
    (is (pointerp (gtk-cell-area-class-find-cell-property "GtkCellAreaBox"
                                                          "align")))
    (is (pointerp (gtk-cell-area-class-find-cell-property "GtkCellAreaBox"
                                                          "expand")))
    (is (pointerp (gtk-cell-area-class-find-cell-property "GtkCellAreaBox"
                                                         "fixed-size")))
    (is (pointerp (gtk-cell-area-class-find-cell-property "GtkCellAreaBox"
                                                          "pack-type")))))

;;;     gtk_cell_area_class_list_cell_properties

(test gtk-call-area-class-list-cell-properties
  (is (equal '("align" "expand" "fixed-size" "pack-type")
             (sort (mapcar #'g-param-spec-name
                           (gtk-cell-area-class-list-cell-properties
                                                              "GtkCellAreaBox"))
                   #'string<))))

;;;     gtk_cell_area_add_with_properties

;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get

(test gtk-cell-area-cell-set/get
  (let ((area (make-instance 'gtk-cell-area-box))
        (renderer (make-instance 'gtk-cell-renderer-pixbuf)))
    (is-false (gtk-cell-area-add area renderer))
    (is-false (gtk-cell-area-cell-set area
                                      renderer
                                      "align" t
                                      "expand" t
                                      "fixed-size" nil
                                      "pack-type" :end))
    (is (equal '(t t nil :end)
               (gtk-cell-area-cell-get area
                                       renderer
                                       "align"
                                       "expand"
                                       "fixed-size"
                                       "pack-type")))))

;;;     gtk_cell_area_cell_set_valist
;;;     gtk_cell_area_cell_get_valist

;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property

(test gtk-cell-area-cell-property
  (let ((area (make-instance 'gtk-cell-area-box))
        (renderer (make-instance 'gtk-cell-renderer-pixbuf)))
    (is-false (gtk-cell-area-add area renderer))
    (is-false (gtk-cell-area-cell-property area renderer "align"))
    (is-false (gtk-cell-area-cell-property area renderer "expand"))
    (is-true (gtk-cell-area-cell-property area renderer "fixed-size"))
    (is (eq :start (gtk-cell-area-cell-property area renderer "pack-type")))))

;;;     gtk_cell_area_is_activatable
;;;     gtk_cell_area_activate
;;;     gtk_cell_area_focus
;;;     gtk_cell_area_set_focus_cell                       Accessor
;;;     gtk_cell_area_get_focus_cell                       Accessor
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_get_edited_cell                      Accessor
;;;     gtk_cell_area_get_edit_widget                      Accessor
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer

;;; 2021-12-19
