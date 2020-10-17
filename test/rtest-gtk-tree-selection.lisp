(def-suite gtk-tree-selection :in gtk-suite)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

(test gtk-tree-selection-class
  ;; Type check
  (is-true (g-type-is-object "GtkTreeSelection"))
  ;; Check the registered name
  (is (eq 'gtk-tree-selection
          (registered-object-type-by-name "GtkTreeSelection")))
  ;; Check the type initializer
  (is (string= "GtkTreeSelection"
               (g-type-name (gtype (foreign-funcall "gtk_tree_selection_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkTreeSelection")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkTreeSelection"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkTreeSelection"))))
  ;; Check the class properties
  (is (equal '("mode")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkTreeSelection"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTreeSelection" GTK-TREE-SELECTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tree_selection_get_type")
                       ((MODE GTK-TREE-SELECTION-MODE "mode" "GtkSelectionMode"
                         T T)))
             (get-g-type-definition "GtkTreeSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkSelectionMode    mode       Read / Write

(test gtk-tree-selection-properties
  (let* ((tree-view (make-instance 'gtk-tree-view))
         (selection (gtk-tree-view-selection tree-view)))
    (is (eq 'gtk-tree-selection (type-of selection)))
    (is (eq :single (gtk-tree-selection-mode selection)))
    (is (eq :multiple (setf (gtk-tree-selection-mode selection) :multiple)))
    (is (eq :multiple (gtk-tree-selection-mode selection)))))

;;; --- Signals ----------------------------------------------------------------

;;;                 void    changed    Run First

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc

;;;     gtk_tree_selection_set_select_function
;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data

;;;     gtk_tree_selection_get_tree_view

(test gtk-tree-selection-tree-view
  (let* ((tree-view (make-instance 'gtk-tree-view))
         (selection (gtk-tree-view-selection tree-view)))
    (is (eq 'gtk-tree-selection (type-of selection)))
    (is (eq 'gtk-tree-view (type-of (gtk-tree-selection-tree-view selection))))))

;;;     gtk_tree_selection_get_selected

(test gtk-tree-selection-selected
  (let* ((tree-view (make-instance 'gtk-tree-view))
         (selection (gtk-tree-view-selection tree-view)))

    (is-false (gtk-tree-selection-selected selection))
))

;;;     gtk_tree_selection_selected_foreach
;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows
;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected
;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected
;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all
;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range

