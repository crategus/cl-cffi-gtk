(def-suite gtk-grid :in gtk-suite)
(in-suite gtk-grid)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkGrid

(test gtk-grid-class
  ;; Type check
  (is-true  (g-type-is-object "GtkGrid"))
  ;; Check the registered name
  (is (eq 'gtk-grid
          (registered-object-type-by-name "GtkGrid")))
  ;; Check the type initializer
  (is (string= "GtkGrid"
               (g-type-name (gtype (foreign-funcall "gtk_grid_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkContainer") (g-type-parent "GtkGrid")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkGrid"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'gtype-name (g-type-interfaces "GtkGrid"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "baseline-row" "border-width" "can-default" "can-focus"
               "child" "column-homogeneous" "column-spacing" "composite-child"
               "double-buffered" "events" "expand" "focus-on-click" "halign" "has-default"
               "has-focus" "has-tooltip" "height-request" "hexpand" "hexpand-set" "is-focus"
               "margin" "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "name" "no-show-all" "opacity" "orientation"
               "parent" "receives-default" "resize-mode" "row-homogeneous" "row-spacing"
               "scale-factor" "sensitive" "style" "tooltip-markup" "tooltip-text" "valign"
               "vexpand" "vexpand-set" "visible" "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkGrid"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkGrid"))))
  ;; Get the names of the child properties
  (is (equal '("left-attach" "top-attach" "width" "height")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkGrid"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkGrid" GTK-GRID
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_grid_get_type")
                       ((BASELINE-ROW GTK-GRID-BASELINE-ROW "baseline-row"
                         "gint" T T)
                        (COLUMN-HOMOGENEOUS GTK-GRID-COLUMN-HOMOGENEOUS
                         "column-homogeneous" "gboolean" T T)
                        (COLUMN-SPACING GTK-GRID-COLUMN-SPACING
                         "column-spacing" "gint" T T)
                        (ROW-HOMOGENEOUS GTK-GRID-ROW-HOMOGENEOUS
                         "row-homogeneous" "gboolean" T T)
                        (ROW-SPACING GTK-GRID-ROW-SPACING "row-spacing" "gint"
                         T T)))
             (get-g-type-definition "GtkGrid"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-grid-properties
  (let ((grid (make-instance 'gtk-grid)))
    (is (= 0 (gtk-grid-baseline-row grid)))
    (is-false (gtk-grid-column-homogeneous grid))
    (is (= 0 (gtk-grid-column-spacing grid)))
    (is-false (gtk-grid-row-homogeneous grid))
    (is (= 0 (gtk-grid-row-spacing grid)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-grid-child-properties
  (let ((grid (make-instance 'gtk-grid))
        (child (make-instance 'gtk-button)))
    (is-false (gtk-grid-attach grid child 0 0 1 1 ))
    (is (= 1 (gtk-grid-child-height grid child)))
    (is (= 0 (gtk-grid-child-left-attach grid child)))
    (is (= 0 (gtk-grid-child-top-attach grid child)))
    (is (= 1 (gtk-grid-child-width grid child)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_grid_new

(test gtk-grid-new
  (is (eq 'gtk-grid (type-of (gtk-grid-new)))))

;;;     gtk_grid_attach
;;;     gtk_grid_attach_next_to
;;;     gtk_grid_get_child_at

(test gtk-grid-attach
  (let ((grid (make-instance 'gtk-grid))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button))
        (button4 (make-instance 'gtk-button)))

    (is (= 1 (gobject::ref-count button1)))

    (gtk-grid-attach grid button1 0 0 2 1)
    (gtk-grid-attach grid button2 1 1 1 2)

    (gtk-grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk-grid-attach-next-to grid button4 button2 :left 1 1)

    (is (= 2 (gobject::ref-count button1)))

    (is (equal button1 (gtk-grid-child-at grid 0 0)))
    (let ((button (gtk-grid-child-at grid 0 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 2 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button2 (gtk-grid-child-at grid 1 1)))
    (let ((button (gtk-grid-child-at grid 1 1)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 1 (gtk-grid-child-left-attach grid button)))
      (is (= 1 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 2 (gtk-grid-child-height grid button))))

    (is (equal button3 (gtk-grid-child-at grid 2 0)))
    (let ((button (gtk-grid-child-at grid 2 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 2 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button4 (gtk-grid-child-at grid 0 1)))
    (let ((button (gtk-grid-child-at grid 0 1)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 1 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))
))

;;;     gtk_grid_insert_row
;;;     gtk_grid_insert_column
;;;     gtk_grid_remove_row
;;;     gtk_grid_remove_column

(test gtk-grid-insert
  (let ((grid (make-instance 'gtk-grid))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button))
        (button4 (make-instance 'gtk-button)))

    (is (= 1 (gobject::ref-count button1)))

    (gtk-grid-attach grid button1 0 0 2 1)
    (gtk-grid-attach grid button2 1 1 1 2)

    (gtk-grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk-grid-attach-next-to grid button4 button2 :left 1 1)

    ;; Insert a row and a column
    (gtk-grid-insert-row grid 1)
    (gtk-grid-insert-column grid 1)

    (is (= 2 (gobject::ref-count button1)))

    (is (equal button1 (gtk-grid-child-at grid 0 0)))
    (let ((button (gtk-grid-child-at grid 0 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 3 (gtk-grid-child-width grid button)))  ; new width
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button2 (gtk-grid-child-at grid 2 2)))
    (let ((button (gtk-grid-child-at grid 2 2))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 2 (gtk-grid-child-left-attach grid button)))
      (is (= 2 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 2 (gtk-grid-child-height grid button))))

    (is (equal button3 (gtk-grid-child-at grid 3 0)))
    (let ((button (gtk-grid-child-at grid 3 0))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 3 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button4 (gtk-grid-child-at grid 0 2)))
    (let ((button (gtk-grid-child-at grid 0 2))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 2 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    ;; Remove a row and a column
    (gtk-grid-remove-row grid 1)
    (gtk-grid-remove-column grid 1)

    (is (equal button1 (gtk-grid-child-at grid 0 0)))
    (let ((button (gtk-grid-child-at grid 0 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 2 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button2 (gtk-grid-child-at grid 1 1)))
    (let ((button (gtk-grid-child-at grid 1 1)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 1 (gtk-grid-child-left-attach grid button)))
      (is (= 1 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 2 (gtk-grid-child-height grid button))))

    (is (equal button3 (gtk-grid-child-at grid 2 0)))
    (let ((button (gtk-grid-child-at grid 2 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 2 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button4 (gtk-grid-child-at grid 0 1)))
    (let ((button (gtk-grid-child-at grid 0 1)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 1 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))
))

;;;     gtk_grid_insert_next_to

(test gtk-grid-insert-next-to
  (let ((grid (make-instance 'gtk-grid))
        (button1 (make-instance 'gtk-button))
        (button2 (make-instance 'gtk-button))
        (button3 (make-instance 'gtk-button))
        (button4 (make-instance 'gtk-button)))

    (is (= 1 (gobject::ref-count button1)))

    (gtk-grid-attach grid button1 0 0 2 1)
    (gtk-grid-attach grid button2 1 1 1 2)

    (gtk-grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk-grid-attach-next-to grid button4 button2 :left 1 1)

    (gtk-grid-insert-next-to grid button2 :left)
    (gtk-grid-insert-next-to grid button2 :top)

    (is (= 2 (gobject::ref-count button1)))

    (is (equal button1 (gtk-grid-child-at grid 0 0)))
    (let ((button (gtk-grid-child-at grid 0 0)))

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 3 (gtk-grid-child-width grid button)))  ; new width
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button2 (gtk-grid-child-at grid 2 2)))
    (let ((button (gtk-grid-child-at grid 2 2))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 2 (gtk-grid-child-left-attach grid button)))
      (is (= 2 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 2 (gtk-grid-child-height grid button))))

    (is (equal button3 (gtk-grid-child-at grid 3 0)))
    (let ((button (gtk-grid-child-at grid 3 0))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 3 (gtk-grid-child-left-attach grid button)))
      (is (= 0 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))

    (is (equal button4 (gtk-grid-child-at grid 0 2)))
    (let ((button (gtk-grid-child-at grid 0 2))) ; new position

      (is (= 2 (gobject::ref-count button)))

      (is (= 0 (gtk-grid-child-left-attach grid button)))
      (is (= 2 (gtk-grid-child-top-attach grid button)))
      (is (= 1 (gtk-grid-child-width grid button)))
      (is (= 1 (gtk-grid-child-height grid button))))
))

;;;     gtk_grid_get_row_baseline_position
;;;     gtk_grid_set_row_baseline_position

(test gtk-grid-row-baseline-position
  (let ((grid (make-instance 'gtk-grid))
        (button (make-instance 'gtk-button)))
    (is-false (gtk-grid-attach grid button 0 0 1 1))
    (is (eq :right (gtk-grid-row-baseline-position grid 0)))
    (is (eq :left (setf (gtk-grid-row-baseline-position grid 0) :left)))
    (is (eq :left (gtk-grid-row-baseline-position grid 0)))))

