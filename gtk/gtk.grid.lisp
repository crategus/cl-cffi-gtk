;;; ----------------------------------------------------------------------------
;;; gtk.grid.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.2. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkGrid
;;; 
;;; Pack widgets in a rows and columns
;;;     
;;; Synopsis
;;; 
;;;     GtkGrid
;;;     
;;;     gtk_grid_new
;;;     gtk_grid_attach
;;;     gtk_grid_attach_next_to
;;;     gtk_grid_get_child_at
;;;     gtk_grid_insert_row
;;;     gtk_grid_insert_column
;;;     gtk_grid_insert_next_to
;;;     gtk_grid_set_row_homogeneous
;;;     gtk_grid_get_row_homogeneous
;;;     gtk_grid_set_row_spacing
;;;     gtk_grid_get_row_spacing
;;;     gtk_grid_set_column_homogeneous
;;;     gtk_grid_get_column_homogeneous
;;;     gtk_grid_set_column_spacing
;;;     gtk_grid_get_column_spacing
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkGrid
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkGrid implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Properties
;;; 
;;;   "column-homogeneous"       gboolean              : Read / Write
;;;   "column-spacing"           gint                  : Read / Write
;;;   "row-homogeneous"          gboolean              : Read / Write
;;;   "row-spacing"              gint                  : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "height"                   gint                  : Read / Write
;;;   "left-attach"              gint                  : Read / Write
;;;   "top-attach"               gint                  : Read / Write
;;;   "width"                    gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; GtkGrid is a container which arranges its child widgets in rows and columns.
;;; It is a very similar to GtkTable and GtkBox, but it consistently uses
;;; GtkWidget's "margin" and "expand" properties instead of custom child
;;; properties, and it fully supports height-for-width geometry management.
;;; 
;;; Children are added using gtk_grid_attach(). They can span multiple rows or
;;; columns. It is also possible to add a child next to an existing child, using
;;; gtk_grid_attach_next_to(). The behaviour of GtkGrid when several children
;;; occupy the same grid cell is undefined.
;;; 
;;; GtkGrid can be used like a GtkBox by just using gtk_container_add(), which
;;; will place children next to each other in the direction determined by the
;;; "orientation" property.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "column-homogeneous" property
;;; 
;;;   "column-homogeneous"       gboolean              : Read / Write
;;; 
;;; If TRUE, the columns are all the same width.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "column-spacing" property
;;; 
;;;   "column-spacing"           gint                  : Read / Write
;;; 
;;; The amount of space between two consecutive columns.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-homogeneous" property
;;; 
;;;   "row-homogeneous"          gboolean              : Read / Write
;;; 
;;; If TRUE, the rows are all the same height.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-spacing" property
;;; 
;;;   "row-spacing"              gint                  : Read / Write
;;; 
;;; The amount of space between two consecutive rows.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "height" child property
;;; 
;;;   "height"                   gint                  : Read / Write
;;; 
;;; The number of rows that a child spans.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-attach" child property
;;; 
;;;   "left-attach"              gint                  : Read / Write
;;; 
;;; The column number to attach the left side of the child to.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "top-attach" child property
;;; 
;;;   "top-attach"               gint                  : Read / Write
;;; 
;;; The row number to attach the top side of a child widget to.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "width" child property
;;; 
;;;   "width"                    gint                  : Read / Write
;;; 
;;; The number of columns that a child spans.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGrid
;;; 
;;; struct GtkGrid;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkGrid" 'gtk-grid))

(define-g-object-class "GtkGrid" gtk-grid
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_grid_get_type")
  ((column-homogeneous
    gtk-grid-column-homogeneous
    "column-homogeneous" "gboolean" t t)
   (column-spacing
    gtk-grid-column-spacing
    "column-spacing" "gint" t t)
   (row-homogeneous
    gtk-grid-row-homogeneous
    "row-homogeneous" "gboolean" t t)
   (row-spacing
    gtk-grid-row-spacing
    "row-spacing" "gint" t t)))

;;; ----------------------------------------------------------------------------
    
(define-child-property "GtkGrid"
                       gtk-grid-child-height
                       "height" "gint" t t t)

(define-child-property "GtkGrid"
                       gtk-grid-child-left-attach
                       "left-attach" "gint" t t t)

(define-child-property "GtkGrid"
                       gtk-grid-child-top-attach
                       "top-attach" "gint" t t t)

(define-child-property "GtkGrid"
                       gtk-grid-child-width
                       "width" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_new ()
;;; 
;;; GtkWidget * gtk_grid_new (void);
;;; 
;;; Creates a new grid widget.
;;; 
;;; Returns :
;;;     the new GtkGrid
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-new))

(defun gtk-grid-new ()
  (make-instance 'gtk-grid))

(export 'gtk-grid-new)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach ()
;;; 
;;; void gtk_grid_attach (GtkGrid *grid,
;;;                       GtkWidget *child,
;;;                       gint left,
;;;                       gint top,
;;;                       gint width,
;;;                       gint height);
;;; 
;;; Adds a widget to the grid.
;;; 
;;; The position of child is determined by left and top. The number of 'cells'
;;; that child will occupy is determined by width and height.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; child :
;;;     the widget to add
;;; 
;;; left :
;;;     the column number to attach the left side of child to
;;; 
;;; top :
;;;     the row number to attach the top side of child to
;;; 
;;; width :
;;;     the number of columns that child will span
;;; 
;;; height :
;;;     the number of rows that child will span
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_attach" gtk-grid-attach) :void
  (grid (g-object gtk-grid))
  (child (g-object gtk-widget))
  (left :int)
  (top :int)
  (width :int)
  (height :int))

(export 'gtk-grid-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach_next_to ()
;;; 
;;; void gtk_grid_attach_next_to (GtkGrid *grid,
;;;                               GtkWidget *child,
;;;                               GtkWidget *sibling,
;;;                               GtkPositionType side,
;;;                               gint width,
;;;                               gint height);
;;; 
;;; Adds a widget to the grid.
;;; 
;;; The widget is placed next to sibling, on the side determined by side. When
;;; sibling is NULL, the widget is placed in row (for left or right placement)
;;; or column 0 (for top or bottom placement), at the end indicated by side.
;;; 
;;; Attaching widgets labeled [1], [2], [3] with sibling == NULL and
;;; side == GTK_POS_LEFT yields a layout of [3][2][1].
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; child :
;;;     the widget to add
;;; 
;;; sibling :
;;;     the child of grid that child will be placed next to, or NULL to place
;;;     child at the beginning or end
;;; 
;;; side :
;;;     the side of sibling that child is positioned next to
;;; 
;;; width :
;;;     the number of columns that child will span
;;; 
;;; height :
;;;     the number of rows that child will span
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_attach_next_to" gtk-grid-attach-next-to) :void
  (grid (g-object gtk-grid))
  (child (g-object gtk-widget))
  (sibling (g-object gtk-widget))
  (side gtk-position-type)
  (width :int)
  (height :int))

(export 'gtk-grid-attach-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_child_at ()
;;; 
;;; GtkWidget * gtk_grid_get_child_at (GtkGrid *grid, gint left, gint top);
;;; 
;;; Gets the child of grid whose area covers the grid cell whose upper left
;;; corner is at left, top.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; left :
;;;     the left edge of the cell
;;; 
;;; top :
;;;     the top edge of the cell
;;; 
;;; Returns :
;;;     the child at the given position, or NULL
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_get_child_at" gtk-grid-get-child-at) (g-object gtk-widget)
  (grid (g-object gtk-grid))
  (left :int)
  (top :int))

(export 'gtk-grid-get-child-at)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_row ()
;;; 
;;; void gtk_grid_insert_row (GtkGrid *grid, gint position);
;;; 
;;; Inserts a row at the specified position.
;;; 
;;; Children which are attached at or below this position are moved one row
;;; down. Children which span across this position are grown to span the new
;;; row.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; position :
;;;     the position to insert the row at
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_row" gtk-grid-insert-row) :void
  (grid (g-object gtk-grid))
  (position :int))

(export 'gtk-grid-insert-row)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_column ()
;;; 
;;; void gtk_grid_insert_column (GtkGrid *grid, gint position);
;;; 
;;; Inserts a column at the specified position.
;;; 
;;; Children which are attached at or to the right of this position are moved
;;; one column to the right. Children which span across this position are grown
;;; to span the new column.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; position :
;;;     the position to insert the column at
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_column" gtk-grid-insert-column) :void
  (grid (g-object gtk-grid))
  (position :int))

(export 'gtk-grid-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_next_to ()
;;; 
;;; void gtk_grid_insert_next_to (GtkGrid *grid,
;;;                               GtkWidget *sibling,
;;;                               GtkPositionType side);
;;; 
;;; Inserts a row or column at the specified position.
;;; 
;;; The new row or column is placed next to sibling, on the side determined by
;;; side. If side is GTK_POS_TOP or GTK_POS_BOTTOM, a row is inserted. If side
;;; is GTK_POS_LEFT of GTK_POS_RIGHT, a column is inserted.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; sibling :
;;;     the child of grid that the new row or column will be placed next to
;;; 
;;; side :
;;;     the side of sibling that child is positioned next to
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_next_to" gtk-grid-insert-next-to) :void
  (grid (g-object gtk-grid))
  (sibling (g-object gtk-widget))
  (side gtk-position-type))

(export 'gtk-grid-insert-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_set_row_homogeneous ()
;;; 
;;; void gtk_grid_set_row_homogeneous (GtkGrid *grid, gboolean homogeneous);
;;; 
;;; Sets whether all rows of grid will have the same height.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; homogeneous :
;;;     TRUE to make rows homogeneous
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-set-row-homogeneous))

(defun gtk-grid-set-row-homogeneous (grid homogeneous)
  (setf (gtk-grid-row-homogeneous grid) homogeneous))

(export 'gtk-grid-set-row-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_row_homogeneous ()
;;; 
;;; gboolean gtk_grid_get_row_homogeneous (GtkGrid *grid);
;;; 
;;; Returns whether all rows of grid have the same height.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; Returns :
;;;     whether all rows of grid have the same height
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-get-row-homogeneous))

(defun gtk-grid-get-row-homogeneous (grid)
  (gtk-grid-row-homogeneous grid))

(export 'gtk-grid-get-row-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_set_row_spacing ()
;;; 
;;; void gtk_grid_set_row_spacing (GtkGrid *grid, guint spacing);
;;; 
;;; Sets the amount of space between rows of grid.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; spacing :
;;;     the amount of space to insert between rows
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-set-row-spacing))

(defun gtk-grid-set-row-spacing (grid spacing)
  (setf (gtk-grid-row-spacing grid) spacing))

(export 'gtk-grid-set-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_row_spacing ()
;;; 
;;; guint gtk_grid_get_row_spacing (GtkGrid *grid);
;;; 
;;; Returns the amount of space between the rows of grid.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; Returns :
;;;     the row spacing of grid
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-get-row-spacing))

(defun gtk-grid-get-row-spacing (grid)
  (gtk-grid-row-spacing grid))

(export 'gtk-grid-get-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_set_column_homogeneous ()
;;; 
;;; void gtk_grid_set_column_homogeneous (GtkGrid *grid, gboolean homogeneous);
;;; 
;;; Sets whether all columns of grid will have the same width.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; homogeneous :
;;;     TRUE to make columns homogeneous
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-set-column-homogeneous))

(defun gtk-grid-set-column-homogeneous (grid homogeneous)
  (setf (gtk-grid-column-homogeneous grid) homogeneous))

(export 'gtk-grid-set-column-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_column_homogeneous ()
;;; 
;;; gboolean gtk_grid_get_column_homogeneous (GtkGrid *grid);
;;; 
;;; Returns whether all columns of grid have the same width.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; Returns :
;;;     whether all columns of grid have the same width
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-get-column-homogeneous))

(defun gtk-grid-get-column-homogeneous (grid)
  (gtk-grid-column-homogeneous grid))

(export 'gtk-grid-get-column-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_set_column_spacing ()
;;; 
;;; void gtk_grid_set_column_spacing (GtkGrid *grid, guint spacing);
;;; 
;;; Sets the amount of space between columns of grid.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; spacing :
;;;     the amount of space to insert between columns
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-set-column-spacing))

(defun gtk-grid-set-column-spacing (grid spacing)
  (setf (gtk-grid-column-spacing grid) spacing))

(export 'gtk-grid-set-column-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_column_spacing ()
;;; 
;;; guint gtk_grid_get_column_spacing (GtkGrid *grid);
;;; 
;;; Returns the amount of space between the columns of grid.
;;; 
;;; grid :
;;;     a GtkGrid
;;; 
;;; Returns :
;;;     the column spacing of grid
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-get-column-spacing))

(defun gtk-grid-get-column-spacing (grid)
  (gtk-grid-column-spacing grid))

(export 'gtk-grid-get-column-spacing)

;;; --- End of file gtk.grid.lisp ----------------------------------------------
