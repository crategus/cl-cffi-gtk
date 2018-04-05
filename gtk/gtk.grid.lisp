;;; ----------------------------------------------------------------------------
;;; gtk.grid.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013, 2014 Dieter Kaiser
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
;;;     gtk_grid_remove_row
;;;     gtk_grid_remove_column
;;;     gtk_grid_insert_next_to
;;;     gtk_grid_set_row_homogeneous
;;;     gtk_grid_get_row_homogeneous
;;;     gtk_grid_set_row_spacing
;;;     gtk_grid_get_row_spacing
;;;     gtk_grid_set_column_homogeneous
;;;     gtk_grid_get_column_homogeneous
;;;     gtk_grid_set_column_spacing
;;;     gtk_grid_get_column_spacing
;;;     gtk_grid_get_baseline_row
;;;     gtk_grid_set_baseline_row
;;;     gtk_grid_get_row_baseline_position
;;;     gtk_grid_set_row_baseline_position
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGrid
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
  (#+gtk-3-10
   (baseline-row
    gtk-grid-baseline-row
    "baseline-row" "gint" t t)
   (column-homogeneous
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-grid 'type)
 "@version{2013-12-7}
  @begin{short}
    @sym{gtk-grid} is a container which arranges its child widgets in rows and
    columns.
  @end{short}
  It is a very similar to @class{gtk-table} and @class{gtk-box}, but it
  consistently uses @class{gtk-widget}'s @code{\"margin\"} and @code{\"expand\"}
  properties instead of custom child properties, and it fully supports
  height-for-width geometry management.

  Children are added using the function @fun{gtk-grid-attach}. They can span
  multiple rows or columns. It is also possible to add a child next to an
  existing child, using the function @fun{gtk-grid-attach-next-to}. The
  behaviour of @sym{gtk-grid} when several children occupy the same grid cell is
  undefined.

  @sym{gtk-grid} can be used like a @class{gtk-box} by just using
  @fun{gtk-container-add}, which will place children next to each other in the
  direction determined by the @code{\"orientation\"} property.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"height\" child property}
      @code{\"height\"} of type @code{:int} (Read / Write) @br{}
      The number of rows that a child spans. @br{}
      Allowed values: >= 1 @br{}
      Default value: 1

    @subheading{The \"left-attach\" child property}
      @code{\"left-attach\"} of type @code{:int} (Read / Write) @br{}
      The column number to attach the left side of the child to. @br{}
      Default value: 0

    @subheading{The \"top-attach\" child property}
      @code{\"top-attach\"} of type @code{:int} (Read / Write) @br{}
      The row number to attach the top side of a child widget to. @br{}
      Default value: 0

    @subheading{The \"width\" child property}
      @code{\"width\"} of type @code{:int} (Read / Write) @br{}
      The number of columns that a child spans. @br{}
      Allowed values: >= 1 @br{}
      Default value: 1
  @end{dictionary}
  @see-slot{gtk-grid-baseline-row}
  @see-slot{gtk-grid-column-homogeneous}
  @see-slot{gtk-grid-column-spacing}
  @see-slot{gtk-grid-row-homogeneous}
  @see-slot{gtk-grid-row-spacing}
  @see-class{gtk-table}
  @see-class{gtk-box}
  @see-function{gtk-grid-attach}
  @see-function{gtk-grid-attach-next-to}
  @see-function{gtk-container-add}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "baseline-row"
                                               'gtk-grid) 't)
 "The @code{\"baseline-row\"} property of type @code{:int}
  (Read / Write) @br{}
  The row to align the to the baseline when valign has the value @code{:center}
  of the @symbol{gtk-align} enumeration. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-grid-baseline-row atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-baseline-row 'function)
 "@version{2014-7-26}
  @argument[object]{a @class{gtk-grid} container}
  @argument[row]{the row index}
  @syntax[]{(gtk-grid-baseline-row object) => row}
  @syntax[]{(setf (gtk-grid-baseline-row object) row)}
  @begin{short}
    Accessor of the slot @slot[gtk-grid]{baseline-row} of the @class{gtk-grid}
    class.
  @end{short}

  The generic function @sym{gtk-grid-baseline-row} returns which row defines
  the global baseline of the grid.

  The generic function @sym{(setf gtk-grid-baseline-row)} sets which row defines
  the global baseline for the entire grid. Each row in the grid can have its own
  local baseline, but only one of those is global, meaning it will be the
  baseline in the parent of the the grid.

  Since 3.10
  @see-class{gtk-grid}
  @see-symbol{gtk-align}")

;;; --- gtk-grid-column-homogeneous --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-homogeneous"
                                               'gtk-grid) 't)
 "The @code{\"column-homogeneous\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the columns are all the same width. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-column-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-column-homogeneous 'function)
 "@version{2014-2-21}
  @argument[object]{a @class{gtk-grid} container}
  @argument[homogeneous]{@em{true} to make columns homogeneous}
  @syntax[]{(gtk-grid-column-homogeneous object) => homogenous}
  @syntax[]{(setf (gtk-grid-column-homogeneous object) homogenous)}
  @begin{short}
    Accessor of the slot @slot[gtk-grid]{column-homogeneous} of the
    @class{gtk-grid} class.
  @end{short}

  The generic function @sym{gtk-grid-column-homogeneous} returns whether all
  columns of @arg{grid} have the same width.

  The generic function @sym{(setf gtk-grid-column-homogeneous)} sets whether
  all columns of @arg{grid} will have the same width.
  @see-class{gtk-grid}")

;;; --- gtk-grid-column-spacing ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-spacing" 'gtk-grid) 't)
 "The @code{\"column-spacing\"} property of type @code{:int}
  (Read / Write) @br{}
  The amount of space between two consecutive columns. @br{}
  Allowed values: [0, 32767] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-column-spacing 'function)
 "@version{2014-2-21}
  @argument[object]{a @class{gtk-grid} container}
  @argument[spacing]{the amount of space to insert between columns}
  @syntax[]{(gtk-grid-column-spacing object) => spacing}
  @syntax[]{(setf (gtk-grid-column-spacing object) spacing)}
  @begin{short}
    Accessor of the slot @slot[gtk-grid]{column-spacing} of the @class{gtk-grid}
    class.
  @end{short}

  The generic function @sym{gtk-grid-column-spacing} returns the amount of space
  between the columns of @arg{grid}.

  The generic function @sym{(setf gtk-grid-column-spacing} sets the amount of
  space between columns of @arg{grid}.
  @see-class{gtk-grid}")

;;; --- gtk-grid-row-homogeneous -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-homogeneous" 'gtk-grid) 't)
 "The @code{\"row-homogeneous\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the rows are all the same height. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-row-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-row-homogeneous 'function)
 "@version{2014-2-26}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[homogeneous]{@em{true} to make rows homogeneous}
  @syntax[]{(gtk-grid-row-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-grid-row-homogeneous object) homogeneous)}
  @begin{short}
    Accessor of the slot @slot[gtk-grid]{row-homogeneous} of the
    @class{gtk-grid} class.
  @end{short}

  The generic function @sym{gtk-grid-row-homogeneous} returns whether all rows
  of @arg{grid} have the same height.

  The generic function @sym{(setf gtk-grid-row-homogeneous} sets whether all
  rows of @arg{grid} will have the same height.
  @see-class{gtk-grid}")

;;; --- gtk-grid-row-spacing ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-spacing" 'gtk-grid) 't)
 "The @code{\"row-spacing\"} property of type @code{:int} (Read / Write) @br{}
  The amount of space between two consecutive rows. @br{}
  Allowed values: [0, 32767] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-row-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-row-spacing 'function)
 "@version{2014-2-26}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[spacing]{the amount of space to insert between rows}
  @syntax[]{(gtk-grid-row-spacing object) => spacing}
  @syntax[]{(setf (gtk-grid-row-spacing object) spacing)}
  @begin{short}
    Accessor of the slot @slot[gtk-grid]{row-spacing} of the @class{gtk-grid}
    class.
  @end{short}

  The generic function @sym{gtk-grid-row-spacing} returns the amount of space
  between the rows of @arg{grid}.

  The generic function @sym{(setf gtk-grid-row-spacing} sets the amount of
  space between rows of @arg{grid}.
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkGrid"
                       gtk-grid-child-height
                       "height" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-child-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-child-height 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"height\"} of the @class{gtk-grid}
  class.
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkGrid"
                       gtk-grid-child-left-attach
                       "left-attach" "gint" t t t)


#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-child-left-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-child-left-attach 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"left-attach\"} of the
  @class{gtk-grid} class.
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkGrid"
                       gtk-grid-child-top-attach
                       "top-attach" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-child-top-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-child-top-attach 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"top-attach\"} of the
  @class{gtk-grid} class.
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkGrid"
                       gtk-grid-child-width
                       "width" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-grid-child-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-grid-child-width 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"width\"} of the @class{gtk-grid}
  class.
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------
;;; gtk_grid_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-grid-new))

(defun gtk-grid-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @return{The new @class{gtk-grid} container.}
  @short{Creates a new grid container.}
  @see-class{gtk-grid}"
  (make-instance 'gtk-grid))

(export 'gtk-grid-new)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_attach" gtk-grid-attach) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[child]{the widget to add}
  @argument[left]{the column number to attach the left side of @arg{child} to}
  @argument[top]{the row number to attach the top side of @arg{child} to}
  @argument[width]{the number of columns that @arg{child} will span}
  @argument[height]{the number of rows that @arg{child} will span}
  @short{Adds a @arg{child} widget to the @arg{grid}.}

  The position of @arg{child} is determined by @arg{left} and @arg{top}. The
  number of \"cells\" that @arg{child} will occupy is determined by @arg{width}
  and @arg{height}.
  @see-class{gtk-grid}
  @see-class{gtk-widget}
  @see-function{gtk-grid-attach-next-to}"
  (grid (g-object gtk-grid))
  (child (g-object gtk-widget))
  (left :int)
  (top :int)
  (width :int)
  (height :int))

(export 'gtk-grid-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_attach_next_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_attach_next_to" gtk-grid-attach-next-to) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[child]{the widget to add}
  @argument[sibling]{the child of grid that @arg{child} will be placed next to,
    or @code{nil} to place @arg{child} at the beginning or end}
  @argument[side]{the side of @arg{sibling} of type @symbol{gtk-position-type}
    that @arg{child} is positioned next to}
  @argument[width]{the number of columns that @arg{child} will span}
  @argument[height]{the number of rows that @arg{child} will span}
  @short{Adds a @arg{child} widget to the @arg{grid}.}

  The @arg{child} widget is placed next to @arg{sibling}, on the side determined
  by @arg{side}. When @arg{sibling} is @code{nil}, the @arg{child} widget is
  placed in row (for left or right placement) or column 0 (for top or bottom
  placement), at the end indicated by @arg{side}.

  Attaching widgets labeled [1], [2], [3] with @arg{sibling} = @code{nil} and
  @arg{side} = @code{:left} yields a layout of [3][2][1].
  @see-class{gtk-grid}
  @see-class{gtk-widget}
  @see-symbol{gtk-position-type}
  @see-function{gtk-grid-attach}"
  (grid (g-object gtk-grid))
  (child (g-object gtk-widget))
  (sibling (g-object gtk-widget))
  (side gtk-position-type)
  (width :int)
  (height :int))

(export 'gtk-grid-attach-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_child_at ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_get_child_at" gtk-grid-get-child-at) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[left]{the left edge of the cell}
  @argument[top]{the top edge of the cell}
  @return{The child at the given position, or @code{nil}.}
  @begin{short}
    Gets the child of @arg{grid} whose area covers the grid cell whose upper
    left corner is at @arg{left}, @arg{top}.
  @end{short}

  Since 3.2
  @see-class{gtk-grid}
  @see-class{gtk-widget}"
  (grid (g-object gtk-grid))
  (left :int)
  (top :int))

(export 'gtk-grid-get-child-at)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_row" gtk-grid-insert-row) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[position]{the position to insert the row at}
  @short{Inserts a row at the specified @arg{position}.}

  Children which are attached at or below this @arg{position} are moved one row
  down. Children which span across this @arg{position} are grown to span the new
  row.

  Since 3.2
  @see-class{gtk-grid}
  @see-function{gtk-grid-insert-column}
  @see-function{gtk-grid-insert-next-to}"
  (grid (g-object gtk-grid))
  (position :int))

(export 'gtk-grid-insert-row)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_column" gtk-grid-insert-column) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[position]{the position to insert the column at}
  @short{Inserts a column at the specified @arg{position}.}

  Children which are attached at or to the right of this @arg{position} are
  moved one column to the right. Children which span across this @arg{position}
  are grown to span the new column.

  Since 3.2
  @see-class{gtk-grid}
  @see-function{gtk-grid-insert-row}
  @see-function{gtk-grid-insert-next-to}"
  (grid (g-object gtk-grid))
  (position :int))

(export 'gtk-grid-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_insert_next_to ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grid_insert_next_to" gtk-grid-insert-next-to) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-7}
  @argument[grid]{a @class{gtk-grid} container}
  @argument[sibling]{the child of @arg{grid} that the new row or column will be
    placed next to}
  @argument[side]{the side of type @symbol{gtk-position-type} of @arg{sibling}
    that child is positioned next to}
  @short{Inserts a row or column at the specified position.}

  The new row or column is placed next to @arg{sibling}, on the side determined
  by @arg{side}. If side is @code{:top} or @code{:bottom}, a row is inserted.
  If @arg{side} is @code{:left} of @code{:right}, a column is inserted.

  Since 3.2
  @see-class{gtk-grid}
  @see-class{gtk-widget}
  @see-symbol{gtk-position-type}
  @see-function{gtk-grid-insert-column}
  @see-function{gtk-grid-insert-row}"
  (grid (g-object gtk-grid))
  (sibling (g-object gtk-widget))
  (side gtk-position-type))

(export 'gtk-grid-insert-next-to)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_get_row_baseline_position ()
;;; 
;;; GtkBaselinePosition gtk_grid_get_row_baseline_position (GtkGrid *grid,
;;;                                                         gint row);
;;;
;;; Returns the baseline position of row as set by
;;; gtk_grid_set_row_baseline_position() or the default value
;;; GTK_BASELINE_POSITION_CENTER.
;;;
;;; grid
;;;     a GtkGrid
;;;
;;; row
;;;     a row index
;;;
;;; Returns
;;;     the baseline position of row
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_grid_set_row_baseline_position ()
;;;
;;; void gtk_grid_set_row_baseline_position (GtkGrid *grid,
;;;                                          gint row,
;;;                                          GtkBaselinePosition pos);
;;;
;;; Sets how the baseline should be positioned on row of the grid, in case that
;;; row is assigned more space than is requested.
;;;
;;; grid
;;;     a GtkGrid
;;;
;;; row
;;;     a row index
;;;
;;; pos
;;;     a GtkBaselinePosition
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.grid.lisp ----------------------------------------------
