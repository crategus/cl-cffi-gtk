;;; ----------------------------------------------------------------------------
;;; gtk.grid.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.2. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; Child Properties
;;; 
;;;   "height"                   gint                  : Read / Write
;;;   "left-attach"              gint                  : Read / Write
;;;   "top-attach"               gint                  : Read / Write
;;;   "width"                    gint                  : Read / Write
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

;;; --- gtk-grid ---------------------------------------------------------------

(setf (documentation 'gtk-grid 'type)
 "@version{2013-1-9}
  @short{Pack widgets in a rows and columns.}

  @sym{gtk-grid} is a container which arranges its child widgets in rows and
  columns. It is a very similar to @class{gtk-table} and @class{gtk-box}, but it
  consistently uses @class{gtk-widget}'s \"margin\" and \"expand\" properties
  instead of custom child properties, and it fully supports height-for-width
  geometry management.

  Children are added using @fun{gtk-grid-attach}. They can span multiple rows or
  columns. It is also possible to add a child next to an existing child, using
  @fun{gtk-grid-attach-next-to}. The behaviour of @sym{gtk-grid} when several
  children occupy the same grid cell is undefined.

  @sym{gtk-grid} can be used like a @class{gtk-box} by just using
  @fun{gtk-container-add}, which will place children next to each other in the
  direction determined by the \"orientation\" property.
  @see-slot{gtk-grid-column-homogeneous}
  @see-slot{gtk-grid-column-spacing}
  @see-slot{gtk-grid-row-homogeneous}
  @see-slot{gtk-grid-row-spacing}
")

#|
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
|#

;;; --- gtk-grid-new -----------------------------------------------------------

(setf (documentation 'gtk-grid-new 'function)
 "@version{2013-1-9}
  @return{The new @class{gtk-grid} instance.}
  @short{Creates a new grid widget.}")

;;; --- gtk-grid-attach --------------------------------------------------------

(setf (documentation 'gtk-grid-attach 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[child]{the widget to add}
  @argument[left]{the column number to attach the left side of @arg{child} to}
  @argument[top]{the row number to attach the top side of @arg{child} to}
  @argument[width]{the number of columns that @arg{child} will span}
  @argument[height]{the number of rows that @arg{child} will span}
  @short{Adds a widget to the grid.}

  The position of @arg{child} is determined by @arg{left} and @arg{top}. The
  number of \"cells\" that @arg{child} will occupy is determined by @arg{width}
  and @arg{height}.")

;;; --- gtk-grid-attach-next-to ------------------------------------------------

(setf (documentation 'gtk-grid-attach-next-to 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[child]{the widget to add}
  @argument[sibling]{the child of grid that @arg{child} will be placed next to,
    or @code{nil} to place @arg{child} at the beginning or end}
  @argument[side]{the side of @arg{sibling} of type @symbol{gtk-position-type}
    that @arg{child} is positioned next to}
  @argument[width]{the number of columns that @arg{child} will span}
  @argument[height]{the number of rows that @arg{child} will span}
  @short{Adds a widget to the grid.}

  The widget is placed next to @arg{sibling}, on the side determined by
  @arg{side}. When @arg{sibling} is @code{nil}, the widget is placed in row (for
  left or right placement) or column 0 (for top or bottom placement), at the end
  indicated by side.

  Attaching widgets labeled [1], [2], [3] with @arg{sibling} = @code{nil} and
  @arg{side} = @code{GTK_POS_LEFT} yields a layout of [3][2][1].")

;;; --- gtk-grid-get-child-at --------------------------------------------------

(setf (documentation 'gtk-grid-get-child-at 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[left]{the left edge of the cell}
  @argument[top]{the top edge of the cell}
  @return{The child at the given position, or @code{nil}.}
  @begin{short}
    Gets the child of @arg{grid} whose area covers the grid cell whose upper
    left corner is at @arg{left}, @arg{top}.
  @end{short}

  Since 3.2")

;;; --- gtk-grid-insert-row ----------------------------------------------------

(setf (documentation 'gtk-grid-insert-row 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[position]{the position to insert the row at}
  @short{Inserts a row at the specified @arg{position}.}

  Children which are attached at or below this @arg{position} are moved one row
  down. Children which span across this @arg{position} are grown to span the new
  row.

  Since 3.2")

;;; --- gtk-grid-insert-column -------------------------------------------------

(setf (documentation 'gtk-grid-insert-column 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[position]{the position to insert the column at}
  @short{Inserts a column at the specified position.}

  Children which are attached at or to the right of this @arg{position} are
  moved one column to the right. Children which span across this @arg{position}
  are grown to span the new column.

 Since 3.2")

;;; --- gtk-grid-insert-next-to ------------------------------------------------

(setf (documentation 'gtk-grid-insert-next-to 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[sibling]{the child of @arg{grid} that the new row or column will be
    placed next to}
  @argument[side]{the side of type @symbol{gtk-position-type} of @arg{sibling}
    that child is positioned next to}
  @short{Inserts a row or column at the specified position.}

  The new row or column is placed next to @arg{sibling}, on the side determined
  by @arg{side}. If side is @code{GTK_POS_TOP} or @code{GTK_POS_BOTTOM}, a row
  is inserted. If @arg{side} is @code{GTK_POS_LEFT} of @code{GTK_POS_RIGHT}, a
  column is inserted.

  Since 3.2")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-grid-set-row-homogeneous -------------------------------------------

(setf (documentation 'gtk-grid-set-row-homogeneous 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[homogeneous]{@em{true} to make rows homogeneous}
  @short{Sets whether all rows of @arg{grid} will have the same height.}")

;;; --- gtk-grid-get-row-homogenous --------------------------------------------

(setf (documentation 'gtk-grid-get-row-homogenous 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @return{Whether all rows of @arg{grid} have the same height.}
  @short{Returns whether all rows of @arg{grid} have the same height.")

;;; --- gtk-grid-set-row-spacing -----------------------------------------------

(setf (documentation 'gtk-grid-set-row-spacing 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @argument[spacing]{the amount of space to insert between rows}
  @short{Sets the amount of space between rows of @arg{grid}.}")

;;; --- gtk-grid-get-row-spacing -----------------------------------------------

(setf (documentation 'gtk-grid-get-row-spacing 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @return{The row spacing of @arg{grid}.}
  @short{Returns the amount of space between the rows of @arg{grid}.}")

;;; --- gtk-grid-set-column-homogenous -----------------------------------------

(setf (documentation 'gtk-grid-set-column-homogenous 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @arg[homogeneous]{@em{true} to make columns homogeneous}
  @short{Sets whether all columns of @arg{grid} will have the same width.")

;;; --- gtk-grid-get-column-homogenous -----------------------------------------

(setf (documentation 'gtk-grid-get-column-homogenous 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @return{Whether all columns of @arg{grid} have the same width.}
  @short{Returns whether all columns of @arg{grid} have the same width.")

;;; --- gtk-grid-set-column-spacing --------------------------------------------

(setf (documentation 'gtk-grid-set-column-spacing 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @arg[spacing]{the amount of space to insert between columns}
  @short{Sets the amount of space between columns of @arg{grid}.}")

;;; --- gtk-grid-get-column-spacing --------------------------------------------

(setf (documentation 'gtk-grid-get-column-spacing 'function)
 "@version{2013-1-9}
  @argument[grid]{a @class{gtk-grid} instance}
  @return{The column spacing of @arg{grid}.}
  @short{Returns the amount of space between the columns of @arg{grid}.}")

;;; --- End of file atdoc-gtk.grid.lisp ----------------------------------------
