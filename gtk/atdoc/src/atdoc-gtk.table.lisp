;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.table.lisp
;;;
;;; Documentation strings for the library GTK+
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

(in-package :gtk)

;;; --- gtk-table --------------------------------------------------------------

(setf (documentation 'gtk-table 'type)
 "@version{2013-1-22}
  @begin{short}
    The @sym{gtk-table} functions allow the programmer to arrange widgets in
    rows and columns, making it easy to align many widgets next to each other,
    horizontally and vertically.
  @end{short}

  Tables are created with a call to @fun{gtk-table-new}, the size of which can
  later be changed with @fun{gtk-table-resize}.
 
  Widgets can be added to a table using @fun{gtk-table-attach} or the more
  convenient (but slightly less flexible) @fun{gtk-table-attach-defaults}.

  To alter the space next to a specific row, use
  @fun{gtk-table-set-row-spacing}, and for a column,
  @fun{gtk-table-set-col-spacing}. The gaps between all rows or
  columns can be changed by calling @fun{gtk-table-set-row-spacings} or
  @fun{gtk-table-set-col-spacings} respectively. Note that spacing is added
  between the children, while padding added by @fun{gtk-table-attach} is added
  on either side of the widget it belongs to.

  @fun{gtk-table-set-homogeneous}, can be used to set whether all cells in the
  table will resize themselves to the size of the largest widget in the table.

  @b{Note}

  @sym{gtk-table} has been deprecated. Use @class{gtk-grid} instead. It provides
  the same capabilities as @sym{gtk-table} for arranging widgets in a
  rectangular grid, but does support height-for-width geometry management.

  @begin[Child Property Details]{dictionary}
    @subheading{The \"bottom-attach\" child property}
    @code{\"bottom-attach\"} @code{guint} (Read / Write)@br{}
    The row number to attach the bottom of the child to.@br{}
    Allowed values: @code{[1,65535]}@br{}
    Default value: @code{1}

    @subheading{The \"left-attach\" child property}
    @code{\"left-attach\"} @code{guint} (Read / Write)@br{}
    The column number to attach the left side of the child to.@br{}
    Allowed values: @code{<= 65535}@br{}
    Default value: @code{0}

    @subheading{The \"right-attach\" child property}
    @code{\"right-attach\"} @code{guint} (Read / Write)@br{}
    The column number to attach the right side of a child widget to.@br{}
    Allowed values: @code{[1,65535]}@br{}
    Default value: @code{1}

    @subheading{The \"top-attach\" child property}
    @code{\"top-attach\"} @code{guint} (Read / Write)@br{}
    The row number to attach the top of a child widget to.@br{}
    Allowed values: @code{<= 65535}
    Default value: @code{0}

    @subheading{The \"x-options\" child property}
    @code{\"x-options\"} @symbol{gtk-attach-options} (Read / Write)@br{}
    Options specifying the horizontal behaviour of the child.@br{}
    Default value: @code{'(:expand :fill)}

    @subheading{The \"x-padding\" child property}
    @code{\"x-padding\"} @code{guint} (Read / Write)@br{}
    Extra space to put between the child and its left and right neighbors, in
    pixels.@br{}
    Allowed values: @code{<= 65535}@br{}
    Default value: @code{0}

    @subheading{The \"y-options\" child property}
    @code{\"y-options\"} @symbol{gtk-attach-options} (Read / Write)@br{}
    Options specifying the vertical behaviour of the child.@br{}
    Default value: @code{'(:expand :fill)}

    @subheading{The \"y-padding\" child property}
    @code{\"y-padding\"} @code{guint} (Read / Write)@br{}
    Extra space to put between the child and its upper and lower neighbors, in
    pixels.@br{}
    Allowed values: @code{<= 65535}@br{}
    Default value: @code{0}
  @end{dictionary}
  @see-slot{gtk-table-column-spacing}
  @see-slot{gtk-table-homogeneous}
  @see-slot{gtk-table-n-columns}
  @see-slot{gtk-table-n-rows}
  @see-slot{gtk-table-row-spacing}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "column-spacing" 'gtk-table) 't)
 "The @code{column-spacing} property of type @code{guint} (Read / Write)@br{}
  The amount of space between two consecutive columns.@br{}
  Allowed values: @code{<= 65535}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "homogeneous" 'gtk-table) 't)
 "The @code{homogeneous} property of type @code{gboolean} (Read / Write)@br{}
  If @arg{true}, the table cells are all the same width/height.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "n-columns" 'gtk-table) 't)
 "The @code{n-columns} property of type @code{guint} (Read / Write)@br{}
  The number of columns in the table.@br{}
  Allowed values: @code{[1,65535]}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "n-rows" 'gtk-table) 't)
 "The @code{n-rows} property of type @code{guint} (Read / Write)@br{}
  The number of rows in the table.@br{}
  Allowed values: @code{[1,65535]}@br{}
  Default value: @code{1}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "row-spacing" 'gtk-table) 't)
 "The @code{row-spacing} property of type @code{guint}  (Read / Write)@br{}
  The amount of space between two consecutive rows.@br{}
  Allowed values: @code{<= 65535}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-table-column-spacing -----------------------------------------------

(setf (gethash 'gtk-table-column-spacing atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-table-column-spacing 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{column-spacing} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-homogeneous -------------------------------------------

(setf (gethash 'gtk-table-homogeneous atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-table-homogeneous 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{homogeneous} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-n-columns ----------------------------------------------------

(setf (gethash 'gtk-table-n-columns atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-table-n-columns 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{n-columns} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-n-rows -------------------------------------------------------

(setf (gethash 'gtk-table-n-rows atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-table-n-rows 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{n-rows} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-row-spacing --------------------------------------------------

(setf (gethash 'gtk-table-row-spacing atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-table-row-spacing 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{row-spacing} of the @class{gtk-table} class.
  @end{short}")

#|
(define-child-property "GtkTable"
                       gtk-table-child-left-attach
                       "left-attach" "guint" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-right-attach
                       "right-attach" "guint" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-top-attach
                       "top-attach" "guint" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-bottom-attach
                       "bottom-attach" "guint" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-x-options
                       "x-options" "GtkAttachOptions" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-y-options
                       "y-options" "GtkAttachOptions" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-x-padding
                       "x-padding" "guint" t t t)

(define-child-property "GtkTable"
                       gtk-table-child-y-padding
                       "y-padding" "guint" t t t)
|#

;;; --- gtk-table-new ----------------------------------------------------------

(setf (documentation 'gtk-table-new 'function)
 "@version{2013-1-22}
  @argument[rows]{The number of rows the new table should have.}
  @argument[columns]{The number of columns the new table should have.}
  @argument[homogeneous]{If set to TRUE, all table cells are resized to the size
    of the cell containing the largest widget.}
  @return{The the newly created table widget.}
  @begin{short}
    Used to create a new table widget.
  @end{short}
  An initial size must be given by specifying how many rows and columns the
  table should have, although this can be changed later with gtk_table_resize().
  rows and columns must both be in the range 1 .. 65535. For historical reasons,
  0 is accepted as well and is silently interpreted as 1.

  @b{Warning}

  gtk_table_new has been deprecated since version 3.4 and should not be used
  in newly-written code. Use gtk_grid_new().")

;;; --- gtk-table-resize -------------------------------------------------------

(setf (documentation 'gtk-table-resize 'function)
 "@version{2013-1-22}
  @argument[table]{The GtkTable you wish to change the size of.}
  @argument[rows]{The new number of rows.}
  @argument[columns]{The new number of columns.}
  @begin{short}
    If you need to change a table's size after it has been created, this
    function allows you to do so.
  @end{short}

  @b{Warning}

  gtk_table_resize has been deprecated since version 3.4 and should not be
  used in newly-written code. GtkGrid resizes automatically.")

;;; --- gtk-table-get-size -----------------------------------------------------

(setf (documentation 'gtk-table-get-size 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @return{n-rows -- number of rows, or @code{nil}@br{}
          n-columns -- the number of columns, or @code{nil}}
  @short{Gets the number of rows and columns in the table.}

  @b{Warning}

  gtk_table_get_size has been deprecated since version 3.4 and should not be
  used in newly-written code. GtkGrid does not expose the number of columns
  and rows.

  Since 2.22")

;;; --- gtk-table-attach -------------------------------------------------------

(setf (documentation 'gtk-table-attach 'function)
 "@version{2013-1-22}
  @argument[table]{The GtkTable to add a new widget to.}
  @argument[child]{The widget to add.}
  @argument[left_attach]{the column number to attach the left side of a child
    widget to.}
  @argument[right_attach]{the column number to attach the right side of a child
    widget to.}
  @argument[top_attach]{the row number to attach the top of a child widget to.}
  @argument[bottom_attach]{the row number to attach the bottom of a child widget
    to.}
  @argument[xoptions]{Used to specify the properties of the child widget when
    the table is resized.}
  @argument[yoptions]{The same as xoptions, except this field determines
    behaviour of vertical resizing.}
  @argument[xpadding]{An integer value specifying the padding on the left and
    right of the widget being added to the table.}
  @argument[ypadding]{The amount of padding above and below the child widget.}
  @begin{short}
    Adds a widget to a table.
  @end{short}
  The number of 'cells' that a widget will occupy is specified by left_attach,
  right_attach, top_attach and bottom_attach. These each represent the leftmost,
  rightmost, uppermost and lowest column and row numbers of the table. (Columns
  and rows are indexed from zero).

  To make a button occupy the lower right cell of a 2x2 table, use
  @begin{pre} 
 gtk_table_attach (table, button,
                   1, 2, // left, right attach
                   1, 2, // top, bottom attach
                   xoptions, yoptions,
                   xpadding, ypadding);
  @end{pre}
  If you want to make the button span the entire bottom row, use
  left_attach == 0 and right_attach = 2 instead.

  @b{Warning}

  gtk_table_attach has been deprecated since version 3.4 and should not be
  used in newly-written code. Use gtk_grid_attach() with GtkGrid. Note that
  the attach arguments differ between those two functions.")

;;; --- gtk-table-attach-defaults ----------------------------------------------

(setf (documentation 'gtk-table-attach-defaults 'function)
 "@version{2013-1-22}
  @argument[table]{The table to add a new child widget to.}
  @argument[widget]{The child widget to add.}
  @argument[left_attach]{The column number to attach the left side of the child
    widget to.}
  @argument[right_attach]{The column number to attach the right side of the
    child widget to.}
  @argument[top_attach]{The row number to attach the top of the child widget
    to.}
  @argument[bottom_attach]{The row number to attach the bottom of the child
    widget to.}
  @begin{short}
    As there are many options associated with gtk_table_attach(), this
    convenience function provides the programmer with a means to add children to
    a table with identical padding and expansion options.
  @end{short}
  The values used for the GtkAttachOptions are GTK_EXPAND | GTK_FILL, and the
  padding is set to 0.

  @b{Warning}

  gtk_table_attach_defaults has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_grid_attach() with GtkGrid. Note
  that the attach arguments differ between those two functions.")

;;; --- gtk-table-set-row-spacing ----------------------------------------------

(setf (documentation 'gtk-table-set-row-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable containing the row whose properties you wish to
    change.}
  @argument[row]{row number whose spacing will be changed.}
  @argument[spacing]{number of pixels that the spacing should take up.}
  @begin{short}
    Changes the space between a given table row and the subsequent row.
  @end{short}

  @b{Warning}

  gtk_table_set_row_spacing has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_widget_set_margin_top() and
  gtk_widget_set_margin_bottom() on the widgets contained in the row if you
  need this functionality. GtkGrid does not support per-row spacing.")

;;; --- gtk-table-set-col-spacing ----------------------------------------------

(setf (documentation 'gtk-table-set-col-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable.}
  @argument[column]{the column whose spacing should be changed.}
  @argument[spacing]{number of pixels that the spacing should take up.}
  @begin{short}
    Alters the amount of space between a given table column and the following
    column.
  @end{short}

  @b{Warning}

  gtk_table_set_col_spacing has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_widget_set_margin_left() and
  gtk_widget_set_margin_right() on the widgets contained in the row if you
  need this functionality. GtkGrid does not support per-row spacing.")

;;; --- gtk-table-set-row-spacings ---------------------------------------------

(setf (documentation 'gtk-table-set-row-spacings 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable.}
  @argument[spacing]{the number of pixels of space to place between every row in
    the table.}
  @short{Sets the space between every row in table equal to spacing.}

  @b{Warning}

  gtk_table_set_row_spacings has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_grid_set_row_spacing() with
  GtkGrid.")

;;; --- gtk-table-set-col-spacings ---------------------------------------------

(setf (documentation 'gtk-table-set-col-spacings 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable.}
  @argument[spacing]{the number of pixels of space to place between every column
    in the table.}
  @short{Sets the space between every column in table equal to spacing.}

  @b{Warning}

  gtk_table_set_col_spacings has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_grid_set_column_spacing() with
  GtkGrid.")

;;; --- gtk-table-set-homogeneous ----------------------------------------------

(setf (documentation 'gtk-table-set-homogeneous 'function)
 "@version{2013-1-22}
  @argument[table]{The GtkTable you wish to set the homogeneous properties of.}
  @argument[homogeneous]{Set to TRUE to ensure all table cells are the same
    size. Set to FALSE if this is not your desired behaviour.}
  @begin{short}
    Changes the homogenous property of table cells, ie. whether all cells are an
    equal size or not.
  @end{short}

  @b{Warning}

  gtk_table_set_homogeneous has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_grid_set_row_homogeneous() and
  gtk_grid_set_column_homogeneous() with GtkGrid.")

;;; --- gtk-table-get-default-row-spacing --------------------------------------

(setf (documentation 'gtk-table-get-default-row-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @return{the default row spacing}
  @begin{short}
    Gets the default row spacing for the table. This is the spacing that will be
    used for newly added rows. (See gtk_table_set_row_spacings())
  @end{short}

  @b{Warning}

  gtk_table_get_default_row_spacing has been deprecated since version 3.4 and
  should not be used in newly-written code. Use gtk_grid_get_row_spacing()
  with GtkGrid.")

;;; --- gtk-table-get-homogeneous ----------------------------------------------

(setf (documentation 'gtk-table-get-homogeneous 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @return{TRUE if the cells are all constrained to the same size}
  @begin{short}
    Returns whether the table cells are all constrained to the same width and
    height. (See gtk_table_set_homogenous())
  @end{short}

  @b{Warning}

  gtk_table_get_homogeneous has been deprecated since version 3.4 and should
  not be used in newly-written code. Use gtk_grid_get_row_homogeneous() and
  gtk_grid_get_column_homogeneous() with GtkGrid.")

;;; --- gtk-table-get-row-spacing ----------------------------------------------

(setf (documentation 'gtk-table-get-row-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @argument[row]{a row in the table, 0 indicates the first row}
  @return{the row spacing}
  @begin{short}
    Gets the amount of space between row row, and row row + 1. See
    gtk_table_set_row_spacing().
  @end{short}

  @b{Warning}

  gtk_table_get_row_spacing has been deprecated since version 3.4 and should
  not be used in newly-written code. GtkGrid does not offer a replacement for
  this functionality.")

;;; --- gtk-table-get-col-spacing ----------------------------------------------

(setf (documentation 'gtk-table-get-col-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @argument[column]{a column in the table, 0 indicates the first column}
  @return{the column spacing}
  @begin{short}
    Gets the amount of space between column col, and column col + 1. See
    gtk_table_set_col_spacing().
  @end{short}

  @b{Warning}

  gtk_table_get_col_spacing has been deprecated since version 3.4 and should
  not be used in newly-written code. GtkGrid does not offer a replacement for
  this functionality.")

;;; --- gtk-table-get-default-col-spacing --------------------------------------

(setf (documentation 'gtk-table-get-default-col-spacing 'function)
 "@version{2013-1-22}
  @argument[table]{a GtkTable}
  @return{the default column spacing}
  @begin{short}
    Gets the default column spacing for the table. This is the spacing that will
    be used for newly added columns. (See gtk_table_set_col_spacings())
  @end{short}

  @b{Warning}

  gtk_table_get_default_col_spacing has been deprecated since version 3.4 and
  should not be used in newly-written code. Use gtk_grid_get_column_spacing()
  with GtkGrid.")

;;; --- End of file atdoc-gtk.table.lisp ---------------------------------------
