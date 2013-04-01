;;; ----------------------------------------------------------------------------
;;; gtk.table.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.2. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkTable
;;; 
;;; Pack widgets in regular patterns
;;;     
;;; Synopsis
;;; 
;;;     GtkTable
;;;     
;;;     gtk_table_new
;;;     gtk_table_resize
;;;     gtk_table_get_size
;;;     gtk_table_attach
;;;     gtk_table_attach_defaults
;;;     gtk_table_set_row_spacing
;;;     gtk_table_set_col_spacing
;;;     gtk_table_set_row_spacings
;;;     gtk_table_set_col_spacings
;;;     gtk_table_set_homogeneous
;;;     gtk_table_get_default_row_spacing
;;;     gtk_table_get_homogeneous
;;;     gtk_table_get_row_spacing
;;;     gtk_table_get_col_spacing
;;;     gtk_table_get_default_col_spacing
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTable
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkTable" 'gtk-table))

(define-g-object-class "GtkTable" gtk-table
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_table_get_type")
  ((column-spacing
    gtk-table-column-spacing
    "column-spacing" "guint" t t)
   (homogeneous
    gtk-table-homogeneous
    "homogeneous" "gboolean" t t)
   (n-columns
    gtk-table-n-columns
    "n-columns" "guint" t t)
   (n-rows
    gtk-table-n-rows
    "n-rows" "guint" t t)
   (row-spacing
    gtk-table-row-spacing
    "row-spacing" "guint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-table 'type)
 "@version{2013-3-30}
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

  @subheading{Note}
    @sym{gtk-table} has been deprecated. Use @class{gtk-grid} instead. It
    provides the same capabilities as @sym{gtk-table} for arranging widgets in
    a rectangular grid, but does support height-for-width geometry management.

  @begin[Child Property Details]{dictionary}
    @subheading{The \"bottom-attach\" child property}
      @code{\"bottom-attach\"} of type @code{:uint} (Read / Write)@br{}
      The row number to attach the bottom of the child to.@br{}
      Allowed values: [1,65535]@br{}
      Default value: 1

    @subheading{The \"left-attach\" child property}
      @code{\"left-attach\"} of type @code{:uint} (Read / Write)@br{}
      The column number to attach the left side of the child to.@br{}
      Allowed values: <= 65535@br{}
      Default value: 0

    @subheading{The \"right-attach\" child property}
      @code{\"right-attach\"} of type @code{:uint} (Read / Write)@br{}
      The column number to attach the right side of a child widget to.@br{}
      Allowed values: [1,65535]@br{}
      Default value: 1

    @subheading{The \"top-attach\" child property}
      @code{\"top-attach\"} of type @code{:uint} (Read / Write)@br{}
      The row number to attach the top of a child widget to.@br{}
      Allowed values: <= 65535@br{}
      Default value: 0

    @subheading{The \"x-options\" child property}
      @code{\"x-options\"} of type  @symbol{gtk-attach-options}
     (Read / Write)@br{}
      Options specifying the horizontal behaviour of the child.@br{}
      Default value: @code{'(:expand :fill)}

    @subheading{The \"x-padding\" child property}
      @code{\"x-padding\"} of type @code{:uint} (Read / Write)@br{}
      Extra space to put between the child and its left and right neighbors, in
      pixels.@br{}
      Allowed values: <= 65535@br{}
      Default value: 0

    @subheading{The \"y-options\" child property}
      @code{\"y-options\"} of type @symbol{gtk-attach-options}
      (Read / Write)@br{}
      Options specifying the vertical behaviour of the child.@br{}
      Default value: @code{'(:expand :fill)}

    @subheading{The \"y-padding\" child property}
      @code{\"y-padding\"} of type @code{:uint} (Read / Write)@br{}
      Extra space to put between the child and its upper and lower neighbors, in
      pixels.@br{}
      Allowed values: <= 65535@br{}
      Default value: 0
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

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-spacing" 'gtk-table) 't)
 "The @code{\"column-spacing\"} property of type @code{:uint}
  (Read / Write)@br{}
  The amount of space between two consecutive columns.@br{}
  Allowed values: <= 65535@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous" 'gtk-table) 't)
 "The @code{\"homogeneous\"} property of type @code{:boolean}
  (Read / Write)@br{}
  If @em{true}, the table cells are all the same width/height.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-columns" 'gtk-table) 't)
 "The @code{\"n-columns\"} property of type @code{:uint} (Read / Write)@br{}
  The number of columns in the table.@br{}
  Allowed values: [1,65535]@br{}
  Default value: 1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-rows" 'gtk-table) 't)
 "The @code{\"n-rows\"} property of type @code{:uint} (Read / Write)@br{}
  The number of rows in the table.@br{}
  Allowed values: [1,65535]@br{}
  Default value: 1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-spacing" 'gtk-table) 't)
 "The @code{\"row-spacing\"} property of type @code{:uint}  (Read / Write)@br{}
  The amount of space between two consecutive rows.@br{}
  Allowed values: <= 65535@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-table-column-spacing -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-column-spacing 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{column-spacing} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-homogeneous -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-homogeneous atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-table-homogeneous 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{homogeneous} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-n-columns ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-n-columns atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-table-n-columns 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{n-columns} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-n-rows -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-n-rows atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-table-n-rows 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{n-rows} of the @class{gtk-table} class.
  @end{short}")

;;; --- gtk-table-row-spacing --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-row-spacing atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-table-row-spacing 'function)
 "@version{2013-1-22}
  @begin{short}
    Accessor of the slot @code{row-spacing} of the @class{gtk-table} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-left-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-left-attach 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"left-attach\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-right-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-right-attach 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"right-attach\"} of the
  @class{gtk-table} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-top-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-top-attach 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"top-attach\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-bottom-attach atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-bottom-attach 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"bottom-attach\"} of the
  @class{gtk-table} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-x-options atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-x-options 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"x-options\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-y-options atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-y-options 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"y-options\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-x-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-x-padding 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"x-padding\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-table-child-y-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-table-child-y-padding 'function)
 "@version{2013-3-30}
  Accessor of the child property @code{\"y-padding\"} of the @class{gtk-table}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_table_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-new))

(defun gtk-table-new (rows columns homogeneous)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[rows]{the number of rows the new table should have}
  @argument[columns]{the number of columns the new table should have}
  @argument[homogeneous]{if set to @em{true}, all table cells are resized to the
    size of the cell containing the largest widget}
  @return{The the newly created table widget.}
  @begin{short}
    Used to create a new table widget.
  @end{short}
  An initial size must be given by specifying how many @arg{rows} and
  @arg{columns} the table should have, although this can be changed later with
  @fun{gtk-table-resize}. @arg{rows} and @arg{columns} must both be in the range
  1 ... 65535. For historical reasons, 0 is accepted as well and is silently
  interpreted as 1.

  @subheading{Warning}
    @sym{gtk-table-new} has been deprecated since version 3.4 and should not be
    used in newly-written code. Use @fun{gtk-grid-new}.
  @see-function{gtk-table-resize}
  @see-function{gtk-grid-new}"
  (make-instance 'gtk-table
                 :rows rows
                 :columns columns
                 :homogeneous homogeneous))

(export 'gtk-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_table_resize ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-resize))

(defun gtk-table-resize (table rows columns)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{the @class{gtk-table} you wish to change the size of}
  @argument[rows]{the new number of rows}
  @argument[columns]{the new number of columns}
  @begin{short}
    If you need to change a table's size after it has been created, this
    function allows you to do so.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-resize} has been deprecated since version 3.4 and should not
    be used in newly-written code. @class{gtk-grid} resizes automatically."
  (setf (gtk-table-n-rows table) rows
        (gtk-table-n-columns table) columns))

(export 'gtk-table-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-get-size))

(defun gtk-table-get-size (table)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @begin{return}
    @code{n-rows} -- number of rows, or @code{nil}@br{}
    @code{n-columns} -- the number of columns, or @code{nil}
  @end{return}
  @short{Gets the number of rows and columns in the table.}

  @subheading{Warning}
    @sym{gtk-table-get-size} has been deprecated since version 3.4 and should
    not be used in newly-written code. @class{gtk-grid} does not expose the
    number of columns and rows.

  Since 2.22"
  (values (gtk-table-n-rows table)
          (gtk-table-n-columns table)))

(export 'gtk-table-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_attach" %gtk-table-attach) :void
  (table (g-object gtk-table))
  (child (g-object gtk-widget))
  (left :uint)
  (right :uint)
  (top :uint)
  (bottom :uint)
  (x-options gtk-attach-options)
  (y-options gtk-attach-options)
  (x-padding :uint)
  (y-padding :uint))

(defun gtk-table-attach (table widget left right top bottom
                                      &key (x-options '(:expand :fill))
                                           (y-options '(:expand :fill))
                                           (x-padding 0)
                                           (y-padding 0))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{the @class{gtk-table} to add a new widget to}
  @argument[child]{the widget to add}
  @argument[left-attach]{the column number to attach the left side of a child
    widget to}
  @argument[right-attach]{the column number to attach the right side of a child
    widget to}
  @argument[top-attach]{the row number to attach the top of a child widget to}
  @argument[bottom-attach]{the row number to attach the bottom of a child widget
    to}
  @argument[x-options]{used to specify the properties of the child widget when
    the table is resized}
  @argument[y-options]{the same as @arg{x-options}, except this field determines
    behaviour of vertical resizing}
  @argument[x-padding]{an integer value specifying the padding on the left and
    right of the widget being added to the table}
  @argument[y-padding]{the amount of padding above and below the child widget}
  @begin{short}
    Adds a widget to a table.
  @end{short}
  The number of cells that a widget will occupy is specified by
  @arg{left-attach}, @arg{right-attach}, @arg{top-attach} and
  @arg{bottom-attach}. These each represent the leftmost, rightmost, uppermost
  and lowest column and row numbers of the table. (Columns and rows are indexed
  from zero).

  To make a button occupy the lower right cell of a 2 x 2 table, use
  @begin{pre} 
 gtk_table_attach (table, button,
                   1, 2, // left, right attach
                   1, 2, // top, bottom attach
                   xoptions, yoptions,
                   xpadding, ypadding);
  @end{pre}
  If you want to make the button span the entire bottom row, use
  @code{left-attach = 0} and @code{right-attach = 2} instead.

  @subheading{Warning}
    @sym{gtk-table-attach} has been deprecated since version 3.4 and should not
    be used in newly-written code. Use @fun{gtk-grid-attach} with
    @class{gtk-grid}. Note that the attach arguments differ between those two
    functions."
  (%gtk-table-attach table widget
                     left right top bottom
                     x-options y-options
                     x-padding y-padding))

(export 'gtk-table-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach_defaults ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-attach-defaults))

(defun gtk-table-attach-defaults (table child left right top bottom)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{the table to add a new child widget to}
  @argument[widget]{the child widget to add}
  @argument[left-attach]{the column number to attach the left side of the child
    widget to}
  @argument[right-attach]{the column number to attach the right side of the
    child widget to}
  @argument[top-attach]{the row number to attach the top of the child widget
    to}
  @argument[bottom-attach]{the row number to attach the bottom of the child
    widget to}
  @begin{short}
    As there are many options associated with @fun{gtk-table-attach}, this
    convenience function provides the programmer with a means to add children to
    a table with identical padding and expansion options.
  @end{short}
  The values used for the @symbol{gtk-attach-options} are
  @code{'(:expand :fill)}, and the padding is set to 0.

  @subheading{Warning}
    @sym{gtk-table-attach-defaults} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use @fun{gtk-grid-attach} with
    @class{gtk-grid}. Note that the attach arguments differ between those two
    functions."
  (gtk-table-attach table child left right top bottom))

(export 'gtk-table-attach-defaults)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_row_spacing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_set_row_spacing" gtk-table-set-row-spacing) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} containing the row whose properties you
    wish to change}
  @argument[row]{row number whose spacing will be changed}
  @argument[spacing]{number of pixels that the spacing should take up}
  @begin{short}
    Changes the space between a given table row and the subsequent row.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-set-row-spacing} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use
    @fun{gtk-widget-set-margin-top} and @fun{gtk-widget-set-margin-bottom} on
    the widgets contained in the row if you need this functionality.
    @class{gtk-grid} does not support per-row spacing."
  (table (g-object gtk-table))
  (row :uint)
  (spacing :uint))

(export 'gtk-table-set-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_col_spacing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_set_col_spacing" gtk-table-set-col-spacing) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @argument[column]{the column whose spacing should be changed}
  @argument[spacing]{number of pixels that the spacing should take up}
  @begin{short}
    Alters the amount of space between a given table column and the following
    column.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-set-col-spacing} has been deprecated since version 3.4 and
    should not be used in newly written code. Use
    @fun{gtk-widget-set-margin-left} and @fun{gtk-widget-set-margin-right} on
    the widgets contained in the row if you need this functionality.
    @class{gtk-grid} does not support per-row spacing."
  (table (g-object gtk-table))
  (column :uint)
  (spacing :uint))

(export 'gtk-table-set-col-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_row_spacings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-set-row-spacings))

(defun gtk-table-set-row-spacings (table spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @argument[spacing]{the number of pixels of space to place between every row in
    the table}
  @short{Sets the space between every row in table equal to spacing.}

  @subheading{Warning}
    @sym{gtk-table-set-row-spacings} has been deprecated since version 3.4 and
    should not be used in newly written code. Use @fun{gtk-grid-set-row-spacing}
    with @class{gtk-grid}."
  (setf (gtk-table-row-spacing table) spacing))

(export 'gtk-table-set-row-spacings)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_col_spacings ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-set-col-spacings))

(defun gtk-table-set-col-spacings (table spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @argument[spacing]{the number of pixels of space to place between every column
    in the table}
  @short{Sets the space between every column in table equal to spacing.}

  @subheading{Warning}
    @sym{gtk-table-set-col-spacings} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use
    @fun{gtk-grid-set-column-spacing} with @class{gtk-grid}."
  (setf (gtk-table-column-spacing table) spacing))

(export 'gtk-table-set-col-spacings)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-set-homogeneous))

(defun gtk-table-set-homogeneous (table homogeneous)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{the @class{gtk-table} you wish to set the homogeneous
    properties of}
  @argument[homogeneous]{set to @em{true} to ensure all table cells are the same
    size, set to @code{nil} if this is not your desired behaviour}
  @begin{short}
    Changes the homogenous property of table cells, i. e. whether all cells are
    an equal size or not.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-set-homogeneous} has been deprecated since version 3.4 and
    should not be used in newly-written code. Use
    @class{gtk-grid-set-row-homogeneous} and
    @fun{gtk-grid-set-column-homogeneous} with @class{gtk-grid}."
  (setf (gtk-table-homogeneous table) homogeneous))

(export 'gtk-table-set-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_default_row_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-get-default-row-spacing))

(defun gtk-table-get-default-row-spacing (table)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @return{The default row spacing.}
  @begin{short}
    Gets the default row spacing for the table. This is the spacing that will be
    used for newly added rows. See @fun{gtk-table-set-row-spacings}.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-get-default-row-spacing} has been deprecated since version
    3.4 and should not be used in newly written code. Use
    @fun{gtk-grid-get-row-spacing} with @class{gtk-grid}."
  (gtk-table-row-spacing table))

(export 'gtk-table-get-default-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-get-homogeneous))

(defun gtk-table-get-homogeneous (table)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @return{@em{True} if the cells are all constrained to the same size}
  @begin{short}
    Returns whether the table cells are all constrained to the same width and
    height. See @fun{gtk-table-set-homogenous}.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-get-homogeneous} has been deprecated since version 3.4 and
    should not be used in newly written code. Use
    @fun{gtk-grid-get-row-homogeneous} and @fun{gtk-grid-get-column-homogeneous}
    with @class{gtk-grid}."
  (gtk-table-homogeneous table))

(export 'gtk-table-get-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_row_spacing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_get_row_spacing" gtk-table-get-row-spacing) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @argument[row]{a row in the table, 0 indicates the first row}
  @return{The row spacing.}
  @begin{short}
    Gets the amount of space between row row, and row row + 1. See
    @fun{gtk-table-set-row-spacing}.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-get-row-spacing} has been deprecated since version 3.4 and
    should not be used in newly-written code. @class{gtk-grid} does not offer a
    replacement for this functionality."
  (table (g-object gtk-table))
  (row :uint))

(export 'gtk-table-get-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_col_spacing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_get_col_spacing" gtk-table-get-col-spacing) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @argument[column]{a column in the table, 0 indicates the first column}
  @return{The column spacing.}
  @begin{short}
    Gets the amount of space between column col, and column col + 1. See
    @fun{gtk-table-set-col-spacing}.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-get-col-spacing} has been deprecated since version 3.4 and
    should not be used in newly written code. @class{gtk-grid} does not offer a
    replacement for this functionality."
  (table (g-object gtk-table))
  (column :uint))

(export 'gtk-table-get-col-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_default_col_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-table-get-default-col-spacing))

(defun gtk-table-get-default-col-spacing (table)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[table]{a @class{gtk-table} widget}
  @return{The default column spacing.}
  @begin{short}
    Gets the default column spacing for the table. This is the spacing that will
    be used for newly added columns. See @fun{gtk-table-set-col-spacings}.
  @end{short}

  @subheading{Warning}
    @sym{gtk-table-get-default-col-spacing} has been deprecated since version
    3.4 and should not be used in newly-written code. Use
    @class{gtk-grid-get-column-spacing} with @class{gtk-grid}."
  (gtk-table-column-spacing table))

(export 'gtk-table-get-default-col-spacing)

;;; --- End of file gtk.table.lisp ---------------------------------------------
