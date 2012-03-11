;;; ----------------------------------------------------------------------------
;;; gtk.table.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkTable
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTable implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "column-spacing"           guint                 : Read / Write
;;;   "homogeneous"              gboolean              : Read / Write
;;;   "n-columns"                guint                 : Read / Write
;;;   "n-rows"                   guint                 : Read / Write
;;;   "row-spacing"              guint                 : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "bottom-attach"            guint                 : Read / Write
;;;   "left-attach"              guint                 : Read / Write
;;;   "right-attach"             guint                 : Read / Write
;;;   "top-attach"               guint                 : Read / Write
;;;   "x-options"                GtkAttachOptions      : Read / Write
;;;   "x-padding"                guint                 : Read / Write
;;;   "y-options"                GtkAttachOptions      : Read / Write
;;;   "y-padding"                guint                 : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkTable functions allow the programmer to arrange widgets in rows and
;;; columns, making it easy to align many widgets next to each other,
;;; horizontally and vertically.
;;; 
;;; Tables are created with a call to gtk_table_new(), the size of which can
;;; later be changed with gtk_table_resize().
;;; 
;;; Widgets can be added to a table using gtk_table_attach() or the more
;;; convenient (but slightly less flexible) gtk_table_attach_defaults().
;;; 
;;; To alter the space next to a specific row, use gtk_table_set_row_spacing(),
;;; and for a column, gtk_table_set_col_spacing(). The gaps between all rows or
;;; columns can be changed by calling gtk_table_set_row_spacings() or
;;; gtk_table_set_col_spacings() respectively. Note that spacing is added
;;; between the children, while padding added by gtk_table_attach() is added on
;;; either side of the widget it belongs to.
;;; 
;;; gtk_table_set_homogeneous(), can be used to set whether all cells in the
;;; table will resize themselves to the size of the largest widget in the table.
;;; 
;;; Note
;;;
;;; Note that GtkGrid provides the same capabilities as GtkTable for arranging
;;; widgets in a rectangular grid, and additionally supports height-for-width
;;; geometry management.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "column-spacing" property
;;; 
;;;   "column-spacing"           guint                 : Read / Write
;;; 
;;; The amount of space between two consecutive columns.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "homogeneous" property
;;; 
;;;   "homogeneous"              gboolean              : Read / Write
;;; 
;;; If TRUE, the table cells are all the same width/height.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "n-columns" property
;;; 
;;;   "n-columns"                guint                 : Read / Write
;;; 
;;; The number of columns in the table.
;;; 
;;; Allowed values: [1,65535]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "n-rows" property
;;; 
;;;   "n-rows"                   guint                 : Read / Write
;;; 
;;; The number of rows in the table.
;;; 
;;; Allowed values: [1,65535]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-spacing" property
;;; 
;;;   "row-spacing"              guint                 : Read / Write
;;; 
;;; The amount of space between two consecutive rows.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "bottom-attach" child property
;;; 
;;;   "bottom-attach"            guint                 : Read / Write
;;; 
;;; The row number to attach the bottom of the child to.
;;; 
;;; Allowed values: [1,65535]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-attach" child property
;;; 
;;;   "left-attach"              guint                 : Read / Write
;;; 
;;; The column number to attach the left side of the child to.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-attach" child property
;;; 
;;;   "right-attach"             guint                 : Read / Write
;;; 
;;; The column number to attach the right side of a child widget to.
;;; 
;;; Allowed values: [1,65535]
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "top-attach" child property
;;; 
;;;   "top-attach"               guint                 : Read / Write
;;; 
;;; The row number to attach the top of a child widget to.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "x-options" child property
;;; 
;;;   "x-options"                GtkAttachOptions      : Read / Write
;;; 
;;; Options specifying the horizontal behaviour of the child.
;;; 
;;; Default value: GTK_EXPAND|GTK_FILL
;;;
;;; ----------------------------------------------------------------------------
;;; The "x-padding" child property
;;; 
;;;   "x-padding"                guint                 : Read / Write
;;; 
;;; Extra space to put between the child and its left and right neighbors,
;;; in pixels.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "y-options" child property
;;; 
;;;   "y-options"                GtkAttachOptions      : Read / Write
;;; 
;;; Options specifying the vertical behaviour of the child.
;;; 
;;; Default value: GTK_EXPAND|GTK_FILL
;;;
;;; ----------------------------------------------------------------------------
;;; The "y-padding" child property
;;; 
;;;   "y-padding"                guint                 : Read / Write
;;; 
;;; Extra space to put between the child and its upper and lower neighbors,
;;; in pixels.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTable
;;; 
;;; struct GtkTable;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkTable" 'gtk-table))

(define-g-object-class "GtkTable" gtk-table
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
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
;;; gtk_table_new ()
;;; 
;;; GtkWidget * gtk_table_new (guint rows, guint columns, gboolean homogeneous)
;;; 
;;; Used to create a new table widget. An initial size must be given by
;;; specifying how many rows and columns the table should have, although this
;;; can be changed later with gtk_table_resize(). rows and columns must both be
;;; in the range 1 .. 65535. For historical reasons, 0 is accepted as well and
;;; is silently interpreted as 1.
;;; 
;;; rows :
;;;     The number of rows the new table should have.
;;; 
;;; columns :
;;;     The number of columns the new table should have.
;;; 
;;; homogeneous :
;;;     If set to TRUE, all table cells are resized to the size of the cell
;;;     containing the largest widget.
;;; 
;;; Returns :
;;;     A pointer to the the newly created table widget.
;;; ----------------------------------------------------------------------------

(defun gtk-table-new (rows columns homogeneous)
  (make-instance 'gtk-table
                 :rows rows
                 :columns columns
                 :homogeneous homogeneous))

(export 'gtk-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_table_resize ()
;;; 
;;; void gtk_table_resize (GtkTable *table, guint rows, guint columns)
;;; 
;;; If you need to change a table's size after it has been created, this
;;; function allows you to do so.
;;; 
;;; table :
;;;     The GtkTable you wish to change the size of.
;;; 
;;; rows :
;;;     The new number of rows.
;;; 
;;; columns :
;;;     The new number of columns.
;;; ----------------------------------------------------------------------------

(defun gtk-table-resize (table rows columns)
  (setf (gtk-table-n-rows table) rows
        (gtk-table-n-columns table) columns))

(export 'gtk-table-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_size ()
;;; 
;;; void gtk_table_get_size (GtkTable *table, guint *rows, guint *columns)
;;; 
;;; Gets the number of rows and columns in the table.
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; rows :
;;;     return location for the number of rows, or NULL.
;;; 
;;; columns :
;;;     return location for the number of columns, or NULL.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defun gtk-table-get-size (table)
  (values (gtk-table-n-rows table)
          (gtk-table-n-columns table)))

(export 'gtk-table-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach ()
;;; 
;;; void gtk_table_attach (GtkTable *table,
;;;                        GtkWidget *child,
;;;                        guint left_attach,
;;;                        guint right_attach,
;;;                        guint top_attach,
;;;                        guint bottom_attach,
;;;                        GtkAttachOptions xoptions,
;;;                        GtkAttachOptions yoptions,
;;;                        guint xpadding,
;;;                        guint ypadding)
;;; 
;;; Adds a widget to a table. The number of 'cells' that a widget will occupy
;;; is specified by left_attach, right_attach, top_attach and bottom_attach.
;;; These each represent the leftmost, rightmost, uppermost and lowest column
;;; and row numbers of the table. (Columns and rows are indexed from zero).
;;; 
;;; To make a button occupy the lower right cell of a 2x2 table, use
;;; 
;;;  gtk_table_attach (table, button,
;;;                    1, 2, // left, right attach
;;;                    1, 2, // top, bottom attach
;;;                    xoptions, yoptions,
;;;                    xpadding, ypadding);
;;; 
;;; If you want to make the button span the entire bottom row, use
;;; left_attach == 0 and right_attach = 2 instead.
;;; 
;;; table :
;;;     The GtkTable to add a new widget to.
;;; 
;;; child :
;;;     The widget to add.
;;; 
;;; left_attach :
;;;     the column number to attach the left side of a child widget to.
;;; 
;;; right_attach :
;;;     the column number to attach the right side of a child widget to.
;;; 
;;; top_attach :
;;;     the row number to attach the top of a child widget to.
;;; 
;;; bottom_attach :
;;;     the row number to attach the bottom of a child widget to.
;;; 
;;; xoptions :
;;;     Used to specify the properties of the child widget when the table is
;;;     resized.
;;; 
;;; yoptions :
;;;     The same as xoptions, except this field determines behaviour of
;;;     vertical resizing.
;;; 
;;; xpadding :
;;;     An integer value specifying the padding on the left and right of the
;;;     widget being added to the table.
;;; 
;;; ypadding :
;;;     The amount of padding above and below the child widget.
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
  (%gtk-table-attach table widget
                     left right top bottom
                     x-options y-options
                     x-padding y-padding))

(export 'gtk-table-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach_defaults ()
;;; 
;;; void gtk_table_attach_defaults (GtkTable *table,
;;;                                 GtkWidget *widget,
;;;                                 guint left,
;;;                                 guint right,
;;;                                 guint top,
;;;                                 guint bottom);
;;; 
;;; As there are many options associated with gtk_table_attach(), this
;;; convenience function provides the programmer with a means to add children
;;; to a table with identical padding and expansion options. The values used
;;; for the GtkAttachOptions are GTK_EXPAND | GTK_FILL, and the padding is set
;;; to 0.
;;; 
;;; table :
;;;     The table to add a new child widget to.
;;; 
;;; widget :
;;;     The child widget to add.
;;; 
;;; left :
;;;     The column number to attach the left side of the child widget to.
;;; 
;;; right :
;;;     The column number to attach the right side of the child widget to.
;;; 
;;; top :
;;;     The row number to attach the top of the child widget to.
;;; 
;;; bottom :
;;;     The row number to attach the bottom of the child widget to.
;;; ----------------------------------------------------------------------------

(defun gtk-table-attach-defaults (table child left right top bottom)
  (gtk-table-attach table child left right top bottom))

(export 'gtk-table-attach-defaults)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_row_spacing ()
;;; 
;;; void gtk_table_set_row_spacing (GtkTable *table, guint row, guint spacing)
;;; 
;;; Changes the space between a given table row and the subsequent row.
;;; 
;;; table :
;;;     a GtkTable containing the row whose properties you wish to change.
;;; 
;;; row :
;;;     row number whose spacing will be changed.
;;; 
;;; spacing :
;;;     number of pixels that the spacing should take up.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_set_row_spacing" gtk-table-set-row-spacing) :void
  (table (g-object gtk-table))
  (row :uint)
  (spacing :uint))

(export 'gtk-table-set-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_col_spacing ()
;;; 
;;; void gtk_table_set_col_spacing (GtkTable *table,
;;;                                 guint column,
;;                                  guint spacing);
;;; 
;;; Alters the amount of space between a given table column and the following
;;; column.
;;; 
;;; table :
;;;     a GtkTable.
;;; 
;;; column :
;;;     the column whose spacing should be changed.
;;; 
;;; spacing :
;;;     number of pixels that the spacing should take up.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_set_col_spacing" gtk-table-set-col-spacing) :void
  (table (g-object gtk-table))
  (column :uint)
  (spacing :uint))

(export 'gtk-table-set-col-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_row_spacings ()
;;; 
;;; void gtk_table_set_row_spacings (GtkTable *table, guint spacing);
;;; 
;;; Sets the space between every row in table equal to spacing.
;;; 
;;; table :
;;;     a GtkTable.
;;; 
;;; spacing :
;;;     the number of pixels of space to place between every row in the table.
;;; ----------------------------------------------------------------------------

(defun gtk-table-set-row-spacings (table spacing)
  (setf (gtk-table-row-spacing table) spacing))

(export 'gtk-table-set-row-spacings)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_col_spacings ()
;;; 
;;; void gtk_table_set_col_spacings (GtkTable *table, guint spacing);
;;; 
;;; Sets the space between every column in table equal to spacing.
;;; 
;;; table :
;;;     a GtkTable.
;;; 
;;; spacing :
;;;     the number of pixels of space to place between every column in
;;;     the table.
;;; ----------------------------------------------------------------------------

(defun gtk-table-set-col-spacings (table spacing)
  (setf (gtk-table-column-spacing table) spacing))

(export 'gtk-table-set-col-spacings)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_homogeneous ()
;;; 
;;; void gtk_table_set_homogeneous (GtkTable *table, gboolean homogeneous);
;;; 
;;; Changes the homogenous property of table cells, ie. whether all cells are
;;; an equal size or not.
;;; 
;;; table :
;;;     The GtkTable you wish to set the homogeneous properties of.
;;; 
;;; homogeneous :
;;;     Set to TRUE to ensure all table cells are the same size. Set to FALSE
;;;     if this is not your desired behaviour.
;;; ----------------------------------------------------------------------------

(defun gtk-table-set-homogeneous (table homogeneous)
  (setf (gtk-table-homogeneous table) homogeneous))

(export 'gtk-table-set-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_default_row_spacing ()
;;; 
;;; guint gtk_table_get_default_row_spacing (GtkTable *table);
;;; 
;;; Gets the default row spacing for the table. This is the spacing that will
;;; be used for newly added rows. (See gtk_table_set_row_spacings())
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; Returns :
;;;     the default row spacing
;;; ----------------------------------------------------------------------------

(defun gtk-table-get-default-row-spacing (table)
  (gtk-table-row-spacing table))

(export 'gtk-table-get-default-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_homogeneous ()
;;; 
;;; gboolean gtk_table_get_homogeneous (GtkTable *table);
;;; 
;;; Returns whether the table cells are all constrained to the same width and
;;; height. (See gtk_table_set_homogenous())
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; Returns :
;;;     TRUE if the cells are all constrained to the same size
;;; ----------------------------------------------------------------------------

(defun gtk-table-get-homogeneous (table)
  (gtk-table-homogeneous table))

(export 'gtk-table-get-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_row_spacing ()
;;; 
;;; guint gtk_table_get_row_spacing (GtkTable *table, guint row);
;;; 
;;; Gets the amount of space between row row, and row row + 1.
;;; See gtk_table_set_row_spacing().
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; row :
;;;     a row in the table, 0 indicates the first row
;;; 
;;; Returns :
;;;     the row spacing
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_get_row_spacing" gtk-table-get-row-spacing) :uint
  (table (g-object gtk-table))
  (row :uint))

(export 'gtk-table-get-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_col_spacing ()
;;; 
;;; guint gtk_table_get_col_spacing (GtkTable *table, guint column);
;;; 
;;; Gets the amount of space between column col, and column col + 1.
;;; See gtk_table_set_col_spacing().
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; column :
;;;     a column in the table, 0 indicates the first column
;;; 
;;; Returns :
;;;     the column spacing
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_table_get_col_spacing" gtk-table-get-col-spacing) :uint
  (table (g-object gtk-table))
  (column :uint))

(export 'gtk-table-get-col-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_default_col_spacing ()
;;; 
;;; guint gtk_table_get_default_col_spacing (GtkTable *table);
;;; 
;;; Gets the default column spacing for the table. This is the spacing that
;;; will be used for newly added columns. (See gtk_table_set_col_spacings())
;;; 
;;; table :
;;;     a GtkTable
;;; 
;;; Returns :
;;;     the default column spacing
;;; ----------------------------------------------------------------------------

(defun gtk-table-get-default-col-spacing (table)
  (gtk-table-column-spacing table))

(export 'gtk-table-get-default-col-spacing)

;;; --- End of file gtk.table.lisp ---------------------------------------------
