;;; ----------------------------------------------------------------------------
;;; gtk.tree-view-column.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkTreeViewColumn
;;;
;;; A visible column in a GtkTreeView widget
;;;
;;; Synopsis
;;;
;;;     GtkTreeViewColumnSizing
;;;     GtkTreeViewColumn
;;;
;;;     gtk_tree_view_column_new
;;;     gtk_tree_view_column_new_with_area
;;;     gtk_tree_view_column_new_with_attributes
;;;     gtk_tree_view_column_pack_start
;;;     gtk_tree_view_column_pack_end
;;;     gtk_tree_view_column_clear
;;;     gtk_tree_view_column_add_attribute
;;;     gtk_tree_view_column_set_attributes
;;;     gtk_tree_view_column_set_cell_data_func
;;;     gtk_tree_view_column_clear_attributes
;;;     gtk_tree_view_column_set_spacing
;;;     gtk_tree_view_column_get_spacing
;;;     gtk_tree_view_column_set_visible
;;;     gtk_tree_view_column_get_visible
;;;     gtk_tree_view_column_set_resizable
;;;     gtk_tree_view_column_get_resizable
;;;     gtk_tree_view_column_set_sizing
;;;     gtk_tree_view_column_get_sizing
;;;     gtk_tree_view_column_get_width
;;;     gtk_tree_view_column_get_fixed_width
;;;     gtk_tree_view_column_set_fixed_width
;;;     gtk_tree_view_column_set_min_width
;;;     gtk_tree_view_column_get_min_width
;;;     gtk_tree_view_column_set_max_width
;;;     gtk_tree_view_column_get_max_width
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_set_title
;;;     gtk_tree_view_column_get_title
;;;     gtk_tree_view_column_set_expand
;;;     gtk_tree_view_column_get_expand
;;;     gtk_tree_view_column_set_clickable
;;;     gtk_tree_view_column_get_clickable
;;;     gtk_tree_view_column_set_widget
;;;     gtk_tree_view_column_get_widget
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_set_alignment
;;;     gtk_tree_view_column_get_alignment
;;;     gtk_tree_view_column_set_reorderable
;;;     gtk_tree_view_column_get_reorderable
;;;     gtk_tree_view_column_set_sort_column_id
;;;     gtk_tree_view_column_get_sort_column_id
;;;     gtk_tree_view_column_set_sort_indicator
;;;     gtk_tree_view_column_get_sort_indicator
;;;     gtk_tree_view_column_set_sort_order
;;;     gtk_tree_view_column_get_sort_order
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view
;;;     gtk_tree_view_column_get_x_offset
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewColumnSizing
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewColumnSizing" gtk-tree-view-column-sizing
  (:export t
   :type-initializer "gtk_tree_view_column_sizing_get_type")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sizing atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-tree-view-column-sizing atdoc:*external-symbols*)
 "@version{2013-5-12}
  @begin{short}
    The sizing method the column uses to determine its width. Please note that
    @code{:autosize} are inefficient for large views, and can make columns
    appear choppy.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkTreeViewColumnSizing\" gtk-tree-view-column-sizing
  (:export t
   :type-initializer \"gtk_tree_view_column_sizing_get_type\")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))
  @end{pre}
  @begin[code]{table}
    @entry[:grow-only]{Columns only get bigger in reaction to changes in the
      model.}
    @entry[:autosize]{Columns resize to be the optimal size everytime the model
      changes.}
    @entry[:fixed]{Columns are a fixed numbers of pixels wide.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeViewColumn
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeViewColumn" gtk-tree-view-column
  (:superclass g-initially-unowned
   :export t
   :interfaces ("GtkBuildable"
                "GtkCellLayout")
   :type-initializer "gtk_tree_view_column_get_type")
  ((alignment
    gtk-tree-view-column-alignment
    "alignment" "gfloat" t t)
   (cell-area
    gtk-tree-view-column-cell-area
    "cell-area" "GtkCellArea" t t)
   (clickable
    gtk-tree-view-column-clickable
    "clickable" "gboolean" t t)
   (expand
    gtk-tree-view-column-expand
    "expand" "gboolean" t t)
   (fixed-width
    gtk-tree-view-column-fixed-width
    "fixed-width" "gint" t t)
   (max-width
    gtk-tree-view-column-max-width
    "max-width" "gint" t t)
   (min-width
    gtk-tree-view-column-min-width
    "min-width" "gint" t t)
   (reorderable
    gtk-tree-view-column-reorderable
    "reorderable" "gboolean" t t)
   (resizable
    gtk-tree-view-column-resizable
    "resizable" "gboolean" t t)
   (sizing
    gtk-tree-view-column-sizing
    "sizing" "GtkTreeViewColumnSizing" t t)
   (sort-column-id
    gtk-tree-view-column-sort-column-id
    "sort-column-id" "gint" t t)
   (sort-indicator
    gtk-tree-view-column-sort-indicator
    "sort-indicator" "gboolean" t t)
   (sort-order
    gtk-tree-view-column-sort-order
    "sort-order" "GtkSortType" t t)
   (spacing
    gtk-tree-view-column-spacing
    "spacing" "gint" t t)
   (title
    gtk-tree-view-column-title
    "title" "gchararray" t t)
   (visible
    gtk-tree-view-column-visible
    "visible" "gboolean" t t)
   (widget
    gtk-tree-view-column-widget
    "widget" "GtkWidget" t t)
   (width
    gtk-tree-view-column-width
    "width" "gint" t nil)
   (x-offset
    gtk-tree-view-column-x-offset
    "x-offset" "gint" t nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-view-column 'type)
 "@version{2013-5-14}
  @begin{short}
    The @class{gtk-tree-view-column} object represents a visible column in a
    @class{gtk-tree-view} widget. It allows to set properties of the column
    header, and functions as a holding pen for the cell renderers which
    determine how the data in the column is displayed.
  @end{short}

  Please refer to the tree widget conceptual overview for an overview of all
  the objects and data types related to the tree widget and how they work
  together.
  @begin[Signal Details]{dictionary}
    @subheading{The \"clicked\" signal}
      @begin{pre}
 lambda (treeviewcolumn)
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-tree-view-column-alignment}
  @see-slot{gtk-tree-view-column-cell-area}
  @see-slot{gtk-tree-view-column-clickable}
  @see-slot{gtk-tree-view-column-expand}
  @see-slot{gtk-tree-view-column-fixed-width}
  @see-slot{gtk-tree-view-column-max-width}
  @see-slot{gtk-tree-view-column-min-width}
  @see-slot{gtk-tree-view-column-reorderable}
  @see-slot{gtk-tree-view-column-resizable}
  @see-slot{gtk-tree-view-column-sizing}
  @see-slot{gtk-tree-view-column-sort-column-id}
  @see-slot{gtk-tree-view-column-sort-indicator}
  @see-slot{gtk-tree-view-column-sort-order}
  @see-slot{gtk-tree-view-column-spacing}
  @see-slot{gtk-tree-view-column-title}
  @see-slot{gtk-tree-view-column-visible}
  @see-slot{gtk-tree-view-column-widget}
  @see-slot{gtk-tree-view-column-width}
  @see-slot{gtk-tree-view-column-x-offset}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "alignment"
                                               'gtk-tree-view-column) 't)
 "The @code{\"alignment\"} property of type @code{:float} (Read / Write)@br{}
  X Alignment of the column header text or widget. @br{}
  Allowed values: [0,1]@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area"
                                               'gtk-tree-view-column) 't)
 "The @code{\"cell-area\"} property of type @class{gtk-cell-area}
  (Read / Write / Construct)@br{}
  The @class{gtk-cell-area} object used to layout cell renderers for this
  column. If no area is specified when creating the tree view column with the
  function @fun{gtk-tree-view-column-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} object will be used. @br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "clickable"
                                               'gtk-tree-view-column) 't)
 "The @code{\"clickable\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the header can be clicked. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expand"
                                               'gtk-tree-view-column) 't)
 "The @code{\"expand\"} property of type @code{:boolean} (Read / Write)@br{}
  Column gets share of extra width allocated to the widget. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fixed-width"
                                               'gtk-tree-view-column) 't)
 "The @code{\"fixed-width\"} property of type @code{:int} (Read / Write)@br{}
  Current fixed width of the column. @br{}
  Allowed values: >= 1@br{}
  Default value: 1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-width"
                                               'gtk-tree-view-column) 't)
 "The @code{\"max-width\"} property of type @code{:int} (Read / Write)@br{}
  Maximum allowed width of the column. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-width"
                                               'gtk-tree-view-column) 't)
 "The @code{\"min-width\"} property of type @code{:int} (Read / Write)@br{}
  Minimum allowed width of the column. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reorderable"
                                               'gtk-tree-view-column) 't)
 "The @code{\"reorderable\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the column can be reordered around the headers. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resizable"
                                               'gtk-tree-view-column) 't)
 "The @code{\"resizable\"} property of type @code{:boolean} (Read / Write)@br{}
  Column is user-resizable. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sizing"
                                               'gtk-tree-view-column) 't)
 "The @code{\"sizing\"} property of type @symbol{gtk-tree-view-column-sizing}
  (Read / Write)@br{}
  Resize mode of the column. @br{}
  Default value: @code{:grow-only}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-column-id"
                                               'gtk-tree-view-column) 't)
 "The @code{\"sort-column-id\"} property of type @code{:int} (Read / Write)@br{}
  Logical sort column ID this column sorts on when selected for sorting.
  Setting the sort column ID makes the column header clickable. Set to -1 to
  make the column unsortable. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-indicator"
                                               'gtk-tree-view-column) 't)
 "The @code{\"sort-indicator\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether to show a sort indicator. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-order"
                                               'gtk-tree-view-column) 't)
 "The @code{\"sort-order\"} property of type @symbol{gtk-sort-type}
  (Read / Write)@br{}
  Sort direction the sort indicator should indicate. @br{}
  Default value: @code{:ascending}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing"
                                               'gtk-tree-view-column) 't)
 "The @code{\"spacing\"} property of type @code{:int} (Read / Write)@br{}
  Space which is inserted between cells. @br{}
  Allowed values: >= 0@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                                               'gtk-tree-view-column) 't)
 "The @code{\"title\"} property of type @code{:string} (Read / Write)@br{}
  Title to appear in column header. @br{}
  Default value: \"\"")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible"
                                               'gtk-tree-view-column) 't)
 "The @code{\"visible\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether to display the column. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "widget"
                                               'gtk-tree-view-column) 't)
 "The @code{\"widget\"} property of type @class{gtk-widget} (Read / Write)@br{}
  Widget to put in column header button instead of column title.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width"
                                               'gtk-tree-view-column) 't)
 "The @code{\"width\"} property of type @code{:int} (Read)@br{}
  Current width of the column. @br{}
  Allowed values: >= 0@br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "x-offset"
                                               'gtk-tree-view-column) 't)
 "The @code{\"x-offset\"} property of type @code{:int} (Read)@br{}
  Current X position of the column. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-alignment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-alignment 'function)
 "@version{2013-3-26}
  Accessor of the slot \"alignment\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-cell-area 'function)
 "@version{2013-3-26}
  Accessor of the slot \"cell-area\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-clickable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-clickable 'function)
 "@version{2013-3-26}
  Accessor of the slot \"clickable\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-expand 'function)
 "@version{2013-3-26}
  Accessor of the slot \"expand\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-fixed-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-fixed-width 'function)
 "@version{2013-3-26}
  Accessor of the slot \"fixed-width\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-max-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-max-width 'function)
 "@version{2013-3-26}
  Accessor of the slot \"max-width\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-min-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-min-width 'function)
 "@version{2013-3-26}
  Accessor of the slot \"min-width\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-reorderable 'function)
 "@version{2013-3-26}
  Accessor of the slot \"reorderable\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-resizable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-resizable 'function)
 "@version{2013-3-26}
  Accessor of the slot \"resizable\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sizing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sizing 'function)
 "@version{2013-3-26}
  Accessor of the slot \"sizing\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-column-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-column-id 'function)
 "@version{2013-3-26}
  Accessor of the slot \"sort-column-id\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-indicator atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-indicator 'function)
 "@version{2013-3-26}
  Accessor of the slot \"sort-indicator\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-order atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-order 'function)
 "@version{2013-3-26}
  Accessor of the slot \"sort-order\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-spacing 'function)
 "@version{2013-3-26}
  Accessor of the slot \"spacing\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-title 'function)
 "@version{2013-3-26}
  Accessor of the slot \"title\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-visible 'function)
 "@version{2013-3-26}
  Accessor of the slot \"visible\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-widget 'function)
 "@version{2013-3-26}
  Accessor of the slot \"widget\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-width 'function)
 "@version{2013-3-26}
  Accessor of the slot \"width\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-x-offset atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-x-offset 'function)
 "@version{2013-3-26}
  Accessor of the slot \"x-offset\" of the
  @class{gtk-tree-view-column} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-new))

(defun gtk-tree-view-column-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-13}
  @return{A newly created @class{gtk-tree-view-column} object.}
  Creates a new @class{gtk-tree-view-column} object."
  (make-instance 'gtk-tree-view-column))

(export 'gtk-tree-view-column-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-new-with-area))

(defun gtk-tree-view-column-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-14}
  @argument[area]{the @class{gtk-cell-area} that the newly created column should
    use to layout cells}
  @return{A newly created @class{gtk-tree-view-column} object.}
  @begin{short}
    Creates a new @class{gtk-tree-view-column} using area to render its cells.
  @end{short}

  Since 3.0"
  (make-instance 'gtk-tree-view-column
                 :cell-area area))

(export 'gtk-tree-view-column-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_attributes ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-view-column-new-with-attributes (title cell &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[title]{the title to set the header to}
  @argument[cell]{the @class{gtk-cell-renderer} object}
  @argument[attributes]{a list of attributes}
  @return{A newly created @class{gtk-tree-view-column} object.}
  @begin{short}
    Creates a new @class{gtk-tree-view-column} with a number of default values.
    This is equivalent to calling the functions
    @fun{gtk-tree-view-column-set-title}, @fun{gtk-tree-view-column-pack-start},
    and @fun{gtk-tree-view-column-set-attributes} on the newly created
    @class{gtk-tree-view-column} object.
  @end{short}

  Here's a simple example:
  @begin{pre}
   enum { TEXT_COLUMN, COLOR_COLUMN, N_COLUMNS @};
   ...
   {
     GtkTreeViewColumn *column;
     GtkCellRenderer   *renderer = gtk_cell_renderer_text_new ();
 
     column = gtk_tree_view_column_new_with_attributes
                                                 (\"Title\",
                                                  renderer,
                                                  \"text\", TEXT_COLUMN,
                                                  \"foreground\", COLOR_COLUMN,
                                                  NULL);
   @}
  @end{pre}"
  (let ((column (make-instance 'gtk-tree-view-column
                               :title title)))
    (gtk-tree-view-column-pack-start column cell :expand t)
    (apply #'gtk-tree-view-column-set-attributes
           (cons column (cons cell attributes)))
    column))

(export 'gtk-tree-view-column-new-with-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_start" %gtk-tree-view-column-pack-start)
    :void
  (tree-column (g-object gtk-tree-view-column))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-start (tree-column cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell]{the @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if cell is to be given extra space allocated to
    @arg{tree-column} object}
  Packs the cell into the beginning of the column. If expand is @code{nil}, then
  the cell is allocated no more space than it needs. Any unused space is
  divided evenly between cells for which expand is @em{true}."
  (%gtk-tree-view-column-pack-start tree-column cell expand))

(export 'gtk-tree-view-column-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_end" %gtk-tree-view-column-pack-end) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-end (tree-column cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell]{the @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if cell is to be given extra space allocated to
    @arg{tree-column}}
  Adds the cell to end of the column. If expand is @code{nil}, then the cell is
  allocated no more space than it needs. Any unused space is divided evenly
  between cells for which expand is @em{true}."
  (%gtk-tree-view-column-pack-end tree-column cell expand))

(export 'gtk-tree-view-column-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear" gtk-tree-view-column-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  Unsets all the mappings on all renderers on the @arg{tree-column}."
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_add_attribute ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_add_attribute"
           gtk-tree-view-column-add-attribute) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-26}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-renderer]{the @class{gtk-cell-renderer} object to set
    attributes on}
  @argument[attribute]{an attribute on the renderer}
  @argument[column]{the column position on the model to get the attribute from}
  Adds an attribute mapping to the list in @arg{tree-column}. The column is the
  column of the model to get a value from, and the attribute is the parameter
  on @arg{cell-renderer} to be set from the value. So for example if column 2 of
  the model contains strings, you could have the \"text\" attribute of a
  @class{gtk-cell-renderer-text} get its values from column 2."
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (attribute :string)
  (column :int))

(export 'gtk-tree-view-column-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_attributes ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-view-column-set-attributes (tree-column
                                            cell-renderer
                                            &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-renderer]{the @class{gtk-cell-renderer} we are setting the
    attributes of}
  @argument[arguments]{a list of attributes}
  Sets the attributes in the list as the attributes of @arg{tree-column}. The
  attributes should be in attribute/column order, as in the function
  @fun{gtk-tree-view-column-add-attribute}. All existing attributes are removed,
  and replaced with the new attributes."
  (let ((n (/ (length attributes) 2)))
    (assert (eql n (truncate (length attributes) 2)))
    (dotimes (i n)
      (gtk-tree-view-column-add-attribute tree-column
                                          cell-renderer
                                          (pop attributes)
                                          (pop attributes)))))

(export 'gtk-tree-view-column-set-attributes)

;;; ----------------------------------------------------------------------------
;;; GtkTreeCellDataFunc ()
;;;
;;; void (*GtkTreeCellDataFunc) (GtkTreeViewColumn *tree_column,
;;;                              GtkCellRenderer *cell,
;;;                              GtkTreeModel *tree_model,
;;;                              GtkTreeIter *iter,
;;;                              gpointer data);
;;;
;;; A function to set the properties of a cell instead of just using the
;;; straight mapping between the cell and the model. This is useful for
;;; customizing the cell renderer. For example, a function might get an integer
;;; from the tree_model, and render it to the "text" attribute of "cell" by
;;; converting it to its written equivilent. This is set by calling
;;; gtk_tree_view_column_set_cell_data_func()
;;;
;;; tree_column :
;;;     A GtkTreeColumn
;;;
;;; cell :
;;;     The GtkCellRenderer that is being rendered by tree_column
;;;
;;; tree_model :
;;;     The GtkTreeModel being rendered
;;;
;;; iter :
;;;     A GtkTreeIter of the current row rendered
;;;
;;; data :
;;;     user data
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-cell-data-func-cb :void
    ((tree-column (g-object gtk-tree-view-column))
     (cell (g-object gtk-cell-renderer))
     (tree-model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (glib::get-stable-pointer-value data)))
    (restart-case
        (funcall fn tree-column cell tree-model iter)
      (return-from-gtk-tree-cell-data-func-cb () nil))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_cell_data_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_set_cell_data_func"
          %gtk-tree-view-column-set-cell-data-func) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (func :pointer)
  (func-data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-column-set-cell-data-func (tree-column cell-renderer func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-renderer]{a @class{gtk-cell-renderer} object}
  @argument[func]{the @class{gtk-tree-view-column-func} to use}
  Sets the @code{GtkTreeViewColumnFunc} to use for the column. This function is
  used instead of the standard attributes mapping for setting the column value,
  and should set the value of @arg{tree-column}'s cell renderer as appropriate.
  @arg{func} may be @code{nil} to remove an older one."
  (%gtk-tree-view-column-set-cell-data-func
                             tree-column
                             cell-renderer
                             (callback gtk-tree-cell-data-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-column-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear_attributes"
           gtk-tree-view-column-clear-attributes) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-renderer]{a @class{gtk-cell-renderer} to clear the attribute
    mapping on}
  Clears all existing attributes previously set with the function
  @fun{gtk-tree-view-column-set-attributes}.
  @see-function{gtk-tree-view-column-set-attributes}"
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-clear-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-spacing))

(defun gtk-tree-view-column-set-spacing (tree-column spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[spacing]{distance between cell renderers in pixels}
  Sets the spacing field of @arg{tree-column}, which is the number of pixels to
  place between cell renderers packed into it."
  (setf (gtk-tree-view-column-spacing tree-column) spacing))

(export 'gtk-tree-view-column-set-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-spacing))

(defun gtk-tree-view-column-get-spacing (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The spacing of @arg{tree-column}.}
  Returns the spacing of @arg{tree-column}."
  (gtk-tree-view-column-spacing tree-column))

(export 'gtk-tree-view-column-get-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-visible))

(defun gtk-tree-view-column-set-visible (tree-column visible)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[visible]{@em{true} if the @arg{tree-column} is visible}
  Sets the visibility of @arg{tree-column}."
  (setf (gtk-tree-view-column-visible tree-column) visible))

(export 'gtk-tree-view-column-set-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-visible))

(defun gtk-tree-view-column-get-visible (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{Whether the column is visible or not. If it is visible, then the tree
    will show the column.}
  Returns @em{true} if @arg{tree-column} is visible."
  (gtk-tree-view-column-visible tree-column))

(export 'gtk-tree-view-column-get-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_resizable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-resizable))

(defun gtk-tree-view-column-set-resizable (tree-column resizable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[resizable]{@em{true}, if the column can be resized}
  If resizable is @em{true}, then the user can explicitly resize the column by
  grabbing the outer edge of the column button. If resizable is @em{true} and
  sizing mode of the column is @code{:autosize}, then the sizing mode is changed
  to @code{:grow-only}."
  (setf (gtk-tree-view-column-resizable tree-column) resizable))

(export 'gtk-tree-view-column-set-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_resizable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-resizable))

(defun gtk-tree-view-column-get-resizable (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{@em{True}, if the @arg{tree-column} can be resized.}
  Returns @em{true} if the @arg{tree-column} can be resized by the end user."
  (gtk-tree-view-column-resizable tree-column))

(export 'gtk-tree-view-column-get-resizable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sizing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-sizing))

(defun gtk-tree-view-column-set-sizing (tree-column type)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[type]{the @class{gtk-tree-view-column-sizing} object}
  Sets the growth behavior of @arg{tree-column} to type."
  (setf (gtk-tree-view-column-sizing tree-column) type))

(export 'gtk-tree-view-column-set-sizing)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sizing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-sizing))

(defun gtk-tree-view-column-get-sizing (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The type of @arg{tree-column}.}
  Returns the current type of @arg{tree-column}."
  (gtk-tree-view-column-sizing tree-column))

(export 'gtk-tree-view-column-get-sizing)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-width))

(defun gtk-tree-view-column-get-width (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The current width of @arg{tree-column}.}
  Returns the current size of @arg{tree-column} in pixels."
  (gtk-tree-view-column-width tree-column))

(export 'gtk-tree-view-column-get-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_fixed_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-fixed-width))

(defun gtk-tree-view-column-get-fixed-width (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The fixed width of the column.}
  Gets the fixed width of the column. This value is only meaning may not be
  the actual width of the column on the screen, just what is requested."
  (gtk-tree-view-column-fixed-width tree-column))

(export 'gtk-tree-view-column-get-fixed-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_fixed_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-fixed-width))

(defun gtk-tree-view-column-set-fixed-width (tree-column fixed-width)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[fixed-width]{the size to set @arg{tree-column} to, must be greater
    than 0}
  Sets the size of the column in pixels. This is meaningful only if the sizing
  type is @code{:fixed}. The size of the column is clamped to the
  min/max width for the column. Please note that the min/max width of the
  column does not actually affect the @code{\"fixed_width\"} property of the
  widget, just the actual size when displayed."
  (setf (gtk-tree-view-column-fixed-width tree-column) fixed-width))

(export 'gtk-tree-view-column-set-fixed-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_min_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-min-width))

(defun gtk-tree-view-column-set-min-width (tree-column min-width)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[min-width]{the minimum width of the column in pixels, or -1}
  Sets the minimum width of the @arg{tree-column}. If @arg{min-width} is -1,
  then the minimum width is unset."
  (setf (gtk-tree-view-column-min-width tree-column) min-width))

(export 'gtk-tree-view-column-set-min-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_min_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-min-width))

(defun gtk-tree-view-column-get-min-width (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The minimum width of the @arg{tree-column}.}
  Returns the minimum width in pixels of the @arg{tree-column}, or -1 if no
  minimum width is set."
  (gtk-tree-view-column-min-width tree-column))

(export 'gtk-tree-view-column-get-min-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_max_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-max-width))

(defun gtk-tree-view-column-set-max-width (tree-column max-width)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[max-width]{the maximum width of the column in pixels, or -1}
  Sets the maximum width of the @arg{tree-column}. If @arg{max-width} is -1,
  then the maximum width is unset. Note, the column can actually be wider than
  max width if it is the last column in a view. In this case, the column expands
  to fill any extra space."
  (setf (gtk-tree-view-column-max-width tree-column) max-width))

(export 'gtk-tree-view-column-set-max-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_max_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-max-width))

(defun gtk-tree-view-column-get-max-width (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The maximum width of the @arg{tree-column}.}
  Returns the maximum width in pixels of the @arg{tree-column}, or -1 if no
  maximum width is set."
  (gtk-tree-view-column-max-width tree-column))

(export 'gtk-tree-view-column-get-max-width)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clicked ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clicked" gtk-tree-view-column-clicked) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gkt-tree-view-column} object}
  Emits the \"clicked\" signal on the column. This function will only work if
  @arg{tree-column} is clickable."
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-title))

(defun gtk-tree-view-column-set-title (tree-column title)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[title]{the title of the @arg{tree-column} object}
  Sets the title of the @arg{tree-column}. If a custom widget has been set, then
  this value is ignored."
  (setf (gtk-tree-view-column-title tree-column) title))

(export 'gtk-tree-view-column-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-title))

(defun gtk-tree-view-column-get-title (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The title of the column.}
  Returns the title of the widget."
  (gtk-tree-view-column-title tree-column))

(export 'gtk-tree-view-column-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_expand ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-expand))

(defun gtk-tree-view-column-set-expand (tree-column expand)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[expand]{@em{true} if the column should take available extra space,
    @code{nil} if not}
  @begin{short}
    Sets the column to take available extra space. This space is shared equally
    amongst all columns that have the expand set to @em{true}. If no column has
    this option set, then the last column gets all extra space. By default,
    every column is created with this @code{nil}.
  @end{short}

  Since 2.4"
  (setf (gtk-tree-view-column-expand tree-column) expand))

(export 'gtk-tree-view-column-set-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_expand ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-expand))

(defun gtk-tree-view-column-get-expand (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{@em{True}, if the column expands.}
  @begin{short}
    Return @em{true} if the column expands to take any available space.
  @end{short}

  Since 2.4"
  (gtk-tree-view-column-expand tree-column))

(export 'gtk-tree-view-column-get-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_clickable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-clickable))

(defun gtk-tree-view-column-set-clickable (tree-column clickable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[clickable]{@em{true} if the header is active}
  Sets the header to be active if clickable is @em{true}. When the header is
  active, then it can take keyboard focus, and can be clicked."
  (setf (gtk-tree-view-column-clickable tree-column) clickable))

(export 'gtk-tree-view-column-set-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_clickable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-clickable))

(defun gtk-tree-view-column-get-clickable (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{@em{True} if user can click the column header.}
  Returns @em{true} if the user can click on the header for the column."
  (gtk-tree-view-column-clickable tree-column))

(export 'gtk-tree-view-column-get-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-widget))

(defun gtk-tree-view-column-set-widget (tree-column widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[widget]{a child @class{gkt-widget}, or @code{nil}}
  Sets the widget in the header to be @arg{widget}. If widget is @code{nil},
  then the header button is set with a @class{gtk-label} set to the title of
  @arg{tree-column}."
  (setf (gtk-tree-view-column-widget tree-column) widget))

(export 'gtk-tree-view-column-set-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-widget))

(defun gtk-tree-view-column-get-widget (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{A @class{gtk-tree-view-column} object}
  @return{The @class{gtk-widget} in the column header, or @code{nil}.}
  Returns the @class{gtk-widget} in the button on the column header. If a custom
    widget has not been set then @code{nil} is returned."
  (gtk-tree-view-column-widget tree-column))

(export 'gtk-tree-view-column-get-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_get_button" gtk-tree-view-column-get-button)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The button for the column header.}
  @begin{short}
    Returns the button used in the treeview column header.
  @end{short}

  Since 3.0"
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-get-button)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-alignment))

(defun gtk-tree-view-column-set-alignment (tree-column xalign)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[xalign]{the alignment, which is between [0.0 and 1.0] inclusive}
  Sets the alignment of the title or custom widget inside the column header.
  The alignment determines its location inside the button -0.0 for left,
  0.5 for center, 1.0 for right."
  (setf (gtk-tree-view-column-alignment tree-column) xalign))

(export 'gtk-tree-view-column-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-alignment))

(defun gtk-tree-view-column-get-alignment (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The current alignent of @arg{tree-column}.}
  Returns the current x alignment of @arg{tree-column}. This value can range
  between 0.0 and 1.0."
  (gtk-tree-view-column-alignment tree-column))

(export 'gtk-tree-view-column-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-reorderable))

(defun gtk-tree-view-column-set-reorderable (tree-column reorderable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[reorderable]{@em{true}, if the column can be reordered}
  If reorderable is @em{true}, then the column can be reordered by the end user
  dragging the header."
  (setf (gtk-tree-view-column-reorderable tree-column) reorderable))

(export 'gtk-tree-view-column-set-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-reorderable))

(defun gtk-tree-view-column-get-reorderable (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{@em{True} if the @arg{tree-column} can be reordered by the user.}
  Returns @em{true} if the @arg{tree-column} can be reordered by the user."
  (gtk-tree-view-column-reorderable tree-column))

(export 'gtk-tree-view-column-get-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_column_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-sort-column-id))

(defun gtk-tree-view-column-set-sort-column-id (tree-column sort-column-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[sort-column-id]{The @code{sort-column-id} of the model to sort on}
  Sets the logical @arg{sort-column-id} that this column sorts on when this
  column is selected for sorting. Doing so makes the column header clickable."
  (setf (gtk-tree-view-column-sort-column-id tree-column) sort-column-id))

(export 'gtk-tree-view-column-set-sort-column-id)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_column_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-sort-column-id))

(defun gtk-tree-view-column-get-sort-column-id (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @begin{return}
    The current @code{sort-column-id} for this column, or -1 if this column
    cannot be used for sorting.
  @end{return}
  Gets the logical @code{sort-column-id} that the model sorts on when this
  column is selected for sorting. See the function
  @fun{gtk-tree-view-column-set-sort-column-id}.
  @see-function{gtk-tree-view-column-set-sort-column-id}"
  (gtk-tree-view-column-sort-column-id tree-column))

(export 'gtk-tree-view-column-get-sort-column-id)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_indicator ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-sort-indicator))

(defun gtk-tree-view-column-set-sort-indicator (tree-column setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[setting]{@em{true} to display an indicator that the column is
    sorted}
  Call this function with a setting of @em{true} to display an arrow in the
  header button indicating the column is sorted. Call the function
  @fun{gtk-tree-view-column-set-sort-order} to change the direction of the
  arrow.
  @see-function{gtk-tree-view-column-set-sort-order}"
  (setf (gtk-tree-view-column-sort-indicator tree-column) setting))

(export 'gtk-tree-view-column-set-sort-indicator)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_indicator ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-sort-indicator))

(defun gtk-tree-view-column-get-sort-indicator (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{Whether the sort indicator arrow is displayed.}
  Gets the value set by the function
  @fun{gtk-tree-view-column-set-sort-indicator}."
  (gtk-tree-view-column-sort-indicator tree-column))

(export 'gtk-tree-view-column-get-sort-indicator)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_order ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-set-sort-order))

(defun gtk-tree-view-column-set-sort-order (tree-column order)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[order]{sort order that the sort indicator should indicate}
  @begin{short}
    Changes the appearance of the sort indicator.
  @end{short}

  This does not actually sort the model. Use the function
  @fun{gtk-tree-view-column-set-sort-column-id} if you want automatic sorting
  support. This function is primarily for custom sorting behavior, and should
  be used in conjunction with the function
  @fun{gtk-tree-sortable-set-sort-column} to do that. For custom models, the
  mechanism will vary.

  The sort indicator changes direction to indicate normal sort or reverse
  sort. Note that you must have the sort indicator enabled to see anything
  when calling this function; see the function
  @fun{gtk-tree-view-column-set-sort-indicator}.
  @see-function{gtk-tree-view-column-set-sort-column-id}
  @see-function{gtk-tree-sortable-set-sort-column}"
  (setf (gtk-tree-view-column-sort-order tree-column) order))

(export 'gtk-tree-view-column-set-sort-order)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_order ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-sort-order))

(defun gtk-tree-view-column-get-sort-order (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The sort order the sort indicator is indicating.}
  Gets the value set by the function @fun{gtk-tree-view-column-set-sort-order}."
  (gtk-tree-view-column-sort-order tree-column))

(export 'gtk-tree-view-column-get-sort-order)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_set_cell_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_set_cell_data"
           gtk-tree-view-column-cell-set-cell-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[tree-model]{the @class{gtk-tree-model} to to get the cell renderers
    attributes from}
  @argument[iter]{the @class{gtk-tree-iter} to to get the cell renderer's
    attributes from}
  @argument[is-expander]{@em{true}, if the row has children}
  @argument[is-expanded]{@em{true}, if the row has visible children}
  Sets the cell renderer based on the @arg{tree-model} and @arg{iter}. That is,
  for every attribute mapping in @arg{tree-column}, it will get a value from the
  set column on the @arg{iter}, and use that value to set the attribute on the
  cell renderer. This is used primarily by the @class{gtk-tree-view}."
  (tree-column (g-object gtk-tree-view-column))
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'gtk-tree-view-column-cell-set-cell-data)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_size"
          %gtk-tree-view-column-cell-get-size) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-area (g-boxed-foreign gdk-rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun tree-view-column-cell-get-size (tree-column cell-area)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-area]{the area a cell in the column will be allocated,
    or @code{nil}}
  @argument[x-offset]{location to return x offset of a cell relative to
    @arg{cell-area}, or @code{nil}}
  @argument[y_offset]{location to return y offset of a cell relative to
    @arg{cell-area}, or @code{nil}}
  @argument[width]{location to return width needed to render a cell,
    or @code{nil}}
  @argument[height]{location to return height needed to render a cell,
    or @code{nil}}
  Obtains the width and height needed to render the column. This is used
  primarily by the @class{gtk-tree-view}."
  (with-foreign-objects ((x-offset :int)
                         (y-offset :int)
                         (width :int)
                         (height :int))
    (%gtk-tree-view-column-cell-get-size tree-column
                                         cell-area
                                         x-offset
                                         y-offset
                                         width
                                         height)
    (values (mem-ref x-offset :int)
            (mem-ref y-offset :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-tree-view-column-cell-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_position"
          %gtk-tree-view-column-cell-get-position) :boolean
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (x-offset (:pointer :int))
  (width (:pointer :int)))

(defun gtk-tree-view-column-cell-get-position (tree-column cell-renderer)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell-renderer]{a @class{gtk-cell-renderer} object}
  @argument[x-offset]{return location for the horizontal position of cell within
    @arg{tree-column}, may be @code{nil}}
  @argument[width]{return location for the width of cell, may be @code{nil}}
  @return{@em{True} if cell belongs to @arg{tree-column}.}
  Obtains the horizontal position and size of a cell in a column. If the cell
  is not found in the column, @arg{start-pos} and @arg{width} are not changed
  and @code{nil} is returned."
  (with-foreign-objects ((x-offset :int) (width :int))
    (when (%gtk-tree-view-column-cell-get-position tree-column
                                                   cell-renderer
                                                   x-offset
                                                   width)
      (values (mem-ref x-offset :int)
              (mem-ref width :int)))))

(export 'gtk-tree-view-column-cell-get-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_is_visible"
           gtk-tree-view-column-cell-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @begin{return}
    @em{True}, if any of the cells packed into the @arg{tree-column} are
    currently visible.
  @end{return}
  Returns @em{true} if any of the cells packed into the @arg{tree-column} are
  visible. For this to be meaningful, you must first initialize the cells with
  the function @fun{gtk-tree-view-column-cell-set-cell-data}."
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-cell-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_focus_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_focus_cell" gtk-tree-view-column-focus-cell)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @begin{short}
    Sets the current keyboard focus to be at cell, if the column contains 2 or
    more editable and activatable cells.
  @end{short}

  Since 2.2"
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-focus-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_queue_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_queue_resize"
          %gtk-tree-view-column-queue-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @begin{short}
    Flags the column, and the cell renderers added to this column, to have their
    sizes renegotiated.
  @end{short}

  Since 2.8"
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_tree_view ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_get_tree_view"
           gtk-tree-view-column-get-tree-view) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @begin{return}
    The tree view wherein column has been inserted if any, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the @class{gtk-tree-view} wherein @arg{tree-column} has been
    inserted. If column is currently not inserted in any tree view,
    @code{nil} is returned.
  @end{short}

  Since 2.12"
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-get-tree-view)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_x_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-get-x-offset))

(defun gtk-tree-view-column-get-x-offset (tree-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-16}
  @argument[tree-column]{a @class{gtk-tree-view-column} object}
  @return{The current X offset of @arg{tree-column}.}
  @begin{short}
    Returns the current X offset of @arg{tree-column} in pixels.
  @end{short}

  Since 3.2"
  (gtk-tree-view-column-x-offset tree-column))

(export 'gtk-tree-view-column-get-x-offset)

;;; --- End of file gtk.tree-view-column.lisp ----------------------------------
