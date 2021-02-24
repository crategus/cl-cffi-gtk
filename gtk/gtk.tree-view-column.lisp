;;; ----------------------------------------------------------------------------
;;; gtk.tree-view-column.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     A visible column in a GtkTreeView widget
;;;
;;; Types and Values
;;;
;;;     GtkTreeViewColumnSizing
;;;     GtkTreeViewColumn
;;;
;;; Functions
;;;
;;;     GtkTreeCellDataFunc
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
;;;     gtk_tree_view_column_set_spacing                   Accessor
;;;     gtk_tree_view_column_get_spacing                   Accessor
;;;     gtk_tree_view_column_set_visible                   Accessor
;;;     gtk_tree_view_column_get_visible                   Accessor
;;;     gtk_tree_view_column_set_resizable                 Accessor
;;;     gtk_tree_view_column_get_resizable                 Accessor
;;;     gtk_tree_view_column_set_sizing                    Accessor
;;;     gtk_tree_view_column_get_sizing                    Accessor
;;;     gtk_tree_view_column_get_width                     Accessor
;;;     gtk_tree_view_column_get_fixed_width               Accessor
;;;     gtk_tree_view_column_set_fixed_width               Accessor
;;;     gtk_tree_view_column_set_min_width                 Accessor
;;;     gtk_tree_view_column_get_min_width                 Accessor
;;;     gtk_tree_view_column_set_max_width                 Accessor
;;;     gtk_tree_view_column_get_max_width                 Accessor
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_set_title                     Accessor
;;;     gtk_tree_view_column_get_title                     Accessor
;;;     gtk_tree_view_column_set_expand                    Accessor
;;;     gtk_tree_view_column_get_expand                    Accessor
;;;     gtk_tree_view_column_set_clickable                 Accessor
;;;     gtk_tree_view_column_get_clickable                 Accessor
;;;     gtk_tree_view_column_set_widget                    Accessor
;;;     gtk_tree_view_column_get_widget                    Accessor
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_set_alignment                 Accessor
;;;     gtk_tree_view_column_get_alignment                 Accessor
;;;     gtk_tree_view_column_set_reorderable               Accessor
;;;     gtk_tree_view_column_get_reorderable               Accessor
;;;     gtk_tree_view_column_set_sort_column_id            Accessor
;;;     gtk_tree_view_column_get_sort_column_id            Accessor
;;;     gtk_tree_view_column_set_sort_indicator            Accessor
;;;     gtk_tree_view_column_get_sort_indicator            Accessor
;;;     gtk_tree_view_column_set_sort_order                Accessor
;;;     gtk_tree_view_column_get_sort_order                Accessor
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view
;;;     gtk_tree_view_column_get_x_offset                  Accessor
;;;
;;; Properties
;;;
;;;                  gfloat    alignment         Read / Write
;;;             GtkCellArea*   cell-area         Read / Write / Construct
;;;                gboolean    clickable         Read / Write
;;;                gboolean    expand            Read / Write
;;;                    gint    fixed-width       Read / Write
;;;                    gint    max-width         Read / Write
;;;                    gint    min-width         Read / Write
;;;                gboolean    reorderable       Read / Write
;;;                gboolean    resizable         Read / Write
;;; GtkTreeViewColumnSizing    sizing            Read / Write
;;;                    gint    sort-column-id    Read / Write
;;;                gboolean    sort-indicator    Read / Write
;;;             GtkSortType    sort-order        Read / Write
;;;                    gint    spacing           Read / Write
;;;                   gchar*   title             Read / Write
;;;                gboolean    visible           Read / Write
;;;               GtkWidget*   widget            Read / Write
;;;                    gint    width             Read
;;;                    gint    x-offset          Read
;;;
;;; Signals
;;;
;;;                    void    clicked           Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkTreeViewColumn
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeViewColumn implements GtkCellLayout and GtkBuildable.
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
(setf (gethash 'gtk-tree-view-column-sizing atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-tree-view-column-sizing atdoc:*external-symbols*)
 "@version{2021-2-24}
  @begin{short}
    The sizing method the tree view column uses to determine its width.
  @end{short}
  Please note that the value @code{:autosize} is inefficient for large tree
  views, and can make tree view columns appear choppy.
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
  @end{table}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-sizing}")

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-view-column 'type)
 "@version{2021-2-24}
  @begin{short}
    The @sym{gtk-tree-view-column} object represents a visible column in a
    @class{gtk-tree-view} widget.
  @end{short}
  It allows to set properties of the tree view column header, and functions as
  a holding pen for the cell renderers which determine how the data in the
  tree view column is displayed.

  Please refer to the tree view widget conceptual overview for an overview of
  all the objects and data types related to the tree view and how they work
  together.
  @begin[Signal Details]{dictionary}
    @subheading{The \"clicked\" signal}
      @begin{pre}
 lambda (treeviewcolumn)    : Run Last
      @end{pre}
    Emitted when the tree view column is clicked with the mouse or activated
    with the keyboard.
    @begin[code]{table}
      @entry[treeviewcolumn]{The @class{gtk-tree-view-column} object which
        emitted the signal.}
    @end{table}
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
  @see-slot{gtk-tree-view-column-x-offset}
  @see-class{gtk-tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tree-view-column-alignment -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "alignment"
                                               'gtk-tree-view-column) 't)
 "The @code{alignment} property of type @code{:float} (Read / Write) @br{}
  Alignment of the tree view column header text or widget, 0.0 for left,
  0.5 for center, and 1.0 for right alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-alignment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-alignment 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-alignment object) => align}
  @syntax[]{(setf (gtk-tree-view-column-alignment object) align)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[align]{a float with the alignment, which is between 0.0 and 1.0
    inclusive}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{alignment} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-alignment} returns the
  current alignment of the title or custom widget inside the tree view column
  header. The slot access function @sym{(setf gtk-tree-view-column-alignment)}
  sets the alignment. The alignment determines the location inside the header
  button, 0.0 for left, 0.5 for center, 1.0 for right alignment.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-cell-area -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area"
                                               'gtk-tree-view-column) 't)
 "The @code{cell-area} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers for this tree view column. If no
  cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk-cell-area-box} object will be used.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-cell-area 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-cell-area object) => cell-area}
  @syntax[]{(setf (gtk-tree-view-column-cell-area object) cell-area)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[cell-area]{a @class{gtk-cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{cell-area} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The cell area used to layout cell renderers for this tree view column. If no
  cell area is specified when creating the tree view column a horizontally
  oriented @class{gtk-cell-area-box} object will be used.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-clickable -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "clickable"
                                               'gtk-tree-view-column) 't)
 "The @code{clickable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view column header can be clicked. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-clickable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-clickable 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-clickable object) => clickable}
  @syntax[]{(setf (gtk-tree-view-column-clickable object) clickable)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[clickable]{@em{true} if the tree view column header is active}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{clickable} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-clickable} returns
  @em{true} if the user can click on the header for the tree view column. The
  slot access function @sym{(setf gtk-tree-view-column-clickable)} sets the
  header to be active if clickable is @em{true}. When the header is active,
  then it can take keyboard focus, and can be clicked.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-expand --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expand"
                                               'gtk-tree-view-column) 't)
 "The @code{expand} property of type @code{:boolean} (Read / Write) @br{}
  Tree view column gets share of extra width allocated to the widget. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-expand 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-expand object) => expand}
  @syntax[]{(setf (gtk-tree-view-column-expand object) expand)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[expand]{@em{true} if the tree view column should take available
    extra space, @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{expand} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-expand} returns @em{true}
  if the tree view column expands to take any available space. The slot access
  function @sym{(setf gtk-tree-view-column-expand)} sets the tree view column
  to take available extra space. This space is shared equally amongst all
  tree view columns that have the expand set to @em{true}. If no column has this
  option set, then the last column gets all extra space. By default, every
  column is created with this @em{false}.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-fixed-width ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fixed-width"
                                               'gtk-tree-view-column) 't)
 "The @code{fixed-width} property of type @code{:int} (Read / Write) @br{}
  Current fixed width of the tree view column. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-fixed-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-fixed-width 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-fixed-width object) => fixed-width}
  @syntax[]{(setf (gtk-tree-view-column-fixed-width object) fixed-width)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[fixed-width]{an integer with the size to set the tree view column
  to, must be greater than 0}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{fixed-width} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-fixed-width} gets the
  fixed width of the tree view column in pixels. The slot access function
  @sym{(setf gtk-tree-view-column-fixed-width)} sets the size. This is
  meaningful only if the @slot[gtk-tree-view-column]{sizing} property is
  @code{:fixed}. The size of the tree view column is clamped to the min/max
  width for the column. Please note that the min/max width of the column does
  not actually affect the @slot[gtk-tree-view-column]{fixed-width} property of
  the widget, just the actual size when displayed.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-sizing}
  @see-function{gtk-tree-view-column-max-width}
  @see-function{gtk-tree-view-column-min-width}")

;;; --- gtk-tree-view-column-max-width -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-width"
                                               'gtk-tree-view-column) 't)
 "The @code{max-width} property of type @code{:int} (Read / Write) @br{}
  Maximum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-max-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-max-width 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-max-width object) => max-width}
  @syntax[]{(setf (gtk-tree-view-column-max-width object) max-width)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[max-width]{an integer with the maximum width of the tree view
    column in pixels, or -1}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{max-width} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-max-width} returns the
  maximum width in pixels of the tree view column, or -1 if no maximum width is
  set. The slot access function @sym{(setf gtk-tree-view-column-max-width)}
  sets the maximum width. If @arg{max-width} is -1, then the maximum width is
  unset. Note, the tree view column can actually be wider than max width if it
  is the last column in a tree view. In this case, the column expands to fill
  any extra space.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-min-width}")

;;; --- gtk-tree-view-column-min-width -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-width"
                                               'gtk-tree-view-column) 't)
 "The @code{min-width} property of type @code{:int} (Read / Write )@br{}
  Minimum allowed width of the tree view column. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-min-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-min-width 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-min-width object) => min-width}
  @syntax[]{(setf (gtk-tree-view-column-min-width object) min-width)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[min-width]{an integer with the minimum width of the tree view
    column in pixels, or -1}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{min-width} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-min-width} returns the
  minimum width in pixels of the tree view column, or -1 if no minimum width is
  set. The slot access function @sym{(setf gtk-tree-view-column-min-width)}
  sets the minimum width. If @arg{min-width} is -1, then the minimum width is
  unset.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-max-width}")

;;; --- gtk-tree-view-column-reorderable ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reorderable"
                                               'gtk-tree-view-column) 't)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tree view column can be reordered around the headers. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-reorderable 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-reorderable object) => reorderable}
  @syntax[]{(setf (gtk-tree-view-column-reorderable object) reorderable)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[reorderable]{@em{true}, if the tree view column can be reordered}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{reorderable} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  If @code{reorderable} is @em{true}, then the tree view column can be
  reordered by the user dragging the header.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-resizable}")

;;; --- gtk-tree-view-column-resizable -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resizable"
                                               'gtk-tree-view-column) 't)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  Column is user-resizable. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-resizable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-resizable 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-resizable object) => resizable}
  @syntax[]{(setf (gtk-tree-view-column-resizable object) resizable)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[resizable]{@em{true}, if the tree view column can be resized}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{resizable} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-resizable} returns
  @em{true} if the tree view column can be resized by the user.

  If @code{resizable} is @em{true}, then the user can explicitly resize the
  tree view column by grabbing the outer edge of the column button. If
  @code{resizable} is @em{true} and the sizing mode of the tree view column is
  @code{:autosize}, then the sizing mode is changed to @code{:grow-only}.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-sizing}
  @see-function{gtk-tree-view-column-reorderable}")

;;; --- gtk-tree-view-column-sizing --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sizing"
                                               'gtk-tree-view-column) 't)
 "The @code{sizing} property of type @symbol{gtk-tree-view-column-sizing}
  (Read / Write) @br{}
  Resize mode of the tree view column. @br{}
  Default value: @code{:grow-only}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sizing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sizing 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-sizing object) => type}
  @syntax[]{(setf (gtk-tree-view-column-sizing object) type)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[type]{a value of the @symbol{gtk-tree-view-column-sizing}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{sizing} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-sizing} returns the
  current sizing type of the tree view column. The slot access function
  @sym{(setf gtk-tree-view-column-sizing)} sets the sizing type.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-resizable}")

;;; --- gtk-tree-view-column-sort-column-id ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-column-id"
                                               'gtk-tree-view-column) 't)
 "The @code{sort-column-id} property of type @code{:int} (Read / Write) @br{}
  Logical sort column ID this tree view column sorts on when selected for
  sorting. Setting the sort column ID makes the tree view column header
  clickable. Set to -1 to make the column unsortable. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-column-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-column-id 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-sort-column-id object) => sort-column-id}
  @syntax[]{(setf (gtk-tree-view-column-sort-column-id object) sort-column-id)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[sort-column-id]{an integer with the @code{sort-column-id} of the
    model to sort on}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{sort-column-id} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-sort-column-id} gets the
  logical sort column ID that the model sorts on when this tree view column is
  selected for sorting. The slot access function
  @sym{(setf gtk-tree-view-column-sort-column-id)} sets the logical sort column
  ID. Doing so makes the tree view column header clickable.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-sort-indicator}
  @see-function{gtk-tree-view-column-sort-order}")

;;; --- gtk-tree-view-column-sort-indicator ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-indicator"
                                               'gtk-tree-view-column) 't)
 "The @code{sort-indicator} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show a sort indicator. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-indicator atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-indicator 'function)
 "@version{2020-2-24}
  @syntax[]{(gtk-tree-view-column-sort-indicator object) => setting}
  @syntax[]{(setf (gtk-tree-view-column-sort-indicator object) setting)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[setting]{@em{true} to display an indicator that the tree view
    column is sorted}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{sort-indicator} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  Call this function with a setting of @em{true} to display an arrow in the
  header button indicating the tree view column is sorted. Call the function
  @fun{gtk-tree-view-column-sort-order} to change the direction of the arrow.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-sort-order}
  @see-function{gtk-tree-view-column-sort-column-id}")

;;; --- gtk-tree-view-column-sort-order ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sort-order"
                                               'gtk-tree-view-column) 't)
 "The @code{sort-order} property of type @symbol{gtk-sort-type} (Read / Write)
  @br{}
  Sort direction the sort indicator should indicate. @br{}
  Default value: @code{:ascending}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-sort-order atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-sort-order 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-sort-order object) => order}
  @syntax[]{(setf (gtk-tree-view-column-sort-order object) order)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[order]{a value of the @symbol{gtk-sort-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{sort-order} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  This does not actually sort the model. Use the function
  @fun{gtk-tree-view-column-sort-column-id} if you want automatic sorting
  support. This function is primarily for custom sorting behavior, and should
  be used in conjunction with the function
  @fun{gtk-tree-sortable-sort-column-id} to do that. For custom models, the
  mechanism will vary.

  The sort indicator changes direction to indicate normal sort or reverse
  sort. Note that you must have the sort indicator enabled to see anything
  when calling this function. See the function
  @fun{gtk-tree-view-column-sort-indicator}.
  @see-class{gtk-tree-view-column}
  @see-symbol{gtk-sort-type}
  @see-function{gtk-tree-view-column-sort-column-id}
  @see-function{gtk-tree-view-column-sort-indicator}
  @see-function{gtk-tree-sortable-sort-column-id}")

;;; --- gtk-tree-view-column-spacing -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing"
                                               'gtk-tree-view-column) 't)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  Space which is inserted between cell renderers. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-spacing 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-spacing object) => spacing}
  @syntax[]{(setf (gtk-tree-view-column-spacing object) spacing)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[spacing]{an integer with the distance between cell renderers in
    pixels}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{spacing} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-spacing} returns the
  spacing of the tree view column, which is the number of pixels to place
  between cell renderers packed into it. The slot access function
  @sym{(setf gtk-tree-view-column-spacing)} sets the spacing field of the tree
  view column.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-title ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                                               'gtk-tree-view-column) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  Title to appear in the tree view column header. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-title 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-title object) => title}
  @syntax[]{(setf (gtk-tree-view-column-title object) title)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[title]{a string with the title of the tree view column}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{title} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-title} returns the title
  of the the tree view column. The slot access function
  @sym{(setf gtk-tree-view-column-spacing)} sets the title. If a custom widget
  has been set, then this value is ignored.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-widget}")

;;; --- gtk-tree-view-column-visible -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible"
                                               'gtk-tree-view-column) 't)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether to display the tree view column. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-visible 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-visible object) => visible}
  @syntax[]{(setf (gtk-tree-view-column-visible object) visible)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[visible]{@em{true} if the tree view column is visible}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{visible} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-visible} returns whether
  the tree view column is visible or not. If it is visible, then the tree will
  show the column. The slot access function
  @sym{(setf gtk-tree-view-column-visible)} sets the visibility of the tree
  view column.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-widget --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "widget"
                                               'gtk-tree-view-column) 't)
 "The @code{widget} property of type @class{gtk-widget} (Read / Write) @br{}
  Widget to put in the tree view column header button instead of column title.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-widget 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-widget object) => widget}
  @syntax[]{(setf (gtk-tree-view-column-widget object) widget)}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[widget]{a @class{gtk-widget} child, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{widget} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-widget} returns the
  child widget in the button on the tree view column header. If a custom widget
  has not been set then @code{nil} is returned. The slot access function
  @sym{(setf gtk-tree-view-column-widget)} sets the child widget. If the child
  widget is @code{nil}, then the header button is set with a @class{gtk-label}
  widget set to the title of the tree view column.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-label}
  @see-function{gtk-tree-view-column-title}")

;;; --- gtk-tree-view-column-width ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width"
                                               'gtk-tree-view-column) 't)
 "The @code{width} property of type @code{:int} (Read) @br{}
  Current width of the tree view column. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-width 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-width object) => width}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[width]{an integer with the current width of the tree view column}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{width} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-width} returns the current
  size of the tree view column in pixels.
  @see-class{gtk-tree-view-column}")

;;; --- gtk-tree-view-column-x-offset ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "x-offset"
                                               'gtk-tree-view-column) 't)
 "The @code{x-offset} property of type @code{:int} (Read) @br{}
  Current x position of the tree view column. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-x-offset atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-column-x-offset 'function)
 "@version{2021-2-24}
  @syntax[]{(gtk-tree-view-column-x-offset object) => offset}
  @argument[object]{a @class{gtk-tree-view-column} object}
  @argument[offset]{an integer with the current x offset in pixels}
  @begin{short}
    Accessor of the @slot[gtk-tree-view-column]{x-offset} slot of the
    @class{gtk-tree-view-column} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-column-width} returns the current
  x offset of the tree view column in pixels.
  @see-class{gtk-tree-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-new))

(defun gtk-tree-view-column-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @return{A newly created @class{gtk-tree-view-column} object.}
  @begin{short}
    Creates a new tree view column.
  @end{short}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-new-with-area}
  @see-function{gtk-tree-view-column-new-with-attributes}"
  (make-instance 'gtk-tree-view-column))

(export 'gtk-tree-view-column-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-column-new-with-area))

(defun gtk-tree-view-column-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[area]{the @class{gtk-cell-area} object that the newly created
   tree view column should use to layout cell renderers}
  @return{A newly created @class{gtk-tree-view-column} object.}
  @begin{short}
    Creates a new tree view column using @arg{area} to render its cells.
  @end{short}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-area}
  @see-function{gtk-tree-view-column-new}
  @see-function{gtk-tree-view-column-new-with-attributes}"
  (make-instance 'gtk-tree-view-column
                 :cell-area area))

(export 'gtk-tree-view-column-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_attributes ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-view-column-new-with-attributes (title renderer
                                                 &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[title]{a string with the title to set the header to}
  @argument[renderer]{the @class{gtk-cell-renderer} object}
  @argument[attributes]{a list of attributes}
  @return{A newly created @class{gtk-tree-view-column} object.}
  @begin{short}
    Creates a new tree view column with a number of default values.
  @end{short}
  This is equivalent to calling the functions
  @fun{gtk-tree-view-column-title}, @fun{gtk-tree-view-column-pack-start},
  and @fun{gtk-tree-view-column-set-attributes} on the newly created
  @class{gtk-tree-view-column} object.

  Here's a simple example:
  @begin{pre}
(let* ((renderer (gtk-cell-renderer-text-new))
       (column (gtk-tree-view-column-new-with-attributes \"Example\"
                                                         renderer
                                                         \"text\" 0
                                                         \"foreground\" 1)))
  ... )
  @end{pre}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-tree-view-column-title}
  @see-function{gtk-tree-view-column-pack-start}
  @see-function{gtk-tree-view-column-set-attributes}"
  (let ((column (make-instance 'gtk-tree-view-column
                               :title title)))
    (gtk-tree-view-column-pack-start column renderer :expand t)
    (apply #'gtk-tree-view-column-set-attributes
           (cons column (cons renderer attributes)))
    column))

(export 'gtk-tree-view-column-new-with-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_start" %gtk-tree-view-column-pack-start)
    :void
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-start (column renderer &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{the @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer into the beginning of the tree view column.
  @end{short}
  If @arg{expand} is @em{false}, then the cell renderer is allocated no more
  space than it needs. Any unused space is divided evenly between cell
  renderers for which @arg{expand} is @em{true}.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-tree-view-column-pack-end}"
  (%gtk-tree-view-column-pack-start column renderer expand))

(export 'gtk-tree-view-column-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_end" %gtk-tree-view-column-pack-end) :void
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-end (column renderer &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{the @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if the cell renderer is to be given extra space
    allocated to the tree view column}
  @begin{short}
    Packs the cell renderer to the end of the tree view column.
  @end{short}
  If @arg{expand} is @em{false}, then the cell renderer is allocated no more
  space than it needs. Any unused space is divided evenly between cell
  renderers for which expand is @em{true}.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-tree-view-column-pack-start}"
  (%gtk-tree-view-column-pack-end column renderer expand))

(export 'gtk-tree-view-column-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear" gtk-tree-view-column-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @begin{short}
    Unsets all the mappings on all cell renderers on the tree view column.
  @end{short}
  @see-class{gtk-tree-view-column}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_add_attribute ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_add_attribute"
           gtk-tree-view-column-add-attribute) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{the @class{gtk-cell-renderer} object to set attributes on}
  @argument[attribute]{a string with an attribute on the cell renderer}
  @argument[position]{an integer with the column position on the model to get
    the attribute from}
  @begin{short}
    Adds an attribute mapping to the list in the tree view column.
  @end{short}
  The argument @arg{column} is the column of the model to get a value from, and
  the attribute is the parameter on the cell renderer to be set from the value.
  So for example if column 2 of the model contains strings, you could have the
  \"text\" attribute of a @class{gtk-cell-renderer-text} get its values from
  column 2.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-cell-renderer-text}
  @see-function{gtk-tree-view-column-new-with-attributes}"
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer))
  (attribute :string)
  (position :int))

(export 'gtk-tree-view-column-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_attributes ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-view-column-set-attributes (column renderer &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{the @class{gtk-cell-renderer} object we are setting the
    attributes of}
  @argument[attributes]{a list of attributes}
  @begin{short}
    Sets the attributes in the list as the attributes of the tree view column.
  @end{short}
  The attributes should be in attribute/column order, as in the function
  @fun{gtk-tree-view-column-add-attribute}. All existing attributes are removed,
  and replaced with the new attributes.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-tree-view-column-add-attribute}"
  (let ((n (/ (length attributes) 2)))
    (assert (eql n (truncate (length attributes) 2)))
    (dotimes (i n)
      (gtk-tree-view-column-add-attribute column
                                          renderer
                                          (pop attributes)
                                          (pop attributes)))))

(export 'gtk-tree-view-column-set-attributes)

;;; ----------------------------------------------------------------------------
;;; GtkTreeCellDataFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-cell-data-func :void
    ((column (g-object gtk-tree-view-column))
     (renderer (g-object gtk-cell-renderer))
     (model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((func (get-stable-pointer-value data)))
    (restart-case
      (funcall func column renderer model iter)
      (return-from-gtk-tree-cell-data-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-cell-data-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-cell-data-func atdoc:*external-symbols*)
 "@version{2021-2-24}
  @begin{short}
    A function to set the properties of a cell instead of just using the
    straight mapping between the cell and the model.
  @end{short}
  This is useful for customizing the cell renderer. For example, a function
  might get an integer from the tree model, and render it to the \"text\"
  attribute of \"cell\" by converting it to its written equivilent. This is set
  by calling the function @fun{gtk-tree-view-column-set-cell-data-func}.
  @begin{pre}
 lambda (column renderer model iter)
  @end{pre}
  @begin[code]{table}
    @entry[column]{A @class{gtk-tree-view-column} object.}
    @entry[renderer]{The @class{gtk-cell-renderer} object that is being
      rendered by the tree view column.}
    @entry[model]{The @class{gtk-tree-model} object being rendered.}
    @entry[iter]{A @class{gtk-tree-iter} object of the current row rendered.}
  @end{table}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-set-cell-data-func}")

(export 'gtk-tree-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_cell_data_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_set_cell_data_func"
          %gtk-tree-view-column-set-cell-data-func) :void
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun gtk-tree-view-column-set-cell-data-func (column renderer func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @argument[func]{the @symbol{gtk-tree-cell-data-func} callback to use}
  @begin{short}
    Sets the callback function to use for the tree view column.
  @end{short}
  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of the tree view column cell
  renderer as appropriate. @arg{func} may be @code{nil} to remove an older one.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-symbol{gtk-tree-cell-data-func}"
  (%gtk-tree-view-column-set-cell-data-func
                                   column
                                   renderer
                                   (callback gtk-tree-cell-data-func)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-column-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear_attributes"
           gtk-tree-view-column-clear-attributes) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{a @class{gtk-cell-renderer} to clear the attribute
    mapping on}
  @begin{short}
    Clears all existing attributes previously set e.g. with the function
    @fun{gtk-tree-view-column-set-attributes}.
  @end{short}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-set-attributes}"
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-clear-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clicked ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clicked" gtk-tree-view-column-clicked) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @begin{short}
    Emits the \"clicked\" signal on the tree view column.
  @end{short}
  This function will only work if the tree view column is clickable.
  @see-class{gtk-tree-view-column}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_button () -> gtk-tree-view-column-button
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_get_button" gtk-tree-view-column-button)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @return{The @class{gtk-widget} button for the tree view column header.}
  @begin{short}
    Returns the button used in the tree view column header.
  @end{short}
  @see-class{gtk-tree-view-column}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-button)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_set_cell_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_set_cell_data"
           gtk-tree-view-column-cell-set-cell-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[model]{the @class{gtk-tree-model} to to get the cell renderers
    attributes from}
  @argument[iter]{the @class{gtk-tree-iter} to to get the cell renderer's
    attributes from}
  @argument[is-expander]{@em{true}, if the row has children}
  @argument[is-expanded]{@em{true}, if the row has visible children}
  @begin{short}
    Sets the cell renderer based on the tree model and @arg{iter}.
  @end{short}
  That is, for every attribute mapping in the tree view column, it will get a
  value from the set column on the @arg{iter}, and use that value to set the
  attribute on the cell renderer. This is used primarily by the tree view.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (column (g-object gtk-tree-view-column))
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'gtk-tree-view-column-cell-set-cell-data)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_size () -> gtk-tree-view-column-cell-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_size" %gtk-tree-view-column-cell-size)
    :void
  (column (g-object gtk-tree-view-column))
  (area (g-boxed-foreign gdk-rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-tree-view-column-cell-size (column area)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[area]{a @class{gdk-rectangle} with the area a cell in the
    column will be allocated, or @code{nil}}
  @begin{return}
    @code{x-offset} -- an integer with the x offset of a cell relative to
      @arg{area} @br{}
    @code{y-offset} -- an integer with the y offset of a cell relative to
      @arg{area} @br{}
    @code{width} -- an integer with the width needed to render a cell @br{}
    @code{height} -- an integer with the height needed to render a cell
  @end{return}
  @begin{short}
    Obtains the width and height needed to render the column.
  @end{short}
  This is used primarily by the tree view.
  @see-class{gtk-tree-view-column}
  @see-class{gdk-rectangle}"
  (with-foreign-objects ((x-offset :int)
                         (y-offset :int)
                         (width :int)
                         (height :int))
    (%gtk-tree-view-column-cell-size column
                                     area
                                     x-offset
                                     y-offset
                                     width
                                     height)
    (values (mem-ref x-offset :int)
            (mem-ref y-offset :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-tree-view-column-cell-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_position ()
;;; -> gtk-tree-view-column-cell-position
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_position"
          %gtk-tree-view-column-cell-position) :boolean
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer))
  (x-offset (:pointer :int))
  (width (:pointer :int)))

(defun gtk-tree-view-column-cell-position (column renderer)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @begin{return}
    @code{x-offset} -- an integer with the horizontal position of the cell
      within the tree view column @br{}
    @code{width} -- an integer with the width of the cell
  @end{return}
  @begin{short}
    Obtains the horizontal position and size of a cell in a tree view column.
  @end{short}
  If the cell is not found in the column, @code{nil} is returned.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}"
  (with-foreign-objects ((x-offset :int) (width :int))
    (when (%gtk-tree-view-column-cell-position column
                                               renderer
                                               x-offset
                                               width)
      (values (mem-ref x-offset :int)
              (mem-ref width :int)))))

(export 'gtk-tree-view-column-cell-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_is_visible"
           gtk-tree-view-column-cell-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @begin{return}
    @em{True}, if any of the cells packed into the tree view column are
    currently visible.
  @end{return}
  @begin{short}
    Returns @em{true} if any of the cells packed into the tree view column are
    visible.
  @end{short}
  For this to be meaningful, you must first initialize the cells with the
  function @fun{gtk-tree-view-column-cell-set-cell-data}.
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-cell-set-cell-data}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-cell-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_focus_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_focus_cell" gtk-tree-view-column-focus-cell)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @argument[renderer]{a @class{gtk-cell-renderer} object}
  @begin{short}
    Sets the current keyboard focus to be at cell, if the column contains 2 or
    more editable and activatable cells.
  @end{short}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}"
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-focus-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_queue_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_queue_resize"
           gtk-tree-view-column-queue-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @begin{short}
    Flags the tree view column, and the cell renderers added to this column, to
    have their sizes renegotiated.
  @end{short}
  @see-class{gtk-tree-view-column}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_tree_view () -> gtk-tree-view-columun-tree-view
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_get_tree_view" gtk-tree-view-column-tree-view)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-24}
  @argument[column]{a @class{gtk-tree-view-column} object}
  @begin{return}
    The @class{gtk-tree-view} widet wherein the tree view column has been
    inserted if any, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Returns the tree view wherein the tree view column has been inserted.
  @end{short}
  If the column is currently not inserted in any tree view, @code{nil} is
  returned.
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-view}"
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-tree-view)

;;; --- End of file gtk.tree-view-column.lisp ----------------------------------
