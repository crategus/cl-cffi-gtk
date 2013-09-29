;;; ----------------------------------------------------------------------------
;;; gtk.icon-view.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkIconView
;;;
;;; A widget which displays a list of icons in a grid
;;;
;;; Synopsis
;;;
;;;     GtkIconView
;;;
;;;     gtk_icon_view_new
;;;     gtk_icon_view_new_with_area
;;;     gtk_icon_view_new_with_model
;;;     gtk_icon_view_set_model
;;;     gtk_icon_view_get_model
;;;     gtk_icon_view_set_text_column
;;;     gtk_icon_view_get_text_column
;;;     gtk_icon_view_set_markup_column
;;;     gtk_icon_view_get_markup_column
;;;     gtk_icon_view_set_pixbuf_column
;;;     gtk_icon_view_get_pixbuf_column
;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_convert_widget_to_bin_window_coords
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_set_selection_mode
;;;     gtk_icon_view_get_selection_mode
;;;     gtk_icon_view_set_item_orientation
;;;     gtk_icon_view_get_item_orientation
;;;     gtk_icon_view_set_columns
;;;     gtk_icon_view_get_columns
;;;     gtk_icon_view_set_item_width
;;;     gtk_icon_view_get_item_width
;;;     gtk_icon_view_set_spacing
;;;     gtk_icon_view_get_spacing
;;;     gtk_icon_view_set_row_spacing
;;;     gtk_icon_view_get_row_spacing
;;;     gtk_icon_view_set_column_spacing
;;;     gtk_icon_view_get_column_spacing
;;;     gtk_icon_view_set_margin
;;;     gtk_icon_view_get_margin
;;;     gtk_icon_view_set_item_padding
;;;     gtk_icon_view_get_item_padding
;;;     gtk_icon_view_get_cell_rect
;;;     gtk_icon_view_select_path
;;;     gtk_icon_view_unselect_path
;;;     gtk_icon_view_path_is_selected
;;;     gtk_icon_view_get_selected_items
;;;     gtk_icon_view_select_all
;;;     gtk_icon_view_unselect_all
;;;     gtk_icon_view_item_activated
;;;     gtk_icon_view_scroll_to_path
;;;     gtk_icon_view_get_visible_range
;;;     gtk_icon_view_set_tooltip_item
;;;     gtk_icon_view_set_tooltip_cell
;;;     gtk_icon_view_get_tooltip_context
;;;     gtk_icon_view_set_tooltip_column
;;;     gtk_icon_view_get_tooltip_column
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column
;;;
;;;     GtkIconViewDropPosition
;;;
;;;     gtk_icon_view_enable_model_drag_source
;;;     gtk_icon_view_enable_model_drag_dest
;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_reorderable
;;;     gtk_icon_view_get_reorderable
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkIconView
;;;
;;; Implemented Interfaces
;;;
;;; GtkIconView implements AtkImplementorIface, GtkBuildable, GtkCellLayout and
;;; GtkScrollable.
;;;
;;;
;;; Style Properties
;;;
;;;   "selection-box-alpha"      guchar               : Read
;;;   "selection-box-color"      GdkColor*            : Read
;;;
;;; Signals
;;;
;;;   "activate-cursor-item"                          : Action
;;;   "item-activated"                                : Run Last
;;;   "move-cursor"                                   : Action
;;;   "select-all"                                    : Action
;;;   "select-cursor-item"                            : Action
;;;   "selection-changed"                             : Run First
;;;   "toggle-cursor-item"                            : Action
;;;   "unselect-all"                                  : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkIconView
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIconView" gtk-icon-view
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkCellLayout")
   :type-initializer "gtk_icon_view_get_type")
  ((cell-area
    gtk-icon-view-cell-area
    "cell-area" "GtkCellArea" t t)
   (column-spacing
    gtk-icon-view-column-spacing
    "column-spacing" "gint" t t)
   (columns
    gtk-icon-view-columns
    "columns" "gint" t t)
   (item-orientation
    gtk-icon-view-item-orientation
    "item-orientation" "GtkOrientation" t t)
   (item-padding
    gtk-icon-view-item-padding
    "item-padding" "gint" t t)
   (item-width
    gtk-icon-view-item-width
    "item-width" "gint" t t)
   (margin
    gtk-icon-view-margin
    "margin" "gint" t t)
   (markup-column
    gtk-icon-view-markup-column
    "markup-column" "gint" t t)
   (model
    gtk-icon-view-model
    "model" "GtkTreeModel" t t)
   (pixbuf-column
    gtk-icon-view-pixbuf-column
    "pixbuf-column" "gint" t t)
   (reorderable
    gtk-icon-view-reorderable
    "reorderable" "gboolean" t t)
   (row-spacing
    gtk-icon-view-row-spacing
    "row-spacing" "gint" t t)
   (selection-mode
    gtk-icon-view-selection-mode
    "selection-mode" "GtkSelectionMode" t t)
   (spacing
    gtk-icon-view-spacing
    "spacing" "gint" t t)
   (text-column
    gtk-icon-view-text-column
    "text-column" "gint" t t)
   (tooltip-column
    gtk-icon-view-tooltip-column
    "tooltip-column" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-icon-view 'type)
 "@version{2013-6-19}
  @begin{short}
    @sym{gtk-icon-view} provides an alternative view on a
    @class{gtk-tree-model}. It displays the model as a grid of icons with
    labels. Like @class{gtk-tree-view}, it allows to select one or multiple
    items (depending on the selection mode, see the function
    @fun{gtk-icon-view-set-selection-mode}). In addition to selection with the
    arrow keys, @sym{gtk-icon-view} supports rubberband selection, which is
    controlled by dragging the pointer.
  @end{short}

  Note that if the tree model is backed by an actual tree store (as opposed to
  a flat list where the mapping to icons is obvious), @sym{gtk-icon-view} will
  only display the first level of the tree and ignore the tree's branches.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"selection-box-alpha\" style property}
      @code{\"selection-box-alpha\"} of type @code{:uchar} (Read) @br{}
      Opacity of the selection box. @br{}
      Default value: 64

    @subheading{The \"selection-box-color\" style property}
      @code{\"selection-box-color\"} of type @class{gdk-color} (Read) @br{}
      Color of the selection box.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-item\" signal}
      @begin{pre}
 lambda (iconview)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user activates the
      currently focused item.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control activation
      programmatically.
      The default bindings for this signal are Space, Return and Enter.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"item-activated\" signal}
      @begin{pre}
 lambda (iconview path)   : Run Last
      @end{pre}
      The \"item-activated\" signal is emitted when the method
      @fun{gtk-icon-view-item-activated} is called or the user double clicks an
      item. It is also emitted when a non-editable item is selected and one of
      the keys: Space, Return or Enter is pressed.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
        @entry[path]{The @class{gtk-tree-path} for the activated item.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (iconview step count)   : Action
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control the cursor
      programmatically.
      The default bindings for this signal include
      @begin{itemize}
        @item{Arrow keys which move by individual steps.}
        @item{Home/End keys which move to the first/last item.}
        @item{PageUp/PageDown which move by \"pages\".}
      @end{itemize}
      All of these will extend the selection when combined with the Shift
      modifier.
      @begin[code]{table}
        @entry[iconview]{The object which received the signal.}
        @entry[step]{The granularity of the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (iconview)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user selects all items.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control selection
      programmatically.
      The default binding for this signal is Ctrl-a.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"select-cursor-item\" signal}
      @begin{pre}
 lambda (iconview)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user selects the item that
      is currently focused.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control selection
      programmatically.
      There is no default binding for this signal.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
 lambda (iconview)   : Run First
      @end{pre}
     The \"selection-changed\" signal is emitted when the selection (i. e. the
     set of selected items) changes.
     @begin[code]{table}
       @entry[iconview]{The object on which the signal is emitted.}
     @end{table}
    @subheading{The \"toggle-cursor-item\" signal}
      @begin{pre}
 lambda (iconview)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user toggles whether the
      currently focused item is selected or not. The exact effect of this depend
      on the selection mode.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control selection
      programmatically.
      There is no default binding for this signal is Ctrl-Space.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (iconview)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user unselects all items.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control selection
      programmatically.
      The default binding for this signal is Ctrl-Shift-a.
      @begin[code]{table}
        @entry[iconview]{The object on which the signal is emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-icon-view-cell-area}
  @see-slot{gtk-icon-view-column-spacing}
  @see-slot{gtk-icon-view-columns}
  @see-slot{gtk-icon-view-item-orientation}
  @see-slot{gtk-icon-view-item-padding}
  @see-slot{gtk-icon-view-item-width}
  @see-slot{gtk-icon-view-margin}
  @see-slot{gtk-icon-view-markup-column}
  @see-slot{gtk-icon-view-model}
  @see-slot{gtk-icon-view-pixbuf-column}
  @see-slot{gtk-icon-view-reorderable}
  @see-slot{gtk-icon-view-row-spacing}
  @see-slot{gtk-icon-view-selection-mode}
  @see-slot{gtk-icon-view-spacing}
  @see-slot{gtk-icon-view-text-column}
  @see-slot{gtk-icon-view-tooltip-column}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area" 'gtk-icon-view) 't)
 "The @code{\"cell-area\"} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The @class{gtk-cell-area} used to layout cell renderers for this view.
  If no area is specified when creating the icon view with
  @fun{gtk-icon-view-new-with-area} a @class{gtk-cell-area-box} will be
  used. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-spacing"
                                               'gtk-icon-view) 't)
 "The @code{\"column-spacing\"} property of type @code{:int}
  (Read / Write) @br{}
  The @code{\"column-spacing\"} property specifies the space which is inserted
  between the columns of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "columns" 'gtk-icon-view) 't)
 "The @code{\"columns\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"columns\"} property contains the number of the columns in which
  the items should be displayed. If it is -1, the number of columns will
  be chosen automatically to fill the available area. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "item-orientation"
                                               'gtk-icon-view) 't)
 "The @code{\"item-orientation\"} property of type @symbol{gtk-orientation}
  (Read / Write) @br{}
  The @code{\"item-orientation\"} property specifies how the cells (i. e. the
  icon and the text) of the item are positioned relative to each other. @br{}
  Default value: @code{:vertical} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "item-padding"
                                               'gtk-icon-view) 't)
 "The @code{\"item-padding\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"item-padding\"} property specifies the padding around each of the
  icon view's item. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6 @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "item-width" 'gtk-icon-view) 't)
 "The @code{\"item-width\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"item-width\"} property specifies the width to use for each item.
  If it is set to @code{-1}, the icon view will automatically determine a
  suitable item size. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "margin" 'gtk-icon-view) 't)
 "The @code{\"margin\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"margin\"} property specifies the space which is inserted at the
  edges of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "markup-column"
                                               'gtk-icon-view) 't)
 "The @code{\"markup-column\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"markup-column\"} property contains the number of the model column
  containing markup information to be displayed. The markup column must be of
  type @var{+g-type-string+}. If this property and the @code{\"text-column\"}
  property are both set to column numbers, it overrides the text column. If both
  are set to -1, no texts are displayed. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-icon-view) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write) @br{}
  The model for the icon view.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-column"
                                               'gtk-icon-view) 't)
 "The @code{\"pixbuf-column\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"pixbuf-column\"} property contains the number of the model column
  containing the pixbufs which are displayed. The pixbuf column must be of
  type @code{GDK_TYPE_PIXBUF}. Setting this property to -1 turns off the
  display of pixbufs. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reorderable" 'gtk-icon-view) 't)
 "The @code{\"reorderable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  The reorderable property specifies if the items can be reordered by DND. @br{}
  Default value: @code{nil} @br{}
  Since 2.8")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-spacing" 'gtk-icon-view) 't)
 "The @code{\"row-spacing\"} property of type @code{:int} (Read / Write) @br{}
  The row-spacing property specifies the space which is inserted between the
  rows of the icon view. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-mode"
                                               'gtk-icon-view) 't)
 "The @code{\"selection-mode\"} property of type @symbol{gtk-selection-mode}
  (Read / Write) @br{}
  The @code{\"selection-mode\"} property specifies the selection mode of an icon
  view. If the mode is @code{:multiple}, rubberband selection is enabled, for
  the other modes, only keyboard selection is possible. @br{}
  Default value: @code{:single} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-icon-view) 't)
 "The @code{\"spacing\"} property of type @code{:int} (Read / Write) @br{}
  The spacing property specifies the space which is inserted between the cells
  (i. e. the icon and the text) of an item. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-column" 'gtk-icon-view) 't)
 "The @code{\"text-column\"} property of type @code{:int} (Read / Write) @br{}
  The @code{\"text-column\"} property contains the number of the model column
  containing the texts which are displayed. The text column must be of type
  @code{G_TYPE_STRING}. If this property and the @code{\"markup-column\"}
  property are both set to -1, no texts are displayed. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-column"
                                               'gtk-icon-view) 't)
 "The @code{\"tooltip-column\"} property of type @code{:int}
  (Read / Write) @br{}
  The column in the model containing the tooltip texts for the items. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-cell-area 'function)
 "@version{2013-3-9}
  @begin{short}
    Accessor of the slot @code{\"cell-area\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-column-spacing 'function)
 "@version{2013-3-9}
  @begin{short}
    Accessor of the slot @code{\"column-spacing\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-columns atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-columns 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"columns\"} of the @class{gtk-icon-view} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-item-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-item-orientation 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"item-orientation\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-item-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-item-padding 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"item-padding\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-item-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-item-width 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"item-width\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-margin 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"margin\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-markup-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-markup-column 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"markup-column\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-model 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"model\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-pixbuf-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-pixbuf-column 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"pixbuf-column\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-reorderable 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"reorderable\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-row-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-row-spacing 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"row-spacing\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-selection-mode 'function)
 "@version{2013-7-4}
  Accessor of the slot @code{\"selection-mode\"} of the @class{gtk-icon-view}
  class.
  @see-function{gtk-icon-view-get-selection-mode}
  @see-function{gtk-icon-view-set-selection-mode}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-spacing 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"spacing\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-text-column 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"text-column\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-tooltip-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-icon-view-tooltip-column 'function)
 "@version{2013-3-8}
  @begin{short}
    Accessor of the slot @code{\"tooltip-column\"} of the @class{gtk-icon-view}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-new))

(defun gtk-icon-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @return{A newly created @class{gtk-icon-view} widget.}
  @begin{short}
    Creates a new @class{gtk-icon-view} widget.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}"
  (make-instance 'gtk-icon-view))

(export 'gtk-icon-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-new-with-area))

(defun gtk-icon-view-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[area]{the @class{gtk-cell-area} object to use to layout cells}
  @return{A newly created @class{gtk-icon-view} widget.}
  @begin{short}
    Creates a new @class{gtk-icon-view} widget using the specified area to
    layout cells inside the icons.
  @end{short}

  Since 3.0
  @see-class{gtk-icon-view}
  @see-class{gtk-cell-area}"
  (make-instance 'gtk-icon-view
                 :cell-area area))

(export 'gtk-icon-view-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-new-with-model))

(defun gtk-icon-view-new-with-model (model)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[model]{the @class{gtk-tree-model}}
  @return{A newly created @class{gtk-icon-view} widget.}
  @begin{short}
    Creates a new @class{gtk-icon-view} widget with the model model.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-model}"
  (make-instance 'gtk-icon-view
                 :model model))

(export 'gtk-icon-view)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-model))

(defun gtk-icon-view-set-model (icon-view model)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[model]{the @class{gtk-tree-model} object}
  @begin{short}
    Sets the model for a @class{gtk-icon-view} widget.
  @end{short}
  If the @arg{icon-view} already has a model set, it will remove it before
  setting the new model. If @arg{model} is @code{nil}, then it will unset the
  old model.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-model}
  @see-function{gtk-icon-view-get-model}"
  (setf (gtk-icon-view-model icon-view) model))

(export 'gtk-icon-view-set-model)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-model))

(defun gtk-icon-view-get-model (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{A @class{gtk-tree-model} object, or @code{nil} if none is currently
    being used.}
  @begin{short}
    Returns the model the @class{gtk-icon-view} is based on.
  @end{short}
  Returns @code{nil} if the model is unset.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-model}
  @see-function{gtk-icon-view-set-model}"
  (gtk-icon-view-model icon-view))

(export 'gtk-icon-view-get-model)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_text_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-text-column))

(defun gtk-icon-view-set-text-column (icon-view column)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[column]{a column in the currently used model, or -1 to display
    no text}
  @begin{short}
    Sets the column with text for @arg{icon-view} to be @arg{column}.
  @end{short}
  The text column must be of type @code{G_TYPE_STRING}.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-text-column}"
  (setf (gtk-icon-view-text-column icon-view) column))

(export 'gtk-icon-view-set-text-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_text_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-text-column))

(defun gtk-icon-view-get-text-column (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The text column, or -1 if it is unset.}
  @short{Returns the column with text for @arg{icon-view}.}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-text-column}"
  (gtk-icon-view-text-column icon-view))

(export 'gtk-icon-view-get-text-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_markup_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-markup-column))

(defun gtk-icon-view-set-markup-column (icon-view column)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[column]{a column in the currently used model, or -1 to display no
    text}
  @begin{short}
    Sets the column with markup information for @arg{icon-view} to be column.
  @end{short}
  The markup column must be of type @code{G_TYPE_STRING}. If the markup column
  is set to something, it overrides the text column set by the function
  @fun{gtk-icon-view-set-text-column}.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-text-column}
  @see-function{gtk-icon-view-get-markup-column}"
  (setf (gtk-icon-view-markup-column icon-view) column))

(export 'gtk-icon-view-set-markup-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_markup_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-markup-column))

(defun gtk-icon-view-get-markup-column (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The markup column, or -1 if it is unset.}
  @short{Returns the column with markup text for @arg{icon-view}.}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-markup-column}"
  (gtk-icon-view-markup-column icon-view))

(export 'gtk-icon-view-get-markup-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_pixbuf_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-pixbuf-column))

(defun gtk-icon-view-set-pixbuf-column (icon-view column)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[column]{a column in the currently used model, or -1 to disable}
  @begin{short}
    Sets the column with pixbufs for @arg{icon-view} to be column.
  @end{short}
  The pixbuf column must be of type @code{GDK_TYPE_PIXBUF}.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-pixbuf-column}"
  (setf (gtk-icon-view-pixbuf-column icon-view) column))

(export 'gtk-icon-view-set-pixbuf-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_pixbuf_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-pixbuf-column))

(defun gtk-icon-view-get-pixbuf-column (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The pixbuf column, or -1 if it is unset.}
  @short{Returns the column with pixbufs for @arg{icon-view}.}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-pixbuf-column}"
  (gtk-icon-view-pixbuf-column icon-view))

(export 'gtk-icon-view-get-pixbuf-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_path_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_path_at_pos" gtk-icon-view-get-path-at-pos)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-19}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[x]{the x position to be identified}
  @argument[y]{the y position to be identified}
  @begin{return}
    The @class{gtk-tree-path} corresponding to the icon or @code{nil} if no
    icon exists at that position.
  @end{return}
  @begin{short}
    Finds the path at the point (x, y), relative to @code{bin_window}
    coordinates.
  @end{short}
  See the function @fun{gtk-icon-view-get-item-at-pos}, if you are also
  interested in the cell at the specified position. See the function
  @fun{gtk-icon-view-convert-widget-to-bin-window-coords} for converting widget
  coordinates to @code{bin_window} coordinates.

  Since 2.6
  @see-function{gtk-icon-view-get-item-at-pos}
  @see-function{gtk-icon-view-convert-widget-to-bin-window-coords}"
  (icon-view g-object)
  (x :int)
  (y :int))

(export 'gtk-icon-view-get-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_item_at_pos" %gtk-icon-view-get-item-at-pos)
    :boolean
  (icon-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (cell :pointer))

(defun gtk-icon-view-get-item-at-pos (icon-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-19}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[x]{the x position to be identified}
  @argument[y]{the y position to be identified}
  @begin{return}
    @code{path} -- the path, or @code{nil} @br{}
    @code{cell} -- the renderer responsible for the cell at (x, y),
                   or @code{nil} @br{}
    or @br{}
    @code{nil} if no item exists at the specified position
  @end{return}
  @begin{short}
    Finds the path at the point (x, y), relative to @code{bin_window}
    coordinates.
  @end{short}
  In contrast to the function @fun{gtk-icon-view-get-path-at-pos}, this function
  also obtains the cell at the specified position. The returned path should be
  freed with the function @fun{gtk-tree-path-free}. See the function
  @fun{gtk-icon-view-convert-widget-to-bin-window-coords} for converting widget
  coordinates to @code{bin_window} coordinates.

  Since 2.8
  @see-function{gtk-icon-view-get-path-at-pos}
  @see-function{gtk-icon-view-convert-widget-to-bin-window-coords}"
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%gtk-icon-view-get-item-at-pos icon-view x y path cell)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'gtk-icon-view-get-item-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_convert_widget_to_bin_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_convert_widget_to_bin_window_coords"
          %gtk-icon-view-convert-widget-to-bin-window-coords) :void
  (icon-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-icon-view-convert-widget-to-bin-window-coords (icon-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-19}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[wx]{x coordinate relative to the widget}
  @argument[wy]{y coordinate relative to the widget}
  @begin{return}
    @code{bx} -- @code{bin_window} x coordinate @br{}
    @code{by} -- @code{bin_window} y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the @code{bin_window}, as
    expected by e. g. the function @fun{gtk-icon-view-get-path-at-pos}.
  @end{short}

  Since 2.12
  @see-function{gtk-icon-view-get-path-at-pos}"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-icon-view-convert-widget-to-bin-window-coords icon-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-icon-view-convert-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_cursor" gtk-icon-view-set-cursor) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-19}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{a @class{gtk-tree-path} structure}
  @argument[cell]{one of the cell renderers of @arg{icon-view}, or @code{nil}}
  @argument[start-editing]{@em{true} if the specified cell should start being
    edited}
  @begin{short}
    Sets the current keyboard focus to be at path, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  item. If cell is not @code{nil}, then focus is given to the cell specified by
  it. Additionally, if @arg{start-editing} is @em{true}, then editing should be
  started in the specified cell.

  This function is often followed by @code{(gtk-widget-grab-focus icon-view)} in
  order to give keyboard focus to the widget. Please note that editing can
  only happen when the widget is realized.

  Since 2.8"
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (cell g-object)
  (start-editing :boolean))

(export 'gtk-icon-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_cursor" %gtk-icon-view-get-cursor) :boolean
  (icon-view g-object)
  (path :pointer)
  (cell :pointer))

(defun gtk-icon-view-get-cursor (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{return}
    @code{path} -- the current cursor path, or @code{nil} @br{}
    @code{cell} -- the current focus cell, or @code{nil} @br{}
    or @br{}
    @code{nil} if the cursor is not set.
  @end{return}
  @begin{short}
    Fills in @arg{path} and @arg{cell} with the current cursor path and cell.
  @end{short}
  If the cursor is not currently set, then @arg{path} will be @code{nil}. If no
  cell currently has focus, then @arg{cell} will be @code{nil}.

  The returned @class{gtk-tree-path} must be freed with the function
  @fun{gtk-tree-path-free}.

  Since 2.8"
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%gtk-icon-view-get-cursor icon-view path cell)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'gtk-icon-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; GtkIconViewForeachFunc ()
;;;
;;; void (*GtkIconViewForeachFunc) (GtkIconView *icon_view,
;;;                                 GtkTreePath *path,
;;;                                 gpointer data);
;;;
;;; A function used by gtk_icon_view_selected_foreach() to map all selected
;;; rows. It will be called on every selected row in the view.
;;;
;;; icon_view :
;;;     a GtkIconView
;;;
;;; path :
;;;     The GtkTreePath of a selected row
;;;
;;; data :
;;;     user data
;;; ----------------------------------------------------------------------------

(defcallback gtk-icon-view-foreach-func-callback :void
    ((icon-view g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (data :pointer))
  (restart-case
      (funcall (glib:get-stable-pointer-value data)
               icon-view
               path)
    (return () nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_selected_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_selected_foreach" %gtk-icon-view-selected-foreach)
    :void
  (icon-view g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-icon-view-selected-foreach (icon-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[func]{the function to call for each selected icon}
  @begin{short}
    Calls a function for each selected icon. Note that the model or selection
    cannot be modified from within this function.
  @end{short}

  Since 2.6"
  (with-stable-pointer (ptr func)
    (%gtk-icon-view-selected-foreach
        icon-view
        (callback gtk-icon-view-foreach-func-callback)
        ptr)))

(export 'gtk-icon-view-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_selection_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-selection-mode))

(defun gtk-icon-view-set-selection-mode (icon-view mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[mode]{the selection mode of type @symbol{gtk-selection-mode}}
  @begin{short}
    Sets the selection mode of the @arg{icon-view}.
  @end{short}

  Since 2.6
  @see-function{gtk-icon-view-get-selection-mode}"
  (setf (gtk-icon-view-selection-mode icon-view) mode))

(export 'gtk-icon-view-set-selection-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_selection_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-selection-mode))

(defun gtk-icon-view-get-selection-mode (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The current selection mode of type @symbol{gtk-selection-mode}.}
  @begin{short}
    Gets the selection mode of the @arg{icon-view}.
  @end{short}

  Since 2.6
  @see-function{gtk-icon-view-set-selection-mode}"
  (gtk-icon-view-selection-mode icon-view))

(export 'gtk-icon-view-get-selection-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_orientation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-item-orientation))

(defun gtk-icon-view-set-item-orientation (icon-view orientation)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[orientation]{the relative position of texts and icons}
  @begin{short}
    Sets the @code{\"item-orientation\"} property which determines whether the
    labels are drawn beside the icons instead of below.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-symbol{gtk-orientation}
  @see-function{gtk-icon-view-get-item-orientation}"
  (setf (gtk-icon-view-item-orientation icon-view) orientation))

(export 'gtk-icon-view-set-item-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_orientation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-item-orientation))

(defun gtk-icon-view-get-item-orientation (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The relative position of texts and icons.}
  @begin{short}
    Returns the value of the @code{\"item-orientation\"} property which
    determines whether the labels are drawn beside the icons instead of below.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-item-orientation}"
  (gtk-icon-view-item-orientation icon-view))

(export 'gtk-icon-view-get-item-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_columns ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-colums))

(defun gtk-icon-view-set-columns (icon-view columns)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[columns]{the number of columns}
  @begin{short}
    Sets the @code{\"columns\"} property which determines in how many columns
    the icons are arranged.
  @end{short}
  If columns is -1, the number of columns will be chosen automatically to fill
  the available area.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-columns}"
  (setf (gtk-icon-view-columns icon-view) columns))

(export 'gtk-icon-view-set-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_columns ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-columns))

(defun gtk-icon-view-get-columns (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The number of columns, or -1.}
  @short{Returns the value of the @code{\"columns\"} property.}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-columns}"
  (gtk-icon-view-columns icon-view))

(export 'gtk-icon-view-get-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-item-width))

(defun gtk-icon-view-set-item-width (icon-view item-width)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[item-width]{the width for each item}
  @begin{short}
    Sets the @code{\"item-width\"} property which specifies the width to use for
    each item.
  @end{short}
  If it is set to -1, the icon view will automatically determine a suitable item
  size.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-item-width}"
  (setf (gtk-icon-view-item-width icon-view) item-width))

(export 'gtk-icon-view-set-item-width)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-item-width))

(defun gtk-icon-view-get-item-width (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The width of a single item, or -1.}
  @short{Returns the value of the @code{\"item-width\"} property.}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-item-width}"
  (gtk-icon-view-item-width icon-view))

(export 'gtk-icon-view-get-item-width)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-spacing))

(defun gtk-icon-view-set-spacing (icon-view spacing)
 #+cl-cffi-gtk-documentation
 "@version{2ß13-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[spacing]{the spacing}
  @begin{short}
    Sets the @code{\"spacing\"} property which specifies the space which is
    inserted between the cells, i. e. the icon and the text, of an item.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-spacing}"
  (setf (gtk-icon-view-spacing icon-view) spacing))

(export 'gtk-icon-view-set-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-spacing))

(defun gtk-icon-view-get-spacing (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The space between cells.}
  @begin{short}
    Returns the value of the @code{\"spacing\"} property.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-spacing}"
  (gtk-icon-view-spacing icon-view))

(export 'gtk-icon-view-get-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_row_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-row-spacing))

(defun gtk-icon-view-set-row-spacing (icon-view row-spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[row-spacing]{the row spacing}
  @begin{short}
    Sets the @code{\"row-spacing\"} property which specifies the space which
    is inserted between the rows of the icon view.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-row-spacing}"
  (setf (gtk-icon-view-row-spacing icon-view) row-spacing))

(export 'gtk-icon-view-set-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_row_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-row-spacing))

(defun gtk-icon-view-get-row-spacing (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The space between rows.}
  @begin{short}
    Returns the value of the @code{\"row-spacing\"} property.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-row-spacing}"
  (gtk-icon-view-row-spacing icon-view))

(export 'gtk-icon-view-get-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_column_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-column-spacing))

(defun gtk-icon-view-set-column-spacing (icon-view column-spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[column-spacing]{the column spacing}
  @begin{short}
    Sets the @code{\"column-spacing\"} property which specifies the space which
    is inserted between the columns of the icon view.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-column-spacing}"
  (setf (gtk-icon-view-column-spacing icon-view) column-spacing))

(export 'gtk-icon-view-set-column-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_column_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-column-spacing))

(defun gtk-icon-view-get-column-spacing (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The space between columns.}
  @begin{short}
    Returns the value of the @code{\"column-spacing\"} property.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-column-spacing}"
  (gtk-icon-view-column-spacing icon-view))

(export 'gtk-icon-view-get-column-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-margin))

(defun gtk-icon-view-set-margin (icon-view margin)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[margin]{the margin}
  @begin{short}
    Sets the @code{\"margin\"} property which specifies the space which is
    inserted at the top, bottom, left and right of the icon view.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-margin}"
  (setf (gtk-icon-view-margin icon-view) margin))

(export 'gtk-icon-view-set-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-margin))

(defun gtk-icon-view-get-margin (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The space at the borders.}
  @begin{short}
    Returns the value of the @code{\"margin\"} property.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-margin}"
  (gtk-icon-view-margin icon-view))

(export 'gtk-icon-view-get-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-item-padding))

(defun gtk-icon-view-set-item-padding (icon-view item-padding)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[item-padding]{the item padding}
  @begin{short}
    Sets the @code{\"item-padding\"} property which specifies the padding around
    each of the icon view's items.
  @end{short}

  Since 2.18
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-item-padding}"
  (setf (gtk-icon-view-item-padding icon-view) item-padding))

(export 'gtk-icon-view-set-item-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-item-padding))

(defun gtk-icon-view-get-item-padding (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{The padding around items.}
  @begin{short}
    Returns the value of the @code{\"item-padding\"} property.
  @end{short}

  Since 2.18
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-item-padding}"
  (gtk-icon-view-item-padding icon-view))

(export 'gtk-icon-view-get-item-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_cell_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_cell_rect" %gtk-icon-view-get-cell-rect) :boolean
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path))
  (cell (g-object gtk-cell-renderer))
  (rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-icon-view-get-cell-rect (icon-view path cell)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{a @class{gtk-tree-path}}
  @argument[cell]{a @class{gtk-cell-renderer} or @code{nil}}
  @return{rect -- rectangle with cell rect or @code{nil}}
  @begin{short}
    Fills the bounding rectangle in widget coordinates for the cell specified by
    path and cell. If cell is @code{nil} the main cell area is used.
  @end{short}

  This function is only valid if @arg{icon-view} is realized.

  Since 3.6
  @see-class{gtk-icon-view}"
  (let ((rect (make-gdk-rectangle)))
    (when (%gtk-icon-view-get-cell-rect icon-view path cell rect)
      rect)))

(export 'gtk-icon-view-get-cell-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_select_path" gtk-icon-view-select-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the @class{gtk-tree-path} to be selected}
  @short{Selects the row at @arg{path}.}

  Since 2.6"
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unselect_path" gtk-icon-view-unselect-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the @class{gtk-tree-path} to be unselected}
  @short{Unselects the row at @arg{path}.}

  Since 2.6"
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_path_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_path_is_selected" gtk-icon-view-path-is-selected)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{a @class{gtk-tree-path} to check selection on}
  @return{@em{True} if @arg{path} is selected.}
  @begin{short}
    Returns @em{true} if the icon pointed to by @arg{path} is currently
    selected.
  @end{short}
  If @arg{path} does not point to a valid location, @code{nil} is returned.

  Since 2.6"
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_selected_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_selected_items" gtk-icon-view-get-selected-items)
    (g-list (g-boxed-foreign gtk-tree-path) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{A list containing a @class{gtk-tree-path} for each selected row.}
  @begin{short}
    Creates a list of paths of all selected items.
  @end{short}
  Additionally, if you are planning on modifying the model after calling this
  function, you may want to convert the returned list into a list of
  @class{gtk-tree-row-reference}'s. To do this, you can use the function
  @fun{gtk-tree-row-reference-new}.

  Since 2.6
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-row-reference-new}"
  (icon-view g-object))

(export 'gtk-icon-view-get-selected-items)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_select_all" gtk-icon-view-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{short}
    Selects all the icons. @arg{icon-view} must has its selection mode set to
    @code{:multiple}.
  @end{short}

  Since 2.6"
  (icon-view g-object))

(export 'gtk-icon-view-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unselect_all" gtk-icon-view-unselect-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @short{Unselects all the icons.}

  Since 2.6"
  (icon-view g-object))

(export 'gtk-icon-view-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_item_activated ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_item_activated" gtk-icon-view-item-activated) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-25}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the @class{gtk-tree-path} object to be activated}
  @begin{short}
    Activates the item determined by path.
  @end{short}

  Since 2.6
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-path}"
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-item-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_scroll_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_scroll_to_path" %gtk-icon-view-scroll-to-path) :void
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun gtk-icon-view-scroll-to-path (icon-view path &key
                                     (row-align 0.5 row-align-supplied-p)
                                     (col-align 0.5 col-align-supplied-p))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the path of the item to move to}
  @argument[row-align]{the vertical alignment of the item specified by path}
  @argument[col-align]{the horizontal alignment of the item specified by path}
  @begin{short}
    Moves the alignments of @arg{icon-view} to the position specified by
    @arg{path}.
  @end{short}
  @arg{row-align} determines where the row is placed, and @arg{col-align}
  determines where column is placed. Both are expected to be between 0.0 and
  1.0. 0.0 means left/top alignment, 1.0 means right/bottom alignment, 0.5 means
  center.

  If @arg{use-align} is @code{nil}, then the alignment arguments are ignored,
  and the tree does the minimum amount of work to scroll the item onto the
  screen. This means that the item will be scrolled to the edge closest to its
  current position. If the item is currently visible on the screen, nothing is
  done.

  This function only works if the model is set, and path is a valid row on the
  model. If the model changes before the @arg{icon-view} is realized, the
  centered @arg{path} will be modified to reflect this change.

  Since 2.8"
  (%gtk-icon-view-scroll-to-path icon-view
                                 path
                                 (or row-align-supplied-p col-align-supplied-p)
                                 row-align col-align))

(export 'gtk-icon-view-scroll-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_visible_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_visible_range" %gtk-icon-view-get-visible-range)
    :boolean
  (icon-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun gtk-icon-view-get-visible-range (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{return}
    @code{start-path} -- start of region, or @code{nil} @br{}
    @code{end-path} -- end of region, or  @code{nil} @br{}
    or @br{}
    @code{nil}, if valid paths were not placed in @arg{start-path} and
    @arg{end-path}
  @end{return}
  @begin{short}
    Sets @arg{start-path} and @arg{end-path} to be the first and last visible
    path.
  @end{short}
  Note that there may be invisible paths in between.

  Since 2.8"
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (%gtk-icon-view-get-visible-range icon-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref end-path '(g-boxed-foreign gtk-tree-path :return))))))

(export 'gtk-icon-view-get-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_tooltip_item" gtk-icon-view-set-tooltip-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path}}
  @begin{short}
    Sets the tip area of tooltip to be the area covered by the item at path.
  @end{short}
  See also the function @fun{gtk-icon-view-set-tooltip-column} for a simpler
  alternative. See also the function @fun{gtk-tooltip-set-tip-area}.

  Since 2.12
  @see-function{gtk-icon-view-set-tooltip-column}
  @see-function{gtk-tooltip-set-tip-area}"
  (icon-view (g-object gtk-icon-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-set-tooltip-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_tooltip_cell" gtk-icon-view-set-tooltip-cell) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path} structure}
  @argument[cell]{a @class{gtk-cell-renderer} or @code{nil}}
  @begin{short}
    Sets the tip area of tooltip to the area which cell occupies in the item
    pointed to by path.
  @end{short}
  See also the function @fun{gtk-tooltip-set-tip-area}.

  See also the function @fun{gtk-icon-view-set-tooltip-column} for a simpler
  alternative.

  Since 2.12
  @see-function{gtk-tooltip-set-tip-area}
  @see-function{gtk-icon-view-set-tooltip-column}"
  (icon-view (g-object gtk-icon-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-icon-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_tooltip_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_tooltip_context"
          %gtk-icon-view-get-tooltip-context) :boolean
  (icon-view (g-object gtk-icon-view))
  (x (:pointer :int))
  (y (:pointer :int))
  (keyboard-tip :boolean)
  (model (:pointer (g-object gtk-tree-model)))
  (path (:pointer (g-boxed-foreign gtk-tree-path)))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-icon-view-get-tooltip-context (icon-view x y keyboard-tip)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[x]{the x coordinate (relative to widget coordinates)}
  @argument[y]{the y coordinate (relative to widget coordinates)}
  @argument[keyboard-tip]{whether this is a keyboard tooltip or not}
  @begin{return}
    @code{model} -- a @class{gtk-tree-model} or @code{nil} @br{}
    @code{path} -- a @class{gtk-tree-path} structure or @code{nil} @br{}
    @code{iter} -- a @class{gtk-tree-iter} or @code{nil} @br{}
    or @br{}
    @code{nil} -- if the given tooltip context does not point to a item
  @end{return}
  @begin{short}
    This function is supposed to be used in a \"query-tooltip\" signal handler
    for @class{gtk-icon-view}. The @arg{x}, @arg{y} and @arg{keyboard-tip}
    values which are received in the signal handler, should be passed to this
    function without modification.
  @end{short}

  The return value indicates whether there is an icon view item at the given
  coordinates (@em{true}) or not (@code{nil}) for mouse tooltips. For keyboard
  tooltips the item returned will be the cursor item. When @em{true}, then any
  of model, path and iter which have been provided will be set to point to that
  row and the corresponding model. x and y will always be converted to be
  relative to @arg{icon-view}'s @code{bin_window} if @arg{keyboard-tooltip} is
  @code{nil}.

  Since 2.12"
  (with-foreign-objects ((xx :int)
                         (yy :int)
                         (model-ptr :pointer)
                         (path-ptr :pointer))
    (setf (mem-ref xx :int) x
          (mem-ref yy :int) y)
    (let ((iter (make-gtk-tree-iter)))
      (when (%gtk-icon-view-get-tooltip-context icon-view
                                                xx
                                                yy
                                                keyboard-tip
                                                model-ptr
                                                path-ptr
                                                iter)
        (values (mem-ref xx :int) (mem-ref yy :int)
                (convert-from-foreign (mem-ref model-ptr :pointer)
                                      '(g-object gtk-tree-model))
                (convert-from-foreign (mem-ref path-ptr :pointer)
                                      '(g-boxed-foreign gtk-tree-path :return))
                iter)))))

(export 'gtk-icon-view-get-tooltip-context)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-tooltip-column))

(defun gtk-icon-view-set-tooltip-column (icon-view column)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[column]{an integer, which is a valid column number for
    @arg{icon-view}'s model}
  @begin{short}
    If you only plan to have simple (text-only) tooltips on full items, you can
    use this function to have @class{gtk-icon-view} handle these automatically
    for you.
  @end{short}
  @arg{column} should be set to the column in @arg{icon-view}'s model containing
  the tooltip texts, or -1 to disable this feature.

  When enabled, @code{\"has-tooltip\"} will be set to @em{true} and
  @arg{icon-view} will connect a @code{\"query-tooltip\"} signal handler.

  Note that the signal handler sets the text with the function
  @fun{gtk-tooltip-set-markup}, so &, <, etc have to be escaped in the text.

  Since 2.12
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-tooltip-column}"
  (setf (gtk-icon-view-tooltip-column icon-view) column))

(export 'gtk-icon-view-set-tooltip-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_tooltip_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-tooltip-column))

(defun gtk-icon-view-get-tooltip-column (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{return}
    The index of the tooltip column that is currently being used, or -1 if
    this is disabled.
  @end{return}
  @begin{short}
    Returns the column of @arg{icon-view}'s model which is being used for
    displaying tooltips on @arg{icon-view}'s rows.
  @end{short}

  Since 2.12
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-tooltip-column}"
  (gtk-icon-view-tooltip-column icon-view))

(export 'gtk-icon-view-get-tooltip-column)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_item_row" gtk-icon-view-get-item-row) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the @class{gtk-tree-path} of the item}
  @return{The row in which the item is displayed.}
  @begin{short}
    Gets the row in which the item path is currently displayed.
  @end{short}
  Row numbers start at 0.

  Since 2.22
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-item-column}"
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-get-item-row)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_item_column" gtk-icon-view-get-item-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the @class{gtk-tree-path} of the item}
  @return{The column in which the item is displayed.}
  @begin{short}
    Gets the column in which the item path is currently displayed.
  @end{short}
  Column numbers start at 0.

  Since 2.22
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-get-item-row}"
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-get-item-column)

;;; ----------------------------------------------------------------------------
;;; enum GtkIconViewDropPosition
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIconViewDropPosition" gtk-icon-view-drop-position
  (:export t
   :type-initializer "gtk_icon_view_drop_position_get_type")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-icon-view-drop-position atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-icon-view-drop-position atdoc:*external-symbols*)
 "@version{2013-6-20}
  @short{An enum for determining where a dropped item goes.}
  @begin{pre}
(define-g-enum \"GtkIconViewDropPosition\" gtk-icon-view-drop-position
  (:export t
   :type-initializer \"gtk_icon_view_drop_position_get_type\")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))
  @end{pre}
  @begin[code]{table}
    @entry[:no-drop]{No drop possible.}
    @entry[:drop-into]{Dropped item replaces the item.}
    @entry[:drop-left]{Droppped item is inserted to the left.}
    @entry[:drop-right]{Dropped item is inserted to the right.}
    @entry[:drop-above]{Dropped item is inserted above.}
    @entry[:drop-below]{Dropped item is inserted below.}
  @end{table}
  @see-class{gtk-icon-view}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_enable_model_drag_source"
          %gtk-icon-view-enable-model-drag-source) :void
  (icon-view (g-object gtk-icon-view))
  (start-button-mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-icon-view-enable-model-drag-source (icon-view
                                               start-button-mask
                                               targets
                                               actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[start-button-mask]{mask of allowed buttons to start drag}
  @argument[targets]{the table of targets that the drag will support}
  @argument[actions]{the bitmask of possible actions for a drag from this
    widget}
  @begin{short}
    Turns @arg{icon-view} into a drag source for automatic DND.
  @end{short}
  Calling this method sets the property @code{\"reorderable\"} to @code{nil}.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-symbol{gdk-modifier-type}
  @see-symbol{gdk-drag-action}"
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-tree-view-enable-model-drag-source icon-view
                                             start-button-mask
                                             targets-ptr
                                             n-targets
                                             actions)))

(export 'gtk-icon-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_enable_model_drag_dest"
          %gtk-icon-view-enable-model-drag-dest) :void
  (icon-view (g-object gtk-icon-view))
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-icon-view-enable-model-drag-dest (icon-view targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[targets]{the table of targets that the drag will support}
  @argument[actions]{the bitmask of possible actions for a drag to this widget}
  @begin{short}
    Turns @arg{icon-view} into a drop destination for automatic DND.
  @end{short}
  Calling this method sets the property @code{\"reorderable\"} to @code{nil}.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-symbol{gdk-drag-action}"
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-icon-view-enable-model-drag-dest icon-view
                                           targets-ptr
                                           n-targets
                                           actions)))

(export 'gtk-icon-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unset_model_drag_source"
           gtk-icon-view-unset-model-drag-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{short}
    Undoes the effect of the function
    @fun{gtk-icon-view-enable-model-drag-source}.
  @end{short}
  Calling this method sets @code{\"reorderable\"} to @code{nil}.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-enable-model-drag-source}"
  (icon-view (g-object gtk-icon-view)))

(export 'gtk-icon-view-unset-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unset_model_drag_dest"
           gtk-icon-view-unset-model-drag-dest) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{short}
    Undoes the effect of the function
    @fun{gtk-icon-view-enable-model-drag-dest}.
  @end{short}
  Calling this method sets the property @code{\"reorderable\"} to @code{nil}.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-enable-model-drag-dest}"
  (icon-view (g-object gtk-icon-view)))

(export 'gtk-icon-view-unset-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-set-reorderable))

(defun gtk-icon-view-set-reorderable (icon-view reorderable)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[reorderable]{@em{true}, if the list of items can be reordered}
  @begin{short}
    This function is a convenience function to allow you to reorder models that
    support the @code{GtkTreeDragSourceIface} and the
    @code{GtkTreeDragDestIface}.
  @end{short}
  Both @class{gtk-tree-store} and @class{gtk-list-store} support these. If
  @arg{reorderable} is @em{true}, then the user can reorder the model by
  dragging and dropping rows. The developer can listen to these changes by
  connecting to the model's \"row-inserted\" and \"row-deleted\" signals. The
  reordering is implemented by setting up the icon view as a drag source and
  destination. Therefore, drag and drop can not be used in a reorderable view
  for any other purpose.

  This function does not give you any degree of control over the order - any
  reordering is allowed. If more control is needed, you should probably handle
  drag and drop manually.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-store}
  @see-class{gtk-list-store}"
  (setf (gtk-icon-view-reorderable icon-view) reorderable))

(export 'gtk-icon-view-set-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-icon-view-get-reorderable))

(defun gtk-icon-view-get-reorderable (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @return{@em{True} if the list can be reordered.}
  @begin{short}
    Retrieves whether the user can reorder the list via drag-and-drop.
  @end{short}
  See the function @fun{gtk-icon-view-set-reorderable}.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-function{gtk-icon-view-set-reorderable}"
  (gtk-icon-view-reorderable icon-view))

(export 'gtk-icon-view-get-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_drag_dest_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_drag_dest_item" gtk-icon-view-set-drag-dest-item)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{the path of the item to highlight, or @code{nil}}
  @argument[pos]{specifies where to drop, relative to the item}
  @begin{short}
    Sets the item that is highlighted for feedback.
  @end{short}

  Since 2.8
  @see-class{gtk-icon-view}
  @see-symbol{gtk-icon-view-drop-position}
  @see-function{gtk-icon-view-get-drag-dest-item}"
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path))
  (pos gtk-icon-view-drop-position))

(export 'gtk-icon-view-set-drag-dest-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_drag_dest_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_drag_dest_item" %gtk-icon-view-get-drag-dest-item)
    :void
  (icon-view (g-object gtk-icon-view))
  (path :pointer)
  (pos :pointer))

(defun gtk-icon-view-get-drag-dest-item (icon-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @begin{return}
    @arg{path} -- the path of the highlighted item, or @code{nil} @br{}
    @arg{pos} -- the drop position, or @code{nil}
  @end{return}
  @begin{short}
    Gets information about the item that is highlighted for feedback.
  @end{short}

  Since 2.8
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-icon-view-drop-position}
  @see-function{gtk-icon-view-set-drag-dest-item}"
  (with-foreign-objects ((path :pointer) (pos :pointer))
    (%gtk-icon-view-get-drag-dest-item icon-view path pos)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref pos 'gtk-icon-view-drop-position))))

(export 'gtk-icon-view-get-drag-dest-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_dest_item_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_dest_item_at_pos"
          %gtk-icon-view-get-dest-item-at-pos) :boolean
  (icon-view (g-object gtk-icon-view))
  (drag-x :int)
  (drag-y :int)
  (path :pointer)
  (pos :pointer))

(defun gtk-icon-view-get-dest-item-at-pos (icon-view drag-x drag-y)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[drag-x]{the position to determine the destination item for}
  @argument[drag-y]{the position to determine the destination item for}
  @begin{return}
    @arg{path} -- the path of the item, or @code{nil} @br{}
    @arg{pos} -- the drop position, or @code{nil}
  @end{return}
  @begin{short}
    Determines the destination item for a given position.
  @end{short}

  Since 2.8
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-icon-view-drop-position}"
  (with-foreign-objects ((path :pointer) (pos :pointer))
    (when (%gtk-icon-view-get-dest-item-at-pos icon-view drag-x drag-y path pos)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref pos 'gtk-icon-view-drop-position)))))

(export 'gtk-icon-view-get-dest-item-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_create_drag_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_create_drag_icon" gtk-icon-view-create-drag-icon)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-27}
  @argument[icon-view]{a @class{gtk-icon-view} widget}
  @argument[path]{a @class{gtk-tree-path} in @arg{icon-view}}
  @return{A newly-allocated surface of the drag icon.}
  @begin{short}
    Creates a @symbol{cairo-surface-t} representation of the item at @arg{path}.
  @end{short}
  This image is used for a drag icon.

  Since 2.8
  @see-class{gtk-icon-view}
  @see-class{gtk-tree-path}
  @see-symbol{cairo-surface-t}"
  (icon-view (g-object gtk-icon-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-create-drag-icon)

;;; --- End of file gtk.icon-view.lisp -----------------------------------------
