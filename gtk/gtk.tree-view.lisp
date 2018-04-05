;;; ----------------------------------------------------------------------------
;;; gtk.tree-view.lisp
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
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; GtkTreeView
;;;
;;; A widget for displaying both trees and lists
;;;
;;; Synopsis
;;;
;;;     GtkTreeView
;;;     GtkTreeViewDropPosition
;;;     GtkTreeViewPrivate
;;;
;;;     gtk_tree_view_new
;;;     gtk_tree_view_get_level_indentation
;;;     gtk_tree_view_get_show_expanders
;;;     gtk_tree_view_set_level_indentation
;;;     gtk_tree_view_set_show_expanders
;;;     gtk_tree_view_new_with_model
;;;     gtk_tree_view_get_model
;;;     gtk_tree_view_set_model
;;;     gtk_tree_view_get_selection
;;;     gtk_tree_view_get_hadjustment                      * deprecated *
;;;     gtk_tree_view_set_hadjustment                      * deprecated *
;;;     gtk_tree_view_get_vadjustment                      * deprecated *
;;;     gtk_tree_view_set_vadjustment                      * deprecated *
;;;     gtk_tree_view_get_headers_visible
;;;     gtk_tree_view_set_headers_visible
;;;     gtk_tree_view_columns_autosize
;;;     gtk_tree_view_get_headers_clickable
;;;     gtk_tree_view_set_headers_clickable
;;;     gtk_tree_view_set_rules_hint
;;;     gtk_tree_view_get_rules_hint
;;;     gtk_tree_view_append_column
;;;     gtk_tree_view_remove_column
;;;     gtk_tree_view_insert_column
;;;     gtk_tree_view_insert_column_with_attributes
;;;     gtk_tree_view_insert_column_with_data_func
;;;     gtk_tree_view_get_n_columns
;;;     gtk_tree_view_get_column
;;;     gtk_tree_view_get_columns
;;;     gtk_tree_view_move_column_after
;;;     gtk_tree_view_set_expander_column
;;;     gtk_tree_view_get_expander_column
;;;     gtk_tree_view_set_column_drag_function
;;;     gtk_tree_view_scroll_to_point
;;;     gtk_tree_view_scroll_to_cell
;;;     gtk_tree_view_set_cursor
;;;     gtk_tree_view_set_cursor_on_cell
;;;     gtk_tree_view_get_cursor
;;;     gtk_tree_view_row_activated
;;;     gtk_tree_view_expand_all
;;;     gtk_tree_view_collapse_all
;;;     gtk_tree_view_expand_to_path
;;;     gtk_tree_view_expand_row
;;;     gtk_tree_view_collapse_row
;;;     gtk_tree_view_map_expanded_rows
;;;     gtk_tree_view_row_expanded
;;;     gtk_tree_view_set_reorderable
;;;     gtk_tree_view_get_reorderable
;;;     gtk_tree_view_get_path_at_pos
;;;     gtk_tree_view_is_blank_at_pos
;;;     gtk_tree_view_get_cell_area
;;;     gtk_tree_view_get_background_area
;;;     gtk_tree_view_get_visible_rect
;;;     gtk_tree_view_get_visible_range
;;;     gtk_tree_view_get_bin_window
;;;     gtk_tree_view_convert_bin_window_to_tree_coords
;;;     gtk_tree_view_convert_bin_window_to_widget_coords
;;;     gtk_tree_view_convert_tree_to_bin_window_coords
;;;     gtk_tree_view_convert_tree_to_widget_coords
;;;     gtk_tree_view_convert_widget_to_bin_window_coords
;;;     gtk_tree_view_convert_widget_to_tree_coords
;;;     gtk_tree_view_enable_model_drag_dest
;;;     gtk_tree_view_enable_model_drag_source
;;;     gtk_tree_view_unset_rows_drag_source
;;;     gtk_tree_view_unset_rows_drag_dest
;;;     gtk_tree_view_set_drag_dest_row
;;;     gtk_tree_view_get_drag_dest_row
;;;     gtk_tree_view_get_dest_row_at_pos
;;;     gtk_tree_view_create_row_drag_icon
;;;     gtk_tree_view_set_enable_search
;;;     gtk_tree_view_get_enable_search
;;;     gtk_tree_view_get_search_column
;;;     gtk_tree_view_set_search_column
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;     gtk_tree_view_get_search_position_func
;;;     gtk_tree_view_set_search_position_func
;;;     gtk_tree_view_get_fixed_height_mode
;;;     gtk_tree_view_set_fixed_height_mode
;;;     gtk_tree_view_get_hover_selection
;;;     gtk_tree_view_set_hover_selection
;;;     gtk_tree_view_get_hover_expand
;;;     gtk_tree_view_set_hover_expand
;;;     gtk_tree_view_set_destroy_count_func
;;;     gtk_tree_view_get_row_separator_func
;;;     gtk_tree_view_set_row_separator_func
;;;     gtk_tree_view_get_rubber_banding
;;;     gtk_tree_view_set_rubber_banding
;;;     gtk_tree_view_is_rubber_banding_active
;;;     gtk_tree_view_get_enable_tree_lines
;;;     gtk_tree_view_set_enable_tree_lines
;;;
;;;     GtkTreeViewGridLines
;;;
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context
;;;     gtk_tree_view_get_tooltip_column
;;;     gtk_tree_view_set_tooltip_column
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeView
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeView" gtk-tree-view
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_tree_view_get_type")
  ((enable-grid-lines
    gtk-tree-view-enable-grid-lines
    "enable-grid-lines" "GtkTreeViewGridLines" t t)
   (enable-search
    gtk-tree-view-enable-search
    "enable-search" "gboolean" t t)
   (enable-tree-lines
    gtk-tree-view-enable-tree-lines
    "enable-tree-lines" "gboolean" t t)
   (expander-column
    gtk-tree-view-expander-column
    "expander-column" "GtkTreeViewColumn" t t)
   (fixed-height-mode
    gtk-tree-view-fixed-height-mode
    "fixed-height-mode" "gboolean" t t)
   (headers-clickable
    gtk-tree-view-headers-clickable
    "headers-clickable" "gboolean" t t)
   (headers-visible
    gtk-tree-view-headers-visible
    "headers-visible" "gboolean" t t)
   (hover-expand
    gtk-tree-view-hover-expand
    "hover-expand" "gboolean" t t)
   (hover-selection
    gtk-tree-view-hover-selection
    "hover-selection" "gboolean" t t)
   (level-indentation
    gtk-tree-view-level-indentation
    "level-indentation" "gint" t t)
   (model
    gtk-tree-view-model
    "model" "GtkTreeModel" t t)
   (reorderable
    gtk-tree-view-reorderable
    "reorderable" "gboolean" t t)
   (rubber-banding
    gtk-tree-view-rubber-banding
    "rubber-banding" "gboolean" t t)
   (rules-hint
    gtk-tree-view-rules-hint
    "rules-hint" "gboolean" t t)
   (search-column
    gtk-tree-view-search-column
    "search-column" "gint" t t)
   (show-expanders
    gtk-tree-view-show-expanders
    "show-expanders" "gboolean" t t)
   (tooltip-column
    gtk-tree-view-tooltip-column
    "tooltip-column" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-view 'type)
 "@version{2013-12-29}
  @begin{short}
    Widget that displays any object that implements the @class{gtk-tree-model}
    interface.
  @end{short}

  Please refer to the tree widget conceptual overview for an overview of all
  the objects and data types related to the tree widget and how they work
  together.

  Several different coordinate systems are exposed in the @sym{gtk-tree-view}
  API.
  These are:
  @begin{table}
    @entry[Coordinate systems in @sym{gtk-tree-view} API]{}
    @entry[Widget coordinates]{Coordinates relative to the widget (usually
      @code{widget->window}).}
    @entry[Bin window coordinates]{Coordinates relative to the window that
      @sym{gtk-tree-view} renders to.}
    @entry[Tree coordinates]{Coordinates relative to the entire scrollable area
      of @sym{gtk-tree-view}. These coordinates start at (0, 0) for row 0 of the
      tree.}
  @end{table}
  Several functions are available for converting between the different
  coordinate systems. The most common translations are between widget and bin
  window coordinates and between bin window and tree coordinates. For the
  former you can use the @fun{gtk-tree-view-convert-widget-to-bin-window-coords}
  function (and vice versa), for the latter the
  @fun{gtk-tree-view-convert-bin-window-to-tree-coords} function (and vice
  versa).

  @subheading{GtkTreeView as GtkBuildable}
    The @sym{gtk-tree-view} implementation of the @class{gtk-buildable}
    interface accepts @class{gtk-tree-view-column} objects as @code{<child>}
    elements and exposes the internal @class{gtk-tree-selection} in UI
    definitions.

    @b{Example:} A UI definition fragment with @class{gtk-tree-view}
    @begin{pre}
   <object class=\"GtkTreeView\" id=\"treeview\">
     <property name=\"model\">liststore1</property>
     <child>
       <object class=\"GtkTreeViewColumn\" id=\"test-column\">
         <property name=\"title\">Test</property>
         <child>
           <object class=\"GtkCellRendererText\" id=\"test-renderer\"/>
           <attributes>
             <attribute name=\"text\">1</attribute>
           </attributes>
         </child>
       </object>
     </child>
     <child internal-child=\"selection\">
       <object class=\"GtkTreeSelection\" id=\"selection\">
         <signal name=\"changed\" handler=\"on_treeview_selection_changed\"/>
       </object>
     </child>
   </object>
    @end{pre}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"allow-rules\" style property}
      @code{\"allow-rules\"} of type @code{:boolean} (Read) @br{}
      Allow drawing of alternating color rows. @br{}
      Default value: @em{true}

    @subheading{The \"even-row-color\" style property}
      @code{\"even-row-color\"} of type @class{gdk-color} (Read) @br{}
      Color to use for even rows.

    @subheading{The \"expander-size\" style property}
      @code{\"expander-size\"} of type @code{:int} (Read) @br{}
      Size of the expander arrow. @br{}
      Allowed values: >= 0 @br{}
      Default value: 14

    @subheading{The \"grid-line-pattern\" style property}
      @code{\"grid-line-pattern\"} of type @code{:string} (Read) @br{}
      Dash pattern used to draw the tree view grid lines. @br{}
      Default value: \"\001\001\"

    @subheading{The \"grid-line-width\" style property}
      @code{\"grid-line-width\"} of type @code{:int} (Read) @br{}
      Width, in pixels, of the tree view grid lines. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1

    @subheading{The \"horizontal-separator\" style property}
      @code{\"horizontal-separator\"} of tpye @code{:int} (Read) @br{}
      Horizontal space between cells. Must be an even number. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2

    @subheading{The \"indent-expanders\" style property}
      @code{\"indent-expanders\"} of type @code{:boolean} (Read) @br{}
      Make the expanders indented. @br{}
      Default value: @em{true}

    @subheading{The \"odd-row-color\" style property}
      @code{\"odd-row-color\"} of type @class{gdk-color} (Read)@br{}
      Color to use for odd rows.

    @subheading{The \"tree-line-pattern\" style property}
      @code{\"tree-line-pattern\"} of type @code{:string} (Read) @br{}
      Dash pattern used to draw the tree view lines. @br{}
      Default value: \"\001\001\"

    @subheading{The \"tree-line-width\" style property}
      @code{\"tree-line-width\"} of type @code{:int} (Read) @br{}
      Width, in pixels, of the tree view lines. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1

    @subheading{The \"vertical-separator\" style property}
      @code{\"vertical-separator\"} of type @code{:int} (Read) @br{}
      Vertical space between cells. Must be an even number. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"columns-changed\" signal}
      @begin{pre}
 lambda (tree-view)   : Run Last
      @end{pre}
      The number of columns of the treeview has changed.
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"cursor-changed\" signal}
      @begin{pre}
 lambda (tree-view)   : Run Last
      @end{pre}
      The position of the cursor (focused cell) has changed.
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
      @end{table}
    @subheading{The \"expand-collapse-cursor-row\" signal}
      @begin{pre}
 lambda (treeview arg1 arg2 arg3)   : Action
      @end{pre}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (treeview arg1 arg2)   : Action
      @end{pre}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
 lambda (tree-view path column)
      @end{pre}
      The \"row-activated\" signal is emitted when the method
      @fun{gtk-tree-view-row-activated} is called or the user double clicks a
      treeview row. It is also emitted when a non-editable row is selected and
      one of the keys: Space, Shift+Space, Return or Enter is pressed.
      For selection handling refer to the tree widget conceptual overview as
      well as @class{gtk-tree-selection}.
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
        @entry[path]{The @class{gtk-tree-path} for the activated row.}
        @entry[column]{The @class{gtk-tree-view-column} in which the activation
          occurred}
      @end{table}
    @subheading{The \"row-collapsed\" signal}
      @begin{pre}
 lambda (tree-view iter path)   : Run Last
      @end{pre}
      The given row has been collapsed (child nodes are hidden).
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
        @entry[iter]{The tree iter of the collapsed row.}
        @entry[path]{A tree path that points to the row.}
      @end{table}
    @subheading{The \"row-expanded\" signal}
      @begin{pre}
 lambda (tree-view iter path)   : Run Last
      @end{pre}
      The given row has been expanded (child nodes are shown).
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
        @entry[iter]{The tree iter of the expanded row.}
        @entry[path]{A tree path that points to the row.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (treeview)   : Action
      @end{pre}
    @subheading{The \"select-cursor-parent\" signal}
      @begin{pre}
 lambda (treeview)   : Action
      @end{pre}
    @subheading{The \"select-cursor-row\" signal}
      @begin{pre}
 lambda (treeview arg1)   : Action
      @end{pre}
    @subheading{The \"start-interactive-search\" signal}
      @begin{pre}
 lambda (treeview)   : Action
      @end{pre}
    @subheading{The \"test-collapse-row\" signal}
      @begin{pre}
 lambda (tree-view iter path)   : Run Last
      @end{pre}
      The given row is about to be collapsed (hide its children nodes). Use this
      signal if you need to control the collapsibility of individual rows.
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
        @entry[iter]{The tree iter of the row to collapsed.}
        @entry[path]{A tree path that points to the row.}
        @entry[Returns]{@code{Nil} to allow collapsing, @em{true} to reject.}
      @end{table}
    @subheading{The \"test-expand-row\" signal}
      @begin{pre}
 lambda (tree-view iter path)   : Run Last
      @end{pre}
      The given row is about to be expanded (show its children nodes). Use this
      signal if you need to control the expandability of individual rows.
      @begin[code]{table}
        @entry[tree-view]{The object on which the signal is emitted.}
        @entry[iter]{The tree iter of the row to expand.}
        @entry[path]{A tree path that points to the row.}
        @entry[Returns]{@code{Nil} to allow expansion, @em{true} to reject.}
      @end{table}
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
 lambda (treeview)   : Action
      @end{pre}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (treeview)   : Action
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-tree-view-enable-grid-lines}
  @see-slot{gtk-tree-view-enable-search}
  @see-slot{gtk-tree-view-enable-tree-lines}
  @see-slot{gtk-tree-view-expander-column}
  @see-slot{gtk-tree-view-fixed-height-mode}
  @see-slot{gtk-tree-view-headers-clickable}
  @see-slot{gtk-tree-view-headers-visible}
  @see-slot{gtk-tree-view-hover-expand}
  @see-slot{gtk-tree-view-hover-selection}
  @see-slot{gtk-tree-view-level-indentation}
  @see-slot{gtk-tree-view-model}
  @see-slot{gtk-tree-view-reorderable}
  @see-slot{gtk-tree-view-rubber-banding}
  @see-slot{gtk-tree-view-rules-hint}
  @see-slot{gtk-tree-view-search-column}
  @see-slot{gtk-tree-view-show-expanders}
  @see-slot{gtk-tree-view-tooltip-column}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-grid-lines"
                                               'gtk-tree-view) 't)
 "The @code{\"enable-grid-lines\"} property of type
  @symbol{gtk-tree-view-grid-lines} (Read / Write) @br{}
  Whether grid lines should be drawn in the tree view. @br{}
  Default value: @code{:none}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-search"
                                               'gtk-tree-view) 't)
 "The @code{\"enable-search\"} property of type @code{:boolean}
  (Read / Write) @br{}
  View allows user to search through columns interactively. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-tree-lines"
                                               'gtk-tree-view) 't)
 "The @code{\"enable-tree-lines\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether tree lines should be drawn in the tree view. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expander-column"
                                               'gtk-tree-view) 't)
 "The @code{\"expander-column\"} property of type @class{gtk-tree-view-column}
  (Read / Write) @br{}
  Set the column for the expander column.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fixed-height-mode"
                                               'gtk-tree-view) 't)
 "The @code{\"fixed-height-mode\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Setting the @code{\"fixed-height-mode\"} property to @em{true} speeds up
  @sym{gtk-tree-view} by assuming that all rows have the same height. Only
  enable this option if all rows are the same height. Please see the function
  @fun{gtk-tree-view-set-fixed-height-mode} for more information on this
  option. @br{}
  Default value: @code{nil} @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "headers-clickable"
                                               'gtk-tree-view) 't)
 "The @code{\"headers-clickable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Column headers respond to click events. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "headers-visible"
                                               'gtk-tree-view) 't)
 "The @code{\"headers-visible\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Show the column header buttons. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hover-expand"
                                               'gtk-tree-view) 't)
 "The @code{\"hover-expand\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Enables or disables the hover expansion mode of @arg{tree-view}. Hover
  expansion makes rows expand or collapse if the pointer moves over them.
  This mode is primarily intended for treeviews in popups, e. g. in
  @class{gtk-combo-box} or @class{gtk-entry-completion}. @br{}
  Default value: @code{nil} @br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hover-selection"
                                               'gtk-tree-view) 't)
 "The @code{\"hover-selection\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Enables or disables the hover selection mode of @arg{tree-view}. Hover
  selection makes the selected row follow the pointer. Currently, this works
  only for the selection modes @code{:single} and @code{:browse}. This mode is
  primarily intended for treeviews in popups, e. g. in @class{gtk-combo-box}
  or @class{gtk-entry-completion}. @br{}
  Default value: @code{nil} @br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "level-indentation"
                                               'gtk-tree-view) 't)
 "The @code{\"level-indentation\"} property of type @code{:int}
  (Read / Write) @br{}
  Extra indentation for each level. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-tree-view) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write) @br{}
  The model for the tree view.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reorderable"
                                               'gtk-tree-view) 't)
 "The @code{\"reorderable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  View is reorderable. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rubber-banding"
                                               'gtk-tree-view) 't)
 "The @code{\"rubber-banding\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to enable selection of multiple items by dragging the mouse
  pointer. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rules-hint"
                                               'gtk-tree-view) 't)
 "The @code{\"rules-hint\"} property of type @code{:boolean} (Read / Write)@br{}
  Set a hint to the theme engine to draw rows in alternating colors. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "search-column"
                                               'gtk-tree-view) 't)
 "The @code{\"search-column\"} property of type @code{:int} (Read / Write) @br{}
  Model column to search through during interactive search. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-expanders"
                                               'gtk-tree-view) 't)
 "The @code{\"show-expanders\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the view has expanders. @br{}
  Default value: @em{true} @br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-column"
                                               'gtk-tree-view) 't)
 "The @code{\"tooltip-column\"} property of type @code{:int} (Read / Write)@br{}
  The column in the model containing the tooltip texts for the rows. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-grid-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-grid-lines 'function)
 "@version{2013-3-27}
  Accessor of the slot \"enable-grid-lines\" of the @class{gtk-tree-view}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-search atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-search 'function)
 "@version{2013-3-27}
  Accessor of the slot \"enable-search\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-tree-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-tree-lines 'function)
 "@version{2013-3-27}
  Accessor of the slot \"enable-tree-lines\" of the @class{gtk-tree-view}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-expander-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-expander-column 'function)
 "@version{2013-3-27}
  Accessor of the slot \"expander-column\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-fixed-height-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-fixed-height-mode 'function)
 "@version{2013-3-27}
  Accessor of the slot \"fixed-height-mode\" of the @class{gtk-tree-view}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-headers-clickable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-headers-clickable 'function)
 "@version{2013-3-27}
  Accessor of the slot \"headers-clickable\" of the @class{gtk-tree-view}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-headers-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-headers-visible 'function)
 "@version{2013-3-27}
  Accessor of the slot \"headers-visible\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-hover-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-hover-expand 'function)
 "@version{2013-3-27}
  Accessor of the slot \"hover-expand\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-hover-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-hover-selection 'function)
 "@version{2013-3-27}
  Accessor of the slot \"hover-selection\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-level-indentation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-level-indentation 'function)
 "@version{2013-3-27}
  Accessor of the slot \"level-indentation\" of the @class{gtk-tree-view}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-model 'function)
 "@version{2013-3-27}
  Accessor of the slot \"model\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-reorderable 'function)
 "@version{2013-3-27}
  Accessor of the slot \"reorderable\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-rubber-banding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-rubber-banding 'function)
 "@version{2013-3-27}
  Accessor of the slot \"rubber-banding\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-rules-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-rules-hint 'function)
 "@version{2013-3-27}
  Accessor of the slot \"rules-hint\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-search-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-search-column 'function)
 "@version{2013-3-27}
  Accessor of the slot \"search-column\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-show-expanders atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-show-expanders 'function)
 "@version{2013-3-27}
  Accessor of the slot \"show-expanders\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-tooltip-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-tooltip-column 'function)
 "@version{2013-3-27}
  Accessor of the slot \"tooltip-column\" of the @class{gtk-tree-view} class.")

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewDropPosition
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewDropPosition" gtk-tree-view-drop-position
  (:export t
   :type-initializer "gtk_tree_view_drop_position_get_type")
  (:before 0)
  (:after 1)
  (:into-or-before 2)
  (:into-or-after 3))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-drop-position atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-tree-view-drop-position atdoc:*external-symbols*)
 "@version{2013-5-17}
  @begin{short}
    An enum for determining where a dropped row goes.
  @end{short}
  @begin[code]{table}
    @entry[:before]{Dropped row is inserted before.}
    @entry[:after]{Dropped row is inserted after.}
    @entry[:into-or-before]{Dropped row becomes a child or is inserted before.}
    @entry[:into-or-after]{Dropped row becomes a child or is inserted after.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewPrivate
;;;
;;; typedef struct _GtkTreeViewPrivate GtkTreeViewPrivate;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchEqualFunc ()
;;;
;;; gboolean (*GtkTreeViewSearchEqualFunc) (GtkTreeModel *model,
;;;                                         gint column,
;;;                                         const gchar *key,
;;;                                         GtkTreeIter *iter,
;;;                                         gpointer search_data);
;;;
;;; A function used for checking whether a row in model matches a search key
;;; string entered by the user. Note the return value is reversed from what you
;;; would normally expect, though it has some similarity to strcmp() returning 0
;;; for equal strings.
;;;
;;; model :
;;;     the GtkTreeModel being searched
;;;
;;; column :
;;;     the search column set by gtk_tree_view_set_search_column()
;;;
;;; key :
;;;     the key string to compare with
;;;
;;; iter :
;;;     a GtkTreeIter pointing the row of model that should be compared with
;;;     key.
;;;
;;; search_data :
;;;     user data from gtk_tree_view_set_search_equal_func()
;;;
;;; Returns :
;;;     FALSE if the row matches, TRUE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new))

(defun gtk-tree-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @return{A newly created @class{gtk-tree-view} widget.}
  Creates a new @class{gtk-tree-view} widget."
  (make-instance 'gtk-tree-view))

(export 'gtk-tree-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_level_indentation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-level-indentation))

(defun gtk-tree-view-get-level-indentation (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @begin{return}
    the amount of extra indentation for child levels in @arg{tree-view}. A
    return value of 0 means that this feature is disabled.
  @end{return}
  @begin{short}
    Returns the amount, in pixels, of extra indentation for child levels in
    @arg{tree-view}.
  @end{short}

  Since 2.12"
  (gtk-tree-view-level-indentation tree-view))

(export 'gtk-tree-view-get-level-indentation)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_show_expanders ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-show-expanders))

(defun gtk-tree-view-get-show-expanders (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@em{True} if expanders are drawn in @arg{tree-view}, @code{nil}
    otherwise.}
  @begin{short}
    Returns whether or not expanders are drawn in @arg{tree-view}.
  @end{short}

  Since 2.12"
  (gtk-tree-view-show-expanders tree-view))

(export 'gtk-tree-view-get-show-expanders)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_level_indentation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-level-indentation indentation))

(defun gtk-tree-view-set-level-indentation (tree-view indentation)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[indentation]{the amount, in pixels, of extra indentation in
    @arg{tree-view}}
  @begin{short}
    Sets the amount of extra indentation for child levels to use in
    @arg{tree-view} in addition to the default indentation. The value should be
    specified in pixels, a value of 0 disables this feature and in this case
    only the default indentation will be used. This does not have any visible
    effects for lists.
  @end{short}

  Since 2.12"
  (setf (gtk-tree-view-level-indentation tree-view) indentation))

(export 'gtk-tree-view-set-level-indentation)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_show_expanders ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-show-expanders))

(defun gtk-tree-view-set-show-expanders (tree-view enabled)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} object}
  @argument[enabled]{@em{True} to enable expander drawing, @code{nil}
    otherwise}
  @begin{short}
    Sets whether to draw and enable expanders and indent child rows in
    @arg{tree-view}. When disabled there will be no expanders visible in trees
    and there will be no way to expand and collapse rows by default. Also note
    that hiding the expanders will disable the default indentation. You can set
    a custom indentation in this case using the function
    @fun{gtk-tree-view-set-level-indentation}. This does not have any visible
    effects for lists.
  @end{short}

  Since 2.12"
  (setf (gtk-tree-view-show-expanders tree-view) enabled))

(export 'gtk-tree-view-set-show-expanders)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new-with-model))

(defun gtk-tree-view-new-with-model (model)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[model]{the model}
  @return{A newly created @class{gtk-tree-view} widget.}
  Creates a new @class{gtk-tree-view} widget with the model initialized to
  model.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}"
  (make-instance 'gtk-tree-view
                 :model model))

(export 'gtk-tree-view-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-model))

(defun gtk-tree-view-get-model (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{A @class{gtk-tree-model} object, or @code{nil} if none is currently
    being used.}
  Returns the model the @class{gtk-tree-view} widget is based on. Returns
  @code{nil} if the model is unset.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-set-model}"
  (gtk-tree-view-model tree-view))

(export 'gtk-tree-view-get-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-model))

(defun gtk-tree-view-set-model (tree-view model)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-29}
  @argument[tree-view]{a @class{gtk-tree-view} object}
  @argument[model]{the model}
  Sets the model for a @class{gtk-tree-view}. If the @arg{tree-view} already has
  a model set, it will remove it before setting the new @arg{model}. If
  @arg{model} is @code{nil}, then it will unset the old model.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-get-model}"
  (setf (gtk-tree-view-model tree-view) model))

(export 'gtk-tree-view-set-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_selection ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_selection" gtk-tree-view-get-selection)
    (g-object gtk-tree-selection)
 #+cl-cffi-gtk-documentation
 "@version{2014-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{A @class{gtk-tree-selection} object.}
  Gets the @class{gtk-tree-selection} object associated with @arg{tree-view}."
  (tree-view (g-object gtk-tree-view)))

(export 'gtk-tree-view-get-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_tree_view_get_hadjustment (GtkTreeView *tree_view);
;;;
;;; Warning
;;;
;;; gtk_tree_view_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Gets the GtkAdjustment currently being used for the horizontal aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A GtkAdjustment object, or NULL if none is currently being used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hadjustment ()
;;;
;;; void gtk_tree_view_set_hadjustment (GtkTreeView *tree_view,
;;;                                     GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_hadjustment()
;;;
;;; Sets the GtkAdjustment for the current horizontal aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; adjustment :
;;;     The GtkAdjustment to set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_tree_view_get_vadjustment (GtkTreeView *tree_view);
;;;
;;; Warning
;;;
;;; gtk_tree_view_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Gets the GtkAdjustment currently being used for the vertical aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A GtkAdjustment object, or NULL if none is currently being used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_vadjustment ()
;;;
;;; void gtk_tree_view_set_vadjustment (GtkTreeView *tree_view,
;;;                                     GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_vadjustment()
;;;
;;; Sets the GtkAdjustment for the current vertical aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; adjustment :
;;;     The GtkAdjustment to set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_headers_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-headers-visible))

(defun gtk-tree-view-get-headers-visible (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{Whether the headers are visible or not.}
  Returns @em{true} if the headers on the @arg{tree-view} are visible."
  (gtk-tree-view-headers-visible tree-view))

(export 'gtk-tree-view-get-headers-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_headers_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-headers-visible))

(defun gtk-tree-view-set-headers-visible (tree-view headers-visible)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[headers-visible]{@em{true} if the headers are visible}
  Sets the visibility state of the headers."
  (setf (gtk-tree-view-headers-visible tree-view) headers-visible))

(export 'gtk-tree-view-set-headers-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_columns_autosize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_columns_autosize" gtk-tree-view-columns-autosize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  Resizes all columns to their optimal width. Only works after the treeview
  has been realized."
  (tree-view g-object))

(export 'gtk-tree-view-columns-autosize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_headers_clickable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-headers-clickable))

(defun gtk-tree-view-get-headers-clickable (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@em{True} if all header columns are clickable, otherwise @code{nil}.}
  @short{Returns whether all header columns are clickable.}

  Since 2.10"
  (gtk-tree-view-headers-clickable tree-view))

(export 'gtk-tree-view-get-headers-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_headers_clickable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-headers-clickable))

(defun gtk-tree-view-set-headers-clickable (tree-view headers-clickable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[setting]{@em{true} if the columns are clickable}
  Allow the column title buttons to be clicked."
  (setf (gtk-tree-view-headers-clickable tree-view) headers-clickable))

(export 'gtk-tree-view-set-headers-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_rules_hint ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-rules-hint))

(defun gtk-tree-view-set-rules-hint (tree-view setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[setting]{@em{true} if the tree requires reading across rows}
  @begin{short}
    This function tells GTK+ that the user interface for your application
    requires users to read across tree rows and associate cells with one
    another.
  @end{short}
  By default, GTK+ will then render the tree with alternating row
  colors. Do not use it just because you prefer the appearance of the ruled
  tree; that is a question for the theme. Some themes will draw tree rows in
  alternating colors even when rules are turned off, and users who prefer that
  appearance all the time can choose those themes. You should call this
  function only as a semantic hint to the theme engine that your tree makes
  alternating colors useful from a functional standpoint (since it has lots of
  columns, generally)."
  (setf (gtk-tree-view-rules-hint tree-view) setting))

(export 'gtk-tree-view-set-rules-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_rules_hint ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-rules-hint))

(defun gtk-tree-view-get-rules-hint (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@em{True} if rules are useful for the user of this tree.}
  Gets the setting set by the @fun{gtk-tree-view-set-rules-hint} function."
  (gtk-tree-view-rules-hint tree-view))

(export 'gtk-tree-view-get-rules-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_append_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_append_column" gtk-tree-view-append-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} to add}
  @return{The number of columns in @arg{tree-view} after appending.}
  Appends column to the list of columns. If @arg{tree-view} has
  @code{\"fixed_height\"} mode enabled, then column must have its
  @code{\"sizing\"} property set to be @code{:fixed}."
  (tree-view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-append-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_remove_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_remove_column" gtk-tree-view-remove-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} to remove}
  @return{The number of columns in @arg{tree-view} after removing.}
  Removes column from @arg{tree-view}."
  (tree-view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_insert_column" gtk-tree-view-insert-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} to be inserted}
  @argument[pos]{the position to insert column in}
  @return{The number of columns in @arg{tree-view} after insertion.}
  This inserts the column into the @arg{tree-view} at position. If position is
  -1, then the column is inserted at the end. If @arg{tree-view} has
  @code{\"fixed_height\"} mode enabled, then column must have its
  @code{\"sizing\"} property set to be @code{:fixed}."
  (tree-view g-object)
  (column g-object)
  (pos :int))

(export 'gtk-tree-view-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_attributes ()
;;;
;;; gint gtk_tree_view_insert_column_with_attributes (GtkTreeView *tree_view,
;;;                                                   gint position,
;;;                                                   const gchar *title,
;;;                                                   GtkCellRenderer *cell,
;;;                                                   ...);
;;;
;;; Creates a new GtkTreeViewColumn and inserts it into the tree_view at
;;; position. If position is -1, then the newly created column is inserted at
;;; the end. The column is initialized with the attributes given. If tree_view
;;; has "fixed_height" mode enabled, then the new column will have its sizing
;;; property set to be GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; position :
;;;     The position to insert the new column in
;;;
;;; title :
;;;     The title to set the header to
;;;
;;; cell :
;;;     The GtkCellRenderer
;;;
;;; ... :
;;;     A NULL-terminated list of attributes
;;;
;;; Returns :
;;;     The number of columns in tree_view after insertion.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_data_func ()
;;;
;;; gint gtk_tree_view_insert_column_with_data_func (GtkTreeView *tree_view,
;;;                                                  gint position,
;;;                                                  const gchar *title,
;;;                                                  GtkCellRenderer *cell,
;;;                                                  GtkTreeCellDataFunc func,
;;;                                                  gpointer data,
;;;                                                  GDestroyNotify dnotify);
;;;
;;; Convenience function that inserts a new column into the GtkTreeView with the
;;; given cell renderer and a GtkCellDataFunc to set cell renderer attributes
;;; (normally using data from the model). See also
;;; gtk_tree_view_column_set_cell_data_func(),
;;; gtk_tree_view_column_pack_start(). If tree_view has "fixed_height" mode
;;; enabled, then the new column will have its "sizing" property set to be
;;; GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; position :
;;;     Position to insert, -1 for append
;;;
;;; title :
;;;     column title
;;;
;;; cell :
;;;     cell renderer for column
;;;
;;; func :
;;;     function to set attributes of cell renderer
;;;
;;; data :
;;;     data for func
;;;
;;; dnotify :
;;;     destroy notifier for data
;;;
;;; Returns :
;;;     number of columns in the tree view post-insert
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_n_columns ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_n_columns" gtk-tree-view-get-n-columns) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{The number of columns in the @arg{tree-view}}
  @begin{short}
    Queries the number of columns in the given @arg{tree-view}.
  @end{short}

  Since 3.4"
  (tree-view (g-object gtk-tree-view)))

(export 'gtk-tree-view-get-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_column" gtk-tree-view-get-column) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget.}
  @argument[n]{the position of the column, counting from 0}
  @begin{return}
    The @class{gtk-tree-view-column}, or @code{nil} if the position is outside
    the range of columns.
  @end{return}
  Gets the @class{gtk-tree-view-column} at the given position in the
  @arg{tree-view}."
  (tree-view g-object)
  (n :int))

(export 'gtk-tree-view-get-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_columns ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_columns" gtk-tree-view-get-columns)
    (g-list g-object)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{A list of @class{gtk-tree-view-column}'s.}
  Returns a list of all the @class{gtk-tree-view-column}'s currently in
  @arg{tree-view}."
  (tree-view g-object))

(export 'gtk-tree-view-get-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_move_column_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_move_column_after" gtk-tree-view-move-column-after)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} to be moved}
  @argument[base-column]{the @class{gtk-tree-view-column} to be moved relative
    to, or @code{nil}}
  Moves column to be after to @arg{base-column}. If @arg{base-column} is
  @code{nil}, then column is placed in the first position."
  (tree-view g-object)
  (column g-object)
  (base-column g-object))

(export 'gtk-tree-view-move-column-after)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_expander_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-expander-column))

(defun gtk-tree-view-set-expander-column (tree-view column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[column]{@code{nil}, or the column to draw the expander arrow at}
  @begin{short}
    Sets the column to draw the expander arrow at. It must be in
    @arg{tree-view}. If column is @code{nil}, then the expander arrow is always
    at the first visible column.
  @end{short}

  If you do not want expander arrow to appear in your tree, set the expander
  column to a hidden column."
  (setf (gtk-tree-view-expander-column tree-view) column))

(export 'gtk-tree-view-set-expander-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_expander_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-expander-column))

(defun gtk-tree-view-get-expander-column (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{The expander column.}
  Returns the column that is the current expander column. This column has the
  expander arrow drawn next to it."
  (gtk-tree-view-expander-column tree-view))

(export 'gtk-tree-view-get-expander-column)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumnDropFunc ()
;;;
;;; gboolean (*GtkTreeViewColumnDropFunc) (GtkTreeView *tree_view,
;;;                                        GtkTreeViewColumn *column,
;;;                                        GtkTreeViewColumn *prev_column,
;;;                                        GtkTreeViewColumn *next_column,
;;;                                        gpointer data);
;;;
;;; Function type for determining whether column can be dropped in a particular
;;; spot (as determined by prev_column and next_column). In left to right
;;; locales, prev_column is on the left of the potential drop spot, and
;;; next_column is on the right. In right to left mode, this is reversed. This
;;; function should return TRUE if the spot is a valid drop spot. Please note
;;; that returning TRUE does not actually indicate that the column drop was
;;; made, but is meant only to indicate a possible drop spot to the user.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     The GtkTreeViewColumn being dragged
;;;
;;; prev_column :
;;;     A GtkTreeViewColumn on one side of column
;;;
;;; next_column :
;;;     A GtkTreeViewColumn on the other side of column
;;;
;;; data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE, if column can be dropped in this spot
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-column-drop-func-cb :boolean
    ((tree-view g-object)
     (column g-object)
     (prev-column g-object)
     (next-column g-object)
     (data :pointer))
  (let ((fn (glib::get-stable-pointer-value data)))
    (funcall fn tree-view column prev-column next-column)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_column_drag_function ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_column_drag_function"
          %gtk-tree-view-set-column-drag-function) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-column-drag-fuction (tree-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[func]{a function to determine which columns are reorderable,
    or @code{nil}}
  @begin{short}
    Sets a user function for determining where a column may be dropped when
    dragged.
  @end{short}
  This function is called on every column pair in turn at the beginning of a
  column drag to determine where a drop can take place. The arguments passed to
  func are: the @arg{tree-view}, the @class{gtk-tree-view-column} being
  dragged, and the two @class{gtk-tree-view-column}'s determining the drop spot.
  If either of the @class{gtk-tree-view-column} arguments for the drop spot
  are @code{nil}, then they indicate an edge. If func is set to be @code{nil},
  then @arg{tree-view} reverts to the default behavior of allowing all columns
  to be dropped everywhere."
  (%gtk-tree-view-set-column-drag-function
                             tree-view
                             (callback gtk-tree-view-column-drop-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_point" gtk-tree-view-scroll-to-point) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[tree-x]{x coordinate of new top-left pixel of visible area, or -1}
  @argument[tree-y]{y coordinate of new top-left pixel of visible area, or -1}
  @begin{short}
    Scrolls the tree view such that the top-left corner of the visible area is
    @arg{tree-x}, @arg{tree-y}, where @arg{tree-x} and @arg{tree-y} are
    specified in tree coordinates.
  @end{short}
  The @arg{tree-view} must be realized before this function is called. If it is
  not, you probably want to be using the @fun{gtk-tree-view-scroll-to-cell}
  function.

  If either @arg{tree-x} or @arg{tree-y} are -1, then that direction is not
  scrolled."
  (tree-view g-object)
  (tree-x :int)
  (tree-y :int))

(export 'gtk-tree-view-scroll-to-point)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_cell" %gtk-tree-view-scroll-to-cell) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun gtk-tree-view-scroll-to-cell (tree-view
                                     path column
                                     &optional
                                     (row-align 0.5 row-align-supplied-p)
                                     (col-align 0.5 col-align-supplied-p))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{the path of the row to move to, or @code{nil}}
  @argument[column]{the @class{gtk-tree-view-column} to move horizontally to,
    or @code{nil}}
  @argument[use-align]{whether to use alignment arguments, or @code{nil}}
  @argument[row-align]{the vertical alignment of the row specified by path}
  @argument[col-align]{the horizontal alignment of the column specified by
    column}
  @begin{short}
    Moves the alignments of @arg{tree-view} to the position specified by column
    and path.
  @end{short}
  If column is @code{nil}, then no horizontal scrolling occurs. Likewise, if
  path is NULL no vertical scrolling occurs. At a minimum, one of column or path
  need to be non-@code{nil}. @arg{row-align} determines where the row is placed,
  and @arg{col-align} determines where column is placed. Both are expected to be
  between 0.0 and 1.0. 0.0 means left/top alignment, 1.0 means right/bottom
  alignment, 0.5 means center.

  If @arg{use-align} is @code{nil}, then the alignment arguments are ignored,
  and the tree does the minimum amount of work to scroll the cell onto the
  screen. This means that the cell will be scrolled to the edge closest to its
  current position. If the cell is currently visible on the screen, nothing is
  done.

  This function only works if the model is set, and path is a valid row on the
  model. If the model changes before the @arg{tree-view} is realized, the
  centered path will be modified to reflect this change."
  (%gtk-tree-view-scroll-to-cell tree-view
                                 path
                                 column
                                 (or row-align-supplied-p col-align-supplied-p)
                                 row-align
                                 col-align))

(export 'gtk-tree-view-scroll-to-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_cursor" %gtk-tree-view-set-cursor) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (focus-column g-object)
  (start-editing :boolean))

(defun gtk-tree-view-set-cursor (tree-view path &key focus-column start-editing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[focus-column]{a @class{gtk-tree-view-column}, or @code{nil}}
  @argument[start-editing]{@em{true} if the specified cell should start being
    edited}
  @begin{short}
    Sets the current keyboard focus to be at path, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If @arg{focus-column} is not @code{nil}, then focus is given to the
  column specified by it. Additionally, if @arg{focus-column} is specified, and
  @arg{start-editing} is @em{true}, then editing should be started in the
  specified cell. This function is often followed by the
  @fun{gtk-widget-grab-focus} (@arg{tree-view}) in order to give keyboard focus
  to the widget. Please note that editing can only happen when the widget is
  realized.

  If @arg{path} is invalid for model, the current cursor (if any) will be unset
  and the function will return without failing."
  (%gtk-tree-view-set-cursor tree-view path focus-column start-editing))

(export 'gtk-tree-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor_on_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_cursor_on_cell" %gtk-tree-view-set-cursor-on-cell)
    :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (focus-column g-object)
  (focus-cell g-object)
  (start-editing :boolean))

(defun gtk-tree-view-set-cursor-on-cell (tree-view
                                         path
                                         &key focus-column
                                              focus-cell
                                              start-editing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[focus-column]{a @class{gtk-tree-view-column}, or @code{nil}}
  @argument[focus-cell]{a @class{gtk-cell-renderer}, or @code{nil}}
  @argument[start-editing]{@em{true} if the specified cell should start being
    edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If @arg{focus-column} is not @code{nil}, then focus is given to the
  column specified by it. If @arg{focus-column} and @arg{focus-cell} are not
  @code{nil}, and @arg{focus-column} contains 2 or more editable or activatable
  cells, then focus is given to the cell specified by focus_cell. Additionally,
  if @arg{focus-column} is specified, and @arg{start-editing} is @em{true}, then
  editing should be started in the specified cell. This function is often
  followed by the @fun{gtk-widget-grab-focus} (@arg{tree-view}) in order to give
  keyboard focus to the widget. Please note that editing can only happen when
  the widget is realized.

  If @arg{path} is invalid for model, the current cursor (if any) will be unset
  and the function will return without failing.

  Since 2.2"
  (%gtk-tree-view-set-cursor-on-cell tree-view
                                     path
                                     focus-column
                                     focus-cell
                                     start-editing))

(export 'gtk-tree-view-set-cursor-on-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cursor" %gtk-tree-view-get-cursor) :void
  (tree-view g-object)
  (path :pointer)
  (focus-column :pointer))

(defun gtk-tree-view-get-cursor (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{path} -- the current cursor path, or @code{nil} @br{}
    @code{focus-column} -- the current focus column, or @code{nil}
  @end{return}
  @begin{short}
    Fills in path and @arg{focus-column} with the current path and focus column.
  @end{short}
  If the cursor is not currently set, then path will be @code{nil}. If no column
  currently has focus, then @arg{focus-column} will be @code{nil}."
  (with-foreign-objects ((path :pointer) (focus-column :pointer))
    (%gtk-tree-view-get-cursor tree-view path focus-column)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref focus-column 'g-object))))

(export 'gtk-tree-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_activated ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_activated" gtk-tree-view-row-activated) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{the @class{gtk-tree-path} to be activated}
  @argument[column]{the @class{gtk-tree-view-column} to be activated}
  Activates the cell determined by @arg{path} and @arg{column}."
  (tree-view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-row-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_all" gtk-tree-view-expand-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  Recursively expands all nodes in the @arg{tree-view}."
  (tree-view g-object))

(export 'gtk-tree-view-expand-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_all" gtk-tree-view-collapse-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  Recursively collapses all visible, expanded nodes in @arg{tree-view}."
  (tree-view g-object))

(export 'gtk-tree-view-collapse-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_to_path" gtk-tree-view-expand-to-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{path to a row}
  @begin{short}
    Expands the row at path. This will also expand all parent rows of path as
    necessary.
  @end{short}

  Since 2.2"
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-expand-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_row" gtk-tree-view-expand-row) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-14}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{path to a row}
  @argument[open-all]{whether to recursively expand, or just expand immediate
    children}
  @return{@em{True} if the row existed and had children.}
  Opens the row so its children are visible.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (tree-view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (open-all :boolean))

(export 'gtk-tree-view-expand-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_row" gtk-tree-view-collapse-row) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{path to a row in the @arg{tree-view}}
  @return{@em{True} if the row was collapsed.}
  Collapses a row (hides its child rows, if they exist)."
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-collapse-row)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewMappingFunc ()
;;;
;;; void (*GtkTreeViewMappingFunc) (GtkTreeView *tree_view,
;;;                                 GtkTreePath *path,
;;;                                 gpointer user_data);
;;;
;;; Function used for gtk_tree_view_map_expanded_rows().
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     The path that's expanded
;;;
;;; user_data :
;;;     user data
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-mapping-func-cb :void
    ((tree-view g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (data :pointer))
  (funcall (glib::get-stable-pointer-value data) tree-view path))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_map_expanded_rows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_map_expanded_rows" %gtk-tree-view-map-expanded-rows)
    :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-view-map-expanded-rows (tree-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[func]{a function to be called}
  Calls @arg{func} on all expanded rows."
  (with-stable-pointer (ptr func)
    (%gtk-tree-view-map-expanded-rows tree-view
                                      (callback gtk-tree-view-mapping-func-cb)
                                      ptr)))

(export 'gtk-tree-view-map-expanded-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_expanded ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_expanded" gtk-tree-view-row-expanded) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} object to test expansion state}
  @return{@em{True} if path is expanded.}
  Returns @em{true} if the node pointed to by path is expanded in
  @arg{tree-view}."
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-row-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-reorderable))

(defun gtk-tree-view-set-reorderable (tree-view reorderable)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[reorderable]{@em{true}, if the tree can be reordered}
  @begin{short}
    This function is a convenience function to allow you to reorder models that
    support the @class{gtk-tree-drag-source} interface and the
    @class{gtk-tree-drag-dest} interface.
  @end{short}
  Both @class{gtk-tree-store} and @class{gtk-list-store} support these. If
  @arg{reorderable} is @em{true}, then the user can reorder the model by
  dragging and dropping rows. The developer can listen to these changes by
  connecting to the model's \"row-inserted\" and \"row-deleted\" signals. The
  reordering is implemented by setting up the tree view as a drag source and
  destination. Therefore, drag and drop can not be used in a reorderable view
  for any other purpose.

  This function does not give you any degree of control over the order - any
  reordering is allowed. If more control is needed, you should probably handle
  drag and drop manually."
  (setf (gtk-tree-view-reorderable tree-view) reorderable))

(export 'gtk-tree-view-set-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_reorderable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-reorderable))

(defun gtk-tree-view-get-reorderable (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@em{True} if the tree can be reordered.}
  Retrieves whether the user can reorder the tree via drag-and-drop. See the
  @fun{gtk-tree-view-set-reorderable} function."
  (gtk-tree-view-reorderable tree-view))

(export 'gtk-tree-view-get-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_path_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_path_at_pos" %gtk-tree-view-get-path-at-pos)
    :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-get-path-at-pos (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{the x position to be identified (relative to bin_window)}
  @argument[y]{the y position to be identified (relative to bin_window)}
  @begin{return}
    @code{path} -- a @class{gtk-tree-path} object, or @code{nil} @br{}
    @code{column} -- a @class{gtk-tree-view-column} object, or @code{nil} @br{}
    @code{cell-x} -- where the x coordinate relative to the cell can be placed,
      or @code{nil} @br{}
    @code{cell-y} -- where the y coordinate relative to the cell can be placed,
      or @code{nil}
  @end{return}
  @begin{short}
    Finds the path at the point (x, y), relative to bin_window coordinates
    (please see the function @fun{gtk-tree-view-get-bin-window}).
  @end{short}
  That is, x and y are relative to an events coordinates. x and y must come from
  an event on the @arg{tree-view} only where @code{event->window ==
  gtk_tree_view_get_bin_window()}. It is primarily for things like popup menus.
  If @arg{path} is non-@code{nil}, then it will be filled with the
  @class{gtk-tree-path} at that point. If @arg{column} is non-@code{nil}, then
  it will be filled with the column at that point. @arg{cell-x} and @arg{cell-y}
  return the coordinates relative to the cell background (i. e. the
  @code{background-area} passed to the @fun{gtk-cell-renderer-render}). This
  function is only meaningful if @arg{tree-view} is realized. Therefore this
  function will always return @code{nil} if @arg{tree-view} is not realized or
  does not have a model.

  For converting widget coordinates (e. g. the ones you get from
  @code{GtkWidget::query-tooltip}), please see the function
  @fun{gtk-tree-view-convert-widget-to-bin-window-coords}.
  @see-function{gtk-tree-view-get-bin-window}
  @see-function{gtk-tree-view-convert-widget-to-bin-window-coords}"
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-get-path-at-pos tree-view
                                          x y
                                          path
                                          column
                                          cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref column 'g-object)
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'gtk-tree-view-get-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_blank_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_blank_at_pos" %gtk-tree-view-is-blank-at-pos)
    :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-is-blank-at-pos (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{the x position to be identified (relative to bin_window)}
  @argument[y]{the y position to be identified (relative to bin_window)}
  @begin{return}
    @code{path} -- a @class{gtk-tree-path} object, or @code{nil} @br{}
    @code{column} -- a @class{gtk-tree-view-column}, or @code{nil} @br{}
    @code{cell-x} -- where the x coordinate relative to the cell can be placed,
      or @code{nil} @br{}
    @code{cell-y} -- where the y coordinate relative to the cell can be placed,
      or @code{nil}
  @end{return}
  @begin{short}
    Determine whether the point (x, y) in @arg{tree-view} is blank, that is no
    cell content nor an expander arrow is drawn at the location. If so, the
    location can be considered as the background. You might wish to take special
    action on clicks on the background, such as clearing a current selection,
    having a custom context menu or starting rubber banding.
  @end{short}

  The x and y coordinate that are provided must be relative to bin_window
  coordinates. That is, x and y must come from an event on @arg{tree-view} where
  @code{event->window == gtk_tree_view_get_bin_window()}.

  For converting widget coordinates (e. g. the ones you get from
  @code{GtkWidget::query-tooltip}), please see the function
  @fun{gtk-tree-view-convert-widget-to-bin-window-coords}.

  The @arg{path}, @arg{column}, @arg{cell-x} and @arg{cell-y} arguments will be
  filled in likewise as for the @fun{gtk-tree-view-get-path-at-pos} function.
  Please see the @fun{gtk-tree-view-get-path-at-pos} function for more
  information.

  Since 3.0
  @see-function{gtk-tree-view-convert-widget-to-bin-window-coords}"
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-is-blank-at-pos tree-view
                                          x y
                                          path
                                          column
                                          cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref column 'g-object)
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'gtk-tree-view-is-blank-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cell_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cell_area" %gtk-tree-view-get-cell-area) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-cell-area (tree-view path column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} object for the row, or @code{nil} to
    get only horizontal coordinates}
  @argument[column]{a @class{gtk-tree-view-column} for the column, or @code{nil}
    to get only vertical coordinates}
  @return{Cell rectangle.}
  Fills the bounding rectangle in bin_window coordinates for the cell at the
  row specified by @arg{path} and the @arg{column} specified by @arg{column}.
  If @arg{path} is @code{nil}, or points to a path not currently displayed, the
  y and height fields of the rectangle will be filled with 0. If column is
  @code{nil}, the x and width fields will be filled with 0. The sum of all cell
  rects does not cover the entire tree; there are extra pixels in between rows,
  for example. The returned rectangle is equivalent to the @arg{cell-area}
  passed to the @fun{gtk-cell-renderer-render} function. This function is only
  valid if @arg{tree-view} is realized."
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-cell-area tree-view path column rect)
    rect))

(export 'gtk-tree-view-get-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_background_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_background_area"
          %gtk-tree-view-get-background-area) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-background-area (tree-view path column)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} for the row, or @code{nil} to get only
    horizontal coordinates}
  @argument[column]{a @class{gtk-tree-view-column} for the column, or @code{nil}
    to get only vertical coordiantes}
  @return{Rectangle with cell background rect.}
  @begin{short}
    Returns the bounding rectangle in bin_window coordinates for the cell at the
    row specified by @arg{path} and the column specified by @arg{column}.
  @end{short}
  If @arg{path} is @code{nil}, or points to a node not found in the tree, the y
  and height fields of the rectangle will be filled with 0. If column is
  @code{nil}, the x and width fields will be filled with 0. The returned
  rectangle is equivalent to the @code{background_area} passed to the
  @fun{gtk-cell-renderer-render} function. These background areas tile to cover
  the entire bin window. Contrast with the @code{cell-area}, returned by the
  @fun{gtk-tree-view-get-cell-area} function, which returns only the cell
  itself, excluding surrounding borders and the tree expander area."
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-background-area tree-view path column rect)
    rect))

(export 'gtk-tree-view-get-background-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_rect" %gtk-tree-view-get-visible-rect)
    :void
  (tree-view g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-visible-rect (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@code{visible-rect} - rectangle}
  Returns @arg{visible-rect} with the currently visible region of the buffer, in
  tree coordinates. Convert to bin_window coordinates with the
  @fun{gtk-tree-view-convert-tree-to-bin-window-coords} function. Tree
  coordinates start at 0,0 for row 0 of the tree, and cover the entire
  scrollable area of the tree."
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-visible-rect tree-view rect)
    rect))

(export 'gtk-tree-view-get-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_range" %gtk-tree-view-get-visible-range)
    :boolean
  (tree-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun gtk-tree-view-get-visible-range (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{start-path} -- start of region, or @code{nil} @br{}
    @code{end-path}  -- end of region, or @code{nil}
  @end{return}
  @begin{short}
    Returns @arg{start-path} and @arg{end-path} to be the first and last visible
    path. Note that there may be invisible paths in between.
  @end{short}

  Since 2.8"
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (%gtk-tree-view-get-visible-range tree-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref end-path '(g-boxed-foreign gtk-tree-path :return))))))

(export 'gtk-tree-view-get-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_bin_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_bin_window" gtk-tree-view-get-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{A @class{gdk-window} object, or @code{nil} when @arg{tree-view}
    has not been realized yet.}
  @begin{short}
    Returns the window that @arg{tree-view} renders to.
  @end{short}
  This is used primarily to compare to @code{event->window} to confirm that the
  event on @arg{tree-view} is on the right window."
  (tree-view (g-object gtk-tree-view)))

(export 'gtk-tree-view-get-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_tree_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_tree_coords"
          %gtk-tree-view-convert-bin-window-to-tree-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-bin-window-to-tree-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{x coordinate relative to \"bin-window\"}
  @argument[y]{y coordinate relative to \"bin-window\"}
  @begin{return}
    @code{tx} -- tree x coordinate @br{}
    @code{ty} -- tree y coordinate
  @end{return}
  @begin{short}
    Converts \"bin-window\" coordinates to coordinates for the tree (the full
    scrollable area of the tree).
  @end{short}

  Since 2.12"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-bin-window-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-bin-window-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_widget_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_widget_coords"
          %gtk-tree-view-convert-bin-window-to-widget-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-bin-window-to-widget-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{\"bin-window\" x coordinate}
  @argument[y]{\"bin-window\" y coordinate}
  @begin{return}
    @code{wx} -- widget x coordinate @br{}
    @code{wy} -- widget y coordinate
  @end{return}
  @begin{short}
    Converts \"bin-window\" coordinates (see the function
    @fun{gtk-tree-view-get-bin-window}) to widget relative coordinates.
  @end{short}

  Since 2.12
  @see-function{gtk-tree-view-get-bin-window}"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-bin-window-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-bin-window-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_bin_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_bin_window_coords"
          %gtk-tree-view-convert-tree-to-bin-window-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-tree-to-bin-window-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree--view} widget}
  @argument[x]{tree x coordinate}
  @argument[y]{tree y coordinate}
  @begin{return}
    @code{bx} -- x coordinate relative to \"bin-window\" @br{}
    @code{by} -- y coordinate relative to \"bin-window\"
  @end{return}
  @begin{short}
    Converts tree coordinates (coordinates in full scrollable area of the tree)
    to \"bin-window\" coordinates.
  @end{short}

  Since 2.12"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-tree-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-tree-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_widget_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_widget_coords"
          %gtk-tree-view-convert-tree-to-widget-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-tree-to-widget-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{x coordinate relative to the tree}
  @argument[y]{y coordinate relative to the tree}
  @begin{return}
    @code{wx} -- widget x coordinate @br{}
    @code{wy} -- widget y coordinate
  @end{return}
  @begin{short}
    Converts tree coordinates (coordinates in full scrollable area of the tree)
    to widget coordinates.
  @end{short}

  Since 2.12"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-tree-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-tree-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_bin_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_bin_window_coords"
          %gtk-tree-view-convert-widget-to-bin-window-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-widget-to-bin-window-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{x coordinate relative to the widget}
  @argument[y]{y coordinate relative to the widget}
  @begin{return}
    @code{bx} -- \"bin-window\" x coordinate @br{}
    @code{by} -- \"bin-window\" y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the \"bin-window\" (see the
    function @fun{gtk-tree-view-get-bin-window}).
  @end{short}

  Since 2.12
  @see-function{gtk-tree-view-get-bin-window}"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-widget-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_tree_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_tree_coords"
          %gtk-tree-view-convert-widget-to-tree-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-widget-to-tree-coords (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{x coordinate relative to the widget}
  @argument[y]{y coordinate relative to the widget}
  @begin{return}
    @code{tx} -- tree x coordinate @br{}
    @code{ty} -- tree y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the tree (the full scrollable
    area of the tree).
  @end{short}

  Since 2.12"
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-widget-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-widget-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_dest"
          %gtk-tree-view-enable-model-drag-dest) :void
  (tree-view (g-object gtk-tree-view))
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-dest (tree-view targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[targets]{the table of targets that the drag will support}
  @argument[actions]{the bitmask of possible actions for a drag from this
    widget}
  Turns @arg{tree-view} into a drop destination for automatic DND. Calling this
  method sets \"reorderable\" to @code{nil}."
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-tree-view-enable-model-drag-dest tree-view
                                           targets-ptr
                                           n-targets
                                           actions)))

(export 'gtk-tree-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_source"
          %gtk-tree-view-enable-model-drag-source) :void
  (tree-view (g-object gtk-tree-view))
  (start-button-mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-source (tree-view start-button-mask
                                                         targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[start-button-mask]{mask of allowed buttons to start drag}
  @argument[targets]{the table of targets that the drag will support}
  @argument[actions]{the bitmask of possible actions for a drag from this
    widget}
  Turns @arg{tree-view} into a drag source for automatic DND. Calling this
  method sets \"reorderable\" to @code{nil}."
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-tree-view-enable-model-drag-source tree-view
                                             start-button-mask
                                             targets-ptr
                                             n-targets
                                             actions)))

(export 'gtk-tree-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_source ()
;;;
;;; void gtk_tree_view_unset_rows_drag_source (GtkTreeView *tree_view);
;;;
;;; Undoes the effect of gtk_tree_view_enable_model_drag_source(). Calling this
;;; method sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_dest ()
;;;
;;; void gtk_tree_view_unset_rows_drag_dest (GtkTreeView *tree_view);
;;;
;;; Undoes the effect of gtk_tree_view_enable_model_drag_dest(). Calling this
;;; method sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_drag_dest_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_drag_dest_row" gtk-tree-view-set-drag-dest-row)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-1}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[path]{the path of the row to highlight, or @code{nil}}
  @argument[pos]{specifies whether to drop before, after or into the row}
  @begin{short}
    Sets the row that is highlighted for feedback.
  @end{short}
  If @arg{path} is @code{nil}, an existing highlight is removed.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-tree-view-drop-position}
  @see-function{gtk-tree-view-get-drag-dest-row}"
  (tree-view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (pos gtk-tree-view-drop-position))

(export 'gtk-tree-view-set-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_drag_dest_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_drag_dest_row" %gtk-tree-view-get-drag-dest-row)
    :void
  (tree-view (g-object gtk-tree-view))
  (path :pointer)
  (pos :pointer))

(defun gtk-tree-view-get-drag-dest-row (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-1}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{path} -- the path of the highlighted row, or @code{nil} @br{}
    @code{pos} -- the drop position, or @code{nil}
  @end{return}
  Gets information about the row that is highlighted for feedback.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-tree-view-drop-position}
  @see-function{gtk-tree-view-set-drag-dest-row}"
  (with-foreign-objects ((path :pointer) (pos :pointer))
    (%gtk-tree-view-get-drag-dest-row tree-view path pos)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref pos 'gtk-tree-view-drop-position))))

(export 'gtk-tree-view-get-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_dest_row_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_dest_row_at_pos"
          %gtk-tree-view-get-dest-row-at-pos) :boolean
  (tree_view (g-object gtk-tree-view))
  (drag-x :int)
  (drag-y :int)
  (path :pointer)
  (pos :pointer))

(defun gtk-tree-view-get-dest-row-at-pos (tree-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[x]{the position to determine the destination row for}
  @argument[y]{the position to determine the destination row for}
  @begin{return}
    @code{path} -- the path of the highlighted row, or @code{nil} @br{}
    @code{pos} -- the drop position, or @code{nil}
  @end{return}
  Determines the destination row for a given position. @arg{x} and @arg{y} are
  expected to be in widget coordinates. This function is only meaningful if
  @arg{tree-view} is realized. Therefore this function will always return
  @code{nil} if @arg{tree-view} is not realized or does not have a model."
  (with-foreign-objects ((path :pointer) (pos :int))
    (when (%gtk-tree-view-get-dest-row-at-pos tree-view x y path pos)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref pos 'gtk-tree-view-drop-position)))))

(export 'gtk-tree-view-get-dest-row-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_create_row_drag_icon ()
;;;
;;; cairo_surface_t * gtk_tree_view_create_row_drag_icon
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      GtkTreePath *path);
;;;
;;; Creates a cairo_surface_t representation of the row at path. This image is
;;; used for a drag icon.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     a GtkTreePath in tree_view
;;;
;;; Returns :
;;;     a newly-allocated surface of the drag icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_enable_search ()
;;;
;;; void gtk_tree_view_set_enable_search (GtkTreeView *tree_view,
;;;                                       gboolean enable_search);
;;;
;;; If enable_search is set, then the user can type in text to search through
;;; the tree interactively (this is sometimes called "typeahead find").
;;;
;;; Note that even if this is FALSE, the user can still initiate a search using
;;; the "start-interactive-search" key binding.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; enable_search :
;;;     TRUE, if the user can search interactively
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_enable_search ()
;;;
;;; gboolean gtk_tree_view_get_enable_search (GtkTreeView *tree_view);
;;;
;;; Returns whether or not the tree allows to start interactive searching by
;;; typing in text.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     whether or not to let the user search interactively
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_column ()
;;;
;;; gint gtk_tree_view_get_search_column (GtkTreeView *tree_view);
;;;
;;; Gets the column searched on by the interactive search code.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the column the interactive search code searches in.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_column ()
;;;
;;; void gtk_tree_view_set_search_column (GtkTreeView *tree_view, gint column);
;;;
;;; Sets column as the column where the interactive search code should search in
;;; for the current model.
;;;
;;; If the search column is set, users can use the "start-interactive-search"
;;; key binding to bring up search popup. The enable-search property controls
;;; whether simply typing text will also start an interactive search.
;;;
;;; Note that column refers to a column of the current model. The search column
;;; is reset to -1 when the model is changed.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     the column of the model to search in, or -1 to disable searching
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_equal_func ()
;;;
;;; GtkTreeViewSearchEqualFunc gtk_tree_view_get_search_equal_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the compare function currently in use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the currently used compare function for the search code.
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-equal-func-cb :boolean
  ((model g-object)
   (column :int)
   (key (:string :free-from-foreign nil))
   (iter (g-boxed-foreign gtk-tree-iter))
   (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data)
               model column key iter)
    (return-true () t)
   (return-false () t)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_equal_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_search_equal_func"
          %gtk-tree-view-set-search-equal-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-equal-func (tree-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[func]{the compare function to use during the search}
  Sets the compare function for the interactive search capabilities; note that
  somewhat like @code{strcmp()} returning 0 for equality
  @code{GtkTreeViewSearchEqualFunc} returns @code{nil} on matches."
  (%gtk-tree-view-set-search-equal-func
                             tree-view
                             (callback gtk-tree-view-search-equal-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-set-search-equal-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_entry ()
;;;
;;; GtkEntry * gtk_tree_view_get_search_entry (GtkTreeView *tree_view);
;;;
;;; Returns the GtkEntry which is currently in use as interactive search entry
;;; for tree_view. In case the built-in entry is being used, NULL will be
;;; returned.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the entry currently in use as search entry
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_entry ()
;;;
;;; void gtk_tree_view_set_search_entry (GtkTreeView *tree_view,
;;;                                      GtkEntry *entry);
;;;
;;; Sets the entry which the interactive search code will use for this
;;; tree_view. This is useful when you want to provide a search entry in our
;;; interface at all time at a fixed position. Passing NULL for entry will make
;;; the interactive search code use the built-in popup entry again.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; entry :
;;;     the entry the interactive search code of tree_view should use or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchPositionFunc ()
;;;
;;; void (*GtkTreeViewSearchPositionFunc) (GtkTreeView *tree_view,
;;;                                        GtkWidget *search_dialog,
;;;                                        gpointer user_data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-position-func :void
    ((tree-view g-object) (search-dialog g-object) (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data)
               tree-view
               search-dialog)
   (return () nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_position_func ()
;;;
;;; GtkTreeViewSearchPositionFunc gtk_tree_view_get_search_position_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the positioning function currently in use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the currently used function for positioning the search dialog.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_position_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_search_position_func"
          %gtk-tree-view-set-search-position-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-position-func (tree-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[func]{the function to use to position the search dialog,
    or @code{nil} to use the default search position function}
  @begin{short}
    Sets the function to use when positioning the search dialog.
  @end{short}

  Since 2.10"
  (if func
      (%gtk-tree-view-set-search-position-func
                               tree-view
                               (callback gtk-tree-view-set-search-position-func)
                               (glib::allocate-stable-pointer func)
                               (callback glib::stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-search-position-func tree-view
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer))))

(export 'gtk-tree-view-set-search-position-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_fixed_height_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-fixed-height-mode))

(defun gtk-tree-view-get-fixed-height-mode (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @return{@em{True} if @arg{tree-view} is in fixed height mode.}
  @begin{short}
    Returns whether fixed height mode is turned on for @arg{tree-view}.
  @end{short}

  Since 2.6
  @see-function{gtk-tree-view-set-fixed-height-mode}"
  (gtk-tree-view-fixed-height-mode tree-view))

(export 'gtk-tree-view-get-fixed-height-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_fixed_height_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-fixed-height-mode))

(defun gtk-tree-view-set-fixed-height-mode (tree-view enable)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[enable]{@em{true} to enable fixed height mode}
  @begin{short}
    Enables or disables the fixed height mode of @arg{tree-view}.
  @end{short}
  Fixed height mode speeds up @class{gtk-tree-view} by assuming that all rows
  have the same height. Only enable this option if all rows are the same height
  and all columns are of type @code{:column-fixed} of the
  @symbol{gtk-tree-view-column-sizing} enumeration.

  Since 2.6
  @see-function{gtk-tree-view-get-fixed-height-mode}"
  (setf (gtk-tree-view-fixed-height-mode tree-view) enable))

(export 'gtk-tree-view-set-fixed-height-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hover_selection ()
;;;
;;; gboolean gtk_tree_view_get_hover_selection (GtkTreeView *tree_view);
;;;
;;; Returns whether hover selection mode is turned on for tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if tree_view is in hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hover_selection ()
;;;
;;; void gtk_tree_view_set_hover_selection (GtkTreeView *tree_view,
;;;                                         gboolean hover);
;;;
;;; Enables or disables the hover selection mode of tree_view. Hover selection
;;; makes the selected row follow the pointer. Currently, this works only for
;;; the selection modes GTK_SELECTION_SINGLE and GTK_SELECTION_BROWSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; hover :
;;;     TRUE to enable hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hover_expand ()
;;;
;;; gboolean gtk_tree_view_get_hover_expand (GtkTreeView *tree_view);
;;;
;;; Returns whether hover expansion mode is turned on for tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if tree_view is in hover expansion mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hover_expand ()
;;;
;;; void gtk_tree_view_set_hover_expand (GtkTreeView *tree_view,
;;;                                      gboolean expand);
;;;
;;; Enables or disables the hover expansion mode of tree_view. Hover expansion
;;; makes rows expand or collapse if the pointer moves over them.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; expand :
;;;     TRUE to enable hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeDestroyCountFunc ()
;;;
;;; void (*GtkTreeDestroyCountFunc) (GtkTreeView *tree_view,
;;;                                  GtkTreePath *path,
;;;                                  gint children,
;;;                                  gpointer user_data);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_destroy_count_func ()
;;;
;;; void gtk_tree_view_set_destroy_count_func (GtkTreeView *tree_view,
;;;                                            GtkTreeDestroyCountFunc func,
;;;                                            gpointer data,
;;;                                            GDestroyNotify destroy);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_destroy_count_func has been deprecated since version 3.4
;;; and should not be used in newly-written code. Accessibility does not need
;;; the function anymore.
;;;
;;; This function should almost never be used. It is meant for private use by
;;; ATK for determining the number of visible children that are removed when the
;;; user collapses a row, or a row is deleted.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; func :
;;;     Function to be called when a view row is destroyed, or NULL.
;;;
;;; data :
;;;     User data to be passed to func, or NULL.
;;;
;;; destroy :
;;;     Destroy notifier for data, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewRowSeparatorFunc ()
;;;
;;; gboolean (*GtkTreeViewRowSeparatorFunc) (GtkTreeModel *model,
;;;                                          GtkTreeIter *iter,
;;;                                          gpointer data);
;;;
;;; Function type for determining whether the row pointed to by iter should be
;;; rendered as a separator. A common way to implement this is to have a boolean
;;; column in the model, whose values the GtkTreeViewRowSeparatorFunc returns.
;;;
;;; model :
;;;     the GtkTreeModel
;;;
;;; iter :
;;;     a GtkTreeIter pointing at a row in model
;;;
;;; data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE if the row is a separator
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-row-separator-func-callback :boolean
    ((tree-model g-object)
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data)
               tree-model
               iter)
    (return-true () t)
   (return-false () nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_row_separator_func ()
;;;
;;; GtkTreeViewRowSeparatorFunc gtk_tree_view_get_row_separator_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the current row separator function.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     the current row separator function.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_row_separator_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_row_separator_func"
          %gtk-tree-view-set-row-separator-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-row-separator-func (tree-view func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[func]{a @code{GtkTreeViewRowSeparatorFunc}}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator. If the row separator function is @code{nil},
    no separators are drawn. This is the default value.
  @end{short}

  Since 2.6"
  (if func
      (%gtk-tree-view-set-row-separator-func tree-view
                            (callback gtk-tree-view-row-separator-func-callback)
                            (glib::allocate-stable-pointer func)
                            (callback glib::stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-row-separator-func tree-view
                                             (null-pointer)
                                             (null-pointer)
                                             (null-pointer))))

(export 'gtk-tree-view-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_rubber_banding ()
;;;
;;; gboolean gtk_tree_view_get_rubber_banding (GtkTreeView *tree_view);
;;;
;;; Returns whether rubber banding is turned on for tree_view. If the selection
;;; mode is GTK_SELECTION_MULTIPLE, rubber banding will allow the user to select
;;; multiple rows by dragging the mouse.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if rubber banding in tree_view is enabled.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_rubber_banding ()
;;;
;;; void gtk_tree_view_set_rubber_banding (GtkTreeView *tree_view,
;;;                                        gboolean enable);
;;;
;;; Enables or disables rubber banding in tree_view. If the selection mode is
;;; GTK_SELECTION_MULTIPLE, rubber banding will allow the user to select
;;; multiple rows by dragging the mouse.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enable :
;;;     TRUE to enable rubber banding
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_rubber_banding_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_rubber_banding_active"
           gtk-tree-view-is-rubber-banding-active) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @em{True} if a rubber banding operation is currently being done in
    @arg{tree-view}.
  @end{return}
  @begin{short}
    Returns whether a rubber banding operation is currently being done in
    @arg{tree-view}.
  @end{short}

  Since 2.12"
  (tree-view g-object))

(export 'gtk-tree-view-is-rubber-banding-active)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_enable_tree_lines ()
;;;
;;; gboolean gtk_tree_view_get_enable_tree_lines (GtkTreeView *tree_view);
;;;
;;; Returns whether or not tree lines are drawn in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView.
;;;
;;; Returns :
;;;     TRUE if tree lines are drawn in tree_view, FALSE otherwise.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_enable_tree_lines ()
;;;
;;; void gtk_tree_view_set_enable_tree_lines (GtkTreeView *tree_view,
;;;                                           gboolean enabled);
;;;
;;; Sets whether to draw lines interconnecting the expanders in tree_view. This
;;; does not have any visible effects for lists.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enabled :
;;;     TRUE to enable tree line drawing, FALSE otherwise.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewGridLines
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewGridLines" gtk-tree-view-grid-lines
  (:export t
   :type-initializer "gtk_tree_view_grid_lines_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-grid-lines atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-tree-view-grid-lines atdoc:*external-symbols*)
 "@version{2013-5-31}
  @begin{short}
    Used to indicate which grid lines to draw in a tree view.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkTreeViewGridLines\" gtk-tree-view-grid-lines
  (:export t
   :type-initializer \"gtk_tree_view_grid_lines_get_type\")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No grid lines.}
    @entry[:horizontal]{Horizontal grid lines.}
    @entry[:vertical]{Vertical grid lines.}
    @entry[:both]{Horizontal and vertical grid lines.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_grid_lines ()
;;;
;;; GtkTreeViewGridLines gtk_tree_view_get_grid_lines (GtkTreeView *tree_view);
;;;
;;; Returns which grid lines are enabled in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     a GtkTreeViewGridLines value indicating which grid lines are enabled.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_grid_lines ()
;;;
;;; void gtk_tree_view_set_grid_lines (GtkTreeView *tree_view,
;;;                                    GtkTreeViewGridLines grid_lines);
;;;
;;; Sets which grid lines to draw in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; grid_lines :
;;;     a GtkTreeViewGridLines value indicating which grid lines to enable.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_row" gtk-tree-view-set-tooltip-row) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path} object}
  @begin{short}
    Sets the tip area of @arg{tooltip} to be the area covered by the row at
    @arg{path}.
  @end{short}
  See also the @fun{gtk-tree-view-set-tooltip-column} function for a simpler
  alternative. See also the @fun{gtk-tooltip-set-tip-area} function.

  Since 2.12
  @see-function{gtk-tree-view-set-tooltip-column}
  @see-function{gtk-tooltip-set-tip-area}"
  (tree-view g-object)
  (tooltip g-object)
  (tree-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-set-tooltip-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_cell" gtk-tree-view-set-tooltip-cell) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path} object or @code{nil}}
  @argument[column]{a @class{gtk-tree-view-column} object or @code{nil}}
  @argument[cell]{a @class{gtk-cell-renderer} or @code{nil}}
  @begin{short}
    Sets the tip area of @arg{tooltip} to the area @arg{path}, @arg{column} and
    @arg{cell} have in common.
  @end{short}
  For example if @arg{path} is @code{nil} and @arg{column} is set, the tip area
  will be set to the full area covered by @arg{column}. See also the
  @fun{gtk-tooltip-set-tip-area} function.

  Note that if @arg{path} is not specified and @arg{cell} is set and part of a
  @arg{column} containing the expander, the @arg{tooltip} might not show and
  hide at the correct position. In such cases path must be set to the current
  node under the mouse cursor for this function to operate correctly.

  See also the @fun{gtk-tree-view-set-tooltip-column} function for a simpler
  alternative.

  Since 2.12
  @see-function{gtk-tooltip-set-tip-area}"
  (tree-view g-object)
  (tooltip g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (cell g-object))

(export 'gtk-tree-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_tooltip_context"
          %gtk-tree-view-get-tooltip-context) :boolean
  (tree-view g-object)
  (x (:pointer :int))
  (y (:pointer :int))
  (keyboard-tip :boolean)
  (model (:pointer :pointer))
  (path (:pointer :pointer))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-view-get-tooltip-context (tree-view x y keyboard-tip)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[tree-view]{a @class{gtk-tree-view} widget}
  @argument{x} -- the x coordinate (relative to widget coordinates) @br{}
  @argument{y} -- the y coordinate (relative to widget coordinates) @br{}
  @argument{keyboard-tip} -- whether this is a keyboard tooltip or not @br{}
  @begin{return}
    @code{points-to-row} -- whether there is a tree view row at the given coordinates @br{}
    @code{x} -- the x coordinate (relative to widget coordinates) @br{}
    @code{y} -- the y coordinate (relative to widget coordinates) @br{}
    @code{model} -- a @class{gtk-tree-model} or @code{nil} @br{}
    @code{path} -- a @class{gtk-tree-path} or @code{nil} @br{}
    @code{iter} -- a @class{gtk-tree-iter} or @code{nil}
  @end{return}
  @begin{short}
    This function is supposed to be used in a \"query-tooltip\" signal handler
    for @class{gtk-tree-view}. The @arg{x}, @arg{y} and @arg{keyboard-tip}
    values which are received in the signal handler, should be passed to this
    function without modification.
  @end{short}

  The return value indicates whether there is a tree view row at the given
  coordinates (@em{true}) or not (@code{nil}) for mouse tooltips. For keyboard
  tooltips the row returned will be the cursor row. When @em{true}, then any of
  @arg{model}, @arg{path} and @arg{iter} which have been provided will be set to
  point to that row and the corresponding model. @arg{x} and @arg{y} will always
  be converted to be relative to @arg{tree-view}'s \"bin-window\" if
  @arg{keyboard-tip} is @code{nil}.

  Since 2.12"
  (with-foreign-objects ((px :int)
                         (py :int)
                         (model :pointer)
                         (path :pointer))
    (setf (mem-ref px :int) x
          (mem-ref py :int) y)
    (let* ((iter (make-gtk-tree-iter))
           (points-to-row (%gtk-tree-view-get-tooltip-context
                           tree-view px py keyboard-tip model path iter)))
      (values points-to-row
              (mem-ref px :int)
              (mem-ref py :int)
              (mem-ref model 'g-object)
              (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              iter))))

(export 'gtk-tree-view-get-tooltip-context)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_column ()
;;;
;;; gint gtk_tree_view_get_tooltip_column (GtkTreeView *tree_view);
;;;
;;; Returns the column of tree_view's model which is being used for displaying
;;; tooltips on tree_view's rows.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     the index of the tooltip column that is currently being used, or -1 if
;;;     this is disabled.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_column ()
;;;
;;; void gtk_tree_view_set_tooltip_column (GtkTreeView *tree_view, gint column);
;;;
;;; If you only plan to have simple (text-only) tooltips on full rows, you can
;;; use this function to have GtkTreeView handle these automatically for you.
;;; column should be set to the column in tree_view's model containing the
;;; tooltip texts, or -1 to disable this feature.
;;;
;;; When enabled, "has-tooltip" will be set to TRUE and tree_view will connect a
;;; "query-tooltip" signal handler.
;;;
;;; Note that the signal handler sets the text with gtk_tooltip_set_markup(), so
;;; &, <, etc have to be escaped in the text.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; column :
;;;     an integer, which is a valid column number for tree_view's model
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tree-view.lisp -----------------------------------------
