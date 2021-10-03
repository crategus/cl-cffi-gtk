;;; ----------------------------------------------------------------------------
;;; gtk.tree-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkTreeView
;;;
;;;     A widget for displaying both trees and lists
;;;
;;; Types and Values
;;;
;;;     GtkTreeView
;;;     GtkTreeViewDropPosition
;;;     GtkTreeViewPrivate
;;;     GtkTreeViewGridLines
;;;
;;; Functions
;;;
;;;     GtkTreeViewColumnDropFunc
;;;     GtkTreeViewMappingFunc
;;;     GtkTreeViewSearchEqualFunc
;;;
;;;     gtk_tree_view_new
;;;     gtk_tree_view_get_level_indentation                  Accessor
;;;     gtk_tree_view_get_show_expanders                     Accessor
;;;     gtk_tree_view_set_level_indentation                  Accessor
;;;     gtk_tree_view_set_show_expanders                     Accessor
;;;     gtk_tree_view_new_with_model
;;;     gtk_tree_view_get_model                              Accessor
;;;     gtk_tree_view_set_model                              Accessor
;;;     gtk_tree_view_get_selection
;;;     gtk_tree_view_get_hadjustment                      * deprecated
;;;     gtk_tree_view_set_hadjustment                      * deprecated
;;;     gtk_tree_view_get_vadjustment                      * deprecated
;;;     gtk_tree_view_set_vadjustment                      * deprecated
;;;     gtk_tree_view_get_headers_visible                    Accessor
;;;     gtk_tree_view_set_headers_visible                    Accessor
;;;     gtk_tree_view_columns_autosize
;;;     gtk_tree_view_get_headers_clickable                  Accessor
;;;     gtk_tree_view_set_headers_clickable                  Accessor
;;;     gtk_tree_view_set_rules_hint                         Accessor
;;;     gtk_tree_view_get_rules_hint                         Accessor
;;;
;;;     gtk_tree_view_set_activate_on_single_click           Accessor
;;;     gtk_tree_view_get_activate_on_single_click           Accessor
;;;
;;;     gtk_tree_view_append_column
;;;     gtk_tree_view_remove_column
;;;     gtk_tree_view_insert_column
;;;     gtk_tree_view_insert_column_with_attributes
;;;     gtk_tree_view_insert_column_with_data_func
;;;     gtk_tree_view_get_n_columns
;;;     gtk_tree_view_get_column
;;;     gtk_tree_view_get_columns
;;;     gtk_tree_view_move_column_after
;;;     gtk_tree_view_set_expander_column                    Accessor
;;;     gtk_tree_view_get_expander_column                    Accessor
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
;;;     gtk_tree_view_set_reorderable                        Accessor
;;;     gtk_tree_view_get_reorderable                        Accessor
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
;;;     gtk_tree_view_set_enable_search                      Accessor
;;;     gtk_tree_view_get_enable_search                      Accessor
;;;     gtk_tree_view_get_search_column                      Accessor
;;;     gtk_tree_view_set_search_column                      Accessor
;;;
;;;     GtkTreeViewSearchPositionFunc
;;;
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;     gtk_tree_view_get_search_position_func
;;;     gtk_tree_view_set_search_position_func
;;;     gtk_tree_view_get_fixed_height_mode                  Accessor
;;;     gtk_tree_view_set_fixed_height_mode                  Accessor
;;;     gtk_tree_view_get_hover_selection                    Accessor
;;;     gtk_tree_view_set_hover_selection                    Accessor
;;;     gtk_tree_view_get_hover_expand                       Accessor
;;;     gtk_tree_view_set_hover_expand                       Accessor
;;;
;;;     GtkTreeDestroyCountFunc                              deprecated
;;;     gtk_tree_view_set_destroy_count_func                 deprecated
;;;
;;;     GtkTreeViewRowSeparatorFunc
;;;
;;;     gtk_tree_view_get_row_separator_func
;;;     gtk_tree_view_set_row_separator_func
;;;     gtk_tree_view_get_rubber_banding                     Accessor
;;;     gtk_tree_view_set_rubber_banding                     Accessor
;;;     gtk_tree_view_is_rubber_banding_active
;;;     gtk_tree_view_get_enable_tree_lines                  Accessor
;;;     gtk_tree_view_set_enable_tree_lines                  Accessor
;;;
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context
;;;     gtk_tree_view_get_tooltip_column                     Accessor
;;;     gtk_tree_view_set_tooltip_column                     Accessor
;;;
;;; Properties
;;;
;;;             gboolean    activate-on-single-click    Read / Write
;;; GtkTreeViewGridLines    enable-grid-lines           Read / Write
;;;             gboolean    enable-search               Read / Write
;;;             gboolean    enable-tree-lines           Read / Write
;;;    GtkTreeViewColumn*   expander-column             Read / Write
;;;             gboolean    fixed-height-mode           Read / Write
;;;             gboolean    headers-clickable           Read / Write
;;;             gboolean    headers-visible             Read / Write
;;;             gboolean    hover-expand                Read / Write
;;;             gboolean    hover-selection             Read / Write
;;;                 gint    level-indentation           Read / Write
;;;         GtkTreeModel*   model                       Read / Write
;;;             gboolean    reorderable                 Read / Write
;;;             gboolean    rubber-banding              Read / Write
;;;             gboolean    rules-hint                  Read / Write
;;;                 gint    search-column               Read / Write
;;;             gboolean    show-expanders              Read / Write
;;;                 gint    tooltip-column              Read / Write
;;;
;;; Style Properties
;;;
;;;             gboolean    allow-rules                 Read
;;;             GdkColor*   even-row-color              Read
;;;                 gint    expander-size               Read
;;;                gchar*   grid-line-pattern           Read
;;;                 gint    grid-line-width             Read
;;;                 gint    horizontal-separator        Read
;;;             gboolean    indent-expanders            Read
;;;             GdkColor*   odd-row-color               Read
;;;                gchar*   tree-line-pattern           Read
;;;                 gint    tree-line-width             Read
;;;                 gint    vertical-separator          Read
;;;
;;; Signals
;;;
;;;                 void    columns-changed             Run Last
;;;                 void    cursor-changed              Run Last
;;;             gboolean    expand-collapse-cursor-row  Action
;;;             gboolean    move-cursor                 Action
;;;                 void    row-activated               Action
;;;                 void    row-collapsed               Run Last
;;;                 void    row-expanded                Run Last
;;;             gboolean    select-all                  Action
;;;             gboolean    select-cursor-parent        Action
;;;             gboolean    select-cursor-row           Action
;;;             gboolean    start-interactive-search    Action
;;;             gboolean    test-collapse-row           Run Last
;;;             gboolean    test-expand-row             Run Last
;;;             gboolean    toggle-cursor-row           Action
;;;             gboolean    unselect-all                Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkTreeView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeView implements AtkImplementorIface, GtkBuildable and
;;;     GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-drop-position atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-tree-view-drop-position atdoc:*external-symbols*)
 "@version{2021-2-25}
  @begin{short}
    An enumumeration for determining where a dropped row goes in a tree view.
  @end{short}
  @begin[code]{table}
    @entry[:before]{Dropped row is inserted before.}
    @entry[:after]{Dropped row is inserted after.}
    @entry[:into-or-before]{Dropped row becomes a child or is inserted before.}
    @entry[:into-or-after]{Dropped row becomes a child or is inserted after.}
  @end{table}
  @see-class{gtk-tree-view}")

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
 "@version{2021-2-25}
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
  @end{table}
  @see-class{gtk-tree-view}")

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
  ((activate-on-single-click
    gtk-tree-view-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (enable-grid-lines
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
 "@version{2021-2-25}
  @begin{short}
    Widget that displays any object that implements the @class{gtk-tree-model}
    interface.
  @end{short}

  Please refer to the tree widget conceptual overview for an overview of all
  the objects and data types related to the tree widget and how they work
  together.

  Several different coordinate systems are exposed in the @sym{gtk-tree-view}
  API. These are:

  @image[tree-view-coordinates]{}

  Coordinate systems in the @sym{gtk-tree-view} API:
  @begin[em]{table}
     @entry[Widget coordinates]{Coordinates relative to the widget. Usually
       the window from the function @fun{gtk-widget-window}.}
    @entry[Bin window coordinates]{Coordinates relative to the window that
      the @sym{gtk-tree-view} widget renders to.}
    @entry[Tree coordinates]{Coordinates relative to the entire scrollable area
      of the @sym{gtk-tree-view} widget. These coordinates start at (0, 0) for
      row 0 of the tree.}
  @end{table}
  Several functions are available for converting between the different
  coordinate systems. The most common translations are between widget and bin
  window coordinates and between bin window and tree coordinates. For the
  former you can use the function
  @fun{gtk-tree-view-convert-widget-to-bin-window-coords} (and vice versa), for
  the latter the function @fun{gtk-tree-view-convert-bin-window-to-tree-coords}
  (and vice versa).
  @begin[GtkTreeView as GtkBuildable]{dictionary}
    The @sym{gtk-tree-view} implementation of the @class{gtk-buildable}
    interface accepts @class{gtk-tree-view-column} objects as @code{<child>}
    elements and exposes the internal @class{gtk-tree-selection} object in UI
    definitions.

    @b{Example:} A UI definition fragment with the @sym{gtk-tree-view} widget
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
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 treeview.view
 ├── header
 │   ├── <column header>
 ┊   ┊
 │   ╰── <column header>
 │
 ╰── [rubberband]
    @end{pre}
    The @sym{gtk-tree-view} widget has a main CSS node with name @code{treeview}
    and style class @code{.view}. It has a subnode with name @code{header},
    which is the parent for all the column header widgets' CSS nodes. For
    rubberband selection, a subnode with name @code{rubberband} is used.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[allow-rules]{entry}
        The @code{allow-rules} style property of type @code{:boolean} (Read)
        @br{}
        Allow drawing of alternating color rows. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[even-row-color]{entry}
        The @code{even-row-color} style property of type @class{gdk-color}
        (Read) @br{}
        Color to use for even rows.
      @end{entry}
      @begin[expander-size]{entry}
        The @code{expander-size} style property of type @code{:int} (Read) @br{}
        Size of the expander arrow. @br{}
        Allowed values: >= 0 @br{}
        Default value: 14
      @end{entry}
      @begin[grid-line-pattern]{entry}
        The @code{grid-line-pattern} style property of type @code{:string}
        (Read) @br{}
        Dash pattern used to draw the tree view grid lines. @br{}
        Default value: \"\001\001\"
      @end{entry}
      @begin[grid-line-width]{entry}
        The @code{grid-line-width} style property of type @code{:int} (Read)
        @br{}
        Width, in pixels, of the tree view grid lines. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[horizontal-separator]{entry}
        The @code{horizontal-separator} style property of tpye @code{:int}
        (Read) @br{}
        Horizontal space between cells. Must be an even number. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
      @begin[indent-expanders]{entry}
        The @code{indent-expanders} style property of type @code{:boolean}
        (Read) @br{}
        Make the expanders indented. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[odd-row-color]{entry}
        The @code{odd-row-color} style property of type @class{gdk-color}
        (Read) @br{}
        Color to use for odd rows.
      @end{entry}
      @begin[tree-line-pattern]{entry}
        The @code{tree-line-pattern} style property of type @code{:string}
        (Read) @br{}
        Dash pattern used to draw the tree view lines. @br{}
        Default value: \"\001\001\"
      @end{entry}
      @begin[tree-line-width]{entry}
        The @code{tree-line-width} style property of type @code{:int} (Read)
        @br{}
        Width, in pixels, of the tree view lines. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[vertical-separator]{entry}
        The @code{vertical-separator} style property of type @code{:int} (Read)
        @br{}
        Vertical space between cells. Must be an even number. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"columns-changed\" signal}
      @begin{pre}
 lambda (view)    : Run Last
      @end{pre}
      The number of columns of the tree view has changed.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"cursor-changed\" signal}
      @begin{pre}
 lambda (view)    : Run Last
      @end{pre}
      The position of the cursor (focused cell) has changed.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"expand-collapse-cursor-row\" signal}
      @begin{pre}
 lambda (view arg1 arg2 arg3)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[arg1]{an undocumented boolean}
        @entry[arg2]{an undocumented boolean}
        @entry[arg3]{an undocumented boolean}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (view step direction)    : Action
      @end{pre}
      Keybinding signal which gets emitted when the user presses one of the
      cursor keys. Applications should not connect to it, but may emit it with
      the function @fun{g-signal-emit} if they need to control the cursor
      programmatically. In contrast to the functions
      @fun{gtk-tree-view-get-cursor} and @fun{gtk-tree-view-set-cursor-on-cell}
      when moving horizontally \"move-cursor\" does not reset the current
      selection.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk-movement-step} enumeration. The values
          @code{:logical-positions}, @code{:visual-positions},
          @code{:display-lines}, @code{:pages} and @code{:buffer-ends} are
          supported. The values @code{:logical-positions} and
          @code{:visual-positions} are treated identically.}
        @entry[direction]{An integer with the direction to move: +1 to move
          forwards; -1 to move backwards. The resulting movement is undefined
          for all other values.}
        @entry[Returns]{@em{True} if @arg{step} is supported, @em{false}
          otherwise.}
      @end{table}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
 lambda (view path column)    :action
      @end{pre}
      The signal is emitted when the @fun{gtk-tree-view-row-activated} function
      is called or the user double clicks a tree view row. It is also emitted
      when a non-editable row is selected and one of the @kbd{Space},
      @kbd{Shift+Space}, @kbd{Return} or @kbd{Enter} keys is pressed. For
      selection handling refer to the tree widget conceptual overview as well
      as the @class{gtk-tree-selection} API documentation.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[path]{The @class{gtk-tree-path} instance for the activated row.}
        @entry[column]{The @class{gtk-tree-view-column} object in which the
          activation occurred.}
      @end{table}
    @subheading{The \"row-collapsed\" signal}
      @begin{pre}
 lambda (view iter path)    : Run Last
      @end{pre}
      The given row has been collapsed (child nodes are hidden).
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk-tree-iter} iterator of the collapsed row.}
        @entry[path]{A @class{gtk-tree-path} instance that points to the row.}
      @end{table}
    @subheading{The \"row-expanded\" signal}
      @begin{pre}
 lambda (view iter path)    : Run Last
      @end{pre}
      The given row has been expanded (child nodes are shown).
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk-tree-iter} iterator of the expanded row.}
        @entry[path]{A @class{gtk-tree-path} instance that points to the row.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"select-cursor-parent\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"select-cursor-row\" signal}
      @begin{pre}
 lambda (view arg)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[arg]{an undocumented boolean}
      @end{table}
    @subheading{The \"start-interactive-search\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"test-collapse-row\" signal}
      @begin{pre}
 lambda (view iter path)    : Run Last
      @end{pre}
      The given row is about to be collapsed (hide its children nodes). Use
      this signal if you need to control the collapsibility of individual rows.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk-tree-iter} iterator of the row to
          collapsed.}
        @entry[path]{A @class{gtk-tree-path} instance that points to the row.}
        @entry[Returns]{@em{False} to allow collapsing, @em{true} to reject.}
      @end{table}
    @subheading{The \"test-expand-row\" signal}
      @begin{pre}
 lambda (view iter path)    : Run Last
      @end{pre}
      The given row is about to be expanded (show its children nodes). Use this
      signal if you need to control the expandability of individual rows.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
        @entry[iter]{The @class{gtk-tree-iter} iterator of the row to expand.}
        @entry[path]{A @class{gtk-tree-path} instance that points to the row.}
        @entry[Returns]{@em{False} to allow expansion, @em{true} to reject.}
      @end{table}
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      @begin[code]{table}
        @entry[view]{The @sym{gtk-tree-view} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tree-view-activate-on-single-click}
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
  @see-slot{gtk-tree-view-tooltip-column}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tree-view-activate-on-single-click ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activate-on-single-click"
                                               'gtk-tree-view) 't)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Specifies whether the \"row-activated\" signal will be emitted after a single
  click. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-activate-on-single-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-activate-on-single-click 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-activate-on-single-click object) => setting}
  @syntax[]{(setf (gtk-tree-view-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[setting]{a boolean that is @em{true} to emit the \"row-activated\"
    signal on a single click}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{activate-on-single-click} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  Cause the \"row-activated\" signal to be emitted on a single click instead of
  a double click.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-enable-grid-lines ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-grid-lines"
                                               'gtk-tree-view) 't)
 "The @code{enable-grid-lines} property of type
  @symbol{gtk-tree-view-grid-lines} (Read / Write) @br{}
  Whether grid lines should be drawn in the tree view. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-grid-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-grid-lines 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-enable-grid-lines object) => setting}
  @syntax[]{(setf (gtk-tree-view-enable-grid-lines object) setting)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[setting]{a boolean whether grid lines should be drawn}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{enable-grid-lines} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  Whether grid lines should be drawn in the tree view.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-enable-search --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-search"
                                               'gtk-tree-view) 't)
 "The @code{enable-search} property of type @code{:boolean} (Read / Write) @br{}
  View allows user to search through columns interactively. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-search atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-search 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-enable-search object) => enable-search}
  @syntax[]{(setf (gtk-tree-view-enable-search object) enable-search)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[enable-search]{@em{true}, if the user can search interactively}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{enable-search} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  If the @code{enable-search} property is set, then the user can type in text
  to search through the tree view interactively, this is sometimes called
  \"typeahead find\".

  Note that even if this is @em{false}, the user can still initiate a search
  using the \"start-interactive-search\" key binding.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-enable-tree-lines ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enable-tree-lines"
                                               'gtk-tree-view) 't)
 "The @code{enable-tree-lines} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether tree lines should be drawn in the tree view. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-enable-tree-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-enable-tree-lines 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-enable-tree-lines object) => enable-tree-lines}
  @syntax[]{(setf (gtk-tree-view-enable-tree-lines object) enable-tree-lines)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[enable-search]{@em{true}, to enable tree line drawing, @em{false}
    otherwise}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{enable-tree-lines} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-enable-tree-lines} returns
  whether or not tree lines are drawn in the tree view. The slot access
  function @sym{(setf gtk-tree-view-enable-tree-lines)} sets whether to draw
  lines. This does not have any visible effects for lists.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-expander-column  -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expander-column"
                                               'gtk-tree-view) 't)
 "The @code{expander-column} property of type @class{gtk-tree-view-column}
  (Read / Write) @br{}
  Set the column for the expander column.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-expander-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-expander-column 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-expander-column object) => column}
  @syntax[]{(setf (gtk-tree-view-expander-column object) column)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[column]{the column to draw the expander arrow at, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{expander-column} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-expander-column} returns the
  column that is the current expander column. The slot access function
  @sym{(setf gtk-tree-view-expander-column)} sets the column to draw the
  expander arrow at. It must be in the tree view. If @arg{column} is @code{nil},
  then the expander arrow is always at the first visible column.

  If you do not want expander arrow to appear in your tree, set the expander
  column to a hidden column.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-fixed-height-mode ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fixed-height-mode"
                                               'gtk-tree-view) 't)
 "The @code{fixed-height-mode} property of type @code{:boolean} (Read / Write)
  @br{}
  Setting the @code{fixed-height-mode} property to @em{true} speeds up the
  @sym{gtk-tree-view} widget by assuming that all rows have the same height.
  Only enable this option if all rows are the same height. Please see the
  function @fun{gtk-tree-view-fixed-height-mode} for more information on this
  option. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-fixed-height-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-fixed-height-mode 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-fixed-height-mode object) => enable}
  @syntax[]{(setf (gtk-tree-view-fixed-height-mode object) enable)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[enable]{@em{true} to enable fixed height mode}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{fixed-height-mode} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-fixed-height-mode} returns whether
  fixed height mode is turned on for the tree view. The slot access function
  @sym{(setf gtk-tree-view-fixed-height-mode)} enables or disables the fixed
  height mode.

  Fixed height mode speeds up the @class{gtk-tree-view} widget by assuming that
  all rows have the same height. Only enable this option if all rows are the
  same height and all columns have the value @code{:fixed} of the
  @symbol{gtk-tree-view-column-sizing} enumeration.
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-column-sizing}")

;;;  --- gtk-tree-view-headers-clickable ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "headers-clickable"
                                               'gtk-tree-view) 't)
 "The @code{headers-clickable} property of type @code{:boolean} (Read / Write)
  @br{}
  Column headers respond to click events. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-headers-clickable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-headers-clickable 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-headers-clickable object) => setting}
  @syntax[]{(setf (gtk-tree-view-headers-clickable object) setting)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[setting]{@em{true} if the columns are clickable}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{headers-clickable} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-headers-clickable} returns
  whether all header columns are clickable. The slot access function
  @sym{(setf gtk-tree-view-headers-clickable)} allow the column title buttons
  to be clicked.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-headers-visible ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "headers-visible"
                                               'gtk-tree-view) 't)
 "The @code{headers-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Show the column header buttons. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-headers-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-headers-visible 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-headers-visible object) => headers-visible}
  @syntax[]{(setf (gtk-tree-view-headers-visible object) headers-visible)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[headers-visible]{@em{true} if the headers are visible}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{headers-visible} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-headers-visible} returns @em{true}
  if the headers on the tree view are visible. The slot access function
  @sym{(setf gtk-tree-view-headers-visible)} sets the visibility state of the
  headers.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-hover-expand ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hover-expand"
                                               'gtk-tree-view) 't)
 "The @code{hover-expand} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the hover expansion mode of the tree view. Hover
  expansion makes rows expand or collapse if the pointer moves over them.
  This mode is primarily intended for tree views in popups, e.g. in
  @class{gtk-combo-box} or @class{gtk-entry-completion} widgets. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-hover-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-hover-expand 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-hover-expand object) => expand}
  @syntax[]{(setf (gtk-tree-view-hover-expand object) expand)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[expand]{@em{true} to enable hover selection mode}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{hover-expand} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-hover-expand} returns whether
  hover expansion mode is turned on for the tree view. The slot access function
  @sym{(setf gtk-tree-view-hover-expand)} enables or disables the hover
  expansion mode. Hover expansion makes rows expand or collapse if the pointer
  moves over them.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-hover-selection ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hover-selection"
                                               'gtk-tree-view) 't)
 "The @code{hover-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  Enables or disables the hover selection mode of the tree view. Hover
  selection makes the selected row follow the pointer. Currently, this works
  only for the selection modes @code{:single} and @code{:browse}. This mode is
  primarily intended for tree views in popups, e.g. in @class{gtk-combo-box}
  or @class{gtk-entry-completion} widgets. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-hover-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-hover-selection 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-hover-selection object) => setting}
  @syntax[]{(setf (gtk-tree-view-hover-selection object) setting)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[setting]{@em{true} to enable hover selection mode}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{hover-selection} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-hover-selection} returns whether
  hover selection mode is turned on for the tree view. The slot access function
  @sym{(setf gtk-tree-view-hover-selection)} enables or disables the hover
  selection mode. Hover selection makes the selected row follow the pointer.
  Currently, this works only for the selection modes @code{:single} and
  @code{:browse}.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-level-indentation ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "level-indentation"
                                               'gtk-tree-view) 't)
 "The @code{level-indentation} property of type @code{:int} (Read / Write) @br{}
  Extra indentation for each level. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-level-indentation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-level-indentation 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-level-indentation object) => indentation}
  @syntax[]{(setf (gtk-tree-view-level-indentation object) indentation)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[indentation]{an integer with the amount, in pixels, of extra
    indentation}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{level-indentation} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-level-indentation} returns the
  amount, in pixels, of extra indentation for child levels in the tree view in
  addition to the default indentation. The slot access function
  @sym{(setf gtk-tree-view-level-indentation)} sets the amount of extra
  indentation.

  The value should be specified in pixels, a value of 0 disables this feature
  and in this case only the default indentation will be used. This does not
  have any visible effects for lists.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-model ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-tree-view) 't)
 "The @code{model} property of type @class{gtk-tree-model} (Read / Write) @br{}
  The model for the tree view.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-model 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-model object) => model}
  @syntax[]{(setf (gtk-tree-view-model object) model)}
  @argument[objet]{a @class{gtk-tree-view} widget}
  @argument[model]{the @class{gtk-tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{model} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-model} returns the model the tree
  view is based on. Returns @code{nil} if the model is unset. The slot access
  function @sym{(setf gtk-tree-view-model)} sets the model. If the tree view
  already has a model set, it will remove it before setting the new model. If
  @arg{model} is @code{nil}, then it will unset the old model.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}")

;;; --- gtk-tree-view-reorderable ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reorderable"
                                               'gtk-tree-view) 't)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  View is reorderable. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-reorderable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-reorderable 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-reorderable object) => reorderable}
  @syntax[]{(setf (gtk-tree-view-reorderable object) rorderable)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[reorderable]{@em{true}, if the tree view can be reordered}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{reorderable} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  This function is a convenience function to allow you to reorder models that
  support the @class{gtk-tree-drag-source} and the @class{gtk-tree-drag-dest}
  interfaces.

  Both @class{gtk-tree-store} and @class{gtk-list-store} classes support these.
  If @arg{reorderable} is @em{true}, then the user can reorder the model by
  dragging and dropping rows. The developer can listen to these changes by
  connecting to the model's \"row-inserted\" and \"row-deleted\" signals. The
  reordering is implemented by setting up the tree view as a drag source and
  destination. Therefore, drag and drop can not be used in a reorderable view
  for any other purpose.

  This function does not give you any degree of control over the order - any
  reordering is allowed. If more control is needed, you should probably handle
  drag and drop manually.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-drag-source}
  @see-class{gtk-tree-drag-dest}
  @see-class{gtk-list-store}
  @see-class{gtk-tree-store}")

;;; --- gtk-tree-view-rubber-banding -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rubber-banding"
                                               'gtk-tree-view) 't)
 "The @code{rubber-banding} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to enable selection of multiple items by dragging the mouse
  pointer. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-rubber-banding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-rubber-banding 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-rubber-banding object) => enable}
  @syntax[]{(setf (gtk-tree-view-rubber-banding object) enable)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[enable]{@em{true}, to enable rubber banding}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{rubber-banding} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-rubber-banding} returns whether
  rubber banding is turned on for the tree view. The slot access function
  @sym{(setf gtk-tree-view-rubber-banding)} enables or disables rubber banding.
  If the selection mode is @code{:multiple}, rubber banding will allow the user
  to select multiple rows by dragging the mouse.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-rules-hint -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rules-hint"
                                               'gtk-tree-view) 't)
 "The @code{rules-hint} property of type @code{:boolean} (Read / Write) @br{}
  Set a hint to the theme engine to draw rows in alternating colors. @br{}
  @em{Warning:} The @code{rules-hint} property has been deprecated since version
  3.14 and should not be used in newly written code. The theme is responsible
  for drawing rows using zebra striping. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-rules-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-rules-hint 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-rules-hint object) => setting}
  @syntax[]{(setf (gtk-tree-view-rules-hint object) setting)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[setting]{@em{true} if the tree view requires reading across rows}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{rules-hint} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  This function tells GTK that the user interface for your application
  requires users to read across tree rows and associate cells with one another.
  By default, GTK will then render the tree view with alternating row colors.
  Do not use it just because you prefer the appearance of the ruled tree. That
  is a question for the theme. Some themes will draw tree rows in alternating
  colors even when rules are turned off, and users who prefer that appearance
  all the time can choose those themes. You should call this function only as a
  semantic hint to the theme engine that your tree view makes alternating colors
  useful from a functional standpoint, since it has lots of columns, generally.
  @begin[Warning]{dictionary}
    The function @sym{gtk-tree-view-rules-hint} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-search-column --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "search-column"
                                               'gtk-tree-view) 't)
 "The @code{search-column} property of type @code{:int} (Read / Write) @br{}
  Model column to search through during interactive search. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-search-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-search-column 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-search-column object) => column}
  @syntax[]{(setf (gtk-tree-view-search-column object) column)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[column]{an integer with the column of the model to search in, or
    -1 to disable searching}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{search-column} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-search-column} gets the column
  searched on by the interactive search code. The slot access function
  @sym{(setf gtk-tree-view-search-column)} sets @arg{column} as the column
  where the interactive search code should search in for the current model.

  If the search column is set, users can use the \"start-interactive-search\"
  key binding to bring up search popup. The @code{enable-search} property
  controls whether simply typing text will also start an interactive search.

  Note that @arg{column} refers to a column of the current model. The search
  column is reset to -1 when the model is changed.
  @see-class{gtk-tree-view}")

;;; --- gtk-tree-view-show-expanders -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-expanders"
                                               'gtk-tree-view) 't)
 "The @code{show-expanders} property of type @code{:boolean} (Read / Write)
  @br{}
  @em{True} if the view has expanders. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-show-expanders atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-show-expanders 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-show-expanders object) => enabled}
  @syntax[]{(setf (gtk-tree-view-show-expanders object) enabled)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[enabled]{@em{True} to enable expander drawing, @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{show-expanders} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-show-expanders} returns whether
  or not expanders are drawn in the tree view. The slot access function
  @sym{(setf gtk-tree-view-show-expanders)} sets whether to draw and enable
  expanders and indent child rows in the tree view. When disabled there will be
  no expanders visible in tree views and there will be no way to expand and
  collapse rows by default. Also note that hiding the expanders will disable
  the default indentation. You can set a custom indentation in this case using
  the function @fun{gtk-tree-view-level-indentation}. This does not have any
  visible effects for lists.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-level-indentation}")

;;; --- gtk-tree-view-tooltip-column -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip-column"
                                               'gtk-tree-view) 't)
 "The @code{tooltip-column} property of type @code{:int} (Read / Write)@br{}
  The column in the model containing the tooltip texts for the rows. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-tooltip-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-view-tooltip-column 'function)
 "@version{2021-2-25}
  @syntax[]{(gtk-tree-view-tooltip-column object) => column}
  @syntax[]{(setf (gtk-tree-view-tooltip-column object) column)}
  @argument[object]{a @class{gtk-tree-view} widget}
  @argument[column]{an integer which is a valid column number for tree
    view's model}
  @begin{short}
    Accessor of the @slot[gtk-tree-view]{tooltip-column} slot of the
    @class{gtk-tree-view} class.
  @end{short}

  The slot access function @sym{gtk-tree-view-tooltip-column} returns the column
  of the tree view's model which is being used for displaying tooltips on the
  tree view's rows.

  If you only plan to have simple (text-only) tooltips on full rows, you can
  use this function to have the @class{gtk-tree-view} widget handle these
  automatically for you. The argument @arg{column} should be set to the column
  in the tree view's model containing the tooltip texts, or -1 to disable this
  feature.

  When enabled, the @slot[gtk-widget]{has-tooltip} property will be set to
  @em{true} and the tree view will connect a \"query-tooltip\" signal handler.

  Note that the signal handler sets the text with the function
  @fun{gtk-tooltip-set-markup}, so &, <, etc have to be escaped in the text.
  @see-class{gtk-tree-view}
  @see-function{gtk-tooltip-set-markup}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new))

(defun gtk-tree-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-2-25}
  @return{A newly created @class{gtk-tree-view} widget.}
  @begin{short}
    Creates a new tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-new-with-model}"
  (make-instance 'gtk-tree-view))

(export 'gtk-tree-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new-with-model))

(defun gtk-tree-view-new-with-model (model)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-25}
  @argument[model]{the @class{gtk-tree-model} object}
  @return{A newly created @class{gtk-tree-view} widget.}
  @begin{short}
    Creates a new tree view with the model initialized to @arg{model}.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-view-new}"
  (make-instance 'gtk-tree-view
                 :model model))

(export 'gtk-tree-view-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_selection () -> gtk-tree-view-selection
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_selection" gtk-tree-view-selection)
    (g-object gtk-tree-selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-25}
  @argument[view]{a @class{gtk-tree-view} widget}
  @return{A @class{gtk-tree-selection} object.}
  @begin{short}
    Gets the tree selection associated with the tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-selection}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_tree_view_get_hadjustment (GtkTreeView *tree_view);
;;;
;;; Warning
;;;
;;; gtk_tree_view_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
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
;;; should not be used in newly written code. Use
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
;;; should not be used in newly written code. Use
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
;;; should not be used in newly written code. Use
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
;;; gtk_tree_view_columns_autosize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_columns_autosize" gtk-tree-view-columns-autosize) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-25}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{short}
    Resizes all columns to their optimal width.
  @end{short}
  Only works after the tree view has been realized.
  @see-class{gtk-tree-view}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-columns-autosize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_append_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_append_column" gtk-tree-view-append-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} object to add}
  @return{An integer with the number of columns in @arg{view} after appending.}
  @begin{short}
    Appends @arg{column} to the list of columns in the tree view.
  @end{short}
  If @arg{view} has fixed height mode enabled, then @arg{column} must have
  its @slot[gtk-tree-view-column]{sizing} property set to be @code{:fixed}.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-append-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_remove_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_remove_column" gtk-tree-view-remove-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} object to remove}
  @return{An integer with the number of columns in @arg{view} after removing.}
  @begin{short}
    Removes a column from the tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_insert_column" gtk-tree-view-insert-column) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} object to be inserted}
  @argument[position]{an integer with the position to insert @arg{column} in}
  @return{An integer with the number of columns in @arg{view} after insertion.}
  @begin{short}
    This inserts the column into the @arg{view} at the given position.
  @end{short}
  If @arg{position} is -1, then the column is inserted at the end. If
  @arg{view} has the @slot[gtk-tree-view]{fixed-height-mode} property enabled,
  then @arg{column} must have its @slot[gtk-tree-view-column]{sizing} property
  set to be @code{:fixed}.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-fixed-height-mode}
  @see-function{gtk-tree-view-column-sizing}"
  (view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column))
  (position :int))

(export 'gtk-tree-view-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_attributes ()
;;; ----------------------------------------------------------------------------

;; FIXME: The argument POSITION is not used.

(defun gtk-tree-view-insert-column-with-attributes (view
                                                    position
                                                    title
                                                    renderer &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[position]{an integer with the position to insert the new column in}
  @argument[title]{an string with the title to set the header to}
  @argument[renderer]{the @class{gtk-cell-renderer} object}
  @argument[attributes]{a list of attributes}
  @return{The number of columns in @arg{view} after insertion.}
  @begin{short}
    Creates a new @class{gtk-tree-view-column} object and inserts it into the
    tree view at @arg{position}.
  @end{short}
  If @arg{position} is -1, then the newly created column is inserted at the
  end. The column is initialized with the attributes given. If @arg{view} has
  the @slot[gtk-tree-view]{fixed-height-mode} property enabled, then the new
  column will have its @slot[gtk-tree-view-column]{sizing} property set to be
  @code{:fixed}.
  @see-class{gtk-tree-view}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-fixed-height-mode}
  @see-function{gtk-tree-view-column-sizing}"
  (declare (ignore position))
  (let ((column (gtk-tree-view-column-new)))
    (when (gtk-tree-view-fixed-height-mode view)
      (setf (gtk-tree-view-column-sizing column) :fixed))
    (setf (gtk-tree-view-column-title column) title)
    (gtk-tree-view-column-pack-start column renderer t)
    (gtk-tree-view-column-set-attributes column renderer attributes)))

(export 'gtk-tree-view-insert-column-with-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_data_func ()
;;; ----------------------------------------------------------------------------

;; FIXME: The argument POSITION is not used.

(defun gtk-tree-view-insert-column-with-data-func (view
                                                   position
                                                   title
                                                   renderer
                                                   func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[position]{an integer with the position to insert the new column in}
  @argument[title]{an string with the title to set the header to}
  @argument[renderer]{the @class{gtk-cell-renderer} object}
  @argument[func]{callback function to set attributes of cell renderer}
  @return{The number of columns in @arg{view} after insertion.}
  @begin{short}
    Convenience function that inserts a new column into the tree view with the
    given cell renderer and a @symbol{gtk-tree-cell-data-func} callback to set
    cell renderer attributes (normally using data from the model).
  @end{short}
  See also the functions @fun{gtk-tree-view-column-set-cell-data-func},
  @fun{gtk-tree-view-column-pack-start}. If @arg{view} has the
  @slot[gtk-tree-view]{fixed-height-mode} property enabled, then the new column
  will have its @slot[gtk-tree-view-column]{sizing} property set to be
  @code{:fixed}.
  @see-class{gtk-tree-view}
  @see-class{gtk-cell-renderer}
  @see-symbol{gtk-tree-cell-data-func}
  @see-function{gtk-tree-view-column-set-cell-data-func}
  @see-function{gtk-tree-view-column-pack-start}
  @see-function{gtk-tree-view-fixed-height-mode}
  @see-function{gtk-tree-view-column-sizing}"
  (declare (ignore position))
  (let ((column (gtk-tree-view-column-new)))
    (when (gtk-tree-view-fixed-height-mode view)
      (setf (gtk-tree-view-column-sizing column) :fixed))
    (setf (gtk-tree-view-column-title column) title)
    (gtk-tree-view-column-pack-start column renderer t)
    (gtk-tree-view-column-set-cell-data-func column renderer func)))

(export 'gtk-tree-view-insert-column-with-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_n_columns () -> gtk-tree-view-n-columns
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_n_columns" gtk-tree-view-n-columns) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @return{An integer with the number of columns in the tree view}
  @begin{short}
    Queries the number of columns in the given tree view.
  @end{short}
  @see-class{gtk-tree-view}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_column () -> gtk-tree-view-column
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_column" gtk-tree-view-column)
    (g-object gtk-tree-view-column)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[position]{an integer with the position of the column, counting
    from 0}
  @begin{return}
    The @class{gtk-tree-view-column} object, or @code{nil} if the position is
    outside the range of columns.
  @end{return}
  @begin{short}
    Gets the tree view column at the given position in the tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (view (g-object gtk-tree-view))
  (position :int))

(export 'gtk-tree-view-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_columns () -> gtk-tree-view-columns
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_columns" gtk-tree-view-columns)
    (g-list (g-object gtk-tree-view-column))
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @return{A list of @class{gtk-tree-view-column} objects.}
  @begin{short}
    Returns a list of all the tree view columns currently in the tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_move_column_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_move_column_after" gtk-tree-view-move-column-after)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[column]{the @class{gtk-tree-view-column} object to be moved}
  @argument[base]{the @class{gtk-tree-view-column} object to be moved relative
    to, or @code{nil}}
  @begin{short}
    Moves @arg{column} to be after to @arg{base}.
  @end{short}
  If @arg{base} is @code{nil}, then @arg{column} is placed in the first
  position.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column))
  (base (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-move-column-after)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumnDropFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-column-drop-func :boolean
    ((view (g-object gtk-tree-view))
     (column (g-object gtk-tree-view-column))
     (prev (g-object gtk-tree-view-column))
     (next (g-object gtk-tree-view-column))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn view column prev next)
      (return () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-column-drop-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-view-column-drop-func atdoc:*external-symbols*)
 "@version{2021-2-26}
  @begin{short}
    Callback function type for determining whether @arg{column} can be dropped
    in a particular spot as determined by @arg{prev} and @arg{next}.
  @end{short}
  In left to right locales, @arg{prev} is on the left of the potential drop
  spot, and @arg{next} is on the right. In right to left mode, this is reversed.
  This callback function should return @em{true} if the spot is a valid drop
  spot. Please note that returning @em{true} does not actually indicate that
  the column drop was made, but is meant only to indicate a possible drop spot
  to the user.
  @begin{pre}
 lambda (view column prev next)
  @end{pre}
  @begin[code]{table}
    @entry[view]{A @class{gtk-tree-view} widget.}
    @entry[column]{The @class{gtk-tree-view-column} object being dragged.}
    @entry[prev]{A @class{gtk-tree-view-column} object on one side of
      @arg{column}.}
    @entry[next]{A @class{gtk-tree-view-column} object on the other side of
      @arg{column}.}
    @entry[Return]{@em{True}, if @arg{column} can be dropped in this spot.}
  @end{table}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}")

(export 'gtk-tree-view-column-drop-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_column_drag_function ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_column_drag_function"
          %gtk-tree-view-set-column-drag-function) :void
  (view (g-object gtk-tree-view))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-column-drag-function (view func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[func]{a callback function to determine which columns are
    reorderable, or @code{nil}}
  @begin{short}
    Sets a user callback function of type
    @symbol{gtk-tree-view-column-drop-func} for determining where a column may
    be dropped when dragged.
  @end{short}
  This function is called on every column pair in turn at the beginning of a
  column drag to determine where a drop can take place. The arguments passed to
  @arg{func} are: the @class{gtk-tree-view} widget, the
  @class{gtk-tree-view-column} object being dragged, and the two
  @class{gtk-tree-view-column} objects determining the drop spot. If either of
  the @class{gtk-tree-view-column} arguments for the drop spot are @code{nil},
  then they indicate an edge. If @arg{func} is set to be @code{nil}, then
  @arg{view} reverts to the default behavior of allowing all columns to be
  dropped everywhere.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}"
  (%gtk-tree-view-set-column-drag-function
                                   view
                                   (callback gtk-tree-view-column-drop-func)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-set-column-drag-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_point" gtk-tree-view-scroll-to-point) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[tx]{an integer with the x coordinate of new top-left pixel
    of visible area, or -1}
  @argument[ty]{an integer with the y coordinate of new top-left pixel
    of visible area, or -1}
  @begin{short}
    Scrolls the tree view such that the top-left corner of the visible area is
    @arg{tx}, @arg{ty}, where @arg{tx} and @arg{ty} are specified in tree
    coordinates.
  @end{short}
  The tree view must be realized before this function is called. If it is
  not, you probably want to be using the function
  @fun{gtk-tree-view-scroll-to-cell}.

  If either @arg{tx} or @arg{ty} are -1, then that direction is not scrolled.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-scroll-to-cell}"
  (view (g-object gtk-tree-view))
  (tx :int)
  (ty :int))

(export 'gtk-tree-view-scroll-to-point)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_cell" %gtk-tree-view-scroll-to-cell) :void
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun gtk-tree-view-scroll-to-cell (view path column
                                     &optional
                                     (row-align 0.5 row-align-supplied-p)
                                     (col-align 0.5 col-align-supplied-p))
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{the @class{gtk-tree-path} instance of the row to move to, or
    @code{nil}}
  @argument[column]{the @class{gtk-tree-view-column} object to move horizontally
    to, or @code{nil}}
  @argument[row-align]{an optional float with the vertical alignment of the row
    specified by path, the default is 0.5}
  @argument[col-align]{an optional float with the horizontal alignment of the
    column specified by column, the default is 0.5}
  @begin{short}
    Moves the alignments of the tree view to the position specified by
    @arg{column} and @arg{path}.
  @end{short}
  If @arg{column} is @code{nil}, then no horizontal scrolling occurs. Likewise,
  if @arg{path} is @code{nil} no vertical scrolling occurs. At a minimum, one
  of @arg{column} or @arg{path} need to be non-@code{nil}. The argument
  @arg{row-align} determines where the row is placed, and the argument
  @arg{col-align} determines where @arg{column} is placed. Both are expected to
  be between 0.0 and 1.0. 0.0 means left/top alignment, 1.0 means right/bottom
  alignment, 0.5 means center.

  If the cell is currently visible on the screen, nothing is done.

  This function only works if the model is set, and path is a valid row on the
  model. If the model changes before the tree view is realized, the centered
  path will be modified to reflect this change.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}"
  (%gtk-tree-view-scroll-to-cell view
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
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (focus (g-object gtk-tree-view-column))
  (start :boolean))

(defun gtk-tree-view-set-cursor (view path &key focus start)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[focus]{a @class{gtk-tree-view-column} object, or @code{nil}}
  @argument[start]{@em{true} if the specified cell should start being
    edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If the argument @arg{focus} is not @code{nil}, then focus is given to
  the column specified by it. Additionally, if @arg{focus} is specified, and
  @arg{start} is @em{true}, then editing should be started in the specified
  cell. This function is often followed by the function
  @fun{gtk-widget-grab-focus} in order to give keyboard focus to the widget.
  Please note that editing can only happen when the tree view is realized.

  If @arg{path} is invalid for the model, the current cursor (if any) will be
  unset and the function will return without failing.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-widget-grab-focus}"
  (%gtk-tree-view-set-cursor view path focus start))

(export 'gtk-tree-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor_on_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_cursor_on_cell" %gtk-tree-view-set-cursor-on-cell)
    :void
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (focus (g-object gtk-tree-view-column))
  (cell (g-object gtk-cell-renderer))
  (start :boolean))

(defun gtk-tree-view-set-cursor-on-cell (view path &key focus cell start)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[focus]{a @class{gtk-tree-view-column}, or @code{nil}}
  @argument[cell]{a @class{gtk-cell-renderer}, or @code{nil}}
  @argument[start]{@em{true} if the specified cell should start being edited}
  @begin{short}
    Sets the current keyboard focus to be at @arg{path}, and selects it.
  @end{short}
  This is useful when you want to focus the user's attention on a particular
  row. If the argument @arg{focus} is not @code{nil}, then focus is given to
  the column specified by it. If @arg{focus} and @arg{cell} are not @code{nil},
  and @arg{focus} contains 2 or more editable or activatable cells, then focus
  is given to the cell specified by @arg{cell}. Additionally, if @arg{focus}
  is specified, and @arg{start} is @em{true}, then editing should be started in
  the specified cell. This function is often followed by the function
  @fun{gtk-widget-grab-focus} in order to give keyboard focus to the widget.
  Please note that editing can only happen when the tree view is realized.

  If @arg{path} is invalid for the model, the current cursor (if any) will be
  unset and the function will return without failing.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-widget-grab-focus}"
  (%gtk-tree-view-set-cursor-on-cell view path focus cell start))

(export 'gtk-tree-view-set-cursor-on-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cursor" %gtk-tree-view-get-cursor) :void
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (focus (g-object gtk-tree-view-column)))

(defun gtk-tree-view-get-cursor (view)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{path} -- the current @class{gtk-tree-path} cursor path, or @code{nil}
      @br{}
    @code{focus} -- the current @class{gtk-tree-view-column} focus column,
      or @code{nil}
  @end{return}
  @begin{short}
    Returns @arg{path} and @arg{focus} with the current path and focus column.
  @end{short}
  If the cursor is not currently set, then @arg{path} will be @code{nil}. If no
  column currently has focus, then @arg{focus} will be @code{nil}.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}"
  (with-foreign-objects ((path :pointer) (focus :pointer))
    (%gtk-tree-view-get-cursor view path focus)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref focus '(g-object gtk-tree-view-column)))))

(export 'gtk-tree-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_activated ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_activated" gtk-tree-view-row-activated) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{the @class{gtk-tree-path} instance to be activated}
  @argument[column]{the @class{gtk-tree-view-column} object to be activated}
  @begin{short}
    Activates the cell determined by @arg{path} and @arg{column}.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-path}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-row-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_all" gtk-tree-view-expand-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{short}
    Recursively expands all nodes in the tree view.
  @end{short}
  @see-class{gtk-tree-view}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-expand-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_all" gtk-tree-view-collapse-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{short}
    Recursively collapses all visible, expanded nodes in the tree view.
  @end{short}
  @see-class{gtk-tree-view}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-collapse-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_to_path" gtk-tree-view-expand-to-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance to a row}
  @begin{short}
    Expands the row at @arg{path}.
  @end{short}
  This will also expand all parent rows of @arg{path} as necessary.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (view (g-object gtk-tree-path))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-expand-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_row" gtk-tree-view-expand-row) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance to a row}
  @argument[all]{a boolean whether to recursively expand, or just expand
    immediate children}
  @return{@em{True} if the row existed and had children.}
  @begin{short}
    Opens the row so its children are visible.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (all :boolean))

(export 'gtk-tree-view-expand-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_row" gtk-tree-view-collapse-row) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance to a row in the tree view}
  @return{@em{True} if the row was collapsed.}
  @begin{short}
    Collapses a row (hides its child rows, if they exist).
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-collapse-row)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewMappingFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-mapping-func :void
    ((view (g-object gkt-tree-view))
     (path (g-boxed-foreign gtk-tree-path))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn view path)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-mapping-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-view-mapping-func atdoc:*external-symbols*)
 "@version{2021-2-26}
  @begin{short}
    Callback function used for the function
    @fun{gtk-tree-view-map-expanded-rows}.
  @end{short}
  @begin{pre}
 lambda (view path)
  @end{pre}
  @begin[code]{table}
    @entry[view]{A @class{gtk-tree-view} widget.}
    @entry[path]{The @class{gtk-tree-path} instance that is expanded.}
  @end{table}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-view-map-expanded-rows}")

(export 'gtk-tree-view-mapping-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_map_expanded_rows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_map_expanded_rows" %gtk-tree-view-map-expanded-rows)
    :void
  (view (g-object gtk-tree-view))
  (func :pointer)
  (data :pointer))

(defun gtk-tree-view-map-expanded-rows (view func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[func]{a callback function of type
    @symbol{gtk-tree-view-mapping-func}  to be called}
  @begin{short}
    Calls @arg{func} on all expanded rows.
  @end{short}
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-mapping-func}"
  (with-stable-pointer (ptr func)
    (%gtk-tree-view-map-expanded-rows view
                                      (callback gtk-tree-view-mapping-func)
                                      ptr)))

(export 'gtk-tree-view-map-expanded-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_expanded ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_expanded" gtk-tree-view-row-expanded) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance to test expansion state}
  @return{@em{True} if @arg{path} is expanded.}
  @begin{short}
    Returns @em{true} if the node pointed to by @arg{path} is expanded in the
    tree view.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-row-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_path_at_pos () -> gtk-tree-view-path-at-pos
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_path_at_pos" %gtk-tree-view-path-at-pos) :boolean
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-path-at-pos (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x position to be identified (relative to
    the bin window)}
  @argument[y]{an integer with the y position to be identified (relative to
    the bin window)}
  @begin{return}
    @code{path} -- a @class{gtk-tree-path} instance, or @code{nil} @br{}
    @code{column} -- a @class{gtk-tree-view-column} object, or @code{nil} @br{}
    @code{cell-x} -- an integer with the x coordinate relative to the cell @br{}
    @code{cell-y} -- an integer with the y coordinate relative to the cell
  @end{return}
  @begin{short}
    Finds the path at the point (@arg{x}, @arg{y}), relative to the bin window
    coordinates.
  @end{short}
  Please see the function @fun{gtk-tree-view-bin-window}. That is, @arg{x} and
  @arg{y} are relative to an events coordinates. The @arg{x} and @arg{y} must
  coordinates come from an event on the tree view only where the event window
  from the function @fun{gdk-event-window} is equal to the bin window from the
  function @fun{gtk-tree-view-bin-window}. It is primarily for things like
  popup menus.

  If @arg{path} is non-@code{nil}, then it will be filled with the
  @class{gtk-tree-path} instance at that point. If @arg{column} is
  non-@code{nil}, then it will be filled with the column at that point. The
  values @arg{cell-x} and @arg{cell-y} return the coordinates relative to the
  cell background, i.e. the @code{background-area} passed to the function
  @fun{gtk-cell-renderer-render}. This function is only meaningful if the tree
  view is realized. Therefore this function will always return @code{nil} if
  the tree view is not realized or does not have a model.

  For converting widget coordinates, e.g. the ones you get from the
  \"query-tooltip\" signal, please see the function
  @fun{gtk-tree-view-convert-widget-to-bin-window-coords}.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-function{gdk-event-window}
  @see-function{gtk-cell-renderer-render}
  @see-function{gtk-tree-view-bin-window}
  @see-function{gtk-tree-view-convert-widget-to-bin-window-coords}"
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-path-at-pos view
                                      x y
                                      path
                                      column
                                      cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref column '(g-object gtk-tree-view-column))
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'gtk-tree-view-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_blank_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_blank_at_pos" %gtk-tree-view-is-blank-at-pos)
    :boolean
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-is-blank-at-pos (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x position to be identified (relative to
    the bin window)}
  @argument[y]{an integer with the y position to be identified (relative to
    the bin window)}
  @begin{return}
    @code{path} -- a @class{gtk-tree-path} instance, or @code{nil} @br{}
    @code{column} -- a @class{gtk-tree-view-column} object, or @code{nil} @br{}
    @code{cell-x} -- an integer with the x coordinate relative to the cell,
      or @code{nil} @br{}
    @code{cell-y} -- an integer where the y coordinate relative to the cell,
      or @code{nil}
  @end{return}
  @begin{short}
    Determine whether the point (@arg{x}, @arg{y}) in the tree view is blank,
    that is no cell content nor an expander arrow is drawn at the location.
  @end{short}
  If so, the location can be considered as the background. You might wish to
  take special action on clicks on the background, such as clearing a current
  selection, having a custom context menu or starting rubber banding.

  The @arg{x} and @arg{y} coordinates that are provided must be relative to the
  bin window coordinates. That is, @arg{x} and @arg{y} must come from an event
  on the tree view where the window from the function @fun{gdk-event-window}
  is equal to the window from the function @fun{gtk-tree-view-bin-window}.

  For converting widget coordinates, e.g. the ones you get from the
  \"query-tooltip\" signal, please see the function
  @fun{gtk-tree-view-convert-widget-to-bin-window-coords}.

  The @arg{path}, @arg{column}, @arg{cell-x} and @arg{cell-y} arguments will
  be returned likewise as for the function @fun{gtk-tree-view-path-at-pos}.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-function{gdk-event-window}
  @see-function{gtk-tree-view-bin-window}
  @see-function{gtk-tree-view-path-at-pos}
  @see-function{gtk-tree-view-convert-widget-to-bin-window-coords}"
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-is-blank-at-pos view
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
;;; gtk_tree_view_get_cell_area () -> gtk-tree-view-cell-area
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cell_area" %gtk-tree-view-cell-area) :void
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-cell-area (view path column)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance for the row, or @code{nil}
    to get only horizontal coordinates}
  @argument[column]{a @class{gtk-tree-view-column} object for the column, or
    @code{nil} to get only vertical coordinates}
  @return{A @class{gdk-rectangle} cell rectangle.}
  @begin{short}
    Returns the bounding rectangle in bin window coordinates for the cell at
    the row specified by @arg{path} and the column specified by @arg{column}.
  @end{short}
  If @arg{path} is @code{nil}, or points to a path not currently displayed, the
  @arg{y} and @arg{height} fields of the rectangle will be filled with 0. If
  @arg{column} is @code{nil}, the @arg{x} and @arg{width} fields will be filled
  with 0. The sum of all cell rectangles does not cover the entire tree. There
  are extra pixels in between rows, for example. The returned rectangle is
  equivalent to the @arg{cell-area} passed to the function
  @fun{gtk-cell-renderer-render}. This function is only valid if the tree view
  is realized.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-class{gdk-rectangle}
  @see-function{gtk-cell-renderer-render}"
  (let ((rect (gdk-rectangle-new)))
    (%gtk-tree-view-cell-area view path column rect)
    rect))

(export 'gtk-tree-view-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_background_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_background_area" %gtk-tree-view-background-area)
    :void
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-background-area (view path column)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance for the row, or @code{nil}
    to get only horizontal coordinates}
  @argument[column]{a @class{gtk-tree-view-column} object for the column, or
    @code{nil} to get only vertical coordiantes}
  @return{A @class{gdk-rectangle} instance with the cell background rectangle.}
  @begin{short}
    Returns the bounding rectangle in the bin window coordinates for the cell at
    the row specified by @arg{path} and the column specified by @arg{column}.
  @end{short}
  If @arg{path} is @code{nil}, or points to a node not found in the tree, the
  @arg{y} and @arg{height} fields of the rectangle will be filled with 0. If
  @arg{column} is @code{nil}, the @arg{x} and @arg{width} fields will be filled
  with 0. The returned rectangle is equivalent to the @code{background-area}
  passed to the function @fun{gtk-cell-renderer-render}. These background areas
  tile to cover the entire bin window. Contrast with the @code{cell-area},
  returned by the function @fun{gtk-tree-view-cell-area}, which returns only
  the cell itself, excluding surrounding borders and the tree expander area.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-view-column}
  @see-class{gdk-rectangle}
  @see-function{gtk-cell-renderer-render}
  @see-function{gtk-tree-view-cell-area}"
  (let ((rect (gdk-rectangle-new)))
    (%gtk-tree-view-background-area view path column rect)
    rect))

(export 'gtk-tree-view-background-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_rect () -> gtk-tree-view-visible-rect
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_rect" %gtk-tree-view-visible-rect) :void
  (view (g-object gtk-tree-view))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-visible-rect (view)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @return{A @class{gdk-rectangle} instance.}
  @begin{short}
    Returns the rectangle with the currently visible region of the tree view
    buffer, in tree view coordinates.
  @end{short}
  Convert to the bin window coordinates with the function
  @fun{gtk-tree-view-convert-tree-to-bin-window-coords}. Tree coordinates start
  at 0,0 for the first row of the tree view, and cover the entire scrollable
  area of the tree view.
  @see-class{gtk-tree-view}
  @see-class{gdk-rectangle}"
  (let ((rect (gdk-rectangle-new)))
    (%gtk-tree-view-visible-rect view rect)
    rect))

(export 'gtk-tree-view-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_range () -> gtk-tree-view-visible-range
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_range" %gtk-tree-view-visible-range)
    :boolean
  (view (g-object gtk-tree-view))
  (start :pointer)
  (end :pointer))

(defun gtk-tree-view-visible-range (view)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{start} -- a @class{gtk-tree-path} instance with the start of region,
      or @code{nil} @br{}
    @code{end} -- a @class{gtk-tree-path} instance with the end of region,
      or @code{nil}
  @end{return}
  @begin{short}
    Returns @arg{start} and @arg{end} to be the first and last visible path.
  @end{short}
  Note that there may be invisible paths in between.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}"
  (with-foreign-objects ((start :pointer) (end :pointer))
    (when (%gtk-tree-view-visible-range view start end)
      (values (mem-ref start '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref end '(g-boxed-foreign gtk-tree-path :return))))))

(export 'gtk-tree-view-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_bin_window () -> gtk-tree-view-bin-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_bin_window" gtk-tree-view-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @return{A @class{gdk-window} object, or @code{nil} when @arg{view}
    has not been realized yet.}
  @begin{short}
    Returns the window that the tree view renders to.
  @end{short}
  This is used primarily to compare to the window from the function
  @fun{gdk-event-window} to confirm that the event on the tree view is on the
  right window.
  @see-class{gtk-tree-view}
  @see-class{gdk-window}
  @see-function{gdk-event-window}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_tree_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_tree_coords"
          %gtk-tree-view-convert-bin-window-to-tree-coords) :void
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (tx :pointer)
  (ty :pointer))

(defun gtk-tree-view-convert-bin-window-to-tree-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to bin window}
  @argument[y]{an integer with the y coordinate relative to bin window}
  @begin{return}
    @code{tx} -- an integer with the tree x coordinate @br{}
    @code{ty} -- an integer with the tree y coordinate
  @end{return}
  @begin{short}
    Converts bin window coordinates to coordinates for the tree view (the full
    scrollable area of the tree view).
  @end{short}
  @see-class{gtk-tree-view}"
  (with-foreign-objects ((tx :int) (ty :int))
    (%gtk-tree-view-convert-bin-window-to-tree-coords view x y tx ty)
    (values (mem-ref tx :int)
            (mem-ref ty :int))))

(export 'gtk-tree-view-convert-bin-window-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_widget_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_widget_coords"
          %gtk-tree-view-convert-bin-window-to-widget-coords) :void
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (wx :pointer)
  (wy :pointer))

(defun gtk-tree-view-convert-bin-window-to-widget-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the bin window x coordinate}
  @argument[y]{an integer with the bin window y coordinate}
  @begin{return}
    @code{wx} -- an integer with the widget x coordinate @br{}
    @code{wy} -- an integer with the widget y coordinate
  @end{return}
  @begin{short}
    Converts bin window coordinates, see the function
    @fun{gtk-tree-view-bin-window}, to widget relative coordinates.
  @end{short}
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-bin-window}"
  (with-foreign-objects ((wx :int) (wy :int))
    (%gtk-tree-view-convert-bin-window-to-widget-coords view x y wx wy)
    (values (mem-ref wx :int)
            (mem-ref wy :int))))

(export 'gtk-tree-view-convert-bin-window-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_bin_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_bin_window_coords"
          %gtk-tree-view-convert-tree-to-bin-window-coords) :void
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (bx :pointer)
  (by :pointer))

(defun gtk-tree-view-convert-tree-to-bin-window-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the tree x coordinate}
  @argument[y]{an integer with the tree y coordinate}
  @begin{return}
    @code{bx} -- an integer with the x coordinate relative to bin window @br{}
    @code{by} -- an integer with the y coordinate relative to bin window
  @end{return}
  @begin{short}
    Converts tree view coordinates, coordinates in full scrollable area of the
    tree view, to bin window coordinates.
  @end{short}
  @see-class{gtk-tree-view}"
  (with-foreign-objects ((bx :int) (by :int))
    (%gtk-tree-view-convert-tree-to-bin-window-coords view x y bx by)
    (values (mem-ref bx :int)
            (mem-ref by :int))))

(export 'gtk-tree-view-convert-tree-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_widget_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_widget_coords"
          %gtk-tree-view-convert-tree-to-widget-coords) :void
  (view g-object)
  (x :int)
  (y :int)
  (wx :pointer)
  (wy :pointer))

(defun gtk-tree-view-convert-tree-to-widget-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the tree view}
  @argument[y]{an integer with the y coordinate relative to the tree view}
  @begin{return}
    @code{wx} -- an integer with the widget x coordinate @br{}
    @code{wy} -- an integer with the widget y coordinate
  @end{return}
  @begin{short}
    Converts tree view coordinates, coordinates in full scrollable area of the
    tree, to widget coordinates.
  @end{short}
  @see-class{gtk-tree-view}"
  (with-foreign-objects ((wx :int) (wy :int))
    (%gtk-tree-view-convert-tree-to-widget-coords view x y wx wy)
    (values (mem-ref wx :int)
            (mem-ref wy :int))))

(export 'gtk-tree-view-convert-tree-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_bin_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_bin_window_coords"
          %gtk-tree-view-convert-widget-to-bin-window-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (bx :pointer)
  (by :pointer))

(defun gtk-tree-view-convert-widget-to-bin-window-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the widget}
  @argument[y]{an integer with the y coordinate relative to the widget}
  @begin{return}
    @code{bx} -- an integer with the bin window x coordinate @br{}
    @code{by} -- an integer with the bin window y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the bin window, see the
    function @fun{gtk-tree-view-bin-window}.
  @end{short}
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-bin-window}"
  (with-foreign-objects ((bx :int) (by :int))
    (%gtk-tree-view-convert-widget-to-bin-window-coords view x y bx by)
    (values (mem-ref bx :int)
            (mem-ref by :int))))

(export 'gtk-tree-view-convert-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_tree_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_tree_coords"
          %gtk-tree-view-convert-widget-to-tree-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (tx :pointer)
  (ty :pointer))

(defun gtk-tree-view-convert-widget-to-tree-coords (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the x coordinate relative to the widget}
  @argument[y]{an integer with the y coordinate relative to the widget}
  @begin{return}
    @code{tx} -- an integer with the tree view x coordinate @br{}
    @code{ty} -- an integer with the tree view y coordinate
  @end{return}
  @begin{short}
    Converts widget coordinates to coordinates for the tree view, the full
    scrollable area of the tree view.
  @end{short}
  @see-class{gtk-tree-view}"
  (with-foreign-objects ((tx :int) (ty :int))
    (%gtk-tree-view-convert-widget-to-tree-coords view x y tx ty)
    (values (mem-ref tx :int)
            (mem-ref ty :int))))

(export 'gtk-tree-view-convert-widget-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_dest"
          %gtk-tree-view-enable-model-drag-dest) :void
  (view (g-object gtk-tree-view))
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-dest (view targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-2}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[targets]{a list of target entries that the drag will support}
  @argument[actions]{the @symbol{gdk-drag-action} bitmask of possible actions
    for a drag from this widget}
  @begin{short}
    Turns the tree view into a drop destination for automatic DND.
  @end{short}
  Calling this method sets \"reorderable\" to the @code{false} value.
  @see-class{gtk-tree-view}
  @see-symbol{gdk-drag-action}
  @see-function{gtk-tree-view-reorderable}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr '(:struct %gtk-target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (mem-aptr targets-ptr
                                       '(:struct %gtk-target-entry) i)
            for entry = (pop targets)
            do (with-foreign-slots ((target flags info)
                                    target-ptr
                                    (:struct %gtk-target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%gtk-tree-view-enable-model-drag-dest view
                                             targets-ptr
                                             n-targets
                                             actions))))

(export 'gtk-tree-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_source"
          %gtk-tree-view-enable-model-drag-source) :void
  (view (g-object gtk-tree-view))
  (mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-source (view mask targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-2}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[mask]{a @symbol{gdk-modifier-type} mask of allowed buttons to start
    drag}
  @argument[targets]{a list of target entries that the drag will support}
  @argument[actions]{the @symbol{gdk-drag-action} bitmask of possible actions
    for a drag from this widget}
  @begin{short}
    Turns the tree view into a drag source for automatic DND.
  @end{short}
  Calling this method sets the @slot[gtk-tree-view]{reorderable} property to
  the @code{false} value.
  @see-class{gtk-tree-view}
  @see-symbol{gdk-modifier-type}
  @see-symbol{gdk-drag-action}
  @see-function{gtk-tree-view-reorderable}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr '(:struct %gtk-target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (mem-aptr targets-ptr
                                       '(:struct %gtk-target-entry) i)
            for entry = (pop targets)
            do (with-foreign-slots ((target flags info)
                                    target-ptr
                                    (:struct %gtk-target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%gtk-tree-view-enable-model-drag-source view
                                               mask
                                               targets-ptr
                                               n-targets
                                               actions))))

(export 'gtk-tree-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_unset_rows_drag_source"
           gtk-tree-view-unset-rows-drag-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{short}
    Undoes the effect of the function
    @fun{gtk-tree-view-enable-model-drag-source}.
  @end{short}
  Calling this method sets the @slot[gtk-tree-view]{reorderable} property to
  @em{false}.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-enable-model-drag-source}
  @see-function{gtk-tree-view-reorderable}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-unset-rows-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_unset_rows_drag_dest"
           gtk-tree-view-unset-rows-drag-dest) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{short}
    Undoes the effect of the function
    @fun{gtk-tree-view-enable-model-drag-dest}.
  @end{short}
  Calling this method sets the @slot[gtk-tree-view]{reorderable} property to
  @em{false}.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-enable-model-drag-dest}
  @see-function{gtk-tree-view-reorderable}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-unset-rows-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_drag_dest_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_drag_dest_row" gtk-tree-view-set-drag-dest-row)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{the @class{gtk-tree-path} instance of the row to highlight,
    or @code{nil}}
  @argument[pos]{a @symbol{gtk-tree-view-drop-position} value wich specifies
    whether to drop before, after or into the row}
  @begin{short}
    Sets the row that is highlighted for feedback.
  @end{short}
  If @arg{path} is @code{nil}, an existing highlight is removed.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-tree-view-drop-position}
  @see-function{gtk-tree-view-get-drag-dest-row}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (pos gtk-tree-view-drop-position))

(export 'gtk-tree-view-set-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_drag_dest_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_drag_dest_row" %gtk-tree-view-get-drag-dest-row)
    :void
  (view (g-object gtk-tree-view))
  (path :pointer)
  (pos :pointer))

(defun gtk-tree-view-get-drag-dest-row (view)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{path} -- the @class{gtk-tree-path} instance of the highlighted row,
      or @code{nil} @br{}
    @code{pos}  -- the @symbol{gtk-tree-view-drop-position} position, or
      @code{nil}
  @end{return}
  @begin{short}
    Gets information about the row that is highlighted for feedback.
  @end{short}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-tree-view-drop-position}
  @see-function{gtk-tree-view-set-drag-dest-row}"
  (with-foreign-objects ((path :pointer) (pos :pointer))
    (%gtk-tree-view-get-drag-dest-row view path pos)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref pos 'gtk-tree-view-drop-position))))

(export 'gtk-tree-view-get-drag-dest-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_dest_row_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_dest_row_at_pos"
          %gtk-tree-view-get-dest-row-at-pos) :boolean
  (view (g-object gtk-tree-view))
  (drag-x :int)
  (drag-y :int)
  (path :pointer)
  (pos :pointer))

(defun gtk-tree-view-get-dest-row-at-pos (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[x]{an integer with the position to determine the destination row
    for}
  @argument[y]{an integer with the position to determine the destination row
    for}
  @begin{return}
    @code{path} -- the @class{gtk-tree-path} instance of the highlighted row,
      or @code{nil} @br{}
    @code{pos}  -- the @symbol{gtk-tree-view-drop-position} position,
      or @code{nil}
  @end{return}
  @begin{short}
    Determines the destination row for a given position.
  @end{short}
  The arguments @arg{x} and @arg{y} are expected to be in widget coordinates.
  This function is only meaningful if the tree view is realized. Therefore this
  function will always return @code{nil} if @arg{tree-view} is not realized or
  does not have a model.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{gtk-tree-view-drop-position}"
  (with-foreign-objects ((path :pointer) (pos :int))
    (when (%gtk-tree-view-get-dest-row-at-pos view x y path pos)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref pos 'gtk-tree-view-drop-position)))))

(export 'gtk-tree-view-get-dest-row-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_create_row_drag_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_create_row_drag_icon"
           gtk-tree-view-create-row-drag-icon)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}

  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{A newly allocated @symbol{cairo-surface-t} instance of the drag icon.}
  @begin{short}
    Creates a @symbol{cairo-surface-t} representation of the row at path.
  @end{short}
  This image is used for a drag icon.
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-path}
  @see-symbol{cairo-surface-t}"
  (view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-create-row-drag-icon)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchEqualFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-equal-func :boolean
  ((model (g-object gtk-tree-model))
   (column :int)
   (key (:string :free-from-foreign nil))
   (iter (g-boxed-foreign gtk-tree-iter))
   (data :pointer))
  (restart-case
    (funcall (get-stable-pointer-value data) model column key iter)
    (return-true () t)
    (return-false () t)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-search-equal-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-view-search-equal-func atdoc:*external-symbols*)
 "@version{2021-2-26}
  @begin{short}
    A callback function used for checking whether a row in model matches a
    search key string entered by the user.
  @end{short}
  Note the return value is reversed from what you would normally expect.
  @begin{pre}
 lambda (model column key iter data)
  @end{pre}
  @begin[code]{table}
    @entry[model]{A @class{gtk-tree-model} object being searched.}
    @entry[column]{an integer with the search column set by the function
      @fun{gtk-tree-view-search-column}}
    @entry[key]{the key string to compare with}
    @entry[iter]{a @class{gtk-tree-iter} iterator pointing the row of model
      that should be compared with key}
    @entry[data]{user data from the function
      @fun{gtk-tree-view-set-search-equal-func}}
    @entry[Return]{@em{False} if the row matches, @em{true} otherwise.}
  @end{table}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}")

(export 'gtk-tree-view-search-equal-func)

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

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_equal_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_search_equal_func"
          %gtk-tree-view-set-search-equal-func) :void
  (view (g-object gtk-tree-view))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-equal-func (view func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[func]{the compare callback function to use during the search}
  @begin{short}
    Sets the compare callback function for the interactive search capabilities.
  @end{short}
  Note that for equality the callback function
  @symbol{gtk-tree-view-search-equal-func} returns @em{false} on matches.
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-search-equal-func}"
  (%gtk-tree-view-set-search-equal-func
                                   view
                                   (callback gtk-tree-view-search-equal-func)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-set-search-equal-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_entry ()
;;; gtk_tree_view_set_search_entry () -> gtk-tree-view-search-entry
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tree-view-search-entry) (value view)
  (foreign-funcall "gtk_tree_view_set_search_entry"
                   (g-object gtk-tree-view) view
                   (g-object gtk-entry) value
                   :void)
  value)

(defcfun ("gtk_tree_view_get_search_entry" gtk-tree-view-search-entry)
    (g-object gtk-entry)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @syntax[]{(gtk-tree-view-search-entry view) => entry}
  @syntax[]{(setf (gtk-tree-view-search-entry view) entry)}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[entry]{the @class{gtk-entry} widget the interactive search code of
    the tree view should use or @code{nil}}
  @begin{short}
    Accessor of the @class{gtk-entry} widget which is currently in use as
    interactive search entry for the tree view.
  @end{short}

  The function @sym{gtk-tree-view-search-entry} returns the search entry which
  is currently in use as interactive search entry for the tree view. In case
  the built-in entry is being used, @code{nil} will be returned. The function
  @sym{(setf gtk-tree-view-search-entry)} sets the search entry.

  This is useful when you want to provide a search entry in your interface at
  all time at a fixed position. Passing @code{nil} for @arg{entry} will make
  the interactive search code use the built-in popup entry again.
  @see-class{gtk-tree-view}
  @see-class{gtk-entry}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-search-entry)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchPositionFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-position-func :void
    ((view (g-object gtk-tree-view))
     (dialog (g-object gtk-widget))
     (data :pointer))
  (restart-case
    (funcall (get-stable-pointer-value data) view dialog)
    (return () nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-search-position-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-view-search-position-func atdoc:*external-symbols*)
 "@version{2021-2-26}
  @begin{short}
    No documentation.
  @end{short}
  @begin{pre}
 lambda (view dialog)
  @end{pre}
  @begin[code]{table}
    @entry[view]{A @class{gtk-tree-view} widget.}
    @entry[dialog]{A @class{gtk-widget} object.}
  @end{table}
  @see-class{gtk-tree-view}
  @see-class{gtk-widget}")

(export 'gtk-tree-view-search-position-func)

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
  (view (g-object gtk-tree-view))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-position-func (view func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[func]{the callback function
    @symbol{gtk-tree-view-search-position-func} to use to position the search
    dialog, or @code{nil} to use the default search position function}
  @begin{short}
    Sets the callback function to use when positioning the search dialog.
  @end{short}
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-search-position-func}"
  (if func
      (%gtk-tree-view-set-search-position-func
                               view
                               (callback gtk-tree-view-set-search-position-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-search-position-func view
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer))))

(export 'gtk-tree-view-set-search-position-func)

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
;;; and should not be used in newly written code. Accessibility does not need
;;; the function anymore.
;;;
;;; This function should almost never be used. It is meant for private use by
;;; ATK for determining the number of visible children that are removed when
;;; the user collapses a row, or a row is deleted.
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
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-row-separator-func :boolean
    ((model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (restart-case
    (funcall (get-stable-pointer-value data) model iter)
    (return-true () t)
    (return-false () nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-view-row-separator-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-view-row-separator-func atdoc:*external-symbols*)
 "@version{2021-3-13}
  @begin{short}
    Callback function type for determining whether the row pointed to by
    @arg{iter} should be rendered as a separator.
  @end{short}
  A common way to implement this is to have a boolean column in the model,
  whose values the callback function returns.
  @begin{pre}
 lambda (model iter)
  @end{pre}
  @begin[code]{table}
    @entry[model]{A @class{gtk-tree-model} object.}
    @entry[iter]{A @class{gtk-tree-iter} instance pointing at a row in model.}
    @entry[Return]{@em{True} if the row is a separator.}
  @end{table}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}")

(export 'gtk-tree-view-row-separator-func)

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
  (view (g-object gtk-tree-view))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-row-separator-func (view func)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[func]{a @symbol{gtk-tree-view-row-separator-func} callback function}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator.
  @end{short}
  If the row separator function is @code{nil}, no separators are drawn. This is
  the default value.
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-row-separator-func}"
  (if func
      (%gtk-tree-view-set-row-separator-func
                                     view
                                     (callback gtk-tree-view-row-separator-func)
                                     (allocate-stable-pointer func)
                                     (callback stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-row-separator-func view
                                             (null-pointer)
                                             (null-pointer)
                                             (null-pointer))))

(export 'gtk-tree-view-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_rubber_banding_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_rubber_banding_active"
           gtk-tree-view-is-rubber-banding-active) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @em{True} if a rubber banding operation is currently being done in
    the tree view.
  @end{return}
  @begin{short}
    Returns whether a rubber banding operation is currently being done in
    the tree view.
  @end{short}
  @see-class{gtk-tree-view}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-is-rubber-banding-active)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_grid_lines ()
;;; gtk_tree_view_set_grid_lines () -> gtk-tree-view-grid-lines
;;; ----------------------------------------------------------------------------

(defun (setf gtk-tree-view-grid-lines) (grid-lines view)
  (foreign-funcall "gtk_tree_view_set_grid_lines"
                   (g-object gtk-tree-view) view
                   gtk-tree-view-grid-lines grid-lines
                   :void)
  grid-lines)

(defcfun ("gtk_tree_view_get_grid_lines" gtk-tree-view-grid-lines)
    gtk-tree-view-grid-lines
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @syntax[]{(gtk-tree-view-grid-lines view) => grid-lines}
  @syntax[]{(setf (gtk-tree-view-grid-lines view) grid-lines)}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[grid-lines]{a @symbol{gtk-tree-view-grid-lines} value indicating
    which grid lines to enable}
  @begin{short}
    Accessor of the @symbol{gtk-tree-view-grid-lines} value.
  @end{short}

  The function @sym{gtk-tree-view-grid-lines} returns which grid lines are
  enabled in the tree view. The function @sym{(setf gtk-tree-view-grid-lines)}
  sets which grid lines to draw in the tree view.
  @see-class{gtk-tree-view}
  @see-symbol{gtk-tree-view-grid-lines}"
  (view (g-object gtk-tree-view)))

(export 'gtk-tree-view-grid-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_row" gtk-tree-view-set-tooltip-row) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path} instance}
  @begin{short}
    Sets the tip area of @arg{tooltip} to be the area covered by the row at
    @arg{path}.
  @end{short}
  See also the function @fun{gtk-tree-view-tooltip-column} for a simpler
  alternative. See also the function @fun{gtk-tooltip-set-tip-area}.
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-tooltip-column}
  @see-function{gtk-tooltip-set-tip-area}"
  (view (g-object gtk-tree-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-set-tooltip-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_cell ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_cell" gtk-tree-view-set-tooltip-cell) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-2-26}
  @argument[view]{a @class{gtk-tree-view} widget}
  @argument[tooltip]{a @class{gtk-tooltip} object}
  @argument[path]{a @class{gtk-tree-path} instance or @code{nil}}
  @argument[column]{a @class{gtk-tree-view-column} object or @code{nil}}
  @argument[renderer]{a @class{gtk-cell-renderer} or @code{nil}}
  @begin{short}
    Sets the tip area of @arg{tooltip} to the area @arg{path}, @arg{column} and
    @arg{renderer} have in common.
  @end{short}
  For example if @arg{path} is @code{nil} and @arg{column} is set, the tip area
  will be set to the full area covered by @arg{column}. See also the function
  @fun{gtk-tooltip-set-tip-area}.

  Note that if @arg{path} is not specified and @arg{renderer} is set and part
  of a @arg{column} containing the expander, the @arg{tooltip} might not show
  and hide at the correct position. In such cases path must be set to the
  current node under the mouse cursor for this function to operate correctly.

  See also the function @fun{gtk-tree-view-tooltip-column} for a simpler
  alternative.
  @see-class{gtk-tree-view}
  @see-function{gtk-tooltip-set-tip-area}"
  (view (g-object gtk-tree-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column))
  (renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_context () -> gtk-tree-view-tooltip-context
;;; ----------------------------------------------------------------------------

;; TODO: This implementation is not correct. The arguments x,y are pointers.

(defcfun ("gtk_tree_view_get_tooltip_context" %gtk-tree-view-tooltip-context)
    :boolean
  (view (g-object gtk-tree-view))
  (x :int)
  (y :int)
  (tip :boolean)
  (model :pointer)
  (path :pointer)
  (iter :pointer))

(defun gtk-tree-view-tooltip-context (tree-view)
 #+cl-cffi-gtk-documentation
 "@version{1111-11-11}
  @argument[view]{a @class{gtk-tree-view} widget}
  @begin{return}
    @code{x}     -- a @code{:int} with the x coordinate (relative to widget
                    coordinates) @br{}
    @code{y}     -- a @code{:int} with the y coordinate (relative to widget
                    coordinates) @br{}
    @code{tip}   -- a boolean whether this is a keyboard tooltip or not @br{}
    @code{model} -- a @class{gtk-tree-model} or @code{nil} @br{}
    @code{path}  -- a @class{gtk-tree-path} or @code{nil} @br{}
    @code{iter}  -- a @class{gtk-tree-iter} or @code{nil}
  @end{return}
  @begin{short}
    This function is supposed to be used in a \"query-tooltip\" signal handler
    for @class{gtk-tree-view}.
  @end{short}
  The @arg{x}, @arg{y} and @arg{tip} values which are received in the signal
  handler, should be passed to this function without modification.

  The return value indicates whether there is a tree view row at the given
  coordinates (@em{true}) or not (@em{false}) for mouse tooltips. For keyboard
  tooltips the row returned will be the cursor row. When @em{true}, then any of
  @arg{model}, @arg{path} and @arg{iter} which have been provided will be set to
  point to that row and the corresponding model. @arg{x} and @arg{y} will always
  be converted to be relative to @arg{tree-view}'s \"bin window\" if @arg{tip}
  is @em{false}.
  @see-class{gtk-tree-view}"
  (with-foreign-objects ((x :int)
                         (y :int)
                         (keyboard-tip :boolean)
                         (model :pointer)
                         (path :pointer)
                         (iter :pointer))
    (when (%gtk-tree-view-tooltip-context tree-view
                                          x
                                          y
                                          keyboard-tip
                                          model
                                          path
                                          iter)
      (values (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref keyboard-tip :boolean)
              (mem-ref model 'g-object)
              (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref iter '(g-boxed-foreign gtk-tree-iter :return))))))

(export 'gtk-tree-view-tooltip-context)

;;; --- End of file gtk.tree-view.lisp -----------------------------------------
