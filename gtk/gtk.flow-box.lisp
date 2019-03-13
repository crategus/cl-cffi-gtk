;;; ----------------------------------------------------------------------------
;;; gtk.flow-box.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkFlowBox
;;;
;;;     A container that allows reflowing its children
;;;
;;; Functions
;;;
;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert 
;;;     gtk_flow_box_get_child_at_index 
;;;     gtk_flow_box_get_child_at_pos 
;;;     gtk_flow_box_set_hadjustment 
;;;     gtk_flow_box_set_vadjustment 
;;;     gtk_flow_box_set_homogeneous 
;;;     gtk_flow_box_get_homogeneous 
;;;     gtk_flow_box_set_row_spacing 
;;;     gtk_flow_box_get_row_spacing 
;;;     gtk_flow_box_set_column_spacing 
;;;     gtk_flow_box_get_column_spacing 
;;;     gtk_flow_box_set_min_children_per_line 
;;;     gtk_flow_box_get_min_children_per_line 
;;;     gtk_flow_box_set_max_children_per_line 
;;;     gtk_flow_box_get_max_children_per_line 
;;;     gtk_flow_box_set_activate_on_single_click 
;;;     gtk_flow_box_get_activate_on_single_click 
;;;     (*GtkFlowBoxForeachFunc) 
;;;     gtk_flow_box_selected_foreach 
;;;     gtk_flow_box_get_selected_children 
;;;     gtk_flow_box_select_child 
;;;     gtk_flow_box_unselect_child 
;;;     gtk_flow_box_select_all 
;;;     gtk_flow_box_unselect_all 
;;;     gtk_flow_box_set_selection_mode 
;;;     gtk_flow_box_get_selection_mode 
;;;     (*GtkFlowBoxFilterFunc) 
;;;     gtk_flow_box_set_filter_func 
;;;     gtk_flow_box_invalidate_filter 
;;;     (*GtkFlowBoxSortFunc) 
;;;     gtk_flow_box_set_sort_func 
;;;     gtk_flow_box_invalidate_sort 
;;;     (*GtkFlowBoxCreateWidgetFunc) 
;;;     gtk_flow_box_bind_model 
;;;     gtk_flow_box_child_new 
;;;     gtk_flow_box_child_get_index 
;;;     gtk_flow_box_child_is_selected 
;;;     gtk_flow_box_child_changed 
;;;
;;; Properties
;;;
;;;     gboolean          activate-on-single-click  Read / Write
;;;     guint             column-spacing            Read / Write
;;;     gboolean          homogeneous               Read / Write
;;;     guint             max-children-per-line     Read / Write
;;;     guint             min-children-per-line     Read / Write
;;;     guint             row-spacing               Read / Write
;;;     GtkSelectionMode  selection-mode            Read / Write
;;;
;;; Signals
;;;
;;;     void      activate-cursor-child      Action
;;;     void      child-activated            Run Last
;;;     gboolean  move-cursor                Action
;;;     void      select-all                 Action
;;;     void      selected-children-changed  Run First
;;;     void      toggle-cursor-child        Action
;;;     void      unselect-all               Action
;;;     void      activate                   Action
;;;
;;; Types and Values
;;;
;;;     GtkFlowBox
;;;     GtkFlowBoxChild
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkFlowBoxChild
;;;                 ╰── GtkFlowBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkFlowBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; GtkFlowBoxChild implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxChild
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFlowBoxChild" gtk-flow-box-child
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_flow_box_child_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFlowBox" gtk-flow-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_flow_box_get_type")
  ((activate-on-single-click
    gtk-flow-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (column-spacing
    gtk-flow-box-column-spacing
    "column-spacing" "guint" t t)
   (homogeneous
    gtk-flow-box-homogeneous
    "homogeneous" "gboolean" t t)
   (max-children-per-line
    gtk-flow-box-max-children-per-line
    "max-children-per-line" "guint" t t)
   (min-children-per-line
    gtk-flow-box-min-children-per-line
    "min-children-per-line" "guint" t t)
   (row-spacing
    gtk-flow-box-row-spacing
    "row-spacing" "guint" t t)
   (selection-mode
    gtk-flow-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-flow-box 'type)
 "@version{2019-3-11}
  @begin{short}
    A @sym{gtk-flow-box} positions child widgets in sequence according to its
    orientation.
  @end{short}

  For instance, with the horizontal orientation, the widgets will be arranged
  from left to right, starting a new row under the previous row when necessary.
  Reducing the width in this case will require more rows, so a larger height
  will be requested.

  Likewise, with the vertical orientation, the widgets will be arranged from
  top to bottom, starting a new column to the right when necessary. Reducing the
  height will require more columns, so a larger width will be requested.

  The size request of a @sym{gtk-flow-box} alone may not be what you expect; if
  you need to be able to shrink it along both axes and dynamically reflow its
  children, you may have to wrap it in a @class{gtk-scrolled-window} to enable
  that.

  The children of a @sym{gtk-flow-box} can be dynamically sorted and filtered.

  Although a @sym{gtk-flow-box} must have only @class{gtk-flow-box-child} 
  children, you can add any kind of widget to it via the function
  @fun{gtk-container-add}, and a @class{gtk-flow-box-child} widget will
  automatically be inserted between the box and the widget.

  Also see @class{gtk-list-box}.

  @sym{gtk-flow-box} was added in GTK+ 3.12.

  @subheading{CSS nodes}
    @begin{pre}
  flowbox
  ├── flowboxchild
  │   ╰── <child>
  ├── flowboxchild
  │   ╰── <child>
  │
   ╰── [rubberband]
    @end{pre}
    @sym{gtk-fow-box} uses a single CSS node with name flowbox. 
    @class{gtk-flow-box-child} uses a single CSS node with name flowboxchild.
    For rubberband selection, a subnode with name rubberband is used.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-child\" signal}
      @begin{pre}
 lambda (box)
      @end{pre}
      The \"activate-cursor-child\" signal is a keybinding signal which gets
      emitted when the user activates the flow box.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
      Since 3.12

    @subheading{The \"child-activated\" signal}
      @begin{pre}
 lambda (box child)
      @end{pre}
      The \"child-activated signal\" is emitted when a child has been activated
      by the user.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
        @entry[child]{The child that is activated.}
      @end{table}
      Since 3.12

    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (box step count)
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement.

      Applications should not connect to it, but may emit it with
      the function @fun{g-signal-emit-by-name} if they need to control the
      cursor programmatically.

      The default bindings for this signal come in two variants, the variant
      with the Shift modifier extends the selection, the variant without the
      Shift modifer does not. There are too many key combinations to list them
      all here.

      Arrow keys move by individual children.
      Home/End keys move to the ends of the box.
      PageUp/PageDown keys move vertically by pages.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
        @entry[step]{The granularity to the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
        @entry[Returns]{TRUE to stop other handlers from being invoked for the
          event. @code{nil} to propagate the event further.}
      @end{table}
      Since 3.12

    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (box)
      @end{pre}
      The \"select-all\" signal is a keybinding signal which gets emitted to
      select all children of the box, if the selection mode permits it.

      The default bindings for this signal is Ctrl-a.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
      Since 3.12

    @subheading{The \"selected-children-changed\" signal}
      @begin{pre}
 lambda (box)
      @end{pre}
      The \"selected-children-changed\" signal is emitted when the set of
      selected children changes.

      Use the function @fun{gtk-flow-box-selected-foreach} or
      @fun{gtk-flow-box-get-selected-children} to obtain the selected children.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
      Since 3.12

    @subheading{The \"toggle-cursor-child\" signal}
      @begin{pre}
 lambda (box)
      @end{pre}
      The \"toggle-cursor-child\" signal is a keybinding signal which toggles
      the selection of the child that has the focus.

      The default binding for this signal is Ctrl-Space.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
      Since 3.12

    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (box)
      @end{pre}
      The \"unselect-all\" signal is a keybinding signal which gets emitted to
      unselect all children of the box, if the selection mode permits it.

      The default bindings for this signal is Ctrl-Shift-a.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
      Since 3.12

    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (child)
      @end{pre}
      The \"activate\" signal is emitted when the user activates a child widget
      in a @sym{gtk-flow-box}, either by clicking or double-clicking, or by
      using the Space or Enter key.

      While this signal is used as a keybinding signal, it can be used by
      applications for their own purposes.
      @begin[code]{table}
        @entry[child]{The child on which the signal is emitted.}
      @end{table}
      Since 3.12
  @end{dictionary}
  @see-slot{gtk-flow-box-activate-on-single-click}
  @see-slot{gtk-flow-box-column-spacing}
  @see-slot{gtk-flow-box-homogeneous}
  @see-slot{gtk-flow-box-max-children-per-line}
  @see-slot{gtk-flow-box-min-children-per-line}
  @see-slot{gtk-flow-box-row-spacing}
  @see-slot{gtk-flow-box-selection-mode}
")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-flow-box-activate-on-single-click ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activate-on-single-click"
                                               'gtk-flow-box) 't)
 "The @code{\"activate-on-single-click\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether children can be activated with a single click, or require
  a double-click.@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-activate-on-single-click 
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-activate-on-single-click 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[single]{@code{Nil} to emit child-activated on a single click.}
  @syntax[]{(gtk-flow-box-acivate-on-click object) => single}
  @syntax[]{(setf (gtk-flow-box-activate-on-click object) single)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{activate-on-single-click} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-activate-on-single-click} returns
  whether children activate on single clicks.

  If single is @em{true}, children will be activated when you click on them,
  otherwise you need to double-click.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-column-spacing --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-spacing"
                                               'gtk-flow-box) 't)
 "The @code{\"column-spacing\"} property of type @code{:uint}
  (Read / Write) @br{}
  The amount of horizontal space between two children. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-column-spacing 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[spacing]{The spacing to use.}
  @syntax[]{(gtk-flow-box-column-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-column-spacing object) spacing)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{column-spacing} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-column-spacing} gets the
  horizontal spacing.

  The generic function @sym{(setf gtk-flow-box-column-spacing)} sets the
  horizontal space to add between children. See the \"column-spacing\"
  property.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-homogeneous -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous"
                                               'gtk-flow-box) 't)
 "The @code{\"homogeneous\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether all children should be allocated the same size. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-homogeneous 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[homogeneous]{@em{{True} to create equal allotments, @code{nil} for
    variable allotments.}
  @syntax[]{(gtk-flow-box-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-flow-box-homogeneous object) homogeneous)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{homogeneous} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-column-spacing} returns whether the box
  is homogeneous (all children are the same size).

  The generic function @sym{(setf gtk-flow-box-column-spacing)} sets the
  \"homogeneous\" property of the flow box, controlling whether or not all
  children of box are given equal space in the box.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-max-children-per-line -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-children-per-line"
                                               'gtk-flow-box) 't)
 "The @code{\"max-children-per-line\"} property of type @code{:uint}
  (Read / Write) @br{}
  The maximum amount of children to request space for consecutively in the given
  orientation. @br{}
  Default value: 7")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-max-children-per-line atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-max-children-per-line 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[n-children]{The maximum number of children per line.}
  @syntax[]{(gtk-flow-box-max-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-max-chilren-per-line object) n-children)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{max-children-per-line} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-max-children-per-llne} gets the
  maximum number of children per line.

  The generic function @sym{(setf gtk-flow-box-max-children-per-line)}
  sets the maximum number of children to request and allocate space for in
  box’s orientation.

  Setting the maximum number of children per line limits the overall natural
  size request to be no more than @arg{n-children} children long in the given
  orientation.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-min-children-per-line -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-children-per-line"
                                               'gtk-flow-box) 't)
 "The @code{\"min-children-per-line\"} property of type @code{:uint}
  (Read / Write) @br{}
  The minimum number of children to allocate consecutively in the given
  orientation.

  Setting the minimum children per line ensures that a reasonably small height
  will be requested for the overall minimum width of the box. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-min-children-per-line atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-min-children-per-line 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[n-children]{the minimum number of children per line.}
  @syntax[]{(gtk-flow-box-min-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-min-chilren-per-line object) n-children)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{min-children-per-line} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-max-children-per-llne} gets the
  minimum number of children per line.

  The generic function @sym{(setf gtk-flow-box-max-children-per-line)}
  sets the minimum number of children to line up in box’s orientation before
  flowing.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-row-spacing -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-spacing"
                                               'gtk-flow-box) 't)
 "The @code{\"row-spacing\"} property of type @code{:uint} (Read / Write) @br{}
  The amount of vertical space between two children. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-row-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-row-spacing 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[spacing]{The spacing to use.}
  @syntax[]{(gtk-flow-box-row-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-row-spacing object) spacing)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{row-spacing} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-max-children-per-llne} gets the
  vertical spacing.

  The generic function @sym{(setf gtk-flow-box-max-children-per-line)}
  sets the vertical space to add between children. See the \"row-spacing\"
  property.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-selection-mode --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-mode"
                                               'gtk-flow-box) 't)
 "The @code{\"selection-mode\"} property of type @symbol{gtk-selection-mode}
 (Read / Write) @br{}
  The selection mode used by the flow box. @br{}
  Default value: @code{:single}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-selection-mode 'function)
 "@version{2019-3-11}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[mode]{The new selection mode.}
  @syntax[]{(gtk-flow-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk-flow-box-selection-mode object) mode)}
  @begin{short}
    Accessor of the slot @slot[gtk-flow-box]{selection-mode} of the
    @class{gtk-flow-box} class.
  @end{short}

  The generic function @sym{gtk-flow-box-selection-mode} gets the
  selection mode of box.

  The generic function @sym{(setf gtk-flow-box-selection-mode)}
  sets how selection works in box . See GtkSelectionMode for details.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_new ()
;;;
;;; GtkWidget * gtk_flow_box_new (void);
;;;
;;; Creates a GtkFlowBox.
;;;
;;; Returns :
;;;     a new GtkFlowBox container
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_insert ()
;;;
;;; void gtk_flow_box_insert (GtkFlowBox *box, GtkWidget *widget, gint position)
;;;
;;; Inserts the widget into box at position .
;;;
;;; If a sort function is set, the widget will actually be inserted at the
;;; calculated position and this function has the same effect as
;;; gtk_container_add().
;;;
;;; If position is -1, or larger than the total number of children in the box ,
;;; then the widget will be appended to the end.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; widget :
;;;     the GtkWidget to add
;;;
;;; position :
;;;     the position to insert child in
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_index ()
;;;
;;; GtkFlowBoxChild * gtk_flow_box_get_child_at_index (GtkFlowBox *box,
;;;                                                    gint idx);
;;;
;;; Gets the nth child in the box .
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; idx :
;;;     the position of the child
;;;
;;; Returns :
;;;     the child widget, which will always be a GtkFlowBoxChild or NULL in case
;;;     no child widget with the given index exists.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_pos ()
;;;
;;; GtkFlowBoxChild * gtk_flow_box_get_child_at_pos (GtkFlowBox *box,
;;;                                                  gint x,
;;;                                                  gint y);
;;;
;;; Gets the child in the (x , y ) position.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; x :
;;;     the x coordinate of the child
;;;
;;; y :
;;;     the y coordinate of the child
;;;
;;; Returns :
;;;     the child widget, which will always be a GtkFlowBoxChild or NULL in case
;;;     no child widget exists for the given x and y coordinates.
;;;
;;; Since: 3.22.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_hadjustment ()
;;;
;;; void gtk_flow_box_set_hadjustment (GtkFlowBox *box, 
;;;                                    GtkAdjustment *adjustment);
;;;
;;; Hooks up an adjustment to focus handling in box . The adjustment is also
;;; used for autoscrolling during rubberband selection. See
;;; gtk_scrolled_window_get_hadjustment() for a typical way of obtaining the
;;; adjustment, and gtk_flow_box_set_vadjustment()for setting the vertical
;;; adjustment.
;;;
;;; The adjustments have to be in pixel units and in the same coordinate system
;;; as the allocation for immediate children of the box.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; adjustment :
;;;     an adjustment which should be adjusted when the focus is moved among the
;;;     descendents of container
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_vadjustment ()
;;;
;;; void gtk_flow_box_set_vadjustment (GtkFlowBox *box,
;;;                                    GtkAdjustment *adjustment);
;;;
;;; Hooks up an adjustment to focus handling in box . The adjustment is also
;;; used for autoscrolling during rubberband selection. See
;;; gtk_scrolled_window_get_vadjustment() for a typical way of obtaining the
;;; adjustment, and gtk_flow_box_set_hadjustment()for setting the horizontal 
;;; adjustment.
;;;
;;; The adjustments have to be in pixel units and in the same coordinate system
;;; as the allocation for immediate children of the box.
;;;
;;; box :
;;;     a GtkFlowBox
;;; 
;;; adjustment :
;;;     an adjustment which should be adjusted when the focus is moved among
;;;     the descendents of container
;;; 
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxForeachFunc ()
;;;
;;; void (*GtkFlowBoxForeachFunc) (GtkFlowBox *box,
;;;                                GtkFlowBoxChild *child,
;;;                                gpointer user_data);
;;;
;;; A function used by gtk_flow_box_selected_foreach(). It will be called on
;;; every selected child of the box .
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; child :
;;;     a GtkFlowBoxChild
;;;
;;; user_data :
;;;     user data.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_selected_foreach ()
;;;
;;; void gtk_flow_box_selected_foreach (GtkFlowBox *box,
;;;                                     GtkFlowBoxForeachFunc func,
;;;                                     gpointer data);
;;;
;;; Calls a function for each selected child.
;;; 
;;; Note that the selection cannot be modified from within this function.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; func :
;;;     the function to call for each selected child.
;;;
;;; data :
;;;     user data to pass to the function
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_selected_children ()
;;;
;;; GList * gtk_flow_box_get_selected_children (GtkFlowBox *box);
;;;
;;; Creates a list of all selected children.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; Returns :
;;;     A GList containing the GtkWidget for each selected child. Free with
;;;     g_list_free() when done.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_child ()
;;;
;;; void gtk_flow_box_select_child (GtkFlowBox *box, GtkFlowBoxChild *child);
;;;
;;; Selects a single child of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkFlowBox
;;; 
;;; child :
;;;     a child of box
;;; 
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_child ()
;;;
;;; void gtk_flow_box_unselect_child (GtkFlowBox *box, GtkFlowBoxChild *child);
;;;
;;; Unselects a single child of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; child :
;;;     a child of box
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_all ()
;;;
;;; void gtk_flow_box_select_all (GtkFlowBox *box);
;;;
;;; Select all children of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_all ()
;;;
;;; void gtk_flow_box_unselect_all (GtkFlowBox *box);
;;;
;;; Unselect all children of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkFlowBox
;;; 
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxFilterFunc ()
;;;
;;; gboolean (*GtkFlowBoxFilterFunc) (GtkFlowBoxChild *child,
;;;                                   gpointer user_data);
;;;
;;; A function that will be called whenrever a child changes or is added. It
;;; lets you control if the child should be visible or not.
;;;
;;; child :
;;;     a GtkFlowBoxChild that may be filtered
;;; 
;;; user_data :
;;;     user data.
;;;
;;; Returns :
;;;     TRUE if the row should be visible, FALSE otherwise
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_filter_func ()
;;;
;;; void gtk_flow_box_set_filter_func (GtkFlowBox *box,
;;;                                    GtkFlowBoxFilterFunc filter_func,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify destroy);
;;;
;;; By setting a filter function on the box one can decide dynamically which of
;;; the children to show. For instance, to implement a search function that only
;;; shows the children matching the search terms.
;;;
;;; The filter_func will be called for each child after the call, and it will
;;; continue to be called each time a child changes (via
;;; gtk_flow_box_child_changed()) or when gtk_flow_box_invalidate_filter() is
;;; called.
;;;
;;; Note that using a filter function is incompatible with using a model (see
;;; gtk_flow_box_bind_model()).
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; filter_func :
;;;     callback that lets you filter which children to show.
;;;
;;; user_data :
;;;     user data passed to filter_func
;;;
;;; destroy :
;;;     destroy notifier for user_data
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_filter ()
;;;
;;; void gtk_flow_box_invalidate_filter (GtkFlowBox *box);
;;;
;;; Updates the filtering for all children.
;;;
;;; Call this function when the result of the filter function on the box is
;;; changed due ot an external factor. For instance, this would be used if the
;;; filter function just looked for a specific search term, and the entry with
;;; the string has changed.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxSortFunc ()
;;;
;;; gint (*GtkFlowBoxSortFunc) (GtkFlowBoxChild *child1,
;;;                             GtkFlowBoxChild *child2,
;;;                             gpointer user_data);
;;;
;;; A function to compare two children to determine which should come first.
;;;
;;; child1 :
;;;     the first child
;;;
;;; child2 :
;;;     the second child
;;;
;;; user_data :
;;;     user data.
;;;
;;; Returns :
;;;     < 0 if child1 should be before child2 , 0 if the are equal, and > 0
;;;     otherwise
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_sort_func ()
;;;
;;; void gtk_flow_box_set_sort_func (GtkFlowBox *box,
;;;                                  GtkFlowBoxSortFunc sort_func,
;;;                                  gpointer user_data,
;;;                                  GDestroyNotify destroy);
;;;
;;; By setting a sort function on the box , one can dynamically reorder the
;;; children of the box, based on the contents of the children.
;;;
;;; The sort_func will be called for each child after the call, and will
;;; continue to be called each time a child changes (via
;;; gtk_flow_box_child_changed()) and when gtk_flow_box_invalidate_sort() is
;;; called.
;;;
;;; Note that using a sort function is incompatible with using a model (see
;;; gtk_flow_box_bind_model()).
;;;
;;; box :
;;;     a GtkFlowBox
;;; 
;;; sort_func :
;;;     the sort function.
;;;
;;; user_data :
;;;     user data passed to sort_func
;;;
;;; destroy :
;;;     destroy notifier for user_data
;;; 
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_sort ()
;;;
;;; void gtk_flow_box_invalidate_sort (GtkFlowBox *box);
;;;
;;; Updates the sorting for all children.
;;;
;;; Call this when the result of the sort function on box is changed due to an
;;; external factor.
;;;
;;; box :
;;;     a GtkFlowBox
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxCreateWidgetFunc ()
;;;
;;; GtkWidget * (*GtkFlowBoxCreateWidgetFunc) (gpointer item,
;;;                                            gpointer user_data);
;;;
;;; Called for flow boxes that are bound to a GListModel with
;;; gtk_flow_box_bind_model() for each item that gets added to the model.
;;;
;;; item :
;;;     the item from the model for which to create a widget for.
;;;
;;; user_data :
;;;     user data from gtk_flow_box_bind_model().
;;;
;;; Returns :
;;;     a GtkWidget that represents item .
;;;
;;; Since: 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_bind_model ()
;;;
;;; void gtk_flow_box_bind_model (GtkFlowBox *box,
;;;                               GListModel *model,
;;;                               GtkFlowBoxCreateWidgetFunc create_widget_func,
;;;                               gpointer user_data,
;;;                               GDestroyNotify user_data_free_func);
;;;
;;; Binds model to box .
;;;
;;; If box was already bound to a model, that previous binding is destroyed.
;;;
;;; The contents of box are cleared and then filled with widgets that represent
;;; items from model . box is updated whenever model changes. If model is NULL,
;;; box is left empty.
;;;
;;; It is undefined to add or remove widgets directly (for example, with
;;; gtk_flow_box_insert() or gtk_container_add()) while box is bound to a model.
;;;
;;; Note that using a model is incompatible with the filtering and sorting
;;; functionality in GtkFlowBox. When using a model, filtering and sorting
;;; should be implemented by the model.
;;;
;;; box :
;;;     a GtkFlowBox
;;; 
;;; model :
;;;     the GListModel to be bound to box .
;;;
;;; create_widget_func :
;;;     a function that creates widgets for items
;;; 
;;; user_data :
;;;     user data passed to create_widget_func
;;; 
;;; user_data_free_func :
;;;     function for freeing user_data
;;;
;;; Since: 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_new ()
;;;
;;; GtkWidget * gtk_flow_box_child_new (void);
;;;
;;; Creates a new GtkFlowBoxChild, to be used as a child of a GtkFlowBox.
;;;
;;; Returns :
;;;     a new GtkFlowBoxChild
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_get_index ()
;;;
;;; gint gtk_flow_box_child_get_index (GtkFlowBoxChild *child);
;;;
;;; Gets the current index of the child in its GtkFlowBox container.
;;;
;;; child :
;;;     a GtkFlowBoxChild
;;; 
;;; Returns :
;;;     the index of the child , or -1 if the child is not in a flow box.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_is_selected ()
;;;
;;; gboolean gtk_flow_box_child_is_selected (GtkFlowBoxChild *child);
;;;
;;; Returns whether the child is currently selected in its GtkFlowBox container.
;;;
;;; child :
;;;     a GtkFlowBoxChild
;;;
;;; Returns :
;;;     TRUE if child is selected
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_changed ()
;;;
;;; void gtk_flow_box_child_changed (GtkFlowBoxChild *child);
;;;
;;; Marks child as changed, causing any state that depends on this to be
;;; updated. This affects sorting and filtering.
;;;
;;; Note that calls to this method must be in sync with the data used for the
;;; sorting and filtering functions. For instance, if the list is mirroring some
;;; external data set, and *two* children changed in the external data set when
;;; you call gtk_flow_box_child_changed() on the first child, the sort function
;;; must only read the new data for the first of the two changed children,
;;; otherwise the resorting of the children will be wrong.
;;;
;;; This generally means that if you don’t fully control the data model, you
;;; have to duplicate the data that affects the sorting and filtering functions
;;; into the widgets themselves. Another alternative is to call
;;; gtk_flow_box_invalidate_sort() on any model change, but that is more
;;; expensive.
;;;
;;; child :
;;;     a GtkFlowBoxChild
;;; 
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.flow-box.lisp ------------------------------------------
