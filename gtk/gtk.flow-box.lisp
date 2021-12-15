;;; ----------------------------------------------------------------------------
;;; gtk.flow-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkFlowBox
;;;     GtkFlowBoxChild
;;;
;;; Functions
;;;
;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment
;;;     gtk_flow_box_set_homogeneous                       Accessor
;;;     gtk_flow_box_get_homogeneous                       Accessor
;;;     gtk_flow_box_set_row_spacing                       Accessor
;;;     gtk_flow_box_get_row_spacing                       Accessor
;;;     gtk_flow_box_set_column_spacing                    Accessor
;;;     gtk_flow_box_get_column_spacing                    Accessor
;;;     gtk_flow_box_set_min_children_per_line             Accessor
;;;     gtk_flow_box_get_min_children_per_line             Accessor
;;;     gtk_flow_box_set_max_children_per_line             Accessor
;;;     gtk_flow_box_get_max_children_per_line             Accessor
;;;     gtk_flow_box_set_activate_on_single_click          Accessor
;;;     gtk_flow_box_get_activate_on_single_click          Accessor
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
;;;         gboolean    activate-on-single-click    Read / Write
;;;            guint    column-spacing              Read / Write
;;;         gboolean    homogeneous                 Read / Write
;;;            guint    max-children-per-line       Read / Write
;;;            guint    min-children-per-line       Read / Write
;;;            guint    row-spacing                 Read / Write
;;; GtkSelectionMode    selection-mode              Read / Write
;;;
;;; Signals
;;;
;;;             void    activate-cursor-child       Action
;;;             void    child-activated             Run Last
;;;         gboolean    move-cursor                 Action
;;;             void    select-all                  Action
;;;             void    selected-children-changed   Run First
;;;             void    toggle-cursor-child         Action
;;;             void    unselect-all                Action
;;;             void    activate                    Action
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-flow-box-child 'type)
 "@version{2021-12-6}
  @begin{short}
    The @sym{gtk-flow-box-child} widget is the kind of widget that can be
    added to a @class{gtk-flow-box} widget.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (child)    :action
      @end{pre}
      The signal is emitted when the user activates a child widget in a
      @class{gtk-flow-box} widget, either by clicking or double-clicking, or by
      using the @kbd{Space} or @kbd{Enter} key. While this signal is used as a
      keybinding signal, it can be used by applications for their own purposes.
      @begin[code]{table}
        @entry[child]{The @sym{gtk-flow-box-child} widget on which the signal
          is emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-flow-box}")

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
 "@version{*2021-12-15}
  @begin{short}
    A @sym{gtk-flow-box} widget positions child widgets in sequence according to
    its orientation.
  @end{short}

  @image[flow-box]{}

  For instance, with the horizontal orientation, the widgets will be arranged
  from left to right, starting a new row under the previous row when necessary.
  Reducing the width in this case will require more rows, so a larger height
  will be requested.

  Likewise, with the vertical orientation, the widgets will be arranged from
  top to bottom, starting a new column to the right when necessary. Reducing
  the height will require more columns, so a larger width will be requested.

  The size request of a @sym{gtk-flow-box} widget alone may not be what you
  expect. If you need to be able to shrink it along both axes and dynamically
  reflow its children, you may have to wrap it in a @class{gtk-scrolled-window}
  widget to enable that.

  The children of a @sym{gtk-flow-box} widget can be dynamically sorted and
  filtered.

  Although a @sym{gtk-flow-box} widget must have only @class{gtk-flow-box-child}
  children, you can add any kind of widget to it via the @fun{gtk-container-add}
  function, and a @class{gtk-flow-box-child} widget will automatically be
  inserted between the box and the widget.

  Also see the @class{gtk-list-box} widget.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
  flowbox
  ├── flowboxchild
  │   ╰── <child>
  ├── flowboxchild
  │   ╰── <child>
  │
   ╰── [rubberband]
    @end{pre}
    The @sym{gtk-flow-box} implementation uses a single CSS node with name
    @code{flowbox}. The @class{gtk-flow-box-child} implementation uses a single
    CSS node with name @code{flowboxchild}. For rubberband selection, a subnode
    with name @code{rubberband} is used.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-child\" signal}
      @begin{pre}
 lambda (flowbox)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      activates the flow box.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"child-activated\" signal}
      @begin{pre}
 lambda (flowbox child)    :run-last
      @end{pre}
      The signal is emitted when a child has been activated by the user.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
        @entry[child]{The @class{gtk-flow-box-child} widget that is activated.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (flowbox step count)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. Applications should not connect to it, but
      may emit it with the @fun{g-signal-emit} function if they need to control
      the cursor programmatically.

      The default bindings for this signal come in two variants, the variant
      with the @kbd{Shift} modifier extends the selection, the variant without
      the @kbd{Shift} modifer does not. There are too many key combinations to
      list them all here. Arrow keys move by individual children.
      @kbd{Home}/@kbd{End} keys move to the ends of the box.
      @kbd{PageUp}/@kbd{PageDown} keys move vertically by pages.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
        @entry[step]{The granularity to the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (flowbox)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to select all
      children of the box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-a} key.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"selected-children-changed\" signal}
      @begin{pre}
 lambda (flowbox)    :run-first
      @end{pre}
      The signal is emitted when the set of selected children changes. Use the
      @fun{gtk-flow-box-selected-foreach} or
      @fun{gtk-flow-box-selected-children} functions to obtain the selected
      children.
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
    @subheading{The \"toggle-cursor-child\" signal}
      @begin{pre}
 lambda (flowbox)    :action
      @end{pre}
      The signal is a keybinding signal which toggles the selection of the
      child that has the focus. The default binding for this signal is the
      @kbd{Ctrl-Space} key.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (flowbox)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to unselect all
      children of the box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-Shift-a} key.
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-flow-box-activate-on-single-click}
  @see-slot{gtk-flow-box-column-spacing}
  @see-slot{gtk-flow-box-homogeneous}
  @see-slot{gtk-flow-box-max-children-per-line}
  @see-slot{gtk-flow-box-min-children-per-line}
  @see-slot{gtk-flow-box-row-spacing}
  @see-slot{gtk-flow-box-selection-mode}
  @see-class{gtk-flow-box-child}
  @see-class{gtk-list-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-flow-box-activate-on-single-click ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activate-on-single-click"
                                               'gtk-flow-box) 't)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether children can be activated with a single click, or require
  a double click. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-activate-on-single-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-activate-on-single-click 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-activate-on-single-click object) => setting}
  @syntax[]{(setf (gtk-flow-box-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[setting]{@em{false} to emit the \"child-activated\" signal on
    a single click}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{activate-on-single-click} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-activate-on-single-click} slot access function returns
  whether children activate on single clicks. If the @arg{setting} argument is
  @em{true}, children will be activated when you click on them, otherwise you
  need to double click.
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-column-spacing --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-spacing"
                                               'gtk-flow-box) 't)
 "The @code{column-spacing} property of type @code{:uint} (Read / Write) @br{}
  The amount of horizontal space between two children. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-column-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-column-spacing 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-column-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-column-spacing object) spacing)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[spacing]{An unsigned integer with the spacing to use.}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{column-spacing} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-column-spacing} slot access function gets the
  horizontal space to add between children. The
  @sym{(setf gtk-flow-box-column-spacing)} slot access function sets the
  horizontal spacing.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-row-spacing}")

;;; --- gtk-flow-box-homogeneous -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous"
                                               'gtk-flow-box) 't)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether all children should be allocated the same size. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-homogeneous 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-flow-box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false} for
    variable allotments}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{homogeneous} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-homogeneous} slot access function returns whether
  the flow box is homogeneous - all children are the same size. The
  @sym{(setf gtk-flow-box-homogeneous)} slot access function sets the
  property.
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-max-children-per-line -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-children-per-line"
                                               'gtk-flow-box) 't)
 "The @code{max-children-per-line} property of type @code{:uint}
  (Read / Write) @br{}
  The maximum amount of children to request space for consecutively in the
  given orientation. @br{}
  Default value: 7")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-max-children-per-line atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-max-children-per-line 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-max-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-max-children-per-line object) n-children)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[n-children]{an unsigned integer with the maximum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{max-children-per-line} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-max-children-per-line} slot access function gets the
  maximum number of children per line to request and allocate space for in the
  orientation of the flow box. The
  @sym{(setf gtk-flow-box-max-children-per-line)} slot access function sets the
  maximum number of children.

  Setting the maximum number of children per line limits the overall natural
  size request to be no more than @arg{n-children} children long in the given
  orientation.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-min-children-per-line}")

;;; --- gtk-flow-box-min-children-per-line -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-children-per-line"
                                               'gtk-flow-box) 't)
 "The @code{min-children-per-line} property of type @code{:uint} (Read / Write)
  @br{}
  The minimum number of children to allocate consecutively in the given
  orientation. Setting the minimum children per line ensures that a reasonably
  small height will be requested for the overall minimum width of the box. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-min-children-per-line atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-min-children-per-line 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-min-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-min-children-per-line object) n-children)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[n-children]{an unsigned integer with the minimum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{min-children-per-line} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-min-children-per-line} slot access function gets the
  minimum number of children per line in the orientation of the flow box
  before flowing. The @sym{(setf gtk-flow-box-min-children-per-line)} slot
  access function sets the minimum number of children per line.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-max-children-per-line}")

;;; --- gtk-flow-box-row-spacing -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-spacing"
                                               'gtk-flow-box) 't)
 "The @code{row-spacing} property of type @code{:uint} (Read / Write) @br{}
  The amount of vertical space between two children. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-row-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-row-spacing 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-row-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-row-spacing object) spacing)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[spacing]{an unsigned integer with the spacing to use}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{row-spacing} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-row-spacing} slot access function gets the vertical
  space to add between children. The @sym{(setf gtk-flow-box-row-spacing)} slot
  access function sets the vertical spacing.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-column-spacing}")

;;; --- gtk-flow-box-selection-mode --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-mode"
                                               'gtk-flow-box) 't)
 "The @code{selection-mode} property of type @symbol{gtk-selection-mode}
  (Read / Write) @br{}
  The selection mode used by the flow box. @br{}
  Default value: @code{:single}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-selection-mode 'function)
 "@version{2021-12-6}
  @syntax[]{(gtk-flow-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk-flow-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk-flow-box} widget}
  @argument[mode]{a value of the @symbol{gtk-selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{selection-mode} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The @sym{gtk-flow-box-selection-mode} slot access function gets the selection
  mode of the flow box. The @sym{(setf gtk-flow-box-selection-mode)} slot access
  function sets how selection works in the flow box.
  @see-class{gtk-flow-box}
  @see-symbol{gtk-selection-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-flow-box-new))

(defun gtk-flow-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @return{A new @class{gtk-flow-box} widget.}
  @begin{short}
    Creates a new flow box.
  @end{short}
  @see-class{gtk-flow-box}"
  (make-instance 'gtk-flow-box))

(export 'gtk-flow-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_insert" gtk-flow-box-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[child]{a @class{gtk-widget} child widget to add}
  @argument[position]{an integer with the position to insert the child widget
    in}
  @begin{short}
    Inserts the child widget into the flow box at a given position.
  @end{short}

  If a sort function is set, the widget will actually be inserted at the
  calculated position and this function has the same effect as the
  @fun{gtk-container-add} function.

  If the @arg{position} argument is -1, or larger than the total number of
  children in the flow box, then the child widget will be appended to the end.
  @see-class{gtk-flow-box}
  @see-class{gtk-widget}"
  (flowbox (g-object gtk-flow-box))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-flow-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_index () -> gtk-flow-box-child-at-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_get_child_at_index" gtk-flow-box-child-at-index)
    (g-object gtk-flow-box-child)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[index]{an integer with the position of the child widget}
  @return{The child widget, which will always be a @class{gtk-flow-box-child}
    widget or @code{nil} in case no child widget with the given index exists.}
  @begin{short}
    Gets the nth child widget in the flow box.
  @end{short}
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}"
  (flowbox (g-object gtk-flow-box))
  (index :int))

(export 'gtk-flow-box-child-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_pos () -> gtk-flow-box-child-at-pos
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_get_child_at_pos" gtk-flow-box-child-at-pos)
    (g-object gtk-flow-box-child)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[x]{an integer with the x coordinate of the child widget}
  @argument[y]{an integer with the y coordinate of the child widget}
  @return{The child widget, which will always be a @class{gtk-flow-box-child}
    widget or @code{nil} in case no child widget exists for the given
    coordinates.}
  @begin{short}
    Gets the child widget in the flow box at the given coordinates.
  @end{short}

  Since 3.22
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}"
  (flowbox (g-object gtk-flow-box))
  (x :int)
  (y :int))

(export 'gtk-flow-box-child-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_hadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_hadjustment" gtk-flow-box-set-hadjustment) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be adjusted
    when the focus is moved among the descendents of @arg{flowbox}}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the @fun{gtk-scrolled-window-hadjustment} function for a typical way of
  obtaining the adjustment, and the @fun{gtk-flow-box-vadjustment} function for
  setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the flow box.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-set-vadjustment}
  @see-function{gtk-scrolled-window-hadjustment}"
  (flowbox (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-flow-box-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_vadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_vadjustment" gtk-flow-box-set-vadjustment) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be adjusted
    when the focus is moved among the descendents of @arg{flowbox}}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the @fun{gtk-scrolled-window-vadjustment} function for a typical way of
  obtaining the adjustment, and the @fun{gtk-flow-box-hadjustment} function for
  setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the box.
  @see-class{gtk-flow-box}
  @see-function{gtk-flow-box-set-hadjustment}
  @see-function{gtk-scrolled-window-vadjustment}"
  (flowbox (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-flow-box-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxForeachFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-flow-box-foreach-func :void
    ((flowbox (g-object gtk-flow-box))
     (child (g-object gtk-flow-box-child))
     (data :pointer))
  (restart-case
    (let ((ptr (get-stable-pointer-value data)))
      (funcall ptr flowbox child))
    (return () :report "Error in the GtkFlowBoxForeachFunc callback." nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-foreach-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-flow-box-foreach-func atdoc:*external-symbols*)
 "@version{2021-12-6}
  @begin{short}
    A callback function used by the @fun{gtk-flow-box-selected-foreach}
    function.
  @end{short}
  It will be called on every selected child widget of the flow box.
  @begin{pre}
 lambda (flowbox child)
  @end{pre}
  @begin[code]{table}
    @entry[flowbox]{a @class{gtk-flow-box} widget}
    @entry[child]{a @class{gtk-flow-box-child} child wiget}
  @end{table}
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}")

(export 'gtk-flow-box-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_selected_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_selected_foreach" %gtk-flow-box-selected-foreach) :void
  (flowbox (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer))

(defun gtk-flow-box-selected-foreach (flowbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[func]{a @symbol{gtk-flow-box-foreach-func} callback function}
  @begin{short}
    Calls a function for each selected child widget in the flow box.
  @end{short}
  Note that the selection cannot be modified from within this function.
  @see-class{gtk-flow-box}
  @see-symbol{gtk-flow-box-foreach-func}"
  (with-stable-pointer (ptr func)
    (%gtk-flow-box-selected-foreach flowbox
                                    (callback gtk-flow-box-foreach-func)
                                    ptr)))

(export 'gtk-flow-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_selected_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_get_selected_children" gtk-flow-box-selected-children)
    (g-list (g-object gtk-flow-box-child))
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @return{A list containing the @class{gtk-flow-box-child} child widget for each
    selected child widget.}
  @begin{short}
    Creates a list of all selected children.
  @end{short}
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-selected-children)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_select_child" gtk-flow-box-select-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[child]{a @class{gtk-widget} child widget of the flow box}
  @begin{short}
    Selects a single child widget of the flow box, if the selection mode allows
    it.
  @end{short}
  @see-class{gtk-flow-box}
  @see-class{gtk-widget}"
  (flowbox (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-select-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_unselect_child" gtk-flow-box-unselect-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[child]{a @class{gtk-widget} child widget of the flow box}
  @begin{short}
    Unselects a single child widget of the flow box, if the selection mode
    allows it.
  @end{short}
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-unselect-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_select_all" gtk-flow-box-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @begin{short}
    Select all children of the flow box, if the selection mode allows it.
  @end{short}
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_unselect_all" gtk-flow-box-unselect-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @begin{short}
    Unselect all children of the flow box, if the selection mode allows it.
  @end{short}
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxFilterFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-flow-box-filter-func :boolean
    ((child (g-object gtk-flow-box-child))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr child)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-filter-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-flow-box-filter-func atdoc:*external-symbols*)
 "@version{2021-12-6}
  @begin{short}
    A function that will be called whenever a child widget changes or is added.
  @end{short}
  It lets you control if the child widget should be visible or not.
  @begin{pre}
 lambda (child)
  @end{pre}
  @begin[code]{table}
    @entry[child]{A @class{gtk-flow-box-child} widget that may be filtered.}
  @end{table}
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}
  @see-function{gtk-flow-box-set-filter-func}")

(export 'gtk-flow-box-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_filter_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_filter_func" %gtk-flow-box-set-filter-func) :void
  (flowbox (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-flow-box-set-filter-func (flowbox func)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-15}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[func]{a @symbol{gtk-flow-box-filter-func} callback function that
    lets you filter which children to show}
  @begin{short}
    By setting a filter function on the flow box one can decide dynamically
    which of the children to show.
  @end{short}
  For instance, to implement a search function that only shows the children
  matching the search terms.

  The @arg{func} function will be called for each child widget after the call,
  and it will continue to be called each time a child changes, via the
  @fun{gtk-flow-box-child-changed} function or when the
  @fun{gtk-flow-box-invalidate-filter} function is called.

  Note that using a filter function is incompatible with using a model. See
  the @fun{gtk-flow-box-bind-model} function.
  @see-class{gtk-flow-box}
  @see-symbol{gtk-flow-box-filter-func}
  @see-function{gtk-flow-box-child-changed}
  @see-function{gtk-flow-box-invalidate-filter}"
  (%gtk-flow-box-set-filter-func flowbox
                                 (callback gtk-flow-box-filter-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify)))

(export 'gtk-flow-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_invalidate_filter" gtk-flow-box-invalidate-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-15}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @begin{short}
    Updates the filtering for all children in the flow box.
  @end{short}

  Call this function when the result of the filter function on the flow box is
  changed due ot an external factor. For instance, this would be used if the
  filter function just looked for a specific search term, and the entry with
  the string has changed.
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxSortFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-flow-box-sort-func :int
    ((child1 (g-object gtk-flow-box-child))
     (child2 (g-object gtk-flow-box-child))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr child1 child2)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-sort-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-flow-box-sort-func atdoc:*external-symbols*)
 "@version{2021-12-6}
  @begin{short}
    A function to compare two children to determine which should come first.
  @end{short}
  @begin{pre}
 lambda (child1 child2)
  @end{pre}
  @begin[code]{table}
    @entry[child1]{The first @class{gtk-flow-box-child} widget.}
    @entry[child2]{The second @class{gtk-flow-box-child} widget.}
    @entry[Returns]{< 0 if @arg{child1} should be before @arg{child2}, 0 if the
      are equal, and > 0 otherwise.}
  @end{table}
  @see-class{gtk-flow-box}
  @see-class{gtk-flow-box-child}
  @see-function{gtk-flow-box-set-sort-func}")

(export 'gtk-flow-box-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_sort_func" %gtk-flow-box-set-sort-func) :void
  (flowbox (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-flow-box-set-sort-func (flowbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[func]{a @symbol{gtk-flow-box-sort-func} callback function for the
    sort function}
  @begin{short}
    By setting a sort function on the flow box, one can dynamically reorder the
    children of the flow box, based on the contents of the children.
  @end{short}

  The @arg{func} function will be called for each child after the call,
  and will continue to be called each time a child changes, via the
  @fun{gtk-flow-box-child-changed} function, and when the
  @fun{gtk-flow-box-invalidate-sort} function is called.

  Note that using a sort function is incompatible with using a model. See
  the @fun{gtk-flow-box-bind-model} function.
  @see-class{gtk-flow-box}
  @see-symbol{gtk-flow-box-sort-func}
  @see-function{gtk-flow-box-child-changed}
  @see-function{gtk-flow-box-invalidate-sort}"
  (%gtk-flow-box-set-sort-func flowbox
                               (callback gtk-flow-box-sort-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify)))

(export 'gtk-flow-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_sort ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_invalidate_sort" gtk-flow-box-invalidate-sort) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @begin{short}
    Updates the sorting for all children in the flow box.
  @end{short}
  Call this when the result of the sort function on the flow box is changed due
  to an external factor.
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxCreateWidgetFunc ()
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defcallback gtk-flow-box-create-widget-func (g-object gtk-widget)
    ((item :pointer)
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr item)))

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-flow-box-create-widget-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-flow-box-create-widget-func atdoc:*external-symbols*)
 "@version{2021-12-6}
  @begin{short}
    Called for flow boxes that are bound to a @class{g-list-model} object with
    the @fun{gtk-flow-box-bind-model} function for each item that gets added to
    the model.
  @end{short}
  @begin{pre}
 lambda (item)
  @end{pre}
  @begin[code]{table}
    @entry[item]{A pointer to the item from the model for which to create a
       widget for.}
    @entry[Returns]{A @class{gtk-widget} object that represents @arg{item}.}
  @end{table}
  @see-class{gtk-flow-box}
  @see-class{g-list-model}
  @see-class{gtk-widget}
  @see-function{gtk-flow-box-bind-model}")

#+gtk-3-18
(export 'gtk-flow-box-create-widget-func)


;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_bind_model ()
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defcfun ("gtk_flow_box_bind_model" %gtk-flow-box-bind-model) :void
  (flowbox (g-object gtk-flow-box))
  (model (g-object g-list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

#+gtk-3-18
(defun gtk-flow-box-bind-model (flowbox model func)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[flowbox]{a @class{gtk-flow-box} widget}
  @argument[model]{a @class{g-list-model} object to be bound to @arg{flowbox}}
  @argument[func]{a @symbol{gtk-flow-box-create-widget-func} callback function
    that creates widgets for items}
  @begin{short}
    Binds a model to the flow box.
  @end{short}
  If the flow box was already bound to a model, that previous binding is
  destroyed.

  The contents of the flow box are cleared and then filled with widgets that
  represent items from the model. The flow box is updated whenever the model
  changes. If the @arg{model} argument is @code{nil}, the flow box is left
  empty.

  It is undefined to add or remove widgets directly, for example, with the
  @fun{gtk-flow-box-insert} or @fun{gtk-container-add} functions, while the
  flow box is bound to a model.

  Note that using a model is incompatible with the filtering and sorting
  functionality in the flow box. When using a model, filtering and sorting
  should be implemented by the model.
  @see-class{gtk-flow-box}
  @see-class{g-list-model}
  @see-symbol{gtk-flow-box-create-widget-func}"
  (%gtk-flow-box-bind-model flowbox
                            model
                            (callback gtk-flow-box-create-widget-func)
                            (allocate-stable-pointer func)
                            (callback stable-pointer-destroy-notify)))

#+gtk-3-18
(export 'gtk-flow-box-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-flow-box-child-new))

(defun gtk-flow-box-child-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @return{A new @class{gtk-flow-box-child} widget.}
  @begin{short}
    Creates a new @class{gtk-flow-box-child} widget, to be used as a child
    widget of a @class{gtk-flow-box} widget.
  @end{short}
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}"
  (make-instance 'gtk-flow-box-child))

(export 'gtk-flow-box-child-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_get_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_child_get_index" gtk-flow-box-child-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @return{An integer with the index of the child, or -1 if the child is not
    in a flow box.}
  @begin{short}
    Gets the current index of the child widget in its flow box.
  @end{short}
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}"
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-child-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_child_is_selected" gtk-flow-box-child-is-selected)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @return{@em{True} if @arg{child} is selected.}
  @begin{short}
    Returns whether the child widget is currently selected in its flow box.
  @end{short}
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}"
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-child-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_child_changed" gtk-flow-box-child-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-6}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @begin{short}
    Marks the child widget as changed, causing any state that depends on this
    to be updated.
  @end{short}
  This affects sorting and filtering.

  Note that calls to this method must be in sync with the data used for the
  sorting and filtering functions. For instance, if the list is mirroring some
  external data set, and *two* children changed in the external data set when
  you call the @sym{gtk-flow-box-child-changed} function on the first child
  widget, the sort function must only read the new data for the first of the
  two changed children, otherwise the resorting of the children will be wrong.

  This generally means that if you do not fully control the data model, you
  have to duplicate the data that affects the sorting and filtering functions
  into the widgets themselves. Another alternative is to call the
  @fun{gtk-flow-box-invalidate-sort} function on any model change, but that is
  more expensive.
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}
  @see-function{gtk-flow-box-invalidate-sort}"
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-child-changed)

;;; --- End of file gtk.flow-box.lisp ------------------------------------------
