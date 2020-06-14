;;; ----------------------------------------------------------------------------
;;; gtk.flow-box.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
 "@version{2020-5-8}
  @begin{short}
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (child)
      @end{pre}
      The \"activate\" signal is emitted when the user activates a child widget
      in a @class{gtk-flow-box}, either by clicking or double-clicking, or by
      using the Space or Enter key. While this signal is used as a keybinding
      signal, it can be used by applications for their own purposes. Since 3.12
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
 "@version{2020-5-8}
  @begin{short}
    A @sym{gtk-flow-box} positions child widgets in sequence according to its
    orientation.
  @end{short}

  @image[flow-box]{}

  For instance, with the horizontal orientation, the widgets will be arranged
  from left to right, starting a new row under the previous row when necessary.
  Reducing the width in this case will require more rows, so a larger height
  will be requested.

  Likewise, with the vertical orientation, the widgets will be arranged from
  top to bottom, starting a new column to the right when necessary. Reducing
  the height will require more columns, so a larger width will be requested.

  The size request of a @sym{gtk-flow-box} container alone may not be what you
  expect. If you need to be able to shrink it along both axes and dynamically
  reflow its children, you may have to wrap it in a @class{gtk-scrolled-window}
  widget to enable that.

  The children of a @sym{gtk-flow-box} container can be dynamically sorted and
  filtered.

  Although a @sym{gtk-flow-box} container must have only
  @class{gtk-flow-box-child} children, you can add any kind of widget to it via
  the function @fun{gtk-container-add}, and a @class{gtk-flow-box-child} widget
  will automatically be inserted between the box and the widget.

  Also see @class{gtk-list-box}. @sym{gtk-flow-box} was added in GTK+ 3.12.
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
    @sym{gtk-fow-box} uses a single CSS node with name @code{flowbox}.
    @class{gtk-flow-box-child} uses a single CSS node with name
    @code{flowboxchild}. For rubberband selection, a subnode with name
    @code{rubberband} is used.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-child\" signal}
      @begin{pre}
 lambda (flowbox)
      @end{pre}
      The \"activate-cursor-child\" signal is a keybinding signal which gets
      emitted when the user activates the flow box. Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
          is emitted.}
      @end{table}
    @subheading{The \"child-activated\" signal}
      @begin{pre}
 lambda (flowbox child)
      @end{pre}
      The \"child-activated signal\" is emitted when a child has been activated
      by the user. Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
          is emitted.}
        @entry[child]{The @class{gtk-flow-box-child} widget that is activated.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (flowbox step count)
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement. Applications should not connect to
      it, but may emit it with the function @fun{g-signal-emit-by-name} if they
      need to control the cursor programmatically.

      The default bindings for this signal come in two variants, the variant
      with the Shift modifier extends the selection, the variant without the
      Shift modifer does not. There are too many key combinations to list them
      all here. Arrow keys move by individual children. Home/End keys move to
      the ends of the box. PageUp/PageDown keys move vertically by pages.
      Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
          is emitted.}
        @entry[step]{The granularity to the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (flowbox)
      @end{pre}
      The \"select-all\" signal is a keybinding signal which gets emitted to
      select all children of the box, if the selection mode permits it. The
      default bindings for this signal is Ctrl-a. Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
        is emitted.}
      @end{table}
    @subheading{The \"selected-children-changed\" signal}
      @begin{pre}
 lambda (flowbox)
      @end{pre}
      The \"selected-children-changed\" signal is emitted when the set of
      selected children changes. Use the functions
      @fun{gtk-flow-box-selected-foreach} or
      @fun{gtk-flow-box-selected-children} to obtain the selected children.
      Since 3.12
      @begin[code]{table}
        @entry[box]{The @sym{gtk-flow-box} on which the signal is emitted.}
      @end{table}
    @subheading{The \"toggle-cursor-child\" signal}
      @begin{pre}
 lambda (flowbox)
      @end{pre}
      The \"toggle-cursor-child\" signal is a keybinding signal which toggles
      the selection of the child that has the focus. The default binding for
      this signal is Ctrl-Space. Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
        is emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (flowbox)
      @end{pre}
      The \"unselect-all\" signal is a keybinding signal which gets emitted to
      unselect all children of the box, if the selection mode permits it. The
      default bindings for this signal is Ctrl-Shift-a. Since 3.12
      @begin[code]{table}
        @entry[flowbox]{The @sym{gtk-flow-box} container on which the signal
        is emitted.}
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
  a double-click. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-activate-on-single-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-activate-on-single-click 'function)
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-acivate-on-click object) => single}
  @syntax[]{(setf (gtk-flow-box-activate-on-click object) single)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[single]{@em{false} to emit the \"child-activated\" signal on
    a single click}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{activate-on-single-click} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-activate-on-single-click} returns
  whether children activate on single clicks.

  If @arg{single} is @em{true}, children will be activated when you click on
  them, otherwise you need to double-click.

  Since 3.12
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
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-column-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-column-spacing object) spacing)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[spacing]{An unsigned integer with the spacing to use.}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{column-spacing} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-column-spacing} gets the
  horizontal spacing. The slot access function
  @sym{(setf gtk-flow-box-column-spacing)} sets the horizontal space to add
  between children.

  Since 3.12
  @see-class{gtk-flow-box}")

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
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-flow-box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false} for
    variable allotments}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{homogeneous} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-column-spacing} returns whether the
  box is homogeneous (all children are the same size). The slot access function
  @sym{(setf gtk-flow-box-column-spacing)} sets the @code{homogeneous} property
  of the flow box, controlling whether or not all children of box are given
  equal space in the box.

  Since 3.12
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
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-max-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-max-chilren-per-line object) n-children)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[n-children]{an unsigned integer with the maximum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{max-children-per-line} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-max-children-per-llne} gets the
  maximum number of children per line. The slot access function
  @sym{(setf gtk-flow-box-max-children-per-line)} sets the maximum number of
  children to request and allocate space for in the orientation of the flow box.

  Setting the maximum number of children per line limits the overall natural
  size request to be no more than @arg{n-children} children long in the given
  orientation.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; --- gtk-flow-box-min-children-per-line -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-children-per-line"
                                               'gtk-flow-box) 't)
 "The @code{min-children-per-line} property of type @code{:uint}
  (Read / Write) @br{}
  The minimum number of children to allocate consecutively in the given
  orientation. Setting the minimum children per line ensures that a reasonably
  small height will be requested for the overall minimum width of the box. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-flow-box-min-children-per-line atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-flow-box-min-children-per-line 'function)
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-min-children-per-line object) => n-children}
  @syntax[]{(setf (gtk-flow-box-min-chilren-per-line object) n-children)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[n-children]{an unsigned integer with the minimum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{min-children-per-line} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-max-children-per-llne} gets the
  minimum number of children per line. The slot access function
  @sym{(setf gtk-flow-box-max-children-per-line)} sets the minimum number of
  children to line up in the orientation of the flow box before flowing.

  Since 3.12
  @see-class{gtk-flow-box}")

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
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-row-spacing object) => spacing}
  @syntax[]{(setf (gtk-flow-box-row-spacing object) spacing)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[spacing]{an unsigned integer with the spacing to use}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{row-spacing} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-max-children-per-llne} gets the
  vertical spacing. The slot access function
  @sym{(setf gtk-flow-box-max-children-per-line)} sets the vertical space to
  add between children.

  Since 3.12
  @see-class{gtk-flow-box}")

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
 "@version{2020-5-8}
  @syntax[]{(gtk-flow-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk-flow-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk-flow-box} container}
  @argument[mode]{the selection mode of type @symbol{gtk-selection-mode}}
  @begin{short}
    Accessor of the @slot[gtk-flow-box]{selection-mode} slot of the
    @class{gtk-flow-box} class.
  @end{short}

  The slot access function @sym{gtk-flow-box-selection-mode} gets the selection
  mode of the flow box. The slot access function
  @sym{(setf gtk-flow-box-selection-mode)} sets how selection works in the flow
  box.

  Since 3.12
  @see-class{gtk-flow-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-flow-box-new))

(defun gtk-flow-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @return{A new @class{gtk-flow-box} container.}
  @begin{short}
    Creates a new flow box container.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (make-instance 'gtk-flow-box))

(export 'gtk-flow-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_insert" gtk-flow-box-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[child]{a @class{gtk-widget} child to add}
  @argument[position]{an integer with the position to insert the child in}
  @begin{short}
    Inserts the child into the flow box at a given position.
  @end{short}

  If a sort function is set, the widget will actually be inserted at the
  calculated position and this function has the same effect as the function
  @fun{gtk-container-add}.

  If @arg{position} is -1, or larger than the total number of children in the
  flow box, then the widget will be appended to the end.

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box))
  (widget (g-object gtk-widget))
  (position :int))

(export 'gtk-flow-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_index () -> gtk-flow-box-child-at-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_get_child_at_index" gtk-flow-box-child-at-index)
    (g-object gtk-flow-box-child)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[index]{an integer with the position of the child}
  @return{The child widget, which will always be a @class{gtk-flow-box-child}
    widget or @code{nil} in case no child widget with the given index exists.}
  @begin{short}
    Gets the nth child in the flow box.
  @end{short}

  Since 3.12
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
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[x]{an integer with the x coordinate of the child}
  @argument[y]{an integer with the y coordinate of the child}
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
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be adjusted
    when the focus is moved among the descendents of container}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the function @fun{gtk-scrolled-window-hadjustment} for a typical way of
  obtaining the adjustment, and the function @fun{gtk-flow-box-vadjustment} for
  setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the box.

  Since 3.12
  @see-class{gtk-flow-box}
  @see-function{gtk-scrolled-window-hadjustment}"
  (flowbox (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-flow-box-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_vadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_vadjustment" gtk-flow-box-set-vadjustment) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[adjustment]{a @class{gtk-adjustment} object which should be adjusted
    when the focus is moved among the descendents of container}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the function @fun{gtk-scrolled-window-vadjustment} for a typical way of
  obtaining the adjustment, and the function @fun{gtk-flow-box-hadjustment} for
  setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the box.

  Since 3.12
  @see-class{gtk-flow-box}
  @see-function{gtk-scrolled-window-vadjustment}"
  (flowbox (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-flow-box-set-vadjustment)

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

(defcallback gtk-flow-box-foreach-func-cb :void
    ((flowbox (g-object gtk-flow-box))
     (child (g-object gtk-flow-box-child))
     (data :pointer))
  (restart-case
      (let ((ptr (get-stable-pointer-value data)))
        (funcall ptr flowbox child))
    (return () :report "Error in GtkFlowBoxForeachFunc callback." nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_selected_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_selected_foreach" %gtk-flow-box-selected-foreach) :void
  (flowbox (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer))

(defun gtk-flow-box-selected-foreach (flowbox func)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[func]{a Lisp function which is passed as a callback}
  @begin{short}
    Calls a function for each selected child in the flow box.
  @end{short}

  Note that the selection cannot be modified from within this function.

  Since 3.12
  @see-class{gtk-flow-box}"
  (with-stable-pointer (ptr func)
    (%gtk-flow-box-selected-foreach flowbox
                                    (callback gtk-flow-box-foreach-func-cb)
                                    ptr)))

(export 'gtk-flow-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_selected_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_get_selected_children" gtk-flow-box-selected-children)
    (g-list (g-object gtk-widget))
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @return{A list containing the @class{gtk-widget} for each selected child.}
  @begin{short}
    Creates a list of all selected children.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-selected-children)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_select_child" gtk-flow-box-select-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[child]{a @class{gtk-widget} child of the flow box}
  @begin{short}
    Selects a single child of the flow box, if the selection mode allows it.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-select-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_unselect_child" gtk-flow-box-unselect-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[child]{a @class{gtk-widget} child of the flow box}
  @begin{short}
    Unselects a single child of the flow box, if the selection mode allows it.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-unselect-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_select_all" gtk-flow-box-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @begin{short}
    Select all children of the flow box, if the selection mode allows it.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_unselect_all" gtk-flow-box-unselect-all) :void

 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @begin{short}
    Unselect all children of the flow box, if the selection mode allows it.
  @end{short}

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-unselect-all)

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

(defcallback gtk-flow-box-filter-func-cb :boolean
    ((child (g-object gtk-flow-box-child))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr child)))

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_filter_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_filter_func" %gtk-flow-box-set-filter-func) :void
  (flowbox (g-object gtk-flow-box))
  (filter-func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-flow-box-set-filter-func (flowbox filter-func)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[filter-func]{callback that lets you filter which children to show}
  @begin{short}
    By setting a filter function on the flow box one can decide dynamically
    which of the children to show.
  @end{short}
  For instance, to implement a search function that only shows the children
  matching the search terms.

  The @arg{filter-func} function will be called for each child after the call,
  and it will continue to be called each time a child changes (via the function
  @fun{gtk-flow-box-child-changed}) or when the function
  @fun{gtk-flow-box-invalidate-filter} is called.

  Note that using a filter function is incompatible with using a model. See
  the function @fun{gtk-flow-box-bind-model}.

  Since 3.12
  @see-class{gtk-flow-box}"
  (%gtk-flow-box-set-filter-func flowbox
                                 (callback gtk-flow-box-filter-func-cb)
                                 (allocate-stable-pointer filter-func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-flow-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_invalidate_filter" gtk-flow-box-invalidate-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @begin{short}
    Updates the filtering for all children in the flow box.
  @end{short}

  Call this function when the result of the filter function on the flow box is
  changed due ot an external factor. For instance, this would be used if the
  filter function just looked for a specific search term, and the entry with
  the string has changed.

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-invalidate-filter)

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

(defcallback gtk-flow-box-sort-func-cb :int
    ((child1 (g-object gtk-flow-box-child))
     (child2 (g-object gtk-flow-box-child))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr child1 child2)))

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_set_sort_func" %gtk-flow-box-set-sort-func) :void
  (flowbox (g-object gtk-flow-box))
  (sort-func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-flow-box-set-sort-func (flowbox sort-func)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @argument[sort-func]{callback for the sort function}
  @begin{short}
    By setting a sort function on the flow box, one can dynamically reorder the
    children of the flow box, based on the contents of the children.
  @end{short}

  The @arg{sort-func} function will be called for each child after the call,
  and will continue to be called each time a child changes (via the function
  @fun{gtk-flow-box-child-changed}) and when the function
  @fun{gtk-flow-box-invalidate-sort} is called.

  Note that using a sort function is incompatible with using a model. See
  the function @fun{gtk-flow-box-bind-model}.

  Since 3.12
  @see-class{gtk-flow-box}"
  (%gtk-flow-box-set-sort-func flowbox
                               (callback gtk-flow-box-sort-func-cb)
                               (allocate-stable-pointer sort-func)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-flow-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_sort ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_invalidate_sort" gtk-flow-box-invalidate-sort) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[flowbox]{a @class{gtk-flow-box} container}
  @begin{short}
    Updates the sorting for all children in the flow box.
  @end{short}

  Call this when the result of the sort function on the flow box is changed due
  to an external factor.

  Since 3.12
  @see-class{gtk-flow-box}"
  (flowbox (g-object gtk-flow-box)))

(export 'gtk-flow-box-invalidate-sort)

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

;; TODO: GListModel ist not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-flow-box-child-new))

(defun gtk-flow-box-child-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @return{A new @class{gtk-flow-box-child} widget.}
  @begin{short}
    Creates a new @class{gtk-flow-box-child} widget, to be used as a child of a
    @class{gtk-flow-box} container.
  @end{short}

  Since 3.12
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}"
  (make-instance 'gtk-flow-box-child))

(export 'gtk-flow-box-child-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_get_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_child_get_index" gtk-flow-box-child-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @return{An integer with the index of the child, or -1 if the child is not
    in a flow box.}
  @begin{short}
    Gets the current index of the child in its @class{gtk-flow-box} container.
  @end{short}

  Since 3.12
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
 "@version{2020-5-8}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @return{@em{True} if @arg{child} is selected.}
  @begin{short}
    Returns whether the child is currently selected in its @class{gtk-flow-box}
    container.
  @end{short}

  Since 3.12
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}"
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-child-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_flow_box_child_changed" gtk-flow-box-child-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-8}
  @argument[child]{a @class{gtk-flow-box-child} widget}
  @begin{short}
    Marks child as changed, causing any state that depends on this to be
    updated.
  @end{short}
  This affects sorting and filtering.

  Note that calls to this method must be in sync with the data used for the
  sorting and filtering functions. For instance, if the list is mirroring some
  external data set, and *two* children changed in the external data set when
  you call @sym{gtk-flow-box-child-changed} on the first child, the sort
  function must only read the new data for the first of the two changed
  children, otherwise the resorting of the children will be wrong.

  This generally means that if you do not fully control the data model, you
  have to duplicate the data that affects the sorting and filtering functions
  into the widgets themselves. Another alternative is to call
  @fun{gtk-flow-box-invalidate-sort} on any model change, but that is more
  expensive.

  Since 3.12
  @see-class{gtk-flox-box-child}
  @see-class{gtk-flox-box}
  @see-function{gtk-flow-box-invalidate-sort}"
  (child (g-object gtk-flow-box-child)))

(export 'gtk-flow-box-child-changed)

;;; --- End of file gtk.flow-box.lisp ------------------------------------------
