;;; ----------------------------------------------------------------------------
;;; gtk.popover.lisp
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
;;; GtkPopover
;;;
;;;     Context dependent bubbles
;;;
;;; Types and Values
;;;
;;;     GtkPopover
;;;     GtkPopoverConstraint
;;;
;;; Functions
;;;
;;;     gtk_popover_new
;;;     gtk_popover_new_from_model
;;;     gtk_popover_bind_model
;;;     gtk_popover_popup
;;;     gtk_popover_popdown
;;;     gtk_popover_set_relative_to                        Accessor
;;;     gtk_popover_get_relative_to                        Accessor
;;;     gtk_popover_set_pointing_to                        Accessor
;;;     gtk_popover_get_pointing_to                        Accessor
;;;     gtk_popover_set_position                           Accessor
;;;     gtk_popover_get_position                           Accessor
;;;     gtk_popover_set_constrain_to                       Accessor
;;;     gtk_popover_get_constrain_to                       Accessor
;;;     gtk_popover_set_modal                              Accessor
;;;     gtk_popover_get_modal                              Accessor
;;;     gtk_popover_set_transitions_enabled                Accessor
;;;     gtk_popover_get_transitions_enabled                Accessor
;;;     gtk_popover_set_default_widget
;;;     gtk_popover_get_default_widget
;;;
;;; Properties
;;;
;;;     GtkPopoverConstraint    constrain-to           Read / Write
;;;                 gboolean    modal                  Read / Write
;;;             GdkRectangle*   pointing-to            Read / Write
;;;          GtkPositionType    position               Read / Write
;;;                GtkWidget*   relative-to            Read / Write
;;;                 gboolean    transitions-enabled    Read / Write
;;;
;;; Signals
;;;
;;;                     void    closed                 Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkPopover
;;;                         ╰── GtkPopoverMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPopover implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPopoverConstraint
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(define-g-enum "GtkPopoverConstraint" gtk-popover-constraint
  (:export t
   :type-initializer "gtk_popover_constraint_get_type")
  :none
  :window)

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-popover-constraint atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-popover-constraint atdoc:*external-symbols*)
 "@version{2021-12-25}
  @begin{short}
    Describes constraints to positioning of popovers.
  @end{short}
  More values may be added to this enumeration in the future.
  @begin{pre}
(define-g-enum \"GtkPopoverConstraint\" gtk-popover-constraint
  (:export t
   :type-initializer \"gtk_popover_constraint_get_type\")
  :none
  :window)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not constrain the popover position beyond what is imposed
      by the implementation.}
    @entry[:window]{Constrain the popover to the boundaries of the window that
      it is attached to.}
  @end{table}
  Since 3.20
  @see-class{gtk-popover}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPopover
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPopover" 'gtk-popover))

(define-g-object-class "GtkPopover" gtk-popover
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_popover_get_type")
  (#+gtk-3-20
   (constrain-to
    gtk-popover-constrain-to
    "constrain-to" "GtkPopoverConstraint" t t)
   (modal
    gtk-popover-modal
    "modal" "gboolean" t t)
   (pointing-to
    gtk-popover-pointing-to
    "pointing-to" "GdkRectangle" t t)
   (position
    gtk-popover-position
    "position" "GtkPositionType" t t)
   (relative-to
    gtk-popover-relative-to
    "relative-to" "GtkWidget" t t)
   (transitions-enabled
    gtk-popover-transitions-enabled
    "transitions-enabled" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-popover 'type)
 "@version{2021-12-25}
  @begin{short}
    A @sym{gtk-popover} widget is a bubble-like context window, primarily meant
    to provide context-dependent information or options.
  @end{short}
  Popovers are attached to a widget, passed at construction time on the
  @fun{gtk-popover-new} function, or updated afterwards through the
  @fun{gtk-popover-relative-to} function, by default they will point to the
  whole widget area, although this behavior can be changed through the
  @fun{gtk-popover-pointing-to} function.

  The position of a popover relative to the widget it is attached to can also
  be changed through the @fun{gtk-popover-position} function.

  By default, the @sym{gtk-popover} widget performs a GTK grab, in order to
  ensure input events get redirected to it while it is shown, and also so the
  popover is dismissed in the expected situations, clicks outside the popover,
  or the @kbd{Escape} key being pressed. If no such modal behavior is desired
  on a popover, the @fun{gtk-popover-modal} function may be called on it to
  tweak its behavior.

  @subheading{GtkPopover as menu replacement}
  A @sym{gtk-popover} widget is often used to replace menus. To facilitate this,
  it supports being populated from a @class{g-menu-model} object, using the
  @fun{gtk-popover-new-from-model} function. In addition to all the regular menu
  model features, this function supports rendering sections in the model in a
  more compact form, as a row of icon buttons instead of menu items.

  To use this rendering, set the \"display-hint\" attribute of the section to
  \"horizontal-buttons\" and set the icons of your items with the \"verb-icon\"
  attribute.
  @begin[Example]{dictionary}
    @begin{pre}
<section>
  <attribute name=\"display-hint\">horizontal-buttons</attribute>
  <item>
    <attribute name=\"label\">Cut</attribute>
    <attribute name=\"action\">app.cut</attribute>
    <attribute name=\"verb-icon\">edit-cut-symbolic</attribute>
  </item>
  <item>
    <attribute name=\"label\">Copy</attribute>
    <attribute name=\"action\">app.copy</attribute>
    <attribute name=\"verb-icon\">edit-copy-symbolic</attribute>
  </item>
  <item>
    <attribute name=\"label\">Paste</attribute>
    <attribute name=\"action\">app.paste</attribute>
    <attribute name=\"verb-icon\">edit-paste-symbolic</attribute>
  </item>
</section>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-popover} implementation has a single css node called
    @code{popover}. It always gets the @code{.background} style class and it
    gets the @code{.menu} style class if it is menu-like, e.g. a
    @class{gtk-popover-menu} widget or created using the
    @fun{gtk-popover-new-from-model} function.

    Particular uses of the @class{gtk-popover} widget, such as touch selection
    popups or magnifiers in @class{gtk-entry} or @class{gtk-text-view} widgets
    get style classes like @code{.touch-selection} or @code{.magnifier} to
    differentiate from plain popovers.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"closed\" signal}
      @begin{pre}
 lambda (popover)    :run-last
      @end{pre}
      The signal is emitted when the popover is dismissed either through API or
      user interaction.
      @begin[code]{table}
        @entry[popover]{The @sym{gtk-popover} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-popover-constrain-to}
  @see-slot{gtk-popover-modal}
  @see-slot{gtk-popover-pointing-to}
  @see-slot{gtk-popover-position}
  @see-slot{gtk-popover-relative-to}
  @see-slot{gtk-popover-transitions-enabled}
  @see-class{gtk-popover-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-popover-constrain-to -----------------------------------------------

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "constrain-to" 'gtk-popover) 't)
 "The @code{constrain-to} property of type @symbol{gtk-popover-constraint}
  (Read / Write) @br{}
  Sets a constraint for the popover position. Since 3.20 @br{}
  Default value: @code{:window}")

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-popover-constrain-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-constrain-to 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-constrain-to object) => constraint}
  @syntax[]{(setf (gtk-popover-constrain-to object) constraint)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[constraint]{a value of the @symbol{gtk-popover-constraint}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-popover]{constrain-to} slot of the
    @class{gtk-popover} class.
  @end{short}

  The @sym{gtk-popover-constrain-to} slot access function returns the constraint
  for placing this popover. The @sym{(setf gtk-popover-constrain-to)} slot
  access function sets a constraint for positioning this popover.

  Note that not all platforms support placing popovers freely, and may already
  impose constraints.

  Since 3.20
  @see-class{gtk-popover}
  @see-symbol{gtk-popover-constraint}")

;;; --- gtk-popover-modal ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "modal" 'gtk-popover) 't)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Sets whether the popover is modal, so other elements in the window do not
  receive input while the popover is visible. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-modal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-modal 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-modal object) => modal}
  @syntax[]{(setf (gtk-popover-modal object) modal)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[modal]{a boolean, that is @em{true} to make the popover claim all
    input within the toplevel}
  @begin{short}
    Accessor of the @slot[gtk-popover]{modal} slot of the @class{gtk-popover}
    class.
  @end{short}

  The @sym{gtk-popover-modal} slot access function returns whether the popover
  is modal. The @sym{(setf gtk-popover-modal)} slot access function sets whether
  the popover is modal, a modal popover will grab all input within the toplevel
  and grab the keyboard focus on it when being displayed. Clicking outside the
  popover area or pressing the @kbd{Escape} key will dismiss the popover and
  ungrab input.
  @see-class{gtk-popover}")

;;; --- gtk-popover-pointing-to ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pointing-to" 'gtk-popover) 't)
 "The @code{pointing-to} property of type @class{gdk-rectangle} (Read / Write)
  @br{}
  Marks a specific rectangle to be pointed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-pointing-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-pointing-to 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-pointing-to object) => rect}
  @syntax[]{(setf (gtk-popover-pointing-to object) rect)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[rect]{a @class{gdk-rectangle} instance to point to}
  @begin{short}
    Accessor of the @slot[gtk-popover]{pointing-to} slot of the
    @class{gtk-popover} class.
  @end{short}

  Sets the rectangle that the popover will point to, in the coordinate space of
  the widget the popover is attached to, see the @fun{gtk-popover-relative-to}
  function.
  @see-class{gtk-popover}
  @see-class{gdk-rectangle}
  @see-function{gtk-popover-relative-to}")

;;; --- gtk-popover-position ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "position" 'gtk-popover) 't)
 "The @code{position} property of type @symbol{gtk-position-type} (Read / Write)
  @br{}
  Sets the preferred position of the popover. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-position 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-pointing-to object) => position}
  @syntax[]{(setf (gtk-popover-pointing-to object) position)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[position]{a @symbol{gtk-position-type} value with the preferred
    popover position}
  @begin{short}
    Accessor of the @slot[gtk-popover]{position} slot of the
    @class{gtk-popover} class.
  @end{short}

  The @sym{gtk-popover-position} slot access function returns the preferred
  position of the popover. The @sym{(setf gtk-popover-position)} slot access
  function sets the preferred position for the popover to appear. If the popover
  is currently visible, it will be immediately updated.

  This preference will be respected where possible, although on lack of space,
  e.g. if close to the window edges, the @class{gtk-popover} widget may choose
  to appear on the opposite side.
  @see-class{gtk-popover}
  @see-symbol{gtk-position-type}")

;;; --- gtk-popover-relative-to ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "relative-to" 'gtk-popover) 't)
 "The @code{relative-to} property of type @class{gtk-widget} (Read / Write)
  @br{}
  Sets the attached widget.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-relative-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-relative-to 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-relative-to object) => relative-to}
  @syntax[]{(setf (gtk-popover-relative-to object) relative-to)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[relative-to]{a @class{gtk-widget} object}
  @begin{short}
    Accessor of the @slot[gtk-popover]{relative-to} slot of the
    @class{gtk-popover} class.
  @end{short}

  The @sym{gtk-popover-relative-to} slot access function returns the widget
  the popover is currently attached to. The @sym{(setf gtk-popover-relative-to)}
  slot access function sets a new widget to be attached to the popover. If the
  popover is visible, the position will be updated.
  @begin[Note]{dictionary}
    The ownership of popovers is always given to their @arg{relative-to}
    widget, so if the @arg{relative-to} argument is set to @code{nil} on an
    attached popover, it will be detached from its previous widget, and
    consequently destroyed unless extra references are kept.
  @end{dictionary}
  @see-class{gtk-popover}
  @see-class{gtk-widget}")

;;; --- gtk-popover-transitions-enabled ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transitions-enabled"
                                               'gtk-popover) 't)
 "The @code{transitions-enabled} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether show/hide transitions are enabled for this popover. @br{}
  @em{Warning:} The @code{transitions-enabled} property has been deprecated
  since version 3.22 and should not be used in newly written code. You can show
  or hide the popover without transitions using the @fun{gtk-widget-show} and
  @fun{gtk-widget-hide} functions while the @fun{gtk-popover-popup} and
  @fun{gtk-popover-popdown} functions will use transitions. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-popover-transitions-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-transitions-enabled 'function)
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-transitions-enabled object) => enabled}
  @syntax[]{(setf (gtk-popover-transitions-enabled object) enabled)}
  @argument[object]{a @class{gtk-popover} widget}
  @argument[enabled]{a boolean whether transitions are enabled}
  @begin{short}
    Accessor of the @slot[gtk-popover]{transitions-enabled} slot of the
    @class{gtk-popover} class.
  @end{short}

  The @sym{gtk-popover-transitions-enabled} slot access function returns whether
  show/hide transitions are enabled on this popover. The
  @sym{(setf gtk-popover-transitions-enabled)} slot access function sets whether
  show/hide transitions are enabled on this popover.
  @begin[Warning]{dictionary}
    The @sym{gtk-popover-transitions-enabled} slot access function has been
    deprecated since version 3.22 and should not be used in newly written code.
    You can show or hide the popover without transitions using the
    @fun{gtk-widget-show} and @fun{gtk-widget-hide} functions while the
    @fun{gtk-popover-popup} and @fun{gtk-popover-popdown} functions will use
    transitions.
  @end{dictionary}
  @see-class{gtk-popover}
  @see-function{gtk-widget-show}
  @see-function{gtk-widget-hide}
  @see-function{gtk-popover-popup}
  @see-function{gtk-popover-popdown}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-popover-new))

(defun gtk-popover-new (relative-to)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @argument[relative-to]{a @class{gtk-widget} the popover is related to}
  @return{A new @class{gtk-popover} widget.}
  @short{Creates a new popover to point to @arg{relative-to}.}
  @see-class{gtk-popover}
  @see-class{gtk-widget}"
  (make-instance 'gtk-popover
                 :relative-to relative-to))

(export 'gtk-popover-new)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new_from_model ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_popover_new_from_model" gtk-popover-new-from-model)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @argument[relative-to]{a @class{gtk-widget} the popover is related to}
  @argument[model]{a @class{g-menu-model} object}
  @return{A new @class{gtk-popover} widget.}
  @begin{short}
    Creates a popover and populates it according to @arg{model}.
  @end{short}
  The popover is pointed to the @arg{relative-to} widget.

  The created buttons are connected to actions found in the
  @class{gtk-application-window} widget to which the popover belongs - typically
  by means of being attached to a widget that is contained within the
  @class{gtk-application-window} widget hierarchy.

  Actions can also be added using the @fun{gtk-widget-insert-action-group}
  function on the menus attach widget or on any of its parent widgets.
  @see-class{gtk-popover}
  @see-class{gtk-widget}
  @see-class{gtk-application-window}
  @see-class{g-menu-model}
  @see-function{gtk-widget-insert-action-group}"
  (relative-to (g-object gtk-widget))
  (model (g-object g-menu-model)))

(export 'gtk-popover-new-from-model)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_bind_model ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_popover_bind_model" gtk-popover-bind-model) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @argument[popover]{a @class{gtk-popover} widget}
  @argument[model]{a @class{g-menu-model} object to bind to or @code{nil} to
    remove the binding}
  @argument[namespace]{a string with the namespace for actions in @arg{model}}
  @begin{short}
    Establishes a binding between a popover and a menu model.
  @end{short}

  The contents of the popover are removed and then refilled with menu items
  according to the menu model. When the menu model changes, the popover is
  updated. Calling this function twice on the popover with different menu model
  will cause the first binding to be replaced with a binding to the new model.
  If the model is @code{nil} then any previous binding is undone and all
  children are removed.

  If the @arg{namespace} argument is non-@code{nil} then the effect is as if all
  actions mentioned in the model have their names prefixed with the namespace,
  plus a dot. For example, if the action \"quit\" is mentioned and the
  @arg{namespace} argument is \"app\" then the effective action name is
  \"app.quit\".

  This function uses the @class{gtk-actionable} interface to define the action
  name and target values on the created menu items. If you want to use an action
  group other than \"app\" and \"win\", or if you want to use a
  @class{gtk-menu-shell} widget outside of a @class{gtk-application-window}
  widget, then you will need to attach your own action group to the widget
  hierarchy using the @fun{gtk-widget-insert-action-group} function. As an
  example, if you created a group with a \"quit\" action and inserted it with
  the name \"mygroup\" then you would use the action name \"mygroup.quit\" in
  your @class{g-menu-model} object.
  @see-class{gtk-popover}
  @see-class{g-menu-model}
  @see-class{gtk-actionable}
  @see-class{gtk-menu-shell}
  @see-class{gtk-application-window}
  @see-function{gtk-widget-insert-action-group}"
  (popover (g-object gtk-popover))
  (model (g-object g-menu-model))
  (namespace :string))

(export 'gtk-popover-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popup ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_popover_popup" gtk-popover-popup) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @argument[popover]{a @class{gtk-popover} widget}
  @begin{short}
    Pops the popover up.
  @end{short}
  This is different than a @fun{gtk-widget-show} call in that it shows the
  popover with a transition. If you want to show the popover without a
  transition, use the @fun{gtk-widget-show} function.

  Since 3.22
  @see-class{gtk-popover}
  @see-function{gtk-widget-show}"
  (popover (g-object gtk-popover)))

#+gtk-3-22
(export 'gtk-popover-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popdown ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gtk_popover_popdown" gtk-popover-popdown) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @argument[popover]{a @class{gtk-popover} widget}
  @begin{short}
    Pops the popover down.
  @end{short}
  This is different than a @fun{gtk-widget-hide} call in that it shows the
  popover with a transition. If you want to hide the popover without a
  transition, use the @fun{gtk-widget-hide} function.

  Since 3.22
  @see-class{gtk-popover}
  @see-function{gtk-widget-hide}"
  (popover (g-object gtk-popover)))

#+gtk-3-22
(export 'gtk-popover-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_default_widget ()
;;; gtk_popover_set_default_widget () -> gtk-popover-default-widget
;;; ----------------------------------------------------------------------------

#+gtk-3-18
(defun (setf gtk-popover-default-widget) (widget popover)
  (foreign-funcall "gtk_popover_set_default_widget"
                   (g-object gtk-popover) popover
                   (g-object gtk-widget) widget
                   :void)
  widget)

#+gtk-3-18
(defcfun ("gtk_popover_get_default_widget" gtk-popover-default-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-25}
  @syntax[]{(gtk-popover-default-widget popover) => widget}
  @syntax[]{(setf (gtk-popover-default-widget popover) widget)}
  @argument[popover]{a @class{gtk-popover} widget}
  @argument[widget]{a default @class{gtk-widget} object, or @code{nil}}
  @begin{short}
    Accessor of the default widget.
  @end{short}

  The @sym{gtk-popover-default-widget} function gets the widget that should be
  set as the default while the popover is shown. The
  @sym{(setf gtk-popover-default-widget)} function sets the widget that should
  be set as default widget while the popover is shown.

  The @class{gtk-popover} widget remembers the previous default widget and
  reestablishes it when the popover is dismissed.

  Since 3.18
  @see-class{gtk-popover}
  @see-class{gtk-widget}"
  (popover (g-object gtk-popover)))

#+gtk-3-18
(export 'gtk-popover-default-widget)

;;; --- End of file gtk.popover.lisp -------------------------------------------
