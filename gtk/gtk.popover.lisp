;;; ----------------------------------------------------------------------------
;;; gtk.popover.lisp
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
;;;     GtkPopoverConstraint   constrain-to           Read / Write
;;;                 gboolean   modal                  Read / Write
;;;             GdkRectangle*  pointing-to            Read / Write
;;;          GtkPositionType   position               Read / Write
;;;                GtkWidget*  relative-to            Read / Write
;;;                 gboolean   transitions-enabled    Read / Write
;;;
;;; Signals
;;;
;;;                     void   closed                 Run Last
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
(setf (gethash 'gtk-popover-constraint atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-popover-constraint atdoc:*external-symbols*)
 "@version{2013-3-18}
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
    @entry[:none]{Don't constrain the popover position beyond what is imposed by
      the implementation.}
    @entry[:window]{Constrain the popover to the boundaries of the window that
      it is attached to.}
  @end{table}
  Since 3.20")

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
   #+gtk-3-16
   (transitions-enabled
    gtk-popover-transitions-enabled
    "transitions-enabled" "gboolean" t t)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-popover 'type)
 "@version{2019-4-5}
  @begin{short}
    A @sym{gtk-popover} widget is a bubble-like context window, primarily meant
    to provide context-dependent information or options.
  @end{short}
  Popovers are attached to a widget, passed at construction time on
  the @fun{gtk-popover-new} function, or updated afterwards through the
  @fun{gtk-popover-set-relative-to} function, by default they will point to the
  whole widget area, although this behavior can be changed through the
  @fun{gtk-popover-set-pointing-to} function.

  The position of a popover relative to the widget it is attached to can also be
  changed through the @fun{gtk-popover-set-position} function.

  By default, @sym{gtk-popover} performs a GTK+ grab, in order to ensure input
  events get redirected to it while it is shown, and also so the popover is
  dismissed in the expected situations, clicks outside the popover, or the Esc
  key being pressed. If no such modal behavior is desired on a popover, the
  @fun{gtk-popover-set-modal} function may be called on it to tweak its
  behavior.

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
    The @sym{gtk-popover} class has a single css node called @code{popover}. It
    always gets the @code{.background} style class and it gets the @code{.menu}
    style class if it is menu-like, e. g. a @class{gtk-popover-menu} widget or
    created using the @fun{gtk-popover-new-from-model} function.

    Particular uses of the @class{gtk-popover} widget, such as touch selection
    popups or magnifiers in @class{gtk-entry} or @class{gtk-text-view} widgets
    get style classes like @code{.touch-selection} or @code{.magnifier} to
    differentiate from plain popovers.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"closed\" signal}
      @begin{pre}
 lambda (popover)    : Run Last
      @end{pre}
      This signal is emitted when the popover is dismissed either through API or
      user interaction.
      @begin[code]{table}
        @entry[popover]{The @sym{gtk-popover} widget.}
      @end{table}
  @end{dictionary}
")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-popover-constrain-to -----------------------------------------------

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "constrain-to" 'gtk-popover) 't)
 "The @code{constrain-to} property of type @symbol{gtk-popover-constraint}
  (Read / Write) @br{}
  Sets a constraint for the popover position. @br{}
  Default value: @code{:window} @br{}
  Since 3.20")

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-popover-constrain-to atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-constrain-to 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{constrain-to} of the
    @class{gtk-popover} class.
  @end{short}

  Since 3.20
  @see-class{gtk-popover}")

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
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{modal} of the
    @class{gtk-popover} class.
  @end{short}
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
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{pointing-to} of the
    @class{gtk-popover} class.
  @end{short}
  @see-class{gtk-popover}")

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
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{position} of the
    @class{gtk-popover} class.
  @end{short}
  @see-class{gtk-popover}")

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
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{relative-to} of the
    @class{gtk-popover} class.
  @end{short}
  @see-class{gtk-popover}")

;;; --- gtk-popover-transitions-enabled ----------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "transitions-enabled"
                                               'gtk-popover) 't)
 "The @code{transitions-enabled} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether show/hide transitions are enabled for this popover.@br{}
  @b{Warning:} @code{transitions-enabled} has been deprecated since version 3.22
  and should not be used in newly-written code. You can show or hide the popover
  without transitions using the @fun{gtk-widget-show} and @fun{gtk-widget-hide}
  functions while the @fun{gtk-popover-popup} and @fun{gtk-popover-popdown}
  functions will use transitions. @br{}
  Default value: @em{true} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-popover-transitions-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-popover-transitions-enabled 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-popover]{transitions-enabled} of the
    @class{gtk-popover} class.
  @end{short}

  Since 3.16
  @see-class{gtk-popover}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new ()
;;;
;;; GtkWidget * gtk_popover_new (GtkWidget *relative_to);
;;;
;; Creates a new popover to point to relative_to
;;;
;;; relative_to :
;;;     GtkWidget the popover is related to.
;;;
;;; Returns ;
;;;     a new GtkPopover
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_new_from_model ()
;;;
;;; GtkWidget *
;;; gtk_popover_new_from_model (GtkWidget *relative_to,
;;;                             GMenuModel *model);
;;;
;;; Creates a GtkPopover and populates it according to model . The popover is
;;; pointed to the relative_to widget.
;;;
;;; The created buttons are connected to actions found in the
;;; GtkApplicationWindow to which the popover belongs - typically by means of
;;; being attached to a widget that is contained within the
;;; GtkApplicationWindows widget hierarchy.
;;;
;;; Actions can also be added using gtk_widget_insert_action_group() on the
;;; menus attach widget or on any of its parent widgets.
;;;
;;; relative_to :
;;;     GtkWidget the popover is related to.
;;;
;;; model :
;;;     a GMenuModel
;;; 
;;; Returns :
;;;     the new GtkPopover
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_bind_model ()
;;;
;;; void
;;; gtk_popover_bind_model (GtkPopover *popover,
;;;                         GMenuModel *model,
;;;                         const gchar *action_namespace);
;;;
;;; Establishes a binding between a GtkPopover and a GMenuModel.
;;;
;;; The contents of popover are removed and then refilled with menu items
;;; according to model . When model changes, popover is updated. Calling this
;;; function twice on popover with different model will cause the first binding
;;; to be replaced with a binding to the new model. If model is NULL then any
;;; previous binding is undone and all children are removed.
;;;
;;; If action_namespace is non-NULL then the effect is as if all actions
;;; mentioned in the model have their names prefixed with the namespace, plus a
;;; dot. For example, if the action “quit” is mentioned and action_namespace is
;;; “app” then the effective action name is “app.quit”.
;;;
;;; This function uses GtkActionable to define the action name and target values
;;; on the created menu items. If you want to use an action group other than
;;; “app” and “win”, or if you want to use a GtkMenuShell outside of a
;;; GtkApplicationWindow, then you will need to attach your own action group to
;;; the widget hierarchy using gtk_widget_insert_action_group(). As an example,
;;; if you created a group with a “quit” action and inserted it with the name
;;; “mygroup” then you would use the action name “mygroup.quit” in your
;;; GMenuModel.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; model :
;;;     the GMenuModel to bind to or NULL to remove binding.
;;;
;;; action_namespace :
;;;     the namespace for actions in model .
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popup ()
;;;
;;; void gtk_popover_popup (GtkPopover *popover);
;;;
;;; Pops popover up. This is different than a gtk_widget_show() call in that it
;;; shows the popover with a transition. If you want to show the popover without
;;; a transition, use gtk_widget_show().
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_popdown ()
;;;
;;; void gtk_popover_popdown (GtkPopover *popover);
;;;
;;; Pops popover down.This is different than a gtk_widget_hide() call in that it
;;; shows the popover with a transition. If you want to hide the popover without
;;; a transition, use gtk_widget_hide().
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Since: 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;; gtk_popover_set_relative_to ()
;;;
;;; void
;;; gtk_popover_set_relative_to (GtkPopover *popover,
;;;                              GtkWidget *relative_to);
;;;
;;; Sets a new widget to be attached to popover . If popover is visible, the
;;; position will be updated.
;;;
;;; Note: the ownership of popovers is always given to their relative_to widget,
;;; so if relative_to is set to NULL on an attached popover , it will be
;;; detached from its previous widget, and consequently destroyed unless extra
;;; references are kept.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; relative_to :
;;;     a GtkWidget.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_relative_to ()
;;;
;;; GtkWidget *
;;; gtk_popover_get_relative_to (GtkPopover *popover);
;;;
;;; Returns the widget popover is currently attached to
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     a GtkWidget.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_set_pointing_to ()
;;;
;;; void
;;; gtk_popover_set_pointing_to (GtkPopover *popover, const GdkRectangle *rect);
;;;
;;; Sets the rectangle that popover will point to, in the coordinate space of
;;; the widget popover is attached to, see gtk_popover_set_relative_to().
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; rect :
;;;     rectangle to point to
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_pointing_to ()
;;;
;;; gboolean
;;; gtk_popover_get_pointing_to (GtkPopover *popover,
;;;                              GdkRectangle *rect);
;;;
;;; If a rectangle to point to has been set, this function will return TRUE and
;;; fill in rect with such rectangle, otherwise it will return FALSE and fill in
;;; rect with the attached widget coordinates.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; rect :
;;;     location to store the rectangle.
;;;
;;; Returns :
;;;     TRUE if a rectangle to point to was set.
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;; gtk_popover_set_position ()
;;;
;;; void
;;; gtk_popover_set_position (GtkPopover *popover,
;;;                           GtkPositionType position);
;;;
;;; Sets the preferred position for popover to appear. If the popover is
;;; currently visible, it will be immediately updated.
;;;
;;; This preference will be respected where possible, although on lack of space,
;;; e. g. if close to the window edges, the GtkPopover may choose to appear on
;;; the opposite side
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; position :
;;;     preferred popover position
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_position ()
;;;
;;; GtkPositionType gtk_popover_get_position (GtkPopover *popover);
;;;
;;; Returns the preferred position of popover .
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     The preferred position.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_set_constrain_to ()
;;;
;;; void
;;; gtk_popover_set_constrain_to (GtkPopover *popover,
;;;                               GtkPopoverConstraint constraint);
;;;
;;; Sets a constraint for positioning this popover.
;;;
;;; Note that not all platforms support placing popovers freely, and may already
;;; impose constraints.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; constraint :
;;;     the new constraint
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_constrain_to ()
;;;
;;; GtkPopoverConstraint
;;; gtk_popover_get_constrain_to (GtkPopover *popover);
;;;
;;; Returns the constraint for placing this popover. See
;;; gtk_popover_set_constrain_to().
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     the constraint for placing this popover.
;;;
;;; Since: 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_set_modal ()
;;;
;;; void
;;; gtk_popover_set_modal (GtkPopover *popover,
;;;                        gboolean modal);
;;;
;;; Sets whether popover is modal, a modal popover will grab all input within
;;; the toplevel and grab the keyboard focus on it when being displayed.
;;; Clicking outside the popover area or pressing Esc will dismiss the popover
;;; and ungrab input.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; modal :
;;;     TRUE to make popover claim all input within the toplevel
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_modal ()
;;;
;;; gboolean
;;; gtk_popover_get_modal (GtkPopover *popover);
;;;
;;; Returns whether the popover is modal, see gtk_popover_set_modal to see the
;;; implications of this.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     TRUE if popover is modal
;;;
;;; Since: 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_set_transitions_enabled ()
;;;
;;; void
;;; gtk_popover_set_transitions_enabled (GtkPopover *popover,
;;;                                      gboolean transitions_enabled);
;;;
;;; gtk_popover_set_transitions_enabled has been deprecated since version 3.22
;;; and should not be used in newly-written code.
;;;
;;; You can show or hide the popover without transitions using gtk_widget_show()
;;; and gtk_widget_hide() while gtk_popover_popup() and gtk_popover_popdown()
;;; will use transitions.
;;;
;;; Sets whether show/hide transitions are enabled on this popover
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; transitions_enabled :
;;;     Whether transitions are enabled
;;;
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_transitions_enabled ()
;;;
;;; gboolean
;;; gtk_popover_get_transitions_enabled (GtkPopover *popover);
;;;
;;; gtk_popover_get_transitions_enabled has been deprecated since version 3.22
;;; and should not be used in newly-written code.
;;;
;;; You can show or hide the popover without transitions using gtk_widget_show()
;;; and gtk_widget_hide() while gtk_popover_popup() and gtk_popover_popdown()
;;; will use transitions.
;;;
;;; Returns whether show/hide transitions are enabled on this popover.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     TRUE if the show and hide transitions of the given popover are enabled,
;;;     FALSE otherwise.
;;;
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_set_default_widget ()
;;;
;;; void
;;; gtk_popover_set_default_widget (GtkPopover *popover,
;;;                                 GtkWidget *widget);
;;;
;;; Sets the widget that should be set as default widget while the popover is
;;; shown (see gtk_window_set_default()). GtkPopover remembers the previous
;;; default widget and reestablishes it when the popover is dismissed.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; widget :
;;;     the new default widget, or NULL.
;;;
;;; Since: 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_popover_get_default_widget ()
;;;
;;; GtkWidget *
;;; gtk_popover_get_default_widget (GtkPopover *popover);
;;;
;;; Gets the widget that should be set as the default while the popover is
;;; shown.
;;;
;;; popover :
;;;     a GtkPopover
;;;
;;; Returns :
;;;     the default widget, or NULL if there is none.
;;;
;;; Since: 3.18
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.popover.lisp -------------------------------------------
