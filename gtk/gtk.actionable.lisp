;;; ----------------------------------------------------------------------------
;;; gtk.actionable.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013, 2014 Dieter Kaiser
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
;;; GtkActionable
;;;
;;; An interface for widgets that can be associated with actions
;;;
;;; Synopsis
;;;
;;;    GtkActionable
;;;    GtkActionableInterface
;;;
;;;    gtk_actionable_get_action_name
;;;    gtk_actionable_set_action_name
;;;    gtk_actionable_get_action_target_value
;;;    gtk_actionable_set_action_target_value
;;;    gtk_actionable_set_action_target
;;;    gtk_actionable_set_detailed_action_name
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Interface gtk-actionable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActionable" gtk-actionable
  (:export t
   :type-initializer "gtk_actionable_get_type")
  (action-name
   gtk-actionable-action-name
   "action-name" "gchar" t t)
  (action-target
   gtk-actionable-action-target
   "action-target" "GVariant" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-actionable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-actionable 'type)
 "@version{2013-8-11}
  @begin{short}
    This interface provides a convenient way of associating widgets with
    actions on a @class{gtk-application-window} or @class{gtk-application}.
  @end{short}

  It primarily consists of two properties: @code{\"action-name\"} and
  @code{\"action-target\"}. There are also some convenience APIs for setting
  these properties.

  This interface is presently only meaningful if used on a widget that is, or
  will be, located inside of a @class{gtk-application-window} and can only be
  used to associate the widget with actions on that window, or its associated
  @class{gtk-application}.
  @see-slot{gtk-actionable-action-name}
  @see-slot{gtk-actionable-action-target}
  @see-class{gtk-application}
  @see-class{gtk-application-window}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-actionable-action-name ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-name"
                                               'gtk-actionable) 't)
 "The @code{\"action-name\"} property of type @code{:string}
  (Read / Write) @br{}
  The name of the associated action, like \"app.quit\". @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-actionable-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-actionable-action-name 'function)
 "@version{2014-7-19}
  @argument[object]{a @class{gtk-actionable} widget}
  @argument[action-name]{an action name, or @code{nil}}
  @syntax[]{(gtk-actionable-action-name object) => action-name}
  @syntax[]{(setf (gtk-actionable-action-name object) action-name)}
  @begin{short}
    Accessor of the slot @slot[gtk-actionable]{action-name} of the
    @class{gtk-actionable} inferface.
  @end{short}

  The generic function @sym{gtk-actionable-action-name} gets the action name
  for @arg{object}, or @code{nil} if none is set.

  The generic function @sym{(setf gtk-actionable-action-name)} specifies the
  name of the action with which this widget should be associated.

  If @arg{action-name} is @code{nil} then the widget will be unassociated from
  any previous action.

  Usually this function is used when the widget is located, or will be
  located, within the hierarchy of a @class{gtk-application-window}.

  Names are of the form \"win.save\" or \"app.quit\" for actions on the
  containing @class{gtk-application-window} or its associated
  @class{gtk-application}, respectively. This is the same form used for actions
  in the @class{g-menu} associated with the window.
  @begin[Example]{dictionary}
    @begin{pre}
 (let ((button (make-instance 'gtk-button))) 
   (setf (gtk-actionable-action-name button) \"win.save\")
     (gtk-actionable-action-name button))
 => \"win.save\"
    @end{pre}
  @end{dictionary}
  Since 3.4
  @see-class{gtk-actionable}
  @see-class{gtk-application}
  @see-class{gtk-application-window}
  @see-class{g-menu}")

;;; --- gtk-actionable-action-target -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-target"
                                               'gtk-actionable) 't)
 "The @code{\"action-target\"} property of type @type{g-variant}
  (Read / Write) @br{}
  The parameter for action invocations. @br{}
  Allowed values: a @type{g-variant} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-actionable-action-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-actionable-action-target 'function)
 "@version{2014-7-19}
  @argument[object]{a @class{gtk-actionable} widget}
  @argument[target-value]{a @type{g-variant} to set as the target value,
    or @code{NULL}}
  @syntax[]{(gtk-actionable-action-target object) => target-value}
  @syntax[]{(setf (gtk-actionable-action-target object) target-value)}
  @begin{short}
    Accessor of the slot @slot[gtk-actionable]{action-target} of the
    @class{gtk-actionable} inferface.
  @end{short}

  The generic function @sym{gtk-actionable-action-target} gets the current
  target value of @arg{object}.

  The generic function @sym{(setf gtk-actionable-action-target)} sets the
  target value of an actionable widget.

  If @arg{target-value} is @code{NULL} then the target value is unset.

  The target value has two purposes. First, it is used as the parameter to
  activation of the action associated with the @class{gtk-actionable} widget.
  Second, it is used to determine if the widget should be rendered as \"active\"
  - the widget is active if the state is equal to the given target.

  Consider the example of associating a set of buttons with a @class{g-action}
  with string state in a typical \"radio button\" situation. Each button will
  be associated with the same action, but with a different target value for
  that action. Clicking on a particular button will activate the action with
  the target of that button, which will typically cause the action's state to
  change to that value. Since the action's state is now equal to the target
  value of the button, the button will now be rendered as active and the
  other buttons, with different targets, rendered inactive.
  @begin[Example]{dictionary}
    @begin{pre}
 (let ((button (make-instance 'gtk-button)))
   (setf (gtk-actionable-action-target button) (g-variant-new-int16 128))
   (g-variant-get-int16 (gtk-actionable-action-target button)))
 => 128
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    The C implementation knows in addition the functions
    @code{gtk_application_get_action_target_value} and
    @code{gtk_application_set_action_target_value}. In the Lisp implementation
    these functions are replaced by the accessor function
    @sym{gtk-application-action-target}.
  @end{dictionary}

  Since 3.4
  @see-class{gtk-actionable}
  @see-type{g-variant}
  @see-class{g-action}")

;;; ----------------------------------------------------------------------------
;;; struct GtkActionableInterface
;;;
;;; struct GtkActionableInterface {
;;;   GTypeInterface g_iface;
;;;
;;;   const gchar * (* get_action_name)         (GtkActionable *actionable);
;;;   void          (* set_action_name)         (GtkActionable *actionable,
;;;                                              const gchar   *action_name);
;;;   GVariant *    (* get_action_target_value) (GtkActionable *actionable);
;;;   void          (* set_action_target_value) (GtkActionable *actionable,
;;;                                              GVariant *action_target_value);
;;; };
;;;
;;; The interface vtable for GtkActionable.
;;;
;;; GTypeInterface g_iface;
;;;
;;; get_action_name ()
;;;     virtual pointer for gtk_actionable_get_action_name()
;;;
;;; set_action_name ()
;;;     virtual pointer for gtk_actionable_set_action_name()
;;;
;;; get_action_target_value ()
;;;     virtual pointer for gtk_actionable_get_action_target_value()
;;;
;;; set_action_target_value ()
;;;     virtual pointer for gtk_actionable_set_action_target_value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_action_target ()
;;;
;;; void gtk_actionable_set_action_target (GtkActionable *actionable,
;;;                                        const gchar *format_string,
;;;                                        ...);
;;;
;;; Sets the target of an actionable widget.
;;;
;;; This is a convenience function that calls g_variant_new() for format_string
;;; and uses the result to call gtk_actionable_set_action_target_value().
;;;
;;; If you are setting a string-valued target and want to set the action name
;;; at the same time, you can use gtk_actionable_set_detailed_action_name().
;;;
;;; actionable :
;;;     a GtkActionable widget
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments appropriate for format_string
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_detailed_action_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_actionable_set_detailed_action_name"
           gtk-actionable-set-detailed-action-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-7-19}
  @argument[actionable]{a @class{gtk-actionable} widget}
  @argument[detailed-action-name]{the detailed action name}
  @begin{short}
    Sets the action-name and associated string target value of an actionable
    widget.
  @end{short}

  This allows for the effect of both the functions
  @fun{gtk-actionable-action-name} and
  @fun{gtk-actionable-action-target} in the common case that the target is
  string-valued.

  @arg{detailed-action-name} is a string of the form \"action::target\" where
  @code{action} is the action name and @code{target} is the string to use as
  the target.
  @begin[Example]{dictionary}
    @begin{pre}
 (let ((button (make-instance 'gtk-button)))
   (gtk-actionable-set-detailed-action-name button \"app::save\")
   (values (gtk-actionable-action-name button)
           (g-variant-get-string (gtk-actionable-action-target button))))
  => \"app\"
  => \"save\"
    @end{pre}
  @end{dictionary}
  Since 3.4
  @see-class{gtk-actionable}
  @see-function{gtk-actionable-action-name}
  @see-function{gtk-actionable-action-target}"
  (actionable (g-object gtk-actionable))
  (detailed-action-name :string))

(export 'gtk-actionable-set-detailed-action-name)

;;; --- End of file gtk.actionable.lisp ----------------------------------------
