;;; ----------------------------------------------------------------------------
;;; gtk.toggle-action.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
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
;;; GtkToggleAction
;;;
;;; An action which can be toggled between two states
;;;
;;; Synopsis
;;;
;;;     GtkToggleAction
;;;
;;;     gtk_toggle_action_new
;;;     gtk_toggle_action_toggled
;;;     gtk_toggle_action_set_active
;;;     gtk_toggle_action_get_active
;;;     gtk_toggle_action_set_draw_as_radio
;;;     gtk_toggle_action_get_draw_as_radio
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleAction" gtk-toggle-action
  (:superclass gtk-action
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_toggle_action_get_type")
  ((active
    gtk-toggle-action-active
    "active" "gboolean" t t)
   (draw-as-radio
    gtk-toggle-action-draw-as-radio
    "draw-as-radio" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-toggle-action 'type)
 "@version{2013-6-2}
  @begin{short}
    A @sym{gtk-toggle-action} corresponds roughly to a
    @class{gtk-check-menu-item}. It has an \"active\" state specifying whether
    the action has been checked or not.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (toggleaction)   : Run First
      @end{pre}
      Should be connected if you wish to perform an action whenever the
      @sym{gtk-toggle-action} state is changed.
      @begin[code]{table}
        @entry[toggleaction]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toggle-action-active}
  @see-slot{gtk-toggle-action-draw-as-radio}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-toggle-action) 't)
 "The @code{\"active\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle action should be active. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-as-radio"
                                               'gtk-toggle-action) 't)
 "The @code{\"draw-as-radio\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the proxies for this action look like radio action proxies.
  This is an appearance property and thus only applies if
  @code{\"use-action-appearance\"} is @em{true}. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-action-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-action-active 'function)
 "@version{2013-12-10}
  Accessor of the slot @code{\"active\"} of the @class{gtk-toggle-action}
  class.
  @see-class{gtk-toggle-action}
  @see-function{gtk-toggle-action-get-active}
  @see-function{gtk-toggle-action-set-active}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-action-draw-as-radio atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-action-draw-as-radio 'function)
 "@version{2013-12-10}
  Accessor of the slot @code{\"draw-as-radio\"} of the @class{gtk-toggle-action}
  class.
  @see-class{gtk-toggle-action}
  @see-function{gtk-toggle-action-get-draw-as-radio}
  @see-function{gtk-toggle-action-set-draw-as-radio}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_new ()
;;;
;;; GtkToggleAction * gtk_toggle_action_new (const gchar *name,
;;;                                          const gchar *label,
;;;                                          const gchar *tooltip,
;;;                                          const gchar *stock_id);
;;;
;;; Creates a new GtkToggleAction object. To add the action to a GtkActionGroup
;;; and set the accelerator for the action, call
;;; gtk_action_group_add_action_with_accel().
;;;
;;; name :
;;;     A unique name for the action
;;;
;;; label :
;;;     The label displayed in menu items and on buttons, or NULL.
;;;
;;; tooltip :
;;;     A tooltip for the action, or NULL.
;;;
;;; stock_id :
;;;     The stock icon to display in widgets representing the action, or NULL.
;;;
;;; Returns :
;;;     a new GtkToggleAction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toggle_action_new" gtk-toggle-action-new)
    (g-object gtk-toggle-action)
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string))

(export 'gtk-toggle-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_toggled ()
;;;
;;; void gtk_toggle_action_toggled (GtkToggleAction *action);
;;;
;;; Emits the "toggled" signal on the toggle action.
;;;
;;; action :
;;;     the action object
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toggle_action_toggled" gtk-toggle-action-toggled) :void
  (action (g-object gtk-toggle-action)))

(export 'gtk-toggle-action-toggled)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_set_active ()
;;;
;;; void gtk_toggle_action_set_active (GtkToggleAction *action,
;;;                                    gboolean is_active);
;;;
;;; Sets the checked state on the toggle action.
;;;
;;; action :
;;;     the action object
;;;
;;; is_active :
;;;     whether the action should be checked or not
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-action-set-active))

(defun gtk-toggle-action-set-active (action is-active)
  (setf (gtk-toggle-action-active action) is-active))

(export 'gtk-toggle-action-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_get_active ()
;;;
;;; gboolean gtk_toggle_action_get_active (GtkToggleAction *action);
;;;
;;; Returns the checked state of the toggle action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     the checked state of the toggle action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-action-get-active))

(defun gtk-toggle-action-get-active (action)
  (gtk-toggle-action-active action))

(export 'gtk-toggle-action-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_set_draw_as_radio ()
;;;
;;; void gtk_toggle_action_set_draw_as_radio (GtkToggleAction *action,
;;;                                           gboolean draw_as_radio);
;;;
;;; Sets whether the action should have proxies like a radio action.
;;;
;;; action :
;;;     the action object
;;;
;;; draw_as_radio :
;;;     whether the action should have proxies like a radio action
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-action-set-draw-as-radio))

(defun gtk-toggle-action-set-draw-as-radio (action draw-as-radio)
  (setf (gtk-toggle-action-draw-as-radio action) draw-as-radio))

(export 'gtk-toggle-action-set-draw-as-radio)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_get_draw_as_radio ()
;;;
;;; gboolean gtk_toggle_action_get_draw_as_radio (GtkToggleAction *action);
;;;
;;; Returns whether the action should have proxies like a radio action.
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     whether the action should have proxies like a radio action.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-action-get-draw-as-radio))

(defun gtk-toggle-action-get-draw-as-radio (action)
  (gtk-toggle-action-active action))

(export 'gtk-toggle-action-get-draw-as-radio)

;;; --- End of file gtk.toggle-action.lisp -------------------------------------
