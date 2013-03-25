;;; ----------------------------------------------------------------------------
;;; gtk.switch.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
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
;;; GtkSwitch
;;; 
;;; A "light switch" style toggle
;;;     
;;; Synopsis
;;; 
;;;     GtkSwitch
;;;     
;;;     gtk_switch_new
;;;     gtk_switch_set_active
;;;     gtk_switch_get_active
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkSwitch
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSwitch implements AtkImplementorIface, GtkBuildable, GtkActionable and
;;; GtkActivatable.
;;; 
;;; 
;;; Style Properties
;;; 
;;;   "slider-width"             gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "activate"                                       : Action
;;; 
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "slider-width" style property
;;; 
;;;   "slider-width"             gint                  : Read
;;; 
;;; The minimum width of the GtkSwitch handle, in pixels.
;;; 
;;; Allowed values: >= 36
;;; 
;;; Default value: 36
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GtkSwitch *widget,
;;;                     gpointer   user_data)      : Action
;;; 
;;; The ::activate signal on GtkSwitch is an action signal and emitting it
;;; causes the switch to animate. Applications should never connect to this
;;; signal, but use the notify::active signal.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSwitch
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSwitch" gtk-switch
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_switch_get_type")
  ((active
    gtk-switch-active
    "active" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-switch 'type)
 "@version{2013-3-23}
  @begin{short}
    GtkSwitch is a widget that has two states: on or off. The user can control
    which state should be active by clicking the empty area, or by dragging the
    handle.
  @end{short}
  @see-slot{gtk-switch-active}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-switch) 't)
 "The @code{\"active\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the GtkSwitch widget is in its on or off state. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-switch-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-switch-active 'function)
 "@version{2013-3-23}
  Accessor of the slot @code{\"active\"} of the @class{gtk-switch} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_switch_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-new))

(defun gtk-switch-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @return{the newly created GtkSwitch instance}
  @short{Creates a new GtkSwitch widget.}

  Since 3.0"
  (make-instance 'gtk-switch))

(export 'gtk-switch-new)

;;; ----------------------------------------------------------------------------
;;; gtk_switch_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-set-active))

(defun gtk-switch-set-active (switch is-active)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[switch]{a GtkSwitch}
  @argument[is-active]{TRUE if sw should be active, and FALSE otherwise}
  @begin{short}
    Changes the state of sw to the desired one.
  @end{short}

  Since 3.0"
  (setf (gtk-switch-active switch) is-active))

(export 'gtk-switch-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_switch_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-get-active))

(defun gtk-switch-get-active (switch)
 "@version{2013-3-23}
  @argument[switch]{a GtkSwitch}
  @return{TRUE if the GtkSwitch is active, and FALSE otherwise}
  @begin{short}
    Gets whether the GtkSwitch is in its \"on\" or \"off\" state.
  @end{short}

  Since 3.0"
  (gtk-switch-active sw))

(export 'gtk-switch-get-active)

;;; --- End of file gtk.switch.lisp --------------------------------------------
