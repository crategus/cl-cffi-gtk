;;; ----------------------------------------------------------------------------
;;; gtk.switch.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
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
;;; GtkSwitch
;;;
;;; A "light switch" style toggle
;;;
;;; Synopsis
;;;
;;;     GtkSwitch
;;;
;;;     gtk_switch_new
;;;     gtk_switch_set_active                              -> Accessor
;;;     gtk_switch_get_active                              -> Accessor
;;;     gtk_switch_set_state                               -> Accessor
;;;     gtk_switch_get_state                               -> Accessor
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
    "active" "gboolean" t t)
   #+gtk-3-14
   (state
    gtk-switch-state
    "state" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-switch 'type)
 "@version{2013-4-27}
  @begin{short}
    @sym{gtk-switch} is a widget that has two states: on or off. The user can
    control which state should be active by clicking the empty area, or by
    dragging the handle.
  @end{short}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"slider-width\" style property}
      @code{\"slider-width\"} of type @code{:int} (Read)@br{}
      The minimum width of the @sym{gtk-switch} handle, in pixels. @br{}
      Allowed values: >= 36 @br{}
      Default value: 36
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (widget)   : Action
      @end{pre}
      The \"activate\" signal on @sym{gtk-switch} is an action signal and
      emitting it causes the switch to animate. Applications should never
      connect to this signal, but use the \"notify::active\" signal.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-switch-active}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-switch) 't)
 "The @code{\"active\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether the @sym{gtk-switch} widget is in its on or off state. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-switch-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-switch-active 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-switch} widget}
  @argument[is-active]{@em{true} if the switch should be active,
    and @code{nil} otherwise}
  @syntax[]{(gtk-switch-active object) => is-active)}
  @syntax[]{(setf (gtk-switch-active object) is-active)}
  @begin{short}
    Accessor of the slot @slot[gtk-switch]{active} of the @class{gtk-switch}
    class.
  @end{short}

  The generic function @sym{gtk-switch-active} gets whether the
  @class{gtk-switch} is in its \"on\" or \"off\" state.

  The generic function @sym{(setf gtk-switch-active)} changes the state of
  the switch to the desired one.

  Since 3.0
  @see-class{gtk-switch}")

;;; ----------------------------------------------------------------------------
;;; gtk_switch_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-new))

(defun gtk-switch-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-4-19}
  @return{The newly created @class{gtk-switch} widget.}
  @short{Creates a new @class{gtk-switch} widget.}

  Since 3.0
  @see-class{gtk-switch}"
  (make-instance 'gtk-switch))

(export 'gtk-switch-new)

;;; --- End of file gtk.switch.lisp --------------------------------------------
