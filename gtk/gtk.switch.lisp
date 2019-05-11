;;; ----------------------------------------------------------------------------
;;; gtk.switch.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     A "light switch" style toggle
;;;
;;; Types and Values
;;;
;;;     GtkSwitch
;;;
;;; Functions
;;;
;;;     gtk_switch_new
;;;     gtk_switch_set_active
;;;     gtk_switch_get_active
;;;     gtk_switch_set_state ()
;;;     gtk_switch_get_state ()
;;;
;;; Properties
;;;
;;;     gboolean  active    Read / Write
;;;     gboolean  state     Read / Write
;;;
;;; Style Properties
;;;
;;;     gint  slider-height    Read
;;;     gint  slider-width     Read
;;;
;;; Signals
;;;
;;;         void  activate     Action
;;;     gboolean  state-set    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSwitch
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSwitch implements AtkImplementorIface, GtkBuildable, GtkActionable
;;;     and GtkActivatable.
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
    "state" "gboolean" t t)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-switch 'type)
 "@version{2013-4-27}
  @begin{short}
    @sym{gtk-switch} is a widget that has two states: on or off.
  @end{short}

  @image[switch]{}

  The user can control which state should be active by clicking the empty area,
  or by dragging the handle.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 switch
 ╰── slider
    @end{pre}
    @sym{gtk-switch} has two css nodes, the main node with the name
    @code{switch} and a subnode named @code{slider}. Neither of them is using
    any style classes.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[slider-height]{entry}
        The @code{slider-height} style property of type @code{:int} (Read) @br{}
        The minimum height of the @sym{gtk-switch} handle, in pixels.
        @em{Warning:} @code{slider-height} has been deprecated since version
        3.20 and should not be used in newly-written code. Use the CSS
        min-height property instead. @br{}
        Allowed values: >= 22 @br{}
        Default value: 22 @br{}
        Since 3.18
      @end{entry}
      @begin[slider-width]{entry}
        The @code{slider-width} style property of type @code{:int} (Read) @br{}
        The minimum width of the @sym{gtk-switch} handle, in pixels. @br{}
        @em{Warning:} @code{slider-width} has been deprecated since version 3.20
        and should not be used in newly-written code. Use the CSS min-height
        property instead. @br{}
        Allowed values: >= 36 @br{}
        Default value: 36
      @end{entry}
    @end{table}
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
    @subheading{The \"state-set\" signal}
      @begin{pre}
 lambda (widget state)    : Action
      @end{pre}
      The \"state-set\" signal on @sym{gtk-switch} is emitted to change the
      underlying state. It is emitted when the user changes the switch position.
      The default handler keeps the state in sync with the  @code{active}
      property.

      To implement delayed state change, applications can connect to this
      signal, initiate the change of the underlying state, and call the function
      @fun{gtk-switch-state} when the underlying state change is complete. The
      signal handler should return @em{true} to prevent the default handler from
      running.

      Visually, the underlying state is represented by the trough color of the
      switch, while the @code{active} property is represented by the position of
      the switch.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-switch} widget which received the signal.}
        @entry[state]{@em{True} to stop the signal emission.}
        @entry[Returns]{@em{True} to stop the signal emission.}
      @end{table}
      Since 3.14
  @end{dictionary}
  @see-slot{gtk-switch-active}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-switch-active ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-switch) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the @sym{gtk-switch} widget is in its on or off state. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-switch-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-switch-active 'function)
 "@version{2014-4-19}
  @syntax[]{(gtk-switch-active object) => is-active)}
  @syntax[]{(setf (gtk-switch-active object) is-active)}
  @argument[object]{a @class{gtk-switch} widget}
  @argument[is-active]{@em{true} if the switch should be active,
    and @code{nil} otherwise}
  @begin{short}
    Accessor of the slot @slot[gtk-switch]{active} of the @class{gtk-switch}
    class.
  @end{short}

  The slot access function @sym{gtk-switch-active} gets whether the
  @class{gtk-switch} is in its \"on\" or \"off\" state.

  The slot access function @sym{(setf gtk-switch-active)} changes the state of
  the switch to the desired one.
  @see-class{gtk-switch}")

;;; --- gtk-switch-state -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state" 'gtk-switch) 't)
 "The @code{state} property of type @code{:boolean} (Read / Write) @br{}
  The backend state that is controlled by the switch. See the \"state-set\"
  signal for details. @br{}
  Default value: @code{nil} @br{}
  Since 3.14")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-switch-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-switch-state 'function)
 "@version{2019-4-2}
  @syntax[]{(gtk-switch-state object) => state)}
  @syntax[]{(setf (gtk-switch-state object) state)}
  @argument[object]{a @class{gtk-switch} widget}
  @argument[state]{the state of type @code{:boolean}}
  @begin{short}
    Accessor of the slot @slot[gtk-switch]{state} of the @class{gtk-switch}
    class.
  @end{short}

  The slot access function @sym{gtk-switch-active}
  gets the underlying state of the switch.

  The slot access function @sym{(setf gtk-switch-active)}
  sets the underlying state of the switch.

  Normally, this is the same as the @code{active} property, unless the switch is
  set up for delayed state changes. This function is typically called from a
  \"state-set\" signal handler. See the \"state-set\" signal for details.

  Since 3.14
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
  @see-class{gtk-switch}"
  (make-instance 'gtk-switch))

(export 'gtk-switch-new)

;;; --- End of file gtk.switch.lisp --------------------------------------------
