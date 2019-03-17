;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-key.lisp
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
;;; GtkEventControllerKey
;;;
;;;     Event controller for key events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerKey
;;;
;;; Functions
;;;
;;;     gtk_event_controller_key_new
;;;
;;; Signals
;;;
;;;         void  focus-in      Run Last
;;;         void  focus-out     Run Last
;;;         void  im-update     Run Last
;;;     gboolean  key-pressed   Run Last
;;;         void  key-released  Run Last
;;;     gboolean  modifiers     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerKey
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventControllerKey
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(define-g-object-class "GtkEventControllerKey" gtk-event-controller-key
  (:superclass gtk-event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_key_get_type")
  nil)

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation 'gtk-event-controller-key 'type)
 "@version{2019-3-17}
  @begin{short}
    @sym{gtk-event-controller-key} is an event controller meant for situations
    where you need access to key events.
  @end{short}

  This object was added in GTK+ 3.24.

  @begin[Signal Details]{dictionary}
    @subheading{The \"focus-in\" signal}
      @begin{pre}
  lambda (eventcontrollerkey)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[eventcontrollerkey]{The @sym{gtk-event-controller-key} object on 
          which the signal is emitted.}
      @end{table}
      Since 3.24

    @subheading{The \"focus-out\" signal}
      @begin{pre}
  lambda (controller)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-key} object on 
          which the signal is emitted.}
      @end{table}
      Since 3.24

    @subheading{The \"im-update\" signal}
      @begin{pre}
  lambda (controller)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-key} object on 
          which the signal is emitted.}
      @end{table}
      Since 3.24

    @subheading{The \"key-pressed\" signal}
      @begin{pre}
  lambda (controller keyval keycode state)    : Run Last
      @end{pre}
      This signal is emitted whenever a key is pressed.
      @begin[code]{table}
        @entry[controller]{The @sytem{gtk-event-controller-key} object on 
          which received the signal.}
        @entry[keyval]{The pressed key.}
        @entry[keycode]{The raw code of the pressed key.}
        @entry[state]{The bitmask, representing the state of modifier keys and 
          pointer buttons. See @symbol{gdk-modifier-type}.}
        @entry[Returns]{@em{True} if the key press was handled, @code{nil} 
          otherwise.}
      @end{table}
      Since 3.24

    @subheading{The \"key-released\" signal}
      @begin{pre}
  lambda (controller keyval keycode state)    : Run Last
      @end{pre}
      This signal is emitted whenever a key is released.
      @begin[code]{table}
        @entry[controller]{The @sytem{gtk-event-controller-key} object on 
          which received the signal.}
        @entry[keyval]{The released key.}
        @entry[keycode]{The raw code of the released key.}
        @entry[state]{The bitmask, representing the state of modifier keys and 
          pointer buttons. See @symbol{gdk-modifier-type}.}
      @end{table}
      Since 3.24

    @subheading{The \"modifiers\" signal}
      @begin{pre}
  lambda (controller arg1)    : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[controller]{The @sytem{gtk-event-controller-key} object on 
          which received the signal.}
        @entry[arg1]{not documented}
        @entry[Returns]{a not documented boolean}
      @end{table}
      Since 3.24
  @end{dictionary}
  @see-class{gtk-event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(declaim (inline gtk-event-controller-key-new))

#+gtk-3-24
(defun gtk-event-controller-key-new ()
 #+cl-cffi-gtk-documentation
 "@version{2019-3-17}
  @return{The new @class{gtk-event-controller-key} objekt.}
  @short{Creates a new event controller.}

  Since 3.24
  @see-class{gtk-event-controller-key}"
  (make-instance 'gtk-event-controller-key))

#+gtk-3-24
(export 'gtk-event-controller-key-new)

;;; --- End of File gtk.event-controller-key.lisp ------------------------------ 
