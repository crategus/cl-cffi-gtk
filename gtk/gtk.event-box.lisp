;;; ----------------------------------------------------------------------------
;;; gtk.event-box.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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
;;; GtkEventBox
;;;
;;;     A widget used to catch events for widgets which do not have their own
;;;     window
;;;
;;; Types and Values
;;;
;;;     GtkEventBox
;;;
;;; Functions
;;;
;;;     gtk_event_box_new
;;;     gtk_event_box_set_above_child                      Accessor
;;;     gtk_event_box_get_above_child                      Accessor
;;;     gtk_event_box_set_visible_window                   Accessor
;;;     gtk_event_box_get_visible_window                   Accessor
;;;
;;; Properties
;;;
;;;     gboolean   above-child       Read / Write
;;;     gboolean   visible-window    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkEventBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkEventBox implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventBox" gtk-event-box
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_event_box_get_type")
  ((above-child
    gtk-event-box-above-child
    "above-child" "gboolean" t t)
   (visible-window
    gtk-event-box-visible-window
    "visible-window" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-event-box 'type)
 "@version{2013-8-17}
  The @sym{gtk-event-box} widget is a subclass of @class{gtk-bin} which also has
  its own window. It is useful since it allows you to catch events for widgets
  which do not have their own window.
  @see-slot{gtk-event-box-above-child}
  @see-slot{gtk-event-box-visible-window}
  @see-class{gtk-bin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-event-box-above-child ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "above-child" 'gtk-event-box) 't)
 "The @code{above-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the event-trapping window of the eventbox is above the window of the
  child widget as opposed to below it. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-event-box-above-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-event-box-above-child 'function)
 "@version{2013-8-17}
  @begin{short}
    Accessor of the slot @slot[gtk-event-box]{above-child} of the
    @class{gtk-event-box} class.
  @end{short}
  @see-class{gtk-event-box}
  @see-function{gtk-event-box-get-above-child}
  @see-function{gtk-event-box-set-above-child}")

;;; --- gtk-event-box-visible-window -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-window" 'gtk-event-box)
      't)
 "The @code{visible-window} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the event box is visible, as opposed to invisible and only used to
  trap events. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-event-box-visible-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-event-box-visible-window 'function)
 "@version{2013-8-17}
  @begin{short}
    Accessor of the slot @slot[gtk-event-box]{visible-window} of the
    @class{gtk-event-box} class.
  @end{short}
  @see-class{gtk-event-box}
  @see-function{gtk-event-box-get-visible-window}
  @see-function{gtk-event-box-set-visible-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-event-box-new))

(defun gtk-event-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @return{A new @class{gtk-event-box} widget.}
  Creates a new @class{gtk-event-box} widget.
  @see-class{gtk-event-box}"
  (make-instance 'gtk-event-box))

(export 'gtk-event-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_set_above_child ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-event-box-set-above-child))

(defun gtk-event-box-set-above-child (event-box above-child)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[event-box]{a @class{gtk-event-box} widget}
  @argument[above-child]{@em{true} if the event box window is above its child}
  @begin{short}
    Set whether the event box window is positioned above the windows of its
    child, as opposed to below it.
  @end{short}
  If the window is above, all events inside the event box will go to the event
  box. If the window is below, events in windows of child widgets will first got
  to that widget, and then to its parents.

  The default is to keep the window below the child.
  @see-class{gtk-event-box}
  @see-function{gtk-event-box-get-above-child}"
  (setf (gtk-event-box-above-child event-box) above-child))

(export 'gtk-event-box-set-above-child)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_get_above_child ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-event-box-get-above-child))

(defun gtk-event-box-get-above-child (event-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[event-box]{a @class{gtk-event-box} object}
  @return{@em{True} if the event box window is above the window of its child.}
  @begin{short}
    Returns whether the event box window is above or below the windows of its
    child.
  @end{short}
  See the function @fun{gtk-event-box-set-above-child} for details.
  @see-class{gtk-event-box}
  @see-function{gtk-event-box-set-above-child}"
  (gtk-event-box-above-child event-box))

(export 'gtk-event-box-get-above-child)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_set_visible_window ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-event-box-set-visible-window))

(defun gtk-event-box-set-visible-window (event-box visible-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[event-box]{a @class{gtk-event-box} widget}
  @argument[visible-window]{@em{true} to make the event box have a visible
    window}
  @begin{short}
    Set whether the event box uses a visible or invisible child window. The
    default is to use visible windows.
  @end{short}

  In an invisible window event box, the window that the event box creates is a
  @code{:input-only} window, which means that it is invisible and only serves to
  receive events.

  A visible window event box creates a visible @code{:input-output} window that
  acts as the parent window for all the widgets contained in the event box.

  You should generally make your event box invisible if you just want to trap
  events. Creating a visible window may cause artifacts that are visible to
  the user, especially if the user is using a theme with gradients or pixmaps.

  The main reason to create a non input-only event box is if you want to set
  the background to a different color or draw on it.

  @subheading{Note}
    There is one unexpected issue for an invisible event box that has its window
    below the child. See the function @fun{gtk-event-box-set-above-child}. Since
    the input-only window is not an ancestor window of any windows that
    descendent widgets of the event box create, events on these windows are not
    propagated up by the windowing system, but only by GTK+. The practical
    effect of this is if an event is not in the event mask for the descendant
    window (see the function @fun{gtk-widget-add-events}), it will not be
    received by the event box.

    This problem does not occur for visible event boxes, because in that case,
    the event box window is actually the ancestor of the descendant windows, not
    just at the same place on the screen.
  @see-class{gtk-event-box}
  @see-function{gtk-widget-add-events}
  @see-function{gtk-event-box-set-above-child}
  @see-function{gtk-event-box-get-visible-window}"
  (setf (gtk-event-box-visible-window event-box) visible-window))

(export 'gtk-event-box-set-visible-window)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_get_visible_window ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-event-box-get-visible-window))

(defun gtk-event-box-get-visible-window (event-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-17}
  @argument[event-box]{a @class{gtk-event-box} widget}
  @return{@em{True} if the event box window is visible.}
  @begin{short}
    Returns whether the event box has a visible window.
  @end{short}
  See the function @fun{gtk-event-box-set-visible-window} for details.
  @see-class{gtk-event-box}
  @see-function{gtk-event-box-set-visible-window}"
  (gtk-event-box-visible-window event-box))

(export 'gtk-event-box-get-visible-window)

;;; --- End of file gtk.event-box.lisp -----------------------------------------
