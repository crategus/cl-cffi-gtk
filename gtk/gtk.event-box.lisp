;;; ----------------------------------------------------------------------------
;;; gtk.event-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; A widget used to catch events for widgets which do not have their own window
;;;     
;;; Synopsis
;;; 
;;;  GtkEventBox
;;;
;;;  gtk_event_box_new
;;;  gtk_event_box_set_above_child
;;;  gtk_event_box_get_above_child
;;;  gtk_event_box_set_visible_window
;;;  gtk_event_box_get_visible_window
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkEventBox
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkEventBox implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "above-child"              gboolean              : Read / Write
;;;   "visible-window"           gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; The GtkEventBox widget is a subclass of GtkBin which also has its own
;;; window. It is useful since it allows you to catch events for widgets which
;;; do not have their own window.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "above-child" property
;;; 
;;;   "above-child"              gboolean              : Read / Write
;;; 
;;; Whether the event-trapping window of the eventbox is above the window of
;;; the child widget as opposed to below it.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible-window" property
;;; 
;;;   "visible-window"           gboolean              : Read / Write
;;; 
;;; Whether the event box is visible, as opposed to invisible and only used to
;;; trap events.
;;; 
;;; Default value: TRUE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventBox
;;; 
;;; struct GtkEventBox;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventBox" gtk-event-box
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_event_box_get_type")
  ((above-child
    gtk-event-box-above-child
    "above-child" "gboolean" t t)
   (visible-window
    gtk-event-box-visible-window
    "visible-window" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_new ()
;;; 
;;; GtkWidget * gtk_event_box_new (void);
;;; 
;;; Creates a new GtkEventBox.
;;; 
;;; Returns :
;;;     a new GtkEventBox
;;; ----------------------------------------------------------------------------

(defun gtk-event-box-new ()
  (make-instance 'gtk-event-box))

(export 'gtk-event-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_set_above_child ()
;;; 
;;; void gtk_event_box_set_above_child (GtkEventBox *event_box,
;;;                                     gboolean above_child);
;;; 
;;; Set whether the event box window is positioned above the windows of its
;;; child, as opposed to below it. If the window is above, all events inside
;;; the event box will go to the event box. If the window is below, events in
;;; windows of child widgets will first got to that widget, and then to its
;;; parents.
;;; 
;;; The default is to keep the window below the child.
;;; 
;;; event_box :
;;;     a GtkEventBox
;;; 
;;; above_child :
;;;     TRUE if the event box window is above its child
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defun gtk-event-box-set-above-child (event-box above-child)
  (setf (gtk-event-box-above-child event-box) above-child))

(export 'gtk-event-box-set-above-child)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_get_above_child ()
;;; 
;;; gboolean gtk_event_box_get_above_child (GtkEventBox *event_box);
;;; 
;;; Returns whether the event box window is above or below the windows of its
;;; child. See gtk_event_box_set_above_child() for details.
;;; 
;;; event_box :
;;;     a GtkEventBox
;;; 
;;; Returns :
;;;     TRUE if the event box window is above the window of its child
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defun gtk-event-box-get-above-child (event-box)
  (gtk-event-box-above-child event-box))

(export 'gtk-event-box-get-above-child)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_set_visible_window ()
;;; 
;;; void gtk_event_box_set_visible_window (GtkEventBox *event_box,
;;;                                        gboolean visible_window);
;;; 
;;; Set whether the event box uses a visible or invisible child window. The
;;; default is to use visible windows.
;;; 
;;; In an invisible window event box, the window that the event box creates is
;;; a GDK_INPUT_ONLY window, which means that it is invisible and only serves
;;; to receive events.
;;; 
;;; A visible window event box creates a visible (GDK_INPUT_OUTPUT) window that
;;; acts as the parent window for all the widgets contained in the event box.
;;; 
;;; You should generally make your event box invisible if you just want to trap
;;; events. Creating a visible window may cause artifacts that are visible to
;;; the user, especially if the user is using a theme with gradients or pixmaps.
;;; 
;;; The main reason to create a non input-only event box is if you want to set
;;; the background to a different color or draw on it.
;;; 
;;; Note
;;; 
;;; There is one unexpected issue for an invisible event box that has its window
;;; below the child. (See gtk_event_box_set_above_child().) Since the input-only
;;; window is not an ancestor window of any windows that descendent widgets of
;;; the event box create, events on these windows aren't propagated up by the
;;; windowing system, but only by GTK+. The practical effect of this is if an
;;; event isn't in the event mask for the descendant window (see
;;; gtk_widget_add_events()), it won't be received by the event box.
;;; 
;;; This problem doesn't occur for visible event boxes, because in that case,
;;; the event box window is actually the ancestor of the descendant windows,
;;; not just at the same place on the screen.
;;; 
;;; event_box :
;;;     a GtkEventBox
;;; 
;;; visible_window :
;;;     TRUE to make the event box have a visible window
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defun gtk-event-box-set-visible-window (event-box visible-window)
  (setf (gtk-event-box-visible-window event-box) visible-window))

(export 'gtk-event-box-set-visible-window)

;;; ----------------------------------------------------------------------------
;;; gtk_event_box_get_visible_window ()
;;; 
;;; gboolean gtk_event_box_get_visible_window (GtkEventBox *event_box);
;;; 
;;; Returns whether the event box has a visible window.
;;; See gtk_event_box_set_visible_window() for details.
;;; 
;;; event_box :
;;;     a GtkEventBox
;;; 
;;; Returns :
;;;     TRUE if the event box window is visible
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defun gtk-event-box-get-visible-window (event-box)
  (gtk-event-box-visible-window event-box))

(export 'gtk-event-box-get-visible-window)

;;; --- End of file gtk.event-box.lisp -----------------------------------------
