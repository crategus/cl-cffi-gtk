;;; ----------------------------------------------------------------------------
;;; gtk.switch.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.2. See http://www.gtk.org.
;;;
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
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "slider-width"             gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "activate"                                       : Action
;;; 
;;; Description
;;; 
;;; GtkSwitch is a widget that has two states: on or off. The user can control
;;; which state should be active by clicking the empty area, or by dragging the
;;; handle.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Whether the GtkSwitch widget is in its on or off state.
;;; 
;;; Default value: FALSE
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
;;; 
;;; struct GtkSwitch;
;;;
;;; The GtkSwitch structure contains private data and it should only be accessed
;;; using the provided API.
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
;;; gtk_switch_new ()
;;; 
;;; GtkWidget * gtk_switch_new (void);
;;; 
;;; Creates a new GtkSwitch widget.
;;; 
;;; Returns :
;;;     the newly created GtkSwitch instance
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-new))

(defun gtk-switch-new ()
  (make-instance 'gtk-switch))

(export 'gtk-switch-new)

;;; ----------------------------------------------------------------------------
;;; gtk_switch_set_active ()
;;; 
;;; void gtk_switch_set_active (GtkSwitch *sw, gboolean is_active);
;;; 
;;; Changes the state of sw to the desired one.
;;; 
;;; sw :
;;;     a GtkSwitch
;;; 
;;; is_active :
;;;     TRUE if sw should be active, and FALSE otherwise
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-set-active))

(defun gtk-switch-set-active (sw is-active)
  (setf (gtk-switch-active sw) is-active))

(export 'gtk-switch-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_switch_get_active ()
;;; 
;;; gboolean gtk_switch_get_active (GtkSwitch *sw);
;;; 
;;; Gets whether the GtkSwitch is in its "on" or "off" state.
;;; 
;;; sw :
;;;     a GtkSwitch
;;; 
;;; Returns :
;;;     TRUE if the GtkSwitch is active, and FALSE otherwise
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-switch-get-active))

(defun gtk-switch-get-active (sw)
  (gtk-switch-active sw))

(export 'gtk-switch-get-active)

;;; --- End of file gtk.switch.lisp --------------------------------------------
