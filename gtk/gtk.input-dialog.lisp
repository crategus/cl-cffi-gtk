;;; ----------------------------------------------------------------------------
;;; gtk.input-dialog.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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
;;; GtkInputDialog
;;;
;;; Configure devices for the XInput extension
;;;
;;; Synopsis
;;;
;;;     GtkInputDialog
;;;
;;;     gtk_input_dialog_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkContainer
;;;                            +----GtkBin
;;;                                  +----GtkWindow
;;;                                        +----GtkDialog
;;;                                              +----GtkInputDialog
;;;
;;; Implemented Interfaces
;;;
;;; GtkInputDialog implements AtkImplementorIface and GtkBuildable.
;;;
;;; Signals
;;;
;;;   "disable-device"                                 : Run Last
;;;   "enable-device"                                  : Run Last
;;;
;;; Description
;;;
;;; GtkInputDialog displays a dialog which allows the user to configure XInput
;;; extension devices. For each device, they can control the mode of the device
;;; (disabled, screen-relative, or window-relative), the mapping of axes to
;;; coordinates, and the mapping of the devices macro keys to key press events.
;;;
;;; GtkInputDialog contains two buttons to which the application can connect;
;;; one for closing the dialog, and one for saving the changes. No actions are
;;; bound to these by default. The changes that the user makes take effect
;;; immediately.
;;;
;;; As of GTK+ 2.20, GtkInputDialog has been deprecated since it is too
;;; specialized.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "disable-device" signal
;;;
;;; void user_function (GtkInputDialog *inputdialog,
;;;                     GdkDevice      *arg1,
;;;                     gpointer        user_data)        : Run Last
;;;
;;; This signal is emitted when the user changes the mode of a device from a
;;; GDK_MODE_SCREEN or GDK_MODE_WINDOW to GDK_MODE_ENABLED.
;;;
;;; inputdialog :
;;;     the object which received the signal
;;;
;;; deviceid :
;;;     The ID of the newly disabled device.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "enable-device" signal
;;;
;;; void user_function (GtkInputDialog *inputdialog,
;;;                     GdkDevice      *arg1,
;;;                     gpointer        user_data)        : Run Last
;;;
;;; This signal is emitted when the user changes the mode of a device from
;;; GDK_MODE_DISABLED to a GDK_MODE_SCREEN or GDK_MODE_WINDOW.
;;;
;;; inputdialog :
;;;     the object which received the signal.
;;;
;;; deviceid :
;;;     The ID of the newly enabled device.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkInputDialog
;;;
;;; struct GtkInputDialog;
;;;
;;; Warning
;;;
;;; GtkInputDialog is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkInputDialog" gtk-input-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_input_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_input_dialog_new ()
;;;
;;; GtkWidget * gtk_input_dialog_new (void);
;;;
;;; Warning
;;;
;;; gtk_input_dialog_new has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Don't use this widget anymore.
;;;
;;; Creates a new GtkInputDialog.
;;;
;;; Returns :
;;;     the new GtkInputDialog.
;;; ----------------------------------------------------------------------------

(defun gtk-input-dialog-new ()
  (make-instance 'gtk-input-dialog-new))

(export 'gtk-input-dialog-new)

;;; --- End of file gtk.input-dialog.lisp --------------------------------------
