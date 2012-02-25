;;; ----------------------------------------------------------------------------
;;; gtk.plug.lisp
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
;;; GtkPlug
;;; 
;;; Toplevel for embedding into other processes
;;; 
;;; Synopsis
;;; 
;;;     GtkPlug
;;;
;;;     gtk_plug_construct
;;;     gtk_plug_construct_for_display
;;;     gtk_plug_new
;;;     gtk_plug_new_for_display
;;;     gtk_plug_get_id
;;;     gtk_plug_get_embedded
;;;     gtk_plug_get_socket_window
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkWindow
;;;                                  +----GtkPlug
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkPlug implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "embedded"                 gboolean              : Read
;;;   "socket-window"            GdkWindow*            : Read
;;; 
;;; Signals
;;; 
;;;   "embedded"                                       : Run Last
;;; 
;;; Description
;;; 
;;; Together with GtkSocket, GtkPlug provides the ability to embed widgets from
;;; one process into another process in a fashion that is transparent to the
;;; user. One process creates a GtkSocket widget and passes the ID of that
;;; widget's window to the other process, which then creates a GtkPlug with that
;;; window ID. Any widgets contained in the GtkPlug then will appear inside the
;;; first application's window.
;;; 
;;; The communication between a GtkSocket and a GtkPlug follows the XEmbed
;;; protocol. This protocol has also been implemented in other toolkits, e.g.
;;; Qt, allowing the same level of integration when embedding a Qt widget in
;;; GTK+ or vice versa.
;;; 
;;; Note
;;; The GtkPlug and GtkSocket widgets are only available when GTK+ is compiled
;;; for the X11 platform and GDK_WINDOWING_X11 is defined. They can only be
;;; used on a GdkX11Display. To use GtkPlug and GtkSocket, you need to include
;;; the gtk/gtkx.h header.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "embedded" property
;;; 
;;;   "embedded"                 gboolean              : Read
;;; 
;;; TRUE if the plug is embedded in a socket.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "socket-window" property
;;; 
;;;   "socket-window"            GdkWindow*            : Read
;;; 
;;; The window of the socket the plug is embedded in.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "embedded" signal
;;; 
;;; void user_function (GtkPlug *plug,
;;;                     gpointer user_data)      : Run Last
;;; 
;;; Gets emitted when the plug becomes embedded in a socket.
;;; 
;;; plug :
;;;     the object on which the signal was emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPlug
;;; 
;;; struct GtkPlug;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPlug" gtk-plug
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_plug_get_type")
  ((embedded
    gtk-plug-embedded
    "embedded" "gboolean" t nil)
   (socket-window
    gtk-plug-socket-window
    "socket-window" "GdkWindow" t nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_plug_construct ()
;;; 
;;; void gtk_plug_construct (GtkPlug *plug, Window socket_id);
;;; 
;;; Finish the initialization of plug for a given GtkSocket identified by
;;; socket_id. This function will generally only be used by classes deriving
;;; from GtkPlug.
;;; 
;;; plug :
;;;     a GtkPlug
;;; 
;;; socket_id :
;;;     the XID of the socket's window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_construct_for_display ()
;;; 
;;; void gtk_plug_construct_for_display (GtkPlug *plug,
;;;                                      GdkDisplay *display,
;;;                                      Window socket_id);
;;; 
;;; Finish the initialization of plug for a given GtkSocket identified by
;;; socket_id which is currently displayed on display. This function will
;;; generally only be used by classes deriving from GtkPlug.
;;; 
;;; plug :
;;;     a GtkPlug.
;;; 
;;; display :
;;;     the GdkDisplay associated with socket_id's GtkSocket
;;; 
;;; socket_id :
;;;     the XID of the socket's window
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_new ()
;;; 
;;; GtkWidget * gtk_plug_new (Window socket_id);
;;; 
;;; Creates a new plug widget inside the GtkSocket identified by socket_id. If
;;; socket_id is 0, the plug is left "unplugged" and can later be plugged into
;;; a GtkSocket by gtk_socket_add_id().
;;; 
;;; socket_id :
;;;     the window ID of the socket, or 0
;;; 
;;; Returns :
;;;     the new GtkPlug widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_new_for_display ()
;;; 
;;; GtkWidget * gtk_plug_new_for_display (GdkDisplay *display, Window socket_id)
;;; 
;;; Create a new plug widget inside the GtkSocket identified by socket_id.
;;; 
;;; display :
;;;     the GdkDisplay on which socket_id is displayed
;;; 
;;; socket_id :
;;;     the XID of the socket's window
;;; 
;;; Returns :
;;;     the new GtkPlug widget
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_get_id ()
;;; 
;;; Window gtk_plug_get_id (GtkPlug *plug);
;;; 
;;; Gets the window ID of a GtkPlug widget, which can then be used to embed this
;;; window inside another window, for instance with gtk_socket_add_id().
;;; 
;;; plug :
;;;     a GtkPlug.
;;; 
;;; Returns :
;;;     the window ID for the plug
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_get_embedded ()
;;; 
;;; gboolean gtk_plug_get_embedded (GtkPlug *plug);
;;; 
;;; Determines whether the plug is embedded in a socket.
;;; 
;;; plug :
;;;     a GtkPlug
;;; 
;;; Returns :
;;;     TRUE if the plug is embedded in a socket
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_get_socket_window ()
;;; 
;;; GdkWindow * gtk_plug_get_socket_window (GtkPlug *plug);
;;; 
;;; Retrieves the socket the plug is embedded in.
;;; 
;;; plug :
;;;     a GtkPlug
;;; 
;;; Returns :
;;;     the window of the socket, or NULL. [transfer none]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.plug.lisp ----------------------------------------------
