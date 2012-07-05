;;; ----------------------------------------------------------------------------
;;; gtk.socket.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkSocket
;;;
;;; Container for widgets from other processes
;;;
;;; Synopsis
;;;
;;;     GtkSocket
;;;
;;;     gtk_socket_new
;;;     gtk_socket_add_id
;;;     gtk_socket_get_id
;;;     gtk_socket_get_plug_window
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkSocket
;;;
;;; Implemented Interfaces
;;;
;;; GtkSocket implements AtkImplementorIface and GtkBuildable.
;;;
;;; Signals
;;;
;;;   "plug-added"                                     : Run Last
;;;   "plug-removed"                                   : Run Last
;;;
;;; Description
;;;
;;; Together with GtkPlug, GtkSocket provides the ability to embed widgets from
;;; one process into another process in a fashion that is transparent to the
;;; user. One process creates a GtkSocket widget and passes that widget's window
;;; ID to the other process, which then creates a GtkPlug with that window ID.
;;; Any widgets contained in the GtkPlug then will appear inside the first
;;; application's window.
;;;
;;; The socket's window ID is obtained by using gtk_socket_get_id(). Before
;;; using this function, the socket must have been realized, and for hence, have
;;; been added to its parent.
;;;
;;; Example 106. Obtaining the window ID of a socket.
;;;
;;;   GtkWidget *socket = gtk_socket_new ();
;;;   gtk_widget_show (socket);
;;;   gtk_container_add (GTK_CONTAINER (parent), socket);
;;;   
;;;   /* The following call is only necessary if one of
;;;    * the ancestors of the socket is not yet visible.
;;;    */
;;;   gtk_widget_realize (socket);
;;;   g_print ("The ID of the sockets window is
;;;             %<GTKDOCLINK HREF="x">x</GTKDOCLINK>\n",
;;;            gtk_socket_get_id (socket));
;;;   
;;;
;;; Note that if you pass the window ID of the socket to another process that
;;; will create a plug in the socket, you must make sure that the socket widget
;;; is not destroyed until that plug is created. Violating this rule will cause
;;; unpredictable consequences, the most likely consequence being that the plug
;;; will appear as a separate toplevel window. You can check if the plug has
;;; been created by using gtk_socket_get_plug_window(). If it returns a non-NULL
;;; value, then the plug has been successfully created inside of the socket.
;;;
;;; When GTK+ is notified that the embedded window has been destroyed, then it
;;; will destroy the socket as well. You should always, therefore, be prepared
;;; for your sockets to be destroyed at any time when the main event loop is
;;; running. To prevent this from happening, you can connect to the
;;; "plug-removed" signal.
;;;
;;; The communication between a GtkSocket and a GtkPlug follows the XEmbed
;;; protocol. This protocol has also been implemented in other toolkits, e.g.
;;; Qt, allowing the same level of integration when embedding a Qt widget in GTK
;;; or vice versa.
;;;
;;; Note
;;;
;;; The GtkPlug and GtkSocket widgets are only available when GTK+ is compiled
;;; for the X11 platform and GDK_WINDOWING_X11 is defined. They can only be used
;;; on a GdkX11Display. To use GtkPlug and GtkSocket, you need to include the
;;; gtk/gtkx.h header.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "plug-added" signal
;;;
;;; void user_function (GtkSocket *socket_,
;;;                     gpointer   user_data)      : Run Last
;;;
;;; This signal is emitted when a client is successfully added to the socket.
;;;
;;; socket_ :
;;;     the object which received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "plug-removed" signal
;;;
;;; gboolean user_function (GtkSocket *socket_,
;;;                         gpointer   user_data)      : Run Last
;;;
;;; This signal is emitted when a client is removed from the socket. The default
;;; action is to destroy the GtkSocket widget, so if you want to reuse it you
;;; must add a signal handler that returns TRUE.
;;;
;;; socket_ :
;;;     the object which received the signal
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     TRUE to stop other handlers from being invoked.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSocket
;;;
;;; struct GtkSocket;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSocket" gtk-socket
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_socket_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_socket_new ()
;;;
;;; GtkWidget * gtk_socket_new (void);
;;;
;;; Create a new empty GtkSocket.
;;;
;;; Returns :
;;;     the new GtkSocket.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_socket_add_id ()
;;;
;;; void gtk_socket_add_id (GtkSocket *socket_, Window window);
;;;
;;; Adds an XEMBED client, such as a GtkPlug, to the GtkSocket. The client may
;;; be in the same process or in a different process.
;;;
;;; To embed a GtkPlug in a GtkSocket, you can either create the GtkPlug with
;;; gtk_plug_new (0), call gtk_plug_get_id() to get the window ID of the plug,
;;; and then pass that to the gtk_socket_add_id(), or you can call
;;; gtk_socket_get_id() to get the window ID for the socket, and call
;;; gtk_plug_new() passing in that ID.
;;;
;;; The GtkSocket must have already be added into a toplevel window before you
;;; can make this call.
;;;
;;; socket_ :
;;;     a GtkSocket
;;;
;;; window :
;;;     the Window of a client participating in the XEMBED protocol.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_socket_get_id ()
;;;
;;; Window gtk_socket_get_id (GtkSocket *socket_);
;;;
;;; Gets the window ID of a GtkSocket widget, which can then be used to create a
;;; client embedded inside the socket, for instance with gtk_plug_new().
;;;
;;; The GtkSocket must have already be added into a toplevel window before you
;;; can make this call.
;;;
;;; socket_ :
;;;     a GtkSocket.
;;;
;;; Returns :
;;;     the window ID for the socket
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_socket_get_plug_window ()
;;;
;;; GdkWindow * gtk_socket_get_plug_window (GtkSocket *socket_);
;;;
;;; Retrieves the window of the plug. Use this to check if the plug has been
;;; created inside of the socket.
;;;
;;; socket_ :
;;;     a GtkSocket.
;;;
;;; Returns :
;;;     the window of the plug if available, or NULL
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.socket.lisp --------------------------------------------
