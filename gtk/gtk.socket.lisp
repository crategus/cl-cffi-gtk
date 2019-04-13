;;; ----------------------------------------------------------------------------
;;; gtk.socket.lisp
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
;;; GtkSocket
;;;
;;;     Container for widgets from other processes
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
;;; Signals
;;;
;;;         void   plug-added      Run Last
;;;     gboolean   plug-removed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkSocket
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSocket implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSocket
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSocket" gtk-socket
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_socket_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-socket 'type)
 "@version{2013-5-26}
  @begin{short}
    Together with @class{gtk-plug}, @sym{gtk-socket} provides the ability to
    embed widgets from one process into another process in a fashion that is
    transparent to the user. One process creates a @sym{gtk-socket} widget and
    passes that widget's window ID to the other process, which then creates a
    @class{gtk-plug} with that window ID. Any widgets contained in the
    @class{gtk-plug} then will appear inside the first application's window.
  @end{short}

  The socket's window ID is obtained by using the function
  @fun{gtk-socket-get-id}. Before using this function, the socket must have been
  realized, and for hence, have been added to its parent.

  @b{Example:} Obtaining the window ID of a socket.
  @begin{pre}
   GtkWidget *socket = gtk_socket_new ();
   gtk_widget_show (socket);
   gtk_container_add (GTK_CONTAINER (parent), socket);

   /* The following call is only necessary if one of
    * the ancestors of the socket is not yet visible.
    */
   gtk_widget_realize (socket);
   g_print (\"The ID of the sockets window is
             %<GTKDOCLINK HREF=\"x\">x</GTKDOCLINK>\n\",
            gtk_socket_get_id (socket));
  @end{pre}
  Note that if you pass the window ID of the socket to another process that
  will create a plug in the socket, you must make sure that the socket widget
  is not destroyed until that plug is created. Violating this rule will cause
  unpredictable consequences, the most likely consequence being that the plug
  will appear as a separate toplevel window. You can check if the plug has
  been created by using the function @fun{gtk-socket-get-plug-window}. If it
  returns a non-@code{nil} value, then the plug has been successfully created
  inside of the socket.

  When GTK+ is notified that the embedded window has been destroyed, then it
  will destroy the socket as well. You should always, therefore, be prepared
  for your sockets to be destroyed at any time when the main event loop is
  running. To prevent this from happening, you can connect to the
  \"plug-removed\" signal.

  The communication between a @sym{gtk-socket} and a @class{gtk-plug} follows
  the XEmbed protocol. This protocol has also been implemented in other
  toolkits, e. g. Qt, allowing the same level of integration when embedding a
  Qt widget in GTK or vice versa.

  @subheading{Note}
    The @class{gtk-plug} and @sym{gtk-socket} widgets are only available when
    GTK+ is compiled for the X11 platform and @code{GDK_WINDOWING_X11} is
    defined. They can only be used on a @code{gdk-x11-display}. To use
    @class{gtk-plug} and @sym{gtk-socket}, you need to include the
    @code{gtk/gtkx.h} header.
  @begin[Signal Details]{dictionary}
    @subheading{The \"plug-added\" signal}
      @begin{pre}
 lambda (socket)    : Run Last
      @end{pre}
      This signal is emitted when a client is successfully added to the socket.
      @begin[code]{table}
        @entry[socket]{The object which received the signal.}
      @end{table}
    @subheading{The \"plug-removed\" signal}
      @begin{pre}
 lambda (socket)    : Run Last
      @end{pre}
      This signal is emitted when a client is removed from the socket. The
      default action is to destroy the @sym{gtk-socket} widget, so if you want
      to reuse it you must add a signal handler that returns @em{true}.
      @begin[code]{table}
        @entry[socket]{The object which received the signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-plug}")

;;; ----------------------------------------------------------------------------
;;; gtk_socket_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-socket-new))

(defun gtk-socket-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-11-27}
  @return{The new @class{gtk-socket}.}
  Create a new empty @class{gtk-socket}.
  @see-class{gtk-socket}")

(export 'gtk-socket-new)

;;; ----------------------------------------------------------------------------
;;; gtk_socket_add_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_socket_add_id" gtk-socket-add-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-27}
  @argument[socket]{a @class{gtk-socket} widget}
  @argument[window]{the Window of a client participating in the XEMBED protocol}
  @begin{short}
    Adds an XEMBED client, such as a @class{gtk-plug}, to the
    @class{gtk-socket}.
  @end{short}
  The client may be in the same process or in a different process.

  To embed a @class{gtk-plug} in a @class{gtk-socket}, you can either create the
  @class{gtk-plug} with @code{gtk-plug-new 0)}, call the function
  @fun{gtk-plug-get-id} to get the window ID of the plug, and then pass that to
  the function @sym{gtk-socket-add-id}, or you can call the function
  @fun{gtk-socket-get-id} to get the window ID for the socket, and call the
  function @fun{gtk-plug-new} passing in that ID.

  The @class{gtk-socket} must have already be added into a toplevel window
  before you can make this call.
  @see-class{gtk-socket}
  @see-class{gtk-plug}
  @see-function{gtk-plug-new}
  @see-function{gtk-plug-get-id}
  @see-function{gtk-socket-get-id}"
  (socket (g-object gtk-socket))
  (window :pointer))

(export 'gtk-socket-add-id)

;;; ----------------------------------------------------------------------------
;;; gtk_socket_get_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_socket_get_id" gtk-socket-get-id) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-11-27}
  @argument[socket]{a @class{gtk-socket}}
  @return{The window ID for the socket.}
  @begin{short}
    Gets the window ID of a @class{gtk-socket} widget, which can then be used
    to create a client embedded inside the socket, for instance with the
    function @fun{gtk-plug-new}.
  @end{short}

  The @class{gtk-socket} must have already be added into a toplevel window
  before you can make this call.
  @see-class{gtk-socket}
  @see-function{gtk-plug-new}"
  (socket (g-object gtk-socket)))

(export 'gtk-socket-get-id)

;;; ----------------------------------------------------------------------------
;;; gtk_socket_get_plug_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_socket_get_plug_window" gtk-socket-get-plug-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-15}
  @argument[socket]{a @class{gtk-socket} widget}
  @return{The window of the plug if available, or @code{nil}.}
  @begin{short}
    Retrieves the window of the plug.
  @end{short}
  Use this to check if the plug has been created inside of the socket.
  @see-class{gtk-socket}
  @see-class{gdk-window}"
  (socket (g-object gtk-socket)))

(export 'gtk-socket-get-plug-window)

;;; --- End of file gtk.socket.lisp --------------------------------------------
