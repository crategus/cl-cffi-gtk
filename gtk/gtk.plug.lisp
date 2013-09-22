;;; ----------------------------------------------------------------------------
;;; gtk.plug.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;;
;;; Properties
;;;
;;;   "embedded"                 gboolean              : Read
;;;   "socket-window"            GdkWindow*            : Read
;;;
;;; Signals
;;;
;;;   "embedded"                                       : Run Last
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPlug
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPlug" gtk-plug
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_plug_get_type")
  ((embedded
    gtk-plug-embedded
    "embedded" "gboolean" t nil)
   (socket-window
    gtk-plug-socket-window
    "socket-window" "GdkWindow" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-plug 'type)
 "@version{2013-5-26}
  @begin{short}
    Together with @class{gtk-socket}, @sym{gtk-plug} provides the ability to
    embed widgets from one process into another process in a fashion that is
    transparent to the user. One process creates a @class{gtk-socket} widget and
    passes the ID of that widget's window to the other process, which then
    creates a @sym{gtk-plug} with that window ID. Any widgets contained in the
    @sym{gtk-plug} then will appear inside the first application's window.
  @end{short}

  The communication between a @class{gtk-socket} and a @sym{gtk-plug} follows
  the XEmbed protocol. This protocol has also been implemented in other
  toolkits, e. g. Qt, allowing the same level of integration when embedding a Qt
  widget in GTK+ or vice versa.

  @subheading{Note}
    The @sym{gtk-plug} and @class{gtk-socket} widgets are only available when
    GTK+ is compiled for the X11 platform and @code{GDK_WINDOWING_X11} is
    defined. They can only be used on a @code{gdk-x11-display}. To use
    @sym{gtk-plug} and @class{gtk-socket}, you need to include the
    @code{gtk/gtkx.h} header.
  @begin[Signal Details]{dictionary}
    @subheading{The \"embedded\" signal}
      @begin{pre}
 lambda (plug)   : Run Last
      @end{pre}
      Gets emitted when the plug becomes embedded in a socket.
      @begin[code]{table}
        @entry[plug]{The object on which the signal was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-plug-embedded}
  @see-slot{gtk-plug-socket-window}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embedded" 'gtk-plug) 't)
 "The @code{\"embedded\"} property of type @code{:boolean} (Read) @br{}
  @em{True} if the plug is embedded in a socket. @br{}
  Default value: @code{nil} @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "socket-window" 'gtk-plug) 't)
 "The @code{\"socket-window\"} property of type @class{gdk-window} (Read) @br{}
  The window of the socket the plug is embedded in. @br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-plug-embedded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-plug-embedded 'function)
 "@version{2013-5-26}
  Accessor of the slot @code{\"embedded\"} of the @class{gtk-plug} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-plug-socket-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-plug-socket-window 'function)
 "@version{2013-5-26}
  Accessor of the slot @code{\"socket-window\"} of the @class{gtk-plug} class.")

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
;;;     a GtkPlug.
;;;
;;; socket_id :
;;;     the XID of the socket's window.
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
;;;     the GdkDisplay associated with socket_id's GtkSocket.
;;;
;;; socket_id :
;;;     the XID of the socket's window.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_new ()
;;;
;;; GtkWidget * gtk_plug_new (Window socket_id);
;;;
;;; Creates a new plug widget inside the GtkSocket identified by socket_id. If
;;; socket_id is 0, the plug is left "unplugged" and can later be plugged into a
;;; GtkSocket by gtk_socket_add_id().
;;;
;;; socket_id :
;;;     the window ID of the socket, or 0.
;;;
;;; Returns :
;;;     the new GtkPlug widget.
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
;;;     the XID of the socket's window.
;;;
;;; Returns :
;;;     the new GtkPlug widget.
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
