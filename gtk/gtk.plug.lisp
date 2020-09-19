;;; ----------------------------------------------------------------------------
;;; gtk.plug.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     Toplevel for embedding into other processes
;;;
;;; Types and Values
;;;
;;;     GtkPlug
;;;
;;; Functions
;;;
;;;     gtk_plug_construct
;;;     gtk_plug_construct_for_display
;;;     gtk_plug_new
;;;     gtk_plug_new_for_display
;;;     gtk_plug_get_id
;;;     gtk_plug_get_embedded                              Accessor
;;;     gtk_plug_get_socket_window                         Accessor
;;;
;;; Properties
;;;
;;;      gboolean    embedded         Read
;;;     GdkWindow*   socket-window    Read
;;;
;;; Signals
;;;
;;;          void    embedded         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkPlug
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPlug implements AtkImplementorIface and GtkBuildable.
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
 "@version{2020-9-15}
  @begin{short}
    Together with @class{gtk-socket}, @sym{gtk-plug} provides the ability to
    embed widgets from one process into another process in a fashion that is
    transparent to the user.
  @end{short}
  One process creates a @class{gtk-socket} widget and passes the ID of that
  widget's window to the other process, which then creates a @sym{gtk-plug} with
  that window ID. Any widgets contained in the @sym{gtk-plug} then will appear
  inside the first application's window.

  The communication between a @class{gtk-socket} and a @sym{gtk-plug} follows
  the XEmbed protocol. This protocol has also been implemented in other
  toolkits, e.g. Qt, allowing the same level of integration when embedding a Qt
  widget in GTK+ or vice versa.

  The @sym{gtk-plug} and @class{gtk-socket} widgets are only available when GTK+
  is compiled for the X11 platform and @code{GDK_WINDOWING_X11} is defined. They
  can only be used on a @code{gdk-x11-display}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"embedded\" signal}
      @begin{pre}
 lambda (plug)    : Run Last
      @end{pre}
      Gets emitted when the plug becomes embedded in a socket.
      @begin[code]{table}
        @entry[plug]{The @sym{gtk-plug} object on which the signal was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-plug-embedded}
  @see-slot{gtk-plug-socket-window}
  @see-class{gtk-socket}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-plug-embedded ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "embedded" 'gtk-plug) 't)
 "The @code{embedded} property of type @code{:boolean} (Read) @br{}
  @em{True} if the plug is embedded in a socket. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-plug-embedded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-plug-embedded 'function)
 "@version{2020-9-15}
  @syntax[]{(gtk-plug-embedded object) => embedded}
  @argument[object]{a @class{gtk-plug} widget}
  @argument[embedded]{a @code{:boolean} wether the plug is embedded in a socket}
  @begin{short}
    Accessor of the @slot[gtk-plug]{embedded} slot of the @class{gtk-plug}
    class.
  @end{short}

  Determines whether the plug is embedded in a socket.
  @see-class{gtk-plug}")

;;; --- gtk-plug-socket-window -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "socket-window" 'gtk-plug) 't)
 "The @code{socket-window} property of type @class{gdk-window} (Read) @br{}
  The window of the socket the plug is embedded in.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-plug-socket-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-plug-socket-window 'function)
 "@version{2020-9-15}
  @syntax[]{(gtk-plug-socket-window object) => socket-window}
  @argument[object]{a @class{gtk-plug} widget}
  @argument[socket-window]{a @class{gdk-window} of the socket}
  @begin{short}
    Accessor of the @slot[gtk-plug]{socket-window} slot of the @class{gtk-plug}
    class.
  @end{short}

  Retrieves the socket the plug is embedded in.
  @see-class{gtk-plug}")

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_plug_new" gtk-plug-new) (g-object gtk-plug)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-15}
  @argument[socket-id]{a @code{:pointer} with the window ID of the socket, or 0}
  @return{The new @class{gtk-plug} widget.}
  @begin{short}
    Creates a new plug widget inside the @class{gtk-socket} identified by
    @arg{socket-id}.
  @end{short}
  If @arg{socket-id} is 0, the plug is left \"unplugged\" and can later be
  plugged into a @class{gtk-socket} by the function @fun{gtk-socket-add-id}.
  @see-class{gtk-plug}
  @see-class{gtk-socket}
  @see-function{gtk-socket-add-id}"
  (socket-id :pointer))

(export 'gtk-plug-new)

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
;;; gtk_plug_get_id () -> gtk-plug-id
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_plug_get_id" gtk-plug-id) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-9-15}
  @argument[plug]{a @class{gtk-plug} widget}
  @return{A @code{:pointer} with the window ID for the plug.}
  @begin{short}
    Gets the window ID of a @class{gtk-plug} widget, which can then be used to
    embed this window inside another window, for instance with the function
    @fun{gtk-socket-add-id}.
  @end{short}
  @see-class{gtk-plug}
  @see-function{gtk-socket-add-id}"
  (plug (g-object gtk-plug)))

(export 'gtk-plug-id)

;;; --- End of file gtk.plug.lisp ----------------------------------------------
