;;; ----------------------------------------------------------------------------
;;; gtk.application-window.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013, 2014 Dieter Kaiser
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
;;; GtkApplicationWindow
;;;
;;; GtkWindow subclass with GtkApplication support
;;;
;;; Synopsis
;;;
;;;     GtkApplicationWindow
;;;
;;;     gtk_application_window_new
;;;     gtk_application_window_set_show_menubar
;;;     gtk_application_window_get_show_menubar
;;;     gtk_application_window_get_id
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplicationWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkApplicationWindow" gtk-application-window
  (:superclass gtk-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_window_get_type")
   ((show-menubar
     gtk-application-window-show-menubar
     "show-menubar" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-application-window 'type)
 "@version{2014-7-19}
  @begin{short}
    @sym{gtk-application-window} is a @class{gtk-window} subclass that offers
    some extra functionality for better integration with @class{gtk-application}
    features.
  @end{short}
  Notably, it can handle both the application menu as well as the menubar. See
  the functions @fun{gtk-application-app-menu} and
  @fun{gtk-application-menubar}.

  This class implements the @class{g-action-group} and @class{g-action-map}
  interfaces, to let you add window-specific actions that will be exported by
  the associated @class{gtk-application}, together with its application-wide
  actions. Window-specific actions are prefixed with the \"win.\" prefix and
  application-wide actions are prefixed with the \"app.\" prefix. Actions must
  be addressed with the prefixed name when referring to them from a
  @class{g-menu-model}.

  Note that widgets that are placed inside a @sym{gtk-application-window} can
  also activate these actions, if they implement the @class{gtk-actionable}
  interface.

  As with @class{gtk-application}, the GDK lock will be acquired when processing
  actions arriving from other processes and should therefore be held when
  activating actions locally if GDK threads are enabled.

  The settings @slot[gtk-settings]{gtk-shell-shows-app-menu} and
  @slot[gtk-settings]{gtk-shell-shows-menubar} tell GTK+ whether the desktop
  environment is showing the application menu and menubar models outside the
  application as part of the desktop shell. For instance, on OS X, both menus
  will be displayed remotely; on Windows neither will be. gnome-shell, starting
  with version 3.4, will display the application menu, but not the menubar.

  If the desktop environment does not display the menubar, then
  @sym{gtk-application-window} will automatically show a @class{gtk-menu-bar}
  for it. See the @class{gtk-application} docs for some screenshots of how this
  looks on different platforms. This behaviour can be overridden with the
  @slot[gtk-application-window]{show-menubar} property. If the desktop
  environment does not display the application menu, then it will automatically
  be included in the menubar.

  @b{Example:} A @class{gtk-application-window} with a menubar
  @begin{pre}
 ;; Intitialize the menubar
 (let ((builder (make-instance 'gtk-builder)))
   ;; Read the menus from a string
   (gtk-builder-add-from-string
       builder
       (format nil
               \"<interface> ~
                   <menu id='menubar'> ~
                     <submenu label='_Edit'> ~
                       <item label='_Copy' action='win.copy'/> ~
                       <item label='_Paste' action='win.paste'/> ~
                     </submenu> ~
                   </menu> ~
                 </interface>\"))
   ;; Set the menubar
   (setf (gtk-application-menubar application)
         (gtk-builder-get-object builder \"menubar\"))
   ... )
  @end{pre}
  The XML format understood by @class{gtk-builder} for @class{g-menu-model}
  consists of a toplevel @code{<menu>} element, which contains one or more
  @code{<item>} elements. Each @code{<item>} element contains @code{<attribute>}
  and @code{<link>} elements with a mandatory name attribute. @code{<link>}
  elements have the same content model as @code{<menu>}.

  Attribute values can be translated using gettext, like other
  @class{gtk-builder} content. @code{<attribute>} elements can be marked for
  translation with a translatable = \"yes\" attribute. It is also possible to
  specify message context and translator comments, using the context and
  comments attributes. To make use of this, the @class{gtk-builder} must have
  been given the gettext domain to use.
  @see-slot{gtk-application-window-show-menubar}
  @see-class{gtk-window}
  @see-class{gtk-application}
  @see-class{g-action-group}
  @see-class{g-action-map}
  @see-class{g-menu-model}
  @see-class{gtk-actionable}
  @see-class{gtk-menu-bar}
  @see-class{gtk-builder}
  @see-class{g-menu-model}
  @see-function{gtk-application-app-menu}
  @see-function{gtk-application-menubar}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-menubar"
                                               'gtk-application-window) 't)
 "The @code{\"show-menubar\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If this property is @em{true}, the window will display a menubar that includes
  the app menu and menubar, unless these are shown by the desktop shell. See the
  functions @fun{gtk-application-app-menu} and @fun{gtk-application-menubar}.
  If @code{nil}, the window will not display a menubar, regardless of whether
  the desktop shell is showing the menus or not. @br{}
  Default value: @code{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-application-window-show-menubar atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-application-window-show-menubar 'function)
 "@version{2014-2-12}
  @argument[window]{a @class{gtk-application-window} widget}
  @argument[show-menubar]{whether to show a menubar when needed}
  @syntax[]{(gtk-application-window-show-menubar object) => show-menubar}
  @syntax[]{(setf (gtk-application-window-show-menubar object) show-menubar)}
  @begin{short}
    Accessor of the slot @slot[gtk-application-window]{show-menubar} of the
    @class{gtk-application-window} class.
  @end{short}

  The generic function @sym{gtk-application-window-show-menubar} returns whether
  the window will display a menubar for the app menu and menubar as needed.

  The generic function @sym{(setf gtk-application-window-show-menubar)} sets
  whether the window will display a menubar for the app menu and menubar as
  needed.

  Since 3.4
  @see-class{gtk-application-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_application_window_new" gtk-application-window-new)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[application]{a @class{gtk-application} object}
  @return{A newly created @class{gtk-application-window} widget.}
  @short{Creates a new @class{gtk-application-window} widget.}

  Since 3.4
  @see-class{gtk-application}
  @see-class{gtk-application-window}"
  (application (g-object gtk-application)))

(export 'gtk-application-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_id ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(defcfun ("gtk_application_window_get_id" gtk-application-window-get-id) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[window]{a @class{gtk-application-window} widget}
  @begin{return}
    The unique ID for @arg{window}, or 0 if the @arg{window} has not yet been
    added to a @class{gtk-application}.
  @end{return}
  @begin{short}
    Returns the unique ID of the @arg{window}. If the @arg{window} has not yet
    been added to a @class{gtk-application}, returns 0.
  @end{short}

  Since 3.6
  @see-class{gtk-application}
  @see-class{gtk-application-window}"
  (window (g-object gtk-application-window)))

#+gtk-3-6
(export 'gtk-application-window-get-id)

;;; --- End of file gtk.application-window.lisp --------------------------------
