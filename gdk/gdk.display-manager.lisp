;;; ----------------------------------------------------------------------------
;;; gdk.display-manager.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; GdkDisplayManager
;;;
;;;     Maintains a list of all open GdkDisplays
;;;
;;; Types and Values
;;;
;;;     GdkDisplayManager
;;;
;;; Functions
;;;
;;;     gdk_display_manager_get
;;;     gdk_display_manager_get_default_display            Accessor
;;;     gdk_display_manager_set_default_display            Accessor
;;;     gdk_display_manager_list_displays
;;;     gdk_display_manager_open_display
;;;
;;; Properties
;;;
;;;     GdkDisplay*   default-display    Read / Write
;;;
;;; Signals
;;;
;;;           void    display-opened     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDisplayManager
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplayManager
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplayManager" gdk-display-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_manager_get_type")
  ((default-display
    gdk-display-manager-default-display
    "default-display" "GdkDisplay" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-display-manager 'type)
 "@version{2020-11-6}
  @begin{short}
    The purpose of the @sym{gdk-display-manager} singleton object is to offer
    notification when displays appear or disappear or the default display
    changes.
  @end{short}

  You can use the function @fun{gdk-display-manager-get} to obtain the
  @sym{gdk-display-manager} singleton, but that should be rarely necessary.
  Typically, initializing GTK+ opens a display that you can work with without
  ever accessing the @sym{gdk-display-manager} object.

  The GDK library can be built with support for multiple backends. The
  @sym{gdk-display-manager} object determines which backend is used at runtime.

  When writing backend specific code that is supposed to work with multiple
  GDK backends, you have to consider both compile time and runtime. At compile
  time, use the @code{GDK_WINDOWING_X11}, @code{GDK_WINDOWING_WIN32} macros,
  etc. to find out which backends are present in the GDK library you are
  building your application against. At runtime, use type check macros like
  @code{GDK_IS_X11_DISPLAY()} to find out which backend is in use:
  @begin[Example]{dictionary}
    Backend specific code
    @begin{pre}
   #ifdef GDK_WINDOWING_X11
     if (GDK_IS_X11_DISPLAY (display))
       {
         /* make X11-specific calls here */
       @}
     else
   #endif
   #ifdef GDK_WINDOWING_QUARTZ
     if (GDK_IS_QUARTZ_DISPLAY (display))
       {
         /* make Quartz-specific calls here */
       @}
     else
   #endif
     g_error (\"Unsupported GDK backend\");
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"display-opened\" signal}
      @begin{pre}
 lambda (manager display)    :run-last
      @end{pre}
      The signal is emitted when a display is opened.
      @begin[code]{table}
        @entry[manager]{The @sym{gdk-display-manager} object on which the
          signal is emitted.}
        @entry[display]{The opened @class{gdk-display} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk-display-manager-default-display}
  @see-class{gdk-display}
  @see-function{gdk-display-manager-get}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-display-manager-default-display ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "default-display"
                                               'gdk-display-manager) 't)
 "The @code{default-display} property of type @class{gdk-display} (Read / Write)
  @br{}
  The default display for GDK.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-display-manager-default-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-display-manager-default-display 'function)
 "@version{2020-11-6}
  @syntax[]{(gdk-display-manager-default-display object) => display}
  @syntax[]{(setf (gdk-display-manager-default-display object) display)}
  @argument[object]{a @class{gdk-display-manager} object}
  @argument[display]{the default @class{gdk-display} object}
  @begin{short}
    Accessor of the @slot[gdk-display-manager]{default-display} slot of the
    @class{gdk-display-manager} class.
  @end{short}

  The slot access function @sym{gdk-display-manager-default-display} gets the
  default @class{gdk-display} object, or @code{nil} if there is no default
  display. The slot access function
  @sym{(setf gdk-display-manager-default-display)} sets @arg{display} as the
  default display.
  @begin[Example]{dictionary}
    @begin{pre}
 (gdk-display-manager-default-display (gdk-display-manager-get))
=> #<GDK-DISPLAY {1001F9A233@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk-display}
  @see-class{gdk-display-manager}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_manager_get" gdk-display-manager-get)
    (g-object gdk-display-manager)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-6}
  @return{The global @class{gdk-display-manager} singleton object.}
  @begin{short}
    Gets the @class{gdk-display-manager} singleton object.
  @end{short}

  When called for the first time, this function consults the @code{GDK_BACKEND}
  environment variable to find out which of the supported GDK backends to use
  in case GDK has been compiled with multiple backends.
  @begin[Example]{dictionary}
    @begin{pre}
 (gdk-display-manager-get)
=> #<GDK-DISPLAY-MANAGER {1001CFF103@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk-display-manager}")

(export 'gdk-display-manager-get)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_manager_list_displays" gdk-display-manager-list-displays)
    (g-slist (g-object gdk-display) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-6}
  @argument[manager]{a @class{gdk-display-manager} object}
  @return{A list of @class{gdk-display} objects.}
  @short{List all currently open displays.}
  @see-class{gdk-display}
  @see-class{gdk-display-manager}"
  (manager (g-object gdk-display-manager)))

(export 'gdk-display-manager-list-displays)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_open_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_manager_open_display" gdk-display-manager-open-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-6}
  @argument[manager]{a @class{gdk-display-manager} object}
  @argument[name]{a string with the name of the display to open}
  @begin{return}
    A @class{gdk-display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @short{Opens a display.}
  @see-class{gdk-display}
  @see-class{gdk-display-manager}"
  (manager (g-object gdk-display-manager))
  (name :string))

(export 'gdk-display-manager-open-display)

;;; --- End of file gdk.display-manager.lisp -----------------------------------
