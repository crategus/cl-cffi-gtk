;;; ----------------------------------------------------------------------------
;;; gdk.display-manager.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
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
;;; GdkDisplayManager
;;; 
;;; Maintains a list of all open GdkDisplays
;;;     
;;; Synopsis
;;; 
;;;     GdkDisplayManager
;;;
;;;     gdk_display_manager_get
;;;     gdk_display_manager_get_default_display
;;;     gdk_display_manager_set_default_display
;;;     gdk_display_manager_list_displays
;;;     gdk_display_manager_open_display
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkDisplayManager
;;; 
;;; Properties
;;; 
;;;   "default-display"          GdkDisplay*           : Read / Write
;;; 
;;; Signals
;;; 
;;;   "display-opened"                                 : Run Last
;;; 
;;; Description
;;; 
;;; The purpose of the GdkDisplayManager singleton object is to offer
;;; notification when displays appear or disappear or the default display
;;; changes.
;;; 
;;; You can use gdk_display_manager_get() to obtain the GdkDisplayManager
;;; singleton, but that should be rarely necessary. Typically, initializing GTK+
;;; opens a display that you can work with without ever accessing the
;;; GdkDisplayManager.
;;; 
;;; The GDK library can be built with support for multiple backends. The
;;; GdkDisplayManager object determines which backend is used at runtime.
;;; 
;;; When writing backend-specific code that is supposed to work with multiple
;;; GDK backends, you have to consider both compile time and runtime. At compile
;;; time, use the GDK_WINDOWING_X11, GDK_WINDOWING_WIN32 macros, etc. to find
;;; out which backends are present in the GDK library you are building your
;;; application against. At runtime, use type-check macros like
;;; GDK_IS_X11_DISPLAY() to find out which backend is in use:
;;; 
;;; Example 2. Backend-specific code
;;; 
;;;   #ifdef GDK_WINDOWING_X11
;;;     if (GDK_IS_X11_DISPLAY (display))
;;;       {
;;;         /* make X11-specific calls here */
;;;       }
;;;     else
;;;   #endif
;;;   #ifdef GDK_WINDOWING_QUARTZ
;;;     if (GDK_IS_QUARTZ_DISPLAY (display))
;;;       {
;;;         /* make Quartz-specific calls here */
;;;       }
;;;     else
;;;   #endif
;;;     g_error ("Unsupported GDK backend");
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "default-display" property
;;; 
;;;   "default-display"          GdkDisplay*           : Read / Write
;;; 
;;; The default display for GDK.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "display-opened" signal
;;; 
;;; void user_function (GdkDisplayManager *manager,
;;;                     GdkDisplay        *display,
;;;                     gpointer           user_data)      : Run Last
;;; 
;;; The ::display-opened signal is emitted when a display is opened.
;;; 
;;; manager :
;;;     the object on which the signal is emitted
;;; 
;;; display :
;;;     the opened display
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplayManager
;;; 
;;; typedef struct _GdkDisplayManager GdkDisplayManager;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplayManager" gdk-display-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_manager_get_type")
  ((default-display
    gdk-display-manager-default-display
    "default-display" "GdkDisplay" t t)
   (:cffi displays
          gdk-display-manager-displays
          (g-slist (g-object gdk-display) :free-from-foreign t)
          "gdk_display_manager_list_displays" nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get ()
;;; 
;;; GdkDisplayManager * gdk_display_manager_get (void);
;;; 
;;; Gets the singleton GdkDisplayManager object.
;;; 
;;; When called for the first time, this function consults the GDK_BACKEND
;;; environment variable to find out which of the supported GDK backends to use
;;; (in case GDK has been compiled with multiple backends).
;;; 
;;; Returns :
;;;     The global GdkDisplayManager singleton; gdk_parse_args(), gdk_init(), or
;;;     gdk_init_check() must have been called first.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_manager_get" gdk-display-manager-get)
    (g-object gdk-display-manager))

(export 'gdk-display-manager-get)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get_default_display ()
;;; 
;;; GdkDisplay * gdk_display_manager_get_default_display
;;;                                                (GdkDisplayManager *manager);
;;; 
;;; Gets the default GdkDisplay.
;;; 
;;; manager :
;;;     a GdkDisplayManager
;;; 
;;; Returns :
;;;     a GdkDisplay, or NULL if there is no default display
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-display-manager-get-default-display))

(defun gdk-display-manager-get-default-display (display-manager)
  (gdk-display-manager-default-display display-manager))

(export 'gdk-display-manager-get-default-display)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_set_default_display ()
;;; 
;;; void gdk_display_manager_set_default_display (GdkDisplayManager *manager,
;;;                                               GdkDisplay *display);
;;; 
;;; Sets display as the default display.
;;; 
;;; manager :
;;;     a GdkDisplayManager
;;; 
;;; display :
;;;     a GdkDisplay
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-display-manager-set-default-display))

(defun gdk-display-manager-set-default-display (display-manager display)
  (setf (gdk-display-manager-default-display display-manager) display))

(export 'gdk-display-manager-set-default-display)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays ()
;;; 
;;; GSList * gdk_display_manager_list_displays (GdkDisplayManager *manager);
;;; 
;;; List all currently open displays.
;;; 
;;; manager :
;;;     a GdkDisplayManager
;;; 
;;; Returns :
;;;     a newly allocated GSList of GdkDisplay objects. Free with g_slist_free()
;;;     when you are done with it
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-display-manager-list-displays))

(defun gdk-display-manager-list-displays (display-manager)
  (gdk-display-manager-displays display-manager))

(export 'gdk-display-manager-list-displays)

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_open_display ()
;;; 
;;; GdkDisplay * gdk_display_manager_open_display (GdkDisplayManager *manager,
;;;                                                const gchar *name);
;;; 
;;; Opens a display.
;;; 
;;; manager :
;;;     a GdkDisplayManager
;;; 
;;; name :
;;;     the name of the display to open
;;; 
;;; Returns :
;;;     a GdkDisplay, or NULL if the display could not be opened
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------


;;; --- End of file gdk.display-manager.lisp -----------------------------------
