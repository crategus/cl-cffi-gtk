;;; ----------------------------------------------------------------------------
;;; gdk.manager.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;;     gdk_display_manager_get
;;;     gdk_display_manager_get_default_display
;;;     gdk_display_manager_set_default_display
;;;     gdk_display_manager_list_displays
;;;     gdk_display_get_core_pointer
;;;
;;; Object Hierarchy
;;;
;;;  GObject
;;;   +----GdkDisplayManager
;;;
;;; Description
;;;
;;; The purpose of the GdkDisplayManager singleton object is to offer
;;; notification when displays appear or disappear or the default display
;;; changes.
;;; ----------------------------------------------------------------------------
;;;
;;; Properties
;;;
;;; The "default-display" property
;;;
;;;  "default-display" GdkDisplay* : Read / Write
;;;
;;; The default display for GDK.
;;; ----------------------------------------------------------------------------
;;;
;;; Signals
;;;
;;; The "display-opened" signal
;;;
;;; void user_function (GdkDisplayManager *display_manager,
;;;                     GdkDisplay        *display,
;;;                     gpointer           user_data)            : Run Last
;;;
;;; The ::display_opened signal is emitted when a display is opened.
;;;
;;; display_manager :
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
;;;
;;; The GdkDisplayManager struct has no interesting fields.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplayManager" gdk-display-manager
  (:type-initializer "gdk_display_manager_get_type")
  ((default-display gdk-display-manager-default-display
                    "default-display" "GdkDisplay" t t)
   (:cffi displays
          gdk-display-manager-displays
          (g-slist (g-object gdk-display) :free-from-foreign t)
          "gdk_display_manager_list_displays"
          nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_get ()
;;;
;;; GdkDisplayManager * gdk_display_manager_get (void)
;;;
;;; Gets the singleton GdkDisplayManager object.
;;;
;;; Returns :
;;;     The global GdkDisplayManager singleton; gdk_parse_pargs(), gdk_init(),
;;;     or gdk_init_check() must have been called first.
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
;;;                                         (GdkDisplayManager *display_manager)
;;;
;;; Gets the default GdkDisplay.
;;;
;;; display_manager :
;;;     a GdkDisplayManager
;;;
;;; Returns :
;;;     a GdkDisplay, or NULL if there is no default display.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_set_default_display ()
;;;
;;; void gdk_display_manager_set_default_display
;;;                                         (GdkDisplayManager *display_manager,
;;;                                          GdkDisplay *display)
;;;
;;; Sets display as the default display.
;;;
;;; display_manager :
;;;     a GdkDisplayManager
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_manager_list_displays ()
;;;
;;; GSList * gdk_display_manager_list_displays
;;;                                         (GdkDisplayManager *display_manager)
;;;
;;; List all currently open displays.
;;;
;;; display_manager :
;;;     a GdkDisplayManager
;;;
;;; Returns :
;;;     a newly allocated GSList of GdkDisplay objects. Free this list with
;;;     g_slist_free() when you are done with it.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_core_pointer ()
;;;
;;; GdkDevice * gdk_display_get_core_pointer (GdkDisplay *display)
;;;
;;; Returns the core pointer device for the given display
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     the core pointer device; this is owned by the display and should
;;;     not be freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; --- End of file gdk.manager.lisp -------------------------------------------
