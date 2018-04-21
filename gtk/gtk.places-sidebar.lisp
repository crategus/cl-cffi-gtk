;;; ----------------------------------------------------------------------------
;;; gtk.places-sidebar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2018 Olof-Joachim Frahm
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
;;; GtkPlacesSidebar
;;;
;;; Sidebar that displays frequently-used places in the file system
;;;
;;; Types and Values
;;;
;;;     GtkPlacesSidebar
;;;     GtkPlacesOpenFlags
;;;
;;; Functions
;;;
;;;     gtk_places_sidebar_new
;;;     gtk_places_sidebar_set_open_flags                  -> Accessor
;;;     gtk_places_sidebar_get_open_flags                  -> Accessor
;;;     gtk_places_sidebar_set_location                    -> Accessor
;;;     gtk_places_sidebar_get_location                    -> Accessor
;;;     gtk_places_sidebar_set_show_recent                 -> Accessor
;;;     gtk_places_sidebar_get_show_recent                 -> Accessor
;;;     gtk_places_sidebar_set_show_desktop                -> Accessor
;;;     gtk_places_sidebar_get_show_desktop                -> Accessor
;;;     gtk_places_sidebar_add_shortcut
;;;     gtk_places_sidebar_remove_shortcut
;;;     gtk_places_sidebar_list_shortcuts
;;;     gtk_places_sidebar_get_nth_bookmark
;;;     gtk_places_sidebar_get_show_connect_to_server      -> Accessor
;;;     gtk_places_sidebar_set_show_connect_to_server      -> Accessor
;;;     gtk_places_sidebar_get_local_only                  -> Accessor
;;;     gtk_places_sidebar_set_local_only                  -> Accessor
;;;     gtk_places_sidebar_get_show_enter_location         -> Accessor
;;;     gtk_places_sidebar_set_show_enter_location         -> Accessor
;;;     gtk_places_sidebar_get_show_trash                  -> Accessor
;;;     gtk_places_sidebar_set_show_trash                  -> Accessor
;;;     gtk_places_sidebar_get_show_other_locations        -> Accessor
;;;     gtk_places_sidebar_set_show_other_locations        -> Accessor
;;;     gtk_places_sidebar_set_drop_targets_visible
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkScrolledWindow
;;;                         ╰── GtkPlacesSidebar
;;;
;;; Implemented Interfaces
;;;
;;; GtkPlacesSidebar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPlacesSidebar
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkPlacesSidebar" gtk-places-sidebar
  (:superclass gtk-scrolled-window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_places_sidebar_get_type")
  (#+gtk-3-12
   (local-only
    gtk-places-sidebar-local-only
    "local-only" "gboolean" t t)
   (location
    gtk-places-sidebar-location
    "location" "GFile" t t)
   (open-flags
    gtk-places-sidebar-open-flags
    "open-flags" "GtkPlacesOpenFlags" t t)
   #+gtk-3-18
   (populate-all
    gtk-places-sidebar-populate-all
    "populate-all" "gboolean" t t)
   (show-connect-to-server
    gtk-places-sidebar-show-connect-to-server
    "show-connect-to-server" "gboolean" t t)
   (show-desktop
    gtk-places-sidebar-show-desktop
    "show-desktop" "gboolean" t t)
   #+gtk-3-14
   (show-enter-location
    gtk-places-sidebar-show-enter-location
    "show-enter-location" "gboolean" t t)
   #+gtk-3-18
   (show-other-locations
    gtk-places-sidebar-show-other-locations
    "show-other-locations" "gboolean" t t)
   #+gtk-3-18
   (show-recent
    gtk-places-sidebar-show-recent
    "show-recent" "gboolean" t t)
   ;; FIXME
   #+gtk-3-22-26
   (show-starred-locations
    gtk-places-sidebar-show-starred-locations
    "show-starred-locations" "gboolean" t t)
   #+gtk-3-18
   (show-trash
    gtk-places-sidebar-show-trash
    "show-trash" "gboolean" t t)))

(deprecated-function :gtk gtk-places-sidebar-show-connect-to-server (3 18) ())

;;; ----------------------------------------------------------------------------
;;; GtkPlacesOpenFlags
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-flags "GtkPlacesOpenFlags" gtk-places-open-flags
  (:export t
   :type-initializer "gtk_places_open_flags_get_type")
  (:normal #.(ash 1 0))
  (:new-tab #.(ash 1 1))
  (:new-window #.(ash 1 2)))

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-places-sidebar-new))

#+gtk-3-10
(defun gtk-places-sidebar-new ()
  (make-instance 'gtk-places-sidebar))

#+gtk-3-10
(export 'gtk-places-sidebar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_add_shortcut ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-places-sidebar-add-shortcut :void
  (widget (g-object gtk-places-sidebar))
  (location (g-object g-file)))

#+gtk-3-10
(export 'gtk-places-sidebar-add-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_remove_shortcut ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-places-sidebar-remove-shortcut :void
  (widget (g-object gtk-places-sidebar))
  (location (g-object g-file)))

#+gtk-3-10
(export 'gtk-places-sidebar-remove-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_list_shortcuts ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-places-sidebar-list-shortcuts (g-slist (g-object g-file))
  (widget (g-object gtk-places-sidebar)))

#+gtk-3-10
(export 'gtk-places-sidebar-list-shortcuts)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_get_nth_bookmark ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-places-sidebar-get-nth-bookmark (g-object g-file)
  (widget (g-object gtk-places-sidebar))
  (n :int))

#+gtk-3-10
(export 'gtk-places-sidebar-get-nth-bookmark)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_set_drop_targets_visible ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-places-sidebar-set-drop-targets-visible :void
  (widget (g-object gtk-places-sidebar))
  (visible :boolean)
  (context (g-object gdk-drag-context)))

#+gtk-3-10
(export 'gtk-places-sidebar-set-drop-targets-visible)

;;; End of file gtk.places-sidebar.lisp ----------------------------------------
