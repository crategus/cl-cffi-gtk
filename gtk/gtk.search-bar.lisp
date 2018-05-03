;;; ----------------------------------------------------------------------------
;;; gtk.search-bar.lisp
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
;;; GtkSearchBar
;;;
;;; A toolbar to integrate a search entry with
;;;
;;; Types and Values
;;;
;;;     GtkSearchBar
;;;
;;; Functions
;;;
;;;     gtk_search_bar_new
;;;     gtk_search_bar_connect_entry
;;;     gtk_search_bar_get_search_mode                     -> Accessor
;;;     gtk_search_bar_set_search_mode                     -> Accessor
;;;     gtk_search_bar_get_show_close_button               -> Accessor
;;;     gtk_search_bar_set_show_close_button               -> Accessor
;;;     gtk_search_bar_handle_event
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkSearchBar
;;;
;;; Implemented Interfaces
;;;
;;; GtkSearchBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSearchBar
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkSearchBar" gtk-search-bar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_search_bar_get_type")
  ((search-mode-enabled
    gtk-search-bar-search-mode-enabled
    "search-mode-enabled" "gboolean" t t)
   (show-close-button
    gtk-search-bar-show-close-button
    "show-close-button" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-search-bar-new))

#+gtk-3-10
(defun gtk-search-bar-new ()
  (make-instance 'gtk-search-bar))

#+gtk-3-10
(export 'gtk-search-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_connect_entry ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-search-bar-connect-entry :void
  (bar (g-object gtk-search-bar))
  (entry (g-object gtk-entry)))

#+gtk-3-10
(export 'gtk-search-bar-connect-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_handle_event ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-search-bar-handle-event :boolean
  (bar (g-object gtk-search-bar))
  (event (g-boxed-foreign gdk-event)))

#+gtk-3-10
(export 'gtk-search-bar-handle-event)

;;; End of file gtk.search-bar.lisp --------------------------------------------
