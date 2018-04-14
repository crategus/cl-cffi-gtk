;;; ----------------------------------------------------------------------------
;;; gtk.action-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2017-2018 Olof-Joachim Frahm
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
;;; GtkActionBar
;;;
;;; A full width bar for presenting contextual actions
;;;
;;; Types and Values
;;;
;;;     GtkActionBar
;;;
;;; Functions
;;;
;;;     gtk_action_bar_new
;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkActionBar
;;;
;;; Implemented Interfaces
;;;
;;; GtkActionBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionBar
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(define-g-object-class "GtkActionBar" gtk-action-bar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_action_bar_get_type")
  ())

#+gtk-3-12
(define-child-property "GtkActionBar"
                       gtk-action-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+gtk-3-12
(define-child-property "GtkActionBar"
                       gtk-action-bar-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(declaim (inline gtk-action-bar-new))

#+gtk-3-12
(defun gtk-action-bar-new ()
  (make-instance 'gtk-action-bar))

#+gtk-3-12
(export 'gtk-action-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_start ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-action-bar-pack-start :void
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

#+gtk-3-12
(export 'gtk-action-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_end ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-action-bar-pack-end :void
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

#+gtk-3-12
(export 'gtk-action-bar-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_get_center_widget ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_action_bar_get_center_widget" gtk-action-bar-center-widget)
    (g-object gtk-widget)
  (action-bar (g-object gtk-action-bar)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_set_center_widget ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_action_bar_set_center_widget" %gtk-action-bar-set-center-widget)
    :void
  (action-bar (g-object gtk-action-bar))
  (center-widget (g-object gtk-widget)))

#+gtk-3-12
(defun (setf gtk-action-bar-center-widget) (new-value action-bar)
  (%gtk-action-bar-set-center-widget action-bar new-value))

#+gtk-3-12
(export 'gtk-action-bar-center-widget)

;;; --- End of file gtk.action-bar.lisp ----------------------------------------
