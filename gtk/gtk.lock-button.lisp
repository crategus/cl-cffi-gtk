;;; ----------------------------------------------------------------------------
;;; gtk.lock-button.lisp
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
;;; GtkLockButton
;;;
;;; A widget that shows a popup when clicked on
;;;
;;; Types and Values
;;;
;;;     GtkLockButton
;;;
;;; Functions
;;;
;;;     gtk_menu_button_new
;;;     gtk_menu_button_get_permission                     -> Accessor
;;;     gtk_menu_button_set_permission                     -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkLockButton
;;;
;;; Implemented Interfaces
;;;
;;; GtkLockButton implements AtkImplementorIface, GtkBuildable, GtkActionable
;;; and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLockButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLockButton" gtk-lock-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_lock_button_get_type")
  ((permission
    gtk-lock-button-permission
    "permission" "GPermission" t t)
   (text-lock
    gtk-lock-button-text-lock
    "text-lock" "gchararray" t t)
   (text-unlock
    gtk-lock-button-text-unlock
    "text-unlock" "gchararray" t t)
   (tooltip-lock
    gtk-lock-button-tooltip-lock
    "tooltip-lock" "gchararray" t t)
   (tooltip-not-authorized
    gtk-lock-button-tooltip-not-authorized
    "tooltip-not-authorized" "gchararray" t t)
   (tooltip-unlock
    gtk-lock-button-tooltip-unlock
    "tooltip-unlock" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_lock_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-lock-button-new))

(defun gtk-lock-button-new (permission)
  (make-instance 'gtk-lock-button :permission permission))

#+gtk-3-10
(export 'gtk-lock-button-new)

;;; End of file gtk.lock-button.lisp -------------------------------------------
