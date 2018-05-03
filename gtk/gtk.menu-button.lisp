;;; ----------------------------------------------------------------------------
;;; gtk.menu-button.lisp
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
;;; GtkMenuButton
;;;
;;; A widget that shows a popup when clicked on
;;;
;;; Types and Values
;;;
;;;     GtkMenuButton
;;;
;;; Functions
;;;
;;;     gtk_menu_button_new
;;;     gtk_menu_button_set_popup                          -> Accessor
;;;     gtk_menu_button_get_popup                          -> Accessor
;;;     gtk_menu_button_set_popover                        -> Accessor
;;;     gtk_menu_button_get_popover                        -> Accessor
;;;     gtk_menu_button_set_menu_model                     -> Accessor
;;;     gtk_menu_button_get_menu_model                     -> Accessor
;;;     gtk_menu_button_set_use_popover                    -> Accessor
;;;     gtk_menu_button_get_use_popover                    -> Accessor
;;;     gtk_menu_button_set_direction                      -> Accessor
;;;     gtk_menu_button_get_direction                      -> Accessor
;;;     gtk_menu_button_set_align_widget                   -> Accessor
;;;     gtk_menu_button_get_align_widget                   -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkToggleButton
;;;                             ╰── GtkMenuButton
;;;
;;; Implemented Interfaces
;;;
;;; GtkMenuButton implements AtkImplementorIface, GtkBuildable, GtkActionable
;;; and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuButton
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(define-g-object-class "GtkMenuButton" gtk-menu-button
  (:superclass gtk-toggle-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_menu_button_get_type")
  ((align-widget
    gtk-menu-button-align-widget
    "align-widget" "GtkContainer" t t)
   (direction
    gtk-menu-button-direction
    "direction" "GtkArrowType" t t)
   (menu-model
    gtk-menu-button-menu-model
    "menu-model" "GMenuModel" t t)
   (popover
    gtk-menu-button-popover
    "popover" "GtkPopover" t t)
   (popup
    gtk-menu-button-popup
    "popup" "gboolean" t t)
   (use-popover
    gtk-menu-button-use-popover
    "use-popover" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-menu-button-new))

#+gtk-3-10
(defun gtk-menu-button-new ()
  (make-instance 'gtk-menu-button))

#+gtk-3-10
(export 'gtk-menu-button-new)

;;; End of file gtk.menu-button.lisp -------------------------------------------
