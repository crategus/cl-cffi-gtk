;;; ----------------------------------------------------------------------------
;;; gtk.revealer.lisp
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
;;; GtkRevealer
;;;
;;; Hide and show with animation
;;;
;;; Types and Values
;;;
;;;     GtkRevealer
;;;     GtkRevealerTransitionType
;;;
;;; Functions
;;;
;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child                      -> Accessor
;;;     gtk_revealer_set_reveal_child                      -> Accessor
;;;     gtk_revealer_get_child_revealed                    -> Accessor
;;;     gtk_revealer_get_transition_duration               -> Accessor
;;;     gtk_revealer_set_transition_duration               -> Accessor
;;;     gtk_revealer_get_transition_type                   -> Accessor
;;;     gtk_revealer_set_transition_type                   -> Accessor
;;;     gtk_list_box_set_selection_mode                    -> Accessor
;;;     gtk_list_box_get_selection_mode                    -> Accessor
;;;     gtk_list_box_set_activate_on_single_click          -> Accessor
;;;     gtk_list_box_get_activate_on_single_click          -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkRevealer
;;;
;;; Implemented Interfaces
;;;
;;; GtkRevealer implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRevealer
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkRevealer" gtk-revealer
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_revealer_get_type")
  ((child-revealed
    gtk-revealer-child-revealed
    "child-revealed" "gboolean" t nil)
   (reveal-child
    gtk-revealer-reveal-child
    "reveal-child" "gboolean" t t)
   (transition-duration
    gtk-revealer-transition-duration
    "transition-duration" "guint" t t)
   (transition-type
    gtk-revealer-transition-type
    "transition-type" "GtkRevealerTransitionType" t t)))

;;; ----------------------------------------------------------------------------
;;; enum GtkRevealerTransitionType
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-enum "GtkRevealerTransitionType" gtk-revealer-transition-type
  (:export t
   :type-initializer "gtk_revealer_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5))

;;; ----------------------------------------------------------------------------
;;; gtk_revealer_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-revealer-new))

#+gtk-3-10
(defun gtk-revealer-new ()
  (make-instance 'gtk-revealer))

#+gtk-3-10
(export 'gtk-revealer-new)

;;; --- End of file gtk.revealer.lisp ------------------------------------------
