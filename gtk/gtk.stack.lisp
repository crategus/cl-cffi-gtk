;;; ----------------------------------------------------------------------------
;;; gtk.stack.lisp
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
;;; GtkStack
;;;
;;; A stacking container
;;;
;;; Types and Values
;;;
;;;     GtkStack
;;;     GtkStackSwitcher
;;;     GtkStackSidebar
;;;     GtkStackTransitionType
;;;
;;; Functions
;;;
;;;     gtk_stack_new
;;;     gtk_stack_add_named                                * not implemented *
;;;     gtk_stack_add_titled                               * not implemented *
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_set_visible_child                        -> Accessor
;;;     gtk_stack_get_visible_child                        -> Accessor
;;;     gtk_stack_set_visible_child_name                   -> Accessor
;;;     gtk_stack_get_visible_child_name                   -> Accessor
;;;     gtk_stack_set_visible_child_full                   * not implemented *
;;;     gtk_stack_set_homogeneous                          -> Accessor
;;;     gtk_stack_get_homogeneous                          -> Accessor
;;;     gtk_stack_set_hhomogeneous                         -> Accessor
;;;     gtk_stack_get_hhomogeneous                         -> Accessor
;;;     gtk_stack_set_vhomogeneous                         -> Accessor
;;;     gtk_stack_get_vhomogeneous                         -> Accessor
;;;     gtk_stack_set_transition_duration                  -> Accessor
;;;     gtk_stack_get_transition_duration                  -> Accessor
;;;     gtk_stack_set_transition_type                      -> Accessor
;;;     gtk_stack_get_transition_type                      -> Accessor
;;;     gtk_stack_get_transition_running                   -> Accessor
;;;     gtk_stack_get_interpolate_size                     -> Accessor
;;;     gtk_stack_set_interpolate_size                     -> Accessor
;;;     gtk_stack_switcher_new
;;;     gtk_stack_switcher_set_stack                       -> Accessor
;;;     gtk_stack_switcher_get_stack                       -> Accessor
;;;     gtk_stack_sidebar_new
;;;     gtk_stack_sidebar_set_stack                        -> Accessor
;;;     gtk_stack_sidebar_get_stack                        -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkStackSidebar
;;;                 ├── GtkBox
;;;                 │   ╰── GtkStackSwitcher
;;;                 ╰── GtkStack
;;;
;;; Implemented Interfaces
;;;
;;; GtkStack implements AtkImplementorIface and GtkBuildable.
;;;
;;; GtkStackSidebar implements AtkImplementorIface and GtkBuildable.
;;;
;;; GtkStackSwitcher implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStack
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkStack" gtk-stack
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_stack_get_type")
  (#+gtk-3-16
   (hhomogeneous
    gtk-stack-hhomogeneous
    "hhomogeneous" "gboolean" t t)
   (homogeneous
    gtk-stack-homogeneous
    "homogeneous" "gboolean" t t)
   (interpolate-size
    gtk-stack-interpolate-size
    "interpolate-size" "gboolean" t t)
   (transition-duration
    gtk-stack-transition-duration
    "transition-duration" "guint" t nil)
   (transition-running
    gtk-stack-transition-running
    "transition-running" "gboolean" t t)
   (transition-type
    gtk-stack-transition-type
    "transition-type" "GtkStackTransitionType" t t)
   #+gtk-3-16
   (vhomogeneous
    gtk-stack-vhomogeneous
    "vhomogeneous" "gboolean" t t)
   (visible-child
    gtk-stack-visible-child
    "visible-child" "GtkWidget" t t)
   (visible-child-name
    gtk-stack-visible-child-name
    "visible-child-name" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-child-property "GtkStack"
                       gtk-stack-child-icon-name
                       "icon-name" "gchararray" t t t)

#+gtk-3-10
(define-child-property "GtkStack"
                       gtk-stack-child-name
                       "name" "gchararray" t t t)

#+gtk-3-12
(define-child-property "GtkStack"
                       gtk-stack-child-needs-attention
                       "needs-attention" "gint" t t t)

#+gtk-3-10
(define-child-property "GtkStack"
                       gtk-stack-child-position
                       "position" "gint" t t t)

#+gtk-3-10
(define-child-property "GtkStack"
                       gtk-stack-child-title
                       "title" "gchararray" t t t)

;;; ----------------------------------------------------------------------------
;;; enum GtkStackTransitionType
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-enum "GtkStackTransitionType" gtk-stack-transition-type
  (:export t
   :type-initializer "gtk_stack_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5)
  (:slide-left-right 6)
  (:slide-up-down 7)
  (:over-up 8)
  (:over-down 9)
  (:over-left 10)
  (:over-right 11)
  (:under-up 12)
  (:under-down 13)
  (:under-left 14)
  (:under-right 15)
  (:over-up-down 16)
  (:over-down-up 17)
  (:over-left-right 18)
  (:over-right-left 19))

;;; ----------------------------------------------------------------------------
;;; gtk_stack_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-stack-new))

#+gtk-3-10
(defun gtk-stack-new ()
  (make-instance 'gtk-stack))

#+gtk-3-10
(export 'gtk-stack-new)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_add_titled ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_stack_add_titled" %gtk-stack-add-titled) :void
  (stack (g-object gtk-stack))
  (child (g-object gtk-widget))
  (name :string)
  (title :string))

#+gtk-3-10
(defun gtk-stack-add-titled (stack child name &optional title)
  (%gtk-stack-add-titled stack child name title))

#+gtk-3-10
(export 'gtk-stack-add-titled)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_get_child_by_name ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(declaim (inline gtk-stack-get-child-by-name))

#+gtk-3-12
(defcfun gtk-stack-get-child-by-name (g-object gtk-widget)
  (stack (g-object gtk-stack))
  (name :string))

#+gtk-3-12
(export 'gtk-stack-get-child-by-name)

;;; ----------------------------------------------------------------------------
;;; struct GtkStackSwitcher
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkStackSwitcher" gtk-stack-switcher
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_stack_switcher_get_type")
  (#+gtk-3-20
   (icon-size
    gtk-stack-switcher-icon-size
    "icon-size" "gint" t t)
   (stack
    gtk-stack-switcher-stack
    "stack" "GtkStack" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_stack_switcher_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-stack-switch-new))

#+gtk-3-10
(defun gtk-stack-switcher-new ()
  (make-instance 'gtk-stack-switcher))

#+gtk-3-10
(export 'gtk-stack-switcher-new)

;;; ----------------------------------------------------------------------------
;;; struct GtkStackSidebar
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(define-g-object-class "GtkStackSidebar" gtk-stack-sidebar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_stack_sidebar_get_type")
  ((stack
    gtk-stack-sidebar-stack
    "stack" "GtkStack" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_stack_sidebar_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(declaim (inline gtk-stack-sidebar-new))

#+gtk-3-16
(defun gtk-stack-sidebar-new ()
  (make-instance 'gtk-stack-sidebar))

#+gtk-3-16
(export 'gtk-stack-sidebar-new)

;;; --- End of file gtk.stack.lisp ---------------------------------------------
