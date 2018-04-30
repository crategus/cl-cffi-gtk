;;; ----------------------------------------------------------------------------
;;; gtk.flow-box.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkFlowBox
;;;
;;; A container that allows reflowing its children
;;;
;;; Types and Values
;;;
;;;     GtkFlowBox
;;;     GtkFlowBoxChild
;;;
;;; Functions
;;;
;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment
;;;     gtk_flow_box_selected_foreach
;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all
;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter
;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort
;;;     gtk_flow_box_bind_model
;;;     gtk_flow_box_child_new
;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkFlowBoxChild
;;;                 ╰── GtkFlowBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkFlowBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; GtkFlowBoxChild implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFlowBox
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(define-g-object-class "GtkFlowBox" gtk-flow-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_flow_box_get_type")
  ((activate-on-single-click
    gtk-flow-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (column-spacing
    gtk-flow-box-column-spacing
    "column-spacing" "guint" t t)
   (homogeneous
    gtk-flow-box-homogeneous
    "homogeneous" "gboolean" t t)
   (max-children-per-line
    gtk-flow-box-max-children-per-line
    "max-children-per-line" "guint" t t)
   (min-children-per-line
    gtk-flow-box-min-children-per-line
    "min-children-per-line" "guint" t t)
   (row-spacing
    gtk-flow-box-row-spacing
    "row-spacing" "guint" t t)
   (selection-mode
    gtk-flow-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)))

;;; ----------------------------------------------------------------------------
;;; struct GtkFlowBoxChild
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(define-g-object-class "GtkFlowBoxChild" gtk-flow-box-child
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_flow_box_child_get_type")
  ())

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(declaim (inline gtk-flow-box-new))

#+gtk-3-12
(defun gtk-flow-box-new ()
  (make-instance 'gtk-flow-box))

#+gtk-3-12
(export 'gtk-flow-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_insert ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-insert :void
  (box (g-object gtk-flow-box))
  (widget (g-object gtk-widget))
  (position :int))

#+gtk-3-12
(export 'gtk-flow-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_index ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-get-child-at-index (g-object gtk-flow-box-child)
  (box (g-object gtk-flow-box))
  (idx :int))

#+gtk-3-12
(export 'gtk-flow-box-get-child-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_pos ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-get-child-at-pos (g-object gtk-flow-box-child)
  (box (g-object gtk-flow-box))
  (x :int)
  (y :int))

#+gtk-3-12
(export 'gtk-flow-box-get-child-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_hadjustment ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-set-hadjustment :void
  (box (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

#+gtk-3-12
(export 'gtk-flow-box-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_vadjustment ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-set-vadjustment :void
  (box (g-object gtk-flow-box))
  (adjustment (g-object gtk-adjustment)))

#+gtk-3-12
(export 'gtk-flow-box-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_selected_foreach ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_selected_children ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_child ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-select-child :void
  (box (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

#+gtk-3-12
(export 'gtk-flow-box-select-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_child ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-unselect-child :void
  (box (g-object gtk-flow-box))
  (child (g-object gtk-flow-box-child)))

#+gtk-3-12
(export 'gtk-flow-box-unselect-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_all ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-select-all :void
  (box (g-object gtk-flow-box)))

#+gtk-3-12
(export 'gtk-flow-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_all ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-unselect-all :void
  (box (g-object gtk-flow-box)))

#+gtk-3-12
(export 'gtk-flow-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_filter_func ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcallback gtk-flow-box-filter-func-callback :boolean
    ((child (g-object gtk-flow-box-child))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) child)
    (return () NIL)))

#+gtk-3-12
(defcfun ("gtk_flow_box_set_filter_func" %gtk-flow-box-set-filter-func) :void
  (box (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-12
(defun gtk-flow-box-set-filter-func (box func)
  (%gtk-flow-box-set-filter-func
   box
   (callback gtk-flow-box-filter-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-12
(export 'gtk-flow-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_filter ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-invalidate-filter :void
  (box (g-object gtk-flow-box)))

#+gtk-3-12
(export 'gtk-flow-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_sort_func ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcallback gtk-flow-box-sort-func-callback :int
    ((child1 (g-object gtk-flow-box-child))
     (child2 (g-object gtk-flow-box-child))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) child1 child2)
    (return () -1)))

#+gtk-3-12
(defcfun ("gtk_flow_box_set_sort_func" %gtk-flow-box-set-sort-func) :void
  (box (g-object gtk-flow-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-12
(defun gtk-flow-box-set-sort-func (box func)
  (%gtk-flow-box-set-sort-func
   box
   (callback gtk-flow-box-sort-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-12
(export 'gtk-flow-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_sort ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-invalidate-sort :void
  (box (g-object gtk-flow-box)))

#+gtk-3-12
(export 'gtk-flow-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_bind_model ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcallback gtk-flow-box-create-widget-func-callback (g-object gtk-widget)
    ((item g-object)
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) item)
    (return () NIL)))

#+gtk-3-12
(defcfun ("gtk_flow_box_bind_model" %gtk-flow-box-bind-model) :void
  (box (g-object gtk-flow-box))
  (model (g-object g-list-model))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-12
(defun gtk-flow-box-bind-model (box model func)
  (%gtk-flow-box-bind-model
   box
   model
   (callback gtk-flow-box-create-widget-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-12
(export 'gtk-flow-box-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(declaim (inline gtk-flow-box-child-new))

#+gtk-3-12
(defun gtk-flow-box-child-new ()
  (make-instance 'gtk-flow-box-child))

#+gtk-3-12
(export 'gtk-flow-box-child-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_get_index ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-child-get-index :int
  (child (g-object gtk-flow-box-child)))

#+gtk-3-12
(export 'gtk-flow-box-child-get-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_is_selected ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-child-is-selected :boolean
  (child (g-object gtk-flow-box-child)))

#+gtk-3-12
(export 'gtk-flow-box-child-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_changed ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun gtk-flow-box-child-changed :void
  (child (g-object gtk-flow-box-child)))

#+gtk-3-12
(export 'gtk-flow-box-child-changed)

;;; --- End of file gtk-flow-box.lisp ------------------------------------------
