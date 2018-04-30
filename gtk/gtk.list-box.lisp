;;; ----------------------------------------------------------------------------
;;; gtk.list-box.lisp
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
;;; GtkListBox
;;;
;;; A list container
;;;
;;; Types and Values
;;;
;;;     GtkListBox
;;;     GtkListBoxRow
;;;
;;; Functions
;;;
;;;     gtk_list_box_new
;;;     gtk_list_box_prepend
;;;     gtk_list_box_insert
;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all
;;;     gtk_list_box_get_selected_row
;;;     gtk_list_box_selected_foreach
;;;     gtk_list_box_get_selected_rows
;;;     gtk_list_box_set_selection_mode                    -> Accessor
;;;     gtk_list_box_get_selection_mode                    -> Accessor
;;;     gtk_list_box_set_activate_on_single_click          -> Accessor
;;;     gtk_list_box_get_activate_on_single_click          -> Accessor
;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment
;;;     gtk_list_box_set_placeholder
;;;     gtk_list_box_get_row_at_index
;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort
;;;     gtk_list_box_set_filter_func
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row
;;;     gtk_list_box_bind_model
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index
;;;     gtk_list_box_row_set_activatable                   -> Accessor
;;;     gtk_list_box_row_get_activatable                   -> Accessor
;;;     gtk_list_box_row_set_selectable                    -> Accessor
;;;     gtk_list_box_row_get_selectable                    -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkListBoxRow
;;;                 ╰── GtkListBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkListBox implements AtkImplementorIface and GtkBuildable.
;;;
;;; GtkListBoxRow implements AtkImplementorIface, GtkBuildable and
;;; GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkListBox
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkListBox" gtk-list-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_list_box_get_type")
  ((activate-on-single-click
    gtk-list-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (selection-mode
    gtk-list-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)))

;;; ----------------------------------------------------------------------------
;;; struct GtkListBoxRow
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(define-g-object-class "GtkListBoxRow" gtk-list-box-row
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                ;; "GtkActionable"
                )
   :type-initializer "gtk_list_box_row_get_type")
  ((activatable
    gtk-list-box-row-activatable
    "activatable" "gboolean" t t)
   (selectable
    gtk-list-box-row-selectable
    "selectable" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-list-box-new))

#+gtk-3-10
(defun gtk-list-box-new ()
  (make-instance 'gtk-list-box))

#+gtk-3-10
(export 'gtk-list-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_prepend ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-prepend :void
  (box (g-object gtk-list-box))
  (child (g-object gtk-widget)))

#+gtk-3-10
(export 'gtk-list-box-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_insert ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-insert :void
  (box (g-object gtk-list-box))
  (child (g-object gtk-widget))
  (position :int))

#+gtk-3-10
(export 'gtk-list-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_row ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-select-row :void
  (box (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

#+gtk-3-10
(export 'gtk-list-box-select-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_row ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-unselect-row :void
  (box (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

#+gtk-3-10
(export 'gtk-list-box-unselect-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_all ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-select-all :void
  (box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_all ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-unselect-all :void
  (box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_row ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-get-selected-row (g-object gtk-list-box-row)
  (box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-get-selected-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_selected_foreach ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcallback gtk-list-box-foreach-func-callback :boolean
    ((box (g-object gtk-list-box))
     (row (g-object gtk-list-box-row))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) box row)
    (return () NIL)))

#+gtk-3-14
(defcfun ("gtk_list_box_selected_foreach" %gtk-list-box-selected-foreach) :void
  (box (g-object gtk-list-box))
  (func :pointer)
  (data :pointer))

#+gtk-3-14
(defun gtk-list-box-selected-foreach (box func)
  (with-stable-pointer (ptr func)
    (%gtk-list-box-selected-foreach box (callback gtk-list-box-selected-foreach-callback) ptr)))

#+gtk-3-14
(export 'gtk-list-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_rows ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun gtk-list-box-get-selected-rows (g-list (g-object gtk-list-box-row))
  (box (g-object gtk-list-box)))

#+gtk-3-14
(export 'gtk-list-box-get-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_adjustment ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_list_box_get_adjustment" gtk-list-box-adjustment)
    (g-object gtk-adjustment)
  (box (g-object gtk-list-box)))

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_adjustment ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_list_box_set_adjustment" %gtk-list-box-set-adjustment)
    :void
  (list-box (g-object gtk-list-box))
  (adjustment (g-object gtk-adjustment)))

#+gtk-3-10
(defun (setf gtk-list-box-adjustment) (new-value list-box)
  (%gtk-list-box-set-adjustment list-box new-value))

#+gtk-3-10
(export 'gtk-list-box-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_placeholder ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-set-placeholder :void
  (list-box (g-object gtk-list-box))
  (placeholder (g-object gtk-widget)))

#+gtk-3-10
(export 'gtk-list-box-set-placeholder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_index ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-get-row-at-index (g-object gtk-list-box-row)
  (list-box (g-object gtk-list-box))
  (index :int))

#+gtk-3-10
(export 'gtk-list-box-get-row-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_y ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-get-row-at-y (g-object gtk-list-box-row)
  (list-box (g-object gtk-list-box))
  (y :int))

#+gtk-3-10
(export 'gtk-list-box-get-row-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_filter ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-invalidate-filter :void
  (list-box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_headers ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-invalidate-headers :void
  (list-box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-invalidate-headers)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_sort ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-invalidate-sort :void
  (list-box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_filter_func ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcallback gtk-list-box-filter-func-callback :boolean
    ((row (g-object gtk-list-box-row))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) row)
    (return () NIL)))

#+gtk-3-10
(defcfun ("gtk_list_box_set_filter_func" %gtk-list-box-set-filter-func) :void
  (box (g-object gtk-list-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-10
(defun gtk-list-box-set-filter-func (box func)
  (%gtk-list-box-set-filter-func
   box
   (callback gtk-list-box-filter-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-10
(export 'gtk-list-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_header_func ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcallback gtk-list-box-update-header-func-callback :void
    ((row (g-object gtk-list-box-row))
     (before (g-object gtk-list-box-row))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) row before)
    (return () NIL)))

#+gtk-3-10
(defcfun ("gtk_list_box_set_header_func" %gtk-list-box-set-header-func) :void
  (box (g-object gtk-list-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-10
(defun gtk-list-box-set-header-func (box func)
  (%gtk-list-box-set-header-func
   box
   (callback gtk-list-box-update-header-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-10
(export 'gtk-list-box-set-header-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_sort_func ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcallback gtk-list-box-sort-func-callback :int
    ((row1 (g-object gtk-list-box-row))
     (row2 (g-object gtk-list-box-row))
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) row1 row2)
    (return () -1)))

#+gtk-3-10
(defcfun ("gtk_list_box_set_sort_func" %gtk-list-box-set-sort-func) :void
  (box (g-object gtk-list-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-10
(defun gtk-list-box-set-sort-func (box func)
  (%gtk-list-box-set-sort-func
   box
   (callback gtk-list-box-sort-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-10
(export 'gtk-list-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_highlight_row ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-drag-highlight-row :void
  (list-box (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

#+gtk-3-10
(export 'gtk-list-box-drag-highlight-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_unhighlight_row ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-drag-unhighlight-row :void
  (list-box (g-object gtk-list-box)))

#+gtk-3-10
(export 'gtk-list-box-drag-unhighlight-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_bind_model ()
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(defcallback gtk-list-box-create-widget-func-callback (g-object gtk-widget)
    ((item g-object)
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) item)
    (return () NIL)))

#+gtk-3-16
(defcfun ("gtk_list_box_bind_model" %gtk-list-box-bind-model) :void
  (box (g-object gtk-list-box))
  (model (g-object g-list-model))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

#+gtk-3-16
(defun gtk-list-box-bind-model (box model func)
  (%gtk-list-box-bind-model
   box
   model
   (callback gtk-list-box-create-widget-func-callback)
   (glib::allocate-stable-pointer func)
   (callback glib::stable-pointer-destroy-notify-cb)))

#+gtk-3-16
(export 'gtk-list-box-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-list-box-row-new))

#+gtk-3-10
(defun gtk-list-box-row-new ()
  (make-instance 'gtk-list-box-row))

#+gtk-3-10
(export 'gtk-list-box-row-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_changed ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-row-changed :void
  (row (g-object gtk-list-box-row)))

#+gtk-3-10
(export 'gtk-list-box-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_is_selected ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun gtk-list-box-row-is-selected :boolean
  (row (g-object gtk-list-box-row)))

#+gtk-3-14
(export 'gtk-list-box-row-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_header ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_list_box_row_get_header" gtk-list-box-row-header)
    (g-object gtk-widget)
  (row (g-object gtk-list-box-row)))

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_set_header ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun ("gtk_list_box_row_set_header" %gtk-list-box-row-set-header)
    :void
  (row (g-object gtk-list-box-row))
  (header (g-object gtk-widget)))

#+gtk-3-10
(defun (setf gtk-list-box-row-header) (new-value row)
  (%gtk-list-box-row-set-header row new-value))

#+gtk-3-10
(export 'gtk-list-box-row-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_index ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(defcfun gtk-list-box-row-get-index :int
  (list-box-row (g-object gtk-list-box-row)))

#+gtk-3-10
(export 'gtk-list-box-row-get-index)

;;; --- End of file gtk.list-box.lisp ------------------------------------------
