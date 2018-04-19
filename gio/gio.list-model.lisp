;;; ----------------------------------------------------------------------------
;;; gio.list-model.lisp
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
;;; Synopsis
;;;
;;;     GListModel
;;;
;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item
;;;     g_list_model_get_object
;;;     g_list_model_items_changed
;;;
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GListModel
;;; ----------------------------------------------------------------------------

(define-g-interface "GListModel" g-list-model
  (:export t
   :type-initializer "g_list_model_get_type"))

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item_type ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-model-get-item-type g-type
  (model (g-object g-list-model)))

(export 'g-list-model-get-item-type)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_n_items ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-model-get-n-items :uint
  (model (g-object g-list-model)))

(export 'g-list-model-get-n-items)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-model-get-item g-object
  (model (g-object g-list-model))
  (position :uint))

(export 'g-list-model-get-item)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_object ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-list-model-get-object))

(defun g-list-model-get-object (model position)
  (g-list-model-get-item model position))

(export 'g-list-model-get-object)

;;; ----------------------------------------------------------------------------
;;; g_list_model_items_changed ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-model-items-changed :void
  (model (g-object g-list-model))
  (position :uint)
  (removed :uint)
  (added :uint))

(export 'g-list-model-items-changed)
