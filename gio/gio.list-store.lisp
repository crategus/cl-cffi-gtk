;;; ----------------------------------------------------------------------------
;;; gio.list-store.lisp
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
;;;     GListStore
;;;
;;;     g_list_store_new
;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort
;;;
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; struct GListStore
;;; ----------------------------------------------------------------------------

(define-g-object-class "GListStore" g-list-store
  (:superclass g-object
   :export t
   :interfaces ("GListModel")
   :type-initializer "g_list_store_get_type")
  ((item-type
    g-list-store-item-type
    "item-type" "GType" t t)))

;;; ----------------------------------------------------------------------------
;;; g_list_store_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-list-store-new))

(defun g-list-store-new (item-type)
  (make-instance 'g-list-store :item-type item-type))

(export 'g-list-store-new)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-store-insert :void
  (store (g-object g-list-store))
  (position :uint)
  (item g-object))

(export 'g-list-store-insert)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert_sorted ()
;;; ----------------------------------------------------------------------------

(defcallback g-compare-data-func-callback :int
    ((a g-object)
     (b g-object)
     (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data) a b)
    (return () -1)))

(defcfun ("g_list_store_insert_sorted" %g-list-store-insert-sorted) :uint
  (store (g-object g-list-store))
  (item g-object)
  (func :pointer)
  (data :pointer))

(defun g-list-store-insert-sorted (store item func)
  (with-stable-pointer (ptr func)
    (%g-list-store-insert-sorted store item (callback g-compare-data-func-callback) ptr)))

(export 'g-list-store-insert-sorted)

;;; ----------------------------------------------------------------------------
;;; g_list_store_append ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-store-append :void
  (store (g-object g-list-store))
  (item g-object))

(export 'g-list-store-append)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-store-remove :void
  (store (g-object g-list-store))
  (position :uint))

(export 'g-list-store-remove)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun g-list-store-remove-all :void
  (store (g-object g-list-store)))

(export 'g-list-store-remove-all)

;;; ----------------------------------------------------------------------------
;;; g_list_store_splice ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_splice" %g-list-store-splice) :void
  (store (g-object g-list-store))
  (position :uint)
  (n-removals :uint)
  (additions :pointer)
  (n-additions :uint))

(defun g-list-store-splice (store position n-removals additions n-additions)
  (with-foreign-objects ((items :pointer n-additions))
    (iterate
      (for i from 0 below n-additions)
      (for item in-sequence additions)
      (setf (mem-aref items :pointer i) (pointer item)))
    (%g-list-store-splice store position n-removals items n-additions)))

(export 'g-list-store-splice)

;;; ----------------------------------------------------------------------------
;;; g_list_store_sort ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_sort" %g-list-store-sort) :void
  (store (g-object g-list-store))
  (func :pointer)
  (data :pointer))

(defun g-list-store-sort (store func)
  (with-stable-pointer (ptr func)
    (%g-list-store-sort store (callback g-compare-data-func-callback) ptr)))

(export 'g-list-store-sort)
