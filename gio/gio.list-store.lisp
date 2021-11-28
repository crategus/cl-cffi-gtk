;;; ----------------------------------------------------------------------------
;;; gio.list-store.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2021 Dieter Kaiser
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
;;; GListStore
;;;
;;;     A simple implementation of GListModel
;;;
;;; Types and Values
;;;
;;;     GListStore
;;;
;;; Functions
;;;
;;;     g_list_store_new
;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort
;;;     g_list_store_find
;;;     g_list_store_find_with_equal_func
;;;
;;; Properties
;;;
;;;    GType*   item-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GListStore
;;;
;;; Implemented Interfaces
;;;
;;;     GListStore implements GListModel.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GListStore
;;; ----------------------------------------------------------------------------

(define-g-object-class "GListStore" g-list-store
  (:superclass g-object
   :export t
   :interfaces ("GListModel")
   :type-initializer "g_list_store_get_type")
  ((item-type
    g-list-store-item-type
    "item-type" "GType" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-list-store 'type)
 "@version{2021-11-15}
  @begin{short}
    The @sym{g-list-store} object is an implementation of the
    @class{g-list-model} interface that stores all items in memory.
  @end{short}
  It provides insertions, deletions, and lookups in logarithmic time with a fast
  path for the common case of iterating the list linearly.
  @see-slot{g-list-store-item-type}
  @see-class{g-list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-list-store-item-type -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "item-type" 'g-list-store) 't)
 "The @code{item-type} property of type @class{g-type}
  (Read / Write / Construct Only) @br{}
  The type of items contained in the list store. Items must be subclasses of
  the @class{g-object} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-enabled 'function)
 "@version{2021-11-15}
 ")

;;; ----------------------------------------------------------------------------
;;; g_list_store_new ()
;;;
;;; GListStore *
;;; g_list_store_new (GType item_type);
;;;
;;; Creates a new GListStore with items of type item_type . item_type must be a
;;; subclass of GObject.
;;;
;;; item_type:
;;;     the GType of items in the list
;;;
;;; Returns:
;;;     a new GListStore
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defun g-list-store-new (itype)
  (make-instance 'g-list-store
                 :item-type itype))

(export 'g-list-store-new)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert ()
;;;
;;; void
;;; g_list_store_insert (GListStore *store,
;;;                      guint position,
;;;                      gpointer item);
;;;
;;; Inserts item into store at position . item must be of type “item-type” or
;;; derived from it. position must be smaller than the length of the list, or
;;; equal to it to append.
;;;
;;; This function takes a ref on item .
;;;
;;; Use g_list_store_splice() to insert multiple items at the same time
;;; efficiently.
;;;
;;; store:
;;;     a GListStore
;;;
;;; position:
;;;     the position at which to insert the new item
;;;
;;; item:
;;;     the new item.
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_insert" g-list-store-insert) :void
  (store (g-object g-list-store))
  (position :uint)
  (item :pointer))

(export 'g-list-store-insert)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert_sorted ()
;;;
;;; guint
;;; g_list_store_insert_sorted (GListStore *store,
;;;                             gpointer item,
;;;                             GCompareDataFunc compare_func,
;;;                             gpointer user_data);
;;;
;;; Inserts item into store at a position to be determined by the compare_func .
;;;
;;; The list must already be sorted before calling this function or the result
;;; is undefined. Usually you would approach this by only ever inserting items
;;; by way of this function.
;;;
;;; This function takes a ref on item .
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     the new item.
;;;
;;; compare_func:
;;;     pairwise comparison function for sorting.
;;;
;;; user_data
;;;     user data for compare_func .
;;;
;;; Returns
;;;     the position at which item was inserted
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_append ()
;;;
;;; void
;;; g_list_store_append (GListStore *store,
;;;                      gpointer item);
;;;
;;; Appends item to store . item must be of type “item-type”.
;;;
;;; This function takes a ref on item .
;;;
;;; Use g_list_store_splice() to append multiple items at the same time
;;; efficiently.
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     the new item.
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove ()
;;;
;;; void
;;; g_list_store_remove (GListStore *store,
;;;                      guint position);
;;;
;;; Removes the item from store that is at position . position must be smaller
;;; than the current length of the list.
;;;
;;; Use g_list_store_splice() to remove multiple items at the same time
;;; efficiently.
;;;
;;; store:
;;;     a GListStore
;;;
;;; position:
;;;     the position of the item that is to be removed
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove_all ()
;;;
;;; void
;;; g_list_store_remove_all (GListStore *store);
;;;
;;; Removes all items from store .
;;;
;;; store:
;;;     a GListStore
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_splice ()
;;;
;;; void
;;; g_list_store_splice (GListStore *store,
;;;                      guint position,
;;;                      guint n_removals,
;;;                      gpointer *additions,
;;;                      guint n_additions);
;;;
;;; Changes store by removing n_removals items and adding n_additions items to
;;; it. additions must contain n_additions items of type “item-type”. NULL is
;;; not permitted.
;;;
;;; This function is more efficient than g_list_store_insert() and
;;; g_list_store_remove(), because it only emits “items-changed” once for the
;;; change.
;;;
;;; This function takes a ref on each item in additions .
;;;
;;; The parameters position and n_removals must be correct (ie: position +
;;; n_removals must be less than or equal to the length of the list at the time
;;; this function is called).
;;;
;;; store:
;;;     a GListStore
;;;
;;; position:
;;;     the position at which to make the change
;;;
;;; n_removals:
;;;     the number of items to remove
;;;
;;; additions:
;;;     the items to add.
;;;
;;; n_additions:
;;;     the number of items to add
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_sort ()
;;;
;;; void
;;; g_list_store_sort (GListStore *store,
;;;                    GCompareDataFunc compare_func,
;;;                    gpointer user_data);
;;;
;;; Sort the items in store according to compare_func .
;;;
;;; store:
;;;     a GListStore
;;;
;;; compare_func:
;;;     pairwise comparison function for sorting.
;;;
;;; user_data:
;;;     user data for compare_func .
;;;
;;; Since: 2.46
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_find ()
;;;
;;; gboolean
;;; g_list_store_find (GListStore *store,
;;;                    gpointer item,
;;;                    guint *position);
;;;
;;; Looks up the given item in the list store by looping over the items until
;;; the first occurrence of item . If item was not found, then position will not
;;; be set, and this method will return FALSE.
;;;
;;; If you need to compare the two items with a custom comparison function, use
;;; g_list_store_find_with_equal_func() with a custom GEqualFunc instead.
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     an item.
;;;
;;; position:
;;;     the first position of item , if it was found.
;;;
;;; Returns:
;;;     Whether store contains item . If it was found, position will be set to
;;;     the position where item occurred for the first time.
;;;
;;; Since: 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_store_find_with_equal_func ()
;;;
;;; gboolean
;;; g_list_store_find_with_equal_func (GListStore *store,
;;;                                    gpointer item,
;;;                                    GEqualFunc equal_func,
;;;                                    guint *position);
;;;
;;; Looks up the given item in the list store by looping over the items and
;;; comparing them with compare_func until the first occurrence of item which
;;; matches. If item was not found, then position will not be set, and this
;;; method will return FALSE.
;;;
;;; store:
;;;     a GListStore
;;;
;;; item:
;;;     an item.
;;;
;;; equal_func:
;;;     A custom equality check function.
;;;
;;; position:
;;;     the first position of item , if it was found.
;;;
;;; Returns:
;;;     Whether store contains item . If it was found, position will be set to
;;;     the position where item occurred for the first time.
;;;
;;; Since: 2.64
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.list-store.lisp ----------------------------------------
