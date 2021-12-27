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
 "@version{2021-12-10}
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
(setf (gethash 'g-list-store-item-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-list-store-item-type 'function)
 "@version{2021-12-10}
  @syntax[]{(g-list-store-item-type object) => pointer}
  @argument[object]{a @class{g-list-store} object}
  @argument[pointer]{a pointer to a @class{g-type} type}
  @begin{short}
    Accessor of the @slot[item-type]{g-list-store} slot of the
    @class{g-list-store} class.
  @end{short}

  The type of items contained in the list store. Items must be subclasses of
  the @class{g-object} class.
  @begin[Note]{dictionary}
    This function returns a pointer to the @class{g-type} type. Use the
    @fun{g-list-model-item-type} function to get the @class{g-type} type.
  @end{dictionary}
  @see-class{g-list-store}
  @see-class{g-type}
  @see-function{g-list-model-item-type}")

;;; ----------------------------------------------------------------------------
;;; g_list_store_new ()
;;; ----------------------------------------------------------------------------

;; Use the C implementation and not MAKE-INSTANCE because we have to pass
;; a pointer of a GType for the ITEM-TYPE property.

(defcfun ("g_list_store_new" g-list-store-new) (g-object g-list-store)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[itype]{a @class{g-type} type for the items in the list}
  @return{A new @class{g-list-store} object.}
  @begin{short}
    Creates a new list store with items of @arg{itype} type
  @end{short}
  The @arg{itype} type must be a subclass of the @class{g-object} class.
  @see-class{g-list-store}
  @see-class{g-type}
  @see-class{g-object}"
  (itype g-type))

(export 'g-list-store-new)

;;; ----------------------------------------------------------------------------
;;; g_list_store_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_insert" g-list-store-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[store]{a @class{g-list-store} object}
  @argument[position]{an unsigned integer with the position at which to insert
    the new item}
  @argument[item]{a @class{g-object} object with the new item}
  @begin{short}
    Inserts the item into the list store at @arg{position}.
  @end{short}
  The item must be of type @slot[g-list-store]{item-type} type or derived from
  it. The @arg{position} argument must be smaller than the length of the list
  store, or equal to it to append.

  This function takes a ref on @arg{item}.

  Use the @fun{g-list-store-splice} function to insert multiple items at the
  same time efficiently.
  @see-class{g-list-store}
  @see-class{g-object}
  @see-function{g-list-model-item-type}
  @see-function{g-list-store-splice}"
  (store (g-object g-list-store))
  (position :uint)
  (item g-object))

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_append" g-list-store-append) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-store} object}
  @argument[item]{a @class{g-object} object with the new item}
  @begin{short}
    Appends the item to the list store.
  @end{short}
  The item must be of type @slot[g-list-store]{item-type} type.

  This function takes a ref on @arg{item}.

  Use the @fun{g-list-store-splice} function to append multiple items at the
  same time efficiently.
  @see-class{gtk-list-store}
  @see-class{g-object}
  @see-function{g-list-store-splice}"
  (store (g-object g-list-store))
  (item g-object))

(export 'g-list-store-append)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_remove" g-list-store-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-store} object}
  @argument[position]{an unsigned integer with the position of the item that
    is to be removed}
  @begin{short}
    Removes the item from the list store that is at @arg{position}.
  @end{short}
  The @arg{position} argument must be smaller than the current length of the
  list store.

  Use the @fun{g-list-store-splice} function to remove multiple items at the
  same time efficiently.
  @see-class{g-list-store}
  @see-function{g-list-store-splice}"
  (list (g-object g-list-store))
  (position :uint))

(export 'g-list-store-remove)

;;; ----------------------------------------------------------------------------
;;; g_list_store_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_remove_all" g-list-store-remove-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-store} object}
  @short{Removes all items from the list store.}
  @see-class{g-list-store}"
  (list (g-object g-list-store)))

(export 'g-list-store-remove-all)

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
;;; it. additions must contain n_additions items of type "item-type". NULL is
;;; not permitted.
;;;
;;; This function is more efficient than g_list_store_insert() and
;;; g_list_store_remove(), because it only emits "items-changed" once for the
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_store_find" %g-list-store-find) :boolean
  (list (g-object g-list-store))
  (item g-object)
  (position (:pointer :uint)))

(defun g-list-store-find (list item)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-store} object}
  @argument[item]{a @class{g-object} item}
  @return{An unsigned integer with the first position of the item, if it was
    found, otherwise @code{nil}.}
  @begin{short}
    Looks up the given item in the list store by looping over the items until
    the first occurrence of @arg{item}.
  @end{short}
  If the @arg{item} argument was not found, then this method will return
  @code{nil}.

  If you need to compare the two items with a custom comparison function, use
  the @fun{g-list-store-find-with-equal-func} function with a custom
  @code{GEqualFunc} instead.
  @see-class{g-list-store}"
  (with-foreign-object (position :uint)
    (when (%g-list-store-find list item position)
      (mem-ref position :uint))))

(export 'g-list-store-find)

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
