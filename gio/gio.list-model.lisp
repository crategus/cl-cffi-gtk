;;; ----------------------------------------------------------------------------
;;; gio.list-model.lisp
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
;;; GListModel
;;;
;;;     An interface describing a dynamic list of objects
;;;
;;; Types and Values
;;;
;;;     GListModel
;;;     GListModelInterface
;;;
;;; Functions
;;;
;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item
;;;     g_list_model_get_object
;;;     g_list_model_items_changed
;;;
;;; Signals
;;;
;;;     void    items-changed
;;;
;;; Object Hierarchy
;;;
;;;    GInterface
;;;     ╰── GListModel
;;;
;;; Prerequisites
;;;
;;;     GListModel requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GListModel is implemented by GListStore.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GListModel
;;; ----------------------------------------------------------------------------

(define-g-interface "GListModel" g-list-model
  (:export t
   :type-initializer "g_list_model_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-list-model atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-list-model 'type)
 "@version{2021-11-15}
  @begin{short}
    The @sym{g-list-model} interface is an interface that represents a mutable
    list of @class{g-object} instances.
  @end{short}
  Its main intention is as a model for various widgets in user interfaces, such
  as list views, but it can also be used as a convenient method of returning
  lists of data, with support for updates.

  Each object in the list may also report changes in itself via some mechanism,
  normally the \"notify\" signal. Taken together with the \"items-changed\"
  signal, this provides for a list that can change its membership, and in which
  the members can change their individual properties.

  A good example would be the list of visible wireless network access points,
  where each access point can report dynamic properties such as signal strength.

  It is important to note that the @sym{g-list-model} implementation itself does
  not report changes to the individual items. It only reports changes to the
  list membership. If you want to observe changes to the objects themselves then
  you need to connect signals to the objects that you are interested in.

  All items in a @sym{g-list-model} instance are of, or derived from, the same
  type. The @fun{g-list-model-item-type} function returns that type. The type
  may be an interface, in which case all objects in the list must implement it.

  The semantics are close to that of an array: the @fun{g-list-model-n-items}
  function returns the number of items in the list and the
  @fun{g-list-model-item} function returns an item at a (0-based) position. In
  order to allow implementations to calculate the list length lazily, you can
  also iterate over items: starting from 0, repeatedly call the
  @fun{g-list-model-item} function until it returns @code{nil}.

  An implementation may create objects lazily, but must take care to return the
  same object for a given position until all references to it are gone.

  On the other side, a consumer is expected only to hold references on objects
  that are currently \"user visible\", in order to facilitate the maximum level
  of laziness in the implementation of the list and to reduce the required
  number of signal connections at a given time.

  This interface is intended only to be used from a single thread. The thread in
  which it is appropriate to use it depends on the particular implementation,
  but typically it will be from the thread that owns the thread-default main
  context in effect at the time that the model was created.
  @begin[Signal Details]{dictionary}
    @subheading{The \"items-changed\" signal}
      @begin{pre}
 lambda (list position removed added)    :run-last
      @end{pre}
      This signal is emitted whenever items were added to or removed from
      @arg{list}. At @arg{position}, removed items were removed and added items
      were added in their place. Note: If @arg{removed} is not equal
      @arg{added}, the positions of all later items in the model change.
      @begin[code]{table}
        @entry[list]{The @sym{g-list-model} instance that changed.}
        @entry[position]{An unsigned integer with the position at which
          @arg{list} changed.}
        @entry[removed]{An unsigned integer with the number of items removed.}
        @entry[added]{An unsigned integer with the number of items addes.}
      @end{table}
  @end{dictionary}
  @see-class{g-list-store}")

;;; ----------------------------------------------------------------------------
;;; struct GListModelInterface
;;;
;;; struct GListModelInterface {
;;;   GTypeInterface g_iface;
;;;
;;;   GType     (* get_item_type)   (GListModel *list);
;;;   guint     (* get_n_items)     (GListModel *list);
;;;   gpointer  (* get_item)        (GListModel *list,
;;;                                  guint       position);
;;; };
;;;
;;; The virtual function table for GListModel.
;;;
;;; Members
;;;
;;; get_item_type ()
;;;     the virtual function pointer for g_list_model_get_item_type()
;;;
;;; get_n_items ()
;;;     the virtual function pointer for g_list_model_get_n_items()
;;;
;;; get_item ()
;;;     the virtual function pointer for g_list_model_get_item()
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item_type ()
;;;
;;; GType
;;; g_list_model_get_item_type (GListModel *list);
;;;
;;; Gets the type of the items in list . All items returned from
;;; g_list_model_get_type() are of that type or a subtype, or are an
;;; implementation of that interface.
;;;
;;; The item type of a GListModel can not change during the life of the model.
;;;
;;; list:
;;;     a GListModel
;;;
;;; Returns:
;;;     the GType of the items contained in list .
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_item_type" g-list-model-item-type) g-type
  (list (g-object g-list-model)))

(export 'g-list-model-item-type)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_n_items ()
;;;
;;; guint
;;; g_list_model_get_n_items (GListModel *list);
;;;
;;; Gets the number of items in list .
;;;
;;; Depending on the model implementation, calling this function may be less
;;; efficient than iterating the list with increasing values for position until
;;; g_list_model_get_item() returns NULL.
;;;
;;; list:
;;;     a GListModel
;;;
;;; Returns:
;;;     the number of items in list .
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_n_items" g-list-model-n-items) :uint
  (list (g-object g-list-model)))

(export 'g-list-model-n-items)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item ()
;;;
;;; gpointer
;;; g_list_model_get_item (GListModel *list,
;;;                        guint position);
;;;
;;; Get the item at position . If position is greater than the number of items
;;; in list , NULL is returned.
;;;
;;; NULL is never returned for an index that is smaller than the length of the
;;; list. See g_list_model_get_n_items().
;;;
;;; list:
;;;     a GListModel
;;;
;;; position:
;;;     the position of the item to fetch
;;;
;;; Returns:
;;;     the item at position .
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_item" g-list-model-item) :pointer
  (list (g-object g-list-model))
  (position :uint))

(export 'g-list-model-item)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_object ()
;;;
;;; GObject *
;;; g_list_model_get_object (GListModel *list,
;;;                          guint position);
;;;
;;; Get the item at position . If position is greater than the number of items
;;; in list , NULL is returned.
;;;
;;; NULL is never returned for an index that is smaller than the length of the
;;; list. See g_list_model_get_n_items().
;;;
;;; list:
;;;     a GListModel
;;;
;;; position:
;;;     the position of the item to fetch
;;;
;;; Returns:
;;;     the object at position .
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_object" g-list-model-object) g-object
  (list (g-object g-list-model))
  (position :uint))

(export 'g-list-model-object)

;;; ----------------------------------------------------------------------------
;;; g_list_model_items_changed ()
;;;
;;; void
;;; g_list_model_items_changed (GListModel *list,
;;;                             guint position,
;;;                             guint removed,
;;;                             guint added);
;;;
;;; Emits the “items-changed” signal on list .
;;;
;;; This function should only be called by classes implementing GListModel. It
;;; has to be called after the internal representation of list has been updated,
;;; because handlers connected to this signal might query the new state of the
;;; list.
;;;
;;; Implementations must only make changes to the model (as visible to its
;;; consumer) in places that will not cause problems for that consumer. For
;;; models that are driven directly by a write API (such as GListStore), changes
;;; can be reported in response to uses of that API. For models that represent
;;; remote data, changes should only be made from a fresh mainloop dispatch. It
;;; is particularly not permitted to make changes in response to a call to the
;;; GListModel consumer API.
;;;
;;; Stated another way: in general, it is assumed that code making a series of
;;; accesses to the model via the API, without returning to the mainloop, and
;;; without calling other code, will continue to view the same contents of the
;;; model.
;;;
;;; list:
;;;     a GListModel
;;;
;;; position:
;;;     the position at which list changed
;;;
;;; removed:
;;;     the number of items removed
;;;
;;; added:
;;;     the number of items added
;;;
;;; Since: 2.44
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_items_changed" g-list-model-items-changed) :void
  (list (g-object g-list-model))
  (position :uint)
  (removed :uint)
  (added :uint))

(export 'g-list-model-items-changed)

;;; --- End of file gio.list-model.lisp ----------------------------------------
