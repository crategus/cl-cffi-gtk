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
;;; g_list_model_get_item_type () -> g-list-model-item-type
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_item_type" g-list-model-item-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-model} object}
  @return{The @class{g-type} type of the items contained in @arg{list}.}
  @begin{short}
    Gets the type of the items in the list.
  @end{short}
  All items returned from the @sym{g-list-model-type} function are of that type
  or a subtype, or are an implementation of that interface.

  The item type of a @class{g-list-model} object can not change during the life
  of the model.
  @see-class{g-list-model}
  @see-class{g-type}"
  (list (g-object g-list-model)))

(export 'g-list-model-item-type)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_n_items () -> g-list-model-n-items
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_n_items" g-list-model-n-items) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-model} object}
  @return{An integer with the number of items in @arg{list}.}
  @begin{short}
    Gets the number of items in the list.
  @end{short}
  Depending on the model implementation, calling this function may be less
  efficient than iterating the list with increasing values for position until
  the @fun{g-list-model-item} functions returns @code{null-pointer}.
  @see-class{g-list-model}
  @see-function{g-list-model-item}"
  (list (g-object g-list-model)))

(export 'g-list-model-n-items)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_item () -> g-list-model-item
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_item" g-list-model-item) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-model} object}
  @argument[position]{an unsigned integer with the position of the item to
    fetch}
  @return{An pointer with the item at @arg{position}.}
  @begin{short}
    Get the item at @arg{position}.
  @end{short}
  If the @arg{position} argument is greater than the number of items in the
  list, @code{null-pointer} is returned.

  The @code{null-pointer} value is never returned for an index that is smaller
  than the length of the list. See the @fun{g-list-model-n-items} function.
  @see-class{g-list-model}
  @see-function{g-list-model-n-items}"
  (list (g-object g-list-model))
  (position :uint))

(export 'g-list-model-item)

;;; ----------------------------------------------------------------------------
;;; g_list_model_get_object () -> g-list-model-object
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_get_object" g-list-model-object) g-object
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-model} object}
  @argument[position]{an unsigned integer with the position of the item to
    fetch}
  @return{The @class{g-object} instance at @arg{position}.}
  @begin{short}
    Get the item at @arg{position}.
  @end{short}
  If the @arg{position} argument is greater than the number of items in the
  list, @code{nil} is returned. The @code{nil} value is never returned for an
  index that is smaller than the length of the list. See the
  @fun{g-list-model-n-items} function.
  @see-class{g-list-model}
  @see-function{g-list-model-n-items}"
  (list (g-object g-list-model))
  (position :uint))

(export 'g-list-model-object)

;;; ----------------------------------------------------------------------------
;;; g_list_model_items_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_model_items_changed" g-list-model-items-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-10}
  @argument[list]{a @class{g-list-model} object}
  @argument[position]{an unsigned integer with the position at which @arg{list}
    changed}
  @argument[removed]{an unsigned integer with the number of items removed}
  @argument[added]{an unsigned integer with the number of items added}
  @begin{short}
    Emits the \"items-changed\" signal on @arg{list}.
  @end{short}

  This function should only be called by classes implementing the
  @class{g-list-model} interface. It has to be called after the internal
  representation of list has been updated, because handlers connected to this
  signal might query the new state of the list.

  Implementations must only make changes to the model, as visible to its
  consumer, in places that will not cause problems for that consumer. For models
  that are driven directly by a write API, such as the @class{g-list-store}
  object, changes can be reported in response to uses of that API. For models
  that represent remote data, changes should only be made from a fresh mainloop
  dispatch. It is particularly not permitted to make changes in response to a
  call to the @class{g-list-model} consumer API.

  Stated another way: in general, it is assumed that code making a series of
  accesses to the model via the API, without returning to the main loop, and
  without calling other code, will continue to view the same contents of the
  model.
  @see-class{g-list-model}"
  (list (g-object g-list-model))
  (position :uint)
  (removed :uint)
  (added :uint))

(export 'g-list-model-items-changed)

;;; --- End of file gio.list-model.lisp ----------------------------------------
