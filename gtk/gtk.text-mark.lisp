;;; ----------------------------------------------------------------------------
;;; gtk.text-mark.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkTextMark
;;;
;;; A position in the buffer preserved across buffer modifications
;;;
;;; Synopsis
;;;
;;;     GtkTextMark
;;;
;;;     gtk_text_mark_new
;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible
;;;     gtk_text_mark_get_deleted
;;;     gtk_text_mark_get_name
;;;     gtk_text_mark_get_buffer
;;;     gtk_text_mark_get_left_gravity
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextMark
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextMark" gtk-text-mark
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_mark_get_type")
  ((left-gravity
    gtk-text-mark-left-gravity
    "left-gravity" "gboolean" t nil)
   (name
    gtk-text-mark-name
    "name" "gchararray" t nil)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-mark 'type)
 "@version{2013-4-29}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}

  A @sym{gtk-text-mark} is like a bookmark in a text buffer; it preserves a
  position in the text. You can convert the mark to an iterator using the
  function @fun{gtk-text-buffer-get-iter-at-mark}. Unlike iterators, marks
  remain valid across buffer mutations, because their behavior is defined when
  text is inserted or deleted. When text containing a mark is deleted, the mark
  remains in the position originally occupied by the deleted text. When text
  is inserted at a mark, a mark with left gravity will be moved to the
  beginning of the newly-inserted text, and a mark with right gravity will be
  moved to the end.

  Marks are reference counted, but the reference count only controls the
  validity of the memory; marks can be deleted from the buffer at any time
  with the function @fun{gtk-text-buffer-delete-mark}. Once deleted from the
  buffer, a mark is essentially useless.

  Marks optionally have names; these can be convenient to avoid passing the
  @sym{gtk-text-mark} object around.

  Marks are typically created using the @fun{gtk-text-buffer-create-mark}
  function.
  @see-slot{gtk-text-mark-left-gravity}
  @see-slot{gtk-text-mark-name}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "left-gravity"
                                               'gtk-text-mark) 't)
 "The @code{\"left-gravity\"} property of type @code{:boolean}
  (Read / Write / Construct)@br{}
  Whether the mark has left gravity. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-text-mark) 't)
 "The @code{\"name\"} property of type @code{:string}
  (Read / Write / Construct)@br{}
  Mark name. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-mark-left-gravity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-mark-left-gravity 'function)
 "@version{2013-3-24}
  Accessor of the slot @code{\"left-gravity\"} of the @class{gtk-text-mark}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-mark-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-mark-name 'function)
 "@version{2013-3-24}
  Accessor of the slot @code{\"name\"} of the @class{gtk-text-mark} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-new))

(defun gtk-text-mark-new (name left-gravity)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[name]{mark name or @code{nil}}
  @argument[left-gravity]{whether the mark should have left gravity}
  @return{New @class{gtk-text-mark} object.}
  @begin{short}
    Creates a text mark. Add it to a buffer using the function
    @fun{gtk-text-buffer-add-mark}. If @arg{name} is @code{nil}, the mark is
    anonymous; otherwise, the mark can be retrieved by @arg{name} using
    @fun{gtk-text-buffer-get-mark}. If a mark has left gravity, and text
    is inserted at the mark's current location, the mark will be moved to the
    left of the newly-inserted text. If the mark has right gravity
    (@arg{left-gravity} = @code{nil}), the mark will end up on the right of
    newly inserted text. The standard left-to-right cursor is a mark with right
    gravity (when you type, the cursor stays on the right side of the text
    you are typing).
  @end{short}

  Since 2.12
  @see-function{gtk-text-buffer-add-mark}
  @see-function{gtk-text-buffer-get-mark}"
  (make-instance 'gtk-text-mark
                 :name name
                 :left-gravity left-gravity))

(export 'gtk-text-mark-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_set_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_set_visible" gtk-text-mark-set-visible) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @argument[setting]{visibility of @arg{mark}}
  Sets the visibility of @arg{mark}; the insertion point is normally visible,
  i. e. you can see it as a vertical bar. Also, the text widget uses a visible
  mark to indicate where a drop will occur when dragging-and-dropping text. Most
  other marks are not visible. Marks are not visible by default."
  (mark (g-object gtk-text-mark))
  (setting :boolean))

(export 'gtk-text-mark-set-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_get_visible" gtk-text-mark-get-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{@em{True} if visible.}
  Returns @em{true} if the @arg{mark} is visible (i. e. a cursor is displayed
  for it)."
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-mark-get-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_deleted ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_get_deleted" gtk-text-mark-get-deleted) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{Whether the @arg{mark} is deleted.}
  Returns @em{true} if the @arg{mark} has been removed from its buffer with the
  function @fun{gtk-text-buffer-delete-mark}. See the function
  @fun{gtk-text-buffer-add-mark} for a way to add it to a buffer again.
  @see-function{gtk-text-buffer-delete-mark}
  @see-function{gtk-text-buffer-add-mark}"
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-mark-get-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-name))

(defun gtk-text-mark-get-name (mark)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{The name of @arg{mark}.}
  Returns the @arg{mark} name; returns @code{nil} for anonymous marks."
  (gtk-text-mark-name mark))

(export 'gtk-text-mark-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_buffer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_get_buffer" gtk-text-mark-get-buffer)
    (g-object gtk-text-buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{The @arg{mark}'s @class{gtk-text-buffer} object.}
  Gets the buffer this @arg{mark} is located inside, or @code{nil} if the
  @arg{mark} is deleted."
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-mark-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_left_gravity ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-mark-get-left-gravity))

(defun gtk-text-mark-get-left-gravity (mark)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{@em{True} if the @arg{mark} has left gravity, @code{nil} otherwise.}
  Determines whether the @arg{mark} has left gravity."
  (gtk-text-mark-left-gravity mark))

(export 'gtk-text-mark-get-left-gravity)

;;; --- End of file gtk.text-mark.lisp -----------------------------------------
