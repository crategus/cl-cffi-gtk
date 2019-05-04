;;; ----------------------------------------------------------------------------
;;; gtk.entry-buffer.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;; GtkEntryBuffer
;;;
;;;     Text buffer for GtkEntry
;;;
;;; Types and Values
;;;
;;;     GtkEntryBuffer
;;;
;;; Functions
;;;
;;;     gtk_entry_buffer_new
;;;     gtk_entry_buffer_get_text
;;;     gtk_entry_buffer_set_text
;;;     gtk_entry_buffer_get_bytes
;;;     gtk_entry_buffer_get_length
;;;     gtk_entry_buffer_get_max_length
;;;     gtk_entry_buffer_set_max_length
;;;     gtk_entry_buffer_insert_text
;;;     gtk_entry_buffer_delete_text
;;;     gtk_entry_buffer_emit_deleted_text
;;;     gtk_entry_buffer_emit_inserted_text
;;;
;;; Properties
;;;
;;;     guint   length           Read
;;;      gint   max-length       Read / Write
;;;     gchar*  text             Read / Write
;;;
;;; Signals
;;;
;;;      void   deleted-text     Run First
;;;      void   inserted-text    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEntryBuffer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEntryBuffer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEntryBuffer" gtk-entry-buffer
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_entry_buffer_get_type")
  ((length
    gtk-entry-buffer-length
    "length" "guint" t nil)
   (max-length
    gtk-entry-buffer-max-length
    "max-length" "gint" t t)
   (text
    gtk-entry-buffer-text
    "text" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-entry-buffer 'type)
 "@version{2013-7-26}
  @begin{short}
    The @sym{gtk-entry-buffer} object contains the actual text displayed in a
    @class{gtk-entry} widget.
  @end{short}

  A single @sym{gtk-entry-buffer} object can be shared by multiple
  @class{gtk-entry} widgets which will then share the same text content, but
  not the cursor position, visibility attributes, icon etc.

  @sym{gtk-entry-buffer} may be derived from. Such a derived class might allow
  text to be stored in an alternate location, such as non-pageable memory,
  useful in the case of important passwords. Or a derived class could integrate
  with an application's concept of undo/redo.
  @begin[Signal Details]{dictionary}
    @subheading{The \"deleted-text\" signal}
      @begin{pre}
 lambda (buffer position n-chars)   : Run First
      @end{pre}
      This signal is emitted after text is deleted from the buffer.
      @begin[code]{table}
        @entry[buffer]{A @sym{gtk-entry-buffer} object.}
        @entry[position]{The position the text was deleted at.}
        @entry[n-chars]{The number of characters that were deleted.}
      @end{table}
    @subheading{The \"inserted-text\" signal}
      @begin{pre}
 lambda (buffer position chars n-chars)   : Run First
      @end{pre}
      This signal is emitted after text is inserted into the buffer.
      @begin[code]{table}
        @entry[buffer]{A @sym{gtk-entry-buffer} object.}
        @entry[position]{The position the text was inserted at.}
        @entry[chars]{The text that was inserted.}
        @entry[n-chars]{The number of characters that were inserted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-entry-buffer-length}
  @see-slot{gtk-entry-buffer-max-length}
  @see-slot{gtk-entry-buffer-text}
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-entry-buffer-length ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "length" 'gtk-entry-buffer) 't)
 "The @code{length} property of type @code{:uint} (Read) @br{}
  The length (in characters) of the text in buffer. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-length 'function)
 "@version{2014-9-11}
  @argument[object]{a @class{gtk-entry-buffer} object}
  @begin{short}
    Accessor of the slot @slot[gtk-entry-buffer]{length} of the
    @class{gtk-entry-buffer} class.
  @end{short}

  The generic function @sym{gtk-entry-buffer-length} retrieves the length in
  characters of the buffer.
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-get-bytes}")

;;; --- gtk-entry-buffer-max-length --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-length"
                                               'gtk-entry-buffer) 't)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  The maximum length (in characters) of the text in the buffer. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-max-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-max-length 'function)
 "@version{2014-11-9}
  @argument[object]{a @class{gtk-entry-buffer} object}
  @argument[max-length]{the maximum length of the entry @arg{buffer}, or 0 for
    no maximum. The value passed in will be clamped to the range 0-65536.}
  @syntax[]{(gtk-entry-buffer-max-length object) => max-length}
  @syntax[]{(setf gtk-entry-buffer-max-length object) max-length)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry-buffer]{max-length} of the
    @class{gtk-entry-buffer} class.
  @end{short}

  The generic function @sym{gtk-entry-buffer-max-length} returns the maximum
  allowed number of characters in a @class{gtk-entry-buffer} object, or 0 if
  there is no maximum.

  The generic function @sym{(setf get-entry-buffer-max-length)} sets the
  maximum allowed length of the contents of the buffer. If the current contents
  are longer than the given length, then they will be truncated to fit.
  @see-class{gtk-entry-buffer}")

;;; --- gtk-entry-buffer-text --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-entry-buffer) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The contents of the buffer. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-text 'function)
 "@version{2014-11-9}
  @argument[object]{a @class{gtk-entry-buffer} object}
  @argument[text]{the new text}
  @syntax[]{(gtk-entry-buffer-text object) => text}
  @syntax[]{(setf gtk-entry-buffer-text object) text)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry-buffer]{text} of the
    @class{gtk-entry-buffer} class.
  @end{short}

  The generic function @sym{gtk-entry-buffer-text} retrieves the contents of
  the buffer.

  The generic function @sym{(setf gtk-entry-buffer-text} sets the @arg{text} in
  the @arg{buffer}.

  This is roughly equivalent to calling the functions
  @fun{gtk-entry-buffer-delete-text} and @fun{gtk-entry-buffer-insert-text}.
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-delete-text}
  @see-function{gtk-entry-buffer-insert-text}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-entry-buffer-new (&optional (text nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[text]{initial buffer text, or nil}
  @return{A new @class{gtk-entry-buffer} object.}
  @begin{short}
    Create a new @class{gtk-entry-buffer} object.
  @end{short}

  Optionally, specify initial text to set in the buffer.
  @see-class{gtk-entry-buffer}"
  (let ((buffer (make-instance 'gtk-entry-buffer)))
    (when text
      (setf (gtk-entry-buffer-text buffer) text))
    buffer))

(export 'gtk-entry-buffer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_bytes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_buffer_get_bytes" gtk-entry-buffer-get-bytes) g-size
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @return{The byte length of the @arg{buffer}.}
  @begin{short}
    Retrieves the length in bytes of the @arg{buffer}.
  @end{short}
  See the function @fun{gtk-entry-buffer-get-length}.
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-get-length}"
  (buffer (g-object gtk-entry-buffer)))

(export 'gtk-entry-buffer-get-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_insert_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_buffer_insert_text" %gtk-entry-buffer-insert-text) :uint
  (buffer (g-object gtk-entry-buffer))
  (position :uint)
  (text :string)
  (n-chars :int))

(defun gtk-entry-buffer-insert-text (buffer position text)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @argument[position]{the position at which to insert text}
  @argument[text]{the text to insert into the @arg{buffer}}
  @return{The number of characters actually inserted.}
  @begin{short}
    Inserts @arg{text} into the contents of the @arg{buffer}, at position
    @arg{position}.
  @end{short}

  If @arg{position} or the length of the @arg{text} are out of bounds, or the
  maximum buffer text length is exceeded, then they are coerced to sane
  values.

  Note that the @arg{position} is in characters, not in bytes.
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-delete-text}"
  (%gtk-entry-buffer-insert-text buffer position text -1))

(export 'gtk-entry-buffer-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_delete_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_buffer_delete_text" gtk-entry-buffer-delete-text) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @argument[position]{position at which to delete text}
  @argument[n-chars]{number of characters to delete}
  @return{The number of characters deleted.}
  @begin{short}
    Deletes a sequence of characters from the @arg{buffer}.
  @end{short}
  @arg{n-chars} characters are deleted starting at @arg{position}. If
  @arg{n-chars} is negative, then all characters until the end of the text are
  deleted.

  If @arg{position} or @arg{n-chars} are out of bounds, then they are coerced
  to sane values.

  Note that the positions are specified in characters, not bytes.
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-insert-text}"
  (buffer (g-object gtk-entry-buffer))
  (position :uint)
  (n-chars :int))

(export 'gtk-entry-buffer-delete-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_deleted_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_buffer_emit_deleted_text"
           gtk-entry-buffer-emit-deleted-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @argument[position]{position at which text was deleted}
  @arg{n-chars]{number of characters deleted}
  @begin{short}
    Used when subclassing @class{gtk-entry-buffer}.
  @end{short}
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-emit-inserted-text}"
  (buffer (g-object gtk-entry-buffer))
  (position :uint)
  (n-chars :int))

(export 'gtk-entry-buffer-emit-deleted-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_inserted_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_buffer_emit_inserted_text"
           gtk-entry-buffer-emit-inserted-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-27}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @argument[position]{position at which text was inserted}
  @argument[text]{text that was inserted}
  @argument[n-chars]{number of characters inserted}
  @begin{short}
    Used when subclassing @class{gtk-entry-buffer} object.
  @end{short}
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-emit-deleted-text}"
  (buffer (g-object gtk-entry-buffer))
  (position :uint)
  (text :string)
  (n-chars :int))

(export 'gtk-entry-buffer-emit-inserted-text)

;;; --- End of file gtk.entry-buffer.lisp --------------------------------------
