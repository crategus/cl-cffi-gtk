;;; ----------------------------------------------------------------------------
;;; gtk.entry-buffer.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; Text buffer for GtkEntry
;;;
;;; Synopsis
;;;
;;;     GtkEntryBuffer
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
    "text" "gchar" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-entry-buffer 'type)
 "@version{2013-4-28}
  @begin{short}
    The @class{gtk-entry-buffer} class contains the actual text displayed in a
    @class{gtk-entry} widget.
  @end{short}

  A single @class{gtk-entry-buffer} object can be shared by multiple
  @class{gtk-entry} widgets which will then share the same text content, but not
  the cursor position, visibility attributes, icon etc.

  @class{gtk-entry-buffer} may be derived from. Such a derived class might allow
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
        @entry[buffer]{A @class{gtk-entry-buffer} object.}
        @entry[position]{The position the text was deleted at.}
        @entry[n-chars]{The number of characters that were deleted.}
      @end{table}
      Since 2.18

    @subheading{The \"inserted-text\" signal}
      @begin{pre}
 lambda (buffer position chars n-chars)   : Run First
      @end{pre}
      This signal is emitted after text is inserted into the buffer.
      @begin[code]{table}
        @entry[buffer]{A @class{gtk-entry-buffer} object.}
        @entry[position]{The position the text was inserted at.}
        @entry[chars]{The text that was inserted.}
        @entry[n-chars]{The number of characters that were inserted.}
      @end{table}
      Since 2.18
  @end{dictionary}
  @see-slot{gtk-entry-buffer-length}
  @see-slot{gtk-entry-buffer-max-length}
  @see-slot{gtk-entry-buffer-text}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "length" 'gtk-entry-buffer) 't)
 "The @code{\"length\"} property of type @code{:uint} (Read)@br{}
  The length (in characters) of the text in buffer.@br{}
  Allowed values: <= 65535@br{}
  Default value: 0@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-length"
                                               'gtk-entry-buffer) 't)
 "The @code{\"max-length\"} property of type @code{:int} (Read / Write)@br{}
  The maximum length (in characters) of the text in the buffer.@br{}
  Allowed values: [0,65535]@br{}
  Default value: 0@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-entry-buffer) 't)
 "The @code{\"text\"} property of type @code{:string} (Read / Write)@br{}
  The contents of the buffer.@br{}
  Default value: \"\"@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-length 'function)
 "@version{2013-4-28}
  Accessor of the slot @code{\"length\"} of the @class{gtk-entry-buffer}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-max-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-max-length 'function)
 "@version{2013-4-28}
  Accessor of the slot @code{\"max-length\"} of the @class{gtk-entry-buffer}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer-text 'function)
 "@version{2013-4-28}
  Accessor of the slot @code{\"text\"} of the @class{gtk-entry-buffer} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_new ()
;;;
;;; GtkEntryBuffer * gtk_entry_buffer_new (const gchar *initial_chars,
;;;                                        gint n_initial_chars);
;;;
;;; Create a new GtkEntryBuffer object.
;;;
;;; Optionally, specify initial text to set in the buffer.
;;;
;;; initial_chars :
;;;     initial buffer text, or NULL
;;;
;;; n_initial_chars :
;;;     number of characters in initial_chars, or -1
;;;
;;; Returns :
;;;     A new GtkEntryBuffer object.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_text ()
;;;
;;; const gchar * gtk_entry_buffer_get_text (GtkEntryBuffer *buffer);
;;;
;;; Retrieves the contents of the buffer.
;;;
;;; The memory pointer returned by this call will not change unless this object
;;; emits a signal, or is finalized.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; Returns :
;;;     a pointer to the contents of the widget as a string. This string points
;;;     to internally allocated storage in the buffer and must not be freed,
;;;     modified or stored.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_set_text ()
;;;
;;; void gtk_entry_buffer_set_text (GtkEntryBuffer *buffer,
;;;                                 const gchar *chars,
;;;                                 gint n_chars);
;;;
;;; Sets the text in the buffer.
;;;
;;; This is roughly equivalent to calling gtk_entry_buffer_delete_text() and
;;; gtk_entry_buffer_insert_text().
;;;
;;; Note that n_chars is in characters, not in bytes.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; chars :
;;;     the new text
;;;
;;; n_chars :
;;;     the number of characters in text, or -1
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_bytes ()
;;;
;;; gsize gtk_entry_buffer_get_bytes (GtkEntryBuffer *buffer);
;;;
;;; Retrieves the length in bytes of the buffer. See
;;; gtk_entry_buffer_get_length().
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; Returns :
;;;     The byte length of the buffer.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_length ()
;;;
;;; guint gtk_entry_buffer_get_length (GtkEntryBuffer *buffer);
;;;
;;; Retrieves the length in characters of the buffer.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; Returns :
;;;     The number of characters in the buffer.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_max_length ()
;;;
;;; gint gtk_entry_buffer_get_max_length (GtkEntryBuffer *buffer);
;;;
;;; Retrieves the maximum allowed length of the text in buffer. See
;;; gtk_entry_buffer_set_max_length().
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; Returns :
;;;     the maximum allowed number of characters in GtkEntryBuffer, or 0 if
;;;     there is no maximum.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_set_max_length ()
;;;
;;; void gtk_entry_buffer_set_max_length (GtkEntryBuffer *buffer,
;;;                                       gint max_length);
;;;
;;; Sets the maximum allowed length of the contents of the buffer. If the
;;; current contents are longer than the given length, then they will be
;;; truncated to fit.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; max_length :
;;;     the maximum length of the entry buffer, or 0 for no maximum. (other
;;;     than the maximum length of entries.) The value passed in will be clamped
;;;     to the range 0-65536.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_insert_text ()
;;;
;;; guint gtk_entry_buffer_insert_text (GtkEntryBuffer *buffer,
;;;                                     guint position,
;;;                                     const gchar *chars,
;;;                                     gint n_chars);
;;;
;;; Inserts n_chars characters of chars into the contents of the buffer, at
;;; position position.
;;;
;;; If n_chars is negative, then characters from chars will be inserted until a
;;; null-terminator is found. If position or n_chars are out of bounds, or the
;;; maximum buffer text length is exceeded, then they are coerced to sane
;;; values.
;;;
;;; Note that the position and length are in characters, not in bytes.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; position :
;;;     the position at which to insert text.
;;;
;;; chars :
;;;     the text to insert into the buffer.
;;;
;;; n_chars :
;;;     the length of the text in characters, or -1
;;;
;;; Returns :
;;;     The number of characters actually inserted.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_delete_text ()
;;;
;;; guint gtk_entry_buffer_delete_text (GtkEntryBuffer *buffer,
;;;                                     guint position,
;;;                                     gint n_chars);
;;;
;;; Deletes a sequence of characters from the buffer. n_chars characters are
;;; deleted starting at position. If n_chars is negative, then all characters
;;; until the end of the text are deleted.
;;;
;;; If position or n_chars are out of bounds, then they are coerced to sane
;;; values.
;;;
;;; Note that the positions are specified in characters, not bytes.
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; position :
;;;     position at which to delete text
;;;
;;; n_chars :
;;;     number of characters to delete
;;;
;;; Returns :
;;;     The number of characters deleted.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_deleted_text ()
;;;
;;; void gtk_entry_buffer_emit_deleted_text (GtkEntryBuffer *buffer,
;;;                                          guint position,
;;;                                          guint n_chars);
;;;
;;; Used when subclassing GtkEntryBuffer
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; position :
;;;     position at which text was deleted
;;;
;;; n_chars :
;;;     number of characters deleted
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_inserted_text ()
;;;
;;; void gtk_entry_buffer_emit_inserted_text (GtkEntryBuffer *buffer,
;;;                                           guint position,
;;;                                           const gchar *chars,
;;;                                           guint n_chars);
;;;
;;; Used when subclassing GtkEntryBuffer
;;;
;;; buffer :
;;;     a GtkEntryBuffer
;;;
;;; position :
;;;     position at which text was inserted
;;;
;;; chars :
;;;     text that was inserted
;;;
;;; n_chars :
;;;     number of characters inserted
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.entry-buffer.lisp --------------------------------------
