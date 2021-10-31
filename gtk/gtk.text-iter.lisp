;;; ----------------------------------------------------------------------------
;;; gtk.text-iter.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkTextIter
;;;
;;;     Text buffer iterator
;;;
;;; Types and Values
;;;
;;;     GtkTextIter
;;;     GtkTextSearchFlags
;;;
;;; Functions
;;;
;;;     gtk_text_iter_get_buffer
;;;     gtk_text_iter_copy
;;;     gtk_text_iter_assign
;;;     gtk_text_iter_free
;;;     gtk_text_iter_get_offset
;;;     gtk_text_iter_get_line
;;;     gtk_text_iter_get_line_offset
;;;     gtk_text_iter_get_line_index
;;;     gtk_text_iter_get_visible_line_index
;;;     gtk_text_iter_get_visible_line_offset
;;;     gtk_text_iter_get_char
;;;     gtk_text_iter_get_slice
;;;     gtk_text_iter_get_text
;;;     gtk_text_iter_get_visible_slice
;;;     gtk_text_iter_get_visible_text
;;;     gtk_text_iter_get_pixbuf
;;;     gtk_text_iter_get_marks
;;;     gtk_text_iter_get_toggled_tags
;;;     gtk_text_iter_get_child_anchor
;;;     gtk_text_iter_starts_tag
;;;     gtk_text_iter_begins_tag
;;;     gtk_text_iter_ends_tag
;;;     gtk_text_iter_toggles_tag
;;;     gtk_text_iter_has_tag
;;;     gtk_text_iter_get_tags
;;;     gtk_text_iter_editable
;;;     gtk_text_iter_can_insert
;;;     gtk_text_iter_starts_word
;;;     gtk_text_iter_ends_word
;;;     gtk_text_iter_inside_word
;;;     gtk_text_iter_starts_line
;;;     gtk_text_iter_ends_line
;;;     gtk_text_iter_starts_sentence
;;;     gtk_text_iter_ends_sentence
;;;     gtk_text_iter_inside_sentence
;;;     gtk_text_iter_is_cursor_position
;;;     gtk_text_iter_get_chars_in_line
;;;     gtk_text_iter_get_bytes_in_line
;;;     gtk_text_iter_get_attributes
;;;     gtk_text_iter_get_language
;;;     gtk_text_iter_is_end
;;;     gtk_text_iter_is_start
;;;     gtk_text_iter_forward_char
;;;     gtk_text_iter_backward_char
;;;     gtk_text_iter_forward_chars
;;;     gtk_text_iter_backward_chars
;;;     gtk_text_iter_forward_line
;;;     gtk_text_iter_backward_line
;;;     gtk_text_iter_forward_lines
;;;     gtk_text_iter_backward_lines
;;;     gtk_text_iter_forward_word_ends
;;;     gtk_text_iter_backward_word_starts
;;;     gtk_text_iter_forward_word_end
;;;     gtk_text_iter_backward_word_start
;;;     gtk_text_iter_forward_cursor_position
;;;     gtk_text_iter_backward_cursor_position
;;;     gtk_text_iter_forward_cursor_positions
;;;     gtk_text_iter_backward_cursor_positions
;;;     gtk_text_iter_backward_sentence_start
;;;     gtk_text_iter_backward_sentence_starts
;;;     gtk_text_iter_forward_sentence_end
;;;     gtk_text_iter_forward_sentence_ends
;;;     gtk_text_iter_forward_visible_word_ends
;;;     gtk_text_iter_backward_visible_word_starts
;;;     gtk_text_iter_forward_visible_word_end
;;;     gtk_text_iter_backward_visible_word_start
;;;     gtk_text_iter_forward_visible_cursor_position
;;;     gtk_text_iter_backward_visible_cursor_position
;;;     gtk_text_iter_forward_visible_cursor_positions
;;;     gtk_text_iter_backward_visible_cursor_positions
;;;     gtk_text_iter_forward_visible_line
;;;     gtk_text_iter_backward_visible_line
;;;     gtk_text_iter_forward_visible_lines
;;;     gtk_text_iter_backward_visible_lines
;;;     gtk_text_iter_set_offset
;;;     gtk_text_iter_set_line
;;;     gtk_text_iter_set_line_offset
;;;     gtk_text_iter_set_line_index
;;;     gtk_text_iter_set_visible_line_index
;;;     gtk_text_iter_set_visible_line_offset
;;;     gtk_text_iter_forward_to_end
;;;     gtk_text_iter_forward_to_line_end
;;;     gtk_text_iter_forward_to_tag_toggle
;;;     gtk_text_iter_backward_to_tag_toggle
;;;     gtk_text_iter_forward_find_char
;;;     gtk_text_iter_backward_find_char
;;;
;;;     gtk_text_iter_forward_search
;;;     gtk_text_iter_backward_search
;;;     gtk_text_iter_equal
;;;     gtk_text_iter_compare
;;;     gtk_text_iter_in_range
;;;     gtk_text_iter_order
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkTextIter
;;;
;;; Description
;;;
;;; You may wish to begin by reading the text widget conceptual overview which
;;; gives an overview of all the objects and data types related to the text
;;; widget and how they work together.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------

(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod translate-from-foreign (value (type unichar))
  (code-char value))

(defmethod translate-to-foreign (value (type unichar))
  (char-code value))

;;; ----------------------------------------------------------------------------
;;; enum GtkTextSearchFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTextSearchFlags" gtk-text-search-flags
  (:export t
   :type-initializer "gtk_text_search_flags_get_type")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-search-flags atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-text-search-flags atdoc:*external-symbols*)
 "@version{2021-6-13}
  @short{Flags affecting how a search is done.}

  If neither @code{:visible-only} nor @code{:text-only} are enabled, the match
  must be exact. The special @code{0xFFFC} character will match embedded pixbufs
  or child widgets.
  @begin{pre}
(define-g-flags \"GtkTextSearchFlags\" gtk-text-search-flags
  (:export t
   :type-initializer \"gtk_text_search_flags_get_type\")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))
  @end{pre}
  @begin[code]{table}
    @entry[:visible-only]{Search only visible data. A search match may have
      invisible text interspersed.}
    @entry[:text-only]{Search only text. A match may have pixbufs or child
      widgets mixed inside the matched range.}
    @entry[:case-insensitive]{The text will be matched regardless of what case
      it is in.}
  @end{table}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-search}")

;;; ----------------------------------------------------------------------------
;;; GtkTextIter
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_text_iter_get_type" g-size))

(define-g-boxed-opaque gtk-text-iter "GtkTextIter"
  :alloc (%gtk-text-iter-alloc))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-iter atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-text-iter 'type)
 "@version{2021-6-13}
  @begin{short}
    Most text manipulation is accomplished with iterators, represented by a
    @sym{gtk-text-iter} instance.
  @end{short}
  An iterator represents a position between two characters in the text buffer.

  The @sym{gtk-text-iter} structure is designed to be allocated on the stack.
  It is guaranteed to be copiable by value and never contain any heap-allocated
  data. Iterators are not valid indefinitely. Whenever the text buffer is
  modified in a way that affects the number of characters in the text buffer,
  all outstanding iterators become invalid. Note that deleting 5 characters and
  then reinserting 5 still invalidates iterators, though you end up with the
  same number of characters you pass through a state with a different number.
  @see-class{gtk-text-buffer}")

(export (boxed-related-symbols 'gtk-text-iter))

;;; ----------------------------------------------------------------------------

(defcstruct %gtk-text-iter
  (dummy1 :pointer)
  (dummy2 :pointer)
  (dummy3 :int)
  (dummy4 :int)
  (dummy5 :int)
  (dummy6 :int)
  (dummy7 :int)
  (dummy8 :int)
  (dummy9 :pointer)
  (dummy10 :pointer)
  (dummy11 :int)
  (dummy12 :int)
  (dummy13 :int)
  (dummy14 :pointer))

(defun %gtk-text-iter-alloc ()
  (with-foreign-object (iter '(:struct %gtk-text-iter))
    (%gtk-text-iter-copy iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_buffer () -> gtk-text-iter-buffer
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_buffer" gtk-text-iter-buffer)
    (g-object gtk-text-buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} iterator}
  @return{A @class{gtk-text-buffer} object.}
  @begin{short}
    Returns the text buffer this iterator is associated with.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_copy" %gtk-text-iter-copy) :pointer
  (iter :pointer))

(defcfun ("gtk_text_iter_copy" gtk-text-iter-copy)
    (g-boxed-foreign gtk-text-iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A copy of @arg{iter}.}
  @begin{short}
    Creates a copy of an iterator.
  @end{short}
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_assign ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_assign" gtk-text-iter-assign) :void
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[other]{another @class{gtk-text-iter} instance}
  @begin{short}
    Assigns the value of @arg{other} to @arg{iter}.
  @end{short}
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter))
  (other (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-assign)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_free ()
;;;
;;; void gtk_text_iter_free (GtkTextIter *iter);
;;;
;;; Free an iterator allocated on the heap. This function is intended for use in
;;; language bindings, and is not especially useful for applications, because
;;; iterators can simply be allocated on the stack.
;;;
;;; iter :
;;;     a dynamically-allocated iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_offset ()
;;; gtk_text_iter_set_offset () -> gtk-text-iter-offset
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-offset) (char-offset iter)
  (foreign-funcall "gtk_text_iter_set_offset"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int char-offset
                   :void)
  char-offset)

(defcfun ("gtk_text_iter_get_offset" gtk-text-iter-offset) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @syntax[]{(gtk-text-iter-offset iter) => char-offset}
  @syntax[]{(setf (gtk-text-iter-offset iter) char-offset)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[char-offset]{an integer with a character offset}
  @begin{short}
    Accessor of the character offset of the iterator.
  @end{short}

  The function @sym{gtk-text-iter-offset} returns the character offset of an
  iterator. The funcion @sym{(setf gtk-text-iter-offset)} sets the iterator
  to point to the character offset.

  Each character in a text buffer has an offset, starting with 0 for the first
  character in the text buffer. Use the function
  @fun{gtk-text-buffer-iter-at-offset} to convert an character offset back into
  an iterator.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-iter-at-offset}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line ()
;;; gtk_text_iter_set_line () -> gtk-text-iter-line
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-line) (line-number iter)
  (foreign-funcall "gtk_text_iter_set_line"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int line-number
                   :void)
  line-number)

(defcfun ("gtk_text_iter_get_line" gtk-text-iter-line) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @syntax[]{(gtk-text-iter-line iter) => line-number}
  @syntax[]{(setf (gtk-text-iter-line iter) line-number)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[line-number]{an integer with the line number, counted from 0}
  @begin{short}
    Accessor of the line number containing the iterator.
  @end{short}

  The function @sym{gtk-text-iter-line} returns the line number containing the
  iterator. The function @sym{(setf gtk-text-iter-line)} moves the iterator
  to the start of the given line number.

  Lines in a text buffer are numbered beginning with 0 for the first line in
  the text buffer. If the line number is negative or larger than the number of
  lines in the text buffer, moves the iterator to the start of the last line in
  the text buffer.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_offset ()
;;; gtk_text_iter_set_line_offset () -> gtk-text-iter-line-offset
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-line-offset) (char-on-line iter)
  (foreign-funcall "gtk_text_iter_set_line_offset"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int char-on-line
                   :void)
  char-on-line)

(defcfun ("gtk_text_iter_get_line_offset" gtk-text-iter-line-offset) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-7-24}
  @syntax[]{(gtk-text-iter-line-offset iter) => char-on-line}
  @syntax[]{(setf (gtk-text-iter-line-offset iter) char-on-line)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[char-on-line]{an integer with a character offset relative to the
    start of the current line of the iterator}
  @begin{short}
    Accessor of the character offset relative to the start of the current line
    of the iterator.
  @end{short}

  The function @sym{gtk-text-iter-line-offset} returns the character offset of
  the iterator, counting from the start of a newline-terminated line. The
  function @sym{(setf gtk-text-iter-line-offset)} moves the iterator within a
  line, to a new character offset.

  The first character on the line has offset 0. The given character offset must
  be less than or equal to the number of characters in the line. If equal, the
  iterator moves to the start of the next line. See the function
  @fun{gtk-text-iter-line-index} if you have a byte index rather than a
  character offset.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-line-index}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_index ()
;;; gtk_text_iter_set_line_index () -> gtk-text-iter-line-index
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-line-index) (byte-on-line iter)
  (foreign-funcall "gtk_text_iter_set_line_index"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int byte-on-line
                   :void)
  byte-on-line)

(defcfun ("gtk_text_iter_get_line_index" gtk-text-iter-line-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @syntax[]{(gtk-text-iter-line-index iter) => byte-on-line}
  @syntax[]{(setf (gtk-text-iter-line-offset iter) byte-on-line)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[byte-on-line]{an integer with a byte index relative to the start
    of the current line of the iterator}
  @begin{short}
    Accessor of the byte offset relative to the start of the current line
    of the iterator.
  @end{short}

  The function @sym{gtk-text-iter-line-index} returns the byte index of the
  iterator, counting from the start of a newline-terminated line. The function
  @sym{(setf gtk-text-iter-line-index)} sets the byte index.

  Remember that the text buffer encodes text in UTF-8, and that characters can
  require a variable number of bytes to represent. The given byte index must be
  at the start of a character, it cannot be in the middle of a UTF-8 encoded
  character.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-line-offset}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_line_index ()
;;; gtk_text_iter_set_visible_line_index () -> gtk-text-iter-visible-line-index
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-visible-line-index) (byte-on-line iter)
  (foreign-funcall "gtk_text_iter_set_visible_line_index"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int byte-on-line
                   :void)
  byte-on-line)

(defcfun ("gtk_text_iter_get_visible_line_index"
           gtk-text-iter-visible-line-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @syntax[]{(gtk-text-iter-visible-line-index iter) => byte-on-line}
  @syntax[]{(setf (gtk-text-iter-visible-line-index iter) byte-on-line)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[byte-on-line]{an integer with a byte index}
  @begin{short}
    Accessor of the byte index relative to the start of the current line
    of the iterator.
  @end{short}

  The function @sym{gtk-text-iter-visible-line-index} returns the number of
  bytes from the start of the line to the given iterator, not counting bytes
  that are invisible due to tags with the \"invisible\" flag toggled on.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-visible-line-offset}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-visible-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_line_offset ()
;;; gtk_text_iter_set_visible_line_offset ()
;;; -> gtk-text-iter-visible-line-offset
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-iter-visible-line-offset) (char-on-line iter)
  (foreign-funcall "gtk_text_iter_set_visible_line_offset"
                   (g-boxed-foreign gtk-text-iter) iter
                   :int char-on-line
                   :void)
  char-on-line)

(defcfun ("gtk_text_iter_get_visible_line_offset"
           gtk-text-iter-visible-line-offset) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @syntax[]{(gtk-text-iter-visible-line-offset iter) => char-on-line}
  @syntax[]{(setf (gtk-text-iter-visible-line-offset iter) char-on-line)}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[char-on-line]{an integer with a character offset}
  @begin{short}
    Accessor of the character offset relative to the start of the current line
    of the iterator.
  @end{short}

  The function @sym{gtk-text-iter-visible-line-offset} returns the offset in
  characters from the start of the line to the given iterator, not counting
  characters that are invisible due to tags with the \"invisible\" flag toggled
  on.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-visible-line-index}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-visible-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_char () -> gtk-text-iter-char
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_char" gtk-text-iter-char) unichar
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A Unicode character.}
  @begin{short}
    Returns a Unicode character at this iterator, or 0 if the iterator is not
    dereferenceable.
  @end{short}
  If the element at this iterator is a non-character element, such as an image
  embedded in the text buffer, the Unicode \"unknown\" character @code{0xFFFC}
  is returned. If invoked on the end iterator, zero is returned.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-text}
  @see-function{gtk-text-iter-slice}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_slice () -> gtk-text-iter-slice
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_slice" gtk-text-iter-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[start]{a @class{gtk-text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk-text-iter} instance with the end of a range}
  @return{A string with a slice of text from the text buffer.}
  @begin{short}
    Returns a string with the text in the given range.
  @end{short}
  A \"slice\" is a string of characters encoded in UTF-8 format, including the
  Unicode \"unknown\" character @code{0xFFFC} for iterable non-character
  elements in the text buffer, such as images. Because images are encoded in
  the slice, byte and character offsets in the returned string will correspond
  to byte offsets in the text buffer. Note that the character @code{0xFFFC} can
  occur in normal text as well, so it is not a reliable indicator that a pixbuf
  or widget is in the text buffer.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-text}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_text () -> gtk-text-iter-text
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_text" gtk-text-iter-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[start]{a @class{gtk-text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk-text-iter} instance with the end of a range}
  @return{A string with characters from the text buffer.}
  @begin{short}
    Returns a string with the text in the given range.
  @end{short}

  If the range contains non-text elements such as images, the character and
  byte offsets in the returned string will not correspond to character and byte
  offsets in the text buffer. If you want offsets to correspond, see the
  function @fun{gtk-text-iter-slice}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-slice}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_slice () -> gtk-text-iter-visible-slice
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_slice" gtk-text-iter-visible-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[start]{a @class{gtk-text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk-text-iter} intance with the end of a range}
  @return{A string with a slice of text from the text buffer.}
  @begin{short}
    Like the function @fun{gtk-text-iter-slice}, but invisible text is not
    included.
  @end{short}

  Invisible text is usually invisible because a @class{gtk-text-tag} object
  with the \"invisible\" attribute turned on has been applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-slice}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-visible-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_text () -> gtk-text-iter-visible-text
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_text" gtk-text-iter-visible-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[start]{a @class{gtk-text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk-text-iter} instance with the end of a range}
  @return{A string containing visible text in the range.}
  @begin{short}
    Like the function @fun{gtk-text-iter-text}, but invisible text is not
    included.
  @end{short}

  Invisible text is usually invisible because a @class{gtk-text-tag} object
  with the \"invisible\" attribute turned on has been applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-text}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-visible-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_pixbuf () -> gtk-text-iter-pixbuf
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_pixbuf" gtk-text-iter-pixbuf) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{The @class{gdk-pixbuf} object at the iterator.}
  @begin{short}
    If the element at the iterator is a pixbuf, the pixbuf is returned.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gdk-pixbuf}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_marks () -> gtk-text-iter-marks
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_marks" gtk-text-iter-marks)
    (g-slist (g-object gtk-text-mark) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{List of @class{gtk-text-mark} objects.}
  @begin{short}
    Returns a list of all text marks at this location.
  @end{short}
  Because text marks are not iterable, they do not take up any \"space\" in the
  text buffer, they are just text marks in between iterable locations, multiple
  text marks can exist in the same place. The returned list is not in any
  meaningful order.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_toggled_tags () -> gtk-text-iter-toggled-tags
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_toggled_tags" gtk-text-iter-toggled-tags)
    (g-slist (g-object gtk-text-tag))
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[toggled-on]{@em{true} to get toggled-on tags}
  @return{A list of @class{gtk-text-tag} objects toggled at this point.}
  @begin{short}
    Returns a list of tags that are toggled on or off at this point.
  @end{short}

  If @arg{toggled-on} is @em{true}, the list contains tags that are toggled on.
  If a tag is toggled on at the iterator, then some non-empty range of
  characters following the iterator has that tag applied to it. If a tag is
  toggled off, then some non-empty range following the iterator does not have
  the tag applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (toggled-on :boolean))

(export 'gtk-text-iter-toggled-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_child_anchor () -> gtk-text-iter-child-anchor
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_child_anchor" gtk-text-iter-child-anchor)
    (g-object gtk-text-child-anchor)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{The @class{gtk-text-child-anchor} object at the iterator.}
  @begin{short}
    If the location at the iterator contains a child anchor, the anchor is
   returned.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-child-anchor}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_tag ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_text_iter_starts_tag" gtk-text-iter-starts-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether the iterator is the start of a range tagged with
    @arg{tag}.}
  @begin{short}
    Returns @em{true} if the tag is toggled on at exactly this point.
  @end{short}
  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled on at
  this point.

  Note that if the function @sym{gtk-text-iter-starts-tag} returns @em{true},
  it means that the iterator is at the beginning of the tagged range, and that
  the character at the iterator is inside the tagged range. In other words,
  unlike the function @fun{gtk-text-iter-ends-tag}, if the function
  @sym{gtk-text-iter-starts-tag} returns @em{true}, the function
  @fun{gtk-text-iter-has-tag} will also return @em{true} for the same
  parameters.

  Since 3.20
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-ends-tag}
  @see-function{gtk-text-iter-has-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

#+gtk-3-20
(export 'gtk-text-iter-starts-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_begins_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_begins_tag" gtk-text-iter-begins-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether the iterator is the start of a range tagged with
    @arg{tag}.}
  @begin{short}
    Returns @em{true} if @arg{tag} is toggled on at exactly this point.
  @end{short}

  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled on at
  this point. Note that the function @sym{gtk-text-iter-begins-tag} returns
  @em{true} if the iterator is the start of the tagged range. The function
  @fun{gtk-text-iter-has-tag} tells you whether an iterator is within a
  tagged range.
  @begin[Warning]{dictionary}
    The function @sym{gtk-text-iter-begins-tag} has been deprecated since
    version 3.20 and should not be used in newly written code. Use the function
    @fun{gtk-text-iter-starts-tag} instead.
  @end{dictionary}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-has-tag}
  @see-function{gtk-text-iter-starts-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-begins-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_tag" gtk-text-iter-ends-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether the iterator is the end of a range tagged with
    @arg{tag}.}
  @begin{short}
    Returns @em{true} if @arg{tag} is toggled off at exactly this point.
  @end{short}

  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled off at
  this point. Note that the function @sym{gtk-text-iter-ends-tag} returns
  @em{true} if the iterator is the end of the tagged range. The function
  @fun{gtk-text-iter-has-tag} tells you whether an iterator is within a tagged
  range.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-has-tag}
  @see-function{gtk-text-iter-starts-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-ends-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_toggles_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_toggles_tag" gtk-text-iter-toggles-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether @arg{tag} is toggled on or off at the iterator.}
  @begin{short}
    Tells you whether a range with @arg{tag} applied to it begins or ends at
    the iterator.
  @end{short}

  This is equivalent to
  @begin{pre}
(or (gtk-text-iter-starts-tag iter tag)
    (gtk-text-iter-ends-tag iter tag))
  @end{pre}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-starts-tag}
  @see-function{gtk-text-iter-ends-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-toggles-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_has_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_has_tag" gtk-text-iter-has-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object}
  @return{A boolean whether the iterator is tagged with @arg{tag}.}
  @begin{short}
    Returns @em{true} if the iterator is within a range tagged with @arg{tag}.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-starts-tag}
  @see-function{gtk-text-iter-ends-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-has-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_tags () -> gtk-text-iter-tags
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_tags" gtk-text-iter-tags)
    (g-slist (g-object gtk-text-tag) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{*2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{List of @class{gtk-text-tag} objects.}
  @begin{short}
    Returns a list of tags that apply to the iterator.
  @end{short}
  The list is in ascending order of priority, highest-priority tags are last.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_editable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_editable" gtk-text-iter-editable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[setting]{@em{true} if text is editable by default}
  @return{A boolean whether the iterator is inside an editable range.}
  @begin{short}
    Returns whether the character at the iterator is within an editable region
    of text.
  @end{short}
  Non-editable text is \"locked\" and cannot be changed by the user via the
  @class{gtk-text-view} widget. This function is simply a convenience wrapper
  around the function @fun{gtk-text-iter-attributes}. If no tags applied to
  this text affect editability, @arg{setting} will be returned.

  You do not want to use this function to decide whether text can be inserted
  at the iterator, because for insertion you do not want to know whether the
  char at the iterator is inside an editable range, you want to know whether a
  new character inserted at the iterator would be inside an editable range. Use
  the function @fun{gtk-text-iter-can-insert} to handle this case.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-view}
  @see-function{gtk-text-iter-attributes}
  @see-function{gtk-text-iter-can-insert}"
  (iter (g-boxed-foreign gtk-text-iter))
  (setting :boolean))

(export 'gtk-text-iter-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_can_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_can_insert" gtk-text-iter-can-insert) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[editabe]{@em{true} if text is editable by default}
  @return{A boolean whether text inserted at the iterator would be editable.}
  @begin{short}
    Considering the default editability of the text buffer, and tags that affect
    editability, determines whether text inserted at the iterator would be
    editable.
  @end{short}

  If text inserted at the iterator would be editable then the user should be
  allowed to insert text at the iterator. The function
  @fun{gtk-text-buffer-insert} with the value @em{true} for the argument
  @arg{interactive} uses this function to decide whether insertions are allowed
  at a given position.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert}"
  (iter (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

(export 'gtk-text-iter-can-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_word ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_starts_word" gtk-text-iter-starts-word) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is at the start of a word.}
  @begin{short}
    Determines whether the iterator begins a natural-language word.
  @end{short}

  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-ends-word}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-starts-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_word ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_word" gtk-text-iter-ends-word) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is at the end of a word.}
  @begin{short}
    Determines whether the iterator ends a natural-language word.
  @end{short}

  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-starts-word}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-ends-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_word ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_inside_word" gtk-text-iter-inside-word) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is inside a word.}
  @begin{short}
    Determines whether the iterator is inside a natural-language word, as
    opposed to say inside some whitespace.
  @end{short}

  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-starts-word}
  @see-function{gtk-text-iter-ends-word}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-inside-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_starts_line" gtk-text-iter-starts-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator begins a line.}
  @begin{short}
    Returns @em{true} if the iterator begins a paragraph, i.e. if the function
    @fun{gtk-text-iter-line-offset} would return 0.
  @end{short}

  However this function is potentially more efficient than the function
  @fun{gtk-text-iter-line-offset} because it does not have to compute the
  offset, it just has to see whether it is 0.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-ends-line}
  @see-function{gtk-text-iter-line-offset}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-starts-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_line" gtk-text-iter-ends-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator is at the end of a line.}
  @begin{short}
    Returns @em{true} if the iterator points to the start of the paragraph
    delimiter characters for a line.
  @end{short}
  Delimiters will be either a newline, a carriage return, a carriage return
  followed by a newline, or a Unicode paragraph separator character.

  Note that an iterator pointing to the @code{\\n} of a @code{\\r\\n} pair will
  not be counted as the end of a line, the line ends before the @code{\\r}. The
  end iterator is considered to be at the end of a line, even though there are
  no paragraph delimiter chars there.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-starts-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-ends-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_sentence ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_starts_sentence" gtk-text-iter-starts-sentence)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is at the start of a sentence.}
  @begin{short}
    Determines whether the iterator begins a sentence.
  @end{short}

  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-ends-sentence}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-starts-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_sentence ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_sentence" gtk-text-iter-ends-sentence) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is at the end of a sentence.}
  @begin{short}
    Determines whether the iterator ends a sentence.
  @end{short}

  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-starts-sentence}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-ends-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_sentence ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_inside_sentence" gtk-text-iter-inside-sentence)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator is inside a sentence.}
  @begin{short}
    Determines whether the iterator is inside a sentence, as opposed to in
    between two sentences, e.g. after a period and before the first letter of
    the next sentence.
  @end{short}

  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-starts-sentence}
  @see-function{gtk-text-iter-ends-sentence}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-inside-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_is_cursor_position" gtk-text-iter-is-cursor-position)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the cursor can be placed at the iterator.}
  @begin{short}
    See the function @fun{gtk-text-iter-forward-cursor-position}, the
    @symbol{pango-log-attr} structure or the function @fun{pango-default-break}
    for details on what a cursor position is.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-symbol{pango-log-attr}
  @see-function{pango-default-break}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-is-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_chars_in_line () -> gtk-text-iter-chars-in-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_chars_in_line" gtk-text-iter-chars-in-line) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{An integer with the number of characters in the line.}
  @begin{short}
    Returns the number of characters in the line containing the iterator,
    including the paragraph delimiters.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-bytes-in-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-chars-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_bytes_in_line () -> gtk-text-iter-bytes-in-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_bytes_in_line" gtk-text-iter-bytes-in-line) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{An integer with the number of bytes in the line.}
  @begin{short}
    Returns the number of bytes in the line containing the iterator, including
    the paragraph delimiters.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-chars-in-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-bytes-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_attributes () -> gtk-text-iter-attributes
;;; ----------------------------------------------------------------------------

;; FIXME: Is this implementation correct? Argument attributes can be modified!

(defcfun ("gtk_text_iter_get_attributes" gtk-text-iter-attributes) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[attributes]{a @class{gtk-text-attributes} instance}
  @return{@em{True} if @arg{attributes} was modified.}
  @begin{short}
    Computes the effect of any tags applied to this spot in the text.
  @end{short}
  The attribute parameter should be initialized to the default settings you
  wish to use if no tags are in effect. You would typically obtain the defaults
  from the function @fun{gtk-text-view-default-attributes}.

  The function @sym{gtk-text-iter-attributes} will modify @arg{attributes},
  applying the effects of any tags present at the iterator. If any tags affected
  @arg{attributes}, the function returns @em{true}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-attributes}
  @see-function{gtk-text-view-default-attributes}"
  (iter (g-boxed-foreign gtk-text-iter))
  (attributes (g-boxed-foreign gtk-text-attributes)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_language () -> gtk-text-iter-language
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_language" gtk-text-iter-language)
    (g-boxed-foreign pango-language)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A @class{pange-language} instance with the language in effect at
    the iterator.}
  @begin{short}
    A convenience wrapper around the function
    @fun{gtk-text-iter-attributes}, which returns the language in effect at
    the iterator.
  @end{short}

  If no tags affecting language apply to the iterator, the return value is
  identical to that of the function @fun{gtk-default-language}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-class{pango-language}
  @see-function{gtk-text-iter-attributes}
  @see-function{gtk-default-language}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-language)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_is_end" gtk-text-iter-is-end) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator is the end iterator.}
  @begin{short}
    Returns @em{true} if the iterator is the end iterator, i.e. one past the
    last dereferenceable iterator in the text buffer.
  @end{short}

  The function @sym{gtk-text-iter-is-end} is the most efficient way to check
  whether an iterator is the end iterator.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-is-start}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-is-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_is_start" gtk-text-iter-is-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-13}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator is the first in the text buffer.}
  @begin{short}
    Returns @em{true} if the iterator is the first iterator in the text buffer,
    that is if the iterator has a character offset of 0.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-is-end}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-is-start)

;;; ----------------------------------------------------------------------------
;;; gtk-text-iter-move
;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-move (iter &key (count 1) (by :char) (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the default value 1}
  @argument[by]{a keyword which determines the operation to perform, the
    default value is @code{:char}}
  @argument[direction]{a keyword for the direction, the default value is
    @code{:forward}}
  @begin{short}
    This is a convenience function of the Lisp implementation, which replaces
    the functions to move the iterator in the text buffer.
  @end{short}

  The following operations are performed depending on the argument @arg{by}:
  @begin[code]{table}
    @begin[:char]{entry}
      Moves @arg{count} characters if possible in the given direction, which
      is @code{:forward} or @code{:backward}. If @arg{count} would move past
      the start or end of the text buffer, moves to the start or end of the
      text buffer.

      The return value indicates whether the new position of the iterator is
      different from its original position, and dereferenceable, the last
      iterator in the text buffer is not dereferenceable. If count is 0, the
      function does nothing and returns @em{false}.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-char}}
        @item{@fun{gtk-text-iter-backward-char}}
        @item{@fun{gtk-text-iter-forward-chars}}
        @item{@fun{gtk-text-iter-backward-chars}}
      @end{itemize}
    @end{entry}
    @begin[:line]{entry}
      Moves the iterator to the start of the next line for the @code{:forward}
      direction or to the start of the previous line for the @code{:backward}
      direction.

      If the the iterator is already on the last line of the text buffer for
      a @code{:forward} direction, moves the iterator to the end of the current
      line. If after the operation, the iterator is at the end of the text
      buffer and not dereferencable, returns @em{false}. Otherwise, returns
      @em{true}.

      For the @code{:backward} direction returns @em{true} if the iterator could
      be moved, i.e. if the iterator was at character offset 0, this function
      returns @em{false}. Therefore if the iterator was already on line 0, but
      not at the start of the line, the iterator is snapped to the start of the
      line and the function returns @em{true}. Note that this implies that in a
      loop calling this function, the line number may not change on every
      iteration, if your first iteration is on line 0.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-line}}
        @item{@fun{gtk-text-iter-backward-line}}
        @item{@fun{gtk-text-iter-forward-lines}}
        @item{@fun{gtk-text-iter-backward-lines}}
      @end{itemize}
    @end{entry}
    @begin[:word]{entry}
      Moves forward up to @arg{count} times for the @code{:forward} direction
      to the next word end. If the iterator is currently on a word end, moves
      forward to the next one after that.

      Moves backward up to @arg{count} times for the @code{:backward} direction
      to the previous word start. If the iterator is currently on a word start,
      moves backward to the next one after that.

      Word breaks are determined by Pango and should be correct for nearly any
      language, if not, the correct fix would be to the Pango word break
      algorithms.

      Returns @em{true} if the iterator moved and is not the end iterator.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-word-end}}
        @item{@fun{gtk-text-iter-backward-word-start}}
        @item{@fun{gtk-text-iter-forward-word-ends}}
        @item{@fun{gtk-text-iter-backward-word-starts}}
      @end{itemize}
    @end{entry}
    @begin[:cursor-position]{entry}
      Moves the iterator up to @arg{count} cursor positions forward or backward.

      Cursor positions are (unsurprisingly) positions where the cursor can
      appear. Perhaps surprisingly, there may not be a cursor position between
      all characters. The most common example for European languages would be
      a carriage return/newline sequence. For some Unicode characters, the
      equivalent of say the letter \"a\" with an accent mark will be represented
      as two characters, first the letter then a \"combining mark\" that causes
      the accent to be rendered. So the cursor cannot go between those two
      characters. See also the @class{pango-log-attr} structure and the function
      @fun{pango-default-break}.

      Returns @em{true} if we moved and the new position is dereferenceable.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-cursor-position}}
        @item{@fun{gtk-text-iter-backward-cursor-position}}
        @item{@fun{gtk-text-iter-forward-cursor-positions}}
        @item{@fun{gtk-text-iter-backward-cursor-positions}}
      @end{itemize}
    @end{entry}
    @begin[:sentence]{entry}
      Moves backward to the previous sentence start or forward to the next
      sentence end. If the iterator is already at the start of a sentence, moves
      backward to the next one. If the iterator is at the end of a sentence,
      moves to the next end of sentence.

      Sentence boundaries are determined by Pango and should be correct for
      nearly any language, if not, the correct fix would be to the Pango text
      boundary algorithms.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-sentence-end}}
        @item{@fun{gtk-text-iter-backward-sentence-start}}
        @item{@fun{gtk-text-iter-forward-sentences-ends}}
        @item{@fun{gtk-text-iter-backward-sentence-starts}}
      @end{itemize}
    @end{entry}
    @begin[:visible-word]{entry}
      Moves forward to the next visible word end or backward to the previous
      visible word start.

      If the iterator is currently on a word start, moves backward to the next
      one after that. Word breaks are determined by Pango and should be correct
      for nearly any language. If not, the correct fix would be to the Pango
      word break algorithms.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-visible-word-end}}
        @item{@fun{gtk-text-iter-backward-visible-word-start}}
        @item{@fun{gtk-text-iter-forward-visible-word-ends}}
        @item{@fun{gtk-text-iter-backward-visible-word-starts}}
      @end{itemize}
    @end{entry}
    @begin[:visible-line]{entry}
      Moves the iterator to the start of the next visible line or to the start
      of the previous visible line.

      The return value indicates whether the iterator moved onto a
      dereferenceable position. If the iterator did not move, or moved onto the
      end iterator, then @em{false} is returned. If @arg{count} is 0, the
      function does nothing and returns @em{false}.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-visible-line}}
        @item{@fun{gtk-text-iter-backward-visible-line}}
        @item{@fun{gtk-text-iter-forward-visible-lines}}
        @item{@fun{gtk-text-iter-backward-visible-lines}}
      @end{itemize}
    @end{entry}
    @begin[:visible-cursor-position]{entry}
      Moves the iterator forward to the next visible cursor position or forward
      to the previous visible cursor position.

      Returns @em{true} if we moved and the new position is dereferenceable.

      This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk-text-iter-forward-visible-cursor-position}}
        @item{@fun{gtk-text-iter-backward-visible-cursor-position}}
        @item{@fun{gtk-text-iter-forward-visible-cursor-positions}}
        @item{@fun{gtk-text-iter-backward-visible-cursor-positions}}
      @end{itemize}
    @end{entry}
  @end{table}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (assert (typep by '(member :char :line :word :cursor-position :sentence
                             :visible-word :visible-line
                             :visible-cursor-position)))
  (assert (typep direction '(member :forward :backward)))
  (when (and (member by '(:char :line :cursor-position :visible-line
                           :visible-cursor-position))
             (eq direction :backward))
    (setf count (- count)))
  (ecase by
    (:char
     (gtk-text-iter-forward-chars iter count))
    (:line
     (gtk-text-iter-forward-lines iter count))
    (:word
     (if (eq direction :forward)
         (gtk-text-iter-forward-word-ends iter count)
         (gtk-text-iter-backward-word-starts iter count)))
    (:cursor-position
     (gtk-text-iter-forward-cursor-positions iter count))
    (:sentence
     (if (eq direction :forward)
         (gtk-text-iter-forward-sentence-ends iter count)
         (gtk-text-iter-backward-sentence-starts iter count)))
    (:visible-word
     (if (eq direction :forward)
         (gtk-text-iter-forward-visible-word-ends iter count)
         (gtk-text-iter-backward-visible-word-starts iter count)))
    (:visible-line
     (gtk-text-iter-forward-visible-lines iter count))
    (:visible-cursor-position
     (gtk-text-iter-forward-visible-cursor-positions iter count))))

(export 'gtk-text-iter-move)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_char" gtk-text-iter-forward-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves the iterator forward by one character offset.
  @end{short}

  Note that images embedded in the text buffer occupy 1 character slot, so the
  function @sym{gtk-text-iter-forward-char} may actually move onto an image
  instead of a character, if you have images in your text buffer. If the
  iterator is the end iterator or one character before it, the iterator will now
  point at the end iterator, and the function @sym{gtk-text-iter-forward-char}
  returns @em{false} for convenience when writing loops.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_char" gtk-text-iter-backward-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether movement was possible.}
  @begin{short}
    Moves backward by one character offset.
  @end{short}

  Returns @em{true} if movement was possible. If the iterator was the first in
  the text buffer, character offset 0, the function
  @sym{gtk-text-iter-backward-char} returns @em{false} for convenience when
  writing loops.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_chars ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_chars" gtk-text-iter-forward-chars) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of characters to move, may be
    negative}
  @return{Whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} characters if possible.
  @end{short}
  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer.

  The return value indicates whether the new position of the iterator is
  different from its original position, and dereferenceable, the last iterator
  in the text buffer is not dereferenceable. If @arg{count} is 0, the function
  does nothing and returns @em{false}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-chars}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_chars ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_chars" gtk-text-iter-backward-chars) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with a number of characters to move}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves count characters backward if possible.
  @end{short}
  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer.

  The return value indicates whether the iterator moved onto a dereferenceable
  position. If the iterator did not move, or moved onto the end iterator, then
  @em{false} is returned. If count is 0, the function does nothing and returns
  @em{false}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-chars}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_line" gtk-text-iter-forward-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator can be dereferenced.}
  @begin{short}
    Moves the iterator to the start of the next line.
  @end{short}

  If the the iterator is already on the last line of the text buffer, moves the
  iterator to the end of the current line. If after the operation, the
  iterator is at the end of the text buffer and not dereferencable, returns
  @em{false}. Otherwise, returns @em{true}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_line" gtk-text-iter-backward-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator moved.}
  @begin{short}
    Moves the iterator to the start of the previous line.
  @end{short}

  Returns @em{true} if the iterator could be moved. I.e. if the iterator was
  at character offset 0, this function returns @em{false}. Therefore if the
  iterator was already on line 0, but not at the start of the line, the iterator
  is snapped to the start of the line and the function returns @em{true}. Note
  that this implies that in a loop calling this function, the line number may
  not change on every iteration, if your first iteration is on line 0.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_lines" gtk-text-iter-forward-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of lines to move forward}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} lines forward, if possible.
  @end{short}
  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer.

  The return value indicates whether the iterator moved onto a dereferenceable
  position. If the iterator did not move, or moved onto the end iterator, then
  @em{false} is returned. If @arg{count} is 0, the function does nothing and
  returns @em{false}. If @arg{count} is negative, moves backward by
  @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_lines" gtk-text-iter-backward-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of lines to move backward}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} lines backward, if possible.
  @end{short}
  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer.

  The return value indicates whether the iterator moved onto a dereferenceable
  position. If the iterator did not move, or moved onto the end iterator, then
  @em{false} is returned. If @arg{count} is 0, the function does nothing and
  returns @em{false}. If @arg{count} is negative, moves forward by
  @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_word_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_word_ends" gtk-text-iter-forward-word-ends)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of times to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-forward-word-end} up to @arg{count}
    times.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-word-end}
  @see-function{gtk-text-iter-backward-word-starts}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-word-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_word_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_word_starts"
          gtk-text-iter-backward-word-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of times to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-backward-word-start} up to @arg{count}
    times.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-word-start}
  @see-function{gtk-text-iter-forward-word-ends}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-word-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_word_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_word_end" gtk-text-iter-forward-word-end)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next word end.
  @end{short}
  If the iterator is currently on a word end, moves forward to the next one
  after that.

  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-word-start}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-word-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_word_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_word_start" gtk-text-iter-backward-word-start)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous word start.
  @end{short}
  If the iterator is currently on a word start, moves backward to the next one
  after that.

  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-end}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-word-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_cursor_position"
           gtk-text-iter-forward-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves the iterator forward by a single cursor position.
  @end{short}

  Cursor positions are (unsurprisingly) positions where the cursor can appear.
  Perhaps surprisingly, there may not be a cursor position between all
  characters. The most common example for European languages would be a carriage
  return/newline sequence. For some Unicode characters, the equivalent of say
  the letter \"a\" with an accent mark will be represented as two characters,
  first the letter then a \"combining mark\" that causes the accent to be
  rendered. So the cursor cannot go between those two characters. See also the
  @symbol{pango-log-attr} structure and the function @fun{pango-default-break}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-symbol{pango-log-attr}
  @see-function{pango-default-break}
  @see-function{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_cursor_position"
           gtk-text-iter-backward-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if we moved.}
  @begin{short}
    Like the function @fun{gtk-text-iter-forward-cursor-position}, but moves
    backward.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_cursor_positions"
          gtk-text-iter-forward-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-cursor-position}
  @see-function{gtk-text-iter-backward-cursor-positions}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_cursor_positions"
           gtk-text-iter-backward-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-cursor-position}
  @see-function{gtk-text-iter-forward-cursor-positions}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_sentence_start"
           gtk-text-iter-backward-sentence-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous sentence start.
  @end{short}
  If the iterator is already at the start of a sentence, moves backward to the
  next one.

  Sentence boundaries are determined by Pango and should be correct for nearly
  any language. If not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-sentence-starts}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-sentence-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_sentence_starts"
          gtk-text-iter-backward-sentence-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of sentences to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-backward-sentence-start} up to
    @arg{count} times, or until it returns @em{false}.
  @end{short}
  If @arg{count} is negative, moves forward instead of backward.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-sentence-start}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-sentence-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_sentence_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_sentence_end"
           gtk-text-iter-forward-sentence-end) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next sentence end.
  @end{short}
  If the iterator is at the end of a sentence, moves to the next end of
  sentence.

  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-sentence-ends}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-sentence-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_sentence_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_sentence_ends"
          gtk-text-iter-forward-sentence-ends) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of sentences to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-forward-sentence-end} @arg{count}
    times, or until the function @fun{gtk-text-iter-forward-sentence-end}
    returns @em{false}.
  @end{short}
  If @arg{count} is negative, moves backward instead of forward.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-sentence-end}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-sentence-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_word_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_word_ends"
          gtk-text-iter-forward-visible-word-ends) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of times to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-forward-visible-word-end} up to
    @arg{count} times.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-visible-word-end}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-word-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_word_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_word_starts"
          gtk-text-iter-backward-visible-word-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of times to move}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-backward-visible-word-start} up to
    @arg{count} times.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-visible-word-start}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-word-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_word_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_word_end"
           gtk-text-iter-forward-visible-word-end) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next visible word end.
  @end{short}

  If the iterator is currently on a word end, moves forward to the next one
  after that. Word breaks are determined by Pango and should be correct for
  nearly any language. If not, the correct fix would be to the Pango word break
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-visible-word-start}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-word-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_word_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_word_start"
           gtk-text-iter-backward-visible-word-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if the iterator moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous visible word start.
  @end{short}

  If the iterator is currently on a word start, moves backward to the next one
  after that. Word breaks are determined by Pango and should be correct for
  nearly any language. If not, the correct fix would be to the Pango word break
  algorithms.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-visible-word-end}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-word-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_cursor_position"
           gtk-text-iter-forward-visible-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves the iterator forward to the next visible cursor position.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_cursor_position"
           gtk-text-iter-backward-visible-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves the iterator forward to the previous visible cursor position.
  @end{short}
  See the function @fun{gtk-text-iter-backward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_cursor_positions"
          gtk-text-iter-forward-visible-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} visible cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_cursor_positions"
           gtk-text-iter-backward-visible-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} visible cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-backward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_line"
           gtk-text-iter-forward-visible-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator can be dereferenced.}
  @begin{short}
    Moves the iterator to the start of the next visible line.
  @end{short}
  Returns @em{true} if there was a next line to move to, and @em{false} if the
  iterator was simply moved to the end of the text buffer and is now not
  dereferenceable, or if the iterator was already at the end of the text buffer.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-visible-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_line"
           gtk-text-iter-backward-visible-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{A boolean whether the iterator moved.}
  @begin{short}
    Moves the iterator to the start of the previous visible line.
  @end{short}

  Returns @em{true} if the iterator could be moved. I.e. if the iterator was at
  character offset 0, this function returns @em{false}. Therefore if the
  iterator was already on line 0, but not at the start of the line, the iterator
  is snapped to the start of the line and the function returns @em{true}. Note
  that this implies that in a loop calling this function, the line number may
  not change on every iteration, if your first iteration is on line 0.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-visible-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_lines"
          gtk-text-iter-forward-visible-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of lines to move forward}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} visible lines forward, if possible.
  @end{short}
  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer.

  The return value indicates whether the iterator moved onto a dereferenceable
  position. if the iterator did not move, or moved onto the end iterator, then
  @em{false} is returned. If @arg{count} is 0, the function does nothing and
  returns @em{false}. If @arg{count} is negative, moves backward by @arg{count}
  lines.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-visible-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_lines"
           gtk-text-iter-backward-visible-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[count]{an integer with the number of lines to move backward}
  @return{A boolean whether the iterator moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} visible lines backward, if possible.
  @end{short}

  If @arg{count} would move past the start or end of the text buffer, moves to
  the start or end of the text buffer. The return value indicates whether the
  iterator moved onto a dereferenceable position; if the iterator did not move,
  or moved onto the end iterator, then @em{false} is returned. If @arg{count}
  is 0, the function does nothing and returns @em{false}. If @arg{count} is
  negative, moves forward by @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-visible-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_end" gtk-text-iter-forward-to-end) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @begin{short}
    Moves the iterator forward to the \"end iterator\", which points one past
    the last valid character in the text buffer.
  @end{short}

  The function @fun{gtk-text-iter-char} called on the end iterator returns
  0, which is convenient for writing loops.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_line_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_line_end" gtk-text-iter-forward-to-line-end)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @return{@em{True} if we moved and the new location is not the end iterator.}
  @begin{short}
    Moves the iterator to point to the paragraph delimiter characters, which
    will be either a newline, a carriage return, a carriage return/newline in
    sequence, or the Unicode paragraph separator character.
  @end{short}

  If the iterator is already at the paragraph delimiter characters, moves to the
  paragraph delimiter characters for the next line. If the iterator is on the
  last line in the text buffer, which does not end in paragraph delimiters,
  moves to the end iterator (end of the last line), and returns @em{false}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_tag_toggle"
          gtk-text-iter-forward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether we found a tag toggle after the iterator.}
  @begin{short}
    Moves forward to the next toggle (on or off) of @arg{tag}, or to the next
    toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}

  If no matching tag toggles are found, returns @em{false}, otherwise @em{true}.
  Does not return toggles located at the iterator, only toggles after the
  iterator. Sets the iterator to the location of the toggle, or to the end of
  the text buffer if no toggle is found.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-forward-to-tag-toggle)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_to_tag_toggle"
          gtk-text-iter-backward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{A boolean whether we found a tag toggle before the iterator.}
  @begin{short}
    Moves backward to the next toggle (on or off) of the @arg{tag}, or to the
    next toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}

  If no matching tag toggles are found, returns @em{false}, otherwise @em{true}.
  Does not return toggles located at the iterator, only toggles before the
  iterator. Sets the iterator to the location of the toggle, or the start of
  the text buffer if no toggle is found.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export '(gtk-text-iter-backward-to-tag-toggle))

;;; ----------------------------------------------------------------------------
;;; GtkTextCharPredicate ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-char-predicate :boolean
    ((char unichar)
     (user-data :pointer))
  (let ((func (get-stable-pointer-value user-data)))
    (funcall func char)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-char-predicate atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-text-char-predicate atdoc:*external-symbols*)
 "@version{2021-6-15}
  @begin{short}
    A callback function used by the function @fun{gtk-text-iter-find-char} to
    search a char in the text buffer.
  @end{short}
  @begin{pre}
 lambda (ch)
  @end{pre}
  @begin[code]{table}
    @entry[ch]{A Unichar character.}
    @entry[Returns]{@em{True} if the character was found.}
  @end{table}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-find-char}")

(export 'gtk-text-char-predicate)

;;; ----------------------------------------------------------------------------
;;; gtk-text-iter-find-char
;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-find-char (iter predicate &key limit (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[pred]{a @sym{gtk-text-char-predicate} callback function to be
    called on each character}
  @argument[limit]{a @class{gtk-text-iter} instance with a search limit, or
    @code{nil} for none}
  @argument[direction]{the value @code{:forward} indicates forward search and
    the value @code{:backward} backward search}
  @return{A boolean whether a match was found.}
  @begin{short}
    This is a convenience function of the Lisp implementation which combines
    the functions @fun{gtk-text-iter-forward-find-char} and
  @fun{gtk-text-iter-backward-find-char} into one single function.
  @end{short}

  The direction of the search is indicated with the keyword argument
  @arg{direction} which has a default value of @code{:forward} for forward
  search. For backward search the argument @arg{direction} takes the value
  @code{:backward}.

  In addition the argument @arg{limit} is a keyword argument with the default
  value @code{nil}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-find-char}
  @see-function{gtk-text-iter-backward-find-char}"
  (assert (typep direction '(member :forward :backward)))
  (with-stable-pointer (ptr predicate)
    (if (eq direction :forward)
        (%gtk-text-iter-forward-find-char iter
                                          (callback gtk-text-char-predicate)
                                          ptr
                                          limit)
        (%gtk-text-iter-backward-find-char iter
                                           (callback gtk-text-char-predicate)
                                           ptr
                                           limit))))

(export 'gtk-text-iter-find-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_find_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_find_char"
          %gtk-text-iter-forward-find-char) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-forward-find-char (iter pred limit)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[pred]{a @sym{gtk-text-char-predicate} callback function to be
    called on each character}
  @argument[limit]{a @class{gtk-text-iter} instance with the search limit, or
    @code{nil} for none}
  @return{A boolean whether a match was found.}
  @begin{short}
    Advances the iterator, calling the function @arg{pred} on each character.
  @end{short}
  If the function @arg{pred} returns @em{true}, returns @em{true} and stops
  scanning. If the function @arg{pred} never returns @em{true}, the iterator is
  set to @arg{limit} if @arg{limit} is non-@code{nil}, otherwise to the end
  iterator.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-backward-find-char}"
  (gtk-text-iter-find-char iter
                           pred
                           :limit limit
                           :direction :forward))

(export 'gtk-text-iter-forward-find-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_find_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_find_char" %gtk-text-iter-backward-find-char)
    :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-backward-find-char (iter pred limit)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[pred]{a @sym{gtk-text-char-predicate} callback function to be
    called on each character}
  @argument[limit]{a @class{gtk-text-iter} instance with a search limit, or
    @code{nil} for none}
  @return{A boolean whether a match was found.}
  @begin{short}
    Same as the function @fun{gtk-text-iter-forward-find-char}, but goes
    backward from the iterator.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-forward-find-char}"
  (gtk-text-iter-find-char iter
                           pred
                           :limit limit
                           :direction :backward))

(export 'gtk-text-iter-backward-find-char)

;;; ----------------------------------------------------------------------------
;;; gtk-text-iter-search
;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-search (iter str &key flags limit (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} with the start of search}
  @argument[str]{a search string}
  @argument[flags]{the @symbol{gtk-text-search-flags} flags affecting how
    the search is done}
  @argument[limit]{a @class{gtk-text-iter} instance with the bound for the
    search, or @code{nil} for the end of the text buffer}
  @argument[direction]{the value @code{:forward} indicates forward search and
    the value @code{:backward} backward search}
  @begin{return}
    @code{search-p} -- a boolean whether a match was found @br{}
    @code{match-start} -- a @class{gtk-text-iter} instance with the start of
      match, or @code{nil} @br{}
    @code{match-end} -- a @class{gtk-text-iter} instance with the end of match,
      or @code{nil}
  @end{return}
  @begin{short}
    This is a convenience function of the Lisp implementation which combines
    the functions @fun{gtk-text-iter-forward-search} and
    @fun{gtk-text-iter-backward-search} into one single function.
  @end{short}

  The direction of the search is indicated with the keyword argument
  @arg{direction} which has a default value of @code{:forward} for forward
  search. For backward search the @arg{direction} takes the value
  @code{:backward}.

  In addition the arguments @arg{flags} and @arg{limit} are keyword arguments
  with a default value @code{nil}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-symbol{gtk-text-search-flags}
  @see-function{gtk-text-iter-forward-search}
  @see-function{gtk-text-iter-backward-search}"
  (assert (typep direction '(member :forward :backward)))
  (let ((match-start (make-instance 'gtk-text-iter))
        (match-end (make-instance 'gtk-text-iter)))
    (if (if (eq direction :forward)
            (%gtk-text-iter-forward-search iter
                                           str
                                           flags
                                           match-start
                                           match-end
                                           limit)
            (%gtk-text-iter-backward-search iter
                                            str
                                            flags
                                            match-start
                                            match-end
                                            limit))
        (values t match-start match-end)
        (values nil nil nil))))

(export 'gtk-text-iter-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_search ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_search" %gtk-text-iter-forward-search) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (str (:string :free-to-foreign t))
  (flags gtk-text-search-flags)
  (match-start (g-boxed-foreign gtk-text-iter))
  (match-end (g-boxed-foreign gtk-text-iter))
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-forward-search (iter str flags limit)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance with the start of search}
  @argument[str]{a search string}
  @argument[flags]{the @symbol{gtk-text-search-flags} flags affecting how
    the search is done}
  @argument[limit]{a @class{gtk-text-iter} instance which is the bound for the
    search, or @code{nil} for the end of the text buffer}
  @begin{return}
    @code{search-p} -- a boolean whether a match was found @br{}
    @code{match-start} -- a @class{gtk-text-iter} instance with the start of
      match, or @code{nil} @br{}
    @code{match-end} -- a @class{gtk-text-iter} instance with the end of match,
      or @code{nil}
  @end{return}
  @begin{short}
    Searches forward for @arg{str}.
  @end{short}
  Any match is returned by setting @arg{match-start} to the first character of
  the match and @arg{match-end} to the first character after the match. The
  search will not continue past @arg{limit}. Note that a search is a linear or
  O(n) operation, so you may wish to use @arg{limit} to avoid locking up your
  UI on large text buffers.

  If the @code{:visible-only} flag is present, the match may have
  invisible text interspersed in @arg{str}, i.e. @arg{str} will be a
  possibly-noncontiguous subsequence of the matched range. Similarly, if you
  specify @code{:text-only}, the match may have pixbufs or child widgets mixed
  inside the matched range. If these flags are not given, the match must be
  exact; the special @code{0xFFFC} character in @arg{str} will match embedded
  pixbufs or child widgets. If you specify the @code{:case-insensitive} flag,
  the text will be matched regardless of what case it is in.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-symbol{gtk-text-search-flags}
  @see-function{gtk-text-iter-search}
  @see-function{gtk-text-iter-backward-search}"
  (gtk-text-iter-search iter
                        str
                        :flags flags
                        :limit limit
                        :direction :forward))

(export 'gtk-text-iter-forward-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_search ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_search" %gtk-text-iter-backward-search)
    :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (str (:string :free-to-foreign t))
  (flags gtk-text-search-flags)
  (match-start (g-boxed-foreign gtk-text-iter))
  (match-end (g-boxed-foreign gtk-text-iter))
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-backward-search (iter str flags limit)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance where the search begins}
  @argument[str]{a search string}
  @argument[flags]{the @symbol{gtk-text-search-flags} flags affecting the
    search}
  @argument[limit]{a @class{gtk-text-iter} instance with the location of last
    possible @arg{match-start}, or @code{nil} for start of the text buffer}
  @begin{return}
    @code{search-p} -- a boolean whether a match was found @br{}
    @code{match-start} -- a @class{gtk-text-iter} instance with the start of
      match, or @code{nil} @br{}
    @code{match-end} -- a @class{gtk-text-iter} instance with the end of match,
      or @code{nil}
  @end{return}
  @begin{short}
    Same as the function @fun{gtk-text-iter-forward-search}, but moves backward.
  @end{short}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-symbol{gtk-text-search-flags}
  @see-function{gtk-text-iter-search}
  @see-function{gtk-text-iter-forward-search}"
  (gtk-text-iter-search iter
                        str
                        :flags flags
                        :limit limit
                        :direction :backward))

(export 'gtk-text-iter-backward-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_equal" gtk-text-iter-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[lhs]{a @class{gtk-text-iter} instance}
  @argument[rhs]{another @class{gtk-text-iter} instance}
  @return{@em{True} if the iterators point to the same place in the text
    buffer.}
  @begin{short}
    Tests whether two iterators are equal, using the fastest possible mechanism.
  @end{short}

  This function is very fast. You can expect it to perform better than e.g.
  getting the character offset for each iterator and comparing the offsets
  yourself. Also, it is a bit faster than the function
  @fun{gtk-text-iter-compare}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-compare}"
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_compare" gtk-text-iter-compare) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[lhs]{a @class{gtk-text-iter} instance}
  @argument[rhs]{another @class{gtk-text-iter} instance}
  @return{-1 if @arg{lhs} is less than @arg{rhs}, 1 if @arg{lhs} is greater, 0
    if they are equal.}
  @begin{short}
    A @code{qsort()}-style function that returns negative if @arg{lhs} is less
    than @arg{rhs}, positive if @arg{lhs} is greater than @arg{rhs}, and 0 if
    they are equal.
  @end{short}
  Ordering is in character offset order, i.e. the first character in
  the text buffer is less than the second character in the text buffer.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-equal}"
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_in_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_in_range" gtk-text-iter-in-range) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[iter]{a @class{gtk-text-iter} instance}
  @argument[start]{a @class{gtk-text-iter} instance with the start of range}
  @argument[end]{a @class{gtk-text-iter} instance with the end of range}
  @return{@em{True} if the iterator is in the range.}
  @begin{short}
    Checks whether the iterator falls in the range [@arg{start}, @arg{end}).
  @end{short}
  @arg{start} and @arg{end} must be in ascending order.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_order" gtk-text-iter-order) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-15}
  @argument[first]{a @class{gtk-text-iter} instance}
  @argument[second]{another @class{gtk-text-iter} instance}
  @begin{short}
    Swaps the value of @arg{first} and @arg{second} if @arg{second} comes
    before @arg{first} in the text buffer.
  @end{short}

  That is, ensures that @arg{first} and @arg{second} are in sequence. Most text
  buffer functions that take a range call this automatically on your behalf, so
  there is no real reason to call it yourself in those cases. There are some
  exceptions, such as the function @fun{gtk-text-iter-in-range}, that expect a
  pre-sorted range.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-in-range}"
  (iter-1 (g-boxed-foreign gtk-text-iter))
  (iter-2 (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-order)

;;; --- End of file gtk.text-iter.lisp -----------------------------------------
