;;; ----------------------------------------------------------------------------
;;; gtk.text-iter.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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

(glib-init::at-init () (foreign-funcall "gtk_text_iter_get_type" :int))

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

(defun gtk-text-iter-move (iter &key (count 1) (by :char) (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  This is a convenience function of the Lisp implementation which combines
  the following functions in a single function:
  @fun{gtk-text-iter-forward-chars},
  @fun{gtk-text-iter-forward-lines},
  @fun{gtk-text-iter-forward-word-ends},
  @fun{gtk-text-iter-backward-word-starts},
  @fun{gtk-text-iter-forward-cursor-positions},
  @fun{gtk-text-iter-forward-sentence-ends},
  @fun{gtk-text-iter-backward-sentence-starts},
  @fun{gtk-text-iter-forward-visible-word-ends},
  @fun{gtk-text-iter-backward-visible-word-starts},
  @fun{gtk-text-iter-forward-visible-lines}, and
  @fun{gtk-text-iter-forward-visible-cursor-positions}
  @see-function{gtk-text-iter-forward-chars}
  @see-function{gtk-text-iter-forward-lines}
  @see-function{gtk-text-iter-forward-word-ends}
  @see-function{gtk-text-iter-backward-word-starts}
  @see-function{gtk-text-iter-forward-cursor-positions}
  @see-function{gtk-text-iter-forward-sentence-ends}
  @see-function{gtk-text-iter-backward-sentence-starts}
  @see-function{gtk-text-iter-forward-visible-word-ends}
  @see-function{gtk-text-iter-backward-visible-word-starts}
  @see-function{gtk-text-iter-forward-visible-lines}
  @see-function{gtk-text-iter-forward-visible-cursor-positions}"
  (assert (typep by '(member :char :line :word :cursor-position :sentence
                             :visible-word :visible-line
                             :visible-cursor-position)))
  (assert (typep direction '(member :forward :backward)))
  (when (and (member by '(:char :ine :cursor-position :visible-line
                           :visible-cursor-position))
             (eq direction :backward))
    (setf count (- count)))
  (ecase by
    (:char (gtk-text-iter-forward-chars iter count))
    (:line (gtk-text-iter-forward-lines iter count))
    (:word (if (eq direction :forward)
               (gtk-text-iter-forward-word-ends iter count)
               (gtk-text-iter-backward-word-starts iter count)))
    (:cursor-position (gtk-text-iter-forward-cursor-positions iter count))
    (:sentence (if (eq direction :forward)
                   (gtk-text-iter-forward-sentence-ends iter count)
                   (gtk-text-iter-backward-sentence-starts iter count)))
    (:visible-word (if (eq direction :forward)
                       (gtk-text-iter-forward-visible-word-ends iter count)
                       (gtk-text-iter-backward-visible-word-starts iter count)))
    (:visible-line (gtk-text-iter-forward-visible-lines iter count))
    (:visible-cursor-position
     (gtk-text-iter-forward-visible-cursor-positions iter count))))

(export 'gtk-text-iter-move)

;;; ----------------------------------------------------------------------------
;;; GtkTextIter
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gtk-text-iter "GtkTextIter"
  :alloc (gtk-text-iter-alloc))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-iter atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-text-iter 'type)
 "@version{2016-1-30}
  @begin{short}
    Most text manipulation is accomplished with iterators, represented by a
    @sym{gtk-text-iter}. An iterator represents a position between two
    characters in the text buffer.
  @end{short}
  @sym{gtk-text-iter} is a structure designed to be allocated on the stack; it
  is guaranteed to be copiable by value and never contain any heap-allocated
  data. Iterators are not valid indefinitely; whenever the buffer is modified
  in a way that affects the number of characters in the buffer, all outstanding
  iterators become invalid. Note that deleting 5 characters and then reinserting
  5 still invalidates iterators, though you end up with the same number of
  characters you pass through a state with a different number.")

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

(defun gtk-text-iter-alloc ()
  (with-foreign-object (iter '(:struct %gtk-text-iter))
    (%gtk-text-iter-copy iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_buffer ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-buffer
  :reader "gtk_text_iter_get_buffer"
  :type (g-object gtk-text-buffer))

(declaim (inline gtk-text-iter-get-buffer))

(defun gtk-text-iter-get-buffer (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{The buffer.}
  Returns the @class{gtk-text-buffer} this iterator is associated with.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}"
  (gtk-text-iter-buffer iter))

(export 'gtk-text-iter-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_copy ()
;;; ----------------------------------------------------------------------------

;; Only for internal use and not exported.

(defcfun ("gtk_text_iter_copy" %gtk-text-iter-copy) :pointer
  (iter :pointer))

(defcfun ("gtk_text_iter_copy" gtk-text-iter-copy)
    (g-boxed-foreign gtk-text-iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{A copy of the @arg{iter}.}
  Creates a dynamically-allocated copy of an iterator. The function is used by
  language bindings."
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_assign ()
;;;
;;; void gtk_text_iter_assign (GtkTextIter *iter, const GtkTextIter *other);
;;;
;;; Assigns the value of other to iter. This function is not useful in
;;; applications, because iterators can be assigned with GtkTextIter i = j;. The
;;; function is used by language bindings.
;;;
;;; iter :
;;;     a GtkTextIter
;;;
;;; other :
;;;     another GtkTextIter
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

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
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-offset
  :reader "gtk_text_iter_get_offset"
  :writer "gtk_text_iter_set_offset"
  :type :int)

(declaim (inline gtk-text-iter-get-offset))

(defun gtk-text-iter-get-offset (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{A character offset.}
  Returns the character offset of an iterator. Each character in a
  @class{gtk-text-buffer} object has an offset, starting with 0 for the first
  character in the buffer. Use the function
  @fun{gtk-text-buffer-get-iter-at-offset} to convert an offset back into an
  iterator.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-set-offset}
  @see-function{gtk-text-buffer-get-iter-at-offset}"
  (gtk-text-iter-offset iter))

(export 'gtk-text-iter-get-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-line
  :reader "gtk_text_iter_get_line"
  :writer "gtk_text_iter_set_line"
  :type :int)

(declaim (inline gtk-text-iter-get-line))

(defun gtk-text-iter-get-line (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{A line number.}
  Returns the line number containing the iterator. Lines in a
  @class{gtk-text-buffer} object are numbered beginning with 0 for the first
  line in the buffer.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-set-line}"
  (gtk-text-iter-line iter))

(export 'gtk-text-iter-get-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_offset ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-line-offset
  :reader "gtk_text_iter_get_line_offset"
  :writer "gtk_text_iter_set_line_offset"
  :type :int)

(declaim (inline gtk-text-iter-get-line-offset))

(defun gtk-text-iter-get-line-offset (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Offset from start of line.}
  Returns the character offset of the iterator, counting from the start of a
  newline-terminated line. The first character on the line has offset 0.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-set-line-offset}"
  (gtk-text-iter-line-offset iter))

(export 'gtk-text-iter-get-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_index ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-line-index
  :reader "gtk_text_iter_get_line_index"
  :writer "gtk_text_iter_set_line_index"
  :type :int)

(defun gtk-text-iter-get-line-index (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Distance from start of line, in bytes.}
  Returns the byte index of the iterator, counting from the start of a
  newline-terminated line. Remember that @class{gtk-text-buffer} encodes text
  in UTF-8, and that characters can require a variable number of bytes to
  represent.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-iter-set-line-index}"
  (gtk-text-iter-line-index iter))

(export 'gtk-text-iter-get-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_line_index ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-visible-line-index
  :reader "gtk_text_iter_get_visible_line_index"
  :writer "gtk_text_iter_set_visible_line_index"
  :type :int)

(defun gtk-text-iter-get-visible-line-index (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Byte index of @arg{iter} with respect to the start of the line.}
  Returns the number of bytes from the start of the line to the given
  @arg{iter}, not counting bytes that are invisible due to tags with the
  \"invisible\" flag toggled on.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-set-visible-line-index}"
  (gtk-text-iter-visible-line-index iter))

(export 'gtk-text-iter-get-visible-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_line_offset ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-visible-line-offset
  :reader "gtk_text_iter_get_visible_line_offset"
  :writer "gtk_text_iter_set_visible_line_offset"
  :type :int)

(declaim (inline gtk-text-iter-get-visible-line-offset))

(defun gtk-text-iter-get-visible-line-offset (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{Offset in visible characters from the start of the line.}
  Returns the offset in characters from the start of the line to the given
  @arg{iter}, not counting characters that are invisible due to tags with the
  \"invisible\" flag toggled on.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-set-visible-line-offset}"
  (gtk-text-iter-visible-line-offset iter))

(export 'gtk-text-iter-get-visible-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_char ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-char
  :reader "gtk_text_iter_get_char"
  :type unichar)

(declaim (inline gtk-text-iter-get-char))

(defun gtk-text-iter-get-char (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{A Unicode character, or 0 if @arg{iter} is not dereferenceable.}
  Returns a Unicode character, or 0 if @arg{iter} is not dereferenceable.
  @see-class{gtk-text-iter}"
  (gtk-text-iter-char iter))

(export 'gtk-text-iter-get-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_slice" gtk-text-iter-get-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[start]{iterator at start of a range}
  @argument[end]{iterator at end of a range}
  @return{Slice of text from the buffer.}
  @begin{short}
    Returns the text in the given range.
  @end{short}
  A \"slice\" is an array of characters encoded in UTF-8 format, including the
  Unicode \"unknown\" character @code{0xFFFC} for iterable non-character
  elements in the buffer, such as images. Because images are encoded in the
  slice, byte and character offsets in the returned array will correspond to
  byte offsets in the text buffer. Note that @code{0xFFFC} can occur in normal
  text as well, so it is not a reliable indicator that a pixbuf or widget is in
  the buffer.
  @see-class{gtk-text-iter}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_text" gtk-text-iter-get-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[start]{iterator at start of a range}
  @argument[end]{iterator at end of a range}
  @return{Array of characters from the buffer.}
  @begin{short}
    Returns text in the given range.
  @end{short}
  If the range contains non-text elements such as images, the character and byte
  offsets in the returned string will not correspond to character and byte
  offsets in the buffer. If you want offsets to correspond, see the function
  @fun{gtk-text-iter-get-slice}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-slice}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_slice" gtk-text-iter-get-visible-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[start]{iterator at start of range}
  @argument[end]{iterator at end of range}
  @return{Slice of text from the buffer.}
  @begin{short}
    Like the function @fun{gtk-text-iter-get-slice}, but invisible text is not
    included.
  @end{short}
  Invisible text is usually invisible because a @class{gtk-text-tag} object with
  the \"invisible\" attribute turned on has been applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-get-slice}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-visible-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_text" gtk-text-iter-get-visible-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[start]{iterator at start of range}
  @argument[end]{iterator at end of range}
  @return{String containing visible text in the range.}
  @begin{short}
    Like the function @fun{gtk-text-iter-get-text}, but invisible text is not
    included.
  @end{short}
  Invisible text is usually invisible because a @class{gtk-text-tag} object with
  the \"invisible\" attribute turned on has been applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-get-text}"
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-visible-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_pixbuf ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-pixbuf
  :reader "gtk_text_iter_get_pixbuf"
  :type (g-object gdk-pixbuf))

(declaim (inline gtk-text-iter-get-pixbuf))

(defun gtk-text-iter-get-pixbuf (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{The pixbuf at @arg{iter}.}
  If the element at @arg{iter} is a pixbuf, the pixbuf is returned with no new
  reference count added. Otherwise, @code{nil} is returned.
  @see-class{gtk-text-iter}"
  (gtk-text-iter-pixbuf iter))

(export 'gtk-text-iter-get-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_marks ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-marks
  :reader "gtk_text_iter_get_marks"
  :type (g-slist (g-object gtk-text-mark) :free-from-foreign t))

(declaim (inline gtk-text-iter-get-marks))

(defun gtk-text-iter-get-marks (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{List of @class{gtk-text-mark} objects.}
  @begin{short}
    Returns a list of all @class{gtk-text-mark} objects at this location.
  @end{short}
  Because marks are not iterable, they do not take up any \"space\" in the
  buffer, they are just marks in between iterable locations, multiple marks can
  exist in the same place. The returned list is not in any meaningful order.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-mark}"
  (gtk-text-iter-marks iter))

(export 'gtk-text-iter-get-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_toggled_tags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_toggled_tags" gtk-text-iter-get-toggled-tags)
    (g-slist (g-object gtk-text-tag))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[toggled-on]{@em{true} to get toggled-on tags}
  @return{Tags toggled at this point.}
  @begin{short}
    Returns a list of @class{gtk-text-tag} objects that are toggled on or off at
    this point.
  @end{short}
  If @arg{toggled-on} is @em{true}, the list contains tags that are toggled on.
  If a tag is toggled on at @arg{iter}, then some non-empty range of characters
  following @arg{iter} has that tag applied to it. If a tag is toggled off, then
  some non-empty range following @arg{iter} does not have the tag applied to it.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (toggled-on :boolean))

(export 'gtk-text-iter-get-toggled-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_child_anchor ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-child-anchor
  :reader "gtk_text_iter_get_child_anchor"
  :type (g-object gtk-text-child-anchor))

(declaim (inline gtk-text-iter-get-child-anchor))

(defun gtk-text-iter-get-child-anchor (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{The anchor at @arg{iter}.}
  If the location at @arg{iter} contains a child anchor, the anchor is returned
  with no new reference count added. Otherwise, @code{nil} is returned.
  @see-class{gtk-text-iter}"
  (gtk-text-iter-child-anchor iter))

(export 'gtk-text-iter-get-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_tag ()
;;;
;;; gboolean
;;; gtk_text_iter_starts_tag (const GtkTextIter *iter,
;;;                           GtkTextTag *tag);
;;;
;;; Returns TRUE if tag is toggled on at exactly this point. If tag is NULL,
;;; returns TRUE if any tag is toggled on at this point.
;;;
;;; Note that if gtk_text_iter_starts_tag() returns TRUE, it means that iter is
;;; at the beginning of the tagged range, and that the character at iter is
;;; inside the tagged range. In other words, unlike gtk_text_iter_ends_tag(),
;;; if gtk_text_iter_starts_tag() returns TRUE, gtk_text_iter_has_tag() will
;;; also return TRUE for the same parameters.
;;;
;;; iter :
;;;     an iterator
;;;
;;; tag :
;;;     a GtkTextTag, or NULL.
;;;
;;; Returns :
;;;     whether iter is the start of a range tagged with tag
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_begins_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_begins_tag" gtk-text-iter-begins-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{Whether @arg{iter} is the start of a range tagged with @arg{tag}.}
  @begin{short}
    Returns @em{true} if @arg{tag} is toggled on at exactly this point.
  @end{short}
  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled on at
  this point. Note that the function @sym{gtk-text-iter-begins-tag} returns
  @em{true} if @arg{iter} is the start of the tagged range; the function
  @fun{gtk-text-iter-has-tag} tells you whether an iterator is within a
  tagged range.
  @begin[Warning]{dictionary}
    The @sym{gtk-text-iter-begins-tag} function has been deprecated since
    version 3.20 and should not be used in newly-written code. Use the
    @func{gtk-text-iter-starts-tag} function instead.
  @end{dictionary}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-has-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-begins-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_tag" gtk-text-iter-ends-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{Whether @arg{iter} is the end of a range tagged with @arg{tag}.}
  @begin{short}
    Returns @em{true} if @arg{tag} is toggled off at exactly this point.
  @end{short}
  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled off at
  this point. Note that the function @sym{gtk-text-iter-ends-tag} returns
  @em{true} if @arg{iter} is the end of the tagged range; the function
  @fun{gtk-text-iter-has-tag} tells you whether an iterator is within a tagged
  range.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tab}
  @see-function{gtk-text-iter-has-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-ends-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_toggles_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_toggles_tag" gtk-text-iter-toggles-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{Whether @arg{tag} is toggled on or off at @arg{iter}.}
  This is equivalent to @code{(or (gtk-text-iter-begins-tag iter tag)
  (gtk-text-iter-ends-tag iter tag))}, i. e. it tells you whether a range with
  @arg{tag} applied to it begins or ends at @arg{iter}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-iter-begins-tag}
  @see-function{gtk-text-iter-ends-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-toggles-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_has_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_has_tag" gtk-text-iter-has-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[tag]{a @class{gtk-text-tag} object}
  @return{Whether @arg{iter} is tagged with @arg{tag}.}
  Returns @em{true} if @arg{iter} is within a range tagged with @arg{tag}.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-has-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_tags ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-tags
  :reader "gtk_text_iter_get_tags"
  :type (g-slist (g-object gtk-text-tag) :free-from-foreign t))

(declaim (inline gtk-text-iter-get-tags))

(defun gtk-text-iter-get-tags (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{List of @class{gtk-text-tag} objects.}
  Returns a list of tags that apply to @arg{iter}, in ascending order of
  priority, highest-priority tags are last.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}"
  (gtk-text-iter-tags iter))

(export 'gtk-text-iter-get-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_editable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_editable" gtk-text-iter-editable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[default-setting]{@em{true} if text is editable by default}
  @return{Whether @arg{iter} is inside an editable range.}
  @begin{short}
    Returns whether the character at @arg{iter} is within an editable region of
    text.
  @end{short}
  Non-editable text is \"locked\" and cannot be changed by the user via
  @class{gtk-text-view}. This function is simply a convenience wrapper around
  the function @fun{gtk-text-iter-get-attributes}. If no tags applied to this
  text affect editability, @arg{default-setting} will be returned.

  You do not want to use this function to decide whether text can be inserted
  at @arg{iter}, because for insertion you do not want to know whether the char
  at @arg{iter} is inside an editable range, you want to know whether a new
  character inserted at @arg{iter} would be inside an editable range. Use
  the function @fun{gtk-text-iter-can-insert} to handle this case.
  @see-class{gtk-text-iter}
  @see-class{gtk-text-view}
  @see-function{gtk-text-iter-get-attributes}
  @see-function{gtk-text-iter-can-insert}"
  (iter (g-boxed-foreign gtk-text-iter))
  (default :boolean))

(export 'gtk-text-iter-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_can_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_can_insert" gtk-text-iter-can-insert) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[default-editability]{@em{true} if text is editable by default}
  @return{Whether text inserted at @arg{iter} would be editable.}
  @begin{short}
    Considering the default editability of the buffer, and tags that affect
    editability, determines whether text inserted at @arg{iter} would be
    editable.
  @end{short}
  If text inserted at @arg{iter} would be editable then the user should be
  allowed to insert text at @arg{iter}. The function
  @fun{gtk-text-buffer-insert-interactive} uses this function to decide whether
  insertions are allowed at a given position.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-insert-interactive}"
  (iter (g-boxed-foreign gtk-text-iter))
  (default-editable :boolean))

(export 'gtk-text-iter-can-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-starts-word
  :reader "gtk_text_iter_starts_word"
  :type :boolean)

(declaim (inline gtk-text-iter-starts-word))

(defun gtk-text-iter-starts-word (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is at the start of a word.}
  @begin{short}
    Determines whether @arg{iter} begins a natural-language word.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-starts-word iter))

(export 'gtk-text-iter-starts-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-ends-word
  :reader "gtk_text_iter_ends_word"
  :type :boolean)

(declaim (inline gtk-text-iter-ends-word))

(defun gtk-text-iter-ends-word (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is at the end of a word.}
  @begin{short}
    Determines whether @arg{iter} ends a natural-language word.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-ends-word iter))

(export 'gtk-text-iter-ends-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-inside-word
  :reader "gtk_text_iter_inside_word"
  :type :boolean)

(declaim (inline gtk-text-iter-inside-word))

(defun gtk-text-iter-inside-word (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is inside a word.}
  @begin{short}
    Determines whether @arg{iter} is inside a natural-language word, as opposed
    to say inside some whitespace.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-inside-word iter))

(export 'gtk-text-iter-inside-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-starts-line
  :reader "gtk_text_iter_starts_line"
  :type :boolean)

(declaim (inline gtk-text-iter-starts-line))

(defun gtk-text-iter-starts-line (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} begins a line.}
  @begin{short}
    Returns @em{true} if @arg{iter} begins a paragraph, i. e. if the function
    @fun{gtk-text-iter-get-line-offset} would return 0.
  @end{short}
  However this function is potentially more efficient than the function
  @fun{gtk-text-iter-get-line-offset} because it does not have to compute the
  offset, it just has to see whether it is 0.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-line-offset}"
  (%gtk-text-iter-starts-line iter))

(export 'gtk-text-iter-starts-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-ends-line
  :reader "gtk_text_iter_ends_line"
  :type :boolean)

(declaim (inline gtk-text-iter-ends-line))

(defun gtk-text-iter-ends-line (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} is at the end of a line.}
  @begin{short}
    Returns @em{true} if @arg{iter} points to the start of the paragraph
    delimiter characters for a line, delimiters will be either a newline, a
    carriage return, a carriage return followed by a newline, or a Unicode
    paragraph separator character.
  @end{short}
  Note that an iterator pointing to the \n of a \r\n pair will not be counted as
  the end of a line, the line ends before the \r. The end iterator is considered
  to be at the end of a line, even though there are no paragraph delimiter chars
  there.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-ends-line iter))

(export 'gtk-text-iter-ends-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-starts-sentence
  :reader "gtk_text_iter_starts_sentence"
  :type :boolean)

(declaim (inline gtk-text-iter-starts-sentence))

(defun gtk-text-iter-starts-sentence (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is at the start of a sentence.}
  @begin{short}
    Determines whether @arg{iter} begins a sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-starts-sentence iter))

(export 'gtk-text-iter-starts-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-ends-sentence
  :reader "gtk_text_iter_ends_sentence"
  :type :boolean)

(declaim (inline gtk-text-iter-ends-sentence))

(defun gtk-text-iter-ends-sentence (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is at the end of a sentence.}
  @begin{short}
    Determines whether @arg{iter} ends a sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-ends-sentence iter))

(export 'gtk-text-iter-ends-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-inside-sentence
  :reader "gtk_text_iter_inside_sentence"
  :type :boolean)

(declaim (inline gtk-text-iter-inside-sentence))

(defun gtk-text-iter-inside-sentence (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} is inside a sentence.}
  @begin{short}
    Determines whether @arg{iter} is inside a sentence, as opposed to in between
    two sentences, e. g. after a period and before the first letter of the next
    sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}"
  (%gtk-text-iter-inside-sentence iter))

(export 'gtk-text-iter-inside-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_cursor_position ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-is-cursor-position
  :reader "gtk_text_iter_is_cursor_position"
  :type :boolean)

(declaim (inline gtk-text-iter-is-cursor-position))

(defun gtk-text-iter-is-cursor-position (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if the cursor can be placed at @arg{iter}.}
  See the function @fun{gtk-text-iter-forward-cursor-position}, the
  @symbol{pango-log-attr} structure or the function @fun{pango-break} for
  details on what a cursor position is.
  @see-class{gtk-text-iter}
  @see-symbol{pango-log-attr}
  @see-function{pango-break}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (%gtk-text-iter-is-cursor-position iter))

(export 'gtk-text-iter-is-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_chars_in_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-chars-in-line
  :reader "gtk_text_iter_get_chars_in_line"
  :type :int)

(declaim (inline gtk-text-iter-get-chars-in-line))

(defun gtk-text-iter-get-chars-in-line (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Number of characters in the line.}
  Returns the number of characters in the line containing @arg{iter}, including
  the paragraph delimiters.
  @see-class{gtk-text-iter}"
  (gtk-text-iter-chars-in-line iter))

(export 'gtk-text-iter-get-chars-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_bytes_in_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-bytes-in-line
  :reader "gtk_text_iter_get_bytes_in_line"
  :type :int)

(declaim (inline gtk-text-iter-get-bytes-in-line))

(defun gtk-text-iter-get-bytes-in-line (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Number of bytes in the line.}
  Returns the number of bytes in the line containing @arg{iter}, including the
  paragraph delimiters.
  @see-class{gtk-text-iter}"
  (gtk-text-iter-bytes-in-line iter))

(export 'gtk-text-iter-get-bytes-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_attributes" %gtk-text-iter-get-attributes) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (values (:pointer (:struct gtk-text-attributes))))

(defun gtk-text-iter-get-attributes (iter default-attributes)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @begin{return}
    @code{values} -- a @symbol{gtk-text-attributes} @br{}
    @code{change-p} -- @em{True} if values was modified
  @end{return}
  @begin{short}
    Computes the effect of any tags applied to this spot in the text. The values
    parameter should be initialized to the default settings you wish to use if
    no tags are in effect.
  @end{short}
  You would typically obtain the defaults from the function
  @fun{gtk-text-view-get-default-attributes}.

  The function @sym{gtk-text-iter-get-attributes} will modify values, applying
  the effects of any tags present at @arg{iter}. If any tags affected values,
  the function returns @em{true}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-view-get-default-attributes}"
  (let ((changed-p (%gtk-text-iter-get-attributes iter default-attributes)))
    (values default-attributes changed-p)))

(export 'gtk-text-iter-get-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_language ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-language
  :reader "gtk_text_iter_get_language"
  :type :pointer)

(declaim (inline gtk-text-iter-get-language))

(defun gtk-text-iter-get-language (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Language in effect at @arg{iter}.}
  @begin{short}
    A convenience wrapper around the function
    @fun{gtk-text-iter-get-attributes}, which returns the language in effect at
    @arg{iter}.
  @end{short}
  If no tags affecting language apply to @arg{iter}, the return value is
  identical to that of the function @fun{gtk-get-default-language}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-attributes}
  @see-function{gtk-get-default-language}"
  (gtk-text-iter-language iter))

(export 'gtk-text-iter-get-language)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_end ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-is-end
  :reader "gtk_text_iter_is_end"
  :type :boolean)

(declaim (inline gtk-text-iter-is-end))

(defun gtk-text-iter-is-end (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} is the end iterator.}
  @begin{short}
    Returns @em{true} if @arg{iter} is the end iterator, i. e. one past the last
    dereferenceable iterator in the buffer.
  @end{short}
  The function @sym{gtk-text-iter-is-end} is the most efficient way to check
  whether an iterator is the end iterator.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-is-start}"
  (%gtk-text-iter-is-end iter))

(export 'gtk-text-iter-is-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_start ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter %gtk-text-iter-is-start
  :reader "gtk_text_iter_is_start"
  :type :boolean)

(declaim (inline gtk-text-iter-is-start))

(defun gtk-text-iter-is-start (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} is the first in the buffer.}
  Returns @em{true} if @arg{iter} is the first iterator in the buffer, that is
  if @arg{iter} has a character offset of 0.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-is-end}"
  (%gtk-text-iter-is-start iter))

(export 'gtk-text-iter-is-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_char" gtk-text-iter-forward-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{iter} forward by one character offset.
  @end{short}
  Note that images embedded in the buffer occupy 1 character slot, so the
  function @sym{gtk-text-iter-forward-char} may actually move onto an image
  instead of a character, if you have images in your buffer. If @arg{iter} is
  the end iterator or one character before it, @arg{iter} will now point at the
  end iterator, and the function @sym{gtk-text-iter-forward-char} returns
  @code{nil} for convenience when writing loops.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_char" gtk-text-iter-backward-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @return{Whether movement was possible.}
  @begin{short}
    Moves backward by one character offset.
  @end{short}
  Returns @em{true} if movement was possible; if @arg{iter} was the first in the
  buffer, character offset 0, the function @sym{gtk-text-iter-backward-char}
  returns @code{nil} for convenience when writing loops.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_chars ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_chars" gtk-text-iter-forward-chars) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[count]{number of characters to move, may be negative}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} characters if possible, if @arg{count} would move past the
    start or end of the buffer, moves to the start or end of the buffer.
  @end{short}
  The return value indicates whether the new position of @arg{iter} is different
  from its original position, and dereferenceable, the last iterator in the
  buffer is not dereferenceable. If @arg{count} is 0, the function does nothing
  and returns @code{nil}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-chars}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_chars ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_chars" gtk-text-iter-backward-chars) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{an iterator}
  @argument[count]{number of characters to move}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves count characters backward, if possible, if count would move past the
    start or end of the buffer, moves to the start or end of the buffer.
  @end{short}
  The return value indicates whether the iterator moved onto a dereferenceable
  position; if the iterator did not move, or moved onto the end iterator, then
  @code{nil} is returned. If count is 0, the function does nothing and returns
  @code{nil}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-chars}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_line" gtk-text-iter-forward-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} can be dereferenced.}
  @begin{short}
    Moves @arg{iter} to the start of the next line.
  @end{short}
  If the @arg{iter} is already on the last line of the buffer, moves the
  @arg{iter} to the end of the current line. If after the operation, the
  @arg{iter} is at the end of the buffer and not dereferencable, returns
  @code{nil}. Otherwise, returns @em{true}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_line" gtk-text-iter-backward-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} moved.}
  @begin{short}
    Moves @arg{iter} to the start of the previous line.
  @end{short}
  Returns @em{true} if @arg{iter} could be moved; i. e. if @arg{iter} was at
  character offset 0, this function returns @code{nil}. Therefore if @arg{iter}
  was already on line 0, but not at the start of the line, @arg{iter} is snapped
  to the start of the line and the function returns @em{true}. Note that this
  implies that in a loop calling this function, the line number may not change
  on every iteration, if your first iteration is on line 0.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_lines" gtk-text-iter-forward-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of lines to move forward}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} lines forward, if possible, if @arg{count} would move past
    the start or end of the buffer, moves to the start or end of the buffer.
  @end{short}
  The return value indicates whether the iterator moved onto a dereferenceable
  position; if the iterator did not move, or moved onto the end iterator, then
  @code{nil} is returned. If @arg{count} is 0, the function does nothing and
  returns @code{nil}. If @arg{count} is negative, moves backward by
  0 - @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_lines" gtk-text-iter-backward-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @argument[count]{number of lines to move backward}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} lines backward, if possible, if @arg{count} would move
    past the start or end of the buffer, moves to the start or end of the
    buffer.
  @end{short}
  The return value indicates whether the iterator moved onto a dereferenceable
  position; if the iterator did not move, or moved onto the end iterator, then
  @code{nil} is returned. If @arg{count} is 0, the function does nothing and
  returns @code{nil}. If @arg{count} is negative, moves forward by
  0 - @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-lines}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_word_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_word_ends" gtk-text-iter-forward-word-ends)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of times to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  Calls the function @fun{gtk-text-iter-forward-word-end} up to @arg{count}
  times.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of times to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  Calls the function @fun{gtk-text-iter-backward-word-start} up to @arg{count}
  times.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next word end. If @arg{iter} is currently on a word
    end, moves forward to the next one after that.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-word-start}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-word-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_word_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_word_start" gtk-text-iter-backward-word-start)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous word start. If @arg{iter} is currently on a
    word start, moves backward to the next one after that.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-end}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-word-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_cursor_position"
           gtk-text-iter-forward-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves @arg{iter} forward by a single cursor position.
  @end{short}
  Cursor positions are (unsurprisingly) positions where the cursor can appear.
  Perhaps surprisingly, there may not be a cursor position between all
  characters. The most common example for European languages would be a carriage
  return/newline sequence. For some Unicode characters, the equivalent of say
  the letter \"a\" with an accent mark will be represented as two characters,
  first the letter then a \"combining mark\" that causes the accent to be
  rendered; so the cursor cannot go between those two characters. See also the
  @symbol{pango-log-attr} structure and @fun{pango-break} function.
  @see-class{gtk-text-iter}
  @see-symbol{pango-log-attr}
  @see-function{pango-break}
  @see-function{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_cursor_position"
           gtk-text-iter-backward-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if we moved.}
  @begin{short}
    Like the function @fun{gtk-text-iter-forward-cursor-position}, but moves
    backward.
  @end{short}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_cursor_positions"
          gtk-text-iter-forward-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @argument[count]{number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-cursor-position}
  @see-function{gtk-text-iter-forward-cursor-poistions}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_sentence_start"
           gtk-text-iter-backward-sentence-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-2}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous sentence start; if @arg{iter} is already at
    the start of a sentence, moves backward to the next one.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language. If not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-sentence-starts}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-sentence-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_sentence_starts"
          gtk-text-iter-backward-sentence-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of sentences to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-backward-sentence-start} up to
    @arg{count} times, or until it returns @code{nil}.
  @end{short}
  If @arg{count} is negative, moves forward instead of backward.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next sentence end.
  @end{short}
  If @arg{iter} is at the end of a sentence, moves to the next end of sentence.
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-sentence-ends}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-sentence-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_sentence_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_sentence_ends"
          gtk-text-iter-forward-sentence-ends) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of sentences to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-forward-sentence-end} @arg{count}
    times, or until the function @fun{gtk-text-iter-forward-sentence-end}
    returns @code{nil}.
  @end{short}
  If @arg{count} is negative, moves backward instead of forward.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of times to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-forward-visible-word-end} up to
    @arg{count} times.
  @end{short}
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of times to move}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Calls the function @fun{gtk-text-iter-backward-visible-word-start} up to
    @arg{count} times.
  @end{short}
  @see-class{gtk-text-iter}
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
 "@version{2013-8-2}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves forward to the next visible word end.
  @end{short}
  If @arg{iter} is currently on a word end, moves forward to the next one after
  that. Word breaks are determined by Pango and should be correct for nearly any
  language. If not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-visible-word-start}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-word-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_word_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_word_start"
           gtk-text-iter-backward-visible-word-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-2}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if @arg{iter} moved and is not the end iterator.}
  @begin{short}
    Moves backward to the previous visible word start.
  @end{short}
  If @arg{iter} is currently on a word start, moves backward to the next one
  after that. Word breaks are determined by Pango and should be correct for
  nearly any language. If not, the correct fix would be to the Pango word break
  algorithms.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-visible-word-end}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-word-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_cursor_position"
           gtk-text-iter-forward-visible-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves @arg{iter} forward to the next visible cursor position.
  @end{short}
  See the function @fun{gtk-text-iter-forward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_cursor_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_cursor_position"
           gtk-text-iter-backward-visible-cursor-position) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves @arg{iter} forward to the previous visible cursor position.
  @end{short}
  See the function @fun{gtk-text-iter-backward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_cursor_positions"
          gtk-text-iter-forward-visible-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-29}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} visible cursor positions. See the function
    @fun{gtk-text-iter-forward-cursor-position} for details.
  @end{short}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @argument[count]{number of positions to move}
  @return{@em{True} if we moved and the new position is dereferenceable.}
  @begin{short}
    Moves up to @arg{count} visible cursor positions.
  @end{short}
  See the function @fun{gtk-text-iter-backward-cursor-position} for details.
  @see-class{gtk-text-iter}
  @fun{gtk-text-iter-backward-cursor-position}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_line"
           gtk-text-iter-forward-visible-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} can be dereferenced.}
  @begin{short}
    Moves @arg{iter} to the start of the next visible line.
  @end{short}
  Returns @em{true} if there was a next line to move to, and @code{nil} if
  @arg{iter} was simply moved to the end of the buffer and is now not
  dereferenceable, or if @arg{iter} was already at the end of the buffer.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-visible-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-visible-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_line"
           gtk-text-iter-backward-visible-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{an iterator}
  @return{Whether @arg{iter} moved.}
  @begin{short}
    Moves @arg{iter} to the start of the previous visible line.
  @end{short}
  Returns @em{true} if @arg{iter}could be moved; i. e. if @arg{iter} was at
  character offset 0, this function returns @code{nil}. Therefore if @arg{iter}
  was already on line 0, but not at the start of the line, @arg{iter} is snapped
  to the start of the line and the function returns @em{true}. Note that this
  implies that in a loop calling this function, the line number may not change
  on every iteration, if your first iteration is on line 0.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-visible-line}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-visible-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_lines"
          gtk-text-iter-forward-visible-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of lines to move forward}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} visible lines forward, if possible, if @arg{count} would
    move past the start or end of the buffer, moves to the start or end of the
    buffer.
  @end{short}
  The return value indicates whether the iterator moved onto a dereferenceable
  position; if the iterator did not move, or moved onto the end iterator, then
  @code{nil} is returned. If @arg{count} is 0, the function does nothing and
  returns @code{nil}. If @arg{count} is negative, moves backward by
  0 - @arg{count} lines.
  @see-class{gtk-text-iter}
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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter}}
  @argument[count]{number of lines to move backward}
  @return{Whether @arg{iter} moved and is dereferenceable.}
  @begin{short}
    Moves @arg{count} visible lines backward, if possible.
  @end{short}
  If @arg{count} would move past the start or end of the buffer, moves to the
  start or end of the buffer. The return value indicates whether the iterator
  moved onto a dereferenceable position; if the iterator did not move, or
  moved onto the end iterator, then @code{nil} is returned. If @arg{count} is 0,
  the function does nothing and returns @code{nil}. If @arg{count} is negative,
  moves forward by 0 - @arg{count} lines.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-visible-lines}"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-offset))

(defun gtk-text-iter-set-offset (iter char-offset)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[char-offset]{a character number}
  @begin{short}
    Sets @arg{iter} to point to @arg{char-offset}.
  @end{short}
  @arg{char-offset} counts from the start of the entire text buffer, starting
  with 0.
  @see-class{gtk-text-iter}"
  (setf (gtk-text-iter-offset iter) char-offset))

(export 'gtk-text-iter-set-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-line))

(defun gtk-text-iter-set-line (iter line-number)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[line-number]{line number, counted from 0}
  @begin{short}
    Moves iterator @arg{iter} to the start of the line @arg{line-number}.
  @end{short}
  If @arg{line-number} is negative or larger than the number of lines in the
  buffer, moves @arg{iter} to the start of the last line in the buffer.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-line}"
  (setf (gtk-text-iter-line iter) line-number))

(export 'gtk-text-iter-set-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-line-offset))

(defun gtk-text-iter-set-line-offset (iter char-on-line)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[char-on-line]{a character offset relative to the start of
    @arg{iter}'s current line}
  @begin{short}
    Moves @arg{iter} within a line, to a new character (not byte) offset.
  @end{short}
  The given character offset must be less than or equal to the number of
  characters in the line; if equal, @arg{iter} moves to the start of the next
  line. See the function @fun{gtk-text-iter-set-line-index} if you have a byte
  index rather than a character offset.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-line-offset}
  @see-function{gtk-text-iter-set-line-index}"
  (setf (gtk-text-iter-line-offset iter) char-on-line))

(export 'gtk-text-iter-set-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_set_line_index" gtk-text-iter-set-line-index) :void
 #+cl-cffi-gtk-documentation
 "@ver{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[byte-on-line]{a byte index relative to the start of @arg{iter}'s
    current line}
  @begin{short}
    Same as the function @fun{gtk-text-iter-set-line-offset}, but works with a
    byte index.
  @end{short}
  The given byte index must be at the start of a character, it cannot be in the
  middle of a UTF-8 encoded character.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-line-index}
  @see-function{gtk-text-iter-set-line-offset} "
  (iter (g-boxed-foreign gtk-text-iter))
  (byte-on-line :int))

(export 'gtk-text-iter-set-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_visible_line_index ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-visible-line-index))

(defun gtk-text-iter-set-visible-line-index (iter byte-on-line)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter}}
  @argument[byte-on-line]{a byte index}
  Like the function @fun{gtk-text-iter-set-line-index}, but the index is in
  visible bytes, i. e. text with a tag making it invisible is not counted in
  the index.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-set-line-index}
  @see-function{gtk-text-iter-get-visible-line-index}"
  (setf (gtk-text-iter-visible-line-index iter) byte-on-line))

(export 'gtk-text-iter-set-visible-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_visible_line_offset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_set_visible_line_offset"
           gtk-text-iter-set-visible-line-offset) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[char-on-line]{a character offset}
  Like the function @fun{gtk-text-iter-set-line-offset}, but the offset is in
  visible characters, i. e. text with a tag making it invisible is not counted
  in the offset.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-visible-line-offset}
  @see-function{gtk-text-iter-set-line-offset}"
  (iter (g-boxed-foreign gtk-text-iter))
  (char-on-line :int))

(export 'gtk-text-iter-set-visible-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_end" gtk-text-iter-forward-to-end) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-13}
  @argument[iter]{a @class{gtk-text-iter} object}
  @begin{short}
    Moves @arg{iter} forward to the \"end iterator\", which points one past the
    last valid character in the buffer.
  @end{short}
  The function @fun{gtk-text-iter-get-char} called on the end iterator returns
  0, which is convenient for writing loops.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-get-char}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_line_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_line_end" gtk-text-iter-forward-to-line-end)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if we moved and the new location is not the end iterator.}
  @begin{short}
    Moves the iterator to point to the paragraph delimiter characters, which
    will be either a newline, a carriage return, a carriage return/newline in
    sequence, or the Unicode paragraph separator character.
  @end{short}
  If the iterator is already at the paragraph delimiter characters, moves to the
  paragraph delimiter characters for the next line. If @arg{iter} is on the last
  line in the buffer, which does not end in paragraph delimiters, moves to the
  end iterator (end of the last line), and returns @code{nil}.
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_tag_toggle"
          gtk-text-iter-forward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{Whether we found a @arg{tag} toggle after @arg{iter}.}
  @begin{short}
    Moves forward to the next toggle (on or off) of the @class{gtk-text-tag}
    @arg{tag}, or to the next toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}
  If no matching tag toggles are found, returns @code{nil}, otherwise @em{true}.
  Does not return toggles located at @arg{iter}, only toggles after @arg{iter}.
  Sets @arg{iter} to the location of the toggle, or to the end of the buffer if
  no toggle is found.
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-forward-to-tag-toggle)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_to_tag_toggle"
          gtk-text-iter-backward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[tag]{a @class{gtk-text-tag} object, or @code{nil}}
  @return{Whether we found a @arg{tag} toggle before @arg{iter}.}
  @begin{short}
    Moves backward to the next toggle (on or off) of the @class{gtk-text-tag}
    @arg{tag}, or to the next toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}
  If no matching tag toggles are found, returns @code{nil}, otherwise @em{true}.
  Does not return toggles located at @arg{iter}, only toggles before @arg{iter}.
  Sets @arg{iter} to the location of the toggle, or the start of the buffer if
  no toggle is found.
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export '(gtk-text-iter-backward-to-tag-toggle))

;;; ----------------------------------------------------------------------------
;;; GtkTextCharPredicate ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-char-predicate :boolean ((char unichar)
                                               (user-data :pointer))
  (let ((function (glib::get-stable-pointer-value user-data)))
    (funcall function char)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_find_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_find_char"
          %gtk-text-iter-forward-find-char) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-find-char (iter predicate &key limit (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[iter]{start of search}
  @argument[pred]{a function to be called on each character}
  @argument[limit]{bound for the search, or @code{nil} for the end of the
    buffer}
  @argument[direction]{the value @code{:forward} indicates forward search and
    the value @code{:backward} backward search}
  @return{Whether a match was found.}
  @begin{short}
    This is a convenience function of the Lisp implementation which combines
    the functions @fun{gtk-text-iter-forward-find-char} and
  @fun{gtk-text-iter-backward-find-char} into one single function.
  @end{short}
  The direction of the search is indicated with the keyword argument
  @arg{direction} which has a default value of @code{:forward} for forward
  search. For backward search the @arg{direction} takes the value
  @code{:backward}.

  In addition the argument @arg{limit} is a keyword argument with a default
  value @code{nil}.
  @see-class{gtk-text-iter}
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

(defun gtk-text-iter-forward-find-char (iter pred limit)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[pred]{a function to be called on each character}
  @argument[limit]{search limit, or @code{nil} for none}
  @return{Whether a match was found.}
  @begin{short}
    Advances @arg{iter}, calling @arg{pred} on each character.
  @end{short}
  If @arg{pred} returns @em{true}, returns @em{true} and stops scanning. If
  @arg{pred} never returns @em{true}, @arg{iter} is set to @arg{limit} if
  @arg{limit} is non-@code{nil}, otherwise to the end iterator.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-backward-find-char}"
  (gtk-text-iter-find-char iter pred :limit limit :direction :forward))

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
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[pred]{function to be called on each character}
  @argument[limit]{search limit, or @code{nil} for none}
  @return{Whether a match was found.}
  Same as the function @fun{gtk-text-iter-forward-find-char}, but goes backward
  from @arg{iter}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-find-char}"
  (gtk-text-iter-find-char iter pred :limit limit :direction :backward))

(export 'gtk-text-iter-backward-find-char)

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
(setf (gethash 'gtk-text-search-flags atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-search-flags atdoc:*external-symbols*)
 "@version{2013-8-14}
  @short{}
  @begin{pre}
(define-g-flags \"GtkTextSearchFlags\" gtk-text-search-flags
  (:export t
   :type-initializer \"gtk_text_search_flags_get_type\")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))
  @end{pre}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-forward-search}
  @see-function{gtk-text-iter-backward-search}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_search ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-search (iter str &key flags limit (direction :forward))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-12}
  @argument[iter]{start of search}
  @argument[str]{a search string}
  @argument[flags]{flags of type @symbol{gtk-text-search-flags} affecting how
    the search is done}
  @argument[limit]{bound for the search, or @code{nil} for the end of the
    buffer}
  @argument[direction]{the value @code{:forward} indicates forward search and
    the value @code{:backward} backward search}
  @begin{return}
    Whether a match was found. @br{}
    @code{match-start} -- start of match, or @code{nil} @br{}
    @code{match-end} -- end of match, or @code{nil}
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

(defcfun ("gtk_text_iter_forward_search" %gtk-text-iter-forward-search) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (str (:string :free-to-foreign t))
  (flags gtk-text-search-flags)
  (match-start (g-boxed-foreign gtk-text-iter))
  (match-end (g-boxed-foreign gtk-text-iter))
  (limit (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-iter-forward-search (iter str flags limit)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-12}
  @argument[iter]{start of search}
  @argument[str]{a search string}
  @argument[flags]{flags @symbol{gtk-text-search-flags} affecting how the search
    is done}
  @argument[limit]{bound for the search, or @code{nil} for the end of the
    buffer}
  @begin{return}
    Whether a match was found. @br{}
    @code{match-start} -- start of match, or @code{nil} @br{}
    @code{match-end} -- end of match, or @code{nil}
  @end{return}
  @begin{short}
    Searches forward for @arg{str}. Any match is returned by setting
    @arg{match-start} to the first character of the match and @arg{match-end}
    to the first character after the match. The search will not continue past
    @arg{limit}. Note that a search is a linear or O(n) operation, so you may
    wish to use @arg{limit} to avoid locking up your UI on large buffers.
  @end{short}

  If the @code{:visible-only} flag is present, the match may have
  invisible text interspersed in @arg{str}, i. e. @arg{str} will be a
  possibly-noncontiguous subsequence of the matched range. Similarly, if you
  specify @code{:text-only}, the match may have pixbufs or child widgets mixed
  inside the matched range. If these flags are not given, the match must be
  exact; the special @code{0xFFFC} character in @arg{str} will match embedded
  pixbufs or child widgets. If you specify the @code{:case-insensitive} flag,
  the text will be matched regardless of what case it is in.
  @see-class{gtk-text-iter}
  @see-symbol{gtk-text-search-flags}
  @see-function{gtk-text-iter-search}
  @see-function{gtk-text-iter-backward-search}"
  (gtk-text-iter-search iter str :flags flags :limit limit :direction :forward))

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
 "@version{2013-8-12}
  @argument[iter]{a @class{gtk-text-iter} object where the search begins}
  @argument[str]{search string}
  @argument[flags]{bitmask of flags of type @symbol{gtk-text-search-flags}
    affecting the search}
  @argument[limit]{location of last possible @arg{match-start}, or @code{nil}
    for start of buffer}
  @begin{return}
    Whether a match was found. @br{}
    @code{match-start} -- start of match, or @code{nil} @br{}
    @code{match-end} -- end of match, or @code{nil}
  @end{return}
  Same as the function @fun{gtk-text-iter-forward-search}, but moves backward.
  @see-class{gtk-text-iter}
  @see-symbol{gtk-text-search-flags}
  @see-function{gtk-text-iter-search}
  @see-function{gtk-text-iter-forward-search}"
  (gtk-text-iter-search iter str :flags flags
                                 :limit limit
                                 :direction :backward))

(export 'gtk-text-iter-backward-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_equal" gtk-text-iter-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[lhs]{a @class{gtk-text-iter} object}
  @argument[rhs]{another @class{gtk-text-iter} object}
  @return{@em{True} if the iterators point to the same place in the buffer.}
  @begin{short}
    Tests whether two iterators are equal, using the fastest possible mechanism.
  @end{short}
  This function is very fast; you can expect it to perform better than e. g.
  getting the character offset for each iterator and comparing the offsets
  yourself. Also, it is a bit faster than the function
  @fun{gtk-text-iter-compare}.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-compare}"
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_compare" gtk-text-iter-compare) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[lhs]{a @class{gtk-text-iter} object}
  @argument[rhs]{another @class{gtk-text-iter} object}
  @return{-1 if @arg{lhs} is less than @arg{rhs}, 1 if @arg{lhs} is greater, 0
    if they are equal.}
  @begin{short}
    A @code{qsort()}-style function that returns negative if @arg{lhs} is less
    than @arg{rhs}, positive if @arg{lhs} is greater than @arg{rhs}, and 0 if
    they are equal.
  @end{short}
  Ordering is in character offset order, i. e. the first character in
  the buffer is less than the second character in the buffer.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-equal}"
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_in_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_in_range" gtk-text-iter-in-range) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[start]{start of range}
  @argument[end]{end of range}
  @return{@em{True} if @arg{iter} is in the range.}
  @begin{short}
    Checks whether @arg{iter} falls in the range [@arg{start}, @arg{end}).
  @end{short}
  @arg{start} and @arg{end} must be in ascending order.
  @see-class{gtk-text-iter}"
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_order" gtk-text-iter-order) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-14}
  @argument[first]{a @class{gtk-text-iter} object}
  @argument[second]{another @class{gtk-text-iter} object}
  @begin{short}
    Swaps the value of @arg{first} and @arg{second} if @arg{second} comes before
    @arg{first} in the buffer.
  @end{short}
  That is, ensures that @arg{first} and @arg{second} are in sequence. Most text
  buffer functions that take a range call this automatically on your behalf, so
  there is no real reason to call it yourself in those cases. There are some
  exceptions, such as the function @fun{gtk-text-iter-in-range}, that expect a
  pre-sorted range.
  @see-class{gtk-text-iter}
  @see-function{gtk-text-iter-in-range}"
  (iter-1 (g-boxed-foreign gtk-text-iter))
  (iter-2 (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-order)

;;; --- End of file gtk.text-iter.lisp -----------------------------------------
