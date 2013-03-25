;;; ----------------------------------------------------------------------------
;;; gtk.text-iter.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkTextIter
;;; 
;;; Text buffer iterator
;;;     
;;; Synopsis
;;; 
;;;     GtkTextIter
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
;;;     GtkTextSearchFlags
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
;;;   GBoxed
;;;    +----GtkTextIter
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which
;;; gives an overview of all the objects and data types related to the text
;;; widget and how they work together.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(glib::at-init () (foreign-funcall "gtk_text_iter_get_type" :int))

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
;;; 
;;; typedef struct {
;;;   /* GtkTextIter is an opaque datatype; ignore all these fields.
;;;    * Initialize the iter with gtk_text_buffer_get_iter_*
;;;    * functions
;;;    */
;;; } GtkTextIter;
;;; 
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gtk-text-iter "GtkTextIter"
  :alloc (gtk-text-iter-alloc))

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
  (with-foreign-object (iter '%gtk-text-iter)
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{the buffer}
  Returns the GtkTextBuffer this iterator is associated with."
  (gtk-text-iter-buffer iter))

(export 'gtk-text-iter-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_copy" %gtk-text-iter-copy) :pointer
  (iter :pointer))
  
(defcfun ("gtk_text_iter_copy" gtk-text-iter-copy)
    (g-boxed-foreign gtk-text-iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{A copy of the iter, free with gtk_text_iter_free().}
  Creates a dynamically-allocated copy of an iterator. This function is not
  useful in applications, because iterators can be copied with a simple
  assignment (GtkTextIter i = j;). The function is used by language bindings."
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

(defun gtk-text-iter-get-offset (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{a character offset}
  Returns the character offset of an iterator. Each character in a
  GtkTextBuffer has an offset, starting with 0 for the first character in the
  buffer. Use gtk_text_buffer_get_iter_at_offset() to convert an offset back
  into an iterator."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{a line number}
  Returns the line number containing the iterator. Lines in a GtkTextBuffer
  are numbered beginning with 0 for the first line in the buffer."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{offset from start of line}
  Returns the character offset of the iterator, counting from the start of a
  newline-terminated line. The first character on the line has offset 0."
  (gtk-text-iter-line-offset iter))

(export 'gtk-text-iter-get-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_index ()
;;; 
;;; gint gtk_text_iter_get_line_index (const GtkTextIter *iter);
;;; 
;;; Returns the byte index of the iterator, counting from the start of a
;;; newline-terminated line. Remember that GtkTextBuffer encodes text in UTF-8,
;;; and that characters can require a variable number of bytes to represent.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     distance from start of line, in bytes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_line_index ()
;;; 
;;; gint gtk_text_iter_get_visible_line_index (const GtkTextIter *iter);
;;; 
;;; Returns the number of bytes from the start of the line to the given iter,
;;; not counting bytes that are invisible due to tags with the "invisible" flag
;;; toggled on.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     byte index of iter with respect to the start of the line
;;; ----------------------------------------------------------------------------

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
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{offset in visible characters from the start of the line}
  Returns the offset in characters from the start of the line to the given
  iter, not counting characters that are invisible due to tags with the
  \"invisible\" flag toggled on."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{A Unicode character, or 0 if iter is not dereferenceable.}
  returns 0."
  (gtk-text-iter-char iter))

(export 'gtk-text-iter-get-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_slice" gtk-text-iter-get-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[start]{iterator at start of a range}
  @argument[end]{iterator at end of a range}
  @return{slice of text from the buffer}
  Returns the text in the given range. A \"slice\" is an array of characters
  encoded in UTF-8 format, including the Unicode \"unknown\" character 0xFFFC
  for iterable non-character elements in the buffer, such as images. Because
  images are encoded in the slice, byte and character offsets in the returned
  array will correspond to byte offsets in the text buffer. Note that 0xFFFC
  can occur in normal text as well, so it is not a reliable indicator that a
  pixbuf or widget is in the buffer."
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_text" gtk-text-iter-get-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[start]{iterator at start of a range}
  @argument[end]{iterator at end of a range}
  @return{array of characters from the buffer}
  Returns text in the given range. If the range contains non-text elements
  such as images, the character and byte offsets in the returned string will
  not correspond to character and byte offsets in the buffer. If you want
  offsets to correspond, see gtk_text_iter_get_slice()."
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_slice" gtk-text-iter-get-visible-slice)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[start]{iterator at start of range}
  @argument[end]{iterator at end of range}
  @return{slice of text from the buffer}
  Like gtk_text_iter_get_slice(), but invisible text is not included.
  Invisible text is usually invisible because a GtkTextTag with the
  \"invisible\" attribute turned on has been applied to it."
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-visible-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_visible_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_visible_text" gtk-text-iter-get-visible-text)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[start]{iterator at start of range}
  @argument[end]{iterator at end of range}
  @return{string containing visible text in the range}
  Like gtk_text_iter_get_text(), but invisible text is not included. Invisible
  text is usually invisible because a GtkTextTag with the \"invisible\"
  attribute turned on has been applied to it."
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-get-visible-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_pixbuf ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-pixbuf
  :reader "gtk_text_iter_get_pixbuf"
  :type (g-object gtk-pixbuf))

(declaim (inline gtk-text-iter-get-pixbuf))

(defun gtk-text-iter-get-pixbuf (iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{The pixbuf at iter.}
  If the element at iter is a pixbuf, the pixbuf is returned (with no new
  reference count added). Otherwise, NULL is returned."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{list of GtkTextMark}
  Returns a list of all GtkTextMark at this location. Because marks are not
  iterable (they don't take up any \"space\" in the buffer, they are just marks
  in between iterable locations), multiple marks can exist in the same place.
  The returned list is not in any meaningful order."
  (gtk-text-iter-marks iter))

(export 'gtk-text-iter-get-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_toggled_tags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_toggled_tags" gtk-text-iter-get-toggled-tags)
    (g-slist (g-object gtk-text-tag))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[toggled_on]{TRUE to get toggled-on tags}
  @return{Tags toggled at this point.}
  Returns a list of GtkTextTag that are toggled on or off at this point. (If
  toggled_on is TRUE, the list contains tags that are toggled on.) If a tag is
  toggled on at iter, then some non-empty range of characters following iter
  has that tag applied to it. If a tag is toggled off, then some non-empty
  range following iter does not have the tag applied to it."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @reurn{The anchor at iter.}
  If the location at iter contains a child anchor, the anchor is returned
  (with no new reference count added). Otherwise, NULL is returned."
  (gtk-text-iter-child-anchor iter))

(export 'gtk-text-iter-get-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_begins_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_begins_tag" gtk-text-iter-begins-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[tag]{a GtkTextTag, or NULL}
  @return{Whether iter is the start of a range tagged with tag.}
  Returns TRUE if tag is toggled on at exactly this point. If tag is NULL,
  returns TRUE if any tag is toggled on at this point. Note that the
  gtk_text_iter_begins_tag() returns TRUE if iter is the start of the tagged
  range; gtk_text_iter_has_tag() tells you whether an iterator is within a
  tagged range."
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-begins-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_ends_tag" gtk-text-iter-ends-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[tag]{a GtkTextTag, or NULL}
  @return{Whether iter is the end of a range tagged with tag.}
  Returns TRUE if tag is toggled off at exactly this point. If tag is NULL,
  returns TRUE if any tag is toggled off at this point. Note that the
  gtk_text_iter_ends_tag() returns TRUE if iter is the end of the tagged
  range; gtk_text_iter_has_tag() tells you whether an iterator is within a
  tagged range."
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-ends-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_toggles_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_toggles_tag" gtk-text-iter-toggles-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[tag]{a GtkTextTag, or NULL}
  @return{Whether tag is toggled on or off at iter.}
  This is equivalent to (gtk_text_iter_begins_tag() ||
  gtk_text_iter_ends_tag()), i.e. it tells you whether a range with tag
  applied to it begins or ends at iter."
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-toggles-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_has_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_has_tag" gtk-text-iter-has-tag) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[tag]{a GtkTextTag}
  @return{Whether iter is tagged with tag.}
  Returns TRUE if iter is within a range tagged with tag."
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
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{list of GtkTextTag}
  Returns a list of tags that apply to iter, in ascending order of priority
  (highest-priority tags are last). The GtkTextTag in the list don't have a
  reference added, but you have to free the list itself."
  (gtk-text-iter-tags iter))

(export 'gtk-text-iter-get-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_editable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_editable" gtk-text-iter-editable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[default_setting]{TRUE if text is editable by default}
  @return{whether iter is inside an editable range}
  @begin{short}
    Returns whether the character at iter is within an editable region of text.
    Non-editable text is \"locked\" and can't be changed by the user via
    GtkTextView. This function is simply a convenience wrapper around
    gtk_text_iter_get_attributes(). If no tags applied to this text affect
    editability, default_setting will be returned.
  @end{short}

  You don't want to use this function to decide whether text can be inserted
  at iter, because for insertion you don't want to know whether the char at
  iter is inside an editable range, you want to know whether a new character
  inserted at iter would be inside an editable range. Use
  gtk_text_iter_can_insert() to handle this case."
  (iter (g-boxed-foreign gtk-text-iter))
  (default :boolean))

(export 'gtk-text-iter-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_can_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_can_insert" gtk-text-iter-can-insert) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[default_editability]{TRUE if text is editable by default}
  @return{whether text inserted at iter would be editable}
  Considering the default editability of the buffer, and tags that affect
  editability, determines whether text inserted at iter would be editable. If
  text inserted at iter would be editable then the user should be allowed to
  insert text at iter. gtk_text_buffer_insert_interactive() uses this function
  to decide whether insertions are allowed at a given position."
  (iter (g-boxed-foreign gtk-text-iter))
  (default-editable :boolean))

(export 'gtk-text-iter-can-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-starts-word
  :reader "gtk_text_iter_starts_word"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-starts-word 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is at the start of a word}
  Determines whether iter begins a natural-language word. Word breaks are
  determined by Pango and should be correct for nearly any language (if not,
  the correct fix would be to the Pango word break algorithms).")

(export 'gtk-text-iter-starts-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-ends-word
  :reader "gtk_text_iter_ends_word"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-ends-word 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is at the end of a word}
  Determines whether iter ends a natural-language word. Word breaks are
  determined by Pango and should be correct for nearly any language (if not,
  the correct fix would be to the Pango word break algorithms).")

(export 'gtk-text-iter-ends-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_word ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-inside-word
  :reader "gtk_text_iter_inside_word"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-inside-word 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is inside a word}
  Determines whether iter is inside a natural-language word (as opposed to say
  inside some whitespace). Word breaks are determined by Pango and should be
  correct for nearly any language (if not, the correct fix would be to the
  Pango word break algorithms).")

(export 'gtk-text-iter-inside-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-starts-line
  :reader "gtk_text_iter_starts_line"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-starts-line 'function)
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{whether iter begins a line}
  Returns TRUE if iter begins a paragraph, i.e. if
  gtk_text_iter_get_line_offset() would return 0. However this function is
  potentially more efficient than gtk_text_iter_get_line_offset() because it
  doesn't have to compute the offset, it just has to see whether it's 0.")

(export 'gtk-text-iter-starts-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_line ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-ends-line
  :reader "gtk_text_iter_ends_line"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-ends-line 'function)
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{whether iter is at the end of a line}
  Returns TRUE if iter points to the start of the paragraph delimiter
  characters for a line (delimiters will be either a newline, a carriage
  return, a carriage return followed by a newline, or a Unicode paragraph
  separator character). Note that an iterator pointing to the \n of a \r\n
  pair will not be counted as the end of a line, the line ends before the \r.
  The end iterator is considered to be at the end of a line, even though there
  are no paragraph delimiter chars there.")

(export 'gtk-text-iter-ends-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-starts-sentence
  :reader "gtk_text_iter_starts_sentence"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-starts-sentence 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is at the start of a sentence.}
  Determines whether iter begins a sentence. Sentence boundaries are
  determined by Pango and should be correct for nearly any language (if not,
  the correct fix would be to the Pango text boundary algorithms).")

(export 'gtk-text-iter-starts-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-ends-sentence
  :reader "gtk_text_iter_ends_sentence"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-ends-sentence 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is at the end of a sentence.}
  Determines whether iter ends a sentence. Sentence boundaries are determined
  by Pango and should be correct for nearly any language (if not, the correct
  fix would be to the Pango text boundary algorithms).")

(export 'gtk-text-iter-ends-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_sentence ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-inside-sentence
  :reader "gtk_text_iter_inside_sentence"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-inside-sentence 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if iter is inside a sentence.}
  Determines whether iter is inside a sentence (as opposed to in between two
  sentences, e.g. after a period and before the first letter of the next
  sentence). Sentence boundaries are determined by Pango and should be correct
  for nearly any language (if not, the correct fix would be to the Pango text
  boundary algorithms).")

(export 'gtk-text-iter-inside-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_cursor_position ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-is-cursor-position
  :reader "gtk_text_iter_is_cursor_position"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-is-cursor-position 'function)
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if the cursor can be placed at iter}
  See gtk_text_iter_forward_cursor_position() or PangoLogAttr or pango_break()
  for details on what a cursor position is.")

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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{number of characters in the line}
  Returns the number of characters in the line containing iter, including the
  paragraph delimiters."
  (gtk-text-iter-chars-in-line iter))

(export 'gtk-text-iter-get-chars-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_bytes_in_line ()
;;; 
;;; gint gtk_text_iter_get_bytes_in_line (const GtkTextIter *iter);
;;; 
;;; Returns the number of bytes in the line containing iter, including the
;;; paragraph delimiters.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     number of bytes in the line
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_get_attributes" %gtk-text-iter-get-attributes) :boolean
  (iter (g-boxed-foreign gtk-text-iter))
  (values (g-object gtk-text-attributes)))

(defun gtk-text-iter-get-attributes (iter default-attributes)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[values]{a GtkTextAttributes to be filled in.}
  @return{TRUE if values was modified}
  @begin{short}
    Computes the effect of any tags applied to this spot in the text. The values
    parameter should be initialized to the default settings you wish to use if
    no tags are in effect. You'd typically obtain the defaults from
    gtk_text_view_get_default_attributes().
  @end{short}

  gtk_text_iter_get_attributes() will modify values, applying the effects of
  any tags present at iter. If any tags affected values, the function returns
  TRUE."
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
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{language in effect at iter}
  A convenience wrapper around gtk_text_iter_get_attributes(), which returns
  the language in effect at iter. If no tags affecting language apply to iter,
  the return value is identical to that of gtk_get_default_language()."
  (gtk-text-iter-language iter))

(export 'gtk-text-iter-get-language)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_end ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-is-end
  :reader "gtk_text_iter_is_end"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-is-end 'function)
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{whether iter is the end iterator}
  Returns TRUE if iter is the end iterator, i.e. one past the last
  dereferenceable iterator in the buffer. gtk_text_iter_is_end() is the most
  efficient way to check whether an iterator is the end iterator.")

(export 'gtk-text-iter-is-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_start ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-text-iter gtk-text-iter-is-start
  :reader "gtk_text_iter_is_start"
  :type :boolean)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-iter-is-start 'function)
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{whether iter is the first in the buffer}
  Returns TRUE if iter is the first iterator in the buffer, that is if iter
  has a character offset of 0.")

(export 'gtk-text-iter-is-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_char" gtk-text-iter-forward-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @return{Whether iter moved and is dereferenceable.}
  Moves iter forward by one character offset. Note that images embedded in the
  buffer occupy 1 character slot, so gtk_text_iter_forward_char() may actually
  move onto an image instead of a character, if you have images in your
  buffer. If iter is the end iterator or one character before it, iter will
  now point at the end iterator, and gtk_text_iter_forward_char() returns
  FALSE for convenience when writing loops."
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_char ()
;;; 
;;; gboolean gtk_text_iter_backward_char (GtkTextIter *iter);
;;; 
;;; Moves backward by one character offset. Returns TRUE if movement was
;;; possible; if iter was the first in the buffer (character offset 0),
;;; gtk_text_iter_backward_char() returns FALSE for convenience when writing
;;; loops.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     whether movement was possible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_chars ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_chars" gtk-text-iter-forward-chars) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{an iterator}
  @argument[count]{number of characters to move, may be negative}
  @return{Whether iter moved and is dereferenceable.}
  Moves count characters if possible (if count would move past the start or
  end of the buffer, moves to the start or end of the buffer). The return
  value indicates whether the new position of iter is different from its
  original position, and dereferenceable (the last iterator in the buffer is
  not dereferenceable). If count is 0, the function does nothing and returns
  FALSE."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_chars ()
;;; 
;;; gboolean gtk_text_iter_backward_chars (GtkTextIter *iter, gint count);
;;; 
;;; Moves count characters backward, if possible (if count would move past the
;;; start or end of the buffer, moves to the start or end of the buffer). The
;;; return value indicates whether the iterator moved onto a dereferenceable
;;; position; if the iterator didn't move, or moved onto the end iterator, then
;;; FALSE is returned. If count is 0, the function does nothing and returns
;;; FALSE.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; count :
;;;     number of characters to move
;;; 
;;; Returns :
;;;     whether iter moved and is dereferenceable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_line ()
;;; 
;;; gboolean gtk_text_iter_forward_line (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the next line. If the iter is already on the last
;;; line of the buffer, moves the iter to the end of the current line. If after
;;; the operation, the iter is at the end of the buffer and not dereferencable,
;;; returns FALSE. Otherwise, returns TRUE.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     whether iter can be dereferenced
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_line ()
;;; 
;;; gboolean gtk_text_iter_backward_line (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the previous line. Returns TRUE if iter could be
;;; moved; i.e. if iter was at character offset 0, this function returns FALSE.
;;; Therefore if iter was already on line 0, but not at the start of the line,
;;; iter is snapped to the start of the line and the function returns TRUE.
;;; (Note that this implies that in a loop calling this function, the line
;;; number may not change on every iteration, if your first iteration is on
;;; line 0.)
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     whether iter moved
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_lines" gtk-text-iter-forward-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of lines to move forward}
  @return{Whether iter moved and is dereferenceable.}
  Moves count lines forward, if possible (if count would move past the start
  or end of the buffer, moves to the start or end of the buffer). The return
  value indicates whether the iterator moved onto a dereferenceable position;
  if the iterator didn't move, or moved onto the end iterator, then FALSE is
  returned. If count is 0, the function does nothing and returns FALSE. If
  count is negative, moves backward by 0 - count lines."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_lines ()
;;; 
;;; gboolean gtk_text_iter_backward_lines (GtkTextIter *iter, gint count);
;;; 
;;; Moves count lines backward, if possible (if count would move past the start
;;; or end of the buffer, moves to the start or end of the buffer). The return
;;; value indicates whether the iterator moved onto a dereferenceable position;
;;; if the iterator didn't move, or moved onto the end iterator, then FALSE is
;;; returned. If count is 0, the function does nothing and returns FALSE. If
;;; count is negative, moves forward by 0 - count lines.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; count :
;;;     number of lines to move backward
;;; 
;;; Returns :
;;;     whether iter moved and is dereferenceable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_word_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_word_ends" gtk-text-iter-forward-word-ends)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of times to move}
  @return{TRUE if iter moved and is not the end iterator}
  Calls gtk_text_iter_forward_word_end() up to count times."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-word-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_word_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_word_starts"
          gtk-text-iter-backward-word-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of times to move}
  @return{TRUE if iter moved and is not the end iterator}
  Calls gtk_text_iter_backward_word_start() up to count times."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-word-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_word_end ()
;;; 
;;; gboolean gtk_text_iter_forward_word_end (GtkTextIter *iter);
;;; 
;;; Moves forward to the next word end. (If iter is currently on a word end,
;;; moves forward to the next one after that.) Word breaks are determined by
;;; Pango and should be correct for nearly any language (if not, the correct fix
;;; would be to the Pango word break algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_word_start ()
;;; 
;;; gboolean gtk_text_iter_backward_word_start (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous word start. (If iter is currently on a word
;;; start, moves backward to the next one after that.) Word breaks are
;;; determined by Pango and should be correct for nearly any language (if not,
;;; the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_position ()
;;; 
;;; gboolean gtk_text_iter_forward_cursor_position (GtkTextIter *iter);
;;; 
;;; Moves iter forward by a single cursor position. Cursor positions are
;;; (unsurprisingly) positions where the cursor can appear. Perhaps
;;; surprisingly, there may not be a cursor position between all characters.
;;; The most common example for European languages would be a carriage
;;; return/newline sequence. For some Unicode characters, the equivalent of say
;;; the letter "a" with an accent mark will be represented as two characters,
;;; first the letter then a "combining mark" that causes the accent to be
;;; rendered; so the cursor can't go between those two characters. See also the
;;; PangoLogAttr structure and pango_break() function.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if we moved and the new position is dereferenceable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_cursor_position ()
;;; 
;;; gboolean gtk_text_iter_backward_cursor_position (GtkTextIter *iter);
;;; 
;;; Like gtk_text_iter_forward_cursor_position(), but moves backward.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if we moved
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_cursor_positions"
          gtk-text-iter-forward-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of positions to move}
  @return{TRUE if we moved and the new position is dereferenceable}
  Moves up to count cursor positions. See
  gtk_text_iter_forward_cursor_position() for details."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_cursor_positions ()
;;; 
;;; gboolean gtk_text_iter_backward_cursor_positions (GtkTextIter *iter,
;;;                                                   gint count);
;;; 
;;; Moves up to count cursor positions. See
;;; gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; count :
;;;     number of positions to move
;;; 
;;; Returns :
;;;     TRUE if we moved and the new position is dereferenceable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_start ()
;;; 
;;; gboolean gtk_text_iter_backward_sentence_start (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous sentence start; if iter is already at the
;;; start of a sentence, moves backward to the next one. Sentence boundaries are
;;; determined by Pango and should be correct for nearly any language (if not,
;;; the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_sentence_starts ()
;;; ----------------------------------------------------------------------------


(defcfun ("gtk_text_iter_backward_sentence_starts"
          gtk-text-iter-backward-sentence-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of sentences to move}
  @return{TRUE if iter moved and is not the end iterator}
  Calls gtk_text_iter_backward_sentence_start() up to count times, or until it
  returns FALSE. If count is negative, moves forward instead of backward."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-sentence-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_sentence_end ()
;;; 
;;; gboolean gtk_text_iter_forward_sentence_end (GtkTextIter *iter);
;;; 
;;; Moves forward to the next sentence end. (If iter is at the end of a
;;; sentence, moves to the next end of sentence.) Sentence boundaries are
;;; determined by Pango and should be correct for nearly any language (if not,
;;; the correct fix would be to the Pango text boundary algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_sentence_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_sentence_ends"
          gtk-text-iter-forward-sentence-ends) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of sentences to move}
  @return{TRUE if iter moved and is not the end iterator}
  Calls gtk_text_iter_forward_sentence_end() count times (or until
  gtk_text_iter_forward_sentence_end() returns FALSE). If count is negative,
  moves backward instead of forward."
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-sentence-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_word_ends ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_word_ends"
          gtk-text-iter-forward-visible-word-ends) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of times to move}
  @return{TRUE if iter moved and is not the end iterator}
  @begin{short}
    Calls gtk_text_iter_forward_visible_word_end() up to count times.
  @end{short}

  Since 2.4"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-word-ends)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_word_starts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_visible_word_starts"
          gtk-text-iter-backward-visible-word-starts) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of times to move}
  @return{TRUE if iter moved and is not the end iterator.}
  @begin{short}
    Calls gtk_text_iter_backward_visible_word_start() up to count times.
  @end{short}

  Since 2.4"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-backward-visible-word-starts)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_word_end ()
;;; 
;;; gboolean gtk_text_iter_forward_visible_word_end (GtkTextIter *iter);
;;; 
;;; Moves forward to the next visible word end. (If iter is currently on a word
;;; end, moves forward to the next one after that.) Word breaks are determined
;;; by Pango and should be correct for nearly any language (if not, the correct
;;; fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_word_start ()
;;; 
;;; gboolean gtk_text_iter_backward_visible_word_start (GtkTextIter *iter);
;;; 
;;; Moves backward to the previous visible word start. (If iter is currently on
;;; a word start, moves backward to the next one after that.) Word breaks are
;;; determined by Pango and should be correct for nearly any language (if not,
;;; the correct fix would be to the Pango word break algorithms).
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if iter moved and is not the end iterator
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_position ()
;;; 
;;; gboolean gtk_text_iter_forward_visible_cursor_position (GtkTextIter *iter);
;;; 
;;; Moves iter forward to the next visible cursor position. See
;;; gtk_text_iter_forward_cursor_position() for details.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_cursor_position ()
;;; 
;;; gboolean gtk_text_iter_backward_visible_cursor_position (GtkTextIter *iter);
;;; 
;;; Moves iter forward to the previous visible cursor position. See
;;; gtk_text_iter_backward_cursor_position() for details.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; Returns :
;;;     TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_cursor_positions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_cursor_positions"
          gtk-text-iter-forward-visible-cursor-positions) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of positions to move}
  @return{TRUE if we moved and the new position is dereferenceable}
  @begin{short}
    Moves up to count visible cursor positions. See
    gtk_text_iter_forward_cursor_position() for details.
  @end{short}

  Since 2.4"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-cursor-positions)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_cursor_positions ()
;;; 
;;; gboolean gtk_text_iter_backward_visible_cursor_positions (GtkTextIter *iter,
;;;                                                           gint count);
;;; 
;;; Moves up to count visible cursor positions. See
;;; gtk_text_iter_backward_cursor_position() for details.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; count :
;;;     number of positions to move
;;; 
;;; Returns :
;;;     TRUE if we moved and the new position is dereferenceable
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_line ()
;;; 
;;; gboolean gtk_text_iter_forward_visible_line (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the next visible line. Returns TRUE if there was
;;; a next line to move to, and FALSE if iter was simply moved to the end of the
;;; buffer and is now not dereferenceable, or if iter was already at the end of
;;; the buffer.
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     whether iter can be dereferenced
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_line ()
;;; 
;;; gboolean gtk_text_iter_backward_visible_line (GtkTextIter *iter);
;;; 
;;; Moves iter to the start of the previous visible line. Returns TRUE if iter
;;; could be moved; i.e. if iter was at character offset 0, this function
;;; returns FALSE. Therefore if iter was already on line 0, but not at the start
;;; of the line, iter is snapped to the start of the line and the function
;;; returns TRUE. (Note that this implies that in a loop calling this function,
;;; the line number may not change on every iteration, if your first iteration
;;; is on line 0.)
;;; 
;;; iter :
;;;     an iterator
;;; 
;;; Returns :
;;;     whether iter moved
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_visible_lines ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_visible_lines"
          gtk-text-iter-forward-visible-lines) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[count]{number of lines to move forward}
  @return{Whether iter moved and is dereferenceable.}
  @begin{short}
    Moves count visible lines forward, if possible (if count would move past the
    start or end of the buffer, moves to the start or end of the buffer). The
    return value indicates whether the iterator moved onto a dereferenceable
    position; if the iterator didn't move, or moved onto the end iterator, then
    FALSE is returned. If count is 0, the function does nothing and returns
    FALSE. If count is negative, moves backward by 0 - count lines.
  @end{short}

  Since 2.8"
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-iter-forward-visible-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_visible_lines ()
;;; 
;;; gboolean gtk_text_iter_backward_visible_lines (GtkTextIter *iter,
;;;                                                gint count);
;;; 
;;; Moves count visible lines backward, if possible (if count would move past
;;; the start or end of the buffer, moves to the start or end of the buffer).
;;; The return value indicates whether the iterator moved onto a dereferenceable
;;; position; if the iterator didn't move, or moved onto the end iterator, then
;;; FALSE is returned. If count is 0, the function does nothing and returns
;;; FALSE. If count is negative, moves forward by 0 - count lines.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; count :
;;;     number of lines to move backward
;;; 
;;; Returns :
;;;     whether iter moved and is dereferenceable
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-offset))

(defun gtk-text-iter-set-offset (iter char-offset)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[char_offset]{a character number}
  Sets iter to point to char_offset. char_offset counts from the start of the
  entire text buffer, starting with 0."
  (setf (gtk-text-iter-offset iter) char-offset))

(export 'gtk-text-iter-set-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-line))

(defun gtk-text-iter-set-line (iter line-number)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[line_number]{line number (counted from 0)}
  Moves iterator iter to the start of the line line_number. If line_number is
  negative or larger than the number of lines in the buffer, moves iter to the
  start of the last line in the buffer."
  (setf (gtk-text-iter-line iter) line-number))

(export 'gtk-text-iter-set-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-line-offset))

(defun gtk-text-iter-set-line-offset (iter char-on-line)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[char_on_line]{a character offset relative to the start of iter's
    current line}
  Moves iter within a line, to a new character (not byte) offset. The given
  character offset must be less than or equal to the number of characters in
  the line; if equal, iter moves to the start of the next line. See
  gtk_text_iter_set_line_index() if you have a byte index rather than a
  character offset."
  (setf (gtk-text-iter-line-offset iter) char-on-line))

(export 'gtk-text-iter-set-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_line_index ()
;;; 
;;; void gtk_text_iter_set_line_index (GtkTextIter *iter, gint byte_on_line);
;;; 
;;; Same as gtk_text_iter_set_line_offset(), but works with a byte index. The
;;; given byte index must be at the start of a character, it can't be in the
;;; middle of a UTF-8 encoded character.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; byte_on_line :
;;;     a byte index relative to the start of iter's current line
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_visible_line_index ()
;;; 
;;; void gtk_text_iter_set_visible_line_index (GtkTextIter *iter,
;;;                                            gint byte_on_line);
;;; 
;;; Like gtk_text_iter_set_line_index(), but the index is in visible bytes, i.e.
;;; text with a tag making it invisible is not counted in the index.
;;; 
;;; iter :
;;;     a GtkTextIter
;;; 
;;; byte_on_line :
;;;     a byte index
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_set_visible_line_offset ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-iter-set-visible-line-offset))

(defun gtk-text-iter-set-visible-line-offset (iter char-on-line)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[char_on_line]{a character offset}
  Like gtk_text_iter_set_line_offset(), but the offset is in visible
  characters, i.e. text with a tag making it invisible is not counted in the
  offset."
  (setf (gtk-text-iter-visisble-line-offset iter) char-on-line))

(export 'gtk-text-iter-set-visible-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_end" gtk-text-iter-forward-to-end) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  Moves iter forward to the \"end iterator\", which points one past the last
  valid character in the buffer. gtk_text_iter_get_char() called on the end
  iterator returns 0, which is convenient for writing loops."
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_line_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_line_end" gtk-text-iter-forward-to-line-end)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @return{TRUE if we moved and the new location is not the end iterator}
  Moves the iterator to point to the paragraph delimiter characters, which
  will be either a newline, a carriage return, a carriage return/newline in
  sequence, or the Unicode paragraph separator character. If the iterator is
  already at the paragraph delimiter characters, moves to the paragraph
  delimiter characters for the next line. If iter is on the last line in the
  buffer, which does not end in paragraph delimiters, moves to the end
  iterator (end of the last line), and returns FALSE."
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-to-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_to_tag_toggle"
          gtk-text-iter-forward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[tag]{a GtkTextTag, or NULL}
  @return{Whether we found a tag toggle after iter.}
  Moves forward to the next toggle (on or off) of the GtkTextTag tag, or to
  the next toggle of any tag if tag is NULL. If no matching tag toggles are
  found, returns FALSE, otherwise TRUE. Does not return toggles located at
  iter, only toggles after iter. Sets iter to the location of the toggle, or
  to the end of the buffer if no toggle is found."
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export 'gtk-text-iter-forward-to-tag-toggle)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_to_tag_toggle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_to_tag_toggle"
          gtk-text-iter-backward-to-tag-toggle) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[tag]{a GtkTextTag, or NULL}
  @return{Whether we found a tag toggle before iter.}
  Moves backward to the next toggle (on or off) of the GtkTextTag tag, or to
  the next toggle of any tag if tag is NULL. If no matching tag toggles are
  found, returns FALSE, otherwise TRUE. Does not return toggles located at
  iter, only toggles before iter. Sets iter to the location of the toggle, or
  the start of the buffer if no toggle is found."
  (iter (g-boxed-foreign gtk-text-iter))
  (tag (g-object gtk-text-tag)))

(export '(gtk-text-iter-backward-to-tag-toggle))

;;; ----------------------------------------------------------------------------
;;; GtkTextCharPredicate ()
;;; 
;;; gboolean (*GtkTextCharPredicate) (gunichar ch, gpointer user_data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-char-predicate :boolean ((char unichar)
                                               (user-data :pointer))
  (let ((function (glib::get-stable-pointer-value user-data)))
    (funcall function char)))

;;; ---------------------------------------------------------------------------- 
;;; gtk_text_iter_forward_find_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_find_char"
          gtk-text-iter-forward-find-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[pred]{a function to be called on each character}
  @argument[user_data]{user data for pred}
  @argument[limit]{search limit, or NULL for none}
  @return{Whether a match was found.}
  Advances iter, calling pred on each character. If pred returns TRUE, returns
  TRUE and stops scanning. If pred never returns TRUE, iter is set to limit if
  limit is non-NULL, otherwise to the end iterator."
  (iter (g-boxed-foreign gtk-text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-find-char)

;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-find-char (iter predicate &key limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (with-stable-pointer (ptr predicate)
    (if (eq direction :forward)
        (gtk-text-iter-forward-find-char iter
                                         (callback gtk-text-char-predicate)
                                         ptr
                                         limit)
        (gtk-text-iter-backward-find-char iter
                                          (callback gtk-text-char-predicate)
                                          ptr
                                          limit))))

(export 'gtk-text-iter-find-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_find_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_find_char" gtk-text-iter-backward-find-char)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[pred]{function to be called on each character}
  @argument[user_data]{user data for pred}
  @argument[limit]{search limit, or NULL for none}
  @return{Whether a match was found.}
  Same as gtk_text_iter_forward_find_char(), but goes backward from iter."
  (iter (g-boxed-foreign gtk-text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign gtk-text-iter)))

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-search-flags atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-search-flags atdoc:*external-symbols*)
 "@version{2013-3-24}
  @short{}
  @begin{pre}
(define-g-flags \"GtkTextSearchFlags\" gtk-text-search-flags
  (:export t
   :type-initializer \"gtk_text_search_flags_get_type\")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_search ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_forward_search" gtk-text-iter-forward-search) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{start of search}
  @argument[str]{a search string}
  @argument[flags]{flags affecting how the search is done}
  @argument[match_start]{return location for start of match, or NULL}
  @argument[match_end]{return location for end of match, or NULL}
  @argument[limit]{bound for the search, or NULL for the end of the buffer}
  @return{Whether a match was found.}
  @begin{short}
    Searches forward for str. Any match is returned by setting match_start to
    the first character of the match and match_end to the first character after
    the match. The search will not continue past limit. Note that a search is a
    linear or O(n) operation, so you may wish to use limit to avoid locking up
    your UI on large buffers.
  @end{short}

  If the GTK_TEXT_SEARCH_VISIBLE_ONLY flag is present, the match may have
  invisible text interspersed in str. i.e. str will be a
  possibly-noncontiguous subsequence of the matched range. similarly, if you
  specify GTK_TEXT_SEARCH_TEXT_ONLY, the match may have pixbufs or child
  widgets mixed inside the matched range. If these flags are not given, the
  match must be exact; the special 0xFFFC character in str will match embedded
  pixbufs or child widgets. If you specify the
  GTK_TEXT_SEARCH_CASE_INSENSITIVE flag, the text will be matched regardless
  of what case it is in."
  (iter (g-boxed-foreign gtk-text-iter))
  (str (:string :free-to-foreign t))
  (flags gtk-text-search-flags)
  (match-start (g-boxed-foreign gtk-text-iter))
  (match-end (g-boxed-foreign gtk-text-iter))
  (limit (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-forward-search)

;;; ----------------------------------------------------------------------------

(defun gtk-text-iter-search (start-position string &key
                                            flags limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (let ((i1 (make-instance 'gtk-text-iter))
        (i2 (make-instance 'gtk-text-iter)))
    (if (if (eq direction :forward)
            (gtk-text-iter-forward-search start-position
                                          string
                                          flags
                                          i1
                                          i2
                                          limit)
            (gtk-text-iter-backward-search start-position
                                           string
                                           flags
                                           i1
                                           i2
                                           limit))
        (values t i1 i2)
        (values nil nil nil))))

(export 'gtk-text-iter-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_search ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_backward_search" gtk-text-iter-backward-search)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter where the search begins}
  @argument[str]{search string}
  @argument[flags]{bitmask of flags affecting the search}
  @argument[match_start]{return location for start of match, or NULL}
  @argument[match_end]{return location for end of match, or NULL}
  @argument[limit]{location of last possible match_start, or NULL for start of
    buffer}
  @return{whether a match was found.}
  Same as gtk_text_iter_forward_search(), but moves backward."
  (iter (g-boxed-foreign gtk-text-iter))
  (str (:string :free-to-foreign t))
  (flags gtk-text-search-flags)
  (match-start (g-boxed-foreign gtk-text-iter))
  (match-end (g-boxed-foreign gtk-text-iter))
  (limit (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-backward-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_equal" gtk-text-iter-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[lhs]{a GtkTextIter}
  @argument[rhs]{another GtkTextIter}
  @return{TRUE if the iterators point to the same place in the buffer.}
  Tests whether two iterators are equal, using the fastest possible mechanism.
  This function is very fast; you can expect it to perform better than e.g.
  getting the character offset for each iterator and comparing the offsets
  yourself. Also, it's a bit faster than gtk_text_iter_compare()."
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_compare" gtk-text-iter-compare) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[lhs]{a GtkTextIter}
  @argument[rhs]{another GtkTextIter}
  @return{-1 if lhs is less than rhs, 1 if lhs is greater, 0 if they are equal}
  A qsort()-style function that returns negative if lhs is less than rhs,
  positive if lhs is greater than rhs, and 0 if they're equal. Ordering is in
  character offset order, i.e. the first character in the buffer is less than
  the second character in the buffer."
  (lhs (g-boxed-foreign gtk-text-iter))
  (rhs (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_in_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_in_range" gtk-text-iter-in-range) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[iter]{a GtkTextIter}
  @argument[start]{start of range}
  @argument[end]{end of range}
  @return{TRUE if iter is in the range}
  Checks whether iter falls in the range [start, end). start and end must be
  in ascending order."
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_order ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_iter_order" gtk-text-iter-order) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-24}
  @argument[first]{a GtkTextIter}
  @argument[second]{another GtkTextIter}
  Swaps the value of first and second if second comes before first in the
  buffer. That is, ensures that first and second are in sequence. Most text
  buffer functions that take a range call this automatically on your behalf,
  so there's no real reason to call it yourself in those cases. There are some
  exceptions, such as gtk_text_iter_in_range(), that expect a pre-sorted
  range."
  (iter-1 (g-boxed-foreign gtk-text-iter))
  (iter-2 (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-iter-order)

;;; --- End of file gtk.text-iter.lisp -----------------------------------------
