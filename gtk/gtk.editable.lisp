;;; ----------------------------------------------------------------------------
;;; gtk.editable.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GtkEditable
;;;
;;;     Interface for text-editing widgets
;;;
;;; Types and Values
;;;
;;;     GtkEditable
;;;
;;; Functions
;;;
;;;     gtk_editable_select_region
;;;     gtk_editable_get_selection_bounds
;;;     gtk_editable_insert_text
;;;     gtk_editable_delete_text
;;;     gtk_editable_get_chars
;;;     gtk_editable_cut_clipboard
;;;     gtk_editable_copy_clipboard
;;;     gtk_editable_paste_clipboard
;;;     gtk_editable_delete_selection
;;;     gtk_editable_set_position
;;;     gtk_editable_get_position
;;;     gtk_editable_set_editable
;;;     gtk_editable_get_editable
;;;
;;; Signals
;;;
;;;     void   changed        Run Last
;;;     void   delete-text    Run Last
;;;     void   insert-text    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkEditable
;;;
;;; Known Implementations
;;;
;;;     GtkEditable is implemented by GtkEntry, GtkSearchEntry and GtkSpinButton
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEditable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkEditable" gtk-editable
  (:export t
   :type-initializer "gtk_editable_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-editable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-editable 'type)
 "@version{2020-6-1}
  @begin{short}
    The @sym{gtk-editable} interface is an interface which should be
    implemented by text editing widgets, such as @class{gtk-entry} and
    @class{gtk-spin-button}. It contains functions for generically manipulating
    an editable widget, a large number of action signals used for key bindings,
    and several signals that an application can connect to to modify the
    behavior of a widget.
  @end{short}
  @begin[Example]{dictionary}
    As an example of the latter usage, by connecting the following handler to
    \"insert-text\", an application can convert all entry into a widget into
    uppercase.

    Forcing entry to uppercase.
    @begin{pre}
 #include <ctype.h>

 void
 insert_text_handler (GtkEditable *editable,
                      const gchar *text,
                      gint         length,
                      gint        *position,
                      gpointer     data)
 {
   gchar *result = g_utf8_strup (text, length);

   g_signal_handlers_block_by_func (editable,
                                (gpointer) insert_text_handler, data);
   gtk_editable_insert_text (editable, result, length, position);
   g_signal_handlers_unblock_by_func (editable,
                                      (gpointer) insert_text_handler, data);

   g_signal_stop_emission_by_name (editable, \"insert_text\");

   g_free (result);
 @}
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (editable)    : Run Last
      @end{pre}
      The \"changed\" signal is emitted at the end of a single user visible
      operation on the contents of the @sym{gtk-editable}.

      E.g., a paste operation that replaces the contents of the selection will
      cause only one signal emission, even though it is implemented by first
      deleting the selection, then inserting the new content, and may cause
      multiple \"notify::text\" signals to be emitted.
      @begin[code]{table}
        @entry[editable]{The @sym{gtk-editable} widget which received the
          signal.}
      @end{table}
    @subheading{The \"delete-text\" signal}
      @begin{pre}
 lambda (editable start-pos end-pos)    : Run Last
      @end{pre}
      This signal is emitted when text is deleted from the widget by the user.
      The default handler for this signal will normally be responsible for
      deleting the text, so by connecting to this signal and then stopping the
      signal with the function @fun{g-signal-stop-emission}, it is possible to
      modify the range of deleted text, or prevent it from being deleted
      entirely. The @arg{start-pos} and @arg{end-pos} parameters are interpreted
      as for the function @fun{gtk-editable-delete-text}.
      @begin[code]{table}
        @entry[editable]{The @sym{gtk-editable} widget which received the
          signal.}
        @entry[start-pos]{An integer with the starting position.}
        @entry[end-pos]{An integer with the end position.}
      @end{table}
    @subheading{The \"insert-text\" signal}
      @begin{pre}
 lambda (editable new-text new-text-length position)    : Run Last
      @end{pre}
      This signal is emitted when text is inserted into the widget by the user.
      The default handler for this signal will normally be responsible for
      inserting the text, so by connecting to this signal and then stopping the
      signal with the function @fun{g-signal-stop-emission}, it is possible to
      modify the inserted text, or prevent it from being inserted entirely.
      @begin[code]{table}
        @entry[editable]{The @sym{gtk-editable} widget which received the
          signal.}
        @entry[new-text]{A string with the new text to insert.}
        @entry[new-text-length]{An integer with the length of the new text, in
          bytes, or -1 if @arg{new-text} is nul-terminated.}
        @entry[position]{A pointer to the position, in characters, at which to
          insert the new text. This is an in-out parameter. After the signal
          emission is finished, it should point after the newly inserted text.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-entry}
  @see-class{gtk-spin-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_editable_select_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_select_region" gtk-editable-select-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @argument[start]{an integer with the start of region}
  @argument[end]{an integer with the end of region}
  @begin{short}
    Selects a region of text.
  @end{short}
  The characters that are selected are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative, then
  the the characters selected are those characters from @arg{start} to the end
  of the text.

  Note that positions are specified in characters, not bytes.
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable))
  (start :int)
  (end :int))

(export 'gtk-editable-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_selection_bounds () -> gtk-editable-selection-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_get_selection_bounds"
          %gtk-editable-get-selection-bounds)
    :boolean
  (editable (g-object gtk-editable))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun gtk-editable-selection-bounds (editable)
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @begin{return}
    @code{selected-p} -- @em{true} if an area is selected, @em{false}
                         otherwise @br{}
    @code{start} -- an integer with the starting position, or @code{nil} @br{}
    @code{end} -- an integer with the end position, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves the selection bound of the editable.
  @end{short}
  @arg{start} will be filled with the start of the selection and @arg{end} with
  end. If no text was selected both will be identical and @code{nil} will be
  returned.

  Note that positions are specified in characters, not bytes.
  @see-class{gtk-editable}"
  (with-foreign-objects ((start :int) (end :int))
    (let ((selected-p (%gtk-editable-get-selection-bounds editable start end)))
      (values selected-p
              (mem-ref start :int)
              (mem-ref end :int)))))

(export 'gtk-editable-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_insert_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_insert_text" %gtk-editable-insert-text) :void
  (editable (g-object gtk-editable))
  (new-text :string)
  (new-text-length :int)
  (position (:pointer :int)))

(defun gtk-editable-insert-text (editable text position)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @argument[new-text]{a string with the text to append}
  @argument[position]{an integer with the position text will be inserted at}
  @return{An integer with the position after the newly inserted text.}
  @begin{short}
    Inserts @arg{new-text} into the contents of the widget, at position
    @arg{position}.
  @end{short}

  Note that @arg{position} is in characters, not in bytes. The function
  returns the position to point after the newly inserted text.
  @see-class{gtk-editable}"
  (with-foreign-object (pos :int)
    (setf (mem-ref pos :int) position)
    (%gtk-editable-insert-text editable text (length text) pos)
    (mem-ref pos :int)))

(export 'gtk-editable-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_delete_text" %gtk-editable-delete-text) :void
  (editable (g-object gtk-editable))
  (start-pos :int)
  (end-pos :int))

(defun gtk-editable-delete-text (editable &key start-pos end-pos)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @argument[start]{an integer with the start position}
  @argument[end]{an integer with the end position}
  @begin{short}
    Deletes a sequence of characters.
  @end{short}
  The characters that are deleted are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative, then
  the characters deleted are those from @arg{start} to the end of the text.

  Note that the positions are specified in characters, not bytes.
  @see-class{gtk-editable}"
  (%gtk-editable-delete-text editable (or start-pos -1) (or end-pos -1)))

(export 'gtk-editable-delete-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_chars () -> gtk-editable-chars
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_get_chars" %gtk-editable-get-chars) g-string
  (editable (g-object gtk-editable))
  (start-pos :int)
  (end-pos :int))

(defun gtk-editable-chars (editable &key (start 0) (end -1))
 #+cl-cffi-gtk-documentation
 "@version{2020-5-30}
  @argument[editable]{a @class{gtk-editable} object}
  @argument[start]{an integer with the start of text}
  @argument[end]{an integer with the end of text}
  @return{A string with the contents of the widget.}
  @begin{short}
    Retrieves a sequence of characters.
  @end{short}
  The characters that are retrieved are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative,
  then the characters retrieved are those characters from @arg{start} to the
  end of the text.

  Note that positions are specified in characters, not bytes.
  @see-class{gtk-editable}"
  (%gtk-editable-get-chars editable start end))

(export 'gtk-editable-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_cut_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_cut_clipboard" gtk-editable-cut-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @begin{short}
    Removes the contents of the currently selected content in the editable and
    puts it on the clipboard.
  @end{short}
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_copy_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_copy_clipboard" gtk-editable-copy-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @begin{short}
    Copies the contents of the currently selected content in the editable and
    puts it on the clipboard.
  @end{short}
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-copy-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_paste_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_paste_clipboard" gtk-editable-paste-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @begin{short}
    Pastes the content of the clipboard to the current position of the cursor
    in the editable.
  @end{short}
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_selection ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_editable_delete_selection" gtk-editable-delete-selection) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[editable]{a @class{gtk-editable} widget}
  @begin{short}
    Deletes the currently selected text of the editable.
  @end{short}
  This call does not do anything if there is no selected text.
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_set_position ()
;;; gtk_editable_get_position () -> gtk-editable-position
;;; ----------------------------------------------------------------------------

(defun (setf gtk-editable-position) (position editable)
  (foreign-funcall "gtk_editable_set_position"
                   (g-object gtk-editable) editable
                   :int position
                   :void)
  position)

(defcfun ("gtk_editable_get_position" gtk-editable-position) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @syntax[]{(gtk-editable-position editable) => position}
  @syntax[]{(setf (gtk-editable-position editable) position)}
  @argument[editable]{a @class{gtk-editable} widget}
  @argument[position]{an integer with the position of the cursor}
  @begin{short}
    Accessor of the cursor position in the editable.
  @end{short}

  The function @sym{gtk-editable-position} retrieves the current position of
  the cursor relative to the start of the content of the editable. The function
  The function @sym{(setf gtk-editable-position)} sets the cursor position in
  the editable to the given value.

  The cursor is displayed before the character with the given (base 0) index
  in the contents of the editable. The value must be less than or equal to the
  number of characters in the editable. A value of -1 indicates that the
  position should be set after the last character of the editable. Note that
  position is in characters, not in bytes.
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-position)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_set_editable ()
;;; gtk_editable_get_editable () -> gtk-editable-editable
;;; ----------------------------------------------------------------------------

(defun (setf gtk-editable-editable) (is-editable editable)
  (foreign-funcall "gtk_editable_set_editable"
                   (g-object gtk-editable) editable
                   :boolean is-editable
                   :void)
  is-editable)

(defcfun ("gtk_editable_get_editable" gtk-editable-editable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @syntax[]{(gtk-editable-editable editable) => is-editable}
  @syntax[]{(setf (gtk-editable-editable editable) is-editable)}
  @argument[editable]{a @class{gtk-editable} widget}
  @argument[is-editable]{@em{true} if the user is allowed to edit the text in
    the widget}
  @begin{short}
    Accessor of the editable property of the editable.
  @end{short}

  The function @sym{gtk-editable-editable} retrieves whether the editable is
  editable. The function @sym{(setf gtk-editable-editable)} determines if the
  user can edit the text in the editable widget or not.
  @see-class{gtk-editable}"
  (editable (g-object gtk-editable)))

(export 'gtk-editable-editable)

;;; --- End of file gtk.editable.lisp ------------------------------------------
