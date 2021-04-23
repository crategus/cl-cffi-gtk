;;; ----------------------------------------------------------------------------
;;; gtk.text-buffer.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; GtkTextBuffer
;;;
;;;     Stores attributed text for display in a GtkTextView
;;;
;;; Types and Values
;;;
;;;     GtkTextBuffer
;;;     GtkTextBufferTargetInfo
;;;
;;; Functions
;;;
;;;     gtk_text_buffer_new
;;;     gtk_text_buffer_get_line_count
;;;     gtk_text_buffer_get_char_count
;;;     gtk_text_buffer_get_tag_table                      Accessor
;;;     gtk_text_buffer_insert
;;;     gtk_text_buffer_insert_at_cursor
;;;     gtk_text_buffer_insert_interactive
;;;     gtk_text_buffer_insert_interactive_at_cursor
;;;     gtk_text_buffer_insert_range
;;;     gtk_text_buffer_insert_range_interactive
;;;     gtk_text_buffer_insert_with_tags
;;;     gtk_text_buffer_insert_with_tags_by_name
;;;     gtk_text_buffer_insert_markup
;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive
;;;     gtk_text_buffer_backspace
;;;     gtk_text_buffer_set_text                           Accessor
;;;     gtk_text_buffer_get_text                           Accessor
;;;     gtk_text_buffer_get_slice
;;;     gtk_text_buffer_insert_pixbuf
;;;     gtk_text_buffer_insert_child_anchor
;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name
;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark
;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_get_has_selection                  Accessor
;;;     gtk_text_buffer_place_cursor
;;;     gtk_text_buffer_select_range
;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag_by_name
;;;     gtk_text_buffer_remove_all_tags
;;;     gtk_text_buffer_create_tag
;;;     gtk_text_buffer_get_iter_at_line_offset
;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line
;;;     gtk_text_buffer_get_iter_at_line_index
;;;     gtk_text_buffer_get_iter_at_mark
;;;     gtk_text_buffer_get_iter_at_child_anchor
;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter
;;;     gtk_text_buffer_get_bounds
;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified
;;;     gtk_text_buffer_delete_selection
;;;     gtk_text_buffer_paste_clipboard
;;;     gtk_text_buffer_copy_clipboard
;;;     gtk_text_buffer_cut_clipboard
;;;     gtk_text_buffer_get_selection_bounds
;;;     gtk_text_buffer_begin_user_action
;;;     gtk_text_buffer_end_user_action
;;;     gtk_text_buffer_add_selection_clipboard
;;;     gtk_text_buffer_remove_selection_clipboard
;;;
;;;     gtk_text_buffer_deserialize
;;;     gtk_text_buffer_deserialize_get_can_create_tags
;;;     gtk_text_buffer_deserialize_set_can_create_tags
;;;     gtk_text_buffer_get_copy_target_list               Accessor
;;;     gtk_text_buffer_get_deserialize_formats
;;;     gtk_text_buffer_get_paste_target_list              Accessor
;;;     gtk_text_buffer_get_serialize_formats
;;;     gtk_text_buffer_register_deserialize_format
;;;     gtk_text_buffer_register_deserialize_tagset
;;;     gtk_text_buffer_register_serialize_format
;;;     gtk_text_buffer_register_serialize_tagset
;;;     gtk_text_buffer_serialize
;;;     gtk_text_buffer_unregister_deserialize_format
;;;     gtk_text_buffer_unregister_serialize_format
;;;
;;; Properties
;;;
;;;       GtkTargetList*  copy-target-list       Read
;;;                gint   cursor-position        Read
;;;            gboolean   has-selection          Read
;;;       GtkTargetList*  paste-target-list      Read
;;;     GtkTextTagTable*  tag-table              Read / Write / Construct Only
;;;               gchar*  text                   Read / Write
;;;
;;; Signals
;;;
;;;                void   apply-tag              Run Last
;;;                void   begin-user-action      Run Last
;;;                void   changed                Run Last
;;;                void   delete-range           Run Last
;;;                void   end-user-action        Run Last
;;;                void   insert-child-anchor    Run Last
;;;                void   insert-pixbuf          Run Last
;;;                void   insert-text            Run Last
;;;                void   mark-deleted           Run Last
;;;                void   mark-set               Run Last
;;;                void   modified-changed       Run Last
;;;                void   paste-done             Run Last
;;;                void   remove-tag             Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextBuffer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextBuffer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextBuffer" gtk-text-buffer
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_text_buffer_get_type")
  ((copy-target-list
    gtk-text-buffer-copy-target-list
    "copy-target-list" "GtkTargetList" t nil)
   (cursor-position
    gtk-text-buffer-cursor-position
    "cursor-position" "gint" t nil)
   (has-selection
    gtk-text-buffer-has-selection
    "has-selection" "gboolean" t nil)
   (paste-target-list
    gtk-text-buffer-paste-target-list
    "paste-target-list" "GtkTargetList" t nil)
   (tag-table
    gtk-text-buffer-tag-table
    "tag-table" "GtkTextTagTable" t nil)
   (text
    gtk-text-buffer-text
    "text" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-buffer 'type)
 "@version{2021-2-14}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"apply-tag\" signal}
      @begin{pre}
 lambda (buffer tag start end)    : Run Last
      @end{pre}
      This signal is emitted to apply a tag to a range of text in a
      @sym{gtk-text-buffer} object. Applying actually occurs in the default
      handler. Note that if your handler runs before the default handler it
      must not invalidate the @arg{start} and @arg{end} iterators, or has to
      revalidate them.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[tag]{The @class{gtk-text-tag} applied tag.}
        @entry[start]{The @class{gtk-text-iter} start iterator of the range
          the tag is applied to.}
        @entry[end]{The @class{gtk-text-iter} end iterator of the range
          the tag is applied to.}
      @end{table}
    @subheading{The \"begin-user-action\" signal}
      @begin{pre}
 lambda (buffer)    : Run Last
      @end{pre}
      This signal is emitted at the beginning of a single user visible
      operation on a @sym{gtk-text-buffer} object.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (buffer)    : Run Last
      @end{pre}
      This signal is emitted when the content of a @sym{gtk-text-buffer} object
      has changed.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"delete-range\" signal}
      @begin{pre}
 lambda (buffer start end)    : Run Last
      @end{pre}
      This signal is emitted to delete a range from a @sym{gtk-text-buffer}
      object. Note that if your handler runs before the default handler it must
      not invalidate the @arg{start} and @arg{end} iterators, or has to
      revalidate them. The default signal handler revalidates the @arg{start}
      and @arg{end} iterators to both point to the location where text was
      deleted. Handlers which run after the default handler do not have access
      to the deleted text.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[start]{The @class{gtk-text-iter} start iterator of the range
          to be deleted.}
        @entry[end]{The @class{gtk-text-iter} end iterator of the range
          to be deleted.}
      @end{table}
    @subheading{The \"end-user-action\" signal}
      @begin{pre}
 lambda (buffer)    : Run Last
      @end{pre}
      This signal is emitted at the end of a single user visible operation on
      the @sym{gtk-text-buffer} object.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"insert-child-anchor\" signal}
      @begin{pre}
 lambda (buffer location anchor)    : Run Last
      @end{pre}
      This signal is emitted to insert a @class{gtk-text-child-anchor} object
      in a @sym{gtk-text-buffer} object. Insertion actually occurs in the
      default handler. Note that if your handler runs before the default handler
      it must not invalidate the @arg{location} iterator, or has to revalidate
      it. The default signal handler revalidates it to be placed after the
      inserted @arg{anchor}.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert
          @arg{anchor} in @arg{buffer}.}
        @entry[anchor]{The @class{gtk-text-child-anchor} object to be inserted.}
      @end{table}
    @subheading{The \"insert-pixbuf\" signal}
      @begin{pre}
 lambda (buffer location pixbuf)    : Run Last
      @end{pre}
      This signal is emitted to insert a @class{gdk-pixbuf} object in a
      @sym{gtk-text-buffer} object. Insertion actually occurs in the default
      handler. Note that if your handler runs before the default handler it
      must not invalidate the @arg{location} iterator, or has to revalidate it.
      The default signal handler revalidates it to be placed after the inserted
      @arg{pixbuf}.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert
          @arg{pixbuf} in @arg{buffer}.}
        @entry[pixbuf]{The @class{gdk-pixbuf} object to be inserted.}
      @end{table}
    @subheading{The \"insert-text\" signal}
      @begin{pre}
 lambda (buffer location text len)    : Run Last
      @end{pre}
      This signal is emitted to insert text in a @sym{gtk-text-buffer} object.
      Insertion actually occurs in the default handler. Note that if your
      handler runs before the default handler it must not invalidate the
      @arg{location} iterator, or has to revalidate it. The default signal
      handler revalidates it to point to the end of the inserted text.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert text in
          @arg{buffer}.}
        @entry[text]{A string with the UTF-8 text to be inserted.}
        @entry[len]{An integer with the length of the inserted text in bytes.}
      @end{table}
    @subheading{The \"mark-deleted\" signal}
      @begin{pre}
 lambda (buffer mark)    : Run Last
      @end{pre}
      This signal is emitted as notification after a @class{gtk-text-mark}
      object is deleted.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[mark]{The @class{gtk-text-mark} object that was deleted.}
      @end{table}
    @subheading{The \"mark-set\" signal}
      @begin{pre}
 lambda (buffer location mark)    : Run Last
      @end{pre}
      This signal is emitted as notification after a @class{gtk-text-mark}
      object is set.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} location of @arg{mark} in
          @arg{buffer}.}
        @entry[mark]{The @class{gtk-text-mark} object that is set.}
      @end{table}
    @subheading{The \"modified-changed\" signal}
      @begin{pre}
 lambda (buffer)    : Run Last
      @end{pre}
      This signal is emitted when the modified bit of a @sym{gtk-text-buffer}
      object flips.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"paste-done\" signal}
      @begin{pre}
 lambda (buffer clipboard)    : Run Last
      @end{pre}
      This signal is emitted after paste operation has been completed. This is
      useful to properly scroll the view to the end of the pasted text.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"remove-tag\" signal}
      @begin{pre}
 lambda (buffer tag start end)    : Run Last
      @end{pre}
      This signal is emitted to remove all occurrences of tag from a range of
      text in a @sym{gtk-text-buffer} object. Removal actually occurs in the
      default handler. Note that if your handler runs before the default handler
      it must not invalidate the @arg{start} and @arg{end} iterators, or has to
      revalidate them.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[tag]{The @class{gtk-text-tag} object to be removed.}
        @entry[start]{The @class{gtk-text-iter} start iterator of the range
          the tag is removed from.}
        @entry[end]{The @class{gtk-text-iter} end iterator of the range
          the tag is removed from.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-text-buffer-copy-target-list}
  @see-slot{gtk-text-buffer-cursor-position}
  @see-slot{gtk-text-buffer-has-selection}
  @see-slot{gtk-text-buffer-paste-target-list}
  @see-slot{gtk-text-buffer-tag-table}
  @see-slot{gtk-text-buffer-text}
  @see-class{gtk-text-view}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-mark}
  @see-class{gtk-text-tab}
  @see-class{gtk-text-tag-table}
  @see-class{gtk-text-child-anchor}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-text-buffer-copy-target-list ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "copy-target-list"
                                               'gtk-text-buffer) 't)
 "The @code{copy-target-list} property of type @class{gtk-target-list}
  (Read) @br{}
  The list of targets this text buffer supports for clipboard copying and as
  DND source.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-copy-target-list atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-copy-target-list 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-buffer-copy-target-list object) => target-list}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[target-list]{a @class{gtk-target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{copy-target-list} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  This function returns the list of targets this text buffer can provide
  for copying and as DND source. The targets in the list are added with info
  values from the @symbol{gtk-text-buffer-target-info} enumeration, using the
  functions @fun{gtk-target-list-add-rich-text-targets} and
  @fun{gtk-target-list-add-text-targets}.
  @see-class{gtk-text-buffer}
  @see-class{gtk-target-list}
  @see-symbol{gtk-text-buffer-target-info}
  @see-function{gtk-target-list-add-text-targets}
  @see-function{gtk-target-list-add-rich-text-targets}")

;;; --- gtk-text-buffer-cursor-position ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-position"
                                               'gtk-text-buffer) 't)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The position of the insert mark, as offset from the beginning of the
  buffer. It is useful for getting notified when the cursor moves. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-cursor-position 'function)
 "@version{*2021-2-14}
  @syntax[]{(gtk-text-buffer-cursor-position object) => position}
  @syntax[]{(setf (gtk-text-buffer-cursor-position object) position)}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[position]{an integer with the position of the insert mark}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{cursor-position} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  The position of the insert mark, as offset from the beginning of the
  buffer. It is useful for getting notified when the cursor moves.
  @see-class{gtk-text-buffer}")

;;; --- gtk-text-buffer-has-selection ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-selection"
                                               'gtk-text-buffer) 't)
 "The @code{has-selection} property of type @code{:boolean} (Read) @br{}
  Whether the buffer has some text currently selected. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-has-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-has-selection 'function)
 "@version{*2021-2-14}
  @syntax[]{(gtk-text-buffer-has-selection object) => has-selection}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[has-selection]{@em{true} if there is text selected}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{has-selection} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  Indicates whether the text buffer has some text currently selected.
  @see-class{gtk-text-buffer}")

;;; --- gtk-text-buffer-paste-target-list --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paste-target-list"
                                               'gtk-text-buffer) 't)
 "The @code{paste-target-list} property of type @class{gtk-target-list}
  (Read) @br{}
  The list of targets this buffer supports for clipboard pasting and as DND
  destination.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-paste-target-list atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-paste-target-list 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-buffer-paste-target-list object) => target-list}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[target-list]{a @class{gtk-target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{paste-target-list} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  This function returns the list of targets this text buffer supports for
  pasting and as DND destination. The targets in the list are added with info
  values from the @symbol{gtk-text-buffer-target-info} enumeration, using the
  functions @fun{gtk-target-list-add-rich-text-targets} and
  @fun{gtk-target-list-add-text-targets}.
  @see-class{gtk-text-buffer}
  @see-class{gtk-target-list}
  @see-symbol{gtk-text-buffer-target-info}
  @see-function{gtk-target-list-add-targets}
  @see-function{gtk-target-list-add-rich-text-targets}")

;;; --- gtk-text-buffer-tag-table ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tag-table" 'gtk-text-buffer) 't)
 "The @code{tag-table} property of type @class{gtk-text-tag-table}
  (Read / Write / Construct) @br{}
  The text tag table associated with the text bufffer.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-tag-table atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-tag-table 'function)
 "@version{*2021-2-21}
  @syntax[]{(gtk-text-buffer-tag-table object) => tag-table}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[tag-table]{a @class{gtk-text-tag-table} object}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{tag-table} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  Gets the text tag table associated with the text buffer.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag-table}")

;;; --- gtk-text-buffer-text ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-text-buffer) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The text content of the text buffer, without child widgets and images. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-text 'function)
 "@version{*2021-2-14}
  @syntax[]{(setf (gtk-text-buffer-text object) text)}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the UTF-8 text to insert}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{text} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  Deletes current contents of the text buffer, and inserts @arg{text} instead.
  The text must be valid UTF-8.
  @begin[Note]{dictionary}
    The @slot[gtk-text-buffer]{text} slot is not readable. Therefore this
    function does not allow to retrieve the text from the text buffer. Use the
    functions @fun{gtk-text-buffer-get-text} or @fun{gtk-text-buffer-get-slice}
    to retrieve the text.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-get-text}
  @see-function{gtk-text-buffer-get-slice}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-buffer-new))

(defun gtk-text-buffer-new (&optional table)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[table]{an optional tag table of type @class{gtk-text-tag-table},
    or no argument to create a new one}
  @return{A new @class{gtk-text-buffer} object.}
  @begin{short}
    Creates a new text buffer.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag-table}"
  (make-instance 'gtk-text-buffer
                 :tag-table table))

(export 'gtk-text-buffer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_line_count () -> gtk-text-buffer-line-count
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_line_count" gtk-text-buffer-line-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{An integer with the number of lines in the text buffer.}
  @begin{short}
    Obtains the number of lines in the text buffer.
  @end{short}
  This value is cached, so the function is very fast.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-char-count}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-line-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_char_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_char_count" gtk-text-buffer-char-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{An integer with the number of characters in the text buffer.}
  @begin{short}
    Gets the number of characters in the text buffer.
  @end{short}

  Note that characters and bytes are not the same, you cannot e.g. expect the
  contents of the buffer in string form to be this many bytes long. The
  character count is cached, so this function is very fast.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-line-count}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-char-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert" %gtk-text-buffer-insert) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (text (:string :free-to-foreign t))
  (len :int))

(defun gtk-text-buffer-insert (buffer text &key (position :cursor)
                                                (interactive nil)
                                                (editable t))
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @syntax[]{(gtk-text-buffer-insert buffer text) => t}
  @syntax[]{(gtk-text-buffer-insert buffer text :position position) => t}
  @syntax[]{(gtk-text-buffer-insert buffer text :interative t) => t}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @argument[position]{a @class{gtk-text-iter} iterator or the default
    @code{:cursor}}
  @argument[interative]{a @code{:boolean} or the default @em{false}}
  @argument[editable]{a @code{:boolean} or the default @em{true}}
  @return{A boolean whether the text was actually inserted.}
  @begin{short}
    Inserts text in the text buffer.
  @end{short}

  The Lisp function @sym{gtk-text-buffer-insert} combines the functions
  @sym{gtk-text-buffer-insert}, @fun{gtk-text-buffer-insert-at-cursor},
  @fun{gtk-text-buffer-insert-interative}, and
  @fun{gtk-text-buffer-insert-interactive-at-cursor} into one function using
  the keyword arguments @arg{position}, @arg{interative}, and @arg{editable}.

  Emits the \"insert-text\" signal; insertion actually occurs in the default
  handler for the signal. The iterator is invalidated when insertion occurs
  (because the buffer contents change), but the default signal handler
  revalidates it to point to the end of the inserted text.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert-at-cursor}
  @see-function{gtk-text-buffer-insert-interactive}
  @see-function{gtk-text-buffer-insert-interactive-at-cursor}"
  (assert (typep position '(or gtk-text-iter (member :cursor))))
  (if interactive
      (if (eq position :cursor)
          (%gtk-text-buffer-insert-interactive-at-cursor buffer
                                                         text
                                                         -1
                                                         editable)
          (%gtk-text-buffer-insert-interactive buffer
                                               position
                                               text
                                               -1
                                               editable))
      (progn
        (if (eq position :cursor)
            (%gtk-text-buffer-insert-at-cursor buffer text -1)
            (%gtk-text-buffer-insert buffer position text -1))
        t))) ; TODO: Return value should be from the functions calls

(export 'gtk-text-buffer-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_at_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_at_cursor" %gtk-text-buffer-insert-at-cursor)
    :void
  (buffer (g-object gtk-text-buffer))
  (text (:string :free-to-foreign t))
  (len :int))

(defun gtk-text-buffer-insert-at-cursor (buffer text)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @begin{short}
    Calls the function @fun{gtk-text-buffer-insert}, using the current cursor
    position as the insertion point.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert}"
  (%gtk-text-buffer-insert-at-cursor buffer text -1))

(export 'gtk-text-buffer-insert-at-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_interactive"
          %gtk-text-buffer-insert-interactive) :boolean
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (text (:string :free-to-foreign t))
  (len :int)
  (editable :boolean))

(defun gtk-text-buffer-insert-interactive (buffer iter text editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[text]{a string with some UTF-8 text}
  @argument[editable]{default editability of the text buffer}
  @return{A boolean whether the text was actually inserted.}
  @begin{short}
    Like the function @fun{gtk-text-buffer-insert}, but the insertion will not
    occur if the iterator is at a non-editable location in the text buffer.
  @end{short}
  Usually you want to prevent insertions at ineditable locations if the
  insertion results from a user action (is interactive).

  The argument @arg{editable} indicates the editability of text that does not
  have a tag affecting editability applied to it. Typically the result of the
  function @fun{gtk-text-view-editable} is appropriate here.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-view-editable}"
  (%gtk-text-buffer-insert-interactive buffer iter text -1 editable))

(export 'gtk-text-buffer-insert-interactive)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive_at_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_interactive_at_cursor"
          %gtk-text-buffer-insert-interactive-at-cursor) :boolean
  (buffer (g-object gtk-text-buffer))
  (text (:string :free-to-foreign t))
  (len :int)
  (editable :boolean))

(defun gtk-text-buffer-insert-interactive-at-cursor (buffer text editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @argument[editable]{default editability of @arg{buffer}}
  @return{A boolean whether the text was actually inserted.}
  @begin{short}
    Calls the function @fun{gtk-text-buffer-insert-interactive} at the cursor
    position.
  @end{short}

  The argument @arg{editable} indicates the editability of text that does not
  have a tag affecting editability applied to it. Typically the result of the
  function @fun{gtk-text-view-editable} is appropriate here.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert-interactive}
  @see-function{gtk-text-view-editable}"
  (%gtk-text-buffer-insert-interactive-at-cursor buffer text -1 editable))

(export 'gtk-text-buffer-insert-interactive-at-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_range" %gtk-text-buffer-insert-range) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (range-start (g-boxed-foreign gtk-text-iter))
  (range-end (g-boxed-foreign gtk-text-iter)))

;; TODO: Update the documentation for the keyword arguments

(defun gtk-text-buffer-insert-range (buffer iter start end
                                     &key interactive editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a position in @arg{buffer} object}
  @argument[start]{a position in a @class{gtk-text-buffer} object}
  @argument[end]{another position in the same buffer as start}
  @begin{short}
    Copies text, tags, and pixbufs between @arg{start} and @arg{end} (the order
    of @arg{start} and @arg{end} does not matter) and inserts the copy at
    the iterator.
  @end{short}

  Used instead of simply getting/inserting text because it preserves images and
  tags. If @arg{start} and @arg{end} are in a different buffer from
  @arg{buffer}, the two buffers must share the same tag table.

  Implemented via emissions of the \"insert-text\" and \"apply-tag\" signals,
  so expect those.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert-range-interactive}"
  (if interactive
      (gtk-text-buffer-insert-range-interactive buffer
                                                iter
                                                start
                                                end
                                                editable)
      (progn
        (%gtk-text-buffer-insert-range buffer iter start end)
        t)))

(export 'gtk-text-buffer-insert-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range_interactive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_range_interactive"
           gtk-text-buffer-insert-range-interactive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[start]{a @class{gtk-text-iter} with a position in the text buffer}
  @argument[end]{a @class{gtk-text-iter} with another position in the same
    buffer as @arg{start}}
  @argument[editable]{a @code{:boolean} with the default editability of the
    text buffer}
  @return{A boolean whether an insertion was possible at the iterator.}
  @begin{short}
    Same as the function @fun{gtk-text-buffer-insert-range}, but does nothing
    if the insertion point is not editable.
  @end{short}

  The @arg{editable} parameter indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the function @fun{gtk-text-view-editable} is appropriate here.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert-range}
  @see-function{gtk-text-view-editable}"
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

(export 'gtk-text-buffer-insert-range-interactive)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-buffer-insert-with-tags (buffer iter text &rest tags)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator in @arg{buffer}}
  @argument[text]{a string with the UTF-8 text}
  @argument[tags]{the @class{gtk-text-tag} objects to apply to @arg{text}}
  @begin{short}
    Inserts text into the text buffer at the position @arg{iter}, applying the
    list of tags to the newly inserted text.
  @end{short}

  Equivalent to calling the function @fun{gtk-text-buffer-insert}, then
  the function @fun{gtk-text-buffer-apply-tag} on the inserted text. The
  function @sym{gtk-text-buffer-insert-with-tags} is just a convenience
  function.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-buffer-insert}
  @see-function{gtk_text-buffer-applay-tag}"
  (let ((offset (gtk-text-iter-offset iter)))
    (prog1
      (gtk-text-buffer-insert buffer text :position iter)
      (let ((start-iter (gtk-text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (gtk-text-buffer-apply-tag buffer tag start-iter iter))))))

(export 'gtk-text-buffer-insert-with-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags_by_name ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-buffer-insert-with-tags-by-name (buffer iter text &rest tags)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator in @arg{buffer}}
  @argument[text]{a string with the UTF-8 text}
  @argument[tags]{strings with the tag names to apply to @arg{text}}
  @begin{short}
    Same as the function @fun{gtk-text-buffer-insert-with-tags}, but allows you
    to pass in tag names instead of tag objects.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-insert-with-tags}"
  (let ((offset (gtk-text-iter-offset iter)))
    (prog1
      (gtk-text-buffer-insert buffer text :position iter)
      (let ((start-iter (gtk-text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (gtk-text-buffer-apply-tag-by-name buffer tag start-iter iter))))))

(export 'gtk-text-buffer-insert-with-tags-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_markup" %gtk-text-buffer-insert-markup) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (markup g-string)
  (len :int))

(defun gtk-text-buffer-insert-markup (buffer iter markup)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-15}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[markup]{a UTF-8 string containing Pango markup}
  @begin{short}
    Inserts the text in @arg{markup} at the position of the iterator.
  @end{short}
  The text @arg{markup} will be inserted in its entirety and must be valid
  UTF-8. Emits the \"insert-text\" signal, possibly multiple times; insertion
  actually occurs in the default handler for the signal. The iterator will
  point to the end of the inserted text on return.
  @see-class{gtk-text-buffer}"
  (%gtk-text-buffer-insert-markup buffer iter markup -1))

(export 'gtk-text-buffer-insert-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete" %gtk-text-buffer-delete) :void
  (buffer (g-object gtk-text-buffer))
  (range-start (g-boxed-foreign gtk-text-iter))
  (range-end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-delete (buffer start end &key interactive editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} position in @arg{buffer}}
  @argument[end]{another @class{gtk-text-iter} position in @arg{buffer}}
  @begin{short}
    Deletes text between @arg{start} and @arg{end}.
  @end{short}
  The order of @arg{start} and @arg{end} is not actually relevant. the function
  @sym{gtk-text-buffer-delete} will reorder them. This function actually emits
  the \"delete-range\" signal, and the default handler of that signal deletes
  the text. Because the buffer is modified, all outstanding iterators become
  invalid after calling this function. However, the @arg{start} and @arg{end}
  interators will be re-initialized to point to the location where text was
  deleted.
  @see-class{gtk-text-buffer}"
  (if interactive
      (gtk-text-buffer-delete-interactive buffer
                                          start
                                          end
                                          editable)
      (progn
        (%gtk-text-buffer-delete buffer start end)
        t)))

(export 'gtk-text-buffer-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_interactive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_interactive"
          gtk-text-buffer-delete-interactive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start-iter]{a @class{gtk-text-iter} start of range to delete}
  @argument[end-iter]{a @class{gtk-text-iter} end of range}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{A boolean whether some text was actually deleted.}
  @begin{short}
    Deletes all editable text in the given range.
  @end{short}
  Calls the function @fun{gtk-text-buffer-delete} for each editable sub range
  of [@arg{start}, @arg{end}). @arg{start} and @arg{end} are revalidated to
  point to the location of the last deleted range, or left untouched if no
  text was deleted.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-delete}"
  (buffer (g-object gtk-text-buffer))
  (start-iter (g-boxed-foreign gtk-text-iter))
  (end-iter (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

(export 'gtk-text-buffer-delete-interactive)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_backspace ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_backspace" %gtk-text-buffer-backspace) :boolean
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (interactive :boolean)
  (editable :boolean))

(defun gtk-text-buffer-backspace (buffer iter &key interactive editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} position in @arg{buffer}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the @arg{buffer} is editable by default}
  @return{@em{True} if the buffer was modified.}
  @begin{short}
    Performs the appropriate action as if the user hit the delete key with the
    cursor at the position specified by @arg{iter}.
  @end{short}
  In the normal case a single character will be deleted, but when combining
  accents are involved, more than one character can be deleted, and when
  precomposed character and accent combinations are involved, less than one
  character will be deleted.

  Because the buffer is modified, all outstanding iterators become invalid
  after calling this function; however, the iter will be re-initialized to
  point to the location where text was deleted.
  @see-class{gtk-text-buffer}"
  (%gtk-text-buffer-backspace buffer iter interactive editable))

(export 'gtk-text-buffer-backspace)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_text" gtk-text-buffer-get-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start iterator of a range}
  @argument[end]{a @class{gtk-text-iter} end iterator of a range}
  @argument[include-hidden-chars]{a boolean whether to include invisible text}
  @return{An allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}

  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if @arg{include-hidden-chars} is @em{false}. Does not include
  characters representing embedded images, so byte and character indexes into
  the returned string do not correspond to byte and character indexes into the
  buffer. Contrast with the function @fun{gtk-text-buffer-get-slice}.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-text}
  @see-function{gtk-text-buffer-get-slice}"
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (include-hidden-chars :boolean))

(export 'gtk-text-buffer-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_slice" %gtk-text-buffer-get-slice)
    (:string :free-from-foreign t)
  (buffer (g-object gtk-text-buffer))
  (range-start (g-boxed-foreign gtk-text-iter))
  (range-end (g-boxed-foreign gtk-text-iter))
  (include-hidden-chars :boolean))

(defun gtk-text-buffer-get-slice (buffer start end &key include-hidden-chars)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start of a range}
  @argument[end]{a @class{gtk-text-iter} end of a range}
  @argument[include-hidden-chars]{a boolean whether to include invisible text}
  @return{An allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if @arg{include-hidden-chars} is @em{false}.

  The returned string includes a @code{0xFFFC} character whenever the buffer
  contains embedded images, so byte and character indexes into the returned
  string do correspond to byte and character indexes into the buffer. Contrast
  with the function @fun{gtk-text-buffer-get-text}. Note that @code{0xFFFC} can
  occur in normal text as well, so it is not a reliable indicator that a pixbuf
  or widget is in the buffer.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-get-text}"
  (%gtk-text-buffer-get-slice buffer
                              start
                              end
                              include-hidden-chars))

(export 'gtk-text-buffer-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_pixbuf" gtk-text-buffer-insert-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-21}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location to insert the pixbuf}
  @argument[pixbuf]{a @class{gdk-pixbuf} structure}
  @begin{short}
    Inserts an image into the text buffer at @arg{iter}.
  @end{short}
  The image will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the buffer as a string include this
  character for pixbufs, but the \"text\" variants do not. E.g. see the
  functions @fun{gtk-text-buffer-get-slice} and @fun{gtk-text-buffer-get-text}.
  @see-class{gtk-text-buffer}
  @see-class{gdk-pixbuf}
  @see-function{gtk-text-buffer-get-slice}
  @see-function{gtk-text-buffer-get-text}"
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-text-buffer-insert-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_child_anchor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_child_anchor"
          %gtk-text-buffer-insert-child-anchor) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (anchor (g-object gtk-text-child-anchor)))

(defun gtk-text-buffer-insert-child-anchor (buffer position &optional anchor)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location to insert the anchor}
  @argument[anchor]{a @class{gtk-text-child-anchor} object}
  @begin{short}
    Inserts a child widget anchor into the text buffer at iter.
  @end{short}
  The anchor will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the buffer as a string include this
  character for child anchors, but the \"text\" variants do not. e.g. see the
  functions @fun{gtk-text-buffer-get-slice} and @fun{gtk-text-buffer-get-text}.
  Consider the function @fun{gtk-text-buffer-create-child-anchor} as a more
  convenient alternative to this function. The buffer will add a reference to
  the anchor, so you can unref it after insertion.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-get-slice}
  @see-function{gtk-text-buffer-get-text}
  @see-function{gtk-text-buffer-create-child-anchor}"
  (if anchor
      (progn
        (%gtk-text-buffer-insert-child-anchor buffer position anchor)
        anchor)
      (gtk-text-buffer-create-child-anchor buffer position)))

(export 'gtk-text-buffer-insert-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_child_anchor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_create_child_anchor"
          gtk-text-buffer-create-child-anchor) (g-object gtk-text-child-anchor)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location in the @arg{buffer}}
  @return{The created child anchor of type @class{gtk-text-child-anchor}.}
  @begin{short}
    This is a convenience function which simply creates a child anchor with the
    function @fun{gtk-text-child-anchor-new} and inserts it into the
    @arg{buffer} with the function @fun{gtk-text-buffer-insert-child-anchor}.
  @end{short}
  The new anchor is owned by the buffer. No reference count is returned to the
  caller of the function @sym{gtk-text-buffer-create-child-anchor}.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-child-anchor}
  @see-function{gtk-text-child-anchor-new}
  @see-function{gtk-text-buffer-insert-child-anchor}"
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-create-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_create_mark" %gtk-text-buffer-create-mark)
    (g-object gtk-text-mark)
  (buffer (g-object gtk-text-buffer))
  (mark-name (:string :free-to-foreign t))
  (where (g-boxed-foreign gtk-text-iter))
  (left-gravity :boolean))

(defun gtk-text-buffer-create-mark (buffer mark-name pos
                                    &optional (left-gravity t))
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark-name]{a string with the name for the mark, or @code{nil}}
  @argument[pos]{a @class{gtk-text-iter} location to place the mark}
  @argument[left-gravity]{a boolean whether the mark has left gravity}
  @return{The new @class{gtk-text-mark} object.}
  @begin{short}
    Creates a mark at position @arg{pos}.
  @end{short}
  If @arg{mark-name} is @code{nil}, the mark is anonymous. Otherwise, the mark
  can be retrieved by name using the function @fun{gtk-text-buffer-mark}.
  If a mark has left gravity, and text is inserted at the mark's current
  location, the mark will be moved to the left of the newly inserted text. If
  the mark has right gravity (@arg{left-gravity} = @em{false}), the mark will
  end up on the right of newly inserted text. The standard left-to-right cursor
  is a mark with right gravity (when you type, the cursor stays on the right
  side of the text you are typing).

  The caller of this function does not own a reference to the returned
  @class{gtk-text-mark}, so you can ignore the return value if you like. Marks
  are owned by the buffer and go away when the buffer does.

  Emits the \"mark-set\" signal as notification of the mark's initial
  placement.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-mark}"
  (%gtk-text-buffer-create-mark buffer mark-name pos left-gravity))

(export 'gtk-text-buffer-create-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_move_mark" %gtk-text-buffer-move-mark) :void
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark))
  (position (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-move-mark (buffer mark position)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object}
  @argument[where]{new @class{gtk-text-iter} location for @arg{mark} in
    @arg{buffer}}
  @begin{short}
    Moves @arg{mark} to the new location @arg{where}.
  @end{short}
  Emits the \"mark-set\" signal as notification of the move.
  @see-class{gtk-text-buffer}"
  (etypecase mark
    (string (gtk-text-buffer-move-mark-by-name buffer mark position))
    (gtk-text-mark (%gtk-text-buffer-move-mark buffer mark position))))

(export 'gtk-text-buffer-move-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_move_mark_by_name" gtk-text-buffer-move-mark-by-name)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a mark}
  @argument[where]{new @class{gtk-text-iter} location for mark}
  @begin{short}
    Moves the mark named @arg{name} (which must exist) to location @arg{where}.
  @end{short}
  See the function @fun{gtk-text-buffer-move-mark} for details.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t))
  (position (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-move-mark-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_add_mark" gtk-text-buffer-add-mark) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-3-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} with the mark to add}
  @argument[position]{a @class{gtk-text-iter} with the location to place
    @arg{mark}}
  @begin{short}
    Adds the mark at the given position.
  @end{short}
  The mark must not be added to another buffer, and if its name is not
  @code{nil} then there must not be another mark in the buffer with the same
  name.

  Emits the \"mark-set\" signal as notification of the mark's initial placement.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-class{gtk-text-iter}"
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark))
  (position (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_mark" %gtk-text-buffer-delete-mark) :void
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark)))

(defun gtk-text-buffer-delete-mark (buffer mark)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object in @arg{buffer}}
  @begin{short}
    Deletes @arg{mark}, so that it is no longer located anywhere in the
    @arg{buffer}.
  @end{short}
  Removes the reference the buffer holds to the mark, so if you have not called
  the function @fun{g-object-ref} on the mark, it will be freed. Even if the
  mark is not freed, most operations on mark become invalid, until it gets added
  to a buffer again with the function @fun{gtk-text-buffer-add-mark}. Use the
  function @fun{gtk-text-mark-deleted} to find out if a mark has been removed
  from its buffer. The \"mark-deleted\" signal will be emitted as notification
  after the mark is deleted.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-function{g-object-ref}
  @see-fun{gtk-text-buffer-add-mark}
  @see-fun{gtk-text-mark-deleted}"
  (etypecase mark
    (string (gtk-text-buffer-delete-mark-by-name buffer mark))
    (gtk-text-mark (%gtk-text-buffer-delete-mark buffer mark))))

(export 'gtk-text-buffer-delete-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_mark_by_name"
          gtk-text-buffer-delete-mark-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a mark in @arg{buffer}}
  @begin{short}
    Deletes the mark named @arg{name}.
  @end{short}
  The mark must exist. See the function @fun{gtk-text-buffer-delete-mark} for
  details.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-delete-mark}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t)))

(export 'gtk-text-buffer-delete-mark-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_mark () -> gtk-text-buffer-mark
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_mark" gtk-text-buffer-mark)
    (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with a mark name}
  @return{A @class{gtk-text-mark} object, or @code{nil}.}
  @begin{short}
    Returns the mark named @arg{name} in buffer @arg{buffer}, or @code{nil} if
    no such mark exists in the @arg{buffer}.
  @end{short}
  @see-class{gtk-text-buffer}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t)))

(export 'gtk-text-buffer-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_insert ()
;;; ----------------------------------------------------------------------------

;; It is wrong to implent this as gtk-text-buffer-insert, we have already a
;; function with this name.

(defcfun ("gtk_text_buffer_get_insert" gtk-text-buffer-get-insert)
    (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A @class{gtk-text-mark} insertion point mark.}
  @begin{short}
    Returns the mark that represents the cursor (insertion point).
  @end{short}
  Equivalent to calling the function @fun{gtk-text-buffer-mark} to get the mark
  named \"insert\", but very slightly more efficient, and involves less typing.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-mark}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-get-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bound () -> gtk-text-buffer-selection-bound
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_selection_bound"
           gtk-text-buffer-selection-bound) (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{Selection bound mark of type @class{gtk-text-mark}.}
  @begin{short}
    Returns the mark that represents the selection bound.
  @end{short}
  Equivalent to calling the function @fun{gtk-text-buffer-mark} to get the mark
  named \"selection_bound\", but very slightly more efficient, and involves less
  typing.

  The currently selected text in buffer is the region between the
  \"selection_bound\" and \"insert\" marks. If \"selection_bound\" and
  \"insert\" are in the same place, then there is no current selection. The
  function @fun{gtk-text-buffer-selection-bounds} is another convenient
  function for handling the selection, if you just want to know whether there
  is a selection and what its bounds are.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-function{gtk-text-buffer-mark}
  @see-function{gtk-text-buffer-selection-bounds}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-selection-bound)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_place_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_place_cursor" gtk-text-buffer-place-cursor) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[position]{a @class{gtk-text-iter} where to put the cursor}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them to the same place in two steps with the function
  @fun{gtk-text-buffer-move-mark}, you will temporarily select a region in
  between their old and new locations, which can be pretty inefficient since
  the temporarily-selected region will force stuff to be recalculated. This
  function moves them as a unit, which can be optimized.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (position (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-place-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_select_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_select_range" gtk-text-buffer-select-range) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[insertion-point]{a @class{gtk-text-iter} where to put the \"insert\"
    mark}
  @argument[bound]{a @class{gtk-text-iter} where to put the \"selection_bound\"
    mark}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them in two steps with the function
  @fun{gtk-text-buffer-move-mark}, you will temporarily select a region in
  between their old and new locations, which can be pretty inefficient since
  the temporarily-selected region will force stuff to be recalculated. This
  function moves them as a unit, which can be optimized.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (insertion-point (g-boxed-foreign gtk-text-iter))
  (selection-bound (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_apply_tag" %gtk-text-buffer-apply-tag) :void
  (buffer (g-object gtk-text-buffer))
  (tag (g-object gtk-text-tag))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-apply-tag (buffer tag start end)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  @argument[start]{a @class{gtk-text-iter} iterator with one bound of range
    to be tagged}
  @argument[end]{a @class{gtk-text-iter} iterator with other bound of range
    to be tagged}
  @begin{short}
    Emits the \"apply-tag\" signal on text buffer.
  @end{short}
  The default handler for the signal applies @arg{tag} to the given range.
  The @arg{start} and @arg{end} iterators do not have to be in order.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}"
  (etypecase tag
    (string (gtk-text-buffer-apply-tag-by-name buffer tag start end))
    (gtk-text-tag (%gtk-text-buffer-apply-tag buffer tag start end))))

(export 'gtk-text-buffer-apply-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_tag" %gtk-text-buffer-remove-tag) :void
  (buffer (g-object gtk-text-buffer))
  (tag (g-object gtk-text-tag))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-remove-tag (buffer tag start end)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tag]{a @class{gtk-text-tag} object}
  @argument[start]{a @class{gtk-text-iter} with one bound of range to be
    untagged}
  @argument[end]{a @class{gtk-text-iter} with other bound of range to be
    untagged}
  @begin{short}
    Emits the \"remove-tag\" signal.
  @end{short}
  The default handler for the signal removes all occurrences of @arg{tag} from
  the given range. @arg{start} and @arg{end} do not have to be in order.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}"
  (etypecase tag
    (string (gtk-text-buffer-remove-tag-by-name buffer tag start end))
    (gtk-text-tag (%gtk-text-buffer-remove-tag buffer tag start end))))

(export 'gtk-text-buffer-remove-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_apply_tag_by_name"
          gtk-text-buffer-apply-tag-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a named @class{gtk-text-tag} object}
  @argument[start]{a @class{gtk-text-iter} with one bound of range to be tagged}
  @argument[end]{a @class{gtk-text-iter} with other bound of range to be tagged}
  @begin{short}
    Calls the function @fun{gtk-text-tag-table-lookup} on the text buffer's tag
    table to get a @class{gtk-text-tag} object, then calls the function
    @fun{gtk-text-buffer-apply-tag}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-tag-table-lookup}
  @see-function{gtk-text-buffer-apply-tag}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-apply-tag-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_tag_by_name"
          gtk-text-buffer-remove-tag-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a @class{gtk-text-tag} object}
  @argument[start]{a @class{gtk-text-iter} iterator with one bound of range
    to be untagged}
  @argument[end]{a @class{gtk-text-iter} iterator with other bound of range
    to be untagged}
  @begin{short}
    Calls the function @fun{gtk-text-tag-table-lookup} on the text buffer's tag
    table to get a @class{gtk-text-tag} object, then calls the function
    @fun{gtk-text-buffer-remove-tag}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-tag-table-lookup}
  @see-function{gtk-text-buffer-remove-tag}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-remove-tag-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_all_tags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_all_tags" gtk-text-buffer-remove-all-tags)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} with one bound of range to be
    untagged}
  @argument[end]{a @class{gtk-text-iter} with other bound of range to be
    untagged}
  @begin{short}
    Removes all tags in the range between @arg{start} and @arg{end}.
  @end{short}
  Be careful with this function; it could remove tags added in code unrelated to
  the code you are currently writing. That is, using this function is probably a
  bad idea if you have two or more unrelated code sections that add tags.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-remove-all-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_tag ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-buffer-create-tag (buffer name &rest args)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of the new tag, or @code{nil}}
  @argument[args]{list of property keywords and values}
  @return{The new @class{gtk-text-tag} object.}
  @begin{short}
    Creates a tag and adds it to the tag table for the text buffer.
  @end{short}
  Equivalent to calling the function @fun{gtk-text-tag-new} and then adding the
  tag to the text buffer's tag table.

  If @arg{name} is @code{nil}, the tag is anonymous. If @arg{name} is
  non-@code{nil}, a tag called @arg{name} must not already exist in the tag
  table for this buffer.

  The argument @arg{args} is a list of properties and values to set on the
  tag.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-tag-new}"
  (let ((tag (apply #'make-instance (list* 'gtk-text-tag :name name args))))
    (when (gtk-text-tag-table-add (gtk-text-buffer-tag-table buffer) tag)
      tag)))

(export 'gtk-text-buffer-create-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_offset ()
;;;     -> gtk-text-buffer-iter-at-line-offset
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_line_offset"
          %gtk-text-buffer-iter-at-line-offset) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (line-number :int)
  (char-offset :int))

(defun gtk-text-buffer-iter-at-line-offset (buffer line-number char-offset)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-12}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{@class{gtk-text-iter} iterator to initialize}
  @argument[line-number]{@code{:int} line number counting from 0}
  @argument[char-offset]{@code{:int} char offset from start of line}
  @begin{short}
    Obtains an iterator pointing to @arg{char-offset} within the given line.
  @end{short}
  Note characters, not bytes; UTF-8 may encode one character as multiple bytes.

  Before the 3.20 version, it was not allowed to pass an invalid location.

  Since the 3.20 version, if @arg{line-number} is greater than the number of
  lines in the buffer, the end iterator is returned. And if @arg{char-offset} is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line-offset buffer
                                          iter
                                          line-number
                                          char-offset)
    iter))

(export 'gtk-text-buffer-iter-at-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_offset () -> gtk-text-buffer-iter-at-offset
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_offset" %gtk-text-buffer-iter-at-offset)
    :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (char-offset :int))

(defun gtk-text-buffer-iter-at-offset (buffer offset)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[offset]{an integer with the char offset from start of the text
    buffer, counting from 0, or -1}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to a position @arg{offset} chars from the
    start of the entire text buffer.
  @end{short}
  If @arg{offset} is -1 or greater than the number of characters in the text
  buffer, the iterator is initialized to the end iterator, the iterator one
  past the last valid character in the text buffer.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-offset buffer iter offset)
    iter))

(export 'gtk-text-buffer-iter-at-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line () -> gtk-text-buffer-iter-at-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_line" %gtk-text-buffer-iter-at-line)
    :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (line-number :int))

(defun gtk-text-buffer-iter-at-line (buffer line-number)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[line-number]{an integer with the line number counting from 0}
  @return{iter -- a @class{gtk-text-iter} iterator to initialize.}
  @begin{short}
    Initializes @arg{iter} to the start of the given line.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line buffer iter line-number)
    iter))

(export 'gtk-text-buffer-iter-at-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_index ()
;;; -> gtk-text-buffer-iter-at-line-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_line_index"
          %gtk-text-buffer-iter-at-line-index) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (line-number :int)
  (byte-index :int))

(defun gtk-text-buffer-iter-at-line-index (buffer line-number byte-index)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[line-number]{an integer with the line number counting from 0}
  @argument[byte-index]{an integer with the byte index from start of line}
  @return{iter -- a @class{gtk-text-iter} iterator to initialize.}
  @begin{short}
    Obtains an iterator pointing to @arg{byte-index} within the given line.
  @end{short}
  @arg{byte-index} must be the start of a UTF-8 character. Note bytes, not
  characters; UTF-8 may encode one character as multiple bytes.

  Before the 3.20 version, it was not allowed to pass an invalid location.

  Since the 3.20 version, if @arg{line-number} is greater than the number of
  lines in the buffer, the end iterator is returned. And if @arg{byte-index} is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line-index buffer iter line-number byte-index)
    iter))

(export 'gtk-text-buffer-iter-at-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_mark () -> gtk-text-buffer-iter-at-mark
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_mark" %gtk-text-buffer-iter-at-mark)
    :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (mark (g-object gtk-text-mark)))

(defun gtk-text-buffer-iter-at-mark (buffer mark)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object in @arg{buffer}}
  @return{iter -- iterator to initialize}
  @begin{short}
    Initializes @arg{iter} with the current position of @arg{mark}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}"
  (when (stringp mark)
    (setf mark (gtk-text-buffer-mark buffer mark)))
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-mark buffer iter mark)
    iter))

(export 'gtk-text-buffer-iter-at-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_child_anchor ()
;;; -> gtk-text-buffer-iter-at-child-anchor
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_child_anchor"
          %gtk-text-buffer-iter-at-child-anchor) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (anchor (g-object gtk-text-child-anchor)))

(defun gtk-text-buffer-iter-at-child-anchor (buffer anchor)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[anchor]{a child anchor that appears in @arg{buffer}}
  @return{iter -- an iterator to be initialized}
  @begin{short}
    Obtains the location of @arg{anchor} within @arg{buffer}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-child-anchor}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-child-anchor buffer iter anchor)
    iter))

(export 'gtk-text-buffer-iter-at-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_start_iter () -> gtk-text-buffer-start-iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_start_iter" %gtk-text-buffer-start-iter) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-start-iter (buffer)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{Returns a @class{gtk-text-iter} iterator.}
  @begin{short}
    Returns an iterator with the first position in the text buffer.
  @end{short}
  This is the same as using the function @fun{gtk-text-buffer-iter-at-offset}
  to get the itererator at character offset 0.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-iter-at-offset}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-start-iter buffer iter)
    iter))

(export 'gtk-text-buffer-start-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_end_iter () --> gtk-text-buffer-end-iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_end_iter" %gtk-text-buffer-end-iter) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-end-iter (buffer)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{Returns a @class{gtk-text-iter} iterator.}
  @begin{short}
    Returns an iterator with the \"end iterator\", one past the last valid
    character in the text buffer.
  @end{short}
  If dereferenced with the function @fun{gtk-text-iter-char}, the end iterator
  has a character value of 0. The entire buffer lies in the range from the
  first position in the text buffer to the end iterator. Call the function
  @fun{gtk-text-buffer-start-iter} to get character position 0.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-start-iter}
  @see-function{gtk-text-iter-char}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-end-iter buffer iter)
    iter))

(export 'gtk-text-buffer-end-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_bounds () -> gtk-text-buffer-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_bounds" %gtk-text-buffer-bounds) :void
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-bounds (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @code{start} -- a @class{gtk-text-iter} iterator with first position in the
                    buffer @br{}
    @code{end}   -- a @class{gtk-text-iter} iterator with the end position in
                    the buffer
  @end{return}
  @begin{short}
    Retrieves the first and last iterators in the buffer, i.e. the entire
    @arg{buffer} lies within the range [@arg{start}, @arg{end}).
  @end{short}
  @see-class{gtk-text-buffer}"
  (let ((start (make-instance 'gtk-text-iter))
        (end (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-bounds buffer start end)
    (values start end)))

(export 'gtk-text-buffer-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_modified ()
;;; gtk_text_buffer_set_modified () -> gtk-text-buffer-modified
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-buffer-modified) (setting buffer)
  (foreign-funcall "gtk_text_buffer_set_modified"
                   (g-object gtk-text-buffer) buffer :boolean setting :void)
  setting)

(defcfun ("gtk_text_buffer_get_modified" gtk-text-buffer-modified) :boolean
 "@version{2020-3-15}
  @syntax[]{(gtk-text-buffer-modified buffer) => setting}
  @syntax[]{(setf (gtk-text-buffer-modified buffer) setting)}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[setting]{modification flag setting}
  @begin{short}
    Returns @em{true} if the buffer has been modified.
  @end{short}

  The function @sym{gtk-text-buffer-modified} indicates whether the buffer has
  been modified since the last call to the last function
  @sym{(setf gtk-text-buffer-modified)} set the modification flag to @em{false}.

  Used to keep track of whether the buffer has been modified since the last
  time it was saved. Whenever the buffer is saved to disk, call the function
  @sym{(setf gtk-text-buffer-modified)} with the value @em{false}. When the
  buffer is modified, it will automatically toggled on the modified bit again.
  When the modified bit flips, the buffer emits a \"modified-changed\" signal.
  @see-class{gtk-text-buffer}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-modified)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_selection ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_selection" %gtk-text-buffer-delete-selection)
    :boolean
  (buffer (g-object gtk-text-buffer))
  (interactive :boolean)
  (editable :boolean))

(defun gtk-text-buffer-delete-selection (buffer &key interactive editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the @arg{buffer} is editable by default}
  @return{A boolean whether there was a non-empty selection to delete.}
  @begin{short}
    Deletes the range between the \"insert\" and \"selection_bound\" marks, that
    is, the currently-selected text.
  @end{short}
  If interactive is @em{true}, the editability of the selection will be
  considered (users cannot delete uneditable text).
  @see-class{gtk-text-buffer}"
  (%gtk-text-buffer-delete-selection buffer interactive editable))

(export 'gtk-text-buffer-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_paste_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_paste_clipboard" %gtk-text-buffer-paste-clipboard)
    :void
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard))
  (override-location (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

(defun gtk-text-buffer-paste-clipboard (buffer clipboard &key
                                               override-location
                                               editable)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{the @class{gtk-clipboard} object to paste from}
  @argument[override-location]{a @class{gtk-text-iter} location to insert pasted
    text, or @code{nil} to insert at the cursor}
  @argument[editable]{a boolean whether the buffer is editable by default}
  @begin{short}
    Pastes the contents of a clipboard at the insertion point, or at
    @arg{override-location}.
  @end{short}
  Note: Pasting is asynchronous, that is, we will ask for the paste data and
  return, and at some point later after the main loop runs, the paste data will
  be inserted.
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}"
  (%gtk-text-buffer-paste-clipboard buffer
                                    clipboard
                                    override-location
                                    editable))

(export 'gtk-text-buffer-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_copy_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_copy_clipboard" gtk-text-buffer-copy-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{the @class{gtk-clipboard} object to copy to}
  @begin{short}
    Copies the currently selected text to a @arg{clipboard}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-text-buffer-copy-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_cut_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_cut_clipboard" gtk-text-buffer-cut-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{the @class{gtk-clipboard} object to cut to}
  @argument[editable]{a boolean with the default editability of the
    @arg{buffer}}
  @begin{short}
    Copies the currently selected text to a clipboard, then deletes said
    text if it is editable.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard))
  (editable :boolean))

(export 'gtk-text-buffer-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bounds ()
;;;   -> gtk-text-buffer-selection-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_selection_bounds"
          %gtk-text-buffer-selection-bounds) :boolean
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-selection-bounds (buffer)
 #+cl-cffi-gtk-documentation
 "@version{*2021-2-14}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @code{start} -- a @class{gtk-text-iter} iterator to initialize with
    selection start, or @code{nil} @br{}
    @code{end} -- a @class{gtk-text-iter} iterator to initialize with
    selection end, or @code{nil}
  @end{return}
  @begin{short}
    Returns the @arg{start} and @arg{end} iterators if some text is selected.
  @end{short}
  If the selection has length 0, then the @arg{start} and @arg{end} iterators
  are filled in with the same value. The @arg{start} and @arg{end} iterators
  will be in ascending order.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((start (make-instance 'gtk-text-iter))
        (end (make-instance 'gtk-text-iter)))
    (if (%gtk-text-buffer-selection-bounds buffer start end)
        (values start end)
        (values nil nil))))

(export 'gtk-text-buffer-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_begin_user_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_begin_user_action" gtk-text-buffer-begin-user-action)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Called to indicate that the buffer operations between here and a call to the
    function @fun{gtk-text-buffer-end-user-action} are part of a single
    user-visible operation.
  @end{short}
  The operations between the functions @sym{gtk-text-buffer-begin-user-action}
  and @fun{gtk-text-buffer-end-user-action} can then be grouped when creating
  an undo stack. @class{gtk-text-buffer} maintains a count of calls to
  @sym{gtk-text-buffer-begin-user-action} that have not been closed with a
  call to the function @fun{gtk-text-buffer-end-user-action}, and emits the
  \"begin-user-action\" and \"end-user-action\" signals only for the outermost
  pair of calls. This allows you to build user actions from other user actions.

  The \"interactive\" buffer mutation functions, such as the function
  @fun{gtk-text-buffer-insert-interactive}, automatically call begin/end user
  action around the buffer operations they perform, so there is no need to add
  extra calls if you user action consists solely of a single call to one of
  those functions.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-end-user-action}
  @see-function{gtk-text-buffer-insert-interactive}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-begin-user-action)

;;; ----------------------------------------------------------------------------

(defmacro with-text-buffer-user-action ((buffer) &body body)
  (let ((g (gensym)))
    `(let ((,g ,buffer))
       (gtk-text-buffer-begin-user-action ,g)
       (unwind-protect
         (progn ,@body)
         (gtk-text-buffer-end-user-action ,g)))))

(export 'with-text-buffer-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_end_user_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_end_user_action" gtk-text-buffer-end-user-action)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Should be paired with a call to the function
    @fun{gtk-text-buffer-begin-user-action}.
  @end{short}
  See that function for a full explanation.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-begin-user-action}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-end-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_selection_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_add_selection_clipboard"
           gtk-text-buffer-add-selection-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @begin{short}
    Adds @arg{clipboard} to the list of clipboards in which the selection
    contents of @arg{buffer} are available.
  @end{short}
  In most cases, @arg{clipboard} will be the @class{gtk-clipboard} of type
  @code{:primary} for a view of @arg{buffer}.
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-text-buffer-add-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_selection_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_selection_clipboard"
          gtk-text-buffer-remove-selection-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object added to @arg{buffer} by
    the function @fun{gtk-text-buffer-add-selection-clipboard}}
  @begin{short}
    Removes a @class{gtk-clipboard} object added with the function
    @fun{gtk-text-buffer-add-selection-clipboard}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-add-selection-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-text-buffer-remove-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; enum GtkTextBufferTargetInfo
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTextBufferTargetInfo" gtk-text-buffer-target-info
  (:export t
   :type-initializer "gtk_text_buffer_target_info_get_type")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-target-info atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-buffer-target-info atdoc:*external-symbols*)
 "@version{2020-7-16}
  @begin{short}
    These values are used as \"info\" for the targets contained in the lists
    returned by the functions @fun{gtk-text-buffer-copy-target-list} and
    @fun{gtk-text-buffer-paste-target-list}.
  @end{short}

  The values counts down from -1 to avoid clashes with application added drag
  destinations which usually start at 0.
  @begin{pre}
(define-g-enum \"GtkTextBufferTargetInfo\" gtk-text-buffer-target-info
  (:export t
   :type-initializer \"gtk_text_buffer_target_info_get_type\")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))
  @end{pre}
  @begin[code]{table}
    @entry[:buffer-contents]{Buffer contents.}
    @entry[:rich-text]{Rich text.}
    @entry[:text]{Text.}
  @end{table}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-copy-target-list}
  @see-function{gtk-text-buffer-paste-target-list}")

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferDeserializeFunc ()
;;;
;;; gboolean (*GtkTextBufferDeserializeFunc) (GtkTextBuffer *register_buffer,
;;;                                           GtkTextBuffer *content_buffer,
;;;                                           GtkTextIter *iter,
;;;                                           const guint8 *data,
;;;                                           gsize length,
;;;                                           gboolean create_tags,
;;;                                           gpointer user_data,
;;;                                           GError **error);
;;;
;;; A function that is called to deserialize rich text that has been serialized
;;; with gtk_text_buffer_serialize(), and insert it at iter.
;;;
;;; register_buffer :
;;;     the GtkTextBuffer the format is registered with
;;;
;;; content_buffer :
;;;     the GtkTextBuffer to deserialize into
;;;
;;; iter :
;;;     insertion point for the deserialized text
;;;
;;; data :
;;;     data to deserialize
;;;
;;; length :
;;;     length of data
;;;
;;; create_tags :
;;;     TRUE if deserializing may create tags
;;;
;;; user_data :
;;;     user data that was specified when registering the format
;;;
;;; error :
;;;     return location for a GError
;;;
;;; Returns :
;;;     TRUE on success, FALSE otherwise
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-buffer-deserialize-cb :boolean
    ((register-buffer (g-object gtk-text-buffer))
     (content-buffer (g-object gtk-text-buffer))
     (iter (g-boxed-foreign gtk-text-iter))
     (data :pointer)
     (length g-size)
     (create-tags :boolean)
     (user-data :pointer)
     (error :pointer))
  (with-catching-to-g-error (error)
    (let ((fn (glib::get-stable-pointer-value user-data)))
      (restart-case
       (let ((bytes (iter (with bytes = (make-array length
                                                    :element-type
                                                    '(unsigned-byte 8)))
                           (for i from 0 below length)
                           (setf (aref bytes i) (mem-ref data :uint8 i))
                           (finally (return bytes)))))
         (progn
           (funcall fn register-buffer content-buffer iter bytes create-tags)
           t))
        (return-from-gtk-text-buffer-deserialize-cb ()
          (error 'g-error-condition
                 :domain "cl-gtk2"
                 :code 0
                 :message
               "'return-from-gtk-text-buffer-deserialize-cb' restart was called"
                 ))))))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_deserialize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_deserialize" %gtk-text-buffer-deserialize) :boolean
  (register-buffer (g-object gtk-text-buffer))
  (content-buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string)
  (iter (g-boxed-foreign gtk-text-iter))
  (data :pointer)
  (length g-size)
  (error :pointer))

(defun gtk-text-buffer-deserialize (register-buffer content-buffer
                                                    format iter data)
 #+cl-cffi-gtk-documentation
 "@version{2020-7-16}
  @argument[register-buffer]{the @class{gtk-text-buffer} format is registered
    with}
  @argument[content-buffer]{the @class{gtk-text-buffer} to deserialize into}
  @argument[format]{the rich text format to use for deserializing}
  @argument[iter]{a @class{gtk-text-iter} insertion point for the deserialized
    text}
  @argument[data]{data to deserialize}
  @return{@em{True} on success, @code{nil} otherwise.}
  @begin{short}
    This function deserializes rich text in format format and inserts it at
    @arg{iter}.
  @end{short}

  @arg{format}'s to be used must be registered using the functions
  @fun{gtk-text-buffer-register-deserialize-format} or
  @fun{gtk-text-buffer-register-deserialize-tagset} beforehand.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-deserialize-format}
  @see-function{gtk-text-buffer-register-deserialize-tagset}"
  (let ((bytes (foreign-alloc :uint8 :count (length data))))
    (iter (for i from 0 below (length data))
          (setf (mem-aref bytes :uint8 i) (aref data i)))
    (unwind-protect
      (with-g-error (err)
        (%gtk-text-buffer-deserialize register-buffer
                                      content-buffer
                                      format
                                      iter
                                      bytes
                                      (length data)
                                      err))
      (foreign-free bytes))))

(export 'gtk-text-buffer-deserialize)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_deserialize_get_can_create_tags ()
;;; gtk_text_buffer_deserialize_set_can_create_tags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_deserialize_set_can_create_tags"
          %gtk-text-buffer-deserialize-set-can-create-tags) :void
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string)
  (can-create-tags :boolean))

(defun (setf gtk-text-buffer-deserialize-can-create-tags)
       (new-value buffer format)
  (%gtk-text-buffer-deserialize-set-can-create-tags buffer format new-value))

(defcfun ("gtk_text_buffer_deserialize_get_can_create_tags"
           gtk-text-buffer-deserialize-can-create-tags) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @syntax[]{(gtk-text-buffer-deserialize-can-create-tags buffer format) => can-create-tags}
  @syntax[]{(setf (gtk-text-buffer-deserialize-can-create-tags buffer format) can-create-tags)}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{an atom as a string representing a registered rich text
    format}
  @argument[can-create-tags]{a boolean whether deserializing this format may
    create tags}
  @begin{short}
    Use this function to allow a rich text deserialization function to create
    new tags in the receiving buffer.
  @end{short}
  Note that using this function is almost always a bad idea, because the rich
  text functions you register should know how to map the rich text format they
  handler to your text buffers set of tags.

  The ability of creating new (arbitrary!) tags in the receiving buffer is
  meant for special rich text formats like the internal one that is registered
  using the function @fun{gtk-text-buffer-register-deserialize-tagset}, because
  that format is essentially a dump of the internal structure of the source
  buffer, including its tag names.

  You should allow creation of tags only if you know what you are doing, e.g.
  if you defined a tagset name for your application suite's text buffers and
  you know that it is fine to receive new tags from these buffers, because you
  know that your application can handle the newly created tags.
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-deserialize-set-can-create-tags}"
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string))

(export 'gtk-text-buffer-deserialize-can-create-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_deserialize_formats ()
;;; -> gtk-text-buffer-deserialize-formats
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_deserialize_formats"
          %gtk-text-buffer-deserialize-formats)
    (:pointer gdk-atom-as-string)
  (text-buffer (g-object gtk-text-buffer))
  (n-formats (:pointer :int)))

(defun gtk-text-buffer-deserialize-formats (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A list of atoms as strings representing the registered formats.}
  @begin{short}
    This function returns the rich text deserialize formats registered with
    @arg{buffer} using the functions
    @fun{gtk-text-buffer-register-deserialize-format} or
    @fun{gtk-text-buffer-register-deserialize-tagset}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-deserialize-format}
  @see-function{gtk-text-buffer-register-deserialize-tagset}"
  (with-foreign-object (n-formats :int)
    (let ((atoms-ptr (%gtk-text-buffer-deserialize-formats buffer n-formats)))
      (iter (for i from 0 below (mem-ref n-formats :int))
            (for atom = (mem-aref atoms-ptr 'gdk-atom-as-string i))
            (collect atom)))))

(export 'gtk-text-buffer-deserialize-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_serialize_formats ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_serialize_formats"
          %gtk-text-buffer-serialize-formats) (:pointer gdk-atom-as-string)
  (buffer (g-object gtk-text-buffer))
  (n-formats (:pointer :int)))

(defun gtk-text-buffer-serialize-formats (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A list of atoms as strings representing the registered formats.}
  @begin{short}
    This function returns the rich text serialize formats registered with
    @arg{buffer} using the functions
    @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-serialize-tagset}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (with-foreign-object (n-formats :int)
    (let ((atoms-ptr (%gtk-text-buffer-serialize-formats buffer n-formats)))
      (iter (for i from 0 below (mem-ref n-formats :int))
            (for atom = (mem-aref atoms-ptr 'gdk-atom-as-string i))
            (collect atom)))))

(export 'gtk-text-buffer-serialize-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_deserialize_format ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_deserialize_format"
          %gtk-text-buffer-register-deserialize-format) gdk-atom-as-string
  (buffer (g-object gtk-text-buffer))
  (mime-type :string)
  (function :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun gtk-text-buffer-register-deserialize-format (buffer mime-type func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mime-type]{a string with the format's MIME type}
  @argument[func]{the deserialize function to register}
  @begin{return}
    The atom as a string that corresponds to the newly registered format's
    @arg{mime-type}.
  @end{return}
  @begin{short}
    This function registers a rich text deserialization function along with its
    @arg{mime-type} with the passed @arg{buffer}.
  @end{short}
  @see-class{gtk-text-buffer}"
  (%gtk-text-buffer-register-deserialize-format
                                   buffer
                                   mime-type
                                   (callback gtk-text-buffer-deserialize-cb)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-text-buffer-register-deserialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_deserialize_tagset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_deserialize_tagset"
          gtk-text-buffer-register-deserialize-tagset) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tagset-name]{an string with an optional tagset name, on @code{nil}}
  @begin{return}
    The atom as a string that corresponds to the newly registered format's
    MIME type.
  @end{return}
  @begin{short}
    This function registers GTK+'s internal rich text serialization format with
    the passed buffer.
  @end{short}
  See the function @fun{gtk-text-buffer-register-serialize-tagset} for details.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (tagset-name :string))

(export 'gtk-text-buffer-register-deserialize-tagset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_format ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_serialize_format"
          %gtk-text-buffer-register-serialize-format) gdk-atom-as-string
  (buffer (g-object gtk-text-buffer))
  (mime-type :string)
  (function :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun gtk-text-buffer-register-serialize-format (buffer mime-type function)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mime-type]{a string with the format's MIME type}
  @argument[function]{the serialize function to register}
  @begin{return}
    The atom as a string that corresponds to the newly registered format's
    @arg{mime-type}.
  @end{return}
  @begin{short}
    This function registers a rich text serialization function along with its
    @arg{mime-type} with the passed @arg{buffer}.
  @end{short}
  @see-class{gtk-text-buffer}"
  (%gtk-text-buffer-register-serialize-format
                                   buffer
                                   mime-type
                                   (callback gtk-text-buffer-serialize-cb)
                                   (allocate-stable-pointer function)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-text-buffer-register-serialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_tagset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_serialize_tagset"
          gtk-text-buffer-register-serialize-tagset) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tagset-name]{a string with an optional tagset name, or @code{nil}}
  @begin{return}
    The atom as a string that corresponds to the newly registered format's
    MIME type.
  @end{return}
  @begin{short}
    This function registers GTK+'s internal rich text serialization format with
    the passed buffer.
  @end{short}
  The internal format does not comply to any standard rich text format and only
  works between @class{gtk-text-buffer} instances. It is capable of serializing
  all of a text buffer's tags and embedded pixbufs.

  This function is just a wrapper around the function
  @fun{gtk-text-buffer-register-serialize-format}. The MIME type used for
  registering is \"application/x-gtk-text-buffer-rich-text\", or
  \"application/x-gtk-text-buffer-rich-text;format=tagset_name\" if a
  @arg{tagset-name} was passed.

  The @arg{tagset-name} can be used to restrict the transfer of rich text to
  buffers with compatible sets of tags, in order to avoid unknown tags from
  being pasted. It is probably the common case to pass an identifier
  != @code{nil} here, since the @code{nil} tagset requires the receiving buffer
  to deal with with pasting of arbitrary tags.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}"
  (buffer (g-object gtk-text-buffer))
  (tagset-name :string))

(export 'gtk-text-buffer-register-serialize-tagset)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferSerializeFunc ()
;;;
;;; guint8 * (*GtkTextBufferSerializeFunc) (GtkTextBuffer *register_buffer,
;;;                                         GtkTextBuffer *content_buffer,
;;;                                         const GtkTextIter *start,
;;;                                         const GtkTextIter *end,
;;;                                         gsize *length,
;;;                                         gpointer user_data);
;;;
;;; A function that is called to serialize the content of a text buffer. It must
;;; return the serialized form of the content.
;;;
;;; register_buffer :
;;;     the GtkTextBuffer for which the format is registered
;;;
;;; content_buffer :
;;;     the GtkTextBuffer to serialize
;;;
;;; start :
;;;     start of the block of text to serialize
;;;
;;; end :
;;;     end of the block of text to serialize
;;;
;;; length :
;;;     Return location for the length of the serialized data
;;;
;;; user_data :
;;;     user data that was specified when registering the format
;;;
;;; Returns :
;;;     a newly-allocated array of guint8 which contains the serialized data, or
;;;     NULL if an error occurred
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-buffer-serialize-cb :pointer
    ((register-buffer (g-object gtk-text-buffer))
     (content-buffer (g-object gtk-text-buffer))
     (start-iter (g-boxed-foreign gtk-text-iter))
     (end-iter (g-boxed-foreign gtk-text-iter))
     (length (:pointer g-size))
     (user-data :pointer))
  (let ((fn (glib::get-stable-pointer-value user-data)))
    (restart-case
     (let* ((bytes (funcall fn
                             register-buffer
                             content-buffer
                             start-iter
                             end-iter))
            (bytes-ptr (g-malloc (length bytes))))
       (setf (mem-ref length 'g-size) (length bytes))
       (iter (for i from 0 below (length bytes))
             (setf (mem-aref bytes-ptr :uint8 i) (aref bytes i)))
       bytes-ptr)
     (return-from-gtk-text-buffer-serialize-cb () nil))))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_serialize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_serialize" %gtk-text-buffer-serialize) :pointer
  (register-buffer (g-object gtk-text-buffer))
  (content-buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string)
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (length (:pointer g-size)))

(defun gtk-text-buffer-serialize (register-buffer content-buffer
                                                  format start end)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[register-buffer]{the @class{gtk-text-buffer} object format is
    registered with}
  @argument[content-buffer]{the @class{gtk-text-buffer} object to serialize}
  @argument[format]{an atom as a string with the rich text format to use for
    serializing}
  @argument[start]{a @class{gtk-text-iter} start of block of text to serialize}
  @argument[end]{a @class{gtk-text-iter} end of block of test to serialize}
  @return{The serialized data, encoded as format.}
  @begin{short}
    This function serializes the portion of text between @arg{start} and
    @arg{end} in the rich text format represented by @arg{format}.
  @end{short}

  The @arg{format} arguments to be used must be registered using the functions
  @fun{gtk-text-buffer-register-serialize-format} or
  @fun{gtk-text-buffer-register-serialize-tagset} beforehand.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (with-foreign-object (length 'g-size)
    (let ((bytes (%gtk-text-buffer-serialize register-buffer
                                             content-buffer
                                             format
                                             start
                                             end
                                             length)))
      (iter (for i from 0 to (mem-ref length 'g-size))
            (for byte = (mem-aref bytes :uint8 i))
            (collect byte result-type vector)
            (finally (g-free bytes))))))

(export 'gtk-text-buffer-serialize)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_unregister_deserialize_format ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_unregister_deserialize_format"
          gtk-text-buffer-unregister-deserialize-format) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{an atom as a string representing a registered rich text
    format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the functions @fun{gtk-text-buffer-register-deserialize-format} or
    @fun{gtk-text-buffer-register-deserialize-tagset}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-deserialize-format}
  @see-function{gtk-text-buffer-register-deserialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string))

(export 'gtk-text-buffer-unregister-deserialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_unregister_serialize_format ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_unregister_serialize_format"
          gtk-text-buffer-unregister-serialize-format) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-24}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{an atom as a string representing a registered rich text
    format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the functions @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-serialize-tagset}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string))

(export 'gtk-text-buffer-unregister-serialize-format)

;;; --- End of file gtk.text-buffer.lisp ---------------------------------------
