;;; ----------------------------------------------------------------------------
;;; gtk.text-buffer.lisp
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
;;; enum GtkTextBufferTargetInfo
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTextBufferTargetInfo" gtk-text-buffer-target-info
  (:export t
   :type-initializer "gtk_text_buffer_target_info_get_type")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-target-info atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-text-buffer-target-info atdoc:*external-symbols*)
 "@version{2021-11-16}
  @begin{short}
    These values are used as \"info\" for the targets contained in the lists
    returned by the @fun{gtk-text-buffer-copy-target-list} and
    @fun{gtk-text-buffer-paste-target-list} functions.
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
 "@version{*2021-11-16}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"apply-tag\" signal}
      @begin{pre}
 lambda (buffer tag start end)    :run-last
      @end{pre}
      The signal is emitted to apply a tag to a range of text in a text buffer.
      Applying actually occurs in the default handler. Note that if your handler
      runs before the default handler it must not invalidate the @arg{start} and
      @arg{end} iterators, or has to revalidate them.
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
 lambda (buffer)    :run-last
      @end{pre}
      The signal is emitted at the beginning of a single user visible operation
      on a text buffer.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (buffer)    :run-last
      @end{pre}
      The signal is emitted when the content of a text buffer has changed.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"delete-range\" signal}
      @begin{pre}
 lambda (buffer start end)    :run-last
      @end{pre}
      The signal is emitted to delete a range from a text buffer. Note that if
      your handler runs before the default handler it must not invalidate the
      @arg{start} and @arg{end} iterators, or has to revalidate them. The
      default signal handler revalidates the @arg{start} and @arg{end} iterators
      to both point to the location where text was deleted. Handlers which run
      after the default handler do not have access to the deleted text.
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
 lambda (buffer)    :run-last
      @end{pre}
      The signal is emitted at the end of a single user visible operation on
      the text buffer.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"insert-child-anchor\" signal}
      @begin{pre}
 lambda (buffer location anchor)    :run-last
      @end{pre}
      The signal is emitted to insert a @class{gtk-text-child-anchor} object
      in a text buffer. Insertion actually occurs in the default handler. Note
      that if your handler runs before the default handler it must not
      invalidate the @arg{location} iterator, or has to revalidate it. The
      default signal handler revalidates it to be placed after the inserted
      @arg{anchor}.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert
          @arg{anchor} in @arg{buffer}.}
        @entry[anchor]{The @class{gtk-text-child-anchor} object to be inserted.}
      @end{table}
    @subheading{The \"insert-pixbuf\" signal}
      @begin{pre}
 lambda (buffer location pixbuf)    :run-last
      @end{pre}
      The signal is emitted to insert a @class{gdk-pixbuf} object in a text
      buffer. Insertion actually occurs in the default handler. Note that if
      your handler runs before the default handler it must not invalidate the
      @arg{location} iterator, or has to revalidate it. The default signal
      handler revalidates it to be placed after the inserted @arg{pixbuf}.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert
          @arg{pixbuf} in @arg{buffer}.}
        @entry[pixbuf]{The @class{gdk-pixbuf} object to be inserted.}
      @end{table}
    @subheading{The \"insert-text\" signal}
      @begin{pre}
 lambda (buffer location text len)    :run-last
      @end{pre}
      The signal is emitted to insert text in a text buffer. Insertion actually
      occurs in the default handler. Note that if your handler runs before the
      default handler it must not invalidate the @arg{location} iterator, or has
      to revalidate it. The default signal handler revalidates it to point to
      the end of the inserted text.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk-text-iter} position to insert @arg{text}
          in @arg{buffer}.}
        @entry[text]{A string with the UTF-8 text to be inserted.}
        @entry[len]{An integer with the length of the inserted text in bytes.}
      @end{table}
    @subheading{The \"mark-deleted\" signal}
      @begin{pre}
 lambda (buffer mark)    :run-last
      @end{pre}
      The signal is emitted as notification after a @class{gtk-text-mark}
      object is deleted.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[mark]{The @class{gtk-text-mark} object that was deleted.}
      @end{table}
    @subheading{The \"mark-set\" signal}
      @begin{pre}
 lambda (buffer location mark)    :run-last
      @end{pre}
      The signal is emitted as notification after a @class{gtk-text-mark}
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
 lambda (buffer)    :run-last
      @end{pre}
      The signal is emitted when the modified bit of a text buffer flips.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
      @end{table}
    @subheading{The \"paste-done\" signal}
      @begin{pre}
 lambda (buffer clipboard)    :run-last
      @end{pre}
      The signal is emitted after paste operation has been completed. This is
      useful to properly scroll the view to the end of the pasted text.
      @begin[code]{table}
        @entry[buffer]{The @sym{gtk-text-buffer} object which received the
          signal.}
        @entry[clipboard]{The @class{gtk-clipboard} object.}
      @end{table}
    @subheading{The \"remove-tag\" signal}
      @begin{pre}
 lambda (buffer tag start end)    :run-last
      @end{pre}
      The signal is emitted to remove all occurrences of @arg{tag} from a range
      of text in a text buffer. Removal actually occurs in the default handler.
      Note that if your handler runs before the default handler it must not
      invalidate the @arg{start} and @arg{end} iterators, or has to revalidate
      them.
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
 "The @code{copy-target-list} property of type @class{gtk-target-list} (Read)
  @br{}
  The list of targets the text buffer supports for clipboard copying and as
  drag and drop source.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-copy-target-list atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-copy-target-list 'function)
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-copy-target-list object) => tlist}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[tlist]{a @class{gtk-target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{copy-target-list} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  This function returns the list of targets this text buffer can provide
  for copying and as drag and drag source. The targets in the list are added
  with info values from the @symbol{gtk-text-buffer-target-info} enumeration
  using the @fun{gtk-target-list-add-rich-text-targets} and
  @fun{gtk-target-list-add-text-targets} functions.
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
  The position of the insert mark, as offset from the beginning of the text
  buffer. It is useful for getting notified when the cursor moves. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-cursor-position 'function)
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-cursor-position object) => position}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[position]{an integer with the position of the insert mark}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{cursor-position} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  The position of the insert mark, as offset from the beginning of the text
  buffer. It is useful for getting notified when the cursor moves.
  @see-class{gtk-text-buffer}")

;;; --- gtk-text-buffer-has-selection ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-selection"
                                               'gtk-text-buffer) 't)
 "The @code{has-selection} property of type @code{:boolean} (Read) @br{}
  Whether the text buffer has some text currently selected. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-has-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-has-selection 'function)
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-has-selection object) => setting}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[setting]{@em{true} if there is text selected}
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
 "The @code{paste-target-list} property of type @class{gtk-target-list} (Read)
  @br{}
  The list of targets the text buffer supports for clipboard pasting and as
  drag and drop destination.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-paste-target-list atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-paste-target-list 'function)
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-paste-target-list object) => tlist}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[tlist]{a @class{gtk-target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{paste-target-list} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  This function returns the list of targets the text buffer supports for
  pasting and as drag and drop destination. The targets in the list are added
  with info values from the @symbol{gtk-text-buffer-target-info} enumeration
  using the @fun{gtk-target-list-add-rich-text-targets} and
  @fun{gtk-target-list-add-text-targets} functions.
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
  The tag table associated with the text buffer.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-tag-table atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-buffer-tag-table 'function)
 "@version{*2021-11-16}
  @syntax[]{(gtk-text-buffer-tag-table object) => table}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[table]{a @class{gtk-text-tag-table} object}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{tag-table} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  Gets the tag table associated with the text buffer.
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
 "@version{*2021-11-16}
  @syntax[]{(gtk-text-buffer-text object) => text}
  @syntax[]{(setf (gtk-text-buffer-text object) text)}
  @argument[object]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the UTF-8 text}
  @begin{short}
    Accessor of the @slot[gtk-text-buffer]{text} slot of the
    @class{gtk-text-buffer} class.
  @end{short}

  The @sym{gtk-text-buffer} function retrieves the text of the text buffer,
  without child widgets and images. The @sym{(setf gtk-text-buffer-text)}
  function deletes current contents of the text buffer, and inserts @arg{text}
  instead. The text must be valid UTF-8.
  @begin[Note]{dictionary}
    Use the @fun{gtk-text-buffer-get-text} function to retrieve a range of text
    from the text buffer and the @fun{gtk-text-buffer-get-slice} function to
    include widgets and images.
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
 "@version{2021-11-16}
  @argument[table]{an optional @class{gtk-text-tag-table} object, or no
    argument to create a new one}
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
 "@version{*2021-11-16}
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
;;; gtk_text_buffer_get_char_count () -> gtk-text-buffer-char-count
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_char_count" gtk-text-buffer-char-count) :int
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{An integer with the number of characters in the text buffer.}
  @begin{short}
    Gets the number of characters in the text buffer.
  @end{short}
  Note that characters and bytes are not the same, you cannot e.g. expect the
  contents of the text buffer in string form to be this many bytes long. The
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
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-insert buffer text) => t}
  @syntax[]{(gtk-text-buffer-insert buffer text :position position) => t}
  @syntax[]{(gtk-text-buffer-insert buffer text :interactive t) => t}
  @syntax[]{(gtk-text-buffer-insert buffer text :interactive t :editable nil) => t}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @argument[position]{a @class{gtk-text-iter} iterator or the default value
    @code{:cursor}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction, the default value is @em{false}}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default,
    the default value is @em{true}}
  @return{A boolean whether the text was actually inserted.}
  @begin{short}
    Inserts text in the text buffer.
  @end{short}

  If the @arg{position} keyword argument has the @code{:cursor} value, the
  default, inserts the text using the current cursor position as the insertion
  point.

  If the @arg{interactive} keyword argument is @em{true}, the insertion will
  not occur if the iterator is at a non-editable location in the text buffer.
  Usually you want to prevent insertions at ineditable locations if the
  insertion results from a user action (is interactive).

  The @arg{editable} keyword argument indicates the editability of text that
  does not have a tag affecting editability applied to it. Typically the result
  of the @fun{gtk-text-view-editable} function is appropriate here.

  Emits the \"insert-text\" signal. Insertion actually occurs in the default
  handler for the signal. The iterator is invalidated when insertion occurs,
  because the text buffer contents change, but the default signal handler
  revalidates it to point to the end of the inserted text.
  @begin[Note]{dictionary}
    The @sym{gtk-text-buffer-insert} function combines the
    @code{gtk_text_buffer_insert()}, @code{gtk_text_buffer_insert_at_cursor()},
    @code{gtk_text_buffer_insert_interactive()}, and
    @code{gtk_text_buffer_insert_interactive_at_cursor()} functions into one
    function using the @arg{position}, @arg{interactive}, and @arg{editable}
    keyword arguments. The corresponding Lisp functions except for
    @sym{gtk-text-buffer-insert} are not exported in the Lisp implementation.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-view-editable}"
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
        t)))

(export 'gtk-text-buffer-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_at_cursor ()                    not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_at_cursor" %gtk-text-buffer-insert-at-cursor)
    :void
  (buffer (g-object gtk-text-buffer))
  (text (:string :free-to-foreign t))
  (len :int))

(defun gtk-text-buffer-insert-at-cursor (buffer text)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @begin{short}
    Calls the function @fun{gtk-text-buffer-insert}, using the current cursor
    position as the insertion point.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert}"
  (%gtk-text-buffer-insert-at-cursor buffer text -1))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive ()                  not exported
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
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[text]{a string with the UTF-8 text}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
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
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-insert}
  @see-function{gtk-text-view-editable}"
  (%gtk-text-buffer-insert-interactive buffer iter text -1 editable))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive_at_cursor ()        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_interactive_at_cursor"
          %gtk-text-buffer-insert-interactive-at-cursor) :boolean
  (buffer (g-object gtk-text-buffer))
  (text (:string :free-to-foreign t))
  (len :int)
  (editable :boolean))

(defun gtk-text-buffer-insert-interactive-at-cursor (buffer text editable)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[text]{a string with the text in UTF-8 format}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{A boolean whether the text was actually inserted.}
  @begin{short}
    Calls the function @fun{gtk-text-buffer-insert-interactive} at the cursor
    position.
  @end{short}

  The argument @arg{editable} indicates the editability of text that does not
  have a tag affecting editability applied to it. Typically the result of the
  function @fun{gtk-text-view-editable} is appropriate here.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert}
  @see-function{gtk-text-buffer-insert-interactive}
  @see-function{gtk-text-view-editable}"
  (%gtk-text-buffer-insert-interactive-at-cursor buffer text -1 editable))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_range" %gtk-text-buffer-insert-range) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-insert-range (buffer iter start end &key interactive
                                                                editable)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-insert-range buffer iter start end) => t}
  @syntax[]{(gtk-text-buffer-insert-range buffer iter start end :interactive t)
    => t}
  @syntax[]{(gtk-text-buffer-insert-range buffer iter start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} position in text buffer}
  @argument[start]{a @class{gtk-text-iter} start position}
  @argument[end]{a @class{gtk-text-iter} end position}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{A boolean whether an insertion was possible.}
  @begin{short}
    Copies text, tags, and pixbufs between the @arg{start} and @arg{end}
    iterators, the order of @arg{start} and @arg{end} does not matter, and
    inserts the copy at the @arg{iter} iterator.
  @end{short}

  Used instead of simply getting/inserting text because it preserves images and
  tags. If @arg{start} and @arg{end} are in a different text buffer from
  @arg{buffer}, the two buffers must share the same tag table.

  The @arg{interactive} keyword argument with the @em{true} value is the same,
  but does nothing if the insertion point is not editable. The @arg{editable}
  keyword argument indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the @fun{gtk-text-view-editable} function is appropriate here.

  Implemented via emissions of the \"insert-text\" and \"apply-tag\" signals,
  so expect those.
  @begin[Note]{dictionary}
    The Lisp implementation combines the two
    @code{gtk_text_buffer_insert_range()} and
    @code{gtk_text_buffer_insert_range_interactive()} functions. The second
    function is not exported in the Lisp implementation,
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-view-editable}"
  (if interactive
      (%gtk-text-buffer-insert-range-interactive buffer
                                                 iter
                                                 start
                                                 end
                                                 editable)
      (progn
        (%gtk-text-buffer-insert-range buffer iter start end)
        t)))

(export 'gtk-text-buffer-insert-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range_interactive ()            not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_range_interactive"
          %gtk-text-buffer-insert-range-interactive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[start]{a @class{gtk-text-iter} with a position in the text buffer}
  @argument[end]{a @class{gtk-text-iter} with another position in the same
    buffer as @arg{start}}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{A boolean whether an insertion was possible at the iterator.}
  @begin{short}
    Same as the function @fun{gtk-text-buffer-insert-range}, but does nothing
    if the insertion point is not editable.
  @end{short}

  The argument @arg{editable} indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the function @fun{gtk-text-view-editable} is appropriate here.
  @begin[Note]{dictionary}
    The function @sym{gtk-text-buffer-insert-range-interactive} is called from
    the function @fun{gtk-text-buffer-insert-range}.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-insert-range}
  @see-function{gtk-text-view-editable}"
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags ()
;;; ----------------------------------------------------------------------------

(defun gtk-text-buffer-insert-with-tags (buffer iter text &rest tags)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator in the text buffer}
  @argument[text]{a string with the UTF-8 text}
  @argument[tags]{the @class{gtk-text-tag} objects or strings with the tag
    names to apply to @arg{text}}
  @begin{short}
    Inserts text into the text buffer at the position @arg{iter}, applying the
    list of tags to the newly inserted text.
  @end{short}

  Equivalent to calling the @fun{gtk-text-buffer-insert} function, then the
  @fun{gtk-text-buffer-apply-tag} function on the inserted text. The
  @sym{gtk-text-buffer-insert-with-tags} function is just a convenience
  function.
  @begin[Note]{dictionary}
    The Lisp implementation does not call the
    @code{gtk_text_buffer_insert_with_tags()} function, but uses the
    @fun{gtk-text-buffer-insert} and @fun{gtk-text-buffer-apply-tag} functions.
    The @code{gtk_text_buffer_insert_with_tags_by_name()} function is included
    in this function and not implemented in the Lisp library.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-buffer-insert}
  @see-function{gtk-text-buffer-apply-tag}"
  (let ((offset (gtk-text-iter-offset iter)))
    (prog1
      (gtk-text-buffer-insert buffer text :position iter)
      (let ((start (gtk-text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (gtk-text-buffer-apply-tag buffer tag start iter))))))

(export 'gtk-text-buffer-insert-with-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags_by_name ()            not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defun gtk-text-buffer-insert-with-tags-by-name (buffer iter text &rest tags)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator in text buffer}
  @argument[text]{a string with the UTF-8 text}
  @argument[tags]{strings with the tag names to apply to @arg{text}}
  @begin{short}
    Same as the function @fun{gtk-text-buffer-insert-with-tags}, but allows you
    to pass in tag names instead of tag objects.
  @end{short}
  @begin[Note]{dictionary}
    The Lisp implementation does not call the C function, but uses the
    @fun{gtk-text-buffer-insert} and @fun{gtk-text-buffer-apply-tag} functions.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-insert-with-tags}
  @see-function{gtk-text-buffer-insert}
  @see-function{gtk-text-buffer-apply-tag-by-name}"
  (let ((offset (gtk-text-iter-offset iter)))
    (prog1
      (gtk-text-buffer-insert buffer text :position iter)
      (let ((start (gtk-text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (gtk-text-buffer-apply-tag-by-name buffer tag start iter))))))

#+nil
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} iterator with a position in the text
    buffer}
  @argument[markup]{a UTF-8 string containing Pango markup}
  @begin{short}
    Inserts the text in @arg{markup} at the position of the iterator.
  @end{short}
  The text in @arg{markup} will be inserted in its entirety and must be valid
  UTF-8. Emits the \"insert-text\" signal, possibly multiple times. Insertion
  actually occurs in the default handler for the signal. The iterator will
  point to the end of the inserted text on return.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (%gtk-text-buffer-insert-markup buffer iter markup -1))

(export 'gtk-text-buffer-insert-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete" %gtk-text-buffer-delete) :void
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-delete (buffer start end &key interactive editable)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-delete buffer start end) => t}
  @syntax[]{(gtk-text-buffer-delete buffer start end :interactive t) => t}
  @syntax[]{(gtk-text-buffer-delete buffer start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start position in the text buffer}
  @argument[end]{a @class{gtk-text-iter} end position in the text buffer}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @return{A boolean whether some text was actually deleted.}
  @begin{short}
    Deletes text between the @arg{start} and @arg{end} iterators.
  @end{short}
  The order of the @arg{start} and @arg{end} iterators is not actually relevant.
  The @sym{gtk-text-buffer-delete} function will reorder them. This function
  actually emits the \"delete-range\" signal, and the default handler of that
  signal deletes the text. Because the text buffer is modified, all outstanding
  iterators become invalid after calling this function. However, the @arg{start}
  and @arg{end} interators will be re-initialized to point to the location where
  text was deleted.

  If the @arg{interactive} keyword argument is @em{true} deletes all editable
  text for each editable sub range of [@arg{start}, @arg{end}). The @arg{start}
  and @arg{end} iterators are revalidated to point to the location of the last
  deleted range, or left untouched if no text was deleted.
  @begin[Note]{dictionary}
    The @code{gtk_text_buffer_delete_interactive()} function is included in
    this function and not implemented in the Lisp libraray.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (if interactive
      (%gtk-text-buffer-delete-interactive buffer start end editable)
      (progn
        (%gtk-text-buffer-delete buffer start end)
        t)))

(export 'gtk-text-buffer-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_interactive ()                  not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_interactive"
          %gtk-text-buffer-delete-interactive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start of range to delete}
  @argument[end]{a @class{gtk-text-iter} end of range}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{A boolean whether some text was actually deleted.}
  @begin{short}
    Deletes all editable text in the given range.
  @end{short}
  Calls the function @fun{gtk-text-buffer-delete} for each editable sub range
  of [@arg{start}, @arg{end}). @arg{start} and @arg{end} are revalidated to
  point to the location of the last deleted range, or left untouched if no
  text was deleted.
  @begin[Note]{dictionary}
    In the Lisp implementation this function is called from the function
    @fun{gtk-text-buffer-delete}.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-delete}"
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} position in @arg{buffer}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{@em{True} if the text buffer was modified.}
  @begin{short}
    Performs the appropriate action as if the user hit the delete key with the
    cursor at the position specified by @arg{iter}.
  @end{short}
  In the normal case a single character will be deleted, but when combining
  accents are involved, more than one character can be deleted, and when
  precomposed character and accent combinations are involved, less than one
  character will be deleted.

  Because the text buffer is modified, all outstanding iterators become invalid
  after calling this function. However, the iterator will be re-initialized to
  point to the location where text was deleted.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (%gtk-text-buffer-backspace buffer iter interactive editable))

(export 'gtk-text-buffer-backspace)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_text" %gtk-text-buffer-get-text) :string
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (include :boolean))

(defun gtk-text-buffer-get-text (buffer start end &optional include)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start iterator of a range}
  @argument[end]{a @class{gtk-text-iter} end iterator of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{An allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}. Does not include
  characters representing embedded images, so byte and character indexes into
  the returned string do not correspond to byte and character indexes into the
  text buffer. Contrast with the @fun{gtk-text-buffer-get-slice} function.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-text}
  @see-function{gtk-text-buffer-get-slice}"
  (%gtk-text-buffer-get-text buffer start end include))

(export 'gtk-text-buffer-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_slice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_slice" %gtk-text-buffer-get-slice)
    (:string :free-from-foreign t)
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (include :boolean))

(defun gtk-text-buffer-get-slice (buffer start end &optional include)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} start of a range}
  @argument[end]{a @class{gtk-text-iter} end of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{An allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}.

  The returned string includes a @code{0xFFFC} character whenever the text
  buffer contains embedded images, so byte and character indexes into the
  returned string do correspond to byte and character indexes into the text
  buffer. Contrast with the @fun{gtk-text-buffer-get-text} function. Note that
  @code{0xFFFC} can occur in normal text as well, so it is not a reliable
  indicator that a pixbuf or widget is in the text buffer.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-get-text}"
  (%gtk-text-buffer-get-slice buffer start end include))

(export 'gtk-text-buffer-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_insert_pixbuf" gtk-text-buffer-insert-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location to insert the pixbuf}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    Inserts an image into the text buffer at @arg{iter}.
  @end{short}
  The image will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for pixbufs, but the \"text\" variants do not, e.g. see the
  @fun{gtk-text-buffer-get-slice} and @fun{gtk-text-buffer-get-text} functions.
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location to insert the anchor}
  @argument[anchor]{an optional @class{gtk-text-child-anchor} object}
  @return{A @class{gtk-text-child-anchor} child widget anchor.}
  @begin{short}
    Inserts a child widget anchor into the text buffer at @arg{iter}.
  @end{short}
  The anchor will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for anchors, but the \"text\" variants do not, e.g. see the
  @fun{gtk-text-buffer-get-slice} and @fun{gtk-text-buffer-get-text} functions.
  Consider the @fun{gtk-text-buffer-create-child-anchor} function as a more
  convenient alternative to this function. The text buffer will add a reference
  to the anchor, so you can unref it after insertion.

  If the @arg{anchor} argument is @code{nil} creates an anchor with the
  @fun{gtk-text-child-anchor-new} function and inserts it into @arg{buffer} with
  the @fun{gtk-text-buffer-insert-child-anchor} function. The new anchor is
  owned by the text buffer.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-get-slice}
  @see-function{gtk-text-buffer-get-text}
  @see-function{gtk-text-buffer-child-anchor-new}
  @see-function{gtk-text-buffer-insert-child-anchor}
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[iter]{a @class{gtk-text-iter} location in the text buffer}
  @return{The created @class{gtk-text-child-anchor} anchor.}
  @begin{short}
    This is a convenience function which simply creates an anchor with the
    @fun{gtk-text-child-anchor-new} function and inserts it into the text buffer
    with the @fun{gtk-text-buffer-insert-child-anchor} function.
  @end{short}
  The new anchor is owned by the text buffer.
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
  (name (:string :free-to-foreign t))
  (where (g-boxed-foreign gtk-text-iter))
  (left-gravity :boolean))

(defun gtk-text-buffer-create-mark (buffer name pos &optional (gravity t))
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name for the mark, or @code{nil}}
  @argument[pos]{a @class{gtk-text-iter} location to place the mark}
  @argument[gravity]{a boolean whether the mark has left gravity}
  @return{The new @class{gtk-text-mark} object.}
  @begin{short}
    Creates a mark at position @arg{pos}.
  @end{short}
  If the @arg{mark} argument is @code{nil}, the mark is anonymous. Otherwise,
  the mark can be retrieved by name using the @fun{gtk-text-buffer-mark}
  function. If a mark has left gravity, and text is inserted at the current
  location of the mark, the mark will be moved to the left of the newly inserted
  text. If the mark has right gravity, the mark will end up on the right of
  newly inserted text. The standard left-to-right cursor is a mark with right
  gravity, when you type, the cursor stays on the right side of the text you are
  typing.

  The caller of this function does not own a reference to the returned
  @class{gtk-text-mark} object, so you can ignore the return value if you like.
  Marks are owned by the text buffer and go away when the text buffer does.

  Emits the \"mark-set\" signal as notification of the initial placement of
  the mark.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-mark}
  @see-function{gtk-text-buffer-mark}"
  (%gtk-text-buffer-create-mark buffer name pos gravity))

(export 'gtk-text-buffer-create-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_move_mark" %gtk-text-buffer-move-mark) :void
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark))
  (pos (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-move-mark (buffer mark pos)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object, or a string with the name
    of the mark}
  @argument[pos]{new @class{gtk-text-iter} location for @arg{mark} in the
    text buffer}
  @begin{short}
    Moves the mark to the new location @arg{pos}.
  @end{short}
  Emits the \"mark-set\" signal as notification of the move.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-class{gtk-text-mark}"
  (if (stringp mark)
      (%gtk-text-buffer-move-mark-by-name buffer mark pos)
      (%gtk-text-buffer-move-mark buffer mark pos)))

(export 'gtk-text-buffer-move-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark_by_name ()                   not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_move_mark_by_name"
          %gtk-text-buffer-move-mark-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a mark}
  @argument[where]{new @class{gtk-text-iter} location for mark}
  @begin{short}
    Moves the mark named @arg{name}, which must exist, to location @arg{where}.
  @end{short}
  See the function @fun{gtk-text-buffer-move-mark} for details.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t))
  (pos (g-boxed-foreign gtk-text-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_add_mark" gtk-text-buffer-add-mark) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object with the mark to add}
  @argument[pos]{a @class{gtk-text-iter} iterator with the location to place
    the mark}
  @begin{short}
    Adds the mark at the given position.
  @end{short}
  The mark must not be added to another text buffer, and if its name is not
  @code{nil} then there must not be another mark in the text buffer with the
  same name.

  Emits the \"mark-set\" signal as notification of the initial placement of
  the mark.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-class{gtk-text-iter}"
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark))
  (pos (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_mark" %gtk-text-buffer-delete-mark) :void
  (buffer (g-object gtk-text-buffer))
  (mark (g-object gtk-text-mark)))

(defun gtk-text-buffer-delete-mark (buffer mark)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object, or a string with the name
    of a mark in the text buffer}
  @begin{short}
    Deletes the mark, so that it is no longer located anywhere in the text
    buffer.
  @end{short}
  Removes the reference the text buffer holds to the mark. Most operations on
  the mark become invalid, until it gets added to a text buffer again with the
  @fun{gtk-text-buffer-add-mark} function. Use the @fun{gtk-text-mark-deleted}
  function to find out if a mark has been removed from its text buffer. The
  \"mark-deleted\" signal will be emitted as notification after the mark is
  deleted.
  @begin[Note]{dictionary}
    The @code{gtk_text_buffer_delete_mark_by_name} function is included in
    this function and not exported in the Lisp library.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-function{gtk-text-buffer-add-mark}
  @see-function{gtk-text-mark-deleted}"
  (if (stringp mark)
      (%gtk-text-buffer-delete-mark-by-name buffer mark)
      (%gtk-text-buffer-delete-mark buffer mark)))

(export 'gtk-text-buffer-delete-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark_by_name ()                 not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_delete_mark_by_name"
          %gtk-text-buffer-delete-mark-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a mark in text buffer}
  @begin{short}
    Deletes the mark named @arg{name}.
  @end{short}
  The mark must exist. See the function @fun{gtk-text-buffer-delete-mark} for
  details.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-delete-mark}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_mark () -> gtk-text-buffer-mark
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_mark" gtk-text-buffer-mark)
    (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with a mark name}
  @return{A @class{gtk-text-mark} object, or @code{nil}.}
  @begin{short}
    Returns the mark named @arg{name} in the text buffer, or @code{nil} if
    no such mark exists in the text buffer.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}"
  (buffer (g-object gtk-text-buffer))
  (name (:string :free-to-foreign t)))

(export 'gtk-text-buffer-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_insert ()
;;; ----------------------------------------------------------------------------

;; It is wrong to implement this as gtk-text-buffer-insert, we have already a
;; function with this name.

(defcfun ("gtk_text_buffer_get_insert" gtk-text-buffer-get-insert)
    (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A @class{gtk-text-mark} insertion point mark.}
  @begin{short}
    Returns the mark that represents the cursor (insertion point).
  @end{short}
  Equivalent to calling the @fun{gtk-text-buffer-mark} function to get the mark
  named \"insert\", but more efficient.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}
  @see-function{gtk-text-buffer-mark}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-get-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bound () -> gtk-text-buffer-selection-bound
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_selection_bound"
           gtk-text-buffer-selection-bound) (g-object gtk-text-mark)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{The @class{gtk-text-mark} selection bound mark.}
  @begin{short}
    Returns the mark that represents the selection bound.
  @end{short}
  Equivalent to calling the @fun{gtk-text-buffer-mark} function to get the mark
  named \"selection_bound\", but very slightly more efficient, and involves less
  typing.

  The currently selected text in the text buffer is the region between the
  \"selection_bound\" and \"insert\" marks. If the \"selection_bound\" and
  \"insert\" marks are in the same place, then there is no current selection.
  The @fun{gtk-text-buffer-selection-bounds} function is another convenient
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[pos]{a @class{gtk-text-iter} iterator where to put the cursor}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them to the same place in two steps with the
  @fun{gtk-text-buffer-move-mark} function, you will temporarily select a region
  in between their old and new locations, which can be pretty inefficient since
  the temporarily selected region will force stuff to be recalculated. This
  function moves them as a unit, which can be optimized.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (pos (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-buffer-place-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_select_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_select_range" gtk-text-buffer-select-range) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[insertion]{a @class{gtk-text-iter} iterator where to put the
    \"insert\" mark}
  @argument[selection]{a @class{gtk-text-iter} iterator where to put the
    \"selection_bound\" mark}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them in two steps with the @fun{gtk-text-buffer-move-mark}
  function, you will temporarily select a region in between their old and new
  locations, which can be pretty inefficient since the temporarily selected
  region will force stuff to be recalculated. This function moves them as a
  unit, which can be optimized.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-move-mark}"
  (buffer (g-object gtk-text-buffer))
  (insertion (g-boxed-foreign gtk-text-iter))
  (selection (g-boxed-foreign gtk-text-iter)))

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
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tag]{a @class{gtk-text-tag} object, or a string with the tag name}
  @argument[start]{a @class{gtk-text-iter} iterator with the start bound of
    the range to be tagged}
  @argument[end]{a @class{gtk-text-iter} iterator with the end bound of the
    range to be tagged}
  @begin{short}
    Emits the \"apply-tag\" signal on the text buffer.
  @end{short}
  The default handler for the signal applies @arg{tag} to the given range.
  The @arg{start} and @arg{end} iterators do not have to be in order.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}"
  (if (stringp tag)
      (%gtk-text-buffer-apply-tag-by-name buffer tag start end)
      (%gtk-text-buffer-apply-tag buffer tag start end)))

(export 'gtk-text-buffer-apply-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag_by_name ()                   no exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_apply_tag_by_name"
          %gtk-text-buffer-apply-tag-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of a named @class{gtk-text-tag} object}
  @argument[start]{a @class{gtk-text-iter} iterator with the start bound of
    the range to be tagged}
  @argument[end]{a @class{gtk-text-iter} iterator with the end bound of the
    range to be tagged}
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[tag]{a @class{gtk-text-tag} object, or a string with the tag name}
  @argument[start]{a @class{gtk-text-iter} iterator with the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk-text-iter} iterator with the end bound of the
    range to be untagged}
  @begin{short}
    Emits the \"remove-tag\" signal.
  @end{short}
  The default handler for the signal removes all occurrences of @arg{tag} from
  the given range. The @arg{start} and @arg{end} iterators do not have to be in
  order.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-class{gtk-text-iter}"
  (if (stringp tag)
      (%gtk-text-buffer-remove-tag-by-name buffer tag start end)
      (%gtk-text-buffer-remove-tag buffer tag start end)))

(export 'gtk-text-buffer-remove-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag_by_name ()                  not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_tag_by_name"
          %gtk-text-buffer-remove-tag-by-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
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

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_all_tags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_remove_all_tags" gtk-text-buffer-remove-all-tags)
    :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[start]{a @class{gtk-text-iter} iterator with the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk-text-iter} iterator with the end bound of the
    range to be untagged}
  @begin{short}
    Removes all tags in the range between the @arg{start} and @arg{end}
    iterators.
  @end{short}
  Be careful with this function: it could remove tags added in code unrelated
  to the code you are currently writing. That is, using this function is
  probably a bad idea if you have two or more unrelated code sections that add
  tags.
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
 "@version{*2021-12-17}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with the name of the new tag, or @code{nil}}
  @argument[args]{list of property keywords and values}
  @return{The new @class{gtk-text-tag} object.}
  @begin{short}
    Creates a tag and adds it to the tag table for the text buffer.
  @end{short}
  Equivalent to calling the @fun{gtk-text-tag-new} function and then adding the
  tag to the tag table of the text buffer.

  If the @arg{name} argument is @code{nil}, the tag is anonymous. If the
  @arg{name} argument is non-@code{nil}, a tag called @arg{name} must not
  already exist in the tag table for this text buffer.

  The @arg{args} argument is a list of properties and values to set on the
  tag.
  @begin[Example]{dictionary}
    Create and add a tag with name \"font-italic\" to the text buffer.
    @begin{pre}
(defvar buffer (gtk-text-buffer-new)) => BUFFER
(gtk-text-buffer-create-tag buffer \"font-italic\"
                                   :font \"fixed\" :style :italic)
=> #<GTK-TEXT-TAG {1002193283@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-tag}
  @see-function{gtk-text-tag-new}"
  (let ((tag (apply #'make-instance 'gtk-text-tag :name name args)))
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
  (line :int)
  (offset :int))

(defun gtk-text-buffer-iter-at-line-offset (buffer line offset)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-18}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[line]{an integer with the line number counting from 0}
  @argument[offset]{an integer with the char offset from the start of the line}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{offset} within the given line.
  @end{short}
  Note characters, not bytes, UTF-8 may encode one character as multiple bytes.

  If the @arg{line} argument is greater than the number of lines in the text
  buffer, the end iterator is returned. And if the @arg{offset} argument is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line-offset buffer iter line offset)
    iter))

(export 'gtk-text-buffer-iter-at-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_offset () -> gtk-text-buffer-iter-at-offset
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_offset"
          %gtk-text-buffer-iter-at-offset) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (offset :int))

(defun gtk-text-buffer-iter-at-offset (buffer offset)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[offset]{an integer with the char offset from the start of the text
    buffer, counting from 0, or -1}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to a position @arg{offset} chars from the
    start of the entire text buffer.
  @end{short}
  If the @arg{offset} argument is -1 or greater than the number of characters
  in the text buffer, the iterator is initialized to the end iterator, the
  iterator one past the last valid character in the text buffer.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-offset buffer iter offset)
    iter))

(export 'gtk-text-buffer-iter-at-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line () -> gtk-text-buffer-iter-at-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_line"
          %gtk-text-buffer-iter-at-line) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (line :int))

(defun gtk-text-buffer-iter-at-line (buffer line)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[line]{an integer with the line number counting from 0}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to the start of the given line.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line buffer iter line)
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
  (line :int)
  (index :int))

(defun gtk-text-buffer-iter-at-line-index (buffer line index)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[line]{an integer with the line number counting from 0}
  @argument[index]{an integer with the byte index from the start of the line}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{index} within the given line.
  @end{short}
  The @arg{index} argument must be the start of a UTF-8 character. Note bytes,
  not characters, UTF-8 may encode one character as multiple bytes.

  If the @arg{line} argument is greater than the number of lines in the text
  buffer, the end iterator is returned. And if the @arg{index} argument is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-buffer-iter-at-line-index buffer iter line index)
    iter))

(export 'gtk-text-buffer-iter-at-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_mark () -> gtk-text-buffer-iter-at-mark
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_iter_at_mark"
          %gtk-text-buffer-iter-at-mark) :void
  (buffer (g-object gtk-text-buffer))
  (iter (g-boxed-foreign gtk-text-iter))
  (mark (g-object gtk-text-mark)))

(defun gtk-text-buffer-iter-at-mark (buffer mark)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mark]{a @class{gtk-text-mark} object, or a string with the mark
    name in the text buffer}
  @return{A @class{gtk-text-iter} interator.}
  @begin{short}
    Returns the iterator with the current position of @arg{mark}.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-mark}"
  (let ((iter (make-instance 'gtk-text-iter))
        (mark (if (stringp mark) (gtk-text-buffer-mark buffer mark) mark)))
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[anchor]{a @class{gtk-text-child-anchor} anchor that appears in text
    buffer}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Obtains the location of @arg{anchor} within the text buffer.
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
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Returns an iterator with the first position in the text buffer.
  @end{short}
  This is the same as using the @fun{gtk-text-buffer-iter-at-offset} function
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
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A @class{gtk-text-iter} iterator.}
  @begin{short}
    Returns an iterator with the \"end iterator\", one past the last valid
    character in the text buffer.
  @end{short}
  If dereferenced with the @fun{gtk-text-iter-char} function, the end iterator
  has a character value of 0. The entire text buffer lies in the range from the
  first position in the text buffer to the end iterator. Call the
  @fun{gtk-text-buffer-start-iter} function to get character position 0.
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @arg{start} -- a @class{gtk-text-iter} iterator with the first position in
                   the text buffer @br{}
    @arg{end}   -- a @class{gtk-text-iter} iterator with the end position in
                   the text buffer
  @end{return}
  @begin{short}
    Retrieves the first and last iterators in the text buffer, i.e. the entire
    text buffer lies within the range [@arg{start}, @arg{end}).
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}"
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
                   (g-object gtk-text-buffer) buffer
                   :boolean setting
                   :void)
  setting)

(defcfun ("gtk_text_buffer_get_modified" gtk-text-buffer-modified) :boolean
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-modified buffer) => setting}
  @syntax[]{(setf (gtk-text-buffer-modified buffer) setting)}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[setting]{a boolean with the modification flag setting}
  @begin{short}
    Returns @em{true} if the text buffer has been modified.
  @end{short}

  The @sym{gtk-text-buffer-modified} function indicates whether the text buffer
  has been modified since the last call to the
  @sym{(setf gtk-text-buffer-modified)} function.

  Used to keep track of whether the text buffer has been modified since the last
  time it was saved. Whenever the text buffer is saved to disk, call the
  @sym{(setf gtk-text-buffer-modified)} function with the @em{false} value.
  When the text buffer is modified, it will automatically toggle on the
  modified bit again. When the modified bit flips, the text buffer emits a
  \"modified-changed\" signal.
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
 "@version{2020-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @return{A boolean whether there was a non-empty selection to delete.}
  @begin{short}
    Deletes the range between the \"insert\" and \"selection_bound\" marks,
    that is, the currently selected text.
  @end{short}
  If the @arg{interactive} argument is @em{true}, the editability of the
  selection will be considered, users cannot delete uneditable text.
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
  (override (g-boxed-foreign gtk-text-iter))
  (editable :boolean))

(defun gtk-text-buffer-paste-clipboard (buffer clipboard &key override editable)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object to paste from}
  @argument[override]{a @class{gtk-text-iter} location to insert pasted text,
    or @code{nil} to insert at the cursor}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Pastes the contents of a clipboard at the insertion point, or at
    @arg{override}.
  @end{short}
  @begin[Note]{dictionary}
    Pasting is asynchronous, that is, we will ask for the paste data and return,
    and at some point later after the main loop runs, the paste data will be
    inserted.
  @end{dictionary}
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}
  @see-class{gtk-text-iter}"
  (%gtk-text-buffer-paste-clipboard buffer clipboard override editable))

(export 'gtk-text-buffer-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_copy_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_copy_clipboard" gtk-text-buffer-copy-clipboard) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object to copy to}
  @begin{short}
    Copies the currently selected text to the clipboard.
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object to cut to}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Copies the currently selected text to a clipboard, then deletes the text
    if it is editable.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard))
  (editable :boolean))

(export 'gtk-text-buffer-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bounds () -> gtk-text-buffer-selection-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_selection_bounds"
          %gtk-text-buffer-selection-bounds) :boolean
  (buffer (g-object gtk-text-buffer))
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter)))

(defun gtk-text-buffer-selection-bounds (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @arg{start} -- a @class{gtk-text-iter} iterator with the selection start,
                   or @code{nil} @br{}
    @arg{end}   -- a @class{gtk-text-iter} iterator with the selection end,
                   or @code{nil}
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

;; Example implementation of a macro, not exported at this time.

(defmacro with-text-buffer-user-action ((buffer) &body body)
  (let ((g (gensym)))
    `(let ((,g ,buffer))
       (gtk-text-buffer-begin-user-action ,g)
       (unwind-protect
         (progn ,@body)
         (gtk-text-buffer-end-user-action ,g)))))

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_begin_user_action" gtk-text-buffer-begin-user-action)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Called to indicate that the text buffer operations between here and a call
    to the @fun{gtk-text-buffer-end-user-action} function are part of a single
    user visible operation.
  @end{short}
  The operations between the @sym{gtk-text-buffer-begin-user-action} and
  @fun{gtk-text-buffer-end-user-action} functions can then be grouped when
  creating an undo stack. The text buffer maintains a count of calls to the
  @sym{gtk-text-buffer-begin-user-action} function that have not been closed
  with a call to the @fun{gtk-text-buffer-end-user-action} function, and emits
  the \"begin-user-action\" and \"end-user-action\" signals only for the
  outermost pair of calls. This allows you to build user actions from other
  user actions.

  The \"interactive\" text buffer mutation functions automatically call
  begin/end user action around the text buffer operations they perform, so there
  is no need to add extra calls if the user action consists solely of a single
  call to one of those functions.
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-end-user-action}"
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-text-buffer-begin-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_end_user_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_end_user_action" gtk-text-buffer-end-user-action)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Should be paired with a call to the @fun{gtk-text-buffer-begin-user-action}
    function.
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object}
  @begin{short}
    Adds a clipboard to the list of clipboards in which the selection contents
    of the text buffer are available.
  @end{short}
  In most cases, the clipboard will be of type \"PRIMARY\" for a view of the
  text buffer.
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[clipboard]{a @class{gtk-clipboard} object added to the text buffer}
  @begin{short}
    Removes a clipboard added with the
    @fun{gtk-text-buffer-add-selection-clipboard} function.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-class{gtk-clipboard}
  @see-function{gtk-text-buffer-add-selection-clipboard}"
  (buffer (g-object gtk-text-buffer))
  (clipboard (g-object gtk-clipboard)))

(export 'gtk-text-buffer-remove-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferDeserializeFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-buffer-deserialize-func :boolean
    ((buffer (g-object gtk-text-buffer))
     (content (g-object gtk-text-buffer))
     (iter (g-boxed-foreign gtk-text-iter))
     (data :pointer)
     (len g-size)
     (create :boolean)
     (user :pointer)
     (err :pointer))
  (with-catching-to-g-error (err)
    (let ((fn (get-stable-pointer-value user)))
      (restart-case
        (let ((bytes (iter (with bytes = (make-array len
                                                     :element-type
                                                     '(unsigned-byte 8)))
                           (for i from 0 below len)
                           (setf (aref bytes i) (mem-ref data :uint8 i))
                           (finally (return bytes)))))
          (progn
            (funcall fn buffer content iter bytes create)
            t))
        (return-from-gtk-text-buffer-deserialize-func
            ()
            (error 'g-error-condition
                   :domain "cl-cffi-gtk"
                   :code 0
                   :message
                   "return-from-gtk-text-buffer-deserialize-func"))))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-deserialize-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-text-buffer-deserialize-func atdoc:*external-symbols*)
 "@version{2021-11-16}
  @begin{short}
    A function that is called to deserialize rich text that has been serialized
    with the @fun{gtk-text-buffer-serialize} function, and insert it at
    @arg{iter}.
  @end{short}
  @begin{pre}
 lambda (buffer content iter data len create)
  @end{pre}
  @begin[code]{table}
    @entry[buffer]{The @class{gtk-text-buffer} object the format is registered
      with.}
    @entry[content]{The @class{gtk-text-buffer} object to deserialize into.}
    @entry[iter]{The @class{gtk-text-iter} insertion point for the deserialized
      text.}
    @entry[data]{The pointer to the data to deserialize.}
    @entry[len]{The length of @arg{data}.}
    @entry[create]{@em{True} if deserializing may create tags.}
    @entry[Return]{@em{True} on success, @em{false} otherwise.}
  @end{table}
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-buffer-serialize}")

(export 'gtk-text-buffer-deserialize-func)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_deserialize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_deserialize" %gtk-text-buffer-deserialize) :boolean
  (buffer (g-object gtk-text-buffer))
  (content (g-object gtk-text-buffer))
  (format gdk-atom-as-string)
  (iter (g-boxed-foreign gtk-text-iter))
  (data :pointer)
  (len g-size)
  (err :pointer))

(defun gtk-text-buffer-deserialize (buffer content format iter data)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object format is registered with}
  @argument[content]{a @class{gtk-text-buffer} object to deserialize into}
  @argument[format]{a @symbol{gdk-atom} as a string with the rich text format
    to use for deserializing}
  @argument[iter]{a @class{gtk-text-iter} insertion point for the deserialized
    text}
  @argument[data]{a pointer to the data to deserialize}
  @return{@em{True} on success, @code{nil} otherwise.}
  @begin{short}
    This function deserializes rich text in @arg{format} and inserts it at
    @arg{iter}.
  @end{short}
  The rich text formats to be used must be registered using the
  @fun{gtk-text-buffer-register-deserialize-format} or
  @fun{gtk-text-buffer-register-deserialize-tagset} functions beforehand.
  @see-class{gtk-text-buffer}
  @see-class{gtk-text-iter}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-deserialize-format}
  @see-function{gtk-text-buffer-register-deserialize-tagset}"
  (let ((bytes (foreign-alloc :uint8 :count (length data))))
    (iter (for i from 0 below (length data))
          (setf (mem-aref bytes :uint8 i) (aref data i)))
    (unwind-protect
      (with-g-error (err)
        (%gtk-text-buffer-deserialize buffer
                                      content
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
;;; -> gtk-text-buffer-deserialize-can-create-tags
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-buffer-deserialize-can-create-tags) (value buffer format)
  (foreign-funcall "gtk_text_buffer_deserialize_set_can_create_tags"
                   (g-object gtk-text-buffer) buffer
                   gdk-atom-as-string format
                   :boolean value
                   :void)
  value)

(defcfun ("gtk_text_buffer_deserialize_get_can_create_tags"
           gtk-text-buffer-deserialize-can-create-tags) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @syntax[]{(gtk-text-buffer-deserialize-can-create-tags buffer format) => create}
  @syntax[]{(setf (gtk-text-buffer-deserialize-can-create-tags buffer format) create)}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{a @symbol{gdk-atom} as a string representing a registered
    rich text format}
  @argument[create]{a boolean whether deserializing this format may create tags}
  @begin{short}
    Use this function to allow a rich text deserialization function to create
    new tags in the receiving text buffer.
  @end{short}
  Note that using this function is almost always a bad idea, because the rich
  text functions you register should know how to map the rich text format they
  handle to your text buffers set of tags.

  The ability of creating new tags in the receiving text buffer is meant for
  special rich text formats like the internal one that is registered using the
  @fun{gtk-text-buffer-register-deserialize-tagset} function, because that
  format is essentially a dump of the internal structure of the source buffer,
  including its tag names.

  You should allow creation of tags only if you know what you are doing, e.g.
  if you defined a tagset name for your application text buffers and you know
  that it is fine to receive new tags from these buffers, because you know that
  your application can handle the newly created tags.
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-deserialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string))

(export 'gtk-text-buffer-deserialize-can-create-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_deserialize_formats ()
;;; -> gtk-text-buffer-deserialize-formats
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_get_deserialize_formats"
          %gtk-text-buffer-deserialize-formats) (:pointer gdk-atom-as-string)
  (buffer (g-object gtk-text-buffer))
  (n-formats (:pointer :int)))

(defun gtk-text-buffer-deserialize-formats (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A list of @symbol{gdk-atom} as strings representing the registered
    formats.}
  @begin{short}
    This function returns the rich text deserialize formats registered with the
    text buffer using the @fun{gtk-text-buffer-register-deserialize-format} or
    @fun{gtk-text-buffer-register-deserialize-tagset} functions.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A list of @symbol{gdk-atom} as strings representing the registered
    formats.}
  @begin{short}
    This function returns the rich text serialize formats registered with
    @arg{buffer} using the @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-serialize-tagset} functions.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
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
  (mime :string)
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-text-buffer-register-deserialize-format (buffer mime func)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mime]{a string with the MIME type of the format}
  @argument[func]{a @symbol{gtk-text-buffer-deserialize-func} deserialize
    function to register}
  @begin{return}
    The @symbol{gdk-atom} as a string that corresponds to the newly registered
    MIME type of the format.
  @end{return}
  @begin{short}
    This function registers a rich text deserialization function along with its
    MIME type with the passed text buffer.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-symbol{gtk-text-buffer-deserialize-func}"
  (%gtk-text-buffer-register-deserialize-format
                                  buffer
                                  mime
                                  (callback gtk-text-buffer-deserialize-func)
                                  (allocate-stable-pointer func)
                                  (callback stable-pointer-destroy-notify)))

(export 'gtk-text-buffer-register-deserialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_deserialize_tagset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_deserialize_tagset"
           gtk-text-buffer-register-deserialize-tagset) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with an optional tagset name, or @code{nil}}
  @begin{return}
    The @symbol{gdk-atom} as a string that corresponds to the newly registered
    MIME type of the format.
  @end{return}
  @begin{short}
    This function registers internal rich text serialization format of GTK with
    the passed text buffer.
  @end{short}
  See the @fun{gtk-text-buffer-register-serialize-tagset} function for details.
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (name :string))

(export 'gtk-text-buffer-register-deserialize-tagset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_format ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_serialize_format"
          %gtk-text-buffer-register-serialize-format) gdk-atom-as-string
  (buffer (g-object gtk-text-buffer))
  (mime :string)
  (func :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun gtk-text-buffer-register-serialize-format (buffer mime func)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[mime]{a string with the MIME type of the format}
  @argument[func]{the @symbol{gtk-text-buffer-serialize-func} serialize function
    to register}
  @begin{return}
    The @symbol{gdk-atom} as a string that corresponds to the newly registered
    MIME type of the format.
  @end{return}
  @begin{short}
    This function registers a rich text serialization function along with its
    MIME type with the passed text buffer.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-symbol{gtk-text-buffer-serialize-func}"
  (%gtk-text-buffer-register-serialize-format
                                  buffer
                                  mime
                                  (callback gtk-text-buffer-serialize-func)
                                  (allocate-stable-pointer func)
                                  (callback stable-pointer-destroy-notify)))

(export 'gtk-text-buffer-register-serialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_tagset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_register_serialize_tagset"
          gtk-text-buffer-register-serialize-tagset) gdk-atom-as-string
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[name]{a string with an optional tagset name, or @code{nil}}
  @begin{return}
    The @symbol{gdk-atom} as a string that corresponds to the newly registered
    MIME type of the format.
  @end{return}
  @begin{short}
    This function registers the internal rich text serialization format of GTK
    with the passed text buffer.
  @end{short}
  The internal format does not comply to any standard rich text format and only
  works between @class{gtk-text-buffer} objects. It is capable of serializing
  all tags and embedded pixbufs of the text buffer.

  This function is just a wrapper around the
  @fun{gtk-text-buffer-register-serialize-format} function. The MIME type used
  for registering is \"application/x-gtk-text-buffer-rich-text\", or
  \"application/x-gtk-text-buffer-rich-text;format=tagset_name\" if a tagset
  name was passed.

  The @arg{name} argument can be used to restrict the transfer of rich text to
  text buffers with compatible sets of tags, in order to avoid unknown tags from
  being pasted. It is probably the common case to pass an identifier which is
  not @code{nil} here, since the @code{nil} tagset requires the receiving text
  buffer to deal with with pasting of arbitrary tags.
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-serialize-format}"
  (buffer (g-object gtk-text-buffer))
  (name :string))

(export 'gtk-text-buffer-register-serialize-tagset)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferSerializeFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-text-buffer-serialize-func :pointer
    ((buffer (g-object gtk-text-buffer))
     (content (g-object gtk-text-buffer))
     (start (g-boxed-foreign gtk-text-iter))
     (end (g-boxed-foreign gtk-text-iter))
     (length (:pointer g-size))
     (user-data :pointer))
  (let ((fn (get-stable-pointer-value user-data)))
    (restart-case
      (let* ((bytes (funcall fn buffer content start end))
             (bytes-ptr (g-malloc (length bytes))))
        (setf (mem-ref length 'g-size) (length bytes))
        (iter (for i from 0 below (length bytes))
              (setf (mem-aref bytes-ptr :uint8 i) (aref bytes i)))
        bytes-ptr)
      (return-from-gtk-text-buffer-serialize-func () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-buffer-serialize-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-text-buffer-serialize-func atdoc:*external-symbols*)
 "@version{2021-11-16}
  @begin{short}
    A function that is called to serialize the content of a text buffer. It
    must return the serialized form of the content.
  @end{short}
  @begin{pre}
 lambda (buffer content start end)
  @end{pre}
  @begin[code]{table}
    @entry[buffer]{The @class{gtk-text-buffer} object for which the format
      is registered.}
    @entry[content]{The @class{gtk-text-buffer} object to serialize.}
    @entry[start]{A @class{gtk-text-iter} start iterator of the block of text
      to serialize.}
    @entry[end]{A @class{gtk-text-iter} end iterator of the block of text
      to serialize.}
    @entry[Return]{A newly allocated array of @code{guint8} which contains the
      serialized data, or @code{NULL} if an error occured.}
  @end{table}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-serialize}")

(export 'gtk-text-buffer-serialize-func)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_serialize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_buffer_serialize" %gtk-text-buffer-serialize) :pointer
  (buffer (g-object gtk-text-buffer))
  (content (g-object gtk-text-buffer))
  (format gdk-atom-as-string)
  (start (g-boxed-foreign gtk-text-iter))
  (end (g-boxed-foreign gtk-text-iter))
  (len (:pointer g-size)))

(defun gtk-text-buffer-serialize (buffer content format start end)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-16}
  @argument[buffer]{the @class{gtk-text-buffer} object the format is registered
    with}
  @argument[content]{the @class{gtk-text-buffer} object to serialize}
  @argument[format]{a @symbol{gdk-atom} as a string with the rich text format
    to use for serializing}
  @argument[start]{a @class{gtk-text-iter} start iterator of the block of text
    to serialize}
  @argument[end]{a @class{gtk-text-iter} end iterator of the block of text to
    serialize}
  @return{A list with the serialized data, encoded as @arg{format}.}
  @begin{short}
    This function serializes the portion of text between the @arg{start} and
    @arg{end} iterator in the rich text format represented by @arg{format}.
  @end{short}

  The @arg{format} arguments to be used must be registered using the
  @fun{gtk-text-buffer-register-serialize-format} or
  @fun{gtk-text-buffer-register-serialize-tagset} functions beforehand.
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (with-foreign-object (len 'g-size)
    (let ((bytes (%gtk-text-buffer-serialize buffer
                                             content
                                             format
                                             start
                                             end
                                             len)))
      (iter (for i from 0 to (mem-ref len 'g-size))
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{a @symbol{gdk-atom} as a string representing a registered
    rich text format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the @fun{gtk-text-buffer-register-deserialize-format} or
    @fun{gtk-text-buffer-register-deserialize-tagset} functions.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
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
 "@version{2021-11-16}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @argument[format]{a @symbol{gdk-atom} as a string representing a registered
    rich text format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-serialize-tagset} functions.
  @end{short}
  @see-class{gtk-text-buffer}
  @see-symbol{gdk-atom}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-serialize-tagset}"
  (buffer (g-object gtk-text-buffer))
  (format gdk-atom-as-string))

(export 'gtk-text-buffer-unregister-serialize-format)

;;; --- End of file gtk.text-buffer.lisp ---------------------------------------
