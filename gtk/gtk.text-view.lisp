;;; ----------------------------------------------------------------------------
;;; gtk.text-view.lisp
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
;;; GtkTextView
;;;
;;;     Widget that displays a GtkTextBuffer
;;;
;;; Types and Values
;;;
;;;     GtkTextView
;;;     GtkTextViewLayer
;;;     GtkTextWindowType
;;;     GtkTextExtendSelection
;;;     GtkWrapMode                                        ---> gtk.text-tag
;;;     GtkTextChildAnchor
;;;
;;;     GTK_TEXT_VIEW_PRIORITY_VALIDATE
;;;
;;; Functions
;;;
;;;     gtk_text_view_new
;;;     gtk_text_view_new_with_buffer
;;;     gtk_text_view_set_buffer
;;;     gtk_text_view_get_buffer
;;;     gtk_text_view_get_hadjustment
;;;     gtk_text_view_get_vadjustment
;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;     gtk_text_view_get_window
;;;     gtk_text_view_get_window_type
;;;     gtk_text_view_set_border_window_size
;;;     gtk_text_view_get_border_window_size
;;;     gtk_text_view_forward_display_line
;;;     gtk_text_view_backward_display_line
;;;     gtk_text_view_forward_display_line_end
;;;     gtk_text_view_backward_display_line_start
;;;     gtk_text_view_starts_display_line
;;;     gtk_text_view_move_visually
;;;     gtk_text_view_add_child_at_anchor
;;;
;;;     gtk_text_child_anchor_new
;;;     gtk_text_child_anchor_get_widgets
;;;     gtk_text_child_anchor_get_deleted
;;;     gtk_text_view_add_child_in_window
;;;     gtk_text_view_move_child
;;;     gtk_text_view_set_wrap_mode
;;;     gtk_text_view_get_wrap_mode
;;;     gtk_text_view_set_editable
;;;     gtk_text_view_get_editable
;;;     gtk_text_view_set_cursor_visible
;;;     gtk_text_view_get_cursor_visible

;;;     gtk_text_view_reset_cursor_blink ()

;;;     gtk_text_view_set_overwrite
;;;     gtk_text_view_get_overwrite

;;;     gtk_text_view_set_pixels_above_lines
;;;     gtk_text_view_get_pixels_above_lines
;;;     gtk_text_view_set_pixels_below_lines
;;;     gtk_text_view_get_pixels_below_lines
;;;     gtk_text_view_set_pixels_inside_wrap
;;;     gtk_text_view_get_pixels_inside_wrap

;;;     gtk_text_view_set_justification
;;;     gtk_text_view_get_justification
;;;     gtk_text_view_set_left_margin
;;;     gtk_text_view_get_left_margin
;;;     gtk_text_view_set_right_margin
;;;     gtk_text_view_get_right_margin

;;;     gtk_text_view_set_top_margin ()
;;;     gtk_text_view_get_top_margin ()
;;;     gtk_text_view_set_bottom_margin ()
;;;     gtk_text_view_get_bottom_margin ()

;;;     gtk_text_view_set_indent
;;;     gtk_text_view_get_indent

;;;     gtk_text_view_set_tabs
;;;     gtk_text_view_get_tabs
;;;     gtk_text_view_set_accepts_tab
;;;     gtk_text_view_get_accepts_tab
;;;     gtk_text_view_get_default_attributes
;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context
;;;     gtk_text_view_set_input_purpose
;;;     gtk_text_view_get_input_purpose
;;;     gtk_text_view_set_input_hints
;;;     gtk_text_view_get_input_hints

;;;     gtk_text_view_set_monospace ()
;;;     gtk_text_view_get_monospace ()
;;;
;;; Properties
;;;
;;;            gboolean   accepts-tab              Read / Write
;;;                gint   bottom-margin            Read / Write
;;;       GtkTextBuffer*  buffer                   Read / Write
;;;            gboolean   cursor-visible           Read / Write
;;;            gboolean   editable                 Read / Write
;;;               gchar*  im-module                Read / Write
;;;                gint   indent                   Read / Write
;;;       GtkInputHints   input-hints              Read / Write
;;;     GtkInputPurpose   input-purpose            Read / Write
;;;    GtkJustification   justification            Read / Write
;;;                gint   left-margin              Read / Write
;;;            gboolean   monospace                Read / Write
;;;            gboolean   overwrite                Read / Write
;;;                gint   pixels-above-lines       Read / Write
;;;                gint   pixels-below-lines       Read / Write
;;;                gint   pixels-inside-wrap       Read / Write
;;;            gboolean   populate-all             Read / Write
;;;                gint   right-margin             Read / Write
;;;       PangoTabArray*  tabs                     Read / Write
;;;                gint   top-margin               Read / Write
;;;         GtkWrapMode   wrap-mode                Read / Write
;;;
;;; Style Properties
;;;
;;;            GdkColor*  error-underline-color    Read
;;;
;;; Signals
;;;
;;;                void   backspace                Action
;;;                void   copy-clipboard           Action
;;;                void   cut-clipboard            Action
;;;                void   delete-from-cursor       Action
;;;            gboolean   extend-selection         Run Last
;;;                void   insert-at-cursor         Action
;;;                void   insert-emoji             Action
;;;                void   move-cursor              Action
;;;                void   move-viewport            Action
;;;                void   paste-clipboard          Action
;;;                void   populate-popup           Run Last
;;;                void   preedit-changed          Action
;;;                void   select-all               Action
;;;                void   set-anchor               Action
;;;                void   toggle-cursor-visible    Action
;;;                void   toggle-overwrite         Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GInitiallyUnowned
;;;     │   ╰── GtkWidget
;;;     │       ╰── GtkContainer
;;;     │           ╰── GtkTextView
;;;     ╰── GtkTextChildAnchor
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTextView implements AtkImplementorIface, GtkBuildable and
;;;     GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_TEXT_VIEW_PRIORITY_VALIDATE
;;;
;;; #define GTK_TEXT_VIEW_PRIORITY_VALIDATE (GDK_PRIORITY_REDRAW + 5)
;;;
;;; The priority at which the text view validates onscreen lines in an idle job
;;; in the background.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkTextWindowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTextWindowType" gtk-text-window-type
  (:export t
   :type-initializer "gtk_text_window_type_get_type")
  (:private 0)
  (:widget 1)
  (:text 2)
  (:left 3)
  (:right 4)
  (:top 5)
  (:bottom 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-window-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-window-type atdoc:*external-symbols*)
 "@version{2013-3-25}
  @short{Used to reference the parts of @class{gtk-text-view}.}
  @begin{pre}
(define-g-enum \"GtkTextWindowType\" gtk-text-window-type
  (:export t
   :type-initializer \"gtk_text_window_type_get_type\")
  (:private 0)
  (:widget 1)
  (:text 2)
  (:left 3)
  (:right 4)
  (:top 5)
  (:bottom 6))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTextViewLayer
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(define-g-enum "GtkTextViewLayer" gtk-text-view-layer
  (:export t
   :type-initializer "gtk_text_view_layer_get_type")
  (:below 0)
  (:above 1)
  #+gtk-3-20
  (:below-text 2)
  #+gtk-3-20
  (:above-text 3)
  )

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-layer atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-view-layer atdoc:*external-symbols*)
 "@version{2019-4-14}
  @begin{short}
    Used to reference the layers of @class{gtk-text-view} for the purpose of
    customized drawing with the @code{::draw_layer} vfunc.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkTextViewLayer\" gtk-text-view-layer
  (:export t
   :type-initializer \"gtk_text_view_layer_get_type\")
  (:below 0)
  (:above 1)
  (:below-text 2)
  (:above-text 3))
  @end{pre}
  @begin[code]{table}
    @entry[:below]{Old deprecated layer, use @code{:below-text} instead.}
    @entry[:above]{Old deprecated layer, use @code{:above-text} instead.}
    @entry[:below-text]{The layer rendered below the text, but above the
      background. Since 3.20}
    @entry[:above-text]{The layer rendered above the text. Since 3.20}
  @end{table}
  Since 3.14
  @see-class{gtk-text-view}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTextExtendSelection
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(define-g-enum "GtkTextExtendSelection" gtk-text-extend-selection
  (:export t
   :type-initializer "gtk_text_extend_selection_get_type")
  (:word 0)
  (:line 1))

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-extend-selection atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-extend-selection atdoc:*external-symbols*)
 "@version{2019-4-14}
  @begin{short}
    Granularity types that extend the text selection. Use the
    \"extend-selection\" signal to customize the selection.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkTextExtendSelection\" gtk-text-extend-selection
  (:export t
   :type-initializer \"gtk_text_extend_selection_get_type\")
  (:word 0)
  (:line 1))
  @end{pre}
  @begin[code]{table}
    @entry[:word]{Selects the current word. It is triggered by a double-click
      for example.}
    @entry[:line]{Selects the current line. It is triggered by a triple-click
      for example.}
  @end{table}
  Since 3.16
  @see-class{gtk-text-view}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTextView
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextView" gtk-text-view
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_text_view_get_type")
  (
;;;            gboolean   accepts-tab              Read / Write

   (accepts-tab
    gtk-text-view-accepts-tab
    "accepts-tab" "gboolean" t t)

;;;                gint   bottom-margin            Read / Write

   #+gtk-3-18
   (bottom-margin
    gtk-text-view-bottom-margin
    "bottom-margin" "gint" t t)

;;;       GtkTextBuffer*  buffer                   Read / Write

   (buffer
    gtk-text-view-buffer
    "buffer" "GtkTextBuffer" t t)

;;;            gboolean   cursor-visible           Read / Write

   (cursor-visible
    gtk-text-view-cursor-visible
    "cursor-visible" "gboolean" t t)

;;;            gboolean   editable                 Read / Write

   (editable
    gtk-text-view-editable
    "editable" "gboolean" t t)

;;;               gchar*  im-module                Read / Write

   (im-module
    gtk-text-view-im-module
    "im-module" "gchararray" t t)

;;;                gint   indent                   Read / Write

   (indent
    gtk-text-view-indent
    "indent" "gint" t t)

;;;       GtkInputHints   input-hints              Read / Write

   #+gtk-3-6
   (input-hints
    gtk-text-view-input-hints
    "input-hints" "GtkInputHints" t t)

;;;     GtkInputPurpose   input-purpose            Read / Write

   #+gtk-3-6
   (input-purpose
    gtk-text-view-input-purpose
    "input-purpose" "GtkInputPurpose" t t)

;;;    GtkJustification   justification            Read / Write

   (justification
    gtk-text-view-justification
    "justification" "GtkJustification" t t)

;;;                gint   left-margin              Read / Write

   (left-margin
    gtk-text-view-left-margin
    "left-margin" "gint" t t)

;;;            gboolean   monospace                Read / Write

   (monospace
    gtk-text-view-monospace
    "monospace" "gboolean" t t)

;;;            gboolean   overwrite                Read / Write

   (overwrite
    gtk-text-view-overwrite
    "overwrite" "gboolean" t t)

;;;                gint   pixels-above-lines       Read / Write

   (pixels-above-lines
    gtk-text-view-pixels-above-lines
    "pixels-above-lines" "gint" t t)

;;;                gint   pixels-below-lines       Read / Write

   (pixels-below-lines
    gtk-text-view-pixels-below-lines
    "pixels-below-lines" "gint" t t)

;;;                gint   pixels-inside-wrap       Read / Write

   (pixels-inside-wrap
    gtk-text-view-pixels-inside-wrap
    "pixels-inside-wrap" "gint" t t)

;;;            gboolean   populate-all             Read / Write

   (populate-all
    gtk-text-view-populate-all
    "populate-all" "gboolean" t t)

;;;                gint   right-margin             Read / Write

   (right-margin
    gtk-text-view-right-margin
    "right-margin" "gint" t t)

;;;       PangoTabArray*  tabs                     Read / Write

   (tabs
    gtk-text-view-tabs
    "tabs" "PangoTabArray" t t)

;;;                gint   top-margin               Read / Write

   (top-margin
    gtk-text-view-top-margin
    "top-margin" "gint" t t)

;;;         GtkWrapMode   wrap-mode                Read / Write

   (wrap-mode
    gtk-text-view-wrap-mode
    "wrap-mode" "GtkWrapMode" t t)
))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-view 'type)
 "@version{2013-5-5}
  @image[multiline-text]{}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 textview.view
 ├── border.top
 ├── border.left
 ├── text
 │   ╰── [selection]
 ├── border.right
 ├── border.bottom
 ╰── [window.popup]
    @end{pre}
    @sym{gtk-text-view} has a main css node with name @code{textview} and style
    class @code{.view}, and subnodes for each of the border windows, and the
    main text area, with names @code{border} and @code{text}, respectively. The
    border nodes each get one of the style classes @code{.left}, @code{.right},
    @code{.top} or @code{.bottom}.

    A node representing the selection will appear below the text node.

    If a context menu is opened, the window node will appear as a subnode of the
    main node.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[error-underline-color]{entry}
        The @code{error-underline-color} style property of type
        @class{gdk-color} (Read) @br{}
        Color with which to draw error-indication underlines.
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"backspace\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"backspace\" signal is a keybinding signal which gets emitted when
      the user asks for it.
      The default bindings for this signal are Backspace and Shift-Backspace.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"copy-clipboard\" signal is a keybinding signal which gets emitted to
      copy the selection to the clipboard.
      The default bindings for this signal are Ctrl-c and Ctrl-Insert.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"cut-clipboard\" signal is a keybinding signal which gets emitted to
      cut the selection to the clipboard.
      The default bindings for this signal are Ctrl-x and Shift-Delete.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
 lambda (text-view type count)    : Action
      @end{pre}
      The \"delete-from-cursor\" signal is a keybinding signal which gets
      emitted when the user initiates a text deletion.
      If the type is @code{:chars}, GTK+ deletes the selection if there is one,
      otherwise it deletes the requested number of characters. The default
      bindings for this signal are Delete for deleting a character, Ctrl-Delete
      for deleting a word and Ctrl-Backspace for deleting a word backwords.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[type]{The granularity of the deletion, as a
          @symbol{gtk-delete-type}.}
        @entry[count]{The number of type units to delete.}
      @end{table}
    @subheading{The \"extend-selection\" signal}
      @begin{pre}
 lambda (text-view granularity location start end)    : Run Last
      @end{pre}
      The \"extend-selection\" signal is emitted when the selection needs to be
      extended at @arg{location}.
      @begin[code]{table}
        @entry[text-view]{The @class{gtk-text-view} object which received the
          signal.}
        @entry[granularity]{The granularity of type
          @symbol{gtk-text-extend-selection}.}
        @entry[location]{The @class{gtk-text-iter} location where to extend the
          selection.}
        @entry[start]{The @class{gtk-text-iter} where the selection should
          start.}
        @entry[end]{The @class{gtk-text-iter} where the selection should end.}
        @entry[Returns]{@code{GDK_EVENT_STOP} to stop other handlers from being
          invoked for the event. @code{GDK_EVENT_PROPAGATE} to propagate the
          event further.}
      @end{table}
      Since 3.16

    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
 lambda (text-view string)
      @end{pre}
      The \"insert-at-cursor\" signal is a keybinding signal which gets emitted
      when the user initiates the insertion of a fixed string at the cursor.
      This signal has no default bindings.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"insert-emoji\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"insert-emoji\" signal is a keybinding signal which gets emitted to
      present the Emoji chooser for the @arg{text-view}.
      The default bindings for this signal are @code{Ctrl-.} and
      @code{Ctrl-;}
      @begin[code]{table}
        @entry[text-view]{The @class{gtk-text-view} object which received the
          signal.}
      @end{table}
      Since 3.22

    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (text-view step count extended-selection)    : Action
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement. If the cursor is not visible in
      @arg{text-view}, this signal causes the viewport to be moved instead.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control the cursor
      programmatically.
      The default bindings for this signal come in two variants, the variant
      with the Shift modifier extends the selection, the variant without the
      Shift modifer does not. There are too many key combinations to list them
      all here.
      Arrow keys move by individual characters/lines
      Ctrl-arrow key combinations move by words/paragraphs
      Home/End keys move to the ends of the buffer
      PageUp/PageDown keys move vertically by pages
      Ctrl-PageUp/PageDown keys move horizontally by pages
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[step]{The granularity of the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
        @entry[extend-selection]{@em{True} if the move should extend the
          selection.}
      @end{table}
    @subheading{The \"move-viewport\" signal}
      @begin{pre}
 lambda (text-view step count)    : Action
      @end{pre}
      The \"move-viewport\" signal is a keybinding signal which can be bound to
      key combinations to allow the user to move the viewport, i. e. change what
      part of the text view is visible in a containing scrolled window.
      There are no default bindings for this signal.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[step]{The granularity of the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"paste-clipboard\" signal is a keybinding signal which gets emitted
      to paste the contents of the clipboard into the text view.
      The default bindings for this signal are Ctrl-v and Shift-Insert.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (entry menu)    : Run Last
      @end{pre}
      The \"populate-popup\" signal gets emitted before showing the context menu
      of the text view.
      If you need to add items to the context menu, connect to this signal and
      append your menuitems to the menu.
      @begin[code]{table}
        @entry[entry]{The text view on which the signal is emitted.}
        @entry[menu]{The menu that is being populated.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
 lambda (text-view preedit)    : Action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal.
      This signal is only emitted if the text at the given position is actually
      editable.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[preedit]{The current preedit string.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (text-view select)    : Action
      @end{pre}
      The \"select-all\" signal is a keybinding signal which gets emitted to
      select or unselect the complete contents of the text view.
      The default bindings for this signal are Ctrl-a and Ctrl-/ for selecting
      and Shift-Ctrl-a and Ctrl-\ for unselecting.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
        @entry[select]{@em{True} to select, @code{nil} to unselect.}
      @end{table}
    @subheading{The \"set-anchor\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"set-anchor\" signal is a keybinding signal which gets emitted when
      the user initiates setting the \"anchor\" mark. The \"anchor\" mark gets
      placed at the same position as the \"insert\" mark.
      This signal has no default bindings.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"toggle-cursor-visible\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"toggle-cursor-visible signal\" is a keybinding signal which gets
      emitted to toggle the visibility of the cursor.
      The default binding for this signal is F7.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
 lambda (text-view)    : Action
      @end{pre}
      The \"toggle-overwrite\" signal is a keybinding signal which gets emitted
      to toggle the overwrite mode of the text view.
      The default bindings for this signal is Insert.
      @begin[code]{table}
        @entry[text-view]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-text-view-accepts-tab}
  @see-slot{gtk-text-view-bottom-margin}
  @see-slot{gtk-text-view-buffer}
  @see-slot{gtk-text-view-cursor-visible}
  @see-slot{gtk-text-view-editable}
  @see-slot{gtk-text-view-im-module}
  @see-slot{gtk-text-view-indent}
  @see-slot{gtk-text-view-input-hints}
  @see-slot{gtk-text-view-input-purpose}
  @see-slot{gtk-text-view-justification}
  @see-slot{gtk-text-view-left-margin}
  @see-slot{gtk-text-view-monospace}
  @see-slot{gtk-text-view-overwrite}
  @see-slot{gtk-text-view-pixels-above-lines}
  @see-slot{gtk-text-view-pixels-below-lines}
  @see-slot{gtk-text-view-pixels-inside-wrap}
  @see-slot{gtk-text-view-populate-all}
  @see-slot{gtk-text-view-right-margin}
  @see-slot{gtk-text-view-tabs}
  @see-slot{gtk-text-view-top-margin}
  @see-slot{gtk-text-view-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-text-view-accepts-tab ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-tab" 'gtk-text-view) 't)
 "The @code{accepts-tab} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether Tab will result in a tab character being entered. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-accepts-tab atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-accepts-tab 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{accepts-tab} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-accepts-tab}
  @see-function{gtk-text-view-set-accepts-tab}")

;;; --- gtk-text-view-bottom-margin --------------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "bottom-margin" 'gtk-text-view)
                     't)
 "The @code{bottom-margin} property of type @code{:int} (Read / Write) @br{}
  The bottom margin for text in the text view.
  Note that this property is confusingly named. In CSS terms, the value set here
  is padding, and it is applied in addition to the padding from the theme.
  Don't confuse this property with @code{margin-bottom}. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.18")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-bottom-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-bottom-margin 'function)
 "@version{2019-4-14}
  @begin{short}
    Accessor of the slot @slot[gtk-text-view]{bottom-margin} of the
    @class{gtk-text-view} class.
  @end{short}

  Since 3.18
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-buffer ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buffer" 'gtk-text-view) 't)
 "The @code{buffer} property of type  @class{gtk-text-buffer}
  (Read / Write) @br{}
  The buffer which is displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-buffer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-buffer 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{buffer} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-buffer}
  @see-function{gtk-text-view-set-buffer}")

;;; --- gtk-text-view-cursor-visible -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-visible"
                                               'gtk-text-view) 't)
 "The @code{cursor-visible} property of type @code{:boolean}
  (Read / Write) @br{}
  If the insertion cursor is shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-cursor-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-cursor-visible 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{cursor-visible} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-cursor-visible}
  @see-function{gtk-text-view-set-cursor-visible}")

;;; --- gtk-text-view-editable -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable" 'gtk-text-view) 't)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-editable 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{editable} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-editable}
  @see-function{gtk-text-view-set-editable}")

;;; --- gtk-text-view-im-module ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "im-module" 'gtk-text-view) 't)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used for this entry. See
  @class{gtk-im-context}.
  Setting this to a non-@code{nil} value overrides the system-wide IM module
  setting. See the @class{gtk-settings} \"gtk-im-module\" property. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-im-module atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-im-module 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{im-module} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-indent ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "indent" 'gtk-text-view) 't)
 "The @code{indent} property of type @code{:int} (Read / Write) @br{}
  Amount to indent the paragraph, in pixels. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-indent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-indent 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{indent} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-indent}
  @see-function{gtk-text-view-set-indent}")

;;; --- gtk-text-view-input-hints ----------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "input-hints" 'gtk-text-view) 't)
 "The @code{input-hints} property of type @symbol{gtk-input-hints}
  (Read / Write) @br{}
  Additional hints (beyond the \"input-purpose\" signal) that allow input
  methods to fine-tune their behaviour. @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-input-hints atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-input-hints 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{input-hints} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-input-hints}
  @see-function{gtk-text-view-set-input-hints}")

;;; --- gtk-text-view-input-purpose --------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "input-purpose"
                                               'gtk-text-view) 't)
 "The @code{input-purpose} property of type @symbol{gtk-input-purpose}
  (Read / Write) @br{}
  The purpose of this text field.
  This property can be used by on-screen keyboards and other input methods to
  adjust their behaviour. @br{}
  Default value: @code{:free-form} @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-input-purpose atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-input-purpose 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{input-purpose} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-purpose}
  @see-function{gtk-text-view-get-input-purpose}
  @see-function{gtk-text-view-set-input-purpose}")

;;; --- gtk-text-view-justification --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "justification"
                                               'gtk-text-view) 't)
 "The @code{justification} property of type @symbol{gtk-justification}
  (Read / Write) @br{}
  Left, right, or center justification. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-justification atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-justification 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{justification} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-symbol{gtk-justification}
  @see-function{gtk-text-view-get-justification}
  @see-function{gtk-text-view-set-justification}")

;;; --- gtk-text-view-left-margin ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "left-margin" 'gtk-text-view) 't)
 "The @code{left-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the left margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-left-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-left-margin 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{left-margin} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-left-margin}
  @see-function{gtk-text-view-set-left-margin}")

;;; --- gtk-text-view-monospace ------------------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "monospace"
                                               'gtk-text-view) 't)
 "The @code{monospace} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use a monospace font. @br{}
  Default value: @code{nil} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-monospace atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-monospace 'function)
 "@version{2019-4-14}
  @begin{short}
    Accessor of the slot @slot[gtk-text-view]{monospace} of the
    @class{gtk-text-view} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-overwrite ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "overwrite" 'gtk-text-view) 't)
 "The @code{overwrite} property of type @code{:boolean} (Read / Write) @br{}
  Whether entered text overwrites existing contents. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-overwrite atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-overwrite 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{overwrite} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-overwrite}
  @see-function{gtk-text-view-set-overwrite}")

;;; --- gtk-text-view-pixels-above-lines ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-above-lines"
                                               'gtk-text-view) 't)
 "The @code{pixels-above-lines} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space above paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-above-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-above-lines 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{pixels-above-lines} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-above-lines}
  @see-function{gtk-text-view-set-pixels-above-lines}")

;;; --- gtk-text-view-pixels-below-lines ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-below-lines"
                                               'gtk-text-view) 't)
 "The @code{pixels-below-lines} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space below paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-below-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-below-lines 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{pixels-below-lines} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-below-lines}
  @see-function{gtk-text-view-set-pixels-below-lines}")

;;; --- gtk-text-view-pixels-inside-wrap ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-inside-wrap"
                                               'gtk-text-view) 't)
 "The @code{pixels-inside-wrap} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space between wrapped lines in a paragraph. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-inside-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-inside-wrap 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{pixels-inside-wrap} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-inside-wrap}
  @see-function{gtk-text-view-set-pixels-inside-wrap}")

;;; --- gtk-text-view-populate-all ---------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "populate-all"
                                               'gtk-text-view) 't)
 "The @code{populate-all} property of type @code{:boolean} (Read / Write) @br{}
  If @code{populate-all} is @em{true}, the \"populate-popup\" signal is also
  emitted for touch popups. @br{}
  Default value: @code{nil} @br{}
  Since 3.8")

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-populate-all atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-populate-all 'function)
 "@version{2019-4-14}
  @begin{short}
    Accessor of the slot @slot[gtk-text-view]{populate-all} of the
    @class{gtk-text-view} class.
  @end{short}

  Since 3.8
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-right-margin ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-margin"
                                               'gtk-text-view) 't)
 "The @code{right-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the right margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-right-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-right-margin 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{right-margin} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-right-margin}
  @see-function{gtk-text-view-set-right-margin}")

;;; --- gtk-text-view-tabs -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tabs" 'gtk-text-view) 't)
 "The @code{tabs} property of type @class{pango-tab-array}
  (Read / Write) @br{}
  Custom tabs for this text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-tabs 'function)
 "@version{2013-8-10}
  Accessor of the slot @slot[gtk-text-view]{tabs} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-tabs}
  @see-function{gtk-text-view-set-tabs}")

;;; --- gtk-text-view-top-margin -----------------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "top-margin" 'gtk-text-view) 't)
 "The @code{top-margin} property of type @code{:int} (Read / Write) @br{}
  The top margin for text in the text view. Note that this property is
  confusingly named. In CSS terms, the value set here is padding, and it is
  applied in addition to the padding from the theme. Don't confuse this property
  with @code{margin-top}. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.18")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-top-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-top-margin 'function)
 "@version{2019-4-14}
  @begin{short}
    Accessor of the slot @slot[gtk-text-view]{top-margin} of the
    @class{gtk-text-view} class.
  @end{short}

  Since 3.18
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-wrap-mode ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-text-view) 't)
 "The @code{wrap-mode} property of type @symbol{gtk-wrap-mode}
  (Read / Write) @br{}
  Whether to wrap lines never, at word boundaries, or at character
  boundaries. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-wrap-mode 'function)
 "@version{2013-8-20}
  Accessor of the slot @slot[gtk-text-view]{wrap-mode} of the
  @class{gtk-text-view} class.
  @see-class{gtk-text-view}
  @see-symbol{gtk-wrap-mode}
  @see-function{gtk-text-view-get-wrap-mode}
  @see-function{gtk-text-view-set-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-new))

(defun gtk-text-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-5}
  @return{A new @class{gtk-text-view} object.}
  Creates a new @class{gtk-text-view} object. If you do not call the function
  @fun{gtk-text-view-buffer} before using the text view, an empty default
  buffer will be created for you. Get the buffer with the function
  @fun{gtk-text-view-get-buffer}. If you want to specify your own buffer,
  consider the function @fun{gtk-text-view-new-with-buffer}.
  @see-function{gtk-text-view-set-buffer}
  @see-function{gtk-text-view-get-buffer}
  @see-function{gtk-text-view-new-with-buffer}"
  (make-instance 'gtk-text-view))

(export 'gtk-text-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-new-with-buffer))

(defun gtk-text-view-new-with-buffer (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-5}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A new @class{gtk-text-view} object.}
  Creates a new @class{gtk-text-view} widget displaying the buffer @arg{buffer}.
  One buffer can be shared among many widgets. @arg{buffer} may be @code{nil} to
  create a default buffer, in which case this function is equivalent to the
  function @fun{gtk-text-view-new}. The text view adds its own reference count
  to the buffer; it does not take over an existing reference.
  @see-function{gtk-text-view-new}"
  (make-instance 'gtk-text-view
                 :buffer buffer))

(export 'gtk-text-view-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-buffer))

(defun gtk-text-view-set-buffer (text-view buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Sets @arg{buffer} as the buffer being displayed by @arg{text-view}.
  @end{short}
  The previous buffer displayed by the text view is unreferenced, and a
  reference is added to @arg{buffer}. If you owned a reference to @arg{buffer}
  before passing it to this function, you must remove that reference yourself;
  @class{gtk-text-view} will not \"adopt\" it.
  @see-class{gtk-text-view}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-view-get-buffer}"
  (setf (gtk-text-view-buffer text-view) buffer))

(export 'gtk-text-view-set-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-buffer))

(defun gtk-text-view-get-buffer (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{A @class{gtk-text-buffer} widget.}
  Returns the @class{gtk-text-buffer} object being displayed by this text view.
  @see-class{gtk-text-view}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-view-set-buffer}"
  (gtk-text-view-buffer text-view))

(export 'gtk-text-view-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_text_view_get_hadjustment (GtkTextView *text_view);
;;;
;;; Warning
;;;
;;; gtk_text_view_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Gets the horizontal-scrolling GtkAdjustment.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     pointer to the horizontal GtkAdjustment
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_text_view_get_vadjustment (GtkTextView *text_view);
;;;
;;; Warning
;;;
;;; gtk_text_view_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Gets the vertical-scrolling GtkAdjustment.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     pointer to the vertical GtkAdjustment
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_to_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_scroll_to_mark" %gtk-text-view-scroll-to-mark) :void
  (text-view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun gtk-text-view-scroll-to-mark (text-view mark &key
                                               (within-margin 0.4)
                                               (x-align 0.0 x-align-supplied)
                                               (y-align 0.0 y-align-supplied))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-7}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[mark]{a @class{gtk-text-mark} object}
  @argument[within-margin]{margin as a [0.0, 0.5) fraction of screen size}
  @argument[use-align]{whether to use alignment arguments (if @code{nil}, just
    get the mark onscreen)}
  @argument[xalign]{horizontal alignment of mark within visible area}
  @argument[yalign]{vertical alignment of mark within visible area}
  Scrolls @arg{text-view} so that mark is on the screen in the position
  indicated by @arg{xalign} and @arg{yalign}. An alignment of 0.0 indicates left
  or top, 1.0 indicates right or bottom, 0.5 means center. If @arg{use-align} is
  @code{nil}, the text scrolls the minimal distance to get the mark onscreen,
  possibly not scrolling at all. The effective screen for purposes of this
  function is reduced by a margin of size @arg{within-margin}."
  (%gtk-text-view-scroll-to-mark text-view
                                 mark
                                 (coerce within-margin 'double-float)
                                 (or x-align-supplied y-align-supplied)
                                 (coerce x-align 'double-float)
                                 (coerce y-align 'double-float)))

(export 'gtk-text-view-scroll-to-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_to_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_scroll_to_iter" %gtk-text-view-scroll-to-iter) :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun gtk-text-view-scroll-to-iter (text-view iter &key
                                               (within-margin 0.4)
                                               (x-align 0.0 x-align-supplied)
                                               (y-align 0.0 y-align-supplied))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[within-margin]{margin as a [0.0, 0.5) fraction of screen size}
  @argument[use-align]{whether to use alignment arguments (if @code{nil}, just
    get the mark onscreen)}
  @argument[xalign]{horizontal alignment of mark within visible area}
  @argument[yalign]{vertical alignment of mark within visible area}
  @return{@em{True} if scrolling occurred.}
  @begin{short}
    Scrolls @arg{text-view} so that @arg{iter} is on the screen in the position
    indicated by @arg{xalign} and @arg{yalign}. An alignment of 0.0 indicates
    left or top, 1.0 indicates right or bottom, 0.5 means center. If
    @arg{use-align} is @code{nil}, the text scrolls the minimal distance to get
    the mark onscreen, possibly not scrolling at all. The effective screen for
    purposes of this function is reduced by a margin of size
    @arg{within-margin}.
  @end{short}

  Note that this function uses the currently-computed height of the lines in
  the text buffer. Line heights are computed in an idle handler; so this
  function may not have the desired effect if it is called before the height
  computations. To avoid oddness, consider using the function
  @fun{gtk-text-view-scroll-to-mark} which saves a point to be scrolled to after
  line validation.
  @see-function{gtk-text-view-scroll-to-mark}"
  (%gtk-text-view-scroll-to-iter text-view
                                 iter
                                 (coerce within-margin 'double-float)
                                 (or x-align-supplied y-align-supplied)
                                 (coerce x-align 'double-float)
                                 (coerce y-align 'double-float)))

(export 'gtk-text-view-scroll-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_mark_onscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_scroll_mark_onscreen"
          gtk-text-view-scroll-mark-onscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[mark]{a mark in the buffer for @arg{text-view} object}
  Scrolls @arg{text-view} the minimum distance such that mark is contained
  within the visible area of the widget."
  (text-view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-view-scroll-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_mark_onscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_move_mark_onscreen"
          gtk-text-view-move-mark-onscreen) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{@em{True} if the mark moved (was not already onscreen).}
  Moves a @arg{mark} within the buffer so that it it located within the
  currently-visible text area."
  (text-view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-view-move-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_place_cursor_onscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_place_cursor_onscreen"
          gtk-text-view-place-cursor-onscreen) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @return{@em{True} if the cursor had to be moved.}
  Moves the cursor to the currently visible region of the buffer, it it is not
  there already."
  (text-view (g-object gtk-text-view)))

(export 'gtk-text-view-place-cursor-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_visible_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_visible_rect" %gtk-text-view-get-visible-rect)
    :void
  (text-view (g-object gtk-text-view))
  (visible-rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-text-view-get-visible-rect (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-23}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{The current visible region.}
  The currently visible region of the buffer, in buffer coordinates. Convert to
  window coordinates with the function
  @fun{gtk-text-view-buffer-to-window-coords}.
  @see-class{gtk-text-view}
  @see-class{gdk-rectangle}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-text-view-get-visible-rect text-view rect)
    rect))

(export 'gtk-text-view-get-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_location ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_location" %gtk-text-view-get-iter-location)
    :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (location (g-boxed-foreign gdk-rectangle)))

(defun gtk-text-view-get-iter-location (text-view iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{Bounds of the character at @arg{iter}.}
  @begin{short}
    Gets a rectangle which roughly contains the character at @arg{iter}.
  @end{short}
  The rectangle position is in buffer coordinates; use the function
  @fun{gtk-text-view-buffer-to-window-coords} to convert these coordinates to
  coordinates for one of the windows in the text view.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-text-view-get-iter-location text-view iter rect)
    rect))

(export 'gtk-text-view-get-iter-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_cursor_locations ()
;;;
;;; void gtk_text_view_get_cursor_locations (GtkTextView *text_view,
;;;                                          const GtkTextIter *iter,
;;;                                          GdkRectangle *strong,
;;;                                          GdkRectangle *weak);
;;;
;;; Given an iter within a text layout, determine the positions of the strong
;;; and weak cursors if the insertion point is at that iterator. The position of
;;; each cursor is stored as a zero-width rectangle. The strong cursor location
;;; is the location where characters of the directionality equal to the base
;;; direction of the paragraph are inserted. The weak cursor location is the
;;; location where characters of the directionality opposite to the base
;;; direction of the paragraph are inserted.
;;;
;;; If iter is NULL, the actual cursor position is used.
;;;
;;; Note that if iter happens to be the actual cursor position, and there is
;;; currently an IM preedit sequence being entered, the returned locations will
;;; be adjusted to account for the preedit cursor's offset within the preedit
;;; sequence.
;;;
;;; The rectangle position is in buffer coordinates; use
;;; gtk_text_view_buffer_to_window_coords() to convert these coordinates to
;;; coordinates for one of the windows in the text view.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; iter :
;;;     a GtkTextIter
;;;
;;; strong :
;;;     location to store the strong cursor position (may be NULL)
;;;
;;; weak :
;;;     location to store the weak cursor position (may be NULL)
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_at_y ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_line_at_y" %gtk-text-view-get-line-at-y) :void
  (text-view (g-object gtk-text-view))
  (target-iter (g-boxed-foreign gtk-text-iter))
  (y :int)
  (line-top (:pointer :int)))

(defun gtk-text-view-get-line-at-y (text-view y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[y]{a y coordinate}
  @begin{return}
    @code{target-iter} -- a @class{gtk-text-iter} object @br{}
    @code{line-top} -- top coordinate of the line
  @end{return}
  Gets the @class{gtk-text-iter} at the start of the line containing the
  coordinate @arg{y}. @arg{y} is in buffer coordinates, convert from window
  coordinates with the function @fun{gtk-text-view-window-to-buffer-coords}. If
  non-@code{nil}, @arg{line-top} will be filled with the coordinate of the top
  edge of the line.
  @see-function{gtk-text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (with-foreign-object (line-top :int)
      (%gtk-text-view-get-line-at-y text-view iter y line-top)
      (values iter (mem-ref line-top :int)))))

(export 'gtk-text-view-get-line-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_yrange ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_line_yrange" %gtk-text-view-get-line-yrange) :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (y (:pointer :int))
  (height (:pointer :int)))

(defun gtk-text-view-get-line-yrange (text-view iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @begin{return}
    @code{y} -- y coordinate @br{}
    @code{height} -- height
  @end{return}
  Gets the @arg{y} coordinate of the top of the line containing @arg{iter}, and
  the @arg{height} of the line. The coordinate is a buffer coordinate; convert
  to window coordinates with the function
  @fun{gtk-text-view-buffer-to-window-coords}.
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (with-foreign-objects ((y :int) (height :int))
    (%gtk-text-view-get-line-yrange text-view iter y height)
    (values (mem-ref y :int)
            (mem-ref height :int))))

(export 'gtk-text-view-get-line-yrange)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_location ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_at_location"
          %gtk-text-view-get-iter-at-location) :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (x :int)
  (y :int))

(defun gtk-text-view-get-iter-at-location (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[x]{x position, in buffer coordinates}
  @argument[y]{y position, in buffer coordinates}
  Retrieves the iterator at buffer coordinates @arg{x} and @arg{y}. Buffer
  coordinates are coordinates for the entire buffer, not just the
  currently-displayed portion. If you have coordinates from an event, you have
  to convert those to buffer coordinates with the function
  @fun{gtk-text-view-window-to-buffer-coords}.
  @see-function{gtk-text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-view-get-iter-at-location view iter x y)
    iter))

(export 'gtk-text-view-get-iter-at-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_at_position"
          %gtk-text-view-get-iter-at-position) :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (trailing (:pointer :int))
  (x :int)
  (y :int))

(defun gtk-text-view-get-iter-at-position (text-view x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[x]{x position, in buffer coordinates}
  @argument[y]{y position, in buffer coordinates}
  @begin{return}
    @code{iter} -- a @class{gtk-text-iter} object @br{}
    @code{trailing} -- if non-@code{nil}, an integer indicating where in the
    grapheme the user clicked. It will either be zero, or the number of
    characters in the grapheme. 0 represents the trailing edge of the grapheme
  @end{return}
  @begin{short}
    Retrieves the iterator pointing to the character at buffer coordinates
    @arg{x} and @arg{y}. Buffer coordinates are coordinates for the entire
    buffer, not just the currently-displayed portion. If you have coordinates
    from an event, you have to convert those to buffer coordinates with the
    function @fun{gtk-text-view-window-to-buffer-coords}.
  @end{short}

  Note that this is different from the function
  @fun{gtk-text-view-get-iter-at-location}, which returns cursor locations,
  i. e. positions between characters.
  @see-function{gtk-text-view-window-to-buffer-coords}
  @see-function{gtk-text-view-get-iter-at-location}"
  (with-foreign-object (trailing :int)
    (let ((iter (make-instance 'gtk-text-iter)))
      (%gtk-text-view-get-iter-at-position text-view iter trailing x y)
      (values iter (mem-ref trailing :int)))))

(export 'gtk-text-view-get-iter-at-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_buffer_to_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_buffer_to_window_coords"
          %gtk-text-view-buffer-to-window-coords) :void
  (text-view (g-object gtk-text-view))
  (win gtk-text-window-type)
  (buffer-x :int)
  (buffer-y :int)
  (window-x (:pointer :int))
  (window-y (:pointer :int)))

(defun gtk-text-view-buffer-to-window-coords (text-view window-type
                                                        buffer-x buffer-y)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[win]{a @symbol{gtk-text-window-type} except @code{:private}}
  @argument[buffer-x]{buffer x coordinate}
  @argument[buffer-y]{buffer y coordinate}
  @begin{return}
    @code{window-x} -- window x coordinate or @code{nil} @br{}
    @code{window-y} -- window y coordinate or @code{nil}
  @end{return}
  @begin{short}
    Converts coordinate (@arg{buffer-x}, @arg{buffer-y}) to coordinates for the
    window win (@arg{window-x}, @arg{window-y}).
  @end{short}

  Note that you cannot convert coordinates for a nonexisting window (see the
  function @fun{gtk-text-view-set-border-window-size}).
  @see-class{gtk-text-view}
  @see-symbol{gtk-text-window-type}
  @see-function{gtk-text-view-set-border-window-size}"
  (with-foreign-objects ((window-x :int) (window-y :int))
    (%gtk-text-view-buffer-to-window-coords text-view
                                            window-type
                                            buffer-x buffer-y
                                            window-x window-y)
    (values (mem-ref window-x :int) (mem-ref window-y :int))))

(export 'gtk-text-view-buffer-to-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_window_to_buffer_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_window_to_buffer_coords"
          %gtk-text-view-window-to-buffer-coords) :void
  (text-view (g-object gtk-text-view))
  (win gtk-text-window-type)
  (window-x :int)
  (window-y :int)
  (buffer-x :pointer)
  (buffer-y :pointer))

(defun gtk-text-view-window-to-buffer-coords (text-view win window-x window-y)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[win]{a @symbol{gtk-text-window-type} except @code{:private}}
  @argument[window-x]{window x coordinate}
  @argument[window-y]{window y coordinate}
  @begin{return}
    @code{buffer-x} -- buffer x coordinate or @code{nil} @br{}
    @code{buffer-y} -- buffer y coordinate or @code{nil}
  @end{return}
  @begin{short}
    Converts coordinates on the window identified by win to buffer coordinates,
    storing the result in (@arg{buffer-x} ,@arg{buffer-y}).
  @end{short}

  Note that you cannot convert coordinates for a nonexisting window (see the
  function @fun{gtk-text-view-set-border-window-size}).
  @see-function{gtk-text-view-set-border-window-size}"
  (with-foreign-objects ((buffer-x :int) (buffer-y :int))
    (%gtk-text-view-window-to-buffer-coords text-view
                                            win
                                            window-x window-y
                                            buffer-x buffer-y)
    (values (mem-ref buffer-x :int)
            (mem-ref buffer-y :int))))

(export 'gtk-text-view-window-to-buffer-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_window" gtk-text-view-get-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[win]{window to get, one of the @symbol{gtk-text-window-type}
    enumeration}
  @return{a @class{gdk-window,} or @code{nil}}
  @begin{short}
    Retrieves the @class{gdk-window} object corresponding to an area of the text
    view.
  @end{short}
  Possible windows include the overall widget window, child windows on the
  left, right, top, bottom, and the window that displays the text buffer.
  Windows are @code{nil} and nonexistent if their width or height is 0, and are
  nonexistent before the widget has been realized.
  @see-class{gtk-text-view}
  @see-symbol{gtk-text-window-type}"
  (text-view (g-object gtk-text-view))
  (win gtk-text-window-type))

(export 'gtk-text-view-get-window)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_window_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_window_type" gtk-text-view-get-window-type)
    gtk-text-window-type
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[window]{a window type}
  @return{The window type.}
  Usually used to find out which window an event corresponds to. If you
  connect to an event signal on @arg{text-view}, this function should be called
  on @code{event->window} to see which window it was."
  (text-view (g-object gtk-text-view))
  (window (g-object gdk-window)))

(export 'gtk-text-view-get-window-type)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_border_window_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_set_border_window_size"
           gtk-text-view-set-border-window-size) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[type]{window to affect}
  @argument[size]{width or height of the window}
  @begin{short}
    Sets the width of @code{:left} or @code{:right}, or the height of
    @code{:top} or @code{:bottom}.
  @end{short}
  Automatically destroys the corresponding window if the size is set to 0, and
  creates the window if the size is set to non-zero. This function can only be
  used for the \"border windows\", it does not work with @code{:widget},
  @code{:text}, or @code{:private}.
  @see-function{gtk-text-view-get-border-window-size}"
  (text-view (g-object gtk-text-view))
  (type gtk-text-window-type)
  (size :int))

(export 'gtk-text-view-set-border-window-size)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_border_window_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_border_window_size"
           gtk-text-view-get-border-window-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[type]{window to return size from}
  @return{Width of window.}
  Gets the width of the specified border window. See the function
  @fun{gtk-text-view-set-border-window-size}.
  @see-function{gtk-text-view-set-border-window-size}"
  (text-view (g-object gtk-text-view))
  (type gtk-text-window-type))

(export 'gtk-text-view-get-border-window-size)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_forward_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_forward_display_line"
          gtk-text-view-forward-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  Moves the given @arg{iter} forward by one display (wrapped) line. A display
  line is different from a paragraph. Paragraphs are separated by newlines or
  other paragraph separator characters. Display lines are created by
  line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width; paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}."
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-forward-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_backward_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_backward_display_line"
          gtk-text-view-backward-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  Moves the given @arg{iter} backward by one display (wrapped) line. A display
  line is different from a paragraph. Paragraphs are separated by newlines or
  other paragraph separator characters. Display lines are created by
  line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width; paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}."
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-backward-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_forward_display_line_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_forward_display_line_end"
          gtk-text-view-forward-display-line-end) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  Moves the given @arg{iter} forward to the next display line end. A display
  line is different from a paragraph. Paragraphs are separated by newlines or
  other paragraph separator characters. Display lines are created by
  line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width; paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}."
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-forward-display-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_backward_display_line_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_backward_display_line_start"
          gtk-text-view-backward-display-line-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  Moves the given @arg{iter} backward to the next display line start. A display
  line is different from a paragraph. Paragraphs are separated by newlines or
  other paragraph separator characters. Display lines are created by
  line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width; paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}."
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-backward-display-line-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_starts_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_starts_display_line"
          gtk-text-view-starts-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} begins a wrapped line.}
  Determines whether @arg{iter} is at the start of a display line. See the
  function @fun{gtk-text-view-forward-display-line} for an explanation of
  display lines vs. paragraphs.
  @see-function{gtk-text-view-forward-display-line}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-starts-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_visually ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_move_visually" gtk-text-view-move-visually) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of characters to move (negative moves left, positive
    moves right)}
  @return{@em{True} if @arg{iter} moved and is not on the end iterator.}
  @begin{short}
    Move the iterator a given number of characters visually, treating it as the
    strong cursor position. If @arg{count} is positive, then the new strong
    cursor position will be @arg{count} positions to the right of the old cursor
    position. If @arg{count} is negative then the new strong cursor position
    will be @arg{count} positions to the left of the old cursor position.
  @end{short}

  In the presence of bi-directional text, the correspondence between logical
  and visual order will depend on the direction of the current run, and there
  may be jumps when the cursor is moved off of the end of a run."
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (count :int))

(export 'gtk-text-view-move-visually)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_add_child_at_anchor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_add_child_at_anchor" gtk-text-view-add-child-at-anchor)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[child]{a @class{gtk-widget} object}
  @argument[anchor]{a @class{gtk-text-child-anchor} in the
    @class{gtk-text-buffer} for @arg{text-view}}
  Adds a child widget in the text buffer, at the given anchor."
  (view g-object)
  (child g-object)
  (anchor g-object))

(export 'gtk-text-view-add-child-at-anchor)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextChildAnchor
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextChildAnchor" gtk-text-child-anchor
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_text_child_anchor_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-child-anchor 'type)
 "@version{2013-5-10}
  A @sym{gtk-text-child-anchor} is a spot in the buffer where child widgets can
  be \"anchored\" (inserted inline, as if they were characters). The anchor can
  have multiple widgets anchored, to allow for multiple views.")

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-child-anchor-new))

(defun gtk-text-child-anchor-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-9-28}
  @return{A new @class{gtk-text-child-anchor} object}
  @begin{short}
    Creates a new @class{gtk-text-child-anchor} object.
  @end{short}
  Usually you would then insert it into a @class{gtk-text-buffer} with the
  function @fun{gtk-text-buffer-insert-child-anchor}. To perform the
  creation and insertion in one step, use the convenience function
  @fun{gtk-text-buffer-create-child-anchor}.
  @see-class{gtk-text-child-anchor}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-insert-child-anchor}
  @see-function{gtk-text-buffer-create-child-anchor}"
  (make-instance 'gtk-text-child-anchor))

(export 'gtk-text-child-anchor-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_get_widgets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_child_anchor_get_widgets" gtk-text-child-anchor-get-widgets)
    (g-list (g-object gtk-widget) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-18}
  @argument[anchor]{a @class{gtk-text-child-anchor} object}
  @return{List of widgets anchored at @arg{anchor}.}
  Gets a list of all widgets anchored at this child @arg{anchor}.
  @see-class{gtk-text-child-anchor}"
  (anchor (g-object gtk-text-child-anchor)))

(export 'gtk-text-child-anchor-get-widgets)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_get_deleted ()
;;;
;;; gboolean gtk_text_child_anchor_get_deleted (GtkTextChildAnchor *anchor);
;;;
;;; Determines whether a child anchor has been deleted from the buffer. Keep in
;;; mind that the child anchor will be unreferenced when removed from the
;;; buffer, so you need to hold your own reference (with g_object_ref()) if you
;;; plan to use this function - otherwise all deleted child anchors will also be
;;; finalized.
;;;
;;; anchor :
;;;     a GtkTextChildAnchor
;;;
;;; Returns :
;;;     TRUE if the child anchor has been deleted from its buffer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_add_child_in_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_add_child_in_window" gtk-text-view-add-child-in-window)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[child]{a @class{gtk-widget} object}
  @argument[which-window]{which window the child should appear in}
  @argument[xpos]{x position of child in window coordinates}
  @argument[ypos]{y position of child in window coordinates}
  @begin{short}
    Adds a child at fixed coordinates in one of the text widget's windows.
  @end{short}

  The window must have nonzero size (see the function
  @fun{gtk-text-view-set-border-window-size}). Note that the child coordinates
  are given relative to the @class{gdk-window} object in question, and that
  these coordinates have no sane relationship to scrolling. When placing a child
  in @code{:widget}, scrolling is irrelevant, the child floats above all
  scrollable areas. But when placing a child in one of the scrollable windows
  (border windows or text window), you will need to compute the child's correct
  position in buffer coordinates any time scrolling occurs or buffer changes
  occur, and then call the function @fun{gtk-text-view-move-child} to update the
  child's position.
  @see-function{gtk-text-view-set-border-window-size}
  @see-function{gtk-text-view-move-child}"
  (view (g-object gtk-text-view))
  (child (g-object gtk-widget))
  (which-window gtk-text-window-type)
  (x-pos :int)
  (y-pos :int))

(export 'gtk-text-view-add-child-in-window)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_move_child" gtk-text-view-move-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-10}
  @argument[text-view]{a @class{gtk-text-view} object}
  @argument[child]{child widget already added to the text view}
  @argument[xpos]{new X position in window coordinates}
  @argument[ypos]{new Y position in window coordinates}
  Updates the position of a child, as for the function
  @fun{gtk-text-view-add-child-in-window}.
  @see-function{gtk-text-view-add-child-in-window}"
  (view (g-object gtk-text-view))
  (child (g-object gtk-widget))
  (x-pos :int)
  (y-pos :int))

(export 'gtk-text-view-move-child)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_wrap_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-wrap-mode))

(defun gtk-text-view-set-wrap-mode (text-view wrap-mode)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[wrap-mode]{a @symbol{gtk-wrap-mode}}
  Sets the line wrapping for the view.
  @see-class{gtk-text-view}
  @see-symbol{gtk-wrap-mode}
  @see-function{gtk-text-view-get-wrap-mode}"
  (setf (gtk-text-view-wrap-mode text-view) wrap-mode))

(export 'gtk-text-view-set-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_wrap_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-wrap-mode))

(defun gtk-text-view-get-wrap-mode (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{The line wrap setting of type @symbol{gtk-wrap-mode}.}
  Gets the line wrapping for the view.
  @see-class{gtk-text-view}
  @see-symbol{gtk-wrap-mode}
  @see-function{gtk-text-view-set-wrap-mode}"
  (gtk-text-view-wrap-mode text-view))

(export 'gtk-text-view-get-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_editable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-editable))

(defun gtk-text-view-set-editable (text-view setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[setting]{whether @arg{text-view} is editable}
  @begin{short}
    Sets the default editability of the @class{gtk-text-view}.
  @end{short}
  You can override this default setting with tags in the buffer, using the
  \"editable\" attribute of tags.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-editable}"
  (setf (gtk-text-view-editable text-view) setting))

(export 'gtk-text-view-set-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_editable ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-editable))

(defun gtk-text-view-get-editable (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Whether text is editable by default.}
  @begin{short}
    Returns the default editability of the @class{gtk-text-view}.
  @end{short}
  Tags in the buffer may override this setting for some ranges of text.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-editable}"
  (gtk-text-view-editable text-view))

(export 'gtk-text-view-get-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_cursor_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-cursor-visible))

(defun gtk-text-view-set-cursor-visible (text-view setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[setting]{whether to show the insertion cursor}
  @begin{short}
    Toggles whether the insertion point is displayed.
  @end{short}
  A buffer with no editable text probably should not have a visible cursor, so
  you may want to turn the cursor off.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-cursor-visible}"
  (setf (gtk-text-view-cursor-visible text-view) setting))

(export 'gtk-text-view-set-cursor-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_cursor_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-cursor-visible))

(defun gtk-text-view-get-cursor-visible (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Whether the insertion mark is visible.}
  Find out whether the cursor is being displayed.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-cursor-visible}"
  (gtk-text-view-cursor-visible text-view))

(export 'gtk-text-view-get-cursor-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_cursor_blink ()
;;;
;;; void gtk_text_view_reset_cursor_blink (GtkTextView *text_view);
;;;
;;; Ensures that the cursor is shown (i.e. not in an 'off' blink interval) and
;;; resets the time that it will stay blinking (or visible, in case blinking is
;;; disabled).
;;;
;;; This function should be called in response to user input (e.g. from derived
;;; classes that override the textview's “key-press-event” handler).
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_overwrite ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-overwrite))

(defun gtk-text-view-set-overwrite (text-view overwrite)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widet}
  @argument[overwrite]{@em{true} to turn on overwrite mode, @code{nil} to turn
    it off}
  @begin{short}
    Changes the @class{gtk-text-view} overwrite mode.
  @end{short}
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-overwrite}"
  (setf (gtk-text-view-overwrite text-view) overwrite))

(export 'gtk-text-view-set-overwrite)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_overwrite ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-overwrite))

(defun gtk-text-view-get-overwrite (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Whether @arg{text-view} is in overwrite mode or not.}
  @begin{short}
    Returns whether the @class{gtk-text-view} is in overwrite mode or not.
  @end{short}
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-overwrite}"
  (gtk-text-view-overwrite text-view))

(export 'gtk-text-view-get-overwrite)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_pixels_above_lines ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-pixels-above-lines))

(defun gtk-text-view-set-pixels-above-lines (text-view pixels-above-lines)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[pixels-above-lines]{pixels above paragraphs}
  @begin{short}
    Sets the default number of blank pixels above paragraphs in @arg{text-view.}
  @end{short}
  Tags in the buffer for text_view may override the defaults.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-above-lines}"
  (setf (gtk-text-view-pixels-above-lines text-view) pixels-above-lines))

(export 'gtk-text-view-set-pixels-above-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_pixels_above_lines ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-pixels-above-lines))

(defun gtk-text-view-get-pixels-above-lines (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Default number of pixels above paragraphs.}
  Gets the default number of pixels to put above paragraphs.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-pixels-above-lines}"
  (gtk-text-view-pixels-above-lines text-view))

(export 'gtk-text-view-get-pixels-above-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_pixels_below_lines ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-pixels-below-lines))

(defun gtk-text-view-set-pixels-below-lines (text-view pixels-below-lines)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-22}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[pixels-below-lines]{pixels below paragraphs}
  @begin{short}
    Sets the default number of pixels of blank space to put below paragraphs in
    @arg{text-view}.
  @end{short}
  May be overridden by tags applied to @arg{text-view}'s buffer.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-below-lines}"
  (setf (gtk-text-view-pixels-below-lines text-view) pixels-below-lines))

(export 'gtk-text-view-set-pixels-below-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_pixels_below_lines ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-pixels-below-lines))

(defun gtk-text-view-get-pixels-below-lines (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Default number of blank pixels below paragraphs.}
  Gets the value set by the function @fun{gtk-text-view-set-pixels-below-lines}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-pixels-below-lines}"
  (gtk-text-view-pixels-below-lines text-view))

(export 'gtk-text-view-get-pixels-below-lines)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_pixels_inside_wrap ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-pixels-inside-wrap))

(defun gtk-text-view-set-pixels-inside-wrap (text-view pixels-inside-wrap)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[pixels-inside-wrap]{default number of pixels between wrapped lines}
  @begin{short}
    Sets the default number of pixels of blank space to leave between
    display/wrapped lines within a paragraph.
  @end{short}
  May be overridden by tags in @arg{text-view}'s buffer.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-pixels-inside-wrap}"
  (setf (gtk-text-view-pixels-inside-wrap text-view) pixels-inside-wrap))

(export 'gtk-text-view-set-pixels-inside-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_pixels_inside_wrap ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-pixels-inside-wrap))

(defun gtk-text-view-get-pixels-inside-wrap (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Default number of pixels of blank space between wrapped lines.}
  Gets the value set by the function @fun{gtk-text-view-set-pixels-inside-wrap}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-pixels-inside-wrap}"
  (gtk-text-view-pixels-inside-wrap text-view))

(export 'gtk-text-view-get-pixels-inside-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_justification ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-justification))

(defun gtk-text-view-set-justification (text-view justification)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[justification]{justification of type @symbol{gtk-justification}}
  @begin{short}
    Sets the default justification of text in @arg{text-view}.
  @end{short}
  Tags in the view's buffer may override the default.
  @see-class{gtk-text-view}
  @see-symbol{gtk-justification}
  @see-function{gtk-text-view-get-justification}"
  (setf (gtk-text-view-justification text-view) justification))

(export 'gtk-text-view-set-justification)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_justification ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-justification))

(defun gtk-text-view-get-justification (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{The default justification of type @symbol{gtk-justification}.}
  @begin{short}
    Gets the default justification of paragraphs in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-symbol{gtk-justification}
  @see-function{gtk-text-view-set-justification}"
  (gtk-text-view-justification text-view))

(export 'gtk-text-view-get-justification)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_left_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-left-margin))

(defun gtk-text-view-set-left-margin (text-view left-margin)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[left-margin]{left margin in pixels}
  @begin{short}
    Sets the default left margin for text in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-left-margin}"
  (setf (gtk-text-view-left-margin text-view) left-margin))

(export 'gtk-text-view-set-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_left_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-left-margin))

(defun gtk-text-view-get-left-margin (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{left margin in pixels}
  @begin{short}
    Gets the default left margin size of paragraphs in the @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-left-margin}"
  (gtk-text-view-left-margin text-view))

(export 'gtk-text-view-get-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_right_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-right-margin))

(defun gtk-text-view-set-right-margin (text-view right-margin)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[right-margin]{right margin in pixels}
  @begin{short}
    Sets the default right margin for text in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-right-margin}"
  (setf (gtk-text-view-right-margin text-view) right-margin))

(export 'gtk-text-view-set-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_right_margin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-right-margin))

(defun gtk-text-view-get-right-margin (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{right margin in pixels}
  @begin{short}
    Gets the default right margin for text in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-right-margin}"
  (gtk-text-view-right-margin text-view))

(export 'gtk-text-view-get-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_top_margin ()
;;;
;;; void
;;; gtk_text_view_set_top_margin (GtkTextView *text_view,
;;;                               gint top_margin);
;;;
;;; Sets the top margin for text in text_view .
;;;
;;; Note that this function is confusingly named. In CSS terms, the value set
;;; here is padding.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; top_margin :
;;; top margin in pixels
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_top_margin ()
;;;
;;; gint gtk_text_view_get_top_margin (GtkTextView *text_view);
;;;
;;; Gets the top margin for text in the text_view .
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     top margin in pixels
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_bottom_margin ()
;;;
;;; void
;;; gtk_text_view_set_bottom_margin (GtkTextView *text_view,
;;;                                  gint bottom_margin);
;;;
;;; Sets the bottom margin for text in text_view .
;;;
;;; Note that this function is confusingly named. In CSS terms, the value set
;;; here is padding.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; bottom_margin :
;;;     bottom margin in pixels
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_bottom_margin ()
;;;
;;; gint gtk_text_view_get_bottom_margin (GtkTextView *text_view);
;;;
;;; Gets the bottom margin for text in the text_view .
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     bottom margin in pixels
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_indent ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-indent))

(defun gtk-text-view-set-indent (text-view indent)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[indent]{indentation in pixels}
  @begin{short}
    Sets the default indentation for paragraphs in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-indent}"
  (setf (gtk-text-view-indent text-view) indent))

(export 'gtk-text-view-set-indent)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_indent ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-indent))

(defun gtk-text-view-get-indent (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{Number of pixels of indentation.}
  @begin{short}
    Gets the default indentation of paragraphs in @arg{text-view}.
  @end{short}
  Tags in the view's buffer may override the default. The indentation may be
  negative.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-indent}"
  (gtk-text-view-indent text-view))

(export 'gtk-text-view-get-indent)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_tabs ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-tabs))

(defun gtk-text-view-set-tabs (text-view tabs)
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[tabs]{tabs as a @class{pango-tab-array} structure}
  @begin{short}
    Sets the default tab stops for paragraphs in @arg{text-view}.
  @end{short}
  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-class{pango-tab-array}
  @see-function{gtk-text-view-get-tabs}"
  (setf (gtk-text-view-tabs text-view) tabs))

(export 'gtk-text-view-set-tabs)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_tabs ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-tabs))

(defun gtk-text-view-get-tabs (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{return}
    Copy of default tab array, or @code{nil} if \"standard\" tabs are used; must
    be freed with @fun{pango-tab-array-free}.
  @end{return}
  @begin{short}
    Gets the default tabs for @arg{text-view}.
  @end{short}
  Tags in the buffer may override the defaults. The returned array will be
  @code{nil} if \"standard\" (8-space) tabs are used. Free the return value with
  @fun{pango-tab-array-free}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-tabs}
  @see-class{pango-tab-array}
  @see-function{pango-tab-array-free}"
  (gtk-text-view-tabs text-view))

(export 'gtk-text-view-get-tabs)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_accepts_tab ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-set-accepts-tab))

(defun gtk-text-view-set-accepts-tab (text-view accepts-tab)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-19}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[accepts-tab]{@em{True} if pressing the Tab key should insert a tab
    character, @code{nil}, if pressing the Tab key should move the keyboard
    focus.}
  @begin{short}
    Sets the behavior of the text widget when the Tab key is pressed.
  @end{short}
  If @arg{accepts-tab} is @em{true}, a tab character is inserted. If
  @arg{accepts-tab} is @code{nil} the keyboard focus is moved to the next widget
  in the focus chain.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-get-accepts-tab}"
  (setf (gtk-text-view-accepts-tab text-view) accepts-tab))

(export 'gtk-text-view-set-accepts-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_accepts_tab ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-get-accepts-tab))

(defun gtk-text-view-get-accepts-tab (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-19}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{return}
    @em{True} if pressing the Tab key inserts a tab character, @code{nil} if
    pressing the Tab key moves the keyboard focus.
  @end{return}
  @begin{short}
    Returns whether pressing the Tab key inserts a tab characters.
  @end{short}
  See the function @fun{gtk-text-view-set-accepts-tab}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-set-accepts-tab}"
  (gtk-text-view-accepts-tab text-view))

(export 'gtk-text-view-get-accepts-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_default_attributes ()
;;;
;;; GtkTextAttributes * gtk_text_view_get_default_attributes
;;;                                                    (GtkTextView *text_view);
;;;
;;; Obtains a copy of the default text attributes. These are the attributes used
;;; for text unless a tag overrides them. You'd typically pass the default
;;; attributes in to gtk_text_iter_get_attributes() in order to get the
;;; attributes in effect at a given text position.
;;;
;;; The return value is a copy owned by the caller of this function, and should
;;; be freed.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Returns :
;;;     a new GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_im_context_filter_keypress ()
;;;
;;; gboolean gtk_text_view_im_context_filter_keypress (GtkTextView *text_view,
;;;                                                    GdkEventKey *event);
;;;
;;; Allow the GtkTextView input method to internally handle key press and
;;; release events. If this function returns TRUE, then no further processing
;;; should be done for this key event. See gtk_im_context_filter_keypress().
;;;
;;; Note that you are expected to call this function from your handler when
;;; overriding key event handling. This is needed in the case when you need to
;;; insert your own key handling between the input method and the default key
;;; event handling of the GtkTextView.
;;;
;;; static gboolean
;;; gtk_foo_bar_key_press_event (GtkWidget   *widget,
;;;                              GdkEventKey *event)
;;; {
;;;   if ((key->keyval == GDK_KEY_Return || key->keyval == GDK_KEY_KP_Enter))
;;;     {
;;;       if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (view),
;;;                                                     event))
;;;         return TRUE;
;;;     }
;;;
;;;     /* Do some stuff */
;;;
;;;   return GTK_WIDGET_CLASS (gtk_foo_bar_parent_class)
;;;                                           ->key_press_event (widget, event);
;;; }
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; event :
;;;     the key event
;;;
;;; Returns :
;;;     TRUE if the input method handled the key event.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_im_context ()
;;;
;;; void gtk_text_view_reset_im_context (GtkTextView *text_view);
;;;
;;; Reset the input method context of the text view if needed.
;;;
;;; This can be necessary in the case where modifying the buffer would confuse
;;; on-going input method behavior.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_input_purpose ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-text-view-set-input-purpose))

#+gtk-3-6
(defun gtk-text-view-set-input-purpose (text-view purpose)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[purpose]{the purpose of type @symbol{gtk-input-purpose}}
  @begin{short}
    Sets the @code{\"input-purpose\"} property which can be used by on-screen
    keyboards and other input methods to adjust their behaviour.
  @end{short}

  Since 3.6
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-purpose}
  @see-function{gtk-text-view-get-input-purpose}"
  (setf (gtk-text-view-input-purpose text-view) purpose))

#+gtk-3-6
(export 'gtk-text-view-set-input-purpose)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_input_purpose ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-text-view-get-input-purpose))

#+gtk-3-6
(defun gtk-text-view-get-input-purpose (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{short}
    Gets the value of the @code{\"input-purpose\"} property.
  @end{short}

  Since 3.6
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-purpose}
  @see-function{gtk-text-view-set-input-purpose}"
  (gtk-text-view-input-purpose text-view))

#+gtk-3-6
(export 'gtk-text-view-get-input-purpose)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_input_hints ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-text-view-set-input-hints))

#+gtk-3-6
(defun gtk-text-view-set-input-hints (text-view hints)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[hints]{the hints}
  @begin{short}
    Sets the @code{\"input-hints\"} property, which allows input methods to
    fine-tune their behaviour.
  @end{short}

  Since 3.6
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-hints}
  @see-function{gtk-text-view-get-input-hints}"

  (setf (gtk-text-view-input-hints text-view) hints))

#+gtk-3-6
(export 'gtk-text-view-set-input-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_input_hints ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-text-view-get-input-hints))

#+gtk-3-6
(defun gtk-text-view-get-input-hints (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{short}
    Gets the value of the @code{\"input-hints\"} property.
  @end{short}

  Since 3.6
  @see-class{gtk-text-view}
  @see-symbol{gtk-text-view}
  @see-function{gtk-text-view-set-input-hints}"
  (gtk-text-view-input-hints text-view))

#+gtk-3-6
(export 'gtk-text-view-get-input-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_monospace ()
;;;
;;; void
;;; gtk_text_view_set_monospace (GtkTextView *text_view,
;;;                              gboolean monospace);
;;;
;;; Sets the “monospace” property, which indicates that the text view should use
;;; monospace fonts.
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; monospace :
;;;     TRUE to request monospace styling
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_monospace ()
;;;
;;; gboolean gtk_text_view_get_monospace (GtkTextView *text_view);
;;;
;;; Gets the value of the “monospace” property.
;;;
;;; Return: TRUE if monospace fonts are desired
;;;
;;; text_view :
;;;     a GtkTextView
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.text-view.lisp -----------------------------------------
