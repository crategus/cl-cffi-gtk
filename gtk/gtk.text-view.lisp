;;; ----------------------------------------------------------------------------
;;; gtk.text-view.lisp
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
;;;     GtkWrapMode                               ---> gtk.text-attributes.lisp
;;;     GtkTextChildAnchor
;;;
;;;     GTK_TEXT_VIEW_PRIORITY_VALIDATE
;;;
;;; Functions
;;;
;;;     gtk_text_view_new
;;;     gtk_text_view_new_with_buffer
;;;     gtk_text_view_set_buffer                           Accessor
;;;     gtk_text_view_get_buffer                           Accessor
;;;     gtk_text_view_get_hadjustment                      deprecated
;;;     gtk_text_view_get_vadjustment                      deprecated
;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;
;;;     gtk_text_view_get_window
;;;     gtk_text_view_get_window_type
;;;     gtk_text_view_set_border_window_size
;;;     gtk_text_view_get_border_window_size
;;;
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
;;;     gtk_text_view_set_wrap_mode                        Accessor
;;;     gtk_text_view_get_wrap_mode                        Accessor
;;;     gtk_text_view_set_editable                         Accessor
;;;     gtk_text_view_get_editable                         Accessor
;;;     gtk_text_view_set_cursor_visible                   Accessor
;;;     gtk_text_view_get_cursor_visible                   Accessor
;;;     gtk_text_view_reset_cursor_blink
;;;     gtk_text_view_set_overwrite                        Accessor
;;;     gtk_text_view_get_overwrite                        Accessor
;;;     gtk_text_view_set_pixels_above_lines               Accessor
;;;     gtk_text_view_get_pixels_above_lines               Accessor
;;;     gtk_text_view_set_pixels_below_lines               Accessor
;;;     gtk_text_view_get_pixels_below_lines               Accessor
;;;     gtk_text_view_set_pixels_inside_wrap               Accessor
;;;     gtk_text_view_get_pixels_inside_wrap               Accessor
;;;     gtk_text_view_set_justification                    Accessor
;;;     gtk_text_view_get_justification                    Accessor
;;;     gtk_text_view_set_left_margin                      Accessor
;;;     gtk_text_view_get_left_margin                      Accessor
;;;     gtk_text_view_set_right_margin                     Accessor
;;;     gtk_text_view_get_right_margin                     Accessor
;;;     gtk_text_view_set_top_margin                       Accessor
;;;     gtk_text_view_get_top_margin                       Accessor
;;;     gtk_text_view_set_bottom_margin                    Accessor
;;;     gtk_text_view_get_bottom_margin                    Accessor
;;;     gtk_text_view_set_indent                           Accessor
;;;     gtk_text_view_get_indent                           Accessor
;;;     gtk_text_view_set_tabs                             Accessor
;;;     gtk_text_view_get_tabs                             Accessor
;;;     gtk_text_view_set_accepts_tab                      Accessor
;;;     gtk_text_view_get_accepts_tab                      Accessor
;;;     gtk_text_view_get_default_attributes
;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context
;;;     gtk_text_view_set_input_purpose                    Accessor
;;;     gtk_text_view_get_input_purpose                    Accessor
;;;     gtk_text_view_set_input_hints                      Accessor
;;;     gtk_text_view_get_input_hints                      Accessor
;;;     gtk_text_view_set_monospace                        Accessor
;;;     gtk_text_view_get_monospace                        Accessor
;;;
;;; Properties
;;;
;;;            gboolean    accepts-tab              Read / Write
;;;                gint    bottom-margin            Read / Write
;;;       GtkTextBuffer*   buffer                   Read / Write
;;;            gboolean    cursor-visible           Read / Write
;;;            gboolean    editable                 Read / Write
;;;               gchar*   im-module                Read / Write
;;;                gint    indent                   Read / Write
;;;       GtkInputHints    input-hints              Read / Write
;;;     GtkInputPurpose    input-purpose            Read / Write
;;;    GtkJustification    justification            Read / Write
;;;                gint    left-margin              Read / Write
;;;            gboolean    monospace                Read / Write
;;;            gboolean    overwrite                Read / Write
;;;                gint    pixels-above-lines       Read / Write
;;;                gint    pixels-below-lines       Read / Write
;;;                gint    pixels-inside-wrap       Read / Write
;;;            gboolean    populate-all             Read / Write
;;;                gint    right-margin             Read / Write
;;;       PangoTabArray*   tabs                     Read / Write
;;;                gint    top-margin               Read / Write
;;;         GtkWrapMode    wrap-mode                Read / Write
;;;
;;; Style Properties
;;;
;;;            GdkColor*   error-underline-color    Read
;;;
;;; Signals
;;;
;;;                void    backspace                Action
;;;                void    copy-clipboard           Action
;;;                void    cut-clipboard            Action
;;;                void    delete-from-cursor       Action
;;;            gboolean    extend-selection         Run Last
;;;                void    insert-at-cursor         Action
;;;                void    insert-emoji             Action
;;;                void    move-cursor              Action
;;;                void    move-viewport            Action
;;;                void    paste-clipboard          Action
;;;                void    populate-popup           Run Last
;;;                void    preedit-changed          Action
;;;                void    select-all               Action
;;;                void    set-anchor               Action
;;;                void    toggle-cursor-visible    Action
;;;                void    toggle-overwrite         Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GInitiallyUnowned
;;;     │   ╰── GtkWidget
;;;     │       ╰── GtkContainer
;;;     │           ╰── GtkTextView
;;;     │
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
;;; enum GtkTextViewLayer
;;; ----------------------------------------------------------------------------

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-layer atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-view-layer atdoc:*external-symbols*)
 "@version{2020-6-7}
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
  @see-class{gtk-text-view}")

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
 "@version{2020-6-7}
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
  @end{pre}
  @begin[code]{table}
    @entry[:private]{Invalid value, used as a marker.}
    @entry[:widget]{Window that floats over scrolling areas.}
    @entry[:text]{Scrollable text window.}
    @entry[:left]{Left side border window.}
    @entry[:right]{Right side border window.}
    @entry[:top]{Top border window.}
    @entry[:bottom]{Bottom border window.}
  @end{table}
  @see-class{gtk-text-view}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTextExtendSelection
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTextExtendSelection" gtk-text-extend-selection
  (:export t
   :type-initializer "gtk_text_extend_selection_get_type")
  (:word 0)
  (:line 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-extend-selection atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-text-extend-selection atdoc:*external-symbols*)
 "@version{2020-11-25}
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
    @entry[:word]{Selects the current word. It is triggered by a double click
      for example.}
    @entry[:line]{Selects the current line. It is triggered by a triple click
      for example.}
  @end{table}
  @see-class{gtk-text-view}")

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
 "@version{2020-6-7}
  @begin{short}
    A @sym{gtk-text-child-anchor} is a spot in the buffer where child widgets
    can be \"anchored\", inserted inline, as if they were characters.
  @end{short}
  The anchor can have multiple widgets anchored, to allow for multiple views.
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
  ((accepts-tab
    gtk-text-view-accepts-tab
    "accepts-tab" "gboolean" t t)
   #+gtk-3-18
   (bottom-margin
    gtk-text-view-bottom-margin
    "bottom-margin" "gint" t t)
   (buffer
    gtk-text-view-buffer
    "buffer" "GtkTextBuffer" t t)
   (cursor-visible
    gtk-text-view-cursor-visible
    "cursor-visible" "gboolean" t t)
   (editable
    gtk-text-view-editable
    "editable" "gboolean" t t)
   (im-module
    gtk-text-view-im-module
    "im-module" "gchararray" t t)
   (indent
    gtk-text-view-indent
    "indent" "gint" t t)
   (input-hints
    gtk-text-view-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    gtk-text-view-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (justification
    gtk-text-view-justification
    "justification" "GtkJustification" t t)
   (left-margin
    gtk-text-view-left-margin
    "left-margin" "gint" t t)
   (monospace
    gtk-text-view-monospace
    "monospace" "gboolean" t t)
   (overwrite
    gtk-text-view-overwrite
    "overwrite" "gboolean" t t)
   (pixels-above-lines
    gtk-text-view-pixels-above-lines
    "pixels-above-lines" "gint" t t)
   (pixels-below-lines
    gtk-text-view-pixels-below-lines
    "pixels-below-lines" "gint" t t)
   (pixels-inside-wrap
    gtk-text-view-pixels-inside-wrap
    "pixels-inside-wrap" "gint" t t)
   (populate-all
    gtk-text-view-populate-all
    "populate-all" "gboolean" t t)
   (right-margin
    gtk-text-view-right-margin
    "right-margin" "gint" t t)
   (tabs
    gtk-text-view-tabs
    "tabs" "PangoTabArray" t t)
   (top-margin
    gtk-text-view-top-margin
    "top-margin" "gint" t t)
   (wrap-mode
    gtk-text-view-wrap-mode
    "wrap-mode" "GtkWrapMode" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-view 'type)
 "@version{*2021-2-14}
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
    The @sym{gtk-text-view} widget has a main CSS node with name
    @code{textview} and style class @code{.view}, and subnodes for each of the
    border windows, and the main text area, with names @code{border} and
    @code{text}, respectively. The border nodes each get one of the style
    classes @code{.left}, @code{.right}, @code{.top} or @code{.bottom}.

    A node representing the selection will appear below the text node.

    If a context menu is opened, the window node will appear as a subnode of
    the main node.
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
 lambda (view)    :action
      @end{pre}
      This signal is a keybinding signal which gets emitted when the user asks
      for it. The default bindings for this signal are the @kbd{Backspace} and
      @kbd{Shift-Backspace} keys.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (view)    :action
      @end{pre}
      This  signal is a keybinding signal which gets emitted to copy the
      selection to the clipboard. The default bindings for this signal are
      the @kbd{Ctrl-c} and @kbd{Ctrl-Insert} keys.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
 lambda (view)    :action
      @end{pre}
      This signal is a keybinding signal which gets emitted to cut the selection
      to the clipboard. The default bindings for this signal are the
      @kbd{Ctrl-x} and @kbd{Shift-Delete} keys.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
 lambda (view type count)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a text deletion. If the type is @code{:chars}, GTK deletes the
      selection if there is one, otherwise it deletes the requested number of
      characters. The default bindings for this signal are the @kbd{Delete} key
      for deleting a character, the @kbd{Ctrl-Delete} key for deleting a word
      and the @kbd{Ctrl-Backspace} key for deleting a word backwords.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
        @entry[type]{The granularity of the deletion, as a value of the
          @symbol{gtk-delete-type} enumeration.}
        @entry[count]{An integer with the number of type units to delete.}
      @end{table}
    @subheading{The \"extend-selection\" signal}
      @begin{pre}
 lambda (view granularity location start end)    : Run Last
      @end{pre}
      This signal is emitted when the selection needs to be extended at
      @arg{location}.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
        @entry[granularity]{The granularity as a value of the
          @symbol{gtk-text-extend-selection} enumeration.}
        @entry[location]{The @class{gtk-text-iter} iterator where to extend the
          selection.}
        @entry[start]{The @class{gtk-text-iter} iterator where the selection
          should start.}
        @entry[end]{The @class{gtk-text-iter} iterator where the selection
          should end.}
        @entry[Returns]{@var{+gdk-event-stop+} to stop other handlers from being
          invoked for the event, @var{+gdk-event-propagate+} to propagate the
          event further.}
      @end{table}
    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
 lambda (view string)
      @end{pre}
      This signal is a keybinding signal which gets emitted when the user
      initiates the insertion of a fixed string at the cursor. This signal has
      no default bindings.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"insert-emoji\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      This signal is a keybinding signal which gets emitted to present the Emoji
      chooser for the text view. The default bindings for this signal are
      @code{Ctrl-.} and @code{Ctrl-;}. Since 3.22
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (view step count extend)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the text
      view, this signal causes the viewport to be moved instead. Applications
      should not connect to it, but may emit it with the @fun{g-signal-emit}
      function if they need to control the cursor programmatically. The default
      bindings for this signal come in two variants, the variant with the
      @kbd{Shift} modifier extends the selection, the variant without the
      @kbd{Shift} modifer does not. There are too many key combinations to list
      them all here. Arrow keys move by individual characters/lines.
      @kbd{Ctrl}-arrow key combinations move by words/paragraphs.
      @kbd{Home}/@kbd{End} keys move to the ends of the buffer.
      @kbd{PageUp}/@kbd{PageDown} keys move vertically by pages.
      @kbd{Ctrl-PageUp}/@kbd{PageDown} keys move horizontally by pages.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{table}
    @subheading{The \"move-viewport\" signal}
      @begin{pre}
 lambda (view step count)    : Action
      @end{pre}
      This signal is a keybinding signal which can be bound to key combinations
      to allow the user to move the viewport, i.e. change what part of the text
      view is visible in a containing scrolled window. There are no default
      bindings for this signal.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
 lambda (view)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to paste the
      contents of the clipboard into the text view. The default bindings for
      this signal are the @kbd{Ctrl-v} and @code{Shift-Insert} keys.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (view popup)    : Run Last
      @end{pre}
      This signal gets emitted before showing the context menu of the text view.
      If you need to add items to the context menu, connect to this signal and
      append your items to the popup, which will be a @class{gtk-menu} widget
      in this case. If the @code{populate-all} property is @em{true}, this
      signal will also be emitted to populate touch popups. In this case, popup
      will be a different container, e.g. a @class{gtk-toolbar} widget. The
      signal handler should not make assumptions about the type of the widget,
      but check whether popup is a @class{gtk-menu} widget or
      @class{gtk-toolbar} widget or another kind of container.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget on which the signal is
          emitted.}
        @entry[popup]{The @class{gtk-widget} container that is being populated.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
 lambda (view preedit)    : Action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal. This signal is only emitted if the text at the given position
      is actually editable.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} object which received the signal.}
        @entry[preedit]{A string with the current preedit string.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (view select)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to select or unselect
      the complete contents of the text view. The default bindings for the
      signal are the @kbd{Ctrl-a} and @kbd{Ctrl-/} keys for selecting and
      @kbd{Shift-Ctrl-a} and @kbd{Ctrl-\} keys for unselecting.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-viw} widget which received the signal.}
        @entry[select]{@em{True} to select, @em{false} to unselect.}
      @end{table}
    @subheading{The \"set-anchor\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      This signal is a keybinding signal which gets emitted when the user
      initiates setting the \"anchor\" mark. The \"anchor\" mark gets placed at
      the same position as the \"insert\" mark. This signal has no default
      bindings.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"toggle-cursor-visible\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      This  signal is a keybinding signal which gets emitted to toggle the
      visibility of the cursor. The default binding for this signal is F7.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
      @end{table}
    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
 lambda (view)    : Action
      @end{pre}
      This signal is a keybinding signal which gets emitted to toggle the
      overwrite mode of the text view. The default bindings for this signal is
      Insert.
      @begin[code]{table}
        @entry[view]{The @sym{gtk-text-view} widget which received the signal.}
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
  @see-slot{gtk-text-view-wrap-mode}
  @see-class{gtk-text-buffer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-text-view-accepts-tab ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accepts-tab" 'gtk-text-view) 't)
 "The @code{accepts-tab} property of type @code{:boolean} (Read / Write) @br{}
  Whether Tab will result in a tab character being entered. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-accepts-tab atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-accepts-tab 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-accepts-tab object) => accepts-tab}
  @syntax[]{(setf (gtk-text-view-accepts-tab object) accepts-tab)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[accepts-tab]{@em{true} if pressing the Tab key should insert a tab
    character, @em{false}, if pressing the Tab key should move the keyboard
    focus}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{accepts-tab} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function function @sym{gtk-text-view-accepts-tab} returns
  the behavior of the text view when the Tab key is pressed. The slot access
  function @sym{(setf gtk-text-view-accepts-tab)} sets the behavior.

  If @arg{accepts-tab} is @em{true}, a tab character is inserted. If
  @arg{accepts-tab} is @em{false} the keyboard focus is moved to the next
  widget in the focus chain.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-bottom-margin --------------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "bottom-margin" 'gtk-text-view)
                     't)
 "The @code{bottom-margin} property of type @code{:int} (Read / Write) @br{}
  The bottom margin for text in the text view. Note that this property is
  confusingly named. In CSS terms, the value set here is padding, and it is
  applied in addition to the padding from the theme. Do not confuse this
  property with the @slot[gtk-widget]{margin-bottom} property. Since 3.18 @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-bottom-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-bottom-margin 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-bottom-margin object) => bottom-margin}
  @syntax[]{(setf (gtk-text-view-bottom-margin object) bottom-margin)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[bottom-margin]{an integer with the bottom margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{bottom-margin} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-margin-bottom} gets the bottom
  margin for text in the text view. The slot access function
  @sym{(setf gtk-text-view-margin-bottom)} sets the bottom margin.

  Note that this function is confusingly named. In CSS terms, the value set
  here is padding.

  Since 3.18
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-buffer ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buffer" 'gtk-text-view) 't)
 "The @code{buffer} property of type  @class{gtk-text-buffer} (Read / Write)
  @br{}
  The text buffer which is displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-buffer atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-text-view-buffer 'function)
 "@version{*2021-2-14}
  @syntax[]{(gtk-text-view-buffer object) => buffer}
  @syntax[]{(setf (gtk-text-view-buffer object) buffer)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{buffer} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-buffer} returns the text buffer
  being displayed by the text view. The slot access function
  @sym{(setf gtk-text-view-buffer)} sets the text buffer.
  @see-class{gtk-text-view}
  @see-class{gtk-text-buffer}")

;;; --- gtk-text-view-cursor-visible -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-visible"
                                               'gtk-text-view) 't)
 "The @code{cursor-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  WHether the insertion cursor is shown. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-cursor-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-cursor-visible 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-cursor-visible object) => setting}
  @syntax[]{(setf (gtk-text-view-cursor-visible object) setting)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[setting]{a boolean whether to show the insertion cursor}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{cursor-visible} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-cursor-visible} returns whether
  the insertion mark is visible. The slot access function
  @sym{(setf gtk-text-view-cursor-visible)} toggles whether the insertion point
  is displayed.

  A buffer with no editable text probably should not have a visible cursor, so
  you may want to turn the cursor off.
  @see-class{gtk-text-view}")

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
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-editable object) => setting}
  @syntax[]{(setf (gtk-text-view-editable object) setting)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[setting]{a boolean whether the text view is editable}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{editable} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-editable} returns the default
  editability of the text view. The slot access function
  @sym{(setf gtk-text-view-editable)} sets the default editability.

  You can override this default setting with tags in the text buffer, using the
  @slot[gtk-text-tag]{editable} attribute of tags.
  @see-class{gtk-text-view}
  @see-class{gtk-text-tag}")

;;; --- gtk-text-view-im-module ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "im-module" 'gtk-text-view) 't)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used for this entry. See the
  @class{gtk-im-context} class. Setting this to a non-@code{nil} value overrides
  the system-wide IM module setting. See the @slot[gtk-settings]{gtk-im-module}
  setting. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-im-module atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-im-module 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-im-module object) => im-module}
  @syntax[]{(setf (gtk-text-view-im-module object) im-module)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[im-module]{a string with the IM module to use for the entry}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{im-module} slot of the
    @class{gtk-text-view} class.
  @end{short}

  Which IM (input method) module should be used for this entry. See the
  @class{gtk-im-context} class. Setting this to a non-@code{nil} value overrides
  the system-wide IM module setting. See the @slot[gtk-settings]{gtk-im-module}
  setting.
  @see-class{gtk-text-view}
  @see-class{gtk-im-context}")

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
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-indent object) => indent}
  @syntax[]{(setf (gtk-text-view-indent object) indent)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[indent]{an integer with the indentation in pixels}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{indent} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-indent} gets the default
  indentation of paragraphs in the text view. The slot access function
  @sym{(setf gtk-text-view-indent)} sets the default indentation.

  Tags in the view's text buffer may override the default. The indentation may
  be negative.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-input-hints ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-hints" 'gtk-text-view) 't)
 "The @code{input-hints} property of type @symbol{gtk-input-hints}
  (Read / Write) @br{}
  Additional hints, beyond the \"input-purpose\" signal, that allow input
  methods to fine-tune their behaviour.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-input-hints atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-input-hints 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-input-hints object) => hints}
  @syntax[]{(setf (gtk-text-view-input-hints object) hints)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[hints]{the hints as a value of the @symbol{gtk-input-hints} flags}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{input-hints} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-input-hints} gets the value of
  the @slot[gtk-text-view]{input-hints} property, which allows input methods to
  fine-tune their behaviour. The slot access function
  @sym{(setf gtk-text-view-input-hints)} sets the
  @slot[gtk-text-view]{input-hints} property.
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-hints}")

;;; --- gtk-text-view-input-purpose --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-purpose"
                                               'gtk-text-view) 't)
 "The @code{input-purpose} property of type @symbol{gtk-input-purpose}
  (Read / Write) @br{}
  The purpose of this text field. This property can be used by on-screen
  keyboards and other input methods to adjust their behaviour. @br{}
  Default value: @code{:free-form}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-input-purpose atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-input-purpose 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-input-purpose object) => purpose}
  @syntax[]{(setf (gtk-text-view-input-purpose object) purpose)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[purpose]{the purpose as value of the @symbol{gtk-input-purpose}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{input-purpose} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-input-purpose} gets the value of
  the @slot[gtk-text-view]{input-purpose} property, which can be used by
  on-screen keyboards and other input methods to adjust their behaviour. The
  slot access function @sym{(setf gtk-text-view-input-purpose)} sets the
  @slot[gtk-text-view]{input-purpose} property.
  @see-class{gtk-text-view}
  @see-symbol{gtk-input-purpose}")

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
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-justification object) => justification}
  @syntax[]{(setf (gtk-text-view-justification object) justification)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[justification]{a value of the @symbol{gtk-justification}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{justification} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-justification} gets the default
  justification of paragraphs in the text view. The slot access function
  @sym{(setf gtk-text-view-justification)} sets the default justification.

  Tags in the buffer may override the default.
  @see-class{gtk-text-view}
  @see-symbol{gtk-justification}")

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
 "@version{*2021-2-14}
  @syntax[]{(gtk-text-view-left-margin object) => left-margin}
  @syntax[]{(setf (gtk-text-view-left-margin object) left-margin)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[left-margin]{an integer with the left margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{left-margin} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-left-margin} gets the default
  left margin size of paragraphs in the text view. The slot access function
  @sym{(setf gtk-text-view-left-margin)} sets the default left margin.

  Tags in the buffer may override the default.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-monospace ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "monospace"
                                               'gtk-text-view) 't)
 "The @code{monospace} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use a monospace font. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-monospace atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-monospace 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-monospace object) => monospace}
  @syntax[]{(setf (gtk-text-view-monospace object) monospace)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[monospace]{@em{true} to request monospace styling}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{monospace} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-monospace} gets the value of the
  @slot[gtk-text-view]{monospace} property, which indicates that the text view
  should use monospace fonts. The slot access function
  @sym{(setf gtk-text-view-monospace)} sets the @slot[gtk-text-view]{monospace}
  property.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-overwrite ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "overwrite" 'gtk-text-view) 't)
 "The @code{overwrite} property of type @code{:boolean} (Read / Write) @br{}
  Whether entered text overwrites existing contents. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-overwrite atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-overwrite 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-overwrite object) => overwrite}
  @syntax[]{(setf (gtk-text-view-overwrite object) overwrite)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[overwrite]{@em{true} to turn on overwrite mode, @em{false} to turn
    it off}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{overwrite} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-overwrite} returns whether the
  text view is in overwrite mode or not. The slot access function
  @sym{(setf gtk-text-view-overwrite)} changes the text view overwrite mode.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-pixels-above-lines ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-above-lines"
                                               'gtk-text-view) 't)
 "The @code{pixels-above-lines} property of type @code{:int} (Read / Write)
  @br{}
  Pixels of blank space above paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-above-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-above-lines 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-pixels-above-lines object) => pixels-above-lines}
  @syntax[]{(setf (gtk-text-view-pixels-above-lines object) pixels-above-lines)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[pixels-above-lines]{an integer with the pixels above paragraphs}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{pixels-above-lines} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-pixels-above-lines} gets the
  default number of pixels to put above paragraphs in the text view. The slot
  access function @sym{(setf gtk-text-view-pixels-above-lines)} sets the default
  number of blank pixels.

  Tags in the text buffer for the text view may override the defaults.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-pixels-below-lines ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-below-lines"
                                               'gtk-text-view) 't)
 "The @code{pixels-below-lines} property of type @code{:int} (Read / Write)
  @br{}
  Pixels of blank space below paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-below-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-below-lines 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-pixels-below-lines object) => pixels-below-lines}
  @syntax[]{(setf (gtk-text-view-pixels-below-lines object) pixels-below-lines)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[pixels-below-lines]{an integer with the pixels below paragraphs}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{pixels-below-lines} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-pixels-below-lines} gets the
  default number of pixels to put below paragraphs in the text view. The slot
  access function @sym{(setf gtk-text-view-pixels-below-lines)} sets the default
  number of pixels of blank space to put below paragraphs.

  May be overridden by tags applied to the text view's text buffer.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-pixels-inside-wrap ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-inside-wrap"
                                               'gtk-text-view) 't)
 "The @code{pixels-inside-wrap} property of type @code{:int} (Read / Write)
  @br{}
  Pixels of blank space between wrapped lines in a paragraph. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-pixels-inside-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-pixels-inside-wrap 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-pixels-inside-wrap object) => pixels-inside-wrap}
  @syntax[]{(setf (gtk-text-view-pixels-inside-wrap object) pixels-inside-wrap)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[pixels-inside-wrap]{an integer with the default number of pixels
    between wrapped lines}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{pixels-inside-wrap} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-pixels-inside-wrap} gets the
  default number of pixels of blank space to leave between display/wrapped
  lines within a paragraph. The slot access function
  @sym{(setf gtk-text-view-pixels-inside-wrap)} sets the default number of
  pixels.

  May be overridden by tags in the text view's text buffer.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-populate-all ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "populate-all"
                                               'gtk-text-view) 't)
 "The @code{populate-all} property of type @code{:boolean} (Read / Write) @br{}
  If @code{populate-all} is @em{true}, the \"populate-popup\" signal is also
  emitted for touch popups. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-populate-all atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-populate-all 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-populate-all object) => populate-all}
  @syntax[]{(setf (gtk-text-view-populate-all object) populate-all)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[populate-all]{a boolean whether the \"populate-all\" signal is also
    emitted for touch popups}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{populate-all} slot of the
    @class{gtk-text-view} class.
  @end{short}

  If @code{populate-all} is @em{true}, the \"populate-popup\" signal is also
  emitted for touch popups.
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
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-right-margin object) => right-margin}
  @syntax[]{(setf (gtk-text-view-right-margin object) right-margin)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[right-margin]{an integer with the right margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{right-margin} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-right-margin} gets the default
  right margin for text in the text view. The slot access function
  @sym{(setf gtk-text-view-right-margin)} sets the default right margin.

  Tags in the buffer may override the default.
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-tabs -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tabs" 'gtk-text-view) 't)
 "The @code{tabs} property of type @class{pango-tab-array} (Read / Write) @br{}
  Custom tabs for this text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-tabs 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-tabs object) => tabs}
  @syntax[]{(setf (gtk-text-view-tabs object) tabs)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[tabs]{tabs as a @class{pango-tab-array} instance}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{tabs} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-tabs} gets a copy of the default
  Pango tab array, or @code{nil} if \"standard\" tabs are used. The slot access
  function @sym{(setf gtk-text-view-tabs)} sets the default tab stops for
  paragraphs.

  Tags in the text buffer may override the defaults.
  @see-class{gtk-text-view}
  @see-class{pango-tab-array}")

;;; --- gtk-text-view-top-margin -----------------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "top-margin" 'gtk-text-view) 't)
 "The @code{top-margin} property of type @code{:int} (Read / Write) @br{}
  The top margin for text in the text view. Note that this property is
  confusingly named. In CSS terms, the value set here is padding, and it is
  applied in addition to the padding from the theme. Don't confuse this property
  with the @slot[gtk-widget]{margin-top} property. Since 3.18 @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-view-top-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-top-margin 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-top-margin object) => top-margin}
  @syntax[]{(setf (gtk-text-view-top-margin object) top-margin)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[top-margin]{an integer with the top margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{top-margin} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-top-margin} gets the top margin
  for text in the text view. The slot access function
  @sym{(setf gtk-text-view-top-margin)} sets the top margin.

  Note that this function is confusingly named. In CSS terms, the value set
  here is padding.

  Since 3.18
  @see-class{gtk-text-view}")

;;; --- gtk-text-view-wrap-mode ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-text-view) 't)
 "The @code{wrap-mode} property of type @symbol{gtk-wrap-mode} (Read / Write)
  @br{}
  Whether to wrap lines never, at word boundaries, or at character
  boundaries. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-view-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-view-wrap-mode 'function)
 "@version{2021-2-14}
  @syntax[]{(gtk-text-view-wrap-mode object) => wrap-mode}
  @syntax[]{(setf (gtk-text-view-wrap-mode object) wrap-mode)}
  @argument[object]{a @class{gtk-text-view} widget}
  @argument[wrap-mode]{a value of the @symbol{gtk-wrap-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-text-view]{wrap-mode} slot of the
    @class{gtk-text-view} class.
  @end{short}

  The slot access function @sym{gtk-text-view-wrap-mode} gets the line wrapping
  for the text view. The slot access function
  @sym{(setf gtk-text-view-wrap-mode)} sets the line wrapping.
  @see-class{gtk-text-view}
  @see-symbol{gtk-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-new))

(defun gtk-text-view-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @return{A new @class{gtk-text-view} widget.}
  @begin{short}
    Creates a new text view.
  @end{short}
  If you do not call the function @fun{gtk-text-view-buffer} before using the
  text view, an empty default buffer will be created for you. Get the buffer
  with the function @fun{gtk-text-view-buffer}. If you want to specify your own
  buffer, consider the function @fun{gtk-text-view-new-with-buffer}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-buffer}
  @see-function{gtk-text-view-new-with-buffer}"
  (make-instance 'gtk-text-view))

(export 'gtk-text-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-view-new-with-buffer))

(defun gtk-text-view-new-with-buffer (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{A new @class{gtk-text-view} widget.}
  @begin{short}
    Creates a new text view displaying the buffer.
  @end{short}

  One buffer can be shared among many widgets. The argument @arg{buffer} may be
  @code{nil} to create a default buffer, in which case this function is
  equivalent to the function @fun{gtk-text-view-new}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-new}"
  (make-instance 'gtk-text-view
                 :buffer buffer))

(export 'gtk-text-view-new-with-buffer)

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
  (view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun gtk-text-view-scroll-to-mark (view mark &key (within-margin 0.4)
                                                    (x-align 0.0 x-align-p)
                                                    (y-align 0.0 y-align-p))
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[mark]{a @class{gtk-text-mark} object}
  @argument[within-margin]{margin as a [0.0, 0.5) fraction of screen size}
  @argument[xalign]{a @code{:double} with the horizontal alignment of the mark
    within visible area}
  @argument[yalign]{a @code{:double} with the vertical alignment of the mark
    within visible area}
  @begin{short}
    Scrolls the text view so that the mark is on the screen in the position
    indicated by @arg{xalign} and @arg{yalign}.
  @end{short}

  An alignment of 0.0 indicates left or top, 1.0 indicates right or bottom, 0.5
  means center. If you don't pass a value for @arg{x-align} and @arg{y-align},
  the text scrolls the minimal distance to get the mark onscreen, possibly not
  scrolling at all. The effective screen for purposes of this function is
  reduced by a margin of size @arg{within-margin}.
  @see-class{gtk-text-view}"
  (%gtk-text-view-scroll-to-mark view
                                 mark
                                 (coerce within-margin 'double-float)
                                 (or x-align-p y-align-p)
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

(defun gtk-text-view-scroll-to-iter (view iter &key (within-margin 0.4)
                                                    (x-align 0.0 x-align-p)
                                                    (y-align 0.0 y-align-p))
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[within-margin]{margin as a [0.0, 0.5) fraction of screen size}
  @argument[xalign]{a @code{:double} with the horizontal alignment of the mark
    within visible area}
  @argument[yalign]{a @code{:double} with the vertical alignment of the mark
    within visible area}
  @return{@em{True} if scrolling occurred.}
  @begin{short}
    Scrolls the text view so that the iterator is on the screen in the position
    indicated by @arg{xalign} and @arg{yalign}.
  @end{short}
  An alignment of 0.0 indicates left or top, 1.0 indicates right or bottom, 0.5
  means center. If you don't pass a value for @arg{x-align} and @arg{y-align},
  the text scrolls the minimal distance to get the iterator onscreen, possibly
  not scrolling at all. The effective screen for purposes of this function is
  reduced by a margin of size @arg{within-margin}.

  Note that this function uses the currently-computed height of the lines in
  the text buffer. Line heights are computed in an idle handler; so this
  function may not have the desired effect if it is called before the height
  computations. To avoid oddness, consider using the function
  @fun{gtk-text-view-scroll-to-mark} which saves a point to be scrolled to
  after line validation.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-scroll-to-mark}"
  (%gtk-text-view-scroll-to-iter view
                                 iter
                                 (coerce within-margin 'double-float)
                                 (or x-align-p y-align-p)
                                 (coerce x-align 'double-float)
                                 (coerce y-align 'double-float)))

(export 'gtk-text-view-scroll-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_mark_onscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_scroll_mark_onscreen"
          gtk-text-view-scroll-mark-onscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[mark]{a @class{gtk-text-mark} object in the buffer for the text
    view}
  @begin{short}
    Scrolls the text view the minimum distance such that the mark is contained
    within the visible area of the widget.
  @end{short}
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-view-scroll-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_mark_onscreen ()
;;; ----------------------------------------------------------------------------

;; TODO: Check this implementation. Is it correct?

(defcfun ("gtk_text_view_move_mark_onscreen"
          gtk-text-view-move-mark-onscreen) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[mark]{a @class{gtk-text-mark} object}
  @return{@em{True} if the mark moved, was not already onscreen.}
  @begin{short}
    Moves a the mark within the buffer so that it is located within the
    currently visible text area.
  @end{short}
  @see-class{gtk-text-view}"
  (text-view (g-object gtk-text-view))
  (mark (g-object gtk-text-mark)))

(export 'gtk-text-view-move-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_place_cursor_onscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_place_cursor_onscreen"
          gtk-text-view-place-cursor-onscreen) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{@em{True} if the cursor had to be moved.}
  @begin{short}
    Moves the cursor to the currently visible region of the buffer, if it is
    not there already.
  @end{short}
  @see-class{gtk-text-view}"
  (text-view (g-object gtk-text-view)))

(export 'gtk-text-view-place-cursor-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_visible_rect () -> gtk-text-view-visible-rect
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_visible_rect" %gtk-text-view-visible-rect)
    :void
  (text-view (g-object gtk-text-view))
  (visible-rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-text-view-visible-rect (text-view)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{A @class{gdk-rectangle} with the current visible region.}
  @begin{short}
    The currently visible region of the buffer, in buffer coordinates.
  @end{short}
  Convert to window coordinates with the function
  @fun{gtk-text-view-buffer-to-window-coords}.
  @see-class{gtk-text-view}
  @see-class{gdk-rectangle}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (let ((rect (gdk-rectangle-new)))
    (%gtk-text-view-visible-rect text-view rect)
    rect))

(export 'gtk-text-view-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_location () -> gtk-text-view-iter-location
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_location" %gtk-text-view-iter-location) :void
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (location (g-boxed-foreign gdk-rectangle)))

(defun gtk-text-view-iter-location (view iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-19}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @begin{return}
    A @class{gdk-rectangle} instance with the bounds of the character at
    @arg{iter}.
  @end{return}
  @begin{short}
    Gets a rectangle which roughly contains the coordinates of the character at
    the position of the iterator.
  @end{short}
  The rectangle position is in text buffer coordinates. Use the function
  @fun{gtk-text-view-buffer-to-window-coords} to convert these coordinates to
  coordinates for one of the windows in the text view.
  @see-class{gtk-text-view}
  @see-class{gtk-text-iter}
  @see-class{gdk-rectangle}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (let ((rect (gdk-rectangle-new)))
    (%gtk-text-view-iter-location view iter rect)
    rect))

(export 'gtk-text-view-iter-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_cursor_locations () -> gtk-text-view-cursor-locations
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_cursor_locations" %gtk-text-view-cursor-locations)
    :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (strong (g-boxed-foreign gdk-rectangle))
  (weak (g-boxed-foreign gdk-rectangle)))

(defun gtk-text-view-cursor-locations (text-view iter)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @begin{return}
    @code{strong} -- a @class{gdk-rectangle} with the strong cursor position
                     @br{}
    @code{weak}   -- a @class{gdk-rectangle} with the weak cursor position
  @end{return}
  @begin{short}
    Given an iter within a text layout, determine the positions of the strong
    and weak cursors if the insertion point is at that iterator.
  @end{short}
  The position of each cursor is stored as a zero-width rectangle. The strong
  cursor location is the location where characters of the directionality equal
  to the base direction of the paragraph are inserted. The weak cursor location
  is the location where characters of the directionality opposite to the base
  direction of the paragraph are inserted.

  If @arg{iter} is @code{nil}, the actual cursor position is used.

  Note that if @arg{iter} happens to be the actual cursor position, and there
  is currently an IM preedit sequence being entered, the returned locations
  will be adjusted to account for the preedit cursor's offset within the
  preedit sequence.

  The rectangle position is in buffer coordinates. Use the function
  @fun{gtk-text-view-buffer-to-window-coords} to convert these coordinates to
  coordinates for one of the windows in the text view.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (let ((strong (gdk-rectangle-new))
        (weak (gdk-rectangle-new)))
    (%gtk-text-view-cursor-locations text-view iter strong weak)
    (values strong weak)))

(export 'gtk-text-view-cursor-locations)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_at_y () -> gtk-text-view-line-at-y
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_line_at_y" %gtk-text-view-line-at-y) :void
  (text-view (g-object gtk-text-view))
  (target-iter (g-boxed-foreign gtk-text-iter))
  (y :int)
  (line-top (:pointer :int)))

(defun gtk-text-view-line-at-y (text-view y)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[y]{an integer with the y coordinate}
  @begin{return}
    @code{target-iter} -- a @class{gtk-text-iter} object @br{}
    @code{line-top} -- top coordinate of the line
  @end{return}
  @begin{short}
    Gets the @class{gtk-text-iter} at the start of the line containing the
    coordinate @arg{y}.
  @end{short}
  @arg{y} is in buffer coordinates, convert from window coordinates with the
  function @fun{gtk-text-view-window-to-buffer-coords}. If non-@code{nil},
  @arg{line-top} will be filled with the coordinate of the top edge of the line.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (with-foreign-object (line-top :int)
      (%gtk-text-view-line-at-y text-view iter y line-top)
      (values iter (mem-ref line-top :int)))))

(export 'gtk-text-view-line-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_yrange () -> gtk-text-view-line-yrange
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_line_yrange" %gtk-text-view-line-yrange) :void
  (text-view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (y (:pointer :int))
  (height (:pointer :int)))

(defun gtk-text-view-line-yrange (text-view iter)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @begin{return}
    @code{y} -- an integer with the y coordinate @br{}
    @code{height} -- an integer with the height
  @end{return}
  @begin{short}
    Gets the @arg{y} coordinate of the top of the line containing @arg{iter},
    and the @arg{height} of the line.
  @end{short}
  The coordinate is a buffer coordinate. Convert to window coordinates with the
  function @fun{gtk-text-view-buffer-to-window-coords}.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-buffer-to-window-coords}"
  (with-foreign-objects ((y :int) (height :int))
    (%gtk-text-view-line-yrange text-view iter y height)
    (values (mem-ref y :int)
            (mem-ref height :int))))

(export 'gtk-text-view-line-yrange)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_location () -> gtk-text-view-iter-at-location
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_at_location" %gtk-text-view-iter-at-location)
    :void
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (x :int)
  (y :int))

(defun gtk-text-view-iter-at-location (view buffer-x buffer-y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-19}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[buffer-x]{an integer with the x position, in text buffer
    coordinates}
  @argument[buffer-y]{an integer with the y position, in text buffer
    coordinates}
  @begin{short}
    Retrieves the iterator at text buffer coordinates
    (@arg{buffer-x} and @arg{buffer-y}).
  @end{short}
  Text buffer coordinates are coordinates for the entire text buffer, not just
  the currently displayed portion. If you have coordinates from an event, you
  have to convert those to text buffer coordinates with the function
  @fun{gtk-text-view-window-to-buffer-coords}.
  @see-class{gtk-text-view}
  @see-class{gtk-text-iter}
  @see-function{gtk-text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'gtk-text-iter)))
    (%gtk-text-view-iter-at-location view iter buffer-x buffer-y)
    iter))

(export 'gtk-text-view-iter-at-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_position () -> gtk-text-view-iter-at-positon
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_iter_at_position" %gtk-text-view-iter-at-position)
    :void
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter))
  (trailing (:pointer :int))
  (x :int)
  (y :int))

(defun gtk-text-view-iter-at-position (view x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-20}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[x]{an integer with the x position, in text buffer coordinates}
  @argument[y]{an integer with the y position, in text buffer coordinates}
  @begin{return}
    @arg{iter} -- a @class{gtk-text-iter} object @br{}
    @arg{trailing} -- if non-@code{nil}, an integer indicating where in the
    grapheme the user clicked, it will either be zero, or the number of
    characters in the grapheme, 0 represents the trailing edge of the grapheme
  @end{return}
  @begin{short}
    Retrieves the iterator pointing to the character at text buffer coordinates
    @arg{x} and @arg{y}.
  @end{short}
  Text buffer coordinates are coordinates for the entire text buffer, not just
  the currently displayed portion. If you have coordinates from an event, you
  have to convert those to text buffer coordinates with the function
  @fun{gtk-text-view-window-to-buffer-coords}.

  Note that this is different from the function
  @fun{gtk-text-view-iter-at-location}, which returns cursor locations,
  i.e. positions between characters.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-window-to-buffer-coords}
  @see-function{gtk-text-view-iter-at-location}"
  (with-foreign-object (trailing :int)
    (let ((iter (make-instance 'gtk-text-iter)))
      (%gtk-text-view-iter-at-position view iter trailing x y)
      (values iter (mem-ref trailing :int)))))

(export 'gtk-text-view-iter-at-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_buffer_to_window_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_buffer_to_window_coords"
          %gtk-text-view-buffer-to-window-coords) :void
  (view (g-object gtk-text-view))
  (win-type gtk-text-window-type)
  (buffer-x :int)
  (buffer-y :int)
  (window-x (:pointer :int))
  (window-y (:pointer :int)))

(defun gtk-text-view-buffer-to-window-coords (view win-type buffer-x buffer-y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-19}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[win-type]{a value of the @symbol{gtk-text-window-type} enumeration,
    except @code{:private}}
  @argument[buffer-x]{an integer with the text buffer x coordinate}
  @argument[buffer-y]{an integer with the text buffer y coordinate}
  @begin{return}
    @code{window-x} -- an integer with the window x coordinate @br{}
    @code{window-y} -- an integer with the window y coordinate
  @end{return}
  @begin{short}
    Converts the text buffer coordinates (@arg{buffer-x}, @arg{buffer-y}) to
    window coordinates (@arg{window-x}, @arg{window-y}) for the text view
    window of type @arg{win-type}.
  @end{short}

  Note that you cannot convert coordinates for a nonexisting window, see the
  function @fun{gtk-text-view-border-window-size}.
  @see-class{gtk-text-view}
  @see-symbol{gtk-text-window-type}
  @see-function{gtk-text-view-border-window-size}"
  (with-foreign-objects ((window-x :int) (window-y :int))
    (%gtk-text-view-buffer-to-window-coords view
                                            win-type
                                            buffer-x buffer-y
                                            window-x window-y)
    (values (mem-ref window-x :int)
            (mem-ref window-y :int))))

(export 'gtk-text-view-buffer-to-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_window_to_buffer_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_window_to_buffer_coords"
          %gtk-text-view-window-to-buffer-coords) :void
  (view (g-object gtk-text-view))
  (win-type gtk-text-window-type)
  (window-x :int)
  (window-y :int)
  (buffer-x :pointer)
  (buffer-y :pointer))

(defun gtk-text-view-window-to-buffer-coords (view win-type window-x window-y)
 #+cl-cffi-gtk-documentation
 "@version{2021-2-19}
  @argument[view]{a @class{gtk-text-view} widget}
  @argument[win-type]{a value of the @symbol{gtk-text-window-type} enumeration,
    except @code{:private}}
  @argument[window-x]{an integer with the window x coordinate}
  @argument[window-y]{an integer with the window y coordinate}
  @begin{return}
    @code{buffer-x} -- an integer with the text buffer x coordinate or @br{}
    @code{buffer-y} -- an integer with the text buffer y coordinate
  @end{return}
  @begin{short}
    Converts window coordinates (@arg{window-x}, @arg{window-y}) on the text
    view window identified by @arg{win-type} to text buffer coordinates
    (@arg{buffer-x}, @arg{buffer-y}).
  @end{short}

  Note that you cannot convert coordinates for a nonexisting window, see the
  function @fun{gtk-text-view-border-window-size}.
  @see-class{gtk-text-view}
  @see-symbol{gkt-text-view-window-type}
  @see-function{gtk-text-view-border-window-size}"
  (with-foreign-objects ((buffer-x :int) (buffer-y :int))
    (%gtk-text-view-window-to-buffer-coords view
                                            win-type
                                            window-x window-y
                                            buffer-x buffer-y)
    (values (mem-ref buffer-x :int)
            (mem-ref buffer-y :int))))

(export 'gtk-text-view-window-to-buffer-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_window () -> gtk-text-view-window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_window" gtk-text-view-window) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
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

(export 'gtk-text-view-window)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_window_type () -> gtk-text-view-window-type
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_window_type" gtk-text-view-window-type)
    gtk-text-window-type
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[window]{a @class{gdk-window} object}
  @return{A value of the @symbol{gtk-text-window-type} enumeration.}
  @begin{short}
    Usually used to find out which window an event corresponds to.
  @end{short}
  If you connect to an event signal on the text view, this function should be
  called on @code{event->window} to see which window it was.
  @see-class{gtk-text-view}
  @see-symbol{gtk-text-window-type}"
  (text-view (g-object gtk-text-view))
  (window (g-object gdk-window)))

(export 'gtk-text-view-window-type)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_set_border_window_size ()
;;; gtk_text_view_get_border_window_size () -> gtk-text-view-border-window-size
;;; ----------------------------------------------------------------------------

(defun (setf gtk-text-view-border-window-size) (size text-view window-type)
  (foreign-funcall "gtk_text_view_set_border_window_size"
                   (g-object gtk-text-view) text-view
                   gtk-text-window-type window-type
                   :int size
                   :void)
  size)

(defcfun ("gtk_text_view_get_border_window_size"
           gtk-text-view-border-window-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @syntax[]{(gtk-text-view-border-window-size text-view window-type) => size}
  @syntax[]{(setf (gtk-text-view-border-window-size text-view window-type) size)}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[window-type]{a value of the @symbol{gtk-text-window-type}
    enumeration}
  @argument[size]{an integer with the width or height of the window}
  @begin{short}
    Accessor of the border window size of the text view.
  @end{short}

  The function @sym{gtk-text-view-border-window-size} gets the width of the
  specified border window. The function
  @sym{(setf gtk-text-view-border-window-size)} sets the width of @code{:left}
  or @code{:right}, or the height of @code{:top} or @code{:bottom}.

  Automatically destroys the corresponding window if the size is set to 0, and
  creates the window if the size is set to non-zero. This function can only be
  used for the \"border windows\", it does not work with @code{:widget},
  @code{:text}, or @code{:private}.
  @see-class{gtk-text-view}"
  (text-view (g-object gtk-text-view))
  (window-type gtk-text-window-type))

(export 'gtk-text-view-border-window-size)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_forward_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_forward_display_line"
          gtk-text-view-forward-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  @begin{short}
    Moves the given @arg{iter} forward by one display (wrapped) line.
  @end{short}
  A display line is different from a paragraph. Paragraphs are separated by
  newlines or other paragraph separator characters. Display lines are created
  by line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width. Paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}.
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-forward-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_backward_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_backward_display_line"
          gtk-text-view-backward-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  @begin{short}
    Moves the given @arg{iter} backward by one display (wrapped) line.
  @end{short}
  A display line is different from a paragraph. Paragraphs are separated by
  newlines or other paragraph separator characters. Display lines are created
  by line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width. Paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}.
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-backward-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_forward_display_line_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_forward_display_line_end"
          gtk-text-view-forward-display-line-end) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  @begin{short}
    Moves the given @arg{iter} forward to the next display line end.
  @end{short}
  A display line is different from a paragraph. Paragraphs are separated by
  newlines or other paragraph separator characters. Display lines are created
  by line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width. Paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}.
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-forward-display-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_backward_display_line_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_backward_display_line_start"
          gtk-text-view-backward-display-line-start) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  @begin{short}
    Moves the given @arg{iter} backward to the next display line start.
  @end{short}
  A display line is different from a paragraph. Paragraphs are separated by
  newlines or other paragraph separator characters. Display lines are created
  by line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the view's width. Paragraphs are the same in all
  views, since they depend on the contents of the @class{gtk-text-buffer}.
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-backward-display-line-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_starts_display_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_starts_display_line"
           gtk-text-view-starts-display-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @return{@em{True} if @arg{iter} begins a wrapped line.}
  @begin{short}
    Determines whether @arg{iter} is at the start of a display line.
  @end{short}
  See the function @fun{gtk-text-view-forward-display-line} for an explanation
  of display lines vs. paragraphs.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-forward-display-line}"
  (view (g-object gtk-text-view))
  (iter (g-boxed-foreign gtk-text-iter)))

(export 'gtk-text-view-starts-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_visually ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_move_visually" gtk-text-view-move-visually) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[iter]{a @class{gtk-text-iter} object}
  @argument[count]{number of characters to move (negative moves left, positive
    moves right)}
  @return{@em{True} if @arg{iter} moved and is not on the end iterator.}
  @begin{short}
    Move the iterator a given number of characters visually, treating it as the
    strong cursor position.
  @end{short}
  If @arg{count} is positive, then the new strong cursor position will be
  @arg{count} positions to the right of the old cursor position. If @arg{count}
  is negative then the new strong cursor position will be @arg{count} positions
  to the left of the old cursor position.

  In the presence of bi-directional text, the correspondence between logical
  and visual order will depend on the direction of the current run, and there
  may be jumps when the cursor is moved off of the end of a run.
  @see-class{gtk-text-view}"
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
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[child]{a @class{gtk-widget} object}
  @argument[anchor]{a @class{gtk-text-child-anchor} in the
    @class{gtk-text-buffer} for @arg{text-view}}
  @begin{short}
    Adds a child widget in the text buffer, at the given anchor.
  @end{short}
  @see-class{gtk-text-view}"
  (view (g-object gtk-text-view))
  (child (g-object gtk-widget))
  (anchor (g-object gtk-text-child-anchor)))

(export 'gtk-text-view-add-child-at-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-text-child-anchor-new))

(defun gtk-text-child-anchor-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
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
;;; gtk_text_child_anchor_get_widgets () -> gtk-text-child-anchor-widgets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_child_anchor_get_widgets" gtk-text-child-anchor-widgets)
    (g-list (g-object gtk-widget) :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[anchor]{a @class{gtk-text-child-anchor} object}
  @return{List of widgets anchored at @arg{anchor}.}
  @begin{short}
    Gets a list of all widgets anchored at this child @arg{anchor}.
  @end{short}
  @see-class{gtk-text-child-anchor}"
  (anchor (g-object gtk-text-child-anchor)))

(export 'gtk-text-child-anchor-widgets)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_get_deleted () -> gtk-text-view-anchor-deleted
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_child_anchor_get_deleted" gtk-text-child-anchor-deleted)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[anchor]{a @class{gtk-text-child-anchor} object}
  @return{@em{True} if the child anchor has been deleted from its buffer.}
  @begin{short}
    Determines whether a child anchor has been deleted from the buffer.
  @end{short}
  Keep in mind that the child anchor will be unreferenced when removed from the
  buffer, so you need to hold your own reference (with @code{g_object_ref()})
  if you plan to use this function - otherwise all deleted child anchors will
  also be finalized.
  @see-class{gtk-text-child-anchor}"
  (anchor (g-object gtk-text-child-anchor)))

(export 'gtk-text-child-anchor-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_add_child_in_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_add_child_in_window" gtk-text-view-add-child-in-window)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[child]{a @class{gtk-widget} object}
  @argument[which-window]{which window the child should appear in}
  @argument[xpos]{an integer with the x position of child in window coordinates}
  @argument[ypos]{an integer with the y position of child in window coordinates}
  @begin{short}
    Adds a child at fixed coordinates in one of the text widget's windows.
  @end{short}

  The window must have nonzero size, see the function
  @fun{gtk-text-view-border-window-size}). Note that the child coordinates
  are given relative to the @class{gdk-window} object in question, and that
  these coordinates have no sane relationship to scrolling. When placing a child
  in @code{:widget}, scrolling is irrelevant, the child floats above all
  scrollable areas. But when placing a child in one of the scrollable windows,
  border windows or text window, you will need to compute the child's correct
  position in buffer coordinates any time scrolling occurs or buffer changes
  occur, and then call the function @fun{gtk-text-view-move-child} to update the
  child's position.
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-border-window-size}
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
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[child]{child widget already added to the text view}
  @argument[xpos]{integer with the x position in window coordinates}
  @argument[ypos]{integer with the y position in window coordinates}
  @begin{short}
    Updates the position of a child, as for the function
    @fun{gtk-text-view-add-child-in-window}.
  @end{short}
  @see-class{gtk-text-view}
  @see-function{gtk-text-view-add-child-in-window}"
  (view (g-object gtk-text-view))
  (child (g-object gtk-widget))
  (x-pos :int)
  (y-pos :int))

(export 'gtk-text-view-move-child)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_cursor_blink ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_reset_cursor_blink" gtk-text-view-reset-cursor-blink)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{short}
    Ensures that the cursor is shown, i.e. not in an 'off' blink interval, and
    resets the time that it will stay blinking, or visible, in case blinking is
    disabled.
  @end{short}

  This function should be called in response to user input, e.g. from derived
  classes that override the textview's \"key-press-event\" handler.

  Since 3.20
  @see-class{gtk-text-view}"
  (text-view (g-object gtk-text-view)))

(export 'gtk-text-view-reset-cursor-blink)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_default_attributes () -> gtk-text-view-default-attributes
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_get_default_attributes"
           gtk-text-view-default-attributes)
    (g-boxed-foreign gtk-text-attributes)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @return{A @class{gtk-text-attributes} structure.}
  @begin{short}
    Obtains the default text attributes.
  @end{short}
  These are the attributes used for text unless a tag overrides them. You'd
  typically pass the default attributes in to the function
  @fun{gtk-text-iter-attributes} in order to get the attributes in effect at a
  given text position.
  @see-class{gtk-text-view}
  @see-class{gtk-text-attributes}
  @see-function{gtk-text-iter-attributes}"
  (text-view (g-object gtk-text-view)))

(export 'gtk-text-view-default-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_im_context_filter_keypress ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_im_context_filter_keypress"
           gtk-text-view-im-context-filter-keypress) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @argument[event]{a @class{gdk-event} key event}
  @return{@em{True} if the input method handled the key event.}
  @begin{short}
    Allow the text view input method to internally handle key press and
    release events.
  @end{short}
  If this function returns @em{true}, then no further processing should be done
  for this key event. See the function @fun{gtk-im-context-filter-keypress}.

  Note that you are expected to call this function from your handler when
  overriding key event handling. This is needed in the case when you need to
  insert your own key handling between the input method and the default key
  event handling of the text view.
  @begin{pre}
 static gboolean
 gtk_foo_bar_key_press_event (GtkWidget   *widget,
                              GdkEventKey *event)
 {
   if ((key->keyval == GDK_KEY_Return || key->keyval == GDK_KEY_KP_Enter))
     {
       if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (view),
                                                     event))
         return TRUE;
     @}

     /* Do some stuff */

   return GTK_WIDGET_CLASS (gtk_foo_bar_parent_class)
                                           ->key_press_event (widget, event);
 @}
  @end{pre}
  @see-class{gtk-text-view}
  @see-function{gtk-im-context-filter-keypress}"
  (text-view (g-object gtk-text-view))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-text-view-im-context-filter-keypress)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_im_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_view_reset_im_context" gtk-text-view-reset-im-context) :void

 #+cl-cffi-gtk-documentation
 "@version{2020-6-7}
  @argument[text-view]{a @class{gtk-text-view} widget}
  @begin{short}
    Reset the input method context of the text view if needed.
  @end{short}

  This can be necessary in the case where modifying the buffer would confuse
  on-going input method behavior.
  @see-class{gtk-text-view}"
  (text-view (g-object gtk-text-view)))

(export 'gtk-text-view-reset-im-context)

;;; --- End of file gtk.text-view.lisp -----------------------------------------
