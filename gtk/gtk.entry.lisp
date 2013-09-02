;;; ----------------------------------------------------------------------------
;;; gtk.entry.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkEntry
;;;
;;; A single line text entry field
;;;
;;; Synopsis
;;;
;;;     GtkEntry
;;;
;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_get_buffer
;;;     gtk_entry_set_buffer
;;;     gtk_entry_set_text
;;;     gtk_entry_get_text
;;;     gtk_entry_get_text_length
;;;     gtk_entry_get_text_area
;;;     gtk_entry_set_visibility
;;;     gtk_entry_set_invisible_char
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_max_length
;;;     gtk_entry_get_activates_default
;;;     gtk_entry_get_has_frame
;;;     gtk_entry_get_inner_border
;;;     gtk_entry_get_width_chars
;;;     gtk_entry_set_activates_default
;;;     gtk_entry_set_has_frame
;;;     gtk_entry_set_inner_border
;;;     gtk_entry_set_width_chars
;;;     gtk_entry_get_invisible_char
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_set_placeholder_text
;;;     gtk_entry_get_placeholder_text
;;;     gtk_entry_set_overwrite_mode
;;;     gtk_entry_get_overwrite_mode
;;;     gtk_entry_get_layout
;;;     gtk_entry_get_layout_offsets
;;;     gtk_entry_layout_index_to_text_index
;;;     gtk_entry_text_index_to_layout_index
;;;     gtk_entry_set_attributes
;;;     gtk_entry_get_attributes
;;;     gtk_entry_get_max_length
;;;     gtk_entry_get_visibility
;;;     gtk_entry_set_completion
;;;     gtk_entry_get_completion
;;;     gtk_entry_set_cursor_hadjustment
;;;     gtk_entry_get_cursor_hadjustment
;;;     gtk_entry_set_progress_fraction
;;;     gtk_entry_get_progress_fraction
;;;     gtk_entry_set_progress_pulse_step
;;;     gtk_entry_get_progress_pulse_step
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_im_context_filter_keypress
;;;     gtk_entry_reset_im_context
;;;
;;;     GtkEntryIconPosition
;;;
;;;     gtk_entry_set_icon_from_pixbuf
;;;     gtk_entry_set_icon_from_stock
;;;     gtk_entry_set_icon_from_icon_name
;;;     gtk_entry_set_icon_from_gicon
;;;     gtk_entry_get_icon_storage_type
;;;     gtk_entry_get_icon_pixbuf
;;;     gtk_entry_get_icon_stock
;;;     gtk_entry_get_icon_name
;;;     gtk_entry_get_icon_gicon
;;;     gtk_entry_set_icon_activatable
;;;     gtk_entry_get_icon_activatable
;;;     gtk_entry_set_icon_sensitive
;;;     gtk_entry_get_icon_sensitive
;;;     gtk_entry_get_icon_at_pos
;;;     gtk_entry_set_icon_tooltip_text
;;;     gtk_entry_get_icon_tooltip_text
;;;     gtk_entry_set_icon_tooltip_markup
;;;     gtk_entry_get_icon_tooltip_markup
;;;     gtk_entry_set_icon_drag_source
;;;     gtk_entry_get_current_icon_drag_source
;;;     gtk_entry_get_icon_area
;;;
;;;     GtkInputPurpose
;;;
;;;     gtk_entry_set_input_purpose
;;;     gtk_entry_get_input_purpose
;;;
;;;     GtkInputHints
;;;
;;;     gtk_entry_set_input_hints
;;;     gtk_entry_get_input_hints
;;;
;;; Style Properties
;;;
;;;   "icon-prelight"             gboolean            : Read
;;;   "inner-border"              GtkBorder*          : Read
;;;   "invisible-char"            guint               : Read
;;;   "progress-border"           GtkBorder*          : Read
;;;
;;; Signals
;;;
;;;   "activate"                                      : Action
;;;   "backspace"                                     : Action
;;;   "copy-clipboard"                                : Action
;;;   "cut-clipboard"                                 : Action
;;;   "delete-from-cursor"                            : Action
;;;   "icon-press"                                    : Run Last
;;;   "icon-release"                                  : Run Last
;;;   "insert-at-cursor"                              : Action
;;;   "move-cursor"                                   : Action
;;;   "paste-clipboard"                               : Action
;;;   "populate-popup"                                : Run Last
;;;   "preedit-changed"                               : Action
;;;   "toggle-overwrite"                              : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEntry
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEntry" gtk-entry
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkEditable"
                "GtkCellEditable")
   :type-initializer "gtk_entry_get_type")
  ((activates-default
    gtk-entry-activates-default
    "activates-default" "gboolean" t t)
   (attributes
    gtk-entry-attributes
    "attributes" "PangoAttrlist" t t)
   (buffer
    gtk-entry-buffer
    "buffer" "GtkEntryBuffer" t t)
   (caps-lock-warning
    gtk-entry-caps-lock-warning
    "caps-lock-warning" "gboolean" t t)
   (completion
    gtk-entry-completion
    "completion" "GtkEntryCompletion" t t)
   (cursor-position
    gtk-entry-cursor-position
    "cursor-position" "gint" t nil)
   (editable
    gtk-entry-editable
    "editable" "gboolean" t t)
   (has-frame
    gtk-entry-has-frame
    "has-frame" "gboolean" t t)
   (im-module
    gtk-entry-im-module
    "im-module" "gchararray" t t)
   (inner-border
    gtk-entry-inner-border
    "inner-border" "GtkBorder" t t)
   (input-hints
    gtk-entry-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    gtk-entry-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (invisible-char
    gtk-entry-invisible-char
    "invisible-char" "guint" t t)
   (invisible-char-set
    gtk-entry-invisible-char-set
    "invisible-char-set" "gboolean" t t)
   (max-length
    gtk-entry-max-length
    "max-length" "gint" t t)
   (overwrite-mode
    gtk-entry-overwrite-mode
    "overwrite-mode" "gboolean" t t)
   (placeholder-text
    gtk-entry-placeholder-text
    "placeholder-text" "gchar" t t)
   (primary-icon-activatable
    gtk-entry-primary-icon-activatable
    "primary-icon-activatable" "gboolean" t t)
   (primary-icon-gicon
    gtk-entry-primary-icon-gicon
    "primary-icon-gicon" "GIcon" t t)
   (primary-icon-name
    gtk-entry-primary-icon-name
    "primary-icon-name" "gchararray" t t)
   (primary-icon-pixbuf
    gtk-entry-primary-icon-pixbuf
    "primary-icon-pixbuf" "GdkPixbuf" t t)
   (primary-icon-sensitive
    gtk-entry-primary-icon-sensitive
    "primary-icon-sensitive" "gboolean" t t)
   (primary-icon-stock
    gtk-entry-primary-icon-stock
    "primary-icon-stock" "gchararray" t t)
   (primary-icon-storage-type
    gtk-entry-primary-icon-storage-type
    "primary-icon-storage-type" "GtkImageType" t nil)
   (primary-icon-tooltip-markup
    gtk-entry-primary-icon-tooltip-markup
    "primary-icon-tooltip-markup" "gchararray" t t)
   (primary-icon-tooltip-text
    gtk-entry-primary-icon-tooltip-text
    "primary-icon-tooltip-text" "gchararray" t t)
   (progress-fraction
    gtk-entry-progress-fraction
    "progress-fraction" "gdouble" t t)
   (progress-pulse-step
    gtk-entry-progress-pulse-step
    "progress-pulse-step" "gdouble" t t)
   (scroll-offset
    gtk-entry-scroll-offset
    "scroll-offset" "gint" t nil)
   (secondary-icon-activatable
    gtk-entry-secondary-icon-activatable
    "secondary-icon-activatable" "gboolean" t t)
   (secondary-icon-gicon
    gtk-entry-secondary-icon-gicon
    "secondary-icon-gicon" "GIcon" t t)
   (secondary-icon-name
    gtk-entry-secondary-icon-name
    "secondary-icon-name" "gchararray" t t)
   (secondary-icon-pixbuf
    gtk-entry-secondary-icon-pixbuf
    "secondary-icon-pixbuf" "GdkPixbuf" t t)
   (secondary-icon-sensitive
    gtk-entry-secondary-icon-sensitive
    "secondary-icon-sensitive" "gboolean" t t)
   (secondary-icon-stock
    gtk-entry-secondary-icon-stock
    "secondary-icon-stock" "gchararray" t t)
   (secondary-icon-storage-type
    gtk-entry-secondary-icon-storage-type
    "secondary-icon-storage-type" "GtkImageType" t nil)
   (secondary-icon-tooltip-markup
    gtk-entry-secondary-icon-tooltip-markup
    "secondary-icon-tooltip-markup" "gchararray" t t)
   (secondary-icon-tooltip-text
    gtk-entry-secondary-icon-tooltip-text
    "secondary-icon-tooltip-text" "gchararray" t t)
   (selection-bound
    gtk-entry-selection-bound
    "selection-bound" "gint" t nil)
   (shadow-type
    gtk-entry-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (text
    gtk-entry-text
    "text" "gchararray" t t)
   (text-length
    gtk-entry-text-length
    "text-length" "guint" t nil)
   (truncate-multiline
    gtk-entry-truncate-multiline
    "truncate-multiline" "gboolean" t t)
   (visibility
    gtk-entry-visibility
    "visibility" "gboolean" t t)
   (width-chars
    gtk-entry-width-chars
    "width-chars" "gint" t t)
   (xalign
    gtk-entry-xalign
    "xalign" "gfloat" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-entry 'type)
 "@version{2013-4-27}
  @begin{short}
    The @sym{gtk-entry} widget is a single line text entry widget. A fairly
    large set of key bindings are supported by default. If the entered text is
    longer than the allocation of the widget, the widget will scroll so that the
    cursor position is visible.
  @end{short}

  When using an entry for passwords and other sensitive information, it can be
  put into \"password mode\" using the function @fun{gtk-entry-set-visibility}.
  In this mode, entered  text is displayed using a 'invisible' character. By
  default, GTK+ picks the best invisible character that is available in the
  current font, but it can be changed with the function
  @fun{gtk-entry-set-invisible-char}. Since 2.16, GTK+ displays a warning when
  Caps Lock or input methods might interfere with entering text in a password
  entry. The warning can be turned off with the \"caps-lock-warning\" property.

  Since 2.16, @sym{gtk-entry} has the ability to display progress or activity
  information behind the text. To make an entry display such information, use
  the functions @fun{gtk-entry-set-progress-fraction} or
  @fun{gtk-entry-set-progress-pulse-step}.

  Additionally, @sym{gtk-entry} can show icons at either side of the entry.
  These icons can be activatable by clicking, can be set up as drag source and
  can have tooltips. To add an icon, use the function
  @fun{gtk-entry-set-icon-from-gicon} or one of the various other functions that
  set an icon from a stock id, an icon name or a pixbuf. To trigger an action
  when the user clicks an icon, connect to the \"icon-press\" signal. To allow
  DND operations from an icon, use the function
  @fun{gtk-entry-set-icon-drag-source}. To set a tooltip on an icon, use the
  function @fun{gtk-entry-set-icon-tooltip-text} or the corresponding function
  for markup.

  Note that functionality or information that is only available by clicking on
  an icon in an entry may not be accessible at all to users which are not able
  to use a mouse or other pointing device. It is therefore recommended that
  any such functionality should also be available by other means, e. g. via the
  context menu of the entry.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"icon-prelight\" style property}
      @code{\"icon-prelight\"} of type @code{:boolean} (Read)@br{}
      The prelight style property determines whether activatable icons prelight
      on mouseover.@br{}
      Default value: @em{true}@br{}
      Since 2.16

    @subheading{The \"inner-border\" style property}
      @code{\"inner-border\"} of type @class{gtk-border} (Read)@br{}
      @b{Warning:}
      The @code{\"inner-border\"} style property has been deprecated since
      version 3.4 and should not be used in newly written code. Use the standard
      border and padding CSS properties (through objects like
      @class{gtk-style-context} and @class{gtk-css-provider}); the value of this
      style property is ignored.@br{}
      Sets the text area's border between the text and the frame.@br{}
      Since 2.10

    @subheading{The \"invisible-char\" style property}
      @code{\"invisible-char\"} of type @code{:uint} (Read)@br{}
      The invisible character is used when masking entry contents (in \"password
      mode\"). When it is not explicitly set with the \"invisible-char\"
      property, GTK+ determines the character to use from a list of possible
      candidates, depending on availability in the current font.
      This style property allows the theme to prepend a character to the list of
      candidates.@br{}
      Default value: 0@br{}
      Since 2.18

    @subheading{The \"progress-border\" style property}
      @code{\"progress-border\"} of type @class{gtk-border} (Read)@br{}
      @b{Warning:}
      The @code{\"progress-border\"} style property has been deprecated since
      version 3.4 and should not be used in newly written code. Use the standard
      margin CSS property (through objects like @class{gtk-style-context} and
      @class{gtk-css-provider}); the value of this style property is
      ignored.@br{}
      The border around the progress bar in the entry.@br{}
      Since 2.16
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user activates the entry.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control activation
      programmatically.
      The default bindings for this signal are all forms of the Enter key.
      @begin[code]{table}
        @entry[entry]{The entry on which the signal is emitted.}
      @end{table}
    @subheading{The \"backspace\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      The \"backspace\" signal is a keybinding signal which gets emitted when
      the user asks for it.
      The default bindings for this signal are Backspace and Shift-Backspace.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      The \"copy-clipboard\" signal is a keybinding signal which gets emitted to
      copy the selection to the clipboard.
      The default bindings for this signal are Ctrl-c and Ctrl-Insert.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      The \"cut-clipboard\" signal is a keybinding signal which gets emitted to
      cut the selection to the clipboard.
      The default bindings for this signal are Ctrl-x and Shift-Delete.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
 lambda (entry type count)   : Action
      @end{pre}
      The \"delete-from-cursor\" signal is a keybinding signal which gets
      emitted when the user initiates a text deletion.
      If the type is @code{:chars} of the @symbol{gtk-delete-type} enumeration,
      GTK+ deletes the selection if there is one, otherwise it deletes the
      requested number of characters.
      The default bindings for this signal are Delete for deleting a character
      and Ctrl-Delete for deleting a word.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
        @entry[type]{The granularity of the deletion, as a
          @symbol{gtk-delete-type} enumeration.}
        @entry[count]{The number of type units to delete.}
      @end{table}
    @subheading{The \"icon-press\" signal}
      @begin{pre}
 lambda (entry icon-pos event)   : Run Last
      @end{pre}
      The \"icon-press\" signal is emitted when an activatable icon is clicked.
      @begin[code]{table}
        @entry[entry]{The entry on which the signal is emitted.}
        @entry[icon-pos]{The position of the clicked icon.}
        @entry[event]{The button press event.}
      @end{table}
      Since 2.16

    @subheading{The \"icon-release\" signal}
      @begin{pre}
 lambda (entry icon-pos event)   : Run Last
      @end{pre}
      The \"icon-release\" signal is emitted on the button release from a mouse
      click over an activatable icon.
      @begin[code]{table}
        @entry[entry]{The entry on which the signal is emitted.}
        @entry[icon-pos]{The position of the clicked icon.}
        @entry[event]{The button release event.}
      @end{table}
      Since 2.16

    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
 lambda (entry string)   : Action
      @end{pre}
      The \"insert-at-cursor\" signal is a keybinding signal which gets emitted
      when the user initiates the insertion of a fixed string at the cursor.
      This signal has no default bindings.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (entry step count extend-selection)   : Action
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement. If the cursor is not visible in
      entry, this signal causes the viewport to be moved instead.
      Applications should not connect to it, but may emit it with the function
      @fun{g-signal-emit-by-name} if they need to control the cursor
      programmatically.
      The default bindings for this signal come in two variants, the variant
      with the Shift modifier extends the selection, the variant without the
      Shift modifer does not. There are too many key combinations to list them
      all here.
      @begin{itemize}
        @item{Arrow keys move by individual characters/lines.}
        @item{Ctrl-arrow key combinations move by words/paragraphs.}
        @item{Home/End keys move to the ends of the buffer.}
      @end{itemize}
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
        @entry[step]{The granularity of the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
        @entry[extend-selection]{@em{True} if the move should extend the
          selection.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      The \"paste-clipboard\" signal is a keybinding signal which gets emitted
      to paste the contents of the clipboard into the text view.
      The default bindings for this signal are Ctrl-v and Shift-Insert.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (entry menu)   : Run Last
      @end{pre}
      The \"populate-popup\" signal gets emitted before showing the context menu
      of the entry.
      If you need to add items to the context menu, connect to this signal and
      append your menuitems to the menu.
      @begin[code]{table}
        @entry[entry]{The entry on which the signal is emitted.}
        @entry[menu]{The menu that is being populated.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
 lambda (entry preedit)   : Action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
        @entry[preedit]{The current preedit string.}
      @end{table}
      Since 2.20

    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
 lambda (entry)   : Action
      @end{pre}
      The \"toggle-overwrite\" signal is a keybinding signal which gets emitted
      to toggle the overwrite mode of the entry.
      The default bindings for this signal is Insert.
      @begin[code]{table}
        @entry[entry]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-entry-activates-default}
  @see-slot{gtk-entry-attributes}
  @see-slot{gtk-entry-buffer}
  @see-slot{gtk-entry-caps-lock-warning}
  @see-slot{gtk-entry-completion}
  @see-slot{gtk-entry-cursor-position}
  @see-slot{gtk-entry-editable}
  @see-slot{gtk-entry-has-frame}
  @see-slot{gtk-entry-im-module}
  @see-slot{gtk-entry-inner-border}
  @see-slot{gtk-entry-input-hints}
  @see-slot{gtk-entry-input-purpose}
  @see-slot{gtk-entry-invisible-char}
  @see-slot{gtk-entry-invisible-char-set}
  @see-slot{gtk-entry-max-length}
  @see-slot{gtk-entry-overwrite-mode}
  @see-slot{gtk-entry-placeholder-text}
  @see-slot{gtk-entry-primary-icon-activatable}
  @see-slot{gtk-entry-primary-icon-gicon}
  @see-slot{gtk-entry-primary-icon-name}
  @see-slot{gtk-entry-primary-icon-pixbuf}
  @see-slot{gtk-entry-primary-icon-sensitive}
  @see-slot{gtk-entry-primary-icon-stock}
  @see-slot{gtk-entry-primary-icon-storage-type}
  @see-slot{gtk-entry-primary-icon-tooltip-markup}
  @see-slot{gtk-entry-primary-icon-tooltip-text}
  @see-slot{gtk-entry-progress-fraction}
  @see-slot{gtk-entry-progress-pulse-step}
  @see-slot{gtk-entry-scroll-offset}
  @see-slot{gtk-entry-secondary-icon-activatable}
  @see-slot{gtk-entry-secondary-icon-gicon}
  @see-slot{gtk-entry-secondary-icon-name}
  @see-slot{gtk-entry-secondary-icon-pixbuf}
  @see-slot{gtk-entry-secondary-icon-sensitive}
  @see-slot{gtk-entry-secondary-icon-stock}
  @see-slot{gtk-entry-secondary-icon-storage-type}
  @see-slot{gtk-entry-secondary-icon-tooltip-markup}
  @see-slot{gtk-entry-secondary-icon-tooltip-text}
  @see-slot{gtk-entry-selection-bound}
  @see-slot{gtk-entry-shadow-type}
  @see-slot{gtk-entry-text}
  @see-slot{gtk-entry-text-length}
  @see-slot{gtk-entry-truncate-multiline}
  @see-slot{gtk-entry-visibility}
  @see-slot{gtk-entry-width-chars}
  @see-slot{gtk-entry-xalign}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activates-default"
                                               'gtk-entry) 't)
 "The @code{\"activates-default\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to activate the default widget (such as the default button in a
  dialog) when Enter is pressed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attributes" 'gtk-entry) t)
 "The @code{\"attributes\"} property of type @class{pango-attr-list}
  (Read / Write) @br{}
  A list of Pango attributes to apply to the text of the entry.
  This is mainly useful to change the size or weight of the text. @br{}
  Since 3.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buffer" 'gtk-entry) 't)
 "The @code{\"buffer\"} property of type @class{gtk-entry-buffer}
  (Read / Write / Construct) @br{}
  Text buffer object which actually stores entry text.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "caps-lock-warning"
                                               'gtk-entry) 't)
 "The @code{\"caps-lock-warning\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether password entries will show a warning when Caps Lock is on. Note that
  the warning is shown using a secondary icon, and thus does not work if you
  are using the secondary icon position for some other purpose. @br{}
  Default value: @em{true} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "completion" 'gtk-entry) 't)
 "The @code{\"completion\"} property of type @class{gtk-entry-completion}
  (Read / Write) @br{}
  The auxiliary completion object to use with the entry. @br{}
  Since 3.2")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-position" 'gtk-entry) 't)
 "The @code{\"cursor-position\"} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable" 'gtk-entry) 't)
 "The @code{\"editable\"} property of type @code{:boolean} (Read / Write) @br{}
  Whether the entry contents can be edited. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-frame" 'gtk-entry) 't)
 "The @code{\"has-frame\"} property of type @code{:boolean} (Read / Write) @br{}
  @code{Nil} removes outside bevel from entry. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "im-module" 'gtk-entry) 't)
 "The @code{\"im-module\"} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used for this entry. See
  @class{gtk-im-context}.
  Setting this to a non-@code{nil} value overrides the system-wide IM module
  setting. See the @class{gtk-settings} @code{\"gtk-im-module\"} property. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inner-border" 'gtk-entry) 't)
 "The @code{\"inner-border\"} property of type @class{gtk-border}
  (Read / Write) @br{}
  @b{Warning:}
  The @code{\"inner-border\"} property has been deprecated since version 3.4 and
  should not be used in newly written code. Use the standard border and padding
  CSS properties through objects like @class{gtk-style-context} and
  @class{gtk-css-provider}; the value of this style property is ignored. @br{}
  Sets the text area's border between the text and the frame. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-hints" 'gtk-entry) t)
 "The @code{\"input-hints\"} property of type @symbol{gtk-input-hints}
  (Read / Write) @br{}
  Additional hints (beyond the @code{\"input-purpose\"} property) that allow
  input methods to fine-tune their behaviour. @br{}
  Since 3.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "input-purpose" 'gtk-entry) t)
 "The @code{\"input-purpose\"} property of type @symbol{gtk-input-purpose}
  (Read / Write) @br{}
  The purpose of this text field.
  This property can be used by on-screen keyboards and other input methods to
  adjust their behaviour.
  Note that setting the purpose to @code{:password} or @code{:pin} is
  independent from setting the @code{\"visibility\"} property. @br{}
  Default value: @code{:free-form} @br{}
  Since 3.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible-char" 'gtk-entry) 't)
 "The @code{\"invisible-char\"} property of type @code{:uint}
  (Read / Write) @br{}
  The invisible character is used when masking entry contents in \"password
  mode\". When it is not explicitly set with the @code{\"invisible-char\"}
  property, GTK+ determines the character to use from a list of possible
  candidates, depending on availability in the current font.
  This style property allows the theme to prepend a character to the list of
  candidates. @br{}
  Default value: \"*\" @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible-char-set"
                                               'gtk-entry) 't)
 "The @code{\"invisible-char-set\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the invisible char has been set for the @sym{gtk-entry}. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-length" 'gtk-entry) 't)
 "The @code{\"max-length\"} property of type @code{:int} (Read / Write) @br{}
  Maximum number of characters for this entry. Zero if no maximum. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "overwrite-mode" 'gtk-entry) 't)
 "The @code{\"overwrite-mode\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If text is overwritten when typing in the @sym{gtk-entry}. @br{}
  Default value: @code{nil} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "placeholder-text"
                                               'gtk-entry) 't)
 "The @code{\"placeholder-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The text that will be displayed in the @class{gtk-entry} when it is empty and
  unfocused. @br{}
  Default value: @code{nil} @br{}
  Since 3.2")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-activatable"
                                                'gtk-entry) 't)
 "The @code{\"primary-icon-activatable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is activatable.
  GTK+ emits the \"icon-press\" and \"icon-release\" signals only on sensitive,
  activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-gicon"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-gicon\"} property of type @class{g-icon}
  (Read / Write) @br{}
  The @class{g-icon} to use for the primary icon for the entry. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-name"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-name\"} property of type @code{:string}
  (Read / Write) @br{}
  The icon name to use for the primary icon for the entry. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-pixbuf"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-pixbuf\"} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  A pixbuf to use as the primary icon for the entry. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-sensitive"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-sensitive\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is sensitive.
  An insensitive icon appears grayed out. GTK+ does not emit the \"icon-press\"
  and \"icon-release\" signals and does not allow DND from insensitive
  icons.
  An icon should be set insensitive if the action that would trigger when
  clicked is currently not available. @br{}
  Default value: @em{true} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-stock"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-stock\"} property of type @code{:string}
  (Read / Write) @br{}
  The stock ID to use for the primary icon for the entry. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-storage-type"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-storage-type\"} property of type
  @symbol{gtk-image-type} (Read) @br{}
  The representation which is used for the primary icon of the entry. @br{}
  Default value: @code{:empty} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-tooltip-markup"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-tooltip-markup\"} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language.
  Also see the function @fun{gtk-entry-set-icon-tooltip-markup}. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-tooltip-text"
                                               'gtk-entry) 't)
 "The @code{\"primary-icon-tooltip-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon.
  Also see the function @fun{gtk-entry-set-icon-tooltip-text}. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "progress-fraction"
                                               'gtk-entry) 't)
 "The @code{\"progress-fraction\"} property of type @code{:double}
  (Read / Write) @br{}
  The current fraction of the task that is been completed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0 @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "progress-pulse-step"
                                               'gtk-entry) 't)
 "The @code{\"progress-pulse-step\"} property of type @code{:double}
  (Read / Write) @br{}
  The fraction of total entry width to move the progress bouncing block for
  each call to the function @fun{gtk-entry-progress-pulse}. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.1 @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scroll-offset" 'gtk-entry) 't)
 "The @code{\"scroll-offset\"} property of type @code{:int} (Read) @br{}
  Number of pixels of the entry scrolled off the screen to the left. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-activatable"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-activatable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is activatable.
  GTK+ emits the \"icon-press\" and \"icon-release\" signals only on sensitive,
  activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-gicon"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-gicon\"} property of type @class{g-icon}
  (Read / Write) @br{}
  The @class{g-icon} to use for the secondary icon for the entry. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-name"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-name\"} property of type @code{:string}
  (Read / Write) @br{}
  The icon name to use for the secondary icon for the entry. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-pixbuf"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-pixbuf\"} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  An pixbuf to use as the secondary icon for the entry. @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-sensitive"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-sensitive\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is sensitive.
  An insensitive icon appears grayed out. GTK+ does not emit the \"icon-press\"
  and \"icon-release\" signals and does not allow DND from insensitive
  icons.
  An icon should be set insensitive if the action that would trigger when
  clicked is currently not available. @br{}
  Default value: @em{true} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-stock"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-stock\"} property of type @code{:string}
  (Read / Write) @br{}
  The stock ID to use for the secondary icon for the entry. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-storage-type"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-storage-type\"} property of type
  @symbol{gtk-image-type} (Read) @br{}
  The representation which is used for the secondary icon of the entry. @br{}
  Default value: @code{:empty} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-tooltip-markup"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-tooltip-markup\"} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language.
  Also see the function @fun{gtk-entry-set-icon-tooltip-markup}. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-tooltip-text"
                                               'gtk-entry) 't)
 "The @code{\"secondary-icon-tooltip-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon.
  Also see the function @fun{gtk-entry-set-icon-tooltip-text}. @br{}
  Default value: @code{nil} @br{}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-bound" 'gtk-entry) 't)
 "The @code{\"selection-bound\"} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in
  chars. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-entry) 't)
 "The @code{\"shadow-type\"} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Which kind of shadow to draw around the entry when \"has-frame\" is set to
  @em{true}. @br{}
  Default value: @code{:in} @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-entry) 't)
 "The @code{\"text\"} property of type @code{:string} (Read / Write) @br{}
  The contents of the entry. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-length" 'gtk-entry) 't)
 "The @code{\"text-length\"} property of type @code{:uint} (Read) @br{}
  The length of the text in the @class{gtk-entry}. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0 @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "truncate-multiline"
                                               'gtk-entry) 't)
 "The @code{\"truncate-multiline\"} property of type @code{:boolean}
  (Read / Write) @br{}
  When @em{true}, pasted multi-line text is truncated to the first line. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visibility" 'gtk-entry) 't)
 "The @code{\"visibility\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @code{Nil} displays the \"invisible char\" instead of the actual text
  (password mode). @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-entry) 't)
 "The @code{\"width-chars\"} property of tpye @code{:int} (Read / Write) @br{}
  Number of characters to leave space for in the entry. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-entry) 't)
 "The @code{\"xalign\"} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
  layouts. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-activates-default atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-activates-default 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"activates-default\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-activates-default}
  @see-function{gtk-entry-set-activates-default}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-attributes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-attributes 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"attributes\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-attributes}
  @see-function{gtk-entry-set-attributes}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"buffer\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-buffer}
  @see-function{gtk-entry-set-buffer}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-caps-lock-warning atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-caps-lock-warning 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"caps-lock-warning\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"completion\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-class{gtk-entry-completion}
  @see-function{gtk-entry-get-completion}
  @see-function{gtk-entry-set-completion}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-cursor-position 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"cursor-position\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-editable 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"editable\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-has-frame atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-has-frame 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"has-frame\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-has-frame}
  @see-function{gtk-entry-set-has-frame}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-im-module atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-im-module 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"im-module\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-inner-border atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-inner-border 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"inner-border\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-inner-border}
  @see-function{gtk-entry-set-inner-border}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-input-hints atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-input-hints 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"input-hints\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-symbol{gtk-input-hints}
  @see-function{gtk-entry-get-input-hints}
  @see-function{gtk-entry-set-input-hints}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-input-purpose atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-input-purpose 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"input-purpose\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-input-purpose}
  @see-function{gtk-entry-set-input-purpose}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-invisible-char atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-invisible-char 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"invisible-char\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-invisible-char}
  @see-function{gtk-entry-set-invisible-char}
  @see-function{gtk-entry-unset-invisible-char}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-invisible-char-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-invisible-char-set 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"invisible-char-set\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-max-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-max-length 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"max-length\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-max-length}
  @see-function{gtk-entry-set-max-length}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-overwrite-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-overwrite-mode 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"overwrite-mode\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-overwrite-mode}
  @see-function{gtk-entry-set-overwrite-mode}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-placeholder-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-placeholder-text 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"placeholder-text\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-placeholder-text}
  @see-function{gtk-entry-set-placeholder-text}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-activatable 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-activatable\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-activatable}
  @see-function{gtk-entry-set-icon-activatable}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-gicon 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-gicon\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-gicon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-name 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-name\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-pixbuf 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-pixbuf\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-pixbuf}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-sensitive 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-sensitive\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-sensitive}
  @see-function{gtk-entry-set-icon-sensitive}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-stock 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-stock\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-stock}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-storage-type 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-storage-type\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-storage-type}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-tooltip-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-tooltip-markup 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-tooltip-markup\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-entry-set-icon-tooltip-markup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-tooltip-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-tooltip-text 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"primary-icon-tooltip-text\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-entry-set-icon-tooltip-text}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-progress-fraction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-progress-fraction 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"progress-fraction\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-progress-fraction}
  @see-function{gtk-entry-set-progress-fraction}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-progress-pulse-step atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-progress-pulse-step 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"progress-pulse-step\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-progress-pulse-step}
  @see-function{gtk-entry-set-progress-pulse-step}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-scroll-offset atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-scroll-offset 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"scroll-offset\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-activatable
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-activatable 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-activatable\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-activatable}
  @see-function{gtk-entry-set-icon-activatable}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-gicon 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-gicon\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-gicon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-name 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-name\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-pixbuf 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-pixbuf\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-pixbuf}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-sensitive 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-sensitive\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-sensitive}
  @see-function{gtk-entry-set-icon-sensitive}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-stock 'function)
 "@version{2013-8-25}
  Accessor of the slot @code{\"seconary-icon-stock\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-stock}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-storage-type
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-storage-type 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-storage-type\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-storage-type}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-tooltip-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-tooltip-markup 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-tooltip-markup\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-entry-set-icon-tooltip-markup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-tooltip-text
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-tooltip-text 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"seconary-icon-tooltip-text\"} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-entry-set-icon-tooltip-text}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-selection-bound atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-selection-bound 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"selection-bound\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-shadow-type 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"shadow-type\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-text 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"text\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-text}
  @see-function{gtk-entry-set-text}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-text-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-text-length 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"text-length\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-text-length}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-truncate-multiline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-truncate-multiline 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"truncate-multiline\"} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-visibility atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-visibility 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"visibility\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-visibility}
  @see-function{gtk-entry-set-visibility}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-width-chars 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"width-chars\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-width-chars}
  @see-function{gtk-entry-set-width-chars}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-xalign 'function)
 "@version{2013-8-26}
  Accessor of the slot @code{\"xalign\"} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new))

(defun gtk-entry-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @return{A new @class{gtk-entry} widget.}
  @short{Creates a new entry.}"
  (make-instance 'gtk-entry))

(export 'gtk-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new-with-buffer))

(defun gtk-entry-new-with-buffer (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[buffer]{the buffer to use for the new @class{gtk-entry} widget}
  @return{A new @class{gtk-entry} widget.}
  @begin{short}
    Creates a new entry with the specified text @arg{buffer}.
  @end{short}

  Since 2.18"
  (make-instance 'gtk-entry
                 :buffer buffer))

(export 'gtk-entry-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-buffer))

(defun gtk-entry-get-buffer (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{A @class{gtk-entry-buffer} object.}
  @begin{short}
    Get the @class{gtk-entry-buffer} object which holds the text for this
    widget.
  @end{short}

  Since 2.18
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-buffer}"
  (gtk-entry-buffer entry))

(export 'gtk-entry-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-buffer))

(defun gtk-entry-set-buffer (entry buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @begin{short}
    Set the @class{gtk-entry-buffer} object which holds the text for this
    widget.
  @end{short}

  Since 2.18
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-buffer}"
  (setf (gtk-entry-buffer entry) buffer))

(export 'gtk-entry-set-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-text))

(defun gtk-entry-set-text (entry text)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[text]{the new text}
  @begin{short}
    Sets the text in the @arg{entry} to the given value, replacing the current
    contents.
  @end{short}

  See the function @fun{gtk-entry-buffer-set-text}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-text}
  @see-function{gtk-entry-buffer-set-text}"
  (setf (gtk-entry-text entry) text))

(export 'gtk-entry-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-text))

(defun gtk-entry-get-text (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The contents of the @arg{entry} as a string.}
  @begin{short}
    Retrieves the contents of the @arg{entry} widget.
  @end{short}
  See also the function @fun{gtk-editable-get-chars}.

  This is equivalent to:
  @code{(gtk-entry-buffer-get-text (gtk-entry-get-buffer entry))}
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-text}
  @see-function{gtk-editable-get-chars}"
  (gtk-entry-text entry))

(export 'gtk-entry-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_length ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-text-length))

(defun gtk-entry-get-text-length (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The current number of characters in @class{gtk-entry}, or 0 if there
    are none.}
  @begin{short}
    Retrieves the current length of the text in @arg{entry}.
  @end{short}

  This is equivalent to:
  @code{(gtk-entry-buffer-get-length (gtk-entry-get-buffer entry))}

  Since 2.14
  @see-class{gtk-entry}
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-get-length}"
  (gtk-entry-text-length entry))

(export 'gtk-entry-get-text-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_text_area" %gtk-entry-get-text-area) :void
  (entry (g-object gtk-entry))
  (text-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-text-area (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{text-area -- the text area.}
  @begin{short}
    Gets the area where the @arg{entry}'s text is drawn. This function is useful
    when drawing something to the @arg{entry} in a draw callback.
  @end{short}

  If the entry is not realized, @arg{text-area} is filled with zeros.

  See also the function @fun{gtk-entry-get-icon-area}.

  Since 3.0
  @see-function{gtk-entry-get-icon-area}"
  (let ((text-area (make-gdk-rectangle)))
    (%gtk-entry-get-text-area entry text-area)
    text-area))

(export 'gtk-entry-get-text-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_visibility ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-visibility))

(defun gtk-entry-set-visibility (entry visible)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[visible]{@em{true} if the contents of the entry are displayed as
    plaintext}
  @begin{short}
    Sets whether the contents of the @arg{entry} are visible or not.
  @end{short}
  When visibility is set to @code{nil}, characters are displayed as the
  invisible char, and will also appear that way when the text in the entry
  widget is copied elsewhere.

  By default, GTK+ picks the best invisible character available in the current
  font, but it can be changed with the function
  @fun{gtk-entry-set-invisible-char}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-visiblity}
  @see-function{gtk-entry-set-invisible-char}"
  (setf (gtk-entry-visibility entry) visible))

(export 'gtk-entry-set-visibility)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_invisible_char ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-invisible-char))

(defun gtk-entry-set-invisible-char (entry ch)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[ch]{a Unicode character}
  @begin{short}
    Sets the character to use in place of the actual text when the function
    @fun{gtk-entry-set-visibility} has been called to set text visibility to
    @code{nil}.
  @end{short}

  I. e. this is the character used in \"password mode\" to show the user how
  many characters have been typed. By default, GTK+ picks the best invisible
  char available in the current font. If you set the invisible char to 0, then
  the user will get no feedback at all; there will be no text on the screen as
  they type.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-invisible-char}
  @see-function{gtk-entry-set-visibility}"
  (setf (gtk-entry-invisible-char-set entry) t
        (gtk-entry-invisible-char entry) ch))

(export 'gtk-entry-set-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_unset_invisible_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_unset_invisible_char" gtk-entry-unset-invisible-char) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Unsets the invisible char previously set with the function
    @fun{gtk-entry-set-invisible-char}. So that the default invisible char is
    used again.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-invisible-char}
  @see-function{gtk-entry-set-invisible-char}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_max_length ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-max-length))

(defun gtk-entry-set-max-length (entry max)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[max]{the maximum length of the entry, or 0 for no maximum, other
    than the maximum length of entries. The value passed in will be clamped to
    the range [0, 65536].}
  @begin{short}
    Sets the maximum allowed length of the contents of the widget. If the
    current contents are longer than the given length, then they will be
    truncated to fit.
  @end{short}

  This is equivalent to:
  @code{(gtk-entry-buffer-set-max-length (gtk-entry-get-buffer entry) max)}
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-max-length}"
  (setf (gtk-entry-max-length entry) max))

(export 'gtk-entry-set-max-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_activates_default ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-activates-default))

(defun gtk-entry-get-activates-default (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{@em{True} if the @arg{entry} will activate the default widget.}
  Retrieves the value set by the function @fun{gtk-entry-set-activates-default}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-activates-default}"
  (gtk-entry-activates-default entry))

(export 'gtk-entry-get-activates-default)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_has_frame ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-has-frame))

(defun gtk-entry-get-has-frame (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{Whether the @arg{entry} has a beveled frame.}
  Gets the value set by the function @fun{gtk-entry-set-has-frame}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-has-frame}"
  (gtk-entry-has-frame entry))

(export 'gtk-entry-get-has-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_inner_border ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-inner-border))

(defun gtk-entry-get-inner-border (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The @arg{entry}'s @class{gtk-border}, or @code{nil} if none was set.}
  @subheading{Warning}
    The function @sym{gtk-entry-get-inner-border} has been deprecated since
    version 3.4 and should not be used in newly written code. Use the standard
    border and padding CSS properties through objects like
    @class{gtk-style-context} and @class{gtk-css-provider}; the value returned
    by this function is ignored by @class{gtk-entry}.

  @begin{short}
    This function returns the @arg{entry}'s @code{\"inner-border\"} property.
    See the function @fun{gtk-entry-set-inner-border} for more information.
  @end{short}

  Since 2.10
  @see-class{gtk-entry}
  @see-class{gtk-border}
  @see-class{gtk-style-context}
  @see-class{gtk-css-provider}
  @see-function{gtk-entry-set-inner-border}"
  (gtk-entry-inner-border entry))

(export 'gtk-entry-get-inner-border)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-width-chars))

(defun gtk-entry-get-width-chars (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{Number of chars to request space for, or negative if unset.}
  Gets the value set by the function @fun{gtk-entry-set-width-chars}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-width-chars}"
  (gtk-entry-width-chars entry))

(export 'gtk-entry-get-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_activates_default ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-activates-default))

(defun gtk-entry-set-activates-default (entry setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[setting]{@em{true} to activate window's default widget on Enter
    keypress}
  @begin{short}
    If @arg{setting} is @em{true}, pressing Enter in the @arg{entry} will
    activate the default widget for the window containing the @arg{entry}.
  @end{short}
  This usually means that the dialog box containing the @arg{entry} will be
  closed, since the default widget is usually one of the dialog buttons.

  For experts: if @arg{setting} is @em{true}, the @arg{entry} calls the function
  @fun{gtk-window-activate-default} on the window containing the @arg{entry},
  in the default handler for the \"activate\" signal.
  @see-class{gtk-entry}
  @see-function{gtk-window-activate-default}
  @see-function{gtk-entry-get-activates-default}"
  (setf (gtk-entry-activates-default entry) setting))

(export 'gtk-entry-set-activates-default)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_has_frame ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-has-frame))

(defun gtk-entry-set-has-frame (entry setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[setting]{new value}
  Sets whether the @arg{entry} has a beveled frame around it.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-has-frame}"
  (setf (gtk-entry-has-frame entry) setting))

(export 'gtk-entry-set-has-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_inner_border ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-inner-border))

(defun gtk-entry-set-inner-border (entry border)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[border]{a @class{gtk-border}, or @code{nil}}
  @subheading{Warning}
    The function @sym{gtk-entry-set-inner-border} has been deprecated since
    version 3.4 and should not be used in newly written code. Use the standard
    border and padding CSS properties through objects like
    @class{gtk-style-context} and @class{gtk-css-provider}; the value set with
    this function is ignored by @class{gtk-entry}.

  @begin{short}
    Sets @arg{entry}'s @code{\"inner-border\"} property to @arg{border}, or
    clears it if @code{nil} is passed. The @code{\"inner-border\"} is the area
    around the @arg{entry}'s text, but inside its frame.
  @end{short}

  If set, this property overrides the @code{\"inner-border\"} style property.
  Overriding the style-provided border is useful when you want to do in-place
  editing of some text in a canvas or list widget, where pixel-exact positioning
  of the entry is important.

  Since 2.10
  @see-class{gtk-entry}
  @see-class{gtk-border}
  @see-class{gtk-style-context}
  @see-class{gtk-css-provider}
  @see-function{gtk-entry-get-inner-border}"
  (setf (gtk-entry-inner-border entry) border))

(export 'gtk-entry-set-inner-border)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-width-chars))

(defun gtk-entry-set-width-chars (entry n-chars)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[n-chars]{width in chars}
  @begin{short}
    Changes the size request of the @arg{entry} to be about the right size for
    @arg{n-chars} characters.
  @end{short}
  Note that it changes the size request, the size can still be affected by how
  you pack the widget into containers. If @arg{n-chars} is -1, the size reverts
  to the default entry size.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-width-chars}"
  (setf (gtk-entry-width-chars entry) n-chars))

(export 'gtk-entry-set-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_invisible_char ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-invisible-char))

(defun gtk-entry-get-invisible-char (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    The current invisible char, or 0, if the entry does not show invisible
    text at all.
  @end{return}
  @begin{short}
    Retrieves the character displayed in place of the real characters for
    entries with visibility set to false.
  @end{short}
  See the function @fun{gtk-entry-set-invisible-char}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-invisible-char}"
  (gtk-entry-invisible-char entry))

(export 'gtk-entry-get-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-alignment))

(defun gtk-entry-set-alignment (entry xalign)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[xalign]{the horizontal alignment, from 0 (left) to 1 (right),
    reversed for RTL layouts}
  @begin{short}
    Sets the alignment for the contents of the @arg{entry}.
  @end{short}
  This controls the horizontal positioning of the contents when the displayed
  text is shorter than the width of the @arg{entry}.

  Since 2.4"
  (setf (gtk-entry-xalign entry) xalign))

(export 'gtk-entry-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-alignment))

(defun gtk-entry-get-alignment (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The alignment.}
  @begin{short}
    Gets the value set by the fucntion @fun{gtk-entry-set-alignment}.
  @end{short}

  Since 2.4"
  (gtk-entry-xalign entry))

(export 'gtk-entry-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_placeholder_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-placeholder-text))

(defun gtk-entry-set-placeholder-text (entry text)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[text]{a string to be displayed when @arg{entry} is empty an
    unfocused, or @code{nil}}
  @begin{short}
    Sets text to be displayed in @arg{entry} when it is empty and unfocused.
    This can be used to give a visual hint of the expected contents of the
    @class{gtk-entry}.
  @end{short}

  Note that since the placeholder text gets removed when the entry received
  focus, using this feature is a bit problematic if the entry is given the
  initial focus in a window. Sometimes this can be worked around by delaying
  the initial focus setting until the first key event arrives.

  Since 3.2
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-placeholder-text}"
  (setf (gtk-entry-placeholder-text entry) text))

(export 'gtk-entry-set-placeholder-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_placeholder_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-placeholder-text))

(defun gtk-entry-get-placeholder-text (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    The placeholder text as a string.
  @end{return}
  @begin{short}
    Retrieves the text that will be displayed when @arg{entry} is empty and
    unfocused.
  @end{short}

  Since 3.2
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-placeholder-text}"
  (gtk-entry-placeholder-text entry))

(export 'gtk-entry-get-placeholder-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_overwrite_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-overwrite-mode))

(defun gtk-entry-set-overwrite-mode (entry overwrite)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[overwrite]{new value}
  @begin{short}
    Sets whether the text is overwritten when typing in the @class{gtk-entry}.
  @end{short}

  Since 2.14
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-overwrite-mode}"
  (setf (gtk-entry-overwrite-mode entry) overwrite))

(export 'gtk-entry-set-overwrite-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_overwrite_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-overwrite-mode))

(defun gtk-entry-get-overwrite-mode (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{Whether the text is overwritten when typing.}
  @begin{short}
    Gets the value set by the function @fun{gtk-entry-set-overwrite-mode}.
  @end{short}

  Since 2.14
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-overwrite-mode}"
  (gtk-entry-overwrite-mode entry))

(export 'gtk-entry-get-overwrite-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_layout" gtk-entry-get-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The @class{pango-layout} object for this @arg{entry} widget.}
  @begin{short}
    Gets the @class{pango-layout} object used to display the @arg{entry}.
  @end{short}
  The layout is useful to e. g. convert text positions to pixel positions, in
  combination with the function @fun{gtk-entry-get-layout-offsets}. The returned
  layout is owned by the @arg{entry} and must not be modified or freed by the
  caller.

  Keep in mind that the layout text may contain a preedit string, so the
  functions @fun{gtk-entry-layout-index-to-text-index} and
  @fun{gtk-entry-text-index-to-layout-index} are needed to convert byte indices
  in the layout to byte indices in the entry contents.
  @see-function{gtk-entry-get-layout-offsets}
  @see-function{gtk-entry-layout-index-to-text-index}
  @see-function{gtk-entry-text-index-to-layout-index}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout_offsets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_layout_offsets" %gtk-entry-get-layout-offsets) :void
  (entry (g-object entry))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-entry-get-layout-offsets (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-27}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    @code{x} -- x offset of layout, or @code{nil} @br{}
    @code{y} -- y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the position of the @class{pango-layout} used to render text in the
    @arg{entry}, in widget coordinates.
  @end{short}
  Useful if you want to line up the text in an entry with some other text, e. g.
  when using the entry to implement editable cells in a sheet widget.

  Also useful to convert mouse events into coordinates inside the
  @class{pango-layout}, e. g. to take some action if some part of the entry text
  is clicked.

  Note that as the user scrolls around in the entry the offsets will change;
  you will need to connect to the \"notify::scroll-offset\" signal to track
  this. Remember when using the @class{pango-layout} functions you need to
  convert to and from pixels using the function @fun{pango-pixels} or
  the constant @var{+pango-scale+}.

  Keep in mind that the layout text may contain a preedit string, so the
  functions @fun{gtk-entry-layout-index-to-text-index} and
  @fun{gtk-entry-text-index-to-layout-index} are needed to convert byte indices
  in the layout to byte indices in the entry contents.
  @see-class{gtk-entry}
  @see-class{pango-layout}
  @see-function{gtk-entry-layout-index-to-text-index}
  @see-function{gtk-entry-text-index-to-layout-index}
  @see-function{pango-pixels}
  @see-variable{+pango-scale+}"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-entry-get-layout-offsets entry x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-entry-get-layout-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_layout_index_to_text_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_layout_index_to_text_index"
          gtk-entry-layout-index-to-text-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[layout-index]{byte index into the entry layout text}
  @return{Byte index into the entry contents.}
  @begin{short}
    Converts from a position in the entry contents (returned by the function
    @fun{gtk-entry-get-text}) to a position in the @arg{entry}'s
    @class{pango-layout} (returned by the function @fun{gtk-entry-get-layout},
    with text retrieved via the function @fun{pango-layout-get-text}).
  @end{short}
  @see-function{gtk-entry-get-text}
  @see-function{gtk-entry-get-layout}
  @see-function{pango-layout-get-text}"
  (entry (g-object entry))
  (layout-index :int))

(export 'gtk-entry-layout-index-to-text-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_text_index_to_layout_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_text_index_to_layout_index"
          gtk-entry-text-index-to-layout-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[text-index]{byte index into the @arg{entry} contents}
  @return{Byte index into the @arg{entry} layout text.}
  @begin{short}
    Converts from a position in the @arg{entry}'s @class{pango-layout} (returned
    by the function @fun{gtk-entry-get-layout}) to a position in the @arg{entry}
    contents (returned by the function @fun{gtk-entry-get-text}).
  @end{short}
  @see-function{gtk-entry-get-layout}
  @see-function{gtk-entry-get-text}"
  (entry (g-object entry))
  (text-index :int))

(export 'gtk-entry-text-index-to-layout-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_attributes ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-attributes))

(defun gtk-entry-set-attributes (entry attrs)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[attrs]{a @class{pango-attr-list}}
  @begin{short}
    Sets a @class{pango-attr-list}; the attributes in the list are applied to
    the entry text.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-attributes}"
  (setf (gtk-entry-attributes entry) attrs))

(export 'gtk-entry-set-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_attributes ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-attributes))

(defun gtk-entry-get-attributes (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The attribute list, or @code{nil} if none was set.}
  @begin{short}
    Gets the attribute list that was set on the entry using the function
    @fun{gtk-entry-set-attributes}, if any.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-attributes}"
  (gtk-entry-attributes entry))

(export 'gtk-entry-get-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_max_length ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-max-length))

(defun gtk-entry-get-max-length (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    The maximum allowed number of characters in @class{gtk-entry}, or 0 if there
    is no maximum.
  @end{return}
  @begin{short}
    Retrieves the maximum allowed length of the text in @arg{entry}.
  @end{short}
  See the function @fun{gtk-entry-set-max-length}.

  This is equivalent to:
  @code{(gtk-entry-buffer-get-max-length (gtk-entry-get-buffer entry))}
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-max-length}"
  (gtk-entry-max-length entry))

(export 'gtk-entry-get-max-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_visibility ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-visibility))

(defun gtk-entry-get-visibility (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{@em{True} if the text is currently visible.}
  @begin{short}
    Retrieves whether the text in @arg{entry} is visible.
  @end{short}
  See the function @fun{gtk-entry-set-visibility}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-visibility}"
  (gtk-entry-visibility entry))

(export 'gtk-entry-get-visibility)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_completion ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-completion))

(defun gtk-entry-set-completion (entry completion)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[completion]{the @class{gtk-entry-completion} or @code{nil}}
  @begin{short}
    Sets @arg{completion} to be the auxiliary completion object to use with
    @arg{entry}.
  @end{short}
  All further configuration of the completion mechanism is done on
  @arg{completion} using the @class{gtk-entry-completion} API. Completion is
  disabled if @arg{completion} is set to @code{nil}.

  Since 2.4
  @see-class{gtk-entry}
  @see-class{gtk-entry-completion}
  @see-function{gtk-entry-get-completion}"
  (setf (gtk-entry-completion entry) completion))

(export 'gtk-entry-set-completion)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_completion ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-completion))

(defun gtk-entry-get-completion (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The auxiliary completion object currently in use by @arg{entry}.}
  @begin{short}
    Returns the auxiliary completion object currently in use by @arg{entry}.
  @end{short}

  Since 2.4
  @see-class{gtk-entry}
  @see-class{gtk-entry-completion}
  @see-function{get-entry-set-completion}"
  (gtk-entry-completion entry))

(export 'gtk-entry-get-completion)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_cursor_hadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_cursor_hadjustment" gtk-entry-set-cursor-hadjustment)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[adjustment]{an adjustment which should be adjusted when the cursor
    is moved, or @code{nil}}
  @begin{short}
    Hooks up an @arg{adjustment} to the cursor position in an @arg{entry}, so
    that when the cursor is moved, the @arg{adjustment} is scrolled to show that
    position.
  @end{short}
  See the function @fun{gtk-scrolled-window-get-hadjustment} for a typical way
  of obtaining the @arg{adjustment}.

  The @arg{adjustment} has to be in pixel units and in the same coordinate
  system as the @arg{entry}.

  Since 2.12
  @see-function{gtk-scrolled-window-get-hadjustment}"
  (entry (g-object gtk-entry))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-entry-set-cursor-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_cursor_hadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_cursor_hadjustment" gtk-entry-get-cursor-hadjustment)
    (g-object gtk-adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The horizontal cursor adjustment, or @code{nil} if none has been set.}
  @begin{short}
    Retrieves the horizontal cursor adjustment for the @arg{entry}.
  @end{short}
  See the function @fun{gtk-entry-set-cursor-hadjustment}.

  Since 2.12
  @see-function{gtk-entry-set-cursor-hadjustment}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-cursor-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_progress_fraction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-progress-fraction))

(defun gtk-entry-set-progress-fraction (entry fraction)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[fraction]{fraction of the task that is been completed}
  @begin{short}
    Causes the @arg{entry}'s progress indicator to \"fill in\" the given
    @arg{fraction} of the bar. The @arg{fraction} should be between 0.0 and 1.0,
    inclusive.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-progress-fraction}"
  (setf (gtk-entry-progress-fraction entry) fraction))

(export 'gtk-entry-set-progress-fraction)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_progress_fraction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-progress-fraction))

(defun gtk-entry-get-progress-fraction (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{A fraction from 0.0 to 1.0.}
  @begin{short}
    Returns the current fraction of the task that is been completed.
  @end{short}
  See the function @fun{gtk-entry-set-progress-fraction}.

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-progress-fraction}"
  (gtk-entry-progress-fraction entry))

(export 'gtk-entry-get-progress-fraction)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_progress_pulse_step ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-progress-pulse-step))

(defun gtk-entry-set-progress-pulse-step (entry fraction)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[fraction]{fraction between 0.0 and 1.0}
  @begin{short}
    Sets the fraction of total @arg{entry} width to move the progress bouncing
    block for each call to the function @fun{gtk-entry-progress-pulse}.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-progress-pulse}
  @see-function{gtk-entry-get-progress-pulse-step}"
  (setf (gtk-entry-progress-pulse-step entry) fraction))

(export 'gtk-entry-set-progress-pulse-step)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_progress_pulse_step ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-progress-pulse-step))

(defun gtk-entry-get-progress-pulse-step (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{A fraction from 0.0 to 1.0.}
  @begin{short}
    Retrieves the pulse step set with the function
    @fun{gtk-entry-set-progress-pulse-step}.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-progress-pulse-step}"
  (gtk-entry-progress-pulse-step entry))

(export 'gtk-entry-get-progress-pulse-step)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_progress_pulse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_progress_pulse" gtk-entry-progress-pulse) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Indicates that some progress is made, but you do not know how much.
  @end{short}
  Causes the @arg{entry}'s progress indicator to enter \"activity mode\", where
  a block bounces back and forth. Each call to the function
  @sym{gtk-entry-progress-pulse} causes the block to move by a little bit. The
  amount of movement per pulse is determined by the function
  @fun{gtk-entry-set-progress-pulse-step}.

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-progress-pulse-step}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-progress-pulse)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_im_context_filter_keypress ()
;;;
;;; gboolean gtk_entry_im_context_filter_keypress (GtkEntry *entry,
;;;                                                GdkEventKey *event);
;;;
;;; Allow the GtkEntry input method to internally handle key press and release
;;; events. If this function returns TRUE, then no further processing should be
;;; done for this key event. See gtk_im_context_filter_keypress().
;;;
;;; Note that you are expected to call this function from your handler when
;;; overriding key event handling. This is needed in the case when you need to
;;; insert your own key handling between the input method and the default key
;;; event handling of the GtkEntry. See gtk_text_view_reset_im_context() for an
;;; example of use.
;;;
;;; entry :
;;;     a GtkEntry
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
;;; gtk_entry_reset_im_context ()
;;;
;;; void gtk_entry_reset_im_context (GtkEntry *entry);
;;;
;;; Reset the input method context of the entry if needed.
;;;
;;; This can be necessary in the case where modifying the buffer would confuse
;;; on-going input method behavior.
;;;
;;; entry :
;;;     a GtkEntry
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkEntryIconPosition
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkEntryIconPosition" gtk-entry-icon-position
  (:export t
   :type-initializer "gtk_entry_icon_position_get_type")
  (:primary 0)
  (:secondary 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-icon-position atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-entry-icon-position atdoc:*external-symbols*)
 "@version{2013-4-28}
  @begin{short}
    Specifies the side of the entry at which an icon is placed.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkEntryIconPosition\" gtk-entry-icon-position
  (:export t
   :type-initializer \"gtk_entry_icon_position_get_type\")
  (:primary 0)
  (:secondary 1))
  @end{pre}
  @begin[code]{table}
    @entry[:primary]{At the beginning of the entry (depending on the text
      direction).}
    @entry[:secondary]{At the end of the entry (depending on the text
      direction).}
  @end{table}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_pixbuf ()
;;;
;;; void gtk_entry_set_icon_from_pixbuf (GtkEntry *entry,
;;;                                      GtkEntryIconPosition icon_pos,
;;;                                      GdkPixbuf *pixbuf);
;;;
;;; Sets the icon shown in the specified position using a pixbuf.
;;;
;;; If pixbuf is NULL, no icon will be shown in the specified position.
;;;
;;; entry :
;;;     a GtkEntry
;;;
;;; icon_pos :
;;;     Icon position
;;;
;;; pixbuf :
;;;     A GdkPixbuf, or NULL
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_stock ()
;;; ----------------------------------------------------------------------------

(defun gtk-entry-set-icon-from-stock (entry icon-pos stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @arg{stock-id]{the name of the stock item, or @code{nil}}
  @begin{short}
    Sets the icon shown in the @arg{entry} at the specified position from a
    stock image.
  @end{short}

  If @arg{stock-id} is @code{nil}, no icon will be shown in the specified
  position.

  Since 2.16
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-stock}"
  (cond ((eq icon-pos :primary)
         (setf (gtk-entry-primary-icon-stock entry) stock-id))
        ((eq icon-pos :secondary)
         (setf (gtk-entry-secondary-icon-stock entry) stock-id))
        (t
         (error "Unexpected icon position in GTK-ENTRY-SET-ICON-FROM-STOCK"))))

(export 'gtk-entry-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_icon_name ()
;;;
;;; void gtk_entry_set_icon_from_icon_name (GtkEntry *entry,
;;;                                         GtkEntryIconPosition icon_pos,
;;;                                         const gchar *icon_name);
;;;
;;; Sets the icon shown in the entry at the specified position from the current
;;; icon theme.
;;;
;;; If the icon name isn't known, a "broken image" icon will be displayed
;;; instead.
;;;
;;; If icon_name is NULL, no icon will be shown in the specified position.
;;;
;;; entry :
;;;     A GtkEntry
;;;
;;; icon_pos :
;;;     The position at which to set the icon
;;;
;;; icon_name :
;;;     An icon name, or NULL
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_gicon ()
;;;
;;; void gtk_entry_set_icon_from_gicon (GtkEntry *entry,
;;;                                     GtkEntryIconPosition icon_pos,
;;;                                     GIcon *icon);
;;;
;;; Sets the icon shown in the entry at the specified position from the current
;;; icon theme. If the icon isn't known, a "broken image" icon will be
;;; displayed instead.
;;;
;;; If icon is NULL, no icon will be shown in the specified position.
;;;
;;; entry :
;;;     A GtkEntry
;;;
;;; icon_pos :
;;;     The position at which to set the icon
;;;
;;; icon :
;;;     The icon to set, or NULL
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_storage_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_storage_type" gtk-entry-get-storage-type)
    gtk-image-type
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @return{Image representation of type @symbol{gtk-image-type} being used.}
  @begin{short}
    Gets the type of representation being used by the icon to store image data.
    If the icon has no image data, the return value will be @code{:empty}.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-symbol{gtk-image-type}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-storage-type)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_pixbuf" gtk-entry-get-icon-pixbuf)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of @symbol{gtk-entry-icon-position}}
  @begin{return}
    A @class{gdk-pixbuf}, or @code{nil} if no icon is set for this position.
  @end{return}
  @begin{short}
    Retrieves the image used for the icon.
  @end{short}

  Unlike the other methods of setting and getting icon data, this method will
  work regardless of whether the icon was set using a @class{gdk-pixbuf}, a
  @class{g-icon}, a stock item, or an icon name.

  Since 2.16
  @see-class{gtk-entry}
  @see-class{g-icon}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_stock" gtk-entry-get-icon-stock) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @begin{return}
    A stock ID, or @code{nil} if no icon is set or if the icon was not set from
    a stock ID.
  @end{return}
  @begin{short}
    Retrieves the stock ID used for the icon, or @code{nil} if there is no icon
    or if the icon was set by some other method, e. g., by pixbuf, icon name or
    gicon.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_name" gtk-entry-get-icon-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @begin{return}
    An icon name, or @code{nil} if no icon is set or if the icon was not set
    from an icon name.
  @end{return}
  @begin{short}
    Retrieves the icon name used for the icon, or @code{nil} if there is no icon
    or if the icon was set by some other method, e. g., by pixbuf, stock or
    gicon.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_gicon" gtk-entry-get-icon-gicon) (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @return{A @class{g-icon}, or @code{nil} if no icon is set or if the icon is
    not a @class{g-icon}.}
  @begin{short}
    Retrieves the @class{g-icon} used for the icon, or @code{nil} if there is no
    icon or if the icon was set by some other method, e. g., by stock, pixbuf,
    or icon name.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_activatable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_activatable" gtk-entry-set-icon-activatable) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @argument[activatable]{@em{true} if the icon should be activatable}
  @begin{short}
    Sets whether the icon is activatable.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-activatable}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (activatable :boolean))

(export 'gtk-entry-set-icon-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_activatable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_activatable" gtk-entry-get-icon-activatable)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @return{@em{True} if the icon is activatable.}
  @begin{short}
    Returns whether the icon is activatable.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-activatable}
  @see-function{gtk-entry-set-icon-activatable}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_sensitive" gtk-entry-set-icon-sensitive) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @argument[sensitive]{specifies whether the icon should appear sensitive or
    insensitive}
  @begin{short}
    Sets the sensitivity for the specified icon.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-sensitive}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (sensitive :boolean))

(export 'gtk-entry-set-icon-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_sensitive" gtk-entry-get-icon-sensitive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @return{@em{True} if the icon is sensitive.}
  @begin{short}
    Returns whether the icon appears sensitive or insensitive.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-set-icon-sensitive}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_at_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_at_pos" gtk-entry-get-icon-at-pos) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[x]{the x coordinate of the position to find}
  @argument[y]{the y coordinate of the position to find}
  @return{The index of the icon at the given position, or -1.}
  @begin{short}
    Finds the icon at the given position and return its index.
  @end{short}
  The position's coordinates are relative to the @arg{entry}'s top left corner.
  If x, y does not lie inside an icon, -1 is returned. This function is intended
  for use in a \"query-tooltip\" signal handler.

  Since 2.16"
  (entry (g-object gtk-entry))
  (x :int)
  (y :int))

(export 'gtk-entry-get-icon-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_tooltip_text" gtk-entry-set-icon-tooltip-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the icon position of type
    @symbol{gtk-entry-icon-position}}
  @argument[tooltip]{the contents of the tooltip for the icon, or @code{nil}}
  @begin{short}
    Sets @arg{tooltip} as the contents of the tooltip for the icon at the
    specified position.
  @end{short}

  Use @code{nil} for @arg{tooltip} to remove an existing tooltip.

  See also the functions @fun{gtk-widget-set-tooltip-text} and
  @fun{gtk-entry-set-icon-tooltip-markup}.

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-widget-set-tooltip-text}
  @see-function{gtk-entry-set-icon-tooltip-markup}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (tooltip :string))

(export 'gtk-entry-set-icon-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_tooltip_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_tooltip_text" gtk-entry-get-icon-tooltip-text)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the icon position of type
    @symbol{gtk-entry-icon-position}}
  @return{The tooltip text, or @code{nil}.}
  @begin{short}
    Gets the contents of the tooltip on the icon at the specified position in
    entry.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-set-icon-tooltip-text}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_tooltip_markup" gtk-entry-set-icon-tooltip-markup)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the icon position of type
    @symbol{gtk-entry-icon-position}}
  @argument[tooltip]{the contents of the tooltip for the icon, or @code{nil}}
  @begin{short}
    Sets @arg{tooltip} as the contents of the tooltip for the icon at the
    specified position.
  @end{short}
  @arg{tooltip} is assumed to be marked up with the Pango text markup language.

  Use @code{nil} for @arg{tooltip} to remove an existing tooltip.

  See also the functions @fun{gtk-widget-set-tooltip-markup} and
  @fun{gtk-entry-set-icon-tooltip-text}.

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-widget-set-tooltip-markup}
  @see-function{gtk-entry-set-icon-tooltip-text}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (tooltip :string))

(export 'gtk-entry-set-icon-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_tooltip_markup" gtk-entry-get-icon-tooltip-markup)
    :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-26}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the icon position of type
    @symbol{gtk-entry-icon-position}}
  @return{The tooltip text, or @code{nil}.}
  @begin{short}
    Gets the contents of the tooltip on the icon at the specified position in
    entry.
  @end{short}

  Since 2.16
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-set-icon-tooltip-markup}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_drag_source ()
;;;
;;; void gtk_entry_set_icon_drag_source (GtkEntry *entry,
;;;                                      GtkEntryIconPosition icon_pos,
;;;                                      GtkTargetList *target_list,
;;;                                      GdkDragAction actions);
;;;
;;; Sets up the icon at the given position so that GTK+ will start a drag
;;; operation when the user clicks and drags the icon.
;;;
;;; To handle the drag operation, you need to connect to the usual
;;; "drag-data-get" (or possibly "drag-data-delete") signal, and use
;;; gtk_entry_get_current_icon_drag_source() in your signal handler to find out
;;; if the drag was started from an icon.
;;;
;;; By default, GTK+ uses the icon as the drag icon. You can use the
;;; "drag-begin" signal to set a different icon. Note that you have to use
;;; g_signal_connect_after() to ensure that your signal handler gets executed
;;; after the default handler.
;;;
;;; entry :
;;;     a GtkIconEntry
;;;
;;; icon_pos :
;;;     icon position
;;;
;;; target_list :
;;;     the targets (data formats) in which the data can be provided
;;;
;;; actions :
;;;     a bitmask of the allowed drag actions
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_current_icon_drag_source ()
;;;
;;; gint gtk_entry_get_current_icon_drag_source (GtkEntry *entry);
;;;
;;; Returns the index of the icon which is the source of the current DND
;;; operation, or -1.
;;;
;;; This function is meant to be used in a "drag-data-get" callback.
;;;
;;; entry :
;;;     a GtkIconEntry
;;;
;;; Returns :
;;;     index of the icon which is the source of the current DND operation, or
;;;     -1.
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_area" %gtk-entry-get-icon-area) :void
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (icon-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-icon-area (entry icon-pos)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position}
  @return{@code{icon-area} -- return the icon's area}
  @begin{short}
    Gets the area where @arg{entry}'s icon at @arg{icon-pos} is drawn. This
    function is useful when drawing something to the @arg{entry} in a draw
    callback.
  @end{short}

  If the @arg{entry} is not realized or has no icon at the given position,
  @arg{icon-area} is filled with zeros.

  See also the function @fun{gtk-entry-get-text-area}.

  Since 3.0
  @see-function{gtk-entry-get-text-area}"
  (let ((icon-area (make-gdk-rectangle)))
    (%gtk-entry-get-icon-area entry icon-pos icon-area)
    icon-area))

(export 'gtk-entry-get-icon-area)

;;; ----------------------------------------------------------------------------
;;; enum GtkInputPurpose
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkInputPurpose" gtk-input-purpose
  (:export t
   :type-initializer "gtk_input_purpose_get_type")
  (:free-form 0)
  (:alpha 1)
  (:digits 2)
  (:number 3)
  (:phone 4)
  (:url 5)
  (:email 6)
  (:name 7)
  (:password 8)
  (:pin 9))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-input-purpose atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-input-purpose atdoc:*external-symbols*)
 "@version{2013-8-25}
  @begin{short}
    Describes primary purpose of the input widget. This information is useful
    for on-screen keyboards and similar input methods to decide which keys
    should be presented to the user.
  @end{short}

  Note that the purpose is not meant to impose a totally strict rule about
  allowed characters, and does not replace input validation. It is fine for an
  on-screen keyboard to let the user override the character set restriction
  that is expressed by the purpose. The application is expected to validate
  the entry contents, even if it specified a purpose.

  The difference between @code{:digits} and @code{:number} is that the former
  accepts only digits while the latter also some punctuation (like commas or
  points, plus, minus) and 'e' or 'E' as in 3.14E+000.

  This enumeration may be extended in the future; input methods should
  interpret unknown values as 'free form'.
  @begin{pre}
(define-g-enum \"GtkInputPurpose\" gtk-input-purpose
  (:export t
   :type-initializer \"gtk_input_purpose_get_type\")
  (:free-form 0)
  (:alpha 1)
  (:digits 2)
  (:number 3)
  (:phone 4)
  (:url 5)
  (:email 6)
  (:name 7)
  (:password 8)
  (:pin 9))
  @end{pre}
  @begin[code]{table}
    @entry[:free-form]{Allow any character.}
    @entry[:alpha]{Allow only alphabetic characters.}
    @entry[:digits]{Allow only digits.}
    @entry[:number]{Edited field expects numbers.}
    @entry[:phone]{Edited field expects phone number.}
    @entry[:url]{Edited field expects URL.}
    @entry[:email]{Edited field expects email address.}
    @entry[:name]{Edited field expects the name of a person.}
    @entry[:password]{Like @code{:free-form}, but characters are hidden.}
    @entry[:pin]{Like @code{:digits}, but characters are hidden.}
  @end{table}
  Since 3.6
  @see-class{gtk-entry}
  @see-symbol{gtk-input-hints}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_input_purpose ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-input-purpose))

(defun gtk-entry-set-input-purpose (entry purpose)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[purpose]{the purpose}
  @begin{short}
    Sets the @code{\"input-purpose\"} property which can be used by on-screen
    keyboards and other input methods to adjust their behaviour.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-input-purpose}"
  (setf (gtk-entry-input-purpose entry) purpose))

(export 'gtk-entry-set-input-purpose)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_input_purpose ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-input-purpose))

(defun gtk-entry-get-input-purpose (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Gets the value of the @code{\"input-purpose\"} property.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-input-purpose}"
  (gtk-entry-input-purpose entry))

(export 'gtk-entry-get-input-purpose)

;;; ----------------------------------------------------------------------------
;;; enum GtkInputHints
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkInputHints" gtk-input-hints
  (:export t
   :type-initializer "gtk_input_hints_get_type")
  (:none 0)
  (:spellcheck #.(ash 1 0))
  (:no-spellcheck #.(ash 1 1))
  (:word-completion #.(ash 1 2))
  (:lowercase #.(ash 1 3))
  (:uppercase-chars #.(ash 1 4))
  (:uppercase-words #.(ash 1 5))
  (:uppercase-sentences #.(ash 1 6))
  (:inhibit-osk #.(ash 1 7)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-input-hints atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-input-hints atdoc:*external-symbols*)
 "@version{2013-8-20}
  @begin{short}
    Describes hints that might be taken into account by input methods or
    applications. Note that input methods may already tailor their behaviour
    according to the @symbol{gtk-input-purpose} of the entry.
  @end{short}

  Some common sense is expected when using these flags - mixing
  @code{:lowercase} with any of the uppercase hints makes no sense.

  This flags may be extended in the future; input methods should ignore
  unknown values.
  @begin{pre}
(define-g-flags \"GtkInputHints\" gtk-input-hints
  (:export t
   :type-initializer \"gtk_input_hints_get_type\")
  (:none 0)
  (:spellcheck #.(ash 1 0))
  (:no-spellcheck #.(ash 1 1))
  (:word-completion #.(ash 1 2))
  (:lowercase #.(ash 1 3))
  (:uppercase-chars #.(1 4))
  (:uppercase-words #.(1 5))
  (:uppercase-sentences #.(1 6))
  (:inhibit-osk #.(1 7)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No special behaviour suggested.}
    @entry[:spellcheck]{Suggest checking for typos.}
    @entry[:no-spellcheck]{Suggest not checking for typos.}
    @entry[:word-completion]{Suggest word completion.}
    @entry[:lowercase]{Suggest to convert all text to lowercase.}
    @entry[:uppercase-chars]{Suggest to capitalize all text.}
    @entry[:uppercase-words]{Suggest to capitalize the first character of each
      word.}
    @entry[:uppercase-sentences]{Suggest to capitalize the first word of each
      sentence.}
    @entry[:inhibit-osk]{Suggest to not show an onscreen keyboard, e. g for a
      calculator that already has all the keys.}
  @end{table}
  Since 3.6
  @see-symbol{gtk-input-purpose}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_input_hints ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-input-hints))

(defun gtk-entry-set-input-hints (entry hints)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[hints]{the hints}
  @begin{short}
    Sets the @code{\"input-hints\"} property, which allows input methods to
    fine-tune their behaviour.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-input-hints}
  @see-function{gtk-entry-set-input-hints}"
  (setf (gtk-entry-input-hints entry) hints))

(export 'gtk-entry-set-input-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_input_hints ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-input-hints))

(defun gtk-entry-get-input-hints (entry)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Gets the value of the @code{\"input-hints\"} property.
  @end{short}

  Since 3.6
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-input-hints}"
  (gtk-entry-input-hints entry))

(export 'gtk-entry-get-input-hints)

;;; --- End of file gtk.entry.lisp ---------------------------------------------
