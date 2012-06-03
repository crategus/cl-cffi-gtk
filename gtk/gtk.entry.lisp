;;; ----------------------------------------------------------------------------
;;; gtk.entry.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;;﻿
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkEntry
;;;                      +----GtkSpinButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkEntry implements AtkImplementorIface, GtkBuildable, GtkEditable and
;;; GtkCellEditable.
;;; 
;;; Properties
;;; 
;;;   "activates-default"         gboolean            : Read / Write
;;;   "buffer"                    GtkEntryBuffer*     : Read / Write / Construct
;;;   "caps-lock-warning"         gboolean            : Read / Write
;;;   "completion"                GtkEntryCompletion* : Read / Write
;;;   "cursor-position"           gint                : Read
;;;   "editable"                  gboolean            : Read / Write
;;;   "has-frame"                 gboolean            : Read / Write
;;;   "im-module"                 gchar*              : Read / Write
;;;   "inner-border"              GtkBorder*          : Read / Write
;;;   "invisible-char"            guint               : Read / Write
;;;   "invisible-char-set"        gboolean            : Read / Write
;;;   "max-length"                gint                : Read / Write
;;;   "overwrite-mode"            gboolean            : Read / Write
;;;   "placeholder-text"          gchar*              : Read / Write
;;;   "primary-icon-activatable"  gboolean            : Read / Write
;;;   "primary-icon-gicon"        GIcon*              : Read / Write
;;;   "primary-icon-name"         gchar*              : Read / Write
;;;   "primary-icon-pixbuf"       GdkPixbuf*          : Read / Write
;;;   "primary-icon-sensitive"    gboolean            : Read / Write
;;;   "primary-icon-stock"        gchar*              : Read / Write
;;;   "primary-icon-storage-type" GtkImageType        : Read
;;;   "primary-icon-tooltip-markup" gchar*            : Read / Write
;;;   "primary-icon-tooltip-text" gchar*              : Read / Write
;;;   "progress-fraction"         gdouble             : Read / Write
;;;   "progress-pulse-step"       gdouble             : Read / Write
;;;   "scroll-offset"             gint                : Read
;;;   "secondary-icon-activatable" gboolean           : Read / Write
;;;   "secondary-icon-gicon"      GIcon*              : Read / Write
;;;   "secondary-icon-name"       gchar*              : Read / Write
;;;   "secondary-icon-pixbuf"     GdkPixbuf*          : Read / Write
;;;   "secondary-icon-sensitive"  gboolean            : Read / Write
;;;   "secondary-icon-stock"      gchar*              : Read / Write
;;;   "secondary-icon-storage-type" GtkImageType      : Read
;;;   "secondary-icon-tooltip-markup" gchar*          : Read / Write
;;;   "secondary-icon-tooltip-text" gchar*            : Read / Write
;;;   "selection-bound"           gint                : Read
;;;   "shadow-type"               GtkShadowType       : Read / Write
;;;   "text"                      gchar*              : Read / Write
;;;   "text-length"               guint               : Read
;;;   "truncate-multiline"        gboolean            : Read / Write
;;;   "visibility"                gboolean            : Read / Write
;;;   "width-chars"               gint                : Read / Write
;;;   "xalign"                    gfloat              : Read / Write
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
;;; 
;;; Description
;;; 
;;; The GtkEntry widget is a single line text entry widget. A fairly large set
;;; of key bindings are supported by default. If the entered text is longer than
;;; the allocation of the widget, the widget will scroll so that the cursor 
;;; position is visible.
;;; 
;;; When using an entry for passwords and other sensitive information, it can be 
;;; put into "password mode" using gtk_entry_set_visibility(). In this mode, 
;;; entered  text is displayed using a 'invisible' character. By default, GTK+ 
;;; picks the best invisible character that is available in the current font, 
;;; but it can be changed with gtk_entry_set_invisible_char(). Since 2.16, GTK+ 
;;; displays a warning when Caps Lock or input methods might interfere with 
;;; entering text in a password entry. The warning can be turned off with the 
;;; "caps-lock-warning" property.
;;; 
;;; Since 2.16, GtkEntry has the ability to display progress or activity 
;;; information behind the text. To make an entry display such information, use 
;;; gtk_entry_set_progress_fraction() or gtk_entry_set_progress_pulse_step().
;;; 
;;; Additionally, GtkEntry can show icons at either side of the entry. These 
;;; icons can be activatable by clicking, can be set up as drag source and can 
;;; have tooltips. To add an icon, use gtk_entry_set_icon_from_gicon() or one of 
;;; the various other functions that set an icon from a stock id, an icon name 
;;; or a pixbuf. To trigger an action when the user clicks an icon, connect to 
;;; the "icon-press" signal. To allow DND operations from an icon, use 
;;; gtk_entry_set_icon_drag_source(). To set a tooltip on an icon, use 
;;; gtk_entry_set_icon_tooltip_text() or the corresponding function for markup.
;;; 
;;; Note that functionality or information that is only available by clicking on 
;;; an icon in an entry may not be accessible at all to users which are not able 
;;; to use a mouse or other pointing device. It is therefore recommended that 
;;; any such functionality should also be available by other means, e.g. via the 
;;; context menu of the entry.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activates-default" property
;;; 
;;;   "activates-default"        gboolean              : Read / Write
;;; 
;;; Whether to activate the default widget (such as the default button in a
;;; dialog) when Enter is pressed.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "buffer" property
;;; 
;;;   "buffer"                   GtkEntryBuffer*      : Read / Write / Construct
;;; 
;;; Text buffer object which actually stores entry text.
;;;
;;; ----------------------------------------------------------------------------
;;; The "caps-lock-warning" property
;;; 
;;;   "caps-lock-warning"        gboolean              : Read / Write
;;; 
;;; Whether password entries will show a warning when Caps Lock is on.
;;; 
;;; Note that the warning is shown using a secondary icon, and thus does not
;;; work if you are using the secondary icon position for some other purpose.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "completion" property
;;; 
;;;   "completion"               GtkEntryCompletion*   : Read / Write
;;; 
;;; The auxiliary completion object to use with the entry.
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-position" property
;;; 
;;;   "cursor-position"          gint                  : Read
;;; 
;;; The current position of the insertion cursor in chars.
;;; 
;;; Allowed values: [0,65535]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "editable" property
;;; 
;;;   "editable"                 gboolean              : Read / Write
;;; 
;;; Whether the entry contents can be edited.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-frame" property
;;; 
;;;   "has-frame"                gboolean              : Read / Write
;;; 
;;; FALSE removes outside bevel from entry.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "im-module" property
;;; 
;;;   "im-module"                gchar*                : Read / Write
;;; 
;;; Which IM (input method) module should be used for this entry. See
;;; GtkIMContext.
;;; 
;;; Setting this to a non-NULL value overrides the system-wide IM module
;;; setting. See the GtkSettings "gtk-im-module" property.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "inner-border" property
;;; 
;;;   "inner-border"             GtkBorder*            : Read / Write
;;; 
;;; Warning
;;; 
;;; GtkEntry:inner-border has been deprecated since version 3.4 and should not
;;; be used in newly-written code. Use the standard border and padding CSS
;;; properties (through objects like GtkStyleContext and GtkCssProvider); the
;;; value of this style property is ignored.
;;; 
;;; Sets the text area's border between the text and the frame.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "invisible-char" property
;;; 
;;;   "invisible-char"           guint                 : Read / Write
;;; 
;;; The invisible character is used when masking entry contents (in \"password
;;; mode\")"). When it is not explicitly set with the "invisible-char" property,
;;; GTK+ determines the character to use from a list of possible candidates,
;;; depending on availability in the current font.
;;; 
;;; This style property allows the theme to prepend a character to the list of
;;; candidates.
;;; 
;;; Default value: '*'
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "invisible-char-set" property
;;; 
;;;   "invisible-char-set"       gboolean              : Read / Write
;;; 
;;; Whether the invisible char has been set for the GtkEntry.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-length" property
;;; 
;;;   "max-length"               gint                  : Read / Write
;;; 
;;; Maximum number of characters for this entry. Zero if no maximum.
;;; 
;;; Allowed values: [0,65535]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "overwrite-mode" property
;;; 
;;;   "overwrite-mode"           gboolean              : Read / Write
;;; 
;;; If text is overwritten when typing in the GtkEntry.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "placeholder-text" property
;;; 
;;;   "placeholder-text"         gchar*                : Read / Write
;;; 
;;; The text that will be displayed in the GtkEntry when it is empty and
;;; unfocused.
;;; 
;;; Default value: NULL
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-activatable" property
;;; 
;;;   "primary-icon-activatable" gboolean              : Read / Write
;;; 
;;; Whether the primary icon is activatable.
;;; 
;;; GTK+ emits the "icon-press" and "icon-release" signals only on sensitive,
;;; activatable icons.
;;; 
;;; Sensitive, but non-activatable icons can be used for purely informational
;;; purposes.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-gicon" property
;;; 
;;;   "primary-icon-gicon"       GIcon*                : Read / Write
;;; 
;;; The GIcon to use for the primary icon for the entry.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-name" property
;;; 
;;;   "primary-icon-name"        gchar*                : Read / Write
;;; 
;;; The icon name to use for the primary icon for the entry.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-pixbuf" property
;;; 
;;;   "primary-icon-pixbuf"      GdkPixbuf*            : Read / Write
;;; 
;;; A pixbuf to use as the primary icon for the entry.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-sensitive" property
;;; 
;;;   "primary-icon-sensitive"   gboolean              : Read / Write
;;; 
;;; Whether the primary icon is sensitive.
;;; 
;;; An insensitive icon appears grayed out. GTK+ does not emit the "icon-press"
;;; and "icon-release" signals and does not allow DND from insensitive icons.
;;; 
;;; An icon should be set insensitive if the action that would trigger when
;;; clicked is currently not available.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-stock" property
;;; 
;;;   "primary-icon-stock"       gchar*                : Read / Write
;;; 
;;; The stock id to use for the primary icon for the entry.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-storage-type" property
;;; 
;;;   "primary-icon-storage-type" GtkImageType          : Read
;;; 
;;; The representation which is used for the primary icon of the entry.
;;; 
;;; Default value: GTK_IMAGE_EMPTY
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-tooltip-markup" property
;;; 
;;;   "primary-icon-tooltip-markup" gchar*                : Read / Write
;;; 
;;; The contents of the tooltip on the primary icon, which is marked up with
;;; the Pango text markup language.
;;; 
;;; Also see gtk_entry_set_icon_tooltip_markup().
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "primary-icon-tooltip-text" property
;;; 
;;;   "primary-icon-tooltip-text" gchar*                : Read / Write
;;; 
;;; The contents of the tooltip on the primary icon.
;;; 
;;; Also see gtk_entry_set_icon_tooltip_text().
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "progress-fraction" property
;;; 
;;;   "progress-fraction"        gdouble               : Read / Write
;;; 
;;; The current fraction of the task that's been completed.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "progress-pulse-step" property
;;; 
;;;   "progress-pulse-step"      gdouble               : Read / Write
;;; 
;;; The fraction of total entry width to move the progress bouncing block for
;;; each call to gtk_entry_progress_pulse().
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.1
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-offset" property
;;; 
;;;   "scroll-offset"            gint                  : Read
;;; 
;;; Number of pixels of the entry scrolled off the screen to the left.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-activatable" property
;;; 
;;;   "secondary-icon-activatable" gboolean              : Read / Write
;;; 
;;; Whether the secondary icon is activatable.
;;; 
;;; GTK+ emits the "icon-press" and "icon-release" signals only on sensitive,
;;; activatable icons.
;;; 
;;; Sensitive, but non-activatable icons can be used for purely informational
;;; purposes.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-gicon" property
;;; 
;;;   "secondary-icon-gicon"     GIcon*                : Read / Write
;;; 
;;; The GIcon to use for the secondary icon for the entry.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-name" property
;;; 
;;;   "secondary-icon-name"      gchar*                : Read / Write
;;; 
;;; The icon name to use for the secondary icon for the entry.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-pixbuf" property
;;; 
;;;   "secondary-icon-pixbuf"    GdkPixbuf*            : Read / Write
;;; 
;;; An pixbuf to use as the secondary icon for the entry.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-sensitive" property
;;; 
;;;   "secondary-icon-sensitive" gboolean              : Read / Write
;;; 
;;; Whether the secondary icon is sensitive.
;;; 
;;; An insensitive icon appears grayed out. GTK+ does not emit the "icon-press"
;;; and "icon-release" signals and does not allow DND from insensitive icons.
;;; 
;;; An icon should be set insensitive if the action that would trigger when
;;; clicked is currently not available.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-stock" property
;;; 
;;;   "secondary-icon-stock"     gchar*                : Read / Write
;;; 
;;; The stock id to use for the secondary icon for the entry.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-storage-type" property
;;; 
;;;   "secondary-icon-storage-type" GtkImageType          : Read
;;; 
;;; The representation which is used for the secondary icon of the entry.
;;; 
;;; Default value: GTK_IMAGE_EMPTY
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-tooltip-markup" property
;;; 
;;;   "secondary-icon-tooltip-markup" gchar*                : Read / Write
;;; 
;;; The contents of the tooltip on the secondary icon, which is marked up with
;;; the Pango text markup language.
;;; 
;;; Also see gtk_entry_set_icon_tooltip_markup().
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-icon-tooltip-text" property
;;; 
;;;   "secondary-icon-tooltip-text" gchar*                : Read / Write
;;; 
;;; The contents of the tooltip on the secondary icon.
;;; 
;;; Also see gtk_entry_set_icon_tooltip_text().
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-bound" property
;;; 
;;;   "selection-bound"          gint                  : Read
;;; 
;;; The position of the opposite end of the selection from the cursor in chars.
;;; 
;;; Allowed values: [0,65535]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read / Write
;;; 
;;; Which kind of shadow to draw around the entry when "has-frame" is set to
;;; TRUE.
;;; 
;;; Default value: GTK_SHADOW_IN
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; The contents of the entry.
;;; 
;;; Default value: ""
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-length" property
;;; 
;;;   "text-length"              guint                 : Read
;;; 
;;; The length of the text in the GtkEntry.
;;; 
;;; Allowed values: <= 65535
;;; 
;;; Default value: 0
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "truncate-multiline" property
;;; 
;;;   "truncate-multiline"       gboolean              : Read / Write
;;; 
;;; When TRUE, pasted multi-line text is truncated to the first line.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "visibility" property
;;; 
;;;   "visibility"               gboolean              : Read / Write
;;; 
;;; FALSE displays the "invisible char" instead of the actual text (password
;;; mode).
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-chars" property
;;; 
;;;   "width-chars"              gint                  : Read / Write
;;; 
;;; Number of characters to leave space for in the entry.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "xalign" property
;;; 
;;;   "xalign"                   gfloat                : Read / Write
;;; 
;;; The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
;;; layouts.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-prelight" style property
;;; 
;;;   "icon-prelight"            gboolean              : Read
;;; 
;;; The prelight style property determines whether activatable icons prelight
;;; on mouseover.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "inner-border" style property
;;; 
;;;   "inner-border"             GtkBorder*            : Read
;;; 
;;; Warning
;;; 
;;; GtkEntry:inner-border has been deprecated since version 3.4 and should not
;;; be used in newly-written code. Use the standard border and padding CSS
;;; properties (through objects like GtkStyleContext and GtkCssProvider); the
;;; value of this style property is ignored.
;;; 
;;; Sets the text area's border between the text and the frame.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "invisible-char" style property
;;; 
;;;   "invisible-char"           guint                 : Read
;;; 
;;; The invisible character is used when masking entry contents (in \"password
;;; mode\")"). When it is not explicitly set with the "invisible-char" property,
;;; GTK+ determines the character to use from a list of possible candidates,
;;; depending on availability in the current font.
;;; 
;;; This style property allows the theme to prepend a character to the list of
;;; candidates.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "progress-border" style property
;;; 
;;;   "progress-border"          GtkBorder*            : Read
;;; 
;;; Warning
;;; 
;;; GtkEntry:progress-border has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use the standard margin CSS property
;;; (through objects like GtkStyleContext and GtkCssProvider); the value of this
;;; style property is ignored.
;;; 
;;; The border around the progress bar in the entry.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user activates the entry.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control activation programmatically.
;;; 
;;; The default bindings for this signal are all forms of the Enter key.
;;; 
;;; entry :
;;;     The entry on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "backspace" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::backspace signal is a keybinding signal which gets emitted when the
;;; user asks for it.
;;; 
;;; The default bindings for this signal are Backspace and Shift-Backspace.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "copy-clipboard" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::copy-clipboard signal is a keybinding signal which gets emitted to
;;; copy the selection to the clipboard.
;;; 
;;; The default bindings for this signal are Ctrl-c and Ctrl-Insert.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cut-clipboard" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::cut-clipboard signal is a keybinding signal which gets emitted to cut
;;; the selection to the clipboard.
;;; 
;;; The default bindings for this signal are Ctrl-x and Shift-Delete.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "delete-from-cursor" signal
;;; 
;;; void user_function (GtkEntry     *entry,
;;;                     GtkDeleteType type,
;;;                     gint          count,
;;;                     gpointer      user_data)      : Action
;;; 
;;; The ::delete-from-cursor signal is a keybinding signal which gets emitted
;;; when the user initiates a text deletion.
;;; 
;;; If the type is GTK_DELETE_CHARS, GTK+ deletes the selection if there is one,
;;; otherwise it deletes the requested number of characters.
;;; 
;;; The default bindings for this signal are Delete for deleting a character and
;;; Ctrl-Delete for deleting a word.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; type :
;;;     the granularity of the deletion, as a GtkDeleteType
;;; 
;;; count :
;;;     the number of type units to delete
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-press" signal
;;; 
;;; void user_function (GtkEntry            *entry,
;;;                     GtkEntryIconPosition icon_pos,
;;;                     GdkEvent            *event,
;;;                     gpointer             user_data)      : Run Last
;;; 
;;; The ::icon-press signal is emitted when an activatable icon is clicked.
;;; 
;;; entry :
;;;     The entry on which the signal is emitted
;;; 
;;; icon_pos :
;;;     The position of the clicked icon
;;; 
;;; event :
;;;     the button press event
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "icon-release" signal
;;; 
;;; void user_function (GtkEntry            *entry,
;;;                     GtkEntryIconPosition icon_pos,
;;;                     GdkEvent            *event,
;;;                     gpointer             user_data)      : Run Last
;;; 
;;; The ::icon-release signal is emitted on the button release from a mouse
;;; click over an activatable icon.
;;; 
;;; entry :
;;;     The entry on which the signal is emitted
;;; 
;;; icon_pos :
;;;     The position of the clicked icon
;;; 
;;; event :
;;;     the button release event
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.16
;;;
;;; ----------------------------------------------------------------------------
;;; The "insert-at-cursor" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gchar    *string,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::insert-at-cursor signal is a keybinding signal which gets emitted when
;;; the user initiates the insertion of a fixed string at the cursor.
;;; 
;;; This signal has no default bindings.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; string :
;;;     the string to insert
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-cursor" signal
;;; 
;;; void user_function (GtkEntry       *entry,
;;;                     GtkMovementStep step,
;;;                     gint            count,
;;;                     gboolean        extend_selection,
;;;                     gpointer        user_data)             : Action
;;; 
;;; The ::move-cursor signal is a keybinding signal which gets emitted when the
;;; user initiates a cursor movement. If the cursor is not visible in entry,
;;; this signal causes the viewport to be moved instead.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control the cursor programmatically.
;;; 
;;; The default bindings for this signal come in two variants, the variant with
;;; the Shift modifier extends the selection, the variant without the Shift
;;; modifer does not. There are too many key combinations to list them all here.
;;; 
;;;     Arrow keys move by individual characters/lines
;;;     Ctrl-arrow key combinations move by words/paragraphs
;;;     Home/End keys move to the ends of the buffer
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; step :
;;;     the granularity of the move, as a GtkMovementStep
;;; 
;;; count :
;;;     the number of step units to move
;;; 
;;; extend_selection :
;;;     TRUE if the move should extend the selection
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "paste-clipboard" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::paste-clipboard signal is a keybinding signal which gets emitted to
;;; paste the contents of the clipboard into the text view.
;;; 
;;; The default bindings for this signal are Ctrl-v and Shift-Insert.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "populate-popup" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     GtkMenu  *menu,
;;;                     gpointer  user_data)      : Run Last
;;; 
;;; The ::populate-popup signal gets emitted before showing the context menu of
;;; the entry.
;;; 
;;; If you need to add items to the context menu, connect to this signal and
;;; append your menuitems to the menu.
;;; 
;;; entry :
;;;     The entry on which the signal is emitted
;;; 
;;; menu :
;;;     the menu that is being populated
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "preedit-changed" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gchar    *preedit,
;;;                     gpointer  user_data)      : Action
;;; 
;;; If an input method is used, the typed text will not immediately be committed
;;; to the buffer. So if you are interested in the text, connect to this signal.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; preedit :
;;;     the current preedit string
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-overwrite" signal
;;; 
;;; void user_function (GtkEntry *entry,
;;;                     gpointer  user_data)      : Action
;;; 
;;; The ::toggle-overwrite signal is a keybinding signal which gets emitted to
;;; toggle the overwrite mode of the entry.
;;; 
;;; The default bindings for this signal is Insert.
;;; 
;;; entry :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEntry
;;; 
;;; struct GtkEntry;
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
    "xalign" "gfloat" t t)
   (:cffi layout
          gtk-entry-layout g-object
          "gtk_entry_get_layout" nil)
   (:cffi cursor-hadjustment
          gtk-entry-cursor-hadjustment (g-object gtk-adjustment)
          "gtk_entry_get_cursor_hadjustment" "gtk_entry_set_cursor_hadjustment")
   (:cffi layout-offset
          gtk-entry-layout-offset nil
          gtk-entry-get-layout-offset nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new ()
;;; 
;;; GtkWidget * gtk_entry_new (void);
;;; 
;;; Creates a new entry.
;;; 
;;; Returns :
;;;     a new GtkEntry.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new))

(defun gtk-entry-new ()
  (make-instance 'gtk-entry))

(export 'gtk-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new_with_buffer ()
;;; 
;;; GtkWidget * gtk_entry_new_with_buffer (GtkEntryBuffer *buffer);
;;; 
;;; Creates a new entry with the specified text buffer.
;;; 
;;; buffer :
;;;     The buffer to use for the new GtkEntry.
;;; 
;;; Returns :
;;;     a new GtkEntry
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new-with-buffer))

(defun gtk-entry-new-with-buffer (buffer)
  (make-instance 'gtk-entry
                 :buffer buffer))

(export 'gtk-entry-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_buffer ()
;;; 
;;; GtkEntryBuffer * gtk_entry_get_buffer (GtkEntry *entry);
;;; 
;;; Get the GtkEntryBuffer object which holds the text for this widget.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     A GtkEntryBuffer object
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-buffer))

(defun gtk-entry-get-buffer (entry)
  (gtk-entry-buffer entry))

(export 'gtk-entry-get-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_buffer ()
;;; 
;;; void gtk_entry_set_buffer (GtkEntry *entry, GtkEntryBuffer *buffer);
;;; 
;;; Set the GtkEntryBuffer object which holds the text for this widget.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; buffer :
;;;     a GtkEntryBuffer
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-buffer))

(defun gtk-entry-set-buffer (entry buffer)
  (setf (gtk-entry-buffer entry) buffer))

(export 'gtk-entry-set-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_text ()
;;; 
;;; void gtk_entry_set_text (GtkEntry *entry, const gchar *text);
;;; 
;;; Sets the text in the widget to the given value, replacing the current
;;; contents.
;;; 
;;; See gtk_entry_buffer_set_text().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; text :
;;;     the new text
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-text))

(defun gtk-entry-set-text (entry text)
  (setf (gtk-entry-text entry) text))

(export 'gtk-entry-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text ()
;;; 
;;; const gchar * gtk_entry_get_text (GtkEntry *entry);
;;; 
;;; Retrieves the contents of the entry widget. See also
;;; gtk_editable_get_chars().
;;; 
;;; This is equivalent to:
;;; 
;;; gtk_entry_buffer_get_text (gtk_entry_get_buffer (entry));
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     a pointer to the contents of the widget as a string. This string points
;;;     to internally allocated storage in the widget and must not be freed,
;;;     modified or stored.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-text))

(defun gtk-entry-get-text (entry)
  (gtk-entry-text entry))

(export 'gtk-entry-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_length ()
;;; 
;;; guint16 gtk_entry_get_text_length (GtkEntry *entry);
;;; 
;;; Retrieves the current length of the text in entry.
;;; 
;;; This is equivalent to:
;;; 
;;; gtk_entry_buffer_get_length (gtk_entry_get_buffer (entry));
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the current number of characters in GtkEntry, or 0 if there are none.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-text-length))

(defun gtk-entry-get-text-length (entry)
  (gtk-entry-text-length entry))

(export 'gtk-entry-get-text-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_area ()
;;; 
;;; void gtk_entry_get_text_area (GtkEntry *entry, GdkRectangle *text_area);
;;; 
;;; Gets the area where the entry's text is drawn. This function is useful when
;;; drawing something to the entry in a draw callback.
;;; 
;;; If the entry is not realized, text_area is filled with zeros.
;;; 
;;; See also gtk_entry_get_icon_area().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; text_area :
;;;     Return location for the text area.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_text_area" %gtk-entry-get-text-area) :void
  (entry (g-object gtk-entry))
  (text-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-text-area (entry)
  (let ((text-area (make-gdk-rectangle)))
    (%gtk-entry-get-text-area entry text-area)
    text-area))

(export 'gtk-entry-get-text-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_visibility ()
;;; 
;;; void gtk_entry_set_visibility (GtkEntry *entry, gboolean visible);
;;; 
;;; Sets whether the contents of the entry are visible or not. When visibility
;;; is set to FALSE, characters are displayed as the invisible char, and will
;;; also appear that way when the text in the entry widget is copied elsewhere.
;;; 
;;; By default, GTK+ picks the best invisible character available in the current
;;; font, but it can be changed with gtk_entry_set_invisible_char().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; visible :
;;;     TRUE if the contents of the entry are displayed as plaintext
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-visibility))

(defun gtk-entry-set-visibility (entry visible)
  (setf (gtk-entry-visibility entry) visible))

(export 'gtk-entry-set-visibility)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_invisible_char ()
;;; 
;;; void gtk_entry_set_invisible_char (GtkEntry *entry, gunichar ch);
;;; 
;;; Sets the character to use in place of the actual text when
;;; gtk_entry_set_visibility() has been called to set text visibility to FALSE.
;;; i.e. this is the character used in "password mode" to show the user how many
;;; characters have been typed. By default, GTK+ picks the best invisible char
;;; available in the current font. If you set the invisible char to 0, then the
;;; user will get no feedback at all; there will be no text on the screen as
;;; they type.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; ch :
;;;     a Unicode character
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-invisible-char))

(defun gtk-entry-set-invisible-char (entry ch)
  (setf (gtk-entry-invisible-char-set entry) t
        (gtk-entry-invisible-char entry) ch))

(export 'gtk-entry-set-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_unset_invisible_char ()
;;; 
;;; void gtk_entry_unset_invisible_char (GtkEntry *entry);
;;; 
;;; Unsets the invisible char previously set with
;;; gtk_entry_set_invisible_char(). So that the default invisible char is used
;;; again.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-unset-invisible-char))

(defun gtk-entry-unset-invisible-char (entry)
  (setf (gtk-entry-invisible-char-set entry) nil))

(export 'gtk-entry-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_max_length ()
;;; 
;;; void gtk_entry_set_max_length (GtkEntry *entry, gint max);
;;; 
;;; Sets the maximum allowed length of the contents of the widget. If the
;;; current contents are longer than the given length, then they will be
;;; truncated to fit.
;;; 
;;; This is equivalent to:
;;; 
;;; gtk_entry_buffer_set_max_length (gtk_entry_get_buffer (entry), max);
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; max :
;;;     the maximum length of the entry, or 0 for no maximum. (other than the
;;;     maximum length of entries.) The value passed in will be clamped to the
;;;     range 0-65536.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-max-length))

(defun gtk-entry-set-max-length (entry max)
  (setf (gtk-entry-max-length entry) max))

(export 'gtk-entry-set-max-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_activates_default ()
;;; 
;;; gboolean gtk_entry_get_activates_default (GtkEntry *entry);
;;; 
;;; Retrieves the value set by gtk_entry_set_activates_default().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     TRUE if the entry will activate the default widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-activates-default))

(defun gtk-entry-get-activates-default (entry)
  (gtk-entry-activates-default entry))

(export 'gtk-entry-get-activates-default)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_has_frame ()
;;; 
;;; gboolean gtk_entry_get_has_frame (GtkEntry *entry);
;;; 
;;; Gets the value set by gtk_entry_set_has_frame().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     whether the entry has a beveled frame
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-has-frame))

(defun gtk-entry-get-has-frame (entry)
  (gtk-entry-has-frame entry))

(export 'gtk-entry-get-has-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_inner_border ()
;;; 
;;; const GtkBorder * gtk_entry_get_inner_border (GtkEntry *entry);
;;; 
;;; Warning
;;; 
;;; gtk_entry_get_inner_border has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use the standard border and padding CSS
;;; properties (through objects like GtkStyleContext and GtkCssProvider); the
;;; value returned by this function is ignored by GtkEntry.
;;; 
;;; This function returns the entry's "inner-border" property. See
;;; gtk_entry_set_inner_border() for more information.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the entry's GtkBorder, or NULL if none was set
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-inner-border))

(defun gtk-entry-get-inner-border (entry)
  (gtk-entry-inner-border entry))

(export 'gtk-entry-get-inner-border)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_width_chars ()
;;; 
;;; gint gtk_entry_get_width_chars (GtkEntry *entry);
;;; 
;;; Gets the value set by gtk_entry_set_width_chars().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     number of chars to request space for, or negative if unset
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-width-chars))

(defun gtk-entry-get-width-chars (entry)
  (gtk-entry-width-chars entry))

(export 'gtk-entry-get-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_activates_default ()
;;; 
;;; void gtk_entry_set_activates_default (GtkEntry *entry, gboolean setting);
;;; 
;;; If setting is TRUE, pressing Enter in the entry will activate the default
;:; widget for the window containing the entry. This usually means that the
;;; dialog box containing the entry will be closed, since the default widget is
;;; usually one of the dialog buttons.
;;; 
;;; (For experts: if setting is TRUE, the entry calls
;;; gtk_window_activate_default() on the window containing the entry, in the
;;; default handler for the "activate" signal.)
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; setting :
;;;     TRUE to activate window's default widget on Enter keypress
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-activates-default))

(defun gtk-entry-set-activates-default (entry setting)
  (setf (gtk-entry-activates-default entry) setting))

(export 'gtk-entry-set-activates-default)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_has_frame ()
;;; 
;;; void gtk_entry_set_has_frame (GtkEntry *entry, gboolean setting);
;;; 
;;; Sets whether the entry has a beveled frame around it.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; setting :
;;;     new value
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-inner-border))

(defun gtk-entry-set-has-frame (entry setting)
  (setf (gtk-entry-has-frame entry) setting))

(export 'gtk-entry-set-has-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_inner_border ()
;;; 
;;; void gtk_entry_set_inner_border (GtkEntry *entry, const GtkBorder *border);
;;; 
;;; Warning
;;; 
;;; gtk_entry_set_inner_border has been deprecated since version 3.4 and should
;;; not be used in newly-written code. Use the standard border and padding CSS
;;; properties (through objects like GtkStyleContext and GtkCssProvider); the
;;; value set with this function is ignored by GtkEntry.
;;; 
;;; Sets entry's inner-border property to border, or clears it if NULL is
;;; passed. The inner-border is the area around the entry's text, but inside its
;;; frame.
;;; 
;;; If set, this property overrides the inner-border style property. Overriding
;;; the style-provided border is useful when you want to do in-place editing of
;;; some text in a canvas or list widget, where pixel-exact positioning of the
;;; entry is important.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; border :
;;;     a GtkBorder, or NULL
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-inner-border))

(defun gtk-entry-set-inner-border (entry border)
  (setf (gtk-entry-inner-border entry) border))

(export 'gtk-entry-set-inner-border)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_width_chars ()
;;; 
;;; void gtk_entry_set_width_chars (GtkEntry *entry, gint n_chars);
;;; 
;;; Changes the size request of the entry to be about the right size for n_chars
;;; characters. Note that it changes the size request, the size can still be
;;; affected by how you pack the widget into containers. If n_chars is -1, the
;;; size reverts to the default entry size.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; n_chars :
;;;     width in chars
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-width-chars))

(defun gtk-entry-set-width-chars (entry n-chars)
  (setf (gtk-entry-width-chars entry) n-chars))

(export 'gtk-entry-set-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_invisible_char ()
;;; 
;;; gunichar gtk_entry_get_invisible_char (GtkEntry *entry);
;;; 
;;; Retrieves the character displayed in place of the real characters for
;;; entries with visibility set to false. See gtk_entry_set_invisible_char().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the current invisible char, or 0, if the entry does not show invisible
;;;     text at all.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-invisible-char))

(defun gtk-entry-get-invisible-char (entry)
  (gtk-entry-invisible-char entry))

(export 'gtk-entry-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_alignment ()
;;; 
;;; void gtk_entry_set_alignment (GtkEntry *entry, gfloat xalign);
;;; 
;;; Sets the alignment for the contents of the entry. This controls the
;;; horizontal positioning of the contents when the displayed text is shorter
;;; than the width of the entry.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; xalign :
;;;     The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
;;;     layouts
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-alignment))

(defun gtk-entry-set-alignment (entry xalign)
  (setf (gtk-entry-xalign entry) xalign))

(export 'gtk-entry-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_alignment ()
;;; 
;;; gfloat gtk_entry_get_alignment (GtkEntry *entry);
;;; 
;;; Gets the value set by gtk_entry_set_alignment().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the alignment
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-alignment))

(defun gtk-entry-get-alignment (entry)
  (gtk-entry-xalign entry))

(export 'gtk-entry-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_placeholder_text ()
;;; 
;;; void gtk_entry_set_placeholder_text (GtkEntry *entry, const gchar *text);
;;; 
;;; Sets text to be displayed in entry when it is empty and unfocused. This can
;;; be used to give a visual hint of the expected contents of the GtkEntry.
;;; 
;;; Note that since the placeholder text gets removed when the entry received
;;; focus, using this feature is a bit problematic if the entry is given the
;;; initial focus in a window. Sometimes this can be worked around by delaying
;;; the initial focus setting until the first key event arrives.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; text :
;;;     a string to be displayed when entry is empty an unfocused, or NULL
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-placeholder-text))

(defun gtk-entry-set-placeholder-text (entry text)
  (setf (gtk-entry-placeholder-text entry) text))

(export 'gtk-entry-set-placeholder-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_placeholder_text ()
;;; 
;;; const gchar * gtk_entry_get_placeholder_text (GtkEntry *entry);
;;; 
;;; Retrieves the text that will be displayed when entry is empty and unfocused.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     a pointer to the placeholder text as a string. This string points to
;;;     internally allocated storage in the widget and must not be freed,
;;;     modified or stored.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-placeholder-text))

(defun gtk-entry-get-placeholder-text (entry)
  (gtk-entry-placeholder-text entry))

(export 'gtk-entry-get-placeholder-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_overwrite_mode ()
;;; 
;;; void gtk_entry_set_overwrite_mode (GtkEntry *entry, gboolean overwrite);
;;; 
;;; Sets whether the text is overwritten when typing in the GtkEntry.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; overwrite :
;;;     new value
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-overwrite-mode))

(defun gtk-entry-set-overwrite-mode (entry overwrite)
  (setf (gtk-entry-overwrite-mode entry) overwrite))

(export 'gtk-entry-set-overwrite-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_overwrite_mode ()
;;; 
;;; gboolean gtk_entry_get_overwrite_mode (GtkEntry *entry);
;;; 
;;; Gets the value set by gtk_entry_set_overwrite_mode().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     whether the text is overwritten when typing.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-overwrite-mode))

(defun gtk-entry-get-overwrite-mode (entry)
  (gtk-entry-overwrite-mode entry))

(export 'gtk-entry-get-overwrite-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout ()
;;; 
;;; PangoLayout * gtk_entry_get_layout (GtkEntry *entry);
;;; 
;;; Gets the PangoLayout used to display the entry. The layout is useful to e.g.
;;; convert text positions to pixel positions, in combination with
;;; gtk_entry_get_layout_offsets(). The returned layout is owned by the entry
;;; and must not be modified or freed by the caller.
;;; 
;;; Keep in mind that the layout text may contain a preedit string, so
;;; gtk_entry_layout_index_to_text_index() and
;;; gtk_entry_text_index_to_layout_index() are needed to convert byte indices in
;;; the layout to byte indices in the entry contents.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the PangoLayout for this entry
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_layout" gtk-entry-get-layout) (g-object pango-layout)
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout_offsets ()
;;; 
;;; void gtk_entry_get_layout_offsets (GtkEntry *entry, gint *x, gint *y);
;;; 
;;; Obtains the position of the PangoLayout used to render text in the entry, in
;;; widget coordinates. Useful if you want to line up the text in an entry with
;;; some other text, e.g. when using the entry to implement editable cells in a
;;; sheet widget.
;;; 
;;; Also useful to convert mouse events into coordinates inside the PangoLayout,
;;; e.g. to take some action if some part of the entry text is clicked.
;;; 
;;; Note that as the user scrolls around in the entry the offsets will change;
;;; you'll need to connect to the "notify::scroll-offset" signal to track this.
;;; Remember when using the PangoLayout functions you need to convert to and
;;; from pixels using PANGO_PIXELS() or PANGO_SCALE.
;;; 
;;; Keep in mind that the layout text may contain a preedit string, so
;;; gtk_entry_layout_index_to_text_index() and
;;; gtk_entry_text_index_to_layout_index() are needed to convert byte indices in
;;; the layout to byte indices in the entry contents.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; x :
;;;     location to store X offset of layout, or NULL
;;; 
;;; y :
;;;     location to store Y offset of layout, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_layout_offsets" %gtk-entry-get-layout-offsets) :void
  (entry (g-object entry))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-entry-get-layout-offset (entry)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-entry-get-layout-offsets entry x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-entry-get-layout-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_layout_index_to_text_index ()
;;; 
;;; gint gtk_entry_layout_index_to_text_index (GtkEntry *entry,
;;;                                            gint layout_index);
;;; 
;;; Converts from a position in the entry contents (returned by
;;; gtk_entry_get_text()) to a position in the entry's PangoLayout (returned by
;;; gtk_entry_get_layout(), with text retrieved via pango_layout_get_text()).
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; layout_index :
;;;     byte index into the entry layout text
;;; 
;;; Returns :
;;;     byte index into the entry contents
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_layout_index_to_text_index"
          gtk-entry-layout-index-to-text-index) :int
  (entry (g-object entry))
  (layout-index :int))

(export 'gtk-entry-layout-index-to-text-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_text_index_to_layout_index ()
;;; 
;;; gint gtk_entry_text_index_to_layout_index (GtkEntry *entry, gint text_index)
;;; 
;;; Converts from a position in the entry's PangoLayout (returned by
;;; gtk_entry_get_layout()) to a position in the entry contents (returned by
;;; gtk_entry_get_text()).
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; text_index :
;;;     byte index into the entry contents
;;; 
;;; Returns :
;;;     byte index into the entry layout text
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_text_index_to_layout_index"
          gtk-entry-text-index-to-layout-index) :int
  (entry (g-object entry))
  (text-index :int))

(export 'gtk-entry-text-index-to-layout-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_max_length ()
;;; 
;;; gint gtk_entry_get_max_length (GtkEntry *entry);
;;; 
;;; Retrieves the maximum allowed length of the text in entry. See
;;; gtk_entry_set_max_length().
;;; 
;;; This is equivalent to:
;;; 
;;; gtk_entry_buffer_get_max_length (gtk_entry_get_buffer (entry));
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the maximum allowed number of characters in GtkEntry, or 0 if there is
;;;     no maximum.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-max-length))

(defun gtk-entry-get-max-length (entry)
  (gtk-entry-max-length entry))

(export 'gtk-entry-get-max-length)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_visibility ()
;;; 
;;; gboolean gtk_entry_get_visibility (GtkEntry *entry);
;;; 
;;; Retrieves whether the text in entry is visible. See
;;; gtk_entry_set_visibility().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     TRUE if the text is currently visible
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-visibility))

(defun gtk-entry-get-visibility (entry)
  (gtk-entry-visibility entry))

(export 'gtk-entry-get-visibility)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_completion ()
;;; 
;;; void gtk_entry_set_completion (GtkEntry *entry,
;;;                                GtkEntryCompletion *completion);
;;; 
;;; Sets completion to be the auxiliary completion object to use with entry. All
;;; further configuration of the completion mechanism is done on completion
;;; using the GtkEntryCompletion API. Completion is disabled if completion is
;;; set to NULL.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; completion :
;;;     The GtkEntryCompletion or NULL
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-completion))

(defun gtk-entry-set-completion (entry completion)
  (setf (gtk-entry-completion entry) completion))

(export 'gtk-entry-set-completion)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_completion ()
;;; 
;;; GtkEntryCompletion * gtk_entry_get_completion (GtkEntry *entry);
;;; 
;;; Returns the auxiliary completion object currently in use by entry.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; Returns :
;;;     The auxiliary completion object currently in use by entry.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-completion))

(defun gtk-entry-get-completion (entry)
  (gtk-entry-completion entry))

(export 'gtk-entry-get-completion)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_cursor_hadjustment ()
;;; 
;;; void gtk_entry_set_cursor_hadjustment (GtkEntry *entry,
;;;                                        GtkAdjustment *adjustment);
;;; 
;;; Hooks up an adjustment to the cursor position in an entry, so that when the
;;; cursor is moved, the adjustment is scrolled to show that position. See
;;; gtk_scrolled_window_get_hadjustment() for a typical way of obtaining the
;;; adjustment.
;;; 
;;; The adjustment has to be in pixel units and in the same coordinate system as
;;; the entry.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; adjustment :
;;;     an adjustment which should be adjusted when the cursor is moved, or NULL
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_cursor_hadjustment" gtk-entry-set-cursor-hadjustment)
    :void
  (entry (g-object gtk-entry))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-entry-set-cursor-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_cursor_hadjustment ()
;;; 
;;; GtkAdjustment * gtk_entry_get_cursor_hadjustment (GtkEntry *entry);
;;; 
;;; Retrieves the horizontal cursor adjustment for the entry. See
;;; gtk_entry_set_cursor_hadjustment().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     the horizontal cursor adjustment, or NULL if none has been set
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_cursor_hadjustment" gtk-entry-get-cursor-hadjustment)
    (g-object gtk-adjustment)
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-cursor-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_progress_fraction ()
;;; 
;;; void gtk_entry_set_progress_fraction (GtkEntry *entry, gdouble fraction);
;;; 
;;; Causes the entry's progress indicator to "fill in" the given fraction of the
;;; bar. The fraction should be between 0.0 and 1.0, inclusive.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; fraction :
;;;     fraction of the task that's been completed
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-progress-fraction))

(defun gtk-entry-set-progress-fraction (entry fraction)
  (setf (gtk-entry-progress-fraction entry) fraction))

(export 'gtk-entry-set-progress-fraction)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_progress_fraction ()
;;; 
;;; gdouble gtk_entry_get_progress_fraction (GtkEntry *entry);
;;; 
;;; Returns the current fraction of the task that's been completed. See
;;; gtk_entry_set_progress_fraction().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     a fraction from 0.0 to 1.0
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-progress-fraction))

(defun gtk-entry-get-progress-fraction (entry)
  (gtk-entry-progress-fraction entry))

(export 'gtk-entry-get-progress-fraction)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_progress_pulse_step ()
;;; 
;;; void gtk_entry_set_progress_pulse_step (GtkEntry *entry, gdouble fraction);
;;; 
;;; Sets the fraction of total entry width to move the progress bouncing block
;;; for each call to gtk_entry_progress_pulse().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; fraction :
;;;     fraction between 0.0 and 1.0
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-progress-pulse-step))

(defun gtk-entry-set-progress-pulse-step (entry fraction)
  (setf (gtk-entry-progress-pulse-step entry) fraction))

(export 'gtk-entry-set-progress-pulse-step)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_progress_pulse_step ()
;;; 
;;; gdouble gtk_entry_get_progress_pulse_step (GtkEntry *entry);
;;; 
;;; Retrieves the pulse step set with gtk_entry_set_progress_pulse_step().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Returns :
;;;     a fraction from 0.0 to 1.0
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-progress-pulse-step))

(defun gtk-entry-get-progress-pulse-step (entry)
  (gtk-entry-progress-pulse-step entry))

(export 'gtk-entry-get-progress-pulse-step)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_progress_pulse ()
;;; 
;;; void gtk_entry_progress_pulse (GtkEntry *entry);
;;; 
;;; Indicates that some progress is made, but you don't know how much. Causes
;;; the entry's progress indicator to enter "activity mode," where a block
;;; bounces back and forth. Each call to gtk_entry_progress_pulse() causes the
;;; block to move by a little bit (the amount of movement per pulse is
;;; determined by gtk_entry_set_progress_pulse_step()).
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_progress_pulse" gtk-entry-progress-pulse) :void
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
;;; 
;;; typedef enum {
;;;   GTK_ENTRY_ICON_PRIMARY,
;;;   GTK_ENTRY_ICON_SECONDARY
;;; } GtkEntryIconPosition;
;;; 
;;; Specifies the side of the entry at which an icon is placed.
;;; 
;;; GTK_ENTRY_ICON_PRIMARY
;;;     At the beginning of the entry (depending on the text direction).
;;; 
;;; GTK_ENTRY_ICON_SECONDARY
;;;     At the end of the entry (depending on the text direction).
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkEntryIconPosition" gtk-entry-icon-position
  (:export t
   :type-initializer "gtk_entry_icon_position_get_type")
  (:primary 0)
  (:secondary 1))

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
;;; 
;;; void gtk_entry_set_icon_from_stock (GtkEntry *entry,
;;;                                     GtkEntryIconPosition icon_pos,
;;;                                     const gchar *stock_id);
;;; 
;;; Sets the icon shown in the entry at the specified position from a stock
;;; image.
;;; 
;;; If stock_id is NULL, no icon will be shown in the specified position.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; stock_id :
;;;     The name of the stock item, or NULL
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

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
;;; 
;;; GtkImageType gtk_entry_get_icon_storage_type (GtkEntry *entry,
;;;                                               GtkEntryIconPosition icon_pos)
;;; 
;;; Gets the type of representation being used by the icon to store image data.
;;; If the icon has no image data, the return value will be GTK_IMAGE_EMPTY.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     image representation being used
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_pixbuf ()
;;; 
;;; GdkPixbuf * gtk_entry_get_icon_pixbuf (GtkEntry *entry,
;;;                                        GtkEntryIconPosition icon_pos);
;;; 
;;; Retrieves the image used for the icon.
;;; 
;;; Unlike the other methods of setting and getting icon data, this method will
;;; work regardless of whether the icon was set using a GdkPixbuf, a GIcon, a
;;; stock item, or an icon name.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     A GdkPixbuf, or NULL if no icon is set for this position.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_stock ()
;;; 
;;; const gchar * gtk_entry_get_icon_stock (GtkEntry *entry,
;;;                                         GtkEntryIconPosition icon_pos);
;;; 
;;; Retrieves the stock id used for the icon, or NULL if there is no icon or if
;;; the icon was set by some other method (e.g., by pixbuf, icon name or gicon).
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     A stock id, or NULL if no icon is set or if the icon wasn't set from a
;;;     stock id
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_name ()
;;; 
;;; const gchar * gtk_entry_get_icon_name (GtkEntry *entry,
;;;                                        GtkEntryIconPosition icon_pos);
;;; 
;;; Retrieves the icon name used for the icon, or NULL if there is no icon or
;;; if the icon was set by some other method (e.g., by pixbuf, stock or gicon).
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     An icon name, or NULL if no icon is set or if the icon wasn't set from
;;;     an icon name
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_gicon ()
;;; 
;;; GIcon * gtk_entry_get_icon_gicon (GtkEntry *entry,
;;;                                   GtkEntryIconPosition icon_pos);
;;; 
;;; Retrieves the GIcon used for the icon, or NULL if there is no icon or if the
;;; icon was set by some other method (e.g., by stock, pixbuf, or icon name).
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     A GIcon, or NULL if no icon is set or if the icon is not a GIcon.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_activatable ()
;;; 
;;; void gtk_entry_set_icon_activatable (GtkEntry *entry,
;;;                                      GtkEntryIconPosition icon_pos,
;;;                                      gboolean activatable);
;;; 
;;; Sets whether the icon is activatable.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; activatable :
;;;     TRUE if the icon should be activatable
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_activatable ()
;;; 
;;; gboolean gtk_entry_get_icon_activatable (GtkEntry *entry,
;;;                                          GtkEntryIconPosition icon_pos);
;;; 
;;; Returns whether the icon is activatable.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     TRUE if the icon is activatable.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_sensitive ()
;;; 
;;; void gtk_entry_set_icon_sensitive (GtkEntry *entry,
;;;                                    GtkEntryIconPosition icon_pos,
;;;                                    gboolean sensitive);
;;; 
;;; Sets the sensitivity for the specified icon.
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; sensitive :
;;;     Specifies whether the icon should appear sensitive or insensitive
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_sensitive ()
;;; 
;;; gboolean gtk_entry_get_icon_sensitive (GtkEntry *entry,
;;;                                        GtkEntryIconPosition icon_pos);
;;; 
;;; Returns whether the icon appears sensitive or insensitive.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; Returns :
;;;     TRUE if the icon is sensitive.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_at_pos ()
;;; 
;;; gint gtk_entry_get_icon_at_pos (GtkEntry *entry, gint x, gint y);
;;; 
;;; Finds the icon at the given position and return its index. The position's
;;; coordinates are relative to the entry's top left corner. If x, y doesn't
;;; lie inside an icon, -1 is returned. This function is intended for use in a
;;; "query-tooltip" signal handler.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; x :
;;;     the x coordinate of the position to find
;;; 
;;; y :
;;;     the y coordinate of the position to find
;;; 
;;; Returns :
;;;     the index of the icon at the given position, or -1
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_at_pos" gtk-entry-get-icon-at-pos) :int
  (entry (g-object gtk-entry))
  (x :int)
  (y :int))

(export 'gtk-entry-get-icon-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_text ()
;;; 
;;; void gtk_entry_set_icon_tooltip_text (GtkEntry *entry,
;;;                                       GtkEntryIconPosition icon_pos,
;;;                                       const gchar *tooltip);
;;; 
;;; Sets tooltip as the contents of the tooltip for the icon at the specified
;;; position.
;;; 
;;; Use NULL for tooltip to remove an existing tooltip.
;;; 
;;; See also gtk_widget_set_tooltip_text() and
;;; gtk_entry_set_icon_tooltip_markup().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     the icon position
;;; 
;;; tooltip :
;;;     the contents of the tooltip for the icon, or NULL.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_tooltip_text ()
;;; 
;;; gchar * gtk_entry_get_icon_tooltip_text (GtkEntry *entry,
;;;                                          GtkEntryIconPosition icon_pos);
;;; 
;;; Gets the contents of the tooltip on the icon at the specified position in
;;; entry.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     the icon position
;;; 
;;; Returns :
;;;     the tooltip text, or NULL. Free the returned string with g_free() when
;;;     done.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_markup ()
;;; 
;;; void gtk_entry_set_icon_tooltip_markup (GtkEntry *entry,
;;;                                         GtkEntryIconPosition icon_pos,
;;;                                         const gchar *tooltip);
;;; 
;;; Sets tooltip as the contents of the tooltip for the icon at the specified
;;; position. tooltip is assumed to be marked up with the Pango text markup
;;; language.
;;; 
;;; Use NULL for tooltip to remove an existing tooltip.
;;; 
;;; See also gtk_widget_set_tooltip_markup() and
;;; gtk_enty_set_icon_tooltip_text().
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     the icon position
;;; 
;;; tooltip :
;;;     the contents of the tooltip for the icon, or NULL
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_tooltip_markup ()
;;; 
;;; gchar * gtk_entry_get_icon_tooltip_markup (GtkEntry *entry,
;;;                                            GtkEntryIconPosition icon_pos);
;;; 
;;; Gets the contents of the tooltip on the icon at the specified position in
;;; entry.
;;; 
;;; entry :
;;;     a GtkEntry
;;; 
;;; icon_pos :
;;;     the icon position
;;; 
;;; Returns :
;;;     the tooltip text, or NULL. Free the returned string with g_free() when
;;;     done.
;;; 
;;; Since 2.16
;;; ----------------------------------------------------------------------------

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
;;; 
;;; void gtk_entry_get_icon_area (GtkEntry *entry,
;;;                               GtkEntryIconPosition icon_pos,
;;;                               GdkRectangle *icon_area);
;;; 
;;; Gets the area where entry's icon at icon_pos is drawn. This function is
;;; useful when drawing something to the entry in a draw callback.
;;; 
;;; If the entry is not realized or has no icon at the given position,
;;; icon_area is filled with zeros.
;;; 
;;; See also gtk_entry_get_text_area()
;;; 
;;; entry :
;;;     A GtkEntry
;;; 
;;; icon_pos :
;;;     Icon position
;;; 
;;; icon_area :
;;;     Return location for the icon's area.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_area" %gtk-entry-get-icon-area) :void
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (icon-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-icon-area (entry icon-pos)
  (let ((icon-area (make-gdk-rectangle)))
    (%gtk-entry-get-icon-area entry icon-pos icon-area)
    icon-area))

(export 'gtk-entry-get-icon-area)

;;; --- End of file gtk.entry.lisp ---------------------------------------------
