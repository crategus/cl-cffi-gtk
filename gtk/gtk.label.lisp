;;; ----------------------------------------------------------------------------
;;; gtk.label.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;;
;;; GtkLabel
;;; 
;;; A widget that displays a small to medium amount of text
;;; 
;;; Synopsis
;;; 
;;;     GtkLabel
;;;
;;;     gtk_label_new
;;;     gtk_label_set_text
;;;     gtk_label_set_attributes
;;;     gtk_label_set_markup
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_set_pattern
;;;     gtk_label_set_justify
;;;     gtk_label_set_ellipsize
;;;     gtk_label_set_width_chars
;;;     gtk_label_set_max_width_chars
;;;     gtk_label_set_line_wrap
;;;     gtk_label_set_line_wrap_mode
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_get_mnemonic_keyval
;;;     gtk_label_get_selectable
;;;     gtk_label_get_text
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_select_region
;;;     gtk_label_set_mnemonic_widget
;;;     gtk_label_set_selectable
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_get_attributes
;;;     gtk_label_get_justify
;;;     gtk_label_get_ellipsize
;;;     gtk_label_get_width_chars
;;;     gtk_label_get_max_width_chars
;;;     gtk_label_get_label
;;;     gtk_label_get_layout
;;;     gtk_label_get_line_wrap
;;;     gtk_label_get_line_wrap_mode
;;;     gtk_label_get_mnemonic_widget
;;;     gtk_label_get_selection_bounds
;;;     gtk_label_get_use_markup
;;;     gtk_label_get_use_underline 
;;;     gtk_label_get_single_line_mode
;;;     gtk_label_get_angle
;;;     gtk_label_set_label
;;;     gtk_label_set_use_markup
;;;     gtk_label_set_use_underline
;;;     gtk_label_set_single_line_mode
;;;     gtk_label_set_angle
;;;     gtk_label_get_current_uri
;;;     gtk_label_set_track_visited_links
;;;     gtk_label_get_track_visited_links
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkMisc
;;;                      +----GtkLabel
;;;                            +----GtkAccelLabel
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkLabel implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "angle"                    gdouble               : Read / Write
;;;   "attributes"               PangoAttrList*        : Read / Write
;;;   "cursor-position"          gint                  : Read
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;;   "justify"                  GtkJustification      : Read / Write
;;;   "label"                    gchar*                : Read / Write
;;;   "max-width-chars"          gint                  : Read / Write
;;;   "mnemonic-keyval"          guint                 : Read
;;;   "mnemonic-widget"          GtkWidget*            : Read / Write
;;;   "pattern"                  gchar*                : Write
;;;   "selectable"               gboolean              : Read / Write
;;;   "selection-bound"          gint                  : Read
;;;   "single-line-mode"         gboolean              : Read / Write
;;;   "track-visited-links"      gboolean              : Read / Write
;;;   "use-markup"               gboolean              : Read / Write
;;;   "use-underline"            gboolean              : Read / Write
;;;   "width-chars"              gint                  : Read / Write
;;;   "wrap"                     gboolean              : Read / Write
;;;   "wrap-mode"                PangoWrapMode         : Read / Write
;;; 
;;; Signals
;;; 
;;;   "activate-current-link"                          : Action
;;;   "activate-link"                                  : Run Last
;;;   "copy-clipboard"                                 : Action
;;;   "move-cursor"                                    : Action
;;;   "populate-popup"                                 : Run Last
;;; 
;;; Description
;;; 
;;; The GtkLabel widget displays a small amount of text. As the name implies,
;;; most labels are used to label another widget such as a GtkButton, a
;;; GtkMenuItem, or a GtkOptionMenu.
;;; 
;;; GtkLabel as GtkBuildable
;;; 
;;; The GtkLabel implementation of the GtkBuildable interface supports a custom
;;; <attributes> element, which supports any number of <attribute> elements.
;;; the <attribute> element has attributes named name, value, start and end and
;;; allows you to specify PangoAttribute values for this label.
;;; 
;;; Example 51. A UI definition fragment specifying Pango attributes
;;; 
;;;  <object class="GtkLabel">
;;;    <attributes>
;;;      <attribute name="weight" value="PANGO_WEIGHT_BOLD"/>
;;;      <attribute name="background" value="red" start="5" end="10"/>"
;;;    </attributes>
;;;  </object>
;;; 
;;; The start and end attributes specify the range of characters to which the
;;; Pango attribute applies. If start and end are not specified, the attribute
;;; is applied to the whole text. Note that specifying ranges does not make much
;;; sense with translatable attributes. Use markup embedded in the translatable
;;; content instead.
;;; 
;;; Mnemonics
;;; 
;;; Labels may contain mnemonics. Mnemonics are underlined characters in the
;;; label, used for keyboard navigation. Mnemonics are created by providing a
;;; string with an underscore before the mnemonic character, such as "_File",
;;; to the functions gtk_label_new_with_mnemonic() or
;;; gtk_label_set_text_with_mnemonic().
;;; 
;;; Mnemonics automatically activate any activatable widget the label is inside,
;;; such as a GtkButton; if the label is not inside the mnemonic's target
;;; widget, you have to tell the label about the target using
;;; gtk_label_set_mnemonic_widget(). Here's a simple example where the label is
;;; inside a button:
;;; 
;;;  // Pressing Alt+H will activate this button
;;;  button = gtk_button_new ();
;;;  label = gtk_label_new_with_mnemonic ("_Hello");
;;;  gtk_container_add (GTK_CONTAINER (button), label);
;;; 
;;; There's a convenience function to create buttons with a mnemonic label
;;; already inside:
;;; 
;;;  // Pressing Alt+H will activate this button
;;;  button = gtk_button_new_with_mnemonic ("_Hello");
;;; 
;;; To create a mnemonic for a widget alongside the label, such as a GtkEntry,
;;; you have to point the label at the entry with
;;; gtk_label_set_mnemonic_widget():
;;; 
;;;  // Pressing Alt+H will focus the entry
;;;  entry = gtk_entry_new ();
;;;  label = gtk_label_new_with_mnemonic ("_Hello");
;;;  gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
;;; 
;;; Markup (styled text)
;;; 
;;; To make it easy to format text in a label (changing colors, fonts, etc.),
;;; label text can be provided in a simple markup format. Here's how to create
;;; a label with a small font:
;;; 
;;;  label = gtk_label_new (NULL);
;;;  gtk_label_set_markup (GTK_LABEL (label),
;;;                        "<span style="color: red">
;;;                         <small>Small text</small></span>");
;;; 
;;; (See complete documentation of available tags in the Pango manual.)
;;; 
;;; The markup passed to gtk_label_set_markup() must be valid; for example,
;;; literal <, > and & characters must be escaped as \<, \gt;, and \&. If you
;;; pass text obtained from the user, file, or a network to
;;; gtk_label_set_markup(), you'll want to escape it with g_markup_escape_text()
;;; or g_markup_printf_escaped().
;;; 
;;; Markup strings are just a convenient way to set the PangoAttrList on a
;;; label; gtk_label_set_attributes() may be a simpler way to set attributes in
;;; some cases. Be careful though; PangoAttrList tends to cause
;;; internationalization problems, unless you're applying attributes to the
;;; entire string (i.e. unless you set the range of each attribute to
;;; [0, G_MAXINT)). The reason is that specifying the start_index and end_index
;;; for a PangoAttribute requires knowledge of the exact string being displayed,
;;; so translations will cause problems.
;;; 
;;; Selectable labels
;;;
;;; Labels can be made selectable with gtk_label_set_selectable(). Selectable
;;; labels allow the user to copy the label contents to the clipboard. Only
;;; labels that contain useful-to-copy information — such as error messages — 
;;; should be made selectable.
;;; 
;;; Text layout
;;; 
;;; A label can contain any number of paragraphs, but will have performance
;;; problems if it contains more than a small number. Paragraphs are separated
;;; by newlines or other paragraph separators understood by Pango.
;;; 
;;; Labels can automatically wrap text if you call gtk_label_set_line_wrap().
;;; 
;;; gtk_label_set_justify() sets how the lines in a label align with one
;;; another. If you want to set how the label as a whole aligns in its
;;; available space, see gtk_misc_set_alignment().
;;; 
;;; The "width-chars" and "max-width-chars" properties can be used to control
;;; the size allocation of ellipsized or wrapped labels. For ellipsizing labels,
;;; if either is specified (and less than the actual text size), it is used as
;;; the minimum width, and the actual text size is used as the natural width of
;;; the label. For wrapping labels, width-chars is used as the minimum width,
;;; if specified, and max-width-chars is used as the natural width. Even if
;;; max-width-chars specified, wrapping labels will be rewrapped to use all of
;;; the available width.
;;; 
;;; Note
;;; 
;;; Note that the interpretation of "width-chars" and "max-width-chars" has
;;; changed a bit with the introduction of width-for-height geometry management.
;;; 
;;; Links
;;; 
;;; Since 2.18, GTK+ supports markup for clickable hyperlinks in addition to
;;; regular Pango markup. The markup for links is borrowed from HTML, using the
;;; a with href and title attributes. GTK+ renders links similar to the way they
;;; appear in web browsers, with colored, underlined text. The title attribute
;;; is displayed as a tooltip on the link. An example looks like this:
;;; 
;;;  1 gtk_label_set_markup (label, "Go to the <span style="color: red">
;;;                                 <a>GTK+ website</a></span> for more...");
;;; 
;;; It is possible to implement custom handling for links and their tooltips
;;; with the "activate-link" signal and the gtk_label_get_current_uri()
;;; function.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "angle" property
;;; 
;;;   "angle" gdouble               : Read / Write
;;; 
;;; The angle that the baseline of the label makes with the horizontal, in
;;; degrees, measured counterclockwise. An angle of 90 reads from from bottom
;;; to top, an angle of 270, from top to bottom. Ignored if the label is
;;; selectable, wrapped, or ellipsized.
;;; 
;;; Allowed values: [0,360]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "attributes" property
;;; 
;;;   "attributes" PangoAttrList*        : Read / Write
;;; 
;;; A list of style attributes to apply to the text of the label.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-position" property
;;; 
;;;   "cursor-position" gint                  : Read
;;; 
;;; The current position of the insertion cursor in chars.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "ellipsize" property
;;; 
;;;   "ellipsize"  PangoEllipsizeMode    : Read / Write
;;; 
;;; The preferred place to ellipsize the string, if the label does not have
;;; enough room to display the entire string, specified as a PangoEllisizeMode.
;;; 
;;; Note that setting this property to a value other than PANGO_ELLIPSIZE_NONE
;;; has the side-effect that the label requests only enough space to display
;;; the ellipsis "...". In particular, this means that ellipsizing labels do
;;; not work well in notebook tabs, unless the tab's "tab-expand" property is
;;; set to TRUE. Other ways to set a label's width are
;;; gtk_widget_set_size_request() and gtk_label_set_width_chars().
;;; 
;;; Default value: PANGO_ELLIPSIZE_NONE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "justify" property
;;; 
;;;   "justify" GtkJustification      : Read / Write
;;; 
;;; The alignment of the lines in the text of the label relative to each other.
;;; This does NOT affect the alignment of the label within its allocation.
;;; See GtkMisc::xalign for that.
;;; 
;;; Default value: GTK_JUSTIFY_LEFT
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;; 
;;;   "label" gchar*                : Read / Write
;;; 
;;; The text of the label.
;;; 
;;; Default value: ""
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-width-chars" property
;;; 
;;;   "max-width-chars" gint                  : Read / Write
;;; 
;;; The desired maximum width of the label, in characters. If this property is
;;; set to -1, the width will be calculated automatically.
;;; 
;;; See the section on text layout for details of how "width-chars" and
;;; "max-width-chars" determine the width of ellipsized and wrapped labels.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "mnemonic-keyval" property
;;; 
;;;   "mnemonic-keyval" guint                 : Read
;;; 
;;; The mnemonic accelerator key for this label.
;;; 
;;; Default value: 16777215
;;;
;;; ----------------------------------------------------------------------------
;;; The "mnemonic-widget" property
;;; 
;;;   "mnemonic-widget" GtkWidget*            : Read / Write
;;; 
;;; The widget to be activated when the label's mnemonic key is pressed.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pattern" property
;;; 
;;;   "pattern" gchar*                : Write
;;; 
;;; A string with _ characters in positions correspond to characters in the
;;; text to underline.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "selectable" property
;;; 
;;;   "selectable" gboolean              : Read / Write
;;; 
;;; Whether the label text can be selected with the mouse.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-bound" property
;;; 
;;;   "selection-bound" gint                  : Read
;;; 
;;; The position of the opposite end of the selection from the cursor in chars.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "single-line-mode" property
;;; 
;;;   "single-line-mode" gboolean              : Read / Write
;;; 
;;; Whether the label is in single line mode. In single line mode, the height
;;; of the label does not depend on the actual text, it is always set to ascent
;;; + descent of the font. This can be an advantage in situations where
;;; resizing the label because of text changes would be distracting, e.g. in
;;; a statusbar.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "track-visited-links" property
;;; 
;;;   "track-visited-links" gboolean              : Read / Write
;;; 
;;; Set this property to TRUE to make the label track which links have been
;;; clicked. It will then apply the ::visited-link-color color, instead of
;;; ::link-color.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-markup" property
;;; 
;;;   "use-markup" gboolean              : Read / Write
;;; 
;;; The text of the label includes XML markup. See pango_parse_markup().
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-underline" property
;;; 
;;;   "use-underline" gboolean              : Read / Write
;;; 
;;; If set, an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-chars" property
;;; 
;;;   "width-chars" gint                  : Read / Write
;;; 
;;; The desired width of the label, in characters. If this property is set to
;;; -1, the width will be calculated automatically.
;;; 
;;; See the section on text layout for details of how "width-chars" and
;;; "max-width-chars" determine the width of ellipsized and wrapped labels.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap" property
;;; 
;;;   "wrap" gboolean              : Read / Write
;;; 
;;; If set, wrap lines if the text becomes too wide.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-mode" property
;;; 
;;;   "wrap-mode" PangoWrapMode         : Read / Write
;;; 
;;; If line wrapping is on (see the "wrap" property) this controls how the
;;; line wrapping is done. The default is PANGO_WRAP_WORD, which means wrap on
;;; word boundaries.
;;; 
;;; Default value: PANGO_WRAP_WORD
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-current-link" signal
;;; 
;;; void user_function (GtkLabel *label, gpointer  user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user activates a link in
;;; the label.
;;; 
;;; Applications may also emit the signal with g_signal_emit_by_name() if they
;;; need to control activation of URIs programmatically.
;;; 
;;; The default bindings for this signal are all forms of the Enter key.
;;; 
;;; label :
;;;     The label on which the signal was emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-link" signal
;;; 
;;; gboolean user_function (GtkLabel *label, gchar *uri, gpointer user_data)
;;;                                                                   : Run Last
;;; 
;;; The signal which gets emitted to activate a URI. Applications may connect
;;; to it to override the default behaviour, which is to call gtk_show_uri().
;;; 
;;; label :
;;;     The label on which the signal was emitted
;;; 
;;; uri :
;;;     the URI that is activated
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the link has been activated
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "copy-clipboard" signal
;;; 
;;; void user_function (GtkLabel *label, gpointer user_data)      : Action
;;; 
;;; The ::copy-clipboard signal is a keybinding signal which gets emitted to
;;; copy the selection to the clipboard.
;;; 
;;; The default binding for this signal is Ctrl-c.
;;; 
;;; label :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-cursor" signal
;;; 
;;; void user_function (GtkLabel       *entry,
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
;;;     * Arrow keys move by individual characters/lines
;;;     * Ctrl-arrow key combinations move by words/paragraphs
;;;     * Home/End keys move to the ends of the buffer
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
;;; The "populate-popup" signal
;;; 
;;; void user_function (GtkLabel *label,
;;;                     GtkMenu  *menu,
;;;                     gpointer  user_data)      : Run Last
;;; 
;;; The ::populate-popup signal gets emitted before showing the context menu of
;;; the label. Note that only selectable labels have context menus.
;;; 
;;; If you need to add items to the context menu, connect to this signal and
;;; append your menuitems to the menu.
;;; 
;;; label :
;;;     The label on which the signal is emitted
;;; 
;;; menu :
;;;     the menu that is being populated
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLabel
;;; 
;;; struct GtkLabel;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkLabel" 'gtk-label))

(define-g-object-class "GtkLabel" gtk-label
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_label_get_type")
  ((angle
    gtk-label-angle
    "angle" "gdouble" t t)
   (attributes
    gtk-label-attributes
    "attributes" "PangoAttrList" t t)
   (cursor-position
    gtk-label-cursor-position
    "cursor-position" "gint" t nil)
   (ellipsize
    gtk-label-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (justify
    gtk-label-justify
    "justify" "GtkJustification" t t)
   (label
    gtk-label-label
    "label" "gchararray" t t)
   (max-width-chars
    gtk-label-max-width-chars
    "max-width-chars" "gint" t t)
   (mnemonic-keyval
    gtk-label-mnemonic-keyval
    "mnemonic-keyval" "guint" t nil)
   (mnemonic-widget
    gtk-label-mnemonic-widget
    "mnemonic-widget" "GtkWidget" t t)
   (pattern
    gtk-label-pattern
    "pattern" "gchararray" nil t)
   (selectable
    gtk-label-selectable
    "selectable" "gboolean" t t)
   (selection-bound
    gtk-label-selection-bound
    "selection-bound" "gint" t nil)
   (single-line-mode
    gtk-label-single-line-mode
    "single-line-mode" "gboolean" t t)
   (track-visited-links
    gtk-label-track-visited-links
    "track-visited-links" "gboolean" t t)
   (use-markup
    gtk-label-use-markup
    "use-markup" "gboolean" t t)
   (use-underline
    gtk-label-use-underline
    "use-underline" "gboolean" t t)
   (width-chars
    gtk-label-width-chars
    "width-chars" "gint" t t)
   (wrap
    gtk-label-wrap
    "wrap" "gboolean" t t)
   (wrap-mode
    gtk-label-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (:cffi line-wrap
          gtk-label-line-wrap :boolean
          "gtk_label_get_line_wrap" "gtk_label_set_line_wrap")
   (:cffi line-wrap-mode
          gtk-label-line-wrap-mode pango-wrap-mode
          "gtk_label_get_line_wrap_mode" "gtk_label_set_line_wrap_mode")
   (:cffi layout
          gtk-label-layout g-object
          "gtk_label_get_layout" nil)
   (:cffi selection-bounds
          gtk-label-selection-bounds nil
          gtk-label-get-selection-bounds nil)
   (:cffi layout-offsets
          gtk-label-layout-offsets nil
          gtk-label-get-layout-offsets nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_label_new ()
;;; 
;;; GtkWidget * gtk_label_new (const gchar *str);
;;; 
;;; Creates a new label with the given text inside it. You can pass NULL to get
;;; an empty label widget.
;;; 
;;; str :
;;;     The text of the label
;;; 
;;; Returns :
;;;     the new GtkLabel
;;; ----------------------------------------------------------------------------

(defun gtk-label-new (str)
  (make-instance 'gtk-label :label str))

(export 'gtk-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text ()
;;; 
;;; void gtk_label_set_text (GtkLabel *label, const gchar *str);
;;; 
;;; Sets the text within the GtkLabel widget. It overwrites any text that was
;;; there before.
;;; 
;;; This will also clear any previously set mnemonic accelerators.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; str :
;;;     The text you want to set
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-text (label str)
  (setf (gtk-label-label label) str))

(export 'gtk-label-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_attributes ()
;;; 
;;; void gtk_label_set_attributes (GtkLabel *label, PangoAttrList *attrs);
;;; 
;;; Sets a PangoAttrList; the attributes in the list are applied to the label
;;; text.
;;; 
;;; Note
;;; 
;;; The attributes set with this function will be applied and merged with any
;;; other attributes previously effected by way of the "use-underline" or
;;; "use-markup" properties. While it is not recommended to mix markup strings
;;; with manually set attributes, if you must; know that the attributes will be
;;; applied to the label after the markup string is parsed.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; attrs :
;;;     a PangoAttrList
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-attributes (label attrs)
  (setf (gtk-label-attributes label) attrs))

(export 'gtk-label-set-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup ()
;;; 
;;; void gtk_label_set_markup (GtkLabel *label, const gchar *str);
;;; 
;;; Parses str which is marked up with the Pango text markup language, setting
;;; the label's text and attribute list based on the parse results. If the str
;;; is external data, you may need to escape it with g_markup_escape_text() or
;;; g_markup_printf_escaped():
;;; 
;;;  char *markup;
;;;  
;;;  markup = g_markup_printf_escaped("<span style=\"italic\">%s</span>",str);
;;;  gtk_label_set_markup (GTK_LABEL (label), markup);
;;;  g_free (markup);
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; str :
;;;     a markup string (see Pango markup format)
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup" gtk-label-set-markup) :void
  (label (g-object gtk-label))
  (str :string))

(export 'gtk-label-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic ()
;;; 
;;; void gtk_label_set_markup_with_mnemonic (GtkLabel *label, const gchar *str)
;;; 
;;; Parses str which is marked up with the Pango text markup language, setting
;;; the label's text and attribute list based on the parse results. If
;;; characters in str are preceded by an underscore, they are underlined
;;; indicating that they represent a keyboard accelerator called a mnemonic.
;;; 
;;; The mnemonic key can be used to activate another widget, chosen
;;; automatically, or explicitly using gtk_label_set_mnemonic_widget().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; str :
;;;     a markup string (see Pango markup format)
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup_with_mnemonic"
          gtk-label-set-markup-with-mnemonic) :void
  (label (g-object gtk-label))
  (str :string))

(export 'gtk-label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_pattern ()
;;; 
;;; void gtk_label_set_pattern (GtkLabel *label, const gchar *pattern)
;;; 
;;; The pattern of underlines you want under the existing text within the
;;; GtkLabel widget. For example if the current text of the label says
;;; "FooBarBaz" passing a pattern of "___ ___" will underline "Foo" and "Baz"
;;; but not "Bar".
;;; 
;;; label :
;;;     The GtkLabel you want to set the pattern to.
;;; 
;;; pattern :
;;;     The pattern as described above.
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-pattern (label pattern)
  (setf (gtk-label-pattern label) pattern))

(export 'gtk-label-set-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_justify ()
;;; 
;;; void gtk_label_set_justify (GtkLabel *label, GtkJustification jtype)
;;; 
;;; Sets the alignment of the lines in the text of the label relative to each
;;; other. GTK_JUSTIFY_LEFT is the default value when the widget is first
;;; created with gtk_label_new(). If you instead want to set the alignment of
;;; the label as a whole, use gtk_misc_set_alignment() instead.
;;; gtk_label_set_justify() has no effect on labels containing only a single
;;; line.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; jtype :
;;;     a GtkJustification
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-justify (label jtype)
  (setf (gtk-label-justify label) jtype))

(export 'gtk-label-set-justify)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_ellipsize ()
;;; 
;;; void gtk_label_set_ellipsize (GtkLabel *label, PangoEllipsizeMode mode)
;;; 
;;; Sets the mode used to ellipsize (add an ellipsis: "...") to the text if
;;; there is not enough space to render the entire string.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; mode :
;;;     a PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-ellipsize (label mode)
  (setf (gtk-label-ellipsize label) mode))

(export 'gtk-label-set-ellipsize)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_width_chars ()
;;; 
;;; void gtk_label_set_width_chars (GtkLabel *label, gint n_chars)
;;; 
;;; Sets the desired width in characters of label to n_chars.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; n_chars :
;;;     the new desired width, in characters.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-width-chars (label n-chars)
  (setf (gtk-label-width-chars label) n-chars))
  
(export 'gtk-label-set-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_max_width_chars ()
;;; 
;;; void gtk_label_set_max_width_chars (GtkLabel *label, gint n_chars)
;;; 
;;; Sets the desired maximum width in characters of label to n_chars.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; n_chars :
;;;     the new desired maximum width, in characters.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-max-width-chars (label n-chars)
  (setf (gtk-label-max-width-chars label) n-chars))

(export 'gtk-label-set-max-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap ()
;;; 
;;; void gtk_label_set_line_wrap (GtkLabel *label, gboolean wrap)
;;; 
;;; Toggles line wrapping within the GtkLabel widget. TRUE makes it break lines
;;; if text exceeds the widget's size. FALSE lets the text get cut off by the
;;; edge of the widget if it exceeds the widget size.
;;; 
;;; Note that setting line wrapping to TRUE does not make the label wrap at its
;;; parent container's width, because GTK+ widgets conceptually can't make
;;; their requisition depend on the parent container's size. For a label that
;;; wraps at a specific position, set the label's width using
;;; gtk_widget_set_size_request().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; wrap :
;;;     the setting
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-line-wrap (label wrap)
  (setf (gtk-label-wrap label) wrap))

(export 'gtk-label-set-line-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap_mode ()
;;; 
;;; void gtk_label_set_line_wrap_mode (GtkLabel *label, PangoWrapMode wrap_mode)
;;; 
;;; If line wrapping is on (see gtk_label_set_line_wrap()) this controls how
;;; the line wrapping is done. The default is PANGO_WRAP_WORD which means wrap
;;; on word boundaries.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; wrap_mode :
;;;     the line wrapping mode
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-line-wrap-mode (label wrap-mode)
  (setf (gtk-label-wrap-mode label) wrap-mode))  

(export 'gtk-label-set-line-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets ()
;;; 
;;; void gtk_label_get_layout_offsets (GtkLabel *label, gint *x, gint *y)
;;; 
;;; Obtains the coordinates where the label will draw the PangoLayout
;;; representing the text in the label; useful to convert mouse events into
;;; coordinates inside the PangoLayout, e.g. to take some action if some part
;;; of the label is clicked. Of course you will need to create a GtkEventBox to
;;; receive the events, and pack the label inside it, since labels are a
;;; GTK_NO_WINDOW widget. Remember when using the PangoLayout functions you
;;; need to convert to and from pixels using PANGO_PIXELS() or PANGO_SCALE.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; x :
;;;     location to store X offset of layout, or NULL.
;;; 
;;; y :
;;;     location to store Y offset of layout, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout_offsets" %gtk-label-get-layout-offsets) :void
  (label (g-object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-label-get-layout-offsets (label)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-label-get-layout-offsets label x y)
    (list (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-label-get-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_mnemonic_keyval ()
;;; 
;;; guint gtk_label_get_mnemonic_keyval (GtkLabel *label)
;;; 
;;; If the label has been set so that it has an mnemonic key this function
;;; returns the keyval used for the mnemonic accelerator. If there is no
;;; mnemonic set up it returns GDK_VoidSymbol.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     GDK keyval usable for accelerators, or GDK_VoidSymbol
;;; ----------------------------------------------------------------------------

(defun gtk-label-get-mnemonic-keyval (label)
  (gtk-label-mnemonic-keyval label))

(export 'gtk-label-mnemonic-keyval)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selectable ()
;;; 
;;; gboolean gtk_label_get_selectable (GtkLabel *label);
;;; 
;;; Gets the value set by gtk_label_set_selectable().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE if the user can copy text from the label
;;; ----------------------------------------------------------------------------

(defun gtk-label-get-selectable (label)
  (gtk-label-selectable label))

(export 'gtk-label-get-selectable)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text ()
;;; 
;;; const gchar * gtk_label_get_text (GtkLabel *label);
;;; 
;;; Fetches the text from a label widget, as displayed on the screen. This
;;; does not include any embedded underlines indicating mnemonics or Pango
;;; markup. (See gtk_label_get_label())
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the text in the label widget. This is the internal string used by the
;;;     label, and must not be modified.
;;; ----------------------------------------------------------------------------

(defun gtk-label-get-text (label)
  (gtk-label-label label))

(export 'gtk-label-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_label_new_with_mnemonic (const gchar *str);
;;; 
;;; Creates a new GtkLabel, containing the text in str.
;;; 
;;; If characters in str are preceded by an underscore, they are underlined. If
;;; you need a literal underscore character in a label,
;;; use '__' (two underscores). The first underlined character represents a
;;; keyboard accelerator called a mnemonic. The mnemonic key can be used to
;;; activate another widget, chosen automatically, or explicitly using
;;; gtk_label_set_mnemonic_widget().
;;; 
;;; If gtk_label_set_mnemonic_widget() is not called, then the first
;;; activatable ancestor of the GtkLabel will be chosen as the mnemonic widget.
;;; For instance, if the label is inside a button or menu item, the button or
;;; menu item will automatically become the mnemonic widget and be activated by
;;; the mnemonic.
;;; 
;;; str :
;;;     the text of the label, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     the new GtkLabel
;;; ----------------------------------------------------------------------------

(defun gtk-label-new-with-mnemonic (str)
  (make-instance 'gtk-label
                 :label str
                 :use-underline t))

(export 'gtk-label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region ()
;;; 
;;; void gtk_label_select_region (GtkLabel *label,
;;;                               gint start_offset, gint end_offset);
;;; 
;;; Selects a range of characters in the label, if the label is selectable.
;;; See gtk_label_set_selectable(). If the label is not selectable, this
;;; function has no effect. If start_offset or end_offset are -1, then the end
;;; of the label will be substituted.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; start_offset :
;;;     start offset (in characters not bytes)
;;; 
;;; end_offset :
;;;     end offset (in characters not bytes)
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_select_region" gtk-label-select-region) :void
  (label (g-object gtk-label))
  (start-offset :int)
  (end-offset :int))

(export 'gtk-label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_mnemonic_widget ()
;;; 
;;; void gtk_label_set_mnemonic_widget (GtkLabel *label, GtkWidget *widget);
;;; 
;;; If the label has been set so that it has an mnemonic key (using i.e.
;;; gtk_label_set_markup_with_mnemonic(), gtk_label_set_text_with_mnemonic(),
;;; gtk_label_new_with_mnemonic() or the "use_underline" property) the label
;;; can be associated with a widget that is the target of the mnemonic. When
;;; the label is inside a widget (like a GtkButton or a GtkNotebook tab) it is
;;; automatically associated with the correct widget, but sometimes (i.e. when
;;; the target is a GtkEntry next to the label) you need to set it explicitly
;;; using this function.
;;; 
;;; The target widget will be accelerated by emitting the
;;; GtkWidget::mnemonic-activate signal on it. The default handler for this
;;; signal will activate the widget if there are no mnemonic collisions and
;;; toggle focus between the colliding widgets otherwise.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; widget :
;;;     the target GtkWidget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_selectable ()
;;; 
;;; void gtk_label_set_selectable (GtkLabel *label, gboolean setting);
;;; 
;;; Selectable labels allow the user to select text from the label, for
;;; copy-and-paste.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; setting :
;;;     TRUE to allow selecting text in the label
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text_with_mnemonic ()
;;; 
;;; void gtk_label_set_text_with_mnemonic (GtkLabel *label, const gchar *str);
;;; 
;;; Sets the label's text from the string str. If characters in str are
;;; preceded by an underscore, they are underlined indicating that they
;;; represent a keyboard accelerator called a mnemonic. The mnemonic key can
;;; be used to activate another widget, chosen automatically, or explicitly
;;; using gtk_label_set_mnemonic_widget().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; str :
;;;     a string
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_attributes ()
;;; 
;;; PangoAttrList * gtk_label_get_attributes (GtkLabel *label);
;;; 
;;; Gets the attribute list that was set on the label using
;;; gtk_label_set_attributes(), if any. This function does not reflect
;;; attributes that come from the labels markup (see gtk_label_set_markup()).
;;; If you want to get the effective attributes for the label, use
;;; pango_layout_get_attribute (gtk_label_get_layout (label)).
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the attribute list, or NULL if none was set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_justify ()
;;; 
;;; GtkJustification gtk_label_get_justify (GtkLabel *label);
;;; 
;;; Returns the justification of the label. See gtk_label_set_justify().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     GtkJustification
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_ellipsize ()
;;; 
;;; PangoEllipsizeMode gtk_label_get_ellipsize  (GtkLabel *label);
;;; 
;;; Returns the ellipsizing position of the label.
;;; See gtk_label_set_ellipsize().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_width_chars ()
;;; 
;;; gint gtk_label_get_width_chars (GtkLabel *label);
;;; 
;;; Retrieves the desired width of label, in characters.
;;; See gtk_label_set_width_chars().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the width of the label in characters.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_max_width_chars ()
;;; 
;;; gint gtk_label_get_max_width_chars (GtkLabel *label);
;;; 
;;; Retrieves the desired maximum width of label, in characters.
;;; See gtk_label_set_width_chars().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the maximum width of the label in characters.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_label ()
;;; 
;;; const gchar * gtk_label_get_label (GtkLabel *label);
;;; 
;;; Fetches the text from a label widget including any embedded underlines
;;; indicating mnemonics and Pango markup. (See gtk_label_get_text()).
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the text of the label widget. This string is owned by the widget and
;;;     must not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout ()
;;; 
;;; PangoLayout * gtk_label_get_layout (GtkLabel *label);
;;; 
;;; Gets the PangoLayout used to display the label. The layout is useful to
;;; e.g. convert text positions to pixel positions, in combination with
;;; gtk_label_get_layout_offsets(). The returned layout is owned by the label
;;; so need not be freed by the caller. The label is free to recreate its
;;; layout at any time, so it should be considered read-only.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the PangoLayout for this label
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_line_wrap ()
;;; 
;;; gboolean gtk_label_get_line_wrap (GtkLabel *label);
;;; 
;;; Returns whether lines in the label are automatically wrapped.
;;; See gtk_label_set_line_wrap().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE if the lines of the label are automatically wrapped.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_line_wrap_mode ()
;;; 
;;; PangoWrapMode gtk_label_get_line_wrap_mode (GtkLabel *label);
;;; 
;;; Returns line wrap mode used by the label.
;;; See gtk_label_set_line_wrap_mode().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE if the lines of the label are automatically wrapped.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_mnemonic_widget ()
;;; 
;;; GtkWidget * gtk_label_get_mnemonic_widget (GtkLabel *label);
;;; 
;;; Retrieves the target of the mnemonic (keyboard shortcut) of this label.
;;; See gtk_label_set_mnemonic_widget().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the target of the label's mnemonic, or NULL if none has been set and
;;;     the default algorithm will be used.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds ()
;;; 
;;; gboolean gtk_label_get_selection_bounds (GtkLabel *label,
;;;                                          gint *start, gint *end);
;;; 
;;; Gets the selected range of characters in the label, returning TRUE if
;;; there's a selection.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; start :
;;;     return location for start of selection, as a character offset.
;;; 
;;; end :
;;;     return location for end of selection, as a character offset.
;;; 
;;; Returns :
;;;     TRUE if selection is non-empty
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_selection_bounds" %gtk-label-get-selection-bounds)
    :boolean
  (label (g-object label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun gtk-label-get-selection-bounds (label)
  (with-foreign-objects ((start :int) (end :int))
    (when (%gtk-label-get-selection-bounds label start end)
      (list (mem-ref start :int) (mem-ref end :int)))))

(export 'gtk-label-get-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_use_markup ()
;;; 
;;; gboolean gtk_label_get_use_markup (GtkLabel *label);
;;; 
;;; Returns whether the label's text is interpreted as marked up with the
;;; Pango text markup language. See gtk_label_set_use_markup().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE if the label's text will be parsed for markup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_use_underline ()
;;; 
;;; gboolean gtk_label_get_use_underline (GtkLabel *label);
;;; 
;;; Returns whether an embedded underline in the label indicates a mnemonic.
;;; See gtk_label_set_use_underline().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE whether an embedded underline in the label indicates the mnemonic
;;;     accelerator keys.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_single_line_mode ()
;;; 
;;; gboolean gtk_label_get_single_line_mode (GtkLabel *label);
;;; 
;;; Returns whether the label is in single line mode.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE when the label is in single line mode.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_angle ()
;;; 
;;; gdouble gtk_label_get_angle (GtkLabel *label);
;;; 
;;; Gets the angle of rotation for the label. See gtk_label_set_angle().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the angle of rotation for the label
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-get-angle (label)
  (gtk-label-angle label))

(export 'gtk-label-get-angle)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_label ()
;;; 
;;; void gtk_label_set_label (GtkLabel *label, const gchar *str);
;;; 
;;; Sets the text of the label. The label is interpreted as including embedded
;;; underlines and/or Pango markup depending on the values of the
;;; "use-underline"" and "use-markup" properties.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; str :
;;;     the new text to set for the label
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-label (label str)
  (setf (gtk-label-label label) str))

(export 'gtk-label-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_use_markup ()
;;; 
;;; void gtk_label_set_use_markup (GtkLabel *label, gboolean setting);
;;; 
;;; Sets whether the text of the label contains markup in Pango's text markup
;;; language. See gtk_label_set_markup().
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; setting :
;;;     TRUE if the label's text should be parsed for markup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_use_underline ()
;;; 
;;; void gtk_label_set_use_underline (GtkLabel *label, gboolean setting);
;;; 
;;; If true, an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; setting :
;;;     TRUE if underlines in the text indicate mnemonics
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_single_line_mode ()
;;; 
;;; void gtk_label_set_single_line_mode (GtkLabel *label,
;;;                                      gboolean single_line_mode);
;;; 
;;; Sets whether the label is in single line mode.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; single_line_mode :
;;;     TRUE if the label should be in single line mode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-single-line-mode (label single-line-mode)
  (setf (gtk-label-single-line-mode label) single-line-mode))
  
(export 'gtk-label-single-line-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_angle ()
;;; 
;;; void gtk_label_set_angle (GtkLabel *label, gdouble angle);
;;; 
;;; Sets the angle of rotation for the label. An angle of 90 reads from from
;;; bottom to top, an angle of 270, from top to bottom. The angle setting for
;;; the label is ignored if the label is selectable, wrapped, or ellipsized.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; angle :
;;;     the angle that the baseline of the label makes with the horizontal, in
;;;     degrees, measured counterclockwise
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-angle (label angle)
  (setf (gtk-label-angle label) angle))

(export 'gtk-label-set-angle)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri ()
;;; 
;;; const gchar * gtk_label_get_current_uri (GtkLabel *label);
;;; 
;;; Returns the URI for the currently active link in the label. The active link
;;; is the one under the mouse pointer or, in a selectable label, the link in
;;; which the text cursor is currently positioned.
;;; 
;;; This function is intended for use in a "activate-link" handler or for use
;;; in a "query-tooltip" handler.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     the currently active URI. The string is owned by GTK+ and must not be
;;;     freed or modified.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_current_uri" gtk-label-get-current-uri) :string
  (label (g-object gtk-label)))

(export 'gtk-label-get-current-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_track_visited_links ()
;;; 
;;; void gtk_label_set_track_visited_links (GtkLabel *label,
;;;                                         gboolean track_links);
;;; 
;;; Sets whether the label should keep track of clicked links (and use a
;;; different color for them).
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; track_links :
;;;     TRUE to track visited links
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-label-set-track-visited-links (label track-links)
  (setf (gtk-label-track-visited-links label) track-links))  

(export 'gtk-label-set-track-visited-links)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_track_visited_links ()
;;; 
;;; gboolean gtk_label_get_track_visited_links   (GtkLabel *label);
;;; 
;;; Returns whether the label is currently keeping track of clicked links.
;;; 
;;; label :
;;;     a GtkLabel
;;; 
;;; Returns :
;;;     TRUE if clicked links are remembered
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-label-get-track-visited-links (label)
  (gtk-label-track-visited-links label))
  
(export 'gtk-label-get-track-visited-links)

;;; --- End of file gkt.label.lisp ---------------------------------------------
