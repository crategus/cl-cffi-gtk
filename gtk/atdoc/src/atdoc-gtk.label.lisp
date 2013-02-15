;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.label.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;;
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
;;;     user data set when the signal handler was connected
;;;
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-link" signal
;;;
;;; gboolean user_function (GtkLabel *label,
;;;                         gchar    *uri,
;;;                         gpointer  user_data)      : Run Last
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
;;;     user data set when the signal handler was connected
;;;
;;; Returns :
;;;     TRUE if the link has been activated
;;;
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "copy-clipboard" signal
;;;
;;; void user_function (GtkLabel *label, gpointer  user_data)      : Action
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
;;;     user data set when the signal handler was connected
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
;;;   * Arrow keys move by individual characters/lines
;;;   * Ctrl-arrow key combinations move by words/paragraphs
;;;   * Home/End keys move to the ends of the buffer
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
;;;     user data set when the signal handler was connected
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
;;;     user data set when the signal handler was connected
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; --- gtk-label --------------------------------------------------------------

(setf (documentation 'gtk-label 'type)
 "@version{2013-1-23}
  @begin{short}
    The @sym{gtk-label} widget displays a small amount of text. As the name
    implies, most labels are used to label another widget such as a
    @class{gtk-button}, a @class{gtk-menu-item}, or a @class{gtk-option-menu}.
  @end{short}

  @heading{GtkLabel as GtkBuildable}
  The @sym{gtk-label} implementation of the @class{gtk-buildable} interface
  supports a custom @code{<attributes>} element, which supports any number of
  @code{<attribute>} elements. The @code{<attribute>} element has attributes
  named name, value, start and end and allows you to specify
  @code{PangoAttribute} values for this label.

  Example 51. A UI definition fragment specifying Pango attributes
  @begin{pre}
 <object class=\"GtkLabel\">
  <attributes>
     <attribute name=\"weight\" value=\"PANGO_WEIGHT_BOLD\"/>
     <attribute name=\"background\" value=\"red\" start=\"5\" end=\"10\"/>\"
   </attributes>
 </object>
  @end{pre}
  The start and end attributes specify the range of characters to which the
  Pango attribute applies. If start and end are not specified, the attribute
  is applied to the whole text. Note that specifying ranges does not make much
  sense with translatable attributes. Use markup embedded in the translatable
  content instead.

  @heading{Mnemonics}
  Labels may contain mnemonics. Mnemonics are underlined characters in the
  label, used for keyboard navigation. Mnemonics are created by providing a
  string with an underscore before the mnemonic character, such as \"_File\",
  to the functions @fun{gtk-label-new-with-mnemonic} or
  @fun{gtk-label-set-text-with-mnemonic}.

  Mnemonics automatically activate any activatable widget the label is inside,
  such as a @class{gtk-button}; if the label is not inside the mnemonic's target
  widget, you have to tell the label about the target using
  @fun{gtk-label-set-mnemonic-widget}. Here's a simple example where the label
  is inside a button:
  @begin{pre}
 // Pressing Alt+H will activate this button
 button = gtk_button_new ();
 label = gtk_label_new_with_mnemonic (\"_Hello\");
 gtk_container_add (GTK_CONTAINER (button), label);
  @end{pre}
  There's a convenience function to create buttons with a mnemonic label
  already inside:
  @begin{pre}
 // Pressing Alt+H will activate this button
 button = gtk_button_new_with_mnemonic (\"_Hello\");
  @end{pre}
  To create a mnemonic for a widget alongside the label, such as a
  @class{gtk-entry}, you have to point the label at the entry with
  @fun{gtk-label-set-mnemonic-widget}:
  @begin{pre}
 // Pressing Alt+H will focus the entry
 entry = gtk_entry_new ();
 label = gtk_label_new_with_mnemonic (\"_Hello\");
 gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
  @end{pre}
  @heading{Markup (styled text)}
  To make it easy to format text in a label (changing colors, fonts, etc.),
  label text can be provided in a simple markup format. Here's how to create
  a label with a small font:
  @begin{pre}
 label = gtk_label_new (NULL);
 gtk_label_set_markup (GTK_LABEL (label),
                       \"<span style=\"color: red\">
                          <small>Small text</small></span>\");
  @end{pre}
  (See complete documentation of available tags in the Pango manual.)

  The markup passed to gtk_label_set_markup() must be valid; for example,
  literal <, > and & characters must be escaped as \<, \gt;, and \&. If you
  pass text obtained from the user, file, or a network to
  @fun{gtk-label-set-markup}, you'll want to escape it with
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()}.

  Markup strings are just a convenient way to set the PangoAttrList on a
  label; @fun{gtk-label-set-attributes} may be a simpler way to set attributes
  in some cases. Be careful though; PangoAttrList tends to cause
  internationalization problems, unless you're applying attributes to the
  entire string (i.e. unless you set the range of each attribute to
  @code{[0, G_MAXINT]}). The reason is that specifying the start_index and
  end_index for a PangoAttribute requires knowledge of the exact string being
  displayed, so translations will cause problems.

  @heading{Selectable labels}
  Labels can be made selectable with @fun{gtk-label-set-selectable}. Selectable
  labels allow the user to copy the label contents to the clipboard. Only
  labels that contain useful-to-copy information - such as error messages -
  should be made selectable.

  @heading{Text layout}
  A label can contain any number of paragraphs, but will have performance
  problems if it contains more than a small number. Paragraphs are separated
  by newlines or other paragraph separators understood by Pango.

  Labels can automatically wrap text if you call @fun{gtk-label-set-line-wrap}.

  @fun{gtk-label-set-justify} sets how the lines in a label align with one
  another. If you want to set how the label as a whole aligns in its available
  space, see @fun{gtk-misc-set-alignment}.

  The \"width-chars\" and \"max-width-chars\" properties can be used to control
  the size allocation of ellipsized or wrapped labels. For ellipsizing labels,
  if either is specified (and less than the actual text size), it is used as
  the minimum width, and the actual text size is used as the natural width of
  the label. For wrapping labels, width-chars is used as the minimum width,
  if specified, and max-width-chars is used as the natural width. Even if
  max-width-chars specified, wrapping labels will be rewrapped to use all of
  the available width.

  @heading{Note}
  Note that the interpretation of \"width-chars\" and \"max-width-chars\" has
  changed a bit with the introduction of width-for-height geometry management.

  @heading{Links}
  Since 2.18, GTK+ supports markup for clickable hyperlinks in addition to
  regular Pango markup. The markup for links is borrowed from HTML, using the
  a with href and title attributes. GTK+ renders links similar to the way they
  appear in web browsers, with colored, underlined text. The title attribute
  is displayed as a tooltip on the link. An example looks like this:
  @begin{pre}
 gtk_label_set_markup (label,
                       \"Go to the <span style=\"color: red\">
                                    <a>GTK+ website</a></span> for more...\");
  @end{pre}
  It is possible to implement custom handling for links and their tooltips
  with the \"activate-link\" signal and the @fun{gtk-label-get-current-uri}
  function.
  @see-slot{gtk-label-angle}
  @see-slot{gtk-label-attributes}
  @see-slot{gtk-label-cursor-position}
  @see-slot{gtk-label-ellipsize}
  @see-slot{gtk-label-justify}
  @see-slot{gtk-label-label}
  @see-slot{gtk-label-max-width-chars}
  @see-slot{gtk-label-mnemonic-keyval}
  @see-slot{gtk-label-mnemonic-widget}
  @see-slot{gtk-label-pattern}
  @see-slot{gtk-label-selectable}
  @see-slot{gtk-label-selection-bound}
  @see-slot{gtk-label-single-line-mode}
  @see-slot{gtk-label-track-visited-links}
  @see-slot{gtk-label-use-markup}
  @see-slot{gtk-label-use-underline}
  @see-slot{gtk-label-width-chars}
  @see-slot{gtk-label-wrap}
  @see-slot{gtk-label-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "angle" 'gtk-label) 't)
 "The @code{angle} property of type @code{gdouble} (Read / Write)@br{}
  The angle that the baseline of the label makes with the horizontal, in
  degrees, measured counterclockwise. An angle of @code{90} reads from from
  bottom to top, an angle of @code{270}, from top to bottom. Ignored if the
  label is selectable, wrapped, or ellipsized.@br{}
  Allowed values: @code{[0,360]}@br{}
  Default value: @code{0}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "attributes" 'gtk-label) 't)
 "The @code{attributes} property of type @code{PangoAttrList*}
  (Read / Write)@br{}
  A list of style attributes to apply to the text of the label.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "cursor-position" 'gtk-label) 't)
 "The @code{cursor-position} property of type @code{gint} (Read)@br{}
  The current position of the insertion cursor in chars.@br{}
  Allowed values: @code{>= 0}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "ellipsize" 'gtk-label) 't)
 "The @code{ellipsize} property of type @code{PangoEllipsizeMode} 
  (Read / Write)@br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a
  @code{PangoEllisizeMode}.@br{}
  Note that setting this property to a value other than
  @code{PANGO_ELLIPSIZE_NONE} has the side-effect that the label requests only
  enough space to display the ellipsis \"...\". In particular, this means that
  ellipsizing labels do not work well in notebook tabs, unless the tab's
  \"tab-expand\" property is set to @arg{true}. Other ways to set a label's
  width are @fun{gtk-widget-set-size-request} and
  @fun{gtk-label-set-width-chars}.@br{}
  Default value: @code{PANGO_ELLIPSIZE_NONE}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "justify" 'gtk-label) 't)
 "The @code{justify} property of type @symbol{gtk-justification}
  (Read / Write)@br{}
  The alignment of the lines in the text of the label relative to each other.
  This does NOT affect the alignment of the label within its allocation.
  See GtkMisc::xalign for that.@br{}
  Default value: @code{:left}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-label) 't)
 "The @code{label} property of type @code{gchar*} (Read / Write)@br{}
  The text of the label.@br{}
  Default value: \"\"")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "max-width-chars" 'gtk-label) 't)
 "The @code{max-width-chars} property of type @code{gint} (Read / Write)@br{}
  The desired maximum width of the label, in characters. If this property is
  set to @code{-1}, the width will be calculated automatically.@br{}
  See the section on text layout for details of how \"width-chars\" and
  \"max-width-chars\" determine the width of ellipsized and wrapped labels.@br{}
  Allowed values: @code{>= G_MAXULONG}@br{}
  Default value: @code{-1}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "mnemonic-keyval" 'gtk-label) 't)
 "The @code{mnemonic-keyval} property of @code{guint} (Read)@br{}
  The mnemonic accelerator key for this label.@br{}
  Default value: @code{16777215}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "mnemonic-widget" 'gtk-label) 't)
 "The @code{mnemonic-widget} property of type @code{gtk-widget}
  (Read / Write)@br{}
  The widget to be activated when the label's mnemonic key is pressed.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "pattern" 'gtk-label) 't)
 "The @code{pattern} property of type @code{gchar*} (Write)@br{}
  A string with _ characters in positions correspond to characters in the text
  to underline.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "selectable" 'gtk-label) 't)
 "The @code{selectable} property of type @code{gboolean} (Read / Write)@br{}
  Whether the label text can be selected with the mouse.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "selection-bound" 'gtk-label) 't)
 "The @code{selection-bound} property of type @code{gint} (Read)@br{}
  The position of the opposite end of the selection from the cursor in
  chars.@br{}
  Allowed values: @code{>= 0}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "single-line-mode" 'gtk-label) 't)
 "The @code{single-line-mode} property of type  @code{gboolean}
  (Read / Write)@br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to ascent
  + descent of the font. This can be an advantage in situations where resizing
  the label because of text changes would be distracting, e.g. in a
  statusbar.@br{}
  Default value: @code{nil}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "track-visited-links" 'gtk-label) 't)
 "The @code{track-visited-links} property of type @code{gboolean}
  (Read / Write)@br{}
  Set this property to @arg{true} to make the label track which links have been
  clicked. It will then apply the ::visited-link-color color, instead of
  ::link-color.@br{}
  Default value: @arg{true}@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-label) 't)
 "The @code{use-markup} property of type @code{gboolean} (Read / Write)@br{}
  The text of the label includes XML markup. See
  @code{pango_parse_markup()}.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "use-underline" 'gtk-label) 't)
 "The @code{use-underline} property of type @code{gboolean} (Read / Write)@br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-label) 't)
 "The @code{width-chars} property of @code{gint} (Read / Write)@br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically.@br{}
  See the section on text layout for details of how \"width-chars\" and
  \"max-width-chars\" determine the width of ellipsized and wrapped labels.@br{}
  Allowed values: @code{>= G_MAXULONG}@br{}
  Default value: @code{-1}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "wrap" 'gtk-label) 't)
 "The @code{wrap} property of type @code{gboolean} (Read / Write)@br{}
  If set, wrap lines if the text becomes too wide.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-label) 't)
 "The @code{wrap-mode} property of type @code{PangoWrapMode} (Read / Write)@br{}
  If line wrapping is on (see the \"wrap\" property) this controls how the line
  wrapping is done. The default is PANGO_WRAP_WORD, which means wrap on word
  boundaries.@br{}
  Default value: @code{PANGO_WRAP_WORD}@br{}
  Since 2.10")

;;; --- gtk-label-new ----------------------------------------------------------

(setf (documentation 'gtk-label-new 'function)
 "@version{2013-1-23}
  @argument[str]{The text of the label.}
  @return{The new @class{gtk-label} instance.}
  @begin{short}
    Creates a new label with the given text inside it. You can pass @code{nil}
    to get an empty label widget.
  @end{short}")

;;; --- gtk-label-set-text -----------------------------------------------------

(setf (documentation 'gtk-label-set-text 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[str]{The text you want to set.}
  @begin{short}
    Sets the text within the @class{gtk-label} widget. It overwrites any text
    that was there before.
  @end{short}
  This will also clear any previously set mnemonic accelerators.")

;;; --- gtk-label-set-attributes -----------------------------------------------

(setf (documentation 'gtk-label-set-attributes 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[attrs]{a PangoAttrList}
  @begin{short}
    Sets a PangoAttrList; the attributes in the list are applied to the label
    text.
  @end{short}

  @heading{Note}
  The attributes set with this function will be applied and merged with any
  other attributes previously effected by way of the \"use-underline\" or
  \"use-markup\" properties. While it is not recommended to mix markup strings
  with manually set attributes, if you must; know that the attributes will be
  applied to the label after the markup string is parsed.")

;;; --- gtk-label-set-markup ---------------------------------------------------

(setf (documentation 'gtk-label-set-markup 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[str]{a markup string (see Pango markup format)}
  @begin{short}
    Parses @arg{str} which is marked up with the Pango text markup language,
    setting the label's text and attribute list based on the parse results.
  @end{short}
  If the str is external data, you may need to escape it with
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()}:
  @begin{pre}
 char *markup;

 markup = g_markup_printf_escaped (\"<span style=\\\"italic\\\">%s</span>\", str);
  gtk_label_set_markup (GTK_LABEL (label), markup);
  g_free (markup);
  @end{pre}")

;;; --- gtk-label-set-markup-with-mnemonic -------------------------------------

(setf (documentation 'gtk-label-set-markup-with-mnemonic 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[str]{a markup string (see Pango markup format)}
  @begin{short}
    Parses @arg{str} which is marked up with the Pango text markup language,
    setting the label's text and attribute list based on the parse results.
  @end{short}
  If characters in @arg{str} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.

  The mnemonic key can be used to activate another widget, chosen
  automatically, or explicitly using @fun{gtk-label-set-mnemonic-widget}.")

;;; --- gtk-label-set-pattern --------------------------------------------------

(setf (documentation 'gtk-label-set-pattern 'function)
 "@version{2013-1-23}
  @argument[label]{the @class{gtk-label} you want to set the pattern to}
  @argument[pattern]{the pattern as described above}
  @begin{short}
    The pattern of underlines you want under the existing text within the
    @class{gtk-label} widget. For example if the current text of the label says
    \"FooBarBaz\" passing a pattern of \"___ ___\" will underline \"Foo\" and
    \"Baz\" but not \"Bar\".
  @end{short}")

;;; --- gtk-label-set-justify --------------------------------------------------

(setf (documentation 'gtk-label-set-justify 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[jtype]{a @symbol{gtk-justification}}
  @begin{short}
    Sets the alignment of the lines in the text of the label relative to each
    other.
  @end{short}
  @code{:left} is the default value when the widget is first created with
  @fun{gtk-label-new}. If you instead want to set the alignment of the label as
  a whole, use gtk_misc_set_alignment() instead. @fun{gtk-label-set-justify} has
  no effect on labels containing only a single line.")

;;; --- gtk-label-set-ellipsize ------------------------------------------------

(setf (documentation 'gtk-label-set-ellipsize 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[mode]{a PangoEllipsizeMode}
  @begin{short}
    Sets the mode used to ellipsize (add an ellipsis: \"...\") to the text if
    there is not enough space to render the entire string.
  @end{short}

  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-label-set-width-chars 'function)
 "@version{2013-1-23}
  @argument[label]{a @class{gtk-label} instance}
  @argument[n-chars]{the new desired width, in characters}
  @begin{short}
    Sets the desired width in characters of label to @arg{n-chars}.
  @end{short}

  Since 2.6")


;;; --- End of file atdoc-gtk.label.lisp ---------------------------------------
