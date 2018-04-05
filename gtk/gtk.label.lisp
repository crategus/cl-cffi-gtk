;;; ----------------------------------------------------------------------------
;;; gtk.label.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLabel
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkLabel" 'gtk-label))

(define-g-object-class "GtkLabel" gtk-label
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
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
   #+gtk-3-10
   (lines
    gtk-label-lines
    "lines" "gint" t t)
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
    "wrap-mode" "PangoWrapMode" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-label 'type)
 "@version{2013-4-12}
  @begin{short}
    The @sym{gtk-label} widget displays a small amount of text. As the name
    implies, most labels are used to label another widget such as a
    @class{gtk-button}, a @class{gtk-menu-item}, or a @class{gtk-combo-box}.
  @end{short}

  @subheading{GtkLabel as GtkBuildable}
    The @sym{gtk-label} implementation of the @class{gtk-buildable} interface
    supports a custom @code{<attributes>} element, which supports any number of
    @code{<attribute>} elements. The @code{<attribute>} element has attributes
    named @code{name}, @code{value}, @code{start} and @code{end} and allows you
    to specify @code{PangoAttribute} values for this label.

    @b{Example:} A UI definition fragment specifying Pango attributes
  @begin{pre}
 <object class=\"GtkLabel\">
  <attributes>
     <attribute name=\"weight\" value=\"PANGO_WEIGHT_BOLD\"/>
     <attribute name=\"background\" value=\"red\" start=\"5\" end=\"10\"/>\"
   </attributes>
 </object>
    @end{pre}
    The @code{start} and @code{end} attributes specify the range of characters
    to which the Pango attribute applies. If @code{start} and @code{end} are not
    specified, the attribute is applied to the whole text. Note that specifying
    ranges does not make much sense with translatable attributes. Use markup
    embedded in the translatable content instead.

  @subheading{Mnemonics}
    Labels may contain mnemonics. Mnemonics are underlined characters in the
    label, used for keyboard navigation. Mnemonics are created by providing a
    string with an underscore before the mnemonic character, such as \"_File\",
    to the functions @fun{gtk-label-new-with-mnemonic} or
    @fun{gtk-label-set-text-with-mnemonic}.

    Mnemonics automatically activate any activatable widget the label is inside,
    such as a @class{gtk-button} widget; if the label is not inside the
    mnemonic's target widget, you have to tell the label about the target using
    @fun{gtk-label-set-mnemonic-widget}. Here is a simple example where the
    label is inside a button:
    @begin{pre}
 // Pressing Alt+H will activate this button
 button = gtk_button_new ();
 label = gtk_label_new_with_mnemonic (\"_Hello\");
 gtk_container_add (GTK_CONTAINER (button), label);
    @end{pre}
    There is a convenience function to create buttons with a mnemonic label
    already inside:
    @begin{pre}
 // Pressing Alt+H will activate this button
 button = gtk_button_new_with_mnemonic (\"_Hello\");
    @end{pre}
    To create a mnemonic for a widget alongside the label, such as a
    @class{gtk-entry} widget, you have to point the label at the entry with
    @fun{gtk-label-set-mnemonic-widget}:
    @begin{pre}
 // Pressing Alt+H will focus the entry
 entry = gtk_entry_new ();
 label = gtk_label_new_with_mnemonic (\"_Hello\");
 gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
    @end{pre}
  @subheading{Markup (styled text)}
    To make it easy to format text in a label (changing colors, fonts, etc.),
    label text can be provided in a simple markup format. Here is how to create
    a label with a small font:
    @begin{pre}
 label = gtk_label_new (NULL);
 gtk_label_set_markup (GTK_LABEL (label),
                       \"<span style=\"color: red\">
                          <small>Small text</small></span>\");
    @end{pre}
    (See complete documentation of available tags in the Pango manual.)

    The markup passed to @fun{gtk-label-set-markup} must be valid; for example,
    literal <, > and & characters must be escaped as \<, \gt;, and \&. If you
    pass text obtained from the user, file, or a network to
    @fun{gtk-label-set-markup}, you'll want to escape it with
    @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()}.

    Markup strings are just a convenient way to set the @class{pango-attr-list}
    on a label; @fun{gtk-label-set-attributes} may be a simpler way to set
    attributes in some cases. Be careful though; @class{pango-attr-list} tends
    to cause internationalization problems, unless you are applying attributes
    to the entire string (i. e. unless you set the range of each attribute to
    @code{[0, G_MAXINT]}). The reason is that specifying the @code{start_index}
    and @code{end_index} for a @code{PangoAttribute} requires knowledge of the
    exact string being displayed, so translations will cause problems.

  @subheading{Selectable labels}
    Labels can be made selectable with @fun{gtk-label-set-selectable}.
    Selectable labels allow the user to copy the label contents to the
    clipboard. Only labels that contain useful-to-copy information - such as
    error messages - should be made selectable.

  @subheading{Text layout}
    A label can contain any number of paragraphs, but will have performance
    problems if it contains more than a small number. Paragraphs are separated
    by newlines or other paragraph separators understood by Pango.

    Labels can automatically wrap text if you call the function
    @fun{gtk-label-set-line-wrap}.

    The function @fun{gtk-label-justify} sets how the lines in a label align
    with one another. If you want to set how the label as a whole aligns in its
    available space, see the function @fun{gtk-misc-set-alignment}.

    The @code{\"width-chars\"} and @code{\"max-width-chars\"} properties can be
    used to control the size allocation of ellipsized or wrapped labels. For
    ellipsizing labels, if either is specified (and less than the actual text
    size), it is used as the minimum width, and the actual text size is used as
    the natural width of the label. For wrapping labels, @code{\"width-chars\"}
    is used as the minimum width, if specified, and @code{\"max-width-chars\"}
    is used as the natural width. Even if @code{\"max-width-chars\"} specified,
    wrapping labels will be rewrapped to use all of the available width.

  @subheading{Note}
    Note that the interpretation of the properties @code{\"width-chars\"} and
    @code{\"max-width-chars\"} has changed a bit with the introduction of
    \"width-for-height\" geometry management.

  @subheading{Links}
    Since 2.18, GTK+ supports markup for clickable hyperlinks in addition to
    regular Pango markup. The markup for links is borrowed from HTML, using the
    @code{a} with href and title attributes. GTK+ renders links similar to the
    way they appear in web browsers, with colored, underlined text. The title
    attribute is displayed as a tooltip on the link. An example looks like this:
    @begin{pre}
 gtk_label_set_markup (label,
                       \"Go to the <span style=\"color: red\">
                                    <a>GTK+ website</a></span> for more...\");
    @end{pre}
    It is possible to implement custom handling for links and their tooltips
    with the \"activate-link\" signal and @fun{gtk-label-get-current-uri}
    function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-current-link\" signal}
      @begin{pre}
 lambda (label)   : Action
      @end{pre}
      A keybinding signal which gets emitted when the user activates a link in
      the label.
      Applications may also emit the signal with @fun{g-signal-emit-by-name} if
      they need to control activation of URIs programmatically.
      The default bindings for this signal are all forms of the Enter key.
      @begin[code]{table}
        @entry[label]{The label on which the signal was emitted.}
      @end{table}
      Since 2.18

    @subheading{The \"activate-link\" signal}
      @begin{pre}
 lambda (label uri)   : Run Last
      @end{pre}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the function
      @fun{gtk-show-uri}.
      @begin[code]{table}
        @entry[label]{The label on which the signal was emitted.}
        @entry[uri]{The URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{table}
      Since 2.18

    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (label)   : Action
      @end{pre}
      The \"copy-clipboard\" signal is a keybinding signal which gets emitted to
      copy the selection to the clipboard.
      The default binding for this signal is Ctrl-c.
      @begin[code]{table}
        @entry[label]{The object which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (entry step count extended-selection)   : Action
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement. If the cursor is not visible in
      entry, this signal causes the viewport to be moved instead.
      Applications should not connect to it, but may emit it with
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
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{The number of step units to move.}
        @entry[extend-selection]{@em{True} if the move should extend the
          selection.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (label menu)   : Run Last
      @end{pre}
      The \"populate-popup\" signal gets emitted before showing the context menu
      of the label. Note that only selectable labels have context menus.
      If you need to add items to the context menu, connect to this signal and
      append your menuitems to the menu.
      @begin[code]{table}
        @entry[label]{The label on which the signal is emitted.}
        @entry[menu]{The menu that is being populated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-label-angle}
  @see-slot{gtk-label-attributes}
  @see-slot{gtk-label-cursor-position}
  @see-slot{gtk-label-ellipsize}
  @see-slot{gtk-label-justify}
  @see-slot{gtk-label-label}
  @see-slot{gtk-label-lines}
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
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-label-angle --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "angle" 'gtk-label) 't)
 "The @code{\"angle\"} property of type @code{:double} (Read / Write) @br{}
  The angle that the baseline of the label makes with the horizontal, in
  degrees, measured counterclockwise. An angle of 90 reads from
  bottom to top, an angle of 270, from top to bottom. Ignored if the
  label is selectable, wrapped, or ellipsized. @br{}
  Allowed values: [0,360] @br{}
  Default value: 0 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-angle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-angle 'function)
 "@version{2014-3-7}
  @argument[object]{a @class{gtk-label} widget}
  @argument[angle]{the angle that the baseline of the label makes with the
    horizontal, in degrees, measured counterclockwise}
  @syntax[]{(gtk-label-angle object) => angle}
  @syntax[]{(setf (gtk-label-angle object) angle)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{angle} of the @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-angle} sets the angle of rotation for the
  label.

  The generic function @sym{(setf gtk-label-angle)} sets the angle of rotation
  for the label. 

  An angle of 90 reads from from bottom to top, an angle of 270, from top to
  bottom. The angle setting for the label is ignored if the label is selectable,
  wrapped, or ellipsized.

  Since 2.6
  @see-class{gtk-label}")

;;; --- gtk-label-attributes ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attributes" 'gtk-label) 't)
 "The @code{\"attributes\"} property of type @class{pango-attr-list}
  (Read / Write) @br{}
  A list of style attributes to apply to the text of the label.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-attributes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-attributes 'function)
 "@version{2014-3-9}
  @argument[object]{a @class{gtk-label} widget}
  @argument[attrs]{a @class{pango-attr-list} structure}
  @syntax[]{(gtk-label-attributes object) => attrs}
  @syntax[]{(setf (gtk-label-attributes object) attrs)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{attributes} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-attributes} gets the attribute list that
  was set on the label using the generic function
  @sym{(setf gtk-label-attributes)}, if any.

  This function does not reflect attributes that come from the labels markup,
  see the function @fun{gtk-label-set-markup}. If you want to get the
  effective attributes for the label, use
  @code{(pango-layout-get-attribute (gtk-label-get-layout label))}.

  The generic function @sym{(setf gtk-label-attributes)} sets a
  @class{pango-attr-list}; the attributes in the list are applied to the label
  text.

  @begin[Note]{dictionary}
    The attributes set with this function will be applied and merged with any
    other attributes previously effected by way of the 
    @slot[gtk-label]{use-underline} or @slot[gtk-label]{use-markup} properties.
    While it is not recommended to mix markup strings with manually set
    attributes, if you must; know that the attributes will be applied to the
    label after the markup string is parsed.
  @end{dictionary}
  @see-class{gtk-label}
  @see-function{gtk-label-set-markup}")

;;; --- gtk-label-cursor-position ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-position" 'gtk-label) 't)
 "The @code{\"cursor-position\"} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-cursor-position 'function)
 "@version{2014-3-9}
  Accessor of the slot @slot[gtk-label]{cursor-position} of the
  @class{gtk-label} class.
  @see-class{gtk-label}")

;;; --- gtk-label-ellipsize ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize" 'gtk-label) 't)
 "The @code{\"ellipsize\"} property of type @symbol{pango-ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a value of the
  @symbol{pango-ellipsize-mode} enumeration.
  Note that setting this property to a value other than @code{:none} has the
  side-effect that the label requests only enough space to display the ellipsis
  \"...\". In particular, this means that ellipsizing labels do not work well
  in notebook tabs, unless the tab's @code{\"tab-expand\"} property is set to
  @arg{true}. Other ways to set a label's width are the functions
  @fun{gtk-widget-size-request} and @fun{gtk-label-set-width-chars}. @br{}
  Default value: @code{:none} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-ellipsize 'function)
 "@version{2014-3-9}
  @argument[object]{a @class{gtk-label} widget}
  @argument[mode]{a @symbol{pango-ellipsize-mode}}
  @syntax[]{(gtk-label-ellipsize object) => mode}
  @syntax[]{(setf (gtk-label-ellipsize object) mode)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{ellipsize} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-ellipsize} returns the ellipsizing
  position of type @symbol{pango-ellipsize-mode} of the label.

  The generic function @sym{(setf gtk-label-ellipsize)} sets the mode used to
  ellipsize, add an ellipsis: \"...\", to the text if there is not enough space
  to render the entire string.

  Since 2.6
  @see-class{gtk-label}
  @see-symbol{pango-ellipsize-mode}")

;;; --- gtk-label-justify ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "justify" 'gtk-label) 't)
 "The @code{\"justify\"} property of type @symbol{gtk-justification}
  (Read / Write) @br{}
  The alignment of the lines in the text of the label relative to each other.
  This does @em{not} affect the alignment of the label within its allocation.
  See the @slot[gtk-misc]{xalign} property of the @class{gtk-misc} widget for
  that. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-justify atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-justify 'function)
 "@version{2015-2-22}
  @argument[object]{a @class{gtk-label} widget}
  @argument[jtype]{a @symbol{gtk-justification}}
  @syntax[]{(gtk-label-justify object) => jtype}
  @syntax[]{(setf (gtk-label-justify object) jtype)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{justify} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-justify} returns the justification of
  type @symbol{gtk-justification} of the label.

  The generic function @sym{(setf gtk-label-justify)} sets the alignment of the
  lines in the text of the label relative to each other.

  @code{:left} is the default value when the widget is first created with the
  function @fun{gtk-label-new}. If you instead want to set the alignment of the
  label as a whole, use the function @fun{gtk-misc-set-alignment} instead.
  The generic function @sym{gtk-label-justify} has no effect on labels
  containing only a single line.
  @see-class{gtk-label}
  @see-symbol{gtk-justification}
  @see-function{gtk-label-new}
  @see-function{gtk-misc-set-alignment}")

;;; --- gtk-label-label --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-label) 't)
 "The @code{\"label\"} property of type @code{:string} (Read / Write) @br{}
  The text of the label. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-label 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[str]{the new text to set for the label}
  @syntax[]{(gtk-label-label object) => str}
  @syntax[]{(setf (gtk-label-label object) str)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{label} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-label} returns the text of the label
  widget including any embedded underlines indicating mnemonics and Pango
  markup. See the function @fun{gtk-label-get-text}.

  The generic function @sym{(setf gtk-label-label} sets the text of the label.

  The label is interpreted as including embedded underlines and/or Pango markup
  depending on the values of the @slot[gtk-label]{use-underline} and
  @slot[gtk-label]{use-markup} properties.
  @see-class{gtk-label}
  @see-function{gtk-label-get-text}")

;;; --- gtk-label-lines --------------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "lines" 'gtk-label) 't)
 "The @code{\"lines\"} property of type @code{:int} (Read / Write) @br{}
  The number of lines to which an ellipsized, wrapping label should be limited.
  This property has no effect if the label is not wrapping or ellipsized. Set
  this property to -1 if you do not want to limit the number of lines. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}
  Since 3.10")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-label-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-lines 'function)
 "@version{2014-9-11}
  @argument[object]{a @class{gtk-label} widget}
  @argument[lines]{the desired number of lines, or -1}
  @syntax[]{(gtk-label-lines object) => lines}
  @syntax[]{(setf (gtk-label-lines object) lines)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{lines} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-lines} gets the number of lines to which
  an ellipsized, wrapping label should be limited.

  The generic function @sym{(setf gtk-label-lines)} sets the number of lines
  to which an ellipsized, wrapping label should be limited. This has no effect
  if the label is not wrapping or ellipsized. Set this to -1 if you do not want
  to limit the number of lines.

  Since 3.10
  @see-class{gtk-label}")

;;; --- gtk-label-max-width-chars ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-width-chars" 'gtk-label) 't)
 "The @code{\"max-width-chars\"} property of type @code{:int}
  (Read / Write) @br{}
  The desired maximum width of the label, in characters. If this property is
  set to -1, the width will be calculated automatically. See the section on text
  layout for details of how the properties @slot[gtk-label]{width-chars} and
  @code{\"max-width-chars\"} determine the width of ellipsized and wrapped
  labels. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-max-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-max-width-chars 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[n-chars]{the new desired maximum width, in characters}
  @syntax[]{(gtk-label-max-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-label-max-width-chars object) n-chars)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{max-width-chars} of the
    @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-max-width-chars} returns the maximum width
  of the label in characters.

  The generic function @sym{(setf gtk-label-max-width-chars)} sets the desired
  maximum width in characters of the label to @arg{n-chars}.

  Since 2.6
  @see-class{gtk-label}")

;;; --- gtk-label-mnemonic-keyval ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonic-keyval" 'gtk-label) 't)
 "The @code{\"mnemonic-keyval\"} property of @code{:uint} (Read) @br{}
  The mnemonic accelerator key for this label. @br{}
  Default value: 16777215")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-mnemonic-keyval atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-mnemonic-keyval 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{mnemonic-keyval} of the
    @class{gtk-label} class.
  @end{short}

  If the label has been set so that it has an mnemonic key the generic function
  @sym{gtk-label-mnemonic-keyval} returns the keyval used for the mnemonic
  accelerator. If there is no mnemonic set up it returns @code{GDK_VoidSymbol}.
  @see-class{gtk-label}")

;;; --- gtk-label-mnemonic-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonic-widget" 'gtk-label) 't)
 "The @code{\"mnemonic-widget\"} property of type @class{gtk-widget}
  (Read / Write) @br{}
  The widget to be activated when the label's mnemonic key is pressed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-mnemonic-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-mnemonic-widget 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[widget]{the target @class{gtk-widget}}
  @syntax[]{(gtk-label-mnemonic-widget object) => widget}
  @syntax[]{(setf (gtk-label-mnemonic-widget object) widget)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{mnemonic-widget} of the
    @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-mnemonic-widget} returns the target of the
  label's mnemonic, or @code{nil} if none has been set and the default algorithm
  will be used.

  If the label has been set so that it has an mnemonic key, using i. e.
  the functions @fun{gtk-label-set-markup-with-mnemonic},
  @fun{gtk-label-set-text-with-mnemonic}, @fun{gtk-label-new-with-mnemonic} or
  the @slot[gtk-label]{use-underline} property, the label can be associated with
  a widget that is the target of the mnemonic.

  When the label is inside a widget, like a @class{gtk-button} or a
  @class{gtk-notebook} tab, it is automatically associated with the correct
  widget, but sometimes, i. e. when the target is a @class{gtk-entry} next to
  the label, you need to set it explicitly using this function.

  The target widget will be accelerated by emitting the \"mnemonic-activate\"
  signal on it. The default handler for this signal will activate the widget if
  there are no mnemonic collisions and toggle focus between the colliding
  widgets otherwise.
  @see-class{gtk-label}
  @see-function{gtk-label-set-markup-with-mnemonic}
  @see-function{gtk-label-set-text-with-mnemonic}
  @see-function{gtk-label-new-with-mnemonic}")

;;; --- gtk-label-pattern ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pattern" 'gtk-label) 't)
 "The @code{\"pattern\"} property of type @code{:string} (Write) @br{}
  A string with \"_\" characters in positions correspond to characters in the
  text to underline. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-pattern atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-pattern 'function)
 "@version{2014-3-16}
  @argument[object]{the @class{gtk-label} you want to set the pattern to}
  @argument[pattern]{the pattern as described below}
  @syntax[]{(setf (gtk-label-pattern object) pattern)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{pattern} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{(setf gtk-label-pattern)} sets the pattern of
  underlines you want under the existing text within the label. For example if
  the current text of the label says \"FooBarBaz\" passing a pattern of
  \"___ ___\" will underline \"Foo\" and \"Baz\" but not \"Bar\".
  @see-class{gtk-label}")

;;; --- gtk-label-selectable ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selectable" 'gtk-label) 't)
 "The @code{\"selectable\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the label text can be selected with the mouse. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-selectable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-selectable 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} to allow selecting text in the label}
  @syntax[]{(gtk-label-selectable object) => setting}
  @syntax[]{(setf (gtk-label-selectable object) setting)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{selectable} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-selectable} gets the value set by
  the generic function @sym{(gtk-label-selectable)}.

  Selectable labels allow the user to select text from the label, for
  copy-and-paste.
  @see-class{gtk-label}")

;;; --- gtk-label-selection-bound ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-bound" 'gtk-label) 't)
 "The @code{\"selection-bound\"} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in
  chars. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-selection-bound atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-selection-bound 'function)
 "@version{2014-3-16}
  Accessor of the slot @slot[gtk-label]{selection-bound} of the
  @class{gtk-label} class.
  @see-class{gtk-label}")

;;; --- gtk-label-single-line-mode ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "single-line-mode"
                                               'gtk-label) 't)
 "The @code{\"single-line-mode\"} property of type  @code{:boolean}
  (Read / Write) @br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to ascent
  + descent of the font. This can be an advantage in situations where resizing
  the label because of text changes would be distracting, e. g. in a
  statusbar. @br{}
  Default value: @code{nil} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-single-line-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-single-line-mode 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[single-line-mode]{@em{true} if the label should be in single line
    mode}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{single-line-mode} of the
    @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-single-line-mode} returns whether the
  label is in single line mode.


  The generic function @sym{(setf gtk-label-single-line-mode} sets whether the
  label is in single line mode.

  Since 2.6
  @see-class{gtk-label}")

;;; --- gtk-label-track-visited-links ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-visited-links"
                                               'gtk-label) 't)
 "The @code{\"track-visited-links\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Set this property to @arg{true} to make the label track which links have been
  clicked. It will then apply the color of the style property
  @code{\"visited-link-color\"} of a @class{gtk-widget} object, instead of
  the color of the style property @code{\"link-color\"}. @br{}
  Default value: @em{true} @br{}
  Since 2.18")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-track-visited-links atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-track-visited-links 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[track-links]{@em{True} to track visited links.}
  @syntax[]{(gtk-label-track-visited-links object) => track-links}
  @syntax[]{(setf (gtk-label-track-visited-links object) track-links)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{track-visited-links} of the
    @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-track-visited-links} returns whether the
  label is currently keeping track of clicked links.

  The generic function @sym{(setf gtk-label-track-visited-links} sets whether
  the label should keep track of clicked links and use a different color for
  them.

  Since 2.18
  @see-class{gtk-label}")

;;; --- gtk-label-use-markup ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-label) 't)
 "The @code{\"use-markup\"} property of type @code{:boolean}
  (Read / Write) @br{}
  The text of the label includes XML markup. See the function
  @fun{pango-parse-markup}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-use-markup 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} if the label's text should be parsed for markup}
  @syntax[]{(gtk-label-use-markup object) => setting}
  @syntax[]{(setf (gtk-label-use-markup object) setting)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{use-markup} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-use-markup} returns whether the label's
  text is interpreted as marked up with the Pango text markup language.

  The generic function @sym{(setf gtk-label-use-markup)} sets whether the text
  of the label contains markup in Pango's text markup language.

  See the generic function @fun{gtk-label-markup}.
  @see-class{gtk-label}
  @see-function{gtk-label-markup}")

;;; --- gtk-label-use-underline ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline" 'gtk-label) 't)
 "The @code{\"use-underline\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-use-underline 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @syntax[]{(gtk-label-use-underline object) => setting}
  @syntax[]{(setf (gtk-label-use-underline object) setting)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{use-underline} of the
    @class{gtk-label} class.
  @end{short}

  The generic function @sym{gtk-label-use-underline} returns whether an embedded
  underline in the label indicates a mnemonic.

  If @em{true}, an underline in the text indicates the next character should
  be used for the mnemonic accelerator key.
  @see-class{gtk-label}")

;;; --- gtk-label-width-chars --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-label) 't)
 "The @code{\"width-chars\"} property of @code{:int} (Read / Write) @br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically.
  See the section on text layout for details of how the properties
  @code{\"width-chars\"} and @slot[gtk-label]{max-width-chars} determine the
  width of ellipsized and wrapped labels. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-width-chars 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-label} widget}
  @argument[n-chars]{the new desired width, in characters}
  @syntax[]{(gtk-label-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-label-width-chars object) n-chars)}
  @begin{short}
    Accessor of the slot @slot[gtk-label]{width-chars} of the @class{gtk-label}
    class.
  @end{short}

  The generic function @sym{gtk-label-width-chars} retrieves the desired width
  of the label, in characters.

  The generic function @sym{(setf gtk-label-width-chars)} sets the desired width
  in characters of the label to @arg{n-chars}.

  Since 2.6
  @see-class{gtk-label}")

;;; --- gtk-label-wrap ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap" 'gtk-label) 't)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  If set, wrap lines if the text becomes too wide. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-wrap 'function)
 "@version{2014-3-16}
  Accessor of the slot @slot[gtk-label]{wrap} of the @class{gtk-label} class.
  @see-class{gtk-label}")

;;; --- gtk-label-wrap-mode ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-label) 't)
 "The @code{wrap-mode} property of type @symbol{pango-wrap-mode}
  (Read / Write) @br{}
  If line wrapping is on, see the @slot[gtk-label]{wrap} property) this controls
  how the line wrapping is done. The default is @code{:word}, which means wrap
  on word boundaries. @br{}
  Default value: @code{:word} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-wrap-mode 'function)
 "@version{2014-3-16}
  Accessor of the slot @slot[gtk-label]{wrap-mode} of the @class{gtk-label}
  class.
  @see-class{gtk-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_label_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-label-new))

(defun gtk-label-new (str)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[str]{the text of the label}
  @return{The new @class{gtk-label} widget.}
  Creates a new label with the given text inside it. You can pass @code{nil} to
  get an empty label widget.
  @see-class{gtk-label}"
  (make-instance 'gtk-label
                 :label str))

(export 'gtk-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_text" gtk-label-set-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[str]{the text you want to set}
  @begin{short}
    Sets the text within the label. It overwrites any text that was there
    before.
  @end{short}
  This will also clear any previously set mnemonic accelerators.
  @see-class{gtk-label}
  @see-function{gtk-label-set-markup}"
  (label (g-object gtk-label))
  (str :string))

(export 'gtk-label-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup" gtk-label-set-markup) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[str]{a markup string (see Pango markup format)}
  @begin{short}
    Parses @arg{str} which is marked up with the Pango text markup language,
    setting the @arg{label}'s text and attribute list based on the parse
    results.
  @end{short}
  If the @arg{str} is external data, you may need to escape it with
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()}:
  @begin{pre}
 char *markup;

 markup = g_markup_printf_escaped (\"<span style=\\\"italic\\\">%s</span>\", str);
  gtk_label_set_markup (GTK_LABEL (label), markup);
  g_free (markup);
  @end{pre}
  @see-class{gtk-label}
  @see-function{gtk-label-set-text}
  @see-function{gtk-label-set-markup-with-mnemonic}"
  (label (g-object gtk-label))
  (str :string))

(export 'gtk-label-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup_with_mnemonic"
          gtk-label-set-markup-with-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[str]{a markup string (see Pango markup format)}
  @begin{short}
    Parses @arg{str} which is marked up with the Pango text markup language,
    setting the @arg{label}'s text and attribute list based on the parse
    results.
  @end{short}
  If characters in @arg{str} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.

  The mnemonic key can be used to activate another widget, chosen
  automatically, or explicitly using @fun{gtk-label-mnemonic-widget}.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (label (g-object gtk-label))
  (str :string))

(export 'gtk-label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_line_wrap" gtk-label-set-line-wrap) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[wrap]{the setting}
  @begin{short}
    Toggles line wrapping within the @class{gtk-label} widget.
  @end{short}
  @em{True} makes it break lines if text exceeds the widget's size. @code{Nil}
  lets the text get cut off by the edge of the widget if it exceeds the widget
  size.

  Note that setting line wrapping to @em{true} does not make the label wrap at
  its parent container's width, because GTK+ widgets conceptually cannot make
  their requisition depend on the parent container's size. For a label that
  wraps at a specific position, set the label's width using the function
  @fun{gtk-widget-size-request}.
  @see-class{gtk-label}
  @see-function{gtk-widget-size-request}"
  (label (g-object gtk-label))
  (wrap :boolean))

(export 'gtk-label-set-line-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap_mode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_line_wrap_mode" gtk-label-set-line-wrap-mode) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[wrap-mode]{the line wrapping mode}
  @begin{short}
    If line wrapping is on, see the function @fun{gtk-label-set-line-wrap},
    this controls how the line wrapping is done.
  @end{short}
  The default is @code{:word} which means wrap on word boundaries.

  Since 2.10
  @see-class{gtk-label}
  @see-function{gtk-label-set-line-wrap}"
  (label (g-object gtk-label))
  (wrap-mode pango-wrap-mode))

(export 'gtk-label-set-line-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout_offsets" %gtk-label-get-layout-offsets) :void
  (label (g-object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-label-get-layout-offsets (label)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    @code{x} -- x offset of layout, or @code{nil} @br{}
    @code{y} -- y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the coordinates where the label will draw the @class{pango-layout}
    representing the text in the label; useful to convert mouse events into
    coordinates inside the @class{pango-layout}, e. g. to take some action if
    some part of the label is clicked.
  @end{short}
  Of course you will need to create a @class{gtk-event-box} to receive the
  events, and pack the label inside it, since labels are a
  @code{GTK_NO_WINDOW} widget. Remember when using the @class{pango-layout}
  functions you need to convert to and from pixels using the function
  @fun{pango-pixels} or the constant @var{+pango-scale+}.
  @see-class{gtk-label}
  @see-class{gtk-event-box}
  @see-class{pango-layout}
  @see-variable{+pango-scale+}
  @see-function{pango-pixels}"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-label-get-layout-offsets label x y)
    (list (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-label-get-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_text" gtk-label-get-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    The text in the label widget.
  @end{return}
  @begin{short}
    Fetches the text from a label widget, as displayed on the screen.
  @end{short}
  This does not include any embedded underlines indicating mnemonics or Pango
  markup. See the function @fun{gtk-label-label}.
  @see-class{gtk-label}
  @see-function{gtk-label-label}"
  (label (g-object gtk-label)))

(export 'gtk-label-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

;; TODO: Check implementation with make-instance

(defcfun ("gtk_label_new_with_mnemonic" gtk-label-new-with-mnemonic)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[str]{the text of the label, with an underscore in front of
    the mnemonic character}
  @return{The new @class{gtk-label} widget.}
  @begin{short}
    Creates a new @class{gtk-label} widget, containing the text in @arg{str}.
  @end{short}

  If characters in @arg{str} are preceded by an underscore, they are underlined.
  If you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. The mnemonic key can be used to activate
  another widget, chosen automatically, or explicitly using the function
  @fun{gtk-label-mnemonic-widget}.

  If the function @fun{gtk-label-mnemonic-widget} is not called, then the first
  activatable ancestor of the @class{gtk-label} will be chosen as the mnemonic
  widget. For instance, if the label is inside a button or menu item, the button
  or menu item will automatically become the mnemonic widget and be activated by
  the mnemonic.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (str :string))

(export 'gtk-label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_select_region" gtk-label-select-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[start-offset]{start offset (in characters not bytes)}
  @argument[end-offset]{end offset (in characters not bytes)}
  @begin{short}
    Selects a range of characters in the label, if the label is selectable.
  @end{short}
  See the function @fun{gtk-label-selectable}. If the label is not
  selectable, this function has no effect. If @arg{start-offset} or
  @arg{end-offset} are -1, then the end of the label will be substituted.
  @see-class{gtk-label}
  @see-function{gtk-label-selectable}"
  (label (g-object gtk-label))
  (start-offset :int)
  (end-offset :int))

(export 'gtk-label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_text_with_mnemonic" gtk-label-set-text-with-mnemonic)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @argument[str]{a string}
  @begin{short}
    Sets the label's text from the string @arg{str}.
  @end{short}
  If characters in @arg{str} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic. The
  mnemonic key can be used to activate another widget, chosen automatically, or
  explicitly using the function @fun{gtk-label-mnemonic-widget}.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (label (g-object gtk-label)))

(export 'gtk-label-set-text-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout" gtk-label-get-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @return{The @class{pango-layout} for this label.}
  @begin{short}
    Gets the @class{pango-layout} used to display the label.
  @end{short}
  The layout is useful to e. g. convert text positions to pixel positions, in
  combination with the function @fun{gtk-label-get-layout-offsets}. The label
  is free to recreate its layout at any time, so it should be considered
  read-only.
  @see-class{gtk-label}
  @see-class{pango-layout}
  @see-function{gtk-label-get-layout-offsets}"
  (label (g-object gtk-label)))

(export 'gtk-label-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_line_wrap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_line_wrap" gtk-label-get-line-wrap) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @return{@em{True} if the lines of the label are automatically wrapped.}
  @begin{short}
    Returns whether lines in the label are automatically wrapped.
  @end{short}
  See the function @fun{gtk-label-set-line-wrap}.
  @see-class{gtk-label}
  @see-function{gtk-label-set-line-wrap}"
  (label (g-object gtk-label)))

(export 'gtk-label-get-line-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_line_wrap_mode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_line_wrap_mode" gtk-label-get-line-wrap-mode)
    pango-wrap-mode
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @return{@em{True} if the lines of the label are automatically wrapped.}
  @short{Returns the line wrap mode used by the label.}
  See the function @fun{gtk-label-set-line-wrap-mode}.

  Since 2.10
  @see-class{gtk-label}
  @see-function{gtk-label-set-line-wrap-mode}"
  (label (g-object gtk-label)))

(export 'gtk-label-get-line-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_selection_bounds" %gtk-label-get-selection-bounds)
    :boolean
  (label (g-object gtk-label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun gtk-label-get-selection-bounds (label)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    @code{start} -- start of selection, as a character offset @br{}
    @code{end} -- end of selection, as a character offset, @br{}
    or @code{nil} if selection is empty
  @end{return}
  Gets the selected range of characters in the label, returning @em{true}
  if there is a selection.
  @see-class{gtk-label}"
  (with-foreign-objects ((start :int) (end :int))
    (when (%gtk-label-get-selection-bounds label start end)
      (values (mem-ref start :int)
              (mem-ref end :int)))))

(export 'gtk-label-get-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_current_uri" gtk-label-get-current-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    The currently active URI.
  @end{return}
  @begin{short}
    Returns the URI for the currently active link in the label.
  @end{short}
  The active link is the one under the mouse pointer or, in a selectable label,
  the link in which the text cursor is currently positioned.

  This function is intended for use in a \"activate-link\" signal handler or
  for use in a \"query-tooltip\" signal handler.

  Since 2.18
  @see-class{gtk-label}"
  (label (g-object gtk-label)))

(export 'gtk-label-get-current-uri)

;;; --- End of file gtk.label.lisp ---------------------------------------------
