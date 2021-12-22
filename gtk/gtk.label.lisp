;;; ----------------------------------------------------------------------------
;;; gtk.label.lisp
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License. If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkLabel
;;;
;;;     A widget that displays a small to medium amount of text.
;;;
;;; Types and Values
;;;
;;;     GtkLabel
;;;
;;; Functions
;;;
;;;     gtk_label_new
;;;     gtk_label_set_text
;;;     gtk_label_set_attributes                           Accessor
;;;     gtk_label_set_markup
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_set_pattern                              Accessor
;;;     gtk_label_set_justify                              Accessor
;;;     gtk_label_set_xalign                               Accessor
;;;     gtk_label_set_yalign                               Accessor
;;;     gtk_label_set_ellipsize                            Accessor
;;;     gtk_label_set_width_chars                          Accessor
;;;     gtk_label_set_max_width_chars                      Accessor
;;;     gtk_label_set_line_wrap
;;;     gtk_label_set_line_wrap_mode
;;;     gtk_label_set_lines                                Accessor
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_get_mnemonic_keyval                      Accessor
;;;     gtk_label_get_selectable                           Accessor
;;;     gtk_label_get_text
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_select_region
;;;     gtk_label_set_mnemonic_widget                      Accessor
;;;     gtk_label_set_selectable                           Accessor
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_get_attributes                           Accessor
;;;     gtk_label_get_justify                              Accessor
;;;     gtk_label_get_xalign                               Accessor
;;;     gtk_label_get_yalign                               Accessor
;;;     gtk_label_get_ellipsize                            Accessor
;;;     gtk_label_get_width_chars                          Accessor
;;;     gtk_label_get_max_width_chars                      Accessor
;;;     gtk_label_get_label                                Accessor
;;;     gtk_label_get_layout
;;;     gtk_label_get_line_wrap
;;;     gtk_label_get_line_wrap_mode
;;;     gtk_label_get_lines                                Accessor
;;;     gtk_label_get_mnemonic_widget                      Accessor
;;;     gtk_label_get_selection_bounds
;;;     gtk_label_get_use_markup                           Accessor
;;;     gtk_label_get_use_underline                        Accessor
;;;     gtk_label_get_single_line_mode                     Accessor
;;;     gtk_label_get_angle                                Accessor
;;;     gtk_label_set_label                                Accessor
;;;     gtk_label_set_use_markup                           Accessor
;;;     gtk_label_set_use_underline                        Accessor
;;;     gtk_label_set_single_line_mode                     Accessor
;;;     gtk_label_set_angle                                Accessor
;;;     gtk_label_get_current_uri
;;;     gtk_label_set_track_visited_links                  Accessor
;;;     gtk_label_get_track_visited_links                  Accessor
;;;
;;; Properties
;;;
;;;                gdouble  angle                Read / Write
;;;         PangoAttrList*  attributes           Read / Write
;;;                   gint  cursor-position      Read
;;;     PangoEllipsizeMode  ellipsize            Read / Write
;;;       GtkJustification  justify              Read / Write
;;;                gchar *  label                Read / Write
;;;                   gint  lines                Read / Write
;;;                   gint  max-width-chars      Read / Write
;;;                  guint  mnemonic-keyval      Read
;;;            GtkWidget *  mnemonic-widget      Read / Write
;;;                gchar *  pattern                     Write
;;;               gboolean  selectable           Read / Write
;;;                   gint  selection-bound      Read
;;;               gboolean  single-line-mode     Read / Write
;;;               gboolean  track-visited-links  Read / Write
;;;               gboolean  use-markup           Read / Write
;;;               gboolean  use-underline        Read / Write
;;;                   gint  width-chars          Read / Write
;;;               gboolean  wrap                 Read / Write
;;;          PangoWrapMode  wrap-mode            Read / Write
;;;                 gfloat  xalign               Read / Write
;;;                 gfloat  yalign               Read / Write
;;;
;;; Signals
;;;
;;;         void  activate-current-link  Action
;;;     gboolean  activate-link          Run Last
;;;         void  copy-clipboard         Action
;;;         void  move-cursor            Action
;;;         void  populate-popup         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkLabel
;;;                     ╰── GtkAccelLabel
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLabel implements AtkImplementorIface and GtkBuildable.
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
    "wrap-mode" "PangoWrapMode" t t)
   (xalign
    gtk-label-xalign
    "xalign" "gfloat" t t)
   (yalign
    gtk-label-yalign
    "yalign" "gfloat" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-label 'type)
 "@version{*2021-10-31}
  @begin{short}
    The @sym{gtk-label} widget displays a small amount of text.
  @end{short}
  As the name implies, most labels are used to label another widget such as
  a @class{gtk-button}, a @class{gtk-menu-item}, or a @class{gtk-combo-box}
  widget.

  @subheading{Mnemonics}
  Labels may contain mnemonics. Mnemonics are underlined characters in the
  label, used for keyboard navigation. Mnemonics are created by providing a
  string with an underscore before the mnemonic character, such as \"_File\",
  to the @fun{gtk-label-new-with-mnemonic} or
  @fun{gtk-label-set-text-with-mnemonic} functions.

  Mnemonics automatically activate any activatable widget the label is inside,
  such as a @class{gtk-button} widget. If the label is not inside the
  target widget of the mnemonic, you have to tell the label about the target
  using the @fun{gtk-label-mnemonic-widget} function. Here is a simple example
  where the label is inside a button:
  @begin{pre}
;; Pressing Alt+H will activate this button
(let ((button (make-instance 'gtk-button)))
   (gtk-container-add button
                      (gtk-label-new-with-mneonic \"_Hello\"))
   ... )
  @end{pre}
  There is a convenience function to create buttons with a mnemonic label
  already inside:
  @begin{pre}
;; Pressing Alt+H will activate this button
(gtk-button-new-with-mnemonic \"_Hello\")
  @end{pre}
  To create a mnemonic for a widget alongside the label, such as a
  @class{gtk-entry} widget, you have to point the label at the entry with the
  @fun{gtk-label-mnemonic-widget} function:
  @begin{pre}
;; Pressing Alt+H will focus the entry
(let ((entry (make-instance 'gtk-entry))
      (label (gtk-label-new-with-mnemonic \"_Hello\")))
   (gtk-label-mnemonic-widget label entry)
   ... )
  @end{pre}
  @subheading{Markup (styled text)}
  To make it easy to format text in a label, changing colors, fonts, etc.,
  label text can be provided in a simple markup format. Here is how to create
  a label with a small font:
  @begin{pre}
(let ((label (make-instance 'gtk-label)))
  (gtk-label-set-markup label
                        \"<span style=\"color: red\">
                         <small>Small text</small></span>\")
  ... )
  @end{pre}
  See complete documentation of available tags in the Pango manual.

  The markup passed to the @fun{gtk-label-set-markup} function must be valid.
  For example, literal <, > and & characters must be escaped as \<, \gt;, and
  \&. If you pass text obtained from the user, file, or a network to the
  @fun{gtk-label-set-markup} function, you will want to escape it with the
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()} functions.

  Markup strings are just a convenient way to set the @class{pango-attr-list}
  instance on a label. The @fun{gtk-label-attributes} slot access function may
  be a simpler way to set attributes in some cases. Be careful though,
  a @class{pango-attr-list} instance tends to cause internationalization
  problems, unless you are applying attributes to the entire string, i.e. unless
  you set the range of each attribute to @code{[0, G_MAXINT]}). The reason is
  that specifying the @code{start_index} and @code{end_index} for a
  @class{pango-attribute} structure requires knowledge of the exact string being
  displayed, so translations will cause problems.

  @subheading{Selectable labels}
  Labels can be made selectable with the @fun{gtk-label-selectable} slot access
  function. Selectable labels allow the user to copy the label contents to the
  clipboard. Only labels that contain useful-to-copy information
  - such as error messages - should be made selectable.

  @subheading{Text layout}
  A label can contain any number of paragraphs, but will have performance
  problems if it contains more than a small number. Paragraphs are separated
  by newlines or other paragraph separators understood by Pango.

  Labels can automatically wrap text if you call the @fun{gtk-label-line-wrap}
  function.

  The @fun{gtk-label-justify} slot access function sets how the lines in a label
  align with one another. If you want to set how the label as a whole aligns in
  its available space, see the @slot[gtk-widget]{halign} and
  @slot[gtk-widget]{valign} properties.

  The @slot[gtk-label]{width-chars} and @slot[gtk-label]{max-width-chars}
  properties can be used to control the size allocation of ellipsized or
  wrapped labels. For ellipsizing labels, if either is specified and less than
  the actual text size, it is used as the minimum width, and the actual text
  size is used as the natural width of the label. For wrapping labels,
  @slot[gtk-label]{width-chars} is used as the minimum width, if specified, and
  @slot[gtk-label]{max-width-chars} is used as the natural width. Even if
  @slot[gtk-label]{max-width-chars} specified, wrapping labels will be
  rewrapped to use all of the available width.

  Note that the interpretation of the @slot[gtk-label]{width-chars} and
  @slot[gtk-label]{max-width-chars} properties has changed a bit with the
  introduction of \"width-for-height\" geometry management.

  @subheading{Links}
  GTK supports markup for clickable hyperlinks in addition to regular Pango
  markup. The markup for links is borrowed from HTML, using the @code{a} with
  href and title attributes. GTK renders links similar to the way they appear
  in web browsers, with colored, underlined text. The title attribute is
  displayed as a tooltip on the link. An example looks like this:
  @begin{pre}
(gtk-label-set-markup label
                      \"Go to the <span style=\"color: red\">
                                   <a>GTK website</a></span> for more...\")
  @end{pre}
  It is possible to implement custom handling for links and their tooltips
  with the \"activate-link\" signal and the @fun{gtk-label-current-uri}
  function.
  @begin[GtkLabel as GtkBuildable]{dictionary}
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
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 label
 ├── [selection]
 ├── [link]
 ┊
 ╰── [link]
    @end{pre}
    The @sym{gtk-label} implementation has a single CSS node with the name
    @code{label}. A wide variety of style classes may be applied to labels, such
    as the @code{.title}, @code{.subtitle}, @code{.dim-label} style classes. In
    the @class{gtk-shortcuts-window} widget, labels are used wth the
    @code{.keycap} style class.

    If the label has a selection, it gets a subnode with name @code{selection}.

    If the label has links, there is one subnode per link. These subnodes carry
    the link or visited state depending on whether they have been visited.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-current-link\" signal}
      @begin{pre}
 lambda (label)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user activates a link in
      the label. Applications may also emit the signal with the
      @fun{g-signal-emit} function if they need to control activation of URIs
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
      @begin[code]{table}
        @entry[label]{The @sym{gtk-label} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
 lambda (label uri)    :run-last
      @end{pre}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the
      @fun{gtk-show-uri} function.
      @begin[code]{table}
        @entry[label]{The @sym{gtk-label} widget on which the signal was
          emitted.}
        @entry[uri]{A string with the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (label)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to copy the selection
      to the clipboard. The default binding for this signal is the @kbd{Ctrl-c}
      key.
      @begin[code]{table}
        @entry[label]{The @sym{gtk-label} widget which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (label step count extend)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the label,
      this signal causes the viewport to be moved instead. Applications should
      not connect to it, but may emit it with the @fun{g-signal-emit} function
      if they need to control the cursor programmatically. The default bindings
      for this signal come in two variants, the variant with the @kbd{Shift}
      modifier extends the selection, the variant without the @kbd{Shift}
      modifer does not. There are too many key combinations to list them all
      here.
      @begin{itemize}
        @item{Arrow keys move by individual characters/lines.}
        @item{@kbd{Ctrl}-arrow key combinations move by words/paragraphs.}
        @item{@kbd{Home}/@kbd{End} keys move to the ends of the buffer.}
      @end{itemize}
      @begin[code]{table}
        @entry[label]{The @sym{gtk-label} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk-movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (label menu)    :run-last
      @end{pre}
      The signal gets emitted before showing the context menu of the label. Note
      that only selectable labels have context menus. If you need to add items
      to the context menu, connect to this signal and append your menuitems to
      the menu.
      @begin[code]{table}
        @entry[label]{The @sym{gtk-label} widget on which the signal is
          emitted.}
        @entry[menu]{The @class{gtk-menu} widget that is being populated.}
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
  @see-slot{gtk-label-wrap-mode}
  @see-slot{gtk-label-xalign}
  @see-slot{gtk-label-yalign}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-label-angle --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "angle" 'gtk-label) 't)
 "The @code{angle} property of type @code{:double} (Read / Write) @br{}
  The angle that the baseline of the label makes with the horizontal, in
  degrees, measured counterclockwise. An angle of 90 degrees reads from bottom
  to top, an angle of 270 degrees, from top to bottom. Ignored if the label is
  selectable, wrapped, or ellipsized. @br{}
  Allowed values: [0.0, 360.0] @br{}
  Default value: 0.0 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-angle atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-angle 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-angle object) => angle}
  @syntax[]{(setf (gtk-label-angle object) angle)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[angle]{a double float with the angle that the baseline of the
    label makes with the horizontal, in degrees, measured counterclockwise}
  @begin{short}
    Accessor of the @slot[gtk-label]{angle} slot of the @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-angle} slot access function sets the angle of rotation for
  the label. The @sym{(setf gtk-label-angle)} slot access function sets the
  angle of rotation for the label.

  An angle of 90 degrees reads from from bottom to top, an angle of 270 degrees,
  from top to bottom. The angle setting for the label is ignored if the label is
  selectable, wrapped, or ellipsized.
  @see-class{gtk-label}")

;;; --- gtk-label-attributes ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attributes" 'gtk-label) 't)
 "The @code{attributes} property of type @class{pango-attr-list}
  (Read / Write) @br{}
  A list of style attributes to apply to the text of the label.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-attributes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-attributes 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-attributes object) => attrs}
  @syntax[]{(setf (gtk-label-attributes object) attrs)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[attrs]{a @class{pango-attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk-label]{attributes} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-attributes} slot access function gets the attribute list
  that was set on the label, if any. The @sym{(setf gtk-label-attributes)} slot
  access function sets a attribute list. The attributes in the list are applied
  to the label text.

  This function does not reflect attributes that come from the labels markup,
  see the @fun{gtk-label-set-markup} function. If you want to get the
  effective attributes for the label, use
  @begin{pre}
(pango-layout-attributes (gtk-label-layout label))
  @end{pre}
  @begin[Note]{dictionary}
    The attributes set with this function will be applied and merged with any
    other attributes previously effected by way of the
    @slot[gtk-label]{use-underline} or @slot[gtk-label]{use-markup} properties.
    While it is not recommended to mix markup strings with manually set
    attributes, if you must, know that the attributes will be applied to the
    label after the markup string is parsed.
  @end{dictionary}
  @see-class{gtk-label}
  @see-class{pango-attr-list}
  @see-function{gtk-label-set-markup}
  @see-function{gtk-label-layout}
  @see-function{gtk-label-use-markup}
  @see-function{gtk-label-use-underline}")

;;; --- gtk-label-cursor-position ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-position" 'gtk-label) 't)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-cursor-position 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-cursor-position object) => position}
  @syntax[]{(setf (gtk-label-cursor-position object) position)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[position]{an integer with the position of the insertion cursor}
  @begin{short}
    Accessor of the @slot[gtk-label]{cursor-position} slot of the
    @class{gtk-label} class.
  @end{short}

  The current position of the insertion cursor in chars.
  @see-class{gtk-label}")

;;; --- gtk-label-ellipsize ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize" 'gtk-label) 't)
 "The @code{ellipsize} property of type @symbol{pango-ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a value of the
  @symbol{pango-ellipsize-mode} enumeration. Note that setting this property to
  a value other than @code{:none} has the side-effect that the label requests
  only enough space to display the ellipsis \"...\". In particular, this means
  that ellipsizing labels do not work well in notebook tabs, unless the
  @code{tab-expand} child property of the tab is set to @em{true}. Other ways
  to set a width of the label are the @fun{gtk-widget-size-request} and
  @fun{gtk-label-width-chars} functions. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-ellipsize 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-ellipsize object) => mode}
  @syntax[]{(setf (gtk-label-ellipsize object) mode)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[mode]{a value of the @symbol{pango-ellipsize-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-label]{ellipsize} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-ellipsize} slot access function returns the ellipsizing
  position of type @symbol{pango-ellipsize-mode} of the label. The
  @sym{(setf gtk-label-ellipsize)} slot access function sets the mode used to
  ellipsize, add an ellipsis: \"...\", to the text if there is not enough space
  to render the entire string.
  @see-class{gtk-label}
  @see-symbol{pango-ellipsize-mode}")

;;; --- gtk-label-justify ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "justify" 'gtk-label) 't)
 "The @code{justify} property of type @symbol{gtk-justification} (Read / Write)
  @br{}
  The alignment of the lines in the text of the label relative to each other.
  This does not affect the alignment of the label within its allocation. See
  the @slot[gtk-label]{xalign} property for that. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-justify atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-justify 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-justify object) => justify}
  @syntax[]{(setf (gtk-label-justify object) justify)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[justify]{a value of the @symbol{gtk-justification} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-label]{justify} slot of the @class{gtk-label}
    class.
  @end{short}
  The @sym{gtk-label-justify} slot access function returns the justification of
  the label. The @sym{(setf gtk-label-justify)} slot access function sets the
  alignment of the lines in the text of the label relative to each other.

  The @code{:left} value is the default value when the widget is first created
  with the @fun{gtk-label-new} function. If you instead want to set the
  alignment of the label as a whole, use the @fun{gtk-widget-halign} function
  instead. The @sym{(setf gtk-label-justify)} function has no effect on labels
  containing only a single line.
  @see-class{gtk-label}
  @see-symbol{gtk-justification}
  @see-function{gtk-label-new}
  @see-function{gtk-widget-halign}")

;;; --- gtk-label-label --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-label) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text of the label. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-label 'function)
 "@version{*2021-10-31}
  @syntax[]{(gtk-label-label object) => text}
  @syntax[]{(setf (gtk-label-label object) text)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[text]{a string with the text for the label}
  @begin{short}
    Accessor of the @slot[gtk-label]{label} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-label} slot access function returns the text of the label
  widget including any embedded underlines indicating mnemonics and Pango
  markup. See the @fun{gtk-label-text} function.

  The @sym{(setf gtk-label-label)} slot access function sets the text of the
  label. The label is interpreted as including embedded underlines and/or Pango
  markup depending on the values of the @slot[gtk-label]{use-underline} and
  @slot[gtk-label]{use-markup} properties.
  @see-class{gtk-label}
  @see-function{gtk-label-text}
  @see-function{gtk-label-use-markup}
  @see-function{gtk-label-use-underline}")

;;; --- gtk-label-lines --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "lines" 'gtk-label) 't)
 "The @code{lines} property of type @code{:int} (Read / Write) @br{}
  The number of lines to which an ellipsized, wrapping label should be limited.
  This property has no effect if the label is not wrapping or ellipsized. Set
  this property to -1 if you do not want to limit the number of lines. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-lines 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-lines object) => lines}
  @syntax[]{(setf (gtk-label-lines object) lines)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[lines]{an integer with the desired number of lines, or -1}
  @begin{short}
    Accessor of the @slot[gtk-label]{lines} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-lines} slot access function gets the number of lines to
  which an ellipsized, wrapping label should be limited. The
  @sym{(setf gtk-label-lines)} slot access function sets the number of lines to
  which an ellipsized, wrapping label should be limited. This has no effect if
  the label is not wrapping or ellipsized. Set this to -1 if you do not want to
  limit the number of lines.
  @see-class{gtk-label}")

;;; --- gtk-label-max-width-chars ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-width-chars" 'gtk-label) 't)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the label, in characters. If this property is
  set to -1, the width will be calculated automatically. See the section on text
  layout for details of how the @slot[gtk-label]{width-chars} and
  @code{max-width-chars} properties determine the width of ellipsized and
  wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-max-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-max-width-chars 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-max-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-label-max-width-chars object) n-chars)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[n-chars]{an integer with the desired maximum width, in characters}
  @begin{short}
    Accessor of the @slot[gtk-label]{max-width-chars} slot of the
    @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-max-width-chars} slot access function returns the maximum
  width of the label in characters. The @sym{(setf gtk-label-max-width-chars)}
  slot access function sets the desired maximum width in characters of the label
  to @arg{n-chars}.
  @see-class{gtk-label}")

;;; --- gtk-label-mnemonic-keyval ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonic-keyval" 'gtk-label) 't)
 "The @code{mnemonic-keyval} property of @code{:uint} (Read) @br{}
  The mnemonic accelerator key for this label. @br{}
  Default value: @code{#xffffff}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-mnemonic-keyval atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-mnemonic-keyval 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-mnemonic-keyval object) => keyval}
  @argument[object]{a @class{gtk-label} widget}
  @argument[keyval]{an unsigned integer with the keyval}
  @begin{short}
    Accessor of the @slot[gtk-label]{mnemonic-keyval} slot of the
    @class{gtk-label} class.
  @end{short}

  If the label has been set so that it has a mnemonic key the
  @sym{gtk-label-mnemonic-keyval} slot access function returns the keyval used
  for the mnemonic accelerator. If there is no mnemonic set up it returns
  @code{#xffffff}.
  @begin[Example]{dictionary}
    @begin{pre}
(setq label (gtk-label-new-with-mnemonic \"_Print\"))
=> #<GTK-LABEL {1001A051E3@}>
(gtk-label-mnemonic-keyval label) => 112
    @end{pre}
  @end{dictionary}
  @see-class{gtk-label}")

;;; --- gtk-label-mnemonic-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mnemonic-widget" 'gtk-label) 't)
 "The @code{mnemonic-widget} property of type @class{gtk-widget}
  (Read / Write) @br{}
  The widget to be activated when the mnemonic key of the label is pressed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-mnemonic-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-mnemonic-widget 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-mnemonic-widget object) => widget}
  @syntax[]{(setf (gtk-label-mnemonic-widget object) widget)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[widget]{the @class{gtk-widget} target}
  @begin{short}
    Accessor of the @slot[gtk-label]{mnemonic-widget} slot of the
    @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-mnemonic-widget} slot access function returns the target
  of the mnemonic of the label, or @code{nil} if none has been set and the
  default algorithm will be used.

  If the label has been set so that it has an mnemonic key, using i.e.
  the @fun{gtk-label-set-markup-with-mnemonic},
  @fun{gtk-label-set-text-with-mnemonic}, @fun{gtk-label-new-with-mnemonic}
  functions or the @slot[gtk-label]{use-underline} property, the label can be
  associated with a widget that is the target of the mnemonic.

  When the label is inside a widget, like a @class{gtk-button} widget or a
  @class{gtk-notebook} tab, it is automatically associated with the correct
  widget, but sometimes, i.e. when the target is a @class{gtk-entry} widget next
  to the label, you need to set it explicitly using this function.

  The target widget will be accelerated by emitting the \"mnemonic-activate\"
  signal on it. The default handler for this signal will activate the widget if
  there are no mnemonic collisions and toggle focus between the colliding
  widgets otherwise.
  @see-class{gtk-label}
  @see-class{gtk-widget}
  @see-function{gtk-label-set-markup-with-mnemonic}
  @see-function{gtk-label-set-text-with-mnemonic}
  @see-function{gtk-label-new-with-mnemonic}
  @see-function{gtk-label-use-underline}")

;;; --- gtk-label-pattern ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pattern" 'gtk-label) 't)
 "The @code{pattern} property of type @code{:string} (Write) @br{}
  A string with \"_\" characters in positions correspond to characters in the
  text to underline. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-pattern atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-pattern 'function)
 "@version{2021-10-31}
  @syntax[]{(setf (gtk-label-pattern object) pattern)}
  @argument[object]{the @class{gtk-label} you want to set the pattern to}
  @argument[pattern]{a string with the pattern as described below}
  @begin{short}
    Accessor of the @slot[gtk-label]{pattern} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{(setf gtk-label-pattern)} slot access function sets the pattern of
  underlines you want under the existing text within the label. For example if
  the current text of the label says \"FooBarBaz\" passing a pattern of
  @code{\"___   ___\"} will underline \"Foo\" and \"Baz\" but not \"Bar\".
  @see-class{gtk-label}")

;;; --- gtk-label-selectable ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selectable" 'gtk-label) 't)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the label text can be selected with the mouse. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-selectable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-selectable 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-selectable object) => selectable}
  @syntax[]{(setf (gtk-label-selectable object) selectable)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[selectable]{@em{true} to allow selecting text in the label}
  @begin{short}
    Accessor of the @slot[gtk-label]{selectable} slot of the @class{gtk-label}
    class.
  @end{short}

  Selectable labels allow the user to select text from the label, for copy and
  paste.
  @see-class{gtk-label}")

;;; --- gtk-label-selection-bound ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-bound" 'gtk-label) 't)
 "The @code{selection-bound} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in chars.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-selection-bound atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-selection-bound 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-selection-bound object) => bound}
  @argument[object]{a @class{gtk-label} widget}
  @argument[bound]{an integer with a position}
  @begin{short}
    Accessor of the @slot[gtk-label]{selection-bound} slot of the
    @class{gtk-label} class.
  @end{short}

  The position of the opposite end of the selection from the cursor in chars.
  @see-class{gtk-label}")

;;; --- gtk-label-single-line-mode ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "single-line-mode"
                                               'gtk-label) 't)
 "The @code{single-line-mode} property of type  @code{:boolean} (Read / Write)
  @br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to the
  @code{(ascent + descent)} value of the font. This can be an advantage in
  situations where resizing the label because of text changes would be
  distracting, e.g. in a statusbar. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-single-line-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-single-line-mode 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-single-line-mode object) => mode}
  @syntax[]{(setf (gtk-label-single-line-mode object) mode)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[mode]{@em{true} if the label should be in single line mode}
  @begin{short}
    Accessor of the @slot[gtk-label]{single-line-mode} slot of the
    @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-single-line-mode} slot access function returns whether the
  label is in single line mode. The @sym{(setf gtk-label-single-line-mode)}
  slot access function sets whether the label is in single line mode.
  @see-class{gtk-label}")

;;; --- gtk-label-track-visited-links ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "track-visited-links"
                                               'gtk-label) 't)
 "The @code{track-visited-links} property of type @code{:boolean} (Read / Write)
  @br{}
  Set this property to @em{true} to make the label track which links have been
  clicked. It will then apply the color of the @code{visited-link-color} style
  property of a @class{gtk-widget} object, instead of the color of the
  @code{link-color} style property. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-track-visited-links atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-track-visited-links 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-track-visited-links object) => setting}
  @syntax[]{(setf (gtk-label-track-visited-links object) setting)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} to track visited links}
  @begin{short}
    Accessor of the @slot[gtk-label]{track-visited-links} slot of the
    @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-track-visited-links} slot access function returns whether
  the label is currently keeping track of clicked links. The
  @sym{(setf gtk-label-track-visited-links)} slot access function sets whether
  the label should keep track of clicked links and use a different color for
  them.
  @see-class{gtk-label}")

;;; --- gtk-label-use-markup ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-label) 't)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  The text of the label includes XML Pango markup. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-use-markup 'function)
 "@version{*2021-12-3}
  @syntax[]{(gtk-label-use-markup object) => setting}
  @syntax[]{(setf (gtk-label-use-markup object) setting)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} if the text of the label should be parsed for
    markup}
  @begin{short}
    Accessor of the @slot[gtk-label]{use-markup} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-use-markup} slot access function returns whether the
  text of the label is interpreted as marked up with the Pango text markup
  language. The @sym{(setf gtk-label-use-markup)} slot access function sets
  whether the text of the label contains markup. See the
  @fun{gtk-label-set-markup} function.
  @see-class{gtk-label}
  @see-function{gtk-label-set-markup}")

;;; --- gtk-label-use-underline ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline" 'gtk-label) 't)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-use-underline 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-use-underline object) => setting}
  @syntax[]{(setf (gtk-label-use-underline object) setting)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk-label]{use-underline} slot of the
    @class{gtk-label} class.
  @end{short}

  The @sym{gtk-label-use-underline} slot access function returns whether an
  embedded underline in the label indicates a mnemonic. If @em{true}, an
  underline in the text indicates the next character should be used for the
  mnemonic accelerator key.
  @see-class{gtk-label}")

;;; --- gtk-label-width-chars --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-label) 't)
 "The @code{width-chars} property of @code{:int} (Read / Write) @br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically. See the section on text
  layout for details of how the @code{width-chars} and
  @slot[gtk-label]{max-width-chars} properties determine the width of ellipsized
  and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-width-chars 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-label-width-chars object) n-chars)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[n-chars]{an integer with the new desired width, in characters}
  @begin{short}
    Accessor of the @slot[gtk-label]{width-chars} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-width-chars} slot access function retrieves the desired
  width of the label, in characters. The @sym{(setf gtk-label-width-chars)}
  slot access function sets the desired width in characters of the label to
  @arg{n-chars}.
  @see-class{gtk-label}")

;;; --- gtk-label-wrap ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap" 'gtk-label) 't)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  If set, wrap lines if the text becomes too wide. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-wrap 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-wrap object) => wrap}
  @syntax[]{(setf (gtk-label-wrap object) wrap)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[wrap]{a boolean whether lines are wrapped}
  @begin{short}
    Accessor of the @slot[gtk-label]{wrap} slot of the @class{gtk-label} class.
  @end{short}
  If set, wrap lines if the text becomes too wide.
  @see-class{gtk-label}")

;;; --- gtk-label-wrap-mode ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-label) 't)
 "The @code{wrap-mode} property of type @symbol{pango-wrap-mode}
  (Read / Write) @br{}
  If line wrapping is on, see the @slot[gtk-label]{wrap} property, this controls
  how the line wrapping is done. The default is @code{:word}, which means wrap
  on word boundaries. @br{}
  Default value: @code{:word}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-wrap-mode 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-wrap-mode object) => setting}
  @syntax[]{(setf (gtk-label-wrap-mode object) setting)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[setting]{a value of the @symbol{pango-wrap-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-label]{wrap-mode} slot of the @class{gtk-label}
    class.
  @end{short}

  If line wrapping is on, see the @slot[gtk-label]{wrap} property, this controls
  how the line wrapping is done. The default is @code{:word}, which means wrap
  on word boundaries.
  @see-class{gtk-label}
  @see-symbol{pango-wrap-mode}
  @see-function{gtk-label-wrap}")

;;; --- gtk-label-xalign -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-label) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Determines the horizontal aligment of the label text inside the labels size
  allocation. Compare this to the @slot[gtk-widget]{halign} property, which
  determines how the labels size allocation is positioned in the space available
  for the label. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-xalign 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-xalign object) => xalign}
  @syntax[]{(setf (gtk-label-xalign object) xalign)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[xalign]{a float with the xalign value, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk-label]{xalign} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-xalign} slot access function sets the @code{xalign}
  property for the label. The @sym{(setf gtk-label-xalign)} slot access function
  sets the @code{xalign} property.
  @see-class{gtk-label}
  @see-function{gtk-label-yalign}")

;;; --- gtk-label-yalign -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "yalign" 'gtk-label) 't)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Determines the vertical aligment of the label text inside the labels size
  allocation. Compare this to @slot[gtk-widget]{valign}, which determines how
  the labels size allocation is positioned in the space available for the label.
  @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-label-yalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-label-yalign 'function)
 "@version{2021-10-31}
  @syntax[]{(gtk-label-yalign object) => yalign}
  @syntax[]{(setf (gtk-label-yalign object) yalign)}
  @argument[object]{a @class{gtk-label} widget}
  @argument[yalign]{a float with the yalign value, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk-label]{yalign} slot of the @class{gtk-label}
    class.
  @end{short}

  The @sym{gtk-label-yalign} slot access function sets the @code{yalign}
  property for the label. The @sym{(setf gtk-label-yalign)} slot access function
  sets the @code{yalign} property for the label.
  @see-class{gtk-label}
  @see-function{gtk-label-yalign}")

;;; ----------------------------------------------------------------------------
;;; gtk_label_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-label-new (text)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[text]{a string with the text of the label}
  @return{The new @class{gtk-label} widget.}
  @begin{short}
    Creates a new label with the given text inside it.
  @end{short}
  You can pass @code{nil} to get an empty label widget.
  @see-class{gtk-label}"
  (let ((label (make-instance 'gtk-label)))
    (when text
      (setf (gtk-label-label label) text))
    label))

(export 'gtk-label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text ()
;;; gtk_label_set_text () -> gtk-label-text
;;; ----------------------------------------------------------------------------

(defun (setf gtk-label-text) (text label)
  (foreign-funcall "gtk_label_set_text"
                   (g-object gtk-label) label
                   :string text
                   :void)
  text)

(defcfun ("gtk_label_get_text" gtk-label-text) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-31}
  @syntax[]{(gtk-label-text label) => text}
  @syntax[]{(setf (gtk-label-text-label) text)}
  @argument[label]{a @class{gtk-label} widget}
  @argument[text]{a string with the text}
  @begin{short}
    Accessor of the text in the label.
  @end{short}

  The @sym{gtk-label-text} function fetches the text from a label, as displayed
  on the screen. The @sym{(setf gtk-label-text)} function sets the text.

  It overwrites any text that was there before. This will also clear any
  previously set mnemonic accelerators. This does not include any embedded
  underlines indicating mnemonics or Pango markup. See the @fun{gtk-label-label}
  slot access function.
  @see-class{gtk-label}
  @see-function{gtk-label-label}"
  (label (g-object gtk-label)))

(export 'gtk-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup" gtk-label-set-markup) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @argument[text]{a markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language,
    setting the text of the label and attribute list based on the parse results.
  @end{short}
  @see-class{gtk-label}
  @see-function{gtk-label-text}
  @see-function{gtk-label-set-markup-with-mnemonic}"
  (label (g-object gtk-label))
  (text :string))

(export 'gtk-label-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup_with_mnemonic"
           gtk-label-set-markup-with-mnemonic) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @argument[text]{a Pango markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language.
  @end{short}
  This sets the text and attribute list of the label based on the parse results.

  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.
  The mnemonic key can be used to activate another widget, chosen automatically,
  or explicitly using the @fun{gtk-label-mnemonic-widget} function.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (label (g-object gtk-label))
  (text :string))

(export 'gtk-label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap ()
;;; gtk_label_get_line_wrap () -> gtk-label-line-wrap
;;; ----------------------------------------------------------------------------

(defun (setf gtk-label-line-wrap) (wrap label)
  (foreign-funcall "gtk_label_set_line_wrap"
                   (g-object gtk-label) label
                   :boolean wrap
                   :void)
  wrap)

(defcfun ("gtk_label_get_line_wrap" gtk-label-line-wrap) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @syntax[]{(gtk-label-line-wrap label) => wrap}
  @syntax[]{(setf (gtk-label-line-wrap label) wrap)}
  @argument[label]{a @class{gtk-label} widget}
  @argument[wrap]{a boolean whether the lines of the label are automatically
    wrapped}
  @begin{short}
    Accessor of the line wrap setting of the label.
  @end{short}

  The @sym{gtk-label-line-wrap} function returns whether lines in the label are
  automatically wrapped. The @sym{(setf gtk-label-line-wrap)} function toggles
  line wrapping of the label.

  @em{True} makes it break lines if text exceeds the size of the widget.
  @em{False} lets the text get cut off by the edge of the widget if it exceeds
  the widget size.

  Note that setting line wrapping to @em{true} does not make the label wrap at
  the width of the parent container, because GTK widgets conceptually cannot
  make their requisition depend on the size of the parent container. For a label
  that wraps at a specific position, set the width of the label using the
  @fun{gtk-widget-size-request} function.
  @see-class{gtk-label}
  @see-function{gtk-widget-size-request}"
  (label (g-object gtk-label)))

(export 'gtk-label-line-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap_mode ()
;;; gtk_label_get_line_wrap_mode () -> gtk-label-line-wrap-mode
;;; ----------------------------------------------------------------------------

(defun (setf gtk-label-line-wrap-mode) (wrap-mode label)
  (foreign-funcall "gtk_label_set_line_wrap_mode"
                   (g-object gtk-label) label
                   pango-wrap-mode wrap-mode
                   :void)
  wrap-mode)

(defcfun ("gtk_label_get_line_wrap_mode" gtk-label-line-wrap-mode)
    pango-wrap-mode
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @syntax[]{(gtk-label-line-wrap-mode label) => mode}
  @syntax[]{(setf (gtk-label-line-wrap-mode label) mode)}
  @argument[label]{a @class{gtk-label} widget}
  @argument[mode]{the line wrapping mode of type @symbol{pango-wrap-mode}}
  @begin{short}
    Accessor of the line wrap mode of the label.
  @end{short}

  The @sym{gtk-label-line-wrap-mode} function returns the line wrap mode used by
  the label. The @sym{(setf gtk-label-line-wrap-mode)} function sets the line
  wrap mode of the label.

  If line wrapping is on this controls how the line wrapping is done. The
  default is @code{:word} which means wrap on word boundaries.
  @see-class{gtk-label}
  @see-symbol{pango-wrap-mode}
  @see-function{gtk-label-line-wrap}"
  (label (g-object gtk-label)))

(export 'gtk-label-line-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets () -> gtk-label-layout-offsets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout_offsets" %gtk-label-get-layout-offsets) :void
  (label (g-object gtk-label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-label-layout-offsets (label)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    @code{x} -- an integer with the x offset @br{}
    @code{y} -- an integer with the y offset
  @end{return}
  @begin{short}
    Obtains the coordinates where the label will draw the @class{pango-layout}
    object representing the text in the label.
  @end{short}
  This is useful to convert mouse events into coordinates inside the
  @class{pango-layout} object, e.g. to take some action if some part of the
  label is clicked.

  Of course you will need to create a @class{gtk-event-box} widget to receive
  the events, and pack the label inside it, since labels are windowless. Labels
  return @em{false} from the @fun{gtk-widget-has-window} function. Remember
  when using the @class{pango-layout} functions you need to convert to and from
  pixels using the @fun{pango-pixels} function or the @var{+pango-scale+}
  constant.
  @begin[Example]{dictionary}
    @begin{pre}
(gtk-label-layout-offsets (make-instance 'gtk-label))
=> 0
=> -9
(gtk-label-layout-offsets (make-instance 'gtk-label :label \"text\"))
=> -14
=> -9
    @end{pre}
  @end{dictionary}
  @see-class{gtk-label}
  @see-class{gtk-event-box}
  @see-class{pango-layout}
  @see-variable{+pango-scale+}
  @see-function{pango-pixels}
  @see-function{gtk-widget-has-window}"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-label-get-layout-offsets label x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-label-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

;; TODO: Check implementation with make-instance

(defcfun ("gtk_label_new_with_mnemonic" gtk-label-new-with-mnemonic)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[text]{a string with the text of the label, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk-label} widget.}
  @begin{short}
    Creates a new @class{gtk-label} widget, containing the given.
  @end{short}

  If characters in @arg{text} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. The mnemonic key can be used to activate
  another widget, chosen automatically, or explicitly using the
  @fun{gtk-label-mnemonic-widget} function.

  If the @fun{gtk-label-mnemonic-widget} function is not called, then the first
  activatable ancestor of the @class{gtk-label} widget will be chosen as the
  mnemonic widget. For instance, if the label is inside a button or menu item,
  the button or menu item will automatically become the mnemonic widget and be
  activated by the mnemonic.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (text :string))

(export 'gtk-label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_select_region" gtk-label-select-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @argument[start]{an integer with the start offset, in characters not bytes}
  @argument[end]{an integer with the end offset, in characters not bytes}
  @begin{short}
    Selects a range of characters in the label, if the label is selectable.
  @end{short}
  See the @fun{gtk-label-selectable} function. If the label is not
  selectable, this function has no effect. If @arg{start} or @arg{end} are -1,
  then the end of the label will be substituted.
  @see-class{gtk-label}
  @see-function{gtk-label-selectable}"
  (label (g-object gtk-label))
  (start :int)
  (end :int))

(export 'gtk-label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_text_with_mnemonic" gtk-label-set-text-with-mnemonic)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @argument[text]{a string for the label}
  @begin{short}
    Sets the text of the label from the string @arg{text}.
  @end{short}
  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic. The
  mnemonic key can be used to activate another widget, chosen automatically, or
  explicitly using the @fun{gtk-label-mnemonic-widget} function.
  @see-class{gtk-label}
  @see-function{gtk-label-mnemonic-widget}"
  (label (g-object gtk-label))
  (text :string))

(export 'gtk-label-set-text-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout () -> gtk-label-layout
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout" gtk-label-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @return{The @class{pango-layout} object for this label.}
  @begin{short}
    Gets the Pango layout used to display the label.
  @end{short}
  The layout is useful to e.g. convert text positions to pixel positions,
  in combination with the @fun{gtk-label-layout-offsets} function. The label
  is free to recreate its layout at any time, so it should be considered
  read-only.
  @see-class{gtk-label}
  @see-class{pango-layout}
  @see-function{gtk-label-layout-offsets}"
  (label (g-object gtk-label)))

(export 'gtk-label-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds () -> gtk-label-selection-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_selection_bounds" %gtk-label-get-selection-bounds)
    :boolean
  (label (g-object gtk-label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun gtk-label-selection-bounds (label)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    @code{start} -- an integer with the start of selection, as a character
      offset @br{}
    @code{end} -- an integer with the end of selection, as a character offset
  @end{return}
  @begin{short}
    Gets the selected range of characters in the label.
  @end{short}
  @see-class{gtk-label}"
  (with-foreign-objects ((start :int) (end :int))
    (when (%gtk-label-get-selection-bounds label start end)
      (values (mem-ref start :int)
              (mem-ref end :int)))))

(export 'gtk-label-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri () -> gtk-label-current-uri
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_current_uri" gtk-label-current-uri) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-10-31}
  @argument[label]{a @class{gtk-label} widget}
  @begin{return}
    A string with the currently active URI.
  @end{return}
  @begin{short}
    Returns the URI for the currently active link in the label.
  @end{short}
  The active link is the one under the mouse pointer or, in a selectable label,
  the link in which the text cursor is currently positioned.

  This function is intended for use in a \"activate-link\" signal handler or
  for use in a \"query-tooltip\" signal handler.
  @see-class{gtk-label}"
  (label (g-object gtk-label)))

(export 'gtk-label-current-uri)

;;; --- End of file gtk.label.lisp ---------------------------------------------
