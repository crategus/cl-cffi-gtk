;;; ----------------------------------------------------------------------------
;;; gtk.entry.lisp
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
;;; GtkEntry
;;;
;;;     A single line text entry field
;;;
;;; Types and Values
;;;
;;;     GtkEntry
;;;     GtkEntryIconPosition
;;;     GtkInputPurpose
;;;     GtkInputHints
;;;
;;; Functions
;;;
;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_get_buffer                               Accessor
;;;     gtk_entry_set_buffer                               Accessor
;;;     gtk_entry_set_text                                 Accessor
;;;     gtk_entry_get_text                                 Accessor
;;;     gtk_entry_get_text_length                          Accessor
;;;     gtk_entry_get_text_area
;;;     gtk_entry_set_visibility                           Accessor
;;;     gtk_entry_set_invisible_char                       Accessor
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_max_length                           Accessor
;;;     gtk_entry_get_activates_default                    Accessor
;;;     gtk_entry_get_has_frame                            Accessor
;;;     gtk_entry_get_inner_border                         Accessor
;;;     gtk_entry_get_width_chars                          Accessor
;;;     gtk_entry_get_max_width_chars                      Accessor
;;;     gtk_entry_set_activates_default                    Accessor
;;;     gtk_entry_set_has_frame                            Accessor
;;;     gtk_entry_set_inner_border                         Accessor
;;;     gtk_entry_set_width_chars                          Accessor
;;;     gtk_entry_set_max_width_chars                      Accessor
;;;     gtk_entry_get_invisible_char                       Accessor
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_set_placeholder_text                     Accessor
;;;     gtk_entry_get_placeholder_text                     Accessor
;;;     gtk_entry_set_overwrite_mode                       Accessor
;;;     gtk_entry_get_overwrite_mode                       Accessor
;;;     gtk_entry_get_layout
;;;     gtk_entry_get_layout_offsets
;;;     gtk_entry_layout_index_to_text_index
;;;     gtk_entry_text_index_to_layout_index
;;;     gtk_entry_set_attributes                           Accessor
;;;     gtk_entry_get_attributes                           Accessor
;;;     gtk_entry_get_max_length                           Accessor
;;;     gtk_entry_get_visibility                           Accessor
;;;     gtk_entry_set_completion                           Accessor
;;;     gtk_entry_get_completion                           Accessor
;;;     gtk_entry_set_cursor_hadjustment
;;;     gtk_entry_get_cursor_hadjustment
;;;     gtk_entry_set_progress_fraction                    Accessor
;;;     gtk_entry_get_progress_fraction                    Accessor
;;;     gtk_entry_set_progress_pulse_step                  Accessor
;;;     gtk_entry_get_progress_pulse_step                  Accessor
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_im_context_filter_keypress
;;;     gtk_entry_reset_im_context
;;;     gtk_entry_get_tabs                                 Accessor
;;;     gtk_entry_set_tabs                                 Accessor
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
;;;     gtk_entry_set_input_purpose                        Accessor
;;;     gtk_entry_get_input_purpose                        Accessor
;;;     gtk_entry_set_input_hints                          Accessor
;;;     gtk_entry_get_input_hints                          Accessor
;;;     gtk_entry_grab_focus_without_selecting
;;;
;;; Properties
;;;
;;;           gboolean   activates-default              Read / Write
;;;      PangoAttrList*  attributes                     Read / Write
;;;     GtkEntryBuffer*  buffer                         Read / Write / Construct
;;;           gboolean   caps-lock-warning              Read / Write
;;; GtkEntryCompletion*  completion                     Read / Write
;;;               gint   cursor-position                Read
;;;           gboolean   editable                       Read / Write
;;;           gboolean   enable-emoji-completion        Read / Write
;;;           gboolean   has-frame                      Read / Write
;;;              gchar*  im-module                      Read / Write
;;;          GtkBorder*  inner-border                   Read / Write
;;;      GtkInputHints   input-hints                    Read / Write
;;;    GtkInputPurpose   input-purpose                  Read / Write
;;;              guint   invisible-char                 Read / Write
;;;           gboolean   invisible-char-set             Read / Write
;;;               gint   max-length                     Read / Write
;;;               gint   max-width-chars                Read / Write
;;;           gboolean   overwrite-mode                 Read / Write
;;;              gchar*  placeholder-text               Read / Write
;;;           gboolean   populate-all                   Read / Write
;;;           gboolean   primary-icon-activatable       Read / Write
;;;              GIcon*  primary-icon-gicon             Read / Write
;;;              gchar*  primary-icon-name              Read / Write
;;;          GdkPixbuf*  primary-icon-pixbuf            Read / Write
;;;           gboolean   primary-icon-sensitive         Read / Write
;;;              gchar*  primary-icon-stock             Read / Write
;;;       GtkImageType   primary-icon-storage-type      Read
;;;              gchar*  primary-icon-tooltip-markup    Read / Write
;;;              gchar*  primary-icon-tooltip-text      Read / Write
;;;            gdouble   progress-fraction              Read / Write
;;;            gdouble   progress-pulse-step            Read / Write
;;;               gint   scroll-offset                  Read
;;;           gboolean   secondary-icon-activatable     Read / Write
;;;              GIcon*  secondary-icon-gicon           Read / Write
;;;              gchar*  secondary-icon-name            Read / Write
;;;          GdkPixbuf*  secondary-icon-pixbuf          Read / Write
;;;           gboolean   secondary-icon-sensitive       Read / Write
;;;              gchar*  secondary-icon-stock           Read / Write
;;;       GtkImageType   secondary-icon-storage-type    Read
;;;              gchar*  secondary-icon-tooltip-markup  Read / Write
;;;              gchar*  secondary-icon-tooltip-text    Read / Write
;;;               gint   selection-bound                Read
;;;      GtkShadowType   shadow-type                    Read / Write
;;;           gboolean   show-emoji-icon                Read / Write
;;;      PangoTabArray*  tabs                           Read / Write
;;;              gchar*  text                           Read / Write
;;;              guint   text-length                    Read
;;;           gboolean   truncate-multiline             Read / Write
;;;           gboolean   visibility                     Read / Write
;;;               gint   width-chars                    Read / Write
;;;             gfloat   xalign                         Read / Write
;;;
;;; Style Properties
;;;
;;;           gboolean   icon-prelight                  Read
;;;          GtkBorder*  inner-border                   Read
;;;              guint   invisible-char                 Read
;;;          GtkBorder*  progress-border                Read
;;;
;;; Signals
;;;
;;;               void   activate                       Action
;;;               void   backspace                      Action
;;;               void   copy-clipboard                 Action
;;;               void   cut-clipboard                  Action
;;;               void   delete-from-cursor             Action
;;;               void   icon-press                     Run Last
;;;               void   icon-release                   Run Last
;;;               void   insert-at-cursor               Action
;;;               void   insert-emoji                   Action
;;;               void   move-cursor                    Action
;;;               void   paste-clipboard                Action
;;;               void   populate-popup                 Run Last
;;;               void   preedit-changed                Action
;;;               void   toggle-overwrite               Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEntry
;;;                 ├── GtkSearchEntry
;;;                 ╰── GtkSpinButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkEntry implements AtkImplementorIface, GtkBuildable, GtkEditable and
;;;     GtkCellEditable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkInputPurpose
;;; ----------------------------------------------------------------------------

#+gtk-3-6
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

#+(and gtk-3-6 cl-cffi-gtk-documentation)
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
;;; enum GtkInputHints
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(define-g-flags "GtkInputHints" gtk-input-hints
  (:export t
   :type-initializer "gtk_input_hints_get_type")
  (:none 0)
  (:spellcheck          #.(ash 1 0))
  (:no-spellcheck       #.(ash 1 1))
  (:word-completion     #.(ash 1 2))
  (:lowercase           #.(ash 1 3))
  (:uppercase-chars     #.(ash 1 4))
  (:uppercase-words     #.(ash 1 5))
  (:uppercase-sentences #.(ash 1 6))
  (:inhibit-osk         #.(ash 1 7))
  #+gtk-3-18
  (:vertical-writing    #.(ash 1 8))
  #+gtk-3-22
  (:emoji               #.(ash 1 9))
  #+gtk-3-22
  (:no-emoji            #.(ash 1 10))
)

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-input-hints atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-input-hints atdoc:*external-symbols*)
 "@version{2014-11-9}
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
  (:spellcheck          #.(ash 1 0))
  (:no-spellcheck       #.(ash 1 1))
  (:word-completion     #.(ash 1 2))
  (:lowercase           #.(ash 1 3))
  (:uppercase-chars     #.(ash 1 4))
  (:uppercase-words     #.(ash 1 5))
  (:uppercase-sentences #.(ash 1 6))
  (:inhibit-osk         #.(ash 1 7))
  (:vertical-writing    #.(ash 1 8))
  (:emoji               #.(ash 1 9))
  (:no-emoji            #.(ash 1 10)))
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
    @entry[:vertical-writing]{The text is vertical. Since 3.18}
    @entry[:emoji]{Suggest offering Emoji support. Since 3.22}
    @entry[:no-emoji]{Suggest not offering Emoji support. Since 3.22}
  @end{table}
  Since 3.6
  @see-class{gtk-entry}
  @see-symbol{gtk-input-purpose}")

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
   #+gtk-3-6
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
   #+gtk-3-22
   (enable-emoji-completion
    gtk-entry-enable-emoji-completion
    "enable-emoji-completion" "gboolean" t t)
   (has-frame
    gtk-entry-has-frame
    "has-frame" "gboolean" t t)
   (im-module
    gtk-entry-im-module
    "im-module" "gchararray" t t)
   (inner-border
    gtk-entry-inner-border
    "inner-border" "GtkBorder" t t)
   #+gtk-3-6
   (input-hints
    gtk-entry-input-hints
    "input-hints" "GtkInputHints" t t)
   #+gtk-3-6
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
   #+gtk-3-12
   (max-width-chars
    gtk-entry-max-width-chars
    "max-width-chars" "gint" t t)
   (overwrite-mode
    gtk-entry-overwrite-mode
    "overwrite-mode" "gboolean" t t)
   (placeholder-text
    gtk-entry-placeholder-text
    "placeholder-text" "gchararray" t t)
   #+gtk-3-8
   (populate-all
    gtk-entry-populate-all
    "populate-all" "gboolean" t t)
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
   #+gtk-3-22
   (show-emoji-icon
    gtk-entry-show-emoji-icon
    "show-emoji-icon" "gboolean" t t)
   #+gtk-3-10
   (tabs
    gtk-entry-tabs
    "tabs" "PangoTabArray" t t)
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
 "@version{2014-6-8}
  @begin{short}
    The @sym{gtk-entry} widget is a single line text entry widget. A fairly
    large set of key bindings are supported by default. If the entered text is
    longer than the allocation of the widget, the widget will scroll so that
    the cursor position is visible.
  @end{short}

  @image[entry]{}

  When using an entry for passwords and other sensitive information, it can be
  put into \"password mode\" using the generic function
  @fun{gtk-entry-visibility}. In this mode, entered text is displayed using a
  'invisible' character. By default, GTK+ picks the best invisible character
  that is available in the current font, but it can be changed with the generic
  function @fun{gtk-entry-invisible-char}. GTK+ displays a warning when Caps
  Lock or input methods might interfere with entering text in a password entry.
  The warning can be turned off with the @slot[gtk-entry]{caps-lock-warning}
  property.

  @sym{gtk-entry} has the ability to display progress or activity information
  behind the text. To make an entry display such information, use the generic
  functions @fun{gtk-entry-progress-fraction} or
  @fun{gtk-entry-progress-pulse-step}.

  Additionally, @sym{gtk-entry} can show icons at either side of the entry.
  These icons can be activatable by clicking, can be set up as drag source and
  can have tooltips. To add an icon, use the function
  @fun{gtk-entry-set-icon-from-gicon} or one of the various other functions that
  set an icon from a stock ID, an icon name or a pixbuf. To trigger an action
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
    @begin[code]{table}
      @begin[icon-prelight]{entry}
        The @code{icon-prelight} style property of type @code{:boolean}
        (Read) @br{}
        The prelight style property determines whether activatable icons prelight
        on mouseover. @br{}
        @em{Warning:} The @code{icon-prelight} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use CSS to control the appearance of prelighted icons; the value
        of this style property is ignored. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[inner-border]{entry}
        The @code{inner-border} style property of type @class{gtk-border}
        (Read) @br{}
        Sets the text area's border between the text and the frame. @br{}
        @em{Warning:}
        The @code{inner-border} style property has been deprecated since version
        3.4 and should not be used in newly written code. Use the standard
        border and padding CSS properties through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}; the value of
        this style property is ignored.
      @end{entry}
      @begin[invisible-char]{entry}
        The @code{invisible-char} style property of type @code{:uint}
        (Read) @br{}
        The invisible character is used when masking entry contents in
        \"password mode\". When it is not explicitly set with the
        @code{invisible-char} property, GTK+ determines the character to use
        from a list of possible candidates, depending on availability in the
        current font. This style property allows the theme to prepend a
        character to the list of candidates. @br{}
        Default value: 0
      @end{entry}
      @begin[progress-border]{entry}
        The @code{progress-border} style property of type @class{gtk-border}
        (Read) @br{}
        The border around the progress bar in the entry. @br{}
        @em{Warning:}
        The @code{progress-border} style property has been deprecated since
        version 3.4 and should not be used in newly written code. Use the
        standard margin CSS property through objects like
        @class{gtk-style-context} and @class{gtk-css-provider}; the value of
        this style property is ignored.
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      A keybinding signal which gets emitted when the user activates the entry.
      Applications should not connect to it, but may emit it with the
      @fun{g-signal-emit-by-name} function if they need to control activation
      programmatically.
      The default bindings for this signal are all forms of the Enter key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"backspace\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"backspace\" signal is a keybinding signal which gets emitted when
      the user asks for it.
      The default bindings for this signal are Backspace and Shift-Backspace.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"copy-clipboard\" signal is a keybinding signal which gets emitted
      to copy the selection to the clipboard.
      The default bindings for this signal are Ctrl-c and Ctrl-Insert.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"cut-clipboard\" signal is a keybinding signal which gets emitted to
      cut the selection to the clipboard.
      The default bindings for this signal are Ctrl-x and Shift-Delete.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
 lambda (entry type count)    : Action
      @end{pre}
      The \"delete-from-cursor\" signal is a keybinding signal which gets
      emitted when the user initiates a text deletion.
      If the type is @code{:chars} of the @symbol{gtk-delete-type} enumeration,
      GTK+ deletes the selection if there is one, otherwise it deletes the
      requested number of characters.
      The default bindings for this signal are Delete for deleting a character
      and Ctrl-Delete for deleting a word.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
        @entry[type]{The granularity of the deletion, as a
          @symbol{gtk-delete-type} enumeration.}
        @entry[count]{The number of type units to delete.}
      @end{table}
    @subheading{The \"icon-press\" signal}
      @begin{pre}
 lambda (entry icon-pos event)    : Run Last
      @end{pre}
      The \"icon-press\" signal is emitted when an activatable icon is clicked.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object on which the signal is
          emitted.}
        @entry[icon-pos]{The position of type @symbol{gtk-entry-icon-position}
          of the clicked icon.}
        @entry[event]{The button press event.}
      @end{table}
    @subheading{The \"icon-release\" signal}
      @begin{pre}
 lambda (entry icon-pos event)    : Run Last
      @end{pre}
      The \"icon-release\" signal is emitted on the button release from a mouse
      click over an activatable icon.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object on which the signal is
          emitted.}
        @entry[icon-pos]{The position of the clicked icon.}
        @entry[event]{The button release event.}
      @end{table}
    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
 lambda (entry string)    : Action
      @end{pre}
      The \"insert-at-cursor\" signal is a keybinding signal which gets emitted
      when the user initiates the insertion of a fixed string at the cursor.
      This signal has no default bindings.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"insert-emoji\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"insert-emoji\" signal is a keybinding signal which gets emitted to
      present the Emoji chooser for the entry. The default bindings for this
      signal are Ctrl-. and Ctrl-;
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
      @end{table}
      Since 3.22

    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (entry step count extend-selection)    : Action
      @end{pre}
      The \"move-cursor\" signal is a keybinding signal which gets emitted when
      the user initiates a cursor movement. If the cursor is not visible in
      entry, this signal causes the viewport to be moved instead.
      Applications should not connect to it, but may emit it with the
      @fun{g-signal-emit-by-name} function if they need to control the cursor
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
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
        @entry[step]{The granularity of the move, as a
          @symbol{gtk-movement-step}.}
        @entry[count]{The number of step units to move.}
        @entry[extend-selection]{@em{True} if the move should extend the
          selection.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"paste-clipboard\" signal is a keybinding signal which gets emitted
      to paste the contents of the clipboard into the text view.
      The default bindings for this signal are Ctrl-v and Shift-Insert.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
 lambda (entry menu)    : Run Last
      @end{pre}
      The \"populate-popup\" signal gets emitted before showing the context menu
      of the entry.
      If you need to add items to the context menu, connect to this signal and
      append your menuitems to the menu.
      If the @code{populate-all} property is @em{true}, this signal will
      also be emitted to populate touch popups. In this case, widget will be a
      different container, e. g. a @class{gtk-toolbar}. The signal handler
      should not make assumptions about the type of widget.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object on which the signal is
          emitted.}
        @entry[menu]{The menu that is being populated.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
 lambda (entry preedit)    : Action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
        @entry[preedit]{The current preedit string.}
      @end{table}
    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
 lambda (entry)    : Action
      @end{pre}
      The \"toggle-overwrite\" signal is a keybinding signal which gets emitted
      to toggle the overwrite mode of the entry.
      The default bindings for this signal is Insert.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-entry} object which received the signal.}
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
  @see-slot{gtk-entry-populate-all}
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
  @see-slot{gtk-entry-tabs}
  @see-slot{gtk-entry-text}
  @see-slot{gtk-entry-text-length}
  @see-slot{gtk-entry-truncate-multiline}
  @see-slot{gtk-entry-visibility}
  @see-slot{gtk-entry-width-chars}
  @see-slot{gtk-entry-xalign}
  @see-class{gtk-text-view}
  @see-class{gtk-entry-completion}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-entry-activates-default --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activates-default"
                                               'gtk-entry) 't)
 "The @code{activates-default} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to activate the default widget (such as the default button in a
  dialog) when Enter is pressed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-activates-default atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-activates-default 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[setting]{@em{true} to activate window's default widget on Enter
    keypress}
  @syntax[]{(gtk-entry-activates-default object) => setting}
  @syntax[]{(setf (gtk-entry-activates-default object) setting)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{activates-default} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-activates-default} retrieves the value
  set by the generic function @sym{(setf gtk-entry-activates-default)}.

  If @arg{setting} is @em{true}, pressing Enter in the entry will activate the
  default widget for the window containing the entry.

  This usually means that the dialog box containing the entry will be closed,
  since the default widget is usually one of the dialog buttons.

  For experts: if @arg{setting} is @em{true}, the entry calls the function
  @fun{gtk-window-activate-default} on the window containing the entry,
  in the default handler for the \"activate\" signal.
  @see-class{gtk-entry}
  @see-function{gtk-window-activate-default}")

;;; --- gtk-entry-attributes ---------------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "attributes" 'gtk-entry) t)
 "The @code{attributes} property of type @class{pango-attr-list}
  (Read / Write) @br{}
  A list of Pango attributes to apply to the text of the entry.
  This is mainly useful to change the size or weight of the text. @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-attributes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-attributes 'function)
 "@version{2014-4-19}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[attrs]{a @class{pango-attr-list}}
  @syntax[]{(gtk-entry-attributes object) => attrs}
  @syntax[]{(setf (gtk-entry-attribures object) attrs)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{attributes} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-attributes} gets the attribute list that
  was set on the entry using the generic function
  @sym{(setf gtk-entry-attributes)}, if any.

  The generic function @sym{(setf gtk-entry-attribues)} sets a
  @class{pango-attr-list}; the attributes in the list are applied to the entry
  text.

  Since 3.6
  @see-class{gtk-entry}")

;;; --- gtk-entry-buffer -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buffer" 'gtk-entry) 't)
 "The @code{buffer} property of type @class{gtk-entry-buffer}
  (Read / Write / Construct) @br{}
  Text buffer object which actually stores entry text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-buffer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-buffer 'function)
 "@version{2014-4-20}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[buffer]{a @class{gtk-entry-buffer} object}
  @syntax[]{(gtk-entry-buffer object) => buffer}
  @syntax[]{(setf (gtk-entry-buffer object) buffer)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{buffer} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-buffer} gets the @class{gtk-entry-buffer}
  object which holds the text for this widget.

  The generic function @sym{(setf gtk-entry-buffer)} sets the
  @class{gtk-entry-buffer} object which holds the text for this widget.
  @see-class{gtk-entry}")

;;; --- gtk-entry-caps-lock-warning --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "caps-lock-warning"
                                               'gtk-entry) 't)
 "The @code{caps-lock-warning} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether password entries will show a warning when Caps Lock is on. Note that
  the warning is shown using a secondary icon, and thus does not work if you
  are using the secondary icon position for some other purpose. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-caps-lock-warning atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-caps-lock-warning 'function)
 "@version{2014-4-20}
  Accessor of the slot @slot[gtk-entry]{caps-lock-warning} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-completion ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "completion" 'gtk-entry) 't)
 "The @code{completion} property of type @class{gtk-entry-completion}
  (Read / Write) @br{}
  The auxiliary completion object to use with the entry.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-completion atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-completion 'function)
 "@version{2014-4-20}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[completion]{the @class{gtk-entry-completion} or @code{nil}}
  @syntax[]{(gtk-entry-completion object) => completion}
  @syntax[]{(setf (gtk-entry-completion object) completion)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{completion} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-completion} returns the auxiliary
  completion object currently in use by the entry.

  The generic function @sym{(setf gtk-entry-completion} sets @arg{completion}
  to be the auxiliary completion object to use with the entry.

  All further configuration of the completion mechanism is done on
  @arg{completion} using the @class{gtk-entry-completion} API. Completion is
  disabled if @arg{completion} is set to @code{nil}.
  @see-class{gtk-entry}
  @see-class{gtk-entry-completion}")

;;; --- gtk-entry-cursor-position ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor-position" 'gtk-entry) 't)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-cursor-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-cursor-position 'function)
 "@version{2014-4-20}
  Accessor of the slot @slot[gtk-entry]{cursor-position} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-editable -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable" 'gtk-entry) 't)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the entry contents can be edited. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-editable 'function)
 "@version{2014-4-20}
  Accessor of the slot @slot[gtk-entry]{editable} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-enable-emoji-completion --------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "enable-emoji-completion"
                                               'gtk-entry) 't)
 "The @code{enable-emoji-completion} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to suggest Emoji replacements. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-enable-emoji-completion atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-enable-emoji-completion 'function)
 "@version{2019-4-18}
  Accessor of the @slot[gtk-entry]{enable-emoji-completion} slot of the
  @class{gtk-entry} class.

  Since 3.22
  @see-class{gtk-entry}")

;;; --- gtk-entry-has-frame ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-frame" 'gtk-entry) 't)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  @code{Nil} removes outside bevel from entry. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-has-frame atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-has-frame 'function)
 "@version{2014-5-18}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[setting]{new value}
  @syntax[]{(gtk-entry-has-frame object) => setting}
  @syntax[]{(setf (gtk-entry-has-frame object) setting)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{has-frame} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-has-frame} returns whether the @arg{entry}
  has a beveled frame.

  The generic function @sym{(setf gtk-entry-has-frame} sets whether the
  @arg{entry} has a beveled frame around it.
  @see-class{gtk-entry}")

;;; --- gtk-entry-im-module ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "im-module" 'gtk-entry) 't)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used for this entry. See
  @class{gtk-im-context}.
  Setting this to a non-@code{nil} value overrides the system-wide IM module
  setting. See the @class{gtk-settings} @slot[gtk-settings]{gtk-im-module}
  property. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-im-module atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-im-module 'function)
 "@version{2014-5-18}
  Accessor of the slot @slot[gtk-entry]{im-module} of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-inner-border -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inner-border" 'gtk-entry) 't)
 "The @code{inner-border} property of type @class{gtk-border}
  (Read / Write) @br{}
  Sets the text area's border between the text and the frame. @br{}
  @em{Warning:}
  The @code{inner-border} property has been deprecated since version 3.4 and
  should not be used in newly written code. Use the standard border and padding
  CSS properties through objects like @class{gtk-style-context} and
  @class{gtk-css-provider}; the value of this style property is ignored.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-inner-border atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-inner-border 'function)
 "@version{2014-5-18}
  @syntax[]{(gtk-entry-inner-border object) => border}
  @syntax[]{(setf (gtk-entry-inner-border object) border)}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[border]{a @class{gtk-border} structure, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-entry]{inner-border} slot of the @class{gtk-entry}
    class.
  @end{short}

  The @sym{gtk-entry-inner-border} slot access function returns the entry's
  @slot[gtk-entry]{inner-border} property.

  The @sym{(setf gtk-entry-inner-border)} slot access function sets entry's
  @slot[gtk-entry]{inner-border} property to @arg{border}, or clears it if
  @code{nil} is passed. The inner border is the area around the entry's
  text, but inside its frame.

  If set, this property overrides the @code{inner-border} style property.
  Overriding the style-provided border is useful when you want to do in-place
  editing of some text in a canvas or list widget, where pixel-exact positioning
  of the entry is important.
  @begin[Warning]{dictionary}
    The @sym{gtk-entry-inner-border} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the standard
    border and padding CSS properties through objects like
    @class{gtk-style-context} and @class{gtk-css-provider}; the value returned
    by this function is ignored by @class{gtk-entry}.
  @end{dictionary}
  @see-class{gtk-entry}
  @see-class{gtk-border}
  @see-class{gtk-style-context}
  @see-class{gtk-css-provider}")

;;; --- gtk-entry-input-hints --------------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "input-hints" 'gtk-entry) t)
 "The @code{input-hints} property of type @symbol{gtk-input-hints}
  (Read / Write) @br{}
  Additional hints (beyond the @code{input-purpose} property) that allow
  input methods to fine-tune their behaviour. @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-input-hints atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-input-hints 'function)
 "@version{2014-5-18}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[hints]{the hints}
  @syntax[]{(gtk-entry-input-hints object) => hints}
  @syntax[]{(setf (gtk-entry-input-hints object) hints)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{input-hints} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-input-hints} gets the value of the
  @slot[gtk-entry]{input-hints} property.

  The generic function @sym{(setf gtk-entry-input-hints)} sets the
  @slot[gtk-entry]{input-hints} property, which allows input methods to
  fine-tune their behaviour.

  Since 3.6
  @see-class{gtk-entry}
  @see-symbol{gtk-input-hints}")

;;; --- gtk-entry-input-purpose ------------------------------------------------

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "input-purpose" 'gtk-entry) t)
 "The @code{input-purpose} property of type @symbol{gtk-input-purpose}
  (Read / Write) @br{}
  The purpose of this text field.
  This property can be used by on-screen keyboards and other input methods to
  adjust their behaviour.
  Note that setting the purpose to @code{:password} or @code{:pin} is
  independent from setting the @code{visibility} property. @br{}
  Default value: @code{:free-form} @br{}
  Since 3.6")

#+(and gtk-3-6 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-input-purpose atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-input-purpose 'function)
 "@version{2014-5-18}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[purpose]{the purpose}
  @syntax[]{(gtk-entry-input-purpose object) => purpose}
  @syntax[]{(setf (gtk-entry-input-purpose object) purpose)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{input-purpose} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-input-purpose} gets the value of the
  @slot[gtk-entry]{input-purpose} property.

  The generic function @sym{(setf gtk-entry-input-purpose)} sets the
  @slot[gtk-entry]{input-purpose} property which can be used by on-screen
  keyboards and other input methods to adjust their behaviour.

  Since 3.6
  @see-class{gtk-entry}")

;;; --- gtk-entry-invisible-char -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible-char" 'gtk-entry) 't)
 "The @code{invisible-char} property of type @code{:uint}
  (Read / Write) @br{}
  The invisible character is used when masking entry contents in \"password
  mode\". When it is not explicitly set with the @code{invisible-char}
  property, GTK+ determines the character to use from a list of possible
  candidates, depending on availability in the current font.
  This style property allows the theme to prepend a character to the list of
  candidates. @br{}
  Default value: \"*\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-invisible-char atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-invisible-char 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[ch]{a Unicode character}
  @syntax[]{(gtk-entry-invisible-char object) => ch}
  @syntax[]{(setf (gtk-entry-invisble-char object) ch)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{invisible-char} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-invisible-char} retrieves the character
  displayed in place of the real characters for entries with visibility set to
  false.

  The generic function @sym{(setf gtk-entry-invisible-char)} sets the character
  to use in place of the actual text when the generic function
  @fun{gtk-entry-visibility} has been called to set text visibility to
  @code{nil}.

  I. e. this is the character used in \"password mode\" to show the user how
  many characters have been typed. By default, GTK+ picks the best invisible
  char available in the current font. If you set the invisible char to 0, then
  the user will get no feedback at all; there will be no text on the screen as
  they type.
  @see-class{gtk-entry}
  @see-function{gtk-entry-visibility}
  @see-function{gtk-entry-unset-invisible-char}")

;;; --- gtk-entry-invisible-char-set -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible-char-set"
                                               'gtk-entry) 't)
 "The @code{invisible-char-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the invisible char has been set for the @sym{gtk-entry}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-invisible-char-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-invisible-char-set 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{invisible-char-set} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-max-length ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-length" 'gtk-entry) 't)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  Maximum number of characters for this entry. Zero if no maximum. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-max-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-max-length 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[max]{the maximum length of the entry, or 0 for no maximum, other
    than the maximum length of entries. The value passed in will be clamped to
    the range [0, 65536].}
  @syntax[]{(gtk-entry-max-length object) => max}
  @syntax[]{(setf (gtk-entry-max-length object) max)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{max-length} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-max-length} retrieves the maximum allowed
  length of the text in @arg{entry}, or 0 if there is no maximum.

  This is equivalent to:
  @code{(gtk-entry-buffer-max-length (gtk-entry-buffer object))}

  The generic function @sym{(setf gtk-entry-max-length)} sets the maximum
  allowed length of the contents of the widget. If the current contents are
  longer than the given length, then they will be truncated to fit.

  This is equivalent to:
  @code{(setf (gtk-entry-buffer-max-length (gtk-entry-buffer object)) max)}

  @see-class{gtk-entry}
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-max-length}")

;;; --- gtk-entry-max-width-chars ----------------------------------------------

#+(and gtk-3-12 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "max-width-chars" 'gtk-entry) 't)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write)
  The desired maximum width of the entry, in characters. If this property is set
  to -1, the width will be calculated automatically. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1 @br{}
  Since 3.12")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-max-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-max-width-chars 'function)
 "@version{2019-4-18}
  @syntax[]{(gtk-entry-max-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-entry-max-width-chars object) n-chars)}
  @argument[object]{a @class{gtk-entry} object}
  @argument[n-chars]{the @code{:int} maximum width, in characters}
  @begin{short}
    Accessor of the @slot[gtk-entry]{max-width-chars} slot of the
    @class{gtk-entry} class.
  @end{short}

  The @sym{gtk-entry-max-width-chars} slot access function
  retrieves the desired maximum width of the entry, in characters.

  The @sym{(setf gtk-entry-max-width-chars} slot access function
  sets the desired maximum width in characters of the entry.

  Since 3.12
  @see-class{gtk-entry}")

;;; --- gtk-entry-overwrite-mode -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "overwrite-mode" 'gtk-entry) 't)
 "The @code{overwrite-mode} property of type @code{:boolean}
  (Read / Write) @br{}
  If text is overwritten when typing in the @sym{gtk-entry}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-overwrite-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-overwrite-mode 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[overwrite]{new value}
  @syntax[]{(gtk-entry-overwrite-mode object) => overwrite}
  @syntax[]{(setf (gtk-entry-overwrite-mode object) overwrite)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{overwrite-mode} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-overwrite-mode} returns whether the text
  is overwritten when typing.

  The generic function @sym{(setf gtk-entry-overwrite-mode)} sets whether the
  text is overwritten when typing in the @class{gtk-entry}.
  @see-class{gtk-entry}")

;;; --- gtk-entry-placeholder-text ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "placeholder-text"
                                               'gtk-entry) 't)
 "The @code{placeholder-text} property of type @code{:string}
  (Read / Write) @br{}
  The text that will be displayed in the @class{gtk-entry} when it is empty and
  unfocused. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-placeholder-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-placeholder-text 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[text]{a string to be displayed when @arg{entry} is empty an
    unfocused, or @code{nil}}
  @syntax[]{(gtk-entry-placeholder-text object) => text}
  @syntax[]{(setf (gtk-entry-placeholder-text object) text)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{placeholder-text} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-placeholder-text} retrieves the text that
  will be displayed when @arg{entry} is empty and unfocused.

  The generic function @sym{(setf gtk-entry-placeholder-text)} sets text to be
  displayed in @arg{entry} when it is empty and unfocused. This can be used to
  give a visual hint of the expected contents of the @class{gtk-entry}.

  Note that since the placeholder text gets removed when the entry received
  focus, using this feature is a bit problematic if the entry is given the
  initial focus in a window. Sometimes this can be worked around by delaying
  the initial focus setting until the first key event arrives.
  @see-class{gtk-entry}")

;;; --- gtk-entry-populate-all -------------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "populate-all" 'gtk-entry) 't)
 "If @code{populate-all} is @em{true}, the \"populate-popup\" signal is
  also emitted for touch popups. @br{}
  Default value: @code{nil} @br{}
  Since 3.8")

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-populate-all atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-populate-all 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{populate-all} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-primary-icon-activatable -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-activatable"
                                                'gtk-entry) 't)
 "The @code{primary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is activatable.
  GTK+ emits the \"icon-press\" and \"icon-release\" signals only on sensitive,
  activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-activatable 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-activatable} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-activatable}
  @see-function{gtk-entry-set-icon-activatable}")

;;; --- gtk-entry-primary-icon-gicon -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-gicon"
                                               'gtk-entry) 't)
 "The @code{primary-icon-gicon} property of type @class{g-icon}
  (Read / Write) @br{}
  The @class{g-icon} to use for the primary icon for the entry.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-gicon 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-gicon} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-gicon}")

;;; --- gtk-entry-primary-icon-name --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-name"
                                               'gtk-entry) 't)
 "The @code{primary-icon-name} property of type @code{:string}
  (Read / Write) @br{}
  The icon name to use for the primary icon for the entry. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-name 'function)
 "@version{2014-6-6}
  Accessor of the slot @slot[gtk-entry]{primary-icon-name} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-name}")

;;; --- gtk-entry-primary-icon-pixbuf ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-pixbuf"
                                               'gtk-entry) 't)
 "The @code{primary-icon-pixbuf} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  A pixbuf to use as the primary icon for the entry.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-pixbuf 'function)
 "@version{2014-6-6}
  Accessor of the slot @slot[gtk-entry]{primary-icon-pixbuf} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-pixbuf}")

;;; --- gtk-entry-primary-icon-sensitive ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-sensitive"
                                               'gtk-entry) 't)
 "The @code{primary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is sensitive.
  An insensitive icon appears grayed out. GTK+ does not emit the \"icon-press\"
  and \"icon-release\" signals and does not allow DND from insensitive
  icons.
  An icon should be set insensitive if the action that would trigger when
  clicked is currently not available. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-sensitive 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-sensitive} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-sensitive}
  @see-function{gtk-entry-set-icon-sensitive}")

;;; --- gtk-entry-primay-icon-stock --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-stock"
                                               'gtk-entry) 't)
 "The @code{primary-icon-stock} property of type @code{:string} (Read / Write)
  @br{}
  The stock ID to use for the primary icon for the entry. @br{}
  @em{Warning:} The @code{primary-icon-stock} property has been deprecated
  since version 3.10 and should not be used in newly-written code. Use the
  @code{primary-icon-name} property instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-stock 'function)
 "@version{2014-6-8}
  Accessor of the @slot[gtk-entry]{primary-icon-stock} slot of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-primary-icon-storage-type ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-storage-type"
                                               'gtk-entry) 't)
 "The @code{primary-icon-storage-type} property of type
  @symbol{gtk-image-type} (Read) @br{}
  The representation which is used for the primary icon of the entry. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-storage-type 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-storage-type} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-storage-type}")

;;; --- gtk-entry-primary-icon-tooltip-markup ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-tooltip-markup"
                                               'gtk-entry) 't)
 "The @code{primary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language.
  Also see the function @fun{gtk-entry-set-icon-tooltip-markup}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-tooltip-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-tooltip-markup 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-tooltip-markup} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-entry-set-icon-tooltip-markup}")

;;; --- gtk-entry-primary-icon-tooltip-text ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "primary-icon-tooltip-text"
                                               'gtk-entry) 't)
 "The @code{primary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon.
  Also see the function @fun{gtk-entry-set-icon-tooltip-text}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-primary-icon-tooltip-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-primary-icon-tooltip-text 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{primary-icon-tooltip-text} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-entry-set-icon-tooltip-text}")

;;; --- gtk-entry-progress-fraction --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "progress-fraction"
                                               'gtk-entry) 't)
 "The @code{progress-fraction} property of type @code{:double}
  (Read / Write) @br{}
  The current fraction of the task that is been completed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-progress-fraction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-progress-fraction 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[fraction]{fraction of the task that is been completed}
  @syntax[]{(gtk-entry-progress-fraction object) => fraction}
  @syntax[]{(setf (gtk-entry-progress-fraction object) fraction)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{progress-fraction} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-progress-fraction} returns the current
  fraction from 0.0 to 1.0 of the task that is been completed.

  The generic function @sym{(setf gtk-entry-progress-fraction)} causes the
  entry progress indicator to \"fill in\" the given @arg{fraction} of the bar.
  The @arg{fraction} should be between 0.0 and 1.0, inclusive.
  @see-class{gtk-entry}")

;;; --- gtk-entry-progress-pulse-step ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "progress-pulse-step"
                                               'gtk-entry) 't)
 "The @code{progress-pulse-step} property of type @code{:double}
  (Read / Write) @br{}
  The fraction of total entry width to move the progress bouncing block for
  each call to the function @fun{gtk-entry-progress-pulse}. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-progress-pulse-step atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-progress-pulse-step 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[fraction]{fraction between 0.0 and 1.0}
  @syntax[]{(gtk-entry-progress-pulse-step object) => fraction}
  @syntax[]{(setf (gtk-entry-progress-pulse-step object) fraction)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{progress-pulse-step} of the
    @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-progress-pulse-step} retrieves the pulse
  step as a fraction from 0.0 to 1.0.

  The generic function @sym{(setf gtk-entry-progress-pulse-step)} sets the
  fraction of total entry width to move the progress bouncing block for each
  call to the function @fun{gtk-entry-progress-pulse}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-progress-pulse}")

;;; --- gtk-entry-scroll-offset ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scroll-offset" 'gtk-entry) 't)
 "The @code{scroll-offset} property of type @code{:int} (Read) @br{}
  Number of pixels of the entry scrolled off the screen to the left. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-scroll-offset atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-scroll-offset 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{scroll-offset} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-secondary-icon-activatable -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-activatable"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is activatable. GTK+ emits the \"icon-press\" and
  \"icon-release\" signals only on sensitive, activatable icons. Sensitive, but
  non-activatable icons can be used for purely informational purposes. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-activatable
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-activatable 'function)
 "@version{2014-6-8}
  Accessor of the @slot[gtk-entry]{seconary-icon-activatable} slot of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-activatable}
  @see-function{gtk-entry-set-icon-activatable}")

;;; --- gtk-entry-secondary-icon-gicon -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-gicon"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-gicon} property of type @class{g-icon}
  (Read / Write) @br{}
  The @class{g-icon} to use for the secondary icon for the entry.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-gicon 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-gicon} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-gicon}")

;;; --- gkt-entry-secondary-icon-name ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-name"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-name} property of type @code{:string}
  (Read / Write) @br{}
  The icon name to use for the secondary icon for the entry. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-name 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-name} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-name}")

;;; --- gtk-entry-secondary-icon-pixbuf ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-pixbuf"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-pixbuf} property of type @class{gdk-pixbuf}
  (Read / Write) @br{}
  An pixbuf to use as the secondary icon for the entry.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-pixbuf 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-pixbuf} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-pixbuf}")

;;; --- gtk-entry-secondary-icon-sensitive -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-sensitive"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is sensitive. An insensitive icon appears grayed
  out. GTK+ does not emit the \"icon-press\" and \"icon-release\" signals and
  does not allow DND from insensitive icons. An icon should be set insensitive
  if the action that would trigger when clicked is currently not available.@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-sensitive 'function)
 "@version{2014-6-8}
  Accessor of the @slot[gtk-entry]{seconary-icon-sensitive} slot of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-sensitive}
  @see-function{gtk-entry-set-icon-sensitive}")

;;; --- gtk-entry-icon-stock ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-stock"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-stock} property of type @code{:string}
  (Read / Write) @br{}
  The stock ID to use for the secondary icon for the entry. @br{}
  @em{Warning:} The @code{secondary-icon-stock} property has been deprecated
  since version 3.10 and should not be used in newly-written code. Use the
  @code{secondary-icon-name} property instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-stock 'function)
 "@version{2014-6-8}
  Accessor of the @slot[gtk-entry]{seconary-icon-stock} slot of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-secondary-icon-storage-type ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-storage-type"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-storage-type} property of type
  @symbol{gtk-image-type} (Read) @br{}
  The representation which is used for the secondary icon of the entry. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-storage-type
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-storage-type 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-storage-type} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-storage-type}")

;;; --- gtk-entry-secondary-icon-tooltip-markup --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-tooltip-markup"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language.
  Also see the function @fun{gtk-entry-set-icon-tooltip-markup}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-tooltip-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-tooltip-markup 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-tooltip-markup} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-entry-set-icon-tooltip-markup}")

;;; --- gtk-entry-secondary-icon-tooltip-text ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-icon-tooltip-text"
                                               'gtk-entry) 't)
 "The @code{secondary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon.
  Also see the function @fun{gtk-entry-set-icon-tooltip-text}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-secondary-icon-tooltip-text
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-secondary-icon-tooltip-text 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{seconary-icon-tooltip-text} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-entry-set-icon-tooltip-text}")

;;; --- gtk-entry-selection-bound ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-bound" 'gtk-entry) 't)
 "The @code{selection-bound} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in
  chars. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-selection-bound atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-selection-bound 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{selection-bound} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-shadow-type --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "shadow-type" 'gtk-entry) 't)
 "The @code{shadow-type} property of type @symbol{gtk-shadow-type}
  (Read / Write) @br{}
  Which kind of shadow to draw around the entry when \"has-frame\" is set to
  @em{true}. @br{}
  @em{Warning:} The @code{shadow-type} property has been deprecated since version
  3.20 and should not be used in newly-written code. Use CSS to determine the
  style of the border; the value of this style property is ignored. @br{}
  Default value: @code{:in}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-shadow-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-shadow-type 'function)
 "@version{2014-6-8}
  Accessor of the @slot[gtk-entry]{shadow-type} slot of the @class{gtk-entry}
  class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-show-emoji-icon ----------------------------------------------

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "show-emoji-icon" 'gtk-entry) 't)
 "The @code{show-emoji-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an icon for Emoji. @br{}
  Default value: @code{nil} @br{}
  Since 3.22")

#+(and gtk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-show-emoji-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-show-emoji-icon 'function)
 "@version{2019-4-18}
  Accessor of the @slot[gtk-entry]{show-emoji-icon} slot of the
  @class{gtk-entry} class.

  Since 3.22
  @see-class{gtk-entry}")

;;; --- gtk-entry-tabs ---------------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "tabs" 'gtk-entry) 't)
 "The @code{tabs} property of type @class{pango-tab-array}
  (Read / Write) @br{}
  A list of tabstop locations to apply to the text of the entry. @br{}
  Since 3.10")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-entry-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-tabs 'function)
 "@version{2014-11-9}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[tabs]{a @class{pango-tab-array}}
  @syntax[]{(gtk-entry-tabs object) => tabs}
  @syntax[]{(setf (gtk-entry-tabs object) tabs)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{tabs} of the @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-tabs} gets the tabstops that were set on
  the entry using the generic function @sym{(setf gtk-entry-tabs)}, if any.

  The tabstops in the array are applied to the entry text.

  Since 3.10
  @see-class{gtk-entry}")

;;; --- gtk-entry-text ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-entry) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The contents of the entry. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-text 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[text]{the new text}
  @syntax[]{(gtk-entry-text object) => text}
  @syntax[]{(setf (gtk-entry-text object) text)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{text} of the @class{gtk-entry} class.
  @end{short}

  The generic function @sym{gtk-entry-text} retrieves the contents of the
  entry widget as a string.

  See also the function @fun{gtk-editable-get-chars}.

  This is equivalent to:
  @code{(gtk-entry-buffer-text (gtk-entry-buffer object))}

  The generic function @sym{(setf gtk-entry-text)} sets the text in the
  entry to the given value, replacing the current contents.

  See the generic function @fun{gtk-entry-buffer-text}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-buffer-text}
  @see-function{gtk-editable-get-chars}")

;;; --- gtk-entry-text-length --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-length" 'gtk-entry) 't)
 "The @code{text-length} property of type @code{:uint} (Read) @br{}
  The length of the text in the @class{gtk-entry}. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-text-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-text-length 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @syntax[]{gtk-entry-text-length}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{text-length} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-text-length} retrieves the current length
  of the text in entry, or 0 if there are none.

  This is equivalent to:
  @code{(gtk-entry-buffer-length (gtk-entry-buffer entry))}
  @see-class{gtk-entry}
  @see-class{gtk-entry-buffer}
  @see-function{gtk-entry-buffer-length}")

;;; --- gtk-entry-truncate-multiline -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "truncate-multiline"
                                               'gtk-entry) 't)
 "The @code{truncate-multiline} property of type @code{:boolean}
  (Read / Write) @br{}
  When @em{true}, pasted multi-line text is truncated to the first line. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-truncate-multiline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-truncate-multiline 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{truncate-multiline} of the
  @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; --- gtk-entry-visibility ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visibility" 'gtk-entry) 't)
 "The @code{visibility} property of type @code{:boolean}
  (Read / Write) @br{}
  @code{Nil} displays the \"invisible char\" instead of the actual text
  (password mode). @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-visibility atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-visibility 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[visible]{@em{true} if the contents of the entry are displayed as
    plaintext}
  @syntax[]{(gtk-entry-visibility object) => visible}
  @syntax[]{(setf (gtk-entry-visibility object) visible)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{visibility} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-visible} retrieves whether the text in
  entry is visible.

  The generic function @sym{(setf gtk-entry-visible)} sets whether the contents
  of the entry are visible or not.

  When visibility is set to @code{nil}, characters are displayed as the
  invisible char, and will also appear that way when the text in the entry
  widget is copied elsewhere.

  By default, GTK+ picks the best invisible character available in the current
  font, but it can be changed with the generic function
  @fun{gtk-entry-invisible-char}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-invisible-char}")

;;; --- gtk-entry-width-chars --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-entry) 't)
 "The @code{width-chars} property of tpye @code{:int} (Read / Write) @br{}
  Number of characters to leave space for in the entry. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-width-chars 'function)
 "@version{2014-6-8}
  @argument[object]{a @class{gtk-entry} widget}
  @argument[n-chars]{width in chars}
  @syntax[]{(gtk-entry-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-entry-width-chars object) n-chars)}
  @begin{short}
    Accessor of the slot @slot[gtk-entry]{width-chars} of the @class{gtk-entry}
    class.
  @end{short}

  The generic function @sym{gtk-entry-width-chars} returns the number of chars
  to request space for, or negative if unset.

  The generic function @sym{(setf gtk-entry-width-chars)} changes the size
  request of the entry to be about the right size for @arg{n-chars} characters.

  Note that it changes the size request, the size can still be affected by how
  you pack the widget into containers. If @arg{n-chars} is -1, the size reverts
  to the default entry size.
  @see-class{gtk-entry}")

;;; --- gtk-entry-xalign -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "xalign" 'gtk-entry) 't)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
  layouts. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-entry-xalign atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-entry-xalign 'function)
 "@version{2014-6-8}
  Accessor of the slot @slot[gtk-entry]{xalign} of the @class{gtk-entry} class.
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new))

(defun gtk-entry-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-6-8}
  @return{A new @class{gtk-entry} widget.}
  @short{Creates a new entry.}
  @see-class{gtk-entry}"
  (make-instance 'gtk-entry))

(export 'gtk-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-new-with-buffer))

(defun gtk-entry-new-with-buffer (buffer)
 #+cl-cffi-gtk-documentation
 "@version{2014-6-8}
  @argument[buffer]{the buffer to use for the new @class{gtk-entry} widget}
  @return{A new @class{gtk-entry} widget.}
  @begin{short}
    Creates a new entry with the specified text @arg{buffer}.
  @end{short}
  @see-class{gtk-entry}"
  (make-instance 'gtk-entry
                 :buffer buffer))

(export 'gtk-entry-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_text_area" %gtk-entry-get-text-area) :void
  (entry (g-object gtk-entry))
  (text-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-text-area (entry)
 #+cl-cffi-gtk-documentation
 "@version{2014-6-8}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{text-area -- the text area}
  @begin{short}
    Gets the area where the entry's text is drawn. This function is useful
    when drawing something to the entry in a draw callback.
  @end{short}

  If the entry is not realized, @arg{text-area} is filled with zeros.

  See also the @fun{gtk-entry-get-icon-area} function.
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-area}"
  (let ((text-area (make-gdk-rectangle)))
    (%gtk-entry-get-text-area entry text-area)
    text-area))

(export 'gtk-entry-get-text-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_unset_invisible_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_unset_invisible_char" gtk-entry-unset-invisible-char) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-25}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Unsets the invisible char previously set with the function
    @fun{gtk-entry-invisible-char}. So that the default invisible char is
    used again.
  @end{short}
  @see-class{gtk-entry}
  @see-function{gtk-entry-invisible-char}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-set-alignment))

(defun gtk-entry-set-alignment (entry xalign)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[xalign]{the horizontal alignment, from 0 (left) to 1 (right),
    reversed for RTL layouts}
  @begin{short}
    Sets the alignment for the contents of the @arg{entry}.
  @end{short}
  This controls the horizontal positioning of the contents when the displayed
  text is shorter than the width of the @arg{entry}.
  @see-class{gtk-entry}"
  (setf (gtk-entry-xalign entry) xalign))

(export 'gtk-entry-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-entry-get-alignment))

(defun gtk-entry-get-alignment (entry)
 #+cl-cffi-gtk-documentation
 "@version{2014-9-11}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The alignment.}
  @begin{short}
    Gets the value set by the fucntion @fun{gtk-entry-set-alignment}.
  @end{short}
  @see-class{gtk-entry}"
  (gtk-entry-xalign entry))

(export 'gtk-entry-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_layout" gtk-entry-get-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
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
  @see-class{gtk-entry}
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
 "@version{2013-12-17}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    @code{x} -- x offset of layout, or @code{nil} @br{}
    @code{y} -- y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the position of the @class{pango-layout} used to render text in
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

(export 'gtk-entry-get-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_layout_index_to_text_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_layout_index_to_text_index"
          gtk-entry-layout-index-to-text-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[layout-index]{byte index into the entry layout text}
  @return{Byte index into the entry contents.}
  @begin{short}
    Converts from a position in the entry contents (returned by the function
    @fun{gtk-entry-text}) to a position in the @arg{entry}'s
    @class{pango-layout} (returned by the function @fun{gtk-entry-get-layout},
    with text retrieved via the function @fun{pango-layout-get-text}).
  @end{short}
  @see-class{gtk-entry}
  @see-function{gtk-entry-text}
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
 "@version{2014-11-9}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[text-index]{byte index into the @arg{entry} contents}
  @return{Byte index into the @arg{entry} layout text.}
  @begin{short}
    Converts from a position in the @arg{entry}'s @class{pango-layout} (returned
    by the function @fun{gtk-entry-get-layout}) to a position in the @arg{entry}
    contents (returned by the function @fun{gtk-entry-text}).
  @end{short}
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-layout}
  @see-function{gtk-entry-text}"
  (entry (g-object entry))
  (text-index :int))

(export 'gtk-entry-text-index-to-layout-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_cursor_hadjustment ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_cursor_hadjustment" gtk-entry-set-cursor-hadjustment)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2014-9-11}
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
  @see-class{gtk-entry}
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
 "@version{2014-11-9}
  @argument[entry]{a @class{gtk-entry} widget}
  @return{The horizontal cursor adjustment, or @code{nil} if none has been set.}
  @begin{short}
    Retrieves the horizontal cursor adjustment for the @arg{entry}.
  @end{short}
  See the function @fun{gtk-entry-set-cursor-hadjustment}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-cursor-hadjustment}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-cursor-hadjustment)

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
  @fun{gtk-entry-progress-pulse-step}.
  @see-class{gtk-entry}
  @see-function{gtk-entry-set-progress-pulse-step}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-progress-pulse)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_im_context_filter_keypress ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_im_context_filter_keypress"
           gtk-entry-im-context-filter-keypress) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[event]{the key event}
  @return{@em{True} if the input method handled the key event.}
  @begin{short}
    Allow the @class{gtk-entry} input method to internally handle key press and
    release events.
  @end{short}
  If this function returns @em{true}, then no further processing should be
  done for this key event. See the function
  @fun{gtk-im-context-filter-keypress}.

  Note that you are expected to call this function from your handler when
  overriding key event handling. This is needed in the case when you need to
  insert your own key handling between the input method and the default key
  event handling of the @class{gtk-entry}. See the function
  @fun{gtk-text-view-reset-im-context} for an example of use.
  @see-class{gtk-entry}
  @see-class{gdk-event-key}
  @see-function{gtk-im-context-filter-keypress}"
  (entry (g-object gtk-entry))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-entry-im-context-filter-keypress)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_reset_im_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_reset_im_context" gtk-entry-reset-im-context) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{short}
    Reset the input method context of the entry if needed.
  @end{short}

  This can be necessary in the case where modifying the buffer would confuse
  on-going input method behavior.
  @see-class{gtk-entry}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-reset-im-context)

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
 "@version{2014-11-9}
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
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_from_pixbuf" gtk-entry-set-icon-from-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @argument[pixbuf]{a @class{gdk-pixbuf}, or @code{nil}}
  @begin{short}
    Sets the icon shown in the specified position using a pixbuf.
  @end{short}

  If @arg{pixbuf} is @code{nil}, no icon will be shown in the specified
  position.
  @see-class{gtk-entry}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-entry-set-icon-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_stock ()
;;; ----------------------------------------------------------------------------

(defun gtk-entry-set-icon-from-stock (entry icon-pos stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @argument[stock-id]{the name of the stock item, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from a stock
    image.
  @end{short}

  If @arg{stock-id} is @code{nil}, no icon will be shown in the specified
  position.
  @begin[Warning]{dictionary}
    The @sym{gtk-entry-set-icon-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-entry-set-icon-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-icon-stock}
  @see-function{gtk-entry-set-icon-from-stock}"
  (cond ((eq icon-pos :primary)
         (setf (gtk-entry-primary-icon-stock entry) stock-id))
        ((eq icon-pos :secondary)
         (setf (gtk-entry-secondary-icon-stock entry) stock-id))
        (t
         (error "Unexpected icon position in GTK-ENTRY-SET-ICON-FROM-STOCK"))))

(export 'gtk-entry-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_from_icon_name" gtk-entry-set-icon-from-icon-name)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-30}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the position at which to set the icon}
  @argument[icon-name]{An icon name, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from the current
    icon theme.
  @end{short}

  If the icon name is not known, a \"broken image\" icon will be displayed
  instead.

  If @arg{icon-name} is @code{nil}, no icon will be shown in the specified
  position.
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (icon-name :string))

(export 'gtk-entry-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_from_gicon" gtk-entry-set-icon-from-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{the position of type @symbol{gtk-entry-icon-position} at
    which to set the icon}
  @argument[icon]{the icon of type @class{g-icon} to set, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from the current
    icon theme.
  @end{short}
  If the icon is not known, a \"broken image\" icon will be
  displayed instead.

  If @arg{icon} is @code{nil}, no icon will be shown in the specified position.
  @see-class{gtk-entry}
  @see-class{g-icon}
  @see-symbol{gtk-entry-icon-position}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (icon (g-object g-icon)))

(export 'gtk-entry-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_storage_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_storage_type" gtk-entry-get-icon-storage-type)
    gtk-image-type
 #+cl-cffi-gtk-documentation
 "@version{2014-1-22}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @return{Image representation of type @symbol{gtk-image-type} being used.}
  @begin{short}
    Gets the type of representation being used by the icon to store image data.
    If the icon has no image data, the return value will be @code{:empty}.
  @end{short}
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
 "@version{2014-11-9}
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
  @begin[Warning]{dictionary}
    The @sym{gtk-entry-get-icon-stock} function has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-entry-get-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-name}"
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
 "@version{2014-11-9}
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
  @see-class{gtk-entry}"
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

  See also the functions @fun{gtk-widget-tooltip-text} and
  @fun{gtk-entry-set-icon-tooltip-markup}.
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-tooltip-text}
  @see-function{gtk-widget-tooltip-text}
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

  See also the functions @fun{gtk-widget-tooltip-markup} and
  @fun{gtk-entry-set-icon-tooltip-text}.
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-icon-tooltip-markup}
  @see-function{gtk-widget-tooltip-markup}
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
  @see-class{gtk-entry}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-set-icon-tooltip-markup}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position))

(export 'gtk-entry-get-icon-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_set_icon_drag_source" gtk-entry-set-icon-drag-source) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[entry]{a @class{gtk-entry} widget}
  @argument[icon-pos]{icon position of type @symbol{gtk-entry-icon-position}}
  @argument[target-list]{the targets of type @class{gtk-target-list} (data
    formats) in which the data can be provided}
  @argument[actions]{a bitmask of type @symbol{gdk-drag-action} of the allowed
    drag actions}
  @begin{short}
    Sets up the icon at the given position so that GTK+ will start a drag
    operation when the user clicks and drags the icon.
  @end{short}

  To handle the drag operation, you need to connect to the usual
  \"drag-data-get\", or possibly \"drag-data-delete\", signal, and use the
  function @fun{gtk-entry-get-current-icon-drag-source} in your signal handler
  to find out if the drag was started from an icon.

  By default, GTK+ uses the icon as the drag icon. You can use the
  \"drag-begin\" signal to set a different icon. Note that you have to use the
  function @fun{g-signal-connect-after} to ensure that your signal handler gets
  executed after the default handler.
  @see-class{gtk-entry}
  @see-class{gtk-target-list}
  @see-symbol{gdk-drag-action}
  @see-symbol{gtk-entry-icon-position}
  @see-function{gtk-entry-get-current-icon-drag-source}
  @see-function{g-signal-connect-after}"
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (target-list (g-boxed-foreign gtk-target-list))
  (actions gdk-drag-action))

(export 'gtk-entry-set-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_current_icon_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_current_icon_drag_source"
           gtk-entry-get-current-icon-drag-source) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-20}
  @argument[entry]{a @class{gtk-entry} widget}
  @begin{return}
    Index of the icon which is the source of the current DND operation, or -1.
  @end{return}
  @begin{short}
    Returns the index of the icon which is the source of the current DND
    operation, or -1.
  @end{short}

  This function is meant to be used in a \"drag-data-get\" callback.
  @see-class{gtk-entry}"
  (entry (g-object gtk-entry)))

(export 'gtk-entry-get-current-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_get_icon_area" %gtk-entry-get-icon-area) :void
  (entry (g-object gtk-entry))
  (icon-pos gtk-entry-icon-position)
  (icon-area (g-boxed-foreign gdk-rectangle)))

(defun gtk-entry-get-icon-area (entry icon-pos)
 #+cl-cffi-gtk-documentation
 "@version{2014-11-9}
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
  @see-class{gtk-entry}
  @see-function{gtk-entry-get-text-area}"
  (let ((icon-area (make-gdk-rectangle)))
    (%gtk-entry-get-icon-area entry icon-pos icon-area)
    icon-area))

(export 'gtk-entry-get-icon-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_grab_focus_without_selecting ()
;;;
;;; void gtk_entry_grab_focus_without_selecting (GtkEntry *entry);
;;;
;;; Causes entry to have keyboard focus.
;;;
;;; It behaves like gtk_widget_grab_focus(), except that it doesn't select the
;;; contents of the entry. You only want to call this on some special entries
;;; which the user usually doesn't want to replace all text in, such as
;;; search-as-you-type entries.
;;;
;;; entry :
;;;     a GtkEntry
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.entry.lisp ---------------------------------------------
