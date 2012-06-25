;;; ----------------------------------------------------------------------------
;;; gtk.font-button.lisp
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
;;;
;;; GtkFontButton
;;;
;;; A button to launch a font chooser dialog
;;;
;;; Synopsis
;;;
;;;     GtkFontButton
;;;
;;;     gtk_font_button_new
;;;     gtk_font_button_new_with_font
;;;     gtk_font_button_set_font_name
;;;     gtk_font_button_get_font_name
;;;     gtk_font_button_set_show_style
;;;     gtk_font_button_get_show_style
;;;     gtk_font_button_set_show_size
;;;     gtk_font_button_get_show_size
;;;     gtk_font_button_set_use_font
;;;     gtk_font_button_get_use_font
;;;     gtk_font_button_set_use_size
;;;     gtk_font_button_get_use_size
;;;     gtk_font_button_set_title
;;;     gtk_font_button_get_title
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkFontButton
;;;
;;; Implemented Interfaces
;;;
;;; GtkFontButton implements AtkImplementorIface, GtkBuildable, GtkActionable,
;;; GtkActivatable and GtkFontChooser.
;;;
;;; Properties
;;;
;;;   "font-name"                gchar*                : Read / Write
;;;   "show-size"                gboolean              : Read / Write
;;;   "show-style"               gboolean              : Read / Write
;;;   "title"                    gchar*                : Read / Write
;;;   "use-font"                 gboolean              : Read / Write
;;;   "use-size"                 gboolean              : Read / Write
;;;
;;; Signals
;;;
;;;   "font-set"                                       : Run First
;;;
;;; Description
;;;
;;; The GtkFontButton is a button which displays the currently selected font an
;;; allows to open a font chooser dialog to change the font. It is suitable
;;; widget for selecting a font in a preference dialog.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-name" property
;;;
;;;   "font-name"                gchar*                : Read / Write
;;;
;;; The name of the currently selected font.
;;;
;;; Default value: "Sans 12"
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-size" property
;;;
;;;   "show-size"                gboolean              : Read / Write
;;;
;;; If this property is set to TRUE, the selected font size will be shown in the
;;; label. For a more WYSIWYG way to show the selected size, see the ::use-size
;;; property.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-style" property
;;;
;;;   "show-style"               gboolean              : Read / Write
;;;
;;; If this property is set to TRUE, the name of the selected font style will be
;;; shown in the label. For a more WYSIWYG way to show the selected style, see
;;; the ::use-font property.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;;
;;;   "title"                    gchar*                : Read / Write
;;;
;;; The title of the font chooser dialog.
;;;
;;; Default value: "Pick a Font"
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-font" property
;;;
;;;   "use-font"                 gboolean              : Read / Write
;;;
;;; If this property is set to TRUE, the label will be drawn in the selected
;;; font.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-size" property
;;;
;;;   "use-size"                 gboolean              : Read / Write
;;;
;;; If this property is set to TRUE, the label will be drawn with the selected
;;; font size.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-set" signal
;;;
;;; void user_function (GtkFontButton *widget,
;;;                     gpointer       user_data)      : Run First
;;;
;;; The ::font-set signal is emitted when the user selects a font. When handling
;;; this signal, use gtk_font_button_get_font_name() to find out which font was
;;; just selected.
;;;
;;; Note that this signal is only emitted when the user changes the font. If you
;;; need to react to programmatic font changes as well, use the
;;; notify::font-name signal.
;;;
;;; widget :
;;;     the object which received the signal.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontButton
;;;
;;; struct GtkFontButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontButton" gtk-font-button
  (:superclass gtk-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActionable"
                 "GtkActivatable"
                 "GtkFontChooser")
    :type-initializer "gtk_font_button_get_type")
  ((font-name
    gtk-font-button-font-name
    "font-name" "gchararray" t t)
   (show-size
    gtk-font-button-show-size
    "show-size" "gboolean" t t)
   (show-style
    gtk-font-button-show-style
    "show-style" "gboolean" t t)
   (title
    gtk-font-button-title
    "title" "gchararray" t t)
   (use-font
    gtk-font-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    gtk-font-button-use-size
    "use-size" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new ()
;;;
;;; GtkWidget * gtk_font_button_new (void);
;;;
;;; Creates a new font picker widget.
;;;
;;; Returns :
;;;     a new font picker widget.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font ()
;;;
;;; GtkWidget * gtk_font_button_new_with_font (const gchar *fontname);
;;;
;;; Creates a new font picker widget.
;;;
;;; fontname :
;;;     Name of font to display in font chooser dialog
;;;
;;; Returns :
;;;     a new font picker widget.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_font_name ()
;;;
;;; gboolean gtk_font_button_set_font_name (GtkFontButton *font_button,
;;;                                         const gchar *fontname);
;;;
;;; Sets or updates the currently-displayed font in font picker dialog.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; fontname :
;;;     Name of font to display in font chooser dialog
;;;
;;; Returns :
;;;     TRUE
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_font_name ()
;;;
;;; const gchar * gtk_font_button_get_font_name (GtkFontButton *font_button);
;;;
;;; Retrieves the name of the currently selected font. This name includes style
;;; and size information as well. If you want to render something with the font,
;;; use this string with pango_font_description_from_string() . If you're
;;; interested in peeking certain values (family name, style, size, weight) just
;;; query these properties from the PangoFontDescription object.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     an internal copy of the font name which must not be freed.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_show_style ()
;;;
;;; void gtk_font_button_set_show_style (GtkFontButton *font_button,
;;;                                      gboolean show_style);
;;;
;;; If show_style is TRUE, the font style will be displayed along with name of
;;; the selected font.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; show_style :
;;;     TRUE if font style should be displayed in label.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_show_style ()
;;;
;;; gboolean gtk_font_button_get_show_style (GtkFontButton *font_button);
;;;
;;; Returns whether the name of the font style will be shown in the label.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     whether the font style will be shown in the label.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_show_size ()
;;;
;;; void gtk_font_button_set_show_size (GtkFontButton *font_button,
;;;                                     gboolean show_size);
;;;
;;; If show_size is TRUE, the font size will be displayed along with the name of
;;; the selected font.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; show_size :
;;;     TRUE if font size should be displayed in dialog.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_show_size ()
;;;
;;; gboolean gtk_font_button_get_show_size (GtkFontButton *font_button);
;;;
;;; Returns whether the font size will be shown in the label.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     whether the font size will be shown in the label.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_use_font ()
;;;
;;; void gtk_font_button_set_use_font (GtkFontButton *font_button,
;;;                                    gboolean use_font);
;;;
;;; If use_font is TRUE, the font name will be written using the selected font.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; use_font :
;;;     If TRUE, font name will be written using font chosen.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_use_font ()
;;;
;;; gboolean gtk_font_button_get_use_font (GtkFontButton *font_button);
;;;
;;; Returns whether the selected font is used in the label.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     whether the selected font is used in the label.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_use_size ()
;;;
;;; void gtk_font_button_set_use_size (GtkFontButton *font_button,
;;;                                    gboolean use_size);
;;;
;;; If use_size is TRUE, the font name will be written using the selected size.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; use_size :
;;;     If TRUE, font name will be written using the selected size.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_use_size ()
;;;
;;; gboolean gtk_font_button_get_use_size (GtkFontButton *font_button);
;;;
;;; Returns whether the selected size is used in the label.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     whether the selected size is used in the label.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_title ()
;;;
;;; void gtk_font_button_set_title (GtkFontButton *font_button,
;;;                                 const gchar *title);
;;;
;;; Sets the title for the font chooser dialog.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; title :
;;;     a string containing the font chooser dialog title
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_title ()
;;;
;;; const gchar * gtk_font_button_get_title (GtkFontButton *font_button);
;;;
;;; Retrieves the title of the font chooser dialog.
;;;
;;; font_button :
;;;     a GtkFontButton
;;;
;;; Returns :
;;;     an internal copy of the title string which must not be freed.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.font-button.lisp ---------------------------------------
