;;; ----------------------------------------------------------------------------
;;; gtk.font-selection.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkFontSelection
;;; 
;;; Deprecated widget for selecting fonts
;;; 
;;; Synopsis
;;; 
;;;     GtkFontSelection
;;;
;;;     gtk_font_selection_new
;;;     gtk_font_selection_get_font_name
;;;     gtk_font_selection_set_font_name
;;;     gtk_font_selection_get_preview_text
;;;     gtk_font_selection_set_preview_text
;;;     gtk_font_selection_get_face
;;;     gtk_font_selection_get_face_list
;;;     gtk_font_selection_get_family
;;;     gtk_font_selection_get_size
;;;     gtk_font_selection_get_family_list
;;;     gtk_font_selection_get_preview_entry
;;;     gtk_font_selection_get_size_entry
;;;     gtk_font_selection_get_size_list
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontSelection
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFontSelection" 'gtk-font-selection))

(define-g-object-class "GtkFontSelection" gtk-font-selection
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_font_selection_get_type")
  ((font-name
    gtk-font-selection-font-name
    "font-name" "gchararray" t t)
   (preview-text
    gtk-font-selection-preview-text
    "preview-text" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-selection 'type)
 "@version{2013-6-18}
  @subheading{Warning}
    @sym{gtk-font-selection} is deprecated and should not be used in
    newly-written code.

  @begin{short}
    The @sym{gtk-font-selection} widget lists the available fonts, styles and
    sizes, allowing the user to select a font. It is used in the
    @class{gtk-font-selection-dialog} widget to provide a dialog box for
    selecting fonts.
  @end{short}

  To set the font which is initially selected, use the function
  @fun{gtk-font-selection-set-font-name}.

  To get the selected font use the function
  @fun{gtk-font-selection-get-font-name}.

  To change the text which is shown in the preview area, use the function
  @fun{gtk-font-selection-set-preview-text}.

  In GTK+ 3.2, @sym{gtk-font-selection} has been deprecated in favor of
  @class{gtk-font-chooser}.
  @see-slot{gtk-font-selection-get-font-name}
  @see-slot{gtk-font-selection-preview-text}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-name"
                                               'gtk-font-selection) 't)
 "The @code{\"font-name\"} property of type @code{:string} (Read / Write) @br{}
  The string that represents this font. @br{}
  Default value: \"Sans 10\"")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-text"
                                               'gtk-font-selection) 't)
 "The @code{\"preview-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The text to display in order to demonstrate the selected font. @br{}
  Default value: \"abcdefghijk ABCDEFGHIJK\"")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-font-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-font-name 'function)
 "@version{2013-6-18}
  Accessor of the slot @code{\"font-name\"} of the @class{gtk-font-selection}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-preview-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-preview-text 'function)
 "@version{2013-6-19}
  Accessor of the slot @code{\"preview-text\"} of the @class{gtk-font-selection}
  class.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-expand 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the
    @class{gtk-font-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-fill 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the
    @class{gtk-font-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-padding 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the
    @class{gtk-font-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-pack-type 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the
    @class{gtk-font-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-position 'function)
 "@version{2013-3-6}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the
    @class{gtk-font-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_new ()
;;; 
;;; GtkWidget * gtk_font_selection_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_new is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Creates a new GtkFontSelection.
;;; 
;;; Returns :
;;;     a n ew GtkFontSelection
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_font_name ()
;;; 
;;; gchar * gtk_font_selection_get_font_name (GtkFontSelection *fontsel);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_font_name has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the currently-selected font name.
;;; 
;;; Note that this can be a different string than what you set with
;;; gtk_font_selection_set_font_name(), as the font selection widget may
;;; normalize font names and thus return a string with a different structure.
;;; For example, "Helvetica Italic Bold 12" could be normalized to
;;; "Helvetica Bold Italic 12". Use pango_font_description_equal() if you want
;;; to compare two font descriptions.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     A string with the name of the current font, or NULL if no font is
;;;     selected. You must free this string with g_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_font_name ()
;;; 
;;; gboolean gtk_font_selection_set_font_name (GtkFontSelection *fontsel,
;;;                                            const gchar *fontname);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_set_font_name has been deprecated since version 3.2 and
;;; should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Sets the currently-selected font.
;;; 
;;; Note that the fontsel needs to know the screen in which it will appear for
;;; this to work; this can be guaranteed by simply making sure that the fontsel
;;; is inserted in a toplevel window before you call this function.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; fontname :
;;;     a font name like "Helvetica 12" or "Times Bold 18"
;;; 
;;; Returns :
;;;     TRUE if the font could be set successfully; FALSE if no such font exists
;;;     or if the fontsel doesn't belong to a particular screen yet.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_text ()
;;; 
;;; const gchar * gtk_font_selection_get_preview_text
;;;                                                  (GtkFontSelection *fontsel)
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_get_preview_text has been deprecated since version 3.2
;;; and should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Gets the text displayed in the preview area.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; Returns :
;;;     the text displayed in the preview area. This string is owned by the
;;;     widget and should not be modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_preview_text ()
;;; 
;;; void gtk_font_selection_set_preview_text (GtkFontSelection *fontsel,
;;;                                           const gchar *text);
;;; 
;;; Warning
;;; 
;;; gtk_font_selection_set_preview_text has been deprecated since version 3.2
;;; and should not be used in newly-written code. Use GtkFontChooser
;;; 
;;; Sets the text displayed in the preview area. The text is used to show how
;;; the selected font looks.
;;; 
;;; fontsel :
;;;     a GtkFontSelection
;;; 
;;; text :
;;;     the text to display in the preview area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_face" gtk-font-selection-get-face)
    (g-object pango-font-face)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    A @class{pango-font-face} representing the selected font group details.
  @end{return}
  @subheading{Warning}
    @sym{gtk-font-selection-get-face} has been deprecated since version 3.2 and
    should not be used in newly-written code. Use @class{gtk-font-chooser}.
  
  @begin{short}
    Gets the @class{pango-font-face} representing the selected font group
    details (i. e. family, slant, weight, width, etc).
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_face_list" gtk-font-selection-get-face-list)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of fontsel.}
  @subheading{Warning}
    @sym{gtk-font-selection-get-face-list} has been deprecated since version 3.2
    and should not be used in newly-written code. Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} which lists all styles available for
    the selected font. For example, 'Regular', 'Bold', etc.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-face-list)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_family" gtk-font-selection-get-family)
    (g-object pango-font-family)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    A @class{pango-font-family} representing the selected font family. Font
    families are a collection of font faces. The returned object is owned by
    @arg{fontsel} and must not be modified or freed.
  @end{return}
  @subheading{Warning}
    @sym{gtk-font-selection-get-family} has been deprecated since version 3.2
    and should not be used in newly-written code. Use @class{gtk-font-chooser}.

  @begin{short}
    Gets the @class{pango-font-family} representing the selected font family.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size" gtk-font-selection-get-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    An integer representing the selected font size, or -1 if no font size
    is selected.
  @end{return}
  @subheading{Warning}
    @sym{gtk-font-selection-get-size} has been deprecated since version 3.2 and
    should not be used in newly-written code. Use @class{gtk-font-chooser}.

  @short{The selected font size.}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_family_list"
           gtk-font-selection-get-family-list) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    @sym{gtk-font-selection-get-family-list} has been deprecated since version
    3.2 and should not be used in newly-written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} that lists font families, for
    example, 'Sans', 'Serif', etc.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-family-list)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_preview_entry"
           gtk-font-selection-get-preview-entry) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    @sym{gtk-font-selection-get-preview-entry} has been deprecated since
    version 3.2 and should not be used in newly-written code.
    Use @class{gtk-font-chooser}.
  
  @begin{short}
    This returns the @class{gtk-entry} used to display the font as a preview.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-preview-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size_entry" gtk-font-selection-get-size-entry)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    @sym{gtk-font-selection-get-size-entry} has been deprecated since
    version 3.2 and should not be used in newly-written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-entry} used to allow the user to edit the font
    number manually instead of selecting it from the list of font sizes.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-size-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size_list" gtk-font-selection-get-size-list)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    @sym{gtk-font-selection-get-size-list} has been deprecated since
    version 3.2 and should not be used in newly-written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} used to list font sizes.
  @end{short}

  Since 2.14"
  (fontsel (g-object gtk-font-selection)))

(export 'gtk-font-selection-get-size-list)

;;; --- End of file gtk.font-selection.lisp ------------------------------------
