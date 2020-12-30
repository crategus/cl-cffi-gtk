;;; ----------------------------------------------------------------------------
;;; gtk.font-selection.lisp
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
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
   :export nil
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
    newly written code. Use @class{gtk-font-chooser}.

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
  @see-slot{gtk-font-selection-font-name}
  @see-slot{gtk-font-selection-preview-text}
  @see-function{gtk-font-selection-set-font-name}
  @see-function{gtk-font-selection-get-font-name}
  @see-function{gtk-font-selection-set-preview-text}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-selection-font-name -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-name"
                                               'gtk-font-selection) 't)
 "The @code{font-name} property of type @code{:string} (Read / Write) @br{}
  The string that represents this font. @br{}
  Default value: \"Sans 10\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-font-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-font-name 'function)
 "@version{2013-6-18}
  Accessor of the @slot[gtk-font-selection]{font-name} slot of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; --- gtk-font-selection-preview-text ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "preview-text"
                                               'gtk-font-selection) 't)
 "The @code{preview-text} property of type @code{:string}
  (Read / Write) @br{}
  The text to display in order to demonstrate the selected font. @br{}
  Default value: \"abcdefghijk ABCDEFGHIJK\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-preview-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-preview-text 'function)
 "@version{2013-6-19}
  Accessor of the @slot[gtk-font-selection]{preview-text} slot of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-selection-child-expand ----------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-expand
                       "expand" "gboolean" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-expand 'function)
 "@version{2013-8-28}
  Accessor of the @code{expand} child property of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; --- gtk-font-selection-child-fill ------------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-fill
                       "fill" "gboolean" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-fill 'function)
 "@version{2013-8-28}
  Accessor of the @code{fill} child property of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; --- gtk-font-selection-child-padding ---------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-padding
                       "padding" "guint" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-padding 'function)
 "@version{2013-8-28}
  Accessor of the @code{padding} child property of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; --- gtk-font-selection-child-pack-type -------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-pack-type
                       "pack-type" "GtkPackType" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-pack-type 'function)
 "@version{2013-8-28}
  Accessor of the @code{pack-type} child property of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; --- gtk-font-selection-child-position --------------------------------------

(define-child-property "GtkFontSelection"
                       gtk-font-selection-child-position
                       "position" "gint" t t nil)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-selection-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-selection-child-position 'function)
 "@version{2013-8-28}
  Accessor of the @code{position} child property of the
  @class{gtk-font-selection} class.
  @see-class{gtk-font-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-selection-new))

(defun gtk-font-selection-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @return{A new @class{gtk-font-selection} widget.}
  @begin{short}
    Creates a new @class{gtk-font-selection} widget.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-font-selection-new} is deprecated and should not be
    used in newly written code. Use @class{gtk-font-chooser}.
  @end{dictionary}
  @see-class{gtk-font-selection}"
  (make-instance 'gtk-font-selection-new))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-selection-get-font-name))

(defun gtk-font-selection-get-font-name (fontsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A string with the name of the current font, or @code{nil} if no font
    is selected.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-font-name} has been deprecated
    since version 3.2 and should not be used in newly written code. Use
    @class{gtk-font-chooser}.

  @begin{short}
    Gets the currently-selected font name.
  @end{short}

  Note that this can be a different string than what you set with the function
  @fun{gtk-font-selection-set-font-name}, as the font selection widget may
  normalize font names and thus return a string with a different structure.
  For example, \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\". Use the function
  @fun{pango-font-description-equal} if you want to compare two font
  descriptions.
  @see-function{gtk-font-selection-set-font-name}
  @see-function{pango-font-description-equal}"
  (gtk-font-selection-font-name fontsel))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-selection-set-font-name))

(defun gtk-font-selection-set-font-name (fontsel fontname)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @argument[fontname]{a font name like \"Helvetica 12\" or \"Times Bold 18\"}
  @begin{return}
    @em{True} if the font could be set successfully; @code{nil} if no such font
    exists or if the @arg{fontsel} does not belong to a particular screen yet.
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-font-selection-set-font-name} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    Sets the currently-selected font.
  @end{short}

  Note that the @arg{fontsel} needs to know the screen in which it will appear
  for this to work; this can be guaranteed by simply making sure that the
  @arg{fontsel} is inserted in a toplevel window before you call this function.
  @see-function{gtk-font-selection-get-font-name}"
  (setf (gtk-font-selection-font-name fontsel) fontname))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-selection-get-preview-text))

(defun gtk-font-selection-get-preview-text (fontsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{The text displayed in the preview area.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-preview-text} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    Gets the text displayed in the preview area.
  @end{short}
  @see-function{gtk-font-selection-set-preview-text}"
  (gtk-font-selection-preview-text fontsel))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-selection-set-preview-text))

(defun gtk-font-selection-set-preview-text (fontsel text)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @argument[text]{the text to display in the preview area}
  @subheading{Warning}
    The function @sym{gtk-font-selection-set-preview-text} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    Sets the text displayed in the preview area. The text is used to show how
    the selected font looks.
  @end{short}
  @see-function{gtk-font-selection-get-preview-text}"
  (setf (gtk-font-selection-preview-text fontsel) text))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_face" gtk-font-selection-get-face)
    (g-object pango-font-face)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-1}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    A @class{pango-font-face} representing the selected font group details.
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-face} has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    Gets the @class{pango-font-face} representing the selected font group
    details (i.e. family, slant, weight, width, etc).
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_face_list" gtk-font-selection-get-face-list)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of fontsel.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-face-list} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} which lists all styles available for
    the selected font. For example, 'Regular', 'Bold', etc.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_family" gtk-font-selection-get-family)
    (g-object pango-font-family)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    A @class{pango-font-family} representing the selected font family. Font
    families are a collection of font faces. The returned object is owned by
    @arg{fontsel} and must not be modified or freed.
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-family} has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    Gets the @class{pango-font-family} representing the selected font family.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size" gtk-font-selection-get-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @begin{return}
    An integer representing the selected font size, or -1 if no font size
    is selected.
  @end{return}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-size} has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @short{The selected font size.}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_family_list"
           gtk-font-selection-get-family-list) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-family-list} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} that lists font families, for
    example, 'Sans', 'Serif', etc.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_preview_entry"
           gtk-font-selection-get-preview-entry) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-preview-entry} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-entry} used to display the font as a preview.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size_entry" gtk-font-selection-get-size-entry)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-size-entry} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-entry} used to allow the user to edit the font
    number manually instead of selecting it from the list of font sizes.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_font_selection_get_size_list" gtk-font-selection-get-size-list)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[fontsel]{a @class{gtk-font-selection} widget}
  @return{A @class{gtk-widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The function @sym{gtk-font-selection-get-size-list} has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk-font-chooser}.

  @begin{short}
    This returns the @class{gtk-tree-view} used to list font sizes.
  @end{short}
  @see-class{gtk-font-selection}"
  (fontsel (g-object gtk-font-selection)))

;;; --- End of file gtk.font-selection.lisp ------------------------------------
