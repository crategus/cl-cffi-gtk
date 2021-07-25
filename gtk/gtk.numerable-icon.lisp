;;; ----------------------------------------------------------------------------
;;; gtk.numerable-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 - 2021 Dieter Kaiser
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
;;; GtkNumerableIcon
;;;
;;;     A GIcon that allows numbered emblems
;;;
;;; Types and Values
;;;
;;;     GtkNumerableIcon
;;;
;;; Functions
;;;
;;;     gtk_numerable_icon_new
;;;     gtk_numerable_icon_new_with_style_context
;;;     gtk_numerable_icon_get_background_gicon            Accessor
;;;     gtk_numerable_icon_set_background_gicon            Accessor
;;;     gtk_numerable_icon_get_background_icon_name        Accessor
;;;     gtk_numerable_icon_set_background_icon_name        Accessor
;;;     gtk_numerable_icon_get_count                       Accessor
;;;     gtk_numerable_icon_set_count                       Accessor
;;;     gtk_numerable_icon_get_label                       Accessor
;;;     gtk_numerable_icon_set_label                       Accessor
;;;     gtk_numerable_icon_get_style_context               Accessor
;;;     gtk_numerable_icon_set_style_context               Accessor
;;;
;;; Properties
;;;
;;;           GIcon*   background-icon         Read / Write
;;;           gchar*   background-icon-name    Read / Write
;;;            gint    count                   Read / Write
;;;           gchar*   label                   Read / Write
;;; GtkStyleContext*   style-context	       Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GEmblemedIcon
;;;         ╰── GtkNumerableIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GtkNumerableIcon implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkNumerableIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkNumerableIcon" gtk-numerable-icon
  (:superclass g-emblemed-icon
   :export t
   :interfaces ("GIcon")
   :type-initializer "gtk_numerable_icon_get_type")
  ((background-icon
    gtk-numerable-icon-background-icon
    "background-icon" "GIcon" t t)
   (background-icon-name
    gtk-numerable-icon-background-icon-name
    "background-icon-name" "gchararray" t t)
   (count
    gtk-numerable-icon-count
    "count" "gint" t t)
   (label
    gtk-numerable-icon-label
    "label" "gchararray" t t)
   (style-context
    gtk-numerable-icon-style-context
    "style-context" "GtkStyleContext" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-numerable-icon 'type)
 "@version{2021-7-21}
  @begin{short}
    The @sym{gtk-numerable-icon} class is a subclass of the
    @class{g-emblemed-icon} class that can show a number or short string as an
    emblem.
  @end{short}
  The number can be overlayed on top of another emblem, if desired.

  It supports theming by taking font and color information from a provided
  @class{gtk-style-context} object.
  @begin[Example]{dictionary}
    Typical numerable icons:

    @image[numerableicon]{} @image[numerableicon2]{}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @sym{gtk-numerable-icon} class has been deprecated since version 3.14
    and should not be used in newly written code.
  @end{dictionary}
  @see-slot{gtk-numerable-icon-background-icon}
  @see-slot{gtk-numerable-icon-background-icon-name}
  @see-slot{gtk-numerable-icon-count}
  @see-slot{gtk-numerable-icon-label}
  @see-slot{gtk-numerable-icon-style-context}
  @see-class{g-emblemed-icon}
  @see-class{gtk-style-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-numerable-icon-background-icon -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-icon"
                                               'gtk-numerable-icon) 't)
 "The @code{background-icon} property of type @class{g-icon} (Read / Write)
  @br{}
  The icon for the number emblem background.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-background-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-background-icon 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-numerable-icon-background-icon object) => icon}
  @syntax[]{(setf (gtk-numerable-icon-background-icon object) icon)}
  @argument[object]{a @class{gtk-numerable-icon} object}
  @argument[icon]{a @class{g-icon} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-numerable-icon]{background-icon} slot of the
    @class{gtk-numerable-icon} class.
  @end{short}

  The slot access function @sym{gtk-numerable-icon-background-icon} returns the
  icon that was set as the base background image, or @code{nil} if there is
  none. The slot access function @sym{(setf gtk-numerable-icon-background-icon)}
  updates the background icon.

  If @arg{icon} is @code{nil}, the numerable icon will go back using style
  information or default theming for its background image.

  If this method is called and an icon name was already set as background for
  the numerable icon, @arg{icon} will be used, i.e. the last method called
  between the functions @sym{gtk-numerable-icon-background-icon} and
  @fun{gtk-numerable-icon-background-icon-name} has always priority.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-background-icon} has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-background-icon-name}")

;;; --- gtk-numerable-icon-background-icon-name --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-icon-name"
                                               'gtk-numerable-icon) 't)
 "The @code{background-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name for the number emblem background. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-background-icon-name
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-background-icon-name 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-numerable-icon-background-icon-name object) => name}
  @syntax[]{(setf (gtk-numerable-icon-background-icon-name object) name)}
  @argument[object]{a @class{gtk-numerable-icon} object}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-numerable-icon]{background-icon-name} slot of the
    @class{gtk-numerable-icon} class.
  @end{short}

  The slot access function @sym{gtk-numerable-icon-background-icon-name} returns
  the icon name used as the base background image, or @code{nil} if there is
  none. The slot access function
  @sym{(setf gtk-numerable-icon-background-icon-name)} updates the background
  icon.

  If @arg{name} is @code{nil}, the numerable icon will go back using style
  information or default theming for its background image.

  If this method is called and a @class{g-icon} object was already set as
  background for the numerable icon, @arg{name} will be used, i.e. the last
  method called between the functions
  @sym{gtk-numerable-icon-background-icon-name} and
  @fun{gtk-numerable-icon-background-icon} has always priority.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-backgroun-icon-name} has been
    deprecated since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-background-icon}")

;;; --- gtk-numerable-icon-count -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "count" 'gtk-numerable-icon) 't)
 "The @code{count} property of type @code{:int} (Read / Write) @br{}
  The count of the emblem currently displayed. @br{}
  Allowed values: [-99,99] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-count atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-count 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-numerable-icon-count object) => count}
  @syntax[]{(setf (gtk-numerable-icon-count object) count)}
  @argument[object]{a @class{gtk-numerable-icon} object}
  @argument[count]{an integer between -99 and 99}
  @begin{short}
    Accessor of the @slot[gtk-numerable-icon]{count} slot of the
    @class{gtk-numerable-icon} class.
  @end{short}

  The slot access function @sym{gtk-numerable-icon-count} returns the value
  currently displayed by the numerable icon. The slot access function
  @sym{(setf gtk-numerable-icon-count)} sets the currently displayed value.

  The numeric value is always clamped to make it two digits, i.e. between -99
  and 99. Setting a count of zero removes the emblem. If this method is called,
  and a label was already set on the numerable icon, it will automatically be
  reset to @code{nil} before rendering the number, i.e. the last method called
  between the functions @sym{gtk-numerable-icon-count} and
  @fun{gtk-numerable-icon-label} has always priority.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-count} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-label}")

;;; --- gtk-numerable-icon-label -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-numerable-icon) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label to be displayed over the icon. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-label 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-numerable-icon-label object) => label}
  @syntax[]{(setf (gtk-numerable-icon-label object) label)}
  @argument[object]{a @class{gtk-numerable-icon} object}
  @argument[label]{a string with a short label, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-numerable-icon]{label} slot of the
    @class{gtk-numerable-icon} class.
  @end{short}

  The slot access function @sym{gtk-numerable-icon-label} returns the currently
  displayed label of the numerable icon, or @code{nil}. The slot access function
  @sym{(setf gtk-numerable-icon-label)} sets the currently displayed label.
  Setting an empty label removes the emblem.

  Note that this is meant for displaying short labels, such as roman numbers,
  or single letters. For roman numbers, consider using the Unicode characters
  U+2160 - U+217F. Strings longer than two characters will likely not be
  rendered very well.

  If this method is called, and a number was already set on the icon, it will
  automatically be reset to zero before rendering the label, i.e. the last
  method called between the functions @sym{gtk-numerable-icon-label} and
  @fun{gtk-numerable-icon-count} has always priority.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-label} has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-count}")

;;; --- gtk-numerable-icon-style-context ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style-context"
                                               'gtk-numerable-icon) 't)
 "The @code{style-context} property of type @class{gtk-style-context}
  (Read / Write) @br{}
  The style context to theme the icon appearance.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-style-context atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-style-context 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-numerable-icon-style-context object) => style}
  @syntax[]{(setf (gtk-numerable-icon-style-context object) style)}
  @argument[object]{a @class{gtk-numerable-icon} object}
  @argument[style]{a @class{gtk-style-context} object}
  @begin{short}
    Accessor of the @slot[gtk-numerable-icon]{style-context} slot of the
    @class{gtk-numerable-icon} class.
  @end{short}

  The slot access function @sym{gtk-numerable-icon-style-context} returns the
  style context used by the numerable icon for theming, or @code{nil} if there
  is none. The slot access function
  @sym{(setf gtk-numerable-icon-style-context)} updates the numerable icon to
  fetch theme information.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-style-context} has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-class{gtk-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-new))

(defun gtk-numerable-icon-new (icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[icon]{a @class{g-icon} object to overlay on}
  @return{A new @class{gtk-numerable-icon} object.}
  @short{Creates a new unthemed numerable icon.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-new} has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-class{g-icon}"
  (make-instance 'gtk-numerable-icon
                 :background-icon icon))

(export 'gtk-numerable-icon-new)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new_with_style_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-new-with-style-context))

(defun gtk-numerable-icon-new-with-style-context (icon context)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-21}
  @argument[icon]{a @class{g-icon} object to overlay on}
  @argument[context]{a @class{gtk-style-context} object}
  @return{A new @class{gtk-numerable-icon} object.}
  @begin{short}
    Creates a new numerable icon which will themed according to the passed
    style context.
  @end{short}
  This is a convenience constructor that calls the function
  @fun{gtk-numerable-icon-style-context} internally.
  @begin[Warning]{dictionary}
    The function @sym{gtk-numerable-icon-new-with-style-context} has been
    deprecated since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-numerable-icon}
  @see-class{gtk-style-context}
  @see-function{gtk-numerable-icon-style-context}"
  (make-instance 'gtk-numerable-icon
                 :background-icon icon
                 :style-context context))

(export 'gtk-numerable-icon-new-with-style-context)

;;; --- End of file gtk.numerable-icon.lisp ------------------------------------
