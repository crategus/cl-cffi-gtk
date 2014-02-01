;;; ----------------------------------------------------------------------------
;;; gtk.numerable-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 Dieter Kaiser
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
;;; A GIcon that allows numbered emblems
;;;
;;; Synopsis
;;;
;;;     GtkNumerableIcon
;;;
;;;     gtk_numerable_icon_new
;;;     gtk_numerable_icon_new_with_style_context
;;;     gtk_numerable_icon_get_background_gicon
;;;     gtk_numerable_icon_set_background_gicon
;;;     gtk_numerable_icon_get_background_icon_name
;;;     gtk_numerable_icon_set_background_icon_name
;;;     gtk_numerable_icon_get_count
;;;     gtk_numerable_icon_set_count
;;;     gtk_numerable_icon_get_label
;;;     gtk_numerable_icon_set_label
;;;     gtk_numerable_icon_get_style_context
;;;     gtk_numerable_icon_set_style_context
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
    gtk-emblemed-icon-background-icon
    "background-icon" "GIcon" t t)
   (background-icon-name
    gtk-emblemed-icon-backgroun-icon-name
    "background-icon-name" "gchararray" t t)
   (count
    gtk-emblemed-icon-count
    "count" "gint" t t)
   (label
    gtk-emblemed-icon-label
    "label" "gchararray" t t)
   (style-context
    gtk-emblemed-icon-style-context
    "style-context" "GtkStyleContext" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-numerable-icon 'type)
 "@version{2014-1-27}
  @begin{short}
    @sym{gtk-numerable-icon} is a subclass of @class{g-emblemed-icon} that
    can show a number or short string as an emblem.
  @end{short}
  The number can be overlayed on top of another emblem, if desired.

  It supports theming by taking font and color information from a provided
  @class{gtk-style-context}; see the function
  @fun{gtk-numerable-icon-set-style-context}.

  @b{Example}. Typical numerable icons

  @image[numerableicon]{} @image[numerableicon2]{}

  @see-slot{gtk-numerable-icon-background-icon}
  @see-slot{gtk-numerable-icon-background-icon-name}
  @see-slot{gtk-numerable-icon-count}
  @see-slot{gtk-numerable-icon-label}
  @see-slot{gtk-numerable-icon-style-context}
  @see-class{g-emblemed-icon}
  @see-class{gtk-style-context}
  @see-function{gtk-numerable-icon-set-style-context}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation  (atdoc:get-slot-from-name "background-icon"
                                                'gtk-numerable-icon) 't)
 "The @code{\"background-icon\"} property of type @class{g-icon}
  (Read / Write) @br{}
  The icon for the number emblem background.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-icon-name"
                                               'gtk-numerable-icon) 't)
 "The @code{\"background-icon-name\"} property of type @code{:string}
  (Read / Write) @br{}
  The icon name for the number emblem background. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "count" 'gtk-numerable-icon) 't)
 "The @code{\"count\"} property of type @code{:int} (Read / Write) @br{}
  The count of the emblem currently displayed. @br{}
  Allowed values: [-99,99] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-numerable-icon) 't)
 "The @code{\"label\"} property of type @code{:string} (Read / Write) @br{}
  The label to be displayed over the icon. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style-context"
                                               'gtk-numerable-icon) 't)
 "The @code{\"style-context\"} property of type @class{gtk-style-context}
  (Read / Write) @br{}
  The style context to theme the icon appearance.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-background-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-background-icon 'function)
 "@version{2014-1-27}
  Accessor of the slot @slot[gtk-numerable-icon]{background-icon} of the
  @class{gtk-numerable-icon} class.
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-get-background-gicon}
  @see-function{gtk-numerable-icon-set-background-gicon}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-background-icon-name
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-background-icon-name 'function)
 "@version{2014-1-27}
  Accessor of the slot @slot[gtk-numerable-icon]{background-icon-name} of the
  @class{gtk-numerable-icon} class.
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-get-background-icon-name}
  @see-function{gtk-numerable-icon-set-background-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-count atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-count 'function)
 "@version{2014-1-27}
  Accessor of the slot @slot[gtk-numerable-icon]{count} of the
  @class{gtk-numerable-icon} class.
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-get-count}
  @see-function{gtk-numerable-icon-set-count}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-label 'function)
 "@version{2014-1-27}
  Accessor of the slot @slot[gtk-numerable-icon]{label} of the
  @class{gtk-numerable-icon} class.
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-get-label}
  @see-function{gtk-numerable-icon-set-label}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-numerable-icon-style-context atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-numerable-icon-style-context 'function)
 "@version{2014-1-27}
  Accessor of the slot @slot[gtk-numerable-icon]{style-context} of the
  @class{gtk-numerable-icon} class.
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-get-style-context}
  @see-function{gtk-numerable-icon-set-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-new))

(defun gtk-numerable-icon-new (base-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[base-icon]{a @class{g-icon} to overlay on}
  @return{A new @class{g-icon}.}
  @short{Creates a new unthemed @class{gtk-numerable-icon} object.}

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{g-icon}"
  (make-instance 'gtk-numerable-icon
                 :gicon base-icon))

(export 'gtk-numerable-icon-new)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new_with_style_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-new-with-style-context))

(defun gtk-numerable-icon-new-with-style-context (base-icon context)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[base-icon]{a @class{g-icon} to overlay on}
  @argument[context]{a @class{gtk-style-context} object}
  @return{A new @class{g-icon}.}
  @begin{short}
    Creates a new @class{gtk-numerable-icon} object which will themed according
    to the passed @class{gtk-style-context} object.
  @end{short}
  This is a convenience constructor that calls the function
  @fun{gtk-numerable-icon-set-style-context} internally.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{gtk-style-context}
  @see-class{g-icon}
  @see-function{gtk-numerable-icon-set-style-context}"
  (make-instance 'gtk-numerable-icon
                 :background-icon base-icon
                 :style-context context))

(export 'gtk-numerable-icon-new-with-style-context)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_background_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-get-background-gicon))

(defun gtk-numerable-icon-get-background-gicon (numerable-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-26}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @return{A @class{g-icon}, or @code{nil}.}
  @begin{short}
    Returns the @class{g-icon} that was set as the base background image, or
    @code{nil} if there's none.
  @end{short}
  The caller of this function does not own a reference to the returned
  @class{g-icon}.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{g-icon}"
  (gtk-numerable-icon-background-icon numerable-icon))

(export 'gtk-numerable-icon-get-background-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_background_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-set-background-gicon))

(defun gtk-numerable-icon-set-background-gicon (numerable-icon icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @argument[icon]{a @class{g-icon}, or @code{nil}}
  @begin{short}
    Updates the icon to use @arg{icon} as the base background image.
  @end{short}
  If @arg{icon} is @code{nil}, @arg{numerable-icon} will go back using style
  information or default theming for its background image.

  If this method is called and an icon name was already set as background for
  the icon, @arg{icon} will be used, i. e. the last method called between the
  functions @sym{gtk-numerable-icon-set-background-gicon} and
  @fun{gtk-numerable-icon-set-background-icon-name} has always priority.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{g-icon}
  @see-function{gtk-numerable-icon-set-background-icon-name}"
  (setf (gtk-numerable-icon-background-icon numerable-icon) icon))

(export 'gtk-numerable-icon-set-background-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_background_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-get-background-icon-name))

(defun gtk-numerable-icon-get-background-icon-name (numerable-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @return{an icon name, or @code{nil}}
  @begin{short}
    Returns the icon name used as the base background image, or @code{nil}
    if there is none.
  @end{short}

  Since 3.0
  @see-class{gtk-numerable-icon}"
  (gtk-numerable-icon-background-icon-name numerable-icon))

(export 'gtk-numerable-icon-get-background-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_background_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-set-background-icon-name))

(defun gtk-numerable-icon-set-background-icon-name (numerable-icon icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @argument[icon-name]{an icon name, or @code{nil}}
  @begin{short}
    Updates the icon to use the icon named @arg{icon-name} from the current
    icon theme as the base background image.
  @end{short}
  If @arg{icon-name} is @code{nil}, @arg{numerable-icon} will go back using
  style information or default theming for its background image.

  If this method is called and a @class{g-icon} was already set as background
  for the icon, @arg{icon-name} will be used, i. e. the last method called
  between the functions @sym{gtk-numerable-icon-set-background-icon-name} and
  @fun{gtk-numerable-icon-set-background-gicon} has always priority.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{g-icon}
  @see-function{gtk-numerable-icon-set-background-gicon}"
  (setf (gtk-numerable-icon-background-icon-name numerable-icon) icon-name))

(export 'gtk-numerable-icon-set-background-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_count ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-get-count))

(defun gtk-numerable-icon-get-count (numerable-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @return{The currently displayed value.}
  @begin{short}
    Returns the value currently displayed by @arg{numerable-icon}.
  @end{short}

  Since 3.0
  @see-class{gtk-numerable-icon}"
  (gtk-numerable-icon-count numerable-icon))

(export 'gtk-numerable-icon-get-count)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_count ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-set-count))

(defun gtk-numerable-icon-set-count (numerable-icon count)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @argument[count]{a number between -99 and 99}
  @begin{short}
    Sets the currently displayed value of @arg{numerable-icon} to @arg{count}.
  @end{short}

  The numeric value is always clamped to make it two digits, i. e. between -99
  and 99. Setting a count of zero removes the emblem. If this method is called,
  and a label was already set on the icon, it will automatically be reset to
  @code{nil} before rendering the number, i. e. the last method called between
  the functions @sym{gtk-numerable-icon-set-count} and
  @fun{gtk-numerable-icon-set-label} has always priority.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-set-label}"
  (setf (gtk-numerable-icon-count numerable-icon) count))

(export 'gtk-numerable-icon-set-count)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-get-label))

(defun gtk-numerable-icon-get-label (numerable-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @return{The currently displayed label.}
  @begin{short}
    Returns the currently displayed label of the icon, or @code{nil}.
  @end{short}

  Since 3.0
  @see-class{gtk-numerable-icon}"
  (gtk-numerable-icon-label numerable-icon))

(export 'gtk-numerable-icon-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-set-label))

(defun gtk-numerable-icon-set-label (numerable-icon label)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @argument[label]{a short label, or @code{nil}}
  @begin{short}
    Sets the currently displayed value of @arg{numerable-icon} to the string in
    @arg{label}.
  @end{short}
  Setting an empty label removes the emblem.

  Note that this is meant for displaying short labels, such as roman numbers,
  or single letters. For roman numbers, consider using the Unicode characters
  U+2160 - U+217F. Strings longer than two characters will likely not be
  rendered very well.

  If this method is called, and a number was already set on the icon, it will
  automatically be reset to zero before rendering the label, i. e. the last
  method called between the function @sym{gtk-numerable-icon-set-label} and
  @fun{gtk-numerable-icon-set-count} has always priority.

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-function{gtk-numerable-icon-set-count}"
  (setf (gtk-numerable-icon-label numerable-icon) label))

(export 'gtk-numerable-icon-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_style_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-get-style-context))

(defun gtk-numerable-icon-get-style-context (numerable-icon)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @begin{return}
    A @class{gtk-style-context} object, or @code{nil}. This object is internal
    to GTK+ and should not be unreffed. Use the function @fun{g-object-ref} if
    you want to keep it around.
  @end{return}
  @begin{short}
    Returns the @class{gtk-style-context} object used by the icon for theming,
    or @code{nil} if thereis none.
  @end{short}

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{gtk-style-context}
  @see-function{g-object-ref}"
  (gtk-numerable-icon-style-context numerable-icon))

(export 'gtk-numerable-icon-get-style-context)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_style_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-numerable-icon-set-style-context))

(defun gtk-numerable-icon-set-style-context (numerable-icon style)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-27}
  @argument[numerable-icon]{a @class{gtk-numerable-icon} object}
  @argument[style]{a @class{gtk-style-context} object}
  @begin{short}
    Updates the icon to fetch theme information from the given
    @class{gtk-style-context} object.
  @end{short}

  Since 3.0
  @see-class{gtk-numerable-icon}
  @see-class{gtk-style-context}"
  (setf (gtk-numerable-icon-style-context numerable-icon) style))

(export 'gtk-numerable-icon-set-style-context)

;;; --- End of file gtk.numerable-icon.lisp ------------------------------------
