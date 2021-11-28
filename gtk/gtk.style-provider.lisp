;;; ----------------------------------------------------------------------------
;;; gtk.style-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;; GtkStyleProvider
;;;
;;;     Interface to provide style information to GtkStyleContext
;;;
;;; Types and Values
;;;
;;;     GtkStyleProvider
;;;
;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; Functions
;;;
;;;     gtk_style_provider_get_icon_factory                missing / deprecated
;;;     gtk_style_provider_get_style                       missing / ddprectaed
;;;     gtk_style_provider_get_style_property
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkStyleProvider
;;;
;;; Known Implementations
;;;     GtkStyleProvider is implemented by GtkCssProvider and GtkSettings.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkStyleProvider
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkStyleProvider" gtk-style-provider
  (:export t
   :type-initializer "gtk_style_provider_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-provider atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-style-provider 'type)
 "@version{2021-11-27}
  @begin{short}
    The @sym{gtk-style-provider} interface is an interface used to provide style
    information to a @class{gtk-style-context} object.
  @end{short}
  See the @fun{gtk-style-context-add-provider} and
  @fun{gtk-style-context-add-provider-for-screen} functions.
  @see-class{gtk-style-context}
  @see-class{gtk-css-provider}
  @see-function{gtk-style-context-add-provider}
  @see-function{gtk-style-context-add-provider-for-screen}")

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;; ----------------------------------------------------------------------------

(defconstant +gtk-style-provider-priority-fallback+ 1)

#+cl-cffi-gtk-documentation
(setf (gethash '+gtk-style-provider-priority-fallback+
               atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+gtk-style-provider-priority-fallback+ 'variable)
 "@version{2021-11-27}
  @variable-value{1}
  @begin{short}
    The priority used for default style information that is used in the absence
    of themes.
  @end{short}
  @see-class{gtk-style-provider}")

(export '+gtk-style-provider-priority-fallback+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_THEME
;;; ----------------------------------------------------------------------------

(defconstant +gtk-style-provider-priority-theme+ 200)

#+cl-cffi-gtk-documentation
(setf (gethash '+gtk-style-provider-priority-theme+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+gtk-style-provider-priority-theme+ 'variable)
 "@version{2021-11-27}
  @variable-value{200}
  @begin{short}
    The priority used for style information provided by themes.
  @end{short}
  @see-class{gtk-style-provider}")

(export '+gtk-style-provider-priority-theme+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;; ----------------------------------------------------------------------------

(defconstant +gtk-style-provider-priority-settings+ 400)

#+cl-cffi-gtk-documentation
(setf (gethash '+gtk-style-provider-priority-settings+
               atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+gtk-style-provider-priority-settings+ 'variable)
 "@version{2021-11-27}
  @variable-value{400}
  @begin{short}
    The priority used for style information provided via a @class{gtk-settings}
    object.
  @end{short}
  This priority is higher than the @var{+gtk-style-provider-priority-theme+}
  value to let settings override themes.
  @see-class{gtk-style-provider}
  @see-class{gtk-settings}
  @see-variable{+gtk-style-provider-priority-theme+}")

(export '+gtk-style-provider-priority-settings+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;; ----------------------------------------------------------------------------

(defconstant +gtk-style-provider-priority-application+ 600)

#+cl-cffi-gtk-documentation
(setf (gethash '+gtk-style-provider-priority-application+
               atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+gtk-style-provider-priority-application+ 'variable)
 "@version{*2021-11-27}
  @variable-value{600}
  @begin{short}
    A priority that can be used when adding a @class{gtk-style-provider} object
    for application specific style information.
  @end{short}
  @see-class{gtk-style-provider}")

(export '+gtk-style-provider-priority-application+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_USER
;;; ----------------------------------------------------------------------------

(defconstant +gtk-style-provider-priority-user+ 800)

#+cl-cffi-gtk-documentation
(setf (gethash '+gtk-style-provider-priority-user+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+gtk-style-provider-priority-user+ 'variable)
 "@version{*2021-11-27}
  @variable-value{800}
  @begin{short}
    The priority used for the style information from the @file{~/.gtk-3.0.css}
    file.
  @end{short}
  You should not use priorities higher than this, to give the user the last
  word.
  @see-class{gtk-style-provider}")

(export '+gtk-style-provider-priority-user+)

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_icon_factory ()
;;;
;;; GtkIconFactory * gtk_style_provider_get_icon_factory
;;;                                                 (GtkStyleProvider *provider,
;;;                                                  GtkWidgetPath *path);
;;;
;;; Returns the GtkIconFactory defined to be in use for path, or NULL if none is
;;; defined.
;;;
;;; Warning
;;;
;;; gtk_style_provider_get_icon_factory has been deprecated since version 3.8
;;; and should not be used in newly-written code.
;;;
;;; Will always return NULL for all GTK-provided style providers.
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; path :
;;;     GtkWidgetPath to query
;;;
;;; Returns :
;;;     The icon factory to use for path, or NULL.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style ()
;;;
;;; GtkStyleProperties * gtk_style_provider_get_style
;;;                                                 (GtkStyleProvider *provider,
;;;                                                  GtkWidgetPath *path);
;;;
;;; Returns the style settings affecting a widget defined by path, or NULL if
;;; provider does not contemplate styling path.
;;;
;;; Warning
;;;
;;; gtk_style_provider_get_style has been deprecated since version 3.8 and
;;; should not be used in newly-written code.
;;;
;;; Will always return NULL for all GTK-provided style providers as the
;;; interface cannot correctly work the way CSS is specified.
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; path :
;;;     GtkWidgetPath to query
;;;
;;; Returns :
;;;     a GtkStyleProperties containing the style settings affecting path.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style_property ()
;;; -> gtk-style-provider-style-property
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_provider_get_style_property"
          %gtk-style-provider-style-property) :void
  (provider (g-object gtk-style-provider))
  (path (g-boxed-foreign gtk-widget-path))
  (state gtk-state-flags)
  (pspec (:pointer (:struct g-param-spec)))
  (value (:pointer (:struct g-value))))

(defun gtk-style-provider-style-property (provider path state pspec)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-27}
  @argument[provider]{a @class{gtk-style-provider} object}
  @argument[path]{a @symbol{gtk-widget-path} instance to query}
  @argument[state]{a @symbol{gtk-state-flags} value to query the style property
    for}
  @argument[pspec]{a @symbol{g-param-spec} instance to query}
  @return{Returns the value of the style property.}
  @begin{short}
    Looks up the value of a widget style property as defined by the provider
    for the widget represented by @arg{path}.
  @end{short}
  @see-class{gtk-style-provider}
  @see-class{gtk-widget-path}
  @see-symbol{gtk-state-flags}
  @see-symbol{g-param-spec}"
  (let ((gtype (g-param-spec-value-type pspec)))
    (with-foreign-object (value '(:struct g-value))
      (unwind-protect
        (progn
          (g-value-init value gtype)
          (%gtk-style-provider-style-property provider path state pspec value)
          (parse-g-value value))
        (g-value-unset value)))))

(export 'gtk-style-provider-style-property)

;;; --- End of file gtk-style-provider.lisp ------------------------------------
