;;; ----------------------------------------------------------------------------
;;; gtk.css-provider.lisp
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
;;; GtkCssProvider
;;;
;;;     CSS-like styling for widgets
;;;
;;; Types and Values
;;;
;;;     GtkCssProvider
;;;     GtkCssProviderError
;;;     GtkCssSection
;;;     GtkCssSectionType
;;;
;;;     GTK_CSS_PROVIDER_ERROR
;;;
;;; Functions
;;;
;;;     gtk_css_provider_get_default
;;;     gtk_css_provider_get_named
;;;     gtk_css_provider_load_from_data
;;;     gtk_css_provider_load_from_file
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_load_from_resource
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;
;;;     gtk_css_section_get_end_line
;;;     gtk_css_section_get_end_position
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_section_type
;;;     gtk_css_section_get_start_line
;;;     gtk_css_section_get_start_position
;;;
;;;     gtk_css_section_ref                                missing
;;;     gtk_css_section_unref                              missing
;;;
;;; Signals
;;;
;;;     void    parsing-error    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkCssSection
;;;
;;;     GObject
;;;     ╰── GtkCssProvider
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCssProvider implements GtkStyleProvider and GtkStyleProviderPrivate.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(define-g-boxed-opaque g-error "GError"
  :alloc (error "GError cannot be created from the Lisp side."))

(export (boxed-related-symbols 'g-error))

;;; ----------------------------------------------------------------------------
;;; struct GtkCssProvider
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCssProvider" gtk-css-provider
  (:superclass g-object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_css_provider_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-css-provider 'type)
 "@version{*2021-11-18}
  @begin{short}
    The @sym{gtk-css-provider} object is an object implementing the
    @class{gtk-style-provider} interface.
  @end{short}
  It is able to parse CSS-like input in order to style widgets.

  An application can make GTK parse a specific CSS style sheet by calling
  the @fun{gtk-css-provider-load-from-file} or
  @fun{gtk-css-provider-load-from-resource} functions and adding the provider
  with the @fun{gtk-style-context-add-provider} or
  @fun{gtk-style-context-add-provider-for-screen} functions.

  In addition, certain files will be read when GTK is initialized. First, the
  file @file{$XDG_CONFIG_HOME/gtk-3.0/gtk.css} is loaded if it exists. Then,
  GTK loads the first existing file among
  @file{XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk.css},
  @file{$HOME/.themes/THEME/gtk-VERSION/gtk.css},
  @file{$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk.css} and
  @file{DATADIR/share/themes/THEME/gtk-VERSION/gtk.css}, where  @file{THEME} is
  the name of the current theme, see the @slot[gtk-settings]{gtk-theme-name}
  setting, @file{DATADIR} is the prefix configured when GTK was compiled
  (unless overridden by the @code{GTK_DATA_PREFIX} environment variable), and
  @file{VERSION} is the GTK version number. If no file is found for the
  current version, GTK tries older versions all the way back to 3.0.

  In the same way, GTK tries to load a @file{gtk-keys.css} file for the
  current key theme, as defined by the @slot[gtk-settings]{gtk-key-theme-name}
  setting.
  @begin[Signal Details]{dictionary}
    @subheading{The \"parsing-error\" signal}
      @begin{pre}
 lambda (provider section error)    :run-last
      @end{pre}
      Signals that a parsing error occured. The path, line and position of the
      @symbol{gtk-css-section} instance describe the actual location of the
      error as accurately as possible.

      Parsing errors are never fatal, so the parsing will resume after the
      error. Errors may however cause parts of the given data or even all of it
      to not be parsed at all. So it is a useful idea to check that the parsing
      succeeds by connecting to this signal.

      Note that this signal may be emitted at any time as the CSS provider may
      opt to defer parsing parts or all of the input to a later time than when
      a loading function was called.
      @begin[code]{table}
        @entry[provider]{The @sym{gtk-css-provider} object that had a parsing
          error.}
        @entry[section]{The @class{gtk-css-section} instance the error happened
          in.}
        @entry[error]{The parsing error of type @code{GError}.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-style-provider}
  @see-class{gtk-css-section}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_default () -> gtk-css-provider-default
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_default" gtk-css-provider-default)
    (g-object gtk-css-provider)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @return{The @class{gtk-css-provider} object used for fallback styling.}
  @begin{short}
    Returns the provider containing the style settings used as a fallback for
    all widgets.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-css-provider-default} function has been deprecated since
    version 3.24 and should not be used in newly written code. Use the
    @fun{gtk-css-provider-new} function instead.
  @end{dictionary}
  @see-class{gtk-css-provider}
  @see-function{gtk-css-provider-new}")

(export 'gtk-css-provider-default)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_named () -> gtk-css-provider-named
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_named" %gtk-css-provider-named)
    (g-object gtk-css-provider)
  (name :string)
  (variant :string))

(defun gtk-css-provider-named (name variant)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[name]{a string with the theme name}
  @argument[variant]{a string with a variant to load}
  @return{The @class{gtk-css-provider} object with the theme loaded.}
  @begin{short}
    Loads a theme from the usual theme paths, for example, \"dark\", or
    @code{nil} for the default theme.
  @end{short}
  @see-class{gtk-css-provider}"
  (%gtk-css-provider-named name
                           (if variant variant (null-pointer))))

(export 'gtk-css-provider-named)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_data" %gtk-css-provider-load-from-data)
    :boolean
  (provider (g-object gtk-css-provider))
  (data :string)
  (length :long)
  (err :pointer))

(defun gtk-css-provider-load-from-data (provider data)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[provider]{a @class{gtk-css-provider} object}
  @argument[data]{a string with the CSS data}
  @begin{return}
    @em{True}. The return value is deprecated and @em{false} will only be
    returned for backwards compatibility reasons if an error occured.
  @end{return}
  @begin{short}
    Loads data into the CSS provider, making it clear any previously loaded
    information.
  @end{short}
  To track errors while loading CSS, connect to the \"parsing-error\" signal
  of the @class{gtk-css-provider} object.
  @see-class{gtk-css-provider}"
  (with-ignore-g-error (err)
    (%gtk-css-provider-load-from-data provider data -1 err)))

(export 'gtk-css-provider-load-from-data)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_file" %gtk-css-provider-load-from-file)
    :boolean
  (provider (g-object gtk-css-provider))
  (file (g-object g-file))
  (err :pointer))

(defun gtk-css-provider-load-from-file (provider file)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[provider]{a @class{gtk-css-provider} object}
  @argument[file]{a @class{g-file} object pointing to a file to load}
  @return{@em{True}. The return value is deprecated and @em{false} will only
    be returned for backwards compatibility reasons if an error occured.}
  @begin{short}
    Loads the data contained in @arg{file} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the \"parsing-error\" signal
  of the @class{gtk-css-provider} object.
  @see-class{gtk-css-provider}
  @see-class{g-file}"
  (with-g-error (err)
    (%gtk-css-provider-load-from-file provider file err)))

(export 'gtk-css-provider-load-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_path" %gtk-css-provider-load-from-path)
    :boolean
  (provider (g-object gtk-css-provider))
  (path :string)
  (err :pointer))

(defun gtk-css-provider-load-from-path (provider path)
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[provider]{a @class{gtk-css-provider} object}
  @argument[path]{a string with the path of a filename to load, in the GLib
   filename encoding}
  @begin{return}
    @em{True}. The return value is deprecated and @em{false} will only be
    returned for backwards compatibility reasons if an error occured.
  @end{return}
  @begin{short}
    Loads the data contained in @arg{path} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the \"parsing-error\" signal
  of the @class{gtk-css-provider} object.
  @see-class{gtk-css-provider}"
  (with-g-error (err)
    (%gtk-css-provider-load-from-path provider path err)))

(export 'gtk-css-provider-load-from-path)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_resource"
           gtk-css-provider-load-from-resource) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[provider]{a @class{gtk-css-provider} object}
  @argument[path]{a string with the resource path}
  @begin{short}
    Loads the data contained in the resource at @arg{path} into the CSS
    provider, clearing any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the \"parsing-error\" signal
  of the @class{gtk-css-provider} object.
  @see-class{gtk-css-provider}
  @see-class{g-resource}"
  (provider (g-object gtk-css-provider))
  (path :string))

(export 'gtk-css-provider-load-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-css-provider-new))

(defun gtk-css-provider-new ()
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @return{A new @class{gtk-css-provider} object.}
  @short{Returns a newly created CSS provider object.}
  @see-class{gtk-css-provider}"
  (make-instance 'gtk-css-provider))

(export 'gtk-css-provider-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_to_string" gtk-css-provider-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[provider]{a @class{gtk-css-provider} object to write to a string}
  @return{A string representing the provider.}
  @begin{short}
    Convertes the provider into a string representation in CSS format.
  @end{short}
  Using the @fun{gtk-css-provider-load-from-data} function with the return
  value from this function on a new provider created with the
  @fun{gtk-css-provider-new} function will basically create a duplicate of this
  provider.
  @see-class{gtk-css-provider}
  @see-function{gtk-css-provider-new}
  @see-function{gtk-css-provider-load-from-data}"
  (provider (g-object gtk-css-provider)))

(export 'gtk-css-provider-to-string)

;;; ----------------------------------------------------------------------------
;;; GTK_CSS_PROVIDER_ERROR
;;;
;;; #define GTK_CSS_PROVIDER_ERROR (gtk_css_provider_error_quark ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssProviderError
;;;
;;; typedef enum {
;;;   GTK_CSS_PROVIDER_ERROR_FAILED,
;;;   GTK_CSS_PROVIDER_ERROR_SYNTAX,
;;;   GTK_CSS_PROVIDER_ERROR_IMPORT,
;;;   GTK_CSS_PROVIDER_ERROR_NAME,
;;;   GTK_CSS_PROVIDER_ERROR_DEPRECATED,
;;;   GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;; } GtkCssProviderError;
;;;
;;; GTK_CSS_PROVIDER_ERROR_FAILED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_SYNTAX
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_IMPORT
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_NAME
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_DEPRECATED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;;-----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssSectionType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCssSectionType" gtk-css-section-type
  (:export t
   :type-initializer "gtk_css_section_type_get_type")
  (:document 0)
  (:import 1)
  (:color-definition 2)
  (:binding-set 3)
  (:ruleset 4)
  (:selector 5)
  (:declaration 6)
  (:value 7)
  (:keyframes 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-css-section-type atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'gtk-css-section-type atdoc:*external-symbols*)
 "@version{2021-11-18}
  @begin{short}
    The different types of sections indicate parts of a CSS document as parsed
    by the CSS parser of GTK.
  @end{short}
  They are oriented towards the CSS grammar, but may contain extensions.

  More types might be added in the future as the parser incorporates more
  features.
  @begin{pre}
(define-g-enum \"GtkCssSectionType\" gtk-css-section-type
  (:export t
   :type-initializer \"gtk_css_section_type_get_type\")
  (:document 0)
  (:import 1)
  (:color-definition 2)
  (:binding-set 3)
  (:ruleset 4)
  (:selector 5)
  (:declaration 6)
  (:value 7)
  (:keyframes 8))
  @end{pre}
  @begin[code]{table}
    @entry[:document]{The section describes a complete document. This section
      is the only one where the @fun{gtk-css-section-parent} function might
      return @code{nil}.}
    @entry[:import]{The section defines an import rule.}
    @entry[:color-definition]{The section defines a color. This is a GTK
      extension to CSS.}
    @entry[:binding-set]{The section defines a binding set. This is a GTK
      extension to CSS.}
    @entry[:ruleset]{The section defines a CSS ruleset.}
    @entry[:selector]{The section defines a CSS selector.}
    @entry[:declaration]{The section defines the declaration of a CSS variable.}
    @entry[:value]{The section defines the value of a CSS declaration.}
    @entry[:keyframes]{The section defines keyframes. See CSS animations for
      details.}
  @end{table}
  @see-class{gtk-css-provider}")

;;; ----------------------------------------------------------------------------
;;; GtkCssSection
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_css_section_get_type" g-size))

(define-g-boxed-opaque gtk-css-section "GtkCssSection"
  :alloc (error "GtkCssSection cannot be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-css-section atdoc:*class-name-alias*)
      "CStruct"
      (documentation 'gtk-css-section 'type)
 "@version{2021-11-18}
  @begin{short}
    Defines a part of a CSS document.
  @end{short}
  Because sections are nested into one another, you can use the
  @fun{gtk-css-section-parent} function to get the containing region.
  @begin{pre}
(define-g-boxed-opaque gtk-css-section \"GtkCssSection\"
  :alloc (error \"GtkCssSection cannot be created from the Lisp side.\"))
  @end{pre}
  @see-class{gtk-css-provider}
  @see-function{gtk-css-section-parent}")

(export 'gtk-css-section)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_line () -> gtk-css-section-end-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_end_line" gtk-css-section-end-line) :uint
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{An unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section end.
  @end{short}
  The line number is zero-indexed, so the first line of the document will
  return 0. This value may change in future invocations of this function if the
  section is not yet parsed completely. This will for example happen in the
  \"parsing-error\" signal. The end position and line may be identical to the
  start position and line for sections which failed to parse anything
  successfully.
  @see-class{gtk-css-section}
  @see-class{gtk-css-provider}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-end-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_position () -> gtk-css-section-end-position
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_end_position" gtk-css-section-end-position) :uint
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{An unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned via
    the @fun{gtk-css-section-end-line} function.
  @end{short}
  This value may change in future invocations of this function if the section
  is not yet parsed completely. This will for example happen in the
  \"parsing-error\" signal. The end position and line may be identical to the
  start position and line for sections which failed to parse anything
  successfully.
  @see-class{gtk-css-section}
  @see-class{gtk-css-provider}
  @see-function{gtk-css-section-end-line}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-end-position)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file () -> gtk-css-section-file
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_file" gtk-css-section-file) (g-object g-file)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{The @class{g-file} object that section was parsed from or @code{nil}
    if section was parsed from other data.}
  @begin{short}
    Gets the file that the section was parsed from.
  @end{short}
  If no such file exists, for example because the CSS was loaded via the
  @fun{gtk-css-provider-load-from-data} function, then @code{nil} is returned.
  @see-class{gtk-css-section}
  @see-class{gtk-css-provider}
  @see-class{g-file}
  @see-function{gtk-css-provider-load-from-data}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent () -> gtk-css-section-parent
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_parent" gtk-css-section-parent)
    (g-boxed-foreign gtk-css-section)
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{The @symbol{gtk-css-section} parent section, or @code{nil} if none.}
  @begin{short}
    Gets the parent section for the given section.
  @end{short}
  The parent section is the section that contains this section. A special case
  are sections of @code{:document} type. Their parent will either be @code{nil}
  if they are the original CSS document that was loaded by the
  @fun{gtk-css-provider-load-from-file} function or a section of @code{:section}
  type if it was loaded with an import rule from a different file.
  @see-class{gtk-css-section}
  @see-symbol{gtk-css-section-type}
  @see-function{gtk-css-provider-load-from-file}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_section_type () -> gtk-css-section-section-type
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_section_type" gtk-css-section-section-type)
    gtk-css-section-type
 #+cl-cffi-gtk-documentation
 "@version{2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{A @symbol{gtk-css-section-type} value for the section.}
  @begin{short}
    Gets the type of information that the section describes.
  @end{short}
  @see-class{gtk-css-section}
  @see-symbol{gtk-css-section-type}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-section-type)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_line () -> gtk-css-section-start-line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_start_line" gtk-css-section-start-line) :uint
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{An unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section starts.
  @end{short}
  The line number is zero-indexed, so the first line of the document will
  return 0.
  @see-class{gtk-css-section}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-start-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_position () -> gtk-css-section-start-position
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_start_position"
           gtk-css-section-start-position) :uint
 #+cl-cffi-gtk-documentation
 "@version{*2021-11-18}
  @argument[section]{a @class{gtk-css-section} instance}
  @return{An unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned
    via the @fun{gtk-css-section-start-line} function.
  @end{short}
  @see-class{gtk-css-section}
  @see-function{gtk-css-section-start-line}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-start-position)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_ref ()
;;;
;;; GtkCssSection * gtk_css_section_ref (GtkCssSection *section);
;;;
;;; Increments the reference count on section.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Returns :
;;;     section itself.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_unref ()
;;;
;;; void gtk_css_section_unref (GtkCssSection *section);
;;;
;;; Decrements the reference count on section, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.css-provider.lisp --------------------------------------
