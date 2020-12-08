;;; ----------------------------------------------------------------------------
;;; gtk.css-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK+ Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;;     gtk_css_provider_load_from_file                    missing
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_load_from_resource                missing
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;
;;;     gtk_css_section_get_end_line
;;;     gtk_css_section_get_end_position
;;;     gtk_css_section_get_file                           missing
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_section_type
;;;     gtk_css_section_get_start_line
;;;     gtk_css_section_get_start_position
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
 "@version{2020-3-7}
  @begin{short}
    @sym{gtk-css-provider} is an object implementing the
    @class{gtk-style-provider} interface.
  @end{short}
  It is able to parse CSS-like input in order to style widgets.

  An application can make GTK+ parse a specific CSS style sheet by calling the
  functions @fun{gtk-css-provider-load-from-file} or
  @fun{gtk-css-provider-load-from-resource} and adding the provider with
  the functions @fun{gtk-style-context-add-provider} or
  @fun{gtk-style-context-add-provider-for-screen}.

  In addition, certain files will be read when GTK+ is initialized. First, the
  file @file{$XDG_CONFIG_HOME/gtk-3.0/gtk.css} is loaded if it exists. Then,
  GTK+ loads the first existing file among
  @file{XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk.css},
  @file{$HOME/.themes/THEME/gtk-VERSION/gtk.css},
  @file{$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk.css} and
  @file{DATADIR/share/themes/THEME/gtk-VERSION/gtk.css}, where  @file{THEME} is
  the name of the current theme (see the @slot[gtk-settings]{gtk-theme-name}
  setting), @file{DATADIR} is the prefix configured when GTK+ was compiled
  (unless overridden by the @code{GTK_DATA_PREFIX} environment variable), and
  @file{VERSION} is the GTK+ version number. If no file is found for the current
  version, GTK+ tries older versions all the way back to 3.0.

  In the same way, GTK+ tries to load a @file{gtk-keys.css} file for the current
  key theme, as defined by @slot[gtk-settings]{gtk-key-theme-name} property.

  @begin[Signal Details]{dictionary}
    @subheading{The \"parsing-error\" signal}
      @begin{pre}
 lambda (provider section error)    : Run Last
      @end{pre}
      Signals that a parsing error occured. The path, line and position describe
      the actual location of the error as accurately as possible.

      Parsing errors are never fatal, so the parsing will resume after the
      error. Errors may however cause parts of the given data or even all of it
      to not be parsed at all. So it is a useful idea to check that the parsing
      succeeds by connecting to this signal.

      Note that this signal may be emitted at any time as the css provider may
      opt to defer parsing parts or all of the input to a later time than when
      a loading function was called.
      @begin[code]{table}
        @entry[provider]{The @sym{gtk-css-provider} object that had a parsing
          error.}
        @entry[section]{The @class{gtk-css-section} object the error happened
          in.}
        @entry[error]{The parsing error of type @code{GError}.}
      @end{table}
  @end{dictionary}
  @see-function{gtk-theming-engine-register-property}
  @see-function{gtk-widget-class-install-style-property}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_default" gtk-css-provider-default)
    (g-object gtk-css-provider)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-18}
  @return{The @class{gtk-css-provider} object used for fallback styling.}
  @begin{short}
    Returns the provider containing the style settings used as a fallback for
    all widgets.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-css-provider-default} has been deprecated since
    version 3.24 and should not be used in newly-written code. Use the
    @fun{gtk-css-provider-new} function instead.
  @end{dictionary}
  @see-class{gtk-css-provider}
  @see-function{gtk-css-provider-new}")

(export 'gtk-css-provider-default)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_named ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_get_named" gtk-css-provider-get-named)
    (g-object gtk-css-provider)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @argument[name]{a string with the theme name}
  @argument[variant]{a string with a variant to load}
  @return{The @class{gtk-css-provider} with the theme loaded.}
  @begin{short}
    Loads a theme from the usual theme paths, for example, \"dark\", or
    @code{nil} for the default theme.
  @end{short}
  @see-class{gtk-css-provider}"
  (name :string)
  (variant :string))

(export 'gtk-css-provider-get-named)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_data" %gtk-css-provider-load-from-data)
    :boolean
  (provider (g-object gtk-css-provider))
  (data :string)
  (length :long)
  (error :pointer))

(defun gtk-css-provider-load-from-data (provider data)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-26}
  @argument[provider]{a @class{gtk-css-provider} object}
  @argument[data]{a string with the CSS data}
  @begin{return}
    @emd{True}. The return value is deprecated and @em{false} will only be
    returned for backwards compatibility reasons if an error is not @code{NULL}
    and a loading error occured. To track errors while loading CSS, connect to
    the \"parsing-error\" signal of @class{gtk-css-provider}.
  @end{return}
  @begin{short}
    Loads data into the CSS provider, making it clear any previously loaded
    information.
  @end{short}
  @see-class{gtk-css-provider}"
  (with-ignore-g-error (err)
    (%gtk-css-provider-load-from-data provider data -1 err)))

(export 'gtk-css-provider-load-from-data)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_file ()
;;;
;;; gboolean gtk_css_provider_load_from_file (GtkCssProvider *css_provider,
;;;                                           GFile *file,
;;;                                           GError **error);
;;;
;;; Loads the data contained in file into css_provider, making it clear any
;;; previously loaded information.
;;;
;;; css_provider :
;;;     a GtkCssProvider
;;;
;;; file :
;;;     GFile pointing to a file to load
;;;
;;; error :
;;;     return location for a GError, or NULL
;;;
;;; Returns :
;;;     TRUE. The return value is deprecated and FALSE will only be returned for
;;;     backwards compatibility reasons if an error is not NULL and a loading
;;;     error occured. To track errors while loading CSS, connect to the
;;;     GtkCssProvider::parsing-error signal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_load_from_path" %gtk-css-provider-load-from-path)
    :boolean
  (css-provider (g-object gtk-css-provider))
  (path :string)
  (error :pointer))

(defun gtk-css-provider-load-from-path (css-provider path)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-28}
  @argument[css-provider]{a @class{gtk-css-provider} object}
  @argument[path]{the path of a filename to load, in the GLib filename encoding}
  @begin{return}
    @em{True}. The return value is deprecated and @code{nil} will only be
    returned for backwards compatibility reasons if an error is not @code{nil}
    and a loading error occured. To track errors while loading CSS, connect to
    the \"parsing-error\" signal.
  @end{return}
  Loads the data contained in @arg{path} into @arg{css-provider}, making it
  clear any previously loaded information.
  @see-class{gtk-css-provider}"
  (with-g-error (err)
    (%gtk-css-provider-load-from-path css-provider path err)))

(export 'gtk-css-provider-load-from-path)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_resource ()
;;;
;;; void
;;; gtk_css_provider_load_from_resource (GtkCssProvider *css_provider,
;;;                                      const gchar *resource_path);
;;;
;;; Loads the data contained in the resource at resource_path into the
;;; GtkCssProvider, clearing any previously loaded information.
;;;
;;; To track errors while loading CSS, connect to the “parsing-error” signal.
;;;
;;;css_provider :
;;;     a GtkCssProvider
;;;
;;; resource_path :
;;;     a GResource resource path
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-css-provider-new))

(defun gtk-css-provider-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-3-2}
  @return{A new @class{gtk-css-provider} object.}
  Returns a newly created provider object.
  @see-class{gtk-css-provider}"
  (make-instance 'gtk-css-provider))

(export 'gtk-css-provider-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_provider_to_string" gtk-css-provider-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-8-20}
  @argument[provider]{the provider to write to a string}
  @return{A string representing the provider.}
  @begin{short}
    Convertes the provider into a string representation in CSS format.
  @end{short}

  Using the @fun{gtk-css-provider-load-from-data} function with the return value
  from this function on a new provider created with the
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
(setf (gethash 'gtk-css-section-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-css-section-type atdoc:*external-symbols*)
 "@version{2020-2-29}
  @begin{short}
    The different types of sections indicate parts of a CSS document as parsed
    by GTK's CSS parser.
  @end{short}
  They are oriented towards the CSS grammar CSS grammer, but may contain
  extensions.

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
      time is the only one where the function @fun{gtk-css-section-get-parent}
      might return @code{nil}.}
    @entry[:import]{The section defines an import rule.}
    @entry[:color-definition]{The section defines a color. This is a GTK
      extension to CSS.}
    @entry[:binding-set]{The section defines a binding set. This is a GTK
      extension to CSS.}
    @entry[:ruleset]{The section defines a CSS ruleset.}
    @entry{:selector]{The section defines a CSS selector.}
    @entry[:declaration]{The section defines the declaration of a CSS variable.}
    @entry[:value]{The section defines the value of a CSS declaration.}
    @entry[:keyframes]{The section defines keyframes. See CSS animations for
      details. Since 3.6.}
  @end{table}
  @see-class{gtk-css-provider}")

;;; ----------------------------------------------------------------------------
;;; GtkCssSection
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_css_section_get_type" g-size))

(define-g-boxed-opaque gtk-css-section "GtkCssSection"
  :alloc (error "GtkCssSection cannot be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-css-section atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-css-section 'type)
 "@version{2020-2-29}
  @begin{short}
    Defines a part of a CSS document.
  @end{short}
  Because sections are nested into one another, you can use the function
  @fun{gtk-css-section-get-parent} to get the containing region.
  @begin{pre}
(define-g-boxed-opaque gtk-css-section \"GtkCssSection\"
  :alloc (error \"GtkCssSection cannot be created from the Lisp side.\"))
  @end{pre}
  @see-function{gtk-css-section-get-parent}")

(export (boxed-related-symbols 'gtk-css-section))

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_end_line" gtk-css-section-get-end-line) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-3-6}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{An unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section end.
  @end{short}
  The line number is 0-indexed, so the first line of the document will return 0.
  This value may change in future invocations of this function if section is not
  yet parsed completely. This will for example happen in the \"parsing-error\"
  signal. The end position and line may be identical to the start position and
  line for sections which failed to parse anything successfully.
  @see-class{gtk-css-section}
  @see-class{gtk-css-provider}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-end-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_end_position" gtk-css-section-get-end-position)
    :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{An unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned via
    the function @fun{gtk-css-section-get-end-line}.
  @end{short}
  This value may change in future invocations of this function if the section
  is not yet parsed completely. This will for example happen in the
  \"parsing-error\" signal. The end position and line may be identical to the
  start position and line for sections which failed to parse anything
  successfully.
  @see-class{gtk-css-section}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-end-position)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file ()
;;;
;;; GFile * gtk_css_section_get_file (const GtkCssSection *section);
;;;
;;; Gets the file that section was parsed from. If no such file exists, for
;;; example because the CSS was loaded via gtk_css_provider_load_from_data(),
;;; then NULL is returned.
;;;
;;; section :
;;;     the section
;;;
;;; Returns :
;;;     The GFile that section was parsed from or NULL if section was parsed
;;;     from other data.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_parent" gtk-css-section-get-parent)
    (g-boxed-foreign gtk-css-section)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{The parent section of type @symbol{gtk-css-section} of @code{nil}
    if none.}
  @begin{short}
    Gets the parent section for the given section.
  @end{short}
  The parent section is the section that contains this section. A special case
  are sections of type @code{:document} of type @symbol{gtk-css-section-type}.
  Their parent will either be @code{nil} if they are the original CSS document
  that was loaded by the function @fun{gtk-css-provider-load-from-file} or a
  section of type @code{:section} of type @symbol{gtk-css-section-type} if it
  was loaded with an import rule from a different file.
  @see-class{gtk-css-section}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_section_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_section_type" gtk-css-section-get-section-type)
    gtk-css-section-type
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{The type of sectin of type @symbol{gtk-css-section-type}.}
  @begin{short}
    Gets the type of information that section describes.
  @end{short}
  @see-class{gtk-css-section}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-section-type)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_start_line" gtk-css-section-get-start-line) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{An unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section starts.
  @end{short}
  The line number is 0-indexed, so the first line of the document will return 0.
  @see-class{gtk-css-section}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-start-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_css_section_get_start_position"
           gtk-css-section-get-start-position) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-3-7}
  @argument[section]{a @class{gtk-css-section} structure}
  @return{An unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned via
    the function @fun{gtk-css-section-get-start-line}.
  @end{short}
  @see-class{gtk-css-section}
  @see-function{gtk-css-section-get-start-line}"
  (section (g-boxed-foreign gtk-css-section)))

(export 'gtk-css-section-get-start-position)

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
