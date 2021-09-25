;;; ----------------------------------------------------------------------------
;;; gtk.builder.lisp
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkBuilder
;;;
;;;     Build an interface from an XML UI definition
;;;
;;; Types and Values
;;;
;;;     GtkBuilder
;;;     GtkBuilderError
;;;
;;; Functions
;;;
;;;     GtkBuilderConnectFunc
;;;
;;;     gtk_builder_new
;;;     gtk_builder_new_from_file
;;;     gtk_builder_new_from_resource
;;;     gtk_builder_new_from_string
;;;     gtk_builder_add_callback_symbol
;;;     gtk_builder_add_callback_symbols
;;;     gtk_builder_lookup_callback_symbol
;;;     gtk_builder_add_from_file
;;;     gtk_builder_add_from_resource
;;;     gtk_builder_add_from_string
;;;     gtk_builder_add_objects_from_file
;;;     gtk_builder_add_objects_from_string
;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_extend_with_template
;;;     gtk_builder_get_object
;;;     gtk_builder_get_objects
;;;     gtk_builder_expose_object
;;;     gtk_builder_connect_signals
;;;     gtk_builder_connect_signals_full
;;;     gtk_builder_set_translation_domain                 Accessor
;;;     gtk_builder_get_translation_domain                 Accessor
;;;     gtk_builder_set_application
;;;     gtk_builder_get_application
;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type
;;;
;;;     GTK_BUILDER_WARN_INVALID_CHILD_TYPE
;;;
;;; Properties
;;;
;;;     gchar*   translation-domain    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkBuilder
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkBuilderError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkBuilderError" gtk-builder-error
  (:export t
   :type-initializer "gtk_builder_error_get_type")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8)
  (:type-refused 9)
  (:template-mismatch 10)
  (:invalid-property 11)
  (:invalid-signal 12)
  (:invalid-id 13))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-error atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-builder-error atdoc:*external-symbols*)
 "@version{2021-9-23}
  @begin{short}
    Error codes that identify various errors that can occur while parsing the
    @class{gtk-builder} UI definition.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkBuilderError\" gtk-builder-error
  (:export t
   :type-initializer \"gtk_builder_error_get_type\")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8)
  (:type-refused 9)
  (:template-mismatch 10)
  (:invalid-property 11)
  (:invalid-signal 12)
  (:invalid-id 13))
  @end{pre}
  @begin[code]{table}
    @entry[:invalid-type-function]{A @code{type-func} attribute did not name a
      function that returns a @class{g-type} type ID.}
    @entry[:unhandled-tag]{The input contained a tag that a @class{gtk-builder}
      object cannot handle.}
    @entry[:missing-attribute]{An attribute that is required by a
      @class{gtk-builder} object was missing.}
    @entry[:invalid-attribute]{A @class{gtk-builder} object found an attribute
      that it does not understand.}
    @entry[:invalid-tag]{A @class{gtk-builder} object found a tag that it does
      not understand.}
    @entry[:missing-property-value]{A required property value was missing.}
    @entry[:invalid-value]{A @class{gtk-builder} object could not parse some
      attribute value.}
    @entry[:version-mismatch]{The input file requires a newer version of GTK.}
    @entry[:duplicate-id]{An object ID occurred twice.}
    @entry[:type-refused]{A specified object type is of the same type or derived
      from the type of the composite class being extended with builder XML.}
    @entry[:template-mismatch]{The wrong type was specified in a composite
      class’s template XML.}
    @entry[:invalid-property]{The specified property is unknown for the object
      class.}
    @entry[:invalid-signal]{The specified signal is unknown for the object
      class.}
    @entry[:invalid-id]{An object ID is unknown.}
  @end{table}
  @see-class{gtk-builder}")

;;; ----------------------------------------------------------------------------
;;; struct GtkBuilder
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkBuilder" gtk-builder
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_builder_get_type")
  ((translation-domain
    gtk-builder-translation-domain
    "translation-domain" "gchararray" t t)))

;;; This Lisp extension is not documented
(defmethod initialize-instance :after ((builder gtk-builder)
                                       &key from-file from-string)
  (when from-file
    (gtk-builder-add-from-file builder from-file))
  (when from-string
    (gtk-builder-add-from-string builder from-string)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-builder 'type)
 "@version{2021-9-3}
  @begin{short}
    A @sym{gtk-builder} object is an auxiliary object that reads textual
    descriptions of a user interface and instantiates the described objects.
  @end{short}
  To create a @sym{gtk-builder} object from a user interface description,
  call the @fun{gtk-builder-new-from-file}, @fun{gtk-builder-new-from-resource}
  or @fun{gtk-builder-new-from-string} functions.

  In the (unusual) case that you want to add user interface descriptions from
  multiple sources to the same @sym{gtk-builder} object you can call the
  @fun{gtk-builder-new} function to get an empty builder and populate it by
  (multiple) calls to the @fun{gtk-builder-add-from-file},
  @fun{gtk-builder-add-from-resource} or @fun{gtk-builder-add-from-string}
  functions.

  A @sym{gtk-builder} object holds a reference to all objects that it has
  constructed and drops these references when it is finalized. This finalization
  can cause the destruction of non-widget objects or widgets which are not
  contained in a toplevel window. For toplevel windows constructed by a builder,
  it is the responsibility of the user to call the @fun{gtk-widget-destroy}
  function to get rid of them and all the widgets they contain.

  The @fun{gtk-builder-object} and @fun{gtk-builder-objects} functions can be
  used to access the widgets in the interface by the names assigned to them
  inside the UI description. Toplevel windows returned by these functions will
  stay around until the user explicitly destroys them with the
  @fun{gtk-widget-destroy} function. Other widgets will either be part of a
  larger hierarchy constructed by the builder, in which case you should not have
  to worry about their life cycle, or without a parent, in which case they have
  to be added to some container to make use of them.

  The @fun{gtk-builder-connect-signals} function and variants thereof can be
  used to connect handlers to the named signals in the UI description.

  @subheading{GtkBuilder UI Definitions}
  The @sym{gtk-builder} implementation parses textual descriptions of user
  interfaces which are specified in an XML format which can be roughly described
  by the RELAX NG schema below. We refer to these descriptions as
  @sym{gtk-builder} UI definitions or just UI definitions if the context is
  clear. Do not confuse @sym{gtk-builder} UI Definitions with the deprecated
  @class{gtk-ui-manager} UI Definitions, which are more limited in scope. It is
  common to use @code{.ui} as the filename extension for files containing
  @sym{gtk-builder} UI definitions.
  @begin{pre}
 start = element interface {
   attribute domain { text @} ?,
   ( requires | object | menu ) *
 @}

 requires = element requires {
   attribute lib { text @},
   attribute version { text @}
 @}

 object = element object {
   attribute id { xsd:ID @},
   attribute class { text @},
   attribute type-func { text @} ?,
   attribute constructor { text @} ?,
   (property | signal | child | ANY) *
 @}

 template = element template {
   attribute class { text @},
   attribute parent { text @},
   (property | signal | child | ANY) *
 @}

 property = element property {
   attribute name { text @},
   attribute translatable { \"yes\" | \"no\" @} ?,
   attribute comments { text @} ?,
   attribute context { text @} ?,
   text ?
 @}

 signal = element signal {
   attribute name { text @},
   attribute handler { text @},
   attribute after { text @} ?,
   attribute swapped { text @} ?,
   attribute object { text @} ?,
   attribute last_modification_time { text @} ?,
   empty
 @}

 child = element child {
   attribute type { text @} ?,
   attribute internal-child { text @} ?,
   (object | ANY)*
 @}

 menu = element menu {
   attribute id { xsd:ID @},
   attribute domain { text @} ?,
   (item | submenu | section) *
 @}

 item = element item {
   attribute id { xsd:ID @} ?,
   (attribute_ | link) *
 @}

 attribute_ = element attribute {
   attribute name { text @},
   attribute type { text @} ?,
   attribute translatable { \"yes\" | \"no\" @} ?,
   attribute context { text @} ?,
   attribute comments { text @} ?,
   text ?
 @}

 link = element link {
   attribute id { xsd:ID @} ?,
   attribute name { text @},
   item *
 @}

 submenu = element submenu {
   attribute id { xsd:ID @} ?,
   (attribute_ | item | submenu | section) *
 @}

 section = element section {
   attribute id { xsd:ID @} ?,
   (attribute_ | item | submenu | section) *
 @}

 ANY = element * - (interface | requires | object | property | signal
                              | child | menu | item | attribute | link
                              | submenu | section) {
   attribute * { text @} *,
   (ALL * & text ?)
 @}
 ALL = element * {
   attribute * { text @} *,
   (ALL * & text ?)
 @}
  @end{pre}
  The toplevel element is @code{<interface>}. It optionally takes a
  @code{\"domain\"} attribute, which will make the builder look for translated
  strings using GNU gettext in the domain specified. This can also be done by
  calling the @fun{gtk-builder-translation-domain} function on the builder.
  Objects are described by @code{<object>} elements, which can contain
  @code{<property>} elements to set properties, @code{<signal>} elements which
  connect signals to handlers, and @code{<child>} elements, which describe child
  objects, most often widgets inside a container, but also e.g. actions in an
  action group, or columns in a tree model. A @code{<child>} element contains an
  @code{<object>} element which describes the child object. The target toolkit
  version(s) are described by @code{<requires>} elements, the @code{\"lib\"}
  attribute specifies the widget library in question, (currently the only
  supported value is @code{\"gtk+\"} and the @code{\"version\"} attribute
  specifies the target version in the form @code{\"<major>.<minor>\"}. The
  builder will error out if the version requirements are not met.

  Typically, the specific kind of object represented by an @code{<object>}
  element is specified by the @code{\"class\"} attribute. If the type has not
  been loaded yet, GTK tries to find the @code{_get_type()} from the class
  name by applying heuristics. This works in most cases, but if necessary, it
  is possible to specify the name of the @code{_get_type()} explictly with the
  @code{\"type-func\"} attribute. As a special case, the @sym{gtk-builder}
  implementation allows to use an object that has been constructed by a
  @class{gtk-ui-manager} object in another part of the UI definition by
  specifying the ID of the @class{gtk-ui-manager} object in the
  @code{\"constructor\"} attribute and the name of the object in the
  @code{\"id\"} attribute.

  Objects must be given a name with the @code{\"ID\"} attribute, which allows
  the application to retrieve them from the builder with the
  @fun{gtk-builder-object} function. An ID is also necessary to use the object
  as property value in other parts of the UI definition.

  @subheading{Note}
  Prior to 2.20, the @sym{gtk-builder} implementation was setting the
  @code{\"name\"} property of constructed widgets to the @code{\"id\"}
  attribute. In GTK 2.20 or newer, you have to use the @fun{gtk-buildable-name}
  function instead of the @fun{gtk-widget-name} function to obtain the
  @code{\"id\"}, or set the @code{\"name\"} property in your UI definition.

  Setting properties of objects is pretty straightforward with the
  @code{<property>} element: the @code{\"name\"} attribute specifies the name
  of the property, and the content of the element specifies the value. If the
  @code{\"translatable\"} attribute is set to a true value, GTK uses GNU
  gettext to find a translation for the value. This happens before the value is
  parsed, so it can be used for properties of any type, but it is probably most
  useful for string properties. It is also possible to specify a context to
  disambiguate short strings, and comments which may help the translators.

  The @sym{gtk-builder} implementation can parse textual representations for
  the most common property types: characters, strings, integers, floating point
  numbers, booleans, strings like \"TRUE\", \"t\", \"yes\", \"y\", \"1\" are
  interpreted as @em{true}, strings like \"FALSE\", \"f\", \"no\", \"n\", \"0\"
  are interpreted as @em{false}), enumerations, can be specified by their name,
  nick or integer value, flags, can be specified by their name, nick, integer
  value, optionally combined with \"|\", e.g. \"GTK_VISIBLE | GTK_REALIZED\",
  and colors, in a format understood by the @fun{gdk-rgba-parse} function.
  Objects can be referred to by their name. Pixbufs can be specified as a
  filename of an image file to load. In general, the @sym{gtk-builder}
  implementation allows forward references to objects - an object does not have
  to be constructed before it can be referred to. The exception to this rule is
  that an object has to be constructed before it can be used as the value of a
  construct-only property.

  Signal handlers are set up with the @code{<signal>} element. The
  @code{\"name\"} attribute specifies the name of the signal, and the
  @code{\"handler\"} attribute specifies the function to connect to the signal.
  By default, GTK tries to find the handler using the @code{g_module_symbol()}
  funcion, but this can be changed by passing a custom
  @code{GtkBuilderConnectFunc} callback function to the
  @fun{gtk-builder-connect-signals-full} function. The remaining attributes,
  @code{\"after\"}, @code{\"swapped\"} and @code{\"object\"}, have the same
  meaning as the corresponding parameters of the
  @code{g_signal_connect_object()} or @code{g_signal_connect_data()} functions.
  A @code{\"last_modification_time\"} attribute is also allowed, but it does not
  have a meaning to the builder.

  Sometimes it is necessary to refer to widgets which have implicitly been
  constructed by GTK as part of a composite widget, to set properties on them
  or to add further children, e.g. the @code{vbox} of a @class{gtk-dialog}
  widget. This can be achieved by setting the @code{\"internal-child\"} propery
  of the @code{<child>} element to a @em{true} value. Note that a
  @sym{gtk-builder} object still requires an @code{<object>} element for the
  internal child, even if it has already been constructed.

  A number of widgets have different places where a child can be added, e.g.
  tabs versus page content in notebooks. This can be reflected in a UI
  definition by specifying the @code{\"type\"} attribute on a @code{<child>}.
  The possible values for the @code{\"type\"} attribute are described in the
  sections describing the widget specific portions of UI definitions.

  @b{Example:} A @sym{gtk-builder} UI Definition
  @begin{pre}
<interface>
  <object class=\"GtkDialog\" id=\"dialog1\">
    <child internal-child=\"vbox\">
      <object class=\"GtkVBox\" id=\"vbox1\">
        <property name=\"border-width\">10</property>
        <child internal-child=\"action_area\">
          <object class=\"GtkHButtonBox\" id=\"hbuttonbox1\">
            <property name=\"border-width\">20</property>
            <child>
              <object class=\"GtkButton\" id=\"ok_button\">
                <property name=\"label\">gtk-ok</property>
                <property name=\"use-stock\">TRUE</property>
                <signal name=\"clicked\" handler=\"ok_button_clicked\"/>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>
  @end{pre}
  Beyond this general structure, several object classes define their own XML
  DTD fragments for filling in the ANY placeholders in the DTD above. Note
  that a custom element in a @code{<child>} element gets parsed by the custom
  tag handler of the parent object, while a custom element in an @code{<object>}
  element gets parsed by the custom tag handler of the object.

  These XML fragments are explained in the documentation of the respective
  objects.

  Additionally, since 3.10 a special @code{<template>} tag has been added to the
  format allowing one to define a widget class's components.

  @subheading{Embedding other XML}
  Apart from the language for UI descriptions that has been explained in the
  previous section, the @sym{gtk-builder} implementation can also parse XML
  fragments of @code{GMenu} markup. The resulting @class{g-menu} object and its
  named submenus are available via the @fun{gtk-builder-object} function like
  other constructed objects.
  @see-slot{gtk-builder-translation-domain}
  @see-class{gtk-buildable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "translation-domain"
                                               'gtk-builder) 't)
 "The @code{translation-domain} property of type @code{:string} (Read / Write)
  @br{}
  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, the @sym{gtk-builder} object uses GNU gettext, otherwise
  GLIB gettext. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-translation-domain atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-builder-translation-domain 'function)
 "@version{2021-9-3}
  @syntax[]{(gtk-builder-translation-domain object) => domain}
  @syntax[]{(setf (gtk-builder-translation-domain object) domain)}
  @argument[object]{a @class{gtk-builder} object}
  @argument[domain]{a string with the translation domain or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-builder]{translation-domain} slot of the
    @class{gtk-builder} class.
  @end{short}

  The @sym{gtk-builder-translation-domain} slot access function gets the
  translation domain of @arg{object}. The
  @sym{(setf gtk-builder-translation-domain)} slot access function sets the
  translation domain.
  @see-class{gtk-builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-builder-new))

(defun gtk-builder-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @return{A new @class{gtk-builder} object.}
  @begin{short}
    Creates a new builder object.
  @end{short}
  @see-class{gtk-builder}
  @see-function{gtk-builder-new-from-file}
  @see-function{gtk-builder-new-from-resource}
  @see-function{gtk-builder-new-from-string}"
  (make-instance 'gtk-builder))

(export 'gtk-builder-new)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_new_from_file" gtk-builder-new-from-file)
    (g-object gtk-builder)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[filename]{a string with the filename}
  @return{A @class{gtk-builder} object containing the described interface.}
  @begin{short}
    Builds the @class{gtk-builder} UI definition from a user interface
    description file.
  @end{short}
  If there is an error opening the file or parsing the description then the
  program will be aborted. You should only ever attempt to parse user interface
  descriptions that are shipped as part of your program.
  @see-class{gtk-builder}"
  (filename :string))

(export 'gtk-builder-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_new_from_resource" gtk-builder-new-from-resource)
    (g-object gtk-builder)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[path]{a string with the @class{g-resource} path}
  @return{A @class{gtk-builder} object containing the described interface.}
  @begin{short}
    Builds the @class{gtk-builder} UI definition from a resource path.
  @end{short}
  If there is an error locating the resource or parsing the description then
  the program will be aborted.
  @see-class{gtk-builder}
  @see-class{g-resource}
  @see-function{gtk-builder-new}
  @see-function{gtk-builder-new-from-file}
  @see-function{gtk-builder-new-from-string}"
  (path :string))

(export 'gtk-builder-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_new_from_string" %gtk-builder-new-from-string)
    (g-object gtk-builder)
  (string :string)
  (length :int))

(defun gtk-builder-new-from-string (string)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[string]{a string with the user interface description}
  @return{A @class{gtk-builder} object containing the interface described by
    @arg{string}.}
  @begin{short}
    Builds the user interface described by @arg{string} in the
    @class{gtk-builder} UI definition format.
  @end{short}
  If there is an error parsing the string then the program will be aborted. You
  should not attempt to parse user interface description from untrusted
  sources.
  @see-class{gtk-builder}
  @see-function{gtk-builder-new}
  @see-function{gtk-builder-new-from-file}
  @see-function{gtk-builder-new-from-resource}"
  (%gtk-builder-new-from-string string -1))

(export 'gtk-builder-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_callback_symbol ()
;;;
;;; void gtk_builder_add_callback_symbol (GtkBuilder *builder,
;;;                                       const gchar *callback_name,
;;;                                       GCallback callback_symbol);
;;;
;;; Adds the callback_symbol to the scope of builder under the given
;;; callback_name .
;;;
;;; Using this function overrides the behavior of gtk_builder_connect_signals()
;;; for any callback symbols that are added. Using this method allows for better
;;; encapsulation as it does not require that callback symbols be declared in
;;; the global namespace.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; callback_name :
;;;     The name of the callback, as expected in the XML
;;;
;;; callback_symbol :
;;;     The callback pointer.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_callback_symbols ()
;;;
;;; void gtk_builder_add_callback_symbols (GtkBuilder *builder,
;;;                                        const gchar *first_callback_name,
;;;                                        GCallback first_callback_symbol,
;;;                                        ...);
;;;
;;; A convenience function to add many callbacks instead of calling
;;; gtk_builder_add_callback_symbol() for each symbol.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; first_callback_name :
;;;     The name of the callback, as expected in the XML
;;;
;;; first_callback_symbol :
;;;     The callback pointer.
;;;
;;; ... :
;;;     A list of callback name and callback symbol pairs terminated with NULL
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_lookup_callback_symbol ()
;;;
;;; GCallback gtk_builder_lookup_callback_symbol (GtkBuilder *builder,
;;;                                               const gchar *callback_name);
;;;
;;; Fetches a symbol previously added to builder with
;;; gtk_builder_add_callback_symbols()
;;;
;;; This function is intended for possible use in language bindings or for any
;;; case that one might be cusomizing signal connections using
;;; gtk_builder_connect_signals_full()
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; callback_name :
;;;     The name of the callback
;;;
;;; Returns :
;;;     The callback symbol in builder for callback_name , or NULL
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_from_file" %gtk-builder-add-from-file) :uint
  (builder (g-object gtk-builder))
  (filename :string)
  (err :pointer))

(defun gtk-builder-add-from-file (builder filename)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[filename]{a string with the name of the file to parse}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk-builder} UI definition and merges it
    with the current contents of the builder.
  @end{short}
  @see-class{gtk-builder}
  @see-function{gtk-builder-add-from-resource}
  @see-function{gtk-builder-add-from-string}"
  (with-g-error (err)
    (%gtk-builder-add-from-file builder filename err)))

(export 'gtk-builder-add-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_from_resource" %gtk-builder-add-from-resource) :uint
  (builder (g-object gtk-builder))
  (path :string)
  (err :pointer))

(defun gtk-builder-add-from-resource (builder path)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[path]{a string with the path of the resouce file to parse}
  @return{A positive value on success, 0 if an error occured.}
  @begin{short}
    Parses a resource file containing a @class{gtk-builder} UI definition and
    merges it with the current contents of the builder.
  @end{short}
  @see-class{gtk-builder}
  @see-class{g-resource}
  @see-function{gtk-builder-add-from-file}
  @see-function{gtk-builder-add-from-string}"
  (with-g-error (err)
    (%gtk-builder-add-from-resource builder path err)))

(export 'gtk-builder-add-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_from_string" %gtk-builder-add-from-string) :uint
  (builder (g-object gtk-builder))
  (string :string)
  (length :int)
  (err :pointer))

(defun gtk-builder-add-from-string (builder string)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[string]{the string to parse}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a @class{gtk-builder} UI definition and merges
    it with the current contents of the builder.
  @end{short}
  @see-class{gtk-builder}
  @see-function{gtk-builder-add-from-file}
  @see-function{gtk-builder-add-from-resource}"
  (with-g-error (err)
    (%gtk-builder-add-from-string builder string -1 err)))

(export 'gtk-builder-add-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_objects_from_file"
          %gtk-builder-add-objects-from-file) :uint
  (builder (g-object gtk-builder))
  (filename :string)
  (object-ids :pointer)
  (err :pointer))

(defun gtk-builder-add-objects-from-file (builder filename ids)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[filename]{a string with the name of the file to parse}
  @argument[ids]{a list of strings with the object IDs to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk-builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  Upon errors 0 will be returned.
  @begin[Note]{dictionary}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk-tree-view} widget that depends on its
    @class{gtk-tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{dictionary}
  @see-class{gtk-builder}
  @see-function{gtk-builder-add-from-file}
  @see-function{gtk-builder-add-objects-from-string}
  @see-function{gtk-builder-add-objects-from-resource}"
  (let ((ids-ptr (foreign-alloc :pointer :count (1+ (length ids)))))
    (loop for i from 0
          for object-id in ids
          do (setf (mem-aref ids-ptr :pointer i)
                   (foreign-string-alloc object-id)))
    (unwind-protect
      (with-g-error (err)
        (%gtk-builder-add-objects-from-file builder filename ids-ptr err))
      (progn
        (loop for i from 0
              repeat (1- (length ids))
              do (foreign-string-free (mem-aref ids-ptr :pointer i)))
        (foreign-free ids-ptr)))))

(export 'gtk-builder-add-objects-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_objects_from_string"
          %gtk-builder-add-objects-from-string) :uint
  (builder (g-object gtk-builder))
  (string :string)
  (length :int)
  (ids :pointer)
  (err :pointer))

(defun gtk-builder-add-objects-from-string (builder string ids)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[string]{the string to parse}
  @argument[ids]{list of strings with the object IDs to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a @class{gtk-builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  @begin[Note]{dictionary}
    If you are adding an object that depends on an object that is not its child,
    for instance a @class{gtk-tree-view} widget that depends on its
    @class{gtk-tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{dictionary}
  @see-class{gtk-builder}
  @see-function{gtk-builder-add-from-string}
  @see-function{gtk-builder-add-objects-from-file}
  @see-function{gtk-builder-add-objects-from-resource}"
  (let ((ids-ptr (foreign-alloc :pointer :count (1+ (length ids)))))
    (loop for i from 0
          for object-id in ids
          do (setf (mem-aref ids-ptr :pointer i)
                   (foreign-string-alloc object-id)))
    (unwind-protect
      (with-g-error (err)
        (%gtk-builder-add-objects-from-string builder string -1 ids-ptr err))
      (progn
        (loop for i from 0
              repeat (1- (length ids))
              do (foreign-string-free (mem-aref ids-ptr :pointer i)))
        (foreign-free ids-ptr)))))

(export 'gtk-builder-add-objects-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_objects_from_resource"
          %gtk-builder-add-objects-from-resource) :uint
  (builder (g-object gtk-builder))
  (path :string)
  (ids :pointer)
  (err :pointer))

(defun gtk-builder-add-objects-from-resource (builder path ids)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[path]{a string with the path of the resource file to parse}
  @argument[ids]{list of strings with the object IDs to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a resource file containing a @class{gtk-builder} UI definition
    building only the requested objects and merges them with the current
    contents of builder.
  @end{short}
  @begin[Note]{dictionary}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk-tree-view} widget that depends on its
    @class{gtk-tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{dictionary}
  @see-class{gtk-builder}
  @see-function{gtk-builder-add-from-resource}
  @see-function{gtk-builder-add-objects-from-file}
  @see-function{gtk-builder-add-objects-from-string}"
  (let ((ids-ptr (foreign-alloc :pointer :count (1+ (length ids)))))
    (loop for i from 0
          for object-id in ids
          do (setf (mem-aref ids-ptr :pointer i)
                   (foreign-string-alloc object-id)))
    (unwind-protect
      (with-g-error (err)
        (%gtk-builder-add-objects-from-resource builder path ids-ptr err))
      (progn
        (loop for i from 0
              repeat (1- (length ids))
              do (foreign-string-free (mem-aref ids-ptr :pointer i)))
        (foreign-free ids-ptr)))))

(export 'gtk-builder-add-objects-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_extend_with_template ()
;;;
;;; guint
;;; gtk_builder_extend_with_template (GtkBuilder *builder,
;;;                                   GtkWidget *widget,
;;;                                   GType template_type,
;;;                                   const gchar *buffer,
;;;                                   gsize length,
;;;                                   GError **error);
;;;
;;; Main private entry point for building composite container components from
;;; template XML.
;;;
;;; This is exported purely to let gtk-builder-tool validate templates,
;;; applications have no need to call this function.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; widget :
;;;     the widget that is being extended
;;;
;;; template_type :
;;; the type that the template is for
;;;
;;; buffer :
;;;     the string to parse
;;;
;;; length :
;;;     the length of buffer (may be -1 if buffer is nul-terminated)
;;;
;;; error :
;;;     return location for an error, or NULL.
;;;
;;; Returns :
;;;     A positive value on success, 0 if an error occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_object () -> gtk-builder-object
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_object" gtk-builder-object) g-object
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[name]{a string with the name of object to get}
  @return{The @class{g-object} instance named @arg{name} or @code{nil} if it
    could not be found in the object tree.}
  @begin{short}
    Gets the object named @arg{name}.
  @end{short}
  @see-class{gtk-builder}
  @see-class{g-object}
  @see-function{gtk-builder-objects}"
  (builder (g-object gtk-builder))
  (name :string))

(export 'gtk-builder-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_objects () -> gtk-builder-objects
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_objects" gtk-builder-objects) (g-slist g-object)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @begin{return}
    A list containing all the @class{g-object} instances constructed by the
    @class{gtk-builder} object.
  @end{return}
  @begin{short}
    Gets all objects that have been constructed by the builder.
  @end{short}
  @see-class{gtk-builder}
  @see-class{g-object}
  @see-function{gtk-builder-object}"
  (builder (g-object gtk-builder)))

(export 'gtk-builder-objects)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_expose_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_expose_object" gtk-builder-expose-object) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[name]{a string with the name of the object exposed to the builder}
  @argument[object]{a @class{g-object} instance to expose}
  @begin{short}
    Adds an object to the builder object pool so it can be referenced just like
    any other object built by the builder.
  @end{short}
  @see-class{gtk-builder}
  @see-class{g-object}"
  (builder (g-object gtk-builder))
  (name :string)
  (object g-object))

(export 'gtk-builder-expose-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals ()
;;; ----------------------------------------------------------------------------

;; TODO: The documentation does not explain this implementation.

(defun gtk-builder-connect-signals (builder handlers)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[handlers]{a list sent in as user data to all signals}
  @begin{short}
    This method is a simpler variation of the
    @fun{gtk-builder-connect-signals-full} function.
  @end{short}
  It uses introspective features of @code{GModule} to look at the symbol table
  of the application. From here it tries to match the signal handler names given
  in the interface description with symbols in the application and connects the
  signals. Note that this function can only be called once, subsequent calls
  will do nothing.

  Note that this function will not work correctly if @code{GModule} is not
  supported on the platform.

  When compiling applications for Windows, you must declare signal callbacks
  with @code{G_MODULE_EXPORT}, or they will not be put in the symbol table. On
  Linux and Unices, this is not necessary.
  @see-class{gtk-builder}
  @see-function{gtk-builder-connect-signals-full}"
  (flet ((connect-func (builder
                        object
                        signal-name
                        handler-name
                        connect-object
                        flags)
           (declare (ignore builder connect-object))
           (let ((handler (find handler-name
                                handlers
                                :key 'first :test 'string=)))
             (when handler
               (g-signal-connect object
                                 signal-name
                                 (second handler)
                                 :after (member :after flags))))))
    (gtk-builder-connect-signals-full builder #'connect-func)))

(export 'gtk-builder-connect-signals)

;;; ----------------------------------------------------------------------------
;;; GtkBuilderConnectFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-builder-connect-func :void
    ((builder (g-object gtk-builder))
     (object g-object)
     (signal (:string :free-from-foreign nil))
     (handler (:string :free-from-foreign nil))
     (connect g-object)
     (flags g-connect-flags)
     (data :pointer))
  (restart-case
    (let ((ptr (get-stable-pointer-value data)))
      (funcall ptr builder object signal handler connect flags))
    (return () nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-connect-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-builder-connect-func atdoc:*external-symbols*)
 "@version{2021-9-23}
  @begin{short}
    This is the signature of a callback function used to connect signals.
  @end{short}
  It is used by the @fun{gtk-builder-connect-signals} and
  @fun{gtk-builder-connect-signals-full} functions. It is mainly intended for
  interpreted language bindings, but could be useful where the programmer wants
  more control over the signal connection process. Note that this function can
  only be called once, subsequent calls will do nothing.
  @begin{pre}
 lambda (builder object signal-name handler-name connect-object flags)
  @end{pre}
  @begin[code]{table}
    @entry[builder]{a @class{gtk-builder} object}
    @entry[object]{a @class{g-object} instance to connect a signal to}
    @entry[signal-name]{a string with the name of the signal}
    @entry[handler-name]{a string  with the name of the handler}
    @entry[connect-object]{a @class{g-object} instance, if non-@code{nil}, use
      the @fun{g-signal-connect-object} function}
    @entry[flags]{a value of the @symbol{g-connect-flags} flags to use}
  @end{table}
  @see-class{gtk-builder}
  @see-function{gtk-builder-connect-signals}
  @see-function{gtk-builder-connect-signals-full}")

(export 'gtk-builder-connect-func)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_connect_signals_full" %gtk-builder-connect-signals-full)
    :void
  (builder (g-object gtk-builder))
  (func :pointer)
  (data :pointer))

(defun gtk-builder-connect-signals-full (builder func)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[func]{a @symbol{gtk-builder-connect-func} callback function used to
    connect the signals}
  @begin{short}
    This function can be thought of the interpreted language binding version of
    the @fun{gtk-builder-connect-signals} function.
  @end{short}
  @see-class{gtk-builder}
  @see-symbol{gtk-builder-connect-func}
  @see-function{gtk-builder-connect-signals}"
  (with-stable-pointer (ptr func)
    (%gtk-builder-connect-signals-full builder
                                       (callback gtk-builder-connect-func)
                                       ptr)))

(export 'gtk-builder-connect-signals-full)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_application ()
;;; gtk_builder_set_application () -> gtk-builder-application
;;; ----------------------------------------------------------------------------

(defun (setf gtk-builder-application) (application builder)
  (foreign-funcall "gtk_builder_set_application"
                   (g-object gtk-builder) builder
                   (g-object gtk-application) application
                   :void)
  application)

(defcfun ("gtk_builder_get_application" gtk-builder-application)
    (g-object gtk-application)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @syntax[]{(gtk-builder-application builder) => application}
  @syntax[]{(setf (gtk-builder-application builder) application)}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[application]{a @class{gtk-application} instance}
  @begin{short}
    Accessor of the application associated with the builder.
  @end{short}

  The @sym{gtk-builder-application} function gets the application associated
  with the builder. The @sym{(setf gtk-builder-application)} function sets the
  application.

  The application is used for creating action proxies as requested from XML
  that the builder is loading. By default, the builder uses the default
  application: the one from the @fun{g-application-default} function. If you
  want to use another application for constructing proxies, use the
  @sym{(setf gtk-builder-application)} function.

  You only need this function if there is more than one
  @class{g-application} instance in your process. The @arg{application}
  argument cannot be @code{nil}.
  @see-class{gtk-builder}
  @see-class{gtk-application}
  @see-class{g-application}
  @see-function{g-application-default}"
  (builder (g-object gtk-builder)))

(export 'gtk-builder-application)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_type_from_name () -> gtk-builder-type-from-name
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_type_from_name" gtk-builder-type-from-name) g-type
 #+cl-cffi-gtk-documentation
 "@version{2021-9-23}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[name]{a string with the type name to lookup}
  @return{The @class{g-type} type ID found for @arg{name}.}
  @begin{short}
    Looks up a type by name, using the virtual function that the
    @class{gtk-builder} class has for that purpose.
  @end{short}
  This is mainly used when implementing the @class{gtk-buildable} interface on
  a type.
  @see-class{gtk-builder}
  @see-class{gtk-buildable}
  @see-class{g-type}"
  (builder (g-object gtk-builder))
  (name :string))

(export 'gtk-builder-type-from-name)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string ()
;;;
;;; gboolean gtk_builder_value_from_string (GtkBuilder *builder,
;;;                                         GParamSpec *pspec,
;;;                                         const gchar *string,
;;;                                         GValue *value,
;;;                                         GError **error);
;;;
;;; This function demarshals a value from a string. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; This function can handle char, uchar, boolean, int, uint, long, ulong, enum,
;;; flags, float, double, string, GdkColor, GdkRGBA and GtkAdjustment type
;;; values. Support for GtkWidget type values is still to come.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string_type ()
;;;
;;; gboolean gtk_builder_value_from_string_type (GtkBuilder *builder,
;;;                                              GType type,
;;;                                              const gchar *string,
;;;                                              GValue *value,
;;;                                              GError **error);
;;;
;;; Like gtk_builder_value_from_string(), this function demarshals a value from
;;; a string, but takes a GType instead of GParamSpec. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; type :
;;;     the GType of the value
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_BUILDER_WARN_INVALID_CHILD_TYPE()
;;;
;;; #define GTK_BUILDER_WARN_INVALID_CHILD_TYPE(object, type)
;;;
;;; This macro should be used to emit a warning about and unexpected type value
;;; in a GtkBuildable add_child implementation.
;;;
;;; object :
;;;     the GtkBuildable on which the warning ocurred
;;;
;;; type :
;;;     the unexpected type value
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.builder.lisp -------------------------------------------
