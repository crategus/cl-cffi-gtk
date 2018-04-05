;;; ----------------------------------------------------------------------------
;;; gtk.builder.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; Build an interface from an XML UI definition
;;;
;;; Synopsis
;;;
;;;     GtkBuilder
;;;     GtkBuilderError
;;;
;;;     gtk_builder_new
;;;     gtk_builder_new_from_file
;;;     gtk_builder_new_from_resource
;;;     gtk_builder_new_from_string
;;;
;;;     gtk_builder_add_callback_symbol
;;;     gtk_builder_add_callback_symbols
;;;     gtk_builder_lookup_callback_symbol
;;;
;;;     gtk_builder_add_from_file
;;;     gtk_builder_add_from_resource
;;;     gtk_builder_add_from_string
;;;     gtk_builder_add_objects_from_file
;;;     gtk_builder_add_objects_from_string
;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_get_object
;;;     gtk_builder_get_objects
;;;
;;;     gtk_builder_expose_object
;;;
;;;     gtk_builder_connect_signals
;;;     gtk_builder_connect_signals_full
;;;     gtk_builder_set_translation_domain
;;;     gtk_builder_get_translation_domain
;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type
;;;
;;;     GTK_BUILDER_WARN_INVALID_CHILD_TYPE
;;;     GTK_BUILDER_ERROR
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((builder gtk-builder)
                                       &key from-file from-string)
  (when from-file
    (gtk-builder-add-from-file builder from-file))
  (when from-string
    (gtk-builder-add-from-string builder from-string)))

;;; ----------------------------------------------------------------------------

#|



Example 9. A GtkBuilder UI Definition

<interface>
  <object class="GtkDialog" id="dialog1">
    <child internal-child="vbox">
      <object class="GtkVBox" id="vbox1">
        <property name="border-width">10</property>
        <child internal-child="action_area">
          <object class="GtkHButtonBox" id="hbuttonbox1">
            <property name="border-width">20</property>
            <child>
              <object class="GtkButton" id="ok_button">
                <property name="label">gtk-ok</property>
                <property name="use-stock">TRUE</property>
                <signal name="clicked" handler="ok_button_clicked"/>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>



|#

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-builder 'type)
 "@version{2014-7-20}
  @begin{short}
    A @sym{gtk-builder} is an auxiliary object that reads textual descriptions
    of a user interface and instantiates the described objects.
  @end{short}
  To create a @class{gtk-builder} from a user interface description, call
  the functions @fun{gtk-builder-new-from-file},
  @fun{gtk-builder-new-from-resource} or @fun{gtk-builder-new-from-string}.

  In the (unusual) case that you want to add user interface descriptions from
  multiple sources to the same @class{gtk-builder} you can call the function
  @fun{gtk-builder-new} to get an empty builder and populate it by (multiple)
  calls to the functions @fun{gtk-builder-add-from-file}, 
  @fun{gtk-builder-add-from-resource} or @fun{gtk-builder-add-from-string}.

  A @sym{gtk-builder} holds a reference to all objects that it has constructed
  and drops these references when it is finalized. This finalization can cause
  the destruction of non-widget objects or widgets which are not contained in a
  toplevel window. For toplevel windows constructed by a builder, it is the
  responsibility of the user to call the function @fun{gtk-widget-destroy} to
  get rid of them and all the widgets they contain.

  The functions @fun{gtk-builder-get-object} and @fun{gtk-builder-get-objects}
  can be used to access the widgets in the interface by the names assigned to
  them inside the UI description. Toplevel windows returned by these functions
  will stay around until the user explicitly destroys them with the function
  @fun{gtk-widget-destroy}. Other widgets will either be part of a larger
  hierarchy constructed by the builder (in which case you should not have to
  worry about their lifecycle), or without a parent, in which case they have
  to be added to some container to make use of them. Non-widget objects need
  to be reffed with the function @fun{g-object-ref} to keep them beyond the
  lifespan of the builder.

  The function @fun{gtk-builder-connect-signals} and variants thereof can be
  used to connect handlers to the named signals in the description.

  @subheading{@sym{gtk-builder} UI Definitions}
    @sym{gtk-builder} parses textual descriptions of user interfaces which are
    specified in an XML format which can be roughly described by the RELAX NG
    schema below. We refer to these descriptions as @sym{gtk-builder} UI
    definitions or just UI definitions if the context is clear. Do not confuse
    @sym{gtk-builder} UI Definitions with @class{gtk-ui-manager} UI Definitions,
    which are more limited in scope. It is common to use .ui as the filename
    extension for files containing @class{gtk-builder} UI definitions.
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
    The toplevel element is <interface>. It optionally takes a \"domain\"
    attribute, which will make the builder look for translated strings using
    @code{dgettext()} in the domain specified. This can also be done by calling
    @fun{gtk-builder-translation-domain} on the builder. Objects are described
    by <object> elements, which can contain <property> elements to set
    properties, <signal> elements which connect signals to handlers, and <child>
    elements, which describe child objects (most often widgets inside a
    container, but also e. g. actions in an action group, or columns in a tree
    model). A <child> element contains an <object> element which describes the
    child object. The target toolkit version(s) are described by <requires>
    elements, the \"lib\" attribute specifies the widget library in question
    (currently the only supported value is \"gtk+\") and the \"version\"
    attribute specifies the target version in the form \"<major>.<minor>\".
    The builder will error out if the version requirements are not met.

    Typically, the specific kind of object represented by an <object> element is
    specified by the \"class\" attribute. If the type has not been loaded yet,
    GTK+ tries to find the @code{_get_type()} from the class name by applying
    heuristics. This works in most cases, but if necessary, it is possible to
    specify the name of the @code{_get_type()} explictly with the \"type-func\"
    attribute. As a special case, @class{gtk-builder} allows to use an object
    that has been constructed by a @class{gtk-ui-manager} in another part of
    the UI definition by specifying the ID of the @class{gtk-ui-manager} in the
    \"constructor\" attribute and the name of the object in the \"id\"
    attribute. 

    Objects must be given a name with the \"id\" attribute, which allows the
    application to retrieve them from the builder with the function
    @fun{gtk-builder-get-object}. An ID is also necessary to use the object as
    property value in other parts of the UI definition.

    @subheading{Note}
      Prior to 2.20, @sym{gtk-builder} was setting the \"name\" property of
      constructed widgets to the \"id\" attribute. In GTK+ 2.20 or newer, you
      have to use the function @fun{gtk-buildable-get-name} instead of the
      function @fun{gtk-widget-name} to obtain the \"id\", or set the
      \"name\" property in your UI definition.

  Setting properties of objects is pretty straightforward with the <property>
  element: the \"name\" attribute specifies the name of the property, and the
  content of the element specifies the value. If the \"translatable\" attribute
  is set to a true value, GTK+ uses @code{gettext()} (or @code{dgettext()} if
  the builder has a translation domain set) to find a translation for the value.
  This happens before the value is parsed, so it can be used for properties of
  any type, but it is probably most useful for string properties. It is also
  possible to specify a context to disambiguate short strings, and comments
  which may help the translators.

  @sym{gtk-builder} can parse textual representations for the most common
  property types: characters, strings, integers, floating-point numbers,
  booleans (strings like \"TRUE\", \"t\", \"yes\", \"y\", \"1\" are interpreted
  as @em{true}, strings like \"FALSE, \"f\", \"no\", \"n\", \"0\" are
  interpreted as @code{nil}), enumerations (can be specified by their name, nick
  or integer value), flags (can be specified by their name, nick, integer value,
  optionally combined with \"|\", e. g. \"GTK_VISIBLE|GTK_REALIZED\") and colors
  (in a format understood by the function @fun{gdk-color-parse}). Objects can be
  referred to by their name. Pixbufs can be specified as a filename of an image
  file to load. In general, @sym{gtk-builder} allows forward references to
  objects - an object does not have to be constructed before it can be referred
  to. The exception to this rule is that an object has to be constructed before
  it can be used as the value of a construct-only property.

  Signal handlers are set up with the <signal> element. The \"name\" attribute
  specifies the name of the signal, and the \"handler\" attribute specifies the
  function to connect to the signal. By default, GTK+ tries to find the
  handler using the function @fun{g-module-symbol}, but this can be changed by
  passing a custom @code{GtkBuilderConnectFunc} to the function
  @fun{gtk-builder-connect-signals-full}. The remaining attributes, \"after\",
  \"swapped\" and \"object\", have the same meaning as the corresponding
  parameters of the @fun{g-signal-connect-object} or
  @fun{g-signal-connect-data} functions. A \"last_modification_time\" attribute
  is also allowed, but it does not have a meaning to the builder.

  Sometimes it is necessary to refer to widgets which have implicitly been
  constructed by GTK+ as part of a composite widget, to set properties on them
  or to add further children (e. g. the @code{vbox} of a @class{gtk-dialog}).
  This can be achieved by setting the \"internal-child\" propery of the <child>
  element to a @em{true} value. Note that @sym{gtk-builder} still requires an
  <object> element for the internal child, even if it has already been
  constructed.

  A number of widgets have different places where a child can be added (e. g.
  tabs vs. page content in notebooks). This can be reflected in a UI
  definition by specifying the \"type\" attribute on a <child>. The possible
  values for the \"type\" attribute are described in the sections describing the
  widget-specific portions of UI definitions.

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
  that a custom element in a <child> element gets parsed by the custom tag
  handler of the parent object, while a custom element in an <object> element
  gets parsed by the custom tag handler of the object.

  These XML fragments are explained in the documentation of the respective
  objects, see @class{gtk-widget}, @class{gtk-label}, @class{gtk-window},
  @class{gtk-container}, @class{gtk-dialog}, @class{gtk-cell-layout},
  @class{gtk-color-selection-dialog}, @class{gtk-font-selection-dialog},
  @class{gtk-expander}, @class{gtk-frame}, @class{gtk-list-store},
  @class{gtk-tree-store}, @class{gtk-notebook}, @class{gtk-size-group},
  @class{gtk-tree-view}, @class{gtk-ui-manager}, @class{gtk-action-group}.
  @class{gtk-menu-item}, @class{gtk-menu-tool-button}, @class{gtk-assistant},
  @class{gtk-scale}, @class{gtk-combo-box-text}, @class{gtk-recent-filter},
  @class{gtk-file-filter}, @class{gtk-text-tag-table}.

  Additionally, since 3.10 a special <template> tag has been added to the format
  allowing one to define a widget class's components.

  @subheading{Embedding other XML}
    Apart from the language for UI descriptions that has been explained in the
    previous section, @sym{gtk-builder} can also parse XML fragments of
    @code{GMenu} markup. The resulting @code{GMenu} object and its named
    submenus are available via the function @fun{gtk-builder-get-object} like
    other constructed objects.
  @see-slot{gtk-builder-translation-domain}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "translation-domain"
                                               'gtk-builder) 't)
 "The @code{\"translation-domain\"} property of type @code{:string}
  (Read / Write) @br{}
  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, @sym{gtk-builder} uses @code{gettext()}, otherwise
  @code{g_dgettext()}. @br{}
  Default value: @code{nil} @br{}
  Since 2.12")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-translation-domain atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-builder-translation-domain 'function)
 "@version{2014-7-20}
  @argument[object]{a @class{gtk-builder} object}
  @argument[domain]{the translation domain or @code{nil}}
  @syntax[]{(gtk-builder-translation-domain object) => domain}
  @syntax[]{(setf (gtk-builder-translation-domain object) domain)}
  @begin{short}
    Accessor of the slot @slot[gtk-builder]{translation-domain} of the
    @class{gtk-builder} class.
  @end{short}

  The generic function @sym{gtk-builder-translation-domain} gets the translation
  domain of @arg{object}.

  The generic function @sym{(setf gtk-builder-translation-domain)} sets the
  translation domain of @arg{object}. See the
  @slot[gtk-builder]{translation-domain} property.

  Since 2.12
  @see-class{gtk-builder}")

;;; ----------------------------------------------------------------------------
;;; GtkBuilderConnectFunc ()
;;;
;;; void (*GtkBuilderConnectFunc) (GtkBuilder *builder,
;;;                                GObject *object,
;;;                                const gchar *signal_name,
;;;                                const gchar *handler_name,
;;;                                GObject *connect_object,
;;;                                GConnectFlags flags,
;;;                                gpointer user_data);
;;;
;;; This is the signature of a function used to connect signals. It is used by
;;; the gtk_builder_connect_signals() and gtk_builder_connect_signals_full()
;;; methods. It is mainly intended for interpreted language bindings, but could
;;; be useful where the programmer wants more control over the signal connection
;;; process. Note that this function can only be called once, subsequent calls
;;; will do nothing.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; object :
;;;     object to connect a signal to
;;;
;;; signal_name :
;;;     name of the signal
;;;
;;; handler_name :
;;;     name of the handler
;;;
;;; connect_object :
;;;     a GObject, if non-NULL, use g_signal_connect_object()
;;;
;;; flags :
;;;     GConnectFlags to use
;;;
;;; user_data :
;;;     user data
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

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
  (:duplicate-id 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-error atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-builder-error atdoc:*external-symbols*)
 "@version{2013-5-28}
  @begin{short}
    Error codes that identify various errors that can occur while using
    @sym{gtk-builder}.
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
  (:duplicate-id 8))
  @end{pre}
  @begin[code]{table}
    @entry[:invalid-type-function]{A type-func attribute did not name a function
      that returns a @class{g-type}.}
    @entry[:unhandled-tag]{The input contained a tag that @class{gtk-builder}
      cannot handle.}
    @entry[:missing-attribute]{An attribute that is required by
      @sym{gtk-builder} was missing.}
    @entry[:invalid-attribute]{@sym{gtk-builder} found an attribute that it
      does not understand.}
    @entry[:invalid-tag]{@sym{gtk-builder} found a tag that it does not
      understand.}
    @entry[:missing-property-value]{A required property value was missing.}
    @entry[:invalid-value]{@sym{gtk-builder} could not parse some attribute
      value.}
    @entry[:version-mismatch]{The input file requires a newer version of GTK+.}
    @entry[:duplicate-id]{An object id occurred twice.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-builder-new))

(defun gtk-builder-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @return{A new @class{gtk-builder} object.}
  @begin{short}
    Creates a new builder object.
  @end{short}

  Since 2.12
  @see-class{gtk-builder}"
  (make-instance 'gtk-builder))

(export 'gtk-builder-new)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_file ()
;;;
;;; GtkBuilder * gtk_builder_new_from_file (const gchar *filename);
;;;
;;; Builds the GtkBuilder UI definition in the file filename .
;;;
;;; If there is an error opening the file or parsing the description then the
;;; program will be aborted. You should only ever attempt to parse user
;;; interface descriptions that are shipped as part of your program.
;;;
;;; Parameters
;;;
;;; filename
;;;     filename of user interface description file
;;;
;;; Returns
;;;     a Gtkbuilder containing the described interface
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_resource ()
;;;
;;; GtkBuilder * gtk_builder_new_from_resource (const gchar *resource_path);
;;;
;;; Builds the GtkBuilder UI definition at resource_path .
;;;
;;; If there is an error locating the resurce or parsing the description then
;;; the program will be aborted.
;;;
;;; Parameters
;;;
;;; resource_path
;;;     a GResource resource path
;;;
;;; Returns
;;;     a Gtkbuilder containing the described interface
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_string ()
;;;
;;; GtkBuilder * gtk_builder_new_from_string (const gchar *string,
;;;                                           gssize length);
;;;
;;; Builds the user interface described by string (in the GtkBuilder UI
;;; definition format).
;;;
;;; If string is NULL-terminated then length should be -1. If length is not -1
;;; then it is the length of string .
;;;
;;; If there is an error parsing string then the program will be aborted. You
;;; should not attempt to parse user interface description from untrusted
;;; sources.
;;;
;;; Parameters
;;;
;;; string
;;;     a user interface (XML) description
;;;
;;; length
;;;     the length of string , or -1
;;;
;;; Returns
;;;     a Gtkbuilder containing the interface described by string
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

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
;;; Parameters
;;;
;;; builder
;;;     a GtkBuilder
;;;
;;; callback_name
;;;     The name of the callback, as expected in the XML
;;;
;;; callback_symbol
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
;;; Parameters
;;;
;;; builder
;;;     a GtkBuilder
;;;
;;; first_callback_name
;;;     The name of the callback, as expected in the XML
;;;
;;; first_callback_symbol
;;;     The callback pointer.
;;;
;;; ...
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
;;; Parameters
;;;
;;; builder
;;;     a GtkBuilder
;;;
;;; callback_name
;;;     The name of the callback
;;;
;;; Returns
;;;     The callback symbol in builder for callback_name , or NULL
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_from_file" %gtk-builder-add-from-file) :uint
  (builder g-object)
  (filename :string)
  (error :pointer))

(defun gtk-builder-add-from-file (builder filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @sym{gtk-builder} object}
  @argument[filename]{the name of the file to parse}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk-builder} UI definition and merges it
    with the current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a @type{g-error}
  from the @code{GTK_BUILDER_ERROR}, @code{G_MARKUP_ERROR} or
  @code{G_FILE_ERROR domain}.

  Since 2.12"
  (%gtk-builder-add-from-file builder filename (null-pointer)))

(export 'gtk-builder-add-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_resource ()
;;;
;;; guint gtk_builder_add_from_resource (GtkBuilder *builder,
;;;                                      const gchar *resource_path,
;;;                                      GError **error);
;;;
;;; Parses a resource file containing a GtkBuilder UI definition and merges it
;;; with the current contents of builder.
;;;
;;; Upon errors 0 will be returned and error will be assigned a GError from the
;;; GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_RESOURCE_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; resource_path :
;;;     the path of the resource file to parse
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     A positive value on success, 0 if an error occurred
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_from_string" %gtk-builder-add-from-string) :uint
  (builder g-object)
  (string :string)
  (length :int)
  (error :pointer))

(defun gtk-builder-add-from-string (builder buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[buffer]{the string to parse}
  @argument[length]{the length of @arg{buffer} (may be -1 if @arg{buffer} is
    nul-terminated)}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a @class{gtk-builder} UI definition and merges it
    with the current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a @type{g-error}
  from the @code{GTK_BUILDER_ERROR} or @code{G_MARKUP_ERROR domain}.

  Since 2.12"
  (%gtk-builder-add-from-string builder buffer -1 (null-pointer)))

(export 'gtk-builder-add-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_objects_from_file"
          %gtk-builder-add-objects-from-file) :uint
  (builder g-object)
  (filename :string)
  (object-ids :pointer)
  (error :pointer))

(defun gtk-builder-add-objects-from-file (builder filename object-ids)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[filename]{the name of the file to parse}
  @argument[object-ids]{nul-terminated array of objects to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk-builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  Upon errors 0 will be returned and error will be assigned a @type{g-error}
  from the @code{GTK_BUILDER_ERROR}, @code{G_MARKUP_ERROR} or
  @code{G_FILE_ERROR domain}.

  @subheading{Note}
    If you are adding an object that depends on an object that is not its child
    (for instance a @class{gtk-tree-view} that depends on its
    @class{gtk-tree-model}), you have to explicitely list all of them in
    @arg{object-ids}.

  Since 2.14"
  (let ((l (foreign-alloc :pointer :count (1+ (length object-ids)))))
    (loop
       for i from 0
       for object-id in object-ids
       do (setf (mem-aref l :pointer i) (foreign-string-alloc object-id)))
    (unwind-protect
         (%gtk-builder-add-objects-from-file builder filename l (null-pointer))
      (loop
         for i from 0
         repeat (1- (length object-ids))
         do (foreign-string-free (mem-aref l :pointer i)))
      (foreign-free l))))

(export 'gtk-builder-add-objects-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_add_objects_from_string"
          %gtk-builder-add-objects-from-string) :uint
  (builder g-object)
  (string :string)
  (length :int)
  (object-ids :pointer)
  (error :pointer))

(defun gtk-builder-add-objects-from-string (builder buffer object-ids)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[buffer]{the string to parse}
  @argument[object-ids]{nul-terminated array of objects to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a @class{gtk-builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a @type{g-error}
  from the @code{GTK_BUILDER_ERROR} or @code{G_MARKUP_ERROR} domain.

  @subheading{Note}
    If you are adding an object that depends on an object that is not its child
    (for instance a @class{gtk-tree-view} that depends on its
    @class{gtk-tree-model}), you have to explicitely list all of them in
    @arg{object-ids}.

  Since 2.14"
  (let ((l (foreign-alloc :pointer :count (1+ (length object-ids)))))
    (loop
       for i from 0
       for object-id in object-ids
       do (setf (mem-aref l :pointer i) (foreign-string-alloc object-id)))
    (unwind-protect
      (%gtk-builder-add-objects-from-string builder
                                            buffer
                                            -1
                                            l
                                            (null-pointer))
      (loop
         for i from 0
         repeat (1- (length object-ids))
         do (foreign-string-free (mem-aref l :pointer i)))
      (foreign-free l))))

(export 'gtk-builder-add-objects-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_resource ()
;;;
;;; guint gtk_builder_add_objects_from_resource (GtkBuilder *builder,
;;;                                              const gchar *resource_path,
;;;                                              gchar **object_ids,
;;;                                              GError **error);
;;;
;;; Parses a resource file containing a GtkBuilder UI definition building only
;;; the requested objects and merges them with the current contents of builder.
;;;
;;; Upon errors 0 will be returned and error will be assigned a GError from the
;;; GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_RESOURCE_ERROR domain.
;;;
;;; Note
;;;
;;; If you are adding an object that depends on an object that is not its child
;;; (for instance a GtkTreeView that depends on its GtkTreeModel), you have to
;;; explicitely list all of them in object_ids.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; resource_path :
;;;     the path of the resource file to parse
;;;
;;; object_ids :
;;;     nul-terminated array of objects to build
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     A positive value on success, 0 if an error occurred.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_object" gtk-builder-get-object) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-12-1}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[name]{name of object to get}
  @return{the object named name or @code{nil} if it could not be found in the
    object tree}
  @begin{short}
    Gets the object named @arg{name}.
  @end{short}
  Note that this function does not increment the reference count of the
  returned object.

  Since 2.12
  @see-class{gtk-builder}
  @see-function{gtk-builder-get-objects}"
  (builder (g-object gtk-builder))
  (name :string))

(export 'gtk-builder-get-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_objects ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_objects" gtk-builder-get-objects) (g-slist g-object)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-1}
  @argument[builder]{a @class{gtk-builder} object}
  @begin{return}
    A list containing all the objects constructed by the @class{gtk-builder}
    instance.
  @end{return}
  @begin{short}
    Gets all objects that have been constructed by @arg{builder}.
  @end{short}
  Note that this function does not increment the reference counts of the
  returned objects.

  Since 2.12
  @see-class{gtk-builder}
  @see-function{gtk-builder-get-object}"
  (builder (g-object gtk-builder)))

(export 'gtk-builder-get-objects)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_expose_object ()
;;;
;;; void gtk_builder_expose_object (GtkBuilder *builder,
;;;                                 const gchar *name,
;;;                                 GObject *object);
;;;
;;; Add object to the builder object pool so it can be referenced just like any
;;; other object built by builder.
;;;
;;; Parameters
;;;
;;; builder
;;;     a GtkBuilder
;;;
;;; name
;;;     the name of the object exposed to the builder
;;;
;;; object
;;;     the object to expose
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals ()
;;; ----------------------------------------------------------------------------

(defun gtk-builder-connect-signals (builder handlers-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[handlers-list]{a list of (name function) pairs}
  @begin{short}
    This method is a simpler variation of the function
    @fun{gtk-builder-connect-signals-full}.
  @end{short}
  It looks up the signal handler names given in the interface description in the
  @arg{handlers-list} and connects the signals. Note that this function can
  only be called once, subsequent calls will do nothing.

  Since 2.12"
  (flet ((connect-func (builder
                        object
                        signal-name
                        handler-name
                        connect-object
                        flags)
           (declare (ignore builder connect-object))
           (let ((handler (find handler-name handlers-list
                                             :key 'first
                                             :test 'string=)))
             (when handler
               (g-signal-connect object
                                 signal-name
                                 (second handler)
                                 :after (member :after flags))))))
    (gtk-builder-connect-signals-full builder #'connect-func)))

(export 'gtk-builder-connect-signals)

(defun gtk-builder-connect-signals-auto (builder)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @begin{short}
    This method is a simpler variation of the function
    @fun{gtk-builder-connect-signals-full}.
  @end{short}
  It matches the signal handler names given in the interface description with
  function names in the application and connects the signals. Note that this
  function can only be called once, subsequent calls will do nothing.

  Note that handler names are looked up via @fun{read-from-string} in the
  current package, therefore supplying a package prefix might be necessary.

  Since 2.12"
  (flet ((connect-func (builder
                        object
                        signal-name
                        handler-name
                        connect-object
                        flags)
           (declare (ignore builder connect-object))
           (let ((handler (fdefinition (read-from-string handler-name))))
             (when handler
               (g-signal-connect object
                                 signal-name
                                 handler
                                 :after (member :after flags))))))
    (gtk-builder-connect-signals-full builder #'connect-func)))

(export 'gtk-builder-connect-signals-auto)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals_full ()
;;; ----------------------------------------------------------------------------

(defbitfield connect-flags
  :after
  :swapped)

(defcallback builder-connect-func-callback :void
    ((builder g-object)
     (object g-object)
     (signal-name (:string :free-from-foreign nil))
     (handler-name (:string :free-from-foreign nil))
     (connect-object g-object)
     (flags connect-flags) (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data)
               builder object signal-name handler-name connect-object flags)
    (return () nil)))

(defcfun ("gtk_builder_connect_signals_full" %gtk-builder-connect-signals-full)
    :void
  (builder g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-builder-connect-signals-full (builder func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-28}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[func]{the function used to connect the signals}
  @begin{short}
    This function can be thought of the interpreted language binding version of
    the function @fun{gtk-builder-connect-signals}, except that it does not
    require @code{GModule} to function correctly.
  @end{short}

  Since 2.12"
  (with-stable-pointer (ptr func)
    (%gtk-builder-connect-signals-full builder
                                       (callback builder-connect-func-callback)
                                       ptr)))

(export 'gtk-builder-connect-signals-full)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_type_from_name ()
;;;
;;; GType gtk_builder_get_type_from_name (GtkBuilder *builder,
;;;                                       const char *type_name);
;;;
;;; Looks up a type by name, using the virtual function that GtkBuilder has for
;;; that purpose. This is mainly used when implementing the GtkBuildable
;;; interface on a type.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; type_name :
;;;     type name to lookup
;;;
;;; Returns :
;;;     the GType found for type_name or G_TYPE_INVALID if no type was found
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; GTK_BUILDER_ERROR
;;;
;;; #define GTK_BUILDER_ERROR (gtk_builder_error_quark ())
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.builder.lisp -------------------------------------------
