;;; ----------------------------------------------------------------------------
;;; gtk.builder.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;;     gtk_builder_add_from_file
;;;     gtk_builder_add_from_resource
;;;     gtk_builder_add_from_string
;;;     gtk_builder_add_objects_from_file
;;;     gtk_builder_add_objects_from_string
;;;     gtk_builder_get_object
;;;     gtk_builder_get_objects
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-builder 'type)
 "@version{2013-2-13}
  A GtkBuilder is an auxiliary object that reads textual descriptions of a
  user interface and instantiates the described objects. To pass a description
  to a GtkBuilder, call gtk_builder_add_from_file() or
  gtk_builder_add_from_string(). These functions can be called multiple times;
  the builder merges the content of all descriptions.

  A GtkBuilder holds a reference to all objects that it has constructed and
  drops these references when it is finalized. This finalization can cause the
  destruction of non-widget objects or widgets which are not contained in a
  toplevel window. For toplevel windows constructed by a builder, it is the
  responsibility of the user to call gtk_widget_destroy() to get rid of them
  and all the widgets they contain.

  The functions gtk_builder_get_object() and gtk_builder_get_objects() can be
  used to access the widgets in the interface by the names assigned to them
  inside the UI description. Toplevel windows returned by these functions will
  stay around until the user explicitly destroys them with
  gtk_widget_destroy(). Other widgets will either be part of a larger
  hierarchy constructed by the builder (in which case you should not have to
  worry about their lifecycle), or without a parent, in which case they have
  to be added to some container to make use of them. Non-widget objects need
  to be reffed with g_object_ref() to keep them beyond the lifespan of the
  builder.

  The function gtk_builder_connect_signals() and variants thereof can be used
  to connect handlers to the named signals in the description.

  GtkBuilder UI Definitions

  GtkBuilder parses textual descriptions of user interfaces which are
  specified in an XML format which can be roughly described by the RELAX NG
  schema below. We refer to these descriptions as GtkBuilder UI definitions or
  just UI definitions if the context is clear. Do not confuse GtkBuilder UI
  Definitions with GtkUIManager UI Definitions, which are more limited in
  scope.
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
  dgettext() in the domain specified. This can also be done by
  calling gtk_builder_set_translation_domain() on the builder. Objects are
  described by <object> elements, which can contain <property> elements to set
  properties, <signal> elements which connect signals to handlers, and <child>
  elements, which describe child objects (most often widgets inside a
  container, but also e.g. actions in an action group, or columns in a tree
  model). A <child> element contains an <object> element which describes the
  child object. The target toolkit version(s) are described by <requires>
  elements, the \"lib\" attribute specifies the widget library in question
  (currently the only supported value is \"gtk+\") and the \"version\" attribute
  specifies the target version in the form \"<major>.<minor>\". The builder will
  error out if the version requirements are not met.

  Typically, the specific kind of object represented by an <object> element is
  specified by the \"class\" attribute. If the type has not been loaded yet,
  GTK+ tries to find the _get_type() from the class name by applying
  heuristics. This works in most cases, but if necessary, it is possible to
  specify the name of the _get_type() explictly with the \"type-func\"
  attribute. As a special case, GtkBuilder allows to use an object that has
  been constructed by a GtkUIManager in another part of the UI definition by
  specifying the id of the GtkUIManager in the \"constructor\" attribute and the
  name of the object in the \"id\" attribute.

  Objects must be given a name with the \"id\" attribute, which allows the
  application to retrieve them from the builder with gtk_builder_get_object().
  An id is also necessary to use the object as property value in other parts
  of the UI definition.

  Note

  Prior to 2.20, GtkBuilder was setting the \"name\" property of constructed
  widgets to the \"id\" attribute. In GTK+ 2.20 or newer, you have to use
  gtk_buildable_get_name() instead of gtk_widget_get_name() to obtain the
  \"id\", or set the \"name\" property in your UI definition.

  Setting properties of objects is pretty straightforward with the <property>
  element: the \"name\" attribute specifies the name of the property, and the
  content of the element specifies the value. If the \"translatable\" attribute
  is set to a true value, GTK+ uses gettext() (or dgettext() if the builder
  has a translation domain set) to find a translation for the value. This
  happens before the value is parsed, so it can be used for properties of any
  type, but it is probably most useful for string properties. It is also
  possible to specify a context to disambiguate short strings, and comments
  which may help the translators.

  GtkBuilder can parse textual representations for the most common property
  types: characters, strings, integers, floating-point numbers, booleans
  (strings like \"TRUE\", \"t\", \"yes\", \"y\", \"1\" are interpreted as TRUE, strings
  like \"FALSE, \"f\", \"no\", \"n\", \"0\" are interpreted as FALSE), enumerations
  (can be specified by their name, nick or integer value), flags (can be
  specified by their name, nick, integer value, optionally combined with \"|\",
  e.g. \"GTK_VISIBLE|GTK_REALIZED\") and colors (in a format understood by
  gdk_color_parse()). Objects can be referred to by their name. Pixbufs can be
  specified as a filename of an image file to load. In general, GtkBuilder
  allows forward references to objects - an object doesn't have to be
  constructed before it can be referred to. The exception to this rule is that
  an object has to be constructed before it can be used as the value of a
  construct-only property.

  Signal handlers are set up with the <signal> element. The \"name\" attribute
  specifies the name of the signal, and the \"handler\" attribute specifies the
  function to connect to the signal. By default, GTK+ tries to find the
  handler using g_module_symbol(), but this can be changed by passing a custom
  GtkBuilderConnectFunc to gtk_builder_connect_signals_full(). The remaining
  attributes, \"after\", \"swapped\" and \"object\", have the same meaning as the
  corresponding parameters of the g_signal_connect_object() or
  g_signal_connect_data() functions. A \"last_modification_time\" attribute is
  also allowed, but it does not have a meaning to the builder.

  Sometimes it is necessary to refer to widgets which have implicitly been
  constructed by GTK+ as part of a composite widget, to set properties on them
  or to add further children (e.g. the vbox of a GtkDialog). This can be
  achieved by setting the \"internal-child\" propery of the <child> element to a
  true value. Note that GtkBuilder still requires an <object> element for the
  internal child, even if it has already been constructed.

  A number of widgets have different places where a child can be added (e.g.
  tabs vs. page content in notebooks). This can be reflected in a UI
  definition by specifying the \"type\" attribute on a <child>. The possible
  values for the \"type\" attribute are described in the sections describing the
  widget-specific portions of UI definitions.

  Example 109. A GtkBuilder UI Definition
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
  objects, see GtkWidget, GtkLabel, GtkWindow, GtkContainer, GtkDialog,
  GtkCellLayout, GtkColorSelectionDialog, GtkFontSelectionDialog, GtkExpander,
  GtkFrame, GtkListStore, GtkTreeStore, GtkNotebook, GtkSizeGroup,
  GtkTreeView, GtkUIManager, GtkActionGroup. GtkMenuItem, GtkMenuToolButton,
  GtkAssistant, GtkScale, GtkComboBoxText, GtkRecentFilter, GtkFileFilter,
  GtkTextTagTable.

  Embedding other XML

  Apart from the language for UI descriptions that has been explained in the
  previous section, GtkBuilder can also parse XML fragments of GMenu markup.
  The resulting GMenu object and its named submenus are available via
  gtk_builder_get_object() like other constructed objects.
  @see-slot{gtk-builder-translation-domain}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "translation-domain" 'gtk-builder) 't)
 "The @code{\"translation-domain\"} property of type @code{gchar*}
  (Read / Write)@br{}
  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, @class{gtk-builder} uses @code{gettext()}, otherwise
  @code{g_dgettext()}.@br{}
  Default value: @code{nil}@br{}
  Since 2.12")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-builder-translation-domain -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-translation-domain atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-builder-translation-domain 'function)
 "@version{2013-2-14}
  @begin{short}
    Accessor of the slot @code{\"tranlattion-domain\"} of the
    @class{gtk-builder} class.
  @end{short}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-builder-error atdoc:*symbol-name-alias*) "Enum"
      (documentation 'gtk-builder-error atdoc:*external-symbols*)
 "@version{2013-2-24}
  @begin{short}
    Error codes that identify various errors that can occur while using
    GtkBuilder.
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
    @entry[:invalid-type-function]{A type-func attribute didn't name a function
      that returns a GType.}
    @entry[:unhandled-tag]{The input contained a tag that GtkBuilder can't
      handle.}
    @entry[:missing-attribute]{An attribute that is required by GtkBuilder was
      missing.}
    @entry[:invalid-attribute]{GtkBuilder found an attribute that it doesn't
    understand.}
    @entry[:invalid-tag]{GtkBuilder found a tag that it doesn't understand.}
    @entry[:missing-property-value]{A required property value was missing.}
    @entry[:invalid-value]{GtkBuilder couldn't parse some attribute value.}
    @entry[:version-mismatch]{The input file requires a newer version of GTK+.}
    @entry[:duplicate-id]{An object id occurred twice.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new ()
;;;
;;; GtkBuilder * gtk_builder_new (void);
;;;
;;; Creates a new builder object.
;;;
;;; Returns :
;;;     a new GtkBuilder object
;;;
;;; Since 2.12
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
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[filename]{the name of the file to parse}
  @argument[error]{return location for an error, or NULL}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a file containing a GtkBuilder UI definition and merges it with the
    current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a GError from the
  GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_FILE_ERROR domain.

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
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[buffer]{the string to parse}
  @argument[length]{the length of buffer (may be -1 if buffer is
    nul-terminated)}
  @argument[error]{return location for an error, or NULL}
  @return{A positive value on success, 0 if an error occurred}
  @begin{short}
    Parses a string containing a GtkBuilder UI definition and merges it with the
    current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a GError from the
  GTK_BUILDER_ERROR or G_MARKUP_ERROR domain.

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
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[filename]{the name of the file to parse}
  @argument[object-ids]{nul-terminated array of objects to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{pre}
    Parses a file containing a GtkBuilder UI definition building only the
    requested objects and merges them with the current contents of builder.
  @end{pre}
  Upon errors 0 will be returned and error will be assigned a GError from the
  GTK_BUILDER_ERROR, G_MARKUP_ERROR or G_FILE_ERROR domain.
 
  Note

  If you are adding an object that depends on an object that is not its child
  (for instance a GtkTreeView that depends on its GtkTreeModel), you have to
  explicitely list all of them in object_ids.

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
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[buffer]{the string to parse}
  @argument[object_ids]{nul-terminated array of objects to build}
  @return{A positive value on success, 0 if an error occurred.}
  @begin{short}
    Parses a string containing a GtkBuilder UI definition building only the
    requested objects and merges them with the current contents of builder.
  @end{short}

  Upon errors 0 will be returned and error will be assigned a GError from the
  GTK_BUILDER_ERROR or G_MARKUP_ERROR domain.

  Note

  If you are adding an object that depends on an object that is not its child
  (for instance a GtkTreeView that depends on its GtkTreeModel), you have to
  explicitely list all of them in object_ids.

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
;;; gtk_builder_get_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_builder_get_object" gtk-builder-get-object) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[name]{name of object to get}
  @return{the object named name or NULL if it could not be found in the object
    tree}
  @begin{short}
    Gets the object named name. Note that this function does not increment the
    reference count of the returned object.
  @end{short}

  Since 2.12"
  (builder g-object)
  (name :string))

(export 'gtk-builder-get-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_objects ()
;;;
;;; GSList * gtk_builder_get_objects (GtkBuilder *builder);
;;;
;;; Gets all objects that have been constructed by builder. Note that this
;;; function does not increment the reference counts of the returned objects.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; Returns :
;;;     a newly-allocated GSList containing all the objects constructed by the
;;;     GtkBuilder instance. It should be freed by g_slist_free()
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals ()
;;; ----------------------------------------------------------------------------

(defun gtk-builder-connect-signals (builder handlers-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[user_data]{a pointer to a structure sent in as user data to all
    signals}
  @begin{short}
    This method is a simpler variation of gtk_builder_connect_signals_full(). It
    uses GModule's introspective features (by opening the module NULL) to look
    at the application's symbol table. From here it tries to match the signal
    handler names given in the interface description with symbols in the
    application and connects the signals. Note that this function can only be
    called once, subsequent calls will do nothing.
  @end{short}

  Note that this function will not work correctly if GModule is not supported
  on the platform.

  When compiling applications for Windows, you must declare signal callbacks
  with G_MODULE_EXPORT, or they will not be put in the symbol table. On Linux
  and Unices, this is not necessary; applications should instead be compiled
  with the -Wl,--export-dynamic CFLAGS, and linked against gmodule-export-2.0.

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
 "@version{2013-2-14}
  @argument[builder]{a GtkBuilder}
  @argument[func]{the function used to connect the signals}
  @begin{short}
    This function can be thought of the interpreted language binding version of
    gtk_builder_connect_signals(), except that it does not require GModule to
    function correctly.
  @end{short}

  Since 2.12"
  (with-stable-pointer (ptr func)
    (%gtk-builder-connect-signals-full builder
                                       (callback builder-connect-func-callback)
                                       ptr)))

(export 'gtk-builder-connect-signals-full)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_set_translation_domain ()
;;;
;;; void gtk_builder_set_translation_domain (GtkBuilder *builder,
;;;                                          const gchar *domain);
;;;
;;; Sets the translation domain of builder. See "translation-domain".
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; domain :
;;;     the translation domain or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_translation_domain ()
;;;
;;; const gchar * gtk_builder_get_translation_domain (GtkBuilder *builder);
;;;
;;; Gets the translation domain of builder.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; Returns :
;;;     the translation domain. This string is owned by the builder object and
;;;     must not be modified or freed.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

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
