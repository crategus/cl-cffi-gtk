;;; ----------------------------------------------------------------------------
;;; gtk.buildable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkBuildable
;;; 
;;; Interface for objects that can be built by GtkBuilder
;;;     
;;; Synopsis
;;; 
;;;     GtkBuildable
;;;     GtkBuildableIface
;;;
;;;     gtk_buildable_set_name
;;;     gtk_buildable_get_name
;;;     gtk_buildable_add_child
;;;     gtk_buildable_set_buildable_property
;;;     gtk_buildable_construct_child
;;;     gtk_buildable_custom_tag_start
;;;     gtk_buildable_custom_tag_end
;;;     gtk_buildable_custom_finished
;;;     gtk_buildable_parser_finished
;;;     gtk_buildable_get_internal_child
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuildable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkBuildable" gtk-buildable
  (:export t
   :type-initializer "gtk_buildable_get_type"))

;;; --- gtk-buildable ----------------------------------------------------------

(setf (documentation 'gtk-buildable 'type)
 "@short{Interface for objects that can be built by GtkBuilder.}

  GtkBuildable allows objects to extend and customize thier deserialization
  from GtkBuilder UI descriptions. The interface includes methods for setting
  names and properties of objects, parsing custom tags and constructing child
  objects.
 
  The GtkBuildable interface is implemented by all widgets and many of the
  non-widget objects that are provided by GTK+. The main user of this
  interface is GtkBuilder. There should be very little need for applications
  to call any gtk_buildable_... functions.
  @begin[Note]{dictionary}
    An object only needs to implement this interface if it needs to extend the
    GtkBuilder format or run any extra routines at deserialization time
  @end{dictionary}
  @begin[Lisp Implemenation]{dictionary}
    @begin{pre}
(define-g-interface \"GtkBuildable\" gtk-buildable
  (:export t
   :type-initializer \"gtk_buildable_get_type\"))
    @end{pre}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; struct GtkBuildableIface
;;; 
;;; struct GtkBuildableIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* virtual table */
;;;   void          (* set_name)               (GtkBuildable  *buildable,
;;;                                             const gchar   *name);
;;;   const gchar * (* get_name)               (GtkBuildable  *buildable);
;;;   void          (* add_child)              (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             GObject       *child,
;;;                                             const gchar   *type);
;;;   void          (* set_buildable_property) (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             const gchar   *name,
;;;                                             const GValue  *value);
;;;   GObject *     (* construct_child)        (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             const gchar   *name);
;;;   gboolean      (* custom_tag_start)       (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             GObject       *child,
;;;                                             const gchar   *tagname,
;;;                                             GMarkupParser *parser,
;;;                                             gpointer      *data);
;;;   void          (* custom_tag_end)         (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             GObject       *child,
;;;                                             const gchar   *tagname,
;;;                                             gpointer      *data);
;;;   void          (* custom_finished)        (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             GObject       *child,
;;;                                             const gchar   *tagname,
;;;                                             gpointer       data);
;;;   void          (* parser_finished)        (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder);
;;; 
;;;   GObject *     (* get_internal_child)     (GtkBuildable  *buildable,
;;;                                             GtkBuilder    *builder,
;;;                                             const gchar   *childname);
;;; };
;;; 
;;; The GtkBuildableIface interface contains method that are necessary to allow
;;; GtkBuilder to construct an object from a GtkBuilder UI definition.
;;; 
;;; GTypeInterface g_iface;
;;;     the parent class
;;; 
;;; set_name ()
;;;     Stores the name attribute given in the GtkBuilder UI definition.
;;;     GtkWidget stores the name as object data. Implement this method if your
;;;     object has some notion of "name" and it makes sense to map the XML name
;;;     attribute to it.
;;; 
;;; get_name ()
;;;     The getter corresponding to set_name. Implement this if you implement
;;;     set_name.
;;; 
;;; add_child ()
;;;     Adds a child. The type parameter can be used to differentiate the kind
;;;     of child. GtkContainer implements this to add add a child widget to the
;;;     container, GtkNotebook uses the type to distinguish between page labels
;;;     (of type "page-label") and normal children.
;;; 
;;; set_buildable_property ()
;;;     Sets a property of a buildable object. It is normally not necessary to
;;;     implement this, g_object_set_property() is used by default. GtkWindow
;;;     implements this to delay showing itself (i.e. setting the "visible"
;;;     property) until the whole interface is created.
;;; 
;;; construct_child ()
;;;     Constructs a child of a buildable that has been specified as
;;;     "constructor" in the UI definition. GtkUIManager implements this to
;;;     reference to a widget created in a <ui> tag which is outside of the
;;;     normal GtkBuilder UI definition hierarchy. A reference to the
;;;     constructed object is returned and becomes owned by the caller.
;;; 
;;; custom_tag_start ()
;;;     Implement this if the buildable needs to parse content below <child>. To
;;;     handle an element, the implementation must fill in the parser structure
;;;     and user_data and return TRUE. GtkWidget implements this to parse
;;;     keyboard accelerators specified in <accelerator> elements. GtkContainer
;;;     implements it to map properties defined via <packing> elements to child
;;;     properties. Note that user_data must be freed in custom_tag_end or
;;;     custom_finished.
;;; 
;;; custom_tag_end ()
;;;     Called for the end tag of each custom element that is handled by the
;;;     buildable (see custom_tag_start).
;;; 
;;; custom_finished ()
;;;     Called for each custom tag handled by the buildable when the builder
;;;     finishes parsing (see custom_tag_start)
;;; 
;;; parser_finished ()
;;;     Called when a builder finishes the parsing of a UI definition. It is
;;;     normally not necessary to implement this, unless you need to perform
;;;     special cleanup actions. GtkWindow sets the "visible" property here.
;;; 
;;; get_internal_child ()
;;;     Returns an internal child of a buildable. GtkDialog implements this to
;;;     give access to its vbox, making it possible to add children to the vbox
;;;     in a UI definition. Implement this if the buildable has internal
;;      children that may need to be accessed from a UI definition.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_set_name ()
;;; 
;;; void gtk_buildable_set_name (GtkBuildable *buildable, const gchar *name);
;;; 
;;; Sets the name of the buildable object.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; name :
;;;     name to set
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_get_name ()
;;; 
;;; const gchar * gtk_buildable_get_name (GtkBuildable *buildable);
;;; 
;;; Gets the name of the buildable object.
;;; 
;;; GtkBuilder sets the name based on the the GtkBuilder UI definition used to
;;; construct the buildable.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; Returns :
;;;     the name set with gtk_buildable_set_name()
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_add_child ()
;;; 
;;; void gtk_buildable_add_child (GtkBuildable *buildable,
;;;                               GtkBuilder *builder,
;;;                               GObject *child,
;;;                               const gchar *type);
;;; 
;;; Adds a child to buildable. type is an optional string describing how the
;;; child should be added.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder
;;; 
;;; child :
;;;     child to add
;;; 
;;; type :
;;;     kind of child or NULL
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_set_buildable_property ()
;;; 
;;; void gtk_buildable_set_buildable_property (GtkBuildable *buildable,
;;;                                            GtkBuilder *builder,
;;;                                            const gchar *name,
;;;                                            const GValue *value);
;;; 
;;; Sets the property name name to value on the buildable object.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder
;;; 
;;; name :
;;;     name of property
;;; 
;;; value :
;;;     value of property
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_construct_child ()
;;; 
;;; GObject * gtk_buildable_construct_child (GtkBuildable *buildable,
;;;                                          GtkBuilder *builder,
;;;                                          const gchar *name);
;;; 
;;; Constructs a child of buildable with the name name.
;;; 
;;; GtkBuilder calls this function if a "constructor" has been specified in the
;;; UI definition.
;;; 
;;; buildable :
;;;     A GtkBuildable
;;; 
;;; builder :
;;;     GtkBuilder used to construct this object
;;; 
;;; name :
;;;     name of child to construct
;;; 
;;; Returns :
;;;     the constructed child
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_tag_start ()
;;; 
;;; gboolean gtk_buildable_custom_tag_start (GtkBuildable *buildable,
;;;                                          GtkBuilder *builder,
;;;                                          GObject *child,
;;;                                          const gchar *tagname,
;;;                                          GMarkupParser *parser,
;;;                                          gpointer *data);
;;; 
;;; This is called for each unknown element under <child>.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder used to construct this object
;;; 
;;; child :
;;;     child object or NULL for non-child tags
;;; 
;;; tagname :
;;;     name of tag
;;; 
;;; parser :
;;;     a GMarkupParser structure to fill in
;;; 
;;; data :
;;;     return location for user data that will be passed in to parser
;;;     functions
;;; 
;;; Returns :
;;;     TRUE if a object has a custom implementation, FALSE if it doesn't.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_tag_end ()
;;; 
;;; void gtk_buildable_custom_tag_end (GtkBuildable *buildable,
;;;                                    GtkBuilder *builder,
;;;                                    GObject *child,
;;;                                    const gchar *tagname,
;;;                                    gpointer *data);
;;; 
;;; This is called at the end of each custom element handled by the buildable.
;;; 
;;; buildable :
;;;     A GtkBuildable
;;; 
;;; builder :
;;;     GtkBuilder used to construct this object
;;; 
;;; child :
;;;     child object or NULL for non-child tags
;;; 
;;; tagname :
;;;     name of tag
;;; 
;;; data :
;;;     user data that will be passed in to parser functions
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_finished ()
;;; 
;;; void gtk_buildable_custom_finished (GtkBuildable *buildable,
;;;                                     GtkBuilder *builder,
;;;                                     GObject *child,
;;;                                     const gchar *tagname,
;;;                                     gpointer data);
;;; 
;;; This is similar to gtk_buildable_parser_finished() but is called once for
;;; each custom tag handled by the buildable.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder
;;; 
;;; child :
;;;     child object or NULL for non-child tags
;;; 
;;; tagname :
;;;     the name of the tag
;;; 
;;; data :
;;;     user data created in custom_tag_start
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parser_finished ()
;;; 
;;; void gtk_buildable_parser_finished (GtkBuildable *buildable,
;;;                                     GtkBuilder *builder);
;;; 
;;; Called when the builder finishes the parsing of a GtkBuilder UI definition.
;;; Note that this will be called once for each time gtk_builder_add_from_file()
;;; or gtk_builder_add_from_string() is called on a builder.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_get_internal_child ()
;;; 
;;; GObject * gtk_buildable_get_internal_child (GtkBuildable *buildable,
;;;                                             GtkBuilder *builder,
;;;                                             const gchar *childname);
;;; 
;;; Get the internal child called childname of the buildable object.
;;; 
;;; buildable :
;;;     a GtkBuildable
;;; 
;;; builder :
;;;     a GtkBuilder
;;; 
;;; childname :
;;;     name of child
;;; 
;;; Returns :
;;;     the internal child of the buildable object
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.buildable.lisp -----------------------------------------
