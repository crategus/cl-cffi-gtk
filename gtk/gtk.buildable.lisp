;;; ----------------------------------------------------------------------------
;;; gtk.buildable.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; GtkBuildable
;;;
;;;     Interface for objects that can be built by GtkBuilder
;;;
;;; Types and Values
;;;
;;;     GtkBuildable
;;;
;;; Functions
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
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuildable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkBuildable" gtk-buildable
  (:export t
   :type-initializer "gtk_buildable_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-buildable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-buildable 'type)
 "@version{2020-11-27}
  @begin{short}
    Interface for objects that can be built by a @class{gtk-builder} UI
    description.
  @end{short}

  The @sym{gtk-buildable} interface allows objects to extend and customize
  their deserialization from @class{gtk-builder} UI descriptions. The interface
  includes methods for setting names and properties of objects, parsing custom
  tags and constructing child objects.

  The @sym{gtk-buildable} interface is implemented by all widgets and many of
  the non-widget objects that are provided by GTK+. The main user of this
  interface is the @class{gtk-builder} class. There should be very little need
  for applications to call any functions from the @sym{gtk-buildable} interface.
  @begin[Note]{dictionary}
    An object only needs to implement this interface if it needs to extend the
    @class{gtk-builder} format or run any extra routines at deserialization
    time.
  @end{dictionary}
  @see-class{gtk-builder}
  @see-class{gtk-buildable}")

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_set_name ()
;;; gtk_buildable_get_name () -> gtk-buildable-name
;;; ----------------------------------------------------------------------------

(defun (setf gtk-buildable-name) (name buildable)
  (foreign-funcall "gtk_buildable_set_name"
                   (g-object gtk-buildable) buildable
                   :string name
                   :void)
  name)

(defcfun ("gtk_buildable_get_name" gtk-buildable-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @syntax[]{(gtk-buildable-name buildable) => name}
  @syntax[]{(setf (gtk-buildable-name buildable) name)}
  @argument[buildable]{a @class{gtk-buildable} object}
  @argument[name]{a string with the name to set}
  @begin{short}
    Accessor of the name of the buildable widget.
  @end{short}

  The function @sym{gtk-buildable-name} gets the name of the buildable widget.
  The function @sym{(setf gtk-buildable-name)} sets the name of the buildable
  widget.

  @class{gtk-builder} sets the name based on the the @class{gtk-builder} UI
  definition used to construct the buildable.
  @see-class{gtk-buildable}
  @see-class{gtk-builder}"
  (buildable (g-object gtk-buildable)))

(export 'gtk-buildable-name)

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_add_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_buildable_add_child" %gtk-buildable-add-child) :void
  (buildable (g-object gtk-buildable))
  (builder (g-object gtk-builder))
  (child g-object)
  (child-type :string))

(defun gtk-buildable-add-child (buildable builder child child-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @argument[buildable]{a @class{gtk-buildable} widget}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[child]{a @class{g-object} child to add}
  @argument[child-type]{a string with the kind of child or @code{nil}}
  @begin{short}
    Adds a child to the buildable widget.
  @end{short}
  The argument @arg{child-type} is an optional string describing how the child
  should be added.
  @see-class{gtk-buildable}
  @see-class{gtk-builder}
  @see-class{g-object}"
  (if child-type
      (%gtk-buildable-add-child buildable builder child child-type)
      (%gtk-buildable-add-child buildable builder Child (null-pointer))))

(export 'gtk-buildable-add-child)

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
;;;     TRUE if a object has a custom implementation, FALSE if it does not.
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
;;; gtk_buildable_get_internal_child () -> gtk-buildable-internal-child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_buildable_get_internal_child" gtk-buildable-internal-child)
    g-object
 #+cl-cffi-gtk-documentation
 "@version{2020-11-27}
  @argument[buildable]{a @class{gtk-buildable} widget}
  @argument[builder]{a @class{gtk-builder} object}
  @argument[childname]{a string with the name of the child widget}
  @return{The internal child widget of the buildable widget.}
  @begin{short}
    Gets the internal child widget called @arg{childname} of the buildable
    widget.
  @end{short}
  @see-class{gtk-buildable}
  @see-class{gtk-builder}"
  (buildable (g-object gtk-buildable))
  (builder (g-object gtk-builder))
  (childname :string))

(export 'gtk-buildable-internal-child)

;;; --- End of file gtk.buildable.lisp -----------------------------------------
