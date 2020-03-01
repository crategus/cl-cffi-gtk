;;; ----------------------------------------------------------------------------
;;; gtk.widget-path.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
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
;;; GtkWidgetPath
;;;
;;;     Widget path abstraction
;;;
;;; Types and Values
;;;
;;;     GtkWidgetPath
;;;     GtkRegionFlags                               from gtk-style-context.lisp
;;;
;;; Functions
;;;
;;;     gtk_widget_path_append_type
;;;     gtk_widget_path_append_with_siblings
;;;     gtk_widget_path_append_for_widget
;;;     gtk_widget_path_copy
;;;     gtk_widget_path_ref
;;;     gtk_widget_path_unref
;;;     gtk_widget_path_free
;;;     gtk_widget_path_get_object_type
;;;     gtk_widget_path_has_parent
;;;     gtk_widget_path_is_type
;;;     gtk_widget_path_iter_add_class
;;;     gtk_widget_path_iter_add_region
;;;     gtk_widget_path_iter_clear_classes
;;;     gtk_widget_path_iter_clear_regions
;;;     gtk_widget_path_iter_get_name
;;;     gtk_widget_path_iter_get_object_name
;;;     gtk_widget_path_iter_get_object_type
;;;     gtk_widget_path_iter_get_siblings
;;;     gtk_widget_path_iter_get_sibling_index
;;;     gtk_widget_path_iter_get_state
;;;     gtk_widget_path_iter_has_class
;;;     gtk_widget_path_iter_has_name
;;;     gtk_widget_path_iter_has_qclass
;;;     gtk_widget_path_iter_has_qname
;;;     gtk_widget_path_iter_has_qregion
;;;     gtk_widget_path_iter_has_region
;;;     gtk_widget_path_iter_list_classes
;;;     gtk_widget_path_iter_list_regions
;;;     gtk_widget_path_iter_remove_class
;;;     gtk_widget_path_iter_remove_region
;;;     gtk_widget_path_iter_set_name
;;;     gtk_widget_path_iter_set_object_name
;;;     gtk_widget_path_iter_set_object_type
;;;     gtk_widget_path_iter_set_state
;;;     gtk_widget_path_length
;;;     gtk_widget_path_new
;;;     gtk_widget_path_prepend_type
;;;     gtk_widget_path_to_string
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkRegionFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkRegionFlags" gtk-region-flags
  (:export t
   :type-initializer "gtk_region_flags_get_type")
  (:even #.(ash 1 0))
  (:odd #.(ash 1 1))
  (:first #.(ash 1 2))
  (:last #.(ash 1 3))
  (:only #.(ash 1 4))
  (:sorted #.(ash 1 5)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-region-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-region-flags atdoc:*external-symbols*)
 "@version{2020-2-28}
  @begin{short}
    Describes a region within a widget.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkRegionFlags\" gtk-region-flags
  (:export t
   :type-initializer \"gtk_region_flags_get_type\")
  (:even #.(ash 1 0))
  (:odd #.(ash 1 1))
  (:first #.(ash 1 2))
  (:last #.(ash 1 3))
  (:only #.(ash 1 4))
  (:sorted #.(ash 1 5)))
  @end{pre}
  @begin[code]{table}
    @entry[:even]{Region has an even number within a set.}
    @entry[:odd]{Region has an odd number within a set.}
    @entry[:first]{Region is the first one within a set.}
    @entry[:last]{Region is the last one within a set.}
    @entry[:only]{Region is the only one within a set.}
    @entry[:sorted]{Region is part of a sorted area.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; GtkWidgetPath
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gtk-widget-path "GtkWidgetPath"
  :alloc (gtk-widget-path-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-widget-path atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-widget-path 'type)
 "@version{2020-2-28}
  @begin{short}
    @sym{gtk-widget-path} is a boxed type that represents a widget hierarchy
    from the topmost widget, typically a toplevel, to any child.
  @end{short}
  This widget path abstraction is used in @class{gtk-style-context} on behalf of
  the real widget in order to query style information.

  If you are using GTK+ widgets, you probably will not need to use this API
  directly, as there is the function @fun{gtk-widget-get-path}, and the style
  context returned by the function @fun{gtk-widget-get-style-context} will be
  automatically updated on widget hierarchy changes.
  @begin[Example]{dictionary}
    Defining a button within a window:
    @begin{pre}
(let ((path (gtk-widget-path-new)))
  (gtk-widget-path-append-type path \"GtkWindow\")
  (gtk-widget-path-append-type path \"GtkButton\")
  ... )
    @end{pre}
    Although more complex information, such as widget names, or different
    classes (property that may be used by other widget types) and intermediate
    regions may be included:

    Defining the first tab widget in a notebook:
    @begin{pre}
(let ((path (gtk-widget-path-new)))

  (gtk-widget-path-iter-add-region
      path
      (gtk-widget-path-append-type path \"GtkNotebook\")
      \"tab\"
      '(:even :first))

  (gtk-widget-path-iter-set-name
      path
      (gtk-widget-path-append-type path \"GtkLabel\")
      \"first tab label\")

  ... )
    @end{pre}
    All this information will be used to match the style information that
    applies to the described widget.
  @end{dictionary}
  @see-class{gtk-style-context}")

(export 'gtk-widget-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_append_type" gtk-widget-path-append-type) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[type]{the @class{g-type} of the widget to append}
  @return{The position of type @code{:int} where the element was inserted.}
  @begin{short}
    Appends a widget type to the widget hierarchy represented by @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (type g-type))

(export 'gtk-widget-path-append-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_with_siblings ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_append_with_siblings"
           gtk-widget-path-append-with-siblings) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[siblings]{a widget path of type @class{gtk-widget-path} describing
    a list of siblings. This path may not contain any siblings itself and it
    must not be modified afterwards.}
  @argument[sibling-index]{index of type @code{:uint} into siblings for where
    the added element is positioned}
  @return{The position of type @code{:int} where the element was inserted.}
  @begin{short}
    Appends a widget type with all its siblings to the widget hierarchy
    represented by @arg{path}.
  @end{short}
  Using this function instead of the function @fun{gtk-widget-path-append-type}
  will allow the CSS theming to use sibling matches in selectors and apply
  @code{:nth-child()} pseudo classes. In turn, it requires a lot more care in
  widget implementations as widgets need to make sure to call
  @fun{gtk-widget-reset-style} on all involved widgets when the siblings path
  changes.
  @see-class{gtk-widget-path}
  @see-function{gtk-widget-reset-style}
  @see-function{gtk-widget-append-type}"
  (path (g-boxed-foreign gtk-widget-path))
  (siblings (g-boxed-foreign gtk-widget-path))
  (sibling-indes :uint))

(export 'gtk-widget-path-append-with-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_for_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_append_for_widget" gtk-widget-path-append-for-widget)
    :int
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[widget]{the @class{gtk-widget} to append to the widget path}
  @return{The position of type @code{:int} where the data was inserted.}
  @begin{short}
    Appends the data from @arg{widget} to the widget hierarchy represented by
    @arg{path}.
  @end{short}
  This function is a shortcut for adding information from a widget to the given
  path. This includes setting the name or adding the style classes from a
  widget.
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (widget (g-object gtk-widget)))

(export 'gtk-widget-path-append-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_copy ()
;;;
;;; GtkWidgetPath * gtk_widget_path_copy (const GtkWidgetPath *path);
;;;
;;; Returns a copy of path
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Returns :
;;;     a copy of path. [transfer full]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_ref ()
;;;
;;; GtkWidgetPath * gtk_widget_path_ref (GtkWidgetPath *path);
;;;
;;; Increments the reference count on path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Returns :
;;;     path itself.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_unref ()
;;;
;;; void gtk_widget_path_unref (GtkWidgetPath *path);
;;;
;;; Decrements the reference count on path, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_free ()
;;;
;;; void gtk_widget_path_free (GtkWidgetPath *path);
;;;
;;; Decrements the reference count on path, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_get_object_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_get_object_type" gtk-widget-path-get-object-type)
    g-type
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @return{The @class{g-type} of the object.}
  @begin{short}
    Returns the topmost object type.
  @end{short}
  That is, the object type this path is representing.
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path)))

(export 'gtk-widget-path-get-object-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_has_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_has_parent" gtk-widget-path-has-parent) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[type]{the @class{g-type} of the widget to check in parents}
  @return{@arg{True} if any parent is of type @arg{type}.}
  @begin{short}
    Returns @arg{true} if any of the parents of the widget represented in
    @arg{path} is of type @arg{type}, or any subtype of it.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (type g-type))

(export 'gtk-widget-has-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_is_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_is_type" gtk-widget-path-is-type) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[type]{the @class{g-type} of the widget to match}
  @return{@arg{True} if the widget represented by @arg{path} is of type
    @arg{type}}
  @begin{short}
    Returns @arg{true} if the widget type represented by this path is
    @arg{type}, or a subtype of it.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (type g-type))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_add_class" gtk-widget-path-iter-add-class) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with a class name}
  @begin{short}
    Adds the class name to the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  See the function @fun{gtk-style-context-add-class}.
  @see-class{gtk-widget-path}
  @see-function{gtk-style-context-add-class}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

(export 'gtk-widget-path-iter-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_add_region" gtk-widget-path-iter-add-region)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with the region name}
  @argument[flags]{flags of type @symbol{gtk-region-flags} affecting the region}
  @begin{short}
    Adds the region name to the widget at position @arg{pos} in the hierarchy
    defined in path.
  @end{short}
  See the function @fun{gtk-style-context-add-region}.
  @begin[Note]{dictionary}
    Region names must only contain lowercase letters and '-', starting always
    with a lowercase letter.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-widget-path-iter-add-region} has been deprecated since
    version 3.14 and should not be used in newly-written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk-widget-path}
  @see-symbol{gtk-region-flags}
  @see-function{gtk-style-context-add-region}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string)
  (flags gtk-region-flags))

(export 'gtk-widget-path-iter-add-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_classes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_clear_classes"
           gtk-widget-path-iter-clear-classes) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @begin{short}
    Removes all classes from the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (iter :int))

(export 'gtk-widget-path-iter-clear-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_regions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_clear_regions"
           gtk-widget-path-iter-clear-regions) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @begin{short}
    Removes all regions from the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-widget-path-iter-clear-regions} has been deprecated
    since version 3.14 and should not be used in newly-written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-clear-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_get_name" gtk-widget-path-iter-get-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the widget name for,
    -1 for the path head}
  @return{The widget name, or @code{nil} if none was set.}
  @begin{short}
    Returns the name corresponding to the widget found at the position @arg{pos}
    in the widget hierarchy defined by @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_name ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_widget_path_iter_get_object_name"
           gtk-widget-path-iter-get-object-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the oject name for,
    -1 for the path head}
  @return{The object name, or @code{nil} if none was set.}
  @begin{short}
    Returns the object name that is at position @arg{pos} in the widget
    hierarchy defined in @arg{path}.
  @end{short}

  Since 3.20
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

#+gtk-3-20
(export 'gtk-widget-path-iter-get-object-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_get_object_type"
           gtk-widget-path-iter-get-object-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the oject type for,
    -1 for the path head}
  @return{The @class{g-type} of a widget.}
  @begin{short}
    Returns the @class{g-type} of the object that is at position @arg{pos} in
    the widget hierarchy defined in @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-get-object-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_siblings ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_get_siblings" gtk-widget-path-iter-get-siblings)
    (g-boxed-foreign gtk-widget-path)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the siblings for,
    -1 for the path head}
  @return{@arg{Nil} or the list of siblings for the element at @arg{pos}.}
  @begin{short}
    Returns the list of siblings for the element at @arg{pos}.
  @end{short}
  If the element was not added with siblings, @code{nil} is returned.
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-get-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_sibling_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_get_sibling_index"
           gtk-widget-path-iter-get-sibling-index) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the sibling index for,
    -1 for the path head}
  @return{0 or the index of type @code{:uint} into the list of siblings for the
    element at @arg{pos}.}
  @begin{short}
    Returns the index into the list of siblings for the element at @arg{pos} as
    returned by the function @fun{gtk-widget-path-iter-get-siblings}.
  @end{short}
  If that function would return @code{nil} because the element at @arg{pos} has
  no siblings, this function will return 0.
  @see-class{gtk-widget-path}
  @see-function{gtk-widget-path-iter-get-siblings}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-get-sibling-index)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_state ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun ("gtk_widget_path_iter_get_state" gtk-widget-path-iter-get-state)
    gtk-state-flags
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to get the state for,
    -1 for the path head}
  @return{The state flags of type @symbol{gtk-state-flags}.}
  @begin{short}
    Returns the state flags corresponding to the widget found at the position
    @arg{pos} in the widget hierarchy defined by @arg{path}.
  @end{short}

  Since 3.14
  @see-class{gtk-widget-path}
  @see-symbol{gtk-state-flags}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

#+gtk-3-14
(export 'gtk-widget-path-iter-get-state)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_has_class" gtk-widget-path-iter-has-class)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to query, -1 for the path head}
  @argument[name]{a @code{:string} with a class name}
  @return{@arg{True} if the class name is defined for the widget at @arg{pos}.}
  @begin{short}
    Returns @arg{true} if the widget at position @arg{pos} has the class name
    defined, @arg{false} otherwise.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

(export 'gtk-widget-path-iter-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_has_name" gtk-widget-path-iter-has-name)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to query, -1 for the path head}
  @argument[name]{a @code{:string} with a widget name}
  @return{@arg{True} if the widget at @arg{pos} has this name.}
  @begin{short}
    Returns @arg{true} if the widget at position @arg{pos} has the name
    @arg{name}, @arg{false} otherwise.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

(export 'gtk-widget-path-iter-has-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qclass ()
;;;
;;; gboolean gtk_widget_path_iter_has_qclass (const GtkWidgetPath *path,
;;;                                           gint pos,
;;;                                           GQuark qname);
;;;
;;; See gtk_widget_path_iter_has_class(). This is a version that operates with
;;; GQuarks.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     class name as a GQuark
;;;
;;; Returns :
;;;     TRUE if the widget at pos has the class defined.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qname ()
;;;
;;; gboolean gtk_widget_path_iter_has_qname (const GtkWidgetPath *path,
;;;                                          gint pos,
;;;                                          GQuark qname);
;;;
;;; See gtk_widget_path_iter_has_name(). This is a version that operates on
;;; GQuarks.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     widget name as a GQuark
;;;
;;; Returns :
;;;     TRUE if the widget at pos has this name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qregion ()
;;;
;;; gboolean gtk_widget_path_iter_has_qregion (const GtkWidgetPath *path,
;;;                                            gint pos,
;;;                                            GQuark qname,
;;;                                            GtkRegionFlags *flags);
;;;
;;; See gtk_widget_path_iter_has_region(). This is a version that operates with
;;; GQuarks.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_has_qregion has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; The use of regions is deprecated.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     region name as a GQuark
;;;
;;; flags :
;;;     return location for the region flags. [out]
;;;
;;; Returns :
;;;     TRUE if the widget at pos has the region defined.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_has_region" %gtk-widget-path-iter-has-region)
    :boolean
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string)
  (flags :pointer))

(defun gtk-widget-path-iter-has-region (path pos name)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to query, -1 for the path head}
  @argument[name]{a @code{:string} with a region name}
  @return{Returns the region flags of type @symbol{gtk-region-flags}.}
  @begin{short}
    Returns the region flags corresponding to the widget found at the position
    @arg{pos} in the widget hierarchy defined by @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-widget-path-iter-has-region} has been deprecated since
    version 3.14 and should not be used in newly-written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk-widget-path}"
  (with-foreign-object (flags 'gtk-region-flags)
    (%gtk-widget-path-iter-has-region path pos name flags)
    (mem-ref flags 'gtk-region-flags)))

(export 'gtk-widget-path-iter-has-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_classes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_list_classes" gtk-widget-path-iter-list-classes)
    g-strv
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to query, -1 for the path head}
  @return{Returns a list of strings with the class names.}
  @begin{short}
    Returns a list of strings with all the class names defined for the widget at
    position @arg{pos} in the hierarchy defined in @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-list-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_regions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_list_regions" gtk-widget-path-iter-list-regions)
    g-strv
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to query, -1 for the path head}
  @return{Returns a list of strings with the region names.}
  @begin{short}
    Returns a list with all the region names defined for the widget at position
    @arg{pos} in the hierarchy defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-widget-path-iter-list-regions} has been deprecated
    since version 3.14 and should not be used in newly-written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int))

(export 'gtk-widget-path-iter-list-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_remove_class" gtk-widget-path-iter-remove-class)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with a class name}
  @begin{short}
    Removes the class name from the widget at position @arg{pos} in the
    hierarchy defined in @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
 (path (g-boxed-foreign gtk-widget-path))
 (pos :int)
 (name :string))

(export 'gtk-widget-path-iter-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_remove_region"
           gtk-widget-path-iter-remove-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with a region name}
  @begin{short}
    Removes the region name from the widget at position @arg{pos} in the
    hierarchy defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-widget-path-iter-remove-region} has been deprecated
    since version 3.14 and should not be used in newly-written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

(export 'gtk-widget-path-iter-remove-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_set_name" gtk-widget-path-iter-set-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with the widget name}
  @begin{short}
    Sets the widget name for the widget found at position @arg{pos} in the
    widget hierarchy defined by @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

(export 'gtk-widget-path-iter-set-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_object_name ()
;;; ----------------------------------------------------------------------------

#+gtk-3-20
(defcfun ("gtk_widget_path_iter_set_object_name"
           gtk-widget-path-iter-set-object-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[name]{a @code{:string} with the object name to set or @code{nil}
    to unset}
  @begin{short}
    Sets the object name for a given position in the widget hierarchy defined
    by @arg{path}.
  @end{short}
  When set, the object name overrides the object type when matching CSS.

  Since 3.20
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (name :string))

#+gtk-3-20
(export 'gtk-widget-path-iter-set-object-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_object_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_iter_set_object_type"
           gtk-widget-path-iter-set-object-type) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[type]{the object @class{g-type} to set}
  @begin{short}
    Sets the object type for a given position in the widget hierarchy defined
    by @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (type g-type))

(export 'gtk-widget-path-iter-set-object-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_state ()
;;; ----------------------------------------------------------------------------

#+gtk-3-14
(defcfun ("gtk_widget_path_iter_set_state" gtk-widget-path-iter-set-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-29}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[pos]{position of type @code{:int} to modify, -1 for the path head}
  @argument[state]{the @symbol{gtk-state-flags} to set or unset}
  @begin{short}
    Sets the widget name for the widget found at position @arg{pos} in the
    widget hierarchy defined by @arg{path}.
  @end{short}

  If you want to update just a single state flag, you need to do this manually,
  as this function updates all state flags.

  Since 3.14
  @begin[Example]{dictionary}
    Setting more flags
    @begin{pre}
(let ((flags (gtk-widget-path-iter-get-state path pos)))
  (gtk-widget-path-iter-set-state path
                                  pos
                                  (union flags '(:dir-ltr :selected))))
    @end{pre}
    Unsetting a flag
    @begin{pre}
(let ((flags (gtk-widget-path-iter-get-state path pos)))
  (gtk-widget-path-iter-set-state path
                                  pos
                                  (set-difference flags '(:active))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (pos :int)
  (state gtk-state-flags))

#+gtk-3-14
(export 'gtk-widget-path-iter-set-state)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_length" gtk-widget-path-length) :int
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @return{The number of elements in the path.}
  @begin{short}
    Returns the number of @class{gtk-widget} types between the represented
    widget and its topmost container.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path)))

(export 'gtk-widget-path-length)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_new" gtk-widget-path-new)
    (g-boxed-foreign gtk-widget-path)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @return{A newly created, empty, @class{gtk-widget-path}.}
  @short{Returns an empty widget path.}
  @see-class{gtk-widget-path}")

(export 'gtk-widget-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_prepend_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_prepend_type" gtk-widget-path-prepend-type) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @argument[type]{widget type to prepend}
  @begin{short}
    Prepends a widget type to the widget hierachy represented by @arg{path}.
  @end{short}
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path))
  (type g-type))

(export 'gtk-widget-path-prepend-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_to_string" gtk-widget-path-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-2-28}
  @argument[path]{a @class{gtk-widget-path} structure}
  @return{A new string describing @arg{path}.}
  @begin{short}
    Dumps the widget path into a string representation.
  @end{short}
  It tries to match the CSS style as closely as possible (Note that there might
  be paths that cannot be represented in CSS).

  The main use of this code is for debugging purposes.
  @see-class{gtk-widget-path}"
  (path (g-boxed-foreign gtk-widget-path)))

(export 'gtk-widget-path-to-string)

;;; --- End of file gtk.widget-path.lisp ---------------------------------------
