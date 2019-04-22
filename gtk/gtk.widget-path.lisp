;;; ----------------------------------------------------------------------------
;;; gtk.widget-path.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2019 Dieter Kaiser
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
;;; GtkWidgetPath
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gtk-widget-path "GtkWidgetPath"
  :alloc (gtk-widget-path-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-widget-path atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-widget-path 'type)
 "@version{2013-5-25}
  @begin{short}
    @sym{gtk-widget-path} is a boxed type that represents a widget hierarchy
    from the topmost widget, typically a toplevel, to any child. This widget
    path abstraction is used in @class{gtk-style-context} on behalf of the real
    widget in order to query style information.
  @end{short}

  If you are using GTK+ widgets, you probably will not need to use this API
  directly, as there is the function @fun{gtk-widget-get-path}, and the style
  context returned by the function @fun{gtk-widget-get-style-context} will be
  automatically updated on widget hierarchy changes.

  The widget path generation is generally simple:

  @b{Example:} Defining a button within a window
  @begin{pre}
 {
   GtkWidgetPath *path;

   path = gtk_widget_path_new ();
   gtk_widget_path_append_type (path, GTK_TYPE_WINDOW);
   gtk_widget_path_append_type (path, GTK_TYPE_BUTTON);
 @}
  @end{pre}
  Although more complex information, such as widget names, or different
  classes (property that may be used by other widget types) and intermediate
  regions may be included:

  @b{Example:} Defining the first tab widget in a notebook
  @begin{pre}
 {
   GtkWidgetPath *path;
   guint pos;

   path = gtk_widget_path_new ();

   pos = gtk_widget_path_append_type (path, GTK_TYPE_NOTEBOOK);
   gtk_widget_path_iter_add_region (path, pos, \"tab\",
                                    GTK_REGION_EVEN | GTK_REGION_FIRST);

   pos = gtk_widget_path_append_type (path, GTK_TYPE_LABEL);
   gtk_widget_path_iter_set_name (path, pos, \"first tab label\");
 @}
  @end{pre}
  All this information will be used to match the style information that
  applies to the described widget.")

(export 'gtk-widget-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_type ()
;;;
;;; gint gtk_widget_path_append_type (GtkWidgetPath *path, GType type);
;;;
;;; Appends a widget type to the widget hierarchy represented by path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; type :
;;;     widget type to append
;;;
;;; Returns :
;;;     the position where the element was inserted
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_with_siblings ()
;;;
;;; gint gtk_widget_path_append_with_siblings (GtkWidgetPath *path,
;;;                                            GtkWidgetPath *siblings,
;;;                                            guint sibling_index);
;;;
;;; Appends a widget type with all its siblings to the widget hierarchy
;;; represented by path. Using this function instead of
;;; gtk_widget_path_append_type() will allow the CSS theming to use sibling
;;; matches in selectors and apply :nth-child() pseudo classes. In turn, it
;;; requires a lot more care in widget implementations as widgets need to make
;;; sure to call gtk_widget_reset_style() on all involved widgets when the
;;; siblings path changes.
;;;
;;; path :
;;;     the widget path to append to
;;;
;;; siblings :
;;;     a widget path describing a list of siblings. This path may not contain
;;;     any siblings itself and it must not be modified afterwards.
;;;
;;; sibling_index :
;;;     index into siblings for where the added element is positioned.
;;;
;;; Returns :
;;;     the position where the element was inserted.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_for_widget ()
;;;
;;; gint gtk_widget_path_append_for_widget (GtkWidgetPath *path,
;;;                                         GtkWidget *widget);
;;;
;;; Appends the data from widget to the widget hierarchy represented by path.
;;; This function is a shortcut for adding information from widget to the given
;;; path. This includes setting the name or adding the style classes from
;;; widget.
;;;
;;; path :
;;;     a widget path
;;;
;;; widget :
;;;     the widget to append to the widget path
;;;
;;; Returns :
;;;     the position where the data was inserted
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

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
;;;
;;; GType gtk_widget_path_get_object_type (const GtkWidgetPath *path);
;;;
;;; Returns the topmost object type, that is, the object type this path is
;;; representing.
;;;
;;; path :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     The object type
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_has_parent ()
;;;
;;; gboolean gtk_widget_path_has_parent (const GtkWidgetPath *path,
;;;                                      GType type);
;;;
;;; Returns TRUE if any of the parents of the widget represented in path is of
;;; type type, or any subtype of it.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; type :
;;;     widget type to check in parents
;;;
;;; Returns :
;;;     TRUE if any parent is of type type
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_is_type ()
;;;
;;; gboolean gtk_widget_path_is_type (const GtkWidgetPath *path,
;;;                                   GType type);
;;;
;;; Returns TRUE if the widget type represented by this path is type, or a
;;; subtype of it.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; type :
;;;     widget type to match
;;;
;;; Returns :
;;;     TRUE if the widget represented by path is of type type
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_class ()
;;;
;;; void gtk_widget_path_iter_add_class (GtkWidgetPath *path,
;;;                                      gint pos,
;;;                                      const gchar *name);
;;;
;;; Adds the class name to the widget at position pos in the hierarchy defined
;;; in path. See gtk_style_context_add_class().
;;;
;;; path :
;;;     a GtkWidget
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     a class name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_region ()
;;;
;;; void gtk_widget_path_iter_add_region (GtkWidgetPath *path,
;;;                                       gint pos,
;;;                                       const gchar *name,
;;;                                       GtkRegionFlags flags);
;;;
;;; Adds the region name to the widget at position pos in the hierarchy defined
;;; in path. See gtk_style_context_add_region().
;;;
;;; Note
;;;
;;; Region names must only contain lowercase letters and '-', starting always
;;; with a lowercase letter.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_add_region has been deprecated since version 3.14 and
;;; should not be used in newly-written code.
;;;
;;; The use of regions is deprecated.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     region name
;;;
;;; flags :
;;;     flags affecting the region
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_classes ()
;;;
;;; void gtk_widget_path_iter_clear_classes (GtkWidgetPath *path, gint pos);
;;;
;;; Removes all classes from the widget at position pos in the hierarchy defined
;;; in path.
;;;
;;; path :
;;;     a GtkWidget
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_regions ()
;;;
;;; void gtk_widget_path_iter_clear_regions (GtkWidgetPath *path, gint pos);
;;;
;;; Removes all regions from the widget at position pos in the hierarchy defined
;;; in path.
;;;
;;; Warningt
;;;
;;; gtk_widget_path_iter_clear_regions has been deprecated since version 3.14
;;; and should not be used in newly-written code.
;;;
;;; The use of regions is deprecated.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_name ()
;;;
;;; const gchar * gtk_widget_path_iter_get_name (const GtkWidgetPath *path,
;;;                                              gint pos);
;;;
;;; Returns the name corresponding to the widget found at the position pos in
;;; the widget hierarchy defined by path
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the widget name for, -1 for the path head
;;;
;;; Returns :
;;;     The widget name, or NULL if none was set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_name ()
;;;
;;; const char *
;;; gtk_widget_path_iter_get_object_name (const GtkWidgetPath *path,
;;;                                       gint pos);
;;;
;;; Returns the object name that is at position pos in the widget hierarchy
;;; defined in path .
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the object name for, -1 for the path head
;;;
;;; Returns :
;;;     the name or NULL.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_type ()
;;;
;;; GType gtk_widget_path_iter_get_object_type (const GtkWidgetPath *path,
;;;                                             gint pos);
;;;
;;; Returns the object GType that is at position pos in the widget hierarchy
;;; defined in path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the object type for, -1 for the path head
;;;
;;; Returns :
;;;     a widget type
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_siblings ()
;;;
;;; const GtkWidgetPath * gtk_widget_path_iter_get_siblings
;;;                                                  (const GtkWidgetPath *path,
;;;                                                   gint pos);
;;;
;;; Returns the list of siblings for the element at pos. If the element was not
;;; added with siblings, NULL is returned.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the siblings for, -1 for the path head
;;;
;;; Returns :
;;;     NULL or the list of siblings for the element at pos.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_sibling_index ()
;;;
;;; guint gtk_widget_path_iter_get_sibling_index (const GtkWidgetPath *path,
;;;                                               gint pos);
;;;
;;; Returns the index into the list of siblings for the element at pos as
;;; returned by gtk_widget_path_iter_get_siblings(). If that function would
;;; return NULL because the element at pos has no siblings, this function will
;;; return 0.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the sibling index for, -1 for the path head
;;;
;;; Returns :
;;;     0 or the index into the list of siblings for the element at pos.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_state ()
;;;
;;; GtkStateFlags
;;; gtk_widget_path_iter_get_state (const GtkWidgetPath *path, gint pos);
;;;
;;; Returns the state flags corresponding to the widget found at the position
;;; pos in the widget hierarchy defined by path
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to get the state for, -1 for the path head
;;;
;;; Returns :
;;;     The state flags
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_class ()
;;;
;;; gboolean gtk_widget_path_iter_has_class (const GtkWidgetPath *path,
;;;                                          gint pos,
;;;                                          const gchar *name);
;;;
;;; Returns TRUE if the widget at position pos has the class name defined,
;;; FALSE otherwise.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; name :
;;;     class name
;;;
;;; Returns :
;;;     TRUE if the class name is defined for the widget at pos
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_name ()
;;;
;;; gboolean gtk_widget_path_iter_has_name (const GtkWidgetPath *path,
;;;                                         gint pos,
;;;                                         const gchar *name);
;;;
;;; Returns TRUE if the widget at position pos has the name name,
;;; FALSE otherwise.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; name :
;;;     a widget name
;;;
;;; Returns :
;;;     TRUE if the widget at pos has this name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

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
;;;
;;; gboolean gtk_widget_path_iter_has_region (const GtkWidgetPath *path,
;;;                                           gint pos,
;;;                                           const gchar *name,
;;;                                           GtkRegionFlags *flags);
;;;
;;; Returns TRUE if the widget at position pos has the class name defined,
;;; FALSE otherwise.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_has_region has been deprecated since version 3.14 and
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
;;; name :
;;;     region name
;;;
;;; flags :
;;;     return location for the region flags. [out]
;;;
;;; Returns :
;;;     TRUE if the class name is defined for the widget at pos
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_classes ()
;;;
;;; GSList * gtk_widget_path_iter_list_classes (const GtkWidgetPath *path,
;;;                                             gint pos);
;;;
;;; Returns a list with all the class names defined for the widget at position
;;; pos in the hierarchy defined in path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; Returns :
;;;     The list of classes, This is a list of strings, the GSList contents are
;;;     owned by GTK+, but you should use g_slist_free() to free the list
;;;     itself.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_regions ()
;;;
;;; GSList * gtk_widget_path_iter_list_regions (const GtkWidgetPath *path,
;;;                                             gint pos);
;;;
;;; Returns a list with all the region names defined for the widget at position
;;; pos in the hierarchy defined in path.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_list_regions has been deprecated since version 3.14 and
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
;;; Returns :
;;;     The list of regions, This is a list of strings, the GSList contents are
;;;     owned by GTK+, but you should use g_slist_free() to free the list
;;;     itself.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_class ()
;;;
;;; void gtk_widget_path_iter_remove_class (GtkWidgetPath *path,
;;;                                         gint pos,
;;;                                         const gchar *name);
;;;
;;; Removes the class name from the widget at position pos in the hierarchy
;;; defined in path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     class name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_region ()
;;;
;;; void gtk_widget_path_iter_remove_region (GtkWidgetPath *path,
;;;                                          gint pos,
;;;                                          const gchar *name);
;;;
;;; Removes the region name from the widget at position pos in the hierarchy
;;; defined in path.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_remove_region has been deprecated since version 3.14
;;; and should not be used in newly-written code.
;;;
;;; The use of regions is deprecated.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     region name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_name ()
;;;
;;; void gtk_widget_path_iter_set_name (GtkWidgetPath *path,
;;;                                     gint pos,
;;;                                     const gchar *name);
;;;
;;; Sets the widget name for the widget found at position pos in the widget
;;; hierarchy defined by path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     widget name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_object_name ()
;;;
;;; void
;;; gtk_widget_path_iter_set_object_name (GtkWidgetPath *path,
;;;                                       gint pos,
;;;                                       const char *name);
;;;
;;; Sets the object name for a given position in the widget hierarchy defined by
;;; path .
;;;
;;; When set, the object name overrides the object type when matching CSS.
;;;
;;; path :
;;;     a GtkWidgetPath
;;; 
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; name :
;;;     object name to set or NULL to unset.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_object_type ()
;;;
;;; void gtk_widget_path_iter_set_object_type (GtkWidgetPath *path,
;;;                                            gint pos,
;;;                                            GType type);
;;;
;;; Sets the object type for a given position in the widget hierarchy defined
;;; by path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to modify, -1 for the path head
;;;
;;; type :
;;;     object type to set
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_set_state ()
;;;
;;; void
;;; gtk_widget_path_iter_set_state (GtkWidgetPath *path,
;;;                                 gint pos,
;;;                                 GtkStateFlags state);
;;;
;;; Sets the widget name for the widget found at position pos in the widget
;;; hierarchy defined by path .
;;;
;;; If you want to update just a single state flag, you need to do this
;;; manually, as this function updates all state flags.
;;;
;;; Setting a flag
;;;
;;; gtk_widget_path_iter_set_state (path, pos, gtk_widget_path_iter_get_state
;;; (path, pos) | flag);
;;;
;;; Unsetting a flag
;;; gtk_widget_path_iter_set_state (path, pos, gtk_widget_path_iter_get_state
;;; (path, pos) & ~flag);
;;;
;;; path :
;;;     a GtkWidgetPath
;;; 
;;; pos :
;;;     position to modify, -1 for the path head
;;; 
;;; state ;
;;;     state flags
;;; 
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_length ()
;;;
;;; gint gtk_widget_path_length (const GtkWidgetPath *path);
;;;
;;; Returns the number of GtkWidget GTypes between the represented widget and
;;; its topmost container.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Returns :
;;;     the number of elements in the path
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_new" gtk-widget-path-new)
    (g-boxed-foreign gtk-widget-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @return{A newly created, empty, @class{gtk-widget-path}.}
  @short{Returns an empty widget path.}

  Since 3.0")

(export 'gtk-widget-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_prepend_type ()
;;;
;;; void gtk_widget_path_prepend_type (GtkWidgetPath *path, GType type);
;;;
;;; Prepends a widget type to the widget hierachy represented by path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; type :
;;;     widget type to prepend
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path_to_string" gtk-widget-path-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-5-25}
  @argument[path]{the path}
  @return{A new string describing @arg{path}.}
  @begin{short}
    Dumps the widget path into a string representation. It tries to match the
    CSS style as closely as possible (Note that there might be paths that
    cannot be represented in CSS).
  @end{short}

  The main use of this code is for debugging purposes.

  Since 3.2"
  (path (g-boxed-foreign gtk-widget-path)))

(export 'gtk-widget-path-to-string)

;;; --- End of file gtk.widget-path.lisp ---------------------------------------
