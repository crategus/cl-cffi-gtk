;;; ----------------------------------------------------------------------------
;;; gtk.style-properties.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; GtkStyleProperties
;;; 
;;; Store for style property information
;;;     
;;; Synopsis
;;; 
;;;     GtkStyleProperties
;;;
;;;     gtk_style_properties_clear
;;;     gtk_style_properties_get
;;;     gtk_style_properties_get_property
;;;     gtk_style_properties_get_valist
;;;     gtk_style_properties_lookup_color
;;;     gtk_style_properties_lookup_property
;;;     gtk_style_properties_map_color
;;;     gtk_style_properties_merge
;;;     gtk_style_properties_new
;;;     gtk_style_properties_register_property
;;;     gtk_style_properties_set
;;;     gtk_style_properties_set_property
;;;     gtk_style_properties_set_valist
;;;     gtk_style_properties_unset_property
;;; 
;;; Description
;;; 
;;; GtkStyleProperties provides the storage for style information that is used
;;; by GtkStyleContext and other GtkStyleProvider implementations.
;;; 
;;; Before style properties can be stored in GtkStyleProperties, they must be
;;; registered with gtk_style_properties_register_property().
;;; 
;;; Unless you are writing a GtkStyleProvider implementation, you are unlikely
;;; to use this API directly, as gtk_style_context_get() and its variants are
;;; the preferred way to access styling information from widget implementations
;;; and theming engine implementations should use the APIs provided by
;;; GtkThemingEngine instead.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStyleProperties
;;; 
;;; struct GtkStyleProperties {
;;;   GObject parent_object;
;;;   GtkStylePropertiesPrivate *priv;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_clear ()
;;; 
;;; void gtk_style_properties_clear (GtkStyleProperties *props);
;;; 
;;; Clears all style information from props.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_get ()
;;; 
;;; void gtk_style_properties_get (GtkStyleProperties *props,
;;;                                GtkStateFlags state,
;;;                                ...);
;;; 
;;; Retrieves several style property values from props for a given state.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; state :
;;;     state to retrieve the property values for
;;; 
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_get_property ()
;;; 
;;; gboolean gtk_style_properties_get_property (GtkStyleProperties *props,
;;;                                             const gchar *property,
;;;                                             GtkStateFlags state,
;;;                                             GValue *value);
;;; 
;;; Gets a style property from props for the given state. When done with value,
;;; g_value_unset() needs to be called to free any allocated memory.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; property :
;;;     style property name
;;; 
;;; state :
;;;     state to retrieve the property value for
;;; 
;;; value :
;;;     return location for the style property value. [out][transfer full]
;;; 
;;; Returns :
;;;     TRUE if the property exists in props, FALSE otherwise
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_get_valist ()
;;; 
;;; void gtk_style_properties_get_valist (GtkStyleProperties *props,
;;;                                       GtkStateFlags state,
;;;                                       va_list args);
;;; 
;;; Retrieves several style property values from props for a given state.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; state :
;;;     state to retrieve the property values for
;;; 
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_lookup_color ()
;;; 
;;; GtkSymbolicColor * gtk_style_properties_lookup_color
;;;                                                  (GtkStyleProperties *props,
;;;                                                   const gchar *name);
;;; 
;;; Warning
;;; 
;;; gtk_style_properties_lookup_color has been deprecated since version 3.8 and
;;; should not be used in newly-written code. GtkSymbolicColor is deprecated.
;;; 
;;; Returns the symbolic color that is mapped to name.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; name :
;;;     color name to lookup
;;; 
;;; Returns :
;;;     The mapped color. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_lookup_property ()
;;; 
;;; gboolean gtk_style_properties_lookup_property
;;;                                         (const gchar *property_name,
;;;                                          GtkStylePropertyParser *parse_func,
;;;                                          GParamSpec **pspec);
;;; 
;;; Warning
;;; 
;;; gtk_style_properties_lookup_property has been deprecated since version 3.8
;;; and should not be used in newly-written code. This code could only look up
;;; custom properties and those are deprecated.
;;; 
;;; Returns TRUE if a property has been registered, if pspec or parse_func are
;;; not NULL, the GParamSpec and parsing function will be respectively returned.
;;; 
;;; property_name :
;;;     property name to look up
;;; 
;;; parse_func :
;;;     return location for the parse function. [out]
;;; 
;;; pspec :
;;;     return location for the GParamSpec. [out][transfer none]
;;; 
;;; Returns :
;;;     TRUE if the property is registered, FALSE otherwise
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_map_color ()
;;; 
;;; void gtk_style_properties_map_color (GtkStyleProperties *props,
;;;                                      const gchar *name,
;;;                                      GtkSymbolicColor *color);
;;; 
;;; Warning
;;; 
;;; gtk_style_properties_map_color has been deprecated since version 3.8 and
;;; should not be used in newly-written code. GtkSymbolicColor is deprecated.
;;; 
;;; Maps color so it can be referenced by name. See
;;; gtk_style_properties_lookup_color()
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; name :
;;;     color name
;;; 
;;; color :
;;;     GtkSymbolicColor to map name to
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_merge ()
;;; 
;;; void gtk_style_properties_merge (GtkStyleProperties *props,
;;;                                  const GtkStyleProperties *props_to_merge,
;;;                                  gboolean replace);
;;; 
;;; Merges into props all the style information contained in props_to_merge. If
;;; replace is TRUE, the values will be overwritten, if it is FALSE, the older
;;; values will prevail.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; props_to_merge :
;;;     a second GtkStyleProperties
;;; 
;;; replace :
;;;     whether to replace values or not
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_new ()
;;; 
;;; GtkStyleProperties * gtk_style_properties_new (void);
;;; 
;;; Returns a newly created GtkStyleProperties
;;; 
;;; Returns :
;;;     a new GtkStyleProperties
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkStylePropertyParser ()
;;; 
;;; gboolean (*GtkStylePropertyParser) (const gchar *string,
;;;                                     GValue *value,
;;;                                     GError **error);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_register_property ()
;;; 
;;; void gtk_style_properties_register_property
;;;                                          (GtkStylePropertyParser parse_func,
;;;                                           GParamSpec *pspec);
;;; 
;;; Warning
;;; 
;;; gtk_style_properties_register_property has been deprecated since version 3.8
;;; and should not be used in newly-written code. Code should use the default
;;; properties provided by CSS.
;;; 
;;; Registers a property so it can be used in the CSS file format. This function
;;; is the low-level equivalent of gtk_theming_engine_register_property(), if
;;; you are implementing a theming engine, you want to use that function
;;; instead.
;;; 
;;; parse_func :
;;;     parsing function to use, or NULL
;;; 
;;; pspec :
;;;     the GParamSpec for the new property
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_set ()
;;; 
;;; void gtk_style_properties_set (GtkStyleProperties *props,
;;;                                GtkStateFlags state,
;;;                                ...);
;;; 
;;; Sets several style properties on props.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; state :
;;;     state to set the values for
;;; 
;;; ... :
;;;     property name/value pairs, followed by NULL
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_set_property ()
;;; 
;;; void gtk_style_properties_set_property (GtkStyleProperties *props,
;;;                                         const gchar *property,
;;;                                         GtkStateFlags state,
;;;                                         const GValue *value);
;;; 
;;; Sets a styling property in props.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; property :
;;;     styling property to set
;;; 
;;; state :
;;;     state to set the value for
;;; 
;;; value :
;;;     new value for the property
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_set_valist ()
;;; 
;;; void gtk_style_properties_set_valist (GtkStyleProperties *props,
;;;                                       GtkStateFlags state,
;;;                                       va_list args);
;;; 
;;; Sets several style properties on props.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; state :
;;;     state to set the values for
;;; 
;;; args :
;;;     va_list of property name/value pairs, followed by NULL
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_properties_unset_property ()
;;; 
;;; void gtk_style_properties_unset_property (GtkStyleProperties *props,
;;;                                           const gchar *property,
;;;                                           GtkStateFlags state);
;;; 
;;; Unsets a style property in props.
;;; 
;;; props :
;;;     a GtkStyleProperties
;;; 
;;; property :
;;;     property to unset
;;; 
;;; state :
;;;     state to unset
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.style-properties.lisp ----------------------------------
