;;; ----------------------------------------------------------------------------
;;; gtk.theming-engine.lisp
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
;;; GtkThemingEngine
;;;
;;;     Theming renderers
;;;
;;; Types and Values
;;;
;;;     GtkThemingEngine
;;;
;;; Functions
;;;
;;;     gtk_theming_engine_get
;;;     gtk_theming_engine_get_direction
;;;     gtk_theming_engine_get_junction_sides
;;;     gtk_theming_engine_get_path
;;;     gtk_theming_engine_get_property
;;;     gtk_theming_engine_get_screen
;;;     gtk_theming_engine_get_state
;;;     gtk_theming_engine_get_style
;;;     gtk_theming_engine_get_style_property
;;;     gtk_theming_engine_get_style_valist
;;;     gtk_theming_engine_get_valist
;;;     gtk_theming_engine_get_color
;;;     gtk_theming_engine_get_background_color
;;;     gtk_theming_engine_get_border_color
;;;     gtk_theming_engine_get_border
;;;     gtk_theming_engine_get_padding
;;;     gtk_theming_engine_get_margin
;;;     gtk_theming_engine_get_font
;;;     gtk_theming_engine_has_class
;;;     gtk_theming_engine_has_region
;;;     gtk_theming_engine_lookup_color
;;;     gtk_theming_engine_state_is_running
;;;     gtk_theming_engine_load
;;;     gtk_theming_engine_register_property
;;;
;;; Properties
;;;
;;;     gchar*   name    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkThemingEngine
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkThemingEngine
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkThemingEngine" gtk-theming-engine
  (:superclass g-object
   :export t
   :interfaces ()
   :type-initializer "gtk_theming_engine_get_type")
  ((name
    gtk-theming-engine-name
    "name" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-theming-engine 'type)
 "@version{2020-1-17}
  @begin{short}
    @sym{gtk-theming-engine} was the object used for rendering themed content in
    GTK+ widgets.
  @end{short}
  It used to allow overriding GTK+'s default implementation of rendering
  functions by allowing engines to be loaded as modules.
  @begin[Warning]{dictionary}
    @sym{gtk-theming-engine} has been deprecated in GTK 3.14 and will be ignored
    for rendering. The advancements in CSS theming are good enough to allow
    themers to achieve their goals without the need to modify source code.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-theming-engine) 't)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The theming engine name, this name will be used when registering custom
  properties, for a theming engine named \"Clearlooks\" registering a
  \"glossy\" custom property, it could be referenced in the CSS file as
  @code{-Clearlooks-glossy: true;} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-theming-engine-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-theming-engine-name 'function)
 "@version{2013-11-27}
  Accessor of the @slot[gtk-theming-engine]{name} slot of the
  @class{gtk-theming-engine} class.
  @see-class{gtk-theming-engine}")

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get ()
;;;
;;; void gtk_theming_engine_get (GtkThemingEngine *engine,
;;;                              GtkStateFlags state,
;;;                              ...);
;;;
;;; Retrieves several style property values that apply to the currently
;;; rendered element.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve values for
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_direction ()
;;;
;;; GtkTextDirection gtk_theming_engine_get_direction (GtkThemingEngine *engine)
;;;
;;; Warning
;;;
;;; gtk_theming_engine_get_direction has been deprecated since version 3.8 and
;;; should not be used in newly-written code. Use gtk_theming_engine_get_state()
;;; and check for GTK_STATE_FLAG_DIR_LTR and GTK_STATE_FLAG_DIR_RTL instead.
;;;
;;; Returns the widget direction used for rendering.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; Returns :
;;;     the widget direction
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_junction_sides ()
;;;
;;; GtkJunctionSides gtk_theming_engine_get_junction_sides
;;;                                                   (GtkThemingEngine *engine)
;;;
;;; Returns the widget direction used for rendering.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; Returns :
;;;     the widget direction
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_path ()
;;;
;;; const GtkWidgetPath * gtk_theming_engine_get_path (GtkThemingEngine *engine)
;;;
;;; Returns the widget path used for style matching.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; Returns :
;;;     A GtkWidgetPath.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_property ()
;;;
;;; void gtk_theming_engine_get_property (GtkThemingEngine *engine,
;;;                                       const gchar *property,
;;;                                       GtkStateFlags state,
;;;                                       GValue *value);
;;;
;;; Gets a property value as retrieved from the style settings that apply to
;;; the currently rendered element.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; property :
;;;     the property name
;;;
;;; state :
;;;     state to retrieve the value for
;;;
;;; value :
;;;     return location for the property value, you must free this memory using
;;;     g_value_unset() once you are done with it.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_screen ()
;;;
;;; GdkScreen * gtk_theming_engine_get_screen (GtkThemingEngine *engine);
;;;
;;; Returns the GdkScreen to which engine currently rendering to.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; Returns :
;;;     a GdkScreen, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_state ()
;;;
;;; GtkStateFlags gtk_theming_engine_get_state (GtkThemingEngine *engine);
;;;
;;; returns the state used when rendering.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; Returns :
;;;     the state flags
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_style ()
;;;
;;; void gtk_theming_engine_get_style (GtkThemingEngine *engine, ...);
;;;
;;; Retrieves several widget style properties from engine according to the
;;; currently rendered content's style.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_style_property ()
;;;
;;; void gtk_theming_engine_get_style_property (GtkThemingEngine *engine,
;;;                                             const gchar *property_name,
;;;                                             GValue *value);
;;;
;;; Gets the value for a widget style property.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; property_name :
;;;     the name of the widget style property
;;;
;;; value :
;;;     Return location for the property value, free with g_value_unset() after
;;;     use.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_style_valist ()
;;;
;;; void gtk_theming_engine_get_style_valist (GtkThemingEngine *engine,
;;;                                           va_list args);
;;;
;;; Retrieves several widget style properties from engine according to the
;;; currently rendered content's style.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_valist ()
;;;
;;; void gtk_theming_engine_get_valist (GtkThemingEngine *engine,
;;;                                     GtkStateFlags state,
;;;                                     va_list args);
;;;
;;; Retrieves several style property values that apply to the currently
;;; rendered element.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve values for
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_color ()
;;;
;;; void gtk_theming_engine_get_color (GtkThemingEngine *engine,
;;;                                    GtkStateFlags state,
;;;                                    GdkRGBA *color);
;;;
;;; Gets the foreground color for a given state.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the foreground color.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_background_color ()
;;;
;;; void gtk_theming_engine_get_background_color (GtkThemingEngine *engine,
;;;                                               GtkStateFlags state,
;;;                                               GdkRGBA *color);
;;;
;;; Gets the background color for a given state.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the background color.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_border_color ()
;;;
;;; void gtk_theming_engine_get_border_color (GtkThemingEngine *engine,
;;;                                           GtkStateFlags state,
;;;                                           GdkRGBA *color);
;;;
;;; Gets the border color for a given state.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the border color.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_border ()
;;;
;;; void gtk_theming_engine_get_border (GtkThemingEngine *engine,
;;;                                     GtkStateFlags state,
;;;                                     GtkBorder *border);
;;;
;;; Gets the border for a given state as a GtkBorder.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the border for
;;;
;;; border :
;;;     return value for the border settings.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_padding ()
;;;
;;; void gtk_theming_engine_get_padding (GtkThemingEngine *engine,
;;;                                      GtkStateFlags state,
;;;                                      GtkBorder *padding);
;;;
;;; Gets the padding for a given state as a GtkBorder.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the padding for
;;;
;;; padding :
;;;     return value for the padding settings.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_margin ()
;;;
;;; void gtk_theming_engine_get_margin (GtkThemingEngine *engine,
;;;                                     GtkStateFlags state,
;;;                                     GtkBorder *margin);
;;;
;;; Gets the margin for a given state as a GtkBorder.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the border for
;;;
;;; margin :
;;;     return value for the margin settings.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_get_font ()
;;;
;;; const PangoFontDescription * gtk_theming_engine_get_font
;;;                                                   (GtkThemingEngine *engine,
;;;                                                    GtkStateFlags state);
;;;
;;; Warning
;;;
;;; gtk_theming_engine_get_font has been deprecated since version 3.8 and should
;;; not be used in newly-written code. Use gtk_theming_engine_get()
;;;
;;; Returns the font description for a given state.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     state to retrieve the font for
;;;
;;; Returns :
;;;     the PangoFontDescription for the given state. This object is owned by
;;;     GTK+ and should not be freed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_has_class ()
;;;
;;; gboolean gtk_theming_engine_has_class (GtkThemingEngine *engine,
;;;                                        const gchar *style_class);
;;;
;;; Returns TRUE if the currently rendered contents have defined the given
;;; class name.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; style_class :
;;;     class name to look up
;;;
;;; Returns :
;;;     TRUE if engine has class_name defined
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_has_region ()
;;;
;;; gboolean gtk_theming_engine_has_region (GtkThemingEngine *engine,
;;;                                         const gchar *style_region,
;;;                                         GtkRegionFlags *flags);
;;;
;;; Returns TRUE if the currently rendered contents have the region defined. If
;;; flags_return is not NULL, it is set to the flags affecting the region.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; style_region :
;;;     a region name
;;;
;;; flags :
;;;     return location for region flags.
;;;
;;; Returns :
;;;     TRUE if region is defined
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_lookup_color ()
;;;
;;; gboolean gtk_theming_engine_lookup_color (GtkThemingEngine *engine,
;;;                                           const gchar *color_name,
;;;                                           GdkRGBA *color);
;;;
;;; Looks up and resolves a color name in the current style's color map.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; color_name :
;;;     color name to lookup
;;;
;;; color :
;;;     Return location for the looked up color.
;;;
;;; Returns :
;;;     TRUE if color_name was found and resolved, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_state_is_running ()
;;;
;;; gboolean gtk_theming_engine_state_is_running (GtkThemingEngine *engine,
;;;                                               GtkStateType state,
;;;                                               gdouble *progress);
;;;
;;; Warning
;;;
;;; gtk_theming_engine_state_is_running has been deprecated since version 3.6
;;; and should not be used in newly-written code. Always returns FALSE
;;;
;;; Returns TRUE if there is a transition animation running for the current
;;; region (see gtk_style_context_push_animatable_region()).
;;;
;;; If progress is not NULL, the animation progress will be returned there, 0.0
;;; means the state is closest to being FALSE, while 1.0 means it's closest to
;;; being TRUE. This means transition animations will run from 0 to 1 when state
;;; is being set to TRUE and from 1 to 0 when it's being set to FALSE.
;;;
;;; engine :
;;;     a GtkThemingEngine
;;;
;;; state :
;;;     a widget state
;;;
;;; progress :
;;;     return location for the transition progress
;;;
;;; Returns :
;;;     TRUE if there is a running transition animation for state.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_load ()
;;;
;;; GtkThemingEngine * gtk_theming_engine_load (const gchar *name);
;;;
;;; Loads and initializes a theming engine module from the standard directories.
;;;
;;; name :
;;;     Theme engine name to load
;;;
;;; Returns :
;;;     A theming engine, or NULL if the engine name doesn't exist.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_theming_engine_register_property ()
;;;
;;; void gtk_theming_engine_register_property (const gchar *name_space,
;;;                                           GtkStylePropertyParser parse_func,
;;;                                            GParamSpec *pspec);
;;;
;;; Warning
;;;
;;; gtk_theming_engine_register_property has been deprecated since version 3.8
;;; and should not be used in newly-written code. Code should use the default
;;; properties provided by CSS.
;;;
;;; Registers a property so it can be used in the CSS file format, on the CSS
;;; file the property will look like "-${name_space}-${property_name}". being
;;; ${property_name} the given to pspec. name_space will usually be the theme
;;; engine name.
;;;
;;; For any type a parse_func may be provided, being this function used for
;;; turning any property value (between ':' and ';') in CSS to the GValue
;;; needed. For basic types there is already builtin parsing support, so NULL
;;; may be provided for these cases.
;;;
;;; Note
;;; Engines must ensure property registration happens exactly once, usually
;;; GTK+ deals with theming engines as singletons, so this should be guaranteed
;;; to happen once, but bear this in mind when creating GtkThemeEngines
;;; yourself.
;;;
;;; Note
;;;
;;; In order to make use of the custom registered properties in the CSS file,
;;; make sure the engine is loaded first by specifying the engine property,
;;; either in a previous rule or within the same one.
;;;
;;; * {
;;;     engine: someengine;
;;;     -SomeEngine-custom-property: 2;
;;; }
;;;
;;; name_space :
;;;     namespace for the property name
;;;
;;; parse_func :
;;;     parsing function to use, or NULL
;;;
;;; pspec :
;;;     the GParamSpec for the new property
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- gtk.theming-engine.lisp ------------------------------------------------
