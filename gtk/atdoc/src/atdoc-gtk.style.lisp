;;; ----------------------------------------------------------------------------
;;; gtk.style.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; Styles
;;;
;;; Functions for drawing widget parts
;;;
;;; struct              GtkStyle
;;;
;;; GtkStyle *          gtk_style_new
;;; GtkStyle *          gtk_style_copy
;;; GtkStyle *          gtk_style_attach
;;; void                gtk_style_detach
;;; GtkStyle *          gtk_style_ref
;;; void                gtk_style_unref
;;; void                gtk_style_set_background
;;; void                gtk_style_apply_default_background
;;;
;;; #define             gtk_style_apply_default_pixmap
;;;
;;; gboolean            gtk_style_lookup_color
;;; GtkIconSet *        gtk_style_lookup_icon_set
;;; GdkPixbuf *         gtk_style_render_icon
;;; GdkFont *           gtk_style_get_font
;;; void                gtk_style_set_font
;;; void                gtk_style_get_style_property
;;; void                gtk_style_get_valist
;;; void                gtk_style_get
;;;
;;; void                gtk_draw_hline
;;; void                gtk_draw_vline
;;; void                gtk_draw_shadow
;;; void                gtk_draw_polygon
;;; void                gtk_draw_arrow
;;; void                gtk_draw_diamond
;;; void                gtk_draw_string
;;; void                gtk_draw_box
;;; void                gtk_draw_box_gap
;;; void                gtk_draw_check
;;; void                gtk_draw_extension
;;; void                gtk_draw_flat_box
;;; void                gtk_draw_focus
;;; void                gtk_draw_handle
;;; void                gtk_draw_option
;;; void                gtk_draw_shadow_gap
;;; void                gtk_draw_slider
;;; void                gtk_draw_tab
;;; void                gtk_draw_expander
;;; void                gtk_draw_layout
;;; void                gtk_draw_resize_grip
;;;
;;; void                gtk_paint_arrow
;;; void                gtk_paint_box
;;; void                gtk_paint_box_gap
;;; void                gtk_paint_check
;;; void                gtk_paint_diamond
;;; void                gtk_paint_extension
;;; void                gtk_paint_flat_box
;;; void                gtk_paint_focus
;;; void                gtk_paint_handle
;;; void                gtk_paint_hline
;;; void                gtk_paint_option
;;; void                gtk_paint_polygon
;;; void                gtk_paint_shadow
;;; void                gtk_paint_shadow_gap
;;; void                gtk_paint_slider
;;; void                gtk_paint_spinner
;;; void                gtk_paint_string
;;; void                gtk_paint_tab
;;; void                gtk_paint_vline
;;; void                gtk_paint_expander
;;; void                gtk_paint_layout
;;; void                gtk_paint_resize_grip
;;; void                gtk_draw_insertion_cursor
;;;
;;;                     GtkRcProperty
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details of gtk-style
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "context" 'gtk-style) 't)
 "The @arg{\"context\"} property of type @class{gtk-style-context}
  (Read / Write / Construct Only).@br{}
  @class{gtk-style-context} class to get style from.")

(setf (gethash 'gtk-style-context atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-style-context 'function)
 "@version{2013-1-13}
  @begin{short}
    Accessor of the slot \"context\" of the @class{gtk-style} class.
  @end{short}
  @see-class{gtk-style}")

;;; --- gtk-style --------------------------------------------------------------

(setf (documentation 'gtk-style 'type)
 "@version{2013-1-13}
  @begin{short}
    A @sym{gtk-style} object encapsulates the information that provides the look
    and feel for a widget.
  @end{short}

  Each @class{gtk-widget} has an associated @sym{gtk-style} object that is used
  when rendering that widget. Also, a @sym{gtk-style} holds information for the
  five possible widget states though not every widget supports all five states;
  see @symbol{gtk-state-type}.

  Usually the @sym{gtk-style} for a widget is the same as the default style that
  is set by GTK+ and modified the theme engine.

  Usually applications should not need to use or modify the @sym{gtk-style} of
  their widgets.
  @begin[Warning]{dictionary}
    In GTK+ 3.0, @sym{gtk-style} has been deprecated and replaced by
    @class{gtk-style-context}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @b{The \"realize\" signal}
    @begin{pre}
 void user_function (GtkStyle *style,
                     gpointer  user_data)   : Run First
    @end{pre}
    Emitted when the @arg{style} has been initialized for a particular colormap
    and depth. Connecting to this signal is probably seldom useful since most of
    the time applications and widgets only deal with styles that have been
    already realized.
    @begin[code]{table}
      @entry[style]{the object which received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.4

    @b{The \"unrealize\" signal}
    @begin{pre}
 void user_function (GtkStyle *style,
                     gpointer  user_data)   : Run First
    @end{pre}
    Emitted when the aspects of the @arg{style} specific to a particular
    colormap and depth are being cleaned up. A connection to this signal can be
    useful if a widget wants to cache objects like a GdkGC as object data on
    @sym{gtk-style}. This signal provides a convenient place to free such cached
    objects.
    @begin[code]{table}
      @entry[style]{the object which received the signal}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
    Since 2.4
  @end{dictionary}
  @see-slot{gtk-style-context}")

#|
;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_ATTACHED()
;;;
;;; #define GTK_STYLE_ATTACHED(style) (GTK_STYLE (style)->attach_count > 0)
;;;
;;; Returns whether the style is attached to a window.
;;;
;;; style :
;;;     a GtkStyle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_new ()
;;;
;;; GtkStyle * gtk_style_new (void);
;;;
;;; Creates a new GtkStyle.
;;;
;;; Returns :
;;;     a new GtkStyle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_copy ()
;;;
;;; GtkStyle *          gtk_style_copy                      (GtkStyle *style);
;;;
;;; Creates a copy of the passed in GtkStyle object.
;;;
;;; style :
;;;     a GtkStyle
;;;
;;; Returns :
;;;     a copy of style
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_attach ()
;;;
;;; GtkStyle * gtk_style_attach (GtkStyle *style, GdkWindow *window)
;;;
;;; Warning
;;;
;;;    gtk_style_attach has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_widget_style_attach()
;;;    instead
;;;
;;; Attaches a style to a window; this process allocates the colors and
;;; creates the GC's for the style - it specializes it to a particular
;;; visual. The process may involve the creation of a new style if the
;;; style has already been attached to a window with a different style and
;;; visual.
;;;
;;; Since this function may return a new object, you have to use it in the
;;; following way: style = gtk_style_attach (style, window)
;;;
;;; style :
;;;       a GtkStyle.
;;;
;;;   window :
;;;       a GdkWindow.
;;;
;;; Returns :
;;;       Either style, or a newly-created GtkStyle. If the style is newly
;;;       created, the style parameter will be unref'ed, and the new style will
;;;       have a reference count belonging to the caller.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_detach ()
;;;
;;; void gtk_style_detach (GtkStyle *style);
;;;
;;; Warning
;;;
;;;    gtk_style_detach has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use GtkStyleContext instead
;;;
;;;    Detaches a style from a window. If the style is not attached to any
;;;    windows anymore, it is unrealized. See gtk_style_attach().
;;;
;;;    style :
;;;           a GtkStyle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_has_context ()
;;;
;;; gboolean gtk_style_has_context (GtkStyle *style);
;;;
;;;    Returns whether style has an associated GtkStyleContext.
;;;
;;;    style :
;;;             a GtkStyle
;;;
;;;    Returns :
;;;             TRUE if style has a GtkStyleContext
;;;
;;;    Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_set_background ()
;;;
;;; void gtk_style_set_background (GtkStyle *style,
;;;                                GdkWindow *window,
;;;                                GtkStateType state_type);
;;;
;;; Warning
;;;
;;;    gtk_style_set_background has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use
;;;    gtk_style_context_set_background() instead
;;;
;;;    Sets the background of window to the background color or pixmap
;;;    specified by style for the given state.
;;;
;;;    style :
;;;                a GtkStyle
;;;
;;;    window :
;;;                a GdkWindow
;;;
;;;    state_type :
;;;                a state
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_apply_default_background ()
;;;
;;; void gtk_style_apply_default_background (GtkStyle *style,
;;;                                          cairo_t *cr,
;;;                                          GdkWindow *window,
;;;                                          GtkStateType state_type,
;;;                                          gint x,
;;;                                          gint y,
;;;                                          gint width,
;;;                                          gint height);
;;;
;;; Warning
;;;
;;;    gtk_style_apply_default_background has been deprecated since version
;;;    3.0 and should not be used in newly-written code. Use
;;;    GtkStyleContext instead
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_lookup_color ()
;;;
;;; gboolean gtk_style_lookup_color (GtkStyle *style,
;;;                                  const gchar *color_name,
;;;                                  GdkColor *color);
;;;
;;; Warning
;;;
;;;    gtk_style_lookup_color has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use
;;;    gtk_style_context_lookup_color() instead
;;;
;;;    Looks up color_name in the style's logical color mappings, filling in
;;;    color and returning TRUE if found, otherwise returning FALSE.
;;;    Do not cache the found mapping, because it depends on the GtkStyle
;;;    and might change when a theme switch occurs.
;;;
;;;    style :
;;;                a GtkStyle
;;;
;;;    color_name :
;;;                the name of the logical color to look up
;;;
;;;    color :
;;;                the GdkColor to fill in
;;;
;;;    Returns :
;;;                TRUE if the mapping was found.
;;;
;;;    Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_lookup_icon_set ()
;;;
;;; GtkIconSet * gtk_style_lookup_icon_set (GtkStyle *style,
;;;                                         const gchar *stock_id);
;;;
;;; Warning
;;;
;;;    gtk_style_lookup_icon_set has been deprecated since version 3.0 and
;;;    should not be used in newly-written code. Use
;;;    gtk_style_context_lookup_icon_set() instead
;;;
;;;    Looks up stock_id in the icon factories associated with style and the
;;;    default icon factory, returning an icon set if found, otherwise
;;;    NULL.
;;;
;;;    style :
;;;              a GtkStyle
;;;
;;;    stock_id :
;;;              an icon name
;;;
;;;    Returns :
;;;              icon set of stock_id
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_render_icon ()
;;;
;;; GdkPixbuf * gtk_style_render_icon (GtkStyle *style,
;;;                                    const GtkIconSource *source,
;;;                                    GtkTextDirection direction,
;;;                                    GtkStateType state,
;;;                                    GtkIconSize size,
;;;                                    GtkWidget *widget,
;;;                                    const gchar *detail);
;;;
;;; Warning
;;;
;;;    gtk_style_render_icon has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use gtk_render_icon_pixbuf()
;;;    instead
;;;
;;;    Renders the icon specified by source at the given size according to the
;;;    given parameters and returns the result in a pixbuf.
;;;
;;;    style :
;;;    a GtkStyle
;;;
;;;    source :
;;;    the GtkIconSource specifying the icon to render
;;;
;;;    direction :
;;;    a text direction
;;;
;;;    state :
;;;    a state
;;;
;;;    size :
;;;    the size to render the icon at. A size of (GtkIconSize)-1 means render
;;;    at the size of the source and don't scale
;;;
;;;    widget :
;;;    the widget
;;;
;;;    detail :
;;;    a style detail
;;;
;;;    Returns :
;;;    a newly-created GdkPixbuf containing the rendered icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_get_style_property ()
;;;
;;; void gtk_style_get_style_property (GtkStyle *style,
;;;                                    GType widget_type,
;;;                                    const gchar *property_name,
;;;                                    GValue *value);
;;;
;;; Warning
;;;
;;;    gtk_style_get_style_property is deprecated and should not be used in
;;;    newly-written code.
;;;
;;;    Queries the value of a style property corresponding to a widget class
;;;    is in the given style.
;;;
;;;    style :
;;;    a GtkStyle
;;;
;;;    widget_type :
;;;    the GType of a descendant of GtkWidget
;;;
;;;    property_name :
;;;    the name of the style property to get
;;;
;;;    value :
;;;    a GValue where the value of the property being queried will be
;;;    stored
;;;
;;;    Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_get_valist ()
;;;
;;; void gtk_style_get_valist (GtkStyle *style,
;;;                            GType widget_type,
;;;                            const gchar *first_property_name,
;;;                            va_list var_args);
;;;
;;; Warning
;;;
;;;    gtk_style_get_valist is deprecated and should not be used in
;;;    newly-written code.
;;;
;;;    Non-vararg variant of gtk_style_get(). Used primarily by language
;;;    bindings.
;;;
;;;    style :
;;;    a GtkStyle
;;;
;;;    widget_type :
;;;    the GType of a descendant of GtkWidget
;;;
;;;    first_property_name :
;;;    the name of the first style property to get
;;;
;;;    var_args :
;;;    a va_list of pairs of property names and locations to return the
;;;    property values, starting with the location for first_property_name.
;;;
;;;    Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_get ()
;;;
;;; void gtk_style_get (GtkStyle *style,
;;;                     GType widget_type,
;;;                     const gchar *first_property_name,
;;;                     ...);
;;;
;;; Warning
;;;
;;;    gtk_style_get is deprecated and should not be used in newly-written
;;;    code.
;;;
;;;    Gets the values of a multiple style properties for widget_type from
;;;    style.
;;;
;;;    style :
;;;    a GtkStyle
;;;
;;;    widget_type :
;;;    the GType of a descendant of GtkWidget
;;;
;;;    first_property_name :
;;;    the name of the first style property to get
;;;
;;;    ... :
;;;    pairs of property names and locations to return the property values,
;;;    starting with the location for first_property_name, terminated by NULL.
;;;
;;;    Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_arrow ()
;;;
;;; void gtk_paint_arrow (GtkStyle *style,
;;;                       cairo_t *cr,
;;;                       GtkStateType state_type,
;;;                       GtkShadowType shadow_type,
;;;                       GtkWidget *widget,
;;;                       const gchar *detail,
;;;                       GtkArrowType arrow_type,
;;;                       gboolean fill,
;;;                       gint x,
;;;                       gint y,
;;;                       gint width,
;;;                       gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_arrow has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_arrow() instead
;;;
;;;    Draws an arrow in the given rectangle on cr using the given parameters.
;;;    arrow_type determines the direction of the arrow.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    arrow_type :
;;;                 the type of arrow to draw
;;;
;;;    fill :
;;;                 TRUE if the arrow tip should be filled
;;;
;;;    x :
;;;                 x origin of the rectangle to draw the arrow in
;;;
;;;    y :
;;;                 y origin of the rectangle to draw the arrow in
;;;
;;;    width :
;;;                 width of the rectangle to draw the arrow in
;;;
;;;    height :
;;;                 height of the rectangle to draw the arrow in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_box ()
;;;
;;; void gtk_paint_box (GtkStyle *style,
;;;                     cairo_t *cr,
;;;                     GtkStateType state_type,
;;;                     GtkShadowType shadow_type,
;;;                     GtkWidget *widget,
;;;                     const gchar *detail,
;;;                     gint x,
;;;                     gint y,
;;;                     gint width,
;;;                     gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_box has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_frame() and
;;;    gtk_render_background() instead
;;;
;;;    Draws a box on cr with the given parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the box
;;;
;;;    y :
;;;                 y origin of the box
;;;
;;;    width :
;;;                 the width of the box
;;;
;;;    height :
;;;                 the height of the box
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_box_gap ()
;;;
;;; void gtk_paint_box_gap (GtkStyle *style,
;;;                         cairo_t *cr,
;;;                         GtkStateType state_type,
;;;                         GtkShadowType shadow_type,
;;;                         GtkWidget *widget,
;;;                         const gchar *detail,
;;;                         gint x,
;;;                         gint y,
;;;                         gint width,
;;;                         gint height,
;;;                         GtkPositionType gap_side,
;;;                         gint gap_x,
;;;                         gint gap_width);
;;;
;;; Warning
;;;
;;;    gtk_paint_box_gap has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_frame_gap() instead
;;;
;;;    Draws a box in cr using the given style and state and shadow type,
;;;    leaving a gap in one side.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle
;;;
;;;    y :
;;;                 y origin of the rectangle
;;;
;;;    width :
;;;                 width of the rectangle
;;;
;;;    height :
;;;                 width of the rectangle
;;;
;;;    gap_side :
;;;                 side in which to leave the gap
;;;
;;;    gap_x :
;;;                 starting position of the gap
;;;
;;;    gap_width :
;;;                 width of the gap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_check ()
;;;
;;; void gtk_paint_check (GtkStyle *style,
;;;                       cairo_t *cr,
;;;                       GtkStateType state_type,
;;;                       GtkShadowType shadow_type,
;;;                       GtkWidget *widget,
;;;                       const gchar *detail,
;;;                       gint x,
;;;                       gint y,
;;;                       gint width,
;;;                       gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_check has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_check() instead
;;;
;;;    Draws a check button indicator in the given rectangle on cr with the
;;;    given parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle to draw the check in
;;;
;;;    y :
;;;                 y origin of the rectangle to draw the check in
;;;
;;;    width :
;;;                 the width of the rectangle to draw the check in
;;;
;;;    height :
;;;                 the height of the rectangle to draw the check in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_diamond ()
;;;
;;; void gtk_paint_diamond (GtkStyle *style,
;;;                         cairo_t *cr,
;;;                         GtkStateType state_type,
;;;                         GtkShadowType shadow_type,
;;;                         GtkWidget *widget,
;;;                         const gchar *detail,
;;;                         gint x,
;;;                         gint y,
;;;                         gint width,
;;;                         gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_diamond has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use cairo instead
;;;
;;;    Draws a diamond in the given rectangle on window using the given
;;;    parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle to draw the diamond in
;;;
;;;    y :
;;;                 y origin of the rectangle to draw the diamond in
;;;
;;;    width :
;;;                 width of the rectangle to draw the diamond in
;;;
;;;    height :
;;;                 height of the rectangle to draw the diamond in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_extension ()
;;;
;;; void gtk_paint_extension (GtkStyle *style,
;;;                           cairo_t *cr,
;;;                           GtkStateType state_type,
;;;                           GtkShadowType shadow_type,
;;;                           GtkWidget *widget,
;;;                           const gchar *detail,
;;;                           gint x,
;;;                           gint y,
;;;                           gint width,
;;;                           gint height,
;;;                           GtkPositionType gap_side);
;;;
;;; Warning
;;;
;;;    gtk_paint_extension has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use gtk_render_extension()
;;;    instead
;;;
;;;    Draws an extension, i.e. a notebook tab.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the extension
;;;
;;;    y :
;;;                 y origin of the extension
;;;
;;;    width :
;;;                 width of the extension
;;;
;;;    height :
;;;                 width of the extension
;;;
;;;    gap_side :
;;;                 the side on to which the extension is attached
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_flat_box ()
;;;
;;; void gtk_paint_flat_box (GtkStyle *style,
;;;                          cairo_t *cr,
;;;                          GtkStateType state_type,
;;;                          GtkShadowType shadow_type,
;;;                          GtkWidget *widget,
;;;                          const gchar *detail,
;;;                          gint x,
;;;                          gint y,
;;;                          gint width,
;;;                          gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_flat_box has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_frame() and
;;;    gtk_render_background() instead
;;;
;;;    Draws a flat box on cr with the given parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the box
;;;
;;;    y :
;;;                 y origin of the box
;;;
;;;    width :
;;;                 the width of the box
;;;
;;;    height :
;;;                 the height of the box
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_focus ()
;;;
;;; void gtk_paint_focus (GtkStyle *style,
;;;                       cairo_t *cr,
;;;                       GtkStateType state_type,
;;;                       GtkWidget *widget,
;;;                       const gchar *detail,
;;;                       gint x,
;;;                       gint y,
;;;                       gint width,
;;;                       gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_focus has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_focus() instead
;;;
;;;    Draws a focus indicator around the given rectangle on cr using the
;;;    given style.
;;;
;;; style :
;;;     a GtkStyle
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; state_type :
;;;     a state
;;;
;;; widget :
;;;     the widget
;;;
;;; detail :
;;;     a style detail
;;;
;;; x :
;;;     the x origin of the rectangle around which to draw a focus indicator
;;;
;;; y :
;;;     the y origin of the rectangle around which to draw a focus indicator
;;;
;;; width :
;;;     the width of the rectangle around which to draw a focus indicator
;;;
;;; height :
;;;     the height of the rectangle around which to draw a focus indicator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_handle ()
;;;
;;; void gtk_paint_handle (GtkStyle *style,
;;;                        cairo_t *cr,
;;;                        GtkStateType state_type,
;;;                        GtkShadowType shadow_type,
;;;                        GtkWidget *widget,
;;;                        const gchar *detail,
;;;                        gint x,
;;;                        gint y,
;;;                        gint width,
;;;                        gint height,
;;;                        GtkOrientation orientation);
;;;
;;; Warning
;;;
;;;    gtk_paint_handle has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_handle() instead
;;;
;;;    Draws a handle as used in GtkHandleBox and GtkPaned.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the handle
;;;
;;;    y :
;;;                 y origin of the handle
;;;
;;;    width :
;;;                 with of the handle
;;;
;;;    height :
;;;                 height of the handle
;;;
;;;    orientation :
;;;                 the orientation of the handle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_hline ()
;;;
;;; void gtk_paint_hline (GtkStyle *style,
;;;                       cairo_t *cr,
;;;                       GtkStateType state_type,
;;;                       GtkWidget *widget,
;;;                       const gchar *detail,
;;;                       gint x1,
;;;                       gint x2,
;;;                       gint y);
;;;
;;; Warning
;;;
;;;    gtk_paint_hline has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_line() instead
;;;
;;;    Draws a horizontal line from (x1, y) to (x2, y) in cr using the given
;;;    style and state.
;;;
;;;    style :
;;;                a GtkStyle
;;;
;;;    cr :
;;;                a caio_t
;;;
;;;    state_type :
;;;                a state
;;;
;;;    widget :
;;;                the widget
;;;
;;;    detail :
;;;                a style detail
;;;
;;;    x1 :
;;;                the starting x coordinate
;;;
;;;    x2 :
;;;                the ending x coordinate
;;;
;;;    y :
;;;                the y coordinate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_option ()
;;;
;;; void gtk_paint_option (GtkStyle *style,
;;;                        cairo_t *cr,
;;;                        GtkStateType state_type,
;;;                        GtkShadowType shadow_type,
;;;                        GtkWidget *widget,
;;;                        const gchar *detail,
;;;                        gint x,
;;;                        gint y,
;;;                        gint width,
;;;                        gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_option has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_option() instead
;;;
;;;    Draws a radio button indicator in the given rectangle on cr with the
;;;    given parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle to draw the option in
;;;
;;;    y :
;;;                 y origin of the rectangle to draw the option in
;;;
;;;    width :
;;;                 the width of the rectangle to draw the option in
;;;
;;;    height :
;;;                 the height of the rectangle to draw the option in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_shadow ()
;;;
;;; void gtk_paint_shadow (GtkStyle *style,
;;;                        cairo_t *cr,
;;;                        GtkStateType state_type,
;;;                        GtkShadowType shadow_type,
;;;                        GtkWidget *widget,
;;;                        const gchar *detail,
;;;                        gint x,
;;;                        gint y,
;;;                        gint width,
;;;                        gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_shadow has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_frame() instead
;;;
;;;    Draws a shadow around the given rectangle in cr using the given style
;;;    and state and shadow type.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle
;;;
;;;    y :
;;;                 y origin of the rectangle
;;;
;;;    width :
;;;                 width of the rectangle
;;;
;;;    height :
;;;                 width of the rectangle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_shadow_gap ()
;;;
;;; void gtk_paint_shadow_gap (GtkStyle *style,
;;;                            cairo_t *cr,
;;;                            GtkStateType state_type,
;;;                            GtkShadowType shadow_type,
;;;                            GtkWidget *widget,
;;;                            const gchar *detail,
;;;                            gint x,
;;;                            gint y,
;;;                            gint width,
;;;                            gint height,
;;;                            GtkPositionType gap_side,
;;;                            gint gap_x,
;;;                            gint gap_width);
;;;
;;; Warning
;;;
;;;    gtk_paint_shadow_gap has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use gtk_render_frame_gap()
;;;    instead
;;;
;;;    Draws a shadow around the given rectangle in cr using the given style
;;;    and state and shadow type, leaving a gap in one side.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle
;;;
;;;    y :
;;;                 y origin of the rectangle
;;;
;;;    width :
;;;                 width of the rectangle
;;;
;;;    height :
;;;                 width of the rectangle
;;;
;;;    gap_side :
;;;                 side in which to leave the gap
;;;
;;;    gap_x :
;;;                 starting position of the gap
;;;
;;;    gap_width :
;;;                 width of the gap
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_slider ()
;;;
;;; void gtk_paint_slider (GtkStyle *style,
;;;                        cairo_t *cr,
;;;                        GtkStateType state_type,
;;;                        GtkShadowType shadow_type,
;;;                        GtkWidget *widget,
;;;                        const gchar *detail,
;;;                        gint x,
;;;                        gint y,
;;;                        gint width,
;;;                        gint height,
;;;                        GtkOrientation orientation);
;;;
;;; Warning
;;;
;;;    gtk_paint_slider has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_slider() instead
;;;
;;;    Draws a slider in the given rectangle on cr using the given style and
;;;    orientation.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 a shadow
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 the x origin of the rectangle in which to draw a slider
;;;
;;;    y :
;;;                 the y origin of the rectangle in which to draw a slider
;;;
;;;    width :
;;;                 the width of the rectangle in which to draw a slider
;;;
;;;    height :
;;;                 the height of the rectangle in which to draw a slider
;;;
;;;    orientation :
;;;                 the orientation to be used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_spinner ()
;;;
;;; void gtk_paint_spinner (GtkStyle *style,
;;;                         cairo_t *cr,
;;;                         GtkStateType state_type,
;;;                         GtkWidget *widget,
;;;                         const gchar *detail,
;;;                         guint step,
;;;                         gint x,
;;;                         gint y,
;;;                         gint width,
;;;                         gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_spinner has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_activity() instead
;;;
;;;    Draws a spinner on window using the given parameters.
;;;
;;;    style :
;;;                a GtkStyle
;;;
;;;    cr :
;;;                a cairo_t
;;;
;;;    state_type :
;;;                a state
;;;
;;;    widget :
;;;                the widget (may be NULL)
;;;
;;;    detail :
;;;                a style detail (may be NULL)
;;;
;;;    step :
;;;                the nth step, a value between 0 and "num-steps"
;;;
;;;    x :
;;;                the x origin of the rectangle in which to draw the spinner
;;;
;;;    y :
;;;                the y origin of the rectangle in which to draw the spinner
;;;
;;;    width :
;;;                the width of the rectangle in which to draw the spinner
;;;
;;;    height :
;;;                the height of the rectangle in which to draw the spinner
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_tab ()
;;;
;;; void gtk_paint_tab (GtkStyle *style,
;;;                     cairo_t *cr,
;;;                     GtkStateType state_type,
;;;                     GtkShadowType shadow_type,
;;;                     GtkWidget *widget,
;;;                     const gchar *detail,
;;;                     gint x,
;;;                     gint y,
;;;                     gint width,
;;;                     gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_tab has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use cairo instead
;;;
;;;    Draws an option menu tab (i.e. the up and down pointing arrows) in the
;;;    given rectangle on cr using the given parameters.
;;;
;;;    style :
;;;                 a GtkStyle
;;;
;;;    cr :
;;;                 a cairo_t
;;;
;;;    state_type :
;;;                 a state
;;;
;;;    shadow_type :
;;;                 the type of shadow to draw
;;;
;;;    widget :
;;;                 the widget
;;;
;;;    detail :
;;;                 a style detail
;;;
;;;    x :
;;;                 x origin of the rectangle to draw the tab in
;;;
;;;    y :
;;;                 y origin of the rectangle to draw the tab in
;;;
;;;    width :
;;;                 the width of the rectangle to draw the tab in
;;;
;;;    height :
;;;                 the height of the rectangle to draw the tab in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_vline ()
;;;
;;; void gtk_paint_vline (GtkStyle *style,
;;;                       cairo_t *cr,
;;;                       GtkStateType state_type,
;;;                       GtkWidget *widget,
;;;                       const gchar *detail,
;;;                       gint y1_,
;;;                       gint y2_,
;;;                       gint x);
;;;
;;; Warning
;;;
;;;    gtk_paint_vline has been deprecated since version 3.0 and should not be
;;;    used in newly-written code. Use gtk_render_line() instead
;;;
;;;    Draws a vertical line from (x, y1_) to (x, y2_) in cr using the given
;;;    style and state.
;;;
;;;    style :
;;;                a GtkStyle
;;;
;;;    cr :
;;;                a cairo_t
;;;
;;;    state_type :
;;;                a state
;;;
;;;    widget :
;;;                the widget
;;;
;;;    detail :
;;;                a style detail
;;;
;;;    y1_ :
;;;                the starting y coordinate
;;;
;;;    y2_ :
;;;                the ending y coordinate
;;;
;;;    x :
;;;                the x coordinate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_expander ()
;;;
;;; void gtk_paint_expander (GtkStyle *style,
;;;                          cairo_t *cr,
;;;                          GtkStateType state_type,
;;;                          GtkWidget *widget,
;;;                          const gchar *detail,
;;;                          gint x,
;;;                          gint y,
;;;                          GtkExpanderStyle expander_style);
;;;
;;; Warning
;;;
;;;    gtk_paint_expander has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_expander() instead
;;;
;;;    Draws an expander as used in GtkTreeView. x and y specify the
;;;    center the expander. The size of the expander is determined by the
;;;    "expander-size" style property of widget. (If widget is not specified
;;;    or doesn't have an "expander-size" property, an unspecified default
;;;    size will be used, since the caller doesn't have sufficient information
;;;    to position the expander, this is likely not useful.) The expander is
;;;    expander_size pixels tall in the collapsed position and expander_size
;;;    pixels wide in the expanded position.
;;;
;;;    style :
;;;    a GtkStyle
;;;
;;;    cr :
;;;    a cairo_t
;;;
;;;    state_type :
;;;    a state
;;;
;;;    widget :
;;;    the widget
;;;
;;;    detail :
;;;    a style detail
;;;
;;;    x :
;;;    the x position to draw the expander at
;;;
;;;    y :
;;;    the y position to draw the expander at
;;;
;;;    expander_style :
;;;    the style to draw the expander in; determines whether the expander is
;;;    collapsed, expanded, or in an intermediate state.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_layout ()
;;;
;;; void gtk_paint_layout (GtkStyle *style,
;;;                        cairo_t *cr,
;;;                        GtkStateType state_type,
;;;                        gboolean use_text,
;;;                        GtkWidget *widget,
;;;                        const gchar *detail,
;;;                        gint x,
;;;                        gint y,
;;;                        PangoLayout *layout);
;;;
;;; Warning
;;;
;;;    gtk_paint_layout has been deprecated since version 3.0 and should not
;;;    be used in newly-written code. Use gtk_render_layout() instead
;;;
;;;    Draws a layout on cr using the given parameters.
;;;
;;; style :
;;;     a GtkStyle
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; state_type :
;;;     a state
;;;
;;; use_text :
;;;     whether to use the text or foreground graphics context of style
;;;
;;; widget :
;;;     the widget
;;;
;;; detail :
;;;     a style detail
;;;
;;; x :
;;;     x origin
;;;
;;; y :
;;;     y origin
;;;
;;; layout :
;;;     the layout to draw
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paint_resize_grip ()
;;;
;;; void gtk_paint_resize_grip (GtkStyle *style,
;;;                             cairo_t *cr,
;;;                             GtkStateType state_type,
;;;                             GtkWidget *widget,
;;;                             const gchar *detail,
;;;                             GdkWindowEdge edge,
;;;                             gint x,
;;;                             gint y,
;;;                             gint width,
;;;                             gint height);
;;;
;;; Warning
;;;
;;;    gtk_paint_resize_grip has been deprecated since version 3.0 and should
;;;    not be used in newly-written code. Use gtk_render_handle() instead
;;;
;;;    Draws a resize grip in the given rectangle on cr using the given
;;;    parameters.
;;;
;;; style :
;;;     a GtkStyle
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; state_type :
;;;     a state
;;;
;;; widget :
;;;     the widget
;;;
;;; detail :
;;;     a style detail
;;;
;;; edge :
;;;     the edge in which to draw the resize grip
;;;
;;; x :
;;;     the x origin of the rectangle in which to draw the resize grip
;;;
;;; y :
;;;     the y origin of the rectangle in which to draw the resize grip
;;;
;;; width :
;;;     the width of the rectangle in which to draw the resize grip
;;;
;;; height :
;;;     the height of the rectangle in which to draw the resize grip
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_draw_insertion_cursor ()
;;;
;;; void gtk_draw_insertion_cursor (GtkWidget *widget,
;;;                                 cairo_t *cr,
;;;                                 const GdkRectangle *location,
;;;                                 gboolean is_primary,
;;;                                 GtkTextDirection direction,
;;;                                 gboolean draw_arrow);
;;;
;;;    Draws a text caret on cr at location. This is not a style function but
;;;    merely a convenience function for drawing the standard cursor shape.
;;;
;;;    widget :
;;;    a GtkWidget
;;;
;;;    cr :
;;;    cairo context to draw to
;;;
;;;    location :
;;;    location where to draw the cursor (location->width is ignored)
;;;
;;;    is_primary :
;;;    if the cursor should be the primary cursor color.
;;;
;;;    direction :
;;;    whether the cursor is left-to-right or right-to-left. Should never be
;;;    GTK_TEXT_DIR_NONE
;;;
;;;    draw_arrow :
;;;    TRUE to draw a directional arrow on the cursor. Should be
;;;    FALSE unless the cursor is split.
;;;
;;;    Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkRcProperty
;;;
;;; typedef struct {
;;;   /* quark-ified property identifier like "GtkScrollbar::spacing" */
;;;   GQuark type_name;
;;;   GQuark property_name;
;;;
;;;   /* fields similar to GtkSettingsValue */
;;;   gchar *origin;
;;;   GValue value;
;;; } GtkRcProperty;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkRcPropertyParser ()
;;;
;;; gboolean (*GtkRcPropertyParser) (const GParamSpec *pspec,
;;;                                  const GString *rc_string,
;;;                                  GValue *property_value);
;;; ----------------------------------------------------------------------------
|#

;;; --- End of file atdoc-gtk.style.lisp ---------------------------------------
