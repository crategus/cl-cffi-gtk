;;; ----------------------------------------------------------------------------
;;; gdk.window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;; Windows
;;; 
;;; Onscreen display areas in the target window system
;;; 
;;; Synopsis
;;;
;;;     GdkWindowType
;;;     GdkWindowClass
;;;     GdkWindowHints
;;;     GdkWindow
;;;     GdkGravity
;;;     GdkWindowEdge
;;;     GdkWindowTypeHint
;;;     GdkGeometry
;;;     GdkWindowAttributesType
;;;     GdkWindowAttr
;;;     
;;;     gdk_window_new
;;;     gdk_window_destroy
;;;     gdk_window_ref
;;;     gdk_window_unref
;;;     gdk_window_get_display
;;;     gdk_window_get_screen
;;;     gdk_window_get_visual
;;;     gdk_window_get_width
;;;     gdk_window_get_height
;;;     gdk_window_get_window_type
;;;     gdk_window_at_pointer
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide
;;;     gdk_window_is_destroyed
;;;     gdk_window_is_visible
;;;     gdk_window_is_viewable
;;;     gdk_window_is_shaped
;;;     gdk_window_is_input_only
;;;     gdk_window_get_state
;;;     gdk_window_withdraw
;;;     gdk_window_iconify
;;;     gdk_window_deiconify
;;;     gdk_window_stick
;;;     gdk_window_unstick
;;;     gdk_window_maximize
;;;     gdk_window_unmaximize
;;;     gdk_window_fullscreen
;;;     gdk_window_unfullscreen
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited
;;;     gdk_window_get_composited
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_region
;;;     gdk_window_flush
;;;     gdk_window_has_native
;;;     gdk_window_ensure_native
;;;     gdk_window_reparent
;;;     gdk_window_clear
;;;     gdk_window_clear_area
;;;     gdk_window_clear_area_e
;;;     gdk_window_copy_area
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_move_drag
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     
;;;     gdk_window_begin_paint_rect
;;;     gdk_window_begin_paint_region
;;;     gdk_window_end_paint
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates
;;;     gdk_window_process_updates
;;;     gdk_window_set_debug_updates
;;;     gdk_window_get_internal_paint_info
;;;     gdk_window_enable_synchronized_configure
;;;     gdk_window_configure_finished
;;;     gdk_window_set_user_data
;;;     gdk_window_set_override_redirect
;;;     gdk_window_set_accept_focus
;;;     gdk_window_get_accept_focus
;;;     gdk_window_set_focus_on_map
;;;     gdk_window_get_focus_on_map
;;;     gdk_window_add_filter
;;;     gdk_window_remove_filter
;;;
;;;     GdkFilterReturn
;;;     GdkXEvent
;;;
;;;     gdk_window_shape_combine_mask
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_mask
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities
;;;     gdk_window_set_hints
;;;     gdk_window_set_title
;;;     gdk_window_set_background
;;;     gdk_window_set_back_pixmap
;;;     gdk_window_get_background_pattern
;;;     GDK_PARENT_RELATIVE
;;;     gdk_window_set_cursor
;;;     gdk_window_get_cursor
;;;     gdk_window_set_colormap
;;;     gdk_window_get_user_data
;;;     gdk_window_get_geometry
;;;     gdk_window_set_geometry_hints
;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint
;;;     gdk_window_get_position
;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_size
;;;     gdk_window_get_colormap
;;;     gdk_window_get_type
;;;     gdk_window_get_origin
;;;     gdk_window_get_deskrelative_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_coords_from_parent
;;;     gdk_window_coords_to_parent
;;;     gdk_window_get_pointer
;;;
;;;     GdkModifierType
;;;
;;;     gdk_window_get_parent
;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_effective_toplevel
;;;     gdk_window_get_children
;;;     gdk_window_peek_children
;;;     gdk_window_get_events
;;;     gdk_window_set_events
;;;     gdk_window_set_icon
;;;     gdk_window_set_icon_name
;;;     gdk_window_set_transient_for
;;;     gdk_window_set_role
;;;     gdk_window_set_startup_id
;;;     gdk_window_set_group
;;;     gdk_window_get_group
;;;     gdk_window_set_decorations
;;;     gdk_window_get_decorations
;;;
;;;     GdkWMDecoration
;;;
;;;     gdk_window_set_functions
;;;
;;;     GdkWMFunction
;;;
;;;     gdk_window_get_toplevels
;;;     gdk_get_default_root_window
;;;     
;;;     GdkPointerHooks
;;;
;;;     gdk_set_pointer_hooks     
;;;     gdk_offscreen_window_get_pixmap
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed
;;;     gdk_window_redirect_to_drawable
;;;     gdk_window_remove_redirection
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GdkDrawable
;;;          +----GdkWindow
;;; 
;;; Properties
;;; 
;;;   "cursor"                   GdkCursor*            : Read / Write
;;; 
;;; Signals
;;; 
;;;   "from-embedder"                                  : Run Last
;;;   "pick-embedded-child"                            : Run Last
;;;   "to-embedder"                                    : Run Last
;;; 
;;; Description
;;; 
;;; A GdkWindow is a rectangular region on the screen. It's a low-level object,
;;; used to implement high-level objects such as GtkWidget and GtkWindow on the
;;; GTK+ level. A GtkWindow is a toplevel window, the thing a user might think
;;; of as a "window" with a titlebar and so on; a GtkWindow may contain many
;;; GdkWindow. For example, each GtkButton has a GdkWindow associated with it.
;;;
;;; Composited Windows
;;; 
;;; Normally, the windowing system takes care of rendering the contents of a
;;; child window onto its parent window. This mechanism can be intercepted by
;;; calling gdk_window_set_composited() on the child window. For a composited
;;; window it is the responsibility of the application to render the window
;;; contents at the right spot.
;;; 
;;; Example 7. Composited windows
;;; 
;;;  #include <gtk/gtk.h>
;;;  /* The expose event handler for the event box.
;;;   *
;;;   * This function simply draws a transparency onto a widget on the area
;;;   * for which it receives expose events.  This is intended to give the
;;;   * event box a "transparent" background.
;;;   *
;;;   * In order for this to work properly, the widget must have an RGBA
;;;   * colourmap.  The widget should also be set as app-paintable since it
;;;   * doesn't make sense for GTK+ to draw a background if we are drawing it
;;;   * (and because GTK+ might actually replace our transparency with its
;;;   * default background colour).
;;;   */
;;;  static gboolean
;;;  transparent_expose (GtkWidget      *widget,
;;;                      GdkEventExpose *event)
;;;  {
;;;    cairo_t *cr;
;;;    cr = gdk_cairo_create (widget->window);
;;;    cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
;;;    gdk_cairo_region (cr, event->region);
;;;    cairo_fill (cr);
;;;    cairo_destroy (cr);
;;;    return FALSE;
;;;  }
;;;  /* The expose event handler for the window.
;;;   *
;;;   * This function performs the actual compositing of the event box onto
;;;   * the already-existing background of the window at 50% normal opacity.
;;;   *
;;;   * In this case we do not want app-paintable to be set on the widget
;;;   * since we want it to draw its own (red) background. Because of this,
;;;   * however, we must ensure that we use g_signal_connect_after so that
;;;   * this handler is called after the red has been drawn. If it was
;;;   * called before then GTK would just blindly paint over our work.
;;;   *
;;;   * Note: if the child window has children, then you need a cairo 1.6
;;;   * feature to make this work correctly.
;;;   */
;;;  static gboolean
;;;  window_expose_event (GtkWidget      *widget,
;;;                      GdkEventExpose *event)
;;;  {
;;;    GdkRegion *region;
;;;    GtkWidget *child;
;;;    cairo_t *cr;
;;;    /* get our child (in this case, the event box) */
;;;    child = gtk_bin_get_child (GTK_BIN (widget));
;;;    /* create a cairo context to draw to the window */
;;;    cr = gdk_cairo_create (widget->window);
;;;    /* the source data is the (composited) event box */
;;;    gdk_cairo_set_source_pixmap (cr, child->window,
;;;                                 child->allocation.x,
;;;                                 child->allocation.y);
;;;    /* draw no more than our expose event intersects our child */
;;;    region = gdk_region_rectangle (&child->allocation);
;;;    gdk_region_intersect (region, event->region);
;;;    gdk_cairo_region (cr, region);
;;;    cairo_clip (cr);
;;;    /* composite, with a 50% opacity */
;;;    cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
;;;    cairo_paint_with_alpha (cr, 0.5);
;;;    /* we're done */
;;;    cairo_destroy (cr);
;;;    return FALSE;
;;;  }
;;;  int
;;;  main (int argc, char **argv)
;;;  {
;;;    GtkWidget *window, *event, *button;
;;;    GdkScreen *screen;
;;;    GdkColormap *rgba;
;;;    GdkColor red;
;;;    gtk_init (&argc, &argv);
;;;    /* Make the widgets */
;;;    button = gtk_button_new_with_label ("A Button");
;;;    event = gtk_event_box_new ();
;;;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;    /* Put a red background on the window */
;;;    gdk_color_parse ("red", &red);
;;;    gtk_widget_modify_bg (window, GTK_STATE_NORMAL, &red);
;;;    /* Set the colourmap for the event box.
;;;     * Must be done before the event box is realised.
;;;     */
;;;    screen = gtk_widget_get_screen (event);
;;;    rgba = gdk_screen_get_rgba_colormap (screen);
;;;    gtk_widget_set_colormap (event, rgba);
;;;    /* Set our event box to have a fully-transparent background
;;;     * drawn on it. Currently there is no way to simply tell GTK+
;;;     * that "transparency" is the background colour for a widget.
;;;     */
;;;    gtk_widget_set_app_paintable (GTK_WIDGET (event), TRUE);
;;;    g_signal_connect (event, "expose-event",
;;;                      G_CALLBACK (transparent_expose), NULL);
;;;    /* Put them inside one another */
;;;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;;;    gtk_container_add (GTK_CONTAINER (window), event);
;;;    gtk_container_add (GTK_CONTAINER (event), button);
;;;    /* Realise and show everything */
;;;    gtk_widget_show_all (window);
;;;    /* Set the event box GdkWindow to be composited.
;;;     * Obviously must be performed after event box is realised.
;;;     */
;;;    gdk_window_set_composited (event->window, TRUE);
;;;    /* Set up the compositing handler.
;;;     * Note that we do _after_ so that the normal red background is drawn
;;;     * by gtk before our compositing occurs.
;;;     */
;;;    g_signal_connect_after (window, "expose-event",
;;;                            G_CALLBACK (window_expose_event), NULL);
;;;    gtk_main ();
;;;    return 0;
;;;  }
;;; 
;;; In the example Example 7, “Composited windows”, a button is placed inside
;;; of an event box inside of a window. The event box is set as composited and
;;; therefore is no longer automatically drawn to the screen.
;;; 
;;; When the contents of the event box change, an expose event is generated on
;;; its parent window (which, in this case, belongs to the toplevel GtkWindow).
;;; The expose handler for this widget is responsible for merging the changes
;;; back on the screen in the way that it wishes.
;;; 
;;; In our case, we merge the contents with a 50% transparency. We also set the
;;; background colour of the window to red. The effect is that the background
;;; shows through the button.
;;;
;;; Offscreen Windows
;;; 
;;; Offscreen windows are more general than composited windows, since they
;;; allow not only to modify the rendering of the child window onto its parent,
;;; but also to apply coordinate transformations.
;;; 
;;; To integrate an offscreen window into a window hierarchy, one has to call
;;; gdk_offscreen_window_set_embedder() and handle a number of signals. The
;;; "pick-embedded-child" signal on the embedder window is used to select an
;;; offscreen child at given coordinates, and the "to-embedder" and
;;; "from-embedder" signals on the offscreen window are used to translate
;;; coordinates between the embedder and the offscreen window.
;;; 
;;; For rendering an offscreen window onto its embedder, the contents of the
;;; offscreen window are available as a pixmap, via
;;; gdk_offscreen_window_get_pixmap().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor" property
;;; 
;;;   "cursor" GdkCursor*            : Read / Write
;;; 
;;; The mouse pointer for a GdkWindow. See gdk_window_set_cursor() and
;;; gdk_window_get_cursor() for details.
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "from-embedder" signal
;;; 
;;; void user_function (GdkWindow *window,
;;;                     gdouble    embedder-x,
;;;                     gdouble    embedder-y,
;;;                     gpointer   offscreen-x,
;;;                     gpointer   offscreen-y,
;;;                     gpointer   user_data)        : Run Last
;;; 
;;; The ::from-embedder signal is emitted to translate coordinates in the
;;; embedder of an offscreen window to the offscreen window.
;;; 
;;; See also "to-embedder".
;;; 
;;; window :
;;;     the offscreen window on which the signal is emitted
;;; 
;;; embedder-x :
;;;     x coordinate in the embedder window
;;; 
;;; embedder-y :
;;;     y coordinate in the embedder window
;;; 
;;; offscreen-x :
;;;     return location for the x coordinate in the offscreen window.
;;; 
;;; offscreen-y :
;;;     return location for the y coordinate in the offscreen window.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "pick-embedded-child" signal
;;; 
;;; GdkWindow* user_function (GdkWindow *window,
;;;                           gdouble    x,
;;;                           gdouble    y,
;;;                           gpointer   user_data)      : Run Last
;;; 
;;; The ::pick-embedded-child signal is emitted to find an embedded child at
;;; the given position.
;;; 
;;; window :
;;;     the window on which the signal is emitted
;;; 
;;; x :
;;;     x coordinate in the window
;;; 
;;; y :
;;;     y coordinate in the window
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     the GdkWindow of the embedded child at x, y, or NULL
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "to-embedder" signal
;;; 
;;; void user_function (GdkWindow *window,
;;;                     gdouble    offscreen-x,
;;;                     gdouble    offscreen-y,
;;;                     gpointer   embedder-x,
;;;                     gpointer   embedder-y,
;;;                     gpointer   user_data)        : Run Last
;;; 
;;; The ::to-embedder signal is emitted to translate coordinates in an
;;; offscreen window to its embedder.
;;; 
;;; See also "from-embedder".
;;; 
;;; window :
;;;     the offscreen window on which the signal is emitted
;;; 
;;; offscreen-x :
;;;     x coordinate in the offscreen window
;;; 
;;; offscreen-y :
;;;     y coordinate in the offscreen window
;;; 
;;; embedder-x :
;;;     return location for the x coordinate in the embedder window.
;;; 
;;; embedder-y :
;;;     return location for the y coordinate in the embedder window.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkWMDecoration
;;; 
;;; typedef enum
;;; {
;;;   GDK_DECOR_ALL      = 1 << 0,
;;;   GDK_DECOR_BORDER   = 1 << 1,
;;;   GDK_DECOR_RESIZEH  = 1 << 2,
;;;   GDK_DECOR_TITLE    = 1 << 3,
;;;   GDK_DECOR_MENU     = 1 << 4,
;;;   GDK_DECOR_MINIMIZE = 1 << 5,
;;;   GDK_DECOR_MAXIMIZE = 1 << 6
;;; } GdkWMDecoration;
;;; 
;;; These are hints originally defined by the Motif toolkit. The window manager
;;; can use them when determining how to decorate the window. The hint must be
;;; set before mapping the window.
;;; 
;;; GDK_DECOR_ALL
;;;     all decorations should be applied.
;;; 
;;; GDK_DECOR_BORDER
;;;     a frame should be drawn around the window.
;;; 
;;; GDK_DECOR_RESIZEH
;;;     the frame should have resize handles.
;;; 
;;; GDK_DECOR_TITLE
;;;     a titlebar should be placed above the window.
;;; 
;;; GDK_DECOR_MENU
;;;     a button for opening a menu should be included.
;;; 
;;; GDK_DECOR_MINIMIZE
;;;     a minimize button should be included.
;;; 
;;; GDK_DECOR_MAXIMIZE
;;;     a maximize button should be included.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWMDecoration" gdk-wm-decoration
  (:export t
   :type-initializer "gdk_wm_decoration_get_type")
  (:all 1)
  (:border 2)
  (:resizeh 4)
  (:title 8)
  (:menu 16)
  (:minimize 32)
  (:maximize 64))

;;; ----------------------------------------------------------------------------
;;; enum GdkWMFunction
;;; 
;;; typedef enum
;;; {
;;;   GDK_FUNC_ALL      = 1 << 0,
;;;   GDK_FUNC_RESIZE   = 1 << 1,
;;;   GDK_FUNC_MOVE     = 1 << 2,
;;;   GDK_FUNC_MINIMIZE = 1 << 3,
;;;   GDK_FUNC_MAXIMIZE = 1 << 4,
;;;   GDK_FUNC_CLOSE    = 1 << 5
;;; } GdkWMFunction;
;;; 
;;; These are hints originally defined by the Motif toolkit. The window manager
;;; can use them when determining the functions to offer for the window. The
;;; hint must be set before mapping the window.
;;; 
;;; GDK_FUNC_ALL
;;;     all functions should be offered.
;;; 
;;; GDK_FUNC_RESIZE
;;;     the window should be resizable.
;;; 
;;; GDK_FUNC_MOVE
;;;     the window should be movable.
;;; 
;;; GDK_FUNC_MINIMIZE
;;;     the window should be minimizable.
;;; 
;;; GDK_FUNC_MAXIMIZE
;;;     the window should be maximizable.
;;; 
;;; GDK_FUNC_CLOSE
;;;     the window should be closable.
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWMFunction" gdk-wm-function
  (:export t
   :type-initializer "gdk_wm_function_get_type")
  (:all 1)
  (:resize 2)
  (:move 4)
  (:minimize 8)
  (:maximize 16)
  (:close 32))

;;; ----------------------------------------------------------------------------
;;; struct GdkWindow
;;; 
;;; struct GdkWindow;
;;; 
;;; An opaque structure representing an onscreen drawable. Pointers to
;;; structures of type GdkPixmap, GdkBitmap, and GdkWindow, can often be used
;;; interchangeably. The type GdkDrawable refers generically to any of these
;;; types.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkWindow" gdk-window
   (:superclass g-object
    :export t
    :interfaces ()
    :type-initializer "gdk_window_get_type"
    )
   ((cursor
     gdk-window-cursor
     "cursor" "GdkCursor" t t)
    (:cffi window-type
           gdk-window-window-type gdk-window-type
           "gdk_window_get_window_type" nil)
    (:cffi is-destroyed
           gdk-window-is-destroyed :boolean
           "gdk_window_is_destroyed" nil)
    (:cffi is-visible
           gdk-window-is-visible :boolean
           "gdk_window_is_visible" nil)
    (:cffi is-viewable
           gdk-window-is-viewable :boolean
           "gdk_window_is_viewable" nil)
    (:cffi state
           gdk-window-state gdk-window-state
           "gdk_window_get_state" nil)
    (:cffi keep-above
           gdk-window-keep-above :boolean 
           nil "gdk_window_set_keep_above")
    (:cffi keep-below
           gdk-window-keep-below :boolean 
           nil "gdk_window_set_keep_below" )
    (:cffi opacity
           gdk-window-opacity :double
           nil "gdk_window_set_opacity")
    (:cffi composited
           gdk-window-composited :boolean 
           nil "gdk_window_set_composited")
    (:cffi user-data
           gdk-window-user-data :pointer
           "gdk_window_get_user_data" "gdk_window_set_user_data")
    (:cffi override-redirect
           gdk-window-override-redirect :boolean
           nil "gdk_window_set_override_redirect")
    (:cffi accept-focus
           gdk-window-accept-focus :boolean
           nil "gdk_window_set_accept_focus")
    (:cffi focus-on-map
           gdk-window-focus-on-map :boolean
           nil "gdk_window_set_focus_on_map")
    (:cffi title
           gdk-window-title :string
           nil "gdk_window_set_title")
    (:cffi background
           gdk-window-background (g-boxed-foreign gdk-color)
           nil "gdk_window_set_background")
    (:cffi icon-list
           gdk-window-icon-list (g-list (g-object gdk-pixbuf))
           nil "gdk_window_set_icon_list")
    (:cffi modal-hint
           gdk-window-modal-hint :boolean
           nil "gdk_window_set_modal_hint")
    (:cffi type-hint
           gdk-window-type-hint gdk-window-type-hint
           "gdk_window_get_type_hint" "gdk_window_set_type_hint")
    (:cffi skip-taskbar-hint
           gdk-window-skip-taskbar-hint :boolean
           nil "gdk_window_set_skip_taskbar_hint")
    (:cffi skip-pager-hint
           gdk-window-skip-pager-hint :boolean
           nil "gdk_window_set_skip_pager_hint")
    (:cffi urgency-hint
           gdk-window-urgency-hint :boolean
           nil "gdk_window_set_urgency_hint")
    (:cffi parent
           gdk-window-parent (g-object gdk-window)
           "gdk_window_get_parent" nil)
    (:cffi toplevel
           gdk-window-get-toplevel (g-object gdk-window)
           "gdk_window_get_toplevel" nil)
    (:cffi children
           gdk-window-children
           (g-list (g-object gdk-window) :free-from-foreign nil)
           "gdk_window_peek_children" nil)
    (:cffi events
           gdk-window-events gdk-event-mask
           "gdk_window_get_events" "gdk_window_set_events")
    (:cffi icon-name
           gdk-window-icon-name :string
           nil "gdk_window_set_icon_name")
    (:cffi transient-for
           gdk-window-transient-for (g-object gdk-window)
           nil "gdk_window_set_transient_for")
    (:cffi role
           gdk-window-role :string
           nil "gdk_window_set_role")
    (:cffi startup-id
           gdk-window-startup-id :string
           nil "gdk_window_set_startup_id")
    (:cffi group
           gdk-window-group (g-object gdk-window)
           "gdk_window_get_group" "gdk_window_set_group")
    (:cffi decorations
           gdk-window-decoration gdk-wm-decoration
           gdk-window-get-decorations "gdk_window_set_decorations")
    (:cffi functions
           gdk-window-functions gdk-wm-function
           nil "gdk_window_set_functions")))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowType
;;; 
;;; typedef enum
;;; {
;;;   GDK_WINDOW_ROOT,
;;;   GDK_WINDOW_TOPLEVEL,
;;;   GDK_WINDOW_CHILD,
;;;   GDK_WINDOW_DIALOG,
;;;   GDK_WINDOW_TEMP,
;;;   GDK_WINDOW_FOREIGN,
;;;   GDK_WINDOW_OFFSCREEN
;;; } GdkWindowType;
;;; 
;;; Describes the kind of window.
;;; 
;;; GDK_WINDOW_ROOT
;;;     root window; this window has no parent, covers the entire screen,
;;;     and is created by the window system
;;; 
;;; GDK_WINDOW_TOPLEVEL
;;;     toplevel window (used to implement GtkWindow)
;;; 
;;; GDK_WINDOW_CHILD
;;;     child window (used to implement e.g. GtkEntry)
;;; 
;;; GDK_WINDOW_DIALOG
;;;     useless/deprecated compatibility type
;;; 
;;; GDK_WINDOW_TEMP
;;;     override redirect temporary window (used to implement GtkMenu)
;;; 
;;; GDK_WINDOW_FOREIGN
;;;     foreign window (see gdk_window_foreign_new())
;;; 
;;; GDK_WINDOW_OFFSCREEN
;;;     offscreen window (see the section called “Offscreen Windows”).
;;;     Since 2.18
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowType" gdk-window-type
  (:export t
   :type-initializer "gdk_window_type_get_type")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:dialog 3)
  (:temp 4)
  (:foreign 5)
  (:offscreen 6))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowClass
;;; 
;;; typedef enum
;;; {
;;;   GDK_INPUT_OUTPUT,
;;;   GDK_INPUT_ONLY
;;; } GdkWindowClass;
;;; 
;;; GDK_INPUT_OUTPUT windows are the standard kind of window you might expect.
;;; GDK_INPUT_ONLY windows are invisible; they are used to trap events, but you
;;; can't draw on them.
;;; 
;;; GDK_INPUT_OUTPUT
;;;     window for graphics and events
;;; 
;;; GDK_INPUT_ONLY
;;;     window for events only
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowClass" gdk-window-class
  (:export t
;   :type-initializer "gdk_window_class_get_type"
   )
  (:input-output 0)
  (:input-only 1))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowHints
;;; 
;;; typedef enum
;;; {
;;;   GDK_HINT_POS         = 1 << 0,
;;;   GDK_HINT_MIN_SIZE    = 1 << 1,
;;;   GDK_HINT_MAX_SIZE    = 1 << 2,
;;;   GDK_HINT_BASE_SIZE   = 1 << 3,
;;;   GDK_HINT_ASPECT      = 1 << 4,
;;;   GDK_HINT_RESIZE_INC  = 1 << 5,
;;;   GDK_HINT_WIN_GRAVITY = 1 << 6,
;;;   GDK_HINT_USER_POS    = 1 << 7,
;;;   GDK_HINT_USER_SIZE   = 1 << 8
;;; } GdkWindowHints;
;;; 
;;; Used to indicate which fields of a GdkGeometry struct should be paid
;;; attention to. Also, the presence/absence of GDK_HINT_POS, GDK_HINT_USER_POS,
;;; and GDK_HINT_USER_SIZE is significant, though they don't directly refer to
;;; GdkGeometry fields. GDK_HINT_USER_POS will be set automatically by GtkWindow
;;; if you call gtk_window_move(). GDK_HINT_USER_POS and GDK_HINT_USER_SIZE
;;; should be set if the user specified a size/position using a --geometry
;;; command-line argument; gtk_window_parse_geometry() automatically sets these
;;; flags.
;;; 
;;; GDK_HINT_POS
;;;     indicates that the program has positioned the window
;;; 
;;; GDK_HINT_MIN_SIZE
;;;     min size fields are set
;;; 
;;; GDK_HINT_MAX_SIZE
;;;     max size fields are set
;;; 
;;; GDK_HINT_BASE_SIZE
;;;     base size fields are set
;;; 
;;; GDK_HINT_ASPECT
;;;     aspect ratio fields are set
;;; 
;;; GDK_HINT_RESIZE_INC
;;;     resize increment fields are set
;;; 
;;; GDK_HINT_WIN_GRAVITY
;;;     window gravity field is set
;;; 
;;; GDK_HINT_USER_POS
;;;     indicates that the window's position was explicitly set by the user
;;; 
;;; GDK_HINT_USER_SIZE
;;;     indicates that the window's size was explicitly set by the user
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowHints" gdk-window-hints
  (:export t
   :type-initializer "gdk_window_hints_get_type")
  (:pos 1)
  (:min-size 2)
  (:max-size 4)
  (:base-size 8)
  (:aspect 16)
  (:resize-inc 32)
  (:win-gravity 64)
  (:user-pos 128)
  (:user-size 256))

;;; ----------------------------------------------------------------------------
;;; enum GdkGravity
;;; 
;;; typedef enum
;;; {
;;;   GDK_GRAVITY_NORTH_WEST = 1,
;;;   GDK_GRAVITY_NORTH,
;;;   GDK_GRAVITY_NORTH_EAST,
;;;   GDK_GRAVITY_WEST,
;;;   GDK_GRAVITY_CENTER,
;;;   GDK_GRAVITY_EAST,
;;;   GDK_GRAVITY_SOUTH_WEST,
;;;   GDK_GRAVITY_SOUTH,
;;;   GDK_GRAVITY_SOUTH_EAST,
;;;   GDK_GRAVITY_STATIC
;;; } GdkGravity;
;;; 
;;; Defines the reference point of a window and the meaning of coordinates
;;; passed to gtk_window_move(). See gtk_window_move() and the "implementation
;;; notes" section of the Extended Window Manager Hints specification for more
;;; details.
;;; 
;;; GDK_GRAVITY_NORTH_WEST
;;;     the reference point is at the top left corner.
;;; 
;;; GDK_GRAVITY_NORTH
;;;     the reference point is in the middle of the top edge.
;;; 
;;; GDK_GRAVITY_NORTH_EAST
;;;     the reference point is at the top right corner.
;;; 
;;; GDK_GRAVITY_WEST
;;;     the reference point is at the middle of the left edge.
;;; 
;;; GDK_GRAVITY_CENTER
;;;     the reference point is at the center of the window.
;;; 
;;; GDK_GRAVITY_EAST
;;;     the reference point is at the middle of the right edge.
;;; 
;;; GDK_GRAVITY_SOUTH_WEST
;;;     the reference point is at the lower left corner.
;;; 
;;; GDK_GRAVITY_SOUTH
;;;     the reference point is at the middle of the lower edge.
;;; 
;;; GDK_GRAVITY_SOUTH_EAST
;;;     the reference point is at the lower right corner.
;;; 
;;; GDK_GRAVITY_STATIC
;;;     the reference point is at the top left corner of the window itself,
;;;     ignoring window manager decorations.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGravity" gdk-gravity
  ()
  (:north-west 1)
  :north
  :north-east
  :west
  :center
  :east
  :south-west
  :south
  :south-east
  :static)

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowEdge
;;; 
;;; typedef enum
;;; {
;;;   GDK_WINDOW_EDGE_NORTH_WEST,
;;;   GDK_WINDOW_EDGE_NORTH,
;;;   GDK_WINDOW_EDGE_NORTH_EAST,
;;;   GDK_WINDOW_EDGE_WEST,
;;;   GDK_WINDOW_EDGE_EAST,
;;;   GDK_WINDOW_EDGE_SOUTH_WEST,
;;;   GDK_WINDOW_EDGE_SOUTH,
;;;   GDK_WINDOW_EDGE_SOUTH_EAST  
;;; } GdkWindowEdge;
;;; 
;;; Determines a window edge or corner.
;;; 
;;; GDK_WINDOW_EDGE_NORTH_WEST
;;;     the top left corner.
;;; 
;;; GDK_WINDOW_EDGE_NORTH
;;;     the top edge.
;;; 
;;; GDK_WINDOW_EDGE_NORTH_EAST
;;;     the top right corner.
;;; 
;;; GDK_WINDOW_EDGE_WEST
;;;     the left edge.
;;; 
;;; GDK_WINDOW_EDGE_EAST
;;;     the right edge.
;;; 
;;; GDK_WINDOW_EDGE_SOUTH_WEST
;;;     the lower left corner.
;;; 
;;; GDK_WINDOW_EDGE_SOUTH
;;;     the lower edge.
;;; 
;;; GDK_WINDOW_EDGE_SOUTH_EAST
;;;     the lower right corner.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowEdge" gdk-window-edge
  (:export t :type-initializer "gdk_window_edge_get_type")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowTypeHint
;;; 
;;; typedef enum
;;; {
;;;   GDK_WINDOW_TYPE_HINT_NORMAL,
;;;   GDK_WINDOW_TYPE_HINT_DIALOG,
;;;   GDK_WINDOW_TYPE_HINT_MENU,          /* Torn off menu */
;;;   GDK_WINDOW_TYPE_HINT_TOOLBAR,
;;;   GDK_WINDOW_TYPE_HINT_SPLASHSCREEN,
;;;   GDK_WINDOW_TYPE_HINT_UTILITY,
;;;   GDK_WINDOW_TYPE_HINT_DOCK,
;;;   GDK_WINDOW_TYPE_HINT_DESKTOP,
;;;   GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU, /* A drop down menu (from a menubar)*/
;;;   GDK_WINDOW_TYPE_HINT_POPUP_MENU,    /* A popup menu (from right-click) */
;;;   GDK_WINDOW_TYPE_HINT_TOOLTIP,
;;;   GDK_WINDOW_TYPE_HINT_NOTIFICATION,
;;;   GDK_WINDOW_TYPE_HINT_COMBO,
;;;   GDK_WINDOW_TYPE_HINT_DND
;;; } GdkWindowTypeHint;
;;; 
;;; These are hints for the window manager that indicate what type of function
;;; the window has. The window manager can use this when determining decoration
;;; and behaviour of the window. The hint must be set before mapping the window.
;;; 
;;; See the Extended Window Manager Hints specification for more details about
;;; window types.
;;; 
;;; GDK_WINDOW_TYPE_HINT_NORMAL
;;;     Normal toplevel window.
;;; 
;;; GDK_WINDOW_TYPE_HINT_DIALOG
;;;     Dialog window.
;;; 
;;; GDK_WINDOW_TYPE_HINT_MENU
;;;     Window used to implement a menu; GTK+ uses this hint only for torn-off
;;;     menus, see GtkTearoffMenuItem.
;;; 
;;; GDK_WINDOW_TYPE_HINT_TOOLBAR
;;;     Window used to implement toolbars.
;;; 
;;; GDK_WINDOW_TYPE_HINT_SPLASHSCREEN
;;;     Window used to display a splash screen during application startup.
;;; 
;;; GDK_WINDOW_TYPE_HINT_UTILITY
;;;     Utility windows which are not detached toolbars or dialogs.
;;; 
;;; GDK_WINDOW_TYPE_HINT_DOCK
;;;     Used for creating dock or panel windows.
;;; 
;;; GDK_WINDOW_TYPE_HINT_DESKTOP
;;;     Used for creating the desktop background window.
;;; 
;;; GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU
;;;     A menu that belongs to a menubar.
;;; 
;;; GDK_WINDOW_TYPE_HINT_POPUP_MENU
;;;     A menu that does not belong to a menubar, e.g. a context menu.
;;; 
;;; GDK_WINDOW_TYPE_HINT_TOOLTIP
;;;     A tooltip.
;;; 
;;; GDK_WINDOW_TYPE_HINT_NOTIFICATION
;;;     A notification - typically a "bubble" that belongs to a status icon.
;;; 
;;; GDK_WINDOW_TYPE_HINT_COMBO
;;;     A popup from a combo box.
;;; 
;;; GDK_WINDOW_TYPE_HINT_DND
;;;     A window that is used to implement a DND cursor.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowTypeHint" gdk-window-type-hint
  (:export t
   :type-initializer "gdk_window_type_hint_get_type")
  (:normal 0)
  (:dialog 1)
  (:menu 2)
  (:toolbar 3)
  (:splashscreen 4)
  (:utility 5)
  (:dock 6)
  (:desktop 7)
  (:dropdown-menu 8)
  (:popup-menu 9)
  (:tooltip 10)
  (:notification 11)
  (:combo 12)
  (:dnd 13))

;;; ----------------------------------------------------------------------------
;;; struct GdkGeometry
;;; 
;;; struct GdkGeometry {
;;;   gint min_width;
;;;   gint min_height;
;;;   gint max_width;
;;;   gint max_height;
;;;   gint base_width;
;;;   gint base_height;
;;;   gint width_inc;
;;;   gint height_inc;
;;;   gdouble min_aspect;
;;;   gdouble max_aspect;
;;;   GdkGravity win_gravity;
;;; };
;;; 
;;; The GdkGeometry struct gives the window manager information about a window's
;;; geometry constraints. Normally you would set these on the GTK+ level using
;;; gtk_window_set_geometry_hints(). GtkWindow then sets the hints on the
;;; GdkWindow it creates.
;;; 
;;; gdk_window_set_geometry_hints() expects the hints to be fully valid already
;;; and simply passes them to the window manager; in contrast,
;;; gtk_window_set_geometry_hints() performs some interpretation. For example,
;;; GtkWindow will apply the hints to the geometry widget instead of the
;;; toplevel window, if you set a geometry widget. Also, the
;;; min_width/min_height/max_width/max_height fields may be set to -1, and
;;; GtkWindow will substitute the size request of the window or geometry widget.
;;; If the minimum size hint is not provided, GtkWindow will use its requisition
;;; as the minimum size. If the minimum size is provided and a geometry widget
;;; is set, GtkWindow will take the minimum size as the minimum size of the
;;; geometry widget rather than the entire window. The base size is treated
;;; similarly.
;;; 
;;; The canonical use-case for gtk_window_set_geometry_hints() is to get a
;;; terminal widget to resize properly. Here, the terminal text area should be
;;; the geometry widget; GtkWindow will then automatically set the base size to
;;; the size of other widgets in the terminal window, such as the menubar and
;;; scrollbar. Then, the width_inc and height_inc fields should be set to the
;;; size of one character in the terminal. Finally, the base size should be set
;;; to the size of one character. The net effect is that the minimum size of
;;; the terminal will have a 1x1 character terminal area, and only terminal
;;; sizes on the "character grid" will be allowed.
;;; 
;;; Here's an example of how the terminal example would be implemented, assuming
;;; a terminal area widget called "terminal" and a toplevel window "toplevel":
;;; 
;;;  GdkGeometry hints;
;;;  hints.base_width = terminal->char_width;
;;;          hints.base_height = terminal->char_height;
;;;          hints.min_width = terminal->char_width;
;;;          hints.min_height = terminal->char_height;
;;;          hints.width_inc = terminal->char_width;
;;;          hints.height_inc = terminal->char_height;
;;;  gtk_window_set_geometry_hints (GTK_WINDOW (toplevel),
;;;                     GTK_WIDGET (terminal),
;;;                         &hints,
;;;                     GDK_HINT_RESIZE_INC |
;;;                                         GDK_HINT_MIN_SIZE |
;;;                                         GDK_HINT_BASE_SIZE);
;;; 
;;; The other useful fields are the min_aspect and max_aspect fields; these
;;; contain a width/height ratio as a floating point number. If a geometry
;;; widget is set, the aspect applies to the geometry widget rather than the
;;; entire window. The most common use of these hints is probably to set
;;; min_aspect and max_aspect to the same value, thus forcing the window to
;;; keep a constant aspect ratio.
;;; 
;;; gint min_width;
;;;     minimum width of window (or -1 to use requisition, with GtkWindow only)
;;; 
;;; gint min_height;
;;;     minimum height of window (or -1 to use requisition, with GtkWindow only)
;;; 
;;; gint max_width;
;;;     maximum width of window (or -1 to use requisition, with GtkWindow only)
;;; 
;;; gint max_height;
;;;     maximum height of window (or -1 to use requisition, with GtkWindow only)
;;; 
;;; gint base_width;
;;;     allowed window widths are base_width + width_inc * N where N is any
;;;     integer (-1 allowed with GtkWindow)
;;; 
;;; gint base_height;
;;;     allowed window widths are base_height + height_inc * N where N is any
;;;     integer (-1 allowed with GtkWindow)
;;; 
;;; gint width_inc;
;;;     width resize increment
;;; 
;;; gint height_inc;
;;;     height resize increment
;;; 
;;; gdouble min_aspect;
;;;     minimum width/height ratio
;;; 
;;; gdouble max_aspect;
;;;     maximum width/height ratio
;;; 
;;; GdkGravity win_gravity;
;;;     window gravity, see gtk_window_set_gravity()
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-geometry
  ()
  (min-width :int :initform 0)
  (min-height :int :initform 0)
  (max-width :int :initform 0)
  (max-height :int :initform 0)
  (base-width :int :initform 0)
  (base-height :int :initform 0)
  (width-increment :int :initform 0)
  (height-increment :int :initform 0)
  (min-aspect :double :initform 0.0d0)
  (max-aspect :double :initform 0.0d0)
  (gravity gdk-gravity :initform :north-west))

(export (boxed-related-symbols 'gdk-geometry))

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowAttributesType
;;; 
;;; typedef enum
;;; {
;;;   GDK_WA_TITLE     = 1 << 1,
;;;   GDK_WA_X         = 1 << 2,
;;;   GDK_WA_Y         = 1 << 3,
;;;   GDK_WA_CURSOR    = 1 << 4,
;;;   GDK_WA_COLORMAP  = 1 << 5,
;;;   GDK_WA_VISUAL    = 1 << 6,
;;;   GDK_WA_WMCLASS   = 1 << 7,
;;;   GDK_WA_NOREDIR   = 1 << 8,
;;;   GDK_WA_TYPE_HINT = 1 << 9
;;; } GdkWindowAttributesType;
;;; 
;;; Used to indicate which fields in the GdkWindowAttr struct should be honored.
;;; For example, if you filled in the "cursor" and "x" fields of GdkWindowAttr,
;;; pass "GDK_WA_X | GDK_WA_CURSOR" to gdk_window_new(). Fields in GdkWindowAttr
;;; not covered by a bit in this enum are required; for example, the
;;; width/height, wclass, and window_type fields are required, they have no
;;; corresponding flag in GdkWindowAttributesType.
;;; 
;;; GDK_WA_TITLE
;;;     Honor the title field
;;; 
;;; GDK_WA_X
;;;     Honor the X coordinate field
;;; 
;;; GDK_WA_Y
;;;     Honor the Y coordinate field
;;; 
;;; GDK_WA_CURSOR
;;;     Honor the cursor field
;;; 
;;; GDK_WA_COLORMAP
;;;     Honor the colormap field
;;; 
;;; GDK_WA_VISUAL
;;;     Honor the visual field
;;; 
;;; GDK_WA_WMCLASS
;;;     Honor the wmclass_class and wmclass_name fields
;;; 
;;; GDK_WA_NOREDIR
;;;     Honor the override_redirect field
;;; 
;;; GDK_WA_TYPE_HINT
;;;     Honor the type_hint field
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowAttributesType" gdk-window-attributes-type
  (:export t
   :type-initializer "gdk_window_attributes_type_get_type")
  (:title 2)
  (:x 4)
  (:y 8)
  (:cursor 16)
  (:colormap 32)
  (:visual 64)
  (:wmclass 128)
  (:noredir 256)
  (:type-hint 512))

;;; ----------------------------------------------------------------------------
;;; struct GdkWindowAttr
;;; 
;;; struct GdkWindowAttr {
;;;   gchar *title;
;;;   gint event_mask;
;;;   gint x, y;
;;;   gint width;
;;;   gint height;
;;;   GdkWindowClass wclass;
;;;   GdkVisual *visual;
;;;   GdkColormap *colormap;
;;;   GdkWindowType window_type;
;;;   GdkCursor *cursor;
;;;   gchar *wmclass_name;
;;;   gchar *wmclass_class;
;;;   gboolean override_redirect;
;;;   GdkWindowTypeHint type_hint;
;;; };
;;; 
;;; Attributes to use for a newly-created window.
;;; 
;;; gchar *title;
;;;     title of the window (for toplevel windows)
;;; 
;;; gint event_mask;
;;;     event mask (see gdk_window_set_events())
;;; 
;;; gint x;
;;;     X coordinate relative to parent window (see gdk_window_move())
;;; 
;;; gint y;
;;;     Y coordinate relative to parent window (see gdk_window_move())
;;; 
;;; gint width;
;;;     width of window
;;; 
;;; gint height;
;;;     height of window
;;; 
;;; GdkWindowClass wclass;
;;;     GDK_INPUT_OUTPUT (normal window) or GDK_INPUT_ONLY (invisible window
;;;     that receives events)
;;; 
;;; GdkVisual *visual;
;;;     GdkVisual for window
;;; 
;;; GdkColormap *colormap;
;;;     GdkColormap for window
;;; 
;;; GdkWindowType window_type;
;;;     type of window
;;; 
;;; GdkCursor *cursor;
;;;     cursor for the window (see gdk_window_set_cursor())
;;; 
;;; gchar *wmclass_name;
;;;     don't use (see gtk_window_set_wmclass())
;;; 
;;; gchar *wmclass_class;
;;;     don't use (see gtk_window_set_wmclass())
;;; 
;;; gboolean override_redirect;
;;;     TRUE to bypass the window manager
;;; 
;;; GdkWindowTypeHint type_hint;
;;;     a hint of the function of the window
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-window-attr
  ()
  (title (:string :free-from-foreign nil) :initform "")
  (event-mask gdk-event-mask :initform nil)
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0)
  (window-class gdk-window-class :initform :input-output)
  (visual (g-object gdk-visual) :initform nil)
  (colormap (g-object colormap) :initform nil)
  (window-type gdk-window-type :initform :toplevel)
  (cursor (g-object gdk-cursor) :initform nil)
  (wmclass-name (:string :free-from-foreign nil) :initform "")
  (wmclass-class (:string :free-from-foreign nil) :initform "")
  (override-redirect :boolean :initform nil)
  (type-hint gdk-window-type-hint :initform :normal))

(export (boxed-related-symbols 'gdk-window-attr))

;;; ----------------------------------------------------------------------------
;;; gdk_window_new ()
;;; 
;;; GdkWindow * gdk_window_new (GdkWindow *parent,
;;;                             GdkWindowAttr *attributes,
;;;                             gint attributes_mask);
;;; 
;;; Creates a new GdkWindow using the attributes from attributes. See
;;; GdkWindowAttr and GdkWindowAttributesType for more details. Note: to use
;;; this on displays other than the default display, parent must be specified.
;;; 
;;; parent :
;;;     a GdkWindow, or NULL to create the window as a child of the default
;;;     root window for the default display.
;;; 
;;; attributes :
;;;     attributes of the new window
;;; 
;;; attributes-mask :
;;;     mask indicating which fields in attributes are valid
;;; 
;;; Returns :
;;;     the new GdkWindow.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_new" gdk-window-new)
    (g-object gdk-window :already-referenced)
  (parent (g-object gdk-window))
  (attributes (g-boxed-foreign gdk-window-attr))
  (attributes-mask gdk-window-attributes-type))

(export 'gdk-window-new)

;;; ----------------------------------------------------------------------------
;;; gdk_window_destroy ()
;;; 
;;; void gdk_window_destroy (GdkWindow *window);
;;; 
;;; Destroys the window system resources associated with window and decrements
;;; window's reference count. The window system resources for all children of
;;; window are also destroyed, but the children's reference counts are not
;;; decremented.
;;; 
;;; Note that a window will not be destroyed automatically when its reference
;;; count reaches zero. You must call this function yourself before that
;;; happens.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_destroy" gdk-window-destroy) :void
  (window (g-object gdk-window)))

(export 'gdk-window-destroy)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ref
;;; 
;;; #define gdk_window_ref g_object_ref
;;; 
;;; Warning
;;; 
;;; gdk_window_ref is deprecated and should not be used in newly-written code.
;;; 
;;; Deprecated equivalent of g_object_ref()
;;; 
;;; Returns :
;;;     the window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_unref
;;; 
;;; #define gdk_window_unref g_object_unref
;;; 
;;; Warning
;;; 
;;; gdk_window_unref is deprecated and should not be used in newly-written code.
;;; 
;;; Deprecated equivalent of g_object_unref()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_display ()
;;; 
;;; GdkDisplay * gdk_window_get_display (GdkWindow *window);
;;; 
;;; Gets the GdkDisplay associated with a GdkWindow.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the GdkDisplay associated with window
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_display" gdk-window-get-display)
    (g-object gdk-display)
  (window (g-object gdk-window)))

(export 'gdk-window-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_screen ()
;;; 
;;; GdkScreen * gdk_window_get_screen (GdkWindow *window);
;;; 
;;; Gets the GdkScreen associated with a GdkWindow.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the GdkScreen associated with window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_screen" gdk-window-get-screen) (g-object gdk-screen)
  (window (g-object gdk-window)))

(export 'gdk-window-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visual ()
;;; 
;;; GdkVisual * gdk_window_get_visual (GdkWindow *window);
;;; 
;;; Gets the GdkVisual describing the pixel format of window.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     a GdkVisual
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visual" gdk-window-get-visual) (g-object gdk-visual)
  (window (g-object gdk-window)))

(export 'gdk-window-get-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_width ()
;;; 
;;; int gdk_window_get_width (GdkWindow *window);
;;; 
;;; Returns the width of the given window.
;;; 
;;; On the X11 platform the returned size is the size reported in the
;;; most-recently-processed configure event, rather than the current size on
;;; the X server.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     The width of window
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_width" gdk-window-get-width) :int
  (window (g-object gdk-window)))

(export 'gdk-window-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_height ()
;;; 
;;; int gdk_window_get_height (GdkWindow *window);
;;; 
;;; Returns the height of the given window.
;;; 
;;; On the X11 platform the returned size is the size reported in the
;;; most-recently-processed configure event, rather than the current size on
;;; the X server.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     The height of window
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_height" gdk-window-get-height) :int
  (window (g-object gdk-window)))

(export 'gdk-window-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_window_type ()
;;; 
;;; GdkWindowType gdk_window_get_window_type (GdkWindow *window);
;;; 
;;; Gets the type of the window. See GdkWindowType.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     type of window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-window-type))

(defun gdk-window-get-window-type (window)
  (gdk-window-window-type window))

(export 'gdk-window-get-window-type)

;;; ----------------------------------------------------------------------------
;;; gdk_window_at_pointer ()
;;; 
;;; GdkWindow * gdk_window_at_pointer (gint *win_x, gint *win_y);
;;; 
;;; Obtains the window underneath the mouse pointer, returning the location of
;;; that window in win_x, win_y. Returns NULL if the window under the mouse
;;; pointer is not known to GDK (if the window belongs to another application
;;; and a GdkWindow hasn't been created for it with gdk_window_foreign_new())
;;; 
;;; NOTE: For multihead-aware widgets or applications use
;;; gdk_display_get_window_at_pointer() instead.
;;; 
;;; win-x :
;;;     return location for origin of the window under the pointer
;;; 
;;; win-y :
;;;     return location for origin of the window under the pointer
;;; 
;;; Returns :
;;;     window under the mouse pointer
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_at_pointer" %gdk-window-at-pointer) (g-object gdk-window)
  (win-x (:pointer :int))
  (win-y (:pointer :int)))

(defun gdk-window-at-pointer ()
  (with-foreign-objects ((x :int) (y :int))
    (let ((window (%gdk-window-at-pointer x y)))
      (if window
          (values window (mem-ref x :int) (mem-ref y :int))
          (values nil nil nil)))))

(export 'gdk-window-at-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show ()
;;; 
;;; void gdk_window_show (GdkWindow *window);
;;; 
;;; Like gdk_window_show_unraised(), but also raises the window to the top of
;;; the window stack (moves the window to the front of the Z-order).
;;; 
;;; This function maps a window so it's visible onscreen. Its opposite is
;;; gdk_window_hide().
;;; 
;;; When implementing a GtkWidget, you should call this function on the widget's
;;; GdkWindow as part of the "map" method.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show" gdk-window-show) :void
  (window (g-object gdk-window)))

(export 'gdk-window-show)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_unraised ()
;;; 
;;; void gdk_window_show_unraised (GdkWindow *window);
;;; 
;;; Shows a GdkWindow onscreen, but does not modify its stacking order. In
;;; contrast, gdk_window_show() will raise the window to the top of the window
;;; stack.
;;; 
;;; On the X11 platform, in Xlib terms, this function calls XMapWindow() (it
;;; also updates some internal GDK state, which means that you can't really use
;;; XMapWindow() directly on a GDK window).
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_unraised" gdk-window-show-unraised) :void
  (window (g-object gdk-window)))

(export 'gdk-window-show-unraised)

;;; ----------------------------------------------------------------------------
;;; gdk_window_hide ()
;;; 
;;; void gdk_window_hide (GdkWindow *window);
;;; 
;;; For toplevel windows, withdraws them, so they will no longer be known to
;;; the window manager; for all windows, unmaps them, so they won't be
;;; displayed. Normally done automatically as part of gtk_widget_hide().
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_hide" gdk-window-hide) :void
  (window (g-object gdk-window)))

(export 'gdk-window-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_destroyed ()
;;; 
;;; gboolean gdk_window_is_destroyed (GdkWindow *window);
;;; 
;;; Check to see if a window is destroyed.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window is destroyed
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; *** Implemented as the slot accessor for the property is-destroyed ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_visible ()
;;; 
;;; gboolean gdk_window_is_visible (GdkWindow *window);
;;; 
;;; Checks whether the window has been mapped (with gdk_window_show() or
;;; gdk_window_show_unraised()).
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window is mapped
;;; ----------------------------------------------------------------------------

;;; *** Implemented as the slot accessor for the property is-visible ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_viewable ()
;;; 
;;; gboolean gdk_window_is_viewable (GdkWindow *window);
;;; 
;;; Check if the window and all ancestors of the window are mapped. (This is
;;; not necessarily "viewable" in the X sense, since we only check as far as we
;;; have GDK window parents, not to the root window.)
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window is viewable
;;; ----------------------------------------------------------------------------

;;; *** Implemented as the slot accessor for the property is-viewable ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_shaped ()
;;; 
;;; gboolean gdk_window_is_shaped (GdkWindow *window);
;;; 
;;; Determines whether or not the window is shaped.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Returns :
;;;     TRUE if window is shaped
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_shaped" gdk-window-is-shaped) :boolean
  (window (g-object gdk-window)))

(export 'gdk-window-is-shaped)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_input_only ()
;;; 
;;; gboolean gdk_window_is_input_only (GdkWindow *window);
;;; 
;;; Determines whether or not the window is an input only window.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Returns :
;;;     TRUE if window is input only
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_input_only" gdk-window-is-input-only) :boolean
  (window (g-object gdk-window)))

(export 'gdk-window-is-input-only)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_state ()
;;; 
;;; GdkWindowState gdk_window_get_state (GdkWindow *window);
;;; 
;;; Gets the bitwise OR of the currently active window state flags, from the
;;; GdkWindowState enumeration.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     window state bitfield
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-state))

(defun gdk-window-get-state (window)
  (gdk-window-state window))

(export 'gdk-window-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_window_withdraw ()
;;; 
;;; void gdk_window_withdraw (GdkWindow *window);
;;; 
;;; Withdraws a window (unmaps it and asks the window manager to forget about
;;; it). This function is not really useful as gdk_window_hide() automatically
;;; withdraws toplevel windows before hiding them.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_withdraw" gdk-window-withdraw) :void
  (window (g-object gdk-window)))

(export 'gdk-window-withdraw)

;;; ----------------------------------------------------------------------------
;;; gdk_window_iconify ()
;;; 
;;; void gdk_window_iconify (GdkWindow *window);
;;; 
;;; Asks to iconify (minimize) window. The window manager may choose to ignore
;;; the request, but normally will honor it. Using gtk_window_iconify() is
;;; preferred, if you have a GtkWindow widget.
;;; 
;;; This function only makes sense when window is a toplevel window.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_iconify" gdk-window-iconify) :void
  (window (g-object gdk-window)))

(export 'gdk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_deiconify ()
;;; 
;;; void gdk_window_deiconify (GdkWindow *window);
;;; 
;;; Attempt to deiconify (unminimize) window. On X11 the window manager may
;;; choose to ignore the request to deiconify. When using GTK+, use
;;; gtk_window_deiconify() instead of the GdkWindow variant. Or better yet, you
;;; probably want to use gtk_window_present(), which raises the window, focuses
;;; it, unminimizes it, and puts it on the current desktop.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_deiconify" gdk-window-deiconify) :void
  (window (g-object gdk-window)))

(export 'gdk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_stick ()
;;; 
;;; void gdk_window_stick (GdkWindow *window);
;;; 
;;; "Pins" a window such that it's on all workspaces and does not scroll with
;;; viewports, for window managers that have scrollable viewports. (When using
;;; GtkWindow, gtk_window_stick() may be more useful.)
;;; 
;;; On the X11 platform, this function depends on window manager support, so
;;; may have no effect with many window managers. However, GDK will do the best
;;; it can to convince the window manager to stick the window. For window
;;; managers that don't support this operation, there's nothing you can do to
;;; force it to happen.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_stick" gdk-window-stick) :void
  (window (g-object gdk-window)))

(export 'gdk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unstick ()
;;; 
;;; void gdk_window_unstick (GdkWindow *window);
;;; 
;;; Reverse operation for gdk_window_stick(); see gdk_window_stick(), and
;;; gtk_window_unstick().
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unstick" gdk-window-unstick) :void
  (window (g-object gdk-window)))

(export 'gdk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_maximize ()
;;; 
;;; void gdk_window_maximize (GdkWindow *window);
;;; 
;;; Maximizes the window. If the window was already maximized, then this
;;; function does nothing.
;;; 
;;; On X11, asks the window manager to maximize window, if the window manager
;;; supports this operation. Not all window managers support this, and some
;;; deliberately ignore it or don't have a concept of "maximized"; so you can't
;;; rely on the maximization actually happening. But it will happen with most
;;; standard window managers, and GDK makes a best effort to get it to happen.
;;; 
;;; On Windows, reliably maximizes the window.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_maximize" gdk-window-maximize) :void
  (window (g-object gdk-window)))

(export 'gdk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unmaximize ()
;;; 
;;; void gdk_window_unmaximize (GdkWindow *window);
;;; 
;;; Unmaximizes the window. If the window wasn't maximized, then this function
;;; does nothing.
;;; 
;;; On X11, asks the window manager to unmaximize window, if the window manager
;;; supports this operation. Not all window managers support this, and some
;;; deliberately ignore it or don't have a concept of "maximized"; so you can't
;;; rely on the unmaximization actually happening. But it will happen with most
;;; standard window managers, and GDK makes a best effort to get it to happen.
;;; 
;;; On Windows, reliably unmaximizes the window.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unmaximize" gdk-window-unmaximize) :void
  (window (g-object gdk-window)))

(export 'gdk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen ()
;;; 
;;; void gdk_window_fullscreen (GdkWindow *window);
;;; 
;;; Moves the window into fullscreen mode. This means the window covers the
;;; entire screen and is above any panels or task bars.
;;; 
;;; If the window was already fullscreen, then this function does nothing.
;;; 
;;; On X11, asks the window manager to put window in a fullscreen state, if
;;; the window manager supports this operation. Not all window managers support
;;; this, and some deliberately ignore it or don't have a concept of
;;; "fullscreen"; so you can't rely on the fullscreenification actually
;;; happening. But it will happen with most standard window managers, and GDK
;;; makes a best effort to get it to happen.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen" gdk-window-fullscreen) :void
  (window (g-object gdk-window)))

(export 'gdk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unfullscreen ()
;;; 
;;; void gdk_window_unfullscreen (GdkWindow *window);
;;; 
;;; Moves the window out of fullscreen mode. If the window was not fullscreen,
;;; does nothing.
;;; 
;;; On X11, asks the window manager to move window out of the fullscreen state,
;;; if the window manager supports this operation. Not all window managers
;;; support this, and some deliberately ignore it or don't have a concept of
;;; "fullscreen"; so you can't rely on the unfullscreenification actually
;;; happening. But it will happen with most standard window managers, and GDK
;;; makes a best effort to get it to happen.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unfullscreen" gdk-window-unfullscreen) :void
  (window (g-object gdk-window)))

(export 'gdk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_above ()
;;; 
;;; void gdk_window_set_keep_above (GdkWindow *window, gboolean setting);
;;; 
;;; Set if window must be kept above other windows. If the window was already
;;; above, then this function does nothing.
;;; 
;;; On X11, asks the window manager to keep window above, if the window manager
;;; supports this operation. Not all window managers support this, and some
;;; deliberately ignore it or don't have a concept of "keep above"; so you
;;; can't rely on the window being kept above. But it will happen with most
;;; standard window managers, and GDK makes a best effort to get it to happen.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; setting :
;;;     whether to keep window above other windows
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-keep-above))

(defun gdk-window-set-keep-above (window setting)
  (setf (gdk-window-keep-above window) setting))

(export 'gdk-window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_below ()
;;; 
;;; void gdk_window_set_keep_below (GdkWindow *window, gboolean setting);
;;; 
;;; Set if window must be kept below other windows. If the window was already
;;; below, then this function does nothing.
;;; 
;;; On X11, asks the window manager to keep window below, if the window manager
;;; supports this operation. Not all window managers support this, and some
;;; deliberately ignore it or don't have a concept of "keep below"; so you can't
;;; rely on the window being kept below. But it will happen with most standard
;;; window managers, and GDK makes a best effort to get it to happen.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; setting :
;;;     whether to keep window below other windows
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-keep-below))

(defun gdk-window-set-keep-below (window setting)
  (setf (gdk-window-keep-below window) setting))

(export 'gdk-window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opacity ()
;;; 
;;; void gdk_window_set_opacity (GdkWindow *window, gdouble opacity);
;;; 
;;; Request the windowing system to make window partially transparent, with
;;; opacity 0 being fully transparent and 1 fully opaque. (Values of the opacity
;;; parameter are clamped to the [0,1] range.)
;;; 
;;; On X11, this works only on X screens with a compositing manager running.
;;; 
;;; For setting up per-pixel alpha, see gdk_screen_get_rgba_colormap(). For
;;; making non-toplevel windows translucent, see gdk_window_set_composited().
;;; 
;;; window :
;;;     a top-level GdkWindow
;;; 
;;; opacity :
;;;     opacity
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-opacity))

(defun gdk-window-set-opacity (window opacity)
  (setf (gdk-window-opacity window) opacity))

(export 'gdk-window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_composited ()
;;; 
;;; void gdk_window_set_composited (GdkWindow *window, gboolean composited);
;;; 
;;; Sets a GdkWindow as composited, or unsets it. Composited windows do not
;;; automatically have their contents drawn to the screen. Drawing is redirected
;;; to an offscreen buffer and an expose event is emitted on the parent of the
;;; composited window. It is the responsibility of the parent's expose handler
;;; to manually merge the off-screen content onto the screen in whatever way it
;;; sees fit. See Example 7, “Composited windows” for an example.
;;; 
;;; It only makes sense for child windows to be composited; see
;;; gdk_window_set_opacity() if you need translucent toplevel windows.
;;; 
;;; An additional effect of this call is that the area of this window is no
;;; longer clipped from regions marked for invalidation on its parent. Draws
;;; done on the parent window are also no longer clipped by the child.
;;; 
;;; This call is only supported on some systems (currently, only X11 with new
;;; enough Xcomposite and Xdamage extensions). You must call
;;; gdk_display_supports_composite() to check if setting a window as composited
;;; is supported before attempting to do so.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; composited :
;;;     TRUE to set the window as composited
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-composited))

(defun gdk-window-set-composited (window composited)
  (setf (gdk-window-composited window) composited))

(export 'gdk-window-set-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_composited ()
;;; 
;;; gboolean gdk_window_get_composited (GdkWindow *window);
;;; 
;;; Determines whether window is composited.
;;; 
;;; See gdk_window_set_composited().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window is composited
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-composited))

(defun gdk-window-get-composited (window)
  (gdk-window-composited window))

(export 'gdk-window-get-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move ()
;;; 
;;; void gdk_window_move (GdkWindow *window, gint x, gint y);
;;; 
;;; Repositions a window relative to its parent window. For toplevel windows,
;;; window managers may ignore or modify the move; you should probably use
;;; gtk_window_move() on a GtkWindow widget anyway, instead of using GDK
;;; functions. For child windows, the move will reliably succeed.
;;; 
;;; If you're also planning to resize the window, use gdk_window_move_resize()
;;; to both move and resize simultaneously, for a nicer visual effect.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     X coordinate relative to window's parent
;;; 
;;; y :
;;;     Y coordinate relative to window's parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move" gdk-window-move) :void
  (window (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-move)

;;; ----------------------------------------------------------------------------
;;; gdk_window_resize ()
;;; 
;;; void gdk_window_resize (GdkWindow *window, gint width, gint height);
;;; 
;;; Resizes window; for toplevel windows, asks the window manager to resize the
;;; window. The window manager may not allow the resize. When using GTK+, use
;;; gtk_window_resize() instead of this low-level GDK function.
;;; 
;;; Windows may not be resized below 1x1.
;;; 
;;; If you're also planning to move the window, use gdk_window_move_resize() to
;;; both move and resize simultaneously, for a nicer visual effect.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; width :
;;;     new width of the window
;;; 
;;; height :
;;;     new height of the window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_resize" gdk-window-resize) :void
  (window (g-object gdk-window))
  (width :int)
  (height :int))

(export 'gdk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_resize ()
;;; 
;;; void gdk_window_move_resize (GdkWindow *window,
;;;                              gint x,
;;;                              gint y,
;;;                              gint width,
;;;                              gint height);
;;; 
;;; Equivalent to calling gdk_window_move() and gdk_window_resize(), except
;;; that both operations are performed at once, avoiding strange visual effects.
;;; (i.e. the user may be able to see the window first move, then resize, if you
;;; don't use gdk_window_move_resize().)
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     new X position relative to window's parent
;;; 
;;; y :
;;;     new Y position relative to window's parent
;;; 
;;; width :
;;;     new width
;;; 
;;; height :
;;;     new height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_resize" gdk-window-move-resize) :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-window-move-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_scroll ()
;;; 
;;; void gdk_window_scroll (GdkWindow *window, gint dx, gint dy);
;;; 
;;; Scroll the contents of its window, both pixels and children, by the given
;;; amount. Portions of the window that the scroll operation brings in from
;;; offscreen areas are invalidated.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; dx :
;;;     Amount to scroll in the X direction
;;; 
;;; dy :
;;;     Amount to scroll in the Y direction
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_scroll" gdk-window-scroll) :void
  (window (g-object gdk-window))
  (dx :int)
  (dy :int))

(export 'gdk-window-scroll)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_region ()
;;; 
;;; void gdk_window_move_region (GdkWindow *window,
;;;                              const GdkRegion *region,
;;;                              gint dx,
;;;                              gint dy);
;;; 
;;; Move the part of window indicated by region by dy pixels in the Y direction
;;; and dx pixels in the X direction. The portions of region that not covered
;;; by the new position of region are invalidated.
;;; 
;;; Child windows are not moved.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; region :
;;;     The GdkRegion to move
;;; 
;;; dx :
;;;     Amount to move in the X direction
;;; 
;;; dy :
;;;     Amount to move in the Y direction
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_region" gdk-window-move-region) :void
  (window (g-object gdk-window))
  (region cairo-region-t)
  (dx :int)
  (dy :int))

(export 'gdk-window-move-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_flush ()
;;; 
;;; void gdk_window_flush (GdkWindow *window);
;;; 
;;; Flush all outstanding cached operations on a window, leaving the window in
;;; a state which reflects all that has been drawn before.
;;; 
;;; Gdk uses multiple kinds of caching to get better performance and nicer
;;; drawing. For instance, during exposes all paints to a window using double
;;; buffered rendering are keep on a pixmap until the last window has been
;;; exposed. It also delays window moves/scrolls until as long as possible until
;;; next update to avoid tearing when moving windows.
;;; 
;;; Normally this should be completely invisible to applications, as we
;;; automatically flush the windows when required, but this might be needed if
;;; you for instance mix direct native drawing with gdk drawing. For Gtk widgets
;;; that don't use double buffering this will be called automatically before
;;; sending the expose event.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_flush" gdk-window-flush) :void
  (window (g-object gdk-window)))

(export 'gdk-window-flush)
  
;;; ----------------------------------------------------------------------------
;;; gdk_window_has_native ()
;;; 
;;; gboolean gdk_window_has_native (GdkWindow *window);
;;; 
;;; Checks whether the window has a native window or not. Note that you can
;;; use gdk_window_ensure_native() if a native window is needed.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window has a native window, FALSE otherwise
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_has_native" gdk-window-has-native) :boolean
  (window (g-object gdk-window)))

(export 'gdk-window-has-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ensure_native ()
;;; 
;;; gboolean gdk_window_ensure_native (GdkWindow *window);
;;; 
;;; Tries to ensure that there is a window-system native window for this
;;; GdkWindow. This may fail in some situations, returning FALSE.
;;; 
;;; Offscreen window and children of them can never have native windows.
;;; 
;;; Some backends may not support native child windows.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     TRUE if the window has a native window, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_ensure_native" gdk-window-ensure-native) :void
  (window (g-object gdk-window)))
  
(export 'gdk-window-ensure-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_reparent ()
;;; 
;;; void gdk_window_reparent (GdkWindow *window,
;;;                           GdkWindow *new_parent,
;;;                           gint x,
;;;                           gint y);
;;; 
;;; Reparents window into the given new_parent. The window being reparented
;;; will be unmapped as a side effect.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; new_parent :
;;;     new parent to move window into
;;; 
;;; x :
;;;     X location inside the new parent
;;; 
;;; y :
;;;     Y location inside the new parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_reparent" gdk-window-reparent) :void
  (window (g-object gdk-window))
  (new-parent (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-reparent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_clear ()
;;; 
;;; void gdk_window_clear (GdkWindow *window);
;;; 
;;; Clears an entire window to the background color or background pixmap.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_clear_area ()
;;; 
;;; void gdk_window_clear_area (GdkWindow *window,
;;;                             gint x,
;;;                             gint y,
;;;                             gint width,
;;;                             gint height);
;;; 
;;; Clears an area of window to the background color or background pixmap.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     x coordinate of rectangle to clear
;;; 
;;; y :
;;;     y coordinate of rectangle to clear
;;; 
;;; width :
;;;     width of rectangle to clear
;;; 
;;; height :
;;;     height of rectangle to clear
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_clear_area_e ()
;;; 
;;; void gdk_window_clear_area_e (GdkWindow *window,
;;;                               gint x,
;;;                               gint y,
;;;                               gint width,
;;;                               gint height);
;;; 
;;; Like gdk_window_clear_area(), but also generates an expose event for the
;;; cleared area.
;;; 
;;; This function has a stupid name because it dates back to the mists time,
;;; pre-GDK-1.0.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     x coordinate of rectangle to clear
;;; 
;;; y :
;;;     y coordinate of rectangle to clear
;;; 
;;; width :
;;;     width of rectangle to clear
;;; 
;;; height :
;;;     height of rectangle to clear
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_copy_area()
;;; 
;;; #define gdk_window_copy_area (drawable,gc,x,y,source_drawable,
;;;                               source_x,source_y,width,height)
;;; 
;;; Warning
;;; 
;;; gdk_window_copy_area is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Deprecated equivalent to gdk_draw_drawable(), see that function for docs
;;; 
;;; drawable :
;;;     a GdkDrawable
;;; 
;;; gc :
;;;     a GdkGC sharing the drawable's visual and colormap
;;; 
;;; x :
;;;     X position in drawable where the rectangle should be drawn
;;; 
;;; y :
;;;     Y position in drawable where the rectangle should be drawn
;;; 
;;; source_drawable :
;;;     the source GdkDrawable, which may be the same as drawable
;;; 
;;; source_x :
;;;     X position in src of rectangle to draw
;;; 
;;; source_y :
;;;     Y position in src of rectangle to draw
;;; 
;;; width :
;;;     width of rectangle to draw, or -1 for entire src width
;;; 
;;; height :
;;;     height of rectangle to draw, or -1 for entire src height
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_raise ()
;;; 
;;; void gdk_window_raise (GdkWindow *window);
;;; 
;;; Raises window to the top of the Z-order (stacking order), so that other
;;; windows with the same parent window appear below window. This is true
;;; whether or not the windows are visible.
;;; 
;;; If window is a toplevel, the window manager may choose to deny the request
;;; to move the window in the Z-order, gdk_window_raise() only requests the
;;; restack, does not guarantee it.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_raise" gdk-window-raise) :void
  (window (g-object gdk-window)))

(export 'gdk-window-raise)

;;; ----------------------------------------------------------------------------
;;; gdk_window_lower ()
;;; 
;;; void gdk_window_lower (GdkWindow *window);
;;; 
;;; Lowers window to the bottom of the Z-order (stacking order), so that other
;;; windows with the same parent window appear above window. This is true
;;; whether or not the other windows are visible.
;;; 
;;; If window is a toplevel, the window manager may choose to deny the request
;;; to move the window in the Z-order, gdk_window_lower() only requests the
;;; restack, does not guarantee it.
;;; 
;;; Note that gdk_window_show() raises the window again, so don't call this
;;; function before gdk_window_show(). (Try gdk_window_show_unraised().)
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_lower" gdk-window-lower) :void
  (window (g-object gdk-window)))

(export 'gdk-window-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_window_restack ()
;;; 
;;; void gdk_window_restack (GdkWindow *window,
;;;                          GdkWindow *sibling,
;;;                          gboolean above);
;;; 
;;; Changes the position of window in the Z-order (stacking order), so that it
;;; is above sibling (if above is TRUE) or below sibling (if above is FALSE).
;;; 
;;; If sibling is NULL, then this either raises (if above is TRUE) or lowers
;;; the window.
;;; 
;;; If window is a toplevel, the window manager may choose to deny the request
;;; to move the window in the Z-order, gdk_window_restack() only requests the
;;; restack, does not guarantee it.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; sibling :
;;;     a GdkWindow that is a sibling of window, or NULL
;;; 
;;; above :
;;;     a boolean
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_restack" gdk-window-restack) :void
  (window (g-object gdk-window))
  (sibling (g-object gdk-window))
  (above :boolean))

(export 'gdk-window-restack)

;;; ----------------------------------------------------------------------------
;;; gdk_window_focus ()
;;; 
;;; void gdk_window_focus (GdkWindow *window, guint32 timestamp);
;;; 
;;; Sets keyboard focus to window. In most cases, gtk_window_present() should
;;; be used on a GtkWindow, rather than calling this function.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; timestamp :
;;;     timestamp of the event triggering the window focus
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_focus" gdk-window-focus) :void
  (window (g-object gdk-window))
  (timestamp :uint32))

(export 'gdk-window-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_register_dnd ()
;;; 
;;; void gdk_window_register_dnd (GdkWindow *window);
;;; 
;;; Registers a window as a potential drop destination.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_register_dnd" gdk-window-register-dnd) :void
  (window (g-object gdk-window)))

(export 'gdk-window-register-dnd)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag ()
;;; 
;;; void gdk_window_begin_resize_drag (GdkWindow *window,
;;;                                    GdkWindowEdge edge,
;;;                                    gint button,
;;;                                    gint root_x,
;;;                                    gint root_y,
;;;                                    guint32 timestamp);
;;; 
;;; Begins a window resize operation (for a toplevel window). You might use
;;; this function to implement a "window resize grip," for example; in fact
;;; GtkStatusbar uses it. The function works best with window managers that
;;; support the Extended Window Manager Hints, but has a fallback implementation
;;; for other window managers.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; edge :
;;;     the edge or corner from which the drag is started
;;; 
;;; button :
;;;     the button being used to drag
;;; 
;;; root_x :
;;;     root window X coordinate of mouse click that began the drag
;;; 
;;; root_y :
;;;     root window Y coordinate of mouse click that began the drag
;;; 
;;; timestamp :
;;;     timestamp of mouse click that began the drag (use gdk_event_get_time())
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag" gdk-window-begin-resize-drag) :void
  (window (g-object gdk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_move_drag ()
;;; 
;;; void gdk_window_begin_move_drag (GdkWindow *window,
;;;                                  gint button,
;;;                                  gint root_x,
;;;                                  gint root_y,
;;;                                  guint32 timestamp);
;;; 
;;; Begins a window move operation (for a toplevel window). You might use this
;;; function to implement a "window move grip," for example. The function works
;;; best with window managers that support the Extended Window Manager Hints,
;;; but has a fallback implementation for other window managers.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; button :
;;;     the button being used to drag
;;; 
;;; root_x :
;;;     root window X coordinate of mouse click that began the drag
;;; 
;;; root_y :
;;;     root window Y coordinate of mouse click that began the drag
;;; 
;;; timestamp :
;;;     timestamp of mouse click that began the drag
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_move_drag" gdk-window-begin-move-drag) :void
  (window (g-object gdk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_constrain_size ()
;;; 
;;; void gdk_window_constrain_size (GdkGeometry *geometry,
;;;                                 guint flags,
;;;                                 gint width,
;;;                                 gint height,
;;;                                 gint *new_width,
;;;                                 gint *new_height);
;;; 
;;; Constrains a desired width and height according to a set of geometry hints
;;; (such as minimum and maximum size).
;;; 
;;; geometry :
;;;     a GdkGeometry structure
;;; 
;;; flags :
;;;     a mask indicating what portions of geometry are set
;;; 
;;; width :
;;;     desired width of window
;;; 
;;; height :
;;;     desired height of the window
;;; 
;;; new_width :
;;;     location to store resulting width
;;; 
;;; new_height :
;;;     location to store resulting height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_constrain_size" %gdk-window-constrain-size) :void
  (geometry (g-boxed-foreign gdk-geometry))
  (flags gdk-window-hints)
  (width :int)
  (height :int)
  (new-width (:pointer :int))
  (new-height (:pointer :int)))

(defun gdk-window-constrain-size (geometry flags width height)
  (with-foreign-objects ((new-width :int) (new-height :int))
    (%gdk-window-constrain-size geometry
                                flags
                                width
                                height
                                new-width
                                new-height)
    (values (mem-ref new-width :int)
            (mem-ref new-height :int))))

(export 'gdk-window-constrain-size)

;;; ----------------------------------------------------------------------------
;;; gdk_window_beep ()
;;; 
;;; void gdk_window_beep (GdkWindow *window);
;;; 
;;; Emits a short beep associated to window in the appropriate display, if
;;; supported. Otherwise, emits a short beep on the display just as
;;; gdk_display_beep().
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_beep" gdk-window-beep) :void
  (window (g-object gdk-window)))

(export 'gdk-window-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_rect ()
;;; 
;;; void gdk_window_begin_paint_rect (GdkWindow *window,
;;;                                   const GdkRectangle *rectangle);
;;; 
;;; A convenience wrapper around gdk_window_begin_paint_region() which creates
;;; a rectangular region for you. See gdk_window_begin_paint_region() for
;;; details.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; rectangle :
;;;     rectangle you intend to draw to
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_rect" gdk-window-begin-paint-rect) :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-window-begin-paint-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_region ()
;;; 
;;; void gdk_window_begin_paint_region (GdkWindow *window,
;;;                                     const GdkRegion *region);
;;; 
;;; Indicates that you are beginning the process of redrawing region. A backing
;;; store (offscreen buffer) large enough to contain region will be created.
;;; The backing store will be initialized with the background color or
;;; background pixmap for window. Then, all drawing operations performed on
;;; window will be diverted to the backing store. When you call
;;; gdk_window_end_paint(), the backing store will be copied to window, making
;;; it visible onscreen. Only the part of window contained in region will be
;;; modified; that is, drawing operations are clipped to region.
;;; 
;;; The net result of all this is to remove flicker, because the user sees the
;;; finished product appear all at once when you call gdk_window_end_paint().
;;; If you draw to window directly without calling
;;; gdk_window_begin_paint_region(), the user may see flicker as individual
;;; drawing operations are performed in sequence. The clipping and
;;; background-initializing features of gdk_window_begin_paint_region() are
;;; conveniences for the programmer, so you can avoid doing that work yourself.
;;; 
;;; When using GTK+, the widget system automatically places calls to
;;; gdk_window_begin_paint_region() and gdk_window_end_paint() around emissions
;;; of the expose_event signal. That is, if you're writing an expose event
;;; handler, you can assume that the exposed area in GdkEventExpose has already
;;; been cleared to the window background, is already set as the clip region,
;;; and already has a backing store. Therefore in most cases, application code
;;; need not call gdk_window_begin_paint_region(). (You can disable the
;;; automatic calls around expose events on a widget-by-widget basis by calling
;;; gtk_widget_set_double_buffered().)
;;; 
;;; If you call this function multiple times before calling the matching
;;; gdk_window_end_paint(), the backing stores are pushed onto a stack.
;;; gdk_window_end_paint() copies the topmost backing store onscreen, subtracts
;;; the topmost region from all other regions in the stack, and pops the stack.
;;; All drawing operations affect only the topmost backing store in the stack.
;;; One matching call to gdk_window_end_paint() is required for each call to
;;; gdk_window_begin_paint_region().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; region :
;;;     region you intend to draw to
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_region" gdk-window-begin-paint-region) :void
  (window (g-object gdk-window))
  (region cairo-region-t))

(export 'gdk-window-begin-paint-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_paint ()
;;; 
;;; void gdk_window_end_paint (GdkWindow *window);
;;; 
;;; Indicates that the backing store created by the most recent call to
;;; gdk_window_begin_paint_region() should be copied onscreen and deleted,
;;; leaving the next-most-recent backing store or no backing store at all as
;;; the active paint region. See gdk_window_begin_paint_region() for full
;;; details. It is an error to call this function without a matching
;;; gdk_window_begin_paint_region() first.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_paint" gkd-window-end-paint) :void
  (window (g-object gdk-window)))

(export 'gdk-window-end-paint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_rect ()
;;; 
;;; void gdk_window_invalidate_rect (GdkWindow *window,
;;;                                  const GdkRectangle *rect,
;;;                                  gboolean invalidate_children);
;;; 
;;; A convenience wrapper around gdk_window_invalidate_region() which
;;; invalidates a rectangular region. See gdk_window_invalidate_region()
;;; for details.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; rect :
;;;     rectangle to invalidate or NULL to invalidate the whole window.
;;; 
;;; invalidate_children :
;;;     whether to also invalidate child windows
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_rect" gdk-window-invalidate-rect) :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_region ()
;;; 
;;; void gdk_window_invalidate_region (GdkWindow *window,
;;;                                    gboolean invalidate_children);
;;;                                    const GdkRegion *region,
;;; 
;;; Adds region to the update area for window. The update area is the region
;;; that needs to be redrawn, or "dirty region." The call
;;; gdk_window_process_updates() sends one or more expose events to the window,
;;; which together cover the entire update area. An application would normally
;;; redraw the contents of window in response to those expose events.
;;; 
;;; GDK will call gdk_window_process_all_updates() on your behalf whenever your
;;; program returns to the main loop and becomes idle, so normally there's no
;;; need to do that manually, you just need to invalidate regions that you know
;;; should be redrawn.
;;; 
;;; The invalidate_children parameter controls whether the region of each child
;;; window that intersects region will also be invalidated. If FALSE, then the
;;; update area for child windows will remain unaffected. See
;;; gdk_window_invalidate_maybe_recurse if you need fine grained control over
;;; which children are invalidated.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; invalidate_children :
;;;     TRUE to also invalidate child windows
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_region" gdk-window-invalidate-region) :void
  (window (g-object gdk-window))
  (region cairo-region-t)
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_maybe_recurse ()
;;; 
;;; void gdk_window_invalidate_maybe_recurse (GdkWindow *window,
;;;                                           const GdkRegion *region,
;;;                                           gboolean (*child_func)
;;;                                                    (GdkWindow *, gpointer),
;;;                                           gpointer user_data);
;;; 
;;; Adds region to the update area for window. The update area is the region
;;; that needs to be redrawn, or "dirty region." The call
;;; gdk_window_process_updates() sends one or more expose events to the window,
;;; which together cover the entire update area. An application would normally
;;; redraw the contents of window in response to those expose events.
;;; 
;;; GDK will call gdk_window_process_all_updates() on your behalf whenever your
;;; program returns to the main loop and becomes idle, so normally there's no
;;; need to do that manually, you just need to invalidate regions that you know
;;; should be redrawn.
;;; 
;;; The child_func parameter controls whether the region of each child window
;;; that intersects region will also be invalidated. Only children for which
;;; child_func returns TRUE will have the area invalidated.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; region :
;;;     a GdkRegion
;;; 
;;; child_func :
;;;     function to use to decide if to recurse to a child, NULL means never
;;;     recurse.
;;; 
;;; user_data :
;;;     data passed to child_func
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_maybe_recurse"
          %gdk-window-invalidate-maybe-recurse) :void
  (window (g-object gdk-window))
  (region cairo-region-t)
  (child-func :pointer)
  (user-data :pointer))

(defcallback gdk-window-invalidate-maybe-recurse-cb :boolean
  ((window (g-object gdk-window))
   (user-data :pointer))
  (let ((fn (stable-pointer-value user-data)))
    (funcall fn window)))

(defun gdk-window-invalidate-maybe-recurse (window region child-func)
  (with-stable-pointer (ptr child-func)
    (%gdk-window-invalidate-maybe-recurse
                               window
                               region
                               (callback gdk-window-invalidate-maybe-recurse-cb)
                               ptr)))

(export 'gdk-window-invalidate-maybe-recurse)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_update_area ()
;;; 
;;; GdkRegion * gdk_window_get_update_area (GdkWindow *window);
;;; 
;;; Transfers ownership of the update area from window to the caller of the
;;; function. That is, after calling this function, window will no longer have
;;; an invalid/dirty region; the update area is removed from window and handed
;;; to you. If a window has no update area, gdk_window_get_update_area()
;;; returns NULL. You are responsible for calling gdk_region_destroy() on the
;;; returned region if it's non-NULL.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the update area for window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_update_area" gdk-window-get-update-area)
    cairo-region-t
  (window (g-object gdk-window)))

(export 'gdk-window-get-update-area)

;;; ----------------------------------------------------------------------------
;;; gdk_window_freeze_updates ()
;;; 
;;; void gdk_window_freeze_updates (GdkWindow *window);
;;; 
;;; Temporarily freezes a window such that it won't receive expose events. The
;;; window will begin receiving expose events again when
;;; gdk_window_thaw_updates() is called. If gdk_window_freeze_updates() has
;;; been called more than once, gdk_window_thaw_updates() must be called an
;;; equal number of times to begin processing exposes.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_freeze_updates" gdk-window-freeze-updates) :void
  (window (g-object gdk-window)))

(export 'gdk-window-freeze-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_thaw_updates ()
;;; 
;;; void gdk_window_thaw_updates (GdkWindow *window);
;;; 
;;; Thaws a window frozen with gdk_window_freeze_updates().
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_thaw_updates" gdk-window-thaw-updates) :void
  (window (g-object gdk-window)))

(export 'gdk-window-thaw-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_all_updates ()
;;; 
;;; void gdk_window_process_all_updates (void);
;;; 
;;; Calls gdk_window_process_updates() for all windows (see GdkWindow) in the
;;; application.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_all_updates" gdk-window-process-all-updates)
    :void)

(export 'gdk-window-process-all-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_updates ()
;;; 
;;; void gdk_window_process_updates (GdkWindow *window,
;;;                                  gboolean update_children);
;;; 
;;; Sends one or more expose events to window. The areas in each expose event
;;; will cover the entire update area for the window (see
;;; gdk_window_invalidate_region() for details). Normally GDK calls
;;; gdk_window_process_all_updates() on your behalf, so there's no need to call
;;; this function unless you want to force expose events to be delivered
;;; immediately and synchronously (vs. the usual case, where GDK delivers them
;;; in an idle handler). Occasionally this is useful to produce nicer scrolling
;;; behavior, for example.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; update_children :
;;;     whether to also process updates for child windows
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_updates" gdk-window-process-updates) :void
  (window (g-object gdk-window))
  (update-children :boolean))

(export 'gdk-window-process-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_debug_updates ()
;;; 
;;; void gdk_window_set_debug_updates (gboolean setting);
;;; 
;;; With update debugging enabled, calls to gdk_window_invalidate_region()
;;; clear the invalidated region of the screen to a noticeable color, and GDK
;;; pauses for a short time before sending exposes to windows during
;;; gdk_window_process_updates(). The net effect is that you can see the
;;; invalid region for each window and watch redraws as they occur. This allows
;;; you to diagnose inefficiencies in your application.
;;; 
;;; In essence, because the GDK rendering model prevents all flicker, if you
;;; are redrawing the same region 400 times you may never notice, aside from
;;; noticing a speed problem. Enabling update debugging causes GTK to flicker
;;; slowly and noticeably, so you can see exactly what's being redrawn when, in
;;; what order.
;;; 
;;; The --gtk-debug=updates command line option passed to GTK+ programs enables
;;; this debug option at application startup time. That's usually more useful
;;; than calling gdk_window_set_debug_updates() yourself, though you might want
;;; to use this function to enable updates sometime after application startup
;;; time.
;;; 
;;; setting :
;;;     TRUE to turn on update debugging
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_debug_updates" gdk-window-set-debug-updates) :void
  (setting :boolean))

(export 'gdk-window-set-debug-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_internal_paint_info ()
;;; 
;;; void gdk_window_get_internal_paint_info (GdkWindow *window,
;;;                                          GdkDrawable **real_drawable,
;;;                                          gint *x_offset,
;;;                                          gint *y_offset);
;;; 
;;; If you bypass the GDK layer and use windowing system primitives to draw
;;; directly onto a GdkWindow, then you need to deal with two details: there
;;; may be an offset between GDK coordinates and windowing system coordinates,
;;; and GDK may have redirected drawing to a offscreen pixmap as the result of
;;; a gdk_window_begin_paint_region() calls. This function allows retrieving
;;; the information you need to compensate for these effects.
;;; 
;;; This function exposes details of the GDK implementation, and is thus likely
;;; to change in future releases of GDK.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; real_drawable :
;;;     location to store the drawable to which drawing should be done
;;; 
;;; x_offset :
;;;     location to store the X offset between coordinates in window, and the
;;;     underlying window system primitive coordinates for *real_drawable
;;; 
;;; y_offset :
;;;     location to store the Y offset between coordinates in window, and the
;;;     underlying window system primitive coordinates for *real_drawable
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_enable_synchronized_configure ()
;;; 
;;; void gdk_window_enable_synchronized_configure (GdkWindow *window);
;;; 
;;; Indicates that the application will cooperate with the window system in
;;; synchronizing the window repaint with the window manager during resizing
;;; operations. After an application calls this function, it must call
;;; gdk_window_configure_finished() every time it has finished all processing
;;; associated with a set of Configure events. Toplevel GTK+ windows
;;; automatically use this protocol.
;;; 
;;; On X, calling this function makes window participate in the
;;; _NET_WM_SYNC_REQUEST window manager protocol.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_enable_synchronized_configure"
           gdk-window-enable-synchronized-configure) :void
  (window (g-object gdk-window)))

(export 'gdk-window-enable-synchronized-configure)

;;; ----------------------------------------------------------------------------
;;; gdk_window_configure_finished ()
;;; 
;;; void gdk_window_configure_finished (GdkWindow *window);
;;; 
;;; Signal to the window system that the application has finished handling
;;; Configure events it has received. Window Managers can use this to better
;;; synchronize the frame repaint with the application. GTK+ applications will
;;; automatically call this function when appropriate.
;;; 
;;; This function can only be called if
;;; gdk_window_enable_synchronized_configure() was called previously.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_configure_finished" gdk-window-configure-finished) :void
  (window (g-object gdk-window)))

(export 'gdk-window-configure-finished)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_user_data ()
;;; 
;;; void gdk_window_set_user_data (GdkWindow *window, gpointer user_data);
;;; 
;;; For most purposes this function is deprecated in favor of
;;; g_object_set_data(). However, for historical reasons GTK+ stores the
;;; GtkWidget that owns a GdkWindow as user data on the GdkWindow. So, custom
;;; widget implementations should use this function for that. If GTK+ receives
;;; an event for a GdkWindow, and the user data for the window is non-NULL,
;;; GTK+ will assume the user data is a GtkWidget, and forward the event to
;;; that widget.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; user_data :
;;;     user data
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-user-data))

(defun gdk-window-set-user-data (window user-data)
  (setf (gdk-window-user-data window) user-data))

(export 'gdk-window-set-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_override_redirect ()
;;; 
;;; void gdk_window_set_override_redirect (GdkWindow *window,
;;;                                        gboolean override_redirect);
;;; 
;;; An override redirect window is not under the control of the window manager.
;;; This means it won't have a titlebar, won't be minimizable, etc. - it will
;;; be entirely under the control of the application. The window manager can't
;;; see the override redirect window at all.
;;; 
;;; Override redirect should only be used for short-lived temporary windows,
;;; such as popup menus. GtkMenu uses an override redirect window in its
;;; implementation, for example.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; override_redirect :
;;;     TRUE if window should be override redirect
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-override-redirect))

(defun gdk-window-set-override-redirect (window override-redirect)
  (setf (gdk-window-override-redirect window) override-redirect))

(export 'gdk-window-set-override-redirect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_accept_focus ()
;;; 
;;; void gdk_window_set_accept_focus (GdkWindow *window, gboolean accept_focus)
;;; 
;;; Setting accept_focus to FALSE hints the desktop environment that the window
;;; doesn't want to receive input focus.
;;; 
;;; On X, it is the responsibility of the window manager to interpret this hint.
;;; ICCCM-compliant window manager usually respect it.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; accept_focus :
;;;     TRUE if the window should receive input focus
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-accept-focus))

(defun gdk-window-set-accecpt-focus (window accept-focus)
  (setf (gdk-window-accept-focus window) accept-focus))

(export 'gdk-window-set-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_accept_focus ()
;;; 
;;; gboolean gdk_window_get_accept_focus (GdkWindow *window);
;;; 
;;; Determines whether or not the desktop environment should be hinted that the
;;; window does not want to receive input focus.
;;; 
;;; window :
;;;     a toplevel GdkWindow.
;;; 
;;; Returns :
;;;     whether or not the window should receive input focus.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-accept-focus))

(defun gdk-window-get-accept-focus (window)
  (gdk-window-accept-focus window))

(export 'gdk-window-get-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_focus_on_map ()
;;; 
;;; void gdk_window_set_focus_on_map (GdkWindow *window, gboolean focus_on_map)
;;; 
;;; Setting focus_on_map to FALSE hints the desktop environment that the window
;;; doesn't want to receive input focus when it is mapped. focus_on_map should
;;; be turned off for windows that aren't triggered interactively (such as
;;; popups from network activity).
;;; 
;;; On X, it is the responsibility of the window manager to interpret this hint.
;;; Window managers following the freedesktop.org window manager extension
;;; specification should respect it.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; focus_on_map :
;;;     TRUE if the window should receive input focus when mapped
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-focus-on-map))

(defun gdk-window-set-focus-on-map (window focus-on-map)
  (setf (gdk-window-focus-on-map window) focus-on-map))

(export 'gdk-window-set-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_focus_on_map ()
;;; 
;;; gboolean gdk_window_get_focus_on_map (GdkWindow *window);
;;; 
;;; Determines whether or not the desktop environment should be hinted that the
;;; window does not want to receive input focus when it is mapped.
;;; 
;;; window :
;;;     a toplevel GdkWindow.
;;; 
;;; Returns :
;;;     whether or not the window wants to receive input focus when it is
;;;     mapped.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-focus-on-map))

(defun gdk-window-get-focus-on-map (window)
  (gdk-window-focus-on-map window))

(export 'gdk-window-get-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; gdk_window_add_filter ()
;;; 
;;; void gdk_window_add_filter (GdkWindow *window,
;;;                             GdkFilterFunc function,
;;;                             gpointer data);
;;; 
;;; Adds an event filter to window, allowing you to intercept events before
;;; they reach GDK. This is a low-level operation and makes it easy to break
;;; GDK and/or GTK+, so you have to know what you're doing. Pass NULL for
;;; window to get all events for all windows, instead of events for a specific
;;; window.
;;; 
;;; See gdk_display_add_client_message_filter() if you are interested in
;;; X ClientMessage events.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; function :
;;;     filter callback
;;; 
;;; data :
;;;     data to pass to filter callback
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_remove_filter ()
;;; 
;;; void gdk_window_remove_filter (GdkWindow *window,
;;;                                GdkFilterFunc function,
;;;                                gpointer data);
;;; 
;;; Remove a filter previously added with gdk_window_add_filter().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; function :
;;;     previously-added filter function
;;; 
;;; data :
;;;     user data for previously-added filter function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkFilterFunc ()
;;; 
;;; GdkFilterReturn (*GdkFilterFunc) (GdkXEvent *xevent,
;;;                                   GdkEvent *event,
;;;                                   gpointer data);
;;; 
;;; Specifies the type of function used to filter native events before they are
;;; converted to GDK events.
;;; 
;;; When a filter is called, event is unpopulated, except for event->window.
;;; The filter may translate the native event to a GDK event and store the
;;; result in event, or handle it without translation. If the filter translates
;;; the event and processing should continue, it should return
;;; GDK_FILTER_TRANSLATE.
;;; 
;;; xevent :
;;;     the native event to filter.
;;; 
;;; event :
;;;     the GDK event to which the X event will be translated.
;;; 
;;; data :
;;;     user data set when the filter was installed.
;;; 
;;; Returns :
;;;     a GdkFilterReturn value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkFilterReturn
;;; 
;;; typedef enum {
;;;   GDK_FILTER_CONTINUE,   /* Event not handled, continue processesing */
;;;   GDK_FILTER_TRANSLATE,  /* Native event translated into a GDK event and
;;;                             stored in the "event" structure that was
;;;                             passed in */
;;;   GDK_FILTER_REMOVE      /* Terminate processing, removing event */
;;; } GdkFilterReturn;
;;; 
;;; Specifies the result of applying a GdkFilterFunc to a native event.
;;; 
;;; GDK_FILTER_CONTINUE
;;;     event not handled, continue processing.
;;; 
;;; GDK_FILTER_TRANSLATE
;;;     native event translated into a GDK event and stored in the event
;;;     structure that was passed in.
;;; 
;;; GDK_FILTER_REMOVE
;;;     event handled, terminate processing.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFilterReturn" gdk-filter-return
  (:export t
   :type-initializer "gdk_filter_return_get_type")
  (:continue 0)
  (:translate 1)
  (:remove 2))

;;; ----------------------------------------------------------------------------
;;; GdkXEvent
;;; 
;;; typedef void GdkXEvent;  /* Can be cast to window system specific */
;;; 
;;; Used to represent native events (XEvents for the X11 backend,
;;; MSGs for Win32).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_shape_combine_mask ()
;;; 
;;; void gdk_window_shape_combine_mask (GdkWindow *window,
;;;                                     GdkBitmap *mask,
;;;                                     gint x,
;;;                                     gint y);
;;; 
;;; Applies a shape mask to window. Pixels in window corresponding to set bits
;;; in the mask will be visible; pixels in window corresponding to unset bits
;;; in the mask will be transparent. This gives a non-rectangular window.
;;; 
;;; If mask is NULL, the shape mask will be unset, and the x/y parameters are
;;; not used.
;;; 
;;; On the X11 platform, this uses an X server extension which is widely
;;; available on most common platforms, but not available on very old X servers,
;;; and occasionally the implementation will be buggy. On servers without the
;;; shape extension, this function will do nothing.
;;; 
;;; This function works on both toplevel and child windows.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; mask :
;;;     shape mask
;;; 
;;; x :
;;;     X position of shape mask with respect to window
;;; 
;;; y :
;;;     Y position of shape mask with respect to window
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_shape_combine_region ()
;;; 
;;; void gdk_window_shape_combine_region (GdkWindow *window,
;;;                                       const GdkRegion *shape_region,
;;;                                       gint offset_x,
;;;                                       gint offset_y);
;;; 
;;; Makes pixels in window outside shape_region be transparent, so that the
;;; window may be nonrectangular. See also gdk_window_shape_combine_mask() to
;;; use a bitmap as the mask.
;;; 
;;; If shape_region is NULL, the shape will be unset, so the whole window will
;;; be opaque again. offset_x and offset_y are ignored if shape_region is NULL.
;;; 
;;; On the X11 platform, this uses an X server extension which is widely
;;; available on most common platforms, but not available on very old X servers,
;;; and occasionally the implementation will be buggy. On servers without the
;;; shape extension, this function will do nothing.
;;; 
;;; This function works on both toplevel and child windows.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; shape_region :
;;;     region of window to be non-transparent
;;; 
;;; offset_x :
;;;     X position of shape_region in window coordinates
;;; 
;;; offset_y :
;;;     Y position of shape_region in window coordinates
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_shape_combine_region" gdk-window-shape-combine-region)
    :void
  (window (g-object gdk-window))
  (region cairo-region-t)
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_shapes ()
;;; 
;;; void gdk_window_set_child_shapes (GdkWindow *window);
;;; 
;;; Sets the shape mask of window to the union of shape masks for all children
;;; of window, ignoring the shape mask of window itself. Contrast with
;;; gdk_window_merge_child_shapes() which includes the shape mask of window in
;;; the masks to be merged.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_shapes" gdk-window-set-child-shapes) :void
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_shapes ()
;;; 
;;; void gdk_window_merge_child_shapes (GdkWindow *window);
;;; 
;;; Merges the shape masks for any child windows into the shape mask for window.
;;; i.e. the union of all masks for window and its children will become the new
;;; mask for window. See gdk_window_shape_combine_mask().
;;; 
;;; This function is distinct from gdk_window_set_child_shapes() because it
;;; includes window's shape mask in the set of shapes to be merged.
;;; 
;;; window :
;;;     a GdkWindow
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_shapes" gdk-window-merge-child-shapes) :void
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_input_shape_combine_mask ()
;;; 
;;; void gdk_window_input_shape_combine_mask (GdkWindow *window,
;;;                                           GdkBitmap *mask,
;;;                                           gint x,
;;;                                           gint y);
;;; 
;;; Like gdk_window_shape_combine_mask(), but the shape applies only to event
;;; handling. Mouse events which happen while the pointer position corresponds
;;; to an unset bit in the mask will be passed on the window below window.
;;; 
;;; An input shape is typically used with RGBA windows. The alpha channel of
;;; the window defines which pixels are invisible and allows for nicely
;;; antialiased borders, and the input shape controls where the window is
;;; "clickable".
;;; 
;;; On the X11 platform, this requires version 1.1 of the shape extension.
;;; 
;;; On the Win32 platform, this functionality is not present and the function
;;; does nothing.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; mask :
;;;     shape mask, or NULL
;;; 
;;; x :
;;;     X position of shape mask with respect to window
;;; 
;;; y :
;;;     Y position of shape mask with respect to window
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_input_shape_combine_region ()
;;; 
;;; void gdk_window_input_shape_combine_region (GdkWindow *window,
;;;                                             const GdkRegion *shape_region,
;;;                                             gint offset_x,
;;;                                             gint offset_y);
;;; 
;;; Like gdk_window_shape_combine_region(), but the shape applies only to event
;;; handling. Mouse events which happen while the pointer position corresponds
;;; to an unset bit in the mask will be passed on the window below window.
;;; 
;;; An input shape is typically used with RGBA windows. The alpha channel of
;;; the window defines which pixels are invisible and allows for nicely
;;; antialiased borders, and the input shape controls where the window is
;;; "clickable".
;;; 
;;; On the X11 platform, this requires version 1.1 of the shape extension.
;;; 
;;; On the Win32 platform, this functionality is not present and the function
;;; does nothing.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; shape_region :
;;;     region of window to be non-transparent
;;; 
;;; offset_x :
;;;     X position of shape_region in window coordinates
;;; 
;;; offset_y :
;;;     Y position of shape_region in window coordinates
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_input_shape_combine_region"
           gdk-window-input-shape-combine-region) :void
  (window (g-object gdk-window))
  (shape-region cairo-region-t)
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-input-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_input_shapes ()
;;; 
;;; void gdk_window_set_child_input_shapes (GdkWindow *window);
;;; 
;;; Sets the input shape mask of window to the union of input shape masks for
;;; all children of window, ignoring the input shape mask of window itself.
;;; Contrast with gdk_window_merge_child_input_shapes() which includes the
;;; input shape mask of window in the masks to be merged.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_input_shapes" gdk-window-set-child-input-shapes)
    :void
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_input_shapes ()
;;; 
;;; void gdk_window_merge_child_input_shapes (GdkWindow *window);
;;; 
;;; Merges the input shape masks for any child windows into the input shape
;;; mask for window. i.e. the union of all input masks for window and its
;;; children will become the new input mask for window.
;;; See gdk_window_input_shape_combine_mask().
;;; 
;;; This function is distinct from gdk_window_set_child_input_shapes() because
;;; it includes window's input shape mask in the set of shapes to be merged.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_input_shapes"
           gdk-window-merge-child-input-shapes) :void
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_static_gravities ()
;;; 
;;; gboolean gdk_window_set_static_gravities (GdkWindow *window,
;;;                                           gboolean use_static);
;;; 
;;; Set the bit gravity of the given window to static, and flag it so all
;;; children get static subwindow gravity. This is used if you are implementing
;;; scary features that involve deep knowledge of the windowing system. Don't
;;; worry about it unless you have to.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; use_static :
;;;     TRUE to turn on static gravity
;;; 
;;; Returns :
;;;     TRUE if the server supports static gravity
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_static_gravities" gdk-window-set-static-gravities)
    :boolean
  (window (g-object gdk-window))
  (use-static :boolean))

(export 'gdk-window-set-static-gravities)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_hints ()
;;; 
;;; void gdk_window_set_hints (GdkWindow *window,
;;;                            gint x,
;;;                            gint y,
;;;                            gint min_width,
;;;                            gint min_height,
;;;                            gint max_width,
;;;                            gint max_height,
;;;                            gint flags);
;;; 
;;; Warning
;;; 
;;; gdk_window_set_hints is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; This function is broken and useless and you should ignore it. If using GTK+,
;;; use functions such as gtk_window_resize(), gtk_window_set_size_request(),
;;; gtk_window_move(), gtk_window_parse_geometry(), and
;;; gtk_window_set_geometry_hints(), depending on what you're trying to do.
;;; 
;;; If using GDK directly, use gdk_window_set_geometry_hints().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     ignored field, does not matter
;;; 
;;; y :
;;;     ignored field, does not matter
;;; 
;;; min_width :
;;;     minimum width hint
;;; 
;;; min_height :
;;;     minimum height hint
;;; 
;;; max_width :
;;;     max width hint
;;; 
;;; max_height :
;;;     max height hint
;;; 
;;; flags :
;;;     logical OR of GDK_HINT_POS, GDK_HINT_MIN_SIZE, and/or GDK_HINT_MAX_SIZE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_title ()
;;; 
;;; void gdk_window_set_title (GdkWindow *window, const gchar *title);
;;; 
;;; Sets the title of a toplevel window, to be displayed in the titlebar. If
;;; you haven't explicitly set the icon name for the window (using
;;; gdk_window_set_icon_name()), the icon name will be set to title as well.
;;; title must be in UTF-8 encoding (as with all user-readable strings in
;;; GDK/GTK+). title may not be NULL.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; title :
;;;     title of window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-title))

(defun gdk-window-set-title (window title)
  (setf (gdk-window-title window) title))

(export 'gdk-window-set-title)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background ()
;;; 
;;; void gdk_window_set_background (GdkWindow *window, const GdkColor *color);
;;; 
;;; Sets the background color of window. (However, when using GTK+, set the
;;; background of a widget with gtk_widget_modify_bg() - if you're an
;;; application - or gtk_style_set_background() - if you're implementing a
;;; custom widget.)
;;; 
;;; The color must be allocated; gdk_rgb_find_color() is the best way to
;;; allocate a color.
;;; 
;;; See also gdk_window_set_back_pixmap().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; color :
;;;     an allocated GdkColor
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-background))

(defun gdk-window-set-background (window color)
  (setf (gdk-window-background window) color))

(export 'gdk-window-set-background)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_back_pixmap ()
;;; 
;;; void gdk_window_set_back_pixmap (GdkWindow *window,
;;;                                  GdkPixmap *pixmap,
;;;                                  gboolean parent_relative);
;;; 
;;; Sets the background pixmap of window. May also be used to set a background
;;; of "None" on window, by setting a background pixmap of NULL.
;;; 
;;; A background pixmap will be tiled, positioning the first tile at the origin
;;; of window, or if parent_relative is TRUE, the tiling will be done based on
;;; the origin of the parent window (useful to align tiles in a parent with
;;; tiles in a child).
;;; 
;;; A background pixmap of NULL means that the window will have no background.
;;; A window with no background will never have its background filled by the
;;; windowing system, instead the window will contain whatever pixels were
;;; already in the corresponding area of the display.
;;; 
;;; The windowing system will normally fill a window with its background when
;;; the window is obscured then exposed, and when you call gdk_window_clear().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; pixmap :
;;;     a GdkPixmap, or NULL
;;; 
;;; parent_relative :
;;;     whether the tiling origin is at the origin of window's parent
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_background_pattern ()
;;; 
;;; cairo_pattern_t * gdk_window_get_background_pattern (GdkWindow *window);
;;; 
;;; Gets the pattern used to clear the background on window. If window does not
;;; have its own background and reuses the parent's, NULL is returned and you'll
;;; have to query it yourself.
;;; 
;;; window :
;;;     a window
;;; 
;;; Returns :
;;;     The pattern to use for the background or NULL to use the parent's
;;;     background.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PARENT_RELATIVE
;;; 
;;; #define GDK_PARENT_RELATIVE  1L
;;; 
;;; A special value for GdkPixmap* variables, indicating that the background
;;; pixmap for a window should be inherited from the parent window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_cursor ()
;;; 
;;; void gdk_window_set_cursor (GdkWindow *window, GdkCursor *cursor);
;;; 
;;; Sets the mouse pointer for a GdkWindow. Use gdk_cursor_new_for_display() or
;;; gdk_cursor_new_from_pixmap() to create the cursor. To make the cursor
;;; invisible, use GDK_BLANK_CURSOR. Passing NULL for the cursor argument to
;;; gdk_window_set_cursor() means that window will use the cursor of its parent
;;; window. Most windows should use this default.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; cursor :
;;;     a cursor
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-cursor))

(defun gdk-window-set-cursor (window cursor)
  (setf (gdk-window-cursor window) cursor))

(export 'gdk-window-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_cursor ()
;;; 
;;; GdkCursor * gdk_window_get_cursor (GdkWindow *window);
;;; 
;;; Retrieves a GdkCursor pointer for the cursor currently set on the specified
;;; GdkWindow, or NULL. If the return value is NULL then there is no custom
;;; cursor set on the specified window, and it is using the cursor for its
;;; parent window.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     a GdkCursor, or NULL. The returned object is owned by the GdkWindow and
;;;     should not be unreferenced directly. Use gdk_window_set_cursor() to
;;;     unset the cursor of the window
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-cursor))

(defun gdk-window-get-cursor (window)
  (gdk-window-cursor window))

(export 'gdk-window-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_colormap
;;; 
;;; #define gdk_window_set_colormap gdk_drawable_set_colormap
;;; 
;;; Warning
;;; 
;;; gdk_window_set_colormap is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Deprecated equivalent to gdk_drawable_set_colormap()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_user_data ()
;;; 
;;; void gdk_window_get_user_data (GdkWindow *window, gpointer *data);
;;; 
;;; Retrieves the user data for window, which is normally the widget that
;;; window belongs to. See gdk_window_set_user_data().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; data :
;;;     return location for user data
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-user-data))

(defun gdk-window-get-user-data (window)
  (gdk-window-user-data window))

(export 'gdk-window-get-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_geometry ()
;;; 
;;; void gdk_window_get_geometry (GdkWindow *window,
;;;                               gint *x,
;;;                               gint *y,
;;;                               gint *width,
;;;                               gint *height,
;;;                               gint *depth);
;;; 
;;; Any of the return location arguments to this function may be NULL, if you
;;; aren't interested in getting the value of that field.
;;; 
;;; The X and Y coordinates returned are relative to the parent window of
;;; window, which for toplevels usually means relative to the window decorations
;;; (titlebar, etc.) rather than relative to the root window (screen-size
;;; background window).
;;; 
;;; On the X11 platform, the geometry is obtained from the X server, so reflects
;;; the latest position of window; this may be out-of-sync with the position of
;;; window delivered in the most-recently-processed GdkEventConfigure.
;;; gdk_window_get_position() in contrast gets the position from the most recent
;;; configure event.
;;; 
;;; Note
;;;
;;; If window is not a toplevel, it is much better to call
;;; gdk_window_get_position() and gdk_drawable_get_size() instead, because it
;;; avoids the roundtrip to the X server and because gdk_drawable_get_size()
;;; supports the full 32-bit coordinate space, whereas gdk_window_get_geometry()
;;; is restricted to the 16-bit coordinates of X11.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     return location for X coordinate of window (relative to its parent)
;;; 
;;; y :
;;;     return location for Y coordinate of window (relative to its parent)
;;; 
;;; width :
;;;     return location for width of window
;;; 
;;; height :
;;;     return location for height of window
;;; 
;;; depth :
;;;     return location for bit depth of window
;;; ----------------------------------------------------------------------------

(defcfun gdk_window_get_geometry :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int))
  (depth (:pointer :int)))

(defun gdk-window-get-geometry (window)
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int) (depth :int))
    (gdk_window_get_geometry window x y width height depth)
    (values (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref width :int)
            (mem-ref height :int)
            (mem-ref depth :int))))

(export 'gdk-window-get-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_geometry_hints ()
;;; 
;;; void gdk_window_set_geometry_hints (GdkWindow *window,
;;;                                     const GdkGeometry *geometry,
;;;                                     GdkWindowHints geom_mask);
;;; 
;;; Sets the geometry hints for window. Hints flagged in geom_mask are set,
;;; hints not flagged in geom_mask are unset. To unset all hints, use a
;;; geom_mask of 0 and a geometry of NULL.
;;; 
;;; This function provides hints to the windowing system about acceptable sizes
;;; for a toplevel window. The purpose of this is to constrain user resizing,
;;; but the windowing system will typically (but is not required to) also
;;; constrain the current size of the window to the provided values and
;;; constrain programatic resizing via gdk_window_resize() or
;;; gdk_window_move_resize().
;;; 
;;; Note that on X11, this effect has no effect on windows of type
;;; GDK_WINDOW_TEMP or windows where override redirect has been turned on via
;;; gdk_window_set_override_redirect() since these windows are not resizable by
;;; the user.
;;; 
;;; Since you can't count on the windowing system doing the constraints for
;;; programmatic resizes, you should generally call gdk_window_constrain_size()
;;; yourself to determine appropriate sizes.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; geometry :
;;;     geometry hints
;;; 
;;; geom_mask :
;;;     bitmask indicating fields of geometry to pay attention to
;;; ----------------------------------------------------------------------------

(defcfun gdk-window-set-geometry-hints :void
  (window (g-object gdk-window))
  (geometry (g-boxed-foreign gdk-geometry))
  (geometry-mask gdk-window-hints))

(export 'gdk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_list ()
;;; 
;;; void gdk_window_set_icon_list (GdkWindow *window, GList *pixbufs);
;;; 
;;; Sets a list of icons for the window. One of these will be used to represent
;;; the window when it has been iconified. The icon is usually shown in an icon
;;; box or some sort of task bar. Which icon size is shown depends on the window
;;; manager. The window manager can scale the icon but setting several size
;;; icons can give better image quality since the window manager may only need
;;; to scale the icon by a small amount or not at all.
;;; 
;;; window :
;;;     The GdkWindow toplevel window to set the icon of.
;;; 
;;; pixbufs :
;;;     A list of pixbufs, of different sizes.
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-icon-list))

(defun gdk-window-set-icon-list (window pixbufs)
  (setf (gdk-window-icon-list window) pixbufs))

(export 'gdk-window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_modal_hint ()
;;; 
;;; void gdk_window_set_modal_hint (GdkWindow *window, gboolean modal);
;;; 
;;; The application can use this hint to tell the window manager that a certain
;;; window has modal behaviour. The window manager can use this information to
;;; handle modal windows in a special way.
;;; 
;;; You should only use this on windows for which you have previously called
;;; gdk_window_set_transient_for()
;;; 
;;; window :
;;;     A toplevel GdkWindow
;;; 
;;; modal :
;;;     TRUE if the window is modal, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-modal-hint))

(defun gdk-window-set-modal-hint (window modal)
  (setf (gdk-window-modal-hint window) modal))

(export 'gdk-window-set-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_modal_hint ()
;;; 
;;; gboolean gdk_window_get_modal_hint (GdkWindow *window);
;;; 
;;; Determines whether or not the window manager is hinted that window has
;;; modal behaviour.
;;; 
;;; window :
;;;     A toplevel GdkWindow.
;;; 
;;; Returns :
;;;     whether or not the window has the modal hint set.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-modal-hint))

(defun gdk-window-get-modal-hint (window)
  (gdk-window-modal-hint window))

(export 'gdk-window-get-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_type_hint ()
;;; 
;;; void gdk_window_set_type_hint (GdkWindow *window, GdkWindowTypeHint hint);
;;; 
;;; The application can use this call to provide a hint to the window manager
;;; about the functionality of a window. The window manager can use this
;;; information when determining the decoration and behaviour of the window.
;;; 
;;; The hint must be set before the window is mapped.
;;; 
;;; window :
;;;     A toplevel GdkWindow
;;; 
;;; hint :
;;;     A hint of the function this window will have
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-type-hint))

(defun gdk-window-set-type-hint (window hint)
  (setf (gdk-window-type-hint window) hint))

(export 'gdk-window-set-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type_hint ()
;;; 
;;; GdkWindowTypeHint gdk_window_get_type_hint (GdkWindow *window);
;;; 
;;; This function returns the type hint set for a window.
;;; 
;;; window :
;;;     A toplevel GdkWindow
;;; 
;;; Returns :
;;;     The type hint set for window
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-type-hint))

(defun gdk-window-get-type-hint (window)
  (gdk-window-type-hint window))

(export 'gdk-window-get-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_taskbar_hint ()
;;; 
;;; void gdk_window_set_skip_taskbar_hint (GdkWindow *window,
;;;                                        gboolean skips_taskbar);
;;; 
;;; Toggles whether a window should appear in a task list or window list. If
;;; a window's semantic type as specified with gdk_window_set_type_hint()
;;; already fully describes the window, this function should not be called in
;;; addition, instead you should allow the window to be treated according to
;;; standard policy for its semantic type.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; skips_taskbar :
;;;     TRUE to skip the taskbar
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-skip-taskbar-hint))

(defun gdk-window-set-taskbar-hint (window skips-taskbar)
  (setf (gdk-window-skip-taskbar-hint window) skips-taskbar))

(export 'gdk-window-set-skip-taskbar-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_pager_hint ()
;;; 
;;; void gdk_window_set_skip_pager_hint (GdkWindow *window,
;;;                                      gboolean skips_pager);
;;; 
;;; Toggles whether a window should appear in a pager (workspace switcher, or
;;; other desktop utility program that displays a small thumbnail representation
;;; of the windows on the desktop). If a window's semantic type as specified
;;; with gdk_window_set_type_hint() already fully describes the window, this
;;; function should not be called in addition, instead you should allow the
;;; window to be treated according to standard policy for its semantic type.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; skips_pager :
;;;     TRUE to skip the pager
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-skip-pager-hint))

(defun gdk-window-set-skip-pager-hint (window skips-pager)
  (setf (gdk-window-skip-pager-hint window) skips-pager))

(export 'gdk-window-set-skip-pager-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_urgency_hint ()
;;; 
;;; void gdk_window_set_urgency_hint (GdkWindow *window, gboolean urgent);
;;; 
;;; Toggles whether a window needs the user's urgent attention.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; urgent :
;;;     TRUE if the window is urgent
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-urgency-hint))

(defun gdk-window-set-urgency-hint (window urgent)
  (setf (gdk-window-urgency-hint window) urgent))

(export 'gdk-window-set-urgency-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_position ()
;;; 
;;; void gdk_window_get_position (GdkWindow *window, gint *x, gint *y);
;;; 
;;; Obtains the position of the window as reported in the
;;; most-recently-processed GdkEventConfigure. Contrast with
;;; gdk_window_get_geometry() which queries the X server for the current
;;; window position, regardless of which events have been received or processed.
;;; 
;;; The position coordinates are relative to the window's parent window.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     X coordinate of window
;;; 
;;; y :
;;;     Y coordinate of window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_position" %gdk-window-get-position) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-position (window)
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-position window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-get-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_origin ()
;;; 
;;; void gdk_window_get_root_origin (GdkWindow *window, gint *x, gint *y);
;;; 
;;; Obtains the top-left corner of the window manager frame in root window
;;; coordinates.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; x :
;;;     return location for X position of window frame
;;; 
;;; y :
;;;     return location for Y position of window frame
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_origin" %gdk-window-get-root-origin) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-root-origin (window)
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-root-origin window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gdk-window-get-root-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_extents ()
;;; 
;;; void gdk_window_get_frame_extents (GdkWindow *window, GdkRectangle *rect);
;;; 
;;; Obtains the bounding box of the window, including window manager
;;; titlebar/borders if any. The frame position is given in root window
;;; coordinates. To get the position of the window itself (rather than the
;;; frame) in root window coordinates, use gdk_window_get_origin().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; rect :
;;;     rectangle to fill with bounding box of the window frame
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_extents" %gdk-window-get-frame-extents) :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gdk-window-get-frame-extents (window)
  (let ((rectangle (make-gdk-rectangle)))
    (%gdk-window-get-frame-extents window rectangle)
    rectangle))

(export 'gdk-window-get-frame-extents)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_size
;;; 
;;; #define gdk_window_get_size gdk_drawable_get_size
;;; 
;;; Warning
;;; 
;;; gdk_window_get_size is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Deprecated equivalent of gdk_drawable_get_size().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_colormap
;;; 
;;; #define gdk_window_get_colormap gdk_drawable_get_colormap
;;; 
;;; Warning
;;; 
;;; gdk_window_get_colormap is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Deprecated equivalent of gdk_drawable_get_colormap().
;;; 
;;; Returns :
;;;     colormap for the window
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type
;;; 
;;; #define gdk_window_get_type gdk_window_get_window_type
;;; 
;;; Warning
;;; 
;;; gdk_window_get_type is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Deprecated equivalent of gdk_drawable_get_type().
;;; 
;;; Returns :
;;;     type of drawable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_origin ()
;;; 
;;; gint gdk_window_get_origin (GdkWindow *window, gint *x, gint *y);
;;; 
;;; Obtains the position of a window in root window coordinates. (Compare with
;;; gdk_window_get_position() and gdk_window_get_geometry() which return the
;;; position of a window relative to its parent window.)
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     return location for X coordinate
;;; 
;;; y :
;;;     return location for Y coordinate
;;; 
;;; Returns :
;;;     not meaningful, ignore
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_origin" %gdk-window-get-origin) :int
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-origin (window)
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-origin window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-get-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_deskrelative_origin ()
;;; 
;;; gboolean gdk_window_get_deskrelative_origin (GdkWindow *window,
;;;                                              gint *x,
;;;                                              gint *y);
;;; 
;;; Warning
;;; 
;;; gdk_window_get_deskrelative_origin is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; This gets the origin of a GdkWindow relative to an
;;; Enlightenment-window-manager desktop. As long as you don't assume that the
;;; user's desktop/workspace covers the entire root window (i.e. you don't
;;; assume that the desktop begins at root window coordinate 0,0) this function
;;; is not necessary. It's deprecated for that reason.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; x :
;;;     return location for X coordinate
;;; 
;;; y :
;;;     return location for Y coordinate
;;; 
;;; Returns :
;;;     not meaningful
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_coords ()
;;; 
;;; void gdk_window_get_root_coords (GdkWindow *window,
;;;                                  gint x,
;;;                                  gint y,
;;;                                  gint *root_x,
;;;                                  gint *root_y);
;;; 
;;; Obtains the position of a window position in root window coordinates. This
;;; is similar to gdk_window_get_origin() but allows you go pass in any position
;;; in the window, not just the origin.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     X coordinate in window
;;; 
;;; y :
;;;     Y coordinate in window
;;; 
;;; root_x :
;;;     return location for X coordinate
;;; 
;;; root_y :
;;;     return location for Y coordinate
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_coords" %gdk-window-get-root-coords) :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (root-x :int)
  (root-y :int))

(defun gdk-window-get-root-coords (window x y)
  (with-foreign-objects ((root-x :int) (root-y :int))
    (%gdk-window-get-root-coords window x y root-x root-y)
    (values (mem-ref root-x :int)
            (mem-ref root-y :int))))
  
(export 'gdk-window-get-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_from_parent ()
;;; 
;;; void gdk_window_coords_from_parent (GdkWindow *window,
;;;                                     gdouble parent_x,
;;;                                     gdouble parent_y,
;;;                                     gdouble *x,
;;;                                     gdouble *y);
;;; 
;;; Transforms window coordinates from a parent window to a child window, where
;;; the parent window is the normal parent as returned by
;;; gdk_window_get_parent() for normal windows, and the window's embedder as
;;; returned by gdk_offscreen_window_get_embedder() for offscreen windows.
;;; 
;;; For normal windows, calling this function is equivalent to subtracting the
;;; return values of gdk_window_get_position() from the parent coordinates. For
;;; offscreen windows however (which can be arbitrarily transformed), this
;;; function calls the GdkWindow::from-embedder: signal to translate the
;;; coordinates.
;;; 
;;; You should always use this function when writing generic code that walks
;;; down a window hierarchy.
;;; 
;;; See also: gdk_window_coords_to_parent()
;;; 
;;; window :
;;;     a child window
;;; 
;;; parent_x :
;;;     X coordinate in parent's coordinate system
;;; 
;;; parent_y :
;;;     Y coordinate in parent's coordinate system
;;; 
;;; x :
;;;     return location for X coordinate in child's coordinate system.
;;; 
;;; y :
;;;     return location for Y coordinate in child's coordinate system.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_from_parent" %gdk-window-coords-from-parent) :void
  (window (g-object gdk-window))
  (parent-x :double)
  (parent-y :double)
  (x :double)
  (y :double))

(defun gdk-window-coords-from-parent (window parent-x parent-y)
  (with-foreign-objects ((x :double) (y :double))
    (%gdk-window-coords-from-parent window parent-x parent-y x y)
    (values (mem-ref x :double)
            (mem-ref y :double))))
  
(export 'gdk-window-coords-form-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_to_parent ()
;;; 
;;; void gdk_window_coords_to_parent (GdkWindow *window,
;;;                                   gdouble x,
;;;                                   gdouble y,
;;;                                   gdouble *parent_x,
;;;                                   gdouble *parent_y);
;;; 
;;; Transforms window coordinates from a child window to its parent window,
;;; where the parent window is the normal parent as returned by
;;; gdk_window_get_parent() for normal windows, and the window's embedder as
;;; returned by gdk_offscreen_window_get_embedder() for offscreen windows.
;;; 
;;; For normal windows, calling this function is equivalent to adding the
;;; return values of gdk_window_get_position() to the child coordinates. For
;;; offscreen windows however (which can be arbitrarily transformed), this
;;; function calls the GdkWindow::to-embedder: signal to translate the
;;; coordinates.
;;; 
;;; You should always use this function when writing generic code that walks
;;; up a window hierarchy.
;;; 
;;; See also: gdk_window_coords_from_parent()
;;; 
;;; window :
;;;     a child window
;;; 
;;; x :
;;;     X coordinate in child's coordinate system
;;; 
;;; y :
;;;     Y coordinate in child's coordinate system
;;; 
;;; parent_x :
;;;     return location for X coordinate in parent's coordinate system, or NULL.
;;; 
;;; parent_y :
;;;     return location for Y coordinate in parent's coordinate system, or NULL.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_to_parent" %gdk-window-coords-to-parent) :void
  (window (g-object gdk-window))
  (x :double)
  (y :double)
  (parent-x :double)
  (parent-y :double))

(defun gdk-window-coords-to-parent (window x y)
  (with-foreign-objects ((parent-x :double) (parent-y :double))
    (%gdk-window-coords-to-parent window x y parent-x parent-y)
    (values (mem-ref parent-x :double)
            (mem-ref parent-y :double))))
  
(export 'gdk-window-coords-to-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pointer ()
;;; 
;;; GdkWindow * gdk_window_get_pointer (GdkWindow *window,
;;;                                     gint *x,
;;;                                     gint *y,
;;;                                     GdkModifierType *mask);
;;; 
;;; Obtains the current pointer position and modifier state. The position is
;;; given in coordinates relative to the upper left corner of window.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; x :
;;;     return location for X coordinate of pointer or NULL to not return the
;;;     X coordinate.
;;; 
;;; y :
;;;     return location for Y coordinate of pointer or NULL to not return the
;;;     Y coordinate
;;; 
;;; mask :
;;;     return location for modifier mask or NULL to not return the modifier
;;;     mask
;;; 
;;; Returns :
;;;     the window containing the pointer (as with gdk_window_at_pointer()), or
;;;     NULL if the window containing the pointer isn't known to GDK
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_pointer" %gdk-window-get-pointer)
    (g-object gdk-window)
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-get-pointer (window)
  (with-foreign-objects ((x :int) (y :int) (mask 'gdk-modifier-type))
    (let ((w (%gdk-window-get-pointer window x y mask)))
      (values w
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-get-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_parent ()
;;; 
;;; GdkWindow * gdk_window_get_parent (GdkWindow *window);
;;; 
;;; Obtains the parent of window, as known to GDK. Does not query the X server;
;;; thus this returns the parent as passed to gdk_window_new(), not the actual
;;; parent. This should never matter unless you're using Xlib calls mixed with
;;; GDK calls on the X11 platform. It may also matter for toplevel windows,
;;; because the window manager may choose to reparent them.
;;; 
;;; Note that you should use gdk_window_get_effective_parent() when writing
;;; generic code that walks up a window hierarchy, because
;;; gdk_window_get_parent() will most likely not do what you expect if there
;;; are offscreen windows in the hierarchy.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     parent of window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-parent))

(defun gdk-window-get-parent (window)
  (gdk-window-parent window))

(export 'gdk-window-get-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_parent ()
;;; 
;;; GdkWindow * gdk_window_get_effective_parent (GdkWindow *window);
;;; 
;;; Obtains the parent of window, as known to GDK. Works like
;;; gdk_window_get_parent() for normal windows, but returns the window's
;;; embedder for offscreen windows.
;;; 
;;; See also: gdk_offscreen_window_get_embedder()
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     effective parent of window
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_parent" gdk-window-get-effective-parent)
    (g-object gdk-window)
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevel ()
;;; 
;;; GdkWindow * gdk_window_get_toplevel (GdkWindow *window);
;;; 
;;; Gets the toplevel window that's an ancestor of window.
;;; 
;;; Any window type but GDK_WINDOW_CHILD is considered a toplevel window, as
;;; is a GDK_WINDOW_CHILD window that has a root window as parent.
;;; 
;;; Note that you should use gdk_window_get_effective_toplevel() when you want
;;; to get to a window's toplevel as seen on screen, because
;;; gdk_window_get_toplevel() will most likely not do what you expect if there
;;; are offscreen windows in the hierarchy.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the toplevel window containing window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-toplevel))

(defun gdk-window-get-toplevel (window)
  (gdk-window-toplevel window))

(export 'gdk-window-get-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_toplevel ()
;;; 
;;; GdkWindow * gdk_window_get_effective_toplevel (GdkWindow *window);
;;; 
;;; Gets the toplevel window that's an ancestor of window.
;;; 
;;; Works like gdk_window_get_toplevel(), but treats an offscreen window's
;;; embedder as its parent, using gdk_window_get_effective_parent().
;;; 
;;; See also: gdk_offscreen_window_get_embedder()
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the effective toplevel window containing window
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_toplevel" gdk-window-get-effective-toplevel)
    (g-object gdk-window)
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children ()
;;; 
;;; GList * gdk_window_get_children (GdkWindow *window);
;;; 
;;; Gets the list of children of window known to GDK. This function only
;;; returns children created via GDK, so for example it's useless when used
;;; with the root window; it only returns windows an application created itself.
;;; 
;;; The returned list must be freed, but the elements in the list need not be.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     list of child windows inside window.
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-children))

(defun gdk-window-get-children (window)
  (gdk-window-children window))

(export 'gdk-window-get-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_peek_children ()
;;; 
;;; GList * gdk_window_peek_children (GdkWindow *window);
;;; 
;;; Like gdk_window_get_children(), but does not copy the list of children, so
;;; the list does not need to be freed.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     a reference to the list of child windows in window.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_peek_children" gdk-window-peek-children)
    (g-list (g-object gdk-window))
  (window (g-object gdk-window)))

(export 'gdk-window-peek-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_events ()
;;; 
;;; GdkEventMask gdk_window_get_events (GdkWindow *window);
;;; 
;;; Gets the event mask for window. See gdk_window_set_events().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     event mask for window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-events))

(defun gdk-window-get-events (window)
  (gdk-window-events window))

(export 'gdk-window-get-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_events ()
;;; 
;;; void gdk_window_set_events (GdkWindow *window, GdkEventMask event_mask);
;;; 
;;; The event mask for a window determines which events will be reported for
;;; that window. For example, an event mask including GDK_BUTTON_PRESS_MASK
;;; means the window should report button press events. The event mask is the
;;; bitwise OR of values from the GdkEventMask enumeration.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; event_mask :
;;;     event mask for window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-events))

(defun gdk-window-set-events (window event-mask)
  (setf (gdk-window-events window) event-mask))

(export 'gdk-window-set-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon ()
;;; 
;;; void gdk_window_set_icon (GdkWindow *window,
;;;                           GdkWindow *icon_window,
;;;                           GdkPixmap *pixmap,
;;;                           GdkBitmap *mask);
;;; 
;;; Sets the icon of window as a pixmap or window. If using GTK+, investigate
;;; gtk_window_set_default_icon_list() first, and then
;;; gtk_window_set_icon_list() and gtk_window_set_icon(). If those don't meet
;;; your needs, look at gdk_window_set_icon_list(). Only if all those are too
;;; high-level do you want to fall back to gdk_window_set_icon().
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; icon_window :
;;;     a GdkWindow to use for the icon, or NULL to unset
;;; 
;;; pixmap :
;;;     a GdkPixmap to use as the icon, or NULL to unset
;;; 
;;; mask :
;;;     a 1-bit pixmap (GdkBitmap) to use as mask for pixmap, or NULL to have
;;;     none
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_name ()
;;; 
;;; void gdk_window_set_icon_name (GdkWindow *window, const gchar *name);
;;; 
;;; Windows may have a name used while minimized, distinct from the name they
;;; display in their titlebar. Most of the time this is a bad idea from a user
;;; interface standpoint. But you can set such a name with this function, if
;;; you like.
;;; 
;;; After calling this with a non-NULL name, calls to gdk_window_set_title()
;;; will not update the icon title.
;;; 
;;; Using NULL for name unsets the icon title; further calls to
;;; gdk_window_set_title() will again update the icon title as well.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; name :
;;;     name of window while iconified (minimized)
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-icon-name))

(defun gdk-window-set-icon-name (window name)
  (setf (gdk-window-icon-name window) name))

(export 'gdk-window-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_transient_for ()
;;; 
;;; void gdk_window_set_transient_for (GdkWindow *window, GdkWindow *parent);
;;; 
;;; Indicates to the window manager that window is a transient dialog associated
;;; with the application window parent. This allows the window manager to do
;;; things like center window on parent and keep window above parent.
;;; 
;;; See gtk_window_set_transient_for() if you're using GtkWindow or GtkDialog.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; parent :
;;;     another toplevel GdkWindow
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-transient-for))

(defun gdk-window-set-transient-for (window parent)
  (setf (gdk-window-transient-for window) parent))

(export 'gdk-window-set-transient-for)
 
;;; ----------------------------------------------------------------------------
;;; gdk_window_set_role ()
;;; 
;;; void gdk_window_set_role (GdkWindow *window, const gchar *role);
;;; 
;;; When using GTK+, typically you should use gtk_window_set_role() instead of
;;; this low-level function.
;;; 
;;; The window manager and session manager use a window's role to distinguish
;;; it from other kinds of window in the same application. When an application
;;; is restarted after being saved in a previous session, all windows with the
;;; same title and role are treated as interchangeable. So if you have two
;;; windows with the same title that should be distinguished for session
;;; management purposes, you should set the role on those windows. It doesn't
;;; matter what string you use for the role, as long as you have a different
;;; role for each non-interchangeable kind of window.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; role :
;;;     a string indicating its role
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-role))

(defun gdk-window-set-role (window role)
  (setf (gdk-window-role window) role))

(export 'gdk-window-set-role)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_startup_id ()
;;; 
;;; void gdk_window_set_startup_id (GdkWindow *window, const gchar *startup_id);
;;; 
;;; When using GTK+, typically you should use gtk_window_set_startup_id()
;;; instead of this low-level function.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; startup_id :
;;;     a string with startup-notification identifier
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-startup-id))

(defun gdk-window-set-startup-id (window startup-id)
  (setf (gdk-window-startup-id window) startup-id))

(export 'gdk-window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_group ()
;;; 
;;; void gdk_window_set_group (GdkWindow *window, GdkWindow *leader);
;;; 
;;; Sets the group leader window for window. By default, GDK sets the group
;;; leader for all toplevel windows to a global window implicitly created by
;;; GDK. With this function you can override this default.
;;; 
;;; The group leader window allows the window manager to distinguish all
;;; windows that belong to a single application. It may for example allow users
;;; to minimize/unminimize all windows belonging to an application at once. You
;;; should only set a non-default group window if your application pretends to
;;; be multiple applications.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; leader :
;;;     group leader window, or NULL to restore the default group leader window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-group))

(defun gdk-window-set-group (window leader)
  (setf (gdk-window-group window) leader))

(export 'gdk-window-set-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_group ()
;;; 
;;; GdkWindow * gdk_window_get_group (GdkWindow *window);
;;; 
;;; Returns the group leader window for window. See gdk_window_set_group().
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; Returns :
;;;     the group leader window for window
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-group))

(defun gdk-window-get-group (window)
  (gdk-window-group window))

(export 'gdk-window-get-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_decorations ()
;;; 
;;; void gdk_window_set_decorations (GdkWindow *window,
;;;                                  GdkWMDecoration decorations);
;;; 
;;; "Decorations" are the features the window manager adds to a toplevel
;;; GdkWindow. This function sets the traditional Motif window manager hints
;;; that tell the window manager which decorations you would like your window
;;; to have. Usually you should use gtk_window_set_decorated() on a GtkWindow
;;; instead of using the GDK function directly.
;;; 
;;; The decorations argument is the logical OR of the fields in the
;;; GdkWMDecoration enumeration. If GDK_DECOR_ALL is included in the mask, the
;;; other bits indicate which decorations should be turned off. If
;;; GDK_DECOR_ALL is not included, then the other bits indicate which
;;; decorations should be turned on.
;;; 
;;; Most window managers honor a decorations hint of 0 to disable all
;;; decorations, but very few honor all possible combinations of bits.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; decorations :
;;;     decoration hint mask
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-decorations))

(defun gdk-window-set-decorations (window decorations)
  (setf (gdk-window-decorations window) decorations))

(export 'gdk-window-set-decorations)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_decorations ()
;;; 
;;; gboolean gdk_window_get_decorations (GdkWindow *window,
;;;                                      GdkWMDecoration *decorations);
;;; 
;;; Returns the decorations set on the GdkWindow with gdk_window_set_decorations
;;; 
;;; window :
;;;     The toplevel GdkWindow to get the decorations from
;;; 
;;; decorations :
;;;     The window decorations will be written here
;;; 
;;; Returns :
;;;     TRUE if the window has decorations set, FALSE otherwise.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_decorations" %gdk-window-get-decorations) :boolean
  (window (g-object gdk-window))
  (decorations (:pointer gdk-wm-decoration)))

(defun gdk-window-get-decorations (window)
  (with-foreign-object (decorations 'gdk-wm-decoration)
    (%gdk-window-get-decorations window decorations)
    (mem-ref decorations 'gdk-wm-decoration)))

(export 'gdk-window-get-decorations)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_functions ()
;;; 
;;; void gdk_window_set_functions (GdkWindow *window, GdkWMFunction functions);
;;; 
;;; Sets hints about the window management functions to make available via
;;; buttons on the window frame.
;;; 
;;; On the X backend, this function sets the traditional Motif window manager
;;; hint for this purpose. However, few window managers do anything reliable or
;;; interesting with this hint. Many ignore it entirely.
;;; 
;;; The functions argument is the logical OR of values from the GdkWMFunction
;;; enumeration. If the bitmask includes GDK_FUNC_ALL, then the other bits
;;; indicate which functions to disable; if it doesn't include GDK_FUNC_ALL,
;;; it indicates which functions to enable.
;;; 
;;; window :
;;;     a toplevel GdkWindow
;;; 
;;; functions :
;;;     bitmask of operations to allow on window
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-functions))

(defun gdk-window-set-functions (window functions)
  (setf (gdk-window-functions window) functions))

(export 'gdk-window-set-functions)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevels ()
;;; 
;;; GList * gdk_window_get_toplevels (void);
;;; 
;;; Warning
;;; 
;;; gdk_window_get_toplevels has been deprecated since version 2.16 and should
;;; not be used in newly-written code. Use gdk_screen_get_toplevel_windows()
;;; instead.
;;; 
;;; Obtains a list of all toplevel windows known to GDK on the default screen
;;; (see gdk_screen_get_toplevel_windows()). A toplevel window is a child of
;;; the root window (see gdk_get_default_root_window()).
;;; 
;;; The returned list should be freed with g_list_free(), but its elements need
;;; not be freed.
;;; 
;;; Returns :
;;;     list of toplevel windows, free with g_list_free()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_get_default_root_window ()
;;; 
;;; GdkWindow * gdk_get_default_root_window (void);
;;; 
;;; Obtains the root window (parent all other windows are inside) for the
;;; default display and screen.
;;; 
;;; Returns :
;;;     the default root window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_default_root_window" gdk-get-default-root-window)
    (g-object gdk-window))

(export 'gdk-get-default-root-window)

;;; ----------------------------------------------------------------------------
;;; struct GdkPointerHooks
;;; 
;;; struct GdkPointerHooks {
;;;   GdkWindow* (*get_pointer)       (GdkWindow       *window,
;;;                                    gint            *y,
;;;                                    gint            *x,
;;;                                    GdkModifierType *mask);
;;;
;;;   GdkWindow* (*window_at_pointer) (GdkScreen *screen, /* unused */
;;;                                    gint      *win_x,
;;;                                    gint      *win_y);
;;; };
;;; 
;;; A table of pointers to functions for getting quantities related to the
;;; current pointer position. GDK has one global table of this type, which can
;;; be set using gdk_set_pointer_hooks().
;;; 
;;; This is only useful for such low-level tools as an event recorder.
;;; Applications should never have any reason to use this facility
;;; 
;;; get_pointer ()
;;;     Obtains the current pointer position and modifier state. The position
;;;     is given in coordinates relative to the window containing the pointer,
;;;     which is returned in window.
;;; 
;;; window_at_pointer ()
;;;     Obtains the window underneath the mouse pointer, returning the location
;;;     of that window in win_x, win_y. Returns NULL if the window under the
;;;     mouse pointer is not known to GDK (for example, belongs to another
;;;     application).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_set_pointer_hooks ()
;;; 
;;; GdkPointerHooks * gdk_set_pointer_hooks (const GdkPointerHooks *new_hooks);
;;; 
;;; Warning
;;; 
;;; gdk_set_pointer_hooks has been deprecated since version 2.24 and should not
;;; be used in newly-written code. This function will go away in GTK 3 for lack
;;; of use cases.
;;; 
;;; This function allows for hooking into the operation of getting the current
;;; location of the pointer. This is only useful for such low-level tools as an
;;; event recorder. Applications should never have any reason to use this
;;; facility.
;;; 
;;; This function is not multihead safe. For multihead operation, see
;;; gdk_display_set_pointer_hooks().
;;; 
;;; new_hooks :
;;;     a table of pointers to functions for getting quantities related to the
;;;     current pointer position, or NULL to restore the default table.
;;; 
;;; Returns :
;;;     the previous pointer hook table
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_pixmap ()
;;; 
;;; GdkPixmap * gdk_offscreen_window_get_pixmap (GdkWindow *window);
;;; 
;;; Gets the offscreen pixmap that an offscreen window renders into. If you
;;; need to keep this around over window resizes, you need to add a reference
;;; to it.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     The offscreen pixmap, or NULL if not offscreen
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; *** Not in present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_set_embedder ()
;;; 
;;; void gdk_offscreen_window_set_embedder (GdkWindow *window,
;;;                                         GdkWindow *embedder);
;;; 
;;; Sets window to be embedded in embedder.
;;; 
;;; To fully embed an offscreen window, in addition to calling this function,
;;; it is also necessary to handle the "pick-embedded-child" signal on the
;;; embedder and the "to-embedder" and "from-embedder" signals on window.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; embedder :
;;;     the GdkWindow that window gets embedded in
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_set_embedder" gdk_offscreen_window_set_embedder)
    :void
  (window (g-object gdk-window))
  (embedder (g-object gdk-window)))

(export 'gdk-offscreen-window-set-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_embedder ()
;;; 
;;; GdkWindow * gdk_offscreen_window_get_embedder (GdkWindow *window);
;;; 
;;; Gets the window that window is embedded in.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Returns :
;;;     the embedding GdkWindow, or NULL if window is not an embedded offscreen
;;;     window
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_embedder" gdk-offscreen-get-window-embedder)
    (g-object gdk-window)
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_window_geometry_changed ()
;;; 
;;; void gdk_window_geometry_changed (GdkWindow *window);
;;; 
;;; This function informs GDK that the geometry of an embedded offscreen window
;;; has changed. This is necessary for GDK to keep track of which offscreen
;;; window the pointer is in.
;;; 
;;; window :
;;;     an embedded offscreen GdkWindow
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_geometry_changed" gdk-window-geometry-changed) :void
  (window (g-object gdk-window)))

(export 'gdk-window-geometry-changed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_redirect_to_drawable ()
;;; 
;;; void gdk_window_redirect_to_drawable (GdkWindow *window,
;;;                                       GdkDrawable *drawable,
;;;                                       gint src_x,
;;;                                       gint src_y,
;;;                                       gint dest_x,
;;;                                       gint dest_y,
;;;                                       gint width,
;;;                                       gint height);
;;; 
;;; Redirects drawing into window so that drawing to the window in the
;;; rectangle specified by src_x, src_y, width and height is also drawn into
;;; drawable at dest_x, dest_y.
;;; 
;;; Only drawing between gdk_window_begin_paint_region() or
;;; gdk_window_begin_paint_rect() and gdk_window_end_paint() is redirected.
;;; 
;;; Redirection is active until gdk_window_remove_redirection() is called.
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; drawable :
;;;     a GdkDrawable
;;; 
;;; src_x :
;;;     x position in window
;;; 
;;; src_y :
;;;     y position in window
;;; 
;;; dest_x :
;;;     x position in drawable
;;; 
;;; dest_y :
;;;     y position in drawable
;;; 
;;; width :
;;;     width of redirection, or -1 to use the width of window
;;; 
;;; height :
;;;     height of redirection or -1 to use the height of window
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; ----------------------------------------------------------------------------
;;; gdk_window_remove_redirection ()
;;; 
;;; void gdk_window_remove_redirection (GdkWindow *window);
;;; 
;;; Removes any active redirection started by gdk_window_redirect_to_drawable().
;;; 
;;; window :
;;;     a GdkWindow
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; *** Not present in GTK 3.2 ***

;;; --- End of file gdk.window.lisp --------------------------------------------
