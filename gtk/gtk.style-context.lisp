;;; ----------------------------------------------------------------------------
;;; gtk.style-context.lisp
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
;;; GtkStyleContext
;;;
;;; Rendering UI elements
;;;
;;; Synopsis
;;;
;;;     GTK_STYLE_PROPERTY_BACKGROUND_COLOR
;;;     GTK_STYLE_PROPERTY_COLOR
;;;     GTK_STYLE_PROPERTY_FONT
;;;     GTK_STYLE_PROPERTY_MARGIN
;;;     GTK_STYLE_PROPERTY_PADDING
;;;     GTK_STYLE_PROPERTY_BORDER_WIDTH
;;;     GTK_STYLE_PROPERTY_BORDER_RADIUS
;;;     GTK_STYLE_PROPERTY_BORDER_STYLE
;;;     GTK_STYLE_PROPERTY_BORDER_COLOR
;;;     GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
;;;
;;;     GTK_STYLE_CLASS_BACKGROUND
;;;     GTK_STYLE_CLASS_BUTTON
;;;     GTK_STYLE_CLASS_CALENDAR
;;;     GTK_STYLE_CLASS_CELL
;;;     GTK_STYLE_CLASS_COMBOBOX_ENTRY
;;;     GTK_STYLE_CLASS_CHECK
;;;     GTK_STYLE_CLASS_DEFAULT
;;;     GTK_STYLE_CLASS_ENTRY
;;;     GTK_STYLE_CLASS_HEADER
;;;     GTK_STYLE_CLASS_MENU
;;;     GTK_STYLE_CLASS_RADIO
;;;     GTK_STYLE_CLASS_RUBBERBAND
;;;     GTK_STYLE_CLASS_SCROLLBAR
;;;     GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
;;;     GTK_STYLE_CLASS_SLIDER
;;;     GTK_STYLE_CLASS_TOOLTIP
;;;     GTK_STYLE_CLASS_TROUGH
;;;     GTK_STYLE_CLASS_ACCELERATOR
;;;     GTK_STYLE_CLASS_DOCK
;;;     GTK_STYLE_CLASS_GRIP
;;;     GTK_STYLE_CLASS_MENUBAR
;;;     GTK_STYLE_CLASS_MENUITEM
;;;     GTK_STYLE_CLASS_PROGRESSBAR
;;;     GTK_STYLE_CLASS_SPINNER
;;;     GTK_STYLE_CLASS_TOOLBAR
;;;     GTK_STYLE_CLASS_PRIMARY_TOOLBAR
;;;     GTK_STYLE_CLASS_INLINE_TOOLBAR
;;;     GTK_STYLE_CLASS_PANE_SEPARATOR
;;;     GTK_STYLE_CLASS_SEPARATOR
;;;     GTK_STYLE_CLASS_SIDEBAR
;;;     GTK_STYLE_CLASS_DND
;;;     GTK_STYLE_CLASS_ERROR
;;;     GTK_STYLE_CLASS_EXPANDER
;;;     GTK_STYLE_CLASS_FRAME
;;;     GTK_STYLE_CLASS_HIGHLIGHT
;;;     GTK_STYLE_CLASS_IMAGE
;;;     GTK_STYLE_CLASS_INFO
;;;     GTK_STYLE_CLASS_MARK
;;;     GTK_STYLE_CLASS_NOTEBOOK
;;;     GTK_STYLE_CLASS_QUESTION
;;;     GTK_STYLE_CLASS_SCALE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
;;;     GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
;;;     GTK_STYLE_CLASS_SPINBUTTON
;;;     GTK_STYLE_CLASS_VIEW
;;;     GTK_STYLE_CLASS_WARNING
;;;     GTK_STYLE_CLASS_HORIZONTAL
;;;     GTK_STYLE_CLASS_VERTICAL
;;;     GTK_STYLE_CLASS_TOP
;;;     GTK_STYLE_CLASS_BOTTOM
;;;     GTK_STYLE_CLASS_LEFT
;;;     GTK_STYLE_CLASS_RIGHT
;;;     GTK_STYLE_CLASS_LINKED
;;;     GTK_STYLE_CLASS_ARROW
;;;     GTK_STYLE_REGION_COLUMN
;;;     GTK_STYLE_REGION_COLUMN_HEADER
;;;     GTK_STYLE_REGION_ROW
;;;     GTK_STYLE_REGION_TAB
;;;
;;;     GtkStyleContext
;;;
;;;     gtk_style_context_new
;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_get
;;;     gtk_style_context_get_direction
;;;     gtk_style_context_get_junction_sides
;;;     gtk_style_context_set_parent
;;;     gtk_style_context_get_path
;;;     gtk_style_context_get_property
;;;     gtk_style_context_get_screen
;;;     gtk_style_context_get_state
;;;     gtk_style_context_get_style
;;;     gtk_style_context_get_style_property
;;;     gtk_style_context_get_style_valist
;;;     gtk_style_context_get_valist
;;;     gtk_style_context_get_section
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_background_color
;;;     gtk_style_context_get_border_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_get_font
;;;     gtk_style_context_invalidate
;;;     gtk_style_context_state_is_running
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_lookup_icon_set
;;;     gtk_style_context_notify_state_change
;;;     gtk_style_context_pop_animatable_region
;;;     gtk_style_context_push_animatable_region
;;;     gtk_style_context_cancel_animations
;;;     gtk_style_context_scroll_animations
;;;     gtk_style_context_remove_provider
;;;     gtk_style_context_remove_provider_for_screen
;;;     gtk_style_context_reset_widgets
;;;     gtk_style_context_set_background
;;;     gtk_style_context_restore
;;;     gtk_style_context_save
;;;     gtk_style_context_set_direction
;;;     gtk_style_context_set_junction_sides
;;;     gtk_style_context_set_path
;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_list_classes
;;;     gtk_style_context_add_region
;;;     gtk_style_context_remove_region
;;;     gtk_style_context_has_region
;;;     gtk_style_context_list_regions
;;;     gtk_style_context_set_screen
;;;     gtk_style_context_set_state
;;;
;;;     GtkBorder
;;;
;;;     gtk_border_new
;;;     gtk_border_copy
;;;     gtk_border_free
;;;
;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_extension
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_frame_gap
;;;     gtk_render_handle
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option
;;;     gtk_render_slider
;;;     gtk_render_activity
;;;     gtk_render_icon_pixbuf
;;;     gtk_render_icon
;;;     gtk_render_insertion_cursor
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkStyleContext
;;;
;;;   GBoxed
;;;    +----GtkBorder
;;;
;;; Properties
;;;
;;;   "direction"                GtkTextDirection      : Read / Write
;;;   "parent"                   GtkStyleContext*      : Read / Write
;;;   "screen"                   GdkScreen*            : Read / Write
;;;
;;; Signals
;;;
;;;   "changed"                                        : Run First
;;;
;;; Description
;;;
;;; GtkStyleContext is an object that stores styling information affecting a
;;; widget defined by GtkWidgetPath.
;;;
;;; In order to construct the final style information, GtkStyleContext queries
;;; information from all attached GtkStyleProviders. Style providers can be
;;; either attached explicitly to the context through
;;; gtk_style_context_add_provider(), or to the screen through
;;; gtk_style_context_add_provider_for_screen(). The resulting style is a
;;; combination of all providers' information in priority order.
;;;
;;; For GTK+ widgets, any GtkStyleContext returned by
;;; gtk_widget_get_style_context() will already have a GtkWidgetPath, a
;;; GdkScreen and RTL/LTR information set. The style context will be also
;;; updated automatically if any of these settings change on the widget.
;;;
;;; If you are using the theming layer standalone, you will need to set a widget
;;; path and a screen yourself to the created style context through
;;; gtk_style_context_set_path() and gtk_style_context_set_screen(), as well as
;;; updating the context yourself using gtk_style_context_invalidate() whenever
;;; any of the conditions change, such as a change in the "gtk-theme-name"
;;; setting or a hierarchy change in the rendered widget.
;;;
;;; Transition animations
;;;
;;; GtkStyleContext has built-in support for state change transitions. Note that
;;; these animations respect the "gtk-enable-animations" setting.
;;;
;;; For simple widgets where state changes affect the whole widget area, calling
;;; gtk_style_context_notify_state_change() with a NULL region is sufficient to
;;; trigger the transition animation. And GTK+ already does that when
;;; gtk_widget_set_state() or gtk_widget_set_state_flags() are called.
;;;
;;; If a widget needs to declare several animatable regions (i.e. not affecting
;;; the whole widget area), its "draw" signal handler needs to wrap the render
;;; operations for the different regions with calls to
;;; gtk_style_context_push_animatable_region() and
;;; gtk_style_context_pop_animatable_region(). These functions take an
;;; identifier for the region which must be unique within the style context. For
;;; simple widgets with a fixed set of animatable regions, using an enumeration
;;; works well:
;;;
;;; Example 9. Using an enumeration to identify animatable regions
;;;
;;;   enum {
;;;     REGION_ENTRY,
;;;     REGION_BUTTON_UP,
;;;     REGION_BUTTON_DOWN
;;;   };
;;;
;;;   ...
;;;
;;;   gboolean
;;;   spin_button_draw (GtkWidget *widget,
;;;                     cairo_t   *cr)
;;;   {
;;;     GtkStyleContext *context;
;;;
;;;     context = gtk_widget_get_style_context (widget);
;;;
;;;     gtk_style_context_push_animatable_region
;;;                                           (context,
;;;                                            GUINT_TO_POINTER (REGION_ENTRY));
;;;
;;;     gtk_render_background (cr, 0, 0, 100, 30);
;;;     gtk_render_frame (cr, 0, 0, 100, 30);
;;;
;;;     gtk_style_context_pop_animatable_region (context);
;;;
;;;     ...
;;;   }
;;;
;;;
;;; For complex widgets with an arbitrary number of animatable regions, it is up
;;; to the implementation to come up with a way to uniquely identify each
;;; animatable region. Using pointers to internal structs is one way to achieve
;;; this:
;;;
;;; Example 10. Using struct pointers to identify animatable regions
;;;
;;;   void
;;;   notebook_draw_tab (GtkWidget    *widget,
;;;                      NotebookPage *page,
;;;                      cairo_t      *cr)
;;;   {
;;;     gtk_style_context_push_animatable_region (context, page);
;;;     gtk_render_extension (cr, page->x, page->y, page->width, page->height);
;;;     gtk_style_context_pop_animatable_region (context);
;;;   }
;;;
;;;
;;; The widget also needs to notify the style context about a state change for a
;;; given animatable region so the animation is triggered.
;;;
;;; Example 11. Triggering a state change animation on a region
;;;
;;;   gboolean
;;;   notebook_motion_notify (GtkWidget      *widget,
;;;                           GdkEventMotion *event)
;;;   {
;;;     GtkStyleContext *context;
;;;     NotebookPage *page;
;;;
;;;     context = gtk_widget_get_style_context (widget);
;;;     page = find_page_under_pointer (widget, event);
;;;     gtk_style_context_notify_state_change (context,
;;;                                            gtk_widget_get_window (widget),
;;;                                            page,
;;;                                            GTK_STATE_PRELIGHT,
;;;                                            TRUE);
;;;     ...
;;;   }
;;;
;;;
;;; gtk_style_context_notify_state_change() accepts NULL region IDs as a special
;;; value, in this case, the whole widget area will be updated by the animation.
;;;
;;; Style classes and regions
;;;
;;; Widgets can add style classes to their context, which can be used to
;;; associate different styles by class (see the section called “Selectors”).
;;; Theme engines can also use style classes to vary their rendering. GTK+ has a
;;; number of predefined style classes:
;;;
;;;   GTK_STYLE_CLASS_CELL,
;;;   GTK_STYLE_CLASS_ENTRY,
;;;   GTK_STYLE_CLASS_BUTTON,
;;;   GTK_STYLE_CLASS_COMBOBOX_ENTRY,
;;;   GTK_STYLE_CLASS_CALENDAR,
;;;   GTK_STYLE_CLASS_SLIDER,
;;;   GTK_STYLE_CLASS_BACKGROUND,
;;;   GTK_STYLE_CLASS_RUBBERBAND,
;;;   GTK_STYLE_CLASS_TOOLTIP,
;;;   GTK_STYLE_CLASS_MENU,
;;;   GTK_STYLE_CLASS_MENUBAR,
;;;   GTK_STYLE_CLASS_MENUITEM,
;;;   GTK_STYLE_CLASS_TOOLBAR,
;;;   GTK_STYLE_CLASS_PRIMARY_TOOLBAR,
;;;   GTK_STYLE_CLASS_INLINE_TOOLBAR,
;;;   GTK_STYLE_CLASS_RADIO,
;;;   GTK_STYLE_CLASS_CHECK,
;;;   GTK_STYLE_CLASS_TROUGH,
;;;   GTK_STYLE_CLASS_SCROLLBAR,
;;;   GTK_STYLE_CLASS_SCALE,
;;;   GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE,
;;;   GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW,
;;;   GTK_STYLE_CLASS_HEADER,
;;;   GTK_STYLE_CLASS_ACCELERATOR,
;;;   GTK_STYLE_CLASS_GRIP,
;;;   GTK_STYLE_CLASS_DOCK,
;;;   GTK_STYLE_CLASS_PROGRESSBAR,
;;;   GTK_STYLE_CLASS_SPINNER,
;;;   GTK_STYLE_CLASS_EXPANDER,
;;;   GTK_STYLE_CLASS_SPINBUTTON,
;;;   GTK_STYLE_CLASS_NOTEBOOK,
;;;   GTK_STYLE_CLASS_VIEW,
;;;   GTK_STYLE_CLASS_SIDEBAR,
;;;   GTK_STYLE_CLASS_IMAGE,
;;;   GTK_STYLE_CLASS_HIGHLIGHT,
;;;   GTK_STYLE_CLASS_FRAME,
;;;   GTK_STYLE_CLASS_DND,
;;;   GTK_STYLE_CLASS_PANE_SEPARATOR,
;;;   GTK_STYLE_CLASS_SEPARATOR,
;;;   GTK_STYLE_CLASS_INFO,
;;;   GTK_STYLE_CLASS_WARNING,
;;;   GTK_STYLE_CLASS_QUESTION,
;;;   GTK_STYLE_CLASS_ERROR,
;;;   GTK_STYLE_CLASS_HORIZONTAL,
;;;   GTK_STYLE_CLASS_VERTICAL,
;;;   GTK_STYLE_CLASS_TOP,
;;;   GTK_STYLE_CLASS_BOTTOM,
;;;   GTK_STYLE_CLASS_LEFT,
;;;   GTK_STYLE_CLASS_RIGHT
;;;
;;; Widgets can also add regions with flags to their context. The regions used
;;; by GTK+ widgets are:
;;;
;;; Region        Flags                  Macro                       Used by
;;;
;;; row           even, odd              GTK_STYLE_REGION_ROW        GtkTreeView
;;; column        first, last, sorted    GTK_STYLE_REGION_COLUMN     GtkTreeView
;;; column-header                        GTK_STYLE_REGION_COLUMN_HEADER
;;; tab           even, odd, first, last GTK_STYLE_REGION_TAB        GtkNotebook
;;;
;;; Custom styling in UI libraries and applications
;;;
;;; If you are developing a library with custom GtkWidgets that render
;;; differently than standard components, you may need to add a GtkStyleProvider
;;; yourself with the GTK_STYLE_PROVIDER_PRIORITY_FALLBACK priority, either a
;;; GtkCssProvider or a custom object implementing the GtkStyleProvider
;;; interface. This way theming engines may still attempt to style your UI
;;; elements in a different way if needed so.
;;;
;;; If you are using custom styling on an applications, you probably want then
;;; to make your style information prevail to the theme's, so you must use a
;;; GtkStyleProvider with the GTK_STYLE_PROVIDER_PRIORITY_APPLICATION priority,
;;; keep in mind that the user settings in XDG_CONFIG_HOME/gtk-3.0/gtk.css will
;;; still take precedence over your changes, as it uses the
;;; GTK_STYLE_PROVIDER_PRIORITY_USER priority.
;;;
;;; If a custom theming engine is needed, you probably want to implement a
;;; GtkStyleProvider yourself so it points to your GtkThemingEngine
;;; implementation, as GtkCssProvider uses gtk_theming_engine_load() which loads
;;; the theming engine module from the standard paths.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "direction" property
;;;
;;;   "direction"                GtkTextDirection      : Read / Write
;;;
;;; Text direction.
;;;
;;; Default value: GTK_TEXT_DIR_LTR
;;;
;;; ----------------------------------------------------------------------------
;;; The "parent" property
;;;
;;;   "parent"                   GtkStyleContext*      : Read / Write
;;;
;;; Sets or gets the style context's parent. See gtk_style_context_set_parent()
;;; for details.
;;;
;;; Since 3.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen" property
;;;
;;;   "screen"                   GdkScreen*            : Read / Write
;;;
;;; The associated GdkScreen.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;;
;;; void user_function (GtkStyleContext *stylecontext,
;;;                     gpointer         user_data)         : Run First
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BACKGROUND_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_BACKGROUND_COLOR "background-color"
;;;
;;; A property holding the background color of rendered elements as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_COLOR "color"
;;;
;;; A property holding the foreground color of rendered elements as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_FONT
;;;
;;; #define GTK_STYLE_PROPERTY_FONT "font"
;;;
;;; A property holding the font properties used when rendering text as a
;;; PangoFontDescription.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_MARGIN
;;;
;;; #define GTK_STYLE_PROPERTY_MARGIN "margin"
;;;
;;; A property holding the rendered element's margin as a GtkBorder. The margin
;;; is defined as the spacing between the border of the element and its
;;; surrounding elements. It is external to GtkWidgets's size allocations, and
;;; the most external spacing property of the padding/border/margin series.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_PADDING
;;;
;;; #define GTK_STYLE_PROPERTY_PADDING "padding"
;;;
;;; A property holding the rendered element's padding as a GtkBorder. The
;;; padding is defined as the spacing between the inner part of the element
;;; border and its child. It's the innermost spacing property of the
;;; padding/border/margin series.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_WIDTH
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_WIDTH "border-width"
;;;
;;; A property holding the rendered element's border width in pixels as a
;;; GtkBorder. The border is the intermediary spacing property of the
;;; padding/border/margin series.
;;;
;;; gtk_render_frame() uses this property to find out the frame line width, so
;;; GtkWidgets rendering frames may need to add up this padding when requesting
;;; size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_RADIUS
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_RADIUS "border-radius"
;;;
;;; A property holding the rendered element's border radius in pixels as a gint.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_STYLE
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_STYLE "border-style"
;;;
;;; A property holding the element's border style as a GtkBorderStyle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BORDER_COLOR
;;;
;;; #define GTK_STYLE_PROPERTY_BORDER_COLOR "border-color"
;;;
;;; A property holding the element's border color as a GdkRGBA.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
;;;
;;; #define GTK_STYLE_PROPERTY_BACKGROUND_IMAGE "background-image"
;;;
;;; A property holding the element's background as a cairo_pattern_t.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BACKGROUND
;;;
;;; #define GTK_STYLE_CLASS_BACKGROUND "background"
;;;
;;; A CSS class to match the window background.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BUTTON
;;;
;;; #define GTK_STYLE_CLASS_BUTTON "button"
;;;
;;; A CSS class to match buttons.
;;;
;;; This is used by GtkButton and its subclasses, as well as various other
;;; widget pieces that appear like buttons, e.g. the arrows in a GtkCalendar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CALENDAR
;;;
;;; #define GTK_STYLE_CLASS_CALENDAR "calendar"
;;;
;;; A CSS class to match calendars.
;;;
;;; This is not used by GTK+ itself, currently.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CELL
;;;
;;; #define GTK_STYLE_CLASS_CELL "cell"
;;;
;;; A CSS class to match content rendered in cell views.
;;;
;;; This is used by cell renderers, e.g. in GtkIconView and GtkTreeView.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_COMBOBOX_ENTRY
;;;
;;; #define GTK_STYLE_CLASS_COMBOBOX_ENTRY "combobox-entry"
;;;
;;; A CSS class to match combobox entries.
;;;
;;; This is used by GtkComboBox.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_CHECK
;;;
;;; #define GTK_STYLE_CLASS_CHECK "check"
;;;
;;; A CSS class to match check boxes.
;;;
;;; This is used in GtkCheckButton, GtkCheckMenuItem and GtkCellRendererToggle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DEFAULT
;;;
;;; #define GTK_STYLE_CLASS_DEFAULT "default"
;;;
;;; A CSS class to match the default widget.
;;;
;;; This is used by GtkButton.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ENTRY
;;;
;;; #define GTK_STYLE_CLASS_ENTRY "entry"
;;;
;;; A CSS class to match text entries.
;;;
;;; This is used by GtkEntry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HEADER
;;;
;;; #define GTK_STYLE_CLASS_HEADER "header"
;;;
;;; A CSS class to match a header element.
;;;
;;; This is used for the header in GtkCalendar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENU
;;;
;;; #define GTK_STYLE_CLASS_MENU "menu"
;;;
;;; A CSS class to match popup menus.
;;;
;;; This is used in GtkMenu.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RADIO
;;;
;;; #define GTK_STYLE_CLASS_RADIO "radio"
;;;
;;; A CSS class to match radio buttons.
;;;
;;; This is used in GtkRadioButton, GtkRadioMenuItem and GtkCellRendererToggle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RUBBERBAND
;;;
;;; #define GTK_STYLE_CLASS_RUBBERBAND "rubberband"
;;;
;;; A CSS class to match the rubberband selection rectangle.
;;;
;;; This is used in GtkIconView and GtkTreeView.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCROLLBAR
;;;
;;; #define GTK_STYLE_CLASS_SCROLLBAR "scrollbar"
;;;
;;; A CSS class to match scrollbars.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCROLLBARS_JUNCTION
;;;
;;; #define GTK_STYLE_CLASS_SCROLLBARS_JUNCTION "scrollbars-junction"
;;;
;;; A CSS class to match the junction area between an horizontal and vertical
;;; scrollbar, when they're both shown.
;;;
;;; This is used in GtkScrolledWindow.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SLIDER
;;;
;;; #define GTK_STYLE_CLASS_SLIDER "slider"
;;;
;;; A CSS class to match sliders.
;;;
;;; This is used by GtkSwitch and GtkRange and its subclasses.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOOLTIP
;;;
;;; #define GTK_STYLE_CLASS_TOOLTIP "tooltip"
;;;
;;; A CSS class to match tooltip windows.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TROUGH
;;;
;;; #define GTK_STYLE_CLASS_TROUGH "trough"
;;;
;;; A CSS class to match troughs, as in scrollbars and progressbars.
;;;
;;; This is used in GtkRange and its subclasses, GtkProgressBar and GtkSwitch.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ACCELERATOR
;;;
;;; #define GTK_STYLE_CLASS_ACCELERATOR "accelerator"
;;;
;;; A CSS class to match an accelerator.
;;;
;;; This is used for the accelerator in GtkAccelLabel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DOCK
;;;
;;; #define GTK_STYLE_CLASS_DOCK "dock"
;;;
;;; A CSS class defining a dock area.
;;;
;;; This is used by GtkHandleBox.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_GRIP
;;;
;;; #define GTK_STYLE_CLASS_GRIP "grip"
;;;
;;; A CSS class defining a resize grip.
;;;
;;; This is used for the resize grip in GtkWindow.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENUBAR
;;;
;;; #define GTK_STYLE_CLASS_MENUBAR "menubar"
;;;
;;; A CSS class to menubars.
;;;
;;; This is used in GtkMenuBar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MENUITEM
;;;
;;; #define GTK_STYLE_CLASS_MENUITEM "menuitem"
;;;
;;; A CSS class to match menu items.
;;;
;;; This is used in GtkMenuItem and its subclasses.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PROGRESSBAR
;;;
;;; #define GTK_STYLE_CLASS_PROGRESSBAR "progressbar"
;;;
;;; A CSS class to use when rendering activity as a progressbar.
;;;
;;; This is used in GtkProgressBar and when drawing progress inside a GtkEntry
;;; or in GtkCellRendererProgress.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SPINNER
;;;
;;; #define GTK_STYLE_CLASS_SPINNER "spinner"
;;;
;;; A CSS class to use when rendering activity as a 'spinner'.
;;;
;;; This is used by GtkSpinner and GtkCellRendererSpinner.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_TOOLBAR "toolbar"
;;;
;;; A CSS class to match toolbars.
;;;
;;; This is used in GtkToolbar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PRIMARY_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_PRIMARY_TOOLBAR "primary-toolbar"
;;;
;;; A CSS class to match primary toolbars.
;;;
;;; This should be used for the 'main' toolbar of an application, right below
;;; its menubar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_INLINE_TOOLBAR
;;;
;;; #define GTK_STYLE_CLASS_INLINE_TOOLBAR "inline-toolbar"
;;;
;;; A CSS class to match inline toolbars.
;;;
;;; This should be used for toolbars that are used to hold actions below lists,
;;; as seen e.g. in the left pane of the file chooser.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_PANE_SEPARATOR
;;;
;;; #define GTK_STYLE_CLASS_PANE_SEPARATOR "pane-separator"
;;;
;;; A CSS class for a pane separator, such as those in GtkPaned.
;;;
;;; Used in GtkPaned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SEPARATOR
;;;
;;; #define GTK_STYLE_CLASS_SEPARATOR "separator"
;;;
;;; A CSS class for a separator.
;;;
;;; This is used in GtkSeparator, GtkSeparatorMenuItem, GtkSeparatorToolItem,
;;; and when drawing separators in GtkTreeView.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SIDEBAR
;;;
;;; #define GTK_STYLE_CLASS_SIDEBAR "sidebar"
;;;
;;; A CSS class defining a sidebar, such as the left side in a file chooser.
;;;
;;; This is used in GtkFileChooser and in GtkAssistant.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_DND
;;;
;;; #define GTK_STYLE_CLASS_DND "dnd"
;;;
;;; A CSS class for a drag-and-drop indicator.
;;;
;;; This is used when drawing an outline around a potential drop target during
;;; DND.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ERROR
;;;
;;; #define GTK_STYLE_CLASS_ERROR "error"
;;;
;;; A CSS class for an area displaying an error message, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_EXPANDER
;;;
;;; #define GTK_STYLE_CLASS_EXPANDER "expander"
;;;
;;; A CSS class defining an expander, such as those in treeviews.
;;;
;;; Used for drawing expanders in GtkTreeView, GtkExpander and GtkToolItemGroup.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_FRAME
;;;
;;; #define GTK_STYLE_CLASS_FRAME "frame"
;;;
;;; A CSS class defining a frame delimiting content, such as GtkFrame or the
;;; scrolled window frame around the scrollable area.
;;;
;;; This is used in GtkFrame and GtkScrollbar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HIGHLIGHT
;;;
;;; #define GTK_STYLE_CLASS_HIGHLIGHT "highlight"
;;;
;;; A CSS class defining a highlighted area, such as headings in assistants and
;;; calendars.
;;;
;;; This is used in GtkAssistant and GtkCalendar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_IMAGE
;;;
;;; #define GTK_STYLE_CLASS_IMAGE "image"
;;;
;;; A CSS class defining an image, such as the icon in an entry.
;;;
;;; This is used when rendering icons in GtkEntry.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_INFO
;;;
;;; #define GTK_STYLE_CLASS_INFO "info"
;;;
;;; A CSS class for an area displaying an informational message, such as those
;;; in infobars.
;;;
;;; This is used by GtkInfoBar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_MARK
;;;
;;; #define GTK_STYLE_CLASS_MARK "mark"
;;;
;;; A CSS class defining marks in a widget, such as in scales.
;;;
;;; Used in GtkScale.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_NOTEBOOK
;;;
;;; #define GTK_STYLE_CLASS_NOTEBOOK "notebook"
;;;
;;; A CSS class defining a notebook.
;;;
;;; Used in GtkNotebook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_QUESTION
;;;
;;; #define GTK_STYLE_CLASS_QUESTION "question"
;;;
;;; A CSS class for an area displaying a question to the user, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE
;;;
;;; #define GTK_STYLE_CLASS_SCALE "scale"
;;;
;;; A CSS class to match scale widgets.
;;;
;;; This is used in GtkScale.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE
;;;
;;; #define GTK_STYLE_CLASS_SCALE_HAS_MARKS_ABOVE "scale-has-marks-above"
;;;
;;; A CSS class to match scale widgets with marks attached, all the marks are
;;; above for horizontal GtkScale. left for vertical GtkScale.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW
;;;
;;; #define GTK_STYLE_CLASS_SCALE_HAS_MARKS_BELOW "scale-has-marks-below"
;;;
;;; A CSS class to match scale widgets with marks attached, all the marks are
;;; below for horizontal GtkScale, right for vertical GtkScale.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_SPINBUTTON
;;;
;;; #define GTK_STYLE_CLASS_SPINBUTTON "spinbutton"
;;;
;;; A CSS class defining an spinbutton.
;;;
;;; This is used in GtkSpinButton.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_VIEW
;;;
;;; #define GTK_STYLE_CLASS_VIEW "view"
;;;
;;; A CSS class defining a view, such as iconviews or treeviews.
;;;
;;; This is used in GtkTreeView, GtkIconView, GtkTextView, as well as
;;; GtkCalendar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_WARNING
;;;
;;; #define GTK_STYLE_CLASS_WARNING "warning"
;;;
;;; A CSS class for an area displaying a warning message, such as those in
;;; infobars.
;;;
;;; This is used by GtkInfoBar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_HORIZONTAL
;;;
;;; #define GTK_STYLE_CLASS_HORIZONTAL "horizontal"
;;;
;;; A CSS class for horizontally layered widgets.
;;;
;;; This is used by widgets implementing GtkOrientable.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_VERTICAL
;;;
;;; #define GTK_STYLE_CLASS_VERTICAL "vertical"
;;;
;;; A CSS class for vertically layered widgets.
;;;
;;; This is used by widgets implementing GtkOrientable.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_TOP
;;;
;;; #define GTK_STYLE_CLASS_TOP "top"
;;;
;;; A CSS class to indicate an area at the top of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_BOTTOM
;;;
;;; #define GTK_STYLE_CLASS_BOTTOM "bottom"
;;;
;;; A CSS class to indicate an area at the bottom of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LEFT
;;;
;;; #define GTK_STYLE_CLASS_LEFT "left"
;;;
;;; A CSS class to indicate an area at the left of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_RIGHT
;;;
;;; #define GTK_STYLE_CLASS_RIGHT "right"
;;;
;;; A CSS class to indicate an area at the right of a widget.
;;;
;;; This is used by widgets that can render an area in different positions, such
;;; as tabs in a GtkNotebook.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_LINKED
;;;
;;; #define GTK_STYLE_CLASS_LINKED "linked"
;;;
;;; A CSS class to match a linked area, such as a box containing buttons
;;; belonging to the same control.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_CLASS_ARROW
;;;
;;; #define GTK_STYLE_CLASS_ARROW "arrow"
;;;
;;; A CSS class used when rendering an arrow element.
;;;
;;; Note that gtk_render_arrow automatically adds this style class to the style
;;; context when rendering an arrow element.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_COLUMN
;;;
;;; #define GTK_STYLE_REGION_COLUMN "column"
;;;
;;; A widget region name to define a treeview column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_COLUMN_HEADER
;;;
;;; #define GTK_STYLE_REGION_COLUMN_HEADER "column-header"
;;;
;;; A widget region name to define a treeview column header.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_ROW
;;;
;;; #define GTK_STYLE_REGION_ROW "row"
;;;
;;; A widget region name to define a treeview row.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_REGION_TAB
;;;
;;; #define GTK_STYLE_REGION_TAB "tab"
;;;
;;; A widget region name to define a notebook tab.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkStyleContext
;;;
;;; typedef struct _GtkStyleContext GtkStyleContext;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_new ()
;;;
;;; GtkStyleContext * gtk_style_context_new (void);
;;;
;;; Creates a standalone GtkStyleContext, this style context won't be attached
;;; to any widget, so you may want to call gtk_style_context_set_path()
;;; yourself.
;;;
;;; Note
;;;
;;; This function is only useful when using the theming layer separated from
;;; GTK+, if you are using GtkStyleContext to theme GtkWidgets, use
;;; gtk_widget_get_style_context() in order to get a style context ready to
;;; theme the widget.
;;;
;;; Returns :
;;;     A newly created GtkStyleContext.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider ()
;;;
;;; void gtk_style_context_add_provider (GtkStyleContext *context,
;;;                                      GtkStyleProvider *provider,
;;;                                      guint priority);
;;;
;;; Adds a style provider to context, to be used in style construction.
;;;
;;; Note
;;;
;;; If both priorities are the same, A GtkStyleProvider added through this
;;; function takes precedence over another added through
;;; gtk_style_context_add_provider_for_screen().
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; priority :
;;;     the priority of the style provider. The lower it is, the earlier it will
;;;     be used in the style construction. Typically this will be in the range
;;;     between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider_for_screen ()
;;;
;;; void gtk_style_context_add_provider_for_screen (GdkScreen *screen,
;;;                                                 GtkStyleProvider *provider,
;;;                                                 guint priority);
;;;
;;; Adds a global style provider to screen, which will be used in style
;;; construction for all GtkStyleContexts under screen.
;;;
;;; GTK+ uses this to make styling information from GtkSettings available.
;;;
;;; Note
;;;
;;; If both priorities are the same, A GtkStyleProvider added through
;;; gtk_style_context_add_provider() takes precedence over another added through
;;; this function.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; priority :
;;;     the priority of the style provider. The lower it is, the earlier it will
;;;     be used in the style construction. Typically this will be in the range
;;;     between GTK_STYLE_PROVIDER_PRIORITY_FALLBACK and
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get ()
;;;
;;; void gtk_style_context_get (GtkStyleContext *context,
;;;                             GtkStateFlags state,
;;;                             ...);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
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
;;; gtk_style_context_get_direction ()
;;;
;;; GtkTextDirection gtk_style_context_get_direction (GtkStyleContext *context);
;;;
;;; Returns the widget direction used for rendering.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     the widget direction
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_junction_sides ()
;;;
;;; GtkJunctionSides gtk_style_context_get_junction_sides
;;;                                                  (GtkStyleContext *context);
;;;
;;; Returns the sides where rendered elements connect visually with others.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     the junction sides
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_parent ()
;;;
;;; void gtk_style_context_set_parent (GtkStyleContext *context,
;;;                                    GtkStyleContext *parent);
;;;
;;; Sets the parent style context for context. The parent style context is used
;;; to implement inheritance of properties.
;;;
;;; If you are using a GtkStyleContext returned from
;;; gtk_widget_get_style_context(), the parent will be set for you.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; parent :
;;;     the new parent or NULL
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_path ()
;;;
;;; const GtkWidgetPath * gtk_style_context_get_path (GtkStyleContext *context);
;;;
;;; Returns the widget path used for style matching.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     A GtkWidgetPath.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_property ()
;;;
;;; void gtk_style_context_get_property (GtkStyleContext *context,
;;;                                      const gchar *property,
;;;                                      GtkStateFlags state,
;;;                                      GValue *value);
;;;
;;; Gets a style property from context for the given state.
;;;
;;; When value is no longer needed, g_value_unset() must be called to free any
;;; allocated memory.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; property :
;;;     style property name
;;;
;;; state :
;;;     state to retrieve the property value for
;;;
;;; value :
;;;     return location for the style property value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_screen ()
;;;
;;; GdkScreen * gtk_style_context_get_screen (GtkStyleContext *context);
;;;
;;; Returns the GdkScreen to which context is attached.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     a GdkScreen
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_state ()
;;;
;;; GtkStateFlags gtk_style_context_get_state (GtkStyleContext *context);
;;;
;;; Returns the state used when rendering.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     the state flags
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style ()
;;;
;;; void gtk_style_context_get_style (GtkStyleContext *context, ...);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_property ()
;;;
;;; void gtk_style_context_get_style_property (GtkStyleContext *context,
;;;                                            const gchar *property_name,
;;;                                            GValue *value);
;;;
;;; Gets the value for a widget style property.
;;;
;;; When value is no longer needed, g_value_unset() must be called to free any
;;; allocated memory.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; property_name :
;;;     the name of the widget style property
;;;
;;; value :
;;;     Return location for the property value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_valist ()
;;;
;;; void gtk_style_context_get_style_valist (GtkStyleContext *context,
;;;                                          va_list args);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_valist ()
;;;
;;; void gtk_style_context_get_valist (GtkStyleContext *context,
;;;                                    GtkStateFlags state,
;;;                                    va_list args);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
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
;;; gtk_style_context_get_section ()
;;;
;;; GtkCssSection * gtk_style_context_get_section (GtkStyleContext *context,
;;;                                                const gchar *property);
;;;
;;; Queries the location in the CSS where property was defined for the current
;;; context. Note that the state to be queried is taken from
;;; gtk_style_context_get_state().
;;;
;;; If the location is not available, NULL will be returned. The location might
;;; not be available for various reasons, such as the property being overridden,
;;; property not naming a supported CSS property or tracking of definitions
;;; being disabled for performance reasons.
;;;
;;; Shorthand CSS properties cannot be queried for a location and will always
;;; return NULL.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; property :
;;;     style property name
;;;
;;; Returns :
;;;     NULL or the section where value was defined
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_color ()
;;;
;;; void gtk_style_context_get_color (GtkStyleContext *context,
;;;                                   GtkStateFlags state,
;;;                                   GdkRGBA *color);
;;;
;;; Gets the foreground color for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the foreground color
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_background_color ()
;;;
;;; void gtk_style_context_get_background_color (GtkStyleContext *context,
;;;                                              GtkStateFlags state,
;;;                                              GdkRGBA *color);
;;;
;;; Gets the background color for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the background color
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border_color ()
;;;
;;; void gtk_style_context_get_border_color (GtkStyleContext *context,
;;;                                          GtkStateFlags state,
;;;                                          GdkRGBA *color);
;;;
;;; Gets the border color for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the color for
;;;
;;; color :
;;;     return value for the border color
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border ()
;;;
;;; void gtk_style_context_get_border (GtkStyleContext *context,
;;;                                    GtkStateFlags state,
;;;                                    GtkBorder *border);
;;;
;;; Gets the border for a given state as a GtkBorder. See
;;; GTK_STYLE_PROPERTY_BORDER_WIDTH.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the border for
;;;
;;; border :
;;;     return value for the border settings
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_padding ()
;;;
;;; void gtk_style_context_get_padding (GtkStyleContext *context,
;;;                                     GtkStateFlags state,
;;;                                     GtkBorder *padding);
;;;
;;; Gets the padding for a given state as a GtkBorder. See
;;; GTK_STYLE_PROPERTY_PADDING.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the padding for
;;;
;;; padding :
;;;     return value for the padding settings
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_margin ()
;;;
;;; void gtk_style_context_get_margin (GtkStyleContext *context,
;;;                                    GtkStateFlags state,
;;;                                    GtkBorder *margin);
;;;
;;; Gets the margin for a given state as a GtkBorder. See
;;; GTK_STYLE_PROPERTY_MARGIN.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the border for
;;;
;;; margin :
;;;     return value for the margin settings
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_font ()
;;;
;;; const PangoFontDescription * gtk_style_context_get_font
;;;                                                   (GtkStyleContext *context,
;;;                                                    GtkStateFlags state);
;;;
;;; Returns the font description for a given state. The returned object is const
;;; and will remain valid until the "changed" signal happens.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the font for
;;;
;;; Returns :
;;;     the PangoFontDescription for the given state. This object is owned by
;;;     GTK+ and should not be freed. [transfer none]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_invalidate ()
;;;
;;; void gtk_style_context_invalidate (GtkStyleContext *context);
;;;
;;; Invalidates context style information, so it will be reconstructed again.
;;;
;;; If you're using a GtkStyleContext returned from
;;; gtk_widget_get_style_context(), you do not need to call this yourself.
;;;
;;; context :
;;;     a GtkStyleContext.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_state_is_running ()
;;;
;;; gboolean gtk_style_context_state_is_running (GtkStyleContext *context,
;;;                                              GtkStateType state,
;;;                                              gdouble *progress);
;;;
;;; Returns TRUE if there is a transition animation running for the current
;;; region (see gtk_style_context_push_animatable_region()).
;;;
;;; If progress is not NULL, the animation progress will be returned there, 0.0
;;; means the state is closest to being unset, while 1.0 means it's closest to
;;; being set. This means transition animation will run from 0 to 1 when state
;;; is being set and from 1 to 0 when it's being unset.
;;;
;;; context :
;;;     a GtkStyleContext
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
;;; gtk_style_context_lookup_color ()
;;;
;;; gboolean gtk_style_context_lookup_color (GtkStyleContext *context,
;;;                                          const gchar *color_name,
;;;                                          GdkRGBA *color);
;;;
;;; Looks up and resolves a color name in the context color map.
;;;
;;; context :
;;;     a GtkStyleContext
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
;;; gtk_style_context_lookup_icon_set ()
;;;
;;; GtkIconSet * gtk_style_context_lookup_icon_set (GtkStyleContext *context,
;;;                                                 const gchar *stock_id);
;;;
;;; Looks up stock_id in the icon factories associated to context and the
;;; default icon factory, returning an icon set if found, otherwise NULL.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; stock_id :
;;;     an icon name
;;;
;;; Returns :
;;;     The looked up GtkIconSet, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_notify_state_change ()
;;;
;;; void gtk_style_context_notify_state_change (GtkStyleContext *context,
;;;                                             GdkWindow *window,
;;;                                             gpointer region_id,
;;;                                             GtkStateType state,
;;;                                             gboolean state_value);
;;;
;;; Notifies a state change on context, so if the current style makes use of
;;; transition animations, one will be started so all rendered elements under
;;; region_id are animated for state state being set to value state_value.
;;;
;;; The window parameter is used in order to invalidate the rendered area as the
;;; animation runs, so make sure it is the same window that is being rendered on
;;; by the gtk_render_*() functions.
;;;
;;; If region_id is NULL, all rendered elements using context will be affected
;;; by this state transition.
;;;
;;; As a practical example, a GtkButton notifying a state transition on the
;;; prelight state:
;;;
;;;   gtk_style_context_notify_state_change (context,
;;;                                          gtk_widget_get_window (widget),
;;;                                          NULL,
;;;                                          GTK_STATE_PRELIGHT,
;;;                                          button->in_button);
;;;
;;; Can be handled in the CSS file like this:
;;;
;;;   GtkButton {
;;;       background-color: #f00
;;;   }
;;;   
;;;   GtkButton:hover {
;;;       background-color: #fff;
;;;       transition: 200ms linear
;;;   }
;;;
;;; This combination will animate the button background from red to white if a
;;; pointer enters the button, and back to red if the pointer leaves the button.
;;;
;;; Note that state is used when finding the transition parameters, which is why
;;; the style places the transition under the :hover pseudo-class.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; region_id :
;;;     animatable region to notify on, or NULL. See
;;;     gtk_style_context_push_animatable_region().
;;;
;;; state :
;;;     state to trigger transition for
;;;
;;; state_value :
;;;     TRUE if state is the state we are changing to, FALSE if we are changing
;;;     away from it
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_pop_animatable_region ()
;;;
;;; void gtk_style_context_pop_animatable_region (GtkStyleContext *context);
;;;
;;; Pops an animatable region from context. See
;;; gtk_style_context_push_animatable_region().
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_push_animatable_region ()
;;;
;;; void gtk_style_context_push_animatable_region (GtkStyleContext *context,
;;;                                                gpointer region_id);
;;;
;;; Pushes an animatable region, so all further gtk_render_*() calls between
;;; this call and the following gtk_style_context_pop_animatable_region() will
;;; potentially show transition animations for this region if
;;; gtk_style_context_notify_state_change() is called for a given state, and the
;;; current theme/style defines transition animations for state changes.
;;;
;;; The region_id used must be unique in context so the theming engine can
;;; uniquely identify rendered elements subject to a state transition.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_id :
;;;     unique identifier for the animatable region
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_cancel_animations ()
;;;
;;; void gtk_style_context_cancel_animations (GtkStyleContext *context,
;;;                                           gpointer region_id);
;;;
;;; Stops all running animations for region_id and all animatable regions
;;; underneath.
;;;
;;; A NULL region_id will stop all ongoing animations in context, when dealing
;;; with a GtkStyleContext obtained through gtk_widget_get_style_context(), this
;;; is normally done for you in all circumstances you would expect all widget to
;;; be stopped, so this should be only used in complex widgets with different
;;; animatable regions.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_id :
;;;     animatable region to stop, or NULL. See
;;;     gtk_style_context_push_animatable_region().
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_scroll_animations ()
;;;
;;; void gtk_style_context_scroll_animations (GtkStyleContext *context,
;;;                                           GdkWindow *window,
;;;                                           gint dx,
;;;                                           gint dy);
;;;
;;; This function is analogous to gdk_window_scroll(), and should be called
;;; together with it so the invalidation areas for any ongoing animation are
;;; scrolled together with it.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; window :
;;;     a GdkWindow used previously in gtk_style_context_notify_state_change()
;;;
;;; dx :
;;;     Amount to scroll in the X axis
;;;
;;; dy :
;;;     Amount to scroll in the Y axis
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider ()
;;;
;;; void gtk_style_context_remove_provider (GtkStyleContext *context,
;;;                                         GtkStyleProvider *provider);
;;;
;;; Removes provider from the style providers list in context.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider_for_screen ()
;;;
;;; void gtk_style_context_remove_provider_for_screen
;;;                                                (GdkScreen *screen,
;;;                                                 GtkStyleProvider *provider);
;;;
;;; Removes provider from the global style providers list in screen.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_reset_widgets ()
;;;
;;; void gtk_style_context_reset_widgets (GdkScreen *screen);
;;;
;;; This function recomputes the styles for all widgets under a particular
;;; GdkScreen. This is useful when some global parameter has changed that
;;; affects the appearance of all widgets, because when a widget gets a new
;;; style, it will both redraw and recompute any cached information about its
;;; appearance. As an example, it is used when the color scheme changes in the
;;; related GtkSettings object.
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_background ()
;;;
;;; void gtk_style_context_set_background (GtkStyleContext *context,
;;;                                        GdkWindow *window);
;;;
;;; Sets the background of window to the background pattern or color specified
;;; in context for its current state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_restore ()
;;;
;;; void gtk_style_context_restore (GtkStyleContext *context);
;;;
;;; Restores context state to a previous stage. See gtk_style_context_save().
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save ()
;;;
;;; void gtk_style_context_save (GtkStyleContext *context);
;;;
;;; Saves the context state, so all modifications done through
;;; gtk_style_context_add_class(), gtk_style_context_remove_class(),
;;; gtk_style_context_add_region(), gtk_style_context_remove_region() or 
;;; gtk_style_context_set_junction_sides() can be reverted in one go through
;;; gtk_style_context_restore().
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_direction ()
;;;
;;; void gtk_style_context_set_direction (GtkStyleContext *context,
;;;                                       GtkTextDirection direction);
;;;
;;; Sets the reading direction for rendering purposes.
;;;
;;; If you are using a GtkStyleContext returned from
;;; gtk_widget_get_style_context(), you do not need to call this yourself.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; direction :
;;;     the new direction.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_junction_sides ()
;;;
;;; void gtk_style_context_set_junction_sides (GtkStyleContext *context,
;;;                                            GtkJunctionSides sides);
;;;
;;; Sets the sides where rendered elements (mostly through gtk_render_frame())
;;; will visually connect with other visual elements.
;;;
;;; This is merely a hint that may or may not be honored by theming engines.
;;;
;;; Container widgets are expected to set junction hints as appropriate for
;;; their children, so it should not normally be necessary to call this function
;;; manually.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; sides :
;;;     sides where rendered elements are visually connected to other elements
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_path ()
;;;
;;; void gtk_style_context_set_path (GtkStyleContext *context,
;;;                                  GtkWidgetPath *path);
;;;
;;; Sets the GtkWidgetPath used for style matching. As a consequence, the style
;;; will be regenerated to match the new given path.
;;;
;;; If you are using a GtkStyleContext returned from
;;; gtk_widget_get_style_context(), you do not need to call this yourself.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_class ()
;;;
;;; void gtk_style_context_add_class (GtkStyleContext *context,
;;;                                   const gchar *class_name);
;;;
;;; Adds a style class to context, so posterior calls to gtk_style_context_get()
;;; or any of the gtk_render_*() functions will make use of this new class for
;;; styling.
;;;
;;; In the CSS file format, a GtkEntry defining an "entry" class, would be
;;; matched by:
;;;
;;; GtkEntry.entry { ... }
;;;
;;; While any widget defining an "entry" class would be matched by:
;;;
;;; .entry { ... }
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; class_name :
;;;     class name to use in styling
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class ()
;;;
;;; void gtk_style_context_remove_class (GtkStyleContext *context,
;;;                                      const gchar *class_name);
;;;
;;; Removes class_name from context.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; class_name :
;;;     class name to remove
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class ()
;;;
;;; gboolean gtk_style_context_has_class (GtkStyleContext *context,
;;;                                       const gchar *class_name);
;;;
;;; Returns TRUE if context currently has defined the given class name
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; class_name :
;;;     a class name
;;;
;;; Returns :
;;;     TRUE if context has class_name defined
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_classes ()
;;;
;;; GList * gtk_style_context_list_classes (GtkStyleContext *context);
;;;
;;; Returns the list of classes currently defined in context.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     a GList of strings with the currently defined classes. The contents of
;;;     the list are owned by GTK+, but you must free the list itself with
;;;     g_list_free() when you are done with it.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_region ()
;;;
;;; void gtk_style_context_add_region (GtkStyleContext *context,
;;;                                    const gchar *region_name,
;;;                                    GtkRegionFlags flags);
;;;
;;; Adds a region to context, so posterior calls to gtk_style_context_get() or
;;; any of the gtk_render_*() functions will make use of this new region for
;;; styling.
;;;
;;; In the CSS file format, a GtkTreeView defining a "row" region, would be
;;; matched by:
;;;
;;; GtkTreeView row { ... }
;;;
;;; Pseudo-classes are used for matching flags, so the two following rules:
;;;
;;; GtkTreeView row:nth-child(even) { ... }
;;; GtkTreeView row:nth-child(odd) { ... }
;;;
;;; would apply to even and odd rows, respectively.
;;;
;;; Note
;;;
;;; Region names must only contain lowercase letters and '-', starting always
;;; with a lowercase letter.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_name :
;;;     region name to use in styling
;;;
;;; flags :
;;;     flags that apply to the region
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_region ()
;;;
;;; void gtk_style_context_remove_region (GtkStyleContext *context,
;;;                                       const gchar *region_name);
;;;
;;; Removes a region from context.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_name :
;;;     region name to unset
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_region ()
;;;
;;; gboolean gtk_style_context_has_region (GtkStyleContext *context,
;;;                                        const gchar *region_name,
;;;                                        GtkRegionFlags *flags_return);
;;;
;;; Returns TRUE if context has the region defined. If flags_return is not NULL,
;;; it is set to the flags affecting the region.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_name :
;;;     a region name
;;;
;;; flags_return :
;;;     return location for region flags
;;;
;;; Returns :
;;;     TRUE if region is defined
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_regions ()
;;;
;;; GList * gtk_style_context_list_regions (GtkStyleContext *context);
;;;
;;; Returns the list of regions currently defined in context.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     a GList of strings with the currently defined regions. The contents of
;;;     the list are owned by GTK+, but you must free the list itself with
;;;     g_list_free() when you are done with it.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_screen ()
;;;
;;; void gtk_style_context_set_screen (GtkStyleContext *context,
;;;                                    GdkScreen *screen);
;;;
;;; Attaches context to the given screen.
;;;
;;; The screen is used to add style information from 'global' style providers,
;;; such as the screens GtkSettings instance.
;;;
;;; If you are using a GtkStyleContext returned from
;;; gtk_widget_get_style_context(), you do not need to call this yourself.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_state ()
;;;
;;; void gtk_style_context_set_state (GtkStyleContext *context,
;;;                                   GtkStateFlags flags);
;;;
;;; Sets the state to be used when rendering with any of the gtk_render_*()
;;; functions.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; flags :
;;;     state to represent
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkBorder
;;;
;;; struct GtkBorder {
;;;   gint16 left;
;;;   gint16 right;
;;;   gint16 top;
;;;   gint16 bottom;
;;; };
;;;
;;; A struct that specifies a border around a rectangular area that can be of
;;; different width on each side.
;;;
;;; gint16 left;
;;;     The width of the left border
;;;
;;; gint16 right;
;;;     The width of the right border
;;;
;;; gint16 top;
;;;     The width of the top border
;;;
;;; gint16 bottom;
;;;     The width of the bottom border
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-border "GtkBorder"
  (left   :int16 :initform 0)
  (right  :int16 :initform 0)
  (top    :int16 :initform 0)
  (bottom :int16 :initform 0))

(at-init () (foreign-funcall "gtk_border_get_type" :int))
(at-init () (foreign-funcall "gtk_ui_manager_get_type" :int))

(export (boxed-related-symbols 'gtk-border))

;;; ----------------------------------------------------------------------------
;;; gtk_border_new ()
;;;
;;; GtkBorder * gtk_border_new (void);
;;;
;;; Allocates a new GtkBorder structure and initializes its elements to zero.
;;;
;;; Returns :
;;;     a newly allocated GtkBorder. Free with gtk_border_free()
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_border_copy ()
;;;
;;; GtkBorder * gtk_border_copy (const GtkBorder *border_);
;;;
;;; Copies a GtkBorder structure.
;;;
;;; border_ :
;;;     a GtkBorder
;;;
;;; Returns :
;;;     a copy of border_.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_border_free ()
;;;
;;; void gtk_border_free (GtkBorder *border_);
;;;
;;; Frees a GtkBorder structure.
;;;
;;; border_ :
;;;     a GtkBorder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_arrow ()
;;;
;;; void gtk_render_arrow (GtkStyleContext *context,
;;;                        cairo_t *cr,
;;;                        gdouble angle,
;;;                        gdouble x,
;;;                        gdouble y,
;;;                        gdouble size);
;;;
;;; Renders an arrow pointing to angle.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; angle :
;;;     arrow angle from 0 to 2 * G_PI, being 0 the arrow pointing to the north
;;;
;;; x :
;;;     X origin of the render area
;;;
;;; y :
;;;     Y origin of the render area
;;;
;;; size :
;;;     square side for render area
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_background ()
;;;
;;; void gtk_render_background (GtkStyleContext *context,
;;;                             cairo_t *cr,
;;;                             gdouble x,
;;;                             gdouble y,
;;;                             gdouble width,
;;;                             gdouble height);
;;;
;;; Renders the background of an element.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_check ()
;;;
;;; void gtk_render_check (GtkStyleContext *context,
;;;                        cairo_t *cr,
;;;                        gdouble x,
;;;                        gdouble y,
;;;                        gdouble width,
;;;                        gdouble height);
;;;
;;; Renders a checkmark (as in a GtkCheckButton).
;;;
;;; The GTK_STATE_FLAG_ACTIVE state determines whether the check is on or off,
;;; and GTK_STATE_FLAG_INCONSISTENT determines whether it should be marked as
;;; undefined.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander ()
;;;
;;; void gtk_render_expander (GtkStyleContext *context,
;;;                           cairo_t *cr,
;;;                           gdouble x,
;;;                           gdouble y,
;;;                           gdouble width,
;;;                           gdouble height);
;;;
;;; Renders an expander (as used in GtkTreeView and GtkExpander) in the area
;;; defined by x, y, width, height. The state GTK_STATE_FLAG_ACTIVE determines
;;; whether the expander is collapsed or expanded.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_extension ()
;;;
;;; void gtk_render_extension (GtkStyleContext *context,
;;;                            cairo_t *cr,
;;;                            gdouble x,
;;;                            gdouble y,
;;;                            gdouble width,
;;;                            gdouble height,
;;;                            GtkPositionType gap_side);
;;;
;;; Renders a extension (as in a GtkNotebook tab) in the rectangle defined by x,
;;; y, width, height. The side where the extension connects to is defined by
;;; gap_side.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; gap_side :
;;;     side where the gap is
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus ()
;;;
;;; void gtk_render_focus (GtkStyleContext *context,
;;;                        cairo_t *cr,
;;;                        gdouble x,
;;;                        gdouble y,
;;;                        gdouble width,
;;;                        gdouble height);
;;;
;;; Renders a focus indicator on the rectangle determined by x, y, width,
;;; height.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame ()
;;;
;;; void gtk_render_frame (GtkStyleContext *context,
;;;                        cairo_t *cr,
;;;                        gdouble x,
;;;                        gdouble y,
;;;                        gdouble width,
;;;                        gdouble height);
;;;
;;; Renders a frame around the rectangle defined by x, y, width, height.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame_gap ()
;;;
;;; void gtk_render_frame_gap (GtkStyleContext *context,
;;;                            cairo_t *cr,
;;;                            gdouble x,
;;;                            gdouble y,
;;;                            gdouble width,
;;;                            gdouble height,
;;;                            GtkPositionType gap_side,
;;;                            gdouble xy0_gap,
;;;                            gdouble xy1_gap);
;;;
;;; Renders a frame around the rectangle defined by (x, y, width, height),
;;; leaving a gap on one side. xy0_gap and xy1_gap will mean X coordinates for
;;; GTK_POS_TOP and GTK_POS_BOTTOM gap sides, and Y coordinates for GTK_POS_LEFT
;;; and GTK_POS_RIGHT.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; gap_side :
;;;     side where the gap is
;;;
;;; xy0_gap :
;;;     initial coordinate (X or Y depending on gap_side) for the gap
;;;
;;; xy1_gap :
;;;     end coordinate (X or Y depending on gap_side) for the gap
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle ()
;;;
;;; void gtk_render_handle (GtkStyleContext *context,
;;;                         cairo_t *cr,
;;;                         gdouble x,
;;;                         gdouble y,
;;;                         gdouble width,
;;;                         gdouble height);
;;;
;;; Renders a handle (as in GtkHandleBox, GtkPaned and GtkWindow's resize grip),
;;; in the rectangle determined by x, y, width, height.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout ()
;;;
;;; void gtk_render_layout (GtkStyleContext *context,
;;;                         cairo_t *cr,
;;;                         gdouble x,
;;;                         gdouble y,
;;;                         PangoLayout *layout);
;;;
;;; Renders layout on the coordinates x, y
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin
;;;
;;; y :
;;;     Y origin
;;;
;;; layout :
;;;     the PangoLayout to render
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_line ()
;;;
;;; void gtk_render_line (GtkStyleContext *context,
;;;                       cairo_t *cr,
;;;                       gdouble x0,
;;;                       gdouble y0,
;;;                       gdouble x1,
;;;                       gdouble y1);
;;;
;;; Renders a line from (x0, y0) to (x1, y1).
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x0 :
;;;     X coordinate for the origin of the line
;;;
;;; y0 :
;;;     Y coordinate for the origin of the line
;;;
;;; x1 :
;;;     X coordinate for the end of the line
;;;
;;; y1 :
;;;     Y coordinate for the end of the line
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_option ()
;;;
;;; void gtk_render_option (GtkStyleContext *context,
;;;                         cairo_t *cr,
;;;                         gdouble x,
;;;                         gdouble y,
;;;                         gdouble width,
;;;                         gdouble height);
;;;
;;; Renders an option mark (as in a GtkRadioButton), the GTK_STATE_FLAG_ACTIVE
;;; state will determine whether the option is on or off, and
;;; GTK_STATE_FLAG_INCONSISTENT whether it should be marked as undefined.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_slider ()
;;;
;;; void gtk_render_slider (GtkStyleContext *context,
;;;                         cairo_t *cr,
;;;                         gdouble x,
;;;                         gdouble y,
;;;                         gdouble width,
;;;                         gdouble height,
;;;                         GtkOrientation orientation);
;;;
;;; Renders a slider (as in GtkScale) in the rectangle defined by x, y, width,
;;; height. orientation defines whether the slider is vertical or horizontal.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; orientation :
;;;     orientation of the slider
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity ()
;;;
;;; void gtk_render_activity (GtkStyleContext *context,
;;;                           cairo_t *cr,
;;;                           gdouble x,
;;;                           gdouble y,
;;;                           gdouble width,
;;;                           gdouble height);
;;;
;;; Renders an activity area (Such as in GtkSpinner or the fill line in
;;; GtkRange), the state GTK_STATE_FLAG_ACTIVE determines whether there is
;;; activity going on.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin of the rectangle
;;;
;;; y :
;;;     Y origin of the rectangle
;;;
;;; width :
;;;     rectangle width
;;;
;;; height :
;;;     rectangle height
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_pixbuf ()
;;;
;;; GdkPixbuf * gtk_render_icon_pixbuf (GtkStyleContext *context,
;;;                                     const GtkIconSource *source,
;;;                                     GtkIconSize size);
;;;
;;; Renders the icon specified by source at the given size, returning the result
;;; in a pixbuf.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; source :
;;;     the GtkIconSource specifying the icon to render
;;;
;;; size :
;;;     the size to render the icon at. A size of (GtkIconSize) -1 means render
;;;     at the size of the source and don't scale
;;;
;;; Returns :
;;;     a newly-created GdkPixbuf containing the rendered icon
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon ()
;;;
;;; void gtk_render_icon (GtkStyleContext *context,
;;;                       cairo_t *cr,
;;;                       GdkPixbuf *pixbuf,
;;;                       gdouble x,
;;;                       gdouble y);
;;;
;;; Renders the icon in pixbuf at the specified x and y coordinates.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; pixbuf :
;;;     a GdkPixbuf containing the icon to draw
;;;
;;; x :
;;;     X position for the pixbuf
;;;
;;; y :
;;;     Y position for the pixbuf
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_insertion_cursor ()
;;;
;;; void gtk_render_insertion_cursor (GtkStyleContext *context,
;;;                                   cairo_t *cr,
;;;                                   gdouble x,
;;;                                   gdouble y,
;;;                                   PangoLayout *layout,
;;;                                   int index,
;;;                                   PangoDirection direction);
;;;
;;; Draws a text caret on cr at the specified index of layout.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cr :
;;;     a cairo_t
;;;
;;; x :
;;;     X origin
;;;
;;; y :
;;;     Y origin
;;;
;;; layout :
;;;     the PangoLayout of the text
;;;
;;; index :
;;;     the index in the PangoLayout
;;;
;;; direction :
;;;     the PangoDirection of the text
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.style-context.lisp -------------------------------------
