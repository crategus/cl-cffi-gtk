;;; ----------------------------------------------------------------------------
;;; gtk.widget.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkWidget
;;; 
;;; Base class for all widgets
;;; 
;;; Synopsis
;;; 
;;;     GtkRequisition
;;;     GtkAllocation
;;;     GtkWidgetFlags  from Gtk+ 2 Reference Manual
;;;     GtkWidget
;;;     GtkWidgetClass
;;;     GtkSelectionData
;;;     GtkWidgetAuxInfo
;;;     GtkWidgetHelpType
;;;
;;;     gtk_widget_new
;;;     gtk_widget_destroy
;;;     gtk_widget_in_destruction
;;;     gtk_widget_destroyed
;;;     gtk_widget_unparent
;;;     gtk_widget_show
;;;     gtk_widget_show_now
;;;     gtk_widget_hide
;;;     gtk_widget_show_all
;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_draw
;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_resize_no_redraw
;;;     gtk_widget_size_request
;;;     gtk_widget_get_child_requisition
;;;     gtk_widget_size_allocate
;;;     gtk_widget_add_accelerator
;;;     gtk_widget_remove_accelerator
;;;     gtk_widget_set_accel_path
;;;     gtk_widget_list_accel_closures
;;;     gtk_widget_can_activate_accel
;;;     gtk_widget_event
;;;     gtk_widget_activate
;;;     gtk_widget_reparent
;;;     gtk_widget_intersect
;;;     gtk_widget_is_focus
;;;     gtk_widget_grab_focus
;;;     gtk_widget_grab_default
;;;     gtk_widget_set_name
;;;     gtk_widget_get_name
;;;     gtk_widget_set_state
;;;     gtk_widget_set_sensitive
;;;     gtk_widget_set_parent
;;;     gtk_widget_set_parent_window
;;;     gtk_widget_get_parent_window
;;;     gtk_widget_set_events
;;;     gtk_widget_get_events
;;;     gtk_widget_add_events
;;;     gtk_widget_set_device_events
;;;     gtk_widget_get_device_events
;;;     gtk_widget_add_device_events
;;;     gtk_widget_set_device_enabled
;;;     gtk_widget_get_device_enabled
;;;     gtk_widget_get_toplevel
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_get_visual
;;;     gtk_widget_set_visual
;;;     gtk_widget_get_pointer
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates
;;;     gtk_widget_hide_on_delete
;;;     gtk_widget_set_style
;;;     gtk_widget_ensure_style
;;;     gtk_widget_get_style
;;;     gtk_widget_reset_rc_styles
;;;     gtk_widget_get_default_style
;;;     gtk_widget_set_direction
;;;
;;;     GtkTextDirection
;;;
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_shape_combine_region
;;;     gtk_widget_input_shape_combine_region
;;;     gtk_widget_path
;;;     gtk_widget_class_path
;;;     gtk_widget_get_composite_name
;;;     gtk_widget_override_background_color
;;;     gtk_widget_override_color
;;;     gtk_widget_override_font
;;;     gtk_widget_override_symbolic_color
;;;     gtk_widget_override_cursor
;;;     gtk_widget_modify_style
;;;     gtk_widget_get_modifier_style
;;;     gtk_widget_modify_fg
;;;     gtk_widget_modify_bg
;;;     gtk_widget_modify_text
;;;     gtk_widget_modify_base
;;;     gtk_widget_modify_font
;;;     gtk_widget_modify_cursor
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_render_icon
;;;     gtk_widget_render_icon_pixbuf
;;;     gtk_widget_pop_composite_child
;;;     gtk_widget_push_composite_child
;;;     gtk_widget_queue_draw_area
;;;     gtk_widget_queue_draw_region
;;;     gtk_widget_set_app_paintable
;;;     gtk_widget_set_double_buffered
;;;     gtk_widget_set_redraw_on_allocate
;;;     gtk_widget_set_composite_name
;;;     gtk_widget_mnemonic_activate
;;;     gtk_widget_class_install_style_property
;;;     gtk_widget_class_install_style_property_parser
;;;     gtk_widget_class_find_style_property
;;;     gtk_widget_class_list_style_properties
;;;     gtk_widget_region_intersect
;;;     gtk_widget_send_expose
;;;     gtk_widget_send_focus_change
;;;     gtk_widget_style_get
;;;     gtk_widget_style_get_property
;;;     gtk_widget_style_get_valist
;;;     gtk_widget_style_attach
;;;     gtk_widget_class_set_accessible_type
;;;     gtk_widget_class_set_accessible_role
;;;     gtk_widget_get_accessible
;;;     gtk_widget_child_focus
;;;     gtk_widget_child_notify
;;;     gtk_widget_freeze_child_notify
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_get_parent
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_root_window
;;;     gtk_widget_get_screen
;;;     gtk_widget_has_screen
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_set_size_request
;;;     gtk_widget_thaw_child_notify
;;;     gtk_widget_set_no_show_all
;;;     gtk_widget_get_no_show_all
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_is_composited
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_get_tooltip_markup
;;;     gtk_widget_set_tooltip_markup
;;;     gtk_widget_get_tooltip_text
;;;     gtk_widget_set_tooltip_text
;;;     gtk_widget_get_tooltip_window
;;;     gtk_widget_set_tooltip_window
;;;     gtk_widget_get_has_tooltip
;;;     gtk_widget_set_has_tooltip
;;;     gtk_widget_trigger_tooltip_query
;;;     gtk_widget_get_window
;;;     gtk_cairo_should_draw_window
;;;     gtk_cairo_transform_to_window
;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_set_allocation
;;;     gtk_widget_get_app_paintable
;;;     gtk_widget_get_can_default
;;;     gtk_widget_set_can_default
;;;     gtk_widget_get_can_focus
;;;     gtk_widget_set_can_focus
;;;     gtk_widget_get_double_buffered
;;;     gtk_widget_get_has_window
;;;     gtk_widget_set_has_window
;;;     gtk_widget_get_sensitive
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_get_state
;;;     gtk_widget_get_visible
;;;     gtk_widget_set_visible
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_has_default
;;;     gtk_widget_has_focus
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_has_grab
;;;     gtk_widget_has_rc_style
;;;     gtk_widget_is_drawable
;;;     gtk_widget_is_toplevel
;;;     gtk_widget_set_window
;;;     gtk_widget_set_receives_default
;;;     gtk_widget_get_receives_default
;;;     gtk_widget_set_support_multidevice
;;;     gtk_widget_get_support_multidevice
;;;     gtk_widget_set_realized
;;;     gtk_widget_get_realized
;;;     gtk_widget_set_mapped
;;;     gtk_widget_get_mapped
;;;     gtk_widget_get_requisition
;;;     gtk_widget_device_is_shadowed     
;;;     gtk_widget_get_path
;;;     gtk_widget_get_style_context
;;;     gtk_widget_reset_style
;;;     
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free
;;;     
;;;     GtkSizeRequestMode
;;;     GtkRequestedSize
;;;
;;;     gtk_widget_get_preferred_height
;;;     gtk_widget_get_preferred_width
;;;     gtk_widget_get_preferred_height_for_width
;;;     gtk_widget_get_preferred_width_for_height
;;;     gtk_widget_get_request_mode
;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation
;;;     
;;;     GtkAlign
;;;
;;;     gtk_widget_get_halign
;;;     gtk_widget_set_halign
;;;     gtk_widget_get_valign
;;;     gtk_widget_set_valign
;;;     gtk_widget_get_margin_left
;;;     gtk_widget_set_margin_left
;;;     gtk_widget_get_margin_right
;;;     gtk_widget_set_margin_right
;;;     gtk_widget_get_margin_top
;;;     gtk_widget_set_margin_top
;;;     gtk_widget_get_margin_bottom
;;;     gtk_widget_set_margin_bottom
;;;     
;;;     gtk_widget_get_hexpand
;;;     gtk_widget_set_hexpand
;;;     gtk_widget_get_hexpand_set
;;;     gtk_widget_set_hexpand_set
;;;     gtk_widget_get_vexpand
;;;     gtk_widget_set_vexpand
;;;     gtk_widget_get_vexpand_set
;;;     gtk_widget_set_vexpand_set
;;;     gtk_widget_queue_compute_expand
;;;     gtk_widget_compute_expand
;;;
;;;     GTK_WIDGET_SET_FLAGS()       from GTK+ 2 Reference Manual
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                +----GtkMisc
;;;                +----GtkCalendar
;;;                +----GtkCellView
;;;                +----GtkDrawingArea
;;;                +----GtkEntry
;;;                +----GtkRange
;;;                +----GtkSeparator
;;;                +----GtkHSV
;;;                +----GtkInvisible
;;;                +----GtkProgressBar
;;;                +----GtkSpinner
;;;                +----GtkSwitch
;;; 
;;;   GBoxed
;;;    +----GtkRequisition
;;; 
;;;   GBoxed
;;;    +----GtkSelectionData
;;; 
;;; Known Derived Interfaces
;;; 
;;; GtkWidget is required by GtkAppChooser, GtkCellEditable, GtkFileChooser and
;;; GtkToolShell.
;;;
;;; Implemented Interfaces
;;; 
;;; GtkWidget implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "app-paintable"            gboolean              : Read / Write
;;;   "can-default"              gboolean              : Read / Write
;;;   "can-focus"                gboolean              : Read / Write
;;;   "composite-child"          gboolean              : Read
;;;   "double-buffered"          gboolean              : Read / Write
;;;   "events"                   GdkEventMask          : Read / Write
;;;   "expand"                   gboolean              : Read / Write
;;;   "halign"                   GtkAlign              : Read / Write
;;;   "has-default"              gboolean              : Read / Write
;;;   "has-focus"                gboolean              : Read / Write
;;;   "has-tooltip"              gboolean              : Read / Write
;;;   "height-request"           gint                  : Read / Write
;;;   "hexpand"                  gboolean              : Read / Write
;;;   "hexpand-set"              gboolean              : Read / Write
;;;   "is-focus"                 gboolean              : Read / Write
;;;   "margin"                   gint                  : Read / Write
;;;   "margin-bottom"            gint                  : Read / Write
;;;   "margin-left"              gint                  : Read / Write
;;;   "margin-right"             gint                  : Read / Write
;;;   "margin-top"               gint                  : Read / Write
;;;   "name"                     gchar*                : Read / Write
;;;   "no-show-all"              gboolean              : Read / Write
;;;   "parent"                   GtkContainer*         : Read / Write
;;;   "receives-default"         gboolean              : Read / Write
;;;   "sensitive"                gboolean              : Read / Write
;;;   "style"                    GtkStyle*             : Read / Write
;;;   "tooltip-markup"           gchar*                : Read / Write
;;;   "tooltip-text"             gchar*                : Read / Write
;;;   "valign"                   GtkAlign              : Read / Write
;;;   "vexpand"                  gboolean              : Read / Write
;;;   "vexpand-set"              gboolean              : Read / Write
;;;   "visible"                  gboolean              : Read / Write
;;;   "width-request"            gint                  : Read / Write
;;;   "window"                   GdkWindow*            : Read
;;; 
;;; Style Properties
;;; 
;;;   "cursor-aspect-ratio"      gfloat                : Read
;;;   "cursor-color"             GdkColor*             : Read
;;;   "focus-line-pattern"       gchar*                : Read
;;;   "focus-line-width"         gint                  : Read
;;;   "focus-padding"            gint                  : Read
;;;   "interior-focus"           gboolean              : Read
;;;   "link-color"               GdkColor*             : Read
;;;   "scroll-arrow-hlength"     gint                  : Read
;;;   "scroll-arrow-vlength"     gint                  : Read
;;;   "secondary-cursor-color"   GdkColor*             : Read
;;;   "separator-height"         gint                  : Read
;;;   "separator-width"          gint                  : Read
;;;   "visited-link-color"       GdkColor*             : Read
;;;   "wide-separators"          gboolean              : Read
;;;   "window-dragging"          gboolean              : Read
;;; 
;;; Signals
;;; 
;;;   "accel-closures-changed"                         
;;;   "button-press-event"                             : Run Last
;;;   "button-release-event"                           : Run Last
;;;   "can-activate-accel"                             : Run Last
;;;   "child-notify"                                   : No Hooks
;;;   "composited-changed"                             : Action
;;;   "configure-event"                                : Run Last
;;;   "damage-event"                                   : Run Last
;;;   "delete-event"                                   : Run Last
;;;   "destroy"                                        : No Hooks
;;;   "destroy-event"                                  : Run Last
;;;   "direction-changed"                              : Run First
;;;   "drag-begin"                                     : Run Last
;;;   "drag-data-delete"                               : Run Last
;;;   "drag-data-get"                                  : Run Last
;;;   "drag-data-received"                             : Run Last
;;;   "drag-drop"                                      : Run Last
;;;   "drag-end"                                       : Run Last
;;;   "drag-failed"                                    : Run Last
;;;   "drag-leave"                                     : Run Last
;;;   "drag-motion"                                    : Run Last
;;;   "draw"                                           : Run Last
;;;   "enter-notify-event"                             : Run Last
;;;   "event"                                          : Run Last
;;;   "event-after"                                    
;;;   "focus"                                          : Run Last
;;;   "focus-in-event"                                 : Run Last
;;;   "focus-out-event"                                : Run Last
;;;   "grab-broken-event"                              : Run Last
;;;   "grab-focus"                                     : Action
;;;   "grab-notify"                                    : Run First
;;;   "hide"                                           : Run First
;;;   "hierarchy-changed"                              : Run Last
;;;   "key-press-event"                                : Run Last
;;;   "key-release-event"                              : Run Last
;;;   "keynav-failed"                                  : Run Last
;;;   "leave-notify-event"                             : Run Last
;;;   "map"                                            : Run First
;;;   "map-event"                                      : Run Last
;;;   "mnemonic-activate"                              : Run Last
;;;   "motion-notify-event"                            : Run Last
;;;   "move-focus"                                     : Action
;;;   "parent-set"                                     : Run First
;;;   "popup-menu"                                     : Action
;;;   "property-notify-event"                          : Run Last
;;;   "proximity-in-event"                             : Run Last
;;;   "proximity-out-event"                            : Run Last
;;;   "query-tooltip"                                  : Run Last
;;;   "realize"                                        : Run First
;;;   "screen-changed"                                 : Run Last
;;;   "scroll-event"                                   : Run Last
;;;   "selection-clear-event"                          : Run Last
;;;   "selection-get"                                  : Run Last
;;;   "selection-notify-event"                         : Run Last
;;;   "selection-received"                             : Run Last
;;;   "selection-request-event"                        : Run Last
;;;   "show"                                           : Run First
;;;   "show-help"                                      : Action
;;;   "size-allocate"                                  : Run First
;;;   "state-changed"                                  : Run First
;;;   "state-flags-changed"                            : Run First
;;;   "style-set"                                      : Run First
;;;   "style-updated"                                  : Run First
;;;   "unmap"                                          : Run First
;;;   "unmap-event"                                    : Run Last
;;;   "unrealize"                                      : Run Last
;;;   "visibility-notify-event"                        : Run Last
;;;   "window-state-event"                             : Run Last
;;; 
;;; Description
;;; 
;;; GtkWidget is the base class all widgets in GTK+ derive from. It manages the
;;; widget lifecycle, states and style.
;;; 
;;; Height-for-width Geometry Management
;;; 
;;; GTK+ uses a height-for-width (and width-for-height) geometry management
;;; system. Height-for-width means that a widget can change how much vertical
;;; space it needs, depending on the amount of horizontal space that it is
;;; given (and similar for width-for-height). The most common example is a
;;; label that reflows to fill up the available width, wraps to fewer lines,
;;; and therefore needs less height.
;;; 
;;; Height-for-width geometry management is implemented in GTK+ by way of five
;;; virtual methods:
;;; 
;;;     * GtkWidgetClass.get_request_mode()
;;;     * GtkWidgetClass.get_preferred_width()
;;;     * GtkWidgetClass.get_preferred_height()
;;;     * GtkWidgetClass.get_preferred_height_for_width()
;;;     * GtkWidgetClass.get_preferred_width_for_height()
;;; 
;;; There are some important things to keep in mind when implementing
;;; height-for-width and when using it in container implementations.
;;; 
;;; The geometry management system will query a widget hierarchy in only one
;;; orientation at a time. When widgets are initially queried for their minimum
;;; sizes it is generally done in two initial passes in the GtkSizeRequestMode
;;; chosen by the toplevel.
;;; 
;;; For example, when queried in the normal GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH
;;; mode: First, the default minimum and natural width for each widget in the
;;; interface will be computed using gtk_widget_get_preferred_width(). Because
;;; the preferred widths for each container depend on the preferred widths of
;;; their children, this information propagates up the hierarchy, and finally a
;;; minimum and natural width is determined for the entire toplevel. Next, the
;;; toplevel will use the minimum width to query for the minimum height
;;; contextual to that width using gtk_widget_get_preferred_height_for_width(),
;;; which will also be a highly recursive operation. The minimum height for the
;;; minimum width is normally used to set the minimum size constraint on the
;;; toplevel (unless gtk_window_set_geometry_hints() is explicitly used
;;; instead).
;;; 
;;; After the toplevel window has initially requested its size in both
;;; dimensions it can go on to allocate itself a reasonable size (or a size
;;; previously specified with gtk_window_set_default_size()). During the
;;; recursive allocation process it's important to note that request cycles will
;;; be recursively executed while container widgets allocate their children.
;;; Each container widget, once allocated a size, will go on to first share the
;;; space in one orientation among its children and then request each child's
;;; height for its target allocated width or its width for allocated height,
;;; depending. In this way a GtkWidget will typically be requested its size a
;;; number of times before actually being allocated a size. The size a widget
;;; is finally allocated can of course differ from the size it has requested.
;;; For this reason, GtkWidget caches a small number of results to avoid
;;; re-querying for the same sizes in one allocation cycle.
;;; 
;;; See GtkContainer's geometry management section to learn more about how
;;; height-for-width allocations are performed by container widgets.
;;; 
;;; If a widget does move content around to intelligently use up the allocated
;;; size then it must support the request in both GtkSizeRequestModes even if
;;; the widget in question only trades sizes in a single orientation.
;;; 
;;; For instance, a GtkLabel that does height-for-width word wrapping will not
;;; expect to have GtkWidgetClass.get_preferred_height() called because that
;;; call is specific to a width-for-height request. In this case the label must
;;; return the height required for its own minimum possible width. By following
;;; this rule any widget that handles height-for-width or width-for-height
;;; requests will always be allocated at least enough space to fit its own
;;; content.
;;; 
;;; Here are some examples of how a GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH widget
;;; generally deals with width-for-height requests, for
;;; GtkWidgetClass.get_preferred_height() it will do:
;;; 
;;; static void
;;; foo_widget_get_preferred_height (GtkWidget *widget, gint *min_height, gint
;;;                                  *nat_height)
;;; {
;;;    if (i_am_in_height_for_width_mode)
;;;      {
;;;        gint min_width;
;;; 
;;;        GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
;;;                                                            &min_width,
;;;                                                            NULL);
;;;        GTK_WIDGET_GET_CLASS (widget)->get_preferred_height_for_width
;;;                                 (widget, min_width, min_height, nat_height);
;;;      }
;;;    else
;;;      {
;;;         ... some widgets do both. For instance, if a GtkLabel is rotated to
;;;             90 degrees
;;;         it will return the minimum and natural height for the rotated label
;;;         here.
;;;      }
;;; }
;;; 
;;; And in GtkWidgetClass.get_preferred_width_for_height() it will simply
;;; return the minimum and natural width:
;;; 
;;; static void
;;; foo_widget_get_preferred_width_for_height (GtkWidget *widget,
;;;                                            gint for_height,
;;;                                            gint *min_width, gint *nat_width)
;;; {
;;;    if (i_am_in_height_for_width_mode)
;;;      {
;;;        GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
;;;                                                            min_width,
;;;                                                            nat_width);
;;;      }
;;;    else
;;;      {
;;;         ... again if a widget is sometimes operating in width-for-height
;;;         mode (like a rotated GtkLabel) it can go ahead and do its real
;;;         width for height calculation here.
;;;      }
;;; }
;;; 
;;; Often a widget needs to get its own request during size request or
;;; allocation. For example, when computing height it may need to also compute
;;; width. Or when deciding how to use an allocation, the widget may need to
;;; know its natural size. In these cases, the widget should be careful to call
;;; its virtual methods directly, like this:
;;; 
;;; Example 101. Widget calling its own size request method.
;;; 
;;;  GTK_WIDGET_GET_CLASS(widget)->get_preferred_width (widget),
;;;                                   &min, &natural);
;;; 
;;; It will not work to use the wrapper functions, such as
;;; gtk_widget_get_preferred_width() inside your own size request
;;; implementation. These return a request adjusted by GtkSizeGroup and by the
;;; GtkWidgetClass.adjust_size_request() virtual method. If a widget used the
;;; wrappers inside its virtual method implementations, then the adjustments
;;; (such as widget margins) would be applied twice. GTK+ therefore does not
;;; allow this and will warn if you try to do it.
;;; 
;;; Of course if you are getting the size request for another widget, such as a
;;; child of a container, you must use the wrapper APIs. Otherwise, you would
;;; not properly consider widget margins, GtkSizeGroup, and so forth.
;;; 
;;; Style Properties
;;; 
;;; GtkWidget introduces style properties - these are basically object
;;; properties that are stored not on the object, but in the style object
;;; associated to the widget. Style properties are set in resource files. This
;;; mechanism is used for configuring such things as the location of the
;;; scrollbar arrows through the theme, giving theme authors more control over
;;; the look of applications without the need to write a theme engine in C.
;;; 
;;; Use gtk_widget_class_install_style_property() to install style properties
;;; for a widget class, gtk_widget_class_find_style_property() or
;;; gtk_widget_class_list_style_properties() to get information about existing
;;; style properties and gtk_widget_style_get_property(), gtk_widget_style_get()
;;; or gtk_widget_style_get_valist() to obtain the value of a style property.
;;; 
;;; GtkWidget as GtkBuildable
;;; 
;;; The GtkWidget implementation of the GtkBuildable interface supports a custom
;;; <accelerator> element, which has attributes named key, modifiers and signal
;;; and allows to specify accelerators.
;;; 
;;; Example 102. A UI definition fragment specifying an accelerator
;;; 
;;;  <object class="GtkButton">
;;;    <accelerator key="q" modifiers="GDK_CONTROL_MASK" signal="clicked"/>
;;;  </object>
;;; 
;;; In addition to accelerators, GtkWidget also support a custom <accessible>
;;; element, which supports actions and relations. Properties on the accessible
;;; implementation of an object can be set by accessing the internal child
;;; "accessible" of a GtkWidget.
;;; 
;;; Example 103. A UI definition fragment specifying an accessible
;;; 
;;;  <object class="GtkButton" id="label1"/>
;;;    <property name="label">I am a Label for a Button</property>
;;;  </object>
;;;  <object class="GtkButton" id="button1">
;;;    <accessibility>
;;;      <action action_name="click" translatable="yes">Click the button.</action>
;;;      <relation target="label1" type="labelled-by"/>
;;;    </accessibility>
;;;    <child internal-child="accessible">
;;;      <object class="AtkObject" id="a11y-button1">
;;;        <property name="AtkObject::name">Clickable Button</property>
;;;      </object>
;;;    </child>
;;;  </object>
;;;  
;;; Finally, GtkWidget allows style information such as style classes to be
;;; associated with widgets, using the custom <style> element:
;;; 
;;; Example 104. A UI definition fragment specifying an style class
;;; 
;;;  <object class="GtkButton" id="button1">
;;;    <style>
;;;      <class name="my-special-button-class"/>
;;;      <class name="dark-button"/>
;;;    </style>
;;;  </object>
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "app-paintable" property
;;; 
;;;   "app-paintable"            gboolean              : Read / Write
;;; 
;;; Whether the application will paint directly on the widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "can-default" property
;;; 
;;;   "can-default"              gboolean              : Read / Write
;;; 
;;; Whether the widget can be the default widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "can-focus" property
;;; 
;;;   "can-focus"                gboolean              : Read / Write
;;; 
;;; Whether the widget can accept the input focus.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "composite-child" property
;;; 
;;;   "composite-child"          gboolean              : Read
;;; 
;;; Whether the widget is part of a composite widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "double-buffered" property
;;; 
;;;   "double-buffered"          gboolean              : Read / Write
;;; 
;;; Whether the widget is double buffered.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "events" property
;;; 
;;;   "events"                   GdkEventMask          : Read / Write
;;; 
;;; The event mask that decides what kind of GdkEvents this widget gets.
;;; 
;;; Default value: GDK_STRUCTURE_MASK
;;;
;;; ----------------------------------------------------------------------------
;;; The "expand" property
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;; 
;;; Whether to expand in both directions. Setting this sets both "hexpand" and
;;; "vexpand"
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "halign" property
;;; 
;;;   "halign"                   GtkAlign              : Read / Write
;;; 
;;; How to distribute horizontal space if widget gets extra space, see GtkAlign
;;; 
;;; Default value: GTK_ALIGN_FILL
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-default" property
;;; 
;;;   "has-default"              gboolean              : Read / Write
;;; 
;;; Whether the widget is the default widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-focus" property
;;; 
;;;   "has-focus"                gboolean              : Read / Write
;;; 
;;; Whether the widget has the input focus.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-tooltip" property
;;; 
;;;   "has-tooltip"              gboolean              : Read / Write
;;; 
;;; Enables or disables the emission of "query-tooltip" on widget. A value of
;;; TRUE indicates that widget can have a tooltip, in this case the widget will
;;; be queried using "query-tooltip" to determine whether it will provide a
;;; tooltip or not.
;;; 
;;; Note that setting this property to TRUE for the first time will change the
;;; event masks of the GdkWindows of this widget to include leave-notify and
;;; motion-notify events. This cannot and will not be undone when the property
;;; is set to FALSE again.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "height-request" property
;;; 
;;;   "height-request"           gint                  : Read / Write
;;; 
;;; Override for height request of the widget, or -1 if natural request should
;;; be used.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "hexpand" property
;;; 
;;;   "hexpand"                  gboolean              : Read / Write
;;; 
;;; Whether to expand horizontally. See gtk_widget_set_hexpand().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "hexpand-set" property
;;; 
;;;   "hexpand-set"              gboolean              : Read / Write
;;; 
;;; Whether to use the "hexpand" property. See gtk_widget_get_hexpand_set().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-focus" property
;;; 
;;;   "is-focus"                 gboolean              : Read / Write
;;; 
;;; Whether the widget is the focus widget within the toplevel.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin" property
;;; 
;;;   "margin"                   gint                  : Read / Write
;;; 
;;; Sets all four sides' margin at once. If read, returns max margin on any
;;; side.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin-bottom" property
;;; 
;;;   "margin-bottom"            gint                  : Read / Write
;;; 
;;; Margin on bottom side of widget.
;;; 
;;; This property adds margin outside of the widget's normal size request, the
;;; margin will be added in addition to the size from
;;; gtk_widget_set_size_request() for example.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin-left" property
;;; 
;;;   "margin-left"              gint                  : Read / Write
;;; 
;;; Margin on left side of widget.
;;; 
;;; This property adds margin outside of the widget's normal size request, the
;;; margin will be added in addition to the size from
;;; gtk_widget_set_size_request() for example.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin-right" property
;;; 
;;;   "margin-right"             gint                  : Read / Write
;;; 
;;; Margin on right side of widget.
;;; 
;;; This property adds margin outside of the widget's normal size request, the
;;; margin will be added in addition to the size from
;;; gtk_widget_set_size_request() for example.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin-top" property
;;; 
;;;   "margin-top"               gint                  : Read / Write
;;; 
;;; Margin on top side of widget.
;;; 
;;; This property adds margin outside of the widget's normal size request, the
;;; margin will be added in addition to the size from
;;; gtk_widget_set_size_request() for example.
;;; 
;;; Allowed values: [0,32767]
;;; 
;;; Default value: 0
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;; 
;;;   "name"                     gchar*                : Read / Write
;;; 
;;; The name of the widget.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "no-show-all" property
;;; 
;;;   "no-show-all"              gboolean              : Read / Write
;;; 
;;; Whether gtk_widget_show_all() should not affect this widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "parent" property
;;; 
;;;   "parent"                   GtkContainer*         : Read / Write
;;; 
;;; The parent widget of this widget. Must be a Container widget.
;;;
;;; ----------------------------------------------------------------------------
;;; The "receives-default" property
;;; 
;;;   "receives-default"         gboolean              : Read / Write
;;; 
;;; If TRUE, the widget will receive the default action when it is focused.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "sensitive" property
;;; 
;;;   "sensitive"                gboolean              : Read / Write
;;; 
;;; Whether the widget responds to input.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "style" property
;;; 
;;;   "style"                    GtkStyle*             : Read / Write
;;; 
;;; The style of the widget, which contains information about how it will look
;;; (colors etc).
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-markup" property
;;; 
;;;   "tooltip-markup"           gchar*                : Read / Write
;;; 
;;; Sets the text of tooltip to be the given string, which is marked up with
;;; the Pango text markup language. Also see gtk_tooltip_set_markup().
;;; 
;;; This is a convenience property which will take care of getting the tooltip
;;; shown if the given string is not NULL: "has-tooltip" will automatically be
;;; set to TRUE and there will be taken care of "query-tooltip" in the default
;;; signal handler.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-text" property
;;; 
;;;   "tooltip-text"             gchar*                : Read / Write
;;; 
;;; Sets the text of tooltip to be the given string.
;;; 
;;; Also see gtk_tooltip_set_text().
;;; 
;;; This is a convenience property which will take care of getting the tooltip
;;; shown if the given string is not NULL: "has-tooltip" will automatically be
;;; set to TRUE and there will be taken care of "query-tooltip" in the default
;;; signal handler.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "valign" property
;;; 
;;;   "valign"                   GtkAlign              : Read / Write
;;; 
;;; How to distribute vertical space if widget gets extra space, see GtkAlign
;;; 
;;; Default value: GTK_ALIGN_FILL
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vexpand" property
;;; 
;;;   "vexpand"                  gboolean              : Read / Write
;;; 
;;; Whether to expand vertically. See gtk_widget_set_vexpand().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vexpand-set" property
;;; 
;;;   "vexpand-set"              gboolean              : Read / Write
;;; 
;;; Whether to use the "vexpand" property. See gtk_widget_get_vexpand_set().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;; 
;;;   "visible"                  gboolean              : Read / Write
;;; 
;;; Whether the widget is visible.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-request" property
;;; 
;;;   "width-request"            gint                  : Read / Write
;;; 
;;; Override for width request of the widget, or -1 if natural request should
;;; be used.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "window" property
;;; 
;;;   "window"                   GdkWindow*            : Read
;;; 
;;; The widget's window if it is realized, NULL otherwise.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-aspect-ratio" style property
;;; 
;;;   "cursor-aspect-ratio"      gfloat                : Read
;;; 
;;; Aspect ratio with which to draw insertion cursor.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.04
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-color" style property
;;; 
;;;   "cursor-color"             GdkColor*             : Read
;;; 
;;; Color with which to draw insertion cursor.
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-line-pattern" style property
;;; 
;;;   "focus-line-pattern"       gchar*                : Read
;;; 
;;; Dash pattern used to draw the focus indicator.
;;; 
;;; Default value: "\001\001"
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-line-width" style property
;;; 
;;;   "focus-line-width"         gint                  : Read
;;; 
;;; Width, in pixels, of the focus indicator line.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-padding" style property
;;; 
;;;   "focus-padding"            gint                  : Read
;;; 
;;; Width, in pixels, between focus indicator and the widget 'box'.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "interior-focus" style property
;;; 
;;;   "interior-focus"           gboolean              : Read
;;; 
;;; Whether to draw the focus indicator inside widgets.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "link-color" style property
;;; 
;;;   "link-color"               GdkColor*             : Read
;;; 
;;; The "link-color" style property defines the color of unvisited links.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-arrow-hlength" style property
;;; 
;;;   "scroll-arrow-hlength"     gint                  : Read
;;; 
;;; The "scroll-arrow-hlength" style property defines the length of horizontal
;;; scroll arrows.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 16
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-arrow-vlength" style property
;;; 
;;;   "scroll-arrow-vlength"     gint                  : Read
;;; 
;;; The "scroll-arrow-vlength" style property defines the length of vertical
;;; scroll arrows.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 16
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "secondary-cursor-color" style property
;;; 
;;;   "secondary-cursor-color"   GdkColor*             : Read
;;; 
;;; Color with which to draw the secondary insertion cursor when editing mixed
;;; right-to-left and left-to-right text.
;;;
;;; ----------------------------------------------------------------------------
;;; The "separator-height" style property
;;; 
;;;   "separator-height"         gint                  : Read
;;; 
;;; The "separator-height" style property defines the height of separators.
;;; This property only takes effect if "wide-separators" is TRUE.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "separator-width" style property
;;; 
;;;   "separator-width"          gint                  : Read
;;; 
;;; The "separator-width" style property defines the width of separators.
;;; This property only takes effect if "wide-separators" is TRUE.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "visited-link-color" style property
;;; 
;;;   "visited-link-color"       GdkColor*             : Read
;;; 
;;; The "visited-link-color" style property defines the color of visited links.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "wide-separators" style property
;;; 
;;;   "wide-separators"          gboolean              : Read
;;; 
;;; The "wide-separators" style property defines whether separators have
;;; configurable width and should be drawn using a box instead of a line.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-dragging" style property
;;; 
;;;   "window-dragging"          gboolean              : Read
;;; 
;;; Whether windows can be dragged by clicking on empty areas.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accel-closures-changed" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-press-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::button-press-event signal will be emitted when a button (typically
;;; from a mouse) is pressed.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_BUTTON_PRESS_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEventButton which triggered this signal. [type Gdk.EventButton]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-release-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::button-release-event signal will be emitted when a button (typically
;;; from a mouse) is released.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_BUTTON_RELEASE_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEventButton which triggered this signal. [type Gdk.EventButton]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "can-activate-accel" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         guint      signal_id,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; Determines whether an accelerator that activates the signal identified by
;;; signal_id can currently be activated. This signal is present to allow
;;; applications and derived widgets to override the default GtkWidget handling
;;; for determining whether an accelerator can be activated.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; signal_id :
;;;     the ID of a signal installed on widget
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the signal can be activated.
;;;
;;; ----------------------------------------------------------------------------
;;; The "child-notify" signal
;;; 
;;; void user_function (GtkWidget  *widget,
;;;                     GParamSpec *pspec,
;;;                     gpointer    user_data)      : No Hooks
;;; 
;;; The ::child-notify signal is emitted for each child property that has
;;; changed on an object. The signal's detail holds the property name.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; pspec :
;;;     the GParamSpec of the changed child property
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "composited-changed" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Action
;;; 
;;; The ::composited-changed signal is emitted when the composited status of
;;; widgets screen changes. See gdk_screen_is_composited().
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "configure-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::configure-event signal will be emitted when the size, position or
;;; stacking of the widget's window has changed.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventConfigure which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "damage-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent *event,
;;;                         gpointer user_data)      : Run Last
;;; 
;;; Emitted when a redirected window belonging to widget gets drawn into. The
;;; region/area members of the event shows what area of the redirected drawable
;;; was drawn into.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventExpose event. [type Gdk.EventExpose]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "delete-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer  user_data)      : Run Last
;;; 
;;; The ::delete-event signal is emitted if a user requests that a toplevel
;;; window is closed. The default handler for this signal destroys the window.
;;; Connecting gtk_widget_hide_on_delete() to this signal will cause the window
;;; to be hidden instead, so that it can later be shown again without
;;; reconstructing it.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the event which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "destroy" signal
;;; 
;;; void user_function (GtkWidget *object, gpointer user_data)      : No Hooks
;;; 
;;; Signals that all holders of a reference to the widget should release the
;;; reference that they hold. May result in finalization of the widget if all
;;; references are released.
;;; 
;;; object :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "destroy-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::destroy-event signal is emitted when a GdkWindow is destroyed. You
;;; rarely get this signal, because most widgets disconnect themselves from
;;; their window before they destroy it, so no widget owns the window at
;;; destroy time.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the event which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "direction-changed" signal
;;; 
;;; void user_function (GtkWidget       *widget,
;;;                     GtkTextDirection previous_direction,
;;;                     gpointer         user_data)               : Run First
;;; 
;;; The ::direction-changed signal is emitted when the text direction of a
;;; widget changes.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; previous_direction :
;;;     the previous text direction of widget
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-begin" signal
;;; 
;;; void user_function (GtkWidget      *widget,
;;;                     GdkDragContext *drag_context,
;;;                     gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-begin signal is emitted on the drag source when a drag is
;;; started. A typical reason to connect to this signal is to set up a custom
;;; drag icon with gtk_drag_source_set_icon().
;;; 
;;; Note that some widgets set up a drag icon in the default handler of this
;;; signal, so you may have to use g_signal_connect_after() to override what
;;; the default handler did.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-data-delete" signal
;;; 
;;; void user_function (GtkWidget      *widget,
;;;                     GdkDragContext *drag_context,
;;;                     gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-data-delete signal is emitted on the drag source when a drag
;;; with the action GDK_ACTION_MOVE is successfully completed. The signal
;;; handler is responsible for deleting the data that has been dropped. What
;;; "delete" means depends on the context of the drag operation.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-data-get" signal
;;; 
;;; void user_function (GtkWidget        *widget,
;;;                     GdkDragContext   *drag_context,
;;;                     GtkSelectionData *data,
;;;                     guint             info,
;;;                     guint             time,
;;;                     gpointer          user_data)         : Run Last
;;; 
;;; The ::drag-data-get signal is emitted on the drag source when the drop site
;;; requests the data which is dragged. It is the responsibility of the signal
;;; handler to fill data with the data in the format which is indicated by info.
;;; See gtk_selection_data_set() and gtk_selection_data_set_text().
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; data :
;;;     the GtkSelectionData to be filled with the dragged data
;;; 
;;; info :
;;;     the info that has been registered with the target in the GtkTargetList
;;; 
;;; time :
;;;     the timestamp at which the data was requested
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-data-received" signal
;;; 
;;; void user_function (GtkWidget        *widget,
;;;                     GdkDragContext   *drag_context,
;;;                     gint              x,
;;;                     gint              y,
;;;                     GtkSelectionData *data,
;;;                     guint             info,
;;;                     guint             time,
;;;                     gpointer          user_data)         : Run Last
;;; 
;;; The ::drag-data-received signal is emitted on the drop site when the
;;; dragged data has been received. If the data was received in order to
;;; determine whether the drop will be accepted, the handler is expected to
;;; call gdk_drag_status() and not finish the drag. If the data was received in
;;; response to a "drag-drop" signal (and this is the last target to be
;;; received), the handler for this signal is expected to process the received
;;; data and then call gtk_drag_finish(), setting the success parameter
;;; depending on whether the data was processed successfully.
;;; 
;;; The handler may inspect and modify drag_context->action before calling
;;; gtk_drag_finish(), e.g. to implement GDK_ACTION_ASK as shown in the
;;; following example:
;;; 
;;; void
;;; drag_data_received (GtkWidget          *widget,
;;;                     GdkDragContext     *drag_context,
;;;                     gint                x,
;;;                     gint                y,
;;;                     GtkSelectionData   *data,
;;;                     guint               info,
;;;                     guint               time)
;;; {
;;;   if ((data->length >= 0) && (data->format == 8))
;;;     {
;;;       if (drag_context->action == GDK_ACTION_ASK)
;;;         {
;;;           GtkWidget *dialog;
;;;           gint response;
;;;           dialog = gtk_message_dialog_new (NULL,
;;;                                            GTK_DIALOG_MODAL |
;;;                                            GTK_DIALOG_DESTROY_WITH_PARENT,
;;;                                            GTK_MESSAGE_INFO,
;;;                                            GTK_BUTTONS_YES_NO,
;;;                                            "Move the data ?\n");
;;;           response = gtk_dialog_run (GTK_DIALOG (dialog));
;;;           gtk_widget_destroy (dialog);
;;; 
;;;           if (response == GTK_RESPONSE_YES)
;;;             drag_context->action = GDK_ACTION_MOVE;
;;;           else
;;;             drag_context->action = GDK_ACTION_COPY;
;;;          }
;;; 
;;;       gtk_drag_finish (drag_context, TRUE, FALSE, time);
;;;       return;
;;;     }
;;; 
;;;    gtk_drag_finish (drag_context, FALSE, FALSE, time);
;;;  }
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; x :
;;;     where the drop happened
;;; 
;;; y :
;;;     where the drop happened
;;; 
;;; data :
;;;     the received data
;;; 
;;; info :
;;;     the info that has been registered with the target in the GtkTargetList
;;; 
;;; time :
;;;     the timestamp at which the data was received
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-drop" signal
;;; 
;;; gboolean user_function (GtkWidget      *widget,
;;;                         GdkDragContext *drag_context,
;;;                         gint            x,
;;;                         gint            y,
;;;                         guint           time,
;;;                         gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-drop signal is emitted on the drop site when the user drops the
;;; data onto the widget. The signal handler must determine whether the cursor
;;; position is in a drop zone or not. If it is not in a drop zone, it returns
;;; FALSE and no further processing is necessary. Otherwise, the handler returns
;;; TRUE. In this case, the handler must ensure that gtk_drag_finish() is called
;;; to let the source know that the drop is done. The call to gtk_drag_finish()
;;; can be done either directly or in a "drag-data-received" handler which gets
;;; triggered by calling gtk_drag_get_data() to receive the data for one or more
;;; of the supported targets.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; x :
;;;     the x coordinate of the current cursor position
;;; 
;;; y :
;;;     the y coordinate of the current cursor position
;;; 
;;; time :
;;;     the timestamp of the motion event
;;; 
;;; returns :
;;;     whether the cursor position is in a drop zone
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-end" signal
;;; 
;;; void user_function (GtkWidget      *widget,
;;;                     GdkDragContext *drag_context,
;;;                     gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-end signal is emitted on the drag source when a drag is finished.
;;; A typical reason to connect to this signal is to undo things done in
;;; "drag-begin".
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-failed" signal
;;; 
;;; gboolean user_function (GtkWidget      *widget,
;;;                         GdkDragContext *drag_context,
;;;                         GtkDragResult   result,
;;;                         gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-failed signal is emitted on the drag source when a drag has
;;; failed. The signal handler may hook custom code to handle a failed DND
;;; operation based on the type of error, it returns TRUE is the failure has
;;; been already handled (not showing the default "drag operation failed"
;;; animation), otherwise it returns FALSE.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; result :
;;;     the result of the drag operation
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if the failed drag operation has been already handled.
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-leave" signal
;;; 
;;; void user_function (GtkWidget      *widget,
;;;                     GdkDragContext *drag_context,
;;;                     guint           time,
;;;                     gpointer        user_data)         : Run Last
;;; 
;;; The ::drag-leave signal is emitted on the drop site when the cursor leaves
;;; the widget. A typical reason to connect to this signal is to undo things
;;; done in "drag-motion", e.g. undo highlighting with gtk_drag_unhighlight()
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; time :
;;;     the timestamp of the motion event
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "drag-motion" signal
;;; 
;;; gboolean user_function (GtkWidget      *widget,
;;;                         GdkDragContext *drag_context,
;;;                         gint            x,
;;;                         gint            y,
;;;                         guint           time,
;;;                         gpointer        user_data)         : Run Last
;;; 
;;; The drag-motion signal is emitted on the drop site when the user moves the
;;; cursor over the widget during a drag. The signal handler must determine
;;; whether the cursor position is in a drop zone or not. If it is not in a drop
;;; zone, it returns FALSE and no further processing is necessary. Otherwise,
;;; the handler returns TRUE. In this case, the handler is responsible for
;;; providing the necessary information for displaying feedback to the user, by
;;; calling gdk_drag_status().
;;; 
;;; If the decision whether the drop will be accepted or rejected can't be made
;;; based solely on the cursor position and the type of the data, the handler
;;; may inspect the dragged data by calling gtk_drag_get_data() and defer the
;;; gdk_drag_status() call to the "drag-data-received" handler. Note that you
;;; cannot not pass GTK_DEST_DEFAULT_DROP, GTK_DEST_DEFAULT_MOTION or
;;; GTK_DEST_DEFAULT_ALL to gtk_drag_dest_set() when using the drag-motion
;;; signal that way.
;;; 
;;; Also note that there is no drag-enter signal. The drag receiver has to keep
;;; track of whether he has received any drag-motion signals since the last
;;; "drag-leave" and if not, treat the drag-motion signal as an "enter" signal.
;;; Upon an "enter", the handler will typically highlight the drop site with
;;; gtk_drag_highlight().
;;; 
;;; static void
;;; drag_motion (GtkWidget *widget,
;;;              GdkDragContext *context,
;;;              gint x,
;;;              gint y,
;;;              guint time)
;;; {
;;;   GdkAtom target;
;;; 
;;;   PrivateData *private_data = GET_PRIVATE_DATA (widget);
;;; 
;;;   if (!private_data->drag_highlight)
;;;    {
;;;      private_data->drag_highlight = 1;
;;;      gtk_drag_highlight (widget);
;;;    }
;;; 
;;;   target = gtk_drag_dest_find_target (widget, context, NULL);
;;;   if (target == GDK_NONE)
;;;     gdk_drag_status (context, 0, time);
;;;   else
;;;    {
;;;      private_data->pending_status = context->suggested_action;
;;;      gtk_drag_get_data (widget, context, target, time);
;;;    }
;;; 
;;;   return TRUE;
;;; }
;;; 
;;; static void
;;; drag_data_received (GtkWidget        *widget,
;;;                     GdkDragContext   *context,
;;;                     gint              x,
;;;                     gint              y,
;;;                     GtkSelectionData *selection_data,
;;;                     guint             info,
;;;                     guint             time)
;;; {
;;;   PrivateData *private_data = GET_PRIVATE_DATA (widget);
;;; 
;;;   if (private_data->suggested_action)
;;;    {
;;;      private_data->suggested_action = 0;
;;; 
;;;     /* We are getting this data due to a request in drag_motion,
;;;      * rather than due to a request in drag_drop, so we are just
;;;      * supposed to call gdk_drag_status (), not actually paste in
;;;      * the data.
;;;      */
;;;      str = gtk_selection_data_get_text (selection_data);
;;;      if (!data_is_acceptable (str))
;;;        gdk_drag_status (context, 0, time);
;;;      else
;;;        gdk_drag_status (context, private_data->suggested_action, time);
;;;    }
;;;   else
;;;    {
;;;      /* accept the drop */
;;;    }
;;; }
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; drag_context :
;;;     the drag context
;;; 
;;; x :
;;;     the x coordinate of the current cursor position
;;; 
;;; y :
;;;     the y coordinate of the current cursor position
;;; 
;;; time :
;;;     the timestamp of the motion event
;;; 
;;; returns :
;;;     whether the cursor position is in a drop zone
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw" signal
;;; 
;;; gboolean user_function (GtkWidget    *widget,
;;;                         CairoContext *cr,
;;;                         gpointer      user_data)      : Run Last
;;; 
;;; This signal is emitted when a widget is supposed to render itself. The
;;; widget's top left corner must be painted at the origin of the passed in
;;; context and be sized to the values returned by
;;; gtk_widget_get_allocated_width() and gtk_widget_get_allocated_height().
;;; 
;;; Signal handlers connected to this signal can modify the cairo context
;;; passed as cr in any way they like and don't need to restore it. The signal
;;; emission takes care of calling cairo_save() before and cairo_restore()
;;; after invoking the handler.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; cr :
;;;     the cairo context to draw to
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "enter-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::enter-notify-event will be emitted when the pointer enters the
;;; widget's window.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_ENTER_NOTIFY_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventCrossing which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The GTK+ main loop will emit three signals for each GDK event delivered
;;; to a widget: one generic ::event signal, another, more specific, signal
;;; that matches the type of event delivered (e.g. "key-press-event") and
;;; finally a generic "event-after" signal.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEvent which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event and to
;;;     cancel the emission of the second specific ::event signal. FALSE to
;;;     propagate the event further and to allow the emission of the second
;;;     signal. The ::event-after signal is emitted regardless of the return
;;;     value.
;;;
;;; ----------------------------------------------------------------------------
;;; The "event-after" signal
;;; 
;;; void user_function (GtkWidget *widget, GdkEvent *event, gpointer user_data)
;;; 
;;; After the emission of the "event" signal and (optionally) the second more
;;; specific signal, ::event-after will be emitted regardless of the previous
;;; two signals handlers return values.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEvent which triggered this signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus" signal
;;; 
;;; gboolean user_function (GtkWidget       *widget,
;;;                         GtkDirectionType direction,
;;;                         gpointer         user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-in-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::focus-in-event signal will be emitted when the keyboard focus enters
;;; the widget's window.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_FOCUS_CHANGE_MASK mask.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventFocus which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-out-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::focus-out-event signal will be emitted when the keyboard focus leaves
;;; the widget's window.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_FOCUS_CHANGE_MASK mask.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventFocus which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "grab-broken-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; Emitted when a pointer or keyboard grab on a window belonging to widget
;;; gets broken.
;;; 
;;; On X11, this happens when the grab window becomes unviewable (i.e. it or
;;; one of its ancestors is unmapped), or if the same application grabs the
;;; pointer or keyboard again.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventGrabBroken event. [type Gdk.EventGrabBroken]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "grab-focus" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Action
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "grab-notify" signal
;;; 
;;; void user_function (GtkWidget *widget,
;;;                     gboolean   was_grabbed,
;;;                     gpointer   user_data)        : Run First
;;; 
;;; The ::grab-notify signal is emitted when a widget becomes shadowed by a
;;; GTK+ grab (not a pointer or keyboard grab) on another widget, or when it
;;; becomes unshadowed due to a grab being removed.
;;; 
;;; A widget is shadowed by a gtk_grab_add() when the topmost grab widget in
;;; the grab stack of its window group is not its ancestor.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; was_grabbed :
;;;     FALSE if the widget becomes shadowed, TRUE if it becomes unshadowed
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "hide" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "hierarchy-changed" signal
;;; 
;;; void user_function (GtkWidget *widget,
;;;                     GtkWidget *previous_toplevel,
;;;                     gpointer   user_data)              : Run Last
;;; 
;;; The ::hierarchy-changed signal is emitted when the anchored state of a
;;; widget changes. A widget is anchored when its toplevel ancestor is a
;;; GtkWindow. This signal is emitted when a widget changes from un-anchored to
;;; anchored or vice-versa.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; previous_toplevel :
;;;     the previous toplevel ancestor, or NULL if the widget was previously
;;;     unanchored.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "key-press-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::key-press-event signal is emitted when a key is pressed. The signal
;;; emission will reoccur at the key-repeat rate when the key is kept pressed.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_KEY_PRESS_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventKey which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "key-release-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::key-release-event signal is emitted when a key is released.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_KEY_RELEASE_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventKey which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "keynav-failed" signal
;;; 
;;; gboolean user_function (GtkWidget       *widget,
;;;                         GtkDirectionType direction,
;;;                         gpointer         user_data)      : Run Last
;;; 
;;; Gets emitted if keyboard navigation fails. See gtk_widget_keynav_failed()
;;; for details.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; direction :
;;;     the direction of movement
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if stopping keyboard navigation is fine, FALSE if the emitting
;;;     widget should try to handle the keyboard navigation attempt in its
;;;     parent container(s).
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "leave-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::leave-notify-event will be emitted when the pointer leaves the
;;; widget's window.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_LEAVE_NOTIFY_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventCrossing which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "map" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "map-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::map-event signal will be emitted when the widget's window is mapped.
;;; A window is mapped when it becomes visible on the screen.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventAny which triggered this signal. [type Gdk.EventAny]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "mnemonic-activate" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         gboolean   arg1,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "motion-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::motion-notify-event signal is emitted when the pointer moves over
;;; the widget's GdkWindow.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_POINTER_MOTION_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEventMotion which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-focus" signal
;;; 
;;; void user_function (GtkWidget       *widget,
;;;                     GtkDirectionType direction,
;;;                     gpointer         user_data)      : Action
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "parent-set" signal
;;; 
;;; void user_function (GtkWidget *widget,
;;;                     GtkWidget *old_parent,
;;;                     gpointer   user_data)       : Run First
;;; 
;;; The ::parent-set signal is emitted when a new parent has been set on a
;;; widget.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; old_parent :
;;;     the previous parent, or NULL if the widget just got its initial parent.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-menu" signal
;;; 
;;; gboolean user_function (GtkWidget *widget, gpointer user_data)     : Action
;;; 
;;; This signal gets emitted whenever a widget should pop up a context menu.
;;; This usually happens through the standard key binding mechanism; by pressing
;;; a certain key while a widget is focused, the user can cause the widget to
;;; pop up a menu. For example, the GtkEntry widget creates a menu with
;;; clipboard commands. See the section called “Implement GtkWidget::popup_menu”
;;; for an example of how to use this signal.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if a menu was activated
;;;
;;; ----------------------------------------------------------------------------
;;; The "property-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::property-notify-event signal will be emitted when a property on the
;;; widget's window has been changed or deleted.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_PROPERTY_CHANGE_MASK mask.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventProperty which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "proximity-in-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_PROXIMITY_IN_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventProximity which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "proximity-out-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_PROXIMITY_OUT_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventProximity which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "query-tooltip" signal
;;; 
;;; gboolean user_function (GtkWidget  *widget,
;;;                         gint        x,
;;;                         gint        y,
;;;                         gboolean    keyboard_mode,
;;;                         GtkTooltip *tooltip,
;;;                         gpointer    user_data)          : Run Last
;;; 
;;; Emitted when "has-tooltip" is TRUE and the "gtk-tooltip-timeout" has
;;; expired with the cursor hovering "above" widget; or emitted when widget got
;;; focus in keyboard mode.
;;; 
;;; Using the given coordinates, the signal handler should determine whether
;;; a tooltip should be shown for widget. If this is the case TRUE should be
;;; returned, FALSE otherwise. Note that if keyboard_mode is TRUE, the values
;;; of x and y are undefined and should not be used.
;;; 
;;; The signal handler is free to manipulate tooltip with the therefore
;;; destined function calls.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; x :
;;;     the x coordinate of the cursor position where the request has been
;;;     emitted, relative to widget's left side
;;; 
;;; y :
;;;     the y coordinate of the cursor position where the request has been
;;;     emitted, relative to widget's top
;;; 
;;; keyboard_mode :
;;;     TRUE if the tooltip was trigged using the keyboard
;;; 
;;; tooltip :
;;;     a GtkTooltip
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE if tooltip should be shown right now, FALSE otherwise.
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "realize" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "screen-changed" signal
;;; 
;;; void user_function (GtkWidget *widget,
;;;                     GdkScreen *previous_screen,
;;;                     gpointer   user_data)            : Run Last
;;; 
;;; The ::screen-changed signal gets emitted when the screen of a widget has
;;; changed.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; previous_screen :
;;;     the previous screen, or NULL if the widget was not associated with a
;;;     screen before. [allow-none]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "scroll-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::scroll-event signal is emitted when a button in the 4 to 7 range is
;;; pressed. Wheel mice are usually configured to generate button press events
;;; for buttons 4 and 5 when the wheel is turned.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_BUTTON_PRESS_MASK mask.
;;; 
;;; This signal will be sent to the grab widget if there is one.
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     the GdkEventScroll which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-clear-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::selection-clear-event signal will be emitted when the the widget's
;;; window has lost ownership of a selection.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventSelection which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from beig invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-get" signal
;;; 
;;; void user_function (GtkWidget        *widget,
;;;                     GtkSelectionData *data,
;;;                     guint             info,
;;;                     guint             time,
;;;                     gpointer          user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; event :
;;;     . [type Gdk.EventSelection]
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------3
;;; The "selection-received" signal
;;; 
;;; void user_function (GtkWidget        *widget,
;;;                     GtkSelectionData *data,
;;;                     guint             time,
;;;                     gpointer          user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-request-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::selection-request-event signal will be emitted when another client
;;; requests ownership of the selection owned by the widget's window.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventSelection which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "show" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First

;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-help" signal
;;; 
;;; gboolean user_function (GtkWidget        *widget,
;;;                         GtkWidgetHelpType help_type,
;;;                         gpointer          user_data)      : Action
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-allocate" signal
;;; 
;;; void user_function (GtkWidget    *widget,
;;;                     GdkRectangle *allocation,
;;;                     gpointer      user_data)       : Run First
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-changed" signal
;;; 
;;; void user_function (GtkWidget   *widget,
;;;                     GtkStateType state,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Warning
;;; 
;;; GtkWidget::state-changed is deprecated and should not be used in
;;; newly-written code. 3.0. Use "state-flags-changed" instead.
;;; 
;;; The ::state-changed signal is emitted when the widget state changes. See
;;; gtk_widget_get_state().
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; state :
;;;     the previous state
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-flags-changed" signal
;;; 
;;; void user_function (GtkWidget    *widget,
;;;                     GtkStateFlags flags,
;;;                     gpointer      user_data)      : Run First
;;; 
;;; The ::state-flags-changed signal is emitted when the widget state changes,
;;; see gtk_widget_get_state_flags().
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; flags :
;;;     The previous state flags.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-set" signal
;;; 
;;; void user_function (GtkWidget *widget,
;;;                     GtkStyle  *previous_style,
;;;                     gpointer   user_data)           : Run First
;;; 
;;; Warning
;;; 
;;; GtkWidget::style-set has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use the "style-updated" signal
;;; 
;;; The ::style-set signal is emitted when a new style has been set on a widget.
;;; Note that style-modifying functions like gtk_widget_modify_base() also
;;; cause this signal to be emitted.
;;; 
;;; Note that this signal is emitted for changes to the deprecated GtkStyle. To
;;; track changes to the GtkStyleContext associated with a widget, use the
;;; "style-updated" signal.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; previous_style :
;;;     the previous style, or NULL if the widget just got its initial style.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-updated" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First
;;; 
;;; The ::style-updated signal is emitted when the GtkStyleContext of a widget
;;; is changed. Note that style-modifying functions like
;;; gtk_widget_override_color() also cause this signal to be emitted.
;;; 
;;; widget :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "unmap" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run First
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "unmap-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer user_data)      : Run Last
;;; 
;;; The ::unmap-event signal will be emitted when the widget's window is
;;; unmapped. A window is unmapped when it becomes invisible on the screen.
;;; 
;;; To receive this signal, the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventAny which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "unrealize" signal
;;; 
;;; void user_function (GtkWidget *widget, gpointer user_data)      : Run Last
;;; 
;;; widget :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "visibility-notify-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                         GdkEvent  *event,
;;;                         gpointer   user_data)      : Run Last
;;; 
;;; The ::visibility-notify-event will be emitted when the widget's window is
;;; obscured or unobscured.
;;; 
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_VISIBILITY_NOTIFY_MASK mask.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventVisibility which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;;
;;; ----------------------------------------------------------------------------
;;; The "window-state-event" signal
;;; 
;;; gboolean user_function (GtkWidget *widget,
;;;                                   GdkEvent  *event,
;;;                                   gpointer   user_data)      : Run Last
;;; 
;;; The ::window-state-event will be emitted when the state of the toplevel
;;; window associated to the widget changes.
;;; 
;;; To receive this signal the GdkWindow associated to the widget needs to
;;; enable the GDK_STRUCTURE_MASK mask. GDK will enable this mask automatically
;;; for all new windows.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; event :
;;;     the GdkEventWindowState which triggered this signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     TRUE to stop other handlers from being invoked for the event. FALSE to
;;;     propagate the event further.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_push_colormap" gtk-widget-push-colormap) :void
  (colormap (g-object gdk-colormap)))

(export 'gtk-widget-push-colormap)

(defcfun ("gtk_widget_pop_colormap" gtk-widget-pop-colormap) :void)

(export 'gtk-widget-pop-colormap)

(defcfun ("gtk_widget_set_default_colormap" gtk-widget-set-default-colormap)
    :void
  (colormap (g-object gdk-colormap)))

(export 'gtk-widget-set-default-colormap)

(defcfun ("gtk_widget_get_default_colormap" gtk-widget-default-colormap)
    (g-object gdk-colormap))

(defun (setf gtk-widget-default-colormap) (colormap)
  (gtk-widget-set-default-colormap colormap))

(export 'gtk-widget-default-colormap)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_default_visual" gtk-widget-default-visual)
  (g-object gdk-visual))

(export 'gtk-widget-default-visual)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_shape_combine_mask" gtk-widget-shape-combine-mask) :void
  (widget (g-object gtk-widget))
  (shape-mask g-object)
  (offset-x :int)
  (offset-y :int))

(export 'gtk-widget-shape-combine-mask)

(defcfun ("gtk_widget_input_shape_combine_mask"
          gtk-widget-input-shape-combine-mask) :void
  (widget (g-object gtk-widget))
  (shape-mask g-object)
  (offset-x :int)
  (offset-y :int))

(export 'gtk-widget-input-shape-combine-mask)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_queue_clear" gtk-widget-queue-clear) :void
  (widget (g-object gtk-widget)))

(export 'gtk-widget-queue-clear)

(defcfun ("gtk_widget_queue_clear_area" gtk-widget-queue-clear-area) :void
  (widget (g-object gtk-widget))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gtk-widget-queue-clear-area)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_reset_shapes" gtk-widget-reset-shapes) :void
  (widget g-object))

(export 'gtk-widget-reset-shapes)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_scroll_adjustments"
          gtk-widget-set-scroll-adjustments) :boolean
  (widget g-object)
  (hadjustment g-object)
  (vadjustment g-object))

(export 'gtk-widget-set-scroll-adjustments)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_action" gtk-widget-get-action) g-object
  (widget g-object))

(export 'gtk-widget-get-action)

;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_snapshot" %gtk-widget-get-snapshot) g-object
  (widget g-object)
  (clip-rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-widget-snapshot (widget &optional clip-rectangle)
  (%gtk-widget-get-snapshot widget clip-rectangle))

(export 'gtk-widget-snapshot)

;;; ----------------------------------------------------------------------------
;;; struct GtkRequisition
;;; 
;;; struct GtkRequisition {
;;;   gint width;
;;;   gint height;
;;; };
;;; 
;;; A GtkRequisition represents the desired size of a widget. See the section
;;; called “Height-for-width Geometry Management” for more information.
;;; 
;;; gint width;
;;;     the widget's desired width
;;; 
;;; gint height;
;;;     the widget's desired height
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-requisition "GtkRequisition"
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'gtk-requisition))

;;; ----------------------------------------------------------------------------
;;; GtkAllocation
;;; 
;;; typedef GdkRectangle GtkAllocation;
;;; 
;;; A GtkAllocation of a widget represents region which has been allocated to
;;; the widget by its parent. It is a subregion of its parents allocation. See
;;; the section called “Height-for-width Geometry Management” for more
;;; information.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-allocation "GtkAllocation"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'gtk-allocation))

;;; ----------------------------------------------------------------------------
;;; enum GtkTextDirection
;;; 
;;; typedef enum {
;;;   GTK_TEXT_DIR_NONE,
;;;   GTK_TEXT_DIR_LTR,
;;;   GTK_TEXT_DIR_RTL
;;; } GtkTextDirection;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTextDirection" gtk-text-direction
  (:export t
   :type-initializer "gtk_text_direction_get_type")
  (:none 0)
  (:ltr 1)
  (:rtl 2))

;;; ----------------------------------------------------------------------------
;;; enum GtkWidgetFlags
;;; 
;;; typedef enum {
;;;   GTK_TOPLEVEL             = 1 << 4,
;;;   GTK_NO_WINDOW            = 1 << 5,
;;;   GTK_REALIZED             = 1 << 6,
;;;   GTK_MAPPED               = 1 << 7,
;;;   GTK_VISIBLE              = 1 << 8,
;;;   GTK_SENSITIVE            = 1 << 9,
;;;   GTK_PARENT_SENSITIVE     = 1 << 10,
;;;   GTK_CAN_FOCUS            = 1 << 11,
;;;   GTK_HAS_FOCUS            = 1 << 12,
;;;   GTK_CAN_DEFAULT          = 1 << 13,
;;;   GTK_HAS_DEFAULT          = 1 << 14,
;;;   GTK_HAS_GRAB             = 1 << 15,
;;;   GTK_RC_STYLE             = 1 << 16,
;;;   GTK_COMPOSITE_CHILD      = 1 << 17,
;;; #ifndef GTK_DISABLE_DEPRECATED
;;;   GTK_NO_REPARENT          = 1 << 18,
;;; #endif
;;;   GTK_APP_PAINTABLE        = 1 << 19,
;;;   GTK_RECEIVES_DEFAULT     = 1 << 20,
;;;   GTK_DOUBLE_BUFFERED      = 1 << 21,
;;;   GTK_NO_SHOW_ALL          = 1 << 22
;;; } GtkWidgetFlags;
;;; 
;;; Tells about certain properties of the widget.
;;; 
;;; GTK_TOPLEVEL
;;;     widgets without a real parent, as there are GtkWindows and GtkMenus have
;;;     this flag set throughout their lifetime. Toplevel widgets always contain
;;;     their own GdkWindow.
;;; 
;;; GTK_NO_WINDOW
;;;     Indicative for a widget that does not provide its own GdkWindow. Visible
;;;     action (e.g. drawing) is performed on the parent's GdkWindow.
;;; 
;;; GTK_REALIZED
;;;     Set by gtk_widget_realize(), unset by gtk_widget_unrealize(). A realized
;;;     widget has an associated GdkWindow.
;;; 
;;; GTK_MAPPED
;;;     Set by gtk_widget_map(), unset by gtk_widget_unmap(). Only realized
;;;     widgets can be mapped. It means that gdk_window_show() has been called
;;;     on the widgets window(s).
;;; 
;;; GTK_VISIBLE
;;;     Set by gtk_widget_show(), unset by gtk_widget_hide(). Implies that a
;;;     widget will be mapped as soon as its parent is mapped.
;;; 
;;; GTK_SENSITIVE
;;;     Set and unset by gtk_widget_set_sensitive(). The sensitivity of a widget
;;;     determines whether it will receive certain events (e.g. button or key
;;;     presses). One premise for the widget's sensitivity is to have this flag
;;;     set.
;;; 
;;; GTK_PARENT_SENSITIVE
;;;     Set and unset by gtk_widget_set_sensitive() operations on the parents of
;;;     the widget. This is the second premise for the widget's sensitivity.
;;;     Once it has GTK_SENSITIVE and GTK_PARENT_SENSITIVE set, its state is
;;;     effectively sensitive. This is expressed (and can be examined) by the
;;;     GTK_WIDGET_IS_SENSITIVE macro.
;;; 
;;; GTK_CAN_FOCUS
;;;     Determines whether a widget is able to handle focus grabs.
;;; 
;;; GTK_HAS_FOCUS
;;;     Set by gtk_widget_grab_focus() for widgets that also have GTK_CAN_FOCUS
;;;     set. The flag will be unset once another widget grabs the focus.
;;; 
;;; GTK_CAN_DEFAULT
;;;     The widget is allowed to receive the default action via
;;;     gtk_widget_grab_default() and will reserve space to draw the default if
;;;     possible
;;; 
;;; GTK_HAS_DEFAULT
;;;     The widget currently is receiving the default action and should be drawn
;;;     appropriately if possible
;;; 
;;; GTK_HAS_GRAB
;;;     Set by gtk_grab_add(), unset by gtk_grab_remove(). It means that the
;;;     widget is in the grab_widgets stack, and will be the preferred one for
;;;     receiving events other than ones of cosmetic value.
;;; 
;;; GTK_RC_STYLE
;;;     Indicates that the widget's style has been looked up through the rc
;;;     mechanism. It does not imply that the widget actually had a style
;;;     defined through the rc mechanism.
;;; 
;;; GTK_COMPOSITE_CHILD
;;;     Indicates that the widget is a composite child of its parent;
;;;     see gtk_widget_push_composite_child(), gtk_widget_pop_composite_child().
;;; 
;;; GTK_NO_REPARENT
;;;     Unused since before GTK+ 1.2, will be removed in a future version.
;;; 
;;; GTK_APP_PAINTABLE
;;;     Set and unset by gtk_widget_set_app_paintable(). Must be set on widgets
;;;     whose window the application directly draws on, in order to keep GTK+
;;;     from overwriting the drawn stuff. See the section called “App-paintable
;;;     widgets” for a detailed description of this flag.
;;; 
;;; GTK_RECEIVES_DEFAULT
;;;     The widget when focused will receive the default action and have
;;;     GTK_HAS_DEFAULT set even if there is a different widget set as default.
;;; 
;;; GTK_DOUBLE_BUFFERED
;;;     Set and unset by gtk_widget_set_double_buffered(). Indicates that
;;;     exposes done on the widget should be double-buffered. See the section
;;;     called “Double buffering” for a detailed discussion of how
;;;     double-buffering works in GTK+ and why you may want to disable it for
;;;     special cases.
;;; 
;;; GTK_NO_SHOW_ALL
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkWidgetFlags" gtk-widget-flags
  (:export t
   :type-initializer "gtk_widget_flags_get_type")
  (:toplevel 16)
  (:no-window 32)
  (:realized 64)
  (:mapped 128)
  (:visible 256)
  (:sensitive 512)
  (:parent-sensitive 1024)
  (:can-focus 2048)
  (:has-focus 4096)
  (:can-default 8192)
  (:has-default 16384)
  (:has-grab 32768)
  (:rc-style 65536)
  (:composite-child 131072)
  (:no-reparent 262144)
  (:app-paintable 524288)
  (:receives-default 1048576)
  (:double-buffered 2097152)
  (:no-show-all 4194304))

;;; ----------------------------------------------------------------------------
;;; GtkWidget
;;; 
;;; typedef struct _GtkWidget GtkWidget;
;;; ----------------------------------------------------------------------------

(defcstruct %gtk-widget
  (:object %gtk-object)
  (:private-flags :uint16)
  (:state :uint8)
  (:saved-state :uint8)
  (:name (:pointer :char))
  (:style :pointer)
  (:requisition gtk-requisition-cstruct)
  (:allocation gtk-allocation-cstruct)
  (:window :pointer)
  (:parent :pointer))

;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkWidget" gtk-widget
  (:superclass gtk-object
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_widget_get_type")
  ((app-paintable
    gtk-widget-app-paintable
    "app-paintable" "gboolean" t t)
   (can-default
    gtk-widget-can-default
    "can-default" "gboolean" t t)
   (can-focus
    gtk-widget-can-focus
    "can-focus" "gboolean" t t)
   (composite-child
    gtk-widget-composite-child
    "composite-child" "gboolean" t nil)
   (double-buffered
    gtk-widget-double-buffered
    "double-buffered" "gboolean" t t)
   (events
    gtk-widget-events
    "events" "GdkEventMask" t t)
   (extension-events
    gtk-widget-extension-events
    "extension-events" "GdkExtensionMode" t t)
   (has-default
    gtk-widget-has-default
    "has-default" "gboolean" t t)
   (has-focus
    gtk-widget-has-focus
    "has-focus" "gboolean" t t)
   (has-tooltip
    gtk-widget-has-tooltip
    "has-tooltip" "gboolean" t t)
   (height-request
    gtk-widget-height-request
    "height-request" "gint" t t)
   (is-focus
    gtk-widget-is-focus
    "is-focus" "gboolean" t t)
   (name
    gtk-widget-name
    "name" "gchararray" t t)
   (no-show-all
    gtk-widget-no-show-all
    "no-show-all" "gboolean" t t)
   (parent
    gtk-widget-parent
    "parent" "GtkContainer" t t)
   (receives-default
    gtk-widget-receives-default
    "receives-default" "gboolean" t t)
   (sensitive
    gtk-widget-sensitive
    "sensitive" "gboolean" t t)
   (style
    gtk-widget-style
    "style" "GtkStyle" t t)
   (tooltip-markup
    gtk-widget-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text
    gtk-widget-tooltip-text
    "tooltip-text" "gchararray" t t)
   (visible
    gtk-widget-visible
    "visible" "gboolean" t t)
   (width-request
    gtk-widget-width-request
    "width-request" "gint" t t)
   (window
    gtk-widget-window
    "window" "GdkWindow" t nil)
   (:cffi parent-window
          gtk-widget-parent-window (g-object gdk-window)
          "gtk_widget_get_parent_window" "gtk_widget_set_parent_window")
   (:cffi toplevel
          gtk-widget-toplevel (g-object gtk-widget)
          "gtk_widget_get_toplevel" nil)
   (:cffi colormap
          gtk-widget-colormap (g-object gdk-colormap)
          "gtk_widget_get_colormap" "gtk_widget_set_colormap")
   (:cffi visual
          gtk-widget-visual (g-object gdk-visual)
          "gtk_widget_get_visual" nil)
   (:cffi modifier-style
          gtk-widget-modifier-style (g-object rc-style)
          "gtk_widget_get_modifier_style" "gtk_widget_modify_style")
   (:cffi pango-context
          gtk-widget-pango-context g-object
          "gtk_widget_get_pango_context" nil)
   (:cffi child-visible
          gtk-widget-child-visible :boolean
          "gtk_widget_get_child_visible" "gtk_widget_set_child_visible")
   (:cffi direction
          gtk-widget-direction gtk-text-direction
          "gtk_widget_get_direction" "gtk_widget_set_direction")
   (:cffi composite-name
          gtk-widget-composite-name
          (g-string :free-from-foreign t :free-to-foreign t)
          "gtk_widget_get_composite_name" "gtk_widget_set_composite_name")
   (:cffi redraw-on-allocate
          gtk-widget-redraw-on-allocate :boolean
          nil "gtk_widget_set_redraw_on_allocate")
   (:cffi accessible
          gtk-widget-accessible g-object
          "gtk_widget_get_accessible" nil)
   (:cffi tooltip-window
          gtk-widget-tooltip-window g-object
          "gtk_widget_get_tooltip_window" "gtk_widget_set_tooltip_window")))

;;; ----------------------------------------------------------------------------

(defun gtk-widget-state (widget)
  (convert-from-foreign (foreign-slot-value (pointer widget)
                                            '%gtk-widget :state)
                        'gtk-state-type))

(export 'gtk-widget-state)

(defun gtk-widget-saved-state (widget)
  (convert-from-foreign (foreign-slot-value (pointer widget)
                                            '%gtk-widget :saved-state)
                        'gtk-state-type))

(export 'gtk-widget-saved-state)

(defmacro gtk-widget-p-fn (type)
  (let ((name (intern (format nil "WIDGET-~A-P" (symbol-name type))
                      (find-package :gtk))))
    `(progn (defun ,name (widget)
              (member ,type (gtk-widget-flags widget)))
            (export ',name))))

(gtk-widget-p-fn :toplevel)
(gtk-widget-p-fn :no-window)
(gtk-widget-p-fn :realized)
(gtk-widget-p-fn :mapped)
(gtk-widget-p-fn :visible)
(gtk-widget-p-fn :sensitive)
(gtk-widget-p-fn :parent-sensitive)
(gtk-widget-p-fn :can-focus)
(gtk-widget-p-fn :has-focus)
(gtk-widget-p-fn :can-default)
(gtk-widget-p-fn :has-default)
(gtk-widget-p-fn :has-grab)
(gtk-widget-p-fn :rc-style)
(gtk-widget-p-fn :composite-child)
(gtk-widget-p-fn :no-reparent)
(gtk-widget-p-fn :app-paintable)
(gtk-widget-p-fn :receives-default)
(gtk-widget-p-fn :double-buffered)
(gtk-widget-p-fn :no-show-all)

;;; ---------------------------------------------------------------------------- 
;;; struct GtkWidgetClass
;;; 
;;; struct GtkWidgetClass {
;;;   GInitiallyUnownedClass parent_class;
;;; 
;;;   guint activate_signal;
;;; 
;;;   /* seldomly overidden */
;;;   void (*dispatch_child_properties_changed) (GtkWidget   *widget,
;;;                                              guint        n_pspecs,
;;;                                              GParamSpec **pspecs);
;;; 
;;;   /* basics */
;;;   void (* destroy)             (GtkWidget        *widget);
;;;   void (* show)                (GtkWidget        *widget);
;;;   void (* show_all)            (GtkWidget        *widget);
;;;   void (* hide)                (GtkWidget        *widget);
;;;   void (* map)                 (GtkWidget        *widget);
;;;   void (* unmap)               (GtkWidget        *widget);
;;;   void (* realize)             (GtkWidget        *widget);
;;;   void (* unrealize)           (GtkWidget        *widget);
;;;   void (* size_allocate)       (GtkWidget        *widget,
;;;                                 GtkAllocation    *allocation);
;;;   void (* state_changed)       (GtkWidget        *widget,
;;;                                 GtkStateType      previous_state);
;;;   void (* state_flags_changed) (GtkWidget        *widget,
;;;                                 GtkStateFlags     previous_state_flags);
;;;   void (* parent_set)          (GtkWidget        *widget,
;;;                                 GtkWidget        *previous_parent);
;;;   void (* hierarchy_changed)   (GtkWidget        *widget,
;;;                                 GtkWidget        *previous_toplevel);
;;;   void (* style_set)           (GtkWidget        *widget,
;;;                                 GtkStyle         *previous_style);
;;;   void (* direction_changed)   (GtkWidget        *widget,
;;;                                 GtkTextDirection  previous_direction);
;;;   void (* grab_notify)         (GtkWidget        *widget,
;;;                                 gboolean          was_grabbed);
;;;   void (* child_notify)        (GtkWidget        *widget,
;;;                                 GParamSpec       *pspec);
;;;   gboolean (* draw)            (GtkWidget        *widget,
;;;                                 cairo_t          *cr);
;;; 
;;;   /* size requests */
;;;   GtkSizeRequestMode (* get_request_mode)  (GtkWidget *widget)
;;; 
;;;   void  (* get_preferred_height)           (GtkWidget *widget,
;;;                                                        gint *minimum_height,
;;;                                                        gint *natural_height)
;;;   void  (* get_preferred_width_for_height) (GtkWidget *widget,
;;;                                                        gint  height,
;;;                                                        gint *minimum_width,
;;;                                                        gint *natural_width)
;;;   void  (* get_preferred_width)            (GtkWidget *widget,
;;;                                                        gint *minimum_width,
;;;                                                        gint *natural_width)
;;;   void  (* get_preferred_height_for_width) (GtkWidget *widget,
;;;                                                        gint  width,
;;;                                                        gint *minimum_height,
;;;                                                        gint *natural_height)
;;; 
;;;   /* Mnemonics */
;;;   gboolean (* mnemonic_activate)        (GtkWidget    *widget,
;;;                                          gboolean      group_cycling);
;;; 
;;;   /* explicit focus */
;;;   void     (* grab_focus)               (GtkWidget           *widget);
;;;   gboolean (* focus)                    (GtkWidget           *widget,
;;;                                          GtkDirectionType     direction);
;;; 
;;;   /* keyboard navigation */
;;;   void     (* move_focus)               (GtkWidget           *widget,
;;;                                          GtkDirectionType     direction);
;;;   gboolean (* keynav_failed)            (GtkWidget           *widget,
;;;                                          GtkDirectionType     direction);
;;; 
;;;   /* events */
;;;   gboolean (* event)              (GtkWidget           *widget,
;;;                                    GdkEvent            *event);
;;;   gboolean (* button_press_event) (GtkWidget           *widget,
;;;                                    GdkEventButton      *event);
;;;   gboolean (* button_release_event) (GtkWidget         *widget,
;;;                                    GdkEventButton      *event);
;;;   gboolean (* scroll_event)        (GtkWidget          *widget,
;;;                      GdkEventScroll      *event);
;;;   gboolean (* motion_notify_event) (GtkWidget         *widget,
;;;                      GdkEventMotion      *event);
;;;   gboolean (* delete_event)        (GtkWidget         *widget,
;;;                      GdkEventAny         *event);
;;;   gboolean (* destroy_event)        (GtkWidget         *widget,
;;;                      GdkEventAny         *event);
;;;   gboolean (* key_press_event)        (GtkWidget         *widget,
;;;                      GdkEventKey         *event);
;;;   gboolean (* key_release_event) (GtkWidget         *widget,
;;;                      GdkEventKey         *event);
;;;   gboolean (* enter_notify_event) (GtkWidget         *widget,
;;;                      GdkEventCrossing    *event);
;;;   gboolean (* leave_notify_event) (GtkWidget         *widget,
;;;                      GdkEventCrossing    *event);
;;;   gboolean (* configure_event)        (GtkWidget         *widget,
;;;                      GdkEventConfigure   *event);
;;;   gboolean (* focus_in_event)        (GtkWidget         *widget,
;;;                      GdkEventFocus       *event);
;;;   gboolean (* focus_out_event)        (GtkWidget         *widget,
;;;                      GdkEventFocus       *event);
;;;   gboolean (* map_event)        (GtkWidget         *widget,
;;;                      GdkEventAny         *event);
;;;   gboolean (* unmap_event)        (GtkWidget         *widget,
;;;                      GdkEventAny         *event);
;;;   gboolean (* property_notify_event) (GtkWidget         *widget,
;;;                      GdkEventProperty    *event);
;;;   gboolean (* selection_clear_event) (GtkWidget         *widget,
;;;                      GdkEventSelection   *event);
;;;   gboolean (* selection_request_event) (GtkWidget         *widget,
;;;                      GdkEventSelection   *event);
;;;   gboolean (* selection_notify_event) (GtkWidget         *widget,
;;;                      GdkEventSelection   *event);
;;;   gboolean (* proximity_in_event) (GtkWidget         *widget,
;;;                      GdkEventProximity   *event);
;;;   gboolean (* proximity_out_event) (GtkWidget         *widget,
;;;                      GdkEventProximity   *event);
;;;   gboolean (* visibility_notify_event) (GtkWidget         *widget,
;;;                      GdkEventVisibility  *event);
;;;   gboolean (* window_state_event) (GtkWidget         *widget,
;;;                      GdkEventWindowState *event);
;;;   gboolean (* damage_event)             (GtkWidget           *widget,
;;;                                          GdkEventExpose      *event);
;;;   gboolean (* grab_broken_event)        (GtkWidget           *widget,
;;;                                          GdkEventGrabBroken  *event);
;;; 
;;;   /* selection */
;;;   void     (* selection_get)       (GtkWidget          *widget,
;;;                     GtkSelectionData   *selection_data,
;;;                     guint               info,
;;;                     guint               time_);
;;;   void     (* selection_received)  (GtkWidget          *widget,
;;;                     GtkSelectionData   *selection_data,
;;;                     guint               time_);
;;; 
;;;   /* Source side drag signals */
;;;   void     (* drag_begin)          (GtkWidget         *widget,
;;;                     GdkDragContext     *context);
;;;   void     (* drag_end)               (GtkWidget           *widget,
;;;                     GdkDragContext     *context);
;;;   void     (* drag_data_get)       (GtkWidget          *widget,
;;;                     GdkDragContext     *context,
;;;                     GtkSelectionData   *selection_data,
;;;                     guint               info,
;;;                     guint               time_);
;;;   void     (* drag_data_delete)    (GtkWidget          *widget,
;;;                     GdkDragContext     *context);
;;; 
;;;   /* Target side drag signals */
;;;   void     (* drag_leave)          (GtkWidget          *widget,
;;;                     GdkDragContext     *context,
;;;                     guint               time_);
;;;   gboolean (* drag_motion)         (GtkWidget           *widget,
;;;                     GdkDragContext     *context,
;;;                     gint                x,
;;;                     gint                y,
;;;                     guint               time_);
;;;   gboolean (* drag_drop)           (GtkWidget           *widget,
;;;                     GdkDragContext     *context,
;;;                     gint                x,
;;;                     gint                y,
;;;                     guint               time_);
;;;   void     (* drag_data_received)  (GtkWidget          *widget,
;;;                     GdkDragContext     *context,
;;;                     gint                x,
;;;                     gint                y,
;;;                     GtkSelectionData   *selection_data,
;;;                     guint               info,
;;;                     guint               time_);
;;;   gboolean (* drag_failed)         (GtkWidget          *widget,
;;;                                     GdkDragContext     *context,
;;;                                     GtkDragResult       result);
;;; 
;;;   /* Signals used only for keybindings */
;;;   gboolean (* popup_menu)          (GtkWidget          *widget);
;;; 
;;;   /* If a widget has multiple tooltips/whatsthis, it should show the
;;;    * one for the current focus location, or if that doesn't make
;;;    * sense, should cycle through them showing each tip alongside
;;;    * whatever piece of the widget it applies to.
;;;    */
;;;   gboolean (* show_help)           (GtkWidget          *widget,
;;;                                     GtkWidgetHelpType   help_type);
;;; 
;;;   /* accessibility support
;;;    */
;;;   AtkObject *  (* get_accessible)     (GtkWidget *widget);
;;; 
;;;   void         (* screen_changed)     (GtkWidget *widget,
;;;                                        GdkScreen *previous_screen);
;;;   gboolean     (* can_activate_accel) (GtkWidget *widget,
;;;                                        guint      signal_id);
;;; 
;;; 
;;;   void         (* composited_changed) (GtkWidget *widget);
;;; 
;;;   gboolean     (* query_tooltip)      (GtkWidget *widget,
;;;                                                   gint     x,
;;;                                                   gint     y,
;;;                                                   gboolean keyboard_tooltip,
;;;                                                   GtkTooltip *tooltip);
;;; 
;;;   void         (* compute_expand)     (GtkWidget  *widget,
;;;                                        gboolean   *hexpand_p,
;;;                                        gboolean   *vexpand_p);
;;; 
;;;   void         (* adjust_size_request)    (GtkWidget        *widget,
;;;                                            GtkOrientation    orientation,
;;;                                            gint             *minimum_size,
;;;                                            gint             *natural_size);
;;;   void         (* adjust_size_allocation) (GtkWidget        *widget,
;;;                                            GtkOrientation    orientation,
;;;                                            gint             *minimum_size,
;;;                                            gint             *natural_size,
;;;                                            gint             *allocated_pos,
;;;                                            gint             *allocated_size)
;;; 
;;;   void         (* style_updated)          (GtkWidget *widget);
;;; };
;;; 
;;; GInitiallyUnownedClass parent_class;
;;;     The object class structure needs to be the first element in the widget
;;;     class structure in order for the class mechanism to work correctly.
;;;     This allows a GtkWidgetClass pointer to be cast to a GObjectClass
;;;     pointer.
;;; 
;;; guint activate_signal;
;;;     The signal to emit when a widget of this class is activated,
;;;     gtk_widget_activate() handles the emission. Implementation of this
;;;     signal is optional.
;;; 
;;; dispatch_child_properties_changed ()
;;; destroy ()
;;; show ()
;;; show_all ()
;;; hide ()
;;; map ()
;;; unmap ()
;;; realize ()
;;; unrealize ()
;;; size_allocate ()
;;; state_changed ()
;;; state_flags_changed ()
;;; parent_set ()
;;; hierarchy_changed ()
;;; style_set ()
;;; direction_changed ()
;;; grab_notify ()
;;; child_notify ()
;;; draw ()
;;; 
;;; get_request_mode ()
;;;     This allows a widget to tell its parent container whether it prefers to
;;;     be allocated in GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH or
;;;     GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT mode.
;;;     GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH means the widget prefers to have
;;;     GtkWidgetClass.get_preferred_width() called and then
;;;     GtkWidgetClass.get_preferred_height_for_width().
;;;     GTK_SIZE_REQUEST_CONSTANT_SIZE disables any height-for-width or
;;;     width-for-height geometry management for a said widget and is the
;;;     default return. It's important to note (as described below) that any
;;;     widget which trades height-for-width or width-for-height must respond
;;;     properly to both of the virtual methods
;;;     GtkWidgetClass.get_preferred_height_for_width() and
;;;     GtkWidgetClass.get_preferred_width_for_height() since it might be
;;;     queried in either GtkSizeRequestMode by its parent container.
;;; 
;;; get_preferred_height ()
;;;     This is called by containers to obtain the minimum and natural height
;;;     of a widget. A widget that does not actually trade any height for width
;;;     or width for height only has to implement these two virtual methods
;;;     (GtkWidgetClass.get_preferred_width() and
;;;     GtkWidgetClass.get_preferred_height()).
;;; 
;;; get_preferred_width_for_height ()
;;;     This is analogous to GtkWidgetClass.get_preferred_height_for_width()
;;;     except that it operates in the oposite orientation. It's rare that a
;;;     widget actually does GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT requests but
;;;     this can happen when, for example, a widget or container gets additional
;;;     columns to compensate for a smaller allocated height.
;;; 
;;; get_preferred_width ()
;;;     This is called by containers to obtain the minimum and natural width of
;;;     a widget. A widget will never be allocated a width less than its minimum
;;;     and will only ever be allocated a width greater than the natural width
;;;     once all of the said widget's siblings have received their natural
;;;     widths. Furthermore, a widget will only ever be allocated a width
;;;     greater than its natural width if it was configured to receive extra
;;;     expand space from its parent container.
;;; 
;;; get_preferred_height_for_width ()
;;;     This is similar to GtkWidgetClass.get_preferred_height() except that it
;;;     is passed a contextual width to request height for. By implementing
;;;     this virtual method it is possible for a GtkLabel to tell its parent
;;;     how much height would be required if the label were to be allocated a
;;;     said width.
;;; 
;;; mnemonic_activate ()
;;; grab_focus ()
;;; focus ()
;;; move_focus ()
;;; keynav_failed ()
;;; event ()
;;; button_press_event ()
;;; button_release_event ()
;;; scroll_event ()
;;; motion_notify_event ()
;;; delete_event ()
;;; destroy_event ()
;;; key_press_event ()
;;; key_release_event ()
;;; enter_notify_event ()
;;; leave_notify_event ()
;;; configure_event ()
;;; focus_in_event ()
;;; focus_out_event ()
;;; map_event ()
;;; unmap_event ()
;;; property_notify_event ()
;;; selection_clear_event ()
;;; selection_request_event ()
;;; selection_notify_event ()
;;; proximity_in_event ()
;;; proximity_out_event ()
;;; visibility_notify_event ()
;;; window_state_event ()
;;; damage_event ()
;;; grab_broken_event ()
;;; selection_get ()
;;; selection_received ()
;;; drag_begin ()
;;; drag_end ()
;;; drag_data_get ()
;;; drag_data_delete ()
;;; drag_leave ()
;;; drag_motion ()
;;; drag_drop ()
;;; drag_data_received ()
;;; drag_failed ()
;;; popup_menu ()
;;; show_help ()
;;; get_accessible ()
;;; screen_changed ()
;;; can_activate_accel ()
;;; composited_changed ()
;;; query_tooltip ()
;;; compute_expand ()
;;; 
;;; adjust_size_request ()
;;;     Convert an initial size request from a widget's GtkSizeRequest virtual
;;;     method implementations into a size request to be used by parent
;;;     containers in laying out the widget. adjust_size_request adjusts from
;;;     a child widget's original request to what a parent container should use
;;;     for layout. The for_size argument will be -1 if the request should not
;;;     be for a particular size in the opposing orientation, i.e. if the
;;;     request is not height-for-width or width-for-height. If for_size is
;;;     greater than -1, it is the proposed allocation in the opposing
;;;     orientation that we need the request for. Implementations of
;;;     adjust_size_request should chain up to the default implementation,
;;;     which applies GtkWidget's margin properties and imposes any values from
;;;     gtk_widget_set_size_request(). Chaining up should be last, after your
;;;     subclass adjusts the request, so GtkWidget can apply constraints and
;;;     add the margin properly.
;;; 
;;; adjust_size_allocation ()
;;;     Convert an initial size allocation assigned by a GtkContainer using
;;;     gtk_widget_size_allocate(), into an actual size allocation to be used
;;;     by the widget. adjust_size_allocation adjusts to a child widget's
;;;     actual allocation from what a parent container computed for the child.
;;;     The adjusted allocation must be entirely within the original allocation.
;;;     In any custom implementation, chain up to the default GtkWidget
;;;     implementation of this method, which applies the margin and alignment
;;;     properties of GtkWidget. Chain up before performing your own adjustments
;;;     so your own adjustments remove more allocation after the GtkWidget base
;;;     class has already removed margin and alignment. The natural size passed
;;;     in should be adjusted in the same way as the allocated size, which
;;;     allows adjustments to perform alignments or other changes based on
;;;     natural size.
;;; 
;;; style_updated ()
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; GtkCallback ()
;;; 
;;; void (*GtkCallback) (GtkWidget *widget, gpointer data);
;;; 
;;; The type of the callback functions used for e.g. iterating over the
;;; children of a container, see gtk_container_foreach().
;;; 
;;; widget :
;;;     the widget to operate on
;;; 
;;; data :
;;;     user-supplied data
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; GtkSelectionData
;;; 
;;; typedef struct {
;;;   GdkAtom       selection;
;;;   GdkAtom       target;
;;;   GdkAtom       type;
;;;   gint          format;
;;;   guchar       *data;
;;;   gint          length;
;;;   GdkDisplay   *display;
;;; } GtkSelectionData;
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-selection-data "GtkSelectionData"
  (selection gdk-atom-as-string :initform nil)
  (target gdk-atom-as-string :initform nil)
  (type gdk-atom-as-string :initform nil)
  (format :int :initform 0)
  (data :pointer :initform (null-pointer))
  (length :int :initform 0)
  (display (g-object gdk-display) :initform nil))

(export (boxed-related-symbols 'gtk-selection-data))

;;; ----------------------------------------------------------------------------
;;; struct GtkWidgetAuxInfo
;;; 
;;; struct GtkWidgetAuxInfo {
;;;   gint width;
;;;   gint height;
;;; 
;;;   guint   halign : 4;
;;;   guint   valign : 4;
;;; 
;;;   GtkBorder margin;
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkWidgetHelpType
;;; 
;;; typedef enum {
;;;   GTK_WIDGET_HELP_TOOLTIP,
;;;   GTK_WIDGET_HELP_WHATS_THIS
;;; } GtkWidgetHelpType;
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWidgetHelpType" gtk-widget-help-type
  (:export t
   :type-initializer "gtk_widget_help_type_get_type")
  (:tooltip 0)
  (:whats-this 1))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_new ()
;;; 
;;; GtkWidget *gtk_widget_new (GType type,
;;;                            const gchar *first_property_name,
;;;                            ...);
;;; 
;;; This is a convenience function for creating a widget and setting its
;;; properties in one go. For example you might write:
;;; gtk_widget_new (GTK_TYPE_LABEL, "label", "Hello World", "xalign", 0.0, NULL)
;;; to create a left-aligned label. Equivalent to g_object_new(), but returns a
;;; widget so you don't have to cast the object yourself.
;;; 
;;; type :
;;;     type ID of the widget to create
;;; 
;;; first_property_name :
;;;     name of first property to set
;;; 
;;; ... :
;;;     value of first property, followed by more properties, NULL-terminated
;;; 
;;; Returns :
;;;     a new GtkWidget of type widget_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroy ()
;;; 
;;; void gtk_widget_destroy (GtkWidget *widget)
;;; 
;;; Destroys a widget.
;;; 
;;; When a widget is destroyed, it will break any references it holds to other
;;; objects. If the widget is inside a container, the widget will be removed
;;; from the container. If the widget is a toplevel (derived from GtkWindow),
;;; it will be removed from the list of toplevels, and the reference GTK+ holds
;;; to it will be removed. Removing a widget from its container or the list of
;;; toplevels results in the widget being finalized, unless you've added
;;; additional references to the widget with g_object_ref().
;;; 
;;; In most cases, only toplevel widgets (windows) require explicit destruction,
;;; because when you destroy a toplevel its children will be destroyed as well.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_destroy" gtk-widget-destroy) :void
  (widget g-object))

(export 'gtk-widget-destroy)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_in_destruction ()
;;; 
;;; gboolean gtk_widget_in_destruction (GtkWidget *widget)
;;; 
;;; Returns whether the widget is currently being destroyed. This information
;;; can sometimes be used to avoid doing unnecessary work.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is being destroyed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroyed ()
;;; 
;;; void gtk_widget_destroyed (GtkWidget *widget, GtkWidget **widget_pointer);
;;; 
;;; This function sets *widget_pointer to NULL if widget_pointer != NULL. It's
;;; intended to be used as a callback connected to the "destroy" signal of a
;;; widget. You connect gtk_widget_destroyed() as a signal handler, and pass
;;; the address of your widget variable as user data. Then when the widget is
;;; destroyed, the variable will be set to NULL. Useful for example to avoid
;;; multiple copies of the same dialog.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; widget_pointer :
;;;     address of a variable that contains widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unparent ()
;;; 
;;; void gtk_widget_unparent (GtkWidget *widget)
;;; 
;;; This function is only for use in widget implementations. Should be called
;;; by implementations of the remove method on GtkContainer, to dissociate a
;;; child from the container.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_unparent" gtk-widget-unparent) :void
  (widget g-object))

(export 'gtk-widget-unparent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show ()
;;; 
;;; void gtk_widget_show (GtkWidget *widget)
;;; 
;;; Flags a widget to be displayed. Any widget that isn't shown will not appear
;;; on the screen. If you want to show all the widgets in a container, it's
;;; easier to call gtk_widget_show_all() on the container, instead of
;;; individually showing the widgets.
;;; 
;;; Remember that you have to show the containers containing a widget, in
;;; addition to the widget itself, before it will appear onscreen.
;;; 
;;; When a toplevel container is shown, it is immediately realized and mapped;
;;; other shown widgets are realized and mapped when their toplevel container
;;; is realized and mapped.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_show" %gtk-widget-show) :void
  (widget g-object))

;; The Lisp implementation combines the functions gtk-widget-show and
;; gtk-widget-show-all. The standard is to call gtk-widget-show-all.

(defun gtk-widget-show (widget &key (all t))
  (if all
      (gtk-widget-show-all widget)
      (%gtk-widget-show widget)))

(export 'gtk-widget-show)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show_now ()
;;; 
;;; void gtk_widget_show_now (GtkWidget *widget)
;;; 
;;; Shows a widget. If the widget is an unmapped toplevel widget (i.e. a
;;; GtkWindow that has not yet been shown), enter the main loop and wait for
;;; the window to actually be mapped. Be careful; because the main loop is
;;; running, anything can happen during this function.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_show_now" gtk-widget-show-now) :void
  (widget g-object))

(export 'gtk-widget-show-now)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide ()
;;; 
;;; void gtk_widget_hide (GtkWidget *widget)
;;; 
;;; Reverses the effects of gtk_widget_show(), causing the widget to be hidden
;;; (invisible to the user).
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_hide" %gtk-widget-hide) :void
  (widget g-object))

;; gtk-widget-hide-all seems to be not documented in the GTK Manual.

(defcfun ("gtk_widget_hide_all" gtk-widget-hide-all) :void
  (widget g-object))

(export 'gtk-widget-hide-all)

;; TODO: Combines gtk-widget-hide and gtk-widget-hide-all.
;; But gtk_widget_hide_all is not documented. Consider to cut this out. 

(defun gtk-widget-hide (widget &key (all t))
  (if all
      (gtk-widget-hide-all widget)
      (%gtk-widget-hide widget)))

(export 'gtk-widget-hide)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show_all ()
;;; 
;;; void gtk_widget_show_all (GtkWidget *widget)
;;; 
;;; Recursively shows a widget, and any child widgets (if the widget is a
;;; container).
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_show_all" gtk-widget-show-all) :void
  (widget g-object))

(export 'gtk-widget-show-all)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_map ()
;;; 
;;; void gtk_widget_map (GtkWidget *widget)
;;; 
;;; This function is only for use in widget implementations. Causes a widget
;;; to be mapped if it isn't already.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_map" gtk-widget-map) :void
  (widget g-object))

(export 'gtk-widget-map)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unmap ()
;;; 
;;; void gtk_widget_unmap (GtkWidget *widget)
;;; 
;;; This function is only for use in widget implementations. Causes a widget to
;;; be unmapped if it's currently mapped.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_unmap" gtk-widget-unmap ) :void
  (widget g-object))

(export 'gtk-widget-unmap)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_realize ()
;;; 
;;; void gtk_widget_realize (GtkWidget *widget)
;;; 
;;; Creates the GDK (windowing system) resources associated with a widget. For
;;; example, widget->window will be created when a widget is realized. Normally
;;; realization happens implicitly; if you show a widget and all its parent
;;; containers, then the widget will be realized and mapped automatically.
;;; 
;;; Realizing a widget requires all the widget's parent widgets to be realized;
;;; calling gtk_widget_realize() realizes the widget's parents in addition to
;;; widget itself. If a widget is not yet inside a toplevel window when you
;;; realize it, bad things will happen.
;;; 
;;; This function is primarily used in widget implementations, and isn't very
;;; useful otherwise. Many times when you think you might need it, a better
;;; approach is to connect to a signal that will be called after the widget is
;;; realized automatically, such as "draw". Or simply g_signal_connect() to the
;;; "realize" signal.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_realize" gtk-widget-realize) :void
  (width g-object))

(export 'gtk-widget-realize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unrealize ()
;;; 
;;; void gtk_widget_unrealize(GtkWidget *widget)
;;; 
;;; This function is only useful in widget implementations. Causes a widget to
;;; be unrealized (frees all GDK resources associated with the widget, such as
;;; widget->window).
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_unrealize" gtk-widget-unrealize) :void
  (width g-object))

(export 'gtk-widget-unrealize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_draw ()
;;; 
;;; void gtk_widget_draw (GtkWidget *widget,cairo_t *cr);
;;; 
;;; Draws widget to cr. The top left corner of the widget will be drawn to the
;;; currently set origin point of cr.
;;; 
;;; You should pass a cairo context as cr argument that is in an original state.
;;; Otherwise the resulting drawing is undefined. For example changing the
;;; operator using cairo_set_operator() or the line width using
;;; cairo_set_line_width() might have unwanted side effects. You may however
;;; change the context's transform matrix - like with cairo_scale(),
;;; cairo_translate() or cairo_set_matrix() and clip region with cairo_clip()
;;; prior to calling this function. Also, it is fine to modify the context with
;;; cairo_save() and cairo_push_group() prior to calling this function.
;;; 
;;; Note
;;; 
;;; Special purpose widgets may contain special code for rendering to the
;;; screen and might appear differently on screen and when rendered using
;;; gtk_widget_draw().
;;; 
;;; widget :
;;;     the widget to draw. It must be drawable (see gtk_widget_is_drawable())
;;;     and a size must have been allocated.
;;; 
;;; cr :
;;;     a cairo context to draw to
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw ()
;;; 
;;; void gtk_widget_queue_draw (GtkWidget *widget)
;;; 
;;; Equivalent to calling gtk_widget_queue_draw_area() for the entire area of
;;; a widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_queue_draw" gtk-widget-queue-draw) :void
  (widget (g-object gtk-widget)))

(export 'gtk-widget-queue-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_resize ()
;;; 
;;; void gtk_widget_queue_resize (GtkWidget *widget)
;;; 
;;; This function is only for use in widget implementations. Flags a widget to
;;; have its size renegotiated; should be called when a widget for some reason
;;; has a new size request. For example, when you change the text in a GtkLabel,
;;; GtkLabel queues a resize to ensure there's enough space for the new text.
;;; 
;;; Note
;;; 
;;; You cannot call gtk_widget_queue_resize() on a widget from inside its
;;; implementation of the GtkWidgetClass::size_allocate virtual method. Calls
;;; to gtk_widget_queue_resize() from inside GtkWidgetClass::size_allocate will
;;; be silently ignored.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_queue_resize" gtk-widget-queue-resize) :void
  (widget (g-object gtk-widget)))

(export 'gtk-widget-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_resize_no_redraw ()
;;; 
;;; void gtk_widget_queue_resize_no_redraw (GtkWidget *widget)
;;; 
;;; This function works like gtk_widget_queue_resize(), except that the widget
;;; is not invalidated.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_queue_resize_no_redraw" gtk-widget-queue-resize-no-redraw)
    :void
  (widget (g-object gtk-widget)))

(export 'gtk-widget-queue-resize-no-redraw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_request ()
;;; 
;;; void gtk_widget_size_request (GtkWidget *widget,
;;;                               GtkRequisition *requisition);
;;; 
;;; Warning
;;; 
;;; gtk_widget_size_request has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_widget_get_preferred_size()
;;; instead.
;;; 
;;; This function is typically used when implementing a GtkContainer subclass.
;;; Obtains the preferred size of a widget. The container uses this information
;;; to arrange its child widgets and decide what size allocations to give them
;;; with gtk_widget_size_allocate().
;;; 
;;; You can also call this function from an application, with some caveats.
;;; Most notably, getting a size request requires the widget to be associated
;;; with a screen, because font information may be needed. Multihead-aware
;;; applications should keep this in mind.
;;; 
;;; Also remember that the size request is not necessarily the size a widget
;;; will actually be allocated.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; requisition :
;;;     a GtkRequisition to be filled in
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_size_request" gtk-widget-size-request) :void
  (widget g-object)
  (requisition (g-boxed-foreign gtk-requisition)))

(export 'gtk-widget-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_requisition ()
;;; 
;;; void gtk_widget_get_child_requisition (GtkWidget *widget,
;;;                                        GtkRequisition *requisition);
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_child_requisition has been deprecated since version 3.0 and
;;; should not be used in newly-written code.
;;; Use gtk_widget_get_preferred_size() instead.
;;; 
;;; This function is only for use in widget implementations. Obtains
;;; widget->requisition, unless someone has forced a particular geometry on the
;;; widget (e.g. with gtk_widget_set_size_request()), in which case it returns
;;; that geometry instead of the widget's requisition.
;;; 
;;; This function differs from gtk_widget_size_request() in that it retrieves
;;; the last size request value from widget->requisition, while
;;; gtk_widget_size_request() actually calls the "size_request" method on
;;; widget to compute the size request and fill in widget->requisition, and
;;; only then returns widget->requisition.
;;; 
;;; Because this function does not call the "size_request" method, it can only
;;; be used when you know that widget->requisition is up-to-date, that is,
;;; gtk_widget_size_request() has been called since the last time a resize was
;;; queued. In general, only container implementations have this information;
;;; applications should use gtk_widget_size_request().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; requisition :
;;;     a GtkRequisition to be filled in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_allocate ()
;;; 
;;; void gtk_widget_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
;;; 
;;; This function is only used by GtkContainer subclasses, to assign a size and
;;; position to their child widgets.
;;; 
;;; In this function, the allocation may be adjusted. It will be forced to a
;;; 1x1 minimum size, and the adjust_size_allocation virtual method on the child
;;; will be used to adjust the allocation. Standard adjustments include removing
;;; the widget's margins, and applying the widget's "halign" and "valign"
;;; properties.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; allocation :
;;;     position and size to be allocated to widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_accelerator ()
;;; 
;;; void gtk_widget_add_accelerator (GtkWidget *widget,
;;;                                            const gchar *accel_signal,
;;;                                            GtkAccelGroup *accel_group,
;;;                                            guint accel_key,
;;;                                            GdkModifierType accel_mods,
;;;                                            GtkAccelFlags accel_flags)
;;; 
;;; Installs an accelerator for this widget in accel_group that causes
;;; accel_signal to be emitted if the accelerator is activated. The accel_group
;;; needs to be added to the widget's toplevel via gtk_window_add_accel_group(),
;;; and the signal must be of type G_RUN_ACTION. Accelerators added through this
;;; function are not user changeable during runtime. If you want to support
;;; accelerators that can be changed by the user, use gtk_accel_map_add_entry()
;;; and gtk_widget_set_accel_path() or gtk_menu_item_set_accel_path() instead.
;;; 
;;; widget :
;;;     widget to install an accelerator on
;;; 
;;; accel_signal :
;;;     widget signal to emit on accelerator activation
;;; 
;;; accel_group :
;;;     accel group for this widget, added to its toplevel
;;; 
;;; accel_key :
;;;     GDK keyval of the accelerator
;;; 
;;; accel_mods :
;;;     modifier key combination of the accelerator
;;; 
;;; accel_flags :
;;;     flag accelerators, e.g. GTK_ACCEL_VISIBLE
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_add_accelerator" gtk-widget-add-accelerator) :void
  (widget g-object)
  (accel-signal :string)
  (accel-group g-object)
  (accel-key :uint)
  (accel-mods gdk-modifier-type)
  (accel-flags gtk-accel-flags))

(export 'gtk-widget-add-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_accelerator ()
;;; 
;;; gboolean gtk_widget_remove_accelerator (GtkWidget *widget,
;;;                                         GtkAccelGroup *accel_group,
;;;                                         guint accel_key,
;;;                                         GdkModifierType accel_mods)
;;; 
;;; Removes an accelerator from widget, previously installed with
;;; gtk_widget_add_accelerator().
;;; 
;;; widget :
;;;     widget to install an accelerator on
;;; 
;;; accel_group :
;;;     accel group for this widget
;;; 
;;; accel_key :
;;;     GDK keyval of the accelerator
;;; 
;;; accel_mods :
;;;     modifier key combination of the accelerator
;;; 
;;; Returns :
;;;     whether an accelerator was installed and could be removed
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_remove_accelerator" gtk-widget-remove-accelerator) :void
  (widget g-object)
  (accel-group g-object)
  (accel-key :uint)
  (accel-mods gdk-modifier-type))

(export 'gtk-widget-remove-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_accel_path ()
;;; 
;;; void gtk_widget_set_accel_path (GtkWidget *widget,
;;;                                 const gchar *accel_path,
;;;                                 GtkAccelGroup *accel_group)
;;; 
;;; Given an accelerator group, accel_group, and an accelerator path,
;;; accel_path, sets up an accelerator in accel_group so whenever the key
;;; binding that is defined for accel_path is pressed, widget will be activated.
;;; This removes any accelerators (for any accelerator group) installed by
;;; previous calls to gtk_widget_set_accel_path(). Associating accelerators with
;;; paths allows them to be modified by the user and the modifications to be
;;; saved for future use. (See gtk_accel_map_save().)
;;; 
;;; This function is a low level function that would most likely be used by a
;;; menu creation system like GtkUIManager. If you use GtkUIManager, setting up
;;; accelerator paths will be done automatically.
;;; 
;;; Even when you you aren't using GtkUIManager, if you only want to set up
;;; accelerators on menu items gtk_menu_item_set_accel_path() provides a
;;; somewhat more convenient interface.
;;; 
;;; Note that accel_path string will be stored in a GQuark. Therefore, if you
;;; pass a static string, you can save some memory by interning it first
;;; with g_intern_static_string().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; accel_path :
;;;     path used to look up the accelerator. [allow-none]
;;; 
;;; accel_group :
;;;     a GtkAccelGroup. [allow-none]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_accel_path" gtk-widget-set-accel-path) :void
  (widget g-object)
  (accel-path :string)
  (accel-group g-object))

(export 'gtk-widget-set-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_accel_closures ()
;;; 
;;; GList * gtk_widget_list_accel_closures (GtkWidget *widget);
;;; 
;;; Lists the closures used by widget for accelerator group connections with
;;; gtk_accel_group_connect_by_path() or gtk_accel_group_connect(). The
;;; closures can be used to monitor accelerator changes on widget, by connecting
;;; to the GtkAccelGroup::accel-changed signal of the GtkAccelGroup of a closure
;;; which can be found out with gtk_accel_group_from_accel_closure().
;;; 
;;; widget :
;;;     widget to list accelerator closures for
;;; 
;;; Returns :
;;;     a newly allocated GList of closures.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_can_activate_accel ()
;;; 
;;; gboolean gtk_widget_can_activate_accel (GtkWidget *widget, guint signal_id)
;;; 
;;; Determines whether an accelerator that activates the signal identified by
;;; signal_id can currently be activated. This is done by emitting the
;;; "can-activate-accel" signal on widget; if the signal isn't overridden by a
;;; handler or in a derived widget, then the default check is that the widget
;;; must be sensitive, and the widget and all its ancestors mapped.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; signal_id :
;;;     the ID of a signal installed on widget
;;; 
;;; Returns :
;;;     TRUE if the accelerator can be activated.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_can_activate_accel" %gtk-widget-can-activate-accel)
    :boolean
  (widget g-object)
  (signal-id :uint))

(defun gtk-widget-can-activate-accel (widget signal)
  (when (stringp signal)
    (setf signal (g-signal-lookup signal (g-type-from-instance widget))))
  (%gtk-widget-can-activate-accel widget signal))

(export 'gtk-widget-can-activate-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_event ()
;;; 
;;; gboolean gtk_widget_event (GtkWidget *widget, GdkEvent *event)
;;; 
;;; Rarely-used function. This function is used to emit the event signals on a
;;; widget (those signals should never be emitted without using this function
;;; to do so). If you want to synthesize an event though, don't use this
;;; function; instead, use gtk_main_do_event() so the event will behave as if
;;; it were in the event queue. Don't synthesize expose events; instead, use
;;; gdk_window_invalidate_rect() to invalidate a region of the window.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; event :
;;;     a GdkEvent
;;; 
;;; Returns :
;;;     return from the event signal emission (TRUE if the event was handled)
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_event" gtk-widget-event) :boolean
  (widget (g-object gtk-widget))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-widget-event)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate ()
;;; 
;;; gboolean gtk_widget_activate (GtkWidget *widget)
;;; 
;;; For widgets that can be "activated" (buttons, menu items, etc.) this
;;; function activates them. Activation is what happens when you press Enter on
;;; a widget during key navigation. If widget isn't activatable, the function
;;; returns FALSE.
;;; 
;;; widget :
;;;     a GtkWidget that's activatable
;;; 
;;; Returns :
;;;     TRUE if the widget was activatable
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_activate" gtk-widget-activate) :boolean
  (widget g-object))

(export 'gtk-widget-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reparent ()
;;; 
;;; void gtk_widget_reparent (GtkWidget *widget, GtkWidget *new_parent)
;;; 
;;; Moves a widget from one GtkContainer to another, handling reference count
;;; issues to avoid destroying the widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; new_parent :
;;;     a GtkContainer to move the widget into
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_reparent" gtk-widget-reparent) :void
  (widget g-object)
  (new-parent g-object))

(export 'gtk-widget-reparent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_intersect ()
;;; 
;;; gboolean gtk_widget_intersect (GtkWidget *widget,
;;;                                const GdkRectangle *area,
;;;                                GdkRectangle *intersection)
;;; 
;;; Computes the intersection of a widget's area and area, storing the
;;; intersection in intersection, and returns TRUE if there was an intersection.
;;; intersection may be NULL if you're only interested in whether there was an
;;; intersection.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; area :
;;;     a rectangle
;;; 
;;; intersection :
;;;     rectangle to store intersection of widget and area
;;; 
;;; Returns :
;;;     TRUE if there was an intersection
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_intersect" %gtk-widget-intersect) :boolean
  (widget g-object)
  (area (g-boxed-foreign gdk-rectangle))
  (intersection (g-boxed-foreign gdk-rectangle)))

(defun gtk-widget-intersect (widget area)
  (let ((intersection (make-gdk-rectangle)))
    (when (%gtk-widget-intersect widget area intersection)
      intersection)))

(export 'gtk-widget-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_focus ()
;;; 
;;; gboolean gtk_widget_is_focus (GtkWidget *widget)
;;; 
;;; Determines if the widget is the focus widget within its toplevel. (This
;;; does not mean that the HAS_FOCUS flag is necessarily set; HAS_FOCUS will
;;; only be set if the toplevel widget additionally has the global input
;;; focus.)
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is the focus widget.
;;; ----------------------------------------------------------------------------

;; This function is already defined as the accessor of the slot is-focus

;(defcfun ("gtk_widget_is_focus" gtk-widget-is-focus) :boolean
;  (widget g-object))

;(export 'gtk-widget-is-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_grab_focus ()
;;; 
;;; void gtk_widget_grab_focus (GtkWidget *widget)
;;; 
;;; Causes widget to have the keyboard focus for the GtkWindow it's inside.
;;; widget must be a focusable widget, such as a GtkEntry; something like
;;; GtkFrame won't work.
;;; 
;;; More precisely, it must have the GTK_CAN_FOCUS flag set. Use
;;; gtk_widget_set_can_focus() to modify that flag.
;;; 
;;; The widget also needs to be realized and mapped. This is indicated by the
;;; related signals. Grabbing the focus immediately after creating the widget
;;; will likely fail and cause critical warnings.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_grab_focus" gtk-widget-grab-focus) :void
  (widget g-object))

(export 'gtk-widget-grab-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_grab_default ()
;;; 
;;; void gtk_widget_grab_default (GtkWidget *widget)
;;; 
;;; Causes widget to become the default widget. widget must have the
;;; GTK_CAN_DEFAULT flag set; typically you have to set this flag yourself by
;;; calling gtk_widget_set_can_default (widget, TRUE). The default widget is
;;; activated when the user presses Enter in a window. Default widgets must be
;;; activatable, that is, gtk_widget_activate() should affect them. Note that
;;; GtkEntry widgets require the "activates-default" property set to TRUE before
;;; they activate the default widget when Enter is pressed and the GtkEntry is
;;; focused.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_grab_default" gtk-widget-grab-default) :void
  (widget g-object))

(export 'gtk-widget-grab-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_name ()
;;; 
;;; void gtk_widget_set_name (GtkWidget *widget, const gchar *name);
;;; 
;;; Widgets can be named, which allows you to refer to them from a CSS file.
;;; You can apply a style to widgets with a particular name in the CSS file.
;;; See the documentation for the CSS syntax (on the same page as the docs for
;;; GtkStyleContext).
;;; 
;;; Note that the CSS syntax has certain special characters to delimit and
;;; represent elements in a selector (period, #, >, *...), so using these will
;;; make your widget impossible to match by name. Any combination of
;;; alphanumeric symbols, dashes and underscores will suffice.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; name :
;;;     name for the widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_name ()
;;; 
;;; const gchar * gtk_widget_get_name (GtkWidget *widget);
;;; 
;;; Retrieves the name of a widget. See gtk_widget_set_name() for the
;;; significance of widget names.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     name of the widget. This string is owned by GTK+ and should not be
;;;     modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_state ()
;;; 
;;; void gtk_widget_set_state (GtkWidget *widget, GtkStateType state)
;;; 
;;; Warning
;;; 
;;; gtk_widget_set_state is deprecated and should not be used in newly-written
;;; code. 3.0. Use gtk_widget_set_state_flags() instead.
;;; 
;;; This function is for use in widget implementations. Sets the state of a
;;; widget (insensitive, prelighted, etc.) Usually you should set the state
;;; using wrapper functions such as gtk_widget_set_sensitive().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     new state for widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_state" gtk-widget-set-state) :void
  (widget (g-object gtk-widget))
  (state gtk-state-type))

(export 'gtk-widget-set-state)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_sensitive ()
;;; 
;;; void gtk_widget_set_sensitive (GtkWidget *widget, gboolean sensitive)
;;; 
;;; Sets the sensitivity of a widget. A widget is sensitive if the user can
;;; interact with it. Insensitive widgets are "grayed out" and the user can't
;;; interact with them. Insensitive widgets are known as "inactive", "disabled",
;;; or "ghosted" in some other toolkits.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; sensitive :
;;;     TRUE to make the widget sensitive
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_parent ()
;;; 
;;; void gtk_widget_set_parent (GtkWidget *widget, GtkWidget *parent);
;;; 
;;; This function is useful only when implementing subclasses of GtkContainer.
;;; Sets the container as the parent of widget, and takes care of some details
;;; such as updating the state and style of the child to reflect its new
;;; location. The opposite function is gtk_widget_unparent().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; parent :
;;;     parent container
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_parent_window ()
;;; 
;;; void gtk_widget_set_parent_window (GtkWidget *widget,
;;;                                    GdkWindow *parent_window);
;;; 
;;; Sets a non default parent window for widget.
;;; 
;;; For GtkWindow classes, setting a parent_window effects whether the window
;;; is a toplevel window or can be embedded into other widgets.
;;; 
;;; Note
;;; 
;;; For GtkWindow classes, this needs to be called before the window is
;;; realized.
;;; 
;;; widget :
;;;     a GtkWidget.
;;; 
;;; parent_window :
;;;     the new parent window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_parent_window ()
;;; 
;;; GdkWindow * gtk_widget_get_parent_window (GtkWidget *widget);
;;; 
;;; Gets widget's parent window.
;;; 
;;; widget :
;;;     a GtkWidget.
;;; 
;;; Returns :
;;;     the parent window of widget. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_events ()
;;; 
;;; void gtk_widget_set_events (GtkWidget *widget, gint events);
;;; 
;;; Sets the event mask (see GdkEventMask) for a widget. The event mask
;;; determines which events a widget will receive. Keep in mind that different
;;; widgets have different default event masks, and by changing the event mask
;;; you may disrupt a widget's functionality, so be careful. This function must
;;; be called while a widget is unrealized. Consider gtk_widget_add_events()
;;; for widgets that are already realized, or if you want to preserve the
;;; existing event mask. This function can't be used with GTK_NO_WINDOW widgets;
;;; to get events on those widgets, place them inside a GtkEventBox and receive
;;; events on the event box.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; events :
;;;     event mask
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_events" gtk-widget-set-events) :void
  (widget g-object)
  (events gdk-event-mask))

(export 'gtk-widget-set-events)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_events ()
;;; 
;;; gint gtk_widget_get_events (GtkWidget *widget);
;;; 
;;; Returns the event mask for the widget (a bitfield containing flags from the
;;; GdkEventMask enumeration). These are the events that the widget will
;;; receive.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     event mask for widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_events ()
;;; 
;;; void gtk_widget_add_events (GtkWidget *widget, gint events);
;;; 
;;; Adds the events in the bitfield events to the event mask for widget.
;;; See gtk_widget_set_events() for details.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; events :
;;;     an event mask, see GdkEventMask
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_device_events ()
;;; 
;;; void gtk_widget_set_device_events (GtkWidget *widget,
;;;                                    GdkDevice *device,
;;;                                    GdkEventMask events);
;;; 
;;; Sets the device event mask (see GdkEventMask) for a widget. The event mask
;;; determines which events a widget will receive from device. Keep in mind
;;; that different widgets have different default event masks, and by changing
;;; the event mask you may disrupt a widget's functionality, so be careful.
;;; This function must be called while a widget is unrealized. Consider
;;; gtk_widget_add_device_events() for widgets that are already realized, or if
;;; you want to preserve the existing event mask. This function can't be used
;;; with GTK_NO_WINDOW widgets; to get events on those widgets, place them
;;; inside a GtkEventBox and receive events on the event box.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; events :
;;;     event mask
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_events ()
;;; 
;;; GdkEventMask gtk_widget_get_device_events (GtkWidget *widget,
;;;                                            GdkDevice *device);
;;; 
;;; Returns the events mask for the widget corresponding to an specific 
;;; device. These are the events that the widget will receive when device
;;; operates on it.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; Returns :
;;;     device event mask for widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_device_events ()
;;; 
;;; void gtk_widget_add_device_events (GtkWidget *widget,
;;;                                    GdkDevice *device,
;;;                                    GdkEventMask events);
;;; 
;;; Adds the device events in the bitfield events to the event mask for 
;;; widget. See gtk_widget_set_device_events() for details.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; events :
;;;     an event mask, see GdkEventMask
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_device_enabled ()
;;; 
;;; void gtk_widget_set_device_enabled (GtkWidget *widget,
;;;                                     GdkDevice *device,
;;;                                     gboolean enabled);
;;; 
;;; Enables or disables a GdkDevice to interact with widget and all its 
;;; children.
;;; 
;;; It does so by descending through the GdkWindow hierarchy and enabling the 
;;; same mask that is has for core events (i.e. the one that 
;;; gdk_window_get_events() returns).
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; enabled :
;;;     whether to enable the device
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_enabled ()
;;; 
;;; gbooleangtk_widget_get_device_enabled (GtkWidget *widget,
;;;                                        GdkDevice *device);
;;; 
;;; Returns whether device can interact with widget and its children. See 
;;; gtk_widget_set_device_enabled().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; Returns :
;;;     TRUE is device is enabled for widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_toplevel ()
;;; 
;;; GtkWidget * gtk_widget_get_toplevel (GtkWidget *widget);
;;; 
;;; This function returns the topmost widget in the container hierarchy widget 
;;; is a part of. If widget has no parent widgets, it will be returned as the 
;;; topmost widget. No reference will be added to the returned widget; it should 
;;; not be unreferenced.
;;; 
;;; Note the difference in behavior vs. gtk_widget_get_ancestor(); 
;;; gtk_widget_get_ancestor (widget, GTK_TYPE_WINDOW) would return NULL if
;;; widget wasn't inside a toplevel window, and if the window was inside a 
;;; GtkWindow-derived widget which was in turn inside the toplevel GtkWindow. 
;;; While the second case may seem unlikely, it actually happens when a GtkPlug
;;; is embedded inside a GtkSocket within the same application.
;;; 
;;; To reliably find the toplevel GtkWindow, use gtk_widget_get_toplevel() and 
;;; check if the TOPLEVEL flags is set on the result.
;;; 
;;; GtkWidget *toplevel = gtk_widget_get_toplevel (widget);
;;; if (gtk_widget_is_toplevel (toplevel))
;;;   {
;;;     /* Perform action on toplevel. */
;;;   }
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the topmost ancestor of widget, or widget itself if there's no 
;;;     ancestor. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_ancestor ()
;;; 
;;; GtkWidget * gtk_widget_get_ancestor (GtkWidget *widget, GType widget_type)
;;; 
;;; Gets the first ancestor of widget with type widget_type. For example,
;;; gtk_widget_get_ancestor (widget, GTK_TYPE_BOX) gets the first GtkBox that's
;;; an ancestor of widget. No reference will be added to the returned widget;
;;; it should not be unreferenced. See note about checking for a toplevel
;;; GtkWindow in the docs for gtk_widget_get_toplevel().
;;; 
;;; Note that unlike gtk_widget_is_ancestor(), gtk_widget_get_ancestor()
;;; considers widget to be an ancestor of itself.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; widget_type :
;;;     ancestor type
;;; 
;;; Returns :
;;;     the ancestor widget, or NULL if not found. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_ancestor" gtk-widget-get-ancestor)
    (g-object gtk-widget)
  (widget (g-object gtk-widget))
  (type g-type-designator))

(export 'gtk-widget-get-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_visual ()
;;; 
;;; GdkVisual * gtk_widget_get_visual (GtkWidget *widget);
;;; 
;;; Gets the visual that will be used to render widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the visual for widget. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_visual ()
;;; 
;;; void gtk_widget_set_visual (GtkWidget *widget, GdkVisual *visual);
;;; 
;;; Sets the visual that should be used for by widget and its children for 
;;; creating GdkWindows. The visual must be on the same GdkScreen as returned by
;;; gdk_widget_get_screen(), so handling the "screen-changed" signal is
;;; necessary.
;;; 
;;; Setting a new visual will not cause widget to recreate its windows, so you 
;;; should call this function before widget is realized.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; visual :
;;;     visual to be used or NULL to unset a previous one
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pointer ()
;;; 
;;; void gtk_widget_get_pointer (GtkWidget *widget, gint *x, gint *y)
;;; 
;;; Obtains the location of the mouse pointer in widget coordinates. Widget
;;; coordinates are a bit odd; for historical reasons, they are defined as
;;; widget->window coordinates for widgets that are not GTK_NO_WINDOW widgets,
;;; and are relative to widget->allocation.x, widget->allocation.y for widgets
;;; that are GTK_NO_WINDOW widgets.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; x :
;;;     return location for the X coordinate, or NULL.
;;; 
;;; y :
;;;     return location for the Y coordinate, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_pointer" gtk-widget-get-pointer) :void
  (widget g-object)
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-widget-pointer (widget)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-widget-get-pointer widget x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-widget-pointer)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_ancestor ()
;;; 
;;; gboolean gtk_widget_is_ancestor (GtkWidget *widget, GtkWidget *ancestor)
;;; 
;;; Determines whether widget is somewhere inside ancestor, possibly with
;;; intermediate containers.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; ancestor :
;;;     another GtkWidget
;;; 
;;; Returns :
;;;     TRUE if ancestor contains widget as a child, grandchild, great
;;;     grandchild, etc.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_is_ancestor" gtk-widget-is-ancestor) :boolean
  (widget g-object)
  (container g-object))

(export 'gtk-widget-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_translate_coordinates ()
;;; 
;;; gboolean gtk_widget_translate_coordinates (GtkWidget *src_widget,
;;;                                            GtkWidget *dest_widget,
;;;                                            gint src_x,
;;;                                            gint src_y,
;;;                                            gint *dest_x,
;;;                                            gint *dest_y)
;;; 
;;; Translate coordinates relative to src_widget's allocation to coordinates
;;; relative to dest_widget's allocations. In order to perform this operation,
;;; both widgets must be realized, and must share a common toplevel.
;;; 
;;; src_widget :
;;;     a GtkWidget
;;; 
;;; dest_widget :
;;;     a GtkWidget
;;; 
;;; src_x :
;;;     X position relative to src_widget
;;; 
;;; src_y :
;;;     Y position relative to src_widget
;;; 
;;; dest_x :
;;;     location to store X position relative to dest_widget.
;;; 
;;; dest_y :
;;;     location to store Y position relative to dest_widget.
;;; 
;;; Returns :
;;;     FALSE if either widget was not realized, or there was no common
;;;     ancestor. In this case, nothing is stored in *dest_x and *dest_y.
;;;     Otherwise TRUE.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_translate_coordinates" %gtk-widget-translate-coordinates)
    :boolean
  (src-widget g-object)
  (dst-widget g-object)
  (src-x :int)
  (src-y :int)
  (dst-x (:pointer :int))
  (dst-y (:pointer :int)))

(defun gtk-widget-translate-coordinates (src-widget dst-widget src-x src-y)
  (with-foreign-objects ((dst-x :int) (dst-y :int))
    (%gtk-widget-translate-coordinates src-widget
                                       dst-widget
                                       src-x src-y
                                       dst-x dst-y)
    (values (mem-ref dst-x :int)
            (mem-ref dst-y :int))))

(export 'gtk-widget-translate-coordinates)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide_on_delete ()
;;; 
;;; gboolean gtk_widget_hide_on_delete (GtkWidget *widget)
;;; 
;;; Utility function; intended to be connected to the "delete-event" signal on
;;; a GtkWindow. The function calls gtk_widget_hide() on its argument, then
;;; returns TRUE. If connected to ::delete-event, the result is that clicking
;;; the close button for a window (on the window frame, top right corner
;;; usually) will hide but not destroy the window. By default, GTK+ destroys
;;; windows when ::delete-event is received.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_style ()
;;; 
;;; void gtk_widget_set_style (GtkWidget *widget, GtkStyle *style);
;;; 
;;; Warning
;;; 
;;; gtk_widget_set_style has been deprecated since version 3.0 and should not 
;;; be used in newly-written code. Use GtkStyleContext instead
;;; 
;;; Used to set the GtkStyle for a widget (widget->style). Since GTK 3, this 
;;; function does nothing, the passed in style is ignored.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; style :
;;;     a GtkStyle, or NULL to remove the effect of a previous call to 
;;;     gtk_widget_set_style() and go back to the default style.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_ensure_style ()
;;; 
;;; void gtk_widget_ensure_style (GtkWidget *widget)
;;; 
;;; Warning
;;; 
;;; gtk_widget_ensure_style has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use GtkStyleContext instead.
;;; 
;;; Ensures that widget has a style (widget->style).
;;; 
;;; Not a very useful function; most of the time, if you want the style, the
;;; widget is realized, and realized widgets are guaranteed to have a style
;;; already.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_ensure_style" gtk-widget-ensure-style) :void
  (widget g-object))

(export 'gtk-widget-ensure-style)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style ()
;;; 
;;; GtkStyle * gtk_widget_get_style (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_style has been deprecated since version 3.0 and should not 
;;; be used in newly-written code. Use GtkStyleContext instead
;;; 
;;; Simply an accessor function that returns widget->style.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the widget's GtkStyle. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_rc_styles ()
;;; 
;;; void gtk_widget_reset_rc_styles (GtkWidget *widget)
;;; 
;;; Warning
;;; 
;;; gtk_widget_reset_rc_styles has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use GtkStyleContext instead, and
;;; gtk_widget_reset_style()
;;; 
;;; Reset the styles of widget and all descendents, so when they are looked up
;;; again, they get the correct values for the currently loaded RC file
;;; settings.
;;; 
;;; This function is not useful for applications.
;;; 
;;; widget :
;;;     a GtkWidget.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_reset_rc_styles" gtk-widget-reset-rc-styles) :void
  (widget g-object))

(export 'gtk-widget-reset-rc-styles)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_default_style ()
;;; 
;;; GtkStyle * gtk_widget_get_default_style (void)
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_default_style has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use GtkStyleContext instead, and
;;; gtk_css_provider_get_default() to obtain a GtkStyleProvider with the
;;; default widget style information.
;;; 
;;; Returns the default style used by all widgets initially.
;;; 
;;; Returns :
;;;     the default style. This GtkStyle object is owned by GTK+ and should not
;;;     be modified or freed. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_default_style" gtk-widget-default-style)
    (g-object gtk-style))

(export 'gtk-widget-default-style)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_direction ()
;;; 
;;; void gtk_widget_set_direction (GtkWidget *widget, GtkTextDirection dir);
;;; 
;;; Sets the reading direction on a particular widget. This direction controls 
;;; the primary direction for widgets containing text, and also the direction in
;;; which the children of a container are packed. The ability to set the
;;; direction is present in order so that correct localization into languages
;;; with right-to-left reading directions can be done. Generally, applications
;;; will let the default reading direction present, except for containers where
;;; the containers are arranged in an order that is explicitely visual rather
;;; than logical (such as buttons for text justification).
;;; 
;;; If the direction is set to GTK_TEXT_DIR_NONE, then the value set by 
;;; gtk_widget_set_default_direction() will be used.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; dir :
;;;     the new direction
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_direction ()
;;; 
;;; GtkTextDirection gtk_widget_get_direction (GtkWidget *widget);
;;; 
;;; Gets the reading direction for a particular widget. See 
;;; gtk_widget_set_direction().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the reading direction for the widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_default_direction ()
;;; 
;;; void gtk_widget_set_default_direction (GtkTextDirection dir)
;;; 
;;; Sets the default reading direction for widgets where the direction has not
;;; been explicitly set by gtk_widget_set_direction().
;;; 
;;; dir :
;;;     the new default direction. This cannot be GTK_TEXT_DIR_NONE.
;;;     gtk_widget_get_default_direction ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTextDirection gtk_widget_get_default_direction (void);
;;; 
;;; Obtains the current default reading direction. See
;;; gtk_widget_set_default_direction().
;;; 
;;; Returns :
;;;     the current default direction.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_default_direction" gtk-widget-default-direction)
    gtk-text-direction)

(defcfun ("gtk_widget_set_default_direction" gtk-widget-set-default-direction)
    :void
  (direction gtk-text-direction))

(defun (setf gtk-widget-default-direction) (new-value)
  (gtk-widget-set-default-direction new-value))

(export 'gtk-widget-default-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_shape_combine_region ()
;;; 
;;; void gtk_widget_shape_combine_region (GtkWidget *widget,
;;;                                       cairo_region_t *region);
;;; 
;;; Sets a shape for this widget's GDK window. This allows for transparent 
;;; windows etc., see gdk_window_shape_combine_region() for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; region :
;;;     shape to be added, or NULL to remove an existing shape. [allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_input_shape_combine_region ()
;;; 
;;; void gtk_widget_input_shape_combine_region (GtkWidget *widget,
;;;                                             cairo_region_t *region);
;;; 
;;; Sets an input shape for this widget's GDK window. This allows for windows
;;; which react to mouse click in a nonrectangular region, see
;;; gdk_window_input_shape_combine_region() for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; region :
;;;     shape to be added, or NULL to remove an existing shape. [allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path ()
;;; 
;;; void gtk_widget_path (GtkWidget *widget,
;;;                       guint *path_length,
;;;                       gchar **path,
;;;                       gchar **path_reversed);
;;; 
;;; Warning
;;; 
;;; gtk_widget_path has been deprecated since version 3.0 and should not be 
;;; used in newly-written code. Use gtk_widget_get_path() instead
;;; 
;;; Obtains the full path to widget. The path is simply the name of a widget
;;; and all its parents in the container hierarchy, separated by periods. The
;;; name of a widget comes from gtk_widget_get_name(). Paths are used to apply
;;; styles to a widget in gtkrc configuration files. Widget names are the type
;;; of the widget by default (e.g. "GtkButton") or can be set to an
;;; application-specific value with gtk_widget_set_name(). By setting the name
;;; of a widget, you allow users or theme authors to apply styles to that
;;; specific widget in their gtkrc file. path_reversed_p fills in the path in
;;; reverse order, i.e. starting with widget's name instead of starting with the
;;; name of widget's outermost ancestor.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; path_length :
;;;     location to store length of the path, or NULL.
;;; 
;;; path :
;;;     location to store allocated path string, or NULL.
;;; 
;;; path_reversed :
;;;     location to store allocated reverse path string, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_path" %gtk-widget-path) :void
  (widget g-object)
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

(defun gtk-widget-path (widget &key (path-type :name))
  (assert (typep path-type '(member :name :class)))
  (with-foreign-object (path :pointer)
    (ecase path-type
      (:name (%gtk-widget-path widget (null-pointer) path (null-pointer)))
      (:class (gtk-widget-class-path widget
                                     (null-pointer)
                                     path
                                     (null-pointer))))
    (mem-ref path '(g-string :free-from-foreign t))))

(export 'gtk-widget-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_path ()
;;; 
;;; void gtk_widget_class_path (GtkWidget *widget,
;;;                             guint *path_length,
;;;                             gchar **path,
;;;                             gchar **path_reversed);
;;; 
;;; Warning
;;; 
;;; gtk_widget_class_path has been deprecated since version 3.0 and should not 
;;; be used in newly-written code. Use gtk_widget_get_path() instead
;;; 
;;; Same as gtk_widget_path(), but always uses the name of a widget's type, 
;;; never uses a custom name set with gtk_widget_set_name().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; path_length :
;;;     location to store the length of the class path, or NULL.
;;; 
;;; path :
;;;     location to store the class path as an allocated string, or NULL.
;;; 
;;; path_reversed :
;;;     location to store the reverse class path as an allocated string, or
;;;     NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_class_path" gtk-widget-class-path) :void
  (widget g-object)
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

(export 'gtk-widget-class-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_composite_name ()
;;; 
;;; gchar * gtk_widget_get_composite_name (GtkWidget *widget);
;;; 
;;; Obtains the composite name of a widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the composite name of widget, or NULL if widget is not a composite 
;;;     child. The string should be freed when it is no longer needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_background_color ()
;;; 
;;; void gtk_widget_override_background_color (GtkWidget *widget,
;;;                                            GtkStateFlags state,
;;;                                            const GdkRGBA *color);
;;; 
;;; Sets the background color to use for a widget.
;;; 
;;; All other style values are left untouched. See gtk_widget_override_color().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the background color
;;; 
;;; color :
;;;     the color to assign, or NULL to undo the effect of previous calls to 
;;;    gtk_widget_override_background_color().
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_color ()
;;; 
;;; void gtk_widget_override_color (GtkWidget *widget,
;;;                                 GtkStateFlags state,
;;;                                 const GdkRGBA *color);
;;; 
;;; Sets the color to use for a widget.
;;; 
;;; All other style values are left untouched.
;;; 
;;; Note
;;; 
;;; This API is mostly meant as a quick way for applications to change a 
;;; widget appearance. If you are developing a widgets library and intend this 
;;; change to be themeable, it is better done by setting meaningful CSS classes 
;;; and regions in your widget/container implementation through 
;;; gtk_style_context_add_class() and gtk_style_context_add_region().
;;; 
;;; This way, your widget library can install a GtkCssProvider with the 
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK priority in order to provide a default 
;;; styling for those widgets that need so, and this theming may fully
;;; overridden by the user's theme.
;;; 
;;; Note
;;; 
;;; Note that for complex widgets this may bring in undesired results (such as 
;;; uniform background color everywhere), in these cases it is better to fully 
;;; style such widgets through a GtkCssProvider with the 
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION priority.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the color
;;; 
;;; color :
;;;     the color to assign, or NULL to undo the effect of previous calls to 
;;;     gtk_widget_override_color().
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_font ()
;;; 
;;; void gtk_widget_override_font (GtkWidget *widget,
;;;                                const PangoFontDescription *font_desc);
;;; 
;;; Sets the font to use for a widget. All other style values are left 
;;; untouched. See gtk_widget_override_color().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; font_desc :
;;;     the font descriptiong to use, or NULL to undo the effect of previous 
;;;     calls to gtk_widget_override_font(). [allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_symbolic_color ()
;;; 
;;; void gtk_widget_override_symbolic_color (GtkWidget *widget,
;;;                                          const gchar *name,
;;;                                          const GdkRGBA *color);
;;; 
;;; Sets a symbolic color for a widget.
;;; 
;;; All other style values are left untouched. See gtk_widget_override_color() 
;;; for overriding the foreground or background color.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; name :
;;;     the name of the symbolic color to modify
;;; 
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo 
;;;     the effect of previous calls to gtk_widget_override_symbolic_color().
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_cursor ()
;;; 
;;; void gtk_widget_override_cursor (GtkWidget *widget,
;;;                                  const GdkRGBA *cursor,
;;;                                  const GdkRGBA *secondary_cursor);
;;; 
;;; Sets the cursor color to use in a widget, overriding the "cursor-color" 
;;; and "secondary-cursor-color" style properties. All other style values are
;;; left untouched. See also gtk_widget_modify_style().
;;; 
;;; Note that the underlying properties have the GdkColor type, so the alpha 
;;; value in primary and secondary will be ignored.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; cursor :
;;;     the color to use for primary cursor (does not need to be allocated), 
;;;     or NULL to undo the effect of previous calls to of 
;;;     gtk_widget_override_cursor().
;;; 
;;; secondary_cursor :
;;;     the color to use for secondary cursor (does not need to be allocated), 
;;;     or NULL to undo the effect of previous calls to of 
;;;     gtk_widget_override_cursor().
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_style ()
;;; 
;;; void gtk_widget_modify_style (GtkWidget *widget, GtkRcStyle *style);
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_style has been deprecated since version 3.0 and should 
;;; not be used in newly-written code. Use GtkStyleContext with a custom 
;;; GtkStyleProvider instead
;;; 
;;; Modifies style values on the widget.
;;; 
;;; Modifications made using this technique take precedence over style values 
;;; set via an RC file, however, they will be overridden if a style is
;;; explicitely set on the widget using gtk_widget_set_style(). The GtkRcStyle
;;; structure is designed so each field can either be set or unset, so it is
;;; possible, using this function, to modify some style values and leave the
;;; others unchanged.
;;; 
;;; Note that modifications made with this function are not cumulative with
;;; previous calls to gtk_widget_modify_style() or with such functions as 
;;; gtk_widget_modify_fg(). If you wish to retain previous values, you must
;;; first call gtk_widget_get_modifier_style(), make your modifications to the
;;; returned style, then call gtk_widget_modify_style() with that style. On the
;;; other hand, if you first call gtk_widget_modify_style(), subsequent calls
;;; to such functions gtk_widget_modify_fg() will have a cumulative effect with
;;; the initial modifications.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; style :
;;;     the GtkRcStyle holding the style modifications
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_modifier_style ()
;;; 
;;; GtkRcStyle * gtk_widget_get_modifier_style (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_modifier_style has been deprecated since version 3.0 and 
;;; should not be used in newly-written code. Use GtkStyleContext with a custom 
;;; GtkStyleProvider instead
;;; 
;;; Returns the current modifier style for the widget. (As set by 
;;; gtk_widget_modify_style().) If no style has previously set, a new GtkRcStyle
;;; will be created with all values unset, and set as the modifier style for the
;;; widget. If you make changes to this rc style, you must call 
;;; gtk_widget_modify_style(), passing in the returned rc style, to make sure
;;; that your changes take effect.
;;; 
;;; Caution: passing the style back to gtk_widget_modify_style() will normally 
;;; end up destroying it, because gtk_widget_modify_style() copies the passed-in
;;; style and sets the copy as the new modifier style, thus dropping any
;;; reference to the old modifier style. Add a reference to the modifier style
;;; if you want to keep it alive.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the modifier style for the widget. This rc style is owned by the 
;;;     widget. If you want to keep a pointer to value this around, you must
;;;     add a refcount using g_object_ref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_fg ()
;;; 
;;; void gtk_widget_modify_fg (GtkWidget *widget,
;;;                            GtkStateType state,
;;;                            const GdkColor *color)
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_fg has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_color() instead.
;;; 
;;; Sets the foreground color for a widget in a particular state.
;;; 
;;; All other style values are left untouched. See also
;;; gtk_widget_modify_style().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the foreground color
;;; 
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo
;;;     the effect of previous calls to of gtk_widget_modify_fg().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_modify_fg" gtk-widget-modify-fg) :void
  (widget (g-object gtk-widget))
  (state gtk-state-type)
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-widget-modify-fg)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_bg ()
;;; 
;;; void gtk_widget_modify_bg (GtkWidget *widget,
;;;                            GtkStateType state,
;;;                            const GdkColor *color)
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_bg has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_background_color()
;;; instead.
;;; 
;;; Sets the background color for a widget in a particular state.
;;; 
;;; All other style values are left untouched. See also
;;; gtk_widget_modify_style().
;;; 
;;; Note
;;; 
;;; Note that "no window" widgets (which have the GTK_NO_WINDOW flag set) draw
;;; on their parent container's window and thus may not draw any background
;;; themselves. This is the case for e.g. GtkLabel.
;;; 
;;; To modify the background of such widgets, you have to set the background
;;; color on their parent; if you want to set the background of a rectangular
;;; area around a label, try placing the label in a GtkEventBox widget and
;;; setting the background color on that.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the background color
;;; 
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo
;;;     the effect of previous calls to of gtk_widget_modify_bg().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_modify_bg" gtk-widget-modify-bg) :void
  (widget (g-object gtk-widget))
  (state gtk-state-type)
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-widget-modify-bg)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_text ()
;;; 
;;; void gtk_widget_modify_text (GtkWidget *widget,
;;;                              GtkStateType state,
;;;                              const GdkColor *color)
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_text has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_color() instead.
;;; 
;;; Sets the text color for a widget in a particular state.
;;; 
;;; All other style values are left untouched. The text color is the foreground
;;; color used along with the base color (see gtk_widget_modify_base()) for
;;; widgets such as GtkEntry and GtkTextView. See also
;;; gtk_widget_modify_style().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the text color
;;; 
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo
;;;     the effect of previous calls to of gtk_widget_modify_text().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_modify_text" gtk-widget-modify-text) :void
  (widget (g-object gtk-widget))
  (state gtk-state-type)
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-widget-modify-text)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_base ()
;;; 
;;; void gtk_widget_modify_base (GtkWidget *widget,
;;;                              GtkStateType state,
;;;                              const GdkColor *color)
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_base has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_background_color()
;;; instead.
;;; 
;;; Sets the base color for a widget in a particular state. All other style
;;; values are left untouched. The base color is the background color used along
;;; with the text color (see gtk_widget_modify_text()) for widgets such as
;;; GtkEntry and GtkTextView. See also gtk_widget_modify_style().
;;; 
;;; Note
;;; 
;;; Note that "no window" widgets (which have the GTK_NO_WINDOW flag set) draw
;;; on their parent container's window and thus may not draw any background
;;; themselves. This is the case for e.g. GtkLabel.
;;; 
;;; To modify the background of such widgets, you have to set the base color on
;;; their parent; if you want to set the background of a rectangular area around
;;; a label, try placing the label in a GtkEventBox widget and setting the base
;;; color on that.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; state :
;;;     the state for which to set the base color
;;; 
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo
;;;     the effect of previous calls to of gtk_widget_modify_base().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_modify_base" gtk-widget-modify-base) :void
  (widget (g-object gtk-widget))
  (state gtk-state-type)
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-widget-modify-base)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_font ()
;;; 
;;; void gtk_widget_modify_font (GtkWidget *widget,
;;;                              PangoFontDescription *font_desc);
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_font has been deprecated since version 3.0 and should 
;;; not be used in newly-written code. Use gtk_widget_override_font() instead
;;; 
;;; Sets the font to use for a widget.
;;; 
;;; All other style values are left untouched. See also 
;;; gtk_widget_modify_style().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; font_desc :
;;;     the font description to use, or NULL to undo the effect of previous 
;;;     calls to gtk_widget_modify_font().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_cursor ()
;;; 
;;; void gtk_widget_modify_cursor (GtkWidget *widget,
;;;                                const GdkColor *primary,
;;;                                const GdkColor *secondary)
;;; 
;;; Warning
;;; 
;;; gtk_widget_modify_cursor is deprecated and should not be used in
;;; newly-written code. 3.0. Use gtk_widget_override_cursor() instead.
;;; 
;;; Sets the cursor color to use in a widget, overriding the "cursor-color" and
;;; "secondary-cursor-color" style properties.
;;; 
;;; All other style values are left untouched. See also
;;; gtk_widget_modify_style().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; primary :
;;;     the color to use for primary cursor (does not need to be allocated), or
;;;     NULL to undo the effect of previous calls to of
;;;     gtk_widget_modify_cursor().
;;; 
;;; secondary :
;;;     the color to use for secondary cursor (does not need to be allocated),
;;;     or NULL to undo the effect of previous calls to of
;;;     gtk_widget_modify_cursor().
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_modify_cursor" gtk-widget-modify-cursor) :void
  (widget (g-object gtk-widget))
  (primary (g-boxed-foreign gdk-color))
  (secondary (g-boxed-foreign gdk-color)))

(export 'gtk-widget-modify-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_context ()
;;; 
;;; PangoContext * gtk_widget_create_pango_context (GtkWidget *widget)
;;; 
;;; Creates a new PangoContext with the appropriate font map, font description,
;;; and base direction for drawing text for this widget. See also
;;; gtk_widget_get_pango_context().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the new PangoContext. [transfer full]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_create_pango_context" gtk-widget-create-pango-context)
    (g-object :already-referenced)
  (widget g-object))

(export 'gtk-widget-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pango_context ()
;;; 
;;; PangoContext * gtk_widget_get_pango_context (GtkWidget *widget);
;;; 
;;; Gets a PangoContext with the appropriate font map, font description, and 
;;; base direction for this widget. Unlike the context returned by 
;;; gtk_widget_create_pango_context(), this context is owned by the widget (it
;;; can be used until the screen for the widget changes or the widget is removed
;;; from its toplevel), and will be updated to match any changes to the widget's
;;; attributes.
;;; 
;;; If you create and keep a PangoLayout using this context, you must deal 
;;; with changes to the context by calling pango_layout_context_changed() on the
;;; layout in response to the "style-updated" and "direction-changed" signals
;;; for the widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the PangoContext for the widget. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_layout ()
;;; 
;;; PangoLayout * gtk_widget_create_pango_layout (GtkWidget *widget,
;;;                                               const gchar *text)
;;; 
;;; Creates a new PangoLayout with the appropriate font map, font description,
;;; and base direction for drawing text for this widget.
;;; 
;;; If you keep a PangoLayout created in this way around, in order to notify
;;; the layout of changes to the base direction or font of this widget, you must
;;; call pango_layout_context_changed() in response to the "style-updated" and
;;; "direction-changed" signals for the widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; text :
;;;     text to set on the layout (can be NULL)
;;; 
;;; Returns :
;;;     the new PangoLayout. [transfer full]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_create_pango_layout" gtk-widget-create-pango-layout)
    (g-object pango-layout :already-referenced)
  (widget (g-object gtk-widget))
  (text :string))

(export 'gtk-widget-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_render_icon ()
;;; 
;;; GdkPixbuf * gtk_widget_render_icon (GtkWidget *widget,
;;;                                     const gchar *stock_id,
;;;                                     GtkIconSize size,
;;;                                     const gchar *detail)
;;; 
;;; Warning
;;; 
;;; gtk_widget_render_icon has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_render_icon_pixbuf() instead.
;;; 
;;; A convenience function that uses the theme settings for widget to look up
;;; stock_id and render it to a pixbuf. stock_id should be a stock icon ID such
;;; as GTK_STOCK_OPEN or GTK_STOCK_OK. size should be a size such as
;;; GTK_ICON_SIZE_MENU. detail should be a string that identifies the widget or
;;; code doing the rendering, so that theme engines can special-case rendering
;;; for that widget or code.
;;; 
;;; The pixels in the returned GdkPixbuf are shared with the rest of the
;;; application and should not be modified. The pixbuf should be freed after
;;; use with g_object_unref().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; stock_id :
;;;     a stock ID
;;; 
;;; size :
;;;     a stock size. A size of (GtkIconSize)-1 means render at the size of the
;;;     source and don't scale (if there are multiple source sizes, GTK+ picks
;;;     one of the available sizes). [type int]
;;; 
;;; detail :
;;;     render detail to pass to theme engine. [allow-none]
;;; 
;;; Returns :
;;;     a new pixbuf, or NULL if the stock ID wasn't known. [transfer full]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_render_icon" gtk-widget-render-icon) g-object
  (widget g-object)
  (stock-id :string)
  (size gtk-icon-size)
  (detail :string))

(export 'gtk-widget-render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_render_icon_pixbuf ()
;;; 
;;; GdkPixbuf * gtk_widget_render_icon_pixbuf (GtkWidget *widget,
;;;                                            const gchar *stock_id,
;;;                                            GtkIconSize size)
;;; 
;;; A convenience function that uses the theme engine and style settings for
;;; widget to look up stock_id and render it to a pixbuf. stock_id should be a
;;; stock icon ID such as GTK_STOCK_OPEN or GTK_STOCK_OK. size should be a size
;;; such as GTK_ICON_SIZE_MENU.
;;; 
;;; The pixels in the returned GdkPixbuf are shared with the rest of the
;;; application and should not be modified. The pixbuf should be freed after
;;; use with g_object_unref().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; stock_id :
;;;     a stock ID
;;; 
;;; size :
;;;     a stock size. A size of (GtkIconSize)-1 means render at the size of the
;;;     source and don't scale (if there are multiple source sizes, GTK+ picks
;;;     one of the available sizes). [type int]
;;; 
;;; Returns :
;;;     a new pixbuf, or NULL if the stock ID wasn't known. [transfer full]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_pop_composite_child ()
;;; 
;;; void gtk_widget_pop_composite_child (void)
;;; 
;;; Cancels the effect of a previous call to gtk_widget_push_composite_child().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_pop_composite_child" gtk-widget-pop-composite-child)
    :void
  (widget g-object))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_push_composite_child ()
;;; 
;;; void gtk_widget_push_composite_child (void)
;;; 
;;; Makes all newly-created widgets as composite children until the
;;; corresponding gtk_widget_pop_composite_child() call.
;;; 
;;; A composite child is a child that's an implementation detail of the
;;; container it's inside and should not be visible to people using the
;;; container. Composite children aren't treated differently by GTK (but
;;; see gtk_container_foreach() vs. gtk_container_forall()), but e.g. GUI
;;; builders might want to treat them in a different way.
;;; 
;;; Here is a simple example:
;;; 
;;;  gtk_widget_push_composite_child ();
;;;  scrolled_window->hscrollbar
;;;               = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL, hadjustment);
;;;  gtk_widget_set_composite_name (scrolled_window->hscrollbar, "hscrollbar");
;;;  gtk_widget_pop_composite_child ();
;;;  gtk_widget_set_parent (scrolled_window->hscrollbar,
;;;                         GTK_WIDGET (scrolled_window));
;;;  g_object_ref (scrolled_window->hscrollbar);
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_push_composite_child" gtk-widget-push-composite-child)
    :void
  (widget g-object))

(export 'gtk-widget-push-composite-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_area ()
;;; 
;;; void gtk_widget_queue_draw_area (GtkWidget *widget,
;;;                                  gint x,
;;;                                  gint y,
;;;                                  gint width,
;;;                                  gint height)
;;; 
;;; Convenience function that calls gtk_widget_queue_draw_region() on the
;;; region created from the given coordinates.
;;; 
;;; The region here is specified in widget coordinates. Widget coordinates are
;;; a bit odd; for historical reasons, they are defined as widget->window
;;; coordinates for widgets that are not GTK_NO_WINDOW widgets, and are
;;; relative to widget->allocation.x, widget->allocation.y for widgets that are
;;; GTK_NO_WINDOW widgets.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; x :
;;;     x coordinate of upper-left corner of rectangle to redraw
;;; 
;;; y :
;;;     y coordinate of upper-left corner of rectangle to redraw
;;; 
;;; width :
;;;     width of region to draw
;;; 
;;; height :
;;;     height of region to draw
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_queue_draw_area" gtk-widget-queue-draw-area) :void
  (widget g-object)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gtk-widget-queue-draw-area)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_region ()
;;; 
;;; void gtk_widget_queue_draw_region (GtkWidget *widget,
;;;                                    const cairo_region_t *region)
;;; 
;;; Invalidates the rectangular area of widget defined by region by calling
;;; gdk_window_invalidate_region() on the widget's window and all its child
;;; windows. Once the main loop becomes idle (after the current batch of events
;;; has been processed, roughly), the window will receive expose events for the
;;; union of all regions that have been invalidated.
;;; 
;;; Normally you would only use this function in widget implementations. You
;;; might also use it to schedule a redraw of a GtkDrawingArea or some portion
;;; thereof.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; region :
;;;     region to draw
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_app_paintable ()
;;; 
;;; void gtk_widget_set_app_paintable (GtkWidget *widget,
;;;                                    gboolean app_paintable)
;;; 
;;; Sets whether the application intends to draw on the widget in an "draw"
;;; handler.
;;; 
;;; This is a hint to the widget and does not affect the behavior of the GTK+
;;; core; many widgets ignore this flag entirely. For widgets that do pay
;;; attention to the flag, such as GtkEventBox and GtkWindow, the effect is to
;;; suppress default themed drawing of the widget's background. (Children of the
;;; widget will still be drawn.) The application is then entirely responsible
;;; for drawing the widget background.
;;; 
;;; Note that the background is still drawn when the widget is mapped.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; app_paintable :
;;;     TRUE if the application will paint on the widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_double_buffered ()
;;; 
;;; void gtk_widget_set_double_buffered (GtkWidget *widget,
;;;                                      gboolean double_buffered)
;;; 
;;; Widgets are double buffered by default; you can use this function to turn
;;; off the buffering. "Double buffered" simply means that
;;; gdk_window_begin_paint_region() and gdk_window_end_paint() are called
;;; automatically around expose events sent to the widget.
;;; gdk_window_begin_paint() diverts all drawing to a widget's window to an
;;; offscreen buffer, and gdk_window_end_paint() draws the buffer to the
;;; screen. The result is that users see the window update in one smooth step,
;;; and don't see individual graphics primitives being rendered.
;;; 
;;; In very simple terms, double buffered widgets don't flicker, so you would
;;; only use this function to turn off double buffering if you had special
;;; needs and really knew what you were doing.
;;; 
;;; Note: if you turn off double-buffering, you have to handle expose events,
;;; since even the clearing to the background color or pixmap will not happen
;;; automatically (as it is done in gdk_window_begin_paint()).
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; double_buffered :
;;;     TRUE to double-buffer a widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_double_buffered" gtk-widget-set-double-buffered) :void
  (widget (g-object gtk-widget))
  (double-buffered :boolean))

(export 'gtk-widget-set-double-buffered)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_redraw_on_allocate ()
;;; 
;;; void gtk_widget_set_redraw_on_allocate (GtkWidget *widget,
;;;                                         gboolean redraw_on_allocate)
;;; 
;;; Sets whether the entire widget is queued for drawing when its size
;;; allocation changes. By default, this setting is TRUE and the entire widget
;;; is redrawn on every size change. If your widget leaves the upper left
;;; unchanged when made bigger, turning this setting off will improve
;;; performance.
;;; 
;;; Note that for NO_WINDOW widgets setting this flag to FALSE turns off all
;;; allocation on resizing: the widget will not even redraw if its position
;;; changes; this is to allow containers that don't draw anything to avoid
;;; excess invalidations. If you set this flag on a NO_WINDOW widget that does
;;; draw on widget->window, you are responsible for invalidating both the old
;;; and new allocation of the widget when the widget is moved and responsible
;;; for invalidating regions newly when the widget increases size.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; redraw_on_allocate :
;;;     if TRUE, the entire widget will be redrawn when it is allocated to a
;;;     new size. Otherwise, only the new portion of the widget will be redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_composite_name ()
;;; 
;;; void gtk_widget_set_composite_name (GtkWidget *widget, const gchar *name)
;;; 
;;; Sets a widgets composite name. The widget must be a composite child of its
;;; parent; see gtk_widget_push_composite_child().
;;; 
;;; widget :
;;;     a GtkWidget.
;;; 
;;; name :
;;;     the name to set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_mnemonic_activate ()
;;; 
;;; gboolean gtk_widget_mnemonic_activate (GtkWidget *widget,
;;;                                        gboolean group_cycling)
;;; 
;;; Emits the "mnemonic-activate" signal.
;;; 
;;; The default handler for this signal activates the widget if group_cycling
;;; is FALSE, and just grabs the focus if group_cycling is TRUE.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; group_cycling :
;;;     TRUE if there are other widgets with the same mnemonic
;;; 
;;; Returns :
;;;     TRUE if the signal has been handled
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_mnemonic_activate" gtk-widget-mnemonic-activate) :boolean
  (widget (g-object gtk-widget))
  (group-cycling :boolean))

(export 'gtk-widget-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property ()
;;; 
;;; void gtk_widget_class_install_style_property (GtkWidgetClass *klass,
;;;                                               GParamSpec *pspec)
;;; 
;;; Installs a style property on a widget class. The parser for the style
;;; property is determined by the value type of pspec.
;;; 
;;; klass :
;;;     a GtkWidgetClass
;;; 
;;; pspec :
;;;     the GParamSpec for the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property_parser ()
;;; 
;;; void gtk_widget_class_install_style_property_parser
;;;                                                 (GtkWidgetClass *klass,
;;;                                                  GParamSpec *pspec,
;;;                                                  GtkRcPropertyParser parser)
;;; 
;;; Installs a style property on a widget class.
;;; 
;;; klass :
;;;     a GtkWidgetClass
;;; 
;;; pspec :
;;;     the GParamSpec for the style property
;;; 
;;; parser :
;;;     the parser for the style property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_find_style_property ()
;;; 
;;; GParamSpec * gtk_widget_class_find_style_property
;;;                                                 (GtkWidgetClass *klass,
;;;                                                  const gchar *property_name)
;;; 
;;; Finds a style property of a widget class by name.
;;; 
;;; klass :
;;;     a GtkWidgetClass
;;; 
;;; property_name :
;;;     the name of the style property to find
;;; 
;;; Returns :
;;;     the GParamSpec of the style property or NULL if class has no style
;;;     property with that name. [transfer none]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_class_find_style_property" 
          gtk-widget-class-find-style-property) (:pointer g-param-spec)
  (class :pointer)
  (property-name :string))

(export 'gtk-widget-class-find-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_list_style_properties ()
;;; 
;;; GParamSpec ** gtk_widget_class_list_style_properties (GtkWidgetClass *klass,
;;;                                                       guint *n_properties)
;;; 
;;; Returns all style properties of a widget class.
;;; 
;;; klass :
;;;     a GtkWidgetClass
;;; 
;;; n_properties :
;;;     location to return the number of style properties found
;;; 
;;; Returns :
;;;     a newly allocated array of GParamSpec*. The array must be freed with
;;;     g_free(). [array length=n_properties][transfer container]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_class_list_style_properties"
          %gtk-widget-class-list-style-properties)
    (:pointer (:pointer g-param-spec))
  (class :pointer)
  (n-properties (:pointer :int)))

(defun gtk-widget-class-list-style-properties (type)
  (setf type (gtype type))
  (let ((class (g-type-class-ref type)))
    (unwind-protect
         (with-foreign-object (np :int)
           (let ((specs (%gtk-widget-class-list-style-properties class np)))
             (unwind-protect
                  (loop
                     repeat (mem-ref np :int)
                     for i from 0
                     for spec = (mem-aref specs :pointer i)
                     collect (parse-g-param-spec spec))
               (g-free specs))))
      (g-type-class-unref class))))

(export 'gtk-widget-class-list-style-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_region_intersect ()
;;; 
;;; cairo_region_t * gtk_widget_region_intersect (GtkWidget *widget,
;;;                                               const cairo_region_t *region)
;;; 
;;; Computes the intersection of a widget's area and region, returning the
;;; intersection. The result may be empty, use cairo_region_is_empty() to check.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; region :
;;;     a cairo_region_t, in the same coordinate system as widget->allocation.
;;;     That is, relative to widget->window for NO_WINDOW widgets; relative to
;;;     the parent window of widget->window for widgets with their own window.
;;; 
;;; Returns :
;;;     A newly allocated region holding the intersection of widget and region.
;;;     The coordinates of the return value are relative to widget->window for
;;;     NO_WINDOW widgets, and relative to the parent window of widget->window
;;;     for widgets with their own window.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_region_intersect" gtk-widget-region-intersect)
    (g-boxed-foreign gdk-region :return)
  (widget (g-object gtk-widget))
  (region (g-boxed-foreign gdk-region)))

(export 'gtk-widget-region-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_expose ()
;;; 
;;; gint gtk_widget_send_expose (GtkWidget *widget, GdkEvent *event);
;;; 
;;; Very rarely-used function. This function is used to emit an expose event 
;;; on a widget. This function is not normally used directly. The only time it
;;; is used is when propagating an expose event to a child NO_WINDOW widget,
;;; and that is normally done using gtk_container_propagate_draw().
;;; 
;;; If you want to force an area of a window to be redrawn, use 
;;; gdk_window_invalidate_rect() or gdk_window_invalidate_region(). To cause
;;; the redraw to be done immediately, follow that call with a call to 
;;; gdk_window_process_updates().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; event :
;;;     a expose GdkEvent
;;; 
;;; Returns :
;;;     return from the event signal emission (TRUE if the event was handled)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_focus_change ()
;;; 
;;; gboolean gtk_widget_send_focus_change (GtkWidget *widget, GdkEvent *event)
;;; 
;;; Sends the focus change event to widget
;;; 
;;; This function is not meant to be used by applications. The only time it 
;;; should be used is when it is necessary for a GtkWidget to assign focus to a
;;; widget that is semantically owned by the first widget even though it's not
;;; a direct child - for instance, a search entry in a floating window similar
;;; to the quick search in GtkTreeView.
;;; 
;;; An example of its usage is:
;;; 
;;; GdkEvent *fevent = gdk_event_new (GDK_FOCUS_CHANGE);
;;; 
;;; fevent->focus_change.type = GDK_FOCUS_CHANGE;
;;; fevent->focus_change.in = TRUE;
;;; fevent->focus_change.window = gtk_widget_get_window (widget);
;;; if (fevent->focus_change.window != NULL)
;;;   g_object_ref (fevent->focus_change.window);
;;; 
;;; gtk_widget_send_focus_change (widget, fevent);
;;; 
;;; gdk_event_free (event);
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; event :
;;;     a GdkEvent of type GDK_FOCUS_CHANGE
;;; 
;;; Returns :
;;;     the return value from the event signal emission: TRUE if the event was 
;;;     handled, and FALSE otherwise
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get ()
;;; 
;;; void gtk_widget_style_get (GtkWidget *widget,
;;;                            const gchar *first_property_name,
;;;                            ...);
;;; 
;;; Gets the values of a multiple style properties of widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; first_property_name :
;;;     the name of the first property to get
;;; 
;;; ... :
;;;     pairs of property names and locations to return the property values, 
;;;     starting with the location for first_property_name, terminated by NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_property ()
;;; 
;;; void gtk_widget_style_get_property (GtkWidget *widget,
;;;                                     const gchar *property_name,
;;;                                     GValue *value)
;;; 
;;; Gets the value of a style property of widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; property_name :
;;;     the name of a style property
;;; 
;;; value :
;;;     location to return the property value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_style_get_property" gtk-widget-style-get-property) :void
  (widget g-object)
  (property-name :string)
  (value (:pointer g-value)))

(export 'gtk-widget-style-get-property)

(defun gtk-widget-style-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
      (let ((g-param-spec (gtk-widget-class-find-style-property class
                                                                property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'gtk-widget-style-property-info)

(defun gtk-widget-style-property-type (widget property-name)
  (let ((property-info (gtk-widget-style-property-info
                                                   (g-type-from-instance widget)
                                                   property-name)))
    (param-spec-type property-info)))

(defun gtk-widget-style-property-value (widget property-name
                                               &optional property-type)
  (unless property-type
    (setf property-type
          (gtk-widget-style-property-type widget property-name)))
  (setf property-type (gtype property-type))
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue property-type)
    (prog1 (gtk-widget-style-get-property widget property-name gvalue)
      (g-value-unset gvalue))))

(export 'gtk-widget-style-property-value)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_valist ()
;;; 
;;; void gtk_widget_style_get_valist (GtkWidget *widget,
;;;                                   const gchar *first_property_name,
;;;                                   va_list var_args)
;;; 
;;; Non-vararg variant of gtk_widget_style_get(). Used primarily by language
;;; bindings.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; first_property_name :
;;;     the name of the first property to get
;;; 
;;; var_args :
;;;     a va_list of pairs of property names and locations to return the
;;;     property values, starting with the location for first_property_name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_attach ()
;;; 
;;; void gtk_widget_style_attach (GtkWidget *widget)
;;; 
;;; Warning
;;; 
;;; gtk_widget_style_attach is deprecated and should not be used in
;;; newly-written code. 3.0. This step is unnecessary with GtkStyleContext.
;;; 
;;; This function attaches the widget's GtkStyle to the widget's GdkWindow.
;;; It is a replacement for
;;; 
;;; widget->style = gtk_style_attach (widget->style, widget->window);
;;; 
;;; and should only ever be called in a derived widget's "realize"
;;; implementation which does not chain up to its parent class' "realize"
;;; implementation, because one of the parent classes (finally GtkWidget) would
;;; attach the style itself.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_type ()
;;; 
;;; void gtk_widget_class_set_accessible_type (GtkWidgetClass *widget_class,
;;;                                            GType type)
;;; 
;;; Sets the type to be used for creating accessibles for widgets of
;;; widget_class. The given type must be a subtype of the type used for
;;; accessibles of the parent class.
;;; 
;;; This function should only be called from class init functions of widgets.
;;; 
;;; widget_class :
;;;     class to set the accessible type for
;;; 
;;; type :
;;;     The object type that implements the accessible for widget_class
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_role ()
;;; 
;;; void gtk_widget_class_set_accessible_role (GtkWidgetClass *widget_class,
;;;                                            AtkRole role)
;;; 
;;; Sets the default AtkRole to be set on accessibles created for widgets of
;;; widget_class. Accessibles may decide to not honor this setting if their role
;;; reporting is more refined. Calls to gtk_widget_class_set_accessible_type()
;;; will reset this value.
;;; 
;;; In cases where you want more fine-grained control over the role of
;;; accessibles created for widget_class, you should provide your own accessible
;;; type and use gtk_widget_class_set_accessible_type() instead.
;;; 
;;; If role is ATK_ROLE_INVALID, the default role will not be changed and the
;;; accessible's default role will be used instead.
;;; 
;;; This function should only be called from class init functions of widgets.
;;; 
;;; widget_class :
;;;     class to set the accessible role for
;;; 
;;; role :
;;;     The role to use for accessibles created for widget_class
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_accessible ()
;;; 
;;; AtkObject * gtk_widget_get_accessible (GtkWidget *widget)
;;; 
;;; Returns the accessible object that describes the widget to an assistive
;;; technology.
;;; 
;;; If accessibility support is not available, this AtkObject instance may be
;;; a no-op. Likewise, if no class-specific AtkObject implementation is
;;; available for the widget instance in question, it will inherit an AtkObject
;;; implementation from the first ancestor class for which such an
;;; implementation is defined.
;;; 
;;; The documentation of the ATK library contains more information about
;;; accessible objects and their uses.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the AtkObject associated with widget. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_focus ()
;;; 
;;; gboolean gtk_widget_child_focus (GtkWidget *widget,
;;;                                  GtkDirectionType direction)
;;; 
;;; This function is used by custom widget implementations; if you're writing
;;; an app, you'd use gtk_widget_grab_focus() to move the focus to a particular
;;; widget, and gtk_container_set_focus_chain() to change the focus tab order.
;;; So you may want to investigate those functions instead.
;;; 
;;; gtk_widget_child_focus() is called by containers as the user moves around
;;; the window using keyboard shortcuts. direction indicates what kind of motion
;;; is taking place (up, down, left, right, tab forward, tab backward).
;;; gtk_widget_child_focus() emits the "focus" signal; widgets override the
;;; default handler for this signal in order to implement appropriate focus
;;; behavior.
;;; 
;;; The default ::focus handler for a widget should return TRUE if moving in
;;; direction left the focus on a focusable location inside that widget, and
;;; FALSE if moving in direction moved the focus outside the widget. If
;;; returning TRUE, widgets normally call gtk_widget_grab_focus() to place the
;;; focus accordingly; if returning FALSE, they don't modify the current focus
;;; location.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; direction :
;;;     direction of focus movement
;;; 
;;; Returns :
;;;     TRUE if focus ended up inside widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_child_focus" gtk-widget-child-focus) :boolean
  (widget g-object)
  (direction gtk-direction-type))

(export 'gtk-widget-child-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_notify ()
;;; 
;;; void gtk_widget_child_notify (GtkWidget *widget,
;;;                               const gchar *child_property)
;;; 
;;; Emits a "child-notify" signal for the child property child_property on
;;; widget.
;;; 
;;; This is the analogue of g_object_notify() for child properties.
;;; 
;;; Also see gtk_container_child_notify().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; child_property :
;;;     the name of a child property installed on the class of widget's parent
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_child_notify" gtk-widget-child-notify) :void
  (widget (g-object gtk-widget))
  (property-name :string))

(export 'gtk-widget-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_freeze_child_notify ()
;;; 
;;; void gtk_widget_freeze_child_notify (GtkWidget *widget)
;;; 
;;; Stops emission of "child-notify" signals on widget. The signals are queued
;;; until gtk_widget_thaw_child_notify() is called on widget.
;;; 
;;; This is the analogue of g_object_freeze_notify() for child properties.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_freeze_child_notify" gtk-widget-freeze-child-notify) :void
  (widget g-object))

(export 'gtk-widget-freeze-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_visible ()
;;; 
;;; gboolean gtk_widget_get_child_visible (GtkWidget *widget)
;;; 
;;; Gets the value set with gtk_widget_set_child_visible(). If you feel a need
;;; to use this function, your code probably needs reorganization.
;;; 
;;; This function is only useful for container implementations and never should
;;; be called by an application.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is mapped with the parent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_parent ()
;;; 
;;; GtkWidget * gtk_widget_get_parent (GtkWidget *widget)
;;; 
;;; Returns the parent container of widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the parent container of widget, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_settings ()
;;; 
;;; GtkSettings * gtk_widget_get_settings (GtkWidget *widget)
;;; 
;;; Gets the settings object holding the settings used for this widget.
;;; 
;;; Note that this function can only be called when the GtkWidget is attached
;;; to a toplevel, since the settings object is specific to a particular
;;; GdkScreen.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the relevant GtkSettings object. [transfer none]
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_settings" gtk-widget-get-settings) g-object
  (widget g-object))

(export 'gtk-widget-get-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_clipboard ()
;;; 
;;; GtkClipboard * gtk_widget_get_clipboard (GtkWidget *widget,
;;;                                          GdkAtom selection)
;;; 
;;; Returns the clipboard object for the given selection to be used with widget.
;;; widget must have a GdkDisplay associated with it, so must be attached to a
;;; toplevel window.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; selection :
;;;     a GdkAtom which identifies the clipboard to use. GDK_SELECTION_CLIPBOARD
;;;     gives the default clipboard. Another common value is
;;;     GDK_SELECTION_PRIMARY, which gives the primary X selection.
;;; 
;;; Returns :
;;;     the appropriate clipboard object. If no clipboard already exists, a new
;;;     one will be created. Once a clipboard object has been created, it is
;;;     persistent for all time. [transfer none]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_clipboard" gtk-widget-clipboard) (g-object clipboard)
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string))

(export 'gtk-widget-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_display ()
;;; 
;;; GdkDisplay * gtk_widget_get_display (GtkWidget *widget)
;;; 
;;; Get the GdkDisplay for the toplevel window associated with this widget.
;;; This function can only be called after the widget has been added to a
;;; widget hierarchy with a GtkWindow at the top.
;;; 
;;; In general, you should only create display specific resources when a widget
;;; has been realized, and you should free those resources when the widget is
;;; unrealized.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the GdkDisplay for the toplevel for this widget.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_display" gtk-widget-get-display) g-object
  (widget g-object))

(export 'gtk-widget-get-display)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_root_window ()
;;; 
;;; GdkWindow * gtk_widget_get_root_window (GtkWidget *widget)
;;; 
;;; Get the root window where this widget is located. This function can only be
;;; called after the widget has been added to a widget hierarchy with GtkWindow
;;; at the top.
;;; 
;;; The root window is useful for such purposes as creating a popup GdkWindow
;;; associated with the window. In general, you should only create display
;;; specific resources when a widget has been realized, and you should free
;;; those resources when the widget is unrealized.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the GdkWindow root window for the toplevel for this widget.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_root_window" gtk-widget-get-root-window) g-object
  (widget g-object))

(export 'gtk-widget-get-root-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_screen ()
;;; 
;;; GdkScreen * gtk_widget_get_screen (GtkWidget *widget)
;;; 
;;; Get the GdkScreen from the toplevel window associated with this widget.
;;; This function can only be called after the widget has been added to a
;;; widget hierarchy with a GtkWindow at the top.
;;; 
;;; In general, you should only create screen specific resources when a widget
;;; has been realized, and you should free those resources when the widget is
;;; unrealized.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the GdkScreen for the toplevel for this widget. [transfer none]
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_screen" gtk-widget-get-screen) g-object
  (widget g-object))

(export 'gtk-widget-get-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_screen ()
;;; 
;;; gboolean gtk_widget_has_screen (GtkWidget *widget)
;;; 
;;; Checks whether there is a GdkScreen is associated with this widget. All
;;; toplevel widgets have an associated screen, and all widgets added into a
;;; hierarchy with a toplevel window at the top.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if there is a GdkScreen associcated with the widget.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_has_screen" gtk-widget-has-screen) :boolean
  (widget g-object))

(export 'gtk-widget-has-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_size_request ()
;;; 
;;; void gtk_widget_get_size_request (GtkWidget *widget,
;;;                                   gint *width,
;;;                                   gint *height);
;;; 
;;; Gets the size request that was explicitly set for the widget using 
;;; gtk_widget_set_size_request(). A value of -1 stored in width or height 
;;; indicates that that dimension has not been set explicitly and the natural 
;;; requisition of the widget will be used intead. See 
;;; gtk_widget_set_size_request(). To get the size a widget will actually
;;; request, call gtk_widget_get_preferred_size() instead of this function.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; width :
;;;     return location for width, or NULL.
;;; 
;;; height :
;;;     return location for height, or NULL.
;;; ----------------------------------------------------------------------------

(defun gtk-widget-get-size-request (widget)
  (values (gtk-widget-width-request widget)
          (gtk-widget-height-request widget)))

(export 'gtk-widget-get-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_child_visible ()
;;; 
;;; void gtk_widget_set_child_visible (GtkWidget *widget, gboolean is_visible);
;;; 
;;; Sets whether widget should be mapped along with its when its parent is 
;;; mapped and widget has been shown with gtk_widget_show().
;;; 
;;; The child visibility can be set for widget before it is added to a 
;;; container with gtk_widget_set_parent(), to avoid mapping children
;;; unnecessary before immediately unmapping them. However it will be reset to
;;; its default state of TRUE when the widget is removed from a container.
;;; 
;;; Note that changing the child visibility of a widget does not queue a 
;;; resize on the widget. Most of the time, the size of a widget is computed
;;; from all visible children, whether or not they are mapped. If this is not
;;; the case, the container can queue a resize itself.
;;; 
;;; This function is only useful for container implementations and never 
;;; should be called by an application.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; is_visible :
;;;     if TRUE, widget should be mapped along with its parent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_size_request ()
;;; 
;;; void gtk_widget_set_size_request (GtkWidget *widget,
;;;                                   gint width,
;;;                                   gint height);
;;; 
;;; Sets the minimum size of a widget; that is, the widget's size request will 
;;; be width by height. You can use this function to force a widget to be either
;;; larger or smaller than it normally would be.
;;; 
;;; In most cases, gtk_window_set_default_size() is a better choice for 
;;; toplevel windows than this function; setting the default size will still
;;; allow users to shrink the window. Setting the size request will force them
;;; to leave the window at least as large as the size request. When dealing
;;; with window sizes, gtk_window_set_geometry_hints() can be a useful function
;;; as well.
;;; 
;;; Note the inherent danger of setting any fixed size - themes, translations 
;;; into other languages, different fonts, and user action can all change the 
;;; appropriate size for a given widget. So, it's basically impossible to
;;; hardcode a size that will always be correct.
;;; 
;;; The size request of a widget is the smallest size a widget can accept 
;;; while still functioning well and drawing itself correctly. However in some 
;;; strange cases a widget may be allocated less than its requested size, and
;;; in many cases a widget may be allocated more space than it requested.
;;; 
;;; If the size request in a given direction is -1 (unset), then the "natural" 
;;; size request of the widget will be used instead.
;;; 
;;; Widgets can't actually be allocated a size less than 1 by 1, but you can 
;;; pass 0,0 to this function to mean "as small as possible."
;;; 
;;; The size request set here does not include any margin from the GtkWidget 
;;; properties margin-left, margin-right, margin-top, and margin-bottom, but it
;;; does include pretty much all other padding or border properties set by any
;;; subclass of GtkWidget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; width :
;;;     width widget should request, or -1 to unset
;;; 
;;; height :
;;;     height widget should request, or -1 to unset
;;; ----------------------------------------------------------------------------

(defun gtk-widget-set-size-request (widget width height)
  (setf (gtk-widget-width-request widget) width)
  (setf (gtk-widget-height-request widget) height))

(export 'gtk-widget-set-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_thaw_child_notify ()
;;; 
;;; void gtk_widget_thaw_child_notify (GtkWidget *widget)
;;; 
;;; Reverts the effect of a previous call to gtk_widget_freeze_child_notify().
;;; This causes all queued "child-notify" signals on widget to be emitted.
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_thaw_child_notify" gtk-widget-thaw-child-notify) :void
  (widget g-object))

(export 'gtk-widget-thaw-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_no_show_all ()
;;; 
;;; void gtk_widget_set_no_show_all (GtkWidget *widget, gboolean no_show_all)
;;; 
;;; Sets the "no-show-all" property, which determines whether calls to
;;; gtk_widget_show_all() will affect this widget.
;;; 
;;; This is mostly for use in constructing widget hierarchies with externally
;;; controlled visibility, see GtkUIManager.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; no_show_all :
;;;     the new value for the "no-show-all" property
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_no_show_all ()
;;; 
;;; gboolean gtk_widget_get_no_show_all (GtkWidget *widget);
;;; 
;;; Returns the current value of the "no-show-all" property, which determines 
;;; whether calls to gtk_widget_show_all() will affect this widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the current value of the "no-show-all" property.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_mnemonic_labels ()
;;; 
;;; GList * gtk_widget_list_mnemonic_labels (GtkWidget *widget)
;;; 
;;; Returns a newly allocated list of the widgets, normally labels, for which
;;; this widget is the target of a mnemonic (see for example,
;;; gtk_label_set_mnemonic_widget()).
;;; 
;;; The widgets in the list are not individually referenced. If you want to
;;; iterate through the list and perform actions involving callbacks that might
;;; destroy the widgets, you must call
;;; g_list_foreach (result, (GFunc)g_object_ref, NULL) first, and then unref
;;; all the widgets afterwards.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the list of mnemonic labels; free this list with g_list_free() when
;;;     you are done with it. [element-type GtkWidget][transfer container]
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_list_mnemonic_labels" gtk-widget-list-mnemonic-labels)
    (g-list (g-object gtk-widget) :free-from-foreign t)
  (widget (g-object gtk-widget)))

(export 'gtk-widget-list-mnemonic-labels)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_mnemonic_label ()
;;; 
;;; void gtk_widget_add_mnemonic_label (GtkWidget *widget, GtkWidget *label)
;;; 
;;; Adds a widget to the list of mnemonic labels for this widget.
;;; (See gtk_widget_list_mnemonic_labels()). Note the list of mnemonic labels
;;; for the widget is cleared when the widget is destroyed, so the caller must
;;; make sure to update its internal state at this point as well, by using a
;;; connection to the "destroy" signal or a weak notifier.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; label :
;;;     a GtkWidget that acts as a mnemonic label for widget
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_add_mnemonic_label" gtk-widget-add-mnemonic-label) :void
  (widget g-object)
  (label g-object))

(export 'gtk-widget-add-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_mnemonic_label ()
;;; 
;;; void gtk_widget_remove_mnemonic_label (GtkWidget *widget, GtkWidget *label)
;;; 
;;; Removes a widget from the list of mnemonic labels for this widget.
;;; (See gtk_widget_list_mnemonic_labels()). The widget must have previously
;;; been added to the list with gtk_widget_add_mnemonic_label().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; label :
;;;     a GtkWidget that was previously set as a mnemnic label for widget with
;;;     gtk_widget_add_mnemonic_label().
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_remove_mnemonic_label" gtk-widget-remove-mnemonic-label)
    :void
  (widget g-object)
  (label g-object))

(export 'gtk-widget-remove-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_composited ()
;;; 
;;; gboolean gtk_widget_is_composited (GtkWidget *widget)
;;; 
;;; Whether widget can rely on having its alpha channel drawn correctly. On X11
;;; this function returns whether a compositing manager is running for widget's
;;; screen.
;;; 
;;; Please note that the semantics of this call will change in the future if
;;; used on a widget that has a composited window in its hierarchy (as set by
;;; gdk_window_set_composited()).
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget can rely on its alpha channel being drawn correctly.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_is_composited" gtk-widget-is-composited) :boolean
  (widget g-object))

(export 'gtk-widget-is-composited)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_error_bell ()
;;; 
;;; void gtk_widget_error_bell (GtkWidget *widget)
;;; 
;;; Notifies the user about an input-related error on this widget. If the
;;; "gtk-error-bell" setting is TRUE, it calls gdk_window_beep(), otherwise it
;;; does nothing.
;;; 
;;; Note that the effect of gdk_window_beep() can be configured in many ways,
;;; depending on the windowing backend and the desktop environment or window
;;; manager that is used.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun (widget-error-bell "gtk_widget_error_bell") :void
  (widget g-object))

(export 'widget-error-bell)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_keynav_failed ()
;;; 
;;; gboolean gtk_widget_keynav_failed (GtkWidget *widget,
;;;                                    GtkDirectionType direction);
;;; 
;;; This function should be called whenever keyboard navigation within a 
;;; single widget hits a boundary. The function emits the "keynav-failed"
;;; signal on the widget and its return value should be interpreted in a way
;;; similar to the return value of gtk_widget_child_focus():
;;; 
;;; When TRUE is returned, stay in the widget, the failed keyboard navigation 
;;; is Ok and/or there is nowhere we can/should move the focus to.
;;; 
;;; When FALSE is returned, the caller should continue with keyboard 
;;; navigation outside the widget, e.g. by calling gtk_widget_child_focus() on
;;; the widget's toplevel.
;;; 
;;; The default ::keynav-failed handler returns TRUE for GTK_DIR_TAB_FORWARD 
;;; and GTK_DIR_TAB_BACKWARD. For the other values of GtkDirectionType, it looks
;;; at the "gtk-keynav-cursor-only" setting and returns FALSE if the setting is
;;; TRUE. This way the entire user interface becomes cursor-navigatable on input
;;; devices such as mobile phones which only have cursor keys but no tab key.
;;; 
;;; Whenever the default handler returns TRUE, it also calls 
;;; gtk_widget_error_bell() to notify the user of the failed keyboard
;;; navigation.
;;; 
;;; A use case for providing an own implementation of ::keynav-failed (either 
;;; by connecting to it or by overriding it) would be a row of GtkEntry widgets
;;; where the user should be able to navigate the entire row with the cursor
;;; keys, as e.g. known from user interfaces that require entering license keys.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; direction :
;;;     direction of focus movement
;;; 
;;; Returns :
;;;     TRUE if stopping keyboard navigation is fine, FALSE if the emitting 
;;;     widget should try to handle the keyboard navigation attempt in its
;;;     parent container(s).
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_tooltip_markup ()
;;; 
;;; gchar * gtk_widget_get_tooltip_markup (GtkWidget *widget);
;;; 
;;; Gets the contents of the tooltip for widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the tooltip text, or NULL. You should free the returned string with
;;;     g_free() when done.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_tooltip_markup ()
;;; 
;;; void gtk_widget_set_tooltip_markup (GtkWidget *widget, const gchar *markup)
;;; 
;;; Sets markup as the contents of the tooltip, which is marked up with the 
;;; Pango text markup language.
;;; 
;;; This function will take care of setting "has-tooltip" to TRUE and of the 
;;; default handler for the "query-tooltip" signal.
;;; 
;;; See also the "tooltip-markup" property and gtk_tooltip_set_markup().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; markup :
;;;     the contents of the tooltip for widget, or NULL. [allow-none]
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_tooltip_text ()
;;; 
;;; gchar * gtk_widget_get_tooltip_text (GtkWidget *widget);
;;; 
;;; Gets the contents of the tooltip for widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the tooltip text, or NULL. You should free the returned string with 
;;;     g_free() when done.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defun gtk-widget-get-tooltip-text (widget)
  (gtk-widget-tooltip-text widget))

(export 'gtk-widget-get-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_tooltip_text ()
;;; 
;;; void gtk_widget_set_tooltip_text (GtkWidget *widget, const gchar *text)
;;; 
;;; Sets text as the contents of the tooltip. This function will take care of 
;;; setting "has-tooltip" to TRUE and of the default handler for the 
;;; "query-tooltip" signal.
;;; 
;;; See also the "tooltip-text" property and gtk_tooltip_set_text().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; text :
;;;     the contents of the tooltip for widget
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defun gtk-widget-set-tooltip-text (widget text)
  (setf (gtk-widget-tooltip-text widget) text))

(export 'gtk-widget-set-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_tooltip_window ()
;;; 
;;; GtkWindow * gtk_widget_get_tooltip_window (GtkWidget *widget);
;;; 
;;; Returns the GtkWindow of the current tooltip. This can be the GtkWindow 
;;; created by default, or the custom tooltip window set using 
;;; gtk_widget_set_tooltip_window().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The GtkWindow of the current tooltip. [transfer none]
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_tooltip_window ()
;;; 
;;; void gtk_widget_set_tooltip_window (GtkWidget *widget,
;;;                                     GtkWindow *custom_window);
;;; 
;;; Replaces the default, usually yellow, window used for displaying tooltips 
;;; with custom_window. GTK+ will take care of showing and hiding custom_window
;;; at the right moment, to behave likewise as the default tooltip window. If 
;;; custom_window is NULL, the default tooltip window will be used.
;;; 
;;; If the custom window should have the default theming it needs to have the 
;;; name "gtk-tooltip", see gtk_widget_set_name().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; custom_window :
;;;     a GtkWindow, or NULL. [allow-none]
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_has_tooltip ()
;;; 
;;; gboolean gtk_widget_get_has_tooltip (GtkWidget *widget);
;;; 
;;; Returns the current value of the has-tooltip property. See "has-tooltip" 
;;; for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     current value of has-tooltip on widget.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_has_tooltip ()
;;; 
;;; void gtk_widget_set_has_tooltip (GtkWidget *widget, gboolean has_tooltip);
;;; 
;;; Sets the has-tooltip property on widget to has_tooltip. See "has-tooltip" 
;;; for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; has_tooltip :
;;;     whether or not widget has a tooltip.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_trigger_tooltip_query ()
;;; 
;;; void gtk_widget_trigger_tooltip_query (GtkWidget *widget)
;;; 
;;; Triggers a tooltip query on the display where the toplevel of widget is
;;; located. See gtk_tooltip_trigger_tooltip_query() for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tooltip_trigger_tooltip_query" gtk-widget-trigger-tooltip-query)
    :void
  (widget g-object))

(export 'gtk-widget-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_window ()
;;; 
;;; GdkWindow * gtk_widget_get_window (GtkWidget *widget);
;;; 
;;; Returns the widget's window if it is realized, NULL otherwise
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     widget's window. [transfer none]
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_should_draw_window ()
;;; 
;;; gboolean gtk_cairo_should_draw_window (cairo_t *cr, GdkWindow *window);
;;; 
;;; This function is supposed to be called in "draw" implementations for 
;;; widgets that support multiple windows. cr must be untransformed from
;;; invoking of the draw function. This function will return TRUE if the
;;; contents of the given window are supposed to be drawn and FALSE otherwise.
;;; Note that when the drawing was not initiated by the windowing system this
;;; function will return TRUE for all windows, so you need to draw the
;;; bottommost window first. Also, do not use "else if" statements to check
;;; which window should be drawn.
;;; 
;;; cr :
;;;     a cairo context
;;; 
;;; window :
;;;     the window to check. window may not be an input-only window.
;;; 
;;; Returns :
;;;     TRUE if window should be drawn
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_transform_to_window ()
;;; 
;;; void gtk_cairo_transform_to_window (cairo_t *cr,
;;;                                     GtkWidget *widget,
;;;                                     GdkWindow *window);
;;; 
;;; Transforms the given cairo context cr that from widget-relative 
;;; coordinates to window-relative coordinates. If the widget's window is not
;;; an ancestor of window, no modification will be applied.
;;; 
;;; This is the inverse to the transformation GTK applies when preparing an 
;;; expose event to be emitted with the "draw" signal. It is intended to help 
;;; porting multiwindow widgets from GTK+ 2 to the rendering architecture of
;;; GTK+ 3.
;;; 
;;; cr :
;;;     the cairo context to transform
;;; 
;;; widget :
;;;     the widget the context is currently centered for
;;; 
;;; window :
;;;     the window to transform the context to
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_width ()
;;; 
;;; int gtk_widget_get_allocated_width (GtkWidget *widget);
;;; 
;;; Returns the width that has currently been allocated to widget. This function
;;; is intended to be used when implementing handlers for the "draw" function.
;;; 
;;; widget :
;;;     the widget to query
;;; 
;;; Returns :
;;;     the width of the widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_height ()
;;; 
;;; int gtk_widget_get_allocated_height (GtkWidget *widget);
;;; 
;;; Returns the height that has currently been allocated to widget. This 
;;; function is intended to be used when implementing handlers for the "draw" 
;;; function.
;;; 
;;; widget :
;;;     the widget to query
;;; 
;;; Returns :
;;;     the height of the widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocation ()
;;; 
;;; void gtk_widget_get_allocation (GtkWidget *widget,
;;;                                 GtkAllocation *allocation)
;;; 
;;; Retrieves the widget's allocation.
;;; 
;;; Note, when implementing a GtkContainer: a widget's allocation will be its
;;; "adjusted" allocation, that is, the widget's parent container typically
;;; calls gtk_widget_size_allocate() with an allocation, and that allocation is
;;; then adjusted (to handle margin and alignment for example) before assignment
;;; to the widget. gtk_widget_get_allocation() returns the adjusted allocation
;;; that was actually assigned to the widget. The adjusted allocation is
;;; guaranteed to be completely contained within the gtk_widget_size_allocate()
;;; allocation, however. So a GtkContainer is guaranteed that its children stay
;;; inside the assigned bounds, but not that they have exactly the bounds the
;;; container assigned. There is no way to get the original allocation assigned
;;; by gtk_widget_size_allocate(), since it isn't stored; if a container
;;; implementation needs that information it will have to track it itself.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; allocation :
;;;     a pointer to a GtkAllocation to copy to
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;; With the type gtk-allocation we get an error. 
;; It works with the type gdk-rectangle.

(defcfun ("gtk_widget_get_allocation" %gtk-widget-get-allocation) :void
  (widget g-object)
  (allocation (g-boxed-foreign gdk-rectangle)))

(defun gtk-widget-get-allocation (widget)
  (let ((allocation (make-gdk-rectangle)))
    (%gtk-widget-get-allocation widget allocation)
    allocation))

(export 'gtk-widget-get-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_allocation ()
;;; 
;;; void gtk_widget_set_allocation (GtkWidget *widget,
;;;                                 const GtkAllocation *allocation);
;;; 
;;; Sets the widget's allocation. This should not be used directly, but from
;;; within a widget's size_allocate method.
;;; 
;;; The allocation set should be the "adjusted" or actual allocation. If you're
;;; implementing a GtkContainer, you want to use gtk_widget_size_allocate()
;;; instead of gtk_widget_set_allocation(). The
;;; GtkWidgetClass::adjust_size_allocation virtual method adjusts the allocation
;;; inside gtk_widget_size_allocate() to create an adjusted allocation.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; allocation :
;;;     a pointer to a GtkAllocation to copy from
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_app_paintable ()
;;; 
;;; gboolean gtk_widget_get_app_paintable (GtkWidget *widget)
;;; 
;;; Determines whether the application intends to draw on the widget in an
;;; "draw" handler.
;;; 
;;; See gtk_widget_set_app_paintable()
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is app paintable
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_can_default ()
;;; 
;;; gboolean gtk_widget_get_can_default (GtkWidget *widget)
;;; 
;;; Determines whether widget can be a default widget. See
;;; gtk_widget_set_can_default().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget can be a default widget, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-widget-get-can-default (widget)
  (gtk-widget-can-default widget))

(export 'gtk-widget-get-can-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_can_default ()
;;; 
;;; void gtk_widget_set_can_default (GtkWidget *widget, gboolean can_default)
;;; 
;;; Specifies whether widget can be a default widget.
;;; See gtk_widget_grab_default() for details about the meaning of "default".
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; can_default :
;;;     whether or not widget can be a default widget
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defun gtk-widget-set-can-default (widget can-default)
  (setf (gtk-widget-can-default widget) can-default))

(export 'gtk-widget-set-can-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_can_focus ()
;;; 
;;; gboolean gtk_widget_get_can_focus (GtkWidget *widget)
;;; 
;;; Determines whether widget can own the input focus.
;;; See gtk_widget_set_can_focus().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget can own the input focus, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_can_focus ()
;;; 
;;; void gtk_widget_set_can_focus (GtkWidget *widget, gboolean can_focus);
;;; 
;;; Specifies whether widget can own the input focus. See 
;;; gtk_widget_grab_focus() for actually setting the input focus on a widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; can_focus :
;;;     whether or not widget can own the input focus.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_double_buffered ()
;;; 
;;; gboolean gtk_widget_get_double_buffered (GtkWidget *widget);
;;; 
;;; Determines whether the widget is double buffered.
;;; 
;;; See gtk_widget_set_double_buffered()
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is double buffered
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_has_window ()
;;; 
;;; gboolean gtk_widget_get_has_window (GtkWidget *widget);
;;; 
;;; Determines whether widget has a GdkWindow of its own. See 
;;; gtk_widget_set_has_window().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget has a window, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_has_window ()
;;; 
;;; void gtk_widget_set_has_window (GtkWidget *widget, gboolean has_window);
;;; 
;;; Specifies whether widget has a GdkWindow of its own. Note that all 
;;; realized widgets have a non-NULL "window" pointer (gtk_widget_get_window() 
;;; never returns a NULL window when a widget is realized), but for many of them
;;; it's actually the GdkWindow of one of its parent widgets. Widgets that do
;;; not create a window for themselves in "realize" must announce this by
;;; calling this function with has_window = FALSE.
;;; 
;;; This function should only be called by widget implementations, and they 
;;; should call it in their init() function.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; has_window :
;;;     whether or not widget has a window.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_sensitive ()
;;; 
;;; gboolean gtk_widget_get_sensitive (GtkWidget *widget);
;;; 
;;; Returns the widget's sensitivity (in the sense of returning the value that 
;;; has been set using gtk_widget_set_sensitive()).
;;; 
;;; The effective sensitivity of a widget is however determined by both its 
;;; own and its parent widget's sensitivity. See gtk_widget_is_sensitive().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is sensitive
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_sensitive ()
;;; 
;;; gboolean gtk_widget_is_sensitive (GtkWidget *widget);
;;; 
;;; Returns the widget's effective sensitivity, which means it is sensitive 
;;; itself and also its parent widget is sensitive
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is effectively sensitive
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state ()
;;; 
;;; GtkStateType gtk_widget_get_state (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_state is deprecated and should not be used in newly-written
;;; code. 3.0. Use gtk_widget_get_state_flags() instead.
;;; 
;;; Returns the widget's state. See gtk_widget_set_state().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the state of widget.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_visible ()
;;; 
;;; gboolean gtk_widget_get_visible (GtkWidget *widget);
;;; 
;;; Determines whether the widget is visible. Note that this doesn't take into
;;; account whether the widget's parent is also visible or the widget is
;;; obscured in any way.
;;; 
;;; See gtk_widget_set_visible().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is visible
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_visible ()
;;; 
;;; void gtk_widget_set_visible (GtkWidget *widget, gboolean visible);
;;; 
;;; Sets the visibility state of widget. Note that setting this to TRUE doesn't
;;; mean the widget is actually viewable, see gtk_widget_get_visible().
;;; 
;;; This function simply calls gtk_widget_show() or gtk_widget_hide() but is
;;; nicer to use when the visibility of the widget depends on some condition.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; visible :
;;;     whether the widget should be shown or not
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_state_flags ()
;;; 
;;; void gtk_widget_set_state_flags (GtkWidget *widget,
;;;                                  GtkStateFlags flags,
;;;                                  gboolean clear);
;;; 
;;; This function is for use in widget implementations. Turns on flag values in
;;; the current widget state (insensitive, prelighted, etc.).
;;; 
;;; It is worth mentioning that any other state than GTK_STATE_FLAG_INSENSITIVE,
;;; will be propagated down to all non-internal children if widget is a
;;; GtkContainer, while GTK_STATE_FLAG_INSENSITIVE itself will be propagated
;;; down to all GtkContainer children by different means than turning on the
;;; state flag down the hierarchy, both gtk_widget_get_state_flags() and
;;; gtk_widget_is_sensitive() will make use of these.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; flags :
;;;     State flags to turn on
;;; 
;;; clear :
;;;     Whether to clear state before turning on flags
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unset_state_flags ()
;;; 
;;; void gtk_widget_unset_state_flags (GtkWidget *widget, GtkStateFlags flags)
;;; 
;;; This function is for use in widget implementations. Turns off flag values 
;;; for the current widget state (insensitive, prelighted, etc.). See 
;;; gtk_widget_set_state_flags().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; flags :
;;;     State flags to turn off
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state_flags ()
;;; 
;;; GtkStateFlags gtk_widget_get_state_flags (GtkWidget *widget);
;;; 
;;; Returns the widget state as a flag set. It is worth mentioning that the 
;;; effective GTK_STATE_FLAG_INSENSITIVE state will be returned, that is, also 
;;; based on parent insensitivity, even if widget itself is sensitive.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The state flags for widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_default ()
;;; 
;;; gboolean gtk_widget_has_default (GtkWidget *widget);
;;; 
;;; Determines whether widget is the current default widget within its 
;;; toplevel. See gtk_widget_set_can_default().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is the current default widget within its toplevel, 
;;;     FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_focus ()
;;; 
;;; gboolean gtk_widget_has_focus (GtkWidget *widget)
;;; 
;;; Determines if the widget has the global input focus. See 
;;; gtk_widget_is_focus() for the difference between having the global input 
;;; focus, and only having the focus within a toplevel.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget has the global input focus.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_visible_focus ()
;;; 
;;; gboolean gtk_widget_has_visible_focus (GtkWidget *widget);
;;; 
;;; Determines if the widget should show a visible indication that it has the
;;; global input focus. This is a convenience function for use in
;;; ::draw handlers that takes into account whether focus indication should
;;; currently be shown in the toplevel window of widget.
;;; See gtk_window_get_focus_visible() for more information about focus
;;; indication.
;;; 
;;; To find out if the widget has the global input focus, use
;;; gtk_widget_has_focus().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget should display a 'focus rectangle'
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_grab ()
;;; 
;;; gboolean gtk_widget_has_grab (GtkWidget *widget);
;;; 
;;; Determines whether the widget is currently grabbing events, so it is the
;;; only widget receiving input events (keyboard and mouse).
;;; 
;;; See also gtk_grab_add().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is in the grab_widgets stack
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_rc_style ()
;;; 
;;; gboolean gtk_widget_has_rc_style (GtkWidget *widget);
;;; 
;;; Warning
;;; 
;;; gtk_widget_has_rc_style has been deprecated since version 3.0 and should 
;;; not be used in newly-written code. Use GtkStyleContext instead.
;;; 
;;; Determines if the widget style has been looked up through the rc mechanism.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget has been looked up through the rc mechanism, FALSE otherwise.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_drawable ()
;;; 
;;; gboolean gtk_widget_is_drawable (GtkWidget *widget);
;;; 
;;; Determines whether widget can be drawn to. A widget can be drawn to if it 
;;; is mapped and visible.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is drawable, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_toplevel ()
;;; 
;;; gboolean gtk_widget_is_toplevel (GtkWidget *widget)
;;; 
;;; Determines whether widget is a toplevel widget.
;;; 
;;; Currently only GtkWindow and GtkInvisible (and out-of-process GtkPlugs) 
;;; are toplevel widgets. Toplevel widgets have no parent widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is a toplevel, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_window ()
;;; 
;;; void gtk_widget_set_window (GtkWidget *widget, GdkWindow *window);
;;; 
;;; Sets a widget's window. This function should only be used in a widget's
;;; "realize" implementation. The window passed is usually either new window
;;; created with gdk_window_new(), or the window of its parent widget as
;;; returned by gtk_widget_get_parent_window().
;;; 
;;; Widgets must indicate whether they will create their own GdkWindow by
;;; calling gtk_widget_set_has_window(). This is usually done in the widget's
;;; init() function.
;;; 
;;; Note
;;; 
;;; This function does not add any reference to window.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; window :
;;;     a GdkWindow. [transfer full]
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_receives_default ()
;;; 
;;; void gtk_widget_set_receives_default (GtkWidget *widget,
;;;                                       gboolean receives_default);
;;; 
;;; Specifies whether widget will be treated as the default widget within its 
;;; toplevel when it has the focus, even if another widget is the default.
;;; 
;;; See gtk_widget_grab_default() for details about the meaning of "default".
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; receives_default :
;;;     whether or not widget can be a default widget.
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_receives_default ()
;;; 
;;; gboolean gtk_widget_get_receives_default (GtkWidget *widget);
;;; 
;;; Determines whether widget is alyways treated as default widget withing its 
;;; toplevel when it has the focus, even if another widget is the default.
;;; 
;;; See gtk_widget_set_receives_default().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget acts as default widget when focussed, FALSE otherwise
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_support_multidevice ()
;;; 
;;; void gtk_widget_set_support_multidevice (GtkWidget *widget,
;;;                                          gboolean support_multidevice);
;;; 
;;; Enables or disables multiple pointer awareness. If this setting is TRUE, 
;;; widget will start receiving multiple, per device enter/leave events. Note
;;; that if custom GdkWindows are created in "realize", 
;;; gdk_window_set_support_multidevice() will have to be called manually on
;;; them.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; support_multidevice :
;;;     TRUE to support input from multiple devices.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_support_multidevice ()
;;; 
;;; gboolean gtk_widget_get_support_multidevice (GtkWidget *widget);
;;; 
;;; Returns TRUE if widget is multiple pointer aware. See 
;;; gtk_widget_set_support_multidevice() for more information.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is multidevice aware.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_realized ()
;;; 
;;; void gtk_widget_set_realized (GtkWidget *widget, gboolean realized)
;;; 
;;; Marks the widget as being realized.
;;; 
;;; This function should only ever be called in a derived widget's "realize"
;;; or "unrealize" implementation.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; realized :
;;;     TRUE to mark the widget as realized
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_realized ()
;;; 
;;; gboolean gtk_widget_get_realized (GtkWidget *widget);
;;; 
;;; Determines whether widget is realized.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if widget is realized, FALSE otherwise
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_mapped ()
;;; 
;;; void gtk_widget_set_mapped (GtkWidget *widget, gboolean mapped);
;;; 
;;; Marks the widget as being realized.
;;; 
;;; This function should only ever be called in a derived widget's "map" or
;;; "unmap" implementation.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; mapped :
;;;     TRUE to mark the widget as mapped
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_mapped ()
;;; 
;;; gboolean gtk_widget_get_mapped (GtkWidget *widget);
;;; 
;;; Whether the widget is mapped.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     TRUE if the widget is mapped, FALSE otherwise.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_requisition ()
;;; 
;;; void gtk_widget_get_requisition (GtkWidget *widget,
;;;                                  GtkRequisition *requisition);
;;; 
;;; Warning
;;; 
;;; gtk_widget_get_requisition has been deprecated since version 3.0 and 
;;; should not be used in newly-written code. The GtkRequisition cache on the 
;;; widget was removed, If you need to cache sizes across requests and 
;;; allocations, add an explicit cache to the widget in question instead.
;;; 
;;; Retrieves the widget's requisition.
;;; 
;;; This function should only be used by widget implementations in order to 
;;; figure whether the widget's requisition has actually changed after some 
;;; internal state change (so that they can call gtk_widget_queue_resize()
;;; instead of gtk_widget_queue_draw()).
;;; 
;;; Normally, gtk_widget_size_request() should be used.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; requisition :
;;;     a pointer to a GtkRequisition to copy to. [out]
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_device_is_shadowed ()
;;; 
;;; gboolean gtk_widget_device_is_shadowed (GtkWidget *widget,
;;;                                         GdkDevice *device);
;;; 
;;; Returns TRUE if device has been shadowed by a GTK+ device grab on another 
;;; widget, so it would stop sending events to widget. This may be used in the 
;;; "grab-notify" signal to check for specific devices.
;;; See gtk_device_grab_add().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; device :
;;;     a GdkDevice
;;; 
;;; Returns :
;;;     TRUE if there is an ongoing grab on device by another GtkWidget than
;;;     widget.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_path ()
;;; 
;;; GtkWidgetPath * gtk_widget_get_path (GtkWidget *widget);
;;; 
;;; Returns the GtkWidgetPath representing widget, if the widget is not 
;;; connected to a toplevel widget, a partial path will be created.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The GtkWidgetPath representing widget. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style_context ()
;;; 
;;; GtkStyleContext * gtk_widget_get_style_context (GtkWidget *widget)
;;; 
;;; Returns the style context associated to widget.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     a GtkStyleContext. This memory is owned by widget and must not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_style ()
;;; 
;;; void gtk_widget_reset_style (GtkWidget *widget);
;;; 
;;; Updates the style context of widget and all descendents by updating its 
;;; widget path. GtkContainers may want to use this on a child when reordering
;;; it in a way that a different style might apply to it. See also 
;;; gtk_container_get_path_for_child().
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_new ()
;;; 
;;; GtkRequisition * gtk_requisition_new (void);
;;; 
;;; Allocates a new GtkRequisition structure and initializes its elements to 
;;; zero.
;;; 
;;; Returns :
;;;     a new empty GtkRequisition. The newly allocated GtkRequisition should 
;;;     be freed with gtk_requisition_free().
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_copy ()
;;; 
;;; GtkRequisition * gtk_requisition_copy (const GtkRequisition *requisition);
;;; 
;;; Copies a GtkRequisition.
;;; 
;;; requisition :
;;;     a GtkRequisition
;;; 
;;; Returns :
;;;     a copy of requisition
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_free ()
;;; 
;;; void gtk_requisition_free (GtkRequisition *requisition)
;;; 
;;; Frees a GtkRequisition.
;;; 
;;; requisition :
;;;     a GtkRequisition
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkSizeRequestMode
;;; 
;;; typedef enum {
;;;   GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH = 0,
;;;   GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT,
;;;   GTK_SIZE_REQUEST_CONSTANT_SIZE
;;; } GtkSizeRequestMode;
;;; 
;;; Specifies a preference for height-for-width or width-for-height geometry 
;;; management.
;;; 
;;; GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH
;;;     Prefer height-for-width geometry management
;;; 
;;; GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT
;;;     Prefer width-for-height geometry management
;;; 
;;; GTK_SIZE_REQUEST_CONSTANT_SIZE
;;;     Dont trade height-for-width or width-for-height
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkRequestedSize
;;; 
;;; struct GtkRequestedSize {
;;;   gpointer data;
;;;   gint     minimum_size;
;;;   gint     natural_size;
;;; };
;;; 
;;; Represents a request of a screen object in a given orientation. These are 
;;; primarily used in container implementations when allocating a natural size
;;; for children calling. See gtk_distribute_natural_allocation().
;;; 
;;; gpointer data;
;;;     A client pointer
;;; 
;;; gint minimum_size;
;;;     The minimum size needed for allocation in a given orientation
;;; 
;;; gint natural_size;
;;;     The natural size for allocation in a given orientation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_height ()
;;; 
;;; void gtk_widget_get_preferred_height (GtkWidget *widget,
;;;                                       gint *minimum_height,
;;;                                       gint *natural_height);
;;; 
;;; Retrieves a widget's initial minimum and natural height.
;;; 
;;; Note
;;; 
;;; This call is specific to width-for-height requests.
;;; 
;;; The returned request will be modified by the 
;;; GtkWidgetClass::adjust_size_request virtual method and by any GtkSizeGroups
;;; that have been applied. That is, the returned request is the one that should
;;; be used for layout, not necessarily the one returned by the widget itself.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; minimum_height :
;;;     location to store the minimum height, or NULL. [out][allow-none]
;;; 
;;; natural_height :
;;;     location to store the natural height, or NULL. [out][allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_width ()
;;; 
;;; void gtk_widget_get_preferred_width (GtkWidget *widget,
;;;                                      gint *minimum_width,
;;;                                      gint *natural_width);
;;; 
;;; Retrieves a widget's initial minimum and natural width.
;;; 
;;; Note
;;; 
;;; This call is specific to height-for-width requests.
;;; 
;;; The returned request will be modified by the 
;;; GtkWidgetClass::adjust_size_request virtual method and by any GtkSizeGroups
;;; that have been applied. That is, the returned request is the one that should
;;; be used for layout, not necessarily the one returned by the widget itself.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; minimum_width :
;;;     location to store the minimum width, or NULL. [out][allow-none]
;;; 
;;; natural_width :
;;;     location to store the natural width, or NULL. [out][allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_height_for_width ()
;;; 
;;; void gtk_widget_get_preferred_height_for_width (GtkWidget *widget,
;;;                                                 gint width,
;;;                                                 gint *minimum_height,
;;;                                                 gint *natural_height);
;;; 
;;; Retrieves a widget's minimum and natural height if it would be given the 
;;; specified width.
;;; 
;;; The returned request will be modified by the 
;;; GtkWidgetClass::adjust_size_request virtual method and by any GtkSizeGroups
;;; that have been applied. That is, the returned request is the one that should
;;; be used for layout, not necessarily the one returned by the widget itself.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; width :
;;;     the width which is available for allocation
;;; 
;;; minimum_height :
;;;     location for storing the minimum height, or NULL. [out][allow-none]
;;; 
;;; natural_height :
;;;     location for storing the natural height, or NULL. [out][allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_width_for_height ()
;;; 
;;; void gtk_widget_get_preferred_width_for_height (GtkWidget *widget,
;;;                                                 gint height,
;;;                                                 gint *minimum_width,
;;;                                                 gint *natural_width);
;;; 
;;; Retrieves a widget's minimum and natural width if it would be given the 
;;; specified height.
;;; 
;;; The returned request will be modified by the 
;;; GtkWidgetClass::adjust_size_request virtual method and by any GtkSizeGroups
;;; that have been applied. That is, the returned request is the one that should
;;; be used for layout, not necessarily the one returned by the widget itself.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; height :
;;;     the height which is available for allocation
;;; 
;;; minimum_width :
;;;     location for storing the minimum width, or NULL. [out][allow-none]
;;; 
;;; natural_width :
;;;     location for storing the natural width, or NULL. [out][allow-none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_request_mode ()
;;; 
;;; GtkSizeRequestMode gtk_widget_get_request_mode (GtkWidget *widget);
;;; 
;;; Gets whether the widget prefers a height-for-width layout or a 
;;; width-for-height layout.
;;; 
;;; Note
;;; 
;;; GtkBin widgets generally propagate the preference of their child, 
;;; container widgets need to request something either in context of their 
;;; children or in context of their allocation capabilities.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; Returns :
;;;     The GtkSizeRequestMode preferred by widget.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_size ()
;;; 
;;; void gtk_widget_get_preferred_size (GtkWidget *widget,
;;;                                     GtkRequisition *minimum_size,
;;;                                     GtkRequisition *natural_size);
;;; 
;;; Retrieves the minimum and natural size of a widget, taking into account 
;;; the widget's preference for height-for-width management.
;;; 
;;; This is used to retrieve a suitable size by container widgets which do not 
;;; impose any restrictions on the child placement. It can be used to deduce 
;;; toplevel window and menu sizes as well as child widgets in free-form 
;;; containers such as GtkLayout.
;;; 
;;; Note
;;; 
;;; Handle with care. Note that the natural height of a height-for-width 
;;; widget will generally be a smaller size than the minimum height, since the 
;;; required height for the natural width is generally smaller than the required
;;; height for the minimum width.
;;; 
;;; widget :
;;;     a GtkWidget instance
;;; 
;;; minimum_size :
;;;     location for storing the minimum size, or NULL
;;; 
;;; natural_size :
;;;     location for storing the natural size, or NULL
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_distribute_natural_allocation ()
;;; 
;;; gint gtk_distribute_natural_allocation (gint extra_space,
;;;                                         guint n_requested_sizes,
;;;                                         GtkRequestedSize *sizes);
;;; 
;;; Distributes extra_space to child sizes by bringing smaller children up to 
;;; natural size first.
;;; 
;;; The remaining space will be added to the minimum_size member of the 
;;; GtkRequestedSize struct. If all sizes reach their natural size then the 
;;; remaining space is returned.
;;; 
;;; extra_space :
;;;     Extra space to redistribute among children after subtracting minimum 
;;;     sizes and any child padding from the overall allocation
;;; 
;;; n_requested_sizes :
;;;     Number of requests to fit into the allocation
;;; 
;;; sizes :
;;;     An array of structs with a client pointer and a minimum/natural size 
;;;     in the orientation of the allocation.
;;; 
;;; Returns :
;;;     The remainder of extra_space after redistributing space to sizes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkAlign
;;; 
;;; typedef enum {
;;;   GTK_ALIGN_FILL,
;;;   GTK_ALIGN_START,
;;;   GTK_ALIGN_END,
;;;   GTK_ALIGN_CENTER
;;; } GtkAlign;
;;; 
;;; Controls how a widget deals with extra space in a single (x or y) dimension.
;;; 
;;; Alignment only matters if the widget receives a "too large" allocation, 
;;; for example if you packed the widget with the "expand" flag inside a GtkBox,
;;; then the widget might get extra space. If you have for example a 16x16 icon
;;; inside a 32x32 space, the icon could be scaled and stretched, it could be 
;;; centered, or it could be positioned to one side of the space.
;;; 
;;; Note that in horizontal context GTK_ALIGN_START and GTK_ALIGN_END are 
;;; interpreted relative to text direction.
;;; 
;;; GTK_ALIGN_FILL
;;;     stretch to fill all space if possible, center if no meaningful way to 
;;;     stretch
;;; 
;;; GTK_ALIGN_START
;;;     snap to left or top side, leaving space on right or bottom
;;; 
;;; GTK_ALIGN_END
;;;     snap to right or bottom side, leaving space on left or top
;;; 
;;; GTK_ALIGN_CENTER
;;;     center natural width of widget inside the allocation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_halign ()
;;; 
;;; GtkAlign gtk_widget_get_halign (GtkWidget *widget);
;;; 
;;; Gets the value of the "halign" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the horizontal alignment of widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_halign ()
;;; 
;;; void gtk_widget_set_halign (GtkWidget *widget, GtkAlign align)
;;; 
;;; Sets the horizontal alignment of widget. See the "halign" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; align :
;;;     the horizontal alignment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_valign ()
;;; 
;;; GtkAlign gtk_widget_get_valign (GtkWidget *widget)
;;; 
;;; Gets the value of the "valign" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     the vertical alignment of widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_valign ()
;;; 
;;; void gtk_widget_set_valign (GtkWidget *widget, GtkAlign align);
;;; 
;;; Sets the vertical alignment of widget. See the "valign" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; align :
;;;     the vertical alignment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_margin_left ()
;;; 
;;; gint gtk_widget_get_margin_left (GtkWidget *widget);
;;; 
;;; Gets the value of the "margin-left" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The left margin of widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_margin_left ()
;;; 
;;; void gtk_widget_set_margin_left (GtkWidget *widget, gint margin);
;;; 
;;; Sets the left margin of widget. See the "margin-left" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; margin :
;;;     the left margin
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_margin_right ()
;;; 
;;; gint gtk_widget_get_margin_right (GtkWidget *widget);
;;; 
;;; Gets the value of the "margin-right" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The right margin of widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_margin_right ()
;;; 
;;; void gtk_widget_set_margin_right (GtkWidget *widget, gint margin);
;;; 
;;; Sets the right margin of widget. See the "margin-right" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; margin :
;;;     the right margin
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_margin_top ()
;;; 
;;; gint gtk_widget_get_margin_top (GtkWidget *widget);
;;; 
;;; Gets the value of the "margin-top" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The top margin of widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_margin_top ()
;;; 
;;; void gtk_widget_set_margin_top (GtkWidget *widget, gint margin);
;;; 
;;; Sets the top margin of widget. See the "margin-top" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; margin :
;;;     the top margin
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_margin_bottom ()
;;; 
;;; gint gtk_widget_get_margin_bottom (GtkWidget *widget);
;;; 
;;; Gets the value of the "margin-bottom" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; Returns :
;;;     The bottom margin of widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_margin_bottom ()
;;; 
;;; void gtk_widget_set_margin_bottom (GtkWidget *widget, gint margin);
;;; 
;;; Sets the bottom margin of widget. See the "margin-bottom" property.
;;; 
;;; widget :
;;;     a GtkWidget
;;; 
;;; margin :
;;;     the bottom margin
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_hexpand ()
;;; 
;;; gboolean gtk_widget_get_hexpand (GtkWidget *widget);
;;; 
;;; Gets whether the widget would like any available extra horizontal space. 
;;; When a user resizes a GtkWindow, widgets with expand=TRUE generally receive
;;; the extra space. For example, a list or scrollable area or document in your
;;; window would often be set to expand.
;;; 
;;; Containers should use gtk_widget_compute_expand() rather than this 
;;; function, to see whether a widget, or any of its children, has the expand
;;; flag set. If any child of a widget wants to expand, the parent may ask to
;;; expand also.
;;; 
;;; This function only looks at the widget's own hexpand flag, rather than 
;;; computing whether the entire widget tree rooted at this widget wants to
;;; expand.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; Returns :
;;;     whether hexpand flag is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_hexpand ()
;;; 
;;; void gtk_widget_set_hexpand (GtkWidget *widget, gboolean expand);
;;; 
;;; Sets whether the widget would like any available extra horizontal space. 
;;; When a user resizes a GtkWindow, widgets with expand=TRUE generally receive 
;;; the extra space. For example, a list or scrollable area or document in your 
;;; window would often be set to expand.
;;; 
;;; Call this function to set the expand flag if you would like your widget to 
;;; become larger horizontally when the window has extra room.
;;; 
;;; By default, widgets automatically expand if any of their children want to 
;;; expand. (To see if a widget will automatically expand given its current 
;;; children and state, call gtk_widget_compute_expand(). A container can decide
;;; how the expandability of children affects the expansion of the container by
;;; overriding the compute_expand virtual method on GtkWidget.).
;;; 
;;; Setting hexpand explicitly with this function will override the automatic 
;;; expand behavior.
;;; 
;;; This function forces the widget to expand or not to expand, regardless of 
;;; children. The override occurs because gtk_widget_set_hexpand() sets the 
;;; hexpand-set property (see gtk_widget_set_hexpand_set()) which causes the 
;;; widget's hexpand value to be used, rather than looking at children and
;;; widget state.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; expand :
;;;     whether to expand
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_hexpand_set ()
;;; 
;;; gboolean gtk_widget_get_hexpand_set (GtkWidget *widget);
;;; 
;;; Gets whether gtk_widget_set_hexpand() has been used to explicitly set the 
;;; expand flag on this widget.
;;; 
;;; If hexpand is set, then it overrides any computed expand value based on 
;;; child widgets. If hexpand is not set, then the expand value depends on
;;; whether any children of the widget would like to expand.
;;; 
;;; There are few reasons to use this function, but it's here for completeness 
;;; and consistency.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; Returns :
;;;     whether hexpand has been explicitly set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_hexpand_set ()
;;; 
;;; void gtk_widget_set_hexpand_set (GtkWidget *widget, gboolean set);
;;; 
;;; Sets whether the hexpand flag (see gtk_widget_get_hexpand()) will be used.
;;; 
;;; The hexpand-set property will be set automatically when you call 
;;; gtk_widget_set_hexpand() to set hexpand, so the most likely reason to use
;;; this function would be to unset an explicit expand flag.
;;; 
;;; If hexpand is set, then it overrides any computed expand value based on 
;;; child widgets. If hexpand is not set, then the expand value depends on
;;; whether any children of the widget would like to expand.
;;; 
;;; There are few reasons to use this function, but it's here for completeness 
;;; and consistency.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; set :
;;;     value for hexpand-set property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_vexpand ()
;;; 
;;; gboolean gtk_widget_get_vexpand (GtkWidget *widget);
;;; 
;;; Gets whether the widget would like any available extra vertical space.
;;; 
;;; See gtk_widget_get_hexpand() for more detail.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; Returns :
;;;     whether vexpand flag is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_vexpand ()
;;; 
;;; void gtk_widget_set_vexpand (GtkWidget *widget, gboolean expand);
;;; 
;;; Sets whether the widget would like any available extra vertical space.
;;; 
;;; See gtk_widget_set_hexpand() for more detail.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; expand :
;;;     whether to expand
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_vexpand_set ()
;;; 
;;; gboolean gtk_widget_get_vexpand_set (GtkWidget *widget);
;;; 
;;; Gets whether gtk_widget_set_vexpand() has been used to explicitly set the 
;;; expand flag on this widget.
;;; 
;;; See gtk_widget_get_hexpand_set() for more detail.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; Returns :
;;;     whether vexpand has been explicitly set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_vexpand_set ()
;;; 
;;; void gtk_widget_set_vexpand_set (GtkWidget *widget, gboolean set);
;;; 
;;; Sets whether the vexpand flag (see gtk_widget_get_vexpand()) will be used.
;;; 
;;; See gtk_widget_set_hexpand_set() for more detail.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; set :
;;;     value for vexpand-set property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_compute_expand ()
;;; 
;;; void gtk_widget_queue_compute_expand (GtkWidget *widget);
;;; 
;;; Mark widget as needing to recompute its expand flags. Call this function 
;;; when setting legacy expand child properties on the child of a container.
;;; 
;;; See gtk_widget_compute_expand().
;;; 
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_expand ()
;;; 
;;; gboolean gtk_widget_compute_expand (GtkWidget *widget,
;;;                                     GtkOrientation orientation);
;;; 
;;; Computes whether a container should give this widget extra space when 
;;; possible. Containers should check this, rather than looking at 
;;; gtk_widget_get_hexpand() or gtk_widget_get_vexpand().
;;; 
;;; This function already checks whether the widget is visible, so visibility 
;;; does not need to be checked separately. Non-visible widgets are not
;;; expanded.
;;; 
;;; The computed expand value uses either the expand setting explicitly set on 
;;; the widget itself, or, if none has been explicitly set, the widget may
;;; expand if some of its children do.
;;; 
;;; widget :
;;;     the widget
;;; 
;;; orientation :
;;;     expand direction
;;; 
;;; Returns :
;;;     whether widget tree rooted here should be expanded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_WIDGET_SET_FLAGS()
;;;
;;; #define GTK_WIDGET_SET_FLAGS(wid,flag)
;;;                G_STMT_START{ (GTK_WIDGET_FLAGS (wid) |= (flag)); }G_STMT_END
;;;
;;; Warning
;;; 
;;; GTK_WIDGET_SET_FLAGS has been deprecated since version 2.22 and should not
;;; be used in newly-written code. Use the proper function instead:
;;;
;;;  gtk_widget_set_app_paintable(),
;;;  gtk_widget_set_can_default(),
;;;  gtk_widget_set_can_focus(), 
;;;  gtk_widget_set_double_buffered(),
;;;  gtk_widget_set_has_window(),
;;;  gtk_widget_set_mapped(),
;;;  gtk_widget_set_no_show_all(),
;;;  gtk_widget_set_realized(),
;;;  gtk_widget_set_receives_default(),
;;;  gtk_widget_set_sensitive() or
;;;  gtk_widget_set_visible().
;;; 
;;; Turns on certain widget flags.
;;; 
;;; wid :
;;;     a GtkWidget.
;;; 
;;; flag :
;;;     the flags to set.
;;; ----------------------------------------------------------------------------

(defun gtk-widget-flags (widget)
  (convert-from-foreign (gtk-object-flags-as-integer widget) 'gtk-widget-flags))

(defun (setf gtk-widget-flags) (new-value widget)
  (setf (gtk-object-flags-as-integer widget)
        (convert-to-foreign new-value 'gtk-widget-flags))
  new-value)

(export 'gtk-widget-flags)

;;; --- End of file gtk.widget.lisp --------------------------------------------
