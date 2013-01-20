;;; ----------------------------------------------------------------------------
;;; atdoc-gdk.package.lisp
;;;
;;; Documentation strings for the library GDK.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(in-package :gdk)

(setf (documentation (find-package :gdk) t)
 "@em{GDK} is an intermediate layer which isolates GTK+ from the details of the
  windowing system.

  This is the API documentation of a Lisp binding to @em{GDK}.
  @begin[General]{section}
    Library initialization and miscellaneous functions
  @end{section}
  @begin[GdkDisplayManager]{section}
    Maintains a list of all open GdkDisplays
  @end{section}
  @begin[GdkDisplay]{section}
    Controls a set of GdkScreens and their associated input devices
  @end{section}
  @begin[GdkScreen]{section}
    Object representing a physical screen
  @end{section}
  @begin[GdkDeviceManager]{section}
    Functions for handling input devices
  @end{section}
  @begin[GdkDevice]{section}
    Object representing an input device
  @end{section}
  @begin[Points and Rectangles]{section}
    Simple graphical data types
  @end{section}
  @begin[Pixbufs]{section}
    Functions for obtaining pixbufs
  @end{section}
  @begin[Colors]{section}
    Manipulation of colors

    @about-class{gdk-color}
    @about-function{gdk-color-copy}
    @about-function{gdk-color-free}
    @about-function{gdk-color-parse}
    @about-function{gdk-color-equal}
    @about-function{gdk-color-hash}
    @about-function{gdk-color-string}
  @end{section}
  @begin[RGBA Colors]{section}
    RGBA colors
  @end{section}
  @begin[Visuals]{section}
    Low-level display hardware information
  @end{section}
  @begin[Cursors]{section}
    Standard and pixmap cursors
  @end{section}
  @begin[Windows]{section}
    Onscreen display areas in the target window system.

    @about-class{gdk-window}
    @about-symbol{gdk-window-type}
    @about-symbol{gdk-window-window-class}
    @about-symbol{gdk-window-hints}
    @about-symbol{gdk-geometry}
    @about-symbol{gdk-gravity}
    @about-symbol{gdk-window-edge}
    @about-symbol{gdk-window-type-hint}
    @about-symbol{gdk-window-attr}
    @about-symbol{gdk-window-attributes-type}
    @about-function{gdk-window-new}
    @about-function{gdk-window-destroy}
    @about-function{gdk-window-get_window_type}
    @about-function{gdk-window-get_display}
    @about-function{gdk-window-get_screen}
    @about-function{gdk-window-get_visual}
    @about-function{gdk-window-at_pointer}
    @about-function{gdk-window-show}
    @about-function{gdk-window-show_unraised}
    @about-function{gdk-window-hide}
    @about-function{gdk-window-is_destroyed}
    @about-function{gdk-window-is_visible}
    @about-function{gdk-window-is_viewable}
    @about-function{gdk-window-is_input_only}
    @about-function{gdk-window-is_shaped}
    @about-function{gdk-window-get_state}
    @about-function{gdk-window-withdraw}
    @about-function{gdk-window-iconify}
    @about-function{gdk-window-deiconify}
    @about-function{gdk-window-stick}
    @about-function{gdk-window-unstick}
    @about-function{gdk-window-maximize}
    @about-function{gdk-window-unmaximize}
    @about-function{gdk-window-fullscreen}
    @about-function{gdk-window-unfullscreen}
    @about-function{gdk-window-set_keep_above}
    @about-function{gdk-window-set_keep_below}
    @about-function{gdk-window-set_opacity}
    @about-function{gdk-window-set_composited}
    @about-function{gdk-window-get_composited}
    @about-function{gdk-window-move}
    @about-function{gdk-window-resize}
    @about-function{gdk-window-move_resize}
    @about-function{gdk-window-scroll}
    @about-function{gdk-window-move_region}
    @about-function{gdk-window-flush}
    @about-function{gdk-window-has_native}
    @about-function{gdk-window-ensure_native}
    @about-function{gdk-window-reparent}
    @about-function{gdk-window-raise}
    @about-function{gdk-window-lower}
    @about-function{gdk-window-restack}
    @about-function{gdk-window-focus}
    @about-function{gdk-window-register_dnd}
    @about-function{gdk-window-begin_resize_drag}
    @about-function{gdk-window-begin_resize_drag_for_device}
    @about-function{gdk-window-begin_move_drag}
    @about-function{gdk-window-begin_move_drag_for_device}
    @about-function{gdk-window-constrain_size}
    @about-function{gdk-window-beep}
    @about-function{gdk-window-get_clip_region}
    @about-function{gdk-window-begin_paint_rect}
    @about-function{gdk-window-begin_paint_region}
    @about-function{gdk-window-end_paint}
    @about-function{gdk-window-get_visible_region}
    @about-function{gdk-window-invalidate_rect}
    @about-function{gdk-window-invalidate_region}
    @about-function{gdk-window-invalidate_maybe_recurse}
    @about-function{gdk-window-get_update_area}
    @about-function{gdk-window-freeze_updates}
    @about-function{gdk-window-thaw_updates}
    @about-function{gdk-window-process_all_updates}
    @about-function{gdk-window-process_updates}
    @about-function{gdk-window-set_debug_updates}
    @about-function{gdk-window-enable_synchronized_configure}
    @about-function{gdk-window-configure_finished}
    @about-function{gdk-window-set_user_data}
    @about-function{gdk-window-set_override_redirect}
    @about-function{gdk-window-set_accept_focus}
    @about-function{gdk-window-get_accept_focus}
    @about-function{gdk-window-set_focus_on_map}
    @about-function{gdk-window-get_focus_on_map}
    @about-function{gdk-window-add_filter}
    @about-function{gdk-window-remove_filter}
    @about-symbol{gdk-filter-return}
    @about-symbol{gdk-x-event}
    @about-function{gdk-window-shape_combine_region}
    @about-function{gdk-window-set_child_shapes}
    @about-function{gdk-window-merge_child_shapes}
    @about-function{gdk-window-input_shape_combine_region}
    @about-function{gdk-window-set_child_input_shapes}
    @about-function{gdk-window-merge_child_input_shapes}
    @about-function{gdk-window-set_static_gravities}
    @about-function{gdk-window-set_title}
    @about-function{gdk-window-set_background}
    @about-function{gdk-window-set_background_rgba}
    @about-function{gdk-window-set_background_pattern}
    @about-function{gdk-window-get_background_patter}
    @about-symbol{GDK_PARENT_RELATIVE}
    @about-function{gdk-window-get_user_data}
    @about-function{gdk-window-get_geometry}
    @about-function{gdk-window-set_geometry_hints}
    @about-function{gdk-window-get_width}
    @about-function{gdk-window-get_height}
    @about-function{gdk-window-set_icon_list}
    @about-function{gdk-window-set_modal_hint}
    @about-function{gdk-window-get_modal_hint}
    @about-function{gdk-window-set_type_hint}
    @about-function{gdk-window-get_type_hint}
    @about-function{gdk-window-set_skip_taskbar_hint}
    @about-function{gdk-window-set_skip_pager_hint}
    @about-function{gdk-window-set_urgency_hint}
    @about-function{gdk-window-get_position}
    @about-function{gdk-window-get_root_origin}
    @about-function{gdk-window-get_frame_extents}
    @about-function{gdk-window-get_origin}
    @about-function{gdk-window-get_root_coords}
    @about-function{gdk-window-get_pointer}
    @about-function{gdk-window-get_device_position}
    @about-symbol{GdkModifierType}
    @about-function{gdk-window-get_parent}
    @about-function{gdk-window-get_toplevel}
    @about-function{gdk-window-get_children}
    @about-function{gdk-window-peek_children}
    @about-function{gdk-window-get_events}
    @about-function{gdk-window-set_events}
    @about-function{gdk-window-set_icon_name}
    @about-function{gdk-window-set_transient_for}
    @about-function{gdk-window-set_role}
    @about-function{gdk-window-set_startup_id}
    @about-function{gdk-window-set_group}
    @about-function{gdk-window-get_group}
    @about-function{gdk-window-set_decorations}
    @about-function{gdk-window-get_decorations}
    @about-symbol{GdkWMDecoration}
    @about-function{gdk-window-set_functions}
    @about-symbol{GdkWMFunction}
    @about-function{gdk_get_default_root_window}
    @about-function{gdk-window-get_support_multidevice}
    @about-function{gdk-window-set_support_multidevice}
    @about-function{gdk-window-get_device_cursor}
    @about-function{gdk-window-set_device_cursor}
    @about-function{gdk-window-get_device_events}
    @about-function{gdk-window-set_device_events}
    @about-function{gdk-window-get_source_events}
    @about-function{gdk-window-set_source_events}
    @about-function{gdk_offscreen_window_get_surface}
    @about-function{gdk_offscreen_window_set_embedder}
    @about-function{gdk_offscreen_window_get_embedder}
    @about-function{gdk-window-geometry_changed}
    @about-function{gdk-window-coords_from_parent}
    @about-function{gdk-window-coords_to_parent}
    @about-function{gdk-window-get_effective_parent}
    @about-function{gdk-window-get_effective_toplevel}
  @end{section}
  @begin[Events]{section}
    Functions for handling events from the window system
  @end{section}
  @begin[Event Structures]{section}
    The event structs contain data specific to each type of event in GDK.

    @b{Note}

    A common mistake is to forget to set the event mask of a widget so that the
    required events are received. See @fun{gtk-widget-set-events}.

    @about-symbol{gdk-scroll-direction}
    @about-symbol{gdk-visibility-state}
    @about-symbol{gdk-crossing-mode}
    @about-symbol{gdk-notify-type}
    @about-symbol{gdk-property-state}
    @about-symbol{gdk-window-state}
    @about-symbol{gdk-setting-action}
    @about-symbol{gdk-owner-change}
    @about-symbol{gdk-event-type}
    @about-symbol{gdk-modifier-type}
    @about-symbol{gdk-event-mask}
    @about-symbol{gdk-event}
    @about-symbol{gdk-event-any}
    @about-symbol{gdk-event-expose}
    @about-symbol{gdk-event-visibility}
    @about-symbol{gdk-event-motion}
    @about-symbol{gdk-event-touch}
    @about-symbol{gdk-event-scroll}
    @about-symbol{gdk-event-key}
    @about-symbol{gdk-event-crossing}
    @about-symbol{gdk-event-focus}
    @about-symbol{gdk-event-configure}
    @about-symbol{gdk-event-property}
    @about-symbol{gdk-event-selection}
    @about-symbol{gdk-event-owner-change}
    @about-symbol{gdk-event-proximity}
    @about-symbol{gdk-event-dnd}
    @about-symbol{gdk-event-window-state}
    @about-symbol{gdk-event-setting}
    @about-symbol{gdk-event-grab-broken}
  @end{section}
  @begin[Key Values]{section}
    Functions for manipulating keyboard codes
  @end{section}
  @begin[Selections]{section}
    Functions for transfering data via the X selection mechanism
  @end{section}
  @begin[Drag And Drop]{section}
    Functions for controlling drag and drop handling
  @end{section}
  @begin[Properties and Atoms]{section}
    Functions to manipulate properties on windows
  @end{section}
  @begin[Threads]{section}
    Functions for using GDK in multi-threaded programs
  @end{section}
  @begin[Pango Interaction]{section}
    Using Pango in GDK
  @end{section}
  @begin[Cairo Interaction]{section}
    Functions to support using cairo
  @end{section}
  @begin[X Window System Interaction]{section}
    X backend-specific functions
  @end{section}
  @begin[Application launching]{section}
    Startup notification for applications
  @end{section}
 ")

;;; --- End of file atdoc-gdk.package.lisp -------------------------------------
