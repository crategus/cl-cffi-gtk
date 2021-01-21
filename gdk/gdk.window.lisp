;;; ----------------------------------------------------------------------------
;;; gdk.window.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Windows
;;;
;;;    Onscreen display areas in the target window system
;;;
;;; Types and Values
;;;
;;;     GdkWindow
;;;     GdkWindowType
;;;     GdkWindowWindowClass
;;;     GdkWindowHints
;;;     GdkGravity
;;;     GdkGeometry
;;;     GdkAnchorHints
;;;     GdkWindowEdge
;;;     GdkWindowTypeHint
;;;     GdkWindowAttr
;;;     GdkWindowAttributesType
;;;     GdkFullscreenMode
;;;     GdkFilterReturn
;;;     GdkModifierType        --> gdk.event-structures.lisp
;;;     GdkModifierIntent
;;;     GdkWMDecoration
;;;     GdkWMFunction
;;;
;;; Functions
;;;
;;;     gdk_window_new
;;;     gdk_window_destroy
;;;     gdk_window_get_window_type
;;;     gdk_window_get_display
;;;     gdk_window_get_screen
;;;     gdk_window_get_visual
;;;     gdk_window_at_pointer                              deprecated
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide
;;;     gdk_window_is_destroyed
;;;     gdk_window_is_visible
;;;     gdk_window_is_viewable
;;;     gdk_window_is_input_only
;;;     gdk_window_is_shaped
;;;     gdk_window_get_state
;;;     gdk_window_withdraw
;;;     gdk_window_iconify
;;;     gdk_window_deiconify
;;;     gdk_window_stick
;;;     gdk_window_unstick
;;;     gdk_window_maximize
;;;     gdk_window_unmaximize
;;;     gdk_window_fullscreen
;;;     gdk_window_fullscreen_on_monitor
;;;     gdk_window_unfullscreen
;;;     gdk_window_get_fullscreen_mode
;;;     gdk_window_set_fullscreen_mode
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited                          deprecated
;;;     gdk_window_get_composited                          deprecated
;;;     gdk_window_set_pass_through
;;;     gdk_window_get_pass_through
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_to_rect
;;;     gdk_window_move_region
;;;     gdk_window_flush                                   deprecated
;;;     gdk_window_has_native
;;;     gdk_window_ensure_native
;;;     gdk_window_reparent
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_resize_drag_for_device
;;;     gdk_window_begin_move_drag
;;;     gdk_window_begin_move_drag_for_device
;;;     gdk_window_show_window_menu
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     gdk_window_get_scale_factor
;;;     gdk_window_set_opaque_region
;;;     gdk_window_create_gl_context
;;;     gdk_window_mark_paint_from_clip
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect                        deprecated
;;;     gdk_window_begin_paint_region                      deprecated
;;;     gdk_window_end_paint                               deprecated
;;;     gdk_window_begin_draw_frame
;;;     gdk_window_end_draw_frame
;;;     gdk_window_get_visible_region
;;;     GdkWindowInvalidateHandlerFunc
;;;     gdk_window_set_invalidate_handler
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     GdkWindowChildFunc
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates                     deprecated
;;;     gdk_window_process_updates                         deprecated
;;;     gdk_window_set_debug_updates                       deprecated
;;;     gdk_window_enable_synchronized_configure           deprecated
;;;     gdk_window_configure_finished                      deprecated
;;;     gdk_window_get_frame_clock
;;;     gdk_window_set_user_data
;;;     gdk_window_set_override_redirect
;;;     gdk_window_set_accept_focus
;;;     gdk_window_get_accept_focus
;;;     gdk_window_set_focus_on_map
;;;     gdk_window_get_focus_on_map
;;;     gdk_window_add_filter
;;;     gdk_window_remove_filter
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities                    deprecated
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          deprecated
;;;     gdk_window_set_background_rgba                     deprecated
;;;     gdk_window_set_background_pattern                  deprecated
;;;     gdk_window_get_background_pattern                  deprecated
;;;     gdk_window_set_cursor                              Accessor
;;;     gdk_window_get_cursor                              Accessor
;;;     gdk_window_get_user_data
;;;     gdk_window_get_geometry
;;;     gdk_window_set_geometry_hints
;;;     gdk_window_get_width
;;;     gdk_window_get_height
;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint
;;;     gdk_window_get_position
;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             deprecated
;;;     gdk_window_get_device_position
;;;     gdk_window_get_device_position_double ()
;;;     gdk_window_get_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_children
;;;     gdk_window_get_children_with_user_data ()
;;;     gdk_window_peek_children
;;;     gdk_window_get_events
;;;     gdk_window_set_events
;;;     gdk_window_set_icon_name
;;;     gdk_window_set_transient_for
;;;     gdk_window_set_role
;;;     gdk_window_set_startup_id
;;;     gdk_window_set_group
;;;     gdk_window_get_group
;;;     gdk_window_set_decorations
;;;     gdk_window_get_decorations
;;;     gdk_window_set_functions
;;;     gdk_get_default_root_window
;;;     gdk_window_get_support_multidevice
;;;     gdk_window_set_support_multidevice
;;;     gdk_window_get_device_cursor
;;;     gdk_window_set_device_cursor
;;;     gdk_window_get_device_events
;;;     gdk_window_set_device_events
;;;     gdk_window_get_source_events
;;;     gdk_window_set_source_events
;;;     gdk_window_get_event_compression ()
;;;     gdk_window_set_event_compression ()
;;;     gdk_offscreen_window_get_surface
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed
;;;     gdk_window_coords_from_parent
;;;     gdk_window_coords_to_parent
;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel
;;;
;;; Properties
;;;
;;;        GdkCursor*   cursor                 Read / Write
;;;
;;; Signals
;;;
;;;     CairoSurface*   create-surface         Run Last
;;;             void    from-embedder          Run Last
;;;             void    moved-to-rect          Run First
;;;        GdkWindow*   pick-embedded-child    Run Last
;;;             void    to-embedder            Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkWindow
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowType" gdk-window-type
  (:export t
   :type-initializer "gdk_window_type_get_type")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:temp 3)
  (:foreign 4)
  (:offscreen 5)
  (:subsurface 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-type atdoc:*external-symbols*)
 "@version{2020-9-6}
  @short{Describes the kind of the window.}
  @begin{pre}
(define-g-enum \"GdkWindowType\" gdk-window-type
  (:export t
   :type-initializer \"gdk_window_type_get_type\")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:temp 3)
  (:foreign 4)
  (:offscreen 5)
  (:subsurface 6))
  @end{pre}
  @begin[code]{table}
    @entry[:root]{Root window; this window has no parent, covers the entire
      screen, and is created by the window system.}
    @entry[:toplevel]{Toplevel window, used to implement @class{gtk-window}.}
    @entry[:child]{Child window, used to implement e.g. @class{gtk-entry}.}
    @entry[:temp]{Override redirect temporary window, used to implement
      @class{gtk-menu}.}
    @entry[:foreign]{Foreign window.}
    @entry[:offscreen]{Offscreen window, see the section called
      \"Offscreen Windows\".}
    @entry[:subsurface]{Subsurface-based window. This window is visually tied
      to a toplevel, and is moved/stacked with it. Currently this window type
      is only implemented in Wayland.}
  @end{table}
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-class{gtk-entry}
  @see-class{gtk-menu}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowWindowClass
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowWindowClass" gdk-window-window-class
  (:export t
   :type-initializer "gdk_window_window_class_get_type")
  (:input-output 0)
  (:input-only 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-window-class atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-window-class atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    @code{:input-output} windows are the standard kind of window you might
    expect.
  @end{short}
  Such windows receive events and are also displayed on screen.
  @code{:input-only} windows are invisible. They are usually placed above other
  windows in order to trap or filter the events. You cannot draw on
  @code{:input-only} windows.
  @begin{pre}
(define-g-enum \"GdkWindowWindowClass\" gdk-window-window-class
  (:export t
   :type-initializer \"gdk_window_window_class_get_type\")
  (:input-output 0)
  (:input-only 1))
  @end{pre}
  @begin[code]{table}
    @entry[:input-output]{Window for graphics and events.}
    @entry[:input-only]{Window for events only.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowHints
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-hints atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-window-hints atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Used to indicate which fields of a @symbol{gdk-geometry} structure should
    be paid attention to.
  @end{short}
  Also, the presence/absence of @code{:pos}, @code{:user-pos}, and
  @code{:user-size} is significant, though they do not directly refer to
  @symbol{gdk-geometry} fields. @code{:user-pos} will be set automatically by
  @class{gtk-window} if you call the function @fun{gtk-window-move}.
  @code{:user-pos} and @code{:user-size} should be set if the user specified a
  size/position using a - geometry command-line argument; the function
  @fun{gtk-window-parse-geometry} automatically sets these flags.
  @begin{pre}
(define-g-flags \"GdkWindowHints\" gdk-window-hints
  (:export t
   :type-initializer \"gdk_window_hints_get_type\")
  (:pos 1)
  (:min-size 2)
  (:max-size 4)
  (:base-size 8)
  (:aspect 16)
  (:resize-inc 32)
  (:win-gravity 64)
  (:user-pos 128)
  (:user-size 256))
  @end{pre}
  @begin[code]{table}
    @entry[:pos]{Indicates that the program has positioned the window.}
    @entry[:min-size]{Min size fields are set.}
    @entry[:max-size]{Max size fields are set.}
    @entry[:base-size]{Base size fields are set.}
    @entry[:aspect]{Aspect ratio fields are set.}
    @entry[:resize-inc]{Resize increment fields are set.}
    @entry[:win-gravity]{Window gravity field is set.}
    @entry[:user-pos]{Indicates that the window's position was explicitly set
      by the user.}
    @entry[:user-size]{Indicates that the window's size was explicitly set by
      the user.}
  @end{table}
  @see-symbol{gdk-geometry}
  @see-class{gtk-window}
  @see-function{gtk-window-parse-geometry}")

;;; ----------------------------------------------------------------------------
;;; enum GdkGravity
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGravity" gdk-gravity
  (:export t
   :type-initializer "gdk_gravity_get_type")
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-gravity atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-gravity atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Defines the reference point of a window and the meaning of coordinates
    passed to the function @fun{gtk-window-move}.
  @end{short}
  See the function @fun{gtk-window-move} and the \"implementation notes\"
  section of the Extended Window Manager Hints specification for more details.
  @begin{pre}
(define-g-enum \"GdkGravity\" gdk-gravity
  (:export t
   :type-initializer \"gdk_gravity_get_type\")
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
  @end{pre}
  @begin[code]{table}
    @entry[:north-west]{The reference point is at the top left corner.}
    @entry[:north]{The reference point is in the middle of the top edge.}
    @entry[:north-east]{The reference point is at the top right corner.}
    @entry[:west]{The reference point is at the middle of the left edge.}
    @entry[:center]{The reference point is at the center of the window.}
    @entry[:east]{The reference point is at the middle of the right edge.}
    @entry[:south-west]{The reference point is at the lower left corner.}
    @entry[:south]{The reference point is at the middle of the lower edge.}
    @entry[:south-east]{The reference point is at the lower right corner.}
    @entry[:static]{The reference point is at the top left corner of the
      window itself, ignoring window manager decorations.}
  @end{table}
  @see-class{gdk-window}
  @see-function{gtk-window-move}")

;;; ----------------------------------------------------------------------------
;;; struct GdkGeometry
;;; ----------------------------------------------------------------------------

(defcstruct gdk-geometry
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (base-width :int)
  (base-height :int)
  (width-increment :int)
  (height-increment :int)
  (min-aspect :double)
  (max-aspect :double)
  (win-gravity gdk-gravity))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gdk-geometry atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    The @sym{gdk-geometry} structure gives the window manager information about
    a window's geometry constraints.
  @end{short}
  Normally you would set these on the GTK+ level using the function
  @fun{gtk-window-set-geometry-hints}. @class{gtk-window} then sets the hints
  on the @class{gdk-window} it creates.

  The function @fun{gdk-window-set-geometry-hints} expects the hints to be fully
  valid already and simply passes them to the window manager. In contrast, the
  function @fun{gtk-window-set-geometry-hints} performs some interpretation. For
  example, @class{gtk-window} will apply the hints to the geometry widget
  instead of the toplevel window, if you set a geometry widget. Also, the
  @code{min-width}/@code{min-height}/@code{max-width}/@code{max-height} fields
  may be set to -1, and @class{gtk-window} will substitute the size request of
  the window or geometry widget. If the minimum size hint is not provided,
  @class{gtk-window} will use its requisition as the minimum size. If the
  minimum size is provided and a geometry widget is set, @class{gtk-window} will
  take the minimum size as the minimum size of the geometry widget rather than
  the entire window. The base size is treated similarly.

  The canonical use-case for the function @fun{gtk-window-set-geometry-hints} is
  to get a terminal widget to resize properly. Here, the terminal text area
  should be the geometry widget. @class{gtk-window} will then automatically set
  the base size to the size of other widgets in the terminal window, such as the
  menubar and scrollbar. Then, the @code{width-increment} and
  @code{height-incement} fields should be set to the size of one character in
  the terminal. Finally, the base size should be set to the size of one
  character. The net effect is that the minimum size of the terminal will have
  a 1 x 1 character terminal area, and only terminal sizes on the
  \"character grid\" will be allowed.

  Here is an example of how the terminal example would be implemented, assuming
  a terminal area widget called \"terminal\" and a toplevel window \"toplevel\":
  @begin{pre}
GdkGeometry hints;

hints.base_width = terminal->char_width;
hints.base_height = terminal->char_height;
hints.min_width = terminal->char_width;
hints.min_height = terminal->char_height;
hints.width_inc = terminal->char_width;
hints.height_inc = terminal->char_height;

gtk_window_set_geometry_hints (GTK_WINDOW (toplevel),
                               GTK_WIDGET (terminal),
                               &hints,
                               GDK_HINT_RESIZE_INC |
                               GDK_HINT_MIN_SIZE |
                               GDK_HINT_BASE_SIZE);
  @end{pre}
  The other useful fields are the @code{min-aspect} and @code{max-aspect}
  fields. These contain a width/height ratio as a floating point number. If a
  geometry widget is set, the aspect applies to the geometry widget rather than
  the entire window. The most common use of these hints is probably to set
  @code{min-aspect} and @code{max-aspect} to the same value, thus forcing the
  window to keep a constant aspect ratio.
  @begin{pre}
(defcstruct gdk-geometry
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (base-width :int)
  (base-height :int)
  (width-increment :int)
  (height-increment :int)
  (min-aspect :double)
  (max-aspect :double)
  (win-gravity gdk-gravity))
  @end{pre}
  @begin[code]{table}
    @entry[min-width]{Minimum width of window or -1 to use requisition,
      with @class{gtk-window} only.}
    @entry[min-height]{Minimum height of window or -1 to use requisition,
      with @class{gtk-window} only.}
    @entry[max-width]{Maximum width of window or -1 to use requisition,
      with @class{gtk-window} only.}
    @entry[max-height]{Maximum height of window or -1 to use requisition,
      with @class{gtk-window} only.}
    @entry[base-width]{Allowed window widths are @code{base-width} +
      @code{width-inc} * @code{N} where @code{N} is any integer, -1 is allowed
      with @class{gtk-window}.}
    @entry[base-height]{Allowed window widths are @code{base-height} +
      @code{height-inc} * @code{N} where @code{N} is any integer, -1 is allowed
      with @class{gtk-window}.}
    @entry[width-increment]{Width resize increment.}
    @entry[height-increment]{Height resize increment.}
    @entry[min-aspect]{Minimum width/height ratio.}
    @entry[max-aspect]{Maximum width/height ratio.}
    @entry[win-gravity]{Window gravity of type @symbol{gdk-gravity}, see the
      @fun{gtk-window-gravity} function.}
  @end{table}
  @see-constructor{make-gdk-geometry}
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gdk-window-set-geometry-hints}
  @see-function{gtk-window-set-geometry-hints}
  @see-function{gtk-window-gravity}")

(export 'gdk-geometry)

;;; ----------------------------------------------------------------------------
;;; Constructors for GdkGeometry
;;; ----------------------------------------------------------------------------

(defun make-gdk-geometry (&key (min-width 0)
                               (min-height 0)
                               (max-width 0)
                               (max-height 0)
                               (base-width 0)
                               (base-height 0)
                               (width-increment 0)
                               (height-increment 0)
                               (min-aspect 0.0d0)
                               (max-aspect 0.0d0)
                               (win-gravity :north-west))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[min-width]{a @code{:int} with the minimum width of window or -1
    to use requisition, with @class{gtk-window} only}
  @argument[min-height]{a @code{:int} with minimum height of window or -1 to
    use requisition, with @class{gtk-window} only}
  @argument[max-width]{a @code{:int} with maximum width of window or -1 to
    use requisition, with @class{gtk-window} only}
  @argument[max-height]{a @code{:int} with maximum height of window or -1 to
    use requisition, with @class{gtk-window} only}
  @argument[base-width]{a @code{:int} with allowed window widths are
    @code{base-width + width-inc * N} where @code{N} is any integer, -1 is
    allowed with @class{gtk-window}}
  @argument[base-height]{a @code{:int} with allowed window widths are
    @code{base-height + height-inc * N} where @code{N} is any integer, -1
    allowed with @class{gtk-window}}
  @argument[width-increment]{a @code{:int} with width resize increment}
  @argument[height-increment]{a @code{:int} with height resize increment}
  @argument[min-aspect]{a @code{:double} with minimum width/height ratio}
  @argument[max-aspect]{a @code{:double} with maximum width/height ratio}
  @argument[win-gravity]{window gravity of type @symbol{gdk-gravity}, see the
    function @fun{gtk-window-gravity}}
  @begin{short}
    Creates a @symbol{gdk-geometry} structure.
  @end{short}
  @see-symbol{gdk-geometry}
  @see-function{gtk-window-gravity}"
  (with-foreign-object (ptr '(:struct gdk-geometry))
    ;; Initialize the slots
    (setf (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-width)
          min-width
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-height)
          min-height
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-width)
          max-width
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-height)
          max-height
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::base-width)
          base-width
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::base-height)
          base-height
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::width-increment)
          width-increment
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::height-increment)
          height-increment
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::min-aspect)
          min-aspect
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::max-aspect)
          max-aspect
          (foreign-slot-value ptr '(:struct gdk-geometry) 'gdk::win-gravity)
          win-gravity)
    ;; Return the pointer to the GdkGeometry structure
    ptr))

(export 'make-gdk-geometry)

;;; ----------------------------------------------------------------------------
;;; enum GdkAnchorHints
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(define-g-flags "GdkAnchorHints" gdk-anchor-hints
  (:export t
   :type-initializer "gdk_anchor_hints_get_type")
  (:flip-x   #.(ash 1 0))
  (:flip-y   #.(ash 1 1))
  (:slide-x  #.(ash 1 2))
  (:slide-y  #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip    3)  ; :flip-x   | :flip-y
  (:slide  12)  ; :slide-x  | :slide-y
  (:resize 48)) ; :resize-x | :resize-y

#+(and gdk-3-22 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-anchor-hints atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-anchor-hints atdoc:*external-symbols*)
 "@version{2020-5-19}
  @begin{short}
    Positioning hints for aligning a window relative to a rectangle.
  @end{short}

  These hints determine how the window should be positioned in the case that
  the window would fall off-screen if placed in its ideal position. For example,
  @code{:flip-x} will replace @code{:north-west} with @code{:north-east} and
  vice versa if the window extends beyond the left or right edges of the
  monitor.

  If @code{:slide-x} is set, the window can be shifted horizontally to fit
  on-screen. If @code{:resize-x} is set, the window can be shrunken
  horizontally to fit.

  In general, when multiple flags are set, flipping should take precedence
  over sliding, which should take precedence over resizing.

  Since 3.22
  @begin{pre}
(define-g-flags \"GdkAnchorHints\" gdk-anchor-hints
  (:export t
   :type-initializer \"gdk_anchor_hints_get_type\")
  (:flip-x   #.(ash 1 0))
  (:flip-y   #.(ash 1 1))
  (:slide-x  #.(ash 1 2))
  (:slide-y  #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip    3)  ; :flip-x   | :flip-y
  (:slide  12)  ; :slide-x  | :slide-y
  (:resize 48)) ; :resize-x | :resize-y
  @end{pre}
  @begin[code]{table}
    @entry[:flip-x]{Allow flipping anchors horizontally.}
    @entry[:fliy-y]{Allow flipping anchors vertically.}
    @entry[:slide-x]{Allow sliding window horizontally.}
    @entry[:slide-y]{Allow sliding window vertically.}
    @entry[:resize-x]{Allow resizing window horizontally.}
    @entry[:resize-y]{Allow resizing window vertically.}
    @entry[:flip]{Allow flipping anchors on both axes.}
    @entry[:slide]{Allow sliding window on both axes.}
    @endtry[:resize]{Allow resizing window on both axes.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowEdge
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowEdge" gdk-window-edge
  (:export t
   :type-initializer "gdk_window_edge_get_type")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-edge atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-edge atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Determines a window edge or corner.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkWindowEdge\" gdk-window-edge
  (:export t
   :type-initializer \"gdk_window_edge_get_type\")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))
  @end{pre}
  @begin[code]{table}
    @entry[:north-west]{The top left corner.}
    @entry[:north]{The top edge.}
    @entry[:north-east]{The top right corner.}
    @entry[:west]{The left edge.}
    @entry[:east]{The right edge.}
    @entry[:south-west]{The lower left corner.}
    @entry[:south]{The lower edge.}
    @entry[:south-east]{The lower right corner.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowTypeHint
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-type-hint atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-type-hint atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    These are hints for the window manager that indicate what type of function
    the window has.
  @end{short}
  The window manager can use this when determining decoration and behaviour of
  the window. The hint must be set before mapping the window.

  See the Extended Window Manager Hints specification for more details about
  window types.
  @begin{pre}
(define-g-enum \"GdkWindowTypeHint\" gdk-window-type-hint
  (:export t
   :type-initializer \"gdk_window_type_hint_get_type\")
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
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Normal toplevel window.}
    @entry[:dialog]{Dialog window.}
    @entry[:menu]{Window used to implement a menu. GTK+ uses this hint only for
      torn-off menus, see @class{gtk-tearoff-menu-item}.}
    @entry[:toolbar]{Window used to implement toolbars.}
    @entry[:splashscreen]{Window used to display a splash screen during
      application startup.}
    @entry[:utility]{Utility windows which are not detached toolbars or
      dialogs.}
    @entry[:dock]{Used for creating dock or panel windows.}
    @entry[:desktop]{Used for creating the desktop background window.}
    @entry[:dropdown-menu]{A menu that belongs to a menubar.}
    @entry[:popup-menu]{A menu that does not belong to a menubar,
      e.g. a context menu.}
    @entry[:tooltip]{A tooltip.}
    @entry[:notification]{A notification - typically a \"bubble\" that belongs
      to a status icon.}
    @entry[:combo]{A popup from a combo box.}
    @entry[:dnd]{A window that is used to implement a DND cursor.}
  @end{table}
  @see-class{gdk-window}
  @see-class{gtk-tearoff-menu-item}")

;;; ----------------------------------------------------------------------------
;;; struct GdkWindowAttr
;;; ----------------------------------------------------------------------------

(defcstruct gdk-window-attr
  (title (:string :free-from-foreign nil))
  (event-mask gdk-event-mask)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (wclass gdk-window-window-class)
  (visual (g-object gdk-visual))
  (window-type gdk-window-type)
  (cursor (g-object gdk-cursor))
  (wmclass-name (:string :free-from-foreign nil))
  (wmclass-class (:string :free-from-foreign nil))
  (override-redirect :boolean)
  (type-hint gdk-window-type-hint))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'gdk-window-attr atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Attributes to use for a newly created window.
  @end{short}
  @begin{pre}
(defcstruct gdk-window-attr
  (title (:string :free-from-foreign nil))
  (event-mask gdk-event-mask)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (wclass gdk-window-window-class)
  (visual (g-object gdk-visual))
  (window-type gdk-window-type)
  (cursor (g-object gdk-cursor))
  (wmclass-name (:string :free-from-foreign nil))
  (wmclass-class (:string :free-from-foreign nil))
  (override-redirect :boolean)
  (type-hint gdk-window-type-hint))
  @end{pre}
  @begin[code]{table}
    @entry[title]{A @code{:string} with the title of the window for toplevel
      windows.}
    @entry[event-mask]{@symbol{gdk-event-mask} flags, see the function
      @fun{gdk-window-events}.}
    @entry[x]{A @code{:int} with the x coordinate relative to parent window,
      see the function @fun{gdk-window-move}.}
    @entry[y]{A @code{:int} with the y coordinate relative to parent window,
      see the function @fun{gdk-window-move}.}
    @entry[width]{A @code{:int} with the width of the window.}
    @entry[height]{A @code{:int} with the height of window.}
    @entry[wclass]{A value of the @symbol{gdk-window-window-class} enumeration,
      @code{:input-output} for a normal window or @code{:input-only} for an
      invisible window that receives events.}
    @entry[visual]{A @class{gdk-visual} object for the window.}
    @entry[window-type]{A value of the @symbol{gdk-window-type} enumeration.}
    @entry[cursor]{A @class{gdk-cusor} object for the window, see the function
      @fun{gdk-window-cursor}.}
    @entry[wmclass-name]{A @code{:string}, do not use.}
    @entry[wmclass-class]{A @code{:string}, do not use.}
    @entry[override-redirect]{@em{True} to bypass the window manager.}
    @entry[type-hint]{A value of the @symbol{gdk-window-type-hint} enumeration
      of the function of the window.}
  @end{table}
  @see-constructor{make-gdk-window-attr}
  @see-class{gdk-visual}
  @see-function{gdk-window-move}
  @see-function{gdk-window-cursor}
  @see-function{gdk-window-events}")

(export 'gdk-window-attr)

;;; ----------------------------------------------------------------------------
;;; Constructors for the GdkWindowAttr structure
;;; ----------------------------------------------------------------------------

(defun make-gdk-window-attr (&key (title "")
                                  (event-mask nil)
                                  (x 0) (y 0)
                                  (width 0) (height 0)
                                  (wclass :input-output)
                                  (visual nil)
                                  (window-type :toplevel)
                                  (cursor nil)
                                  (wmclass-name "")
                                  (wmclass-class "")
                                  (override-redirect nil)
                                  (type-hint :normal))
 "@version{2020-9-6}
  @argument[title]{a @code{:string} with the title of the window, for toplevel
    windows}
  @argument[event-mask]{@symbol{gdk-event-mask} flags, see the function
    @fun{gdk-window-events}}
  @argument[x]{a @code{:int} with the x coordinate relative to the parent
    window, see the function @fun{gdk-window-move}}
  @argument[y]{a @code{:int} with the y coordinate relative to the parent
    window, see the function @fun{gdk-window-move}}
  @argument[width]{a @code{:int} with the width of the window}
  @argument[height]{a @code{:int} with the height of the window}
  @argument[window-class]{a value of the @symbol{gdk-window-window-class},
    @code{:input-output} for a normal window or @code{:input-only} for an
    invisible window that receives events}
  @argument[visual]{a @class{gdk-visual} for the window}
  @argument[window-type]{a value of the @symbol{gdk-window-type} enumeration}
  @argument[cursor]{a @class{gdk-cursor} object for the window, see the function
    @fun{gdk-window-cursor}}
  @argument[wmclass-name]{a @code{:string}, do not use}
  @argument[wmclass-class]{a @code{:string}, do not use}
  @argument[override-redirect]{@em{true} to bypass the window manager}
  @argument[type-hint]{a value of the @symbol{gdk-window-type-hint} enumeration
    of the function of the window}
  @begin{short}
    Creates a @symbol{gdk-window-attr} structure with default values
    for the arguments.
  @end{short}
  @see-symbol{gdk-window-attr}
  @see-function{gdk-window-move}
  @see-function{gdk-window-cursor}
  @see-function{gdk-window-events}"
  (with-foreign-object (ptr '(:struct gdk-window-attr))
    ;; Initialize the slots
    (setf (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::title)
          title
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::event-mask)
          event-mask
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::x)
          x
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::y)
          y
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::width)
          width
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::height)
          height
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::wclass)
          wclass
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::visual)
          visual
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::window-type)
          window-type
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::cursor)
          cursor
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::wmclass-name)
          wmclass-name
          (foreign-slot-value ptr '(:struct gdk-window-attr)
                                  'gdk::wmclass-class)
          wmclass-class
          (foreign-slot-value ptr '(:struct gdk-window-attr)
                                  'gdk::override-redirect)
          override-redirect
          (foreign-slot-value ptr '(:struct gdk-window-attr) 'gdk::type-hint)
          type-hint)
    ;; Return the pointer to the GdkWindowAttr structure
    ptr))

(export 'make-gdk-window-attr)

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowAttributesType
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowAttributesType" gdk-window-attributes-type
  (:export t
   :type-initializer "gdk_window_attributes_type_get_type")
  (:title 2)
  (:x 4)
  (:y 8)
  (:cursor 16)
  (:visual 32)
  (:wmclass 64)
  (:noredir 128)
  (:type-hint 256))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attributes-type atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-window-attributes-type atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Used to indicate which fields in the @symbol{gdk-window-attr} structure
    should be honored.
  @end{short}
  For example, if you filled in the @code{cursor} and @code{x} fields of
  @symbol{gdk-window-attr}, pass @code{'(:x :cursor)} to the function
  @fun{gdk-window-new}. Fields in @symbol{gdk-window-attr} not covered by a bit
  in this enum are required. For example, the @code{width}/@code{height},
  @code{wclass}, and @code{window-type} fields are required, they have no
  corresponding flag in @symbol{gdk-window-attributes-type}.
  @begin{pre}
(define-g-flags \"GdkWindowAttributesType\" gdk-window-attributes-type
  (:export t
   :type-initializer \"gdk_window_attributes_type_get_type\")
  (:title 2)
  (:x 4)
  (:y 8)
  (:cursor 16)
  (:visual 32)
  (:wmclass 64)
  (:noredir 128)
  (:type-hint 256))
  @end{pre}
  @begin[code]{table}
    @entry[:title]{Honor the @code{title} field.}
    @entry[:x]{Honor the @code{x} coordinate field.}
    @entry[:y]{Honor the @code{y} coordinate field.}
    @entry[:cursor]{Honor the @code{cursor} field.}
    @entry[:visual]{Honor the @code{visual} field.}
    @entry[:wmclass]{Honor the @code{wmclass-class} and @code{wmclass-name}
      fields.}
    @entry[:noredir]{Honor the @code{override-redirect} field.}
    @entry[:type-hint]{Honor the @code{type-hint} field.}
  @end{table}
  @see-symbol{gdk-window-attr}
  @see-function{gdk-window-new}")

;;; ----------------------------------------------------------------------------
;;; enum GdkFullscreenMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFullscreenMode" gdk-fullscreen-mode
  (:export t
   :type-initializer "gdk_fullscreen_mode_get_type")
  (:current-monitor 0)
  (:all-monitors 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-fullscreen-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-fullscreen-mode atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Indicates which monitor (in a multi-head setup) a window should span over
    when in fullscreen mode.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkFullscreenMode\" gdk-fullscreen-mode
  (:export t
   :type-initializer \"gdk_fullscreen_mode_get_type\")
  (:on-current-monitor 0)
  (:on-all-monitors 1))
  @end{pre}
  @begin[code]{table}
    @entry[:current-monitor]{Fullscreen on current monitor only.}
    @entry[:all-monitors]{Span across all monitors when fullscreen.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkFilterReturn
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFilterReturn" gdk-filter-return
  (:export t
   :type-initializer "gdk_filter_return_get_type")
  (:continue 0)
  (:translate 1)
  (:remove 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-filter-return atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-filter-return atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    Specifies the result of applying a @code{GdkFilterFunc} to a native event.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkFilterReturn\" gdk-filter-return
  (:export t
   :type-initializer \"gdk_filter_return_get_type\")
  (:continue 0)
  (:translate 1)
  (:remove 2))
  @end{pre}
  @begin[code]{table}
    @entry[:continue]{Event not handled, continue processing.}
    @entry[:translate]{Native event translated into a GDK event and stored in
      the event structure that was passed in.}
    @entry[:remove]{Event handled, terminate processing.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierIntent
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkModifierIntent" gdk-modifier-intent
  (:export t
   :type-initializer "gdk_modifier_intent_get_type")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-modifier-intent atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-modifier-intent atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    This enum is used with the function @fun{gdk-keymap-get-modifier-mask} in
    order to determine what modifiers the currently used windowing system
    backend uses for particular purposes.
  @end{short}
  For example, on X11/Windows, the Control key is used for invoking menu
  shortcuts (accelerators), whereas on Apple computers it’s the Command key
  (which correspond to @code{GDK_CONTROL_MASK} and @code{GDK_MOD2_MASK},
  respectively).
  @begin{pre}
(define-g-enum \"GdkModifierIntent\" gdk-modifier-intent
  (:export t
   :type-initializer \"gdk_modifier_intent_get_type\")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))
  @end{pre}
  @begin[code]{table}
    @entry[:primary-accelerator]{The primary modifier used to invoke menu
      accelerators.}
    @entry[:context-menu]{The modifier used to invoke context menus. Note that
      mouse button 3 always triggers context menus. When this modifier is not 0,
      it additionally triggers context menus when used with mouse button 1.}
    @entry[:extend-selextion]{The modifier used to extend selections using
      modifier-click or modifier-cursor-key.}
    @entry[:modify-selection]{The modifier used to modify selections, which in
      most cases means toggling the clicked item into or out of the selection.}
    @entry[:no-text-input]{When any of these modifiers is pressed, the key event
      cannot produce a symbol directly. This is meant to be used for input
      methods, and for use cases like typeahead search.}
    @entry[:shift-group]{The modifier that switches between keyboard groups
    (AltGr on X11/Windows and Option/Alt on OS X).}
    @entry[:default-mod-mask]{The set of modifier masks accepted as modifiers in
      accelerators. Needed because Command is mapped to MOD2 on OSX, which is
      widely used, but on X11 MOD2 is NumLock and using that for a mod key is
      problematic at best. See reference
      @url[https://bugzilla.gnome.org/show_bug.cgi?id=736125]{Bug 736125}.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWMDecoration
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-decoration atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-decoration atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    These are hints originally defined by the Motif toolkit.
  @end{short}
  The window manager can use them when determining how to decorate the window.
  The hint must be set before mapping the window.
  @begin{pre}
(define-g-flags \"GdkWMDecoration\" gdk-wm-decoration
  (:export t
   :type-initializer \"gdk_wm_decoration_get_type\")
  (:all 1)
  (:border 2)
  (:resizeh 4)
  (:title 8)
  (:menu 16)
  (:minimize 32)
  (:maximize 64))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All decorations should be applied.}
    @entry[:border]{A frame should be drawn around the window.}
    @entry[:resizeh]{The frame should have resize handles.}
    @entry[:title]{A titlebar should be placed above the window.}
    @entry[:menu]{A button for opening a menu should be included.}
    @entry[:minimize]{A minimize button should be included.}
    @entry[:maximize]{A maximize button should be included.}
  @end{table}
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWMFunction
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-function atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-function atdoc:*external-symbols*)
 "@version{2020-9-6}
  @begin{short}
    These are hints originally defined by the Motif toolkit.
  @end{short}
  The window manager can use them when determining the functions to offer for
  the window. The hint must be set before mapping the window.
  @begin{pre}
(define-g-flags \"GdkWMFunction\" gdk-wm-function
  (:export t
   :type-initializer \"gdk_wm_function_get_type\")
  (:all 1)
  (:resize 2)
  (:move 4)
  (:minimize 8)
  (:maximize 16)
  (:close 32))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All functions should be offered.}
    @entry[:resize]{The window should be resizable.}
    @entry[:move]{The window should be movable.}
    @entry[:minimize]{The window should be minimizable.}
    @entry[:maximize]{The window should be maximizable.}
    @entry[:close]{The window should be closable.}
  @end{table}
  @see-class{gdk-window}
  @see-function{gdk-window-set-functions}")

;;; ----------------------------------------------------------------------------
;;; GdkWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkWindow" gdk-window
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_window_get_type")
  ((cursor
    gdk-window-cursor
    "cursor" "GdkCursor" t t)))

(setf (documentation 'gdk-window 'type)
 "@version{2020-9-7}
  @begin{short}
    Onscreen display areas in the target window system.
  @end{short}

  A @sym{gdk-window} object is a usually rectangular region on the screen.
  It is a low-level object, used to implement high-level objects such as
  @class{gtk-widget} and @class{gtk-window} widgets on the GTK+ level. A
  @class{gtk-window} widget is a toplevel window, the thing a user might think
  of as a \"window\" with a titlebar and so on. A @class{gtk-window} widget
  may contain many @sym{gdk-window} objects. For example, each
  @class{gtk-button} widget has a @sym{gdk-window} object associated with it.

  @subheading{Composited Windows}
    Normally, the windowing system takes care of rendering the contents of a
    child window onto its parent window. This mechanism can be intercepted by
    calling the function @fun{gdk-window-composited} on the child window.
    For a composited window it is the responsibility of the application to
    render the window contents at the right spot.

  @subheading{Offscreen Windows}
    Offscreen windows are more general than composited windows, since they allow
    not only to modify the rendering of the child window onto its parent, but
    also to apply coordinate transformations.

    To integrate an offscreen window into a window hierarchy, one has to call
    the function @fun{gdk-offscreen-window-set-embedder} and handle a number of
    signals. The \"pick-embedded-child\" signal on the embedder window is used
    to select an offscreen child at given coordinates, and the \"to-embedder\"
    and \"from-embedder\" signals on the offscreen window are used to translate
    coordinates between the embedder and the offscreen window.

    For rendering an offscreen window onto its embedder, the contents of the
    offscreen window are available as a surface, via the function
    @fun{gdk-offscreen-window-surface}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-surface\" signal}
      @begin{pre}
 lambda (window width height)    : Run Last
      @end{pre}
      The \"create-surface\" signal is emitted when an offscreen window needs
      its surface (re)created, which happens either when the the window is first
      drawn to, or when the window is being resized. The first signal handler
      that returns a non-@code{nil} surface will stop any further signal
      emission, and its surface will be used. Note that it is not possible to
      access the window's previous surface from within any callback of this
      signal. Calling the function @fun{gdk-offscreen-window-surface} will
      lead to a crash.
      @begin[code]{table}
        @entry[window]{The @sym{gdk-window} offscreen window on which the
          signal is emitted.}
        @entry[width]{A @code{:int} with the width of the offscreen surface to
          create.}
        @entry[height]{A @code{:int} with the height of the offscreen surface
          to create.}
        @entry[Returns]{The newly created @symbol{cairo-surface-t} instance
          for the offscreen window.}
      @end{table}
    @subheading{The \"from-embedder\" signal}
      @begin{pre}
 lambda (window embedder-x embedder-y offscreen-x offscreen-y)    : Run Last
      @end{pre}
      The \"from-embedder\" signal is emitted to translate coordinates in the
      embedder of an offscreen window to the offscreen window.
      See also the \"to-embedder\" signal.
      @begin[code]{table}
        @entry[window]{The @sym{gdk-window} offscreen window on which the
          signal is emitted.}
        @entry[embedder-x]{A @code{:double} with the x coordinate in the
          embedder window.}
        @entry[embedder-y]{A @code{:double} with the y coordinate in the
          embedder window.}
        @entry[offscreen-x]{Return location of type @code{:double} for the x
          coordinate in the offscreen window.}
        @entry[offscreen-y]{Return location of type @code{:double} for the y
          coordinate in the offscreen window.}
      @end{table}
    @subheading{The \"moved-to-rect\" signal}
      @begin{pre}
 lambda (window flipped-rect final-rect flipped-x flipped-y)    : Run First
      @end{pre}
      Emitted when the position of window is finalized after being moved to a
      destination rectangle. @arg{window} might be flipped over the destination
      rectangle in order to keep it on-screen, in which case @arg{flipped-x}
      and @arg{flipped-y} will be set to @em{true} accordingly.
      @arg{flipped-rect} is the ideal position of window after any possible
      flipping, but before any possible sliding. @arg{final-rect} is
      @arg{flipped-rect}, but possibly translated in the case that flipping is
      still ineffective in keeping window on-screen. Since 3.22
      @begin[code]{table}
        @entry[window]{The @sym{gdk-window} object that moved.}
        @entry[flipped-rect]{The position of window after any possible flipping
          or @code{nil} if the backend can't obtain it.}
        @entry[final-rect]{The final position of window or NULL if the backend
          can't obtain it.}
        @entry[flipped-x]{@em{True} if the anchors were flipped horizontally.}
        @entry[flipped-y]{@em{True} if the anchors were flipped vertically.}
      @end{table}
    @subheading{The \"pick-embedded-child\" signal}
      @begin{pre}
 lambda (window x y)    : Run Last
      @end{pre}
      The \"pick-embedded-child\" signal is emitted to find an embedded child
      at the given position.
      @begin[code]{table}
        @entry[window]{The @sym{gdk-window} object on which the signal is
          emitted.}
        @entry[x]{A @code{:double} with the x coordinate in the window.}
        @entry[y]{A @code{:douboe} with the y coordinate in the window.}
        @entry[Returns]{The @class{gdk-window} of the embedded child at x, y,
          or @code{nil}.}
      @end{table}
    @subheading{The \"to-embedder\" signal}
      @begin{pre}
 lambda (window offscreen-x offscreen-y embedder-x embedder-y)    : Run Last
      @end{pre}
      The \"to-embedder\" signal is emitted to translate coordinates in an
      offscreen window to its embedder. See also the \"from-embedder\" signal.
      @begin[code]{table}
        @entry[window]{The @sym{gdk-window} offscreen window on which the
          signal is emitted.}
        @entry[offscreen-x]{A @code{:double} with the x coordinate in the
          offscreen window.}
        @entry[offscreen-y]{A @code{:double} with the y coordinate in the
          offscreen window.}
        @entry[embedder-x]{Return location of type @code{:double} for the x
          coordinate in the embedder window.}
        @entry[embedder-y]{Return location of type @code{:double} for the y
          coordinate in the embedder window.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk-window-cursor}
  @see-class{gtk-widget}
  @see-class{gtk-window}
  @see-function{gdk-window-composited}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor" 'gdk-window) 't)
 "The @code{cursor} property of type @class{gdk-cursor} (Read / Write) @br{}
  The mouse pointer for a @sym{gdk-window}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-cursor 'function)
 "@version{2020-9-7}
  @syntax[]{(gdk-window-cursor object) => cursor}
  @syntax[]{(setf (gdk-window-cursor object) cursor)}
  @argument[object]{a @class{gdk-window} object}
  @argument[cursor]{a @class{gdk-cursor} object}
  @begin{short}
    Accessor of the @slot[gdk-window]{cursor} slot of the @class{gdk-window}
    class.
  @end{short}

  The slot access function @sym{gdk-window-cursor} retrieves a
  @class{gdk-cursor} pointer for the cursor currently set on the specified
  window, or @code{nil}. If the return value is @code{nil} then there is no
  custom cursor set on the specified window, and it is using the cursor for its
  parent window. The slot access function @sym{(setf gdk-window-cursor)} sets
  the default mouse pointer for a window.

  Use the functions @fun{gdk-cursor-new-for-display} or
  @fun{gdk-cursor-new-from-pixbuf} to create the cursor. To make the cursor
  invisible, use @code{:blank-cursor} of the @symbol{gdk-cursor-type}
  enumeration. Passing @code{nil} for the @arg{cursor} argument to the
  function @sym{gdk-window-cursor} means that @arg{object} will use the cursor
  of its parent window. Most windows should use this default.
  @see-class{gdk-window}
  @see-class{gdk-cursor}
  @see-symbol{gdk-cursor-type}
  @see-function{gdk-window-cursor}
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-new-from-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_new" gdk-window-new)
    (g-object gdk-window :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[parent]{a @class{gdk-window} object, or @code{nil} to create the
    window as a child of the default root window for the default display}
  @argument[attributes]{attributes of type @symbol{gdk-window-attr} of the new
    window}
  @argument[attributes-mask]{mask of type @symbol{gdk-window-attributes-type}
    indicating which fields in attributes are valid}
  @return{The new @class{gdk-window} object.}
  @begin{short}
    Creates a new window using the attributes from @arg{attributes}.
  @end{short}
  See @symbol{gdk-window-attr} and @symbol{gdk-window-attributes-type} for more
  details.

  Note: To use this on displays other than the default display, @arg{parent}
  must be specified.
  @see-class{gdk-window}
  @see-symbol{gdk-window-attr}
  @see-symbol{gdk-window-attributes-type}"
  (parent (g-object gdk-window))
  (attributes (:pointer (:struct gdk-window-attr)))
  (attributes-mask gdk-window-attributes-type))

(export 'gdk-window-new)

;;; ----------------------------------------------------------------------------
;;; gdk_window_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_destroy" gdk-window-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Destroys the window system resources associated with @arg{window} and
    decrements @arg{window}'s reference count.
  @end{short}
  The window system resources for all children of @arg{window} are also
  destroyed, but the children's reference counts are not decremented.

  Note that a window will not be destroyed automatically when its reference
  count reaches zero. You must call this function yourself before that
  happens.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-destroy)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_window_type () -> gdk-window-window-type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_window_type" gdk-window-window-type)
    gdk-window-type
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{the @symbol{gdk-window-type} of @arg{window}}
  @begin{short}
    Gets the type of the window.
  @end{short}
  See @symbol{gdk-window-type}.
  @see-class{gdk-window}
  @see-symbol{gdk-window-type}"
  (window (g-object gdk-window)))

(export 'gdk-window-window-type)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_display () -> gdk-window-display
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_display" gdk-window-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{The @class{gdk-display} object associated with @arg{window}.}
  @begin{short}
    Gets the display associated with a window.
  @end{short}
  @see-class{gdk-window}
  @see-class{gdk-display}"
  (window (g-object gdk-window)))

(export 'gdk-window-display)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_screen () -> gdk-window-screen
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_screen" gdk-window-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{The @class{gdk-screen} object associated with @arg{window}.}
  @begin{short}
    Gets the screen associated with a window.
  @end{short}
  @see-class{gdk-window}
  @see-class{gdk-screen}"
  (window (g-object gdk-window)))

(export 'gdk-window-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visual () -> gdk-window-visual
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visual" gdk-window-visual) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{A @class{gdk-visual} object.}
  @begin{short}
    Gets the visual describing the pixel format of a window.
  @end{short}
  @see-class{gdk-window}
  @see-class{gdk-visual}"
  (window (g-object gdk-window)))

(export 'gdk-window-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_window_at_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_at_pointer" %gdk-window-at-pointer) (g-object gdk-window)
  (win-x (:pointer :int))
  (win-y (:pointer :int)))

(defun gdk-window-at-pointer ()
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @begin{return}
    @code{window} -- window under the mouse pointer @br{}
    @code{win-x}  -- a @code{:int} with the origin of the window under the
                     pointer @br{}
    @code{win-y}  -- a @code{:int} with the origin of the window under the
                     pointer
  @end{return}
  @begin{short}
    Obtains the window underneath the mouse pointer, returning the location of
    that window in @arg{win-x}, @arg{win-y}.
  @end{short}
  Returns @code{nil} if the window under the mouse pointer is not known to GDK.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-at-pointer} has been deprecated since version
    3.0 and should not be used in newly written code. Use the function
    @fun{gdk-device-window-at-position} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-device-window-at-position}"
  (with-foreign-objects ((x :int) (y :int))
    (let ((window (%gdk-window-at-pointer x y)))
      (when window
        (values window
                (mem-ref x :int)
                (mem-ref y :int))))))

(export 'gdk-window-at-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show" gdk-window-show) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Like the function @fun{gdk-window-show-unraised}, but also raises the window
    to the top of the window stack, moves the window to the front of the
    z-order.
  @end{short}

  This function maps a window so it is visible onscreen. Its opposite is the
  function @fun{gdk-window-hide}.

  When implementing a @class{gtk-widget}, you should call this function on the
  widget's @class{gdk-window} as part of the \"map\" method.
  @see-class{gdk-window}
  @see-class{gtk-widget}
  @see-function{gdk-window-hide}
  @see-function{gdk-window-show-unraised}"
  (window (g-object gdk-window)))

(export 'gdk-window-show)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_unraised ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_unraised" gdk-window-show-unraised) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Shows a window onscreen, but does not modify its stacking order.
  @end{short}
  In contrast, the function @fun{gdk-window-show} will raise the window to the
  top of the window stack.

  On the X11 platform, in Xlib terms, this function calls the function
  @code{XMapWindow()}, it also updates some internal GDK state, which means that
  you cannot really use the function @code{XMapWindow()} directly on a GDK
  window.
  @see-class{gdk-window}
  @see-function{gdk-window-show}"
  (window (g-object gdk-window)))

(export 'gdk-window-show-unraised)

;;; ----------------------------------------------------------------------------
;;; gdk_window_hide ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_hide" gdk-window-hide) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    For toplevel windows, withdraws them, so they will no longer be known to the
    window manager.
  @end{short}
  For all windows, unmaps them, so they will not be displayed. Normally done
  automatically as part of the function @fun{gtk-widget-hide}.
  @see-class{gdk-window}
  @see-function{gtk-widget-hide}"
  (window (g-object gdk-window)))

(export 'gdk-window-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_destroyed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_destroyed" gdk-window-is-destroyed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if @arg{window} is destroyed.}
  @begin{short}
    Check to see if a window is destroyed.
  @end{short}
  @see-class{gdk-window}
  @see-function{gdk-window-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_visible" gdk-window-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if @arg{window} is mapped.}
  @begin{short}
    Checks whether the window has been mapped with the functions
    @fun{gdk-window-show} or @fun{gdk-window-show-unraised}.
  @end{short}
  @see-class{gdk-window}
  @see-function{gdk-window-show}
  @see-function{gdk-window-show-unraised}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-visible)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_viewable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_viewable" gdk-window-is-viewable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if @arg{window} is viewable.}
  @begin{short}
    Check if the window and all ancestors of the window are mapped.
  @end{short}
  This is not necessarily \"viewable\" in the X sense, since we only check as
  far as we have GDK window parents, not to the root window.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-viewable)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_input_only ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_input_only" gdk-window-is-input-only) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a toplevel @class{gdk-window} oject}
  @return{@em{True} if @arg{window} is input only.}
  @begin{short}
    Determines whether or not the window is an input only window.
  @end{short}
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-input-only)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_shaped ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_shaped" gdk-window-is-shaped) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{@em{True} if @arg{window} is shaped.}
  @begin{short}
    Determines whether or not the window is shaped.
  @end{short}
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-shaped)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_state () -> gdk-window-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_state" gdk-window-state) gdk-window-state
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{Window state of type @symbol{gtk-window-state}.}
  @begin{short}
    Gets the bitwise OR of the currently active window state flags, from the
    @symbol{gdk-window-state} enumeration.
  @end{short}
  @see-class{gdk-window}
  @see-symbol{gdk-window-state}"
  (window (g-object gdk-window)))

(export 'gdk-window-state)

;;; ----------------------------------------------------------------------------
;;; gdk_window_withdraw ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_withdraw" gdk-window-withdraw) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Withdraws a window, that is, unmaps it and asks the window manager to
    forget about it.
  @end{short}
  This function is not really useful as the function @fun{gdk-window-hide}
  automatically withdraws toplevel windows before hiding them.
  @see-class{gdk-window}
  @see-function{gdk-window-hide}"
  (window (g-object gdk-window)))

(export 'gdk-window-withdraw)

;;; ----------------------------------------------------------------------------
;;; gdk_window_iconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_iconify" gdk-window-iconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Asks to iconify (minimize) window.
  @end{short}
  The window manager may choose to ignore the request, but normally will honor
  it. Using the function @fun{gtk-window-iconify} is preferred, if you have a
  @class{gtk-window} widget.

  This function only makes sense when @arg{window} is a toplevel window.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gdk-window-deiconify}
  @see-function{gtk-window-iconify}"
  (window (g-object gdk-window)))

(export 'gdk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_deiconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_deiconify" gdk-window-deiconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Attempt to deiconify (unminimize) window.
  @end{short}
  On X11 the window manager may choose to ignore the request to deiconify. When
  using GTK+, use the function @fun{gtk-window-deiconify} instead of the
  @class{gdk-window} variant. Or better yet, you probably want to use
  the function @fun{gtk-window-present}, which raises the window, focuses
  it, unminimizes it, and puts it on the current desktop.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gdk-window-iconify}
  @see-function{gtk-window-deiconify}
  @see-function{gtk-window-present}"
  (window (g-object gdk-window)))

(export 'gdk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_stick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_stick" gdk-window-stick) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    \"Pins\" a window such that it is on all workspaces and does not scroll with
    viewports, for window managers that have scrollable viewports.
  @end{short}
  When using @class{gtk-window}, the function @fun{gtk-window-stick} may be more
  useful.

  On the X11 platform, this function depends on window manager support, so may
  have no effect with many window managers. However, GDK will do the best it
  can to convince the window manager to stick the window. For window managers
  that do not support this operation, there is nothing you can do to force it
  to happen.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gtk-window-stick}
  @see-function{gdk-window-unstick}"
  (window (g-object gdk-window)))

(export 'gdk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unstick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unstick" gdk-window-unstick) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Reverse operation for the function @fun{gdk-window-stick}.
  @end{short}
  See the functions @fun{gdk-window-stick} and @fun{gtk-window-unstick}.
  @see-class{gdk-window}
  @see-function{gdk-window-stick}
  @see-function{gtk-window-unstick}"
  (window (g-object gdk-window)))

(export 'gdk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_maximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_maximize" gdk-window-maximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Maximizes the window.
  @end{short}
  If the window was already maximized, then this function does nothing.

  On X11, asks the window manager to maximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or do not have a concept of \"maximized\"; so you
  cannot rely on the maximization actually happening. But it will happen with
  most standard window managers, and GDK makes a best effort to get it to
  happen.

  On Windows, reliably maximizes the window.
  @see-class{gdk-window}
  @see-function{gdk-window-unmaximize}"
  (window (g-object gdk-window)))

(export 'gdk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unmaximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unmaximize" gdk-window-unmaximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Unmaximizes the window.
  @end{short}
  If the window was not maximized, then this function does nothing.

  On X11, asks the window manager to unmaximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or do not have a concept of \"maximized\"; so you
  cannot rely on the unmaximization actually happening. But it will happen with
  most standard window managers, and GDK makes a best effort to get it to
  happen.

  On Windows, reliably unmaximizes the window.
  @see-class{gdk-window}
  @see-function{gdk-window-maximize}"
  (window (g-object gdk-window)))

(export 'gdk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen" gdk-window-fullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Moves the window into fullscreen mode.
  @end{short}
  This means the window covers the entire screen and is above any panels or
  task bars.

  If the window was already fullscreen, then this function does nothing.

  On X11, asks the window manager to put window in a fullscreen state, if the
  window manager supports this operation. Not all window managers support
  this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the fullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.
  @see-class{gdk-window}
  @see-function{gdk-window-unfullscreen}"
  (window (g-object gdk-window)))

(export 'gdk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen_on_monitor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen_on_monitor" gdk-window-fullscreen-on-monitor)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[monitor]{a @code{:int} which monitor to display fullscreen on}
  @begin{short}
    Moves the window into fullscreen mode on the given monitor.
  @end{short}
  This means the window covers the entire screen and is above any panels or
  task bars.

  If the window was already fullscreen, then this function does nothing.
  @see-class{gdk-window}
  @see-function{gdk-window-fullscreen}
  @see-function{gdk-window-unfullscreen}"
  (window (g-object gdk-window))
  (monitor :int))

(export 'gdk-window-fullscreen-on-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unfullscreen" gdk-window-unfullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Moves the window out of fullscreen mode.
  @end{short}
  If the window was not fullscreen, does nothing.

  On X11, asks the window manager to move window out of the fullscreen state,
  if the window manager supports this operation. Not all window managers
  support this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the unfullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.
  @see-class{gdk-window}
  @see-function{gdk-window-fullscreen}"
  (window (g-object gdk-window)))

(export 'gdk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_fullscreen_mode ()
;;; gdk_window_set_fullscreen_mode () -> gdk-window-fullscreen-mode
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-fullscreen-mode) (mode window)
  (foreign-funcall "gdk_window_set_fullscreen_mode"
                   (g-object gdk-window) window
                   gdk-fullscreen-mode mode
                   :void)
  mode)

(defcfun ("gdk_window_get_fullscreen_mode" gdk-window-fullscreen-mode)
    gdk-fullscreen-mode
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @syntax[]{gdk-window-fullscreen-mode window) => mode}
  @syntax[]{(setf (gdk-window-fullscreen-mode window) mode)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[mode]{a value of the @symbol{gdk-fullscreen-mode} enumeration}
  @begin{short}
    Accessor of the fullscreen mode applied to the window when fullscreen.
  @end{short}

  The function @sym{gdk-window-fullscreen-mode} obtains the fullscreen mode of
  the window. The function @sym{(setf gdk-window-fullscreen-mode)} specifies
  whether the window should span over all monitors (in a multi-head setup) or
  only the current monitor when in fullscreen mode.

  The mode argument is from the @symbol{gdk-fullscreen-mode} enumeration. If
  @code{:on-all-monitors} is specified, the fullscreen window will span over
  all monitors from the @class{gdk-screen} object.

  On X11, searches through the list of monitors from the @class{gdk-screen}
  object the ones which delimit the 4 edges of the entire @class{gdk-screen}
  object and will ask the window manager to span the window over these monitors.

  If the XINERAMA extension is not available or not usable, this function has
  no effect.

  Not all window managers support this, so you can not rely on the fullscreen
  window to span over the multiple monitors when @code{:on-all-monitors} is
  specified.
  @see-class{gdk-window}
  @see-class{gdk-screen}
  @see-symbol{gdk-fullscreen-mode}"
  (window (g-object gdk-window)))

(export 'gdk-window-fullscreen-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_above ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_above" gdk-window-set-keep-above) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[setting]{a boolean whether to keep @arg{window} above other windows}
  @begin{short}
    Set if the window must be kept above other windows.
  @end{short}
  If the window was already above, then this function does nothing.

  On X11, asks the window manager to keep the window above, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep above\"; so
  you cannot rely on the window being kept above. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.
  @see-class{gdk-window}
  @see-function{gdk-window-set-keep-below}"
  (window (g-object gdk-window))
  (setting :boolean))

(export 'gdk-window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_below ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_below" gdk-window-set-keep-below) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[setting]{a boolean whether to keep @arg{window} below other windows}
  @begin{short}
    Set if the window must be kept below other windows.
  @end{short}
  If the window was already below, then this function does nothing.

  On X11, asks the window manager to keep the window below, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep below\"; so you
  cannot rely on the window being kept below. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.
  @see-class{gdk-window}
  @see-function{gdk-window-set-keep-above}"
  (window (g-object gdk-window))
  (setting :boolean))

(export 'gdk-window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opacity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_opacity" gdk-window-set-opacity) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[opacity]{a @code{:double} with the opacity}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    opacity 0 being fully transparent and 1 fully opaque.
  @end{short}
  Values of the opacity parameter are clamped to the [0,1] range.

  On X11, this works only on X screens with a compositing manager running.

  For setting up per-pixel alpha, see the function
  @fun{gdk-screen-rgba-visual}. For making non-toplevel windows translucent,
  see the function @fun{gdk-window-composited}.
  @see-class{gdk-window}
  @see-function{gdk-screen-rgba-visual}
  @see-function{gdk-window-composited}"
  (window (g-object gdk-window))
  (opacity :double))

(export 'gdk-window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_composited ()
;;; gdk_window_set_composited () -> gdk-window-composited
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-composited) (composited window)
  (foreign-funcall "gdk_window_set_composited"
                   (g-object gdk-window) window
                   :boolean composited
                   :void)
  composited)

(defcfun ("gdk_window_get_composited" gdk-window-composited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @syntax[]{(gdk-window-composited window) => composited}
  @syntax[]{(setf (gdk-window-composited window) composited)}
  @argument[window]{a @class{gdk-window} object}
  @argument[composited]{@em{true} to set @arg{window} as composited}
  @begin{short}
    The function @sym{gdk-window-composited}determines whether @arg{window} is
    composited.
  @end{short}
  The function @sym{(setf gdk-window-composited)} sets a window as composited,
  or unsets it.

  Composited windows do not automatically have their contents drawn to the
  screen. Drawing is redirected to an offscreen buffer and an expose event is
  emitted on the parent of the composited window. It is the responsibility of
  the parent's expose handler to manually merge the off-screen content onto the
  screen in whatever way it sees fit.

  It only makes sense for child windows to be composited. See the function
  @fun{gdk-window-set-opacity} if you need translucent toplevel windows.

  An additional effect of this call is that the area of this window is no
  longer clipped from regions marked for invalidation on its parent. Draws
  done on the parent window are also no longer clipped by the child.

  This call is only supported on some systems, currently, only X11 with new
  enough Xcomposite and Xdamage extensions. You must call the function
  @fun{gdk-display-supports-composite} to check if setting a window as
  composited is supported before attempting to do so.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-composited} has been deprecated since version
    3.16 and should not be used in newly-written code. Compositing is an
    outdated technology that only ever worked on X11.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-set-opacity}
  @see-function{gdk-display-supports-composite}"
  (window (g-object gdk-window)))

(export 'gdk-window-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pass_through ()
;;; gdk_window_set_pass_through () -> gdk-window-pass-through
;;; ----------------------------------------------------------------------------

#+gdk-3-18
(defun (setf gdk-window-pass-through) (pass-through window)
  (foreign-funcall "gdk_window_set_pass_through"
                   (g-object gdk-window) window
                   :boolean
                   :void)
  pass-through)

#+gdk-3-18
(defcfun ("gdk_window_get_pass_through" gdk-window-pass-through) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @syntax[]{(gdk-window-pass-through window) => pass-through}
  @syntax[]{(setf (gdk-window-pass-through window) pass-through)}
  @argument[window]{a @class{gdk-window} object}
  @argument[pass-through]{a boolean}
  @begin{short}
    The function @sym{gdk-window-pass-through} returns whether input to the
    window is passed through to the window below.
  @end{short}
  The function @sym{(setf gdk-window-pass-through)} sets whether input to the
  window is passed through to the window below.

  The default value of this is @em{false}, which means that pointer events that
  happen inside the window are send first to the window, but if the event is
  not selected by the event mask then the event is sent to the parent window,
  and so on up the hierarchy.

  If @arg{pass-through} is @em{true} then such pointer events happen as if the
  window was not there at all, and thus will be sent first to any windows below
  @arg{window}. This is useful if the window is used in a transparent fashion.
  In the terminology of the web this would be called \"pointer-events: none\".

  Note that a window with @arg{pass-through} @em{true} can still have a
  subwindow without pass through, so you can get events on a subset of a window.
  And in that cases you would get the in-between related events such as the
  pointer enter/leave events on its way to the destination window.

  Since 3.18
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

#+gdk-3-18
(export 'gdk-window-pass-through)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move" gdk-window-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{a @code{:int} with the x coordinate relative to window's parent}
  @argument[y]{a @code{:int} with the y coordinate relative to window's parent}
  @begin{short}
    Repositions a window relative to its parent window.
  @end{short}
  For toplevel windows, window managers may ignore or modify the move; you
  should probably use the function @fun{gtk-window-move} on a @class{gtk-window}
  widget anyway, instead of using GDK functions. For child windows, the move
  will reliably succeed.

  If you are also planning to resize the window, use the function
  @fun{gdk-window-move-resize} to both move and resize simultaneously, for a
  nicer visual effect.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gtk-window-move}
  @see-function{gdk-window-move-resize}"
  (window (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-move)

;;; ----------------------------------------------------------------------------
;;; gdk_window_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_resize" gdk-window-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[width]{a @code{:int} with the new width of the window}
  @argument[height]{a @code{:int} with the new height of the window}
  @begin{short}
    Resizes the window, for toplevel windows, asks the window manager to
    resize the window.
  @end{short}
  The window manager may not allow the resize. When using GTK+, use the
  function @fun{gtk-window-resize} instead of this low-level GDK function.

  Windows may not be resized below 1x1.

  If you are also planning to move the window, use the function
  @fun{gdk-window-move-resize} to both move and resize simultaneously, for a
  nicer visual effect.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gtk-window-resize}
  @see-function{gdk-window-move-resize}"
  (window (g-object gdk-window))
  (width :int)
  (height :int))

(export 'gdk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_resize" gdk-window-move-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{a @code{:int} with the new x position relative to window's
    parent}
  @argument[y]{a @code{:int} with the new y position relative to window's
    parent}
  @argument[width]{a @code{:int} with the new width}
  @argument[height]{a @code{:int} with the new height}
  @begin{short}
    Equivalent to calling the functions @fun{gdk-window-move} and
    @fun{gdk-window-resize}, except that both operations are performed at once,
    avoiding strange visual effects.
  @end{short}
  I.e. the user may be able to see the window first move, then resize, if you
  do not use the function @sym{gdk-window-move-resize}.
  @see-class{gdk-window}
  @see-function{gdk-window-move}
  @see-function{gdk-window-resize}"
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-window-move-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_scroll ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_scroll" gdk-window-scroll) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[dx]{a @code{:int} with the amount to scroll in the x direction}
  @argument[dy]{a @code{:int} with the amount to scroll in the y direction}
  @begin{short}
    Scroll the contents of window, both pixels and children, by the given
    amount.
  @end{short}
  @arg{window} itself does not move. Portions of the window that the scroll
  operation brings in from offscreen areas are invalidated. The invalidated
  region may be bigger than what would strictly be necessary.

  For X11, a minimum area will be invalidated if the window has no subwindows,
  or if the edges of the window's parent do not extend beyond the edges of the
  window. In other cases, a multi-step process is used to scroll the window
  which may produce temporary visual artifacts and unnecessary invalidations.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (dx :int)
  (dy :int))

(export 'gdk-window-scroll)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_to_rect ()
;;; ----------------------------------------------------------------------------

#+gdk-3-24
(defcfun ("gdk_window_move_to_rect" gdk-window-move-to-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{the @class{gdk-window} object to move}
  @argument[rect]{the destination @class{gdk-rectangle} to align @arg{window}
    with}
  @argument[rect-anchor]{a @symbol{gdk-gravity} value with the point on
    @arg{rect} to align with @arg{window}'s anchor point}
  @argument[window-anchor]{a @symbol{gdk-gravity} value with the point on
    @arg{window} to align with @arg{rect}'s anchor point}
  @argument[anchor-hints]{positioning hints of type @symbol{gdk-anchor-hints}
    to use when limited on space}
  @argument[rect-anchor-dx]{a @code{:int} with the horizontal offset to shift
    @arg{window}, i.e. @arg{rect}'s anchor point}
  @argument[rect-anchor-dy]{a @code{:int} with the vertical offset to shift
    @arg{window}, i.e. @arg{rect}'s anchor point}
  @begin{short}
    Moves the window to @arg{rect}, aligning their anchor points.
  @end{short}

  @arg{rect} is relative to the top-left corner of the window that window is
  transient for. @arg{rect-anchor} and @arg{window-anchor} determine anchor
  points on @arg{rect} and @arg{window} to pin together. @arg{rect}'s anchor
  point can optionally be offset by @arg{rect-anchor-dx} and
  @arg{rect-anchor-dy}, which is equivalent to offsetting the position of
  @arg{window}.

  @arg{anchor-hints} determines how window will be moved if the anchor points
  cause it to move off-screen. For example, @code{:flip-x} will replace
  @code{:north-west} with @code{:north-east} and vice versa if @arg{window}
  extends beyond the left or right edges of the monitor.

  Connect to the \"moved-to-rect\" signal to find out how it was actually
  positioned.

  Since 3.24
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (rect (g-boxed-foreign gdk-rectangle))
  (rect-anchor gdk-gravity)
  (window-anchor gdk-gravity)
  (anchor-hints gdk-anchor-hints)
  (rect-anchor-dx :int)
  (rect-anchor-dy :int))

#+gdk-3-24
(export 'gdk-window-move-to-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_region" gdk-window-move-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{the @symbol{cairo-region-t} to move}
  @argument[dx]{a @code{:int} with the amount to move in the x direction}
  @argument[dy]{a @code{:int} with the amount to move in the y direction}
  @begin{short}
    Move the part of window indicated by region by dy pixels in the y direction
    and dx pixels in the x direction.
  @end{short}
  The portions of region that not covered by the new position of region are
  invalidated.

  Child windows are not moved.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t)))
  (dx :int)
  (dy :int))

(export 'gdk-window-move-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_flush" gdk-window-flush) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    This function does nothing.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-window-flush} function has been deprecated since version 3.14
    and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_window_has_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_has_native" gdk-window-has-native) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the window has a native window, @em{false} otherwise.}
  @begin{short}
    Checks whether the window has a native window or not.
  @end{short}
  Note that you can use the function @fun{gdk-window-ensure-native} if a native
  window is needed.
  @see-class{gdk-window}
  @see-function{gdk-window-ensure-native}"
  (window (g-object gdk-window)))

(export 'gdk-window-has-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ensure_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_ensure_native" gdk-window-ensure-native) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the window has a native window, @em{false} otherwise.}
  @begin{short}
    Tries to ensure that there is a window-system native window for this
    window.
  @end{short}
  This may fail in some situations, returning @em{false}.

  Offscreen window and children of them can never have native windows.

  Some backends may not support native child windows.
  @see-class{gdk-window}
  @see-function{gdk-window-has-native}"
  (window (g-object gdk-window)))

(export 'gdk-window-ensure-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_reparent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_reparent" gdk-window-reparent) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[new-parent]{new @class{gdk-window} parent to move @arg{window} into}
  @argument[x]{a @code{:int} with the x location inside the new parent}
  @argument[y]{a @code{:int} with the y location inside the new parent}
  @begin{short}
    Reparents @arg{window} into the given @arg{new-parent}.
  @end{short}
  The window being reparented will be unmapped as a side effect.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (new-parent (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-reparent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_raise ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_raise" gdk-window-raise) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Raises the window to the top of the z-order (stacking order), so that other
    windows with the same parent window appear below the window.
  @end{short}
  This is true whether or not the windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function @sym{gdk-window-raise}
  only requests the restack, does not guarantee it.
  @see-class{gdk-window}
  @see-function{gdk-window-lower}"
  (window (g-object gdk-window)))

(export 'gdk-window-raise)

;;; ----------------------------------------------------------------------------
;;; gdk_window_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_lower" gdk-window-lower) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Lowers the window to the bottom of the z-order (stacking order), so that
    other windows with the same parent window appear above the window.
  @end{short}
  This is true whether or not the other windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function @sym{gdk-window-lower}
  only requests the restack, does not guarantee it.

  Note that the @fun{gdk-window-show} function raises the window again, so do
  not call this function before the @fun{gdk-window-show} function.
  Try the @fun{gdk-window-show-unraised} function.
  @see-class{gdk-window}
  @see-function{gdk-window-raise}
  @see-function{gdk-window-show}
  @see-function{gdk-window-show-unraised}"
  (window (g-object gdk-window)))

(export 'gdk-window-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_window_restack ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_restack" gdk-window-restack) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[sibling]{a @class{gdk-window} object that is a sibling of
    @arg{window}, or @code{nil}}
  @argument[above]{a boolean}
  @begin{short}
    Changes the position of the window in the z-order (stacking order), so
    that it is above @arg{sibling}, if @arg{above} is @em{true}, or below
    sibling, if @arg{above} is @em{false}.
  @end{short}

  If @arg{sibling} is @code{nil}, then this either raises, if @arg{above} is
  @em{true}, or lowers the window.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function
  @sym{gdk-window-restack} only requests the restack, does not guarantee it.
  @see-class{gdk-window}
  @see-function{gdk-window-raise}
  @see-function{gdk-window-lower}"
  (window (g-object gdk-window))
  (sibling (g-object gdk-window))
  (above :boolean))

(export 'gdk-window-restack)

;;; ----------------------------------------------------------------------------
;;; gdk_window_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_focus" gdk-window-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[timestamp]{an unsigned integer with the timestamp of the event
    triggering the window focus}
  @begin{short}
    Sets keyboard focus to @arg{window}.
  @end{short}
  In most cases, the function @fun{gtk-window-present} should be used on a
  @class{gtk-window} widget, rather than calling this function.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gtk-window-present}"
  (window (g-object gdk-window))
  (timestamp :uint32))

(export 'gdk-window-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_register_dnd ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_register_dnd" gdk-window-register-dnd) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @shot{Registers a window as a potential drop destination.}
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-register-dnd)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag" gdk-window-begin-resize-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[edge]{the edge or corner of type @symbol{gdk-window-edge} from
    which the drag is started}
  @argument[button]{a @code{:int} with the button being used to drag}
  @argument[root-x]{a @code{:int} with the root window x coordinate of mouse
    click that began the drag}
  @argument[root-y]{a @code{:int} with the root window y coordinate of mouse
    click that began the drag}
  @argument[timestamp]{a @code{:uint} with the timestamp of mouse click that
    began the drag, use the function @fun{gdk-event-time}}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}

  This function assumes that the drag is controlled by the client pointer
  device, use the function @fun{gdk-window-begin-resize-drag-for-device} to
  begin a drag with a different device.
  @see-class{gdk-window}
  @see-symbol{gdk-window-edge}
  @see-function{gdk-event-time}
  @see-function{gdk-window-begin-resize-drag-for-device}"
  (window (g-object gdk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag_for_device"
           gdk-window-begin-resize-drag-for-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[edge]{the edge or corner of type @symbol{gdk-window-edge} from
    which the drag is started}
  @argument[device]{the @class{gdk-device} object used for the operation}
  @argument[button]{a @code{:int} with the button being used to drag}
  @argument[root-x]{a @code{:int} with the root window x coordinate of mouse
    click that began the drag}
  @argument[root-y]{a @code{:int} with the root window y coordinate of mouse
    click that began the drag}
  @argument[timestamp]{a @code{:uint} with the timestamp of mouse click that
    began the drag, use the function @fun{gdk-event-time}}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window resize grip\", for
  example; in fact @class{gtk-statusbar} widgets uses it. The function works
  best with window managers that support the Extended Window Manager Hints, but
  has a fallback implementation for other window managers.
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-class{gtk-statusbar}
  @see-function{gdk-event-time}"
  (window (g-object gdk-window))
  (edge gdk-window-edge)
  (device (g-object gdk-device))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-resize-drag-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_move_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_move_drag" gdk-window-begin-move-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[button]{a @code{:int} with the button being used to drag}
  @argument[root-x]{a @code{:int} with the root window x coordinate of mouse
    click that began the drag}
  @argument[root-y]{a @code{:int} with the root window y coordinate of mouse
    click that began the drag}
  @argument[timestamp]{a @code{:uint} with the timestamp of mouse click that
    began the drag}
  @begin{short}
    Begins a window move operation for a toplevel window.
  @end{short}

  This function assumes that the drag is controlled by the client pointer
  device, use the function @fun{gdk-window-begin-move-drag-for-device} to begin
  a drag with a different device.
  @see-class{gdk-window}
  @see-function{gdk-window-begin-move-drag-for-device}"
  (window (g-object gdk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_move_drag_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_move_drag_for_device"
           gdk-window-begin-move-drag-for-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[device]{the @class{gdk-device} object used for the operation}
  @argument[button]{a @code{:int} with the button being used to drag}
  @argument[root-x]{a @code{:int} with the root window x coordinate of mouse
    click that began the drag}
  @argument[root-y]{a @code{:int} with the root window y coordinate of mouse
    click that began the drag}
  @argument[timestamp]{a @code{:uint} with the timestamp of mouse click that
    began the drag}
  @begin{short}
    Begins a window move operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window move grip\", for example.
  The function works best with window managers that support the Extended Window
  Manager Hints, but has a fallback implementation for other window managers.
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-function{gdk-window-begin-move-drag}"
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-move-drag-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_window_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_window_menu" gdk-window-show-window-menu) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[event]{a @class{gdk-event} event to show the menu for}
  @return{@em{True} if the window menu was shown and @em{false} otherwise.}
  @begin{short}
    Asks the windowing system to show the window menu.
  @end{short}
  The window menu is the menu shown when right-clicking the titlebar on
  traditional windows managed by the window manager. This is useful for windows
  using client-side decorations, activating it with a right-click on the window
  decorations.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-window-show-window-menu)

;;; ----------------------------------------------------------------------------
;;; gdk_window_constrain_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_constrain_size" %gdk-window-constrain-size) :void
  (geometry (:pointer (:struct gdk-geometry)))
  (flags gdk-window-hints)
  (width :int)
  (height :int)
  (new-width (:pointer :int))
  (new-height (:pointer :int)))

(defun gdk-window-constrain-size (geometry flags width height)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[geometry]{a @symbol{gdk-geometry} structure}
  @argument[flags]{a mask of type @symbol{gdk-window-hints} indicating what
    portions of geometry are set}
  @argument[width]{a @code{:int} with the desired width of the window}
  @argument[height]{a @code{:int} with the desired height of the window}
  @begin{return}
    @code{new-width}  -- @code{:int} with the resulting width @br{}
    @code{new-height} -- @code{:int} with the resulting height
  @end{return}
  @begin{short}
    Constrains a desired @arg{width} and @arg{height} according to a set of
    geometry hints such as minimum and maximum size.
  @end{short}
  @see-class{gdk-window}
  @see-symbol{gdk-geometry}
  @see-symbol{gdk-window-hints}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_beep" gdk-window-beep) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-22}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Emits a short beep associated to @arg{window} in the appropriate display, if
    supported.
  @end{short}
  Otherwise, emits a short beep on the display just as the function
  @fun{gdk-display-beep}.
  @see-class{gdk-window}
  @see-function{gdk-display-beep}"
  (window (g-object gdk-window)))

(export 'gdk-window-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_scale_factor () -> gdk-window-scale-factor
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_scale_factor" gdk-window-scale-factor) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object to get scale factor for}
  @return{A @code{:int} with the scale factor.}
  @begin{short}
    Returns the internal scale factor that maps from window coordiantes to the
    actual device pixels.
  @end{short}
  On traditional systems this is 1, but on very high density outputs this can
  be a higher value (often 2).

  A higher value means that drawing is automatically scaled up to a higher
  resolution, so any code doing drawing will automatically look nicer. However,
  if you are supplying pixel-based data the scale value can be used to
  determine whether to use a pixel resource with higher resolution data.

  The scale of a window may change during runtime, if this happens a configure
  event will be sent to the toplevel window.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-scale-factor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opaque_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_opaque_region" gdk-window-set-opaque-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a toplevel or non-native @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t} structure, or @code{nil}}
  @begin{short}
    For optimisation purposes, compositing window managers may like to not draw
    obscured regions of windows, or turn off blending during for these regions.
  @end{short}
  With RGB windows with no transparency, this is just the shape of the window,
  but with ARGB32 windows, the compositor does not know what regions of the
  window are transparent or not.

  This function only works for toplevel windows.

  GTK+ will update this property automatically if the window background is
  opaque, as we know where the opaque regions are. If your window background
  is not opaque, please update this property in your \"style-updated\" handler.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t))))

(export 'gdk-window-set-opaque-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_gl_context ()
;;;
;;; GdkGLContext *
;;; gdk_window_create_gl_context (GdkWindow *window,
;;;                               GError **error);
;;;
;;; Creates a new GdkGLContext matching the framebuffer format to the visual of
;;; the GdkWindow. The context is disconnected from any particular window or
;;; surface.
;;;
;;; If the creation of the GdkGLContext failed, error will be set.
;;;
;;; Before using the returned GdkGLContext, you will need to call
;;; gdk_gl_context_make_current() or gdk_gl_context_realize().
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; error
;;;     return location for an error
;;;
;;; Returns
;;;     the newly created GdkGLContext, or NULL on error.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_mark_paint_from_clip ()
;;;
;;; void
;;; gdk_window_mark_paint_from_clip (GdkWindow *window,
;;;                                  cairo_t *cr);
;;;
;;; If you call this during a paint (e.g. between
;;; gdk_window_begin_paint_region() and gdk_window_end_paint() then GDK will
;;; mark the current clip region of the window as being drawn. This is required
;;; when mixing GL rendering via gdk_cairo_draw_from_gl() and cairo rendering,
;;; as otherwise GDK has no way of knowing when something paints over the
;;; GL-drawn regions.
;;;
;;; This is typically called automatically by GTK+ and you don't need to care
;;; about this.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; cr
;;;     a cairo_t
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_clip_region () -> gdk-window-clip-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_clip_region" gdk-window-clip-region)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    A @symbol{cairo-region-t} structure. This must be freed with the function
    @fun{cairo-region-destroy} when you are done.
  @end{return}
  @begin{short}
    Computes the region of a window that potentially can be written to by
    drawing primitives.
  @end{short}
  This region may not take into account other factors such as if the window is
  obscured by other windows, but no area outside of this region will be affected
  by drawing primitives.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_rect" gdk-window-begin-paint-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[rectangle]{a @class{gdk-rectangle} you intend to draw to}
  @begin{short}
    A convenience wrapper around the function
    @fun{gdk-window-begin-paint-region} which creates a rectangular region for
    you.
  @end{short}
  See the function @fun{gdk-window-begin-paint-region} for details.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-begin-paint-rect} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-window-begin-draw-frame} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-class{gdk-rectangle}
  @see-function{gdk-window-begin-paint-region}
  @see-function{gdk-window-begin-draw-frame}"
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-window-begin-paint-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_region" gdk-window-begin-paint-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t} structue you intend to draw to}
  @begin{short}
    Indicates that you are beginning the process of redrawing region.
  @end{short}
  A backing store (offscreen buffer) large enough to contain @arg{region} will
  be created. The backing store will be initialized with the background color or
  background surface for @arg{window}. Then, all drawing operations performed on
  @arg{window} will be diverted to the backing store. When you call the function
  @fun{gdk-window-end-paint}, the backing store will be copied to @arg{window},
  making it visible onscreen. Only the part of @arg{window} contained in
  @arg{region} will be modified; that is, drawing operations are clipped to
  @arg{region}.

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call the function
  @fun{gdk-window-end-paint}. If you draw to @arg{window} directly without
  calling the function @sym{gdk-window-begin-paint-region}, the user may see
  flicker as individual drawing operations are performed in sequence. The
  clipping and background-initializing features of the function
  @sym{gdk-window-begin-paint-region} are conveniences for the programmer, so
  you can avoid doing that work yourself.

  When using GTK+, the widget system automatically places calls to the
  functions @sym{gdk-window-begin-paint-region} and @fun{gdk-window-end-paint}
  around emissions of the \"expose-event\" signal. That is, if you are writing
  an expose event handler, you can assume that the exposed area in
  @class{gdk-event-expose} has already been cleared to the window background,
  is already set as the clip region, and already has a backing store. Therefore
  in most cases, application code need not call the function
  @sym{gdk-window-begin-paint-region}. You can disable the automatic
  calls around expose events on a widget-by-widget basis by calling the
  function @fun{gtk-widget-double-buffered}.

  If you call this function multiple times before calling the matching function
  @fun{gdk-window-end-paint}, the backing stores are pushed onto a stack. The
  function @fun{gdk-window-end-paint} copies the topmost backing store onscreen,
  subtracts the topmost region from all other regions in the stack, and pops the
  stack. All drawing operations affect only the topmost backing store in the
  stack. One matching call to the function @fun{gdk-window-end-paint} is
  required for each call to the function @sym{gdk-window-begin-paint-region}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-begin-paint-region} has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the function
    @fun{gdk-window-begin-draw-frame} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-class{gdk-event-expose}
  @see-function{gdk-window-end-paint}
  @see-function{gtk-widget-double-buffered}
  @see-function{gdk-window-begin-draw-frame}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t))))

(export 'gdk-window-begin-paint-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_paint" gdk-window-end-paint) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Indicates that the backing store created by the most recent call to the
    function @fun{gdk-window-begin-paint-region} should be copied onscreen and
    deleted, leaving the next-most-recent backing store or no backing store at
    all as the active paint region.
  @end{short}
  See the function @fun{gdk-window-begin-paint-region} for full details.

  It is an error to call this function without a matching call to the function
  @fun{gdk-window-begin-paint-region} first.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-end-paint} is deprecated and should not be used
    in newly-written code.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-begin-paint-region}"
  (window (g-object gdk-window)))

(export 'gdk-window-end-paint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_draw_frame ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun ("gdk_window_begin_draw_frame" gdk-window-begin-draw-frame)
    (g-object gdk-drawing-context)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t} structure}
  @return{A @class{gdk-drawing-context} object that should be used to draw the
    contents of the window, the returned context is owned by GDK.}
  @begin{short}
    Indicates that you are beginning the process of redrawing region on
    @arg{window}, and provides you with a @class{gdk-drawing-context} object.
  @end{short}

  If @arg{window} is a top level @class{gdk-window} object, backed by a native
  window implementation, a backing store (offscreen buffer) large enough to
  contain @arg{region} will be created. The backing store will be initialized
  with the background color or background surface for @arg{window}. Then, all
  drawing operations performed on @arg{window} will be diverted to the backing
  store. When you call the function @fun{gdk-window-end-draw-frame}, the
  contents of the backing store will be copied to @arg{window}, making it
  visible on screen. Only the part of @arg{window} contained in @arg{region}
  will be modified; that is, drawing operations are clipped to @arg{region}.

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call the function
  @fun{gdk-window-end-draw-frame}. If you draw to window directly without
  calling the function @sym{gdk-window-begin-draw-frame}, the user may see
  flicker as individual drawing operations are performed in sequence.

  When using GTK+, the widget system automatically places calls to the functions
  @sym{gdk-window-begin-draw-frame} and @fun{gdk-window-end-draw-frame} around
  emissions of the \"draw\" signal. That is, if you are drawing the contents of
  the widget yourself, you can assume that the widget has a cleared background,
  is already set as the clip region, and already has a backing store. Therefore
  in most cases, application code in GTK does not need to call the function
  @sym{gdk-window-begin-draw-frame} explicitly.

  Since 3.22
  @see-class{gdk-window}
  @see-class{gdk-drawing-context}
  @see-symbol{cairo-region-t}
  @see-function{gdk-window-end-frame}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t))))

#+gdk-3-22
(export 'gdk-window-begin-draw-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_draw_frame ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun ("gdk_window_end_draw_frame" gdk-window-end-draw-frame) :void

 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[context]{the @class{gdk-drawing-contect} object created by the
   function @fun{gdk-window-begin-draw-frame}}
  @begin{short}
    Indicates that the drawing of the contents of @arg{window} started with
    the function @fun{gdk-window-begin-draw-frame} has been completed.
  @end{short}

  This function will take care of destroying the @class{gdk-drawing-context}
  object.

  It is an error to call this function without a matching call of the function
  @fun{gdk-window-begin-draw-frame} first.

  Since 3.22
  @see-class{gdk-window}
  @see-class{gdk-drawing-context}"
  (window (g-object gdk-window))
  (context (g-object gdk-drawing-context)))

#+gdk-3-22
(export 'gdk-window-end-draw-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visible_region () -> gdk-window-visible-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visible_region" gdk-window-visible-region)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    A @symbol{cairo-region-t} structure. This must be freed with the function
    @fun{cairo-region-destroy} when you are done.
  @end{return}
  Computes the region of the window that is potentially visible. This does not
  necessarily take into account if the window is obscured by other windows,
  but no area outside of this region is visible.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-visible-region)

;;; ----------------------------------------------------------------------------
;;; GdkWindowInvalidateHandlerFunc ()
;;;
;;; void
;;; (*GdkWindowInvalidateHandlerFunc) (GdkWindow *window,
;;;                                    cairo_region_t *region);
;;;
;;; Whenever some area of the window is invalidated (directly in the window or
;;; in a child window) this gets called with region in the coordinate space of
;;; window . You can use region to just keep track of the dirty region, or you
;;; can actually change region in case you are doing display tricks like showing
;;; a child in multiple places.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; region
;;;     a cairo_region_t
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_invalidate_handler ()
;;;
;;; void
;;; gdk_window_set_invalidate_handler (GdkWindow *window,
;;;                                    GdkWindowInvalidateHandlerFunc handler);
;;;
;;; Registers an invalidate handler for a specific window. This will get called
;;; whenever a region in the window or its children is invalidated.
;;;
;;; This can be used to record the invalidated region, which is useful if you
;;; are keeping an offscreen copy of some region and want to keep it up to date.
;;; You can also modify the invalidated region in case you’re doing some effect
;;; where e.g. a child widget appears in multiple places.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; handler
;;;     a GdkWindowInvalidateHandlerFunc callback function
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_rect" gdk-window-invalidate-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[rect]{a @class{gdk-rectangle} to invalidate or @code{nil} to
    invalidate the whole window}
  @argument[invalidate-children]{a boolean whether to also invalidate child
    windows}
  @begin{short}
    A convenience wrapper around the function @fun{gdk-window-invalidate-region}
    which invalidates a rectangular region.
  @end{short}
  See the function @fun{gdk-window-invalidate-region} for details.
  @see-class{gdk-window}
  @see-class{gdk-rectangle}
  @see-function{gdk-window-invalidate-region}"
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_region" gdk-window-invalidate-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t} structure}
  @argument[invalidate-children]{@em{true} to also invalidate child windows}
  @begin{short}
    Adds @arg{region} to the update area for @arg{window}.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call of the function @fun{gdk-window-process-updates} sends one or more
  expose events to the window, which together cover the entire update area. An
  application would normally redraw the contents of the window in response to
  those expose events.

  GDK will call the function @fun{gdk-window-process-all-updates} on your behalf
  whenever your program returns to the main loop and becomes idle, so normally
  there is no need to do that manually, you just need to invalidate regions that
  you know should be redrawn.

  The @arg{invalidate-children} parameter controls whether the region of each
  child window that intersects @arg{region} will also be invalidated. If
  @code{nil}, then the update area for child windows will remain unaffected. See
  the function @fun{gdk-window-invalidate-maybe-recurse} if you need fine
  grained control over which children are invalidated.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{gdk-window-process-updates}
  @see-function{gdk-window-process-all-updates}
  @see-function{gdk-window-invalidate-maybe-recurse}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t)))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-region)

;;; ----------------------------------------------------------------------------
;;; GdkWindowChildFunc ()
;;;
;;; gboolean (*GdkWindowChildFunc) (GdkWindow *window, gpointer user_data);
;;;
;;; A function of this type is passed to gdk_window_invalidate_maybe_recurse().
;;; It gets called for each child of the window to determine whether to
;;; recursively invalidate it or now.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; user_data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE to invalidate window recursively
;;; ----------------------------------------------------------------------------

(defcallback gdk-window-child-func-cb :boolean
    ((window (g-object gdk-window))
     (user-data :pointer))
  (let ((func (get-stable-pointer-value user-data)))
    (funcall func window)))

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_maybe_recurse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_maybe_recurse"
          %gdk-window-invalidate-maybe-recurse) :void
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t)))
  (child-func :pointer)
  (user-data :pointer))

(defun gdk-window-invalidate-maybe-recurse (window region child-func)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t} structure}
  @argument[child-func]{function to use to decide if to recurse to a child,
    @code{NULL} means never recurse}
  @begin{short}
    Adds region to the update area for window.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call of the function @fun{gdk-window-process-updates} sends one or more
  expose events to the window, which together cover the entire update area. An
  application would normally redraw the contents of @arg{window} in response to
  those expose events.

  GDK will call the function @fun{gdk-window-process-all-updates} on your behalf
  whenever your program returns to the main loop and becomes idle, so normally
  there is no need to do that manually, you just need to invalidate regions that
  you know should be redrawn.

  The @arg{child-func} parameter controls whether the region of each child
  window that intersects region will also be invalidated. Only children for
  which @arg{child-func} returns @em{true} will have the area invalidated.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{gdk-window-process-updates}
  @see-function{gdk-window-process-all-updates}"
  (with-stable-pointer (ptr child-func)
    (%gdk-window-invalidate-maybe-recurse window
                                          region
                                          (callback gdk-window-child-func-cb)
                                          ptr)))

(export 'gdk-window-invalidate-maybe-recurse)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_update_area () -> gdk-window-update-area
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_update_area" gdk-window-update-area)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @return{The update @symbol{cairo-region-t} area for window.}
  @begin{short}
    Transfers ownership of the update area from @arg{window} to the caller of
    the function.
  @end{short}
  That is, after calling this function, @arg{window} will no longer have an
  invalid/dirty region; the update area is removed from @arg{window} and handed
  to you. If a window has no update area, the function
  @sym{gdk-window-update-area} returns @code{nil}. You are responsible for
  calling the function @fun{cairo-region-destroy} on the returned region if it
  is non-@code{nil}.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-update-area)

;;; ----------------------------------------------------------------------------
;;; gdk_window_freeze_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_freeze_updates" gdk-window-freeze-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Temporarily freezes a window such that it will not receive expose events.
  @end{short}
  The window will begin receiving expose events again when the function
  @fun{gdk-window-thaw-updates} is called. If the function
  @sym{gdk-window-freeze-updates} has been called more than once, the function
  @fun{gdk-window-thaw-updates} must be called an equal number of times to
  begin processing exposes.
  @see-class{gdk-window}
  @see-function{gdk-window-thaw-updates}"
  (window (g-object gdk-window)))

(export 'gdk-window-freeze-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_thaw_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_thaw_updates" gdk-window-thaw-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Thaws a window frozen with the function @fun{gdk-window-freeze-updates}.
  @end{short}
  @see-class{gdk-window}
  @see-function{gdk-window-freeze-updates}"
  (window (g-object gdk-window)))

(export 'gdk-window-thaw-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_all_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_all_updates" gdk-window-process-all-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @begin{short}
    Calls the function @fun{gdk-window-process-updates} for all windows.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-process-all-updates} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-process-updates}")

(export 'gdk-window-process-all-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_updates" gdk-window-process-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[update-children]{a boolean whether to also process updates for
    child windows}
  @begin{short}
    Sends one or more expose events to window.
  @end{short}
  The areas in each expose event will cover the entire update area for the
  window, see the function @fun{gdk-window-invalidate-region} for details.
  Normally GDK calls the function @fun{gdk-window-process-all-updates} on your
  behalf, so there is no need to call this function unless you want to force
  expose events to be delivered immediately and synchronously, vs. the usual
  case, where GDK delivers them in an idle handler. Occasionally this is useful
  to produce nicer scrolling behavior, for example.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-process-updates} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-invalidate-region}
  @see-function{gdk-window-process-all-updates}"
  (window (g-object gdk-window))
  (update-children :boolean))

(export 'gdk-window-process-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_debug_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_debug_updates" gdk-window-set-debug-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[setting]{@em{true} to turn on update debugging}
  @begin{short}
    With update debugging enabled, calls to the function
    @fun{gdk-window-invalidate-region} clear the invalidated region of the
    screen to a noticeable color, and GDK pauses for a short time before sending
    exposes to windows during the function @fun{gdk-window-process-updates}.
  @end{short}
  The net effect is that you can see the invalid region for each window and
  watch redraws as they occur. This allows you to diagnose inefficiencies in
  your application.

  In essence, because the GDK rendering model prevents all flicker, if you are
  redrawing the same region 400 times you may never notice, aside from
  noticing a speed problem. Enabling update debugging causes GTK to flicker
  slowly and noticeably, so you can see exactly what is being redrawn when, in
  what order.

  The @code{--gtk-debug=updates} command line option passed to GTK+ programs
  enables this debug option at application startup time. That is usually more
  useful than calling the function @fun{gdk-window-set-debug-updates} yourself,
  though you might want to use this function to enable updates sometime after
  application startup time.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-set-debug-updates} has been deprecated since
    version 3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-invalidate-region}
  @see-function{gdk-window-process-updates}"
  (setting :boolean))

(export 'gdk-window-set-debug-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_enable_synchronized_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_enable_synchronized_configure"
           gdk-window-enable-synchronized-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Does nothing, present only for compatiblity.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-enable-synchronized-configure} has been
    deprecated since version 3.8 and should not be used in newly-written code.
    This function is no longer needed.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-configure-finished}"
  (window (g-object gdk-window)))

(export 'gdk-window-enable-synchronized-configure)

;;; ----------------------------------------------------------------------------
;;; gdk_window_configure_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_configure_finished" gdk-window-configure-finished) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Does nothing, present only for compatiblity.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-configure-finished} has been deprecated since
    version 3.8 and should not be used in newly-written code. This function is
    no longer needed.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-enable-synchronized-configure}"
  (window (g-object gdk-window)))

(export 'gdk-window-configure-finished)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_clock () -> gdk-window-frame-clock
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_clock" gdk-window-frame-clock)
    (g-object gdk-frame-clock)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a @class{gdk-window} obeject}
  @return{The @class{gdk-frame-clock} object.}
  @begin{short}
    Gets the frame clock for the window.
  @end{short}
  The frame clock for a window never changes unless the window is reparented to
  a new toplevel window.
  @see-class{gdk-window}
  @see-class{gdk-frame-clock}"
  (window (g-object gdk-window)))

(export 'gdk-window-frame-clock)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_user_data ()
;;; gdk_window_set_user_data () -> gdk-window-user-data
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-user-data) (user-data window)
  (foreign-funcall "gdk_window_set_user_data"
                   (g-object gdk-window) window
                   :pointer user-data
                   :void)
  user-data)

(defcfun ("gdk_window_get_user_data" %gdk-window-user-data) :void
  (window (g-object gdk-window))
  (data :pointer))

(defun gdk-window-user-data (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @syntax[]{(gdk-window-user-data window) => user-data}
  @syntax[]{(setf (gdk-window-user-data window) user-data)}
  @argument[window]{a @class{gdk-window} object}
  @argument[user-data]{a @code{:pointer} with the user data}
  @begin{short}
    The function @sym{gdk-window-user-data} retrieves the user data for
    @arg{window}, which is normally the widget that @arg{window} belongs to.
  @end{short}
  The function @sym{(setf gdk-window-user-data)} sets the user data.

  For most purposes this function is deprecated in favor of the fucntion
  @fun{g-object-data}. However, for historical reasons GTK+ stores the
  @class{gtk-widget} object that owns a @class{gdk-window} object as user data
  on the @class{gdk-window}. So, custom widget implementations should use this
  function for that. If GTK+ receives an event for a @class{gdk-window} object,
  and the user data for the window is non-@code{nil}, GTK+ will assume the user
  data is a @class{gtk-widget} object, and forward the event to that widget.
  @see-class{gdk-window}
  @see-class{gtk-widget}
  @see-function{g-object-data}"
  (with-foreign-object (data :pointer)
    (%gdk-window-user-data window data)
    (mem-ref data :pointer)))

(export 'gdk-window-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_override_redirect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_override_redirect" gdk-window-set-override-redirect)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[override-redirect]{@em{true} if @arg{window} should be override
    redirect}
  @begin{short}
    An override redirect window is not under the control of the window manager.
  @end{short}
  This means it will not have a titlebar, will not be minimizable, etc. - it
  will be entirely under the control of the application. The window manager
  cannot see the override redirect window at all.

  Override redirect should only be used for short-lived temporary windows,
  such as popup menus. @class{gtk-menu} uses an override redirect window in its
  implementation, for example.
  @see-class{gdk-window}
  @see-class{gtk-menu}"
  (window (g-object gdk-window))
  (override-redirect :boolean))

(export 'gdk-window-set-override-redirect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_accept_focus ()
;;; gdk_window_set_accept_focus () -> gdk-window-accept-focus
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-accept-focus) (accept-focus window)
  (foreign-funcall "gdk_window_set_accept_focus"
                   (g-object gdk-window) window
                   :boolean accept-focus
                   :void)
  accept-focus)

(defcfun ("gdk_window_get_accept_focus" gdk-window-accept-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @syntax[]{(gdk-window-accept-focus window) => accept-focus}
  @syntax[]{(setf (gdk-window-accept-focus window) accept-focus)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[accept-focus]{@em{true} if @arg{window} should receive input focus}
  @begin{short}
    Determines whether or not the desktop environment should be hinted that the
    window does not want to receive input focus.
  @end{short}
  Setting @arg{accept-focus} to @em{false} hints the desktop environment that
  the window does not want to receive input focus.

  On X, it is the responsibility of the window manager to interpret this hint.
  ICCCM-compliant window manager usually respect it.
  @see-class{gdk-window}
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_focus_on_map ()
;;; gdk_window_set_focus_on_map () -> gdk-window-focus-on-map
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-focus-on-map) (focus-on-map window)
  (foreign-funcall "gdk_window_set_focus_on_map"
                   (g-object gdk-window) window
                   :boolean focus-on-map
                   :void)
  focus-on-map)

(defcfun ("gdk_window_get_focus_on_map" gdk-window-focus-on-map) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @syntax[]{(gdk-window-focus-on-map window) => focus-on-map}
  @syntax[]{(setf gdk-window-focus-on-map window) focus-on-map)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[focus-on-map]{@em{true} if the window should receive input focus
    when mapped}
  @begin{short}
    Whether or not the window wants to receive input focus when it is
    mapped.
  @end{short}

  The @sym{gdk-window-focus-on-map} determines whether or not the desktop
  environment should be hinted that the window does not want to receive input
  focus when it is mapped.

  Setting @arg{focus-on-map} to @em{false} hints the desktop environment that
  the window does not want to receive input focus when it is mapped.
  @arg{focus-on-map} should be turned off for windows that are not triggered
  interactively, such as popups from network activity.

  On X, it is the responsibility of the window manager to interpret this hint.
  Window managers following the freedesktop.org window manager extension
  specification should respect it.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; GdkXEvent
;;;
;;; typedef void GdkXEvent;      /* Can be cast to window system specific
;;;
;;; Used to represent native events (XEvents for the X11 backend, MSGs for
;;; Win32).
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
;;; When a filter is called, event is unpopulated, except for event->window. The
;;; filter may translate the native event to a GDK event and store the result in
;;; event, or handle it without translation. If the filter translates the event
;;; and processing should continue, it should return GDK_FILTER_TRANSLATE.
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
;;; gdk_window_add_filter ()
;;;
;;; void gdk_window_add_filter (GdkWindow *window,
;;;                             GdkFilterFunc function,
;;;                             gpointer data);
;;;
;;; Adds an event filter to window, allowing you to intercept events before they
;;; reach GDK. This is a low-level operation and makes it easy to break GDK
;;; and/or GTK+, so you have to know what you're doing. Pass NULL for window to
;;; get all events for all windows, instead of events for a specific window.
;;;
;;; If you are interested in X GenericEvents, bear in mind that XGetEventData()
;;; has been already called on the event, and XFreeEventData() must not be
;;; called within function.
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
;;; gdk_window_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_shape_combine_region" gdk-window-shape-combine-region)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[shape-region]{region of type @symbol{cairo-region-t} of window to
    be non-transparent}
  @argument[offset-x]{a @code{:int} with the x position of @arg{shape-region}
    in window coordinates}
  @argument[offset-y]{a @code{:int} with the y position of @arg{shape-region}
    in window coordinates}
  @begin{short}
    Makes pixels in the window outside @arg{shape-region} be transparent, so
    that the window may be nonrectangular.
  @end{short}

  If @arg{shape-region} is @code{nil}, the shape will be unset, so the whole
  window will be opaque again. @arg{offset-x} and @arg{offset-y} are ignored if
  @arg{shape-region} is @code{nil}.

  On the X11 platform, this uses an X server extension which is widely
  available on most common platforms, but not available on very old X servers,
  and occasionally the implementation will be buggy. On servers without the
  shape extension, this function will do nothing.

  This function works on both toplevel and child windows.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (shape-region (:pointer (:struct cairo-region-t)))
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_shapes" gdk-window-set-child-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the shape mask of the window to the union of shape masks for all
    children of the window, ignoring the shape mask of the window itself.
  @end{short}
  Contrast with the function @fun{gdk-window-merge-child-shapes} function which
  includes the shape mask of the window in the masks to be merged.
  @see-class{gdk-window}
  @see-function{gdk-window-merge-child-shapes}"
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_shapes" gdk-window-merge-child-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Merges the shape masks for any child windows into the shape mask for the
    window.
  @end{short}
  I.e. the union of all masks for the window and its children will become the
  new mask for the window.
  See the function @fun{gdk-window-shape-combine-region}.

  This function is distinct from the function @fun{gdk-window-set-child-shapes}
  because it includes window's shape mask in the set of shapes to be merged.
  @see-class{gdk-window}
  @see-function{gdk-window-shape-combine-region}
  @see-function{gdk-window-set-child-shapes}"
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_input_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_input_shape_combine_region"
           gdk-window-input-shape-combine-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[shape-region]{region of type @symbol{cairo-region-t} of the window
    to be non-transparent}
  @argument[offset-x]{a @code{:int} with the x position of @arg{shape-region}
    in window coordinates}
  @argument[offset-y]{a @code{:int} with the y position of @arg{shape-region}
    in window coordinates}
  @begin{short}
    Like the function @fun{gdk-window-shape-combine-region}, but the shape
    applies only to event handling.
  @end{short}
  Mouse events which happen while the pointer position corresponds to an unset
  bit in the mask will be passed on the window below window.

  An input shape is typically used with RGBA windows. The alpha channel of the
  window defines which pixels are invisible and allows for nicely antialiased
  borders, and the input shape controls where the window is \"clickable\".

  On the X11 platform, this requires version 1.1 of the shape extension.

  On the Win32 platform, this functionality is not present and the function
  does nothing.
  @see-class{gdk-window}
  @see-function{gdk-window-shape-combine-region}"
  (window (g-object gdk-window))
  (shape-region (:pointer (:struct cairo-region-t)))
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-input-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_input_shapes" gdk-window-set-child-input-shapes)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the input shape mask of the window to the union of input shape masks
    for all children of window, ignoring the input shape mask of window itself.
  @end{short}
  Contrast with the function @fun{gdk-window-merge-child-input-shapes} which
  includes the input shape mask of the window in the masks to be merged.
  @see-class{gdk-window}
  @see-function{gdk-window-merge-child-input-shapes}"
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_input_shapes"
           gdk-window-merge-child-input-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Merges the input shape masks for any child windows into the input shape mask
    for @arg{window}.
  @end{short}
  I.e. the union of all input masks for the window and its children will become
  the new input mask for the window.
  See the function @fun{gdk-window-input-shape-combine-region}.

  This function is distinct from the function
  @fun{gdk-window-set-child-input-shapes} because it includes window's input
  shape mask in the set of shapes to be merged.
  @see-class{gdk-window}
  @see-function{gdk-window-input-shape-combine-region}
  @see-function{gdk-window-set-child-input-shapes}"
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_static_gravities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_static_gravities" gdk-window-set-static-gravities)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[use-static]{@em{true} to turn on static gravity}
  @return{@em{True} if the server supports static gravity.}
  @begin{short}
    Set the bit gravity of the given window to static, and flag it so all
    children get static subwindow gravity.
  @end{short}
  This is used if you are implementing scary features that involve deep
  knowledge of the windowing system. Do not worry about it unless you have to.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-set-static-gravities} has been deprecated since
    version 3.16 and should not be used in newly-written code. Static gravities
    have not worked on anything but X11 for a long time.
  @end{dictionary}
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (use-static :boolean))

(export 'gdk-window-set-static-gravities)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_title" gdk-window-set-title) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[title]{a string with the title of @arg{window}}
  @begin{short}
    Sets the title of a toplevel window, to be displayed in the titlebar.
  @end{short}
  If you have not explicitly set the icon name for the window, using the
  function @fun{gdk-window-set-icon-name}, the icon name will be set to title as
  well. @arg{title} must be in UTF-8 encoding, as with all user-readable strings
  in GDK/GTK+. @arg{title} may not be @code{nil}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-icon-name}"
  (window (g-object gdk-window))
  (title :string))

(export 'gdk-window-set-title)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background" gdk-window-set-background) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[color]{a @class{gdk-color} structure}
  @begin{short}
    Sets the background color of the window.
  @end{short}
  However, when using GTK+, set the background of a widget with the function
  @fun{gtk-widget-modify-bg}, if you are implementing an application,
  or the function @fun{gtk-style-context-set-background}, if you are
  implementing a custom widget.

  See also the function @fun{gdk-window-set-background-pattern}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-set-background} has been deprecated since
    version 3.4 and should not be used in newly written code. Use the function
    @fun{gdk-window-set-background-rgba} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gtk-widget-modify-bg}
  @see-function{gdk-window-set-background-rgba}
  @see-function{gtk-style-context-set-background}
  @see-function{gdk-window-background-pattern}"
  (window (g-object gdk-window))
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-window-set-background)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background_rgba" gdk-window-set-background-rgba) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[rgba]{a @class{gdk-rgba} color}
  @begin{short}
    Sets the background color of the window.
  @end{short}

  See also the function @fun{gdk-window-background-pattern}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-set-background-rgba} has been deprecated since
    version 3.22 and should not be used in newly-written code. Do not use this
    function.
  @end{dictionary}
  @see-class{gdk-window}
  @see-function{gdk-window-background-pattern}"
  (window (g-object gdk-window))
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-window-set-background-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_background_pattern ()
;;; gdk_window_set_background_pattern () -> gdk-window-background-pattern
;;; ----------------------------------------------------------------------------

;; TODO: Implement the type cairo-pattern-t

(defctype cairo-pattern-t :pointer)

(defun (setf gdk-window-background-pattern) (pattern window)
  (foreign-funcall "gdk_window_set_background_pattern"
                   (g-object gdk-window) window
                   cairo-pattern-t pattern
                   :void)
  pattern)

(defcfun ("gdk_window_get_background_pattern" gdk-window-background-pattern)
    cairo-pattern-t
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @syntax[]{(gdk-window-background-pattern window) => pattern}
  @syntax[]{(setf (gdk-window-background-pattern window) pattern)}
  @argument[window]{a @class{gdk-window} object}
  @argument[pattern]{a pattern of type @symbol{cairo-pattern-t} to use, or
    @code{nil}}
  @begin{short}
    The pattern to use for the background or @code{nil} to use the parent's
    background.
  @end{short}

  The function @sym{gdk-window-background-pattern} gets the pattern used to
  clear the background on the window. If the window does not have its own
  background and reuses the parent's, @code{nil} is returned and you will have
  to query it yourself. The function @sym{(setf gdk-window-background-pattern)}
  sets the background of the window.

  A background of @code{nil} means that the window will inherit its background
  form its parent window.

  The windowing system will normally fill a window with its background when
  the window is obscured then exposed.
  @begin[Warning]{dictionary}
    The function @sym{gdk-window-background-pattern} has been deprecated since
    version 3.22 and should not be used in newly-written code. Do not use
    this function.
  @end{dictionary}
  @see-class{gdk-window}
  @see-symbol{cairo-pattern-t}"
  (window (g-object gdk-window)))

(export 'gdk-window-background-pattern)

;;; ----------------------------------------------------------------------------
;;; GDK_PARENT_RELATIVE
;;; ----------------------------------------------------------------------------

;; We do not export this constant

(defconstant +gdk-parent-relative+ 1
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @variable-value{1}
  A special value, indicating that the background for a window should be
  inherited from the parent window.
  @see-class{gdk-window}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_geometry () -> gdk-window-geometry
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_geometry" %gdk-window-geometry) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gdk-window-geometry (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x}      -- a @code{:int} with the x coordinate of @arg{window},
                     relative to its parent @br{}
    @code{y}      -- a @code{:int} with the y coordinate of @arg{window},
                     relative to its parent @br{}
    @code{width}  -- a @code{:int} with the width of @arg{window} @br{}
    @code{height} -- a @code{:int} with the height of @arg{window}
  @end{return}
  @begin{short}
    The @arg{x} and @arg{y} coordinates returned are relative to the parent
    window of @arg{window}, which for toplevels usually means relative to the
    window decorations (titlebar, etc.) rather than relative to the root window
    (screen-size background window).
  @end{short}

  On the X11 platform, the geometry is obtained from the X server, so reflects
  the latest position of the window. This may be out-of-sync with the position
  of the window delivered in the most-recently-processed
  @class{gdk-event-configure} event. The function @fun{gdk-window-position} in
  contrast gets the position from the most recent configure event.
  @begin[Note]{dictionary}
    If @arg{window} is not a toplevel, it is much better to call the functions
    @fun{gdk-window-position}, @fun{gdk-window-width} and
    @fun{gdk-window-height} instead, because it avoids the roundtrip to the X
    server and because these functions support the full 32-bit coordinate space,
    whereas the function @sym{gdk-window-geometry} is restricted to the 16-bit
    coordinates of X11.
  @end{dictionary}
  @see-class{gdk-window}
  @see-class{gdk-event-configure}
  @see-function{gdk-window-position}
  @see-function{gdk-window-width}
  @see-function{gdk-window-height}"
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (%gdk-window-geometry window x y width height)
    (values (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gdk-window-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_geometry_hints ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_geometry_hints" gdk-window-set-geometry-hints) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-4-26}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[geometry]{geometry hints of type @symbol{gdk-geometry}}
  @argument[geometry-mask]{bitmask of type @symbol{gdk-window-hints} indicating
    fields of geometry to pay attention to}
  @begin{short}
    Sets the geometry hints for the window.
  @end{short}
  Hints flagged in @arg{geometry-mask} are set, hints not flagged in
  @arg{geometry-mask} are unset.

  This function provides hints to the windowing system about acceptable sizes
  for a toplevel window. The purpose of this is to constrain user resizing,
  but the windowing system will typically, but is not required to, also
  constrain the current size of the window to the provided values and
  constrain programatic resizing via the @fun{gdk-window-resize} or
  @fun{gdk-window-move-resize} functions.

  Note that on X11, this effect has no effect on windows of type @code{:temp}
  or windows where override redirect has been turned on via the
  @fun{gdk-window-set-override-redirect} function since these windows are not
  resizable by the user.

  Since you cannot count on the windowing system doing the constraints for
  programmatic resizes, you should generally call the
  @fun{gdk-window-constrain-size} function yourself to determine appropriate
  sizes.
  @see-class{gdk-window}
  @see-function{gdk-window-resize}
  @see-function{gdk-window-move-resize}
  @see-function{gdk-window-set-override-redirect}
  @see-function{gdk-window-constrain-size}"
  (window (g-object gdk-window))
  (geometry (:pointer (:struct gdk-geometry)))
  (geometry-mask gdk-window-hints))

(export 'gdk-window-set-geometry-hints)

;;; -------------------------------------------------------------_---------------
;;; gdk_window_get_width () -> gdk-window-width
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_width" gdk-window-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[window]{a @class{gdk-window} object}
  @return{A @code{:int} with the width of @arg{window}.}
  @begin{short}
    Returns the width of the given window.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.
  @see-class{gdk-window}
  @see-function{gdk-window-height}
  @see-function{gdk-window-position}"
  (window (g-object gdk-window)))

(export 'gdk-window-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_height () -> gdk-window-height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_height" gdk-window-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[window]{a @class{gdk-window} object}
  @return{A @code{:int} with the height of @arg{window}.}
  @begin{short}
    Returns the height of the given window.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.
  @see-class{gdk-window}
  @see-function{gdk-window-width}
  @see-function{gdk-window-position}"
  (window (g-object gdk-window)))

(export 'gdk-window-height)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_list" gdk-window-set-icon-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{the @class{gdk-window} toplevel window to set the icon of}
  @argument[pixbufs]{a list of @class{gdk-pixbuf} objects, of different sizes}
  @begin{short}
    Sets a list of icons for the window.
  @end{short}
  One of these will be used to represent the window when it has been iconified.
  The icon is usually shown in an icon box or some sort of task bar. Which icon
  size is shown depends on the window manager. The window manager can scale the
  icon but setting several size icons can give better image quality since the
  window manager may only need to scale the icon by a small amount or not at
  all.
  @see-class{gdk-window}
  @see-class{gdk-pixbuf}"
  (window (g-object gdk-window))
  (pixbufs (g-list (g-object gdk-pixbuf))))

(export 'gdk-window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_modal_hint ()
;;; gdk_window_set_modal_hint () -> gdk-window-modal-hint
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-modal-hint) (modal-hint window)
  (foreign-funcall "gdk_window_set_modal_hint"
                   (g-object gdk-window) window
                   :boolean modal-hint
                   :void)
  modal-hint)

(defcfun ("gdk_window_get_modal_hint" gdk-window-modal-hint) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @syntax[]{(gdk-window-modal-hint window) => modal}
  @syntax[]{(setf (gdk-window-modal-hint window) modal)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[modal]{@em{true} if the window is modal, @em{false} otherwise}
  @begin{short}
    Whether or not the window has the modal hint set.
  @end{short}

  The function @sym{gdk-window-modal-hint} determines whether or not the window
  manager is hinted that window has modal behaviour.

  The application can use this hint to tell the window manager that a certain
  window has modal behaviour. The window manager can use this information to
  handle modal windows in a special way.

  You should only use this on windows for which you have previously called
  the function @fun{gdk-window-set-transient-for}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-transient-for}"
  (window (g-object gdk-window)))

(export 'gdk-window-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type_hint ()
;;; gdk_window_set_type_hint () -> gdk-window-type-hint
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-type-hint) (hint window)
  (foreign-funcall "gdk_window_set_type-hint"
                   (g-object gdk-window) window
                   gdk-window-type-hint hint
                   :void)
  hint)

(defcfun ("gdk_window_get_type_hint" gdk-window-type-hint)
    gdk-window-type-hint
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @syntax[]{(gdk-window-type-hint window) => hint}
  @syntax[]{(setf (gdk-window-type-hint window) hint)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[hint]{a hint of type @symbol{gdk-window-type-hint} this window will
    have}
  @begin{short}
    The type hint set for the window.
  @end{short}

  The function @sym{gdk-window-type-hint} returns the type hint set for a
  the window. The function @sym{(setf gdk-window-type-hint)} sets the type hint.

  The application can use this call to provide a hint to the window manager
  about the functionality of a window. The window manager can use this
  information when determining the decoration and behaviour of the window.

  The hint must be set before the window is mapped.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_shadow_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_shadow_width" gdk-window-set-shadow-width) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[left]{a @code{:int} with the left extent}
  @argument[right]{a @code{:int} with the right extent}
  @argument[top]{a @code{:int} with the top extent}
  @argument[bottom]{a @code{:int} with the bottom extent}
  @begin{short}
    Newer GTK+ windows using client-side decorations use extra geometry around
    their frames for effects like shadows and invisible borders.
  @end{short}
  Window managers that want to maximize windows or snap to edges need to know
  where the extents of the actual frame lie, so that users do not feel like
  windows are snapping against random invisible edges.

  Note that this property is automatically updated by GTK+, so this function
  should only be used by applications which do not use GTK+ to create toplevel
  windows.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (left :int)
  (right :int)
  (top :int)
  (bottom :int))

(export 'gdk-window-set-shadow-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_taskbar_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_taskbar_hint" gdk-window-set-skip-taskbar-hint)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[skips-taskbar]{@em{true} to skip the taskbar}
  @begin{short}
    Toggles whether a window should appear in a task list or window list.
  @end{short}
  If a window's semantic type as specified with the function
  @fun{gdk-window-type-hint} already fully describes the window, this function
  should not be called in addition, instead you should allow the window to be
  treated according to standard policy for its semantic type.
  @see-class{gdk-window}
  @see-function{gdk-window-type-hint}"
  (window (g-object gdk-window))
  (skips-taskbar :boolean))

(export 'gdk-window-set-skip-taskbar-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_pager_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_pager_hint" gdk-window-set-skip-pager-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[skips-pager]{@em{true} to skip the pager}
  @begin{short}
    Toggles whether a window should appear in a pager, workspace switcher, or
    other desktop utility program that displays a small thumbnail representation
    of the windows on the desktop.
  @end{short}
  If a window's semantic type as specified with the function
  @fun{gdk-window-type-hint} already fully describes the window, this function
  should not be called in addition, instead you should allow the window to be
  treated according to standard policy for its semantic type.
  @see-class{gdk-window}
  @see-function{gdk-window-type-hint}"
  (window (g-object gdk-window))
  (skips-pager :boolean))

(export 'gdk-window-set-skip-pager-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_urgency_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_urgency_hint" gdk-window-set-urgency-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[urgent]{@em{true} if the window is urgent}
  @begin{short}
    Toggles whether a window needs the user's urgent attention.
  @end{short}
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (urgent :boolean))

(export 'gdk-window-set-urgency-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_position () -> gdk-window-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_position" %gdk-window-position) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-position (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x} -- a @code{:int} with the x coordinate of @arg{window} @br{}
    @code{y} -- a @code{:int} with the y coordinate of @arg{window}
  @end{return}
  @begin{short}
    Obtains the position of the window as reported in the
    most-recently-processed @class{gdk-event-configure} event.
  @end{short}
  Contrast with the function @fun{gdk-window-geometry} which queries the X
  server for the current window position, regardless of which events have been
  received or processed. The position coordinates are relative to the window's
  parent window.
  @see-class{gdk-window}
  @see-class{gdk-event-configure}
  @see-function{gdk-window-width}
  @see-function{gdk-window-height}
  @see-function{gdk-window-geometry}"
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-position window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_origin () -> gdk-window-root-origin
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_origin" %gdk-window-root-origin) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-root-origin (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{return}
    @code{x} -- a @code{:int} with the x position of window frame @br{}
    @code{y} -- a @code{:int} with the y position of window frame
  @end{return}
  @begin{short}
    Obtains the top-left corner of the window manager frame in root window
    coordinates.
  @end{short}
  @see-class{gdk-window}"
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-root-origin window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-root-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_extents () -> gdk-window-frame-extents
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_extents" %gdk-window-frame-extents) :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gdk-window-frame-extents (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{return}
    @code{rect} -- a @class{gdk-rectangle} with bounding box of the window frame
  @end{return}
  @begin{short}
    Obtains the bounding box of the window, including window manager
    titlebar/borders if any.
  @end{short}
  The frame position is given in root window coordinates. To get the position
  of the window itself, rather than the frame, in root window coordinates, use
  the function @fun{gdk-window-origin}.
  @see-class{gdk-window}
  @see-function{gdk-window-origin}"
  (let ((rectangle (make-gdk-rectangle)))
    (%gdk-window-frame-extents window rectangle)
    rectangle))

(export 'gdk-window-frame-extents)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_origin () -> gdk-window-origin
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_origin" %gdk-window-origin) :int
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-origin (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x} -- a @code{:int} with the x coordinate @br{}
    @code{y} -- a @code{:int} with the y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window in root window coordinates.
  @end{short}
  Compare with the functions @fun{gdk-window-position} and
  @fun{gdk-window-geometry} which return the position of a window relative
  to its parent window.
  @see-class{gdk-window}
  @see-function{gdk-window-position}
  @see-function{gdk-window-geometry}"
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-origin window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_coords () -> gdk-window-root-coords
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_coords" %gdk-window-root-coords) :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (root-x :int)
  (root-y :int))

(defun gdk-window-root-coords (window x y)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{a @code{:int} with the x coordinate in window}
  @argument[y]{a @code{:int} with the y coordinate in window}
  @begin{return}
    @code{root-x} -- a @code{:int} with the x coordinate @br{}
    @code{root-y} -- a @code{:int} with the y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window position in root window coordinates.
  @end{short}
  This is similar to the function @fun{gdk-window-origin} but allows you go
  pass in any position in the window, not just the origin.
  @see-class{gdk-window}
  @see-function{gdk-window-origin}"
  (with-foreign-objects ((root-x :int) (root-y :int))
    (%gdk-window-root-coords window x y root-x root-y)
    (values (mem-ref root-x :int)
            (mem-ref root-y :int))))

(export 'gdk-window-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pointer () -> gdk-window-pointer
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_pointer" %gdk-window-pointer)
    (g-object gdk-window)
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-pointer (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-9}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{win} -- the window containing the pointer as with the function
    @fun{gdk-window-at-pointer}, or @code{nil} if the window containing the
    pointer is not known to GDK @br{}
    @code{x} -- an integer with the x coordinate of pointer @br{}
    @code{y} -- an integer with the y coordinate of pointer @br{}
    @code{mask} -- modifier mask of type @symbol{gdk-modifier-type}
  @end{return}
  @begin{short}
    Obtains the current pointer position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk-window-pointer} function has been deprecated since version
    3.0 and should not be used in newly written code. Use the function
    @fun{gdk-window-device-position} instead.
  @end{dictionary}
  @see-class{gdk-window}
  @see-symbol{gdk-modifier-type}
  @see-function{gdk-window-at-pointer}
  @see-function{gdk-window-device-position}"
  (with-foreign-objects ((x :int) (y :int) (mask 'gdk-modifier-type))
    (let ((window (%gdk-window-pointer window x y mask)))
      (values window
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_position () -> gdk-window-device-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_position" %gdk-window-device-position)
    (g-object gdk-window)
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-device-position (window device)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a @class{gdk-device} to query}
  @begin{return}
    @code{win}  -- the @class{gdk-window} object underneath @arg{device}, as
                   with the function @fun{gdk-device-window-at-position}, or
                   @code{nil} if the window is not known to GDK @br{}
    @code{x}    -- a @code{:int} with the x coordinate of the device @br{}
    @code{y}    -- a @code{:int} with the y coordinate of the device @br{}
    @code{mask} -- the modifier mask of type @symbol{gdk-modifier-type}
  @end{return}
  @begin{short}
    Obtains the current device position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}
  Use the @fun{gdk-window-device-position-double} function if you need
  subpixel precision.
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-function{gdk-device-window-at-position}
  @see-function{gdk-window-device-position-double}"
  (with-foreign-objects ((x :int) (y :int) (mask 'gdk-modifier-type))
    (let ((window (%gdk-window-device-position window device x y mask)))
      (values window
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_position_double ()
;;; -> gdk-window-device-position-double
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_position_double"
          %gdk-window-device-position-double) (g-object gdk-window)
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (x (:pointer :double))
  (y (:pointer :double))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-device-position-double (window device)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a @class{gdk-device} object to query to}
  @begin{return}
    @code{win}  -- the @class{gdk-window} object underneath device, as with the
                   function @fun{gdk-device-window-at-position}, or
                   @code{nil} if the window is not known to GDK @br{}
    @code{x}    -- a @code{:double} with the x coordinate of the device @br{}
    @code{y}    -- a @code{:double} with the y coordinate of the device  @br{}
    @code{mask} -- the flags of type @symbol{gdk-modifer-type}
  @end{return}
  @begin{short}
    Obtains the current device position in doubles and the modifier state.
  @end{short}
  The position is given in coordinates relative to the upper left corner of
  the window.
  @see-class{gdk-window}
  @see-function{gdk-window-device-position}"
  (with-foreign-objects ((x :double) (y :double) (mask 'gdk-modifier-type))
    (let ((window (%gdk-window-device-position-double window device x y mask)))
      (values window
              (mem-ref x :double)
              (mem-ref y :double)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-device-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_parent () -> gdk-window-parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_parent" gdk-window-parent) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @return{parent @class{gdk-window} object of @arg{window}}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Does not query the X server. Thus this returns the parent as passed to the
  function @fun{gdk-window-new}, not the actual parent. This should never
  matter unless you are using Xlib calls mixed with GDK calls on the X11
  platform. It may also matter for toplevel windows, because the window manager
  may choose to reparent them.

  Note that you should use the function @fun{gdk-window-effective-parent} when
  writing generic code that walks up a window hierarchy, because the function
  @sym{gdk-window-parent} will most likely not do what you expect if there are
  offscreen windows in the hierarchy.
  @see-class{gdk-window}
  @see-function{gdk-window-new}
  @see-function{gdk-window-children}
  @see-function{gdk-window-effective-parent}"
  (window (g-object gdk-window)))

(export 'gdk-window-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevel () -> gdk-window-toplevel
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_toplevel" gdk-window-toplevel) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{The @class{gdk-window} toplevel window containing @arg{window}.}
  @begin{short}
    Gets the toplevel window that is an ancestor of the window.
  @end{short}

  Any window type but @code{:child} is considered a toplevel window, as is
  a @code{:child} window that has a root window as parent.

  Note that you should use the function @fun{gdk-window-effective-toplevel}
  when you want to get to a window's toplevel as seen on screen, because
  the function @sym{gdk-window-toplevel} will most likely not do what you
  expect if there are offscreen windows in the hierarchy.
  @see-class{gdk-window}
  @see-function{gdk-window-effective-toplevel}"
  (window (g-object gdk-window)))

(export 'gdk-window-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children () -> gdk-window-children
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children" gdk-window-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{List of @class{gdk-window} child windows inside @arg{window}.}
  @begin{short}
    Gets the list of children of the window known to GDK.
  @end{short}
  This function only returns children created via GDK, so for example it is
  useless when used with the root window. It only returns windows an application
  created itself.
  @see-class{gdk-window}
  @see-function{gdk-window-parent}"
  (window (g-object gdk-window)))

(export 'gdk-window-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children_with_user_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children_with_user_data"
           gdk-window-children-with-user-data) (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-24}
  @argument[window]{a @class{gdk-window} object}
  @argument[user-data]{user data to look for as a :pointer}
  @return{list of child windows inside @arg{window}}
  @begin{short}
    Gets the list of children of the window known to GDK with a particular
    @arg{user-data} set on it.
  @end{short}

  The returned list must be freed, but the elements in the list need not be.

  The list is returned in (relative) stacking order, i.e. the lowest window
  is first.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (user-data :pointer))

(export 'gdk-window-children-with-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_peek_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_peek_children" gdk-window-peek-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{A list of @class{gdk-window} child windows in @arg{window}.}
  @begin{short}
    Like the function @fun{gdk-window-children}, but does not copy the list
    of children, so the list does not need to be freed.
  @end{short}
  @see-class{gdk-window}
  @see-function{gdk-window-children}"
  (window (g-object gdk-window)))

(export 'gdk-window-peek-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_events ()
;;; gdk_window_set_events () -> gdk-window-events
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-events) (event-mask window)
  (foreign-funcall "gdk_window_set_events"
                   (g-object gdk-window) window
                   gdk-event-mask event-mask
                   :void)
  event-mask)

(defcfun ("gdk_window_get_events" gdk-window-events) gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2020-9-6}
  @argument[window]{a @class{gdk-window} object}
  @argument[event-mask]{event mask of type @symbol{gdk-event-mask} for
    @arg{window}}
  @begin{short}
    Accessor of the event mask for the window.
  @end{short}

  The function @sym{gdk-window-events} gets the event mask for the window for
  all master input devices. The function @sym{(setf gdk-window-events)} sets
  the event mask.

  The event mask for a window determines which events will be reported for that
  window from all master input devices. For example, an event mask including
  @code{:button-press-mask} means the window should report button press events.
  The event mask is the bitwise OR of values from the @symbol{gdk-event-mask}
  flags.
  @see-class{gdk-window}
  @see-symbol{gdk-event-mask}"
  (window (g-object gdk-window)))

(export 'gdk-window-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_name" gdk-window-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[name]{a strinng with the name of @arg{window} while iconified
    (minimized)}
  @begin{short}
    Windows may have a name used while minimized, distinct from the name they
    display in their titlebar.
  @end{short}
  Most of the time this is a bad idea from a user interface standpoint. But you
  can set such a name with this function, if you like.

  After calling this with a non-@code{nil} name, calls to the function
  @fun{gdk-window-set-title} will not update the icon title.

  Using @code{nil} for name unsets the icon title; further calls to the function
  @fun{gdk-window-set-title} will again update the icon title as well.
  @see-class{gdk-window}
  @see-function{gdk-window-set-title}"
  (window (g-object gdk-window))
  (name :string))

(export 'gdk-window-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_transient_for ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_transient_for" gdk-window-set-transient-for) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[parent]{another toplevel @class{gdk-window} object}
  @begin{short}
    Indicates to the window manager that @arg{window} is a transient dialog
    associated with the application window parent.
  @end{short}
  This allows the window manager to do things like center @arg{window} on
  @arg{parent} and keep @arg{window} above @arg{parent}.

  See the function @fun{gtk-window-transient-for} if you are using
  @class{gtk-window} or @class{gtk-dialog} widgets.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-class{gtk-dialog}
  @see-function{gtk-window-transient-for}"
  (window (g-object gdk-window))
  (parent (g-object gdk-window)))

(export 'gdk-window-set-transient-for)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_role ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_role" gdk-window-set-role) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[role]{a string indicating its role}
  @begin{short}
    When using GTK+, typically you should use the function @fun{gtk-window-role}
    instead of this low-level function.
  @end{short}

  The window manager and session manager use a window's role to distinguish it
  from other kinds of window in the same application. When an application is
  restarted after being saved in a previous session, all windows with the same
  title and role are treated as interchangeable. So if you have two windows
  with the same title that should be distinguished for session management
  purposes, you should set the role on those windows. It does not matter what
  string you use for the role, as long as you have a different role for each
  non-interchangeable kind of window.
  @see-class{gdk-window}
  @see-function{gtk-window-role}"
  (window (g-object gdk-window))
  (role :string))

(export 'gdk-window-set-role)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_startup_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_startup_id" gdk-window-set-startup-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-8-17}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[startup-id]{a string with startup-notification identifier}
  @begin{short}
    When using GTK+, typically you should use the function
    @fun{gtk-window-startup-id} instead of this low-level function.
  @end{short}
  @see-class{gdk-window}
  @see-function{gtk-window-startup-id}"
  (window (g-object gdk-window))
  (startup-id :string))

(export 'gdk-window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_group ()
;;; gdk_window_set_group () -> gdk-window-group
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-group) (leader window)
  (foreign-funcall "gdk_window_set_group"
                   (g-object gdk-window) window
                   (g-object gdk-window) leader
                   :void)
  leader)

(defcfun ("gdk_window_get_group" gdk-window-group) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{(gdk-window-group window) => leader}
  @syntax[]{(setf (gdk-window-group window) leader)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[leader]{group leader @class{gdk-window} object, or @code{nil} to
    restore the default group leader window}
  @begin{short}
    The group leader window for @arg{window}.
  @end{short}

  The function @sym{gdk-window-group} returns the group leader window for
  @arg{window}. The function @sym{(setf gdk-window-group)} sets the group leader
  window for @arg{window}.

  By default, GDK sets the group leader for all toplevel windows to a global
  window implicitly created by GDK. With this function you can override this
  default.

  The group leader window allows the window manager to distinguish all windows
  that belong to a single application. It may for example allow users to
  minimize/unminimize all windows belonging to an application at once. You
  should only set a non-default group window if your application pretends to
  be multiple applications.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_decorations ()
;;; gdk_window_set_decorations () -> gdk-window-decorations
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-decorations) (decorations window)
  (foreign-funcall "gdk_window_set_decorations"
                   (g-object gdk-window) window
                   gdk-wm-decoration decorations
                   :void)
  decorations)

(defcfun ("gdk_window_get_decorations" %gdk-window-decorations) :boolean
  (window (g-object gdk-window))
  (decorations (:pointer gdk-wm-decoration)))

(defun gdk-window-decorations (window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{(gdk-window-decorations window) => decorations}
  @syntax[]{(setf (gdk-window-decorations window) decorations)}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[decorations]{decoration hint mask of type
    @symbol{gdk-wm-decoration}}
  @begin{short}
    \"Decorations\" are the features the window manager adds to a toplevel
    @class{gdk-window}.
  @end{short}
  This function sets the traditional Motif window manager hints that tell the
  window manager which decorations you would like your window to have. Usually
  you should use the function @fun{gtk-window-decorated} on a @class{gtk-window}
  widget instead of using the GDK function directly.

  The decorations argument is the logical OR of the values of the
  @symbol{gdk-wm-decoration} flags. If @code{:all} is included in the mask, the
  other bits indicate which decorations should be turned off. If @code{:all}
  is not included, then the other bits indicate which decorations should be
  turned on.

  Most window managers honor a decorations hint of 0 to disable all
  decorations, but very few honor all possible combinations of bits.
  @see-class{gdk-window}
  @see-symbol{gdk-wm-decoration}
  @see-class{gtk-window}
  @see-function{gtk-window-decorated}"
  (with-foreign-object (decorations 'gdk-wm-decoration)
    (when (%gdk-window-decorations window decorations)
      (mem-ref decorations 'gdk-wm-decoration))))

(export 'gdk-window-decorations)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_functions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_functions" gdk-window-set-functions) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[functions]{bitmask of operations of type @symbol{gdk-wm-function}
    to allow on @arg{window}}
  @begin{short}
    Sets hints about the window management functions to make available via
    buttons on the window frame.
  @end{short}

  On the X backend, this function sets the traditional Motif window manager
  hint for this purpose. However, few window managers do anything reliable or
  interesting with this hint. Many ignore it entirely.

  The functions argument is the logical OR of values from the
  @symbol{gdk-wm-function} flags. If the bitmask includes @code{:all}, then the
  other bits indicate which functions to disable; if it does not include
  @code{:all}, it indicates which functions to enable.
  @see-class{gdk-window}
  @see-symbol{gdk-wm-function}"
  (window (g-object gdk-window))
  (functions gdk-wm-function))

(export 'gdk-window-set-functions)

;;; ----------------------------------------------------------------------------
;;; gdk_get_default_root_window () -> gdk-default-root-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_default_root_window" gdk-default-root-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @return{The @class{gdk-window} default root window.}
  @begin{short}
    Obtains the root window, parent all other windows are inside, for the
    default display and screen.
  @end{short}
  @see-class{gdk-window}")

(export 'gdk-default-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_support_multidevice ()
;;; gdk_window_set_support_multidevice () -> gdk-window-support-multidevice
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-support-multidevice) (support-multidevice window)
  (foreign-funcall "gdk_window_set_support_multidevice"
                   (g-object gdk-window) window
                   :boolean support-multidevice
                   :void)
  support-multidevice)

(defcfun ("gdk_window_get_support_multidevice" gdk-window-support-multidevice)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-18}
  @syntax[]{(gdk-window-support-multidevice window) => support-multidevice}
  @syntax[]{(setf (gdk-window-support-multidevice window) support-multidevice)}
  @argument[window]{a @class{gdk-window} object}
  @argument[support-multidevice]{@em{true} to enable multidevice support in
    @arg{window}}
  @begin{short}
    The function @sym{gdk-window-support-multidevice} returns @em{true} if the
    window is aware of the existence of multiple devices.
  @end{short}
  The function @sym{(setf gdk-window-support-multidevice)} will enable
  multidevice features in @arg{window}.

  Multidevice aware windows will need to handle properly multiple, per device
  enter/leave events, device grabs and grab ownerships.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_cursor ()
;;; gdk_window_set_device_cursor () -> gdk-window-device-cursor
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-device-cursor) (cursor window device)
  (foreign-funcall "gdk_window_set_device_cursor"
                   (g-object gdk-window) window
                   (g-object gdk-device) device
                   (g-object gdk-cursor) cursor
                   :void)
  cursor)

(defcfun ("gdk_window_get_device_cursor" gdk-window-device-cursor)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{(gdk-window-device-cursor window device) => cursor}
  @syntax[]{(setf (gdk-window-device-cursor window device) cursor)}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a master @class{gdk-device} object}
  @argument[cursor]{a @class{gdk-cursor} object}
  @return{ A @class{gdk-cursor} object, or @code{nil}.}
  @begin{short}
    The function @sym{gdk-window-device-cursor} retrieves a @class{gdk-cursor}
    pointer for the device currently set on the specified @class{gdk-window}
    object, or @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified window, and it is using the cursor for its parent window.

  The function @sym{(setf gdk-window-device-cursor)} sets a specific
  @class{gdk-cursor} object for a given device when it gets inside window.
  Use the functions @fun{gdk-cursor-new-for-display} or
  @fun{gdk-cursor-new-from-pixbuf} to create the cursor. To make the cursor
  invisible, use @code{:blank-cursor}. Passing @code{nil} for the cursor
  argument to the function @fun{gdk-window-cursor} means that the window will
  use the cursor of its parent window. Most windows should use this default.

  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-class{gdk-cursor}
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-new-from-pixbuf}"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-device-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_events ()
;;; gdk_window_set_device_events () -> gdk-window-device-events
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-device-events) (event-mask window device)
  (foreign-funcall "gdk_window_set_device_events"
                   (g-object gdk-window) window
                   (g-object gdk-device) device
                   gdk-event-mask event-mask
                   :void)
  event-mask)

(defcfun ("gdk_window_get_device_events" gdk-window-device-events)
    gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a @class{gdk-device} object}
  @argument[event-mask]{event mask of type @symbol{gdk-event-mask} for
    @arg{window}}
  @begin{short}
    The function @sym{gdk-window-device-events} returns the event mask for
    the window corresponding to an specific device.
  @end{short}
  The function @sym{(setf gdk-window-device-events)} sets the event mask for a
  given device.

  Normally a floating device, not attached to any visible pointer to the window.
  For example, an event mask including @code{:button-press-mask} means the
  window should report button press events. The event mask is the bitwise OR of
  values from the @symbol{gdk-event-mask} flags.
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-symbol{gdk-event-mask}"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-device-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_source_events ()
;;; gdk_window_set_source_events () -> gdk-window-source-events
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-source-events) (event-mask window source)
  (foreign-funcall "gdk_window_set_source_events"
                   (g-object gdk-window) window
                   gdk-input-source source
                   gdk-event-mask event-mask
                   :void)
  event-mask)

(defcfun ("gdk_window_get_source_events" gdk-window-source-events)
    gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{(gdk-window-source-events window source) => event-mask}
  @syntax[]{(setf (gdk-window-source-events window source) event-mask)}
  @argument[window]{a @class{gdk-window} object}
  @argument[source]{a @symbol{gdk-input-source} to define the source class}
  @argument[event-mask]{event mask of type @symbol{gdk-event-mask} for
    @arg{window}}
  @begin{short}
    The function @sym{gdk-window-source-events} returns the event mask for
    the window corresponding to the device class specified by @arg{source}.
  @end{short}
  The function @sym{(setf gdk-window-source-events)} sets the event mask for
  any floating device, i.e. not attached to any visible pointer, that has
  @arg{source} defined as source.

  This event mask will be applied both to currently existing, newly added
  devices after this call, and devices being attached/detached.
  @see-class{gdk-window}
  @see-symbol{gdk-input-source}
  @see-symbol{gdk-event-mask}"
  (window (g-object gdk-window))
  (source gdk-input-source))

(export 'gdk-window-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_event_compression ()
;;; gdk_window_set_event_compression () -> gdk-window-event-compression
;;; ----------------------------------------------------------------------------

(defun (setf gdk-window-event-compression) (event-compression window)
  (foreign-funcall "gdk_window_set_event_compression"
                   (g-object gdk-window) window
                   :boolean event-compression
                   :void)
  event-compression)

(defcfun ("gdk_window_get_event_compression" gdk-window-event-compression)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{(gdk-window-event-compression window) => event-compression}
  @syntax[]{(setf (gdk-window-event-compression window) event-compression)}
  @argument[window]{a @class{gdk-window} object}
  @argument[event-compression]{a @em{true} if motion events will be compressed}
  @begin{short}
    Determines whether or not extra unprocessed motion events in the event
    queue can be discarded.
  @end{short}
  If @em{true} only the most recent event will be delivered.

  Some types of applications, e.g. paint programs, need to see all motion
  events and will benefit from turning off event compression. By default, event
  compression is enabled.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-event-compression)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_surface () -> gdk-offscreen-window-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_surface" gdk-offscreen-window-surface)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a @class{gdk-window} object}
  @return{The offscreen @symbol{cairo-surface-t} instance, or @code{nil} if
    not offscreen.}
  @begin{short}
    Gets the offscreen surface that an offscreen window renders into.
  @end{short}
  If you need to keep this around over window resizes, you need to add a
  reference to it.
  @see-class{gdk-window}
  @see-symbol{cairo-surface-t}"
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_embedder ()
;;; gdk_offscreen_window_set_embedder () -> gdk-offscreen-window-embedder
;;; ----------------------------------------------------------------------------

(defun (setf gdk-offscreen-window-embedder) (embedder window)
  (foreign-funcall "gdk_offscreen_window_set_embedder"
                   (g-object gdk-window) window
                   (g-object gdk-window) embedder
                   :void)
  embedder)

(defcfun ("gdk_offscreen_window_get_embedder" gdk-offscreen-window-embedder)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @syntax[]{gdk-offscreen-window-embedder window) => embedder}
  @syntax[]{(setf gdk-offscreen-window-embedded window) embedder)}
  @argument[window]{a @class{gdk-window} object}
  @argument[embedder]{the @class{gdk-window} that window gets embedded in}
  @begin{short}
    The embedding @class{gdk-window} object, or @code{nil} if @arg{window} is
    not an embedded offscreen window.
  @end{short}

  The function @sym{gdk-offscreen-window-embedder} gets the window that
  @arg{window} is embedded in. The function
  @sym{(setf gdk-offscreen-window-embedder)} sets @arg{window} to be embedded
  in @arg{embedder}.

  To fully embed an offscreen window, in addition to calling this function, it
  is also necessary to handle the \"pick-embedded-child\" signal on
  @arg{embedder} and the \"to-embedder\" and \"from-embedder\" signals on
  @arg{window}.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_window_geometry_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_geometry_changed" gdk-window-geometry-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{an embedded offscreen @class{gdk-window} object}
  @begin{short}
    This function informs GDK that the geometry of an embedded offscreen window
    has changed.
  @end{short}
  This is necessary for GDK to keep track of which offscreen window the pointer
  is in.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-geometry-changed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_from_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_from_parent" %gdk-window-coords-from-parent) :void
  (window (g-object gdk-window))
  (parent-x :double)
  (parent-y :double)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gdk-window-coords-from-parent (window parent-x parent-y)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a @class{gdk-window} child window}
  @argument[parent-x]{a number with the x coordinate in parent's coordinate
    system}
  @argument[parent-y]{a number with the y coordinate in parent's coordinate
    system}
  @begin{return}
    @code{x} -- a @code{:double} with the x coordinate in child's coordinate
                system @br{}
    @code{y} -- a @code{:double} with the y coordinate in child's coordinate
                system
  @end{return}
  @begin{short}
    Transforms window coordinates from a parent window to a child window, where
    the parent window is the normal parent as returned by the function
    @fun{gdk-window-parent} for normal windows, and the window's embedder as
    returned by the function @fun{gdk-offscreen-window-embedder} for offscreen
    windows.
  @end{short}

  For normal windows, calling this function is equivalent to subtracting the
  return values of the function @fun{gdk-window-position} from the parent
  coordinates. For offscreen windows however, which can be arbitrarily
  transformed, this function calls the \"from-embedder\" signal to translate
  the coordinates.

  You should always use this function when writing generic code that walks
  down a window hierarchy.

  See also the function @fun{gdk-window-coords-to-parent}.
  @see-function{gdk-window}
  @see-function{gdk-window-parent}
  @see-function{gdk-window-position}
  @see-function{gdk-offscreen-window-embedder}
  @see-function{gdk-window-coords-to-parent}"
  (with-foreign-objects ((x :double) (y :double))
    (%gdk-window-coords-from-parent window
                                    (coerce parent-x 'double-float)
                                    (coerce parent-y 'double-float)
                                    x
                                    y)
    (values (mem-ref x :double)
            (mem-ref y :double))))

(export 'gdk-window-coords-from-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_to_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_to_parent" %gdk-window-coords-to-parent) :void
  (window (g-object gdk-window))
  (x :double)
  (y :double)
  (parent-x (:pointer :double))
  (parent-y (:pointer :double)))

(defun gdk-window-coords-to-parent (window x y)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a @class{gdk-window} child window}
  @argument[x]{a number with the x coordinate in child's coordinate system}
  @argument[y]{a number with the y coordinate in child's coordinate system}
  @begin{return}
    @code{parent-x} -- a @code{:double} with the x coordinate in parent's
                       coordinate system, or @code{nil} @br{}
    @code{parent-y} -- a @code{:double} with the y coordinate in parent's
                       coordinate system, or @code{nil}
  @end{return}
  @begin{short}
    Transforms window coordinates from a child window to its parent window,
    where the parent window is the normal parent as returned by the function
    @fun{gdk-window-parent} for normal windows, and the window's embedder
    as returned by the function @fun{gdk-offscreen-window-embedder} for
    offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to adding the return
  values of the function @fun{gdk-window-position} to the child coordinates.
  For offscreen windows however, which can be arbitrarily transformed, this
  function calls the \"to-embedder\" signal to translate the coordinates.

  You should always use this function when writing generic code that walks up
  a window hierarchy.

  See also the function @fun{gdk-window-coords-from-parent}.
  @see-class{gdk-window}
  @see-function{gdk-window-parent}
  @see-function{gdk-window-position}
  @see-function{gdk-offscreen-window-embedder}
  @see-function{gdk-window-coords-from-parent}"
  (with-foreign-objects ((parent-x :double) (parent-y :double))
    (%gdk-window-coords-to-parent window
                                  (coerce x 'double-float)
                                  (coerce y 'double-float)
                                  parent-x
                                  parent-y)
    (values (mem-ref parent-x :double)
            (mem-ref parent-y :double))))

(export 'gdk-window-coords-to-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_parent () -> gdk-window-effective-parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_parent" gdk-window-effective-parent)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-25}
  @argument[window]{a @class{gdk-window} object}
  @return{Effective @class{gdk-window} parent of @arg{window}.}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Works like the function @fun{gdk-window-parent} for normal windows, but
  returns the window's embedder for offscreen windows.

  See also the function @fun{gdk-offscreen-window-embedder}.
  @see-class{gdk-window}
  @see-function{gdk-window-parent}
  @see-function{gdk-offscreen-window-embedder}"
  (window (g-object gdk-window)))

(export 'gdk-window-effective-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_toplevel () -> gdk-window-effective-toplevel
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_toplevel" gdk-window-effective-toplevel)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-7}
  @argument[window]{a @class{gdk-window} object}
  @return{The effective @class{gdk-window} toplevel containing @arg{window}.}
  @begin{short}
    Gets the toplevel window that is an ancestor of @arg{window}.
  @end{short}

  Works like the function @fun{gdk-window-toplevel}, but treats an offscreen
  window's embedder as its parent, using the function
  @fun{gdk-window-effective-parent}.

  See also the function @fun{gdk-offscreen-window-embedder}.
  @see-class{gdk-window}
  @see-function{gdk-window-toplevel}
  @see-function{gdk-window-effective-parent}
  @see-function{gdk-offscreen-window-embedder}"
  (window (g-object gdk-window)))

(export 'gdk-window-effective-toplevel)

;;; --- End of file gdk.window.lisp --------------------------------------------
