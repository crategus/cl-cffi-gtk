;;; ----------------------------------------------------------------------------
;;; gdk.window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org. See <http://www.gtk.org>.
;;; The API  documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;;     GdkWindow
;;;     GdkWindowType
;;;     GdkWindowWindowClass
;;;     GdkWindowHints
;;;     GdkGeometry
;;;     GdkGravity
;;;     GdkWindowEdge
;;;     GdkWindowTypeHint
;;;     GdkWindowAttr
;;;     GdkWindowAttributesType
;;;
;;;     gdk_window_new
;;;     gdk_window_destroy
;;;     gdk_window_get_window_type
;;;     gdk_window_get_display
;;;     gdk_window_get_screen
;;;     gdk_window_get_visual
;;;     gdk_window_at_pointer                              * deprecated *
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
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_resize_drag_for_device
;;;     gdk_window_begin_move_drag
;;;     gdk_window_begin_move_drag_for_device
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect
;;;     gdk_window_begin_paint_region
;;;     gdk_window_end_paint
;;;     gdk_window_get_visible_region
;;;
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates
;;;     gdk_window_process_updates
;;;     gdk_window_set_debug_updates
;;;     gdk_window_enable_synchronized_configure
;;;     gdk_window_configure_finished
;;;
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
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          * deprecated *
;;;     gdk_window_set_background_rgba
;;;     gdk_window_set_background_pattern
;;;     gdk_window_get_background_patter
;;;
;;;     GDK_PARENT_RELATIVE
;;;
;;;     gdk_window_set_cursor
;;;     gdk_window_get_cursor
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
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint
;;;     gdk_window_get_position
;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             * deprecated *
;;;     gdk_window_get_device_position
;;;
;;;     GdkModifierType   --> gdk.event-structures.lisp
;;;
;;;     gdk_window_get_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_children
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
;;;
;;;     GdkWMDecoration
;;;
;;;     gdk_window_set_functions
;;;
;;;     GdkWMFunction
;;;
;;;     gdk_get_default_root_window
;;;
;;;     gdk_window_get_support_multidevice
;;;     gdk_window_set_support_multidevice
;;;     gdk_window_get_device_cursor
;;;     gdk_window_set_device_cursor
;;;     gdk_window_get_device_events
;;;     gdk_window_set_device_events
;;;     gdk_window_get_source_events
;;;     gdk_window_set_source_events
;;;
;;;     gdk_offscreen_window_get_surface
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed
;;;     gdk_window_coords_from_parent
;;;     gdk_window_coords_to_parent
;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel
;;; ----------------------------------------------------------------------------

(in-package :gdk)

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

;;;-----------------------------------------------------------------------------

(setf (documentation 'gdk-window 'type)
 "@version{2013-1-13}
  @begin{short}
    Onscreen display areas in the target window system.

    A @sym{gdk-window} window is a (usually) rectangular region on the screen.
    It is a low-level object, used to implement high-level objects such as
    @class{gtk-widget} and @class{gtk-window} widgets on the GTK+ level. A
    @class{gtk-window} window is a toplevel window, the thing a user might think
    of as a \"window\" with a titlebar and so on; a @class{gtk-window} widget
    may contain many @sym{gdk-window} windows. For example, each
    @class{gtk-button} button instance has a @sym{gdk-window} window associated
    with it.
  @end{short}

  @subheading{Composited Windows}
    Normally, the windowing system takes care of rendering the contents of a
    child window onto its parent window. This mechanism can be intercepted by
    calling @fun{gdk-window-set-composited} on the child window. For a
    composited window it is the responsibility of the application to render the
    window contents at the right spot.

    @b{Example:} Composited windows

    FIXME: MISSING XINCLUDE CONTENT

    In the example Example, \"Composited windows\", a button is placed inside of
    an event box inside of a window. The event box is set as composited and
    therefore is no longer automatically drawn to the screen.

    When the contents of the event box change, an expose event is generated on
    its parent window (which, in this case, belongs to the toplevel
    @class{gtk-window} widget). The expose handler for this widget is
    responsible for emerging the changes back on the screen in the way that it
    wishes.

    In our case, we merge the contents with a 50% transparency. We also set the
    background colour of the window to red. The effect is that the background
    shows through the button.

  @subheading{Offscreen Windows}
    Offscreen windows are more general than composited windows, since they allow
    not only to modify the rendering of the child window onto its parent, but
    also to apply coordinate transformations.

    To integrate an offscreen window into a window hierarchy, one has to call
    @fun{gdk-offscreen-window-set-embedder} and handle a number of signals. The
    \"pick-embedded-child\" signal on the embedder window is used to select
    an offscreen child at given coordinates, and the \"to-embedder\" and
    \"from-embedder\" signals on the offscreen window are used to translate
    coordinates between the embedder and the offscreen window.

    For rendering an offscreen window onto its embedder, the contents of the
    offscreen window are available as a surface, via the function
    @fun{gdk-offscreen-window-get-surface}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-surface\" signal}
      @begin{pre}
 lambda (window width height)   : Run Last
      @end{pre}
      The \"create-surface\" signal is emitted when an offscreen window needs
      its surface (re)created, which happens either when the the window is first
      drawn to, or when the window is being resized. The first signal handler
      that returns a non-@code{nil} surface will stop any further signal
      emission, and its surface will be used.
      Note that it is not possible to access the window's previous surface from
      within any callback of this signal. Calling the function
      @fun{gdk-offscreen-window-get-surface} will lead to a crash.
      @begin[code]{table}
        @entry[window]{The offscreen window on which the signal is emitted.}
        @entry[width]{The width of the offscreen surface to create.}
        @entry[height]{The height of the offscreen surface to create.}
        @entry[Returns]{The newly created cairo_surface_t for the offscreen
          window.}
      @end{table}
      Since 3.0

    @subheading{The \"from-embedder\" signal}
      @begin{pre}
 lambda (window embedder-x embedder-y offscreen-x offscreen-y)   : Run Last
      @end{pre}
      The \"from-embedder\" signal is emitted to translate coordinates in the
      embedder of an offscreen window to the offscreen window.
      See also \"to-embedder\".
      @begin[code]{table}
        @entry[window]{the offscreen window on which the signal is emitted}
        @entry[embedder-x]{x coordinate in the embedder window}
        @entry[embedder-y]{y coordinate in the embedder window}
        @entry[offscreen-x]{return location for the x coordinate in the
          offscreen window}
        @entry[offscreen-y]{return location for the y coordinate in the
          offscreen window}
      @end{table}
      Since 2.18

    @subheading{The \"pick-embedded-child\" signal}
      @begin{pre}
 lambda (window x y)
      @end{pre}
      The \"pick-embedded-child\" signal is emitted to find an embedded child
      at the given position.
      @begin[code]{table}
        @entry[window]{the window on which the signal is emitted}
        @entry[x]{x coordinate in the window}
        @entry[y]{y coordinate in the window}
        @entry[Returns]{the GdkWindow of the embedded child at x, y, or NULL}
      @end{table}
      Since 2.18

    @subheading{The \"to-embedder\" signal}
      @begin{pre}
 lambda (window offscreen-x offscreen-y embedder-x embedder-y)   : Run Last
      @end{pre}
      The \"to-embedder\" signal is emitted to translate coordinates in an
      offscreen window to its embedder.
      See also \"from-embedder\".
      @begin[code]{table}
        @entry[window]{the offscreen window on which the signal is emitted}
        @entry[offscreen-x]{x coordinate in the offscreen window}
        @entry[offscreen-y]{y coordinate in the offscreen window}
        @entry[embedder-x]{return location for the x coordinate in the embedder
          window}
        @entry[embedder-y]{return location for the y coordinate in the embedder
          window}
      @end{table}
      Since 2.18
  @end{dictionary}
  @see-slot{gdk-window-cursor}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor" 'gdk-window) 't)
 "The @code{cursor} property of type @class{gdk-cursor} (Read / Write).@br{}
  The mouse pointer for a @sym{gdk-window}. See @fun{gdk-window-set-cursor}
  and @fun{gdk-window-get-cursor} for details.@br{}
  Since 2.18")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-cursor 'function)
 "@version{2013-4-4}
  Accessor of the slot @code{\"cursor\"} of the @class{gdk-window} class.")

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
  (:offscreen 5))

;;; --- gdk-window-type --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-type atdoc:*external-symbols*)
 "@version{2013-1-21}
  @short{Describes the kind of window.}
  @begin{pre}
(define-g-enum \"GdkWindowType\" gdk-window-type
  (:export t
   :type-initializer \"gdk_window_type_get_type\")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:temp 3)
  (:foreign 4)
  (:offscreen 5))
  @end{pre}
  @begin[code]{table}
    @entry[:root]{root window; this window has no parent, covers the entire
      screen, and is created by the window system}
    @entry[:toplevel]{toplevel window (used to implement @class{gtk-window})}
    @entry[:child]{child window (used to implement e.g. @class{gtk-entry})}
    @entry[:temp]{override redirect temporary window (used to implement
      @class{gtk-menu})}
    @entry[:foreign]{foreign window (see @code{gdk_window_foreign_new()})}
    @entry[:offscreen]{offscreen window (see the section called
      \"Offscreen Windows\"). Since 2.18.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowWindowClass
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowWindowClass" gdk-window-window-class
  (:export t
   :type-initializer "gdk_window_window_class_get_type")
  (:input-output 0)
  (:input-only 1))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-window-class atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-window-class atdoc:*external-symbols*)
 "@version{2013-4-4}
  @begin{short}
    @code{:input-output} windows are the standard kind of window you might
    expect. Such windows receive events and are also displayed on screen.
    @code{:input-only} windows are invisible; they are usually placed above
    other windows in order to trap or filter the events. You can't draw on
    @code{:input-only} windows.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkWindowWindowClass\" gdk-window-window-class
  (:export t
   :type-initializer \"gdk_window_window_class_get_type\")
  (:input-output 0)
  (:input-only 1))
  @end{pre}
  @begin[code]{table}
    @entry[:input-output]{window for graphics and events}
    @entry[:input-only]{window for events only}
  @end{table}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-hints atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-hints atdoc:*external-symbols*)
 "@version{2013-3-29}
  @begin{short}
    Used to indicate which fields of a @class{gdk-geometry} struct should be
    paid attention to. Also, the presence/absence of @code{:pos},
    @code{:user-pos}, and @code{:user-size} is significant, though they do not
    directly refer to @class{gdk-geometry} fields. @code{:user-pos} will be set
    automatically by @class{gtk-window} if you call @fun{gtk-window-move}.
    @code{:user-pos} and @code{:user-size} should be set if the user specified
    a size/position using a --geometry command-line argument;
    @fun{gtk-window-parse-geometry} automatically sets these flags.
  @end{short}
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
    @entry[:pos]{indicates that the program has positioned the window}
    @entry[:min-size]{min size fields are set}
    @entry[:max-size]{max size fields are set}
    @entry[:base-size]{base size fields are set}
    @entry[:aspect]{aspect ratio fields are set}
    @entry[:resize-inc]{resize increment fields are set}
    @entry[:win-gravity]{window gravity field is set}
    @entry[:user-pos]{indicates that the window's position was explicitly set
      by the user}
    @entry[:user-size]{indicates that the window's size was explicitly set by
      the user}
  @end{table}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-gravity atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-gravity atdoc:*external-symbols*)
 "@version{2013-4-4}
  @begin{short}
    Defines the reference point of a window and the meaning of coordinates
    passed to @fun{gtk-window-move}. See @fun{gtk-window-move} and the
    \"implementation notes\" section of the Extended Window Manager Hints
    specification for more details.
  @end{short}
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
    @entry[:north-west]{the reference point is at the top left corner.}
    @entry[:north]{the reference point is in the middle of the top edge.}
    @entry[:north-east]{the reference point is at the top right corner.}
    @entry[:west]{the reference point is at the middle of the left edge.}
    @entry[:center]{the reference point is at the center of the window.}
    @entry[:east]{the reference point is at the middle of the right edge.}
    @entry[:south-west]{the reference point is at the lower left corner.}
    @entry[:south]{the reference point is at the middle of the lower edge.}
    @entry[:south-east]{the reference point is at the lower right corner.}
    @entry[:static]{the reference point is at the top left corner of the
      window itself, ignoring window manager decorations.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GdkGeometry
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-geometry "GdkGeometry"
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-geometry 'type)
 "@version{2013-4-5}
  @begin{short}
    The GdkGeometry struct gives the window manager information about a window's
    geometry constraints. Normally you would set these on the GTK+ level using
    gtk_window_set_geometry_hints(). GtkWindow then sets the hints on the
    GdkWindow it creates.
  @end{short}

  gdk_window_set_geometry_hints() expects the hints to be fully valid already
  and simply passes them to the window manager; in contrast,
  gtk_window_set_geometry_hints() performs some interpretation. For example,
  GtkWindow will apply the hints to the geometry widget instead of the
  toplevel window, if you set a geometry widget. Also, the
  min_width/min_height/max_width/max_height fields may be set to -1, and
  GtkWindow will substitute the size request of the window or geometry widget.
  If the minimum size hint is not provided, GtkWindow will use its requisition
  as the minimum size. If the minimum size is provided and a geometry widget
  is set, GtkWindow will take the minimum size as the minimum size of the
  geometry widget rather than the entire window. The base size is treated
  similarly.

  The canonical use-case for gtk_window_set_geometry_hints() is to get a
  terminal widget to resize properly. Here, the terminal text area should be
  the geometry widget; GtkWindow will then automatically set the base size to
  the size of other widgets in the terminal window, such as the menubar and
  scrollbar. Then, the width_inc and height_inc fields should be set to the
  size of one character in the terminal. Finally, the base size should be set
  to the size of one character. The net effect is that the minimum size of the
  terminal will have a 1x1 character terminal area, and only terminal sizes on
  the \"character grid\" will be allowed.

  Here's an example of how the terminal example would be implemented, assuming
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
  The other useful fields are the min_aspect and max_aspect fields; these
  contain a width/height ratio as a floating point number. If a geometry
  widget is set, the aspect applies to the geometry widget rather than the
  entire window. The most common use of these hints is probably to set
  min_aspect and max_aspect to the same value, thus forcing the window to keep
  a constant aspect ratio.
  @begin{pre}
(define-g-boxed-cstruct gdk-geometry \"GdkGeometry\"
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
  @end{pre}
  @begin[code]{table}
    @entry[min-width]{minimum width of window (or -1 to use requisition,
      with GtkWindow only)}
    @entry[min-height]{minimum height of window (or -1 to use requisition,
      with GtkWindow only)}
    @entry[max-width}{maximum width of window (or -1 to use requisition,
      with GtkWindow only)}
    @entry[max-height]{maximum height of window (or -1 to use requisition,
      with GtkWindow only)}
    @entry[base-width]{allowed window widths are base_width + width_inc * N
      where N is any integer (-1 allowed with GtkWindow)}
    @entry[base-height]{allowed window widths are base_height + height_inc * N
      where N is any integer (-1 allowed with GtkWindow)}
    @entry[width-increment]{width resize increment}
    @entry[height-increment]{height resize increment}
    @entry[min-aspect]{minimum width/height ratio}
    @entry[max-aspect]{maximum width/height ratio}
    @entry[win-gravity]{window gravity, see gtk_window_set_gravity()}
  @end{table}
  @see-constructor{make-gdk-geometry}
  @see-constructor{copy-gdk-geometry}
  @see-slot{min-width}
  @see-slot{min-height}
  @see-slot{max-width}
  @see-slot{max-height}
  @see-slot{base-width}
  @see-slot{base-height}
  @see-slot{width-increment}
  @see-slot{height-increment}
  @see-slot{min-aspect}
  @see-slot{max-aspect}
  @see-slot{gravity gdk-gravity}")

;;; --- copy-gdk-window-attr ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-geometry 'function)
 "@version{2013-4-5}
  @argument[instance]{a @class{gdk-geometry} struct}
  Copy constructor of a @class{gdk-geometry} struct.")

;;; --- make-gdk-window-attr ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-geometry 'function)
 "@version{2013-4-5}
    @argument[min-width]{minimum width of window (or -1 to use requisition,
      with GtkWindow only)}
    @argument[min-height]{minimum height of window (or -1 to use requisition,
      with GtkWindow only)}
    @argument[max-width}{maximum width of window (or -1 to use requisition,
      with GtkWindow only)}
    @argument[max-height]{maximum height of window (or -1 to use requisition,
      with GtkWindow only)}
    @argument[base-width]{allowed window widths are base_width + width_inc * N
      where N is any integer (-1 allowed with GtkWindow)}
    @argument[base-height]{allowed window widths are base_height + height_inc
       * N where N is any integer (-1 allowed with GtkWindow)}
    @argument[width-increment]{width resize increment}
    @argument[height-increment]{height resize increment}
    @argument[min-aspect]{minimum width/height ratio}
    @argument[max-aspect]{maximum width/height ratio}
    @argument[win-gravity]{window gravity, see gtk_window_set_gravity()}
  @begin{short}
    Creates a @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-width 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{min-width} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-height 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{min-height} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-width 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{max-width} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-height 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{max-height} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-base-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-base-width 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{base-width} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-base-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-base-height 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{base-height} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-width-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-width-increment 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{width-increment} of the @class{gdk-geometry}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-height-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-height-increment 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{height-increment} of the @class{gdk-geometry}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-aspect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-aspect 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{min-aspect} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-aspect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-aspect 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{max-aspect} of the @class{gdk-geometry} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-gravity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-gravity 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{gravity} of the @class{gdk-geometry} struct.
  @end{short}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-edge atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-edge atdoc:*external-symbols*)
 "@version{2013-4-5}
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
    @entry[:north-west]{the top left corner.}
    @entry[:north]{the top edge.}
    @entry[:north-east]{the top right corner.}
    @entry[:west]{the left edge.}
    @entry[:east]{the right edge.}
    @entry[:south-west]{the lower left corner.}
    @entry[:south]{the lower edge.}
    @entry[:south-east]{the lower right corner.}
  @end{table}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-type-hint atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-type-hint atdoc:*external-symbols*)
 "@version{2013-4-5}
  @begin{short}
    These are hints for the window manager that indicate what type of function
    the window has. The window manager can use this when determining decoration
    and behaviour of the window. The hint must be set before mapping the window.
  @end{short}

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
    @entry[:menu]{Window used to implement a menu; GTK+ uses this hint only for
      torn-off menus, see GtkTearoffMenuItem.}
    @entry[:toolbar]{Window used to implement toolbars.}
    @entry[:splashscreen]{Window used to display a splash screen during
      application startup.}
    @entry[:utility]{Utility windows which are not detached toolbars or
      dialogs.}
    @entry[:dock]{Used for creating dock or panel windows.}
    @entry[:desktop]{Used for creating the desktop background window.}
    @entry[:dropdown-menu]{A menu that belongs to a menubar.}
    @entry[:popup-menu]{A menu that does not belong to a menubar,
      e. g. a context menu.}
    @entry[:tooltip]{A tooltip.}
    @entry[:notification]{A notification - typically a \"bubble\" that belongs
      to a status icon.}
    @entry[:combo]{A popup from a combo box.}
    @entry[:dnd]{A window that is used to implement a DND cursor.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GdkWindowAttr
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gdk-window-attr "GdkWindowAttr"
  (title (:string :free-from-foreign nil) :initform "")
  (event-mask gdk-event-mask :initform nil)
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0)
  (window-class gdk-window-window-class :initform :input-output)
  (visual (g-object gdk-visual) :initform nil)
  (window-type gdk-window-type :initform :toplevel)
  (cursor (g-object gdk-cursor) :initform nil)
  (wmclass-name (:string :free-from-foreign nil) :initform "")
  (wmclass-class (:string :free-from-foreign nil) :initform "")
  (override-redirect :boolean :initform nil)
  (type-hint gdk-window-type-hint :initform :normal))

(export (boxed-related-symbols 'gdk-window-attr))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-window-attr 'type)
 "@version{2013-4-4}
  @begin{short}
    Attributes to use for a newly-created window.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gdk-window-attr \"GdkWindowAttr\"
  (title (:string :free-from-foreign nil) :initform \"\")
  (event-mask gdk-event-mask :initform nil)
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0)
  (window-class gdk-window-window-class :initform :input-output)
  (visual (g-object gdk-visual) :initform nil)
  (window-type gdk-window-type :initform :toplevel)
  (cursor (g-object gdk-cursor) :initform nil)
  (wmclass-name (:string :free-from-foreign nil) :initform \"\")
  (wmclass-class (:string :free-from-foreign nil) :initform \"\")
  (override-redirect :boolean :initform nil)
  (type-hint gdk-window-type-hint :initform :normal))
  @end{pre}
  @begin[code]{table}
    @entry[title]{title of the window (for toplevel windows)}
    @entry[event-mask]{event mask (see gdk_window_set_events())}
    @entry[x]{X coordinate relative to parent window (see gdk_window_move())}
    @entry[y]{Y coordinate relative to parent window (see gdk_window_move())}
    @entry[width]{width of window}
    @entry[height]{height of window}
    @entry[window-class]{GDK_INPUT_OUTPUT (normal window) or GDK_INPUT_ONLY
      (invisible window that receives events)}
    @entry[visual]{GdkVisual for window}
    @entry[window-type]{type of window}
    @entry[cursor]{cursor for the window (see gdk_window_set_cursor())}
    @entry[wmclass-name]{don't use (see gtk_window_set_wmclass())}
    @entry[wmclass-class]{do not use (see gtk_window_set_wmclass())}
    @entry[override-redirect]{TRUE to bypass the window manager}
    @entry[type-hint]{a hint of the function of the window}
  @end{table}
  @see-slot{gdk-window-attr-title}
  @see-slot{gdk-window-attr-event-mask}
  @see-slot{gdk-window-attr-x}
  @see-slot{gdk-window-attr-y}
  @see-slot{gdk-window-attr-width}
  @see-slot{gdk-window-attr-height}
  @see-slot{gdk-window-attr-window-class}
  @see-slot{gdk-window-attr-visual}
  @see-slot{gdk-window-attr-window-type}
  @see-slot{gdk-window-attr-cursor}
  @see-slot{gdk-window-attr-wmclass-name}
  @see-slot{gdk-window-attr-wmclass-class}
  @see-slot{gdk-window-attr-override-redirect}
  @see-slot{gdk-window-attr-type-hint gdk-window-type-hint}
  @see-constructor{copy-gdk-window-attr}
  @see-constructor{make-gdk-window-attr}")

;;; --- copy-gdk-window-attr ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-window-attr 'function)
 "@version{2013-4-4}
  @argument[instance]{a @class{gdk-window-attr} struct}
  Copy constructor of a @class{gdk-window-attr} struct.")

;;; --- make-gdk-window-attr ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-window-attr 'function)
 "@version{2013-2-25}
  @argument[title]{title of the window (for toplevel windows)}
  @argument[event-mask]{event mask (see gdk_window_set_events())}
  @argument[x]{X coordinate relative to parent window (see gdk_window_move())}
  @argument[y]{Y coordinate relative to parent window (see gdk_window_move())}
  @argument[width]{width of window}
  @argument[height]{height of window}
  @argument[window-class]{GDK_INPUT_OUTPUT (normal window) or GDK_INPUT_ONLY
    (invisible window that receives events)}
  @argument[visual]{GdkVisual for window}
  @argument[window-type]{type of window}
  @argument[cursor]{cursor for the window (see gdk_window_set_cursor())}
  @argument[wmclass-name]{don't use (see gtk_window_set_wmclass())}
  @argument[wmclass-class]{do not use (see gtk_window_set_wmclass())}
  @argument[override-redirect]{TRUE to bypass the window manager}
  @argument[type-hint]{a hint of the function of the window}
  @begin{short}
    Creates a @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-title 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{title} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-event-mask atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-event-mask 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{event-mask} of the @class{gdk-window-attr}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-x atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-x 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{x} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-y atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-y 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{y} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-width 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{width} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-height 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{height} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-window-class atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-window-class 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{window-class} of the @class{gdk-window-attr}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-visual atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-visual 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{visual} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-window-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-window-type 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{window-type} of the @class{gdk-window-attr}#
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-cursor 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{cursor} of the @class{gdk-window-attr} struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-wmclass-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-wmclass-name 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{wmclass-name} of the @class{gdk-window-attr}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-wmclass-class atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-wmclass-class 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{wmclass-class} of the @class{gdk-window-attr}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-override-redirect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-override-redirect 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{override-redirect} of the @class{gdk-window-attr}
    struct.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-type-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-type-hint 'function)
 "@version{2013-4-4}
  @begin{short}
    Accessor of the slot @code{type-hint} of the @class{gdk-window-attr} struct.
  @end{short}")

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attributes-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-attributes-type atdoc:*external-symbols*)
 "@version{2013-4-5}
  @begin{short}
    Used to indicate which fields in the GdkWindowAttr struct should be honored.
    For example, if you filled in the \"cursor\" and \"x\" fields of
    GdkWindowAttr, pass \"GDK_WA_X | GDK_WA_CURSOR\" to gdk_window_new(). Fields
    in GdkWindowAttr not covered by a bit in this enum are required; for
    example, the width/height, wclass, and window_type fields are required, they
    have no corresponding flag in GdkWindowAttributesType.
  @end{short}
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
    @entry[:title]{Honor the title field}
    @entry[:x]{Honor the X coordinate field}
    @entry[:y]{Honor the Y coordinate field}
    @entry[:cursor]{Honor the cursor field}
    @entry[:visual]{Honor the visual field}
    @entry[:wmclass]{Honor the wmclass_class and wmclass_name fields}
    @entry[:noredir]{Honor the override_redirect field}
    @entry[:type-hint]{Honor the type_hint field}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_new" gdk-window-new)
    (g-object gdk-window :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[parent]{a GdkWindow, or NULL to create the window as a child of the
    default root window for the default display}
  @argument[attributes]{attributes of the new window}
  @argument[attributes_mask]{mask indicating which fields in attributes are
    valid}
  @return{The new GdkWindow.}
  Creates a new GdkWindow using the attributes from attributes. See
  GdkWindowAttr and GdkWindowAttributesType for more details. Note: to use
  this on displays other than the default display, parent must be specified."
  (parent (g-object gdk-window))
  (attributes (g-boxed-foreign gdk-window-attr))
  (attributes-mask gdk-window-attributes-type))

(export 'gdk-window-new)

;;; ----------------------------------------------------------------------------
;;; gdk_window_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_destroy" gdk-window-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Destroys the window system resources associated with window and decrements
    window's reference count. The window system resources for all children of
    window are also destroyed, but the children's reference counts are not
    decremented.
  @end{short}

  Note that a window will not be destroyed automatically when its reference
  count reaches zero. You must call this function yourself before that
  happens."
  (window (g-object gdk-window)))

(export 'gdk-window-destroy)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_window_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_window_type" gdk-window-get-window-type)
    gdk-window-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{type of window}
  Gets the type of the window. See GdkWindowType."
  (window (g-object gdk-window)))

(export 'gdk-window-get-window-type)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_display" gdk-window-get-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{the GdkDisplay associated with window}
  @begin{short}
    Gets the GdkDisplay associated with a GdkWindow.
  @end{short}

  Since 2.24"
  (window (g-object gdk-window)))

(export 'gdk-window-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_screen" gdk-window-get-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{the GdkScreen associated with window}
  @begin{short}
    Gets the GdkScreen associated with a GdkWindow.
  @end{short}

  Since 2.24"
  (window (g-object gdk-window)))

(export 'gdk-window-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visual ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visual" gdk-window-get-visual) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{a GdkVisual}
  @begin{short}
    Gets the GdkVisual describing the pixel format of window.
  @end{short}

  Since 2.24"
  (window (g-object gdk-window)))

(export 'gdk-window-get-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_window_at_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_at_pointer" %gdk-window-at-pointer) (g-object gdk-window)
  (win-x (:pointer :int))
  (win-y (:pointer :int)))

(defun gdk-window-at-pointer ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @begin{return}
    @code{window} -- window under the mouse pointer @br{}
    @code{win-x} -- origin of the window under the pointer @br{}
    @code{win-y} -- origin of the window under the pointer
  @end{return}
  @subheading{Warning}
    @sym{gdk-window-at-pointer} has been deprecated since version 3.0 and
    should not be used in newly-written code.
    Use @fun{gdk-device-get-window-at-position} instead.

  @begin{short}
    Obtains the window underneath the mouse pointer, returning the location of
    that window in win_x, win_y. Returns NULL if the window under the mouse
    pointer is not known to GDK (if the window belongs to another application
    and a GdkWindow hasn't been created for it with gdk_window_foreign_new())
  @end{short}

  NOTE: For multihead-aware widgets or applications use
  gdk_display_get_window_at_pointer() instead."
  (with-foreign-objects ((x :int) (y :int))
    (let ((window (%gdk-window-at-pointer x y)))
      (if window
          (values window (mem-ref x :int) (mem-ref y :int))
          (values nil nil nil)))))

(export 'gdk-window-at-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show" gdk-window-show) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Like gdk_window_show_unraised(), but also raises the window to the top of
    the window stack (moves the window to the front of the Z-order).
  @end{short}

  This function maps a window so it's visible onscreen. Its opposite is
  gdk_window_hide().

  When implementing a GtkWidget, you should call this function on the widget's
  GdkWindow as part of the \"map\" method."
  (window (g-object gdk-window)))

(export 'gdk-window-show)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_unraised ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_unraised" gdk-window-show-unraised) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-4}
  @argument[window]{a GdkWindow}
  @begin{short}
    Shows a GdkWindow onscreen, but does not modify its stacking order. In
    contrast, gdk_window_show() will raise the window to the top of the window
    stack.
  @end{short}

  On the X11 platform, in Xlib terms, this function calls XMapWindow() (it
  also updates some internal GDK state, which means that you can't really use
  XMapWindow() directly on a GDK window)."
  (window (g-object gdk-window)))

(export 'gdk-window-show-unraised)

;;; ----------------------------------------------------------------------------
;;; gdk_window_hide ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_hide" gdk-window-hide) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  For toplevel windows, withdraws them, so they will no longer be known to the
  window manager; for all windows, unmaps them, so they won't be displayed.
  Normally done automatically as part of gtk_widget_hide()."
  (window (g-object gdk-window)))

(export 'gdk-window-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_destroyed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_destroyed" gdk-window-is-destroyed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window is destroyed}
  @begin{short}
    Check to see if a window is destroyed.
  @end{short}

  Since 2.18"
  (window (g-object gdk-window)))

(export 'gdk-window-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_visible" gdk-window-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window is mapped}
  Checks whether the window has been mapped (with gdk_window_show() or
  gdk_window_show_unraised())."
  (window (g-object gdk-window)))

(export 'gdk-window-is-visible)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_viewable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_viewable" gdk-window-is-viewable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window is viewable}
  Check if the window and all ancestors of the window are mapped. (This is not
  necessarily \"viewable\" in the X sense, since we only check as far as we have
  GDK window parents, not to the root window.)"
  (window (g-object gdk-window)))

(export 'gdk-window-is-viewable)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_input_only ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_input_only" gdk-window-is-input-only) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @return{TRUE if window is input only}
  @begin{short}
    Determines whether or not the window is an input only window.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-is-input-only)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_shaped ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_shaped" gdk-window-is-shaped) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @return{TRUE if window is shaped}
  @begin{short}
    Determines whether or not the window is shaped.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-is-shaped)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_state" gdk-window-get-state) gdk-window-state
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{window state bitfield}
  Gets the bitwise OR of the currently active window state flags, from the
  GdkWindowState enumeration."
  (window (g-object gdk-window)))

(export 'gdk-window-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_window_withdraw ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_withdraw" gdk-window-withdraw) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  Withdraws a window (unmaps it and asks the window manager to forget about
  it). This function is not really useful as gdk_window_hide() automatically
  withdraws toplevel windows before hiding them."
  (window (g-object gdk-window)))

(export 'gdk-window-withdraw)

;;; ----------------------------------------------------------------------------
;;; gdk_window_iconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_iconify" gdk-window-iconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Asks to iconify (minimize) window. The window manager may choose to ignore
    the request, but normally will honor it. Using gtk_window_iconify() is
    preferred, if you have a GtkWindow widget.
  @end{short}

  This function only makes sense when window is a toplevel window."
  (window (g-object gdk-window)))

(export 'gdk-window-iconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_deiconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_deiconify" gdk-window-deiconify) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  Attempt to deiconify (unminimize) window. On X11 the window manager may
  choose to ignore the request to deiconify. When using GTK+, use
  gtk_window_deiconify() instead of the GdkWindow variant. Or better yet, you
  probably want to use gtk_window_present(), which raises the window, focuses
  it, unminimizes it, and puts it on the current desktop."
  (window (g-object gdk-window)))

(export 'gdk-window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_stick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_stick" gdk-window-stick) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    \"Pins\" a window such that it's on all workspaces and does not scroll with
    viewports, for window managers that have scrollable viewports. (When using
    GtkWindow, gtk_window_stick() may be more useful.)
  @end{short}

  On the X11 platform, this function depends on window manager support, so may
  have no effect with many window managers. However, GDK will do the best it
  can to convince the window manager to stick the window. For window managers
  that don't support this operation, there's nothing you can do to force it to
  happen."
  (window (g-object gdk-window)))

(export 'gdk-window-stick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unstick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unstick" gdk-window-unstick) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  Reverse operation for gdk_window_stick(); see gdk_window_stick(), and
  gtk_window_unstick()."
  (window (g-object gdk-window)))

(export 'gdk-window-unstick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_maximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_maximize" gdk-window-maximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Maximizes the window. If the window was already maximized, then this
    function does nothing.
  @end{short}

  On X11, asks the window manager to maximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or don't have a concept of \"maximized\"; so you can't
  rely on the maximization actually happening. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.

  On Windows, reliably maximizes the window."
  (window (g-object gdk-window)))

(export 'gdk-window-maximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unmaximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unmaximize" gdk-window-unmaximize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Unmaximizes the window. If the window wasn't maximized, then this function
    does nothing.
  @end{short}

  On X11, asks the window manager to unmaximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or don't have a concept of \"maximized\"; so you can't
  rely on the unmaximization actually happening. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.

  On Windows, reliably unmaximizes the window."
  (window (g-object gdk-window)))

(export 'gdk-window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen" gdk-window-fullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Moves the window into fullscreen mode. This means the window covers the
    entire screen and is above any panels or task bars.
  @end{short}

  If the window was already fullscreen, then this function does nothing.

  On X11, asks the window manager to put window in a fullscreen state, if the
  window manager supports this operation. Not all window managers support
  this, and some deliberately ignore it or don't have a concept of
  \"fullscreen\"; so you can't rely on the fullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.

  Since 2.2"
  (window (g-object gdk-window)))

(export 'gdk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unfullscreen" gdk-window-unfullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Moves the window out of fullscreen mode. If the window was not fullscreen,
    does nothing.
  @end{short}

  On X11, asks the window manager to move window out of the fullscreen state,
  if the window manager supports this operation. Not all window managers
  support this, and some deliberately ignore it or don't have a concept of
  \"fullscreen\"; so you can't rely on the unfullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.

  Since 2.2"
  (window (g-object gdk-window)))

(export 'gdk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_above ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_above" gdk-window-set-keep-above) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[setting]{whether to keep window above other windows}
  @begin{short}
    Set if window must be kept above other windows. If the window was already
    above, then this function does nothing.
  @end{short}

  On X11, asks the window manager to keep window above, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or don't have a concept of \"keep above\"; so you can't
  rely on the window being kept above. But it will happen with most standard
  window managers, and GDK makes a best effort to get it to happen.

  Since 2.4"
  (window (g-object gdk-window))
  (setting :boolean))

(export 'gdk-window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_below ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_below" gdk-window-set-keep-below) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[setting]{whether to keep window below other windows}
  @begin{short}
    Set if window must be kept below other windows. If the window was already
    below, then this function does nothing.
  @end{short}

  On X11, asks the window manager to keep window below, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or don't have a concept of \"keep below\"; so you can't
  rely on the window being kept below. But it will happen with most standard
  window managers, and GDK makes a best effort to get it to happen.

  Since 2.4"
  (window (g-object gdk-window))
  (setting :boolean))

(export 'gdk-window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opacity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_opacity" gdk-window-set-opacity) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a top-level GdkWindow}
  @argument[opacity]{opacity}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    opacity 0 being fully transparent and 1 fully opaque. (Values of the opacity
    parameter are clamped to the [0,1] range.)
  @end{short}

  On X11, this works only on X screens with a compositing manager running.

  For setting up per-pixel alpha, see gdk_screen_get_rgba_visual(). For making
  non-toplevel windows translucent, see gdk_window_set_composited().

  Since 2.12"
  (window (g-object gdk-window))
  (opacity :double))

(export 'gdk-window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_composited" gdk-window-set-composited) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[composited]{TRUE to set the window as composited}
  @begin{short}
    Sets a GdkWindow as composited, or unsets it. Composited windows do not
    automatically have their contents drawn to the screen. Drawing is redirected
    to an offscreen buffer and an expose event is emitted on the parent of the
    composited window. It is the responsibility of the parent's expose handler
    to manually merge the off-screen content onto the screen in whatever way it
    sees fit. See Example 4, “Composited windows” for an example.
  @end{short}

  It only makes sense for child windows to be composited; see
  gdk_window_set_opacity() if you need translucent toplevel windows.

  An additional effect of this call is that the area of this window is no
  longer clipped from regions marked for invalidation on its parent. Draws
  done on the parent window are also no longer clipped by the child.

  This call is only supported on some systems (currently, only X11 with new
  enough Xcomposite and Xdamage extensions). You must call
  gdk_display_supports_composite() to check if setting a window as composited
  is supported before attempting to do so.

  Since 2.12"
  (window (g-object gdk-window))
  (composited :boolean))

(export 'gdk-window-set-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_composited" gdk-window-get-composited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window is composited.}
  @begin{short}
    Determines whether window is composited.
  @end{short}

  See gdk_window_set_composited().

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move" gdk-window-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[x]{X coordinate relative to window's parent}
  @argument[y]{Y coordinate relative to window's parent}
  @begin{short}
    Repositions a window relative to its parent window. For toplevel windows,
    window managers may ignore or modify the move; you should probably use
    gtk_window_move() on a GtkWindow widget anyway, instead of using GDK
    functions. For child windows, the move will reliably succeed.
  @end{short}

  If you're also planning to resize the window, use gdk_window_move_resize()
  to both move and resize simultaneously, for a nicer visual effect."
  (window (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-move)

;;; ----------------------------------------------------------------------------
;;; gdk_window_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_resize" gdk-window-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[width]{new width of the window}
  @argument[height]{new height of the window}
  @begin{short}
    Resizes window; for toplevel windows, asks the window manager to resize the
    window. The window manager may not allow the resize. When using GTK+, use
    gtk_window_resize() instead of this low-level GDK function.
  @end{short}

  Windows may not be resized below 1x1.

  If you're also planning to move the window, use gdk_window_move_resize() to
  both move and resize simultaneously, for a nicer visual effect."
  (window (g-object gdk-window))
  (width :int)
  (height :int))

(export 'gdk-window-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_resize" gdk-window-move-resize) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[x]{new X position relative to window's parent}
  @argument[y]{new Y position relative to window's parent}
  @argument[width]{new width}
  @argument[height]{new height}
  Equivalent to calling gdk_window_move() and gdk_window_resize(), except that
  both operations are performed at once, avoiding strange visual effects.
  (i.e. the user may be able to see the window first move, then resize, if you
  don't use gdk_window_move_resize().)"
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
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[dx]{Amount to scroll in the X direction}
  @argument[dy]{Amount to scroll in the Y direction}
  @begin{short}
    Scroll the contents of window, both pixels and children, by the given
    amount. window itself does not move. Portions of the window that the scroll
    operation brings in from offscreen areas are invalidated. The invalidated
    region may be bigger than what would strictly be necessary.
  @end{short}

  For X11, a minimum area will be invalidated if the window has no subwindows,
  or if the edges of the window's parent do not extend beyond the edges of the
  window. In other cases, a multi-step process is used to scroll the window
  which may produce temporary visual artifacts and unnecessary invalidations."
  (window (g-object gdk-window))
  (dx :int)
  (dy :int))

(export 'gdk-window-scroll)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_region" gdk-window-move-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[region]{The cairo_region_t to move}
  @argument[dx]{Amount to move in the X direction}
  @argument[dy]{Amount to move in the Y direction}
  @begin{short}
    Move the part of window indicated by region by dy pixels in the Y direction
    and dx pixels in the X direction. The portions of region that not covered by
    the new position of region are invalidated.
  @end{short}

  Child windows are not moved.

  Since 2.8"
  (window (g-object gdk-window))
  (region cairo-region-t)
  (dx :int)
  (dy :int))

(export 'gdk-window-move-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_flush" gdk-window-flush) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Flush all outstanding cached operations on a window, leaving the window in a
    state which reflects all that has been drawn before.
  @end{short}

  Gdk uses multiple kinds of caching to get better performance and nicer
  drawing. For instance, during exposes all paints to a window using double
  buffered rendering are keep on a surface until the last window has been
  exposed. It also delays window moves/scrolls until as long as possible until
  next update to avoid tearing when moving windows.

  Normally this should be completely invisible to applications, as we
  automatically flush the windows when required, but this might be needed if
  you for instance mix direct native drawing with gdk drawing. For Gtk widgets
  that don't use double buffering this will be called automatically before
  sending the expose event.

  Since 2.18"
  (window (g-object gdk-window)))

(export 'gdk-window-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_window_has_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_has_native" gdk-window-has-native) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window has a native window, FALSE otherwise.}
  @begin{short}
    Checks whether the window has a native window or not. Note that you can use
    gdk_window_ensure_native() if a native window is needed.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-has-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ensure_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_ensure_native" gdk-window-ensure-native) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{TRUE if the window has a native window, FALSE otherwise}
  @begin{short}
    Tries to ensure that there is a window-system native window for this
    GdkWindow. This may fail in some situations, returning FALSE.
  @end{short}

  Offscreen window and children of them can never have native windows.

  Some backends may not support native child windows.

  Since 2.18"
  (window (g-object gdk-window)))

(export 'gdk-window-ensure-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_reparent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_reparent" gdk-window-reparent) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-4}
  @argument[window]{a GdkWindow}
  @argument[new-parent]{new parent to move window into}
  @argument[x]{X location inside the new parent}
  @argument[y]{Y location inside the new parent}
  Reparents window into the given new_parent. The window being reparented will
  be unmapped as a side effect."
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
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Raises window to the top of the Z-order (stacking order), so that other
    windows with the same parent window appear below window. This is true
    whether or not the windows are visible.
  @end{short}

  If window is a toplevel, the window manager may choose to deny the request
  to move the window in the Z-order, gdk_window_raise() only requests the
  restack, does not guarantee it."
  (window (g-object gdk-window)))

(export 'gdk-window-raise)

;;; ----------------------------------------------------------------------------
;;; gdk_window_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_lower" gdk-window-lower) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Lowers window to the bottom of the Z-order (stacking order), so that other
    windows with the same parent window appear above window. This is true
    whether or not the other windows are visible.
  @end{short}

  If window is a toplevel, the window manager may choose to deny the request
  to move the window in the Z-order, gdk_window_lower() only requests the
  restack, does not guarantee it.

  Note that gdk_window_show() raises the window again, so don't call this
  function before gdk_window_show(). (Try gdk_window_show_unraised().)"
  (window (g-object gdk-window)))

(export 'gdk-window-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_window_restack ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_restack" gdk-window-restack) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[sibling]{a GdkWindow that is a sibling of window, or NULL}
  @argument[above]{a boolean}
  @begin{short}
    Changes the position of window in the Z-order (stacking order), so that it
    is above sibling (if above is TRUE) or below sibling (if above is FALSE).
  @end{short}

  If sibling is NULL, then this either raises (if above is TRUE) or lowers the
  window.

  If window is a toplevel, the window manager may choose to deny the request
  to move the window in the Z-order, gdk_window_restack() only requests the
  restack, does not guarantee it.

  Since 2.18"
  (window (g-object gdk-window))
  (sibling (g-object gdk-window))
  (above :boolean))

(export 'gdk-window-restack)

;;; ----------------------------------------------------------------------------
;;; gdk_window_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_focus" gdk-window-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[timestamp]{timestamp of the event triggering the window focus}
  Sets keyboard focus to window. In most cases, gtk_window_present() should be
  used on a GtkWindow, rather than calling this function."
  (window (g-object gdk-window))
  (timestamp :uint32))

(export 'gdk-window-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_register_dnd ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_register_dnd" gdk-window-register-dnd) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow.}
  Registers a window as a potential drop destination."
  (window (g-object gdk-window)))

(export 'gdk-window-register-dnd)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag" gdk-window-begin-resize-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[edge]{the edge or corner from which the drag is started}
  @argument[button]{the button being used to drag}
  @argument[root_x]{root window X coordinate of mouse click that began the drag}
  @argument[root_y]{root window Y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag
    (use gdk_event_get_time())}
  @begin{short}
    Begins a window resize operation (for a toplevel window).
  @end{short}

  This function assumes that the drag is controlled by the client pointer
  device, use gdk_window_begin_resize_drag_for_device() to begin a drag with a
  different device."
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
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[edge]{the edge or corner from which the drag is started}
  @argument[device]{the device used for the operation}
  @argument[button]{the button being used to drag}
  @argument[root_x]{root window X coordinate of mouse click that began the drag}
  @argument[root_y]{root window Y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag
    (use gdk_event_get_time())}
  @begin{short}
    Begins a window resize operation (for a toplevel window). You might use this
    function to implement a \"window resize grip\", for example; in fact
    GtkStatusbar uses it. The function works best with window managers that
    support the Extended Window Manager Hints, but has a fallback implementation
    for other window managers.
  @end{short}

  Since 3.4"
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
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[button]{the button being used to drag}
  @argument[root_x]{root window X coordinate of mouse click that began the drag}
  @argument[root_y]{root window Y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag}
  @begin{short}
    Begins a window move operation (for a toplevel window).
  @end{short}

  This function assumes that the drag is controlled by the client pointer
  device, use gdk_window_begin_move_drag_for_device() to begin a drag with a
  different device."
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
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[device]{the device used for the operation}
  @argument[button]{the button being used to drag}
  @argument[root_x]{root window X coordinate of mouse click that began the drag}
  @argument[root_y]{root window Y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag}
  @begin{short}
    Begins a window move operation (for a toplevel window). You might use this
    function to implement a \"window move grip\", for example. The function
    works best with window managers that support the Extended Window Manager
    Hints, but has a fallback implementation for other window managers.
  @end{short}

  Since 3.4"
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-move-drag-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_window_constrain_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_constrain_size" %gdk-window-constrain-size) :void
  (geometry (g-boxed-foreign gdk-geometry))
  (flags gdk-window-hints)
  (width :int)
  (height :int)
  (new-width (:pointer :int))
  (new-height (:pointer :int)))

(defun gdk-window-constrain-size (geometry flags width height)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[geometry]{a GdkGeometry structure}
  @argument[flags]{a mask indicating what portions of geometry are set}
  @argument[width]{desired width of window}
  @argument[height]{desired height of the window}
  @begin{return}
    @code{new-width} --  resulting width @br{}
    @code{new-height} -- resulting height
  @end{return}
  Constrains a desired width and height according to a set of geometry hints
  (such as minimum and maximum size)."
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
 "@version{2013-3-10}
  @argument[window]{A toplevel @class{gdk-window} object.}
  @begin{short}
    Emits a short beep associated to @arg{window} in the appropriate display, if
    supported. Otherwise, emits a short beep on the display just as
    @fun{gdk-display-beep}.
  @end{short}

  Since 2.12
  @see-function{gdk-display-beep}"
  (window (g-object gdk-window)))

(export 'gdk-window-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_clip_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_clip_region" gdk-window-get-clip-region)
    cairo-region-t
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    a cairo_region_t. This must be freed with cairo_region_destroy() when
    you are done.
  @end{return}
  Computes the region of a window that potentially can be written to by
  drawing primitives. This region may not take into account other factors such
  as if the window is obscured by other windows, but no area outside of this
  region will be affected by drawing primitives."
  (window (g-object gdk-window)))

(export 'gdk-window-get-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_rect" gdk-window-begin-paint-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[rectangle]{rectangle you intend to draw to}
  A convenience wrapper around gdk_window_begin_paint_region() which creates a
  rectangular region for you. See gdk_window_begin_paint_region() for details."
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-window-begin-paint-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_region" gdk-window-begin-paint-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[region]{region you intend to draw to}
  @begin{short}
    Indicates that you are beginning the process of redrawing region. A backing
    store (offscreen buffer) large enough to contain region will be created. The
    backing store will be initialized with the background color or background
    surface for window. Then, all drawing operations performed on window will be
    diverted to the backing store. When you call gdk_window_end_paint(), the
    backing store will be copied to window, making it visible onscreen. Only the
    part of window contained in region will be modified; that is, drawing
    operations are clipped to region.
  @end{short}

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call gdk_window_end_paint(). If
  you draw to window directly without calling gdk_window_begin_paint_region(),
  the user may see flicker as individual drawing operations are performed in
  sequence. The clipping and background-initializing features of
  gdk_window_begin_paint_region() are conveniences for the programmer, so you
  can avoid doing that work yourself.

  When using GTK+, the widget system automatically places calls to
  gdk_window_begin_paint_region() and gdk_window_end_paint() around emissions
  of the expose_event signal. That is, if you're writing an expose event
  handler, you can assume that the exposed area in GdkEventExpose has already
  been cleared to the window background, is already set as the clip region,
  and already has a backing store. Therefore in most cases, application code
  need not call gdk_window_begin_paint_region(). (You can disable the
  automatic calls around expose events on a widget-by-widget basis by calling
  gtk_widget_set_double_buffered().)

  If you call this function multiple times before calling the matching
  gdk_window_end_paint(), the backing stores are pushed onto a stack.
  gdk_window_end_paint() copies the topmost backing store onscreen, subtracts
  the topmost region from all other regions in the stack, and pops the stack.
  All drawing operations affect only the topmost backing store in the stack.
  One matching call to gdk_window_end_paint() is required for each call to
  gdk_window_begin_paint_region()."
  (window (g-object gdk-window))
  (region cairo-region-t))

(export 'gdk-window-begin-paint-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_paint" gdk-window-end-paint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-25}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Indicates that the backing store created by the most recent call to
    @fun{gdk-window-begin-paint-region} should be copied onscreen and deleted,
    leaving the next-most-recent backing store or no backing store at all as the
    active paint region.
  @end{short}
  See @fun{gdk-window-begin-paint-region} for full details.

  It is an error to call this function without a matching
  @fun{gdk-window-begin-paint-region} first.
  @see-function{gdk-window-begin-paint-region}"
  (window (g-object gdk-window)))

(export 'gdk-window-end-paint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visible_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visible_region" gdk-window-get-visible-region)
    cairo-region-t
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    a cairo_region_t. This must be freed with cairo_region_destroy() when
    you are done.
  @end{return}
  Computes the region of the window that is potentially visible. This does not
  necessarily take into account if the window is obscured by other windows,
  but no area outside of this region is visible."
  (window (g-object gdk-window)))

(export 'gdk-window-get-visible-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_rect" gdk-window-invalidate-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[rect]{rectangle to invalidate or NULL to invalidate the whole
    window}
  @argument[invalidate_children]{whether to also invalidate child windows}
  A convenience wrapper around gdk_window_invalidate_region() which
  invalidates a rectangular region. See gdk_window_invalidate_region() for
  details."
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_region" gdk-window-invalidate-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[region]{a cairo_region_t}
  @argument[invalidate_children]{TRUE to also invalidate child windows}
  @begin{short}
    Adds region to the update area for window. The update area is the region
    that needs to be redrawn, or \"dirty region\". The call
    gdk_window_process_updates() sends one or more expose events to the window,
    which together cover the entire update area. An application would normally
    redraw the contents of window in response to those expose events.
  @end{short}

  GDK will call gdk_window_process_all_updates() on your behalf whenever your
  program returns to the main loop and becomes idle, so normally there's no
  need to do that manually, you just need to invalidate regions that you know
  should be redrawn.

  The invalidate_children parameter controls whether the region of each child
  window that intersects region will also be invalidated. If FALSE, then the
  update area for child windows will remain unaffected. See
  gdk_window_invalidate_maybe_recurse if you need fine grained control over
  which children are invalidated."
  (window (g-object gdk-window))
  (region cairo-region-t)
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

(defcallback gdk-window-invalidate-maybe-recurse-cb :boolean
    ((window (g-object gdk-window))
     (user-data :pointer))
  (let ((fn (glib::get-stable-pointer-value user-data)))
    (funcall fn window)))

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_maybe_recurse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_maybe_recurse"
          %gdk-window-invalidate-maybe-recurse) :void
  (window (g-object gdk-window))
  (region cairo-region-t)
  (child-func :pointer)
  (user-data :pointer))

(defun gdk-window-invalidate-maybe-recurse (window region child-func)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[region]{a cairo_region_t}
  @argument[child-func]{function to use to decide if to recurse to a child,
    NULL means never recurse}
  @begin{short}
    Adds region to the update area for window. The update area is the region
    that needs to be redrawn, or \"dirty region\". The call
    gdk_window_process_updates() sends one or more expose events to the window,
    which together cover the entire update area. An application would normally
    redraw the contents of window in response to those expose events.
  @end{short}

  GDK will call gdk_window_process_all_updates() on your behalf whenever your
  program returns to the main loop and becomes idle, so normally there's no
  need to do that manually, you just need to invalidate regions that you know
  should be redrawn.

  The child_func parameter controls whether the region of each child window
  that intersects region will also be invalidated. Only children for which
  child_func returns TRUE will have the area invalidated."
  (with-stable-pointer (ptr child-func)
    (%gdk-window-invalidate-maybe-recurse
                               window
                               region
                               (callback gdk-window-invalidate-maybe-recurse-cb)
                               ptr)))

(export 'gdk-window-invalidate-maybe-recurse)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_update_area ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_update_area" gdk-window-get-update-area)
    cairo-region-t
 #+cl-cffi-gtk-documentation
 "@argument[window]{a GdkWindow}
  @return{the update area for window}
  Transfers ownership of the update area from window to the caller of the
  function. That is, after calling this function, window will no longer have
  an invalid/dirty region; the update area is removed from window and handed
  to you. If a window has no update area, gdk_window_get_update_area() returns
  NULL. You are responsible for calling cairo_region_destroy() on the returned
  region if it's non-NULL."
  (window (g-object gdk-window)))

(export 'gdk-window-get-update-area)

;;; ----------------------------------------------------------------------------
;;; gdk_window_freeze_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_freeze_updates" gdk-window-freeze-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  Temporarily freezes a window such that it won't receive expose events. The
  window will begin receiving expose events again when
  gdk_window_thaw_updates() is called. If gdk_window_freeze_updates() has been
  called more than once, gdk_window_thaw_updates() must be called an equal
  number of times to begin processing exposes."
  (window (g-object gdk-window)))

(export 'gdk-window-freeze-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_thaw_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_thaw_updates" gdk-window-thaw-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  Thaws a window frozen with gdk_window_freeze_updates()."
  (window (g-object gdk-window)))

(export 'gdk-window-thaw-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_all_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_all_updates" gdk-window-process-all-updates)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  Calls gdk_window_process_updates() for all windows (see GdkWindow) in the
  application.")

(export 'gdk-window-process-all-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_updates" gdk-window-process-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[update-children]{whether to also process updates for child windows}
  Sends one or more expose events to window. The areas in each expose event
  will cover the entire update area for the window (see
  gdk_window_invalidate_region() for details). Normally GDK calls
  gdk_window_process_all_updates() on your behalf, so there's no need to call
  this function unless you want to force expose events to be delivered
  immediately and synchronously (vs. the usual case, where GDK delivers them
  in an idle handler). Occasionally this is useful to produce nicer scrolling
  behavior, for example."
  (window (g-object gdk-window))
  (update-children :boolean))

(export 'gdk-window-process-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_debug_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_debug_updates" gdk-window-set-debug-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[setting]{TRUE to turn on update debugging}
  @begin{short}
    With update debugging enabled, calls to gdk_window_invalidate_region() clear
    the invalidated region of the screen to a noticeable color, and GDK pauses
    for a short time before sending exposes to windows during
    gdk_window_process_updates(). The net effect is that you can see the invalid
    region for each window and watch redraws as they occur. This allows you to
    diagnose inefficiencies in your application.
  @end{short}

  In essence, because the GDK rendering model prevents all flicker, if you are
  redrawing the same region 400 times you may never notice, aside from
  noticing a speed problem. Enabling update debugging causes GTK to flicker
  slowly and noticeably, so you can see exactly what's being redrawn when, in
  what order.

  The --gtk-debug=updates command line option passed to GTK+ programs enables
  this debug option at application startup time. That's usually more useful
  than calling gdk_window_set_debug_updates() yourself, though you might want
  to use this function to enable updates sometime after application startup
  time."
  (setting :boolean))

(export 'gdk-window-set-debug-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_enable_synchronized_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_enable_synchronized_configure"
           gdk-window-enable-synchronized-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Indicates that the application will cooperate with the window system in
    synchronizing the window repaint with the window manager during resizing
    operations. After an application calls this function, it must call
    gdk_window_configure_finished() every time it has finished all processing
    associated with a set of Configure events. Toplevel GTK+ windows
    automatically use this protocol.
  @end{short}

  On X, calling this function makes window participate in the
  _NET_WM_SYNC_REQUEST window manager protocol.

  Since 2.6"
  (window (g-object gdk-window)))

(export 'gdk-window-enable-synchronized-configure)

;;; ----------------------------------------------------------------------------
;;; gdk_window_configure_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_configure_finished" gdk-window-configure-finished) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{short}
    Signal to the window system that the application has finished handling
    Configure events it has received. Window Managers can use this to better
    synchronize the frame repaint with the application. GTK+ applications will
    automatically call this function when appropriate.
  @end{short}

  This function can only be called if
  gdk_window_enable_synchronized_configure() was called previously.

  Since 2.6"
  (window (g-object gdk-window)))

(export 'gdk-window-configure-finished)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_user_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_user_data" gdk-window-set-user-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[user_data]{user data}
  For most purposes this function is deprecated in favor of
  g_object_set_data(). However, for historical reasons GTK+ stores the
  GtkWidget that owns a GdkWindow as user data on the GdkWindow. So, custom
  widget implementations should use this function for that. If GTK+ receives
  an event for a GdkWindow, and the user data for the window is non-NULL, GTK+
  will assume the user data is a GtkWidget, and forward the event to that
  widget."
  (window (g-object gdk-window))
  (user-data :pointer))

(export 'gdk-window-set-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_override_redirect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_override_redirect" gdk-window-set-override-redirect)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[override_redirect]{TRUE if window should be override redirect}
  @begin{short}
    An override redirect window is not under the control of the window manager.
    This means it won't have a titlebar, won't be minimizable, etc. - it will be
    entirely under the control of the application. The window manager can't see
    the override redirect window at all.
  @end{short}

  Override redirect should only be used for short-lived temporary windows,
  such as popup menus. GtkMenu uses an override redirect window in its
  implementation, for example."
  (window (g-object gdk-window))
  (override-redirect :boolean))

(export 'gdk-window-set-override-redirect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_accept_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_accept_focus" gdk-window-set-accept-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[accept_focus]{TRUE if the window should receive input focus}
  @begin{short}
    Setting accept_focus to FALSE hints the desktop environment that the window
    doesn't want to receive input focus.
  @end{short}

  On X, it is the responsibility of the window manager to interpret this hint.
  ICCCM-compliant window manager usually respect it.

  Since 2.4"
  (window (g-object gdk-window))
  (accept-focus :boolean))

(export 'gdk-window-set-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_accept_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_accept_focus" gdk-window-get-accept-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow.}
  @return{whether or not the window should receive input focus.}
  @begin{short}
    Determines whether or not the desktop environment shuld be hinted that the
    window does not want to receive input focus.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_focus_on_map ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_focus_on_map" gdk-window-set-focus-on-map) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[focus_on_map]{TRUE if the window should receive input focus when
    mapped}
  @begin{short}
    Setting focus_on_map to FALSE hints the desktop environment that the window
    doesn't want to receive input focus when it is mapped. focus_on_map should
    be turned off for windows that aren't triggered interactively (such as
    popups from network activity).
  @end{short}

  On X, it is the responsibility of the window manager to interpret this hint.
  Window managers following the freedesktop.org window manager extension
  specification should respect it.

  Since 2.6"
  (window (g-object gdk-window))
  (focus-on-map :boolean))

(export 'gdk-window-set-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_focus_on_map ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_focus_on_map" gdk-window-get-focus-on-map) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow.}
  @begin{return}
    whether or not the window wants to receive input focus when it is
    mapped.
  @end{return}
  @begin{short}
    Determines whether or not the desktop environment should be hinted that the
    window does not want to receive input focus when it is mapped.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; enum GdkFilterReturn
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFilterReturn" gdk-filter-return
  (:export t
   :type-initializer "gdk_filter_return_get_type")
  (:continue 0)
  (:translate 1)
  (:remove 2))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-filter-return atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-filter-return atdoc:*external-symbols*)
 "@version{2013-4-5}
  @begin{short}
    Specifies the result of applying a GdkFilterFunc to a native event.
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
    @entry[:continue]{event not handled, continue processing.}
    @entry[:translate]{native event translated into a GDK event and stored in
      the event structure that was passed in.}
    @entry[:remove]{event handled, terminate processing.}
  @end{table}")

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
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[shape_region]{region of window to be non-transparent}
  @argument[offset_x]{X position of shape_region in window coordinates}
  @argument[offset_y]{Y position of shape_region in window coordinates}
  @begin{short}
    Makes pixels in window outside shape_region be transparent, so that the
    window may be nonrectangular.
  @end{short}

  If shape_region is NULL, the shape will be unset, so the whole window will
  be opaque again. offset_x and offset_y are ignored if shape_region is NULL.

  On the X11 platform, this uses an X server extension which is widely
  available on most common platforms, but not available on very old X servers,
  and occasionally the implementation will be buggy. On servers without the
  shape extension, this function will do nothing.

  This function works on both toplevel and child windows."
  (window (g-object gdk-window))
  (region cairo-region-t)
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_shapes" gdk-window-set-child-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  Sets the shape mask of window to the union of shape masks for all children
  of window, ignoring the shape mask of window itself. Contrast with
  gdk_window_merge_child_shapes() which includes the shape mask of window in
  the masks to be merged."
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_shapes" gdk-window-merge-child-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Merges the shape masks for any child windows into the shape mask for window.
    i. e. the union of all masks for window and its children will become the new
    mask for window. See gdk_window_shape_combine_region().
  @end{short}

  This function is distinct from gdk_window_set_child_shapes() because it
  includes window's shape mask in the set of shapes to be merged."
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_input_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_input_shape_combine_region"
           gdk-window-input-shape-combine-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[shape_region]{region of window to be non-transparent}
  @argument[offset_x]{X position of shape_region in window coordinates}
  @argument[offset_y]{Y position of shape_region in window coordinates}
  @begin{short}
    Like gdk_window_shape_combine_region(), but the shape applies only to event
    handling. Mouse events which happen while the pointer position corresponds
    to an unset bit in the mask will be passed on the window below window.
  @end{short}

  An input shape is typically used with RGBA windows. The alpha channel of the
  window defines which pixels are invisible and allows for nicely antialiased
  borders, and the input shape controls where the window is \"clickable\".

  On the X11 platform, this requires version 1.1 of the shape extension.

  On the Win32 platform, this functionality is not present and the function
  does nothing.

  Since 2.10"
  (window (g-object gdk-window))
  (shape-region cairo-region-t)
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-input-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_input_shapes" gdk-window-set-child-input-shapes)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Sets the input shape mask of window to the union of input shape masks for
    all children of window, ignoring the input shape mask of window itself.
    Contrast with gdk_window_merge_child_input_shapes() which includes the input
    shape mask of window in the masks to be merged.
  @end{short}

  Since 2.10"
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_input_shapes"
           gdk-window-merge-child-input-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    Merges the input shape masks for any child windows into the input shape mask
    for window. i.e. the union of all input masks for window and its children
    will become the new input mask for window. See
    gdk_window_input_shape_combine_region().
  @end{short}

  This function is distinct from gdk_window_set_child_input_shapes() because
  it includes window's input shape mask in the set of shapes to be merged.

  Since 2.10"
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_static_gravities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_static_gravities" gdk-window-set-static-gravities)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[use_static]{TRUE to turn on static gravity}
  @return{TRUE if the server supports static gravity}
  Set the bit gravity of the given window to static, and flag it so all
  children get static subwindow gravity. This is used if you are implementing
  scary features that involve deep knowledge of the windowing system. Don't
  worry about it unless you have to."
  (window (g-object gdk-window))
  (use-static :boolean))

(export 'gdk-window-set-static-gravities)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_title" gdk-window-set-title) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[title]{title of window}
  Sets the title of a toplevel window, to be displayed in the titlebar. If you
  haven't explicitly set the icon name for the window (using
  gdk_window_set_icon_name()), the icon name will be set to title as well.
  title must be in UTF-8 encoding (as with all user-readable strings in
  GDK/GTK+). title may not be NULL."
  (window (g-object gdk-window))
  (title :string))

(export 'gdk-window-set-title)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background" gdk-window-set-background) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @arg[color]{a GdkColor}
  @subheading{Warning}
    gdk_window_set_background has been deprecated since version 3.4 and should
    not be used in newly-written code. Use gdk_window_set_background_rgba()
    instead.

  @begin{short}
    Sets the background color of window. (However, when using GTK+, set the
    background of a widget with gtk_widget_modify_bg() - if you're an
    application - or gtk_style_set_background() - if you're implementing a
    custom widget.)
  @end{short}

  See also gdk_window_set_background_pattern()."
  (window (g-object gdk-window))
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-window-set-background)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background_rgba" gdk-window-set-background-rgba) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[rgba]{a GdkRGBA color}
  @begin{short}
    Sets the background color of window.
  @end{short}

  See also gdk_window_set_background_pattern()."
  (window (g-object gdk-window))
  (rgba (g-boxed-foreign gdk-rgba)))

(export 'gdk-window-set-background-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background_pattern ()
;;; ----------------------------------------------------------------------------

;; TODO: Implement the type cairo-pattern-t

(defctype cairo-pattern-t :pointer)

(defcfun ("gdk_window_set_background_pattern" gdk-window-set-background-pattern)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[pattern]{a pattern to use, or NULL}
  @begin{short}
    Sets the background of window.
  @end{short}

  A background of NULL means that the window will inherit its background form
  its parent window.

  The windowing system will normally fill a window with its background when
  the window is obscured then exposed."
  (window (g-object gdk-window))
  (pattern cairo-pattern-t))

(export 'gdk-window-set-background-pattern)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_background_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_background_pattern" gdk-window-get-background-pattern)
    cairo-pattern-t
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a window}
  @begin{return}
    The pattern to use for the background or NULL to use the parent's
    background.
  @end{return}
  @begin{short}
    Gets the pattern used to clear the background on window. If window does not
    have its own background and reuses the parent's, NULL is returned and you'll
    have to query it yourself.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-set-background-pattern)

;;; ----------------------------------------------------------------------------
;;; GDK_PARENT_RELATIVE
;;;
;;; #define GDK_PARENT_RELATIVE 1L
;;;
;;; A special value, indicating that the background for a window should be
;;; inherited from the parent window.
;;; ----------------------------------------------------------------------------

(defconstant +gdk-parent-relative+ 1)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_cursor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-cursor))

(defun gdk-window-set-cursor (window cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-13}
  @argument[window]{a @class{gdk-window} instance}
  @argument[cursor]{a cursor}
  @begin{short}
    Sets the default mouse pointer for a @class{gdk-window} instance.
  @end{short}
  Use @fun{gdk-cursor-new-for-display} or @fun{gdk-cursor-new-from-pixbuf} to
  create the @arg{cursor}. To make the @arg{cursor} invisible, use
  @code{:blank-cursor} of the @symbol{gdk-cursor-type} enumeration. Passing
  @code{nil} for the @arg{cursor} argument to @sym{gdk-window-set-cursor} means
  that @arg{window} will use the cursor of its parent window. Most windows
  should use this default.
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-new-from-pixbuf}
  @see-symbol{gdk-cursor-type}"
  (setf (gdk-window-cursor window) cursor))

(export 'gdk-window-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_cursor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-cursor))

(defun gdk-window-get-cursor (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-13}
  @argument[window]{a @class{gdk-window} instance}
  @return{A @class{gdk-cursor}, or @code{nil}. The returned object is owned by
    the @class{gdk-window} and should not be unreferenced directly. Use
    @fun{gdk-window-set-cursor} to unset the cursor of the window.}
  @begin{short}
    Retrieves a @class{gdk-cursor} pointer for the cursor currently set on the
    specified @class{gdk-window}, or @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified window, and it is using the cursor for its parent window.

  Since 2.18"
  (gdk-window-cursor window))

(export 'gdk-window-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_user_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_user_data" %gdk-window-get-user-data) :void
  (window (g-object gdk-window))
  (data :pointer))

(defun gdk-window-get-user-data (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[data]{return location for user data}
  Retrieves the user data for window, which is normally the widget that window
  belongs to. See gdk_window_set_user_data()."
  (with-foreign-object (data :pointer)
    (%gdk-window-get-user-data window data)
    (mem-ref data :pointer)))

(export 'gdk-window-get-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_geometry ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_geometry" %gdk-window-get-geometry) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gdk-window-get-geometry (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    @code{x} -- X coordinate of window (relative to its parent) @br{}
    @code{y} -- Y coordinate of window (relative to its parent) @br{}
    @code{width} -- width of window @br{}
    @code{height} -- height of window
  @end{return}
  @begin{short}
    Any of the return location arguments to this function may be NULL, if you
    aren't interested in getting the value of that field.
  @end{short}

  The X and Y coordinates returned are relative to the parent window of
  window, which for toplevels usually means relative to the window decorations
  (titlebar, etc.) rather than relative to the root window (screen-size
  background window).

  On the X11 platform, the geometry is obtained from the X server, so reflects
  the latest position of window; this may be out-of-sync with the position of
  window delivered in the most-recently-processed GdkEventConfigure.
  gdk_window_get_position() in contrast gets the position from the most recent
  configure event.

  @subheading{Note}
    If window is not a toplevel, it is much better to call
    gdk_window_get_position(), gdk_window_get_width() and
    gdk_window_get_height() instead, because it avoids the roundtrip to the X
    server and because these functions support the full 32-bit coordinate space,
    whereas gdk_window_get_geometry() is restricted to the 16-bit coordinates of
    X11."
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (%gdk-window-get-geometry window x y width height)
    (values (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gdk-window-get-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_geometry_hints ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_geometry_hints" gdk-window-set-geometry-hints) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[geometry]{geometry hints}
  @argument[geom_mask]{bitmask indicating fields of geometry to pay attention
    to}
  @begin{short}
    Sets the geometry hints for window. Hints flagged in geom_mask are set,
    hints not flagged in geom_mask are unset. To unset all hints, use a
    geom_mask of 0 and a geometry of NULL.
  @end{short}

  This function provides hints to the windowing system about acceptable sizes
  for a toplevel window. The purpose of this is to constrain user resizing,
  but the windowing system will typically (but is not required to) also
  constrain the current size of the window to the provided values and
  constrain programatic resizing via gdk_window_resize() or
  gdk_window_move_resize().

  Note that on X11, this effect has no effect on windows of type
  GDK_WINDOW_TEMP or windows where override redirect has been turned on via
  gdk_window_set_override_redirect() since these windows are not resizable by
  the user.

  Since you can't count on the windowing system doing the constraints for
  programmatic resizes, you should generally call gdk_window_constrain_size()
  yourself to determine appropriate sizes."
  (window (g-object gdk-window))
  (geometry (g-boxed-foreign gdk-geometry))
  (geometry-mask gdk-window-hints))

(export 'gdk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_width" gdk-window-get-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{The width of window}
  @begin{short}
    Returns the width of the given window.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.

  Since 2.24"
  (window (g-object gdk-window)))

(export 'gdk-window-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_height" gdk-window-get-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{The height of window}
  @begin{short}
    Returns the height of the given window.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.

  Since 2.24"
  (window (g-object gdk-window)))

(export 'gdk-window-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_list" gdk-window-set-icon-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{The GdkWindow toplevel window to set the icon of.}
  @argument[pixbufs]{A list of pixbufs, of different sizes.}
  Sets a list of icons for the window. One of these will be used to represent
  the window when it has been iconified. The icon is usually shown in an icon
  box or some sort of task bar. Which icon size is shown depends on the window
  manager. The window manager can scale the icon but setting several size
  icons can give better image quality since the window manager may only need
  to scale the icon by a small amount or not at all."
  (window (g-object gdk-window))
  (pixbufs (g-list (g-object gdk-pixbuf))))

(export 'gdk-window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_modal_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_modal_hint" gdk-window-set-modal-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{A toplevel GdkWindow}
  @argument[modal]{TRUE if the window is modal, FALSE otherwise.}
  @begin{short}
    The application can use this hint to tell the window manager that a certain
    window has modal behaviour. The window manager can use this information to
    handle modal windows in a special way.
  @end{short}

  You should only use this on windows for which you have previously called
  gdk_window_set_transient_for()"
  (window (g-object gdk-window))
  (modal :boolean))

(export 'gdk-window-set-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_modal_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_modal_hint" gdk-window-get-modal-hint) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{A toplevel GdkWindow.}
  @return{whether or not the window has the modal hint set.}
  @begin{short}
    Determines whether or not the window manager is hinted that window has modal
    behaviour.
  @end{short}

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_type_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_type_hint" gdk-window-set-type-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{A toplevel GdkWindow}
  @argument[hint]{A hint of the function this window will have}
  @begin{short}
    The application can use this call to provide a hint to the window manager
    about the functionality of a window. The window manager can use this
    information when determining the decoration and behaviour of the window.
  @end{short}

  The hint must be set before the window is mapped."
  (window (g-object gdk-window))
  (hint gdk-window-type-hint))

(export 'gdk-window-set-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_type_hint" gdk-window-get-type-hint)
    gdk-window-type-hint
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{A toplevel GdkWindow}
  @return{The type hint set for window}
  @begin{short}
    This function returns the type hint set for a window.
  @end{short}

  Since 2.10"
  (window (g-object gdk-window)))

(export 'gdk-window-get-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_taskbar_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_taskbar_hint" gdk-window-set-skip-taskbar-hint)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[skips_taskbar]{TRUE to skip the taskbar}
  @begin{short}
    Toggles whether a window should appear in a task list or window list. If a
    window's semantic type as specified with gdk_window_set_type_hint() already
    fully describes the window, this function should not be called in addition,
    instead you should allow the window to be treated according to standard
    policy for its semantic type.
  @end{short}

  Since 2.2"
  (window (g-object gdk-window))
  (skips-taskbar :boolean))

(export 'gdk-window-set-skip-taskbar-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_pager_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_pager_hint" gdk-window-set-skip-pager-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[skips_pager]{TRUE to skip the pager}
  @begin{short}
    Toggles whether a window should appear in a pager (workspace switcher, or
    other desktop utility program that displays a small thumbnail representation
    of the windows on the desktop). If a window's semantic type as specified
    with gdk_window_set_type_hint() already fully describes the window, this
    function should not be called in addition, instead you should allow the
    window to be treated according to standard policy for its semantic type.
  @end{short}

  Since 2.2"
  (window (g-object gdk-window))
  (skips-pager :boolean))

(export 'gdk-window-set-skip-pager-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_urgency_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_urgency_hint" gdk-window-urgency-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[urgent]{TRUE if the window is urgent}
  @begin{short}
    Toggles whether a window needs the user's urgent attention.
  @end{short}

  Since 2.8"
  (window (g-object gdk-window))
  (urgent :boolean))

(export 'gdk-window-set-urgency-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_position" %gdk-window-get-position) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-position (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    @code{x} -- X coordinate of window @br{}
    @code{y} -- Y coordinate of window
  @end{return}
  @begin{return}
    Obtains the position of the window as reported in the
    most-recently-processed GdkEventConfigure. Contrast with
    gdk_window_get_geometry() which queries the X server for the current window
    position, regardless of which events have been received or processed.
  @end{return}

  The position coordinates are relative to the window's parent window."
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-position window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-get-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_origin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_origin" %gdk-window-get-root-origin) :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-root-origin (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{return}
    @code{x} -- X position of window frame @br{}
    @code{y} -- Y position of window frame
  @end{return}
  Obtains the top-left corner of the window manager frame in root window
  coordinates."
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-root-origin window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-get-root-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_extents" %gdk-window-get-frame-extents) :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gdk-window-get-frame-extents (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @begin{return}
    @code{rect} -- rectangle with bounding box of the window frame
  @end{return}
  Obtains the bounding box of the window, including window manager
  titlebar/borders if any. The frame position is given in root window
  coordinates. To get the position of the window itself (rather than the
  frame) in root window coordinates, use gdk_window_get_origin()."
  (let ((rectangle (make-gdk-rectangle)))
    (%gdk-window-get-frame-extents window rectangle)
    rectangle))

(export 'gdk-window-get-frame-extents)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_origin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_origin" %gdk-window-get-origin) :int
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-origin (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{short}
    @code{x} -- X coordinate @br{}
    @code{y} -- Y coordinate
  @end{short}
  Obtains the position of a window in root window coordinates. (Compare with
  gdk_window_get_position() and gdk_window_get_geometry() which return the
  position of a window relative to its parent window.)"
  (with-foreign-objects ((x :int) (y :int))
    (%gdk-window-get-origin window x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gdk-window-get-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_coords" %gdk-window-get-root-coords) :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (root-x :int)
  (root-y :int))

(defun gdk-window-get-root-coords (window x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[x]{X coordinate in window}
  @argument[y]{Y coordinate in window}
  @begin{return}
    @code{root_x} -- return for X coordinate @br{}
    @code{root_y} -- return  for Y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window position in root window coordinates. This
    is similar to gdk_window_get_origin() but allows you go pass in any position
    in the window, not just the origin.
  @end{short}

  Since 2.18"
  (with-foreign-objects ((root-x :int) (root-y :int))
    (%gdk-window-get-root-coords window x y root-x root-y)
    (values (mem-ref root-x :int)
            (mem-ref root-y :int))))

(export 'gdk-window-get-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_pointer" %gdk-window-get-pointer)
    (g-object gdk-window)
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-get-pointer (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    @code{win} -- the window containing the pointer (as with
                  gdk_window_at_pointer()), or NULL if the window containing
                   the pointer isn't known to GDK @br{}
    @code{x} -- X coordinate of pointer or NULL to not return the X
                coordinate @br{}
    @code{y} -- Y coordinate of pointer or NULL to not return the Y
                coordinate @br{}
    @code{mask} -- modifier mask or NULL to not return the modifier mask
  @end{return}
  @subheading{Warning}
    gdk_window_get_pointer has been deprecated since version 3.0 and should not
    be used in newly-written code. Use gdk_window_get_device_position() instead.

  @begin{short}
    Obtains the current pointer position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}"
  (with-foreign-objects ((x :int) (y :int) (mask 'gdk-modifier-type))
    (let ((w (%gdk-window-get-pointer window x y mask)))
      (values w
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-get-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_position" %gdk-window-get-device-position)
    (g-object gdk-window)
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer gdk-modifier-type)))

(defun gdk-window-get-device-position (window device)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow.}
  @argument[device]{pointer GdkDevice to query to.}
  @begin{return}
    @code{win} -- The window underneath device (as with
                  gdk_device_get_window_at_position()), or NULL if the window
                  is not known to GDK. @br{}
    @code{x} -- the X coordinate of device, or NULL @br{}
    @code{y} -- the Y coordinate of device, or NULL @br{}
    @code{mask} -- the modifier mask, or NULL
  @end{return}
  @begin{short}
    Obtains the current device position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}

  Since 3.0"
  (with-foreign-objects ((x :int) (y :int) (mask 'gdk-modifier-type))
    (let ((win (%gdk-window-get-device-position window device x y mask)))
      (values win
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'gdk-modifier-type)))))

(export 'gdk-window-get-device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_parent" gdk-window-get-parent) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{parent of window}
  @begin{short}
    Obtains the parent of window, as known to GDK. Does not query the X server;
    thus this returns the parent as passed to gdk_window_new(), not the actual
    parent. This should never matter unless you're using Xlib calls mixed with
    GDK calls on the X11 platform. It may also matter for toplevel windows,
    because the window manager may choose to reparent them.
  @end{short}

  Note that you should use gdk_window_get_effective_parent() when writing
  generic code that walks up a window hierarchy, because
  gdk_window_get_parent() will most likely not do what you expect if there are
  offscreen windows in the hierarchy."
  (window (g-object gdk-window)))

(export 'gdk-window-get-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_toplevel" gdk-window-get-toplevel)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{the toplevel window containing window}
  @begin{short}
    Gets the toplevel window that's an ancestor of window.
  @end{short}

  Any window type but GDK_WINDOW_CHILD is considered a toplevel window, as is
  a GDK_WINDOW_CHILD window that has a root window as parent.

  Note that you should use gdk_window_get_effective_toplevel() when you want
  to get to a window's toplevel as seen on screen, because
  gdk_window_get_toplevel() will most likely not do what you expect if there
  are offscreen windows in the hierarchy."
  (window (g-object gdk-window)))

(export 'gdk-window-get-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children" gdk-window-get-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{list of child windows inside window}
  @begin{short}
    Gets the list of children of window known to GDK. This function only returns
    children created via GDK, so for example it's useless when used with the
    root window; it only returns windows an application created itself.
  @end{short}

  The returned list must be freed, but the elements in the list need not be."
  (window (g-object gdk-window)))

(export 'gdk-window-get-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_peek_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_peek_children" gdk-window-peek-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{a reference to the list of child windows in window}
  Like gdk_window_get_children(), but does not copy the list of children, so
  the list does not need to be freed."
  (window (g-object gdk-window)))

(export 'gdk-window-peek-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_events" gdk-window-get-events) gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{event mask for window}
  Gets the event mask for window for all master input devices. See
  gdk_window_set_events()."
  (window (g-object gdk-window)))

(export 'gdk-window-get-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_events" gdk-window-set-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[event_mask]{event mask for window}
  The event mask for a window determines which events will be reported for
  that window from all master input devices. For example, an event mask
  including GDK_BUTTON_PRESS_MASK means the window should report button press
  events. The event mask is the bitwise OR of values from the GdkEventMask
  enumeration."
  (window (g-object gdk-window))
  (event-mask gdk-event-mask))

(export 'gdk-window-set-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_name" gdk-window-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[name]{name of window while iconified (minimized)}
  @begin{short}
    Windows may have a name used while minimized, distinct from the name they
    display in their titlebar. Most of the time this is a bad idea from a user
    interface standpoint. But you can set such a name with this function, if you
    like.
  @end{short}

  After calling this with a non-NULL name, calls to gdk_window_set_title()
  will not update the icon title.

  Using NULL for name unsets the icon title; further calls to
  gdk_window_set_title() will again update the icon title as well."
  (window (g-object gdk-window))
  (name :string))

(export 'gdk-window-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_transient_for ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_transient_for" gdk-window-set-transient-for) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[parent]{another toplevel GdkWindow}
  @begin{short}
    Indicates to the window manager that window is a transient dialog associated
    with the application window parent. This allows the window manager to do
    things like center window on parent and keep window above parent.
  @end{short}

  See gtk_window_set_transient_for() if you're using GtkWindow or GtkDialog."
  (window (g-object gdk-window))
  (parent (g-object gdk-window)))

(export 'gdk-window-set-transient-for)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_role ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_role" gdk-window-set-role) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[role]{a string indicating its role}
  @begin{short}
    When using GTK+, typically you should use gtk_window_set_role() instead of
    this low-level function.
  @end{short}

  The window manager and session manager use a window's role to distinguish it
  from other kinds of window in the same application. When an application is
  restarted after being saved in a previous session, all windows with the same
  title and role are treated as interchangeable. So if you have two windows
  with the same title that should be distinguished for session management
  purposes, you should set the role on those windows. It doesn't matter what
  string you use for the role, as long as you have a different role for each
  non-interchangeable kind of window."
  (window (g-object gdk-window))
  (role :string))

(export 'gdk-window-set-role)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_startup_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_startup_id" gdk-window-set-startup-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[startup_id]{a string with startup-notification identifier}
  @begin{short}
    When using GTK+, typically you should use gtk_window_set_startup_id()
    instead of this low-level function.
  @end{short}

  Since 2.12"
  (window (g-object gdk-window))
  (startup-id :string))

(export 'gdk-window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_group" gdk-window-set-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[leader]{group leader window, or NULL to restore the default group
    leader window}
  @begin{short}
    Sets the group leader window for window. By default, GDK sets the group
    leader for all toplevel windows to a global window implicitly created by
    GDK. With this function you can override this default.
  @end{short}

  The group leader window allows the window manager to distinguish all windows
  that belong to a single application. It may for example allow users to
  minimize/unminimize all windows belonging to an application at once. You
  should only set a non-default group window if your application pretends to
  be multiple applications."
  (window (g-object gdk-window))
  (leader (g-object gdk-window)))

(export 'gdk-window-set-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_group" gdk-window-get-group) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @return{the group leader window for window}
  @begin{return}
    Returns the group leader window for window. See gdk_window_set_group().
  @end{return}

  Since 2.4"
  (window (g-object gdk-window)))

(export 'gdk-window-get-group)

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-decoration atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-decoration atdoc:*external-symbols*)
 "@version{2013-4-5}
  @begin{short}
    These are hints originally defined by the Motif toolkit. The window manager
    can use them when determining how to decorate the window. The hint must be
    set before mapping the window.
  @end{short}
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
    @entry[:all]{all decorations should be applied.}
    @entry[:border]{a frame should be drawn around the window.}
    @entry[:resizeh]{the frame should have resize handles.}
    @entry[:title]{a titlebar should be placed above the window.}
    @entry[:menu]{a button for opening a menu should be included.}
    @entry[:minimize]{a minimize button should be included.}
    @entry[:maximize]{a maximize button should be included.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_decorations ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_decorations" gdk-window-set-decorations) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[decorations]{decoration hint mask}
  @begin{short}
    \"Decorations\" are the features the window manager adds to a toplevel
    GdkWindow. This function sets the traditional Motif window manager hints
    that tell the window manager which decorations you would like your window to
    have. Usually you should use gtk_window_set_decorated() on a GtkWindow
    instead of using the GDK function directly.
  @end{short}

  The decorations argument is the logical OR of the fields in the
  GdkWMDecoration enumeration. If GDK_DECOR_ALL is included in the mask, the
  other bits indicate which decorations should be turned off. If GDK_DECOR_ALL
  is not included, then the other bits indicate which decorations should be
  turned on.

  Most window managers honor a decorations hint of 0 to disable all
  decorations, but very few honor all possible combinations of bits."
  (window (g-object gdk-window))
  (decorations gdk-wm-decoration))

(export 'gdk-window-set-decorations)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_decorations ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_decorations" %gdk-window-get-decorations) :boolean
  (window (g-object gdk-window))
  (decorations (:pointer gdk-wm-decoration)))

(defun gdk-window-get-decorations (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{The toplevel GdkWindow to get the decorations from}
  @argument[decorations]{The window decorations will be written here.}
  @return{TRUE if the window has decorations set, FALSE otherwise.}
  Returns the decorations set on the GdkWindow with
  gdk_window_set_decorations()."
  (with-foreign-object (decorations 'gdk-wm-decoration)
    (%gdk-window-get-decorations window decorations)
    (mem-ref decorations 'gdk-wm-decoration)))

(export 'gdk-window-get-decorations)

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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-function atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-function atdoc:*external-symbols*)
 "@version{2013-4-5}
  @begin{short}
    These are hints originally defined by the Motif toolkit. The window manager
    can use them when determining the functions to offer for the window. The
    hint must be set before mapping the window.
  @end{short}
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
    @entry[:all]{all functions should be offered.}
    @entry[:resize]{the window should be resizable.}
    @entry[:move]{the window should be movable.}
    @entry[:minimize]{the window should be minimizable.}
    @entry[:maximize]{the window should be maximizable.}
    @entry[:close]{the window should be closable.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_functions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_functions" gdk-window-set-functions) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a toplevel GdkWindow}
  @argument[functions]{bitmask of operations to allow on window}
  @begin{short}
    Sets hints about the window management functions to make available via
    buttons on the window frame.
  @end{short}

  On the X backend, this function sets the traditional Motif window manager
  hint for this purpose. However, few window managers do anything reliable or
  interesting with this hint. Many ignore it entirely.

  The functions argument is the logical OR of values from the GdkWMFunction
  enumeration. If the bitmask includes GDK_FUNC_ALL, then the other bits
  indicate which functions to disable; if it doesn't include GDK_FUNC_ALL, it
  indicates which functions to enable."
  (window (g-object gdk-window))
  (functions gdk-wm-function))

(export 'gdk-window-set-functions)

;;; ----------------------------------------------------------------------------
;;; gdk_get_default_root_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_default_root_window" gdk-get-default-root-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @return{the default root window}
  Obtains the root window (parent all other windows are inside) for the
  default display and screen.")

(export 'gdk-get-default-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_support_multidevice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_support_multidevice"
           gdk-window-get-support-multidevice) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-4}
  @argument[window]{a GdkWindow.}
  @return{TRUE if the window handles multidevice features.}
  @begin{short}
    Returns TRUE if the window is aware of the existence of multiple devices.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window)))

(export 'gdk-window-get-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_support_multidevice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_support_multidevice"
           gdk-window-set-support-multidevice) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow.}
  @argument[support_multidevice]{TRUE to enable multidevice support in window.}
  @begin{short}
    This function will enable multidevice features in window.
  @end{short}

  Multidevice aware windows will need to handle properly multiple, per device
  enter/leave events, device grabs and grab ownerships.

  Since 3.0"
  (window (g-object gdk-window))
  (support-multidevice :boolean))

(export 'gdk-window-set-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_cursor" gdk-window-get-device-cursor)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow.}
  @argument[device]{a master, pointer GdkDevice.}
  @begin{return}
    a GdkCursor, or NULL. The returned object is owned by the GdkWindow and
    should not be unreferenced directly. Use gdk_window_set_cursor() to
    unset the cursor of the window.
  @end{return}
  @begin{short}
    Retrieves a GdkCursor pointer for the device currently set on the specified
    GdkWindow, or NULL. If the return value is NULL then there is no custom
    cursor set on the specified window, and it is using the cursor for its
    parent window.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-get-device-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_device_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_device_cursor" gdk-window-set-device-cursor) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a Gdkwindow}
  @argument[device]{a master, pointer GdkDevice}
  @argument[cursor]{a GdkCursor}
  @begin{short}
    Sets a specific GdkCursor for a given device when it gets inside window. Use
    gdk_cursor_new_for_display() or gdk_cursor_new_from_pixbuf() to create the
    cursor. To make the cursor invisible, use GDK_BLANK_CURSOR. Passing NULL for
    the cursor argument to gdk_window_set_cursor() means that window will use
    the cursor of its parent window. Most windows should use this default.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (cursor (g-object gdk-cursor)))

(export 'gdk-window-set-device-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_events" gdk-window-get-device-events)
    gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow.}
  @argument[device]{a GdkDevice.}
  @return{device event mask for window}
  @begin{short}
    Returns the event mask for window corresponding to an specific device.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-get-device-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_device_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_device_events" gdk-window-set-device-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[device]{GdkDevice to enable events for.}
  @argument[event_mask]{event mask for window}
  @begin{short}
    Sets the event mask for a given device (Normally a floating device, not
    attached to any visible pointer) to window. For example, an event mask
    including GDK_BUTTON_PRESS_MASK means the window should report button press
    events. The event mask is the bitwise OR of values from the GdkEventMask
    enumeration.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window))
  (device (g-object gdk-device))
  (event-mask gdk-event-mask))

(export 'gdk-window-set-device-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_source_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_source_events" gdk-window-get-source-events)
    gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[source]{a GdkInputSource to define the source class.}
  @return{source event mask for window}
  Returns the event mask for window corresponding to the device class
  specified by source."
  (window (g-object gdk-window))
  (source gdk-input-source))

(export 'gdk-window-get-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_source_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_source_events" gdk-window-set-source-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[source]{a GdkInputSource to define the source class.}
  @argument[event_mask]{event mask for window}
  @begin{short}
    Sets the event mask for any floating device (i.e. not attached to any
    visible pointer) that has the source defined as source. This event mask will
    be applied both to currently existing, newly added devices after this call,
    and devices being attached/detached.
  @end{short}

  Since 3.0"
  (window (g-object gdk-window))
  (source gdk-input-source)
  (event-mask gdk-event-mask))

(export 'gdk-window-set-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_surface" gdk-offscreen-window-get-surface)
    cairo-surface-t
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{The offscreen surface, or NULL if not offscreen.}
  Gets the offscreen surface that an offscreen window renders into. If you
  need to keep this around over window resizes, you need to add a reference to
  it."
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_set_embedder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_set_embedder" gdk-offscreen-window-set-embedder)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @argument[embedder]{the GdkWindow that window gets embedded in}
  @begin{short}
    Sets window to be embedded in embedder.
  @end{short}

  To fully embed an offscreen window, in addition to calling this function, it
  is also necessary to handle the \"pick-embedded-child\" signal on the embedder
  and the \"to-embedder\" and \"from-embedder\" signals on window.

  Since 2.18"
  (window (g-object gdk-window))
  (embedder (g-object gdk-window)))

(export 'gdk-offscreen-window-set-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_embedder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_embedder" gdk-offscreen-get-window-embedder)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @begin{return}
    the embedding GdkWindow, or NULL if window is not an mbedded offscreen
    window
  @end{return}
  @begin{short}
    Gets the window that window is embedded in.
  @end{short}

  Since 2.18"
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_window_geometry_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_geometry_changed" gdk-window-geometry-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{an embedded offscreen GdkWindow}
  @begin{short}
    This function informs GDK that the geometry of an embedded offscreen window
    has changed. This is necessary for GDK to keep track of which offscreen
    window the pointer is in.
  @end{short}

  Since 2.18"
  (window (g-object gdk-window)))

(export 'gdk-window-geometry-changed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_from_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_from_parent" %gdk-window-coords-from-parent) :void
  (window (g-object gdk-window))
  (parent-x :double)
  (parent-y :double)
  (x :double)
  (y :double))

(defun gdk-window-coords-from-parent (window parent-x parent-y)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a child window}
  @argument[parent_x]{X coordinate in parent's coordinate system}
  @argument[parent_y]{Y coordinate in parent's coordinate system}
  @begin{return}
    @code{x} -- X coordinate in child's coordinate system @br{}
    @code{y} -- Y coordinate in child's coordinate system
  @end{return}
  @begin{short}
    Transforms window coordinates from a parent window to a child window, where
    the parent window is the normal parent as returned by
    gdk_window_get_parent() for normal windows, and the window's embedder as
    returned by gdk_offscreen_window_get_embedder() for offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to subtracting the
  return values of gdk_window_get_position() from the parent coordinates. For
  offscreen windows however (which can be arbitrarily transformed), this
  function calls the GdkWindow::from-embedder: signal to translate the
  coordinates.

  You should always use this function when writing generic code that walks
  down a window hierarchy.

  See also: gdk_window_coords_to_parent()

  Since 2.22"
  (with-foreign-objects ((x :double) (y :double))
    (%gdk-window-coords-from-parent window parent-x parent-y x y)
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
  (parent-x :double)
  (parent-y :double))

(defun gdk-window-coords-to-parent (window x y)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a child window}
  @argument[x]{X coordinate in child's coordinate system}
  @argument[y]{Y coordinate in child's coordinate system}
  @begin{return}
    @code{parent_x} -- X coordinate in parent's coordinate system, or NULL @br{}
    @code{parent_y} -- Y coordinate in parent's coordinate system, or NULL
  @end{return}
  @begin{short}
    Transforms window coordinates from a child window to its parent window,
    where the parent window is the normal parent as returned by
    gdk_window_get_parent() for normal windows, and the window's embedder as
    returned by gdk_offscreen_window_get_embedder() for offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to adding the return
  values of gdk_window_get_position() to the child coordinates. For offscreen
  windows however (which can be arbitrarily transformed), this function calls
  the GdkWindow::to-embedder: signal to translate the coordinates.

  You should always use this function when writing generic code that walks up
  a window hierarchy.

  See also: gdk_window_coords_from_parent()

  Since 2.22"
  (with-foreign-objects ((parent-x :double) (parent-y :double))
    (%gdk-window-coords-to-parent window x y parent-x parent-y)
    (values (mem-ref parent-x :double)
            (mem-ref parent-y :double))))

(export 'gdk-window-coords-to-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_parent" gdk-window-get-effective-parent)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{effective parent of window}
  @begin{short}
    Obtains the parent of window, as known to GDK. Works like
    gdk_window_get_parent() for normal windows, but returns the window's
    embedder for offscreen windows.
  @end{short}

  See also: gdk_offscreen_window_get_embedder()

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_toplevel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_toplevel" gdk-window-get-effective-toplevel)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a GdkWindow}
  @return{the effective toplevel window containing window}
  @begin{short}
    Gets the toplevel window that's an ancestor of window.
  @end{short}

  Works like gdk_window_get_toplevel(), but treats an offscreen window's
  embedder as its parent, using gdk_window_get_effective_parent().

  See also: gdk_offscreen_window_get_embedder()

  Since 2.22"
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-toplevel)

;;; --- End of file gdk.window.lisp --------------------------------------------
