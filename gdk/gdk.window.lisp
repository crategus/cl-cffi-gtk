;;; ----------------------------------------------------------------------------
;;; gdk.window.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;;     gdk_window_get_background_pattern
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

(setf (documentation 'gdk-window 'type)
 "@version{2013-6-5}
  @begin{short}
    Onscreen display areas in the target window system.

    A @sym{gdk-window} object is a usually rectangular region on the screen.
    It is a low-level object, used to implement high-level objects such as
    @class{gtk-widget} and @class{gtk-window} widgets on the GTK+ level. A
    @class{gtk-window} widget is a toplevel window, the thing a user might think
    of as a \"window\" with a titlebar and so on; a @class{gtk-window} widget
    may contain many @sym{gdk-window} objects. For example, each
    @class{gtk-button} widget has a @sym{gdk-window} object associated with it.
  @end{short}

  @subheading{Composited Windows}
    Normally, the windowing system takes care of rendering the contents of a
    child window onto its parent window. This mechanism can be intercepted by
    calling the function @fun{gdk-window-set-composited} on the child window.
    For a composited window it is the responsibility of the application to
    render the window contents at the right spot.

    @b{Example:} Composited windows

    FIXME: MISSING XINCLUDE CONTENT

    In the example Example, \"Composited windows\", a button is placed inside of
    an event box inside of a window. The event box is set as composited and
    therefore is no longer automatically drawn to the screen.

    When the contents of the event box change, an expose event is generated on
    its parent window which, in this case, belongs to the toplevel
    @class{gtk-window} widget. The expose handler for this widget is
    responsible for emerging the changes back on the screen in the way that it
    wishes.

    In our case, we merge the contents with a 50 % transparency. We also set the
    background colour of the window to red. The effect is that the background
    shows through the button.

  @subheading{Offscreen Windows}
    Offscreen windows are more general than composited windows, since they allow
    not only to modify the rendering of the child window onto its parent, but
    also to apply coordinate transformations.

    To integrate an offscreen window into a window hierarchy, one has to call
    the @fun{gdk-offscreen-window-set-embedder} function and handle a number of
    signals. The \"pick-embedded-child\" signal on the embedder window is used
    to select an offscreen child at given coordinates, and the \"to-embedder\"
    and \"from-embedder\" signals on the offscreen window are used to translate
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
        @entry[Returns]{The newly created @symbol{cairo-surface-t} for the
          offscreen window.}
      @end{table}
      Since 3.0

    @subheading{The \"from-embedder\" signal}
      @begin{pre}
 lambda (window embedder-x embedder-y offscreen-x offscreen-y)   : Run Last
      @end{pre}
      The \"from-embedder\" signal is emitted to translate coordinates in the
      embedder of an offscreen window to the offscreen window.
      See also the \"to-embedder\" signal.
      @begin[code]{table}
        @entry[window]{The offscreen window on which the signal is emitted.}
        @entry[embedder-x]{x coordinate in the embedder window.}
        @entry[embedder-y]{y coordinate in the embedder window.}
        @entry[offscreen-x]{Return location for the x coordinate in the
          offscreen window.}
        @entry[offscreen-y]{Return location for the y coordinate in the
          offscreen window.}
      @end{table}
      Since 2.18

    @subheading{The \"pick-embedded-child\" signal}
      @begin{pre}
 lambda (window x y)
      @end{pre}
      The \"pick-embedded-child\" signal is emitted to find an embedded child
      at the given position.
      @begin[code]{table}
        @entry[window]{The window on which the signal is emitted.}
        @entry[x]{x coordinate in the window.}
        @entry[y]{y coordinate in the window.}
        @entry[Returns]{The @class{gdk-window} of the embedded child at x, y,
          or @code{nil}.}
      @end{table}
      Since 2.18

    @subheading{The \"to-embedder\" signal}
      @begin{pre}
 lambda (window offscreen-x offscreen-y embedder-x embedder-y)   : Run Last
      @end{pre}
      The \"to-embedder\" signal is emitted to translate coordinates in an
      offscreen window to its embedder.
      See also the \"from-embedder\" signal.
      @begin[code]{table}
        @entry[window]{The offscreen window on which the signal is emitted.}
        @entry[offscreen-x]{x coordinate in the offscreen window.}
        @entry[offscreen-y]{y coordinate in the offscreen window.}
        @entry[embedder-x]{Return location for the x coordinate in the embedder
          window.}
        @entry[embedder-y]{Return location for the y coordinate in the embedder
          window.}
      @end{table}
      Since 2.18
  @end{dictionary}
  @see-slot{gdk-window-cursor}
  @see-class{gtk-widget}
  @see-class{gtk-window}
  @see-function{gdk-window-set-composited}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cursor" 'gdk-window) 't)
 "The @code{cursor} property of type @class{gdk-cursor} (Read / Write) @br{}
  The mouse pointer for a @sym{gdk-window}. See the functions
  @fun{gdk-window-set-cursor} and @fun{gdk-window-get-cursor} for details. @br{}
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
 "@version{2013-8-23}
  Accessor of the slot @code{\"cursor\"} of the @class{gdk-window} class.
  @see-class{gdk-window}
  @see-function{gdk-window-get-cursor}
  @see-function{gdk-window-set-cursor}")

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-type atdoc:*external-symbols*)
 "@version{2013-8-23}
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
    @entry[:root]{Root window; this window has no parent, covers the entire
      screen, and is created by the window system.}
    @entry[:toplevel]{Toplevel window, used to implement @class{gtk-window}.}
    @entry[:child]{Child window, used to implement e. g. @class{gtk-entry}.}
    @entry[:temp]{Override redirect temporary window, used to implement
      @class{gtk-menu}.}
    @entry[:foreign]{Foreign window.}
    @entry[:offscreen]{Offscreen window, see the section called
      \"Offscreen Windows\". Since 2.18.}
  @end{table}
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-class{gtk-entry}")

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
 "@version{2013-8-23}
  @begin{short}
    @code{:input-output} windows are the standard kind of window you might
    expect. Such windows receive events and are also displayed on screen.
    @code{:input-only} windows are invisible; they are usually placed above
    other windows in order to trap or filter the events. You cannot draw on
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
 "@version{2013-8-23}
  @begin{short}
    Used to indicate which fields of a @class{gdk-geometry} structure should be
    paid attention to. Also, the presence/absence of @code{:pos},
    @code{:user-pos}, and @code{:user-size} is significant, though they do not
    directly refer to @class{gdk-geometry} fields. @code{:user-pos} will be set
    automatically by @class{gtk-window} if you call the function
    @fun{gtk-window-move}. @code{:user-pos} and @code{:user-size} should be set
    if the user specified a size/position using a - geometry command-line
    argument; the function @fun{gtk-window-parse-geometry} automatically sets
    these flags.
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
  @see-class{gdk-geometry}
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
 "@version{2013-8-23}
  @begin{short}
    Defines the reference point of a window and the meaning of coordinates
    passed to the function @fun{gtk-window-move}. See the function
    @fun{gtk-window-move} and the \"implementation notes\" section of the
    Extended Window Manager Hints specification for more details.
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
  (win-gravity gdk-gravity :initform :north-west))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-geometry 'type)
 "@version{2013-9-16}
  @begin{short}
    The @sym{gdk-geometry} structure gives the window manager information about
    a window's geometry constraints. Normally you would set these on the GTK+
    level using the function @fun{gtk-window-set-geometry-hints}.
    @class{gtk-window} then sets the hints on the @class{gdk-window} it creates.
  @end{short}

  The function @fun{gdk-window-set-geometry-hints} expects the hints to be fully
  valid already and simply passes them to the window manager; in contrast, the
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
  should be the geometry widget; @class{gtk-window} will then automatically set
  the base size to the size of other widgets in the terminal window, such as the
  menubar and scrollbar. Then, the @code{width-inc} and @code{height-inc} fields
  should be set to the size of one character in the terminal. Finally, the base
  size should be set to the size of one character. The net effect is that the
  minimum size of the terminal will have a 1 x 1 character terminal area, and
  only terminal sizes on the \"character grid\" will be allowed.

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
  fields; these contain a width/height ratio as a floating point number. If a
  geometry widget is set, the aspect applies to the geometry widget rather than
  the entire window. The most common use of these hints is probably to set
  @code{min-aspect} and @code{max-aspect} to the same value, thus forcing the
  window to keep a constant aspect ratio.
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
  (win-gravity gdk-gravity :initform :north-west))
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
    @entry[win-gravity]{Window gravity, see the function
      @fun{gtk-window-gravity}.}
  @end{table}
  @see-constructor{make-gdk-geometry}
  @see-constructor{copy-gdk-geometry}
  @see-slot{gdk-geometry-min-width}
  @see-slot{gdk-geometry-min-height}
  @see-slot{gdk-geometry-max-width}
  @see-slot{gdk-geometry-max-height}
  @see-slot{gdk-geometry-base-width}
  @see-slot{gdk-geometry-base-height}
  @see-slot{gdk-geometry-width-increment}
  @see-slot{gdk-geometry-height-increment}
  @see-slot{gdk-geometry-min-aspect}
  @see-slot{gdk-geometry-max-aspect}
  @see-slot{gdk-geometry-win-gravity}
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gdk-window-set-geometry-hints}
  @see-function{gtk-window-set-geometry-hints}
  @see-function{gtk-window-gravity}")

(export (boxed-related-symbols 'gdk-geometry))

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for GdkGeometry
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-geometry 'function)
 "@version{2013-8-23}
  @argument[instance]{a @class{gdk-geometry} structure}
  Copy constructor of a @class{gdk-geometry} structure.
  @see-class{gdk-geometry}
  @see-function{copy-gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-geometry 'function)
 "@version{2013-4-5}
    @argument[min-width]{minimum width of window or -1 to use requisition,
      with @class{gtk-window} only}
    @argument[min-height]{minimum height of window or -1 to use requisition,
      with @class{gtk-window} only}
    @argument[max-width}{maximum width of window or -1 to use requisition,
      with @class{gtk-window} only}
    @argument[max-height]{maximum height of window or -1 to use requisition,
      with @class{gtk-window} only}
    @argument[base-width]{allowed window widths are @code{base-width +
      width-inc * N} where @code{N} is any integer, -1 is allowed with
      @class{gtk-window}}
    @argument[base-height]{allowed window widths are @code{base-height +
      height-inc * N} where @code{N} is any integer, -1 allowed with
      @class{gtk-window}}
    @argument[width-increment]{width resize increment}
    @argument[height-increment]{height resize increment}
    @argument[min-aspect]{minimum width/height ratio}
    @argument[max-aspect]{maximum width/height ratio}
    @argument[win-gravity]{window gravity, see the function
      @fun{gtk-window-gravity}}
  @begin{short}
    Creates a @class{gdk-geometry} structure.
  @end{short}
  @see-class{gdk-geometry}
  @see-function{copy-gdk-geometry}
  @see-function{gtk-window-gravity}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkGeometry structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-width 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{min-width} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-height 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{min-height} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-width 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{max-width} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-height 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{max-height} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-base-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-base-width 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{base-width} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-base-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-base-height 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{base-height} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-width-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-width-increment 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{width-increment} of the @class{gdk-geometry}
  structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-height-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-height-increment 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{height-increment} of the @class{gdk-geometry}
  structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-min-aspect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-min-aspect 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{min-aspect} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-max-aspect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-max-aspect 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{max-aspect} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-geometry-win-gravity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-geometry-win-gravity 'function)
 "@version{2013-9-16}
  Accessor of the slot @code{gravity} of the @class{gdk-geometry} structure.
  @see-class{gdk-geometry}")

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
 "@version{2013-8-23}
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
 "@version{2013-8-23}
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
      e. g. a context menu.}
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-window-attr 'type)
 "@version{2013-8-23}
  @begin{short}
    Attributes to use for a newly created window.
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
    @entry[title]{Title of the window for toplevel windows.}
    @entry[event-mask]{Event mask, see the function
      @fun{gdk-window-set-events}.}
    @entry[x]{x coordinate relative to parent window, see the function
      @fun{gdk-window-move}.}
    @entry[y]{y coordinate relative to parent window, see the function
      @fun{gdk-window-move}.}
    @entry[width]{Width of window.}
    @entry[height]{Height of window.}
    @entry[window-class]{@code{:input-output} normal window or
      @code{:input-only} invisible window that receives events.}
    @entry[visual]{A @class{gdk-visual} for window.}
    @entry[window-type]{Type of window.}
    @entry[cursor]{Cursor for the window, see the function
      @fun{gdk-window-set-cursor}.}
    @entry[wmclass-name]{Do not use,  see the function
      @fun{gtk-window-set-wmclass}.}
    @entry[wmclass-class]{Do not use, see the function
      @fun{gtk-window-set-wmclass}.}
    @entry[override-redirect]{@em{True} to bypass the window manager.}
    @entry[type-hint]{A hint of the function of the window.}
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
  @see-constructor{make-gdk-window-attr}
  @see-class{gdk-visual}
  @see-function{gdk-window-move}
  @see-function{gdk-window-set-cursor}
  @see-function{gdk-window-set-events}
  @see-function{gtk-window-set-wmclass}")

(export (boxed-related-symbols 'gdk-window-attr))

;;; ----------------------------------------------------------------------------
;;;
;;; Constructors for the GdkWindowAttr structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gdk-window-attr 'function)
 "@version{2013-8-23}
  @argument[instance]{a @class{gdk-window-attr} structure}
  Copy constructor of a @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gdk-window-attr 'function)
 "@version{2013-8-23}
  @argument[title]{title of the window, for toplevel windows}
  @argument[event-mask]{event mask, see the function
    @fun{gdk-window-set-events}}
  @argument[x]{x coordinate relative to parent window, see the function
    @fun{gdk-window-move}}
  @argument[y]{y coordinate relative to parent window, see the function
    @fun{gdk-window-move}}
  @argument[width]{width of window}
  @argument[height]{height of window}
  @argument[window-class]{@code{:input-output} normal window or
    @code{:input-only} invisible window that receives events}
  @argument[visual]{a @class{gdk-visual} for window}
  @argument[window-type]{type of window}
  @argument[cursor]{cursor for the window, see the function
    @fun{gdk-window-set-cursor}}
  @argument[wmclass-name]{do not use, see the function
    @fun{gtk-window-set-wmclass}}
  @argument[wmclass-class]{do not use, see the function
    @fun{gtk-window-set-wmclass}}
  @argument[override-redirect]{@em{true} to bypass the window manager}
  @argument[type-hint]{a hint of the function of the window}
  @begin{short}
    Creates a @class{gdk-window-attr} structure.
  @end{short}
  @see-class{gdk-window-attr}
  @see-function{gdk-window-move}
  @see-function{gdk-window-set-cursor}
  @see-function{gdk-window-set-events}
  @see-function{gtk-window-set-wmclass}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of the GdkWindowAttr structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-title 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{title} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-event-mask atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-event-mask 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{event-mask} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-x atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-x 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{x} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-y atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-y 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{y} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-width 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{width} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-height 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{height} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-window-class atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-window-class 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{window-class} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-visual atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-visual 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{visual} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-window-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-window-type 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{window-type} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-cursor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-cursor 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{cursor} of the @class{gdk-window-attr} structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-wmclass-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-wmclass-name 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{wmclass-name} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-wmclass-class atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-wmclass-class 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{wmclass-class} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-override-redirect atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-override-redirect 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{override-redirect} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-window-attr-type-hint atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-window-attr-type-hint 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{type-hint} of the @class{gdk-window-attr}
  structure.
  @see-class{gdk-window-attr}")

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
(setf (gethash 'gdk-window-attributes-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-window-attributes-type atdoc:*external-symbols*)
 "@version{2013-8-23}
  @begin{short}
    Used to indicate which fields in the @class{gdk-window-attr} structure
    should be honored.
  @end{short}
  For example, if you filled in the @code{cursor} and @code{x} fields of
  @class{gdk-window-attr}, pass @code{'(:x :cursor)} to the function
  @fun{gdk-window-new}. Fields in @class{gdk-window-attr} not covered by a bit
  in this enum are required; for example, the @code{width}/@code{height},
  @code{wclass}, and @code{window-type} fields are required, they have no
  corresponding flag in @sym{gdk-window-attributes-type}.
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
  @see-class{gdk-window-attr}
  @see-function{gdk-window-new}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_new" gdk-window-new)
    (g-object gdk-window :already-referenced)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[parent]{a @class{gdk-window}, or @code{nil} to create the window as
    a child of the default root window for the default display}
  @argument[attributes]{attributes of type @class{gdk-window-attr} of the new
    window}
  @argument[attributes-mask]{mask of type @symbol{gdk-window-attributes-type}
    indicating which fields in attributes are valid}
  @return{The new @class{gdk-window}.}
  @begin{short}
    Creates a new @class{gdk-window} using the attributes from @arg{attributes}.
  @end{short}
  See @class{gdk-window-attr} and @symbol{gdk-window-attributes-type} for more
  details.

  @b{Note:} To use this on displays other than the default display, @arg{parent}
  must be specified.
  @see-class{gdk-window}
  @see-class{gdk-window-attr}
  @see-symbol{gdk-window-attributes-type}"
  (parent (g-object gdk-window))
  (attributes (g-boxed-foreign gdk-window-attr))
  (attributes-mask gdk-window-attributes-type))

(export 'gdk-window-new)

;;; ----------------------------------------------------------------------------
;;; gdk_window_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_destroy" gdk-window-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
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
;;; gdk_window_get_window_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_window_type" gdk-window-get-window-type)
    gdk-window-type
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{type of @arg{window}}
  @begin{short}
    Gets the type of the window.
  @end{short}
  See @symbol{gdk-window-type}.
  @see-class{gdk-window}
  @see-symbol{gdk-window-type}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-window-type)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_display" gdk-window-get-display)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{The @class{gdk-display} associated with @arg{window}.}
  @begin{short}
    Gets the @class{gdk-display} associated with a @class{gdk-window}.
  @end{short}

  Since 2.24
  @see-class{gdk-window}
  @see-class{gdk-display}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_screen" gdk-window-get-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{The @class{gdk-screen} associated with @arg{window}.}
  @begin{short}
    Gets the @class{gdk-screen} associated with a @class{gdk-window}.
  @end{short}

  Since 2.24
  @see-class{gdk-window}
  @see-class{gdk-screen}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visual ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visual" gdk-window-get-visual) (g-object gdk-visual)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{A @class{gdk-visual} object.}
  @begin{short}
    Gets the @class{gdk-visual} describing the pixel format of @arg{window}.
  @end{short}

  Since 2.24
  @see-class{gdk-window}
  @see-class{gdk-screen}"
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
 "@version{2013-8-23}
  @begin{return}
    @code{window} -- window under the mouse pointer @br{}
    @code{win-x} -- origin of the window under the pointer @br{}
    @code{win-y} -- origin of the window under the pointer
  @end{return}
  @subheading{Warning}
    @sym{gdk-window-at-pointer} has been deprecated since version 3.0 and
    should not be used in newly written code.
    Use the function @fun{gdk-device-get-window-at-position} instead.

  @begin{short}
    Obtains the window underneath the mouse pointer, returning the location of
    that window in @arg{win-x}, @arg{win-y}. Returns @code{nil} if the window
    under the mouse pointer is not known to GDK.
  @end{short}
  @see-class{gdk-window}
  @fun{gdk-device-get-window-at-position}"
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
 "@version{2013-8-31}
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
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Shows a @class{gdk-window} onscreen, but does not modify its stacking order.
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
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    For toplevel windows, withdraws them, so they will no longer be known to the
    window manager; for all windows, unmaps them, so they will not be displayed.
  @end{short}
  Normally done automatically as part of the function @fun{gtk-widget-hide}.
  @see-class{gdk-window}
  @see-function{gtk-widget-hide}"
  (window (g-object gdk-window)))

(export 'gdk-window-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_destroyed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_destroyed" gdk-window-is-destroyed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the @arg{window} is destroyed.}
  @begin{short}
    Check to see if a @arg{window} is destroyed.
  @end{short}

  Since 2.18
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_visible" gdk-window-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the @arg{window} is mapped.}
  Checks whether the window has been mapped with the functions
  @fun{gdk-window-show} or @fun{gdk-window-show-unraised}.
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
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the @arg{window} is viewable.}
  @begin{short}
    Check if the @arg{window} and all ancestors of the @arg{window} are mapped.
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
 "@version{2013-8-23}
  @argument[window]{a toplevel @class{gdk-window} oject}
  @return{@em{True} if @arg{window} is input only.}
  @begin{short}
    Determines whether or not the @arg{window} is an input only window.
  @end{short}

  Since 2.22
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-input-only)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_shaped ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_shaped" gdk-window-is-shaped) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{@em{True} if @arg{window} is shaped.}
  @begin{short}
    Determines whether or not the @arg{window} is shaped.
  @end{short}

  Since 2.22
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-is-shaped)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_state" gdk-window-get-state) gdk-window-state
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @return{window state bitfield}
  Gets the bitwise OR of the currently active window state flags, from the
  @symbol{gdk-window-state} enumeration.
  @see-class{gdk-window}
  @see-symbol{gdk-window-state}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_window_withdraw ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_withdraw" gdk-window-withdraw) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Withdraws a window, that is, unmaps it and asks the window manager to forget
    about it.
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Asks to iconify (minimize) window.
  @end{short}
  The window manager may choose to ignore the request, but normally will honor
  it. Using the function @fun{gtk-window-iconify} is preferred, if you have a
  @class{gtk-window} widget.

  This function only makes sense when window is a toplevel window.
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
 "@version{2013-8-13}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Attempt to deiconify (unminimize) window.
  @end{short}
  On X11 the window manager may choose to ignore the request to deiconify. When
  using GTK+, use the @fun{gtk-window-deiconify} instead of the
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
 "@version{2013-8-31}
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
 "@version{2013-8-31}
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Maximizes the window. If the window was already maximized, then this
    function does nothing.
  @end{short}

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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Unmaximizes the window. If the window was not maximized, then this function
    does nothing.
  @end{short}

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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Moves the window into fullscreen mode. This means the window covers the
    entire screen and is above any panels or task bars.
  @end{short}

  If the window was already fullscreen, then this function does nothing.

  On X11, asks the window manager to put window in a fullscreen state, if the
  window manager supports this operation. Not all window managers support
  this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the fullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.

  Since 2.2
  @see-class{gdk-window}
  @see-function{gdk-window-unfullscreen}"
  (window (g-object gdk-window)))

(export 'gdk-window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unfullscreen" gdk-window-unfullscreen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Moves the window out of fullscreen mode. If the window was not fullscreen,
    does nothing.
  @end{short}

  On X11, asks the window manager to move window out of the fullscreen state,
  if the window manager supports this operation. Not all window managers
  support this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the unfullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.

  Since 2.2
  @see-class{gdk-window}
  @see-function{gdk-window-fullscreen}"
  (window (g-object gdk-window)))

(export 'gdk-window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_above ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_above" gdk-window-set-keep-above) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[setting]{whether to keep window above other windows}
  @begin{short}
    Set if @arg{window} must be kept above other windows. If the @arg{window}
    was already above, then this function does nothing.
  @end{short}

  On X11, asks the window manager to keep @arg{window} above, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep above\"; so
  you cannot rely on the window being kept above. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.

  Since 2.4
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[setting]{whether to keep window below other windows}
  @begin{short}
    Set if @arg{window} must be kept below other windows. If the window was
    already below, then this function does nothing.
  @end{short}

  On X11, asks the window manager to keep @arg{window} below, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep below\"; so you
  cannot rely on the window being kept below. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.

  Since 2.4
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[opacity]{opacity}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    opacity 0 being fully transparent and 1 fully opaque. Values of the opacity
    parameter are clamped to the [0,1] range.
  @end{short}

  On X11, this works only on X screens with a compositing manager running.

  For setting up per-pixel alpha, see the function
  @fun{gdk-screen-get-rgba-visual}. For making non-toplevel windows translucent,
  see the function @fun{gdk-window-set-composited}.

  Since 2.12
  @see-class{gdk-window}
  @see-function{gdk-screen-get-rgba-visual}
  @see-function{gdk-window-set-composited}"
  (window (g-object gdk-window))
  (opacity :double))

(export 'gdk-window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_composited" gdk-window-set-composited) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[composited]{@em{true} to set the @arg{window} as composited}
  @begin{short}
    Sets a @class{gdk-window} as composited, or unsets it.
  @end{short}
  Composited windows do not automatically have their contents drawn to the
  screen. Drawing is redirected to an offscreen buffer and an expose event is
  emitted on the parent of the composited window. It is the responsibility of
  the parent's expose handler to manually merge the off-screen content onto the
  screen in whatever way it sees fit.

  It only makes sense for child windows to be composited; see the function
  @fun{gdk-window-set-opacity} if you need translucent toplevel windows.

  An additional effect of this call is that the area of this window is no
  longer clipped from regions marked for invalidation on its parent. Draws
  done on the parent window are also no longer clipped by the child.

  This call is only supported on some systems, currently, only X11 with new
  enough Xcomposite and Xdamage extensions. You must call the function
  @fun{gdk-display-supports-composite} to check if setting a window as
  composited is supported before attempting to do so.

  Since 2.12
  @see-class{gdk-window}
  @see-function{gdk-window-get-composited}
  @see-function{gdk-window-set-opacity}
  @see-function{gdk-display-supports-composite}"
  (window (g-object gdk-window))
  (composited :boolean))

(export 'gdk-window-set-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_composited ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_composited" gdk-window-get-composited) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the @arg{window} is composited.}
  @begin{short}
    Determines whether @arg{window} is composited.
  @end{short}

  See the function @fun{gdk-window-set-composited}.

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-set-composited}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move" gdk-window-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{x coordinate relative to window's parent}
  @argument[y]{y coordinate relative to window's parent}
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[width]{new width of the window}
  @argument[height]{new height of the window}
  @begin{short}
    Resizes @arg{window}; for toplevel windows, asks the window manager to
    resize the window.
  @end{short}
  The window manager may not allow the resize. When using GTK+, use the function
  @fun{gtk-window-resize} instead of this low-level GDK function.

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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{new x position relative to window's parent}
  @argument[y]{new y position relative to window's parent}
  @argument[width]{new width}
  @argument[height]{new height}
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[dx]{amount to scroll in the x direction}
  @argument[dy]{amount to scroll in the y direction}
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
;;; gdk_window_move_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_region" gdk-window-move-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{the @symbol{cairo-region-t} to move}
  @argument[dx]{amount to move in the x direction}
  @argument[dy]{amount to move in the y direction}
  @begin{short}
    Move the part of window indicated by region by dy pixels in the y direction
    and dx pixels in the x direction. The portions of region that not covered by
    the new position of region are invalidated.
  @end{short}

  Child windows are not moved.

  Since 2.8
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
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
  that do not use double buffering this will be called automatically before
  sending the expose event.

  Since 2.18
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_window_has_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_has_native" gdk-window-has-native) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the window has a native window, @code{nil} otherwise.}
  @begin{short}
    Checks whether the window has a native window or not.
  @end{short}
  Note that you can use the function @fun{gdk-window-ensure-native} if a native
  window is needed.

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-ensure-native}"
  (window (g-object gdk-window)))

(export 'gdk-window-has-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ensure_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_ensure_native" gdk-window-ensure-native) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the window has a native window, @code{nil} otherwise.}
  @begin{short}
    Tries to ensure that there is a window-system native window for this
    @class{gdk-window}. This may fail in some situations, returning @code{nil}.
  @end{short}

  Offscreen window and children of them can never have native windows.

  Some backends may not support native child windows.

  Since 2.18
  @see-class{gdk-window}
  @see-function{gdk-window-has-native}"
  (window (g-object gdk-window)))

(export 'gdk-window-ensure-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_reparent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_reparent" gdk-window-reparent) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[new-parent]{new parent to move window into}
  @argument[x]{x location inside the new parent}
  @argument[y]{y location inside the new parent}
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Raises window to the top of the z-order (stacking order), so that other
    windows with the same parent window appear below window.
  @end{short}
  This is true whether or not the windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function
  @fun{gdk-window-raise} only requests the restack, does not guarantee it.
  @see-class{gdk-window}
  @see-function{gdk-window-lower}"
  (window (g-object gdk-window)))

(export 'gdk-window-raise)

;;; ----------------------------------------------------------------------------
;;; gdk_window_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_lower" gdk-window-lower) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Lowers window to the bottom of the z-order (stacking order), so that other
    windows with the same parent window appear above window.
  @end{short}
  This is true whether or not the other windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function @sym{gdk-window-lower}
  only requests the restack, does not guarantee it.

  Note that the function @fun{gdk-window-show} raises the window again, so do
  not call this function before the function @fun{gdk-window-show}.
  Try the function @fun{gdk-window-show-unraised}.
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
 "@version{2013-10-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[sibling]{a @class{gdk-window} that is a sibling of window,
    or @code{nil}}
  @argument[above]{a boolean}
  @begin{short}
    Changes the position of @arg{window} in the z-order (stacking order), so
    that it is above @arg{sibling}, if above is @em{true}, or below sibling,
    if @arg{above} is @code{nil}.
  @end{short}

  If @arg{sibling} is @code{nil}, then this either raises, if @arg{above} is
  @em{true}, or lowers the window.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the function
  @sym{gdk-window-restack} only requests the restack, does not guarantee it.

  Since 2.18
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[timestamp]{timestamp of the event triggering the window focus}
  @begin{short}
    Sets keyboard focus to @arg{window}.
  @end{short}
  In most cases, the function @fun{gtk-window-present} should be used on a
  @class{gtk-window}, rather than calling this function.
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  Registers a window as a potential drop destination.
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-register-dnd)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag" gdk-window-begin-resize-drag) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[edge]{the edge or corner from which the drag is started}
  @argument[button]{the button being used to drag}
  @argument[root-x]{root window x coordinate of mouse click that began the drag}
  @argument[root-y]{root window y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag,
     use the function @fun{gdk-event-get-time}}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}

  This function assumes that the drag is controlled by the client pointer
  device, use the function @fun{gdk-window-begin-resize-drag-for-device} to
  begin a drag with a different device.
  @see-class{gdk-window}
  @see-function{gdk-event-get-time}
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[edge]{the edge or corner from which the drag is started}
  @argument[device]{the device used for the operation}
  @argument[button]{the button being used to drag}
  @argument[root-x]{root window x coordinate of mouse click that began the drag}
  @argument[root-y]{root window Y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag,
    use the function @fun{gdk-event-get-time}}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window resize grip\", for
  example; in fact @class{gtk-statusbar} uses it. The function works best with
  window managers that support the Extended Window Manager Hints, but has a
  fallback implementation for other window managers.

  Since 3.4
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-class{gtk-statusbar}
  @see-function{gdk-event-get-time}"
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[button]{the button being used to drag}
  @argument[root-x]{root window x coordinate of mouse click that began the drag}
  @argument[root-y]{root window y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag}
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[device]{the device used for the operation}
  @argument[button]{the button being used to drag}
  @argument[root-x]{root window x coordinate of mouse click that began the drag}
  @argument[root-y]{root window y coordinate of mouse click that began the drag}
  @argument[timestamp]{timestamp of mouse click that began the drag}
  @begin{short}
    Begins a window move operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window move grip\", for example.
  The function works best with window managers that support the Extended Window
  Manager Hints, but has a fallback implementation for other window managers.

  Since 3.4
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
 "@version{2013-8-31}
  @argument[geometry]{a @class{gdk-geometry} structure}
  @argument[flags]{a mask of type @symbol{gdk-window-hints} indicating what
    portions of geometry are set}
  @argument[width]{desired width of window}
  @argument[height]{desired height of the window}
  @begin{return}
    @code{new-width} --  resulting width @br{}
    @code{new-height} -- resulting height
  @end{return}
  Constrains a desired @arg{width} and @arg{height} according to a set of
  geometry hints such as minimum and maximum size.
  @see-class{gdk-window}
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
 "@version{2013-8-31}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Emits a short beep associated to @arg{window} in the appropriate display, if
    supported. Otherwise, emits a short beep on the display just as the function
    @fun{gdk-display-beep}.
  @end{short}

  Since 2.12
  @see-class{gdk-window}
  @see-function{gdk-display-beep}"
  (window (g-object gdk-window)))

(export 'gdk-window-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_clip_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_clip_region" gdk-window-get-clip-region)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    A @symbol{cairo-region-t}. This must be freed with the function
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
  @see-symbol{cairo-region-t}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_rect" gdk-window-begin-paint-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[rectangle]{rectangle you intend to draw to}
  @begin{short}
    A convenience wrapper around the function
    @fun{gdk-window-begin-paint-region} which creates a rectangular region for
    you.
  @end{short}
  See the function @fun{gdk-window-begin-paint-region} for details.
  @see-class{gdk-window}
  @see-function{gdk-window-begin-paint-region}"
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign gdk-rectangle)))

(export 'gdk-window-begin-paint-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_region" gdk-window-begin-paint-region) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{region you intend to draw to}
  @begin{short}
    Indicates that you are beginning the process of redrawing region.
  @end{short}
  A backing store (offscreen buffer) large enough to contain @arg{region} will
  be created. The backing store will be initialized with the background color or
  background surface for @arg{window}. Then, all drawing operations performed on
  @arg{window} will be diverted to the backing store. When you call the function
  @fun{gdk-window-end-paint}, the backing store will be copied to @arg{window},
  making it visible onscreen. Only the part of @arg{window} contained in region
  will be modified; that is, drawing operations are clipped to @arg{region}.

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call the function
  @fun{gdk-window-end-paint}. If you draw to @arg{window} directly without
  calling the function @sym{gdk-window-begin-paint-region}, the user may see
  flicker as individual drawing operations are performed in sequence. The
  clipping and background-initializing features of the function
  @sym{gdk-window-begin-paint-region} are conveniences for the programmer, so
  you can avoid doing that work yourself.

  When using GTK+, the widget system automatically places calls to the functions
  @sym{gdk-window-begin-paint-region} and @fun{gdk-window-end-paint} around
  emissions of the \"expose-event\" signal. That is, if you are writing an
  expose event handler, you can assume that the exposed area in
  @class{gdk-event-expose} has already been cleared to the window background, is
  already set as the clip region, and already has a backing store. Therefore in
  most cases, application code need not call the function
  @sym{gdk-window-begin-paint-region}. You can disable the automatic calls
  around expose events on a widget-by-widget basis by calling the function
  @fun{gtk-widget-set-double-buffered}.

  If you call this function multiple times before calling the matching function
  @fun{gdk-window-end-paint}, the backing stores are pushed onto a stack. The
  function @fun{gdk-window-end-paint} copies the topmost backing store onscreen,
  subtracts the topmost region from all other regions in the stack, and pops the
  stack. All drawing operations affect only the topmost backing store in the
  stack. One matching call to the function @fun{gdk-window-end-paint} is
  required for each call to the function @sym{gdk-window-begin-paint-region}.
  @see-class{gdk-window}
  @see-class{gdk-event-expose}
  @see-function{gdk-window-end-paint}
  @see-function{gtk-widget-set-double-buffered}"
  (window (g-object gdk-window))
  (region (:pointer (:struct cairo-region-t))))

(export 'gdk-window-begin-paint-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_paint" gdk-window-end-paint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
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
  @see-class{gdk-window}
  @see-function{gdk-window-begin-paint-region}"
  (window (g-object gdk-window)))

(export 'gdk-window-end-paint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visible_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visible_region" gdk-window-get-visible-region)
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    A @symbol{cairo-region-t}. This must be freed with the function
    @fun{cairo-region-destroy} when you are done.
  @end{return}
  Computes the region of the window that is potentially visible. This does not
  necessarily take into account if the window is obscured by other windows,
  but no area outside of this region is visible.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{cairo-region-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-visible-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_rect" gdk-window-invalidate-rect) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[rect]{rectangle to invalidate or @code{nil} to invalidate the whole
    window}
  @argument[invalidate-children]{whether to also invalidate child windows}
  @begin{short}
    A convenience wrapper around the function @fun{gdk-window-invalidate-region}
    which invalidates a rectangular region.
  @end{short}
  See the function @fun{gdk-window-invalidate-region} for details.
  @see-class{gdk-window}
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
 "@version{2013-8-31}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t}}
  @argument[invalidate-children]{@em{true} to also invalidate child windows}
  @begin{short}
    Adds @arg{region} to the update area for @arg{window}.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call @fun{gdk-window-process-updates} sends one or more expose events to
  the window, which together cover the entire update area. An application would
  normally redraw the contents of window in response to those expose events.

  GDK will call @fun{gdk-window-process-all-updates} on your behalf whenever
  your program returns to the main loop and becomes idle, so normally there is
  no need to do that manually, you just need to invalidate regions that you know
  should be redrawn.

  The @arg{invalidate-children} parameter controls whether the region of each
  child window that intersects region will also be invalidated. If @code{nil},
  then the update area for child windows will remain unaffected. See the
  function @fun{gdk-window-invalidate-maybe-recurse} if you need fine grained
  control over which children are invalidated.
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
  (region (:pointer (:struct cairo-region-t)))
  (child-func :pointer)
  (user-data :pointer))

(defun gdk-window-invalidate-maybe-recurse (window region child-func)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[region]{a @symbol{cairo-region-t}}
  @argument[child-func]{function to use to decide if to recurse to a child,
    @code{NULL} means never recurse}
  @begin{short}
    Adds region to the update area for window.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call @fun{gdk-window-process-updates} sends one or more expose events to
  the window, which together cover the entire update area. An application would
  normally redraw the contents of window in response to those expose events.

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
    (:pointer (:struct cairo-region-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{The update area for window.}
  @begin{short}
    Transfers ownership of the update area from @arg{window} to the caller of
    the function.
  @end{short}
  That is, after calling this function, window will no longer have an
  invalid/dirty region; the update area is removed from window and handed to
  you. If a window has no update area, the function
  @fun{gdk-window-get-update-area} returns @code{NULL}. You are responsible for
  calling the function @fun{cairo-region-destroy} on the returned region if it
  is non-@code{NULL}.
  @see-class{gdk-window}
  @see-symbol{cairo-region-t}
  @see-function{gdk-window-get-update-area}
  @see-function{cairo-region-destroy}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-update-area)

;;; ----------------------------------------------------------------------------
;;; gdk_window_freeze_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_freeze_updates" gdk-window-freeze-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Temporarily freezes a window such that it will not receive expose events.
  @end{short}
  The window will begin receiving expose events again when the function
  @fun{gdk-window-thaw-updates} is called. If the function
  @fun{gdk-window-freeze-updates} has been called more than once,
  @fun{gdk-window-thaw-updates} must be called an equal number of times to begin
  processing exposes.
  @see-class{gdk-window}
  @see-function{gdk-window-thaw-updates}
  @see-function{gdk-window-freeze-updates}"
  (window (g-object gdk-window)))

(export 'gdk-window-freeze-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_thaw_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_thaw_updates" gdk-window-thaw-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  Thaws a window frozen with the function @fun{gdk-window-freeze-updates}.
  @see-class{gdk-window}
  @see-function{gdk-window-freeze-updates}"
  (window (g-object gdk-window)))

(export 'gdk-window-thaw-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_all_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_all_updates" gdk-window-process-all-updates)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  Calls the function @fun{gdk-window-process-updates} for all windows, see
  @class{gdk-window}, in the application.
  @see-class{gdk-window}
  @see-function{gdk-window-process-updates}")

(export 'gdk-window-process-all-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_updates" gdk-window-process-updates) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[update-children]{whether to also process updates for child windows}
  @begin{short}
    Sends one or more expose events to window.
  @end{short}
  The areas in each expose event will cover the entire update area for the
  window, see the function @fun{gdk-window-invalidate-region} for details.
  Normally GDK calls the functtion @fun{gdk-window-process-all-updates} on your
  behalf, so there's no need to call this function unless you want to force
  expose events to be delivered immediately and synchronously, vs. the usual
  case, where GDK delivers them in an idle handler. Occasionally this is useful
  to produce nicer scrolling behavior, for example.
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
 "@version{2013-9-1}
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
  slowly and noticeably, so you can see exactly what's being redrawn when, in
  what order.

  The @code{--gtk-debug=updates} command line option passed to GTK+ programs
  enables this debug option at application startup time. That is usually more
  useful than calling the function @fun{gdk-window-set-debug-updates} yourself,
  though you might want to use this function to enable updates sometime after
  application startup time.
  @see-class{gdk-window}
  @see-function{gdk-window-invalidate-region}
  @see-function{gdk-window-process-updates}
  @see-function{gdk-window-set-debug-updates}"
  (setting :boolean))

(export 'gdk-window-set-debug-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_enable_synchronized_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_enable_synchronized_configure"
           gdk-window-enable-synchronized-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Indicates that the application will cooperate with the window system in
    synchronizing the window repaint with the window manager during resizing
    operations.
  @end{short}
  After an application calls this function, it must call the function
  @fun{gdk-window-configure-finished} every time it has finished all processing
  associated with a set of Configure events. Toplevel GTK+ windows automatically
  use this protocol.

  On X, calling this function makes window participate in the
  @code{_NET_WM_SYNC_REQUEST} window manager protocol.

  Since 2.6
  @see-class{gdk-window}
  @see-function{gdk-window-configure-finished}"
  (window (g-object gdk-window)))

(export 'gdk-window-enable-synchronized-configure)

;;; ----------------------------------------------------------------------------
;;; gdk_window_configure_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_configure_finished" gdk-window-configure-finished) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{short}
    Signal to the window system that the application has finished handling
    Configure events it has received.
  @end{short}
  Window Managers can use this to better synchronize the frame repaint with the
  application. GTK+ applications will automatically call this function when
  appropriate.

  This function can only be called if the function
  @fun{gdk-window-enable-synchronized-configure} was called previously.

  Since 2.6
  @see-class{gdk-window}
  @see-function{gdk-window-enable-synchronized-configure}"
  (window (g-object gdk-window)))

(export 'gdk-window-configure-finished)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_user_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_user_data" gdk-window-set-user-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[user-data]{user data}
  @begin{short}
    For most purposes this function is deprecated in favor of the function
    @fun{g-object-set-data}.
  @end{short}
  However, for historical reasons GTK+ stores the @class{gtk-widget} that owns a
  @class{gdk-window} as user data on the @class{gdk-window}. So, custom widget
  implementations should use this function for that. If GTK+ receives an event
  for a @class{gdk-window}, and the user data for the window is non-@code{NULL},
  GTK+ will assume the user data is a @class{gtk-widget}, and forward the event
  to that widget.
  @see-class{gdk-window}
  @see-class{gtk-widget}
  @see-function{g-object-set-data}"
  (window (g-object gdk-window))
  (user-data :pointer))

(export 'gdk-window-set-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_override_redirect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_override_redirect" gdk-window-set-override-redirect)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
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
;;; gdk_window_set_accept_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_accept_focus" gdk-window-set-accept-focus) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[accept-focus]{@em{true} if the @arg{window} should receive input
    focus}
  @begin{short}
    Setting @arg{accept-focus} to @code{nil} hints the desktop environment that
    the window does not want to receive input focus.
  @end{short}

  On X, it is the responsibility of the window manager to interpret this hint.
  ICCCM-compliant window manager usually respect it.

  Since 2.4
  @see-class{gdk-window}
  @see-function{gdk-window-get-accept-focus}"
  (window (g-object gdk-window))
  (accept-focus :boolean))

(export 'gdk-window-set-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_accept_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_accept_focus" gdk-window-get-accept-focus) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{Whether or not the window should receive input focus.}
  @begin{short}
    Determines whether or not the desktop environment should be hinted that the
    window does not want to receive input focus.
  @end{short}

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-set-accept-focus}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_focus_on_map ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_focus_on_map" gdk-window-set-focus-on-map) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[focus-on-map]{@em{true} if the window should receive input focus
    when mapped}
  @begin{short}
    Setting @arg{focus-on-map} to @code{nil} hints the desktop environment that
    the window does not want to receive input focus when it is mapped.
  @end{short}
  @arg{focus-on-map} should be turned off for windows that are not triggered
  interactively, such as popups from network activity.

  On X, it is the responsibility of the window manager to interpret this hint.
  Window managers following the freedesktop.org window manager extension
  specification should respect it.

  Since 2.6
  @see-class{gdk-window}
  @see-function{gdk-window-get-focus-on-map}"
  (window (g-object gdk-window))
  (focus-on-map :boolean))

(export 'gdk-window-set-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_focus_on_map ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_focus_on_map" gdk-window-get-focus-on-map) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{return}
    Whether or not the window wants to receive input focus when it is
    mapped.
  @end{return}
  @begin{short}
    Determines whether or not the desktop environment should be hinted that the
    window does not want to receive input focus when it is mapped.
  @end{short}

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-set-focus-on-map}"
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-filter-return atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-filter-return atdoc:*external-symbols*)
 "@version{2013-9-1}
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[shape-region]{region of window to be non-transparent}
  @argument[offset-x]{x position of @arg{shape-region} in window coordinates}
  @argument[offset-y]{y position of @arg{shape-region} in window coordinates}
  @begin{short}
    Makes pixels in window outside @arg{shape-region} be transparent, so that
    the window may be nonrectangular.
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the shape mask of window to the union of shape masks for all children
    of window, ignoring the shape mask of window itself.
  @end{short}
  Contrast with the function @fun{gdk-window-merge-child-shapes} which includes
  the shape mask of window in the masks to be merged.
  @see-class{gdk-window}
  @see-function{gdk-window-merge-child-shapes}"
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_shapes" gdk-window-merge-child-shapes) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Merges the shape masks for any child windows into the shape mask for window.
  @end{short}
  I. e. the union of all masks for window and its children will become the new
  mask for window. See the function @fun{gdk-window-shape-combine-region}.

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
 "@version{2013-10-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[shape-region]{region of window to be non-transparent}
  @argument[offset-x]{x position of @arg{shape-region} in window coordinates}
  @argument[offset-y]{y position of @arg{shape-region} in window coordinates}
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

  Since 2.10
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Sets the input shape mask of window to the union of input shape masks for
    all children of window, ignoring the input shape mask of window itself.
  @end{short}
  Contrast with the function @fun{gdk-window-merge-child-input-shapes} which
  includes the input shape mask of window in the masks to be merged.

  Since 2.10
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Merges the input shape masks for any child windows into the input shape mask
    for @arg{window}.
  @end{short}
  I.e. the union of all input masks for window and its children will become the
  new input mask for window. See the function
  @fun{gdk-window-input-shape-combine-region}.

  This function is distinct from the function
  @fun{gdk-window-set-child-input-shapes} because it includes window's input
  shape mask in the set of shapes to be merged.

  Since 2.10
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[use-static]{@em{true} to turn on static gravity}
  @return{@em{True} if the server supports static gravity.}
  @begin{short}
    Set the bit gravity of the given window to static, and flag it so all
    children get static subwindow gravity.
  @end{short}
  This is used if you are implementing scary features that involve deep
  knowledge of the windowing system. Do not worry about it unless you have to.
  @see-class{gdk-window}"
  (window (g-object gdk-window))
  (use-static :boolean))

(export 'gdk-window-set-static-gravities)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_title" gdk-window-set-title) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[title]{title of window}
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
 "@version{2014-1-22}
  @argument[window]{a @class{gdk-window} object}
  @argument[color]{a @class{gdk-color} structure}
  @begin{short}
    Sets the background color of window.
  @end{short}
  However, when using GTK+, set the background of a widget with the function
  @fun{gtk-widget-modify-bg}, if you are implementing an application, or the
  function @fun{gtk-style-context-set-background}, if you are implementing a
  custom widget.

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
  @see-function{gdk-window-set-background-pattern}"
  (window (g-object gdk-window))
  (color (g-boxed-foreign gdk-color)))

(export 'gdk-window-set-background)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background_rgba" gdk-window-set-background-rgba) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[rgba]{a @class{gdk-rgba} color}
  @begin{short}
    Sets the background color of window.
  @end{short}

  See also the function @fun{gdk-window-set-background-pattern}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-background-pattern}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[pattern]{a pattern to use, or @code{NULL}}
  @begin{short}
    Sets the background of window.
  @end{short}

  A background of @code{NULL} means that the window will inherit its background
  form its parent window.

  The windowing system will normally fill a window with its background when
  the window is obscured then exposed.
  @see-class{gdk-window}
  @see-symbol{cairo-pattern-t}"
  (window (g-object gdk-window))
  (pattern cairo-pattern-t))

(export 'gdk-window-set-background-pattern)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_background_pattern ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_background_pattern" gdk-window-get-background-pattern)
    cairo-pattern-t
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    The pattern to use for the background or @code{NULL} to use the parent's
    background.
  @end{return}
  @begin{short}
    Gets the pattern used to clear the background on window.
  @end{short}
  If window does not have its own background and reuses the parent's,
  @code{NULL} is returned and you will have to query it yourself.

  Since 2.22
  @see-class{gdk-window}"
  (window (g-object gdk-window)))

(export 'gdk-window-set-background-pattern)

;;; ----------------------------------------------------------------------------
;;; GDK_PARENT_RELATIVE
;;; ----------------------------------------------------------------------------

(defconstant +gdk-parent-relative+ 1
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @variable-value{1}
  A special value, indicating that the background for a window should be
  inherited from the parent window.
  @see-class{gdk-window}")

(export '+gdk-parent-relative+)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_cursor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-set-cursor))

(defun gdk-window-set-cursor (window cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @argument[cursor]{a @class{gdk-cursor}}
  @begin{short}
    Sets the default mouse pointer for a @class{gdk-window} object.
  @end{short}
  Use the functions @fun{gdk-cursor-new-for-display} or
  @fun{gdk-cursor-new-from-pixbuf} to create the @arg{cursor}. To make the
  @arg{cursor} invisible, use @code{:blank-cursor} of the
  @symbol{gdk-cursor-type} enumeration. Passing @code{nil} for the @arg{cursor}
  argument to @sym{gdk-window-set-cursor} means that @arg{window} will use the
  cursor of its parent window. Most windows should use this default.
  @see-class{gdk-window}
  @see-class{gdk-cursor}
  @see-symbol{gdk-cursor-type}
  @see-function{gdk-cursor-get-cursor}
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-new-from-pixbuf}"
  (setf (gdk-window-cursor window) cursor))

(export 'gdk-window-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_cursor ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-window-get-cursor))

(defun gdk-window-get-cursor (window)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[window]{a @class{gdk-window} object}
  @return{A @class{gdk-cursor}, or @code{nil}. The returned object is owned by
    the @class{gdk-window} and should not be unreferenced directly. Use
    @fun{gdk-window-set-cursor} to unset the cursor of the window.}
  @begin{short}
    Retrieves a @class{gdk-cursor} pointer for the cursor currently set on the
    specified @class{gdk-window}, or @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified window, and it is using the cursor for its parent window.

  Since 2.18
  @see-class{gdk-window}
  @see-class{gdk-cursor}
  @see-function{gdk-window-set-cursor}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{@code{data} -- user data}
  @begin{short}
    Retrieves the user data for window, which is normally the widget that window
    belongs to.
  @end{short}
  See the function @fun{gdk-window-set-user-data}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-user-data}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x} -- x coordinate of window, relative to its parent @br{}
    @code{y} -- y coordinate of window, relative to its parent @br{}
    @code{width} -- width of window @br{}
    @code{height} -- height of window
  @end{return}
  @begin{short}
    The @arg{x} and @arg{y} coordinates returned are relative to the parent
    window of @arg{window}, which for toplevels usually means relative to the
    window decorations (titlebar, etc.) rather than relative to the root window
    (screen-size background window).
  @end{short}

  On the X11 platform, the geometry is obtained from the X server, so reflects
  the latest position of window; this may be out-of-sync with the position of
  window delivered in the most-recently-processed @class{gdk-event-configure}.
  The function @fun{gdk-window-get-position} in contrast gets the position from
  the most recent configure event.

  @subheading{Note}
    If @arg{window} is not a toplevel, it is much better to call the functions
    @fun{gdk-window-get-position}, @fun{gdk-window-get-width} and
    @fun{gdk-window-get-height} instead, because it avoids the roundtrip to the
    X server and because these functions support the full 32-bit coordinate
    space, whereas the function @sym{gdk-window-get-geometry} is restricted to
    the 16-bit coordinates of X11.
  @see-class{gdk-window}
  @see-class{gdk-event-configure}
  @see-function{gdk-window-get-position}
  @see-function{gdk-window-get-width}
  @see-function{gdk-window-get-height}"
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
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[geometry]{geometry hints}
  @argument[geom-mask]{bitmask indicating fields of geometry to pay attention
    to}
  @begin{short}
    Sets the geometry hints for @arg{window}. Hints flagged in @arg{geom-mask}
    are set, hints not flagged in @arg{geom-mask} are unset. To unset all hints,
    use a @arg{geom-mask} of 0 and a geometry of @code{nil}.
  @end{short}

  This function provides hints to the windowing system about acceptable sizes
  for a toplevel window. The purpose of this is to constrain user resizing,
  but the windowing system will typically, but is not required to, also
  constrain the current size of the window to the provided values and
  constrain programatic resizing via the functions @fun{gdk-window-resize} or
  @fun{gdk-window-move-resize}.

  Note that on X11, this effect has no effect on windows of type @code{:temp}
  or windows where override redirect has been turned on via the function
  @fun{gdk-window-set-override-redirect} since these windows are not resizable by
  the user.

  Since you cannot count on the windowing system doing the constraints for
  programmatic resizes, you should generally call the function
  @fun{gdk-window-constrain-size} yourself to determine appropriate sizes.
  @see-class{gdk-window}
  @see-function{gdk-window-resize}
  @see-function{gdk-window-move-resize}
  @see-function{gdk-window-set-override-redirect}
  @see-function{gdk-window-constrain-size}"
  (window (g-object gdk-window))
  (geometry (g-boxed-foreign gdk-geometry))
  (geometry-mask gdk-window-hints))

(export 'gdk-window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_width" gdk-window-get-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{The width of window.}
  @begin{short}
    Returns the width of the given @arg{window}.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.

  Since 2.24
  @see-class{gdk-window}
  @see-function{gdk-window-get-height}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_height" gdk-window-get-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-2}
  @argument[window]{a @class{gdk-window} object}
  @return{The height of window.}
  @begin{short}
    Returns the height of the given @arg{window}.
  @end{short}

  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.

  Since 2.24
  @see-class{gdk-window}
  @see-function{gdk-window-get-width}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-height)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_list" gdk-window-set-icon-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{the @class{gdk-window} toplevel window to set the icon of}
  @argument[pixbufs]{a list of pixbufs, of different sizes}
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
;;; gdk_window_set_modal_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_modal_hint" gdk-window-set-modal-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[modal]{@em{true} if the window is modal, @code{nil} otherwise}
  @begin{short}
    The application can use this hint to tell the window manager that a certain
    window has modal behaviour.
  @end{short}
  The window manager can use this information to handle modal windows in a
  special way.

  You should only use this on windows for which you have previously called
  the function @fun{gdk-window-set-transient-for}.
  @see-class{gdk-window}
  @see-function{gdk-window-get-modal-hint}
  @see-function{gdk-window-set-transient-for}"
  (window (g-object gdk-window))
  (modal :boolean))

(export 'gdk-window-set-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_modal_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_modal_hint" gdk-window-get-modal-hint) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{Whether or not the window has the modal hint set.}
  @begin{short}
    Determines whether or not the window manager is hinted that window has modal
    behaviour.
  @end{short}

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-set-modal-hint}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_type_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_type_hint" gdk-window-set-type-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[hint]{a hint of the function this window will have}
  @begin{short}
    The application can use this call to provide a hint to the window manager
    about the functionality of a window.
  @end{short}
  The window manager can use this information when determining the decoration
  and behaviour of the window.

  The hint must be set before the window is mapped.
  @see-class{gdk-window}
  @see-function{gdk-window-get-type-hint}"
  (window (g-object gdk-window))
  (hint gdk-window-type-hint))

(export 'gdk-window-set-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_type_hint" gdk-window-get-type-hint)
    gdk-window-type-hint
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{The type hint set for @arg{window}.}
  @begin{short}
    This function returns the type hint set for a window.
  @end{short}

  Since 2.10
  @see-class{gdk-window}
  @see-function{gdk-window-set-type-hint}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_taskbar_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_taskbar_hint" gdk-window-set-skip-taskbar-hint)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[skips-taskbar]{@em{true} to skip the taskbar}
  @begin{short}
    Toggles whether a @arg{window} should appear in a task list or window list.
  @end{short}
  If a window's semantic type as specified with the function
  @fun{gdk-window-set-type-hint} already fully describes the window, this
  function should not be called in addition, instead you should allow the
  window to be treated according to standard policy for its semantic type.

  Since 2.2
  @see-class{gdk-window}
  @see-function{gdk-window-set-type-hint}"
  (window (g-object gdk-window))
  (skips-taskbar :boolean))

(export 'gdk-window-set-skip-taskbar-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_pager_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_pager_hint" gdk-window-set-skip-pager-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[skips-pager]{@em{true} to skip the pager}
  @begin{short}
    Toggles whether a window should appear in a pager, workspace switcher, or
    other desktop utility program that displays a small thumbnail representation
    of the windows on the desktop.
  @end{short}
  If a window's semantic type as specified with the function
  @fun{gdk-window-set-type-hint} already fully describes the window, this
  function should not be called in addition, instead you should allow the
  window to be treated according to standard policy for its semantic type.

  Since 2.2
  @see-class{gdk-window}
  @see-function{gdk-window-set-type-hint}"
  (window (g-object gdk-window))
  (skips-pager :boolean))

(export 'gdk-window-set-skip-pager-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_urgency_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_urgency_hint" gdk-window-set-urgency-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[urgent]{@em{true} if the window is urgent}
  @begin{short}
    Toggles whether a window needs the user's urgent attention.
  @end{short}

  Since 2.8
  @see-class{gdk-window}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x} -- x coordinate of window @br{}
    @code{y} -- y coordinate of window
  @end{return}
  @begin{return}
    Obtains the position of the window as reported in the
    most-recently-processed @class{gdk-event-configure}. Contrast with the
    @fun{gdk-window-get-geometry} which queries the X server for the current
    window position, regardless of which events have been received or processed.
  @end{return}
  The position coordinates are relative to the window's parent window.
  @see-class{gdk-window}
  @see-class{gdk-event-configure}
  @see-function{gdk-window-get-geometry}"
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
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{return}
    @code{x} -- x position of window frame @br{}
    @code{y} -- y position of window frame
  @end{return}
  Obtains the top-left corner of the window manager frame in root window
  coordinates.
  @see-class{gdk-window}"
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
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @begin{return}
    @code{rect} -- rectangle with bounding box of the window frame
  @end{return}
  @begin{short}
    Obtains the bounding box of the window, including window manager
    titlebar/borders if any.
  @end{short}
  The frame position is given in root window coordinates. To get the position
  of the window itself, rather than the frame, in root window coordinates, use
  the function @fun{gdk-window-get-origin}.
  @see-class{gdk-window}
  @see-function{gdk-window-get-origin}"
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
 "@version{2013-8-18}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{x} -- x coordinate @br{}
    @code{y} -- y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window in root window coordinates.
  @end{short}
  Compare with the functions @fun{gdk-window-get-position} and
  @fun{gdk-window-get-geometry} which return the position of a window relative
  to its parent window.
  @see-class{gdk-window}
  @see-function{gdk-window-get-position}
  @see-function{gdk-window-get-geometry}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[x]{x coordinate in window}
  @argument[y]{y coordinate in window}
  @begin{return}
    @code{root-x} -- return for x coordinate @br{}
    @code{root-y} -- return  for y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window position in root window coordinates.
  @end{short}
  This is similar to the function @fun{gdk-window-get-origin} but allows you go
  pass in any position in the window, not just the origin.

  Since 2.18
  @see-class{gdk-window}
  @see-function{gdk-window-get-origin}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    @code{win} -- the window containing the pointer as with the function
                  @fun{gdk-window-at-pointer}, or @code{nil} if the window
                  containing the pointer is not known to GDK @br{}
    @code{x} -- x coordinate of pointer @br{}
    @code{y} -- y coordinate of pointer @br{}
    @code{mask} -- modifier mask
  @end{return}
  @subheading{Warning}
    The function @sym{gdk-window-get-pointer} has been deprecated since version
    3.0 and should not be used in newly written code. Use the function
    @fun{gdk-window-get-device-position} instead.

  @begin{short}
    Obtains the current pointer position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}
  @see-class{gdk-window}
  @see-function{gdk-window-at-pointer}
  @see-function{gdk-window-get-device-position}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a @class{gdk-device} to query}
  @begin{return}
    @code{win} -- the window underneath device, as with the function
                  @fun{gdk-device-get-window-at-position}, or @code{nil} if the
                  window is not known to GDK @br{}
    @code{x} -- the x coordinate of device @br{}
    @code{y} -- the y coordinate of device @br{}
    @code{mask} -- the modifier mask
  @end{return}
  @begin{short}
    Obtains the current device position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}

  Since 3.0
  @see-class{gdk-window}
  @see-function{gdk-device-get-window-at-position}"
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
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{parent of @arg{window}}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Does not query the X server; thus this returns the parent as passed to the
  function @fun{gdk-window-new}, not the actual parent. This should never
  matter unless you are using Xlib calls mixed with GDK calls on the X11
  platform. It may also matter for toplevel windows, because the window manager
  may choose to reparent them.

  Note that you should use the function @fun{gdk-window-get-effective-parent}
  when writing generic code that walks up a window hierarchy, because the
  function @sym{gdk-window-get-parent} will most likely not do what you expect
  if there are offscreen windows in the hierarchy.
  @see-class{gdk-window}
  @see-function{gdk-window-new}
  @see-function{gdk-window-get-children}
  @see-function{gdk-window-get-effective-parent}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_toplevel" gdk-window-get-toplevel)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a @class{gdk-window} object}
  @return{The toplevel window containing @arg{window}.}
  @begin{short}
    Gets the toplevel window that is an ancestor of @arg{window}.
  @end{short}

  Any window type but @code{:child} is considered a toplevel window, as is
  a @code{:child} window that has a root window as parent.

  Note that you should use the function @fun{gdk-window-get-effective-toplevel}
  when you want to get to a window's toplevel as seen on screen, because
  the function @sym{gdk-window-get-toplevel} will most likely not do what you
  expect if there are offscreen windows in the hierarchy.
  @see-class{gdk-window}
  @see-function{gdk-window-get-effective-toplevel}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children" gdk-window-get-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{List of child windows inside @arg{window}.}
  @begin{short}
    Gets the list of children of @arg{window} known to GDK.
  @end{short}
  This function only returns children created via GDK, so for example it is
  useless when used with the root window; it only returns windows an application
  created itself.
  @see-class{gdk-window}
  @see-function{gdk-window-get-parent}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_peek_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_peek_children" gdk-window-peek-children)
    (g-list (g-object gdk-window))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a @class{gdk-window} object}
  @return{A reference to the list of child windows in @arg{window}.}
  Like the function @fun{gdk-window-get-children}, but does not copy the list
  of children, so the list does not need to be freed.
  @see-class{gdk-window}
  @see-function{gdk-window-get-children}"
  (window (g-object gdk-window)))

(export 'gdk-window-peek-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_events" gdk-window-get-events) gdk-event-mask
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @return{Event mask for @arg{window}.}
  @begin{short}
    Gets the event mask for window for all master input devices.
  @end{short}
  See the function @fun{gdk-window-set-events}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-events}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_events" gdk-window-set-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[event-mask]{event mask of type @symbol{gdk-event-mask} for window}
  @begin{short}
    The event mask for a window determines which events will be reported for
    that window from all master input devices.
  @end{short}
  For example, an event mask including @code{:button-press-mask} means the
  window should report button press events. The event mask is the bitwise OR of
  values from the @symbol{gdk-event-mask} flags.
  @see-class{gdk-window}
  @see-symbol{gdk-event-mask}
  @see-function{gdk-window-get-events}"
  (window (g-object gdk-window))
  (event-mask gdk-event-mask))

(export 'gdk-window-set-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_name" gdk-window-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[name]{name of window while iconified (minimized)}
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
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[parent]{another toplevel @class{gdk-window} object}
  @begin{short}
    Indicates to the window manager that @arg{window} is a transient dialog
    associated with the application window parent.
  @end{short}
  This allows the window manager to do things like center @arg{window} on
  @arg{parent} and keep @arg{window} above @arg{parent}.

  See the function @fun{gtk-window-transient-for} if you are using
  @class{gtk-window} or @class{gtk-dialog}.
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
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[role]{a string indicating its role}
  @begin{short}
    When using GTK+, typically you should use the function
    @fun{gtk-window-set-role} instead of this low-level function.
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
  @see-function{gtk-window-set-role}"
  (window (g-object gdk-window))
  (role :string))

(export 'gdk-window-set-role)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_startup_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_startup_id" gdk-window-set-startup-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[startup-id]{a string with startup-notification identifier}
  @begin{short}
    When using GTK+, typically you should use the function
    @fun{gtk-window-set-startup-id} instead of this low-level function.
  @end{short}

  Since 2.12
  @see-class{gdk-window}
  @see-function{gtk-window-set-startup-id}"
  (window (g-object gdk-window))
  (startup-id :string))

(export 'gdk-window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_group" gdk-window-set-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[leader]{group leader window, or @code{nil} to restore the default
    group leader window}
  @begin{short}
    Sets the group leader window for @arg{window}.
  @end{short}
  By default, GDK sets the group leader for all toplevel windows to a global
  window implicitly created by GDK. With this function you can override this
  default.

  The group leader window allows the window manager to distinguish all windows
  that belong to a single application. It may for example allow users to
  minimize/unminimize all windows belonging to an application at once. You
  should only set a non-default group window if your application pretends to
  be multiple applications.
  @see-class{gdk-window}
  @see-function{gdk-window-get-group}"
  (window (g-object gdk-window))
  (leader (g-object gdk-window)))

(export 'gdk-window-set-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_group" gdk-window-get-group) (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[window]{a toplevel @class{gdk-window} object}
  @return{The group leader window for @arg{window}.}
  @begin{short}
    Returns the group leader window for @arg{window}.
  @end{short}
  See the function @fun{gdk-window-set-group}.

  Since 2.4
  @see-class{gdk-window}
  @see-function{gdk-window-set-group}"
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-decoration atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-decoration atdoc:*external-symbols*)
 "@version{2013-9-2}
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
;;; gdk_window_set_decorations ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_decorations" gdk-window-set-decorations) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[decorations]{decoration hint mask of type
    @symbol{gdk-wm-decoration}}
  @begin{short}
    \"Decorations\" are the features the window manager adds to a toplevel
    @class{gdk-window}. This function sets the traditional Motif window manager
    hints that tell the window manager which decorations you would like your
    window to have.
  @end{short}
  Usually you should use the function @fun{gtk-window-set-decorated} on a
  @class{gtk-window} instead of using the GDK function directly.

  The decorations argument is the logical OR of the values of the
  @symbol{gdk-wm-decoration} flags. If @code{:all} is included in the mask, the
  other bits indicate which decorations should be turned off. If @code{:all}
  is not included, then the other bits indicate which decorations should be
  turned on.

  Most window managers honor a decorations hint of 0 to disable all
  decorations, but very few honor all possible combinations of bits.
  @see-class{gdk-window}
  @see-class{gtk-window}
  @see-function{gdk-window-get-decorations}
  @see-function{gtk-window-set-decorated}"
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
 "@version{2013-9-2}
  @argument[window]{the toplevel @class{gdk-window} to get the decorations from}
  @argument[decorations]{the window decorations will be written here}
  @return{@em{True} if the window has decorations set, @code{nil} otherwise.}
  Returns the decorations set on the @class{gdk-window} with the function
  @fun{gdk-window-set-decorations}.
  @see-class{gdk-window}
  @see-function{gdk-window-set-decorations}"
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-wm-function atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gdk-wm-function atdoc:*external-symbols*)
 "@version{2013-9-2}
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
;;; gdk_window_set_functions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_functions" gdk-window-set-functions) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a toplevel @class{gdk-window} object}
  @argument[functions]{bitmask of operations to allow on window}
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
;;; gdk_get_default_root_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_default_root_window" gdk-get-default-root-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @return{The default root window.}
  Obtains the root window, parent all other windows are inside, for the
  default display and screen.
  @see-class{gdk-window}")

(export 'gdk-get-default-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_support_multidevice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_support_multidevice"
           gdk-window-get-support-multidevice) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @return{@em{True} if the window handles multidevice features.}
  @begin{short}
    Returns @em{true} if the window is aware of the existence of multiple
    devices.
  @end{short}

  Since 3.0
  @see-class{gdk-window}
  @see-function{gdk-window-set-support-multidevice}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_support_multidevice ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_support_multidevice"
           gdk-window-set-support-multidevice) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[support-multidevice]{@em{true} to enable multidevice support in
    @arg{window}}
  @begin{short}
    This function will enable multidevice features in @arg{window}.
  @end{short}

  Multidevice aware windows will need to handle properly multiple, per device
  enter/leave events, device grabs and grab ownerships.

  Since 3.0
  @see-class{gdk-window}
  @see-function{gdk-window-get-support-multidevice}"
  (window (g-object gdk-window))
  (support-multidevice :boolean))

(export 'gdk-window-set-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_cursor" gdk-window-get-device-cursor)
    (g-object gdk-cursor)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a master @class{gdk-device} object}
  @begin{return}
    A @class{gdk-cursor}, or @code{nil}. The returned object is owned by the
    @class{gdk-window} and should not be unreferenced directly. Use the function
    @fun{gdk-window-set-cursor} to unset the cursor of the window.
  @end{return}
  @begin{short}
    Retrieves a @class{gdk-cursor} pointer for the device currently set on the
    specified @class{gdk-window}, or @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified window, and it is using the cursor for its parent window.

  Since 3.0
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-class{gdk-cursor}
  @see-function{gdk-window-set-device-cursor}"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-get-device-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_device_cursor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_device_cursor" gdk-window-set-device-cursor) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a master @class{gdk-device} object}
  @argument[cursor]{a @class{gdk-cursor} object}
  @begin{short}
    Sets a specific @class{gdk-cursor} for a given device when it gets inside
    window.
  @end{short}
  Use the functions @fun{gdk-cursor-new-for-display} or
  @fun{gdk-cursor-new-from-pixbuf} to create the cursor. To make the cursor
  invisible, use @code{:blank-cursor}. Passing @code{nil} for the cursor
  argument to the function @sym{gdk-window-set-cursor} means that window will
  use the cursor of its parent window. Most windows should use this default.

  Since 3.0
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-class{gdk-cursor}
  @see-function{gdk-cursor-new-for-display}
  @see-function{gdk-cursor-new-from-pixbuf}
  @see-function{gdk-window-get-device-cursor}"
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
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{a @class{gdk-device} object}
  @return{device event mask for window}
  @begin{short}
    Returns the event mask for window corresponding to an specific device.
  @end{short}

  Since 3.0
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-function{gdk-window-set-device-events}"
  (window (g-object gdk-window))
  (device (g-object gdk-device)))

(export 'gdk-window-get-device-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_device_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_device_events" gdk-window-set-device-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[device]{@class{gdk-device} object to enable events for}
  @argument[event-mask]{event mask for @arg{window}}
  @begin{short}
    Sets the event mask for a given device.
  @end{short}
  Normally a floating device, not attached to any visible pointer to window.
  For example, an event mask including @code{:button-press-mask} means the
  window should report button press events. The event mask is the bitwise OR of
  values from the @symbol{gdk-event-mask} flags.

  Since 3.0
  @see-class{gdk-window}
  @see-class{gdk-device}
  @see-symbol{gdk-event-mask}
  @see-function{gdk-window-get-device-events}"
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
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[source]{a @symbol{gdk-input-source} to define the source class}
  @return{Source event mask for @arg{window}.}
  Returns the event mask for window corresponding to the device class
  specified by source.
  @see-class{gdk-window}
  @see-symbol{gdk-input-source}
  @see-function{gdk-window-set-source-events}"
  (window (g-object gdk-window))
  (source gdk-input-source))

(export 'gdk-window-get-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_source_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_source_events" gdk-window-set-source-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-5}
  @argument[window]{a @class{gdk-window} object}
  @argument[source]{a @symbol{gdk-input-source} to define the source class}
  @argument[event-mask]{event mask for @arg{window}}
  @begin{short}
    Sets the event mask for any floating device, i. e. not attached to any
    visible pointer, that has the source defined as source.
  @end{short}
  This event mask will be applied both to currently existing, newly added
  devices after this call, and devices being attached/detached.

  Since 3.0
  @see-class{gdk-window}
  @see-symbol{gdk-input-source}
  @see-symbol{gdk-event-mask}
  @see-function{gdk-window-get-source-events}"
  (window (g-object gdk-window))
  (source gdk-input-source)
  (event-mask gdk-event-mask))

(export 'gdk-window-set-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_surface" gdk-offscreen-window-get-surface)
    (:pointer (:struct cairo-surface-t))
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @return{The offscreen surface, or @code{nil} if not offscreen.}
  @begin{short}
    Gets the offscreen surface that an offscreen window renders into.
  @end{short}
  If you need to keep this around over window resizes, you need to add a
  reference to it.
  @see-class{gdk-window}
  @see-symbol{cairo-surface-t}"
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_set_embedder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_set_embedder" gdk-offscreen-window-set-embedder)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @argument[embedder]{the @class{gdk-window} that window gets embedded in}
  @begin{short}
    Sets @arg{window} to be embedded in @arg{embedder}.
  @end{short}

  To fully embed an offscreen window, in addition to calling this function, it
  is also necessary to handle the \"pick-embedded-child\" signal on the
  @arg{embedder} and the \"to-embedder\" and \"from-embedder\" signals on
  @arg{window}.

  Since 2.18
  @see-class{gdk-window}
  @see-function{gdk-offscreen-window-get-embedder}"
  (window (g-object gdk-window))
  (embedder (g-object gdk-window)))

(export 'gdk-offscreen-window-set-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_embedder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_embedder" gdk-offscreen-window-get-embedder)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @begin{return}
    The embedding @class{gdk-window}, or @code{nil} if @arg{window} is not an
    embedded offscreen window.
  @end{return}
  @begin{short}
    Gets the window that @arg{window} is embedded in.
  @end{short}

  Since 2.18
  @see-class{gdk-window}
  @see-function{gdk-offscreen-window-set-embedder}"
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_window_geometry_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_geometry_changed" gdk-window-geometry-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{an embedded offscreen @class{gdk-window} object}
  @begin{short}
    This function informs GDK that the geometry of an embedded offscreen window
    has changed.
  @end{short}
  This is necessary for GDK to keep track of which offscreen window the pointer
  is in.

  Since 2.18
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
  (x :double)
  (y :double))

(defun gdk-window-coords-from-parent (window parent-x parent-y)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a child window}
  @argument[parent-x]{x coordinate in parent's coordinate system}
  @argument[parent-y]{y coordinate in parent's coordinate system}
  @begin{return}
    @code{x} -- x coordinate in child's coordinate system @br{}
    @code{y} -- y coordinate in child's coordinate system
  @end{return}
  @begin{short}
    Transforms window coordinates from a parent window to a child window, where
    the parent window is the normal parent as returned by the function
    @fun{gdk-window-get-parent} for normal windows, and the window's embedder as
    returned by the function @fun{gdk-offscreen-window-get-embedder} for
    offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to subtracting the
  return values of the function @fun{gdk-window-get-position} from the parent
  coordinates. For offscreen windows however, which can be arbitrarily
  transformed, this function calls the \"from-embedder\" signal to translate the
  coordinates.

  You should always use this function when writing generic code that walks
  down a window hierarchy.

  See also the function @fun{gdk-window-coords-to-parent}.

  Since 2.22
  @see-function{gdk-window}
  @see-function{gdk-window-get-parent}
  @see-function{gdk-offscreen-window-get-embedder}
  @see-function{gdk-window-get-position}"
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
 "@version{2013-9-2}
  @argument[window]{a child window}
  @argument[x]{x coordinate in child's coordinate system}
  @argument[y]{y coordinate in child's coordinate system}
  @begin{return}
    @code{parent-x} -- x coordinate in parent's coordinate system,
                       or @code{nil} @br{}
    @code{parent-y} -- y coordinate in parent's coordinate system, or @code{nil}
  @end{return}
  @begin{short}
    Transforms window coordinates from a child window to its parent window,
    where the parent window is the normal parent as returned by the function
    @fun{gdk-window-get-parent} for normal windows, and the window's embedder as
    returned by the function @fun{gdk-offscreen-window-get-embedder} for
    offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to adding the return
  values of the function @fun{gdk-window-get-position} to the child coordinates.
  For offscreen windows however, which can be arbitrarily transformed, this
  function calls the \"to-embedder\" signal to translate the coordinates.

  You should always use this function when writing generic code that walks up
  a window hierarchy.

  See also the function @fun{gdk-window-coords-from-parent}.

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-get-parent}
  @see-function{gdk-offscreen-window-get-embedder}
  @see-function{gdk-window-get-position}
  @see-function{gdk-window-coords-from-parent}"
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
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @return{effective parent of @arg{window}}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Works like the function @fun{gdk-window-get-parent} for normal windows, but
  returns the window's embedder for offscreen windows.

  See also the function @fun{gdk-offscreen-window-get-embedder}.

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-get-parent}
  @see-function{gdk-offscreen-window-get-embedder}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_toplevel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_toplevel" gdk-window-get-effective-toplevel)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-2}
  @argument[window]{a @class{gdk-window} object}
  @return{The effective toplevel window containing window.}
  @begin{short}
    Gets the toplevel window that is an ancestor of @arg{window}.
  @end{short}

  Works like the function @fun{gdk-window-get-toplevel}, but treats an offscreen
  window's embedder as its parent, using the function
  @fun{gdk-window-get-effective-parent}.

  See also: gdk_offscreen_window_get_embedder()

  Since 2.22
  @see-class{gdk-window}
  @see-function{gdk-window-get-toplevel}
  @see-function{gdk-window-get-effective-parent}"
  (window (g-object gdk-window)))

(export 'gdk-window-get-effective-toplevel)

;;; --- End of file gdk.window.lisp --------------------------------------------
