(def-suite gdk-window :in gdk-suite)
(in-suite gdk-window)

;; TODO: Do not use the functions gtk-widget-show or gtk-widget-show-all

;;; --- Types and Values -------------------------------------------------------

;;;     GdkWindow

;;;     GdkWindowType

(test gdk-window-type
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkWindowType"
    GDK-WINDOW-TYPE
    (:EXPORT T :TYPE-INITIALIZER "gdk_window_type_get_type")
  (:ROOT 0)
  (:TOPLEVEL 1)
  (:CHILD 2)
  (:TEMP 3)
  (:FOREIGN 4)
  (:OFFSCREEN 5)
  (:SUBSURFACE 6))
             (gobject::get-g-type-definition "GdkWindowType")))
  ;; Check the names
  (is (equal '("GDK_WINDOW_ROOT" "GDK_WINDOW_TOPLEVEL" "GDK_WINDOW_CHILD" "GDK_WINDOW_TEMP"
 "GDK_WINDOW_FOREIGN" "GDK_WINDOW_OFFSCREEN" "GDK_WINDOW_SUBSURFACE")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GdkWindowType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GdkWindowType"))))
  ;; Check the nick names
  (is (equal '("root" "toplevel" "child" "temp" "foreign" "offscreen" "subsurface")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GdkWindowType"))))
)

;;;     GdkWindowHints

;;; --- GdkGeometry ------------------------------------------------------------

(test gdk-geometry
 (let ((geometry (make-gdk-geometry)))
   (is (eq 'gdk-geometry (type-of geometry)))
   (is (= 0 (gdk-geometry-min-width geometry)))
   (is (= 0 (gdk-geometry-min-height geometry)))
   (is (= 0 (gdk-geometry-max-width geometry)))
   (is (= 0 (gdk-geometry-base-width geometry)))
   (is (= 0 (gdk-geometry-base-height geometry)))
   (is (= 0 (gdk-geometry-width-increment geometry)))
   (is (= 0 (gdk-geometry-height-increment geometry)))
   (is (= 0 (gdk-geometry-min-aspect geometry)))
   (is (= 0 (gdk-geometry-max-aspect geometry)))
   (is (eq :north-west (gdk-geometry-win-gravity geometry)))
 ))

#+nil
(test gdk-geometry-cstruct
  (let ((geometry (make-gdk-geometry)))
    (is (= 56 (foreign-type-size '(:struct gdk::gdk-geometry-cstruct))))
    (is (equal '(GDK::MIN-WIDTH GDK::MIN-HEIGHT GDK::MAX-WIDTH GDK::MAX-HEIGHT
                 GDK::BASE-WIDTH GDK::BASE-HEIGHT GDK::WIDTH-INCREMENT
                 GDK::HEIGHT-INCREMENT GDK::MIN-ASPECT GDK::MAX-ASPECT
                 GDK::WIN-GRAVITY)
               (foreign-slot-names '(:struct gdk::gdk-geometry-cstruct))))
    (let ((toplevel (make-instance 'gtk-window :type :toplevel)))
      (gtk-widget-show toplevel)
      (let ((window (gtk-widget-window toplevel)))
        ;; A check of the implementation of gdk-window-set-geometry-hints
        (with-foreign-object (ptr '(:struct gdk::gdk-geometry-cstruct))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::min-width)
                (gdk-geometry-min-width geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::min-height)
                (gdk-geometry-min-height geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::max-width)
                (gdk-geometry-max-width geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::max-height)
                (gdk-geometry-max-height geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::base-width)
                (gdk-geometry-base-width geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::base-height)
                (gdk-geometry-base-height geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::width-increment)
                (gdk-geometry-width-increment geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::height-increment)
                (gdk-geometry-height-increment geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::min-aspect)
                (gdk-geometry-min-aspect geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::max-aspect)
                (gdk-geometry-max-aspect geometry))
          (setf (foreign-slot-value ptr '(:struct gdk::gdk-geometry-cstruct) 'gdk::win-gravity)
                (gdk-geometry-win-gravity geometry))
          ;; Call the C function
          (gdk::%gdk-window-set-geometry-hints window ptr '(:min-size :max-size))
    )))))

;;;     GdkGravitgy
;;;     GdkAnchorHint
;;;     GdkWindowEdge
;;;     GdkWindowTypeHint

;;;     GdkWindowAttr

(test gdk-window-attr

  (is (eq 'gdk-window-attr (type-of (make-gdk-window-attr))))

  (let ((attributes (make-gdk-window-attr :title "title")))
    (is (equal "title" (gdk-window-attr-title attributes)))
  ))

;;;     GdkWindowAttributesType



;;;     GdkFullscreenMode
;;;     GdkFilterReturn
;;;     GdkModifierType        --> gdk.event-structures.lisp
;;;     GdkModifierIntent
;;;     GdkWMDecoration
;;;     GdkWMFunction
;;;
;;;
;;;     gdk_window_new

#+nil
(test gdk-window-new
  (let ((root-window (gdk-get-default-root-window))
        (attributes (make-gdk-window-attr :title  "title"
                                          :x       10
                                          :y       10
                                          :width  400
                                          :height 300))
        (attributes-mask '(:title :x :y)))
    ;; TODO: Causes a memory fault
;    (is-false (gdk-window-new root-window attributes attributes-mask))
))

;;;     gdk_window_destroy

;;; --- gdk-window-get-window-type ---------------------------------------------

#+nil
(test gdk-window-get-window-type
  (is (eq :root (gdk-window-get-window-type (gdk-get-default-root-window))))
  (let ((toplevel (make-instance 'gtk-window :type :toplevel))
        (child (make-instance 'gtk-button)))
    (gtk-container-add toplevel child)
    (gtk-widget-show-all toplevel)
    (is (eq 'gdk-window (type-of (gtk-widget-window toplevel))))
    (is (eq :toplevel (gdk-window-get-window-type (gtk-widget-window toplevel))))
    (is (eq 'gdk-window (type-of (gtk-widget-window child))))
    ;; Button is a child widget, but the GdkWindow is :toplevel
    (is (eq :toplevel (gdk-window-get-window-type (gtk-widget-window child))))
  ))

;;;     gdk_window_get_display

(test gdk-window-get-display
  (is (eq 'gdk-display (type-of (gdk-window-get-display (gdk-get-default-root-window)))))
)

;;;     gdk_window_get_screen

(test gdk-window-get-screen
  (is (eq 'gdk-screen (type-of (gdk-window-get-screen (gdk-get-default-root-window)))))
)

;;;     gdk_window_get_visual

(test gdk-window-get-visual
  (is (eq 'gdk-visual (type-of (gdk-window-get-visual (gdk-get-default-root-window)))))
)

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
;;;     gdk_window_fullscreen_on_monitor ()
;;;     gdk_window_unfullscreen
;;;     gdk_window_get_fullscreen_mode ()
;;;     gdk_window_set_fullscreen_mode ()
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited                          * deprecated
;;;     gdk_window_get_composited                          * deprecated
;;;     gdk_window_set_pass_through ()
;;;     gdk_window_get_pass_through ()
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_to_rect ()
;;;     gdk_window_move_region
;;;     gdk_window_flush                                   * deprecated
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
;;;     gdk_window_show_window_menu ()
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     gdk_window_get_scale_factor ()
;;;     gdk_window_set_opaque_region ()
;;;     gdk_window_create_gl_context ()
;;;     gdk_window_mark_paint_from_clip ()
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect                        * deprecated
;;;     gdk_window_begin_paint_region                      * deprecated
;;;     gdk_window_end_paint                               * deprecated
;;;     gdk_window_begin_draw_frame ()
;;;     gdk_window_end_draw_frame ()
;;;     gdk_window_get_visible_region
;;;     GdkWindowInvalidateHandlerFunc
;;;     gdk_window_set_invalidate_handler ()
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     GdkWindowChildFunc
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates                     * deprecated
;;;     gdk_window_process_updates                         * deprecated
;;;     gdk_window_set_debug_updates                       * deprecated
;;;     gdk_window_enable_synchronized_configure           * deprecated
;;;     gdk_window_configure_finished                      * deprecated
;;;     gdk_window_get_frame_clock ()
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
;;;     gdk_window_set_static_gravities                    * deprecated
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          * deprecated
;;;     gdk_window_set_background_rgba                     * deprecated
;;;     gdk_window_set_background_pattern                  * deprecated
;;;     gdk_window_get_background_pattern                  * deprecated
;;;     gdk_window_set_cursor                                Accessor
;;;     gdk_window_get_cursor                                Accessor
;;;     gdk_window_get_user_data

;;; --- gdk-window-get-geometry ------------------------------------------------

#+nil
(test gdk-window-get-geometry
  (let ((toplevel (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-show toplevel)
    (let ((window (gtk-widget-window toplevel)))
      (is (eq 'gtk-window (type-of toplevel)))
      (is (eq 'gdk-window (type-of window)))
      (multiple-value-bind (x y width height)
          (gdk-window-get-geometry window)
        (is (= 0 x))
        (is (= 0 y))
        (is (= 200 width))
        (is (= 200 height))
))))

;;; --- gdk-window-set-geometry-hints ------------------------------------------

#+nil
(test gdk-window-set-geometry-hints
  (let ((toplevel (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-show toplevel)
    (let ((window (gtk-widget-window toplevel)))
      (is (eq 'gtk-window (type-of toplevel)))
      (is (eq 'gdk-window (type-of window)))
      ;; TODO: Find a way to test the settings
      (gdk-window-set-geometry-hints window (make-gdk-geometry) '(:min-size :max-size))
)))

;;; --- gdk-window-get-width ---------------------------------------------------
;;; --- gdk-window-get-height --------------------------------------------------

#+nil
(test gdk-window-get-width
  (let ((toplevel (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-show toplevel)
    (let ((window (gtk-widget-window toplevel)))
      (is (eq 'gtk-window (type-of toplevel)))
      (is (eq 'gdk-window (type-of window)))
      (is (= 200 (gdk-window-get-width window)))
      (is (= 200 (gdk-window-get-height window)))
)))

;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint

;;; --- gdk-window-get-position ------------------------------------------------

#+nil
(test gdk-window-get-position
  (let ((toplevel (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-show toplevel)
    (let ((window (gtk-widget-window toplevel)))
      (is (eq 'gtk-window (type-of toplevel)))
      (is (eq 'gdk-window (type-of window)))
      (multiple-value-bind (x y)
          (gdk-window-get-position window)
        (is (= 0 x))
        (is (= 0 y))
))))

;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             * deprecated *
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

;;; --- gdk-get-default-root-window --------------------------------------------

(test gdk-get-default-root-window
  (let ((root-window (gdk-get-default-root-window)))
    (is (eq 'gdk-window (type-of root-window)))
    (is-true (integerp (gdk-window-get-width root-window)))
    (is-true (integerp (gdk-window-get-height root-window)))))

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

;;; --- gdk-window-coords-from-parent ------------------------------------------

(test gdk-window-coords-from-parent.1
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-from-parent window 20.0d0 10.0d0)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

(test gdk-window-coords-from-parent.2
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-from-parent window 20.0 10.0)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

(test gdk-window-coords-from-parent.3
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-from-parent window 20 10)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

;;; --- gdk-window-coords-to-parent --------------------------------------------

(test gdk-window-coords-to-parent.1
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-to-parent window 20.0d0 10.0d0)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

(test gdk-window-coords-to-parent.2
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-to-parent window 20.0 10.0)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

(test gdk-window-coords-to-parent.3
  (let ((window (gdk-get-default-root-window)))
    (multiple-value-bind (x y)
      (gdk-window-coords-to-parent window 20 10)
      (is (= 20.0d0 x))
      (is (= 10.0d0 y)))))

;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel

