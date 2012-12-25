;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
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

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "app-paintable" 'gtk-widget) 't)
 "Whether the application will paint directly on the widget.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "can-default" 'gtk-widget) 't)
 "Whether the widget can be the default widget.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "can-focus" 'gtk-widget) 't)
 "Whether the widget can accept the input focus.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "composite-child" 'gtk-widget) 't)
 "Whether the widget is part of a composite widget.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "double-buffered" 'gtk-widget) 't)
 "Whether the widget is double buffered.
  Default value: true.
  Since 2.18")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "events" 'gtk-widget) 't)
 "The event mask that decides what kind of @code{gdk-events} this widget
  gets.
  Default value: @code{gdk-structure-mask}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "expand" 'gtk-widget) 't)
 "Whether to expand in both directions. Setting this sets both @code{hexpand}
  and @code{vexpand}.
  Default value: false.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "halign" 'gtk-widget) 't)
 "How to distribute horizontal space if widget gets extra space, see
  @class{gtk-align}.
  Default value: @code{:fill}.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-default" 'gtk-widget) 't)
 "Whether the widget is the default widget.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-focus" 'gtk-widget) 't)
 "Whether the widget has the input focus.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "has-tooltip" 'gtk-widget) 't)
 "Enables or disables the emission of \"query-tooltip\" on widget. A value of
  true indicates that widget can have a tooltip, in this case the widget will
  be queried using \"query-tooltip\" to determine whether it will provide a
  tooltip or not.

  Note that setting this property to true for the first time will change the
  event masks of the @code{GdkWindows} of this widget to include leave-notify
  and motion-notify events. This cannot and will not be undone when the property
  is set to false again.

  Default value: false

  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "height-request" 'gtk-widget) 't)
 "Override for height request of the widget, or -1 if natural request should
  be used.
  Allowed values: >= @code{G_MAXULONG}.
  Default value: -1")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "hexpand" 'gtk-widget) 't)
 "Whether to expand horizontally. See @fun{gtk-widget-set-hexpand}.
  Default value: false.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "hexpand-set" 'gtk-widget) 't)
 "Whether to use the @code{hexpand} property. See
  @fun{gtk-widget-get-hexpand-set}.
  Default value: false.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "is-focus" 'gtk-widget) 't)
 "Whether the widget is the focus widget within the toplevel.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin" 'gtk-widget) 't)
 "Sets all four sides' margin at once. If read, returns max margin on any
  side.
  Allowed values: [0,32767].
  Default value: 0.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-bottom" 'gtk-widget) 't)
 "Margin on bottom side of widget.@br{}
  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  @fun{gtk-widget-set-size-request} for example.
  Allowed values: [0,32767].
  Default value: 0.
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-left" 'gtk-widget) 't)
 "Margin on left side of widget.

  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  gtk_widget_set_size_request() for example.

  Allowed values: [0,32767]

  Default value: 0

  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-right" 'gtk-widget) 't)
 "Margin on right side of widget.

  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  gtk_widget_set_size_request() for example.

  Allowed values: [0,32767]

  Default value: 0

  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "margin-top" 'gtk-widget) 't)
 "Margin on top side of widget.

  This property adds margin outside of the widget's normal size request, the
  margin will be added in addition to the size from
  gtk_widget_set_size_request() for example.

  Allowed values: [0,32767]

  Default value: 0

  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-widget) 't)
 "The name of the widget.
  Default value: NULL")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "no-show-all" 'gtk-widget) 't)
 "Whether @fun{gtk-widget-show-all} should not affect this widget.
  Default value: false")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "parent" 'gtk-widget) 't)
 "The parent widget of this widget. Must be a Container widget.")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "receives-default" 'gtk-widget) 't)
 "If TRUE, the widget will receive the default action when it is focused.
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "sensitive" 'gtk-widget) 't)
 "Whether the widget responds to input.
  Default value: true")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "style" 'gtk-widget) 't)
 "The style of the widget, which contains information about how it will look
  (colors etc).")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-markup" 'gtk-widget) 't)
 "Sets the text of tooltip to be the given string, which is marked up with the
  Pango text markup language. Also see gtk_tooltip_set_markup().

  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not NULL: \"has-tooltip\" will automatically be
  set to TRUE and there will be taken care of \"query-tooltip\" in the default
  signal handler.

  Default value: NULL

  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "tooltip-text" 'gtk-widget) 't)
 "Sets the text of tooltip to be the given string.

  Also see gtk_tooltip_set_text().

  This is a convenience property which will take care of getting the tooltip
  shown if the given string is not NULL: \"has-tooltip\" will automatically be
  set to TRUE and there will be taken care of \"query-tooltip\" in the default
  signal handler.

 Default value: NULL

  Since 2.12")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "valign" 'gtk-widget) 't)
 "How to distribute vertical space if widget gets extra space, see GtkAlign
  Default value: @code{:fill}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "vexpand" 'gtk-widget) 't)
 "Whether to expand vertically. See gtk_widget_set_vexpand().
  Default value: FALSE
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "vexpand-set" 'gtk-widget) 't)
 "Whether to use the \"vexpand\" property. See gtk_widget_get_vexpand_set().
  Default value: FALSE
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-widget) 't)
 "Whether the widget is visible.
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "width-request" 'gtk-widget) 't)
 "Override for width request of the widget, or -1 if natural request should
  be used.

  Allowed values: >= G_MAXULONG

  Default value: -1")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "window" 'gtk-widget) 't)
 "The widget's window if it is realized, NULL otherwise.
  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-requisition 'type)
 "@version{2012-12-22}
  @begin{short}
    A @code{gtk-requisition} represents the desired size of a widget.
  @end{short}
  See the section called “Height-for-width Geometry Management” for more
  information.
  @begin{pre}
(define-g-boxed-cstruct gtk-requisition \"GtkRequisition\"
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @begin{table}
    @entry[width]{the widget's desired width}
    @entry[height]{the widget's desired height}
  @end{table}
  @see-slot{gtk-requisition-width}
  @see-slot{gtk-requisition-height}
  @see-constructor{make-gtk-requisition}
  @see-constructor{copy-gtk-requisition}")

(setf (gethash 'gtk-requisition-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-requisition-width 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{width} of the @class{gtk-requisition} structure.
  @end{short}
  @see-class{gtk-requisition}")

(setf (gethash 'gtk-requisition-height atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-requisition-height 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{height} of the @class{gtk-requisition}
    structure.
  @end{short}
  @see-class{gtk-requisition}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-allocation 'type)
 "@version{2012-12-22}
  @begin{short}
    A @code{gtk-allocation} of a widget represents region which has been
    allocated to the widget by its parent.
  @end{short}
  It is a subregion of its parents allocation. See the section called
  “Height-for-width Geometry Management” for more information.
  @begin{pre}
(define-g-boxed-cstruct gtk-allocation \"GtkAllocation\"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @see-slot{gtk-allocation-x}
  @see-slot{gtk-allocation-y}
  @see-slot{gtk-allocation-width}
  @see-slot{gtk-allocation-height}
  @see-constructor{make-gtk-allocation}
  @see-constructor{copy-gtk-allociation}")

(setf (gethash 'gtk-allocation-x atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-x 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{x} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-y atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-y 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{y} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-width atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-width 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{width} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

(setf (gethash 'gtk-allocation-heigth atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-allocation-heigth 'function)
 "@version{2012-12-23}
  @begin{short}
    Accessor for the slot @code{heigth} of the @class{gtk-allocation}
    structure.
  @end{short}
  @see-class{gtk-allocation}")

;;; ----------------------------------------------------------------------------
;;; GtkWidget
;;;
;;; typedef struct _GtkWidget GtkWidget;
;;; ----------------------------------------------------------------------------

#|
(defcstruct %gtk-widget
;  (:object %gtk-object)
  (:private-flags :uint16)
  (:state :uint8)
  (:saved-state :uint8)
  (:name (:pointer :char))
  (:style :pointer)
  (:requisition gtk-requisition-cstruct)
  (:allocation gtk-allocation-cstruct)
  (:window :pointer)
  (:parent :pointer))
|#

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
;;;   GtkSizeRequestMode (* get_request_mode)       (GtkWidget *widget);
;;;
;;;   void               (* get_preferred_height)   (GtkWidget *widget,
;;;                                                  gint      *minimum_height,
;;;                                                  gint      *natural_height);
;;;   void               (* get_preferred_width_for_height)
;;;                                                 (GtkWidget *widget,
;;;                                                  gint       height,
;;;                                                  gint      *minimum_width,
;;;                                                  gint      *natural_width);
;;;   void               (* get_preferred_width)    (GtkWidget *widget,
;;;                                                  gint      *minimum_width,
;;;                                                  gint      *natural_width);
;;;   void               (* get_preferred_height_for_width)
;;;                                                 (GtkWidget *widget,
;;;                                                  gint       width,
;;;                                                  gint      *minimum_height,
;;;                                                  gint      *natural_height);
;;;
;;;   /* Mnemonics */
;;;   gboolean (* mnemonic_activate)       (GtkWidget           *widget,
;;;                                         gboolean             group_cycling);
;;;
;;;   /* explicit focus */
;;;   void     (* grab_focus)              (GtkWidget           *widget);
;;;   gboolean (* focus)                   (GtkWidget           *widget,
;;;                                         GtkDirectionType     direction);
;;;
;;;   /* keyboard navigation */
;;;   void     (* move_focus)              (GtkWidget           *widget,
;;;                                         GtkDirectionType     direction);
;;;   gboolean (* keynav_failed)           (GtkWidget           *widget,
;;;                                         GtkDirectionType     direction);
;;;
;;;   /* events */
;;;   gboolean (* event)                   (GtkWidget           *widget,
;;;                                         GdkEvent            *event);
;;;   gboolean (* button_press_event)      (GtkWidget           *widget,
;;;                                         GdkEventButton      *event);
;;;   gboolean (* button_release_event)    (GtkWidget           *widget,
;;;                                         GdkEventButton      *event);
;;;   gboolean (* scroll_event)            (GtkWidget           *widget,
;;;                                         GdkEventScroll      *event);
;;;   gboolean (* motion_notify_event)     (GtkWidget           *widget,
;;;                                         GdkEventMotion      *event);
;;;   gboolean (* delete_event)            (GtkWidget           *widget,
;;;                                         GdkEventAny         *event);
;;;   gboolean (* destroy_event)           (GtkWidget           *widget,
;;;                                         GdkEventAny         *event);
;;;   gboolean (* key_press_event)         (GtkWidget           *widget,
;;;                                         GdkEventKey         *event);
;;;   gboolean (* key_release_event)       (GtkWidget           *widget,
;;;                                         GdkEventKey         *event);
;;;   gboolean (* enter_notify_event)      (GtkWidget           *widget,
;;;                                         GdkEventCrossing    *event);
;;;   gboolean (* leave_notify_event)      (GtkWidget           *widget,
;;;                                         GdkEventCrossing    *event);
;;;   gboolean (* configure_event)         (GtkWidget           *widget,
;;;                                         GdkEventConfigure   *event);
;;;   gboolean (* focus_in_event)          (GtkWidget           *widget,
;;;                                         GdkEventFocus       *event);
;;;   gboolean (* focus_out_event)         (GtkWidget           *widget,
;;;                                         GdkEventFocus       *event);
;;;   gboolean (* map_event)               (GtkWidget           *widget,
;;;                                         GdkEventAny         *event);
;;;   gboolean (* unmap_event)             (GtkWidget           *widget,
;;;                                         GdkEventAny         *event);
;;;   gboolean (* property_notify_event)   (GtkWidget           *widget,
;;;                                         GdkEventProperty    *event);
;;;   gboolean (* selection_clear_event)   (GtkWidget           *widget,
;;;                                         GdkEventSelection   *event);
;;;   gboolean (* selection_request_event) (GtkWidget           *widget,
;;;                                         GdkEventSelection   *event);
;;;   gboolean (* selection_notify_event)  (GtkWidget           *widget,
;;;                                         GdkEventSelection   *event);
;;;   gboolean (* proximity_in_event)      (GtkWidget           *widget,
;;;                                         GdkEventProximity   *event);
;;;   gboolean (* proximity_out_event)     (GtkWidget           *widget,
;;;                                         GdkEventProximity   *event);
;;;   gboolean (* visibility_notify_event) (GtkWidget           *widget,
;;;                                         GdkEventVisibility  *event);
;;;   gboolean (* window_state_event)      (GtkWidget           *widget,
;;;                                         GdkEventWindowState *event);
;;;   gboolean (* damage_event)            (GtkWidget           *widget,
;;;                                         GdkEventExpose      *event);
;;;   gboolean (* grab_broken_event)       (GtkWidget           *widget,
;;;                                         GdkEventGrabBroken  *event);
;;;
;;;   /* selection */
;;;   void     (* selection_get)           (GtkWidget           *widget,
;;;                                         GtkSelectionData    *selection_data,
;;;                                         guint                info,
;;;                                         guint                time_);
;;;   void     (* selection_received)      (GtkWidget           *widget,
;;;                                         GtkSelectionData    *selection_data,
;;;                                         guint                time_);
;;;
;;;   /* Source side drag signals */
;;;   void     (* drag_begin)              (GtkWidget           *widget,
;;;                                         GdkDragContext      *context);
;;;   void     (* drag_end)                (GtkWidget           *widget,
;;;                                         GdkDragContext      *context);
;;;   void     (* drag_data_get)           (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         GtkSelectionData    *selection_data,
;;;                                         guint                info,
;;;                                         guint                time_);
;;;   void     (* drag_data_delete)        (GtkWidget           *widget,
;;;                                         GdkDragContext      *context);
;;;
;;;   /* Target side drag signals */
;;;   void     (* drag_leave)              (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         guint                time_);
;;;   gboolean (* drag_motion)             (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         gint                 x,
;;;                                         gint                 y,
;;;                                         guint                time_);
;;;   gboolean (* drag_drop)               (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         gint                 x,
;;;                                         gint                 y,
;;;                                         guint                time_);
;;;   void     (* drag_data_received)      (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         gint                 x,
;;;                                         gint                 y,
;;;                                         GtkSelectionData    *selection_data,
;;;                                         guint                info,
;;;                                         guint                time_);
;;;   gboolean (* drag_failed)             (GtkWidget           *widget,
;;;                                         GdkDragContext      *context,
;;;                                         GtkDragResult        result);
;;;
;;;   /* Signals used only for keybindings */
;;;   gboolean (* popup_menu)              (GtkWidget           *widget);
;;;
;;;   /* If a widget has multiple tooltips/whatsthis, it should show the
;;;    * one for the current focus location, or if that doesn't make
;;;    * sense, should cycle through them showing each tip alongside
;;;    * whatever piece of the widget it applies to.
;;;    */
;;;   gboolean (* show_help)               (GtkWidget           *widget,
;;;                                         GtkWidgetHelpType    help_type);
;;;
;;;   /* accessibility support
;;;    */
;;;   AtkObject *  (* get_accessible)      (GtkWidget         *widget);
;;;
;;;   void         (* screen_changed)      (GtkWidget         *widget,
;;;                                         GdkScreen         *previous_screen);
;;;   gboolean     (* can_activate_accel)  (GtkWidget         *widget,
;;;                                         guint              signal_id);
;;;
;;;   void         (* composited_changed)  (GtkWidget         *widget);
;;;
;;;   gboolean     (* query_tooltip)       (GtkWidget         *widget,
;;;                                         gint               x,
;;;                                         gint               y,
;;;                                         gboolean           keyboard_tooltip,
;;;                                         GtkTooltip        *tooltip);
;;;
;;;   void         (* compute_expand)     (GtkWidget          *widget,
;;;                                        gboolean           *hexpand_p,
;;;                                        gboolean           *vexpand_p);
;;;
;;;   void         (* adjust_size_request)    (GtkWidget      *widget,
;;;                                            GtkOrientation  orientation,
;;;                                            gint           *minimum_size,
;;;                                            gint           *natural_size);
;;;   void         (* adjust_size_allocation) (GtkWidget      *widget,
;;;                                            GtkOrientation  orientation,
;;;                                            gint           *minimum_size,
;;;                                            gint           *natural_size,
;;;                                            gint           *allocated_pos,
;;;                                            gint           *allocated_size);
;;;
;;;   void         (* style_updated)          (GtkWidget      *widget);
;;;
;;;   gboolean     (* touch_event)            (GtkWidget      *widget,
;;;                                            GdkEventTouch  *event);
;;; };
;;;
;;; GInitiallyUnownedClass parent_class;
;;;     The object class structure needs to be the first element in the widget
;;;     class structure in order for the class mechanism to work correctly. This
;;;     allows a GtkWidgetClass pointer to be cast to a GObjectClass pointer.
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
;;;     This is called by containers to obtain the minimum and natural height of
;;;     a widget. A widget that does not actually trade any height for width or
;;;     width for height only has to implement these two virtual methods
;;;     (GtkWidgetClass.get_preferred_width() and
;;;     GtkWidgetClass.get_preferred_height()).
;;;
;;; get_preferred_width_for_height ()
;;;     This is analogous to GtkWidgetClass.get_preferred_height_for_width()
;;;     except that it operates in the oposite orientation. It's rare that a
;;;     widget actually does GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT requests but this
;;;     can happen when, for example, a widget or container gets additional
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
;;;     is passed a contextual width to request height for. By implementing this
;;;     virtual method it is possible for a GtkLabel to tell its parent how much
;;;     height would be required if the label were to be allocated a said width.
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
;;;     containers in laying out the widget. adjust_size_request adjusts from a
;;;     child widget's original request to what a parent container should use
;;;     for layout. The for_size argument will be -1 if the request should not
;;;     be for a particular size in the opposing orientation, i.e. if the
;;;     request is not height-for-width or width-for-height. If for_size is
;;;     greater than -1, it is the proposed allocation in the opposing
;;;     orientation that we need the request for. Implementations of
;;;     adjust_size_request should chain up to the default implementation, which
;;;     applies GtkWidget's margin properties and imposes any values from
;;;     gtk_widget_set_size_request(). Chaining up should be last, after your
;;;     subclass adjusts the request, so GtkWidget can apply constraints and add
;;;     the margin properly.
;;;
;;; adjust_size_allocation ()
;;;     Convert an initial size allocation assigned by a GtkContainer using
;;;     gtk_widget_size_allocate(), into an actual size allocation to be used by
;;;     the widget. adjust_size_allocation adjusts to a child widget's actual
;;;     allocation from what a parent container computed for the child. The
;;;     adjusted allocation must be entirely within the original allocation. In
;;;     any custom implementation, chain up to the default GtkWidget
;;;     implementation of this method, which applies the margin and alignment
;;;     properties of GtkWidget. Chain up before performing your own adjustments
;;;     so your own adjustments remove more allocation after the GtkWidget base
;;;     class has already removed margin and alignment. The natural size passed
;;;     in should be adjusted in the same way as the allocated size, which
;;;     allows adjustments to perform alignments or other changes based on
;;;     natural size.
;;;
;;; style_updated ()
;;; touch_event ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCallback ()
;;;
;;; void (*GtkCallback) (GtkWidget *widget, gpointer data);
;;;
;;; The type of the callback functions used for e.g. iterating over the children
;;; of a container, see gtk_container_foreach().
;;;
;;; widget :
;;;     the widget to operate on
;;;
;;; data :
;;;     user-supplied data
;;; ----------------------------------------------------------------------------

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

(setf (gethash 'gtk-widget-help-type atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-widget-help-type atdoc:*external-symbols*)
 "@short{}
  @begin{pre}
(define-g-enum \"GtkWidgetHelpType\" gtk-widget-help-type
  (:export t
   :type-initializer \"gtk_widget_help_type_get_type\")
  (:tooltip 0)
  (:whats-this 1))
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_new ()
;;;
;;; GtkWidget * gtk_widget_new (GType type,
;;;                             const gchar *first_property_name,
;;;                             ...);
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

(setf (documentation 'gtk-widget-destroy 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @short{Destroys a widget.}

  When a widget is destroyed, it will break any references it holds to other
  objects. If the widget is inside a container, the widget will be removed
  from the container. If the widget is a toplevel (derived from
  @class{gtk-window}), it will be removed from the list of toplevels, and the
  reference GTK+ holds to it will be removed. Removing a widget from its
  container or the list of toplevels results in the widget being finalized,
  unless you've added additional references to the widget with
  @fun{g-object-ref}.

  In most cases, only toplevel widgets (windows) require explicit destruction,
  because when you destroy a toplevel its children will be destroyed as well.
  @see-class{gtk-widget}
  @see-class{gtk-window}
  @see-function{g-object-ref}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-in-destruction 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @return{true if @arg{widget} is being destroyed.}
  @short{Returns whether the @arg{widget} is currently being destroyed.}
  This information can sometimes be used to avoid doing unnecessary work.
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroyed ()
;;;
;;; void gtk_widget_destroyed (GtkWidget *widget, GtkWidget **widget_pointer);
;;;
;;; This function sets *widget_pointer to NULL if widget_pointer != NULL. It's
;;; intended to be used as a callback connected to the "destroy" signal of a
;;; widget. You connect gtk_widget_destroyed() as a signal handler, and pass the
;;; address of your widget variable as user data. Then when the widget is
;;; destroyed, the variable will be set to NULL. Useful for example to avoid
;;; multiple copies of the same dialog.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; widget_pointer :
;;;     address of a variable that contains widget
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-unparent 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @short{This function is only for use in widget implementations.}
  Should be called by implementations of the remove method on
  @class{gtk-container}, to dissociate a child from the container.
  @see-class{gtk-widget}
  @see-class{gtk-container}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-show 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @begin{short}
    Flags a widget to be displayed.
  @end{short}
  Any widget that isn't shown will not appear on the screen. If you want to show
  all the widgets in a container, it's easier to call @fun{gtk-widget-show-all}
  on the container, instead of individually showing the widgets.

  Remember that you have to show the containers containing a widget, in
  addition to the widget itself, before it will appear onscreen.

  When a toplevel container is shown, it is immediately realized and mapped;
  other shown widgets are realized and mapped when their toplevel container is
  realized and mapped.
  @see-class{gtk-widget}
  @see-function{gtk-widget-show-all}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-show-now 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @short{Shows a widget.}
  If the @arg{widget} is an unmapped toplevel widget (i. e. a @class{gtk-window}
  that has not yet been shown), enter the main loop and wait for the window to
  actually be mapped. Be careful; because the main loop is running, anything can
  happen during this function.
  @see-class{gtk-widget}
  @see-class{gtk-window}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-hide 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @begin{short} 
    Reverses the effects of @fun{gtk-widget-show}, causing the @arg{widget} to
    be hidden (invisible to the user).
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-widget-show}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-show-all 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @begin{short}
    Recursively shows a widget, and any child widgets (if the widget is a
    container).
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-widget-show}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-map 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @begin{short}
    This function is only for use in widget implementations.
  @end{short}
  Causes a widget to be mapped if it isn't already.
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-unmap 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @short{This function is only for use in widget implementations.}
  Causes a widget to be unmapped if it's currently mapped.
  @see-class{gtk-widegt}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-realize 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  @short{Creates the GDK (windowing system) resources associated with a widget.}
  For example, @code{widget->window} will be created when a widget is realized.
  Normally realization happens implicitly; if you show a widget and all its
  parent containers, then the widget will be realized and mapped automatically.

  Realizing a widget requires all the widget's parent widgets to be realized;
  calling @sym{gtk-widget-realize} realizes the widget's parents in addition to
  widget itself. If a widget is not yet inside a toplevel window when you
  realize it, bad things will happen.

  This function is primarily used in widget implementations, and isn't very
  useful otherwise. Many times when you think you might need it, a better
  approach is to connect to a signal that will be called after the widget is
  realized automatically, such as \"draw\". Or simply @fun{g-signal-connect} to
  the \"realize\" signal.
  @see-class{gtk-widget}
  @see-function{g-signal-connect}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-unrealize 'function)
 "@version{2012-12-23}
  @argument[widget]{a @class{gtk-widget}}
  This function is only useful in widget implementations. Causes a @arg{widget}
  to be unrealized (frees all GDK resources associated with the @arg{widget},
  such as @code{widget->window}).
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-queue-draw 'function)
 "@version{2012-12-23}
  @argument[widget]{the @arg{widget} to draw. It must be drawable (see
    @fun{gtk-widget-is-drawable}) and a size must have been allocated.}
  @argument[cr]{a cairo context to draw to}
  @begin{short}
    Draws @arg{widget} to a cairo context @arg{cr}.
  @end{short}
  The top left corner of the @arg{widget} will be drawn to the currently set
  origin point of the cairo context @arg{cr}.

  You should pass a cairo context as @arg{cr} argument that is in an original
  state. Otherwise the resulting drawing is undefined. For example changing the
  operator using cairo_set_operator() or the line width using
  @code{cairo_set_line_width()} might have unwanted side effects. You may
  however change the context's transform matrix - like with
  @code{cairo_scale()}, @code{cairo_translate()} or @code{cairo_set_matrix()}
  and clip region with @code{cairo_clip()} prior to calling this function. Also,
  it is fine to modify the context with @code{cairo_save()} and
  @code{cairo_push_group()} prior to calling this function.

  @begin[Note]{dictionary}
    Special purpose widgets may contain special code for rendering to the screen
    and might appear differently on screen and when rendered using
    @fun{gtk-widget-draw}.
  @end{dictionary}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-queue-draw 'function)
 "@argument[widget]{a GtkWidget}
  Equivalent to calling gtk_widget_queue_draw_area() for the entire area of
  a widget.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-queue-resize 'function)
 "@argument[widget]{a GtkWidget}
  This function is only for use in widget implementations. Flags a widget to
  have its size renegotiated; should be called when a widget for some reason
  has a new size request. For example, when you change the text in a GtkLabel,
  GtkLabel queues a resize to ensure there's enough space for the new text.

  Note

  You cannot call gtk_widget_queue_resize() on a widget from inside its
  implementation of the GtkWidgetClass::size_allocate virtual method. Calls to
  gtk_widget_queue_resize() from inside GtkWidgetClass::size_allocate will be
  silently ignored.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-queue-resize-no-redraw 'function)
 "@argument[widget]{a GtkWidget}
  This function works like @fun{gtk-widget-queue-resize}, except that the widget
  is not invalidated. Since 2.4.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-size-request 'function)
 "@argument[widget]{a GtkWidget}
  @argument[requisition]{a GtkRequisition to be filled in}
  Warning

  gtk_widget_size_request has been deprecated since version 3.0 and should not
  be used in newly-written code. Use gtk_widget_get_preferred_size() instead.

  This function is typically used when implementing a GtkContainer subclass.
  Obtains the preferred size of a widget. The container uses this information
  to arrange its child widgets and decide what size allocations to give them
  with gtk_widget_size_allocate().

  You can also call this function from an application, with some caveats. Most
  notably, getting a size request requires the widget to be associated with a
  screen, because font information may be needed. Multihead-aware applications
  should keep this in mind.

  Also remember that the size request is not necessarily the size a widget
  will actually be allocated.")

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
;;; gtk_widget_size_request() actually calls the "size_request" method on widget
;;; to compute the size request and fill in widget->requisition, and only then
;;; returns widget->requisition.
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
;;; In this function, the allocation may be adjusted. It will be forced to a 1x1
;;; minimum size, and the adjust_size_allocation virtual method on the child
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

(setf (documentation 'gtk-widget-add-accelerator 'function)
 "@argument[widget]{widget to install an accelerator on}
  @argument[accel-signal]{widget signal to emit on accelerator activation}
  @argument[accel-group]{accel group for this widget, added to its toplevel}
  @arguemnt[accel-key]{GDK keyval of the accelerator}
  @argument[accel-mods]{modifier key combination of the accelerator}
  @argument[accel-flags]{flag accelerators, e.g. GTK_ACCEL_VISIBLE}
  Installs an accelerator for this widget in accel_group that causes
  accel_signal to be emitted if the accelerator is activated. The accel_group
  needs to be added to the widget's toplevel via gtk_window_add_accel_group(),
  and the signal must be of type G_RUN_ACTION. Accelerators added through this
  function are not user changeable during runtime. If you want to support
  accelerators that can be changed by the user, use gtk_accel_map_add_entry()
  and gtk_widget_set_accel_path() or gtk_menu_item_set_accel_path() instead.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-remove-accelerator 'function)
 "@argument[widget]{widget to install an accelerator on}
  @argument[accel-group]{accel group for this widget}
  @argument[accel-key]{GDK keyval of the accelerator}
  @argument[accel-mods]{modifier key combination of the accelerator}
  @return{whether an accelerator was installed and could be removed}
  Removes an accelerator from @arg{widget}, previously installed with
  @fun{gtk-widget-add-accelerator}.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-set-accel-path 'function)
 "@argument[widget]{a GtkWidget}
  @argument[accel-path]{path used to look up the accelerator}
  @argument[accel-group]{a GtkAccelGroup}
  @begin{short}
    Given an accelerator group, accel_group, and an accelerator path,
    accel_path, sets up an accelerator in accel_group so whenever the key
    binding that is defined for accel_path is pressed, widget will be activated.
    This removes any accelerators (for any accelerator group) installed by
    previous calls to gtk_widget_set_accel_path(). Associating accelerators with
    paths allows them to be modified by the user and the modifications to be
    saved for future use. (See gtk_accel_map_save().)
  @end{short}

  This function is a low level function that would most likely be used by a
  menu creation system like GtkUIManager. If you use GtkUIManager, setting up
  accelerator paths will be done automatically.

  Even when you you aren't using GtkUIManager, if you only want to set up
  accelerators on menu items gtk_menu_item_set_accel_path() provides a
  somewhat more convenient interface.

  Note that accel_path string will be stored in a GQuark. Therefore, if you
  pass a static string, you can save some memory by interning it first with
  g_intern_static_string().")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_accel_closures ()
;;;
;;; GList * gtk_widget_list_accel_closures (GtkWidget *widget);
;;;
;;; Lists the closures used by widget for accelerator group connections with
;;; gtk_accel_group_connect_by_path() or gtk_accel_group_connect(). The closures
;;; can be used to monitor accelerator changes on widget, by connecting to the
;;; GtkAccelGroup::accel-changed signal of the GtkAccelGroup of a closure which
;;; can be found out with gtk_accel_group_from_accel_closure().
;;;
;;; widget :
;;;     widget to list accelerator closures for
;;;
;;; Returns :
;;;     a newly allocated GList of closures
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-can-activate-accel 'function)
 "@argument[widget]{a GtkWidget}
  @argument[signal_id]{the ID of a signal installed on widget}
  @return{TRUE if the accelerator can be activated.}
  Determines whether an accelerator that activates the signal identified by
  signal_id can currently be activated. This is done by emitting the
  \"can-activate-accel\" signal on widget; if the signal isn't overridden by a
  handler or in a derived widget, then the default check is that the widget
  must be sensitive, and the widget and all its ancestors mapped.

  Since 2.4")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-event 'function)
 "@argument[widget]{a GtkWidget}
  @argument[event]{a GdkEvent}
  @return{return from the event signal emission (TRUE if the event was handled)}
  Rarely-used function. This function is used to emit the event signals on a
  widget (those signals should never be emitted without using this function to
  do so). If you want to synthesize an event though, don't use this function;
  instead, use gtk_main_do_event() so the event will behave as if it were in
  the event queue. Don't synthesize expose events; instead, use
  gdk_window_invalidate_rect() to invalidate a region of the window.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-activate 'function)
 "@argument[widget]{a GtkWidget that's activatable}
  @return{TRUE if the widget was activatable}
  For widgets that can be \"activated\" (buttons, menu items, etc.) this
  function activates them. Activation is what happens when you press Enter on
  a widget during key navigation. If widget isn't activatable, the function
  returns FALSE.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-reparent 'function)
 "@argument[widget]{a GtkWidget}
  @argument[new-parent]{a GtkContainer to move the widget into}
  Moves a widget from one GtkContainer to another, handling reference count
  issues to avoid destroying the widget.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-intersect 'function)
 "@argument[widget]{a GtkWidget}
  @argument[area]{a rectangle}
  @arguemnt[intersection]{rectangle to store intersection of widget and area}
  @return{TRUE if there was an intersection}
  Computes the intersection of a widget's area and area, storing the
  intersection in intersection, and returns TRUE if there was an intersection.
  intersection may be NULL if you're only interested in whether there was an
  intersection.")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-widget-is-focus atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-is-focus 'function)
 "@argument[widget]{a @class{gtk-widget}}
  @return{true if the @arg{widget} is the focus widget}
  Determines if the @arg{widget} is the focus widget within its toplevel. (This
  does not mean that the @code{HAS_FOCUS} flag is necessarily set;
  @code{HAS_FOCUS} will only be set if the toplevel widget additionally has the
  global input focus.)")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-grab-focus 'function)
 "@argument[widget]{a @class{gtk-widget}}
  @begin{short}
    Causes @arg{widget} to have the keyboard focus for the @class{gtk-window}
    it's inside. @arg{widget} must be a focusable widget, such as a
    @class{gtk-entry}; something like @class{gtk-frame} won't work.
  @end{short}

  More precisely, it must have the @code{GTK_CAN_FOCUS} flag set. Use
  @fun{gtk-widget-set-can-focus} to modify that flag.

  The @arg{widget} also needs to be realized and mapped. This is indicated by
  the related signals. Grabbing the focus immediately after creating the
  @arg{widget} will likely fail and cause critical warnings.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-grab-default 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget}}
  Causes @arg{widget} to become the default widget. @arg{widget} must have the
  @code{GTK_CAN_DEFAULT} flag set; typically you have to set this flag yourself
  by calling @fun{gtk-widget-set-can-default}. The default widget is
  activated when the user presses Enter in a window. Default widgets must be
  activatable, that is, @fun{gtk-widget-activate} should affect them. Note that
  @class{gtk-entry} widgets require the \"activates-default\" property set to
  TRUE before they activate the default widget when Enter is pressed and the
  @class{gtk-entry} is focused.")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-widget-set-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-set-name 'function)
 "@argument[widget]{a @class{gtk-widget}}
  @argument[name]{name for the widget}
  @begin{short}
    Widgets can be named, which allows you to refer to them from a CSS file. You
    can apply a style to widgets with a particular name in the CSS file. See the
    documentation for the CSS syntax (on the same page as the docs for
    @class{gtk-style-context}).
  @end{short}

  Note that the CSS syntax has certain special characters to delimit and
  represent elements in a selector (period, #, >, *...), so using these will
  make your widget impossible to match by name. Any combination of
  alphanumeric symbols, dashes and underscores will suffice.")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-widget-get-name atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-get-name 'function)
 "@argument[widget]{a @class{gtk-widget}}
  @return{name of the widget. This string is owned by GTK+ and should not be
    modified or freed}
  Retrieves the name of a @arg{widget}. See @fun{gtk-widget-set-name} for the
  significance of widget names.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-widget-set-state 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget}}
  @argument[state]{new state for @arg{widget}}
  @begin{short}
    Warning:
    @sym{gtk-widget-set-state} is deprecated and should not be used in
    newly-written code. 3.0. Use @fun{gtk-widget-set-state-flags} instead.
  @end{short}

  This function is for use in widget implementations. Sets the state of a
  widget (insensitive, prelighted, etc.) Usually you should set the state
  using wrapper functions such as @fun{gtk-widget-set-sensitive}.")

;;; ----------------------------------------------------------------------------

(setf (gethash 'gtk-widget-set-sensitive atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-widget-set-sensitive 'function)
 "@version{2012-12-20}
  @argument[widget]{a @class{gtk-widget}}
  @argument[sensitive]{TRUE to make the widget sensitive}
  Sets the sensitivity of a widget. A widget is sensitive if the user can
  interact with it. Insensitive widgets are \"grayed out\" and the user can't
  interact with them. Insensitive widgets are known as \"inactive\",
  \"disabled\", or \"ghosted\" in some other toolkits.")

#|
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

(declaim (inline gkt-widget-set-parent))

(defun gtk-widget-set-parent (widget parent)
  (setf (gtk-widget-parent widget) parent))

(export 'gtk-widget-set-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_parent_window ()
;;;
;;; void gtk_widget_set_parent_window (GtkWidget *widget,
;;;                                    GdkWindow *parent_window);
;;;
;;; Sets a non default parent window for widget.
;;;
;;; For GtkWindow classes, setting a parent_window effects whether the window is
;;; a toplevel window or can be embedded into other widgets.
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
;;;     the new parent window
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-parent-window))

(defun gtk-widget-set-parent-window (widget parent-window)
  (setf (gtk-widget-parent-window widget) parent-window))

(export 'gtk-widget-set-parent-window)

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
;;;     the parent window of widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-parent-window))

(defun gtk-widget-get-parent-window (widget)
  (gtk-widget-parent-window widget))

(export 'gtk-widget-get-parent-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_events ()
;;;
;;; void gtk_widget_set_events (GtkWidget *widget, gint events);
;;;
;;; Sets the event mask (see GdkEventMask) for a widget. The event mask
;;; determines which events a widget will receive. Keep in mind that different
;;; widgets have different default event masks, and by changing the event mask
;;; you may disrupt a widget's functionality, so be careful. This function must
;;; be called while a widget is unrealized. Consider gtk_widget_add_events() for
;;; widgets that are already realized, or if you want to preserve the existing
;;; event mask. This function can't be used with GTK_NO_WINDOW widgets; to get
;;; events on those widgets, place them inside a GtkEventBox and receive events
;;; on the event box.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; events :
;;;     event mask
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-events))

(defun gtk-widget-set-events (widget events)
  (setf (gtk-widget-events widget) events))

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

(declaim (inline gtk-widget-get-events))

(defun gtk-widget-get-events (widget)
  (gtk-widget-events widget))

(export 'gtk-widget-get-events)

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
;;; determines which events a widget will receive from device. Keep in mind that
;;; different widgets have different default event masks, and by changing the
;;; event mask you may disrupt a widget's functionality, so be careful. This
;;; function must be called while a widget is unrealized. Consider
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
;;; Returns the events mask for the widget corresponding to an specific device.
;;; These are the events that the widget will receive when device operates on
;;; it.
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
;;; Adds the device events in the bitfield events to the event mask for widget.
;;; See gtk_widget_set_device_events() for details.
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
;;; gboolean gtk_widget_get_device_enabled (GtkWidget *widget,
;;;                                         GdkDevice *device);
;;;
;;; Returns whether device can interact with widget and its children.
;;; See gtk_widget_set_device_enabled().
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
;;;     the topmost ancestor of widget, or widget itself if there's no ancestor
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-toplevel))

(defun gtk-widget-get-toplevel (widget)
  (gtk-widget-toplevel widget))

(export 'gtk-widget-get-toplevel)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_ancestor ()
;;;
;;; GtkWidget * gtk_widget_get_ancestor (GtkWidget *widget, GType widget_type);
;;;
;;; Gets the first ancestor of widget with type widget_type. For example,
;;; gtk_widget_get_ancestor (widget, GTK_TYPE_BOX) gets the first GtkBox that's
;;; an ancestor of widget. No reference will be added to the returned widget; it
;;; should not be unreferenced. See note about checking for a toplevel GtkWindow
;;; in the docs for gtk_widget_get_toplevel().
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
;;;     the ancestor widget, or NULL if not found
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_ancestor" gtk-widget-get-ancestor)
    (g-object gtk-widget)
  (widget (g-object gtk-widget))
  (type g-type))

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
;;;     the visual for widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-visual))

(defun gtk-widget-get-visual (widget)
  (gtk-widget-visual widget))

(export 'gtk-widget-get-visual)

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

(declaim (inline gtk-widget-set-visual))

(defun gtk-widget-set-visual (widget visual)
  (setf (gtk-widget-visual widget) visual))

(export 'gtk-widget-set-visual)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pointer ()
;;;
;;; void gtk_widget_get_pointer (GtkWidget *widget, gint *x, gint *y);
;;;
;;; Warning
;;;
;;; gtk_widget_get_pointer has been deprecated since version 3.4 and should not
;;; be used in newly-written code. Use gdk_window_get_device_position() instead.
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
;;;     return location for the X coordinate, or NULL
;;;
;;; y :
;;;     return location for the Y coordinate, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_pointer" %gtk-widget-get-pointer) :void
  (widget g-object)
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-widget-get-pointer (widget)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-widget-get-pointer widget x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-widget-get-pointer)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_ancestor ()
;;;
;;; gboolean gtk_widget_is_ancestor (GtkWidget *widget, GtkWidget *ancestor);
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
;;;                                            gint *dest_y);
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
;;;     location to store X position relative to dest_widget
;;;
;;; dest_y :
;;;     location to store Y position relative to dest_widget
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
;;; gboolean gtk_widget_hide_on_delete (GtkWidget *widget);
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
;;; gtk_widget_set_style has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use GtkStyleContext instead.
;;;
;;; Used to set the GtkStyle for a widget (widget->style). Since GTK 3, this
;;; function does nothing, the passed in style is ignored.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; style :
;;;     a GtkStyle, or NULL to remove the effect of a previous call to
;;;     gtk_widget_set_style() and go back to the default style
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_ensure_style ()
;;;
;;; void gtk_widget_ensure_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_ensure_style has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use GtkStyleContext instead
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
;;;     the widget's GtkStyle
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_rc_styles ()
;;;
;;; void gtk_widget_reset_rc_styles (GtkWidget *widget);
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
;;; GtkStyle * gtk_widget_get_default_style (void);
;;;
;;; Warning
;;;
;;; gtk_widget_get_default_style has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use GtkStyleContext instead, and
;;; gtk_css_provider_get_default() to obtain a GtkStyleProvider with the default
;;; widget style information.
;;;
;;; Returns the default style used by all widgets initially.
;;;
;;; Returns :
;;;     the default style. This GtkStyle object is owned by GTK+ and should not
;;;     be modified or freed
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
|#

(setf (gethash 'gtk-text-direction atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-text-direction atdoc:*external-symbols*)
 "@version{2012-12-23}
  @short{}
  @begin{pre}
(define-g-enum \"GtkTextDirection\" gtk-text-direction
  (:export t
   :type-initializer \"gtk_text_direction_get_type\")
  (:none 0)
  (:ltr 1)
  (:rtl 2))
  @end{pre}")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_direction ()
;;;
;;; GtkTextDirection gtk_widget_get_direction (GtkWidget *widget);
;;;
;;; Gets the reading direction for a particular widget.
;;; See gtk_widget_set_direction().
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
;;; void gtk_widget_set_default_direction (GtkTextDirection dir);
;;;
;;; Sets the default reading direction for widgets where the direction has not
;;; been explicitly set by gtk_widget_set_direction().
;;;
;;; dir :
;;;     the new default direction. This cannot be GTK_TEXT_DIR_NONE.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_set_default_direction" gtk-widget-set-default-direction)
    :void
  (direction gtk-text-direction))

(export 'gtk-widget-set-default-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_default_direction ()
;;;
;;; GtkTextDirection gtk_widget_get_default_direction (void);
;;;
;;; Obtains the current default reading direction.
;;; See gtk_widget_set_default_direction().
;;;
;;; Returns :
;;;     the current default direction.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_default_direction" gtk-widget-get-default-direction)
    gtk-text-direction)

(export 'gtk-widget-get-default-direction)

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
;;;     shape to be added, or NULL to remove an existing shape
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
;;;     shape to be added, or NULL to remove an existing shape
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
;;;     location to store length of the path, or NULL
;;;
;;; path :
;;;     location to store allocated path string, or NULL
;;;
;;; path_reversed :
;;;     location to store allocated reverse path string, or NULL
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
;;;     location to store the length of the class path, or NULL
;;;
;;; path :
;;;     location to store the class path as an allocated string, or NULL
;;;
;;; path_reversed :
;;;     location to store the reverse class path as an allocated string,
;;;     or NULL
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

(declaim (inline gtk-widget-get-composite-name))

(defun gtk-widget-get-composite-name (widget)
  (gtk-widget-composite-name widget))

(export 'gtk-widget-get-composite-name)

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
;;;     gtk_widget_override_background_color()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_override_background_color"
           gtk-widget-override-background-color) :void
  (widget (g-object gtk-widget))
  (state gtk-state-flags)
  (color (g-boxed-foreign gdk-rgba)))

(export 'gtk-widget-override-background-color)

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
;;; This API is mostly meant as a quick way for applications to change a widget
;;; appearance. If you are developing a widgets library and intend this change
;;; to be themeable, it is better done by setting meaningful CSS classes and
;;; regions in your widget/container implementation through
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
;;;     gtk_widget_override_color()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_override_color" gtk-widget-override-color) :void
  (widget (g-object gtk-widget))
  (state gtk-state-flags)
  (color (g-boxed-foreign gdk-rgba)))

(export 'gtk-widget-override-color)

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
;;;     the font description to use, or NULL to undo the effect of previous
;;;     calls to gtk_widget_override_font()
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_override_font" gtk-widget-override-font) :void
  (widget (g-object gtk-widget))
  (font-desc (g-boxed-foreign pango-font-description)))

(export 'gtk-widget-override-font)

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
;;;     the color to assign (does not need to be allocated), or NULL to undo the
;;;     effect of previous calls to gtk_widget_override_symbolic_color()
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
;;; Sets the cursor color to use in a widget, overriding the "cursor-color" and
;;; "secondary-cursor-color" style properties. All other style values are left
;;; untouched. See also gtk_widget_modify_style().
;;;
;;; Note that the underlying properties have the GdkColor type, so the alpha
;;; value in primary and secondary will be ignored.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; cursor :
;;;     the color to use for primary cursor (does not need to be allocated), or
;;;     NULL to undo the effect of previous calls to of
;;;     gtk_widget_override_cursor()
;;;
;;; secondary_cursor :
;;;     the color to use for secondary cursor (does not need to be allocated),
;;;     or NULL to undo the effect of previous calls to of
;;;     gtk_widget_override_cursor()
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
;;; gtk_widget_modify_style has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use GtkStyleContext with a custom
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
;;; other hand, if you first call gtk_widget_modify_style(), subsequent calls to
;;; such functions gtk_widget_modify_fg() will have a cumulative effect with the
;;; initial modifications.
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
;;; GtkStyleProvider instead.
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
;;;     the modifier style for the widget. This rc style is owned by the widget.
;;;     If you want to keep a pointer to value this around, you must add a
;;;     refcount using g_object_ref()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_fg ()
;;;
;;; void gtk_widget_modify_fg (GtkWidget *widget,
;;;                            GtkStateType state,
;;;                            const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_fg has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gtk_widget_override_color() instead
;;;
;;; Sets the foreground color for a widget in a particular state.
;;;
;;; All other style values are left untouched.
;;; See also gtk_widget_modify_style().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; state :
;;;     the state for which to set the foreground color
;;;
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo the
;;;     effect of previous calls to of gtk_widget_modify_fg()
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
;;;                            const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_bg has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gtk_widget_override_background_color()
;;; instead.
;;;
;;; Sets the background color for a widget in a particular state.
;;;
;;; All other style values are left untouched.
;;; See also gtk_widget_modify_style().
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
;;;     the effect of previous calls to of gtk_widget_modify_bg()
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
;;;                              const GdkColor *color);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_text has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_color() instead
;;;
;;; Sets the text color for a widget in a particular state.
;;;
;;; All other style values are left untouched. The text color is the foreground
;;; color used along with the base color (see gtk_widget_modify_base()) for
;;; widgets such as GtkEntry and GtkTextView.
;;; See also gtk_widget_modify_style().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; state :
;;;     the state for which to set the text color
;;;
;;; color :
;;;     the color to assign (does not need to be allocated), or NULL to undo the
;;;     effect of previous calls to of gtk_widget_modify_text()
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
;;;                              const GdkColor *color);
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
;;;     the color to assign (does not need to be allocated), or NULL to undo the
;;;     effect of previous calls to of gtk_widget_modify_base()
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
;;; gtk_widget_modify_font has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gtk_widget_override_font() instead.
;;;
;;; Sets the font to use for a widget.
;;;
;;; All other style values are left untouched.
;;; See also gtk_widget_modify_style().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; font_desc :
;;;     the font description to use, or NULL to undo the effect of previous
;;;     calls to gtk_widget_modify_font()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_cursor ()
;;;
;;; void gtk_widget_modify_cursor (GtkWidget *widget,
;;;                                const GdkColor *primary,
;;;                                const GdkColor *secondary);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_cursor is deprecated and should not be used in
;;; newly-written code. 3.0. Use gtk_widget_override_cursor() instead.
;;;
;;; Sets the cursor color to use in a widget, overriding the "cursor-color" and
;;; "secondary-cursor-color" style properties.
;;;
;;; All other style values are left untouched.
;;; See also gtk_widget_modify_style().
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
;;; PangoContext * gtk_widget_create_pango_context (GtkWidget *widget);
;;;
;;; Creates a new PangoContext with the appropriate font map, font description,
;;; and base direction for drawing text for this widget. See also
;;; gtk_widget_get_pango_context().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the new PangoContext
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
;;; If you create and keep a PangoLayout using this context, you must deal with
;;; changes to the context by calling pango_layout_context_changed() on the
;;; layout in response to the "style-updated" and "direction-changed" signals
;;; for the widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the PangoContext for the widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_layout ()
;;;
;;; PangoLayout * gtk_widget_create_pango_layout (GtkWidget *widget,
;;;                                               const gchar *text);
;;;
;;; Creates a new PangoLayout with the appropriate font map, font description,
;;; and base direction for drawing text for this widget.
;;;
;;; If you keep a PangoLayout created in this way around, in order to notify the
;;; layout of changes to the base direction or font of this widget, you must
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
;;;     the new PangoLayout
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
;;;                                     const gchar *detail);
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
;;; application and should not be modified. The pixbuf should be freed after use
;;; with g_object_unref().
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
;;;     one of the available sizes)
;;;
;;; detail :
;;;     render detail to pass to theme engine
;;;
;;; Returns :
;;;     a new pixbuf, or NULL if the stock ID wasn't known
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
;;;                                            GtkIconSize size);
;;;
;;; A convenience function that uses the theme engine and style settings for
;;; widget to look up stock_id and render it to a pixbuf. stock_id should be a
;;; stock icon ID such as GTK_STOCK_OPEN or GTK_STOCK_OK. size should be a size
;;; such as GTK_ICON_SIZE_MENU.
;;;
;;; The pixels in the returned GdkPixbuf are shared with the rest of the
;;; application and should not be modified. The pixbuf should be freed after use
;;; with g_object_unref().
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
;;;     one of the available sizes)
;;;
;;; Returns :
;;;     a new pixbuf, or NULL if the stock ID wasn't known
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_pop_composite_child ()
;;;
;;; void gtk_widget_pop_composite_child (void);
;;;
;;; Cancels the effect of a previous call to gtk_widget_push_composite_child().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_pop_composite_child" gtk-widget-pop-composite-child)
    :void)

(export 'gtk-widget-pop-composite-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_push_composite_child ()
;;;
;;; void gtk_widget_push_composite_child (void);
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
;;; gtk_widget_push_composite_child ();
;;; scrolled_window->hscrollbar = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL,
;;;                                                  hadjustment);
;;; gtk_widget_set_composite_name (scrolled_window->hscrollbar, "hscrollbar");
;;; gtk_widget_pop_composite_child ();
;;; gtk_widget_set_parent (scrolled_window->hscrollbar,
;;;                        GTK_WIDGET (scrolled_window));
;;; g_object_ref (scrolled_window->hscrollbar);
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_push_composite_child" gtk-widget-push-composite-child)
    :void)

(export 'gtk-widget-push-composite-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_area ()
;;;
;;; void gtk_widget_queue_draw_area (GtkWidget *widget,
;;;                                  gint x,
;;;                                  gint y,
;;;                                  gint width,
;;;                                  gint height);
;;;
;;; Convenience function that calls gtk_widget_queue_draw_region() on the region
;;; created from the given coordinates.
;;;
;;; The region here is specified in widget coordinates. Widget coordinates are a
;;; bit odd; for historical reasons, they are defined as widget->window
;;; coordinates for widgets that are not GTK_NO_WINDOW widgets, and are relative
;;; to widget->allocation.x, widget->allocation.y for widgets that are
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
;;;                                    const cairo_region_t *region);
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
;;;                                    gboolean app_paintable);
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

(declaim (inline gtk-widget-set-app-paintable))

(defun gtk-widget-set-app-paintable (widget app-paintable)
  (setf (gtk-widget-app-paintable widget) app-paintable))

(export 'gtk-widget-set-app-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_double_buffered ()
;;;
;;; void gtk_widget_set_double_buffered (GtkWidget *widget,
;;;                                      gboolean double_buffered);
;;;
;;; Widgets are double buffered by default; you can use this function to turn
;;; off the buffering. "Double buffered" simply means that
;;; gdk_window_begin_paint_region() and gdk_window_end_paint() are called
;;; automatically around expose events sent to the widget.
;;; gdk_window_begin_paint() diverts all drawing to a widget's window to an
;;; offscreen buffer, and gdk_window_end_paint() draws the buffer to the screen.
;;; The result is that users see the window update in one smooth step, and don't
;;; see individual graphics primitives being rendered.
;;;
;;; In very simple terms, double buffered widgets don't flicker, so you would
;;; only use this function to turn off double buffering if you had special needs
;;; and really knew what you were doing.
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

(declaim (inline gtk-widget-set-double-buffered))

(defun gtk-widget-set-double-buffered (widget double-buffered)
  (setf (gtk-widget-double-buffered widget) double-buffered))

(export 'gtk-widget-set-double-buffered)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_redraw_on_allocate ()
;;;
;;; void gtk_widget_set_redraw_on_allocate (GtkWidget *widget,
;;;                                         gboolean redraw_on_allocate);
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
;;;     if TRUE, the entire widget will be redrawn when it is allocated to a new
;;;     size. Otherwise, only the new portion of the widget will be redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_composite_name ()
;;;
;;; void gtk_widget_set_composite_name (GtkWidget *widget, const gchar *name);
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

(declaim (inline gtk-widget-set-composite-name))

(defun gtk-widget-set-composite-name (widget name)
  (setf (gtk-widget-composite-name widget) name))

(export 'gtk-widget-set-composite-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_mnemonic_activate ()
;;;
;;; gboolean gtk_widget_mnemonic_activate (GtkWidget *widget,
;;;                                        gboolean group_cycling);
;;;
;;; Emits the "mnemonic-activate" signal.
;;;
;;; The default handler for this signal activates the widget if group_cycling is
;;; FALSE, and just grabs the focus if group_cycling is TRUE.
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
;;;                                               GParamSpec *pspec);
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
;;;     property with that name
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
;;;                                                       guint *n_properties);
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
;;;     g_free()
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
;;;                                               const cairo_region_t *region);
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
    cairo-region-t
  (widget (g-object gtk-widget))
  (region cairo-region-t))

(export 'gtk-widget-region-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_expose ()
;;;
;;; gint gtk_widget_send_expose (GtkWidget *widget, GdkEvent *event);
;;;
;;; Very rarely-used function. This function is used to emit an expose event on
;;; a widget. This function is not normally used directly. The only time it is
;;; used is when propagating an expose event to a child NO_WINDOW widget, and
;;; that is normally done using gtk_container_propagate_draw().
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
;;; gboolean gtk_widget_send_focus_change (GtkWidget *widget, GdkEvent *event);
;;;
;;; Sends the focus change event to widget
;;;
;;; This function is not meant to be used by applications. The only time it
;;; should be used is when it is necessary for a GtkWidget to assign focus to a
;;; widget that is semantically owned by the first widget even though it's not a
;;; direct child - for instance, a search entry in a floating window similar to
;;; the quick search in GtkTreeView.
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
;;;                                     GValue *value);
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

(defcfun ("gtk_widget_style_get_property" %gtk-widget-style-get-property) :void
  (widget (g-object gtk-widget))
  (property-name :string)
  (value (:pointer g-value)))

;; TODO: Check the implementation. We have to pass a pointer for widget.

(defun gtk-widget-style-get-property (widget property-name)
  (let ((property-type (gtype (gtk-widget-style-property-type widget
                                                              property-name))))
  (with-foreign-object (value 'g-value)
    ;; TODO: Check the implementation of g-value-zero and g-value-init
    ;;       This can be simplified.
    (g-value-zero value)
    (g-value-init value property-type)
    (prog2
      (%gtk-widget-style-get-property widget property-name value)
      (parse-g-value value)
      (g-value-unset value)))))

(export 'gtk-widget-style-get-property)

;;; ----------------------------------------------------------------------------

(defun gtk-widget-style-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
      (let ((g-param-spec (gtk-widget-class-find-style-property class
                                                                property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'gtk-widget-style-property-info)

;;; ----------------------------------------------------------------------------

(defun gtk-widget-style-property-type (widget property-name)
  (let ((property-info (gtk-widget-style-property-info
                                                   (g-type-from-instance widget)
                                                   property-name)))
    (param-spec-type property-info)))

(export 'gtk-widget-style-property-type)

;;; ----------------------------------------------------------------------------

;; This implementation is wrong.

(defun gtk-widget-style-property-value (widget property-name
                                               &optional property-type)
  (unless property-type
    (setf property-type
          (gtk-widget-style-property-type widget property-name)))
  (setf property-type (gtype property-type))
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue property-type)
    (prog1 (%gtk-widget-style-get-property widget property-name gvalue)
      (g-value-unset gvalue))))

(export 'gtk-widget-style-property-value)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_valist ()
;;;
;;; void gtk_widget_style_get_valist (GtkWidget *widget,
;;;                                   const gchar *first_property_name,
;;;                                   va_list var_args);
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
;;; void gtk_widget_style_attach (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_style_attach is deprecated and should not be used in
;;; newly-written code. 3.0. This step is unnecessary with GtkStyleContext.
;;;
;;; This function attaches the widget's GtkStyle to the widget's GdkWindow. It
;;; is a replacement for
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
;;;                                            GType type);
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
;;;                                            AtkRole role);
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
;;; AtkObject * gtk_widget_get_accessible (GtkWidget *widget);
;;;
;;; Returns the accessible object that describes the widget to an assistive
;;; technology.
;;;
;;; If accessibility support is not available, this AtkObject instance may be a
;;; no-op. Likewise, if no class-specific AtkObject implementation is available
;;; for the widget instance in question, it will inherit an AtkObject
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
;;;     the AtkObject associated with widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_focus ()
;;;
;;; gboolean gtk_widget_child_focus (GtkWidget *widget,
;;;                                  GtkDirectionType direction);
;;;
;;; This function is used by custom widget implementations; if you're writing an
;;; app, you'd use gtk_widget_grab_focus() to move the focus to a particular
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
;;;                               const gchar *child_property);
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
;;; void gtk_widget_freeze_child_notify (GtkWidget *widget);
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
;;; gboolean gtk_widget_get_child_visible (GtkWidget *widget);
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
;;; GtkWidget * gtk_widget_get_parent (GtkWidget *widget);
;;;
;;; Returns the parent container of widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the parent container of widget, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-parent))

(defun gtk-widget-get-parent (widget)
  (gtk-widget-parent widget))

(export 'gtk-widget-get-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_settings ()
;;;
;;; GtkSettings * gtk_widget_get_settings (GtkWidget *widget);
;;;
;;; Gets the settings object holding the settings used for this widget.
;;;
;;; Note that this function can only be called when the GtkWidget is attached to
;;; a toplevel, since the settings object is specific to a particular GdkScreen.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the relevant GtkSettings object.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_settings" gtk-widget-get-settings) g-object
  (widget g-object))

(export 'gtk-widget-get-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_clipboard ()
;;;
;;; GtkClipboard * gtk_widget_get_clipboard (GtkWidget *widget,
;;;                                          GdkAtom selection);
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
;;;     persistent for all time.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_clipboard" gtk-widget-get-clipboard)
    (g-object gtk-clipboard)
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string))

(export 'gtk-widget-get-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_display ()
;;;
;;; GdkDisplay * gtk_widget_get_display (GtkWidget *widget);
;;;
;;; Get the GdkDisplay for the toplevel window associated with this widget. This
;;; function can only be called after the widget has been added to a widget
;;; hierarchy with a GtkWindow at the top.
;;;
;;; In general, you should only create display specific resources when a widget
;;; has been realized, and you should free those resources when the widget is
;;; unrealized.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the GdkDisplay for the toplevel for this widget
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_display" gtk-widget-get-display) g-object
  (widget g-object))

(export 'gtk-widget-get-display)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_root_window ()
;;;
;;; GdkWindow * gtk_widget_get_root_window (GtkWidget *widget);
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
;;;     the GdkWindow root window for the toplevel for this widget
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_root_window" gtk-widget-get-root-window) g-object
  (widget g-object))

(export 'gtk-widget-get-root-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_screen ()
;;;
;;; GdkScreen * gtk_widget_get_screen (GtkWidget *widget);
;;;
;;; Get the GdkScreen from the toplevel window associated with this widget. This
;;; function can only be called after the widget has been added to a widget
;;; hierarchy with a GtkWindow at the top.
;;;
;;; In general, you should only create screen specific resources when a widget
;;; has been realized, and you should free those resources when the widget is
;;; unrealized.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the GdkScreen for the toplevel for this widget
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_screen" gtk-widget-get-screen) g-object
  (widget g-object))

(export 'gtk-widget-get-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_screen ()
;;;
;;; gboolean gtk_widget_has_screen (GtkWidget *widget);
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
;;;     return location for width, or NULL
;;;
;;; height :
;;;     return location for height, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-size-request))

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
;;; The child visibility can be set for widget before it is added to a container
;;; with gtk_widget_set_parent(), to avoid mapping children unnecessary before
;;; immediately unmapping them. However it will be reset to its default state of
;;; TRUE when the widget is removed from a container.
;;;
;;; Note that changing the child visibility of a widget does not queue a resize
;;; on the widget. Most of the time, the size of a widget is computed from all
;;; visible children, whether or not they are mapped. If this is not the case,
;;; the container can queue a resize itself.
;;;
;;; This function is only useful for container implementations and never should
;;; be called by an application.
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
;;; In most cases, gtk_window_set_default_size() is a better choice for toplevel
;;; windows than this function; setting the default size will still allow users
;;; to shrink the window. Setting the size request will force them to leave the
;;; window at least as large as the size request. When dealing with window
;;; sizes, gtk_window_set_geometry_hints() can be a useful function as well.
;;;
;;; Note the inherent danger of setting any fixed size - themes, translations
;;; into other languages, different fonts, and user action can all change the
;;; appropriate size for a given widget. So, it's basically impossible to
;;; hardcode a size that will always be correct.
;;;
;;; The size request of a widget is the smallest size a widget can accept while
;;; still functioning well and drawing itself correctly. However in some strange
;;; cases a widget may be allocated less than its requested size, and in many
;;; cases a widget may be allocated more space than it requested.
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

(declaim (inline gtk-widget-set-size-request))

(defun gtk-widget-set-size-request (widget width height)
  (setf (gtk-widget-width-request widget) width)
  (setf (gtk-widget-height-request widget) height))

(export 'gtk-widget-set-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_thaw_child_notify ()
;;;
;;; void gtk_widget_thaw_child_notify (GtkWidget *widget);
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
;;; void gtk_widget_set_no_show_all (GtkWidget *widget, gboolean no_show_all);
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

(declaim (inline gtk-widget-set-no-show-all))

(defun gtk-widget-set-no-show-all (widget no-show-all)
  (setf (gtk-widget-no-show-all widget) no-show-all))

(export 'gtk-widget-set-no-show-all)

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

(declaim (inline gtk-widget-get-no-show-all))

(defun gtk-widget-get-no-show-all (widget)
  (gtk-widget-no-show-all widget))

(export 'gtk-widget-get-no-show-all)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_mnemonic_labels ()
;;;
;;; GList * gtk_widget_list_mnemonic_labels (GtkWidget *widget);
;;;
;;; Returns a newly allocated list of the widgets, normally labels, for which
;;; this widget is the target of a mnemonic (see for example,
;;; gtk_label_set_mnemonic_widget()).
;;;
;;; The widgets in the list are not individually referenced. If you want to
;;; iterate through the list and perform actions involving callbacks that might
;;; destroy the widgets, you must call
;;; g_list_foreach (result, (GFunc)g_object_ref, NULL) first, and then unref all
;;; the widgets afterwards.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the list of mnemonic labels; free this list with g_list_free() when you
;;;     are done with it
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
;;; void gtk_widget_add_mnemonic_label (GtkWidget *widget, GtkWidget *label);
;;;
;;; Adds a widget to the list of mnemonic labels for this widget. (See
;;; gtk_widget_list_mnemonic_labels()). Note the list of mnemonic labels for the
;;; widget is cleared when the widget is destroyed, so the caller must make sure
;;; to update its internal state at this point as well, by using a connection to
;;; the "destroy" signal or a weak notifier.
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
;;; void gtk_widget_remove_mnemonic_label (GtkWidget *widget, GtkWidget *label);
;;;
;;; Removes a widget from the list of mnemonic labels for this widget. (See
;;; gtk_widget_list_mnemonic_labels()). The widget must have previously been
;;; added to the list with gtk_widget_add_mnemonic_label().
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
;;; gboolean gtk_widget_is_composited (GtkWidget *widget);
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
;;; void gtk_widget_error_bell (GtkWidget *widget);
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

(defcfun ("gtk_widget_error_bell" gtk-widget-error-bell) :void
  (widget g-object))

(export 'widget-error-bell)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_keynav_failed ()
;;;
;;; gboolean gtk_widget_keynav_failed (GtkWidget *widget,
;;;                                    GtkDirectionType direction);
;;;
;;; This function should be called whenever keyboard navigation within a single
;;; widget hits a boundary. The function emits the "keynav-failed" signal on the
;;; widget and its return value should be interpreted in a way similar to the
;;; return value of gtk_widget_child_focus():
;;;
;;; When TRUE is returned, stay in the widget, the failed keyboard navigation is
;;; Ok and/or there is nowhere we can/should move the focus to.
;;;
;;; When FALSE is returned, the caller should continue with keyboard navigation
;;; outside the widget, e.g. by calling gtk_widget_child_focus() on the widget's
;;; toplevel.
;;;
;;; The default ::keynav-failed handler returns TRUE for GTK_DIR_TAB_FORWARD and
;;; GTK_DIR_TAB_BACKWARD. For the other values of GtkDirectionType, it looks at
;;; the "gtk-keynav-cursor-only" setting and returns FALSE if the setting is
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

(declaim (inline gtk-widget-get-tooltip-markup))

(defun gtk-widget-get-tooltip-markup (widget)
  (gtk-widget-tooltip-markup widget))

(export 'gtk-widget-get-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_tooltip_markup ()
;;;
;;; void gtk_widget_set_tooltip_markup (GtkWidget *widget, const gchar *markup);
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
;;;     the contents of the tooltip for widget, or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-tooltip-markup))

(defun gtk-widget-set-tooltip-markup (widget markup)
  (setf (gtk-widget-tooltip-markup widget) markup))

(export 'gtk-widget-set-tooltip-markup)

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

(declaim (inline gtk-get-tooltip-text))

(defun gtk-widget-get-tooltip-text (widget)
  (gtk-widget-tooltip-text widget))

(export 'gtk-widget-get-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_tooltip_text ()
;;;
;;; void gtk_widget_set_tooltip_text (GtkWidget *widget, const gchar *text);
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

(declaim (inline gtk-widget-set-tooltip-text))

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
;;;     The GtkWindow of the current tooltip
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-tooltip-window))

(defun gtk-widget-get-tooltip-window (widget)
  (gtk-widget-tooltip-window widget))

(export 'gtk-widget-get-tooltip-window)

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
;;;     a GtkWindow, or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-tooltip-window))

(defun gtk-widget-set-tooltip-window (widget custom-window)
  (setf (gtk-widget-tooltip-window widget) custom-window))

(export 'gtk-widget-set-tooltip-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_has_tooltip ()
;;;
;;; gboolean gtk_widget_get_has_tooltip (GtkWidget *widget);
;;;
;;; Returns the current value of the has-tooltip property. See "has-tooltip" for
;;; more information.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     current value of has-tooltip on widget.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-has-tooltip))

(defun gtk-widget-get-has-tooltip (widget)
  (gtk-widget-has-tooltip widget))

(export 'gtk-widget-get-has-tooltip)

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

(declaim (inline gtk-widget-set-has-tooltip))

(defun gtk-widget-set-has-tooltip (widget has-tooltip)
  (setf (gtk-widget-has-tooltip widget) has-tooltip))

(export 'gtk-widget-set-has-tooltip)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_trigger_tooltip_query ()
;;;
;;; void gtk_widget_trigger_tooltip_query (GtkWidget *widget);
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
;;;     widget's window
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-window))

(defun gtk-widget-get-window (widget)
  (gtk-widget-window widget))

(export 'gtk-widget-get-window)

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_should_draw_window ()
;;;
;;; gboolean gtk_cairo_should_draw_window (cairo_t *cr, GdkWindow *window);
;;;
;;; This function is supposed to be called in "draw" implementations for widgets
;;; that support multiple windows. cr must be untransformed from invoking of the
;;; draw function. This function will return TRUE if the contents of the given
;;; window are supposed to be drawn and FALSE otherwise. Note that when the
;;; drawing was not initiated by the windowing system this function will return
;;; TRUE for all windows, so you need to draw the bottommost window first. Also,
;;; do not use "else if" statements to check which window should be drawn.
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
;;; Transforms the given cairo context cr that from widget-relative coordinates
;;; to window-relative coordinates. If the widget's window is not an ancestor of
;;; window, no modification will be applied.
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

(defcfun ("gtk_widget_get_allocated_width" gtk-widget-get-allocated-width) :int
  (widget (g-object gtk-widget)))

(export 'gtk-widget-get-allocated-width)

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

(defcfun ("gtk_widget_get_allocated_height" gtk-widget-get-allocated-height)
    :int
  (widget (g-object gtk-widget)))

(export 'gtk-widget-get-allocated-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocation ()
;;;
;;; void gtk_widget_get_allocation (GtkWidget *widget,
;;;                                 GtkAllocation *allocation);
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

(defcfun ("gtk_widget_set_allocation" gtk-widget-set-allocation) :void
  (widget (g-object gtk-widget))
  (allocation (g-boxed-foreign gdk-rectangle)))

(export 'gtk-widget-set-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_app_paintable ()
;;;
;;; gboolean gtk_widget_get_app_paintable (GtkWidget *widget);
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

(declaim (inline gtk-widget-get-app-paintable))

(defun gtk-widget-get-app-paintable (widget)
  (gtk-widget-app-paintable widget))

(export 'gtk-widget-get-app-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_can_default ()
;;;
;;; gboolean gtk_widget_get_can_default (GtkWidget *widget);
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

(declaim (inline gkt-widget-get-can-default))

(defun gtk-widget-get-can-default (widget)
  (gtk-widget-can-default widget))

(export 'gtk-widget-get-can-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_can_default ()
;;;
;;; void gtk_widget_set_can_default (GtkWidget *widget, gboolean can_default);
;;;
;;; Specifies whether widget can be a default widget. See
;;; gtk_widget_grab_default() for details about the meaning of "default".
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; can_default :
;;;     whether or not widget can be a default widget.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-can-default))

(defun gtk-widget-set-can-default (widget can-default)
  (setf (gtk-widget-can-default widget) can-default))

(export 'gtk-widget-set-can-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_can_focus ()
;;;
;;; gboolean gtk_widget_get_can_focus (GtkWidget *widget);
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

(declaim (inline gtk-widget-get-can-focus))

(defun gtk-widget-get-can-focus (widget)
  (gtk-widget-can-focus widget))

(export 'gtk-widget-get-can-focus)

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

(declaim (inline gtk-widget-set-can-focus))

(defun gtk-widget-set-can-focus (widget can-focus)
  (setf (gtk-widget-can-focus widget) can-focus))

(export 'gtk-widget-set-can-focus)

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

(declaim (inline gtk-widget-get-double-buffered))

(defun gtk-widget-get-double-buffered (widget)
  (gtk-widget-double-buffered widget))

(export 'gtk-widget-get-double-buffered)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_has_window ()
;;;
;;; gboolean gtk_widget_get_has_window (GtkWidget *widget);
;;;
;;; Determines whether widget has a GdkWindow of its own.
;;; See gtk_widget_set_has_window().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget has a window, FALSE otherwise
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_get_has_window" gtk-widget-get-has-window) :boolean
  (widget (g-object gtk-widget)))

(export 'gtk-widget-get-has-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_has_window ()
;;;
;;; void gtk_widget_set_has_window (GtkWidget *widget, gboolean has_window);
;;;
;;; Specifies whether widget has a GdkWindow of its own. Note that all realized
;;; widgets have a non-NULL "window" pointer (gtk_widget_get_window() never
;;; returns a NULL window when a widget is realized), but for many of them it's
;;; actually the GdkWindow of one of its parent widgets. Widgets that do not
;;; create a window for themselves in "realize" must announce this by calling
;;; this function with has_window = FALSE.
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

(defcfun ("gtk_widget_set_has_window" gtk-widget-set-has-window) :void
  (widget (g-object gtk-widget))
  (has-window :boolean))

(export 'gtk-widget-set-has-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_sensitive ()
;;;
;;; gboolean gtk_widget_get_sensitive (GtkWidget *widget);
;;;
;;; Returns the widget's sensitivity (in the sense of returning the value that
;;; has been set using gtk_widget_set_sensitive()).
;;;
;;; The effective sensitivity of a widget is however determined by both its own
;;; and its parent widget's sensitivity. See gtk_widget_is_sensitive().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget is sensitive
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-sensitive))

(defun gtk-widget-get-sensitive (widget)
  (gtk-widget-sensitive widget))

(export 'gtk-widget-get-sensitive)

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

(defcfun ("gtk_widget_is_sensitive" gtk-widget-is-sensitive) :boolean
  (widget (g-object gtk-widget)))

(export 'gtk-widget-is-sensitive)

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

(declaim (inline gtk-widget-get-visible))

(defun gtk-widget-get-visible (widget)
  (gtk-widget-visible widget))

(export 'gtk-widget-get-visible)

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

(declaim (inline gtk-widget-set-visible))

(defun gtk-widget-set-visible (widget visible)
  (setf (gtk-widget-visible widget) visible))

(export 'gtk-widget-set-visible)

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
;;; void gtk_widget_unset_state_flags (GtkWidget *widget, GtkStateFlags flags);
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
;;; Determines whether widget is the current default widget within its toplevel.
;;; See gtk_widget_set_can_default().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget is the current default widget within its toplevel, FALSE
;;;     otherwise
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;; Is implemented as the accessor of the property "has-default"

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_focus ()
;;;
;;; gboolean gtk_widget_has_focus (GtkWidget *widget);
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

;; Is implemented as the accessor of the property "has-focus"

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_visible_focus ()
;;;
;;; gboolean gtk_widget_has_visible_focus (GtkWidget *widget);
;;;
;;; Determines if the widget should show a visible indication that it has the
;;; global input focus. This is a convenience function for use in ::draw
;;; handlers that takes into account whether focus indication should currently
;;; be shown in the toplevel window of widget. See
;;; gtk_window_get_focus_visible() for more information about focus indication.
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
;;; gtk_widget_has_rc_style has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use GtkStyleContext instead.
;;;
;;; Determines if the widget style has been looked up through the rc mechanism.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget has been looked up through the rc mechanism, FALSE
;;;     otherwise.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_drawable ()
;;;
;;; gboolean gtk_widget_is_drawable (GtkWidget *widget);
;;;
;;; Determines whether widget can be drawn to. A widget can be drawn to if it is
;;; mapped and visible.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget is drawable, FALSE otherwise
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_is_drawable" gtk-widget-is-drawable) :boolean
  (widget (g-object gtk-widget)))

(export 'gtk-widget-is-drawable)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_toplevel ()
;;;
;;; gboolean gtk_widget_is_toplevel (GtkWidget *widget);
;;;
;;; Determines whether widget is a toplevel widget.
;;;
;;; Currently only GtkWindow and GtkInvisible (and out-of-process GtkPlugs) are
;;; toplevel widgets. Toplevel widgets have no parent widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if widget is a toplevel, FALSE otherwise
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_widget_is_toplevel" gtk-widget-is-toplevel) :boolean
  (widget (g-object gtk-widget)))

(export 'gtk-widget-is-toplevel)

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
;;;     a GdkWindow
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-window))

(defun gtk-widget-set-window (widget window)
  (setf (gtk-widget-window widget) window))

(export 'gtk-widget-set-window)

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

(declaim (inline gtk-widget-set-receives-default))

(defun gtk-widget-set-receives-default (widget receives-default)
  (setf (gtk-widget-receives-default widget) receives-default))

(export 'gtk-widget-set-receives-default)

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

(declaim (inline gtk-widget-get-receives-default))

(defun gtk-widget-get-receives-default (widget)
  (gtk-widget-receives-default widget))

(export 'gtk-widget-get-receives-default)

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
;;; void gtk_widget_set_realized (GtkWidget *widget, gboolean realized);
;;;
;;; Marks the widget as being realized.
;;;
;;; This function should only ever be called in a derived widget's "realize" or
;;; "unrealize" implementation.
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
;;; gtk_widget_get_requisition has been deprecated since version 3.0 and should
;;; not be used in newly-written code. The GtkRequisition cache on the widget
;;; was removed, If you need to cache sizes across requests and allocations, add
;;; an explicit cache to the widget in question instead.
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
;;;     a pointer to a GtkRequisition to copy to
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
;;; gtk_widget_get_modifier_mask ()
;;;
;;; GdkModifierType gtk_widget_get_modifier_mask (GtkWidget *widget,
;;;                                               GdkModifierIntent intent);
;;;
;;; Returns the modifier mask the widget's windowing system backend uses for a
;;; particular purpose.
;;;
;;; See gdk_keymap_get_modifier_mask().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; intent :
;;;     the use case for the modifier mask
;;;
;;; Returns :
;;;     the modifier mask used for intent.
;;;
;;; Since 3.4
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
;;;     The GtkWidgetPath representing widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style_context ()
;;;
;;; GtkStyleContext * gtk_widget_get_style_context (GtkWidget *widget);
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
;;;     a new empty GtkRequisition. The newly allocated GtkRequisition should be
;;;     freed with gtk_requisition_free().
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
;;; void gtk_requisition_free (GtkRequisition *requisition);
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
;;;     location to store the minimum height, or NULL
;;;
;;; natural_height :
;;;     location to store the natural height, or NULL
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
;;;     location to store the minimum width, or NULL
;;;
;;; natural_width :
;;;     location to store the natural width, or NULL
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
;;;     location for storing the minimum height, or NULL
;;;
;;; natural_height :
;;;     location for storing the natural height, or NULL
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
;;;     location for storing the minimum width, or NULL
;;;
;;; natural_width :
;;;     location for storing the natural width, or NULL
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
;;; GtkBin widgets generally propagate the preference of their child, container
;;; widgets need to request something either in context of their children or in
;;; context of their allocation capabilities.
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
;;; Retrieves the minimum and natural size of a widget, taking into account the
;;; widget's preference for height-for-width management.
;;;
;;; This is used to retrieve a suitable size by container widgets which do not
;;; impose any restrictions on the child placement. It can be used to deduce
;;; toplevel window and menu sizes as well as child widgets in free-form
;;; containers such as GtkLayout.
;;;
;;; Note
;;;
;;; Handle with care. Note that the natural height of a height-for-width widget
;;; will generally be a smaller size than the minimum height, since the required
;;; height for the natural width is generally smaller than the required height
;;; for the minimum width.
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
;;;     An array of structs with a client pointer and a minimum/natural size in
;;;     the orientation of the allocation.
;;;
;;; Returns :
;;;     The remainder of extra_space after redistributing space to sizes.
;;; ----------------------------------------------------------------------------

|#

(setf (gethash 'gtk-align atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-align atdoc:*external-symbols*)
 "Controls how a widget deals with extra space in a single (x or y) dimension.

  Alignment only matters if the widget receives a \"too large\" allocation, for
  example if you packed the widget with the \"expand\" flag inside a GtkBox,
  then the widget might get extra space. If you have for example a 16x16 icon
  inside a 32x32 space, the icon could be scaled and stretched, it could be
  centered, or it could be positioned to one side of the space.

  Note that in horizontal context @code{:start} and @code{:align} are
  interpreted relative to text direction.
  @begin{pre}
(define-g-enum \"GtkAlign\" gtk-align
  (:export t
   :type-initializer \"gtk_align_get_type\")
  (:fill 0)
  (:start 1)
  (:end 2)
  (:center 3))
  @end{pre}
  @begin{table}
    @entry[:fill]{stretch to fill all space if possible, center if no meaningful
      way to stretch}
    @entry[:start]{snap to left or top side, leaving space on right or bottom}
    @entry[:end]{snap to right or bottom side, leaving space on left or top}
    @entry[:center]{center natural width of widget inside the allocation}
  @end{table}")

#|


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

(declaim (inline gtk-widget-get-halign))

(defun gtk-widget-get-halign (widget)
  (gtk-widget-halign widget))

(export 'gtk-widget-get-halign)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_halign ()
;;;
;;; void gtk_widget_set_halign (GtkWidget *widget, GtkAlign align);
;;;
;;; Sets the horizontal alignment of widget. See the "halign" property.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; align :
;;;     the horizontal alignment
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-set-halign))

(defun gtk-widget-set-halign (widget align)
  (setf (gtk-widget-halign widget) align))

(export 'gtk-widget-set-halign)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_valign ()
;;;
;;; GtkAlign gtk_widget_get_valign (GtkWidget *widget);
;;;
;;; Gets the value of the "valign" property.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the vertical alignment of widget
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-widget-get-valign))

(defun gtk-widget-get-valign (widget)
  (gtk-widget-valign widget))

(export 'gtk-widget-get-valign)

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

(declaim (inline gtk-widget-set-valign))

(defun gtk-widget-set-valign (widget align)
  (setf (gtk-widget-valign widget) align))

(export 'gtk-widget-set-valign)

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

(declaim (inline gtk-widget-get-margin-left))

(defun gtk-widget-get-margin-left (widget)
  (gtk-widget-margin-left widget))

(export 'gtk-widget-get-margin-left)

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

(declaim (inline gtk-widget-set-margin-left))

(defun gtk-widget-set-margin-left (widget margin)
  (setf (gtk-widget-margin-left widget) margin))

(export 'gtk-widget-set-margin-left)

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

(declaim (inline gtk-widget-get-margin-right))

(defun gtk-widget-get-margin-right (widget)
  (gtk-widget-margin-right widget))

(export 'gtk-widget-get-margin-right)

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

(declaim (inline gtk-widget-set-margin-right))

(defun gtk-widget-set-margin-right (widget margin)
  (setf (gtk-widget-margin-right widget) margin))

(export 'gtk-widget-set-margin-right)

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

(declaim (inline gtk-widget-get-margin-top))

(defun gtk-widget-get-margin-top (widget)
  (gtk-widget-margin-top widget))

(export 'gtk-widget-get-margin-top)

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

(declaim (inline gtk-widget-set-margin-top))

(defun gtk-widget-set-margin-top (widget margin)
  (setf (gtk-widget-margin-top widget) margin))

(export 'gtk-widget-set-margin-top)

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

(declaim (inline gtk-widget-get-margin-bottom))

(defun gtk-widget-get-margin-bottom (widget)
  (gtk-widget-margin-bottom widget))

(export 'gtk-widget-get-margin-bottom)

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

(declaim (inline gtk-widget-set-margin-bottom))

(defun gtk-widget-set-margin-bottom (widget margin)
  (setf (gtk-widget-margin-top widget) margin))

(export 'gtk-widget-set-margin-bottom)

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
;;; Containers should use gtk_widget_compute_expand() rather than this function,
;;; to see whether a widget, or any of its children, has the expand flag set. If
;;; any child of a widget wants to expand, the parent may ask to expand also.
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

(declaim (inline gtk-widget-get-hexpand))

(defun gtk-widget-get-hexpand (widget)
  (gtk-widget-hexpand widget))

(export 'gtk-widget-get-hexpand)

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

(declaim (inline gtk-widget-set-hexpand))

(defun gtk-widget-set-hexpand (widget expand)
  (setf (gtk-widget-hexpand widget) expand))

(export 'gtk-widget-set-hexpand)

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

(declaim (inline gtk-widget-get-hexpand-set))

(defun gtk-widget-get-hexpand-set (widget)
  (gtk-widget-hexpand-set widget))

(export 'gtk-widget-get-hexpand-set)

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

(declaim (inline gtk-widget-set-hexpand-set))

(defun gtk-widget-set-hexpand-set (widget set)
  (setf (gtk-widget-hexpand-set widget) set))

(export 'gtk-widget-set-hexpand-set)

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

(declaim (inline gtk-widget-get-vexpand))

(defun gtk-widget-get-vexpand (widget)
  (gtk-widget-vexpand widget))

(export 'gtk-widget-get-vexpand)

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

(declaim (inline gtk-widget-set-vexpand))

(defun gtk-widget-set-vexpand (widget expand)
  (setf (gtk-widget-vexpand widget) expand))

(export 'gtk-widget-set-vexpand)

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

(declaim (inline gtk-widget-get-vexpand-set))

(defun gtk-widget-get-vexpand-set (widget)
  (gtk-widget-vexpand-set widget))

(export 'gtk-widget-get-vexpand-set)

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

(declaim (inline gtk-widget-set-vexpand-set))

(defun gtk-widget-set-vexpand-set (widget set)
  (setf (gtk-widget-vexpand-set widget) set))

(export 'gtk-widget-set-vexpand-set)

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

|#

;;; --- End of file atdoc-gtk.widget.lisp --------------------------------------
