;;; ----------------------------------------------------------------------------
;;; gtk.drag-and-drop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Drag and Drop
;;;
;;; Functions for controlling drag and drop handling
;;;
;;; Synopsis
;;;
;;;     GtkDestDefaults
;;;     GtkTargetFlags
;;;
;;;     gtk_drag_dest_set
;;;     gtk_drag_dest_set_proxy
;;;     gtk_drag_dest_unset
;;;     gtk_drag_dest_find_target
;;;     gtk_drag_dest_get_target_list
;;;     gtk_drag_dest_set_target_list
;;;     gtk_drag_dest_add_text_targets
;;;     gtk_drag_dest_add_image_targets
;;;     gtk_drag_dest_add_uri_targets
;;;     gtk_drag_dest_set_track_motion
;;;     gtk_drag_dest_get_track_motion
;;;     gtk_drag_finish
;;;     gtk_drag_get_data
;;;     gtk_drag_get_source_widget
;;;     gtk_drag_highlight
;;;     gtk_drag_unhighlight
;;;     gtk_drag_begin
;;;     gtk_drag_set_icon_widget
;;;     gtk_drag_set_icon_pixbuf
;;;     gtk_drag_set_icon_stock
;;;     gtk_drag_set_icon_surface
;;;     gtk_drag_set_icon_name
;;;     gtk_drag_set_icon_gicon
;;;     gtk_drag_set_icon_default
;;;     gtk_drag_check_threshold
;;;     gtk_drag_source_set
;;;     gtk_drag_source_set_icon_pixbuf
;;;     gtk_drag_source_set_icon_stock
;;;     gtk_drag_source_set_icon_name
;;;     gtk_drag_source_set_icon_gicon
;;;     gtk_drag_source_unset
;;;     gtk_drag_source_set_target_list
;;;     gtk_drag_source_get_target_list
;;;     gtk_drag_source_add_text_targets
;;;     gtk_drag_source_add_image_targets
;;;     gtk_drag_source_add_uri_targets
;;;
;;; Description
;;;
;;; GTK+ has a rich set of functions for doing inter-process communication via
;;; the drag-and-drop metaphor. GTK+ can do drag-and-drop (DND) via multiple
;;; protocols. The currently supported protocols are the Xdnd and Motif
;;; protocols.
;;;
;;; As well as the functions listed here, applications may need to use some
;;; facilities provided for Selections. Also, the Drag and Drop API makes use of
;;; signals in the GtkWidget class.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkDestDefaults
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkDestDefaults" gtk-dest-defaults
  (:export t
   :type-initializer "gtk_dest_defaults_get_type")
  (:motion 1)
  (:highlight 2)
  (:drop 4)
  (:all 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-dest-defaults atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-dest-defaults atdoc:*external-symbols*)
 "@version{2013-4-17}
  @begin{short}
    The @sym{gtk-dest-defaults} enumeration specifies the various types of
    action that will be taken on behalf of the user for a drag destination site.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkDestDefaults\" gtk-dest-defaults
  (:export t
   :type-initializer \"gtk_dest_defaults_get_type\")
  (:motion 1)
  (:highlight 2)
  (:drop 4)
  (:all 7))
  @end{pre}
  @begin[code]{table}
    @entry[:motion]{If set for a widget, GTK+, during a drag over this widget
      will check if the drag matches this widget's list of possible targets and
      actions. GTK+ will then call the function @fun{gdk-drag-status} as
      appropriate.}
    @entry[:highlight]{If set for a widget, GTK+ will draw a highlight on this
      widget as long as a drag is over this widget and the widget drag format
      and action are acceptable.}
    @entry[:drop]{If set for a widget, when a drop occurs, GTK+ will will check
      if the drag matches this widget's list of possible targets and actions. If
      so, GTK+ will call the function @fun{gtk-drag-get-data} on behalf of the
      widget. Whether or not the drop is successful, GTK+ will call the function
      @fun{gtk-drag-finish}. If the action was a move, then if the drag was
      successful, then @em{true} will be passed for the delete parameter to
      the function @fun{gtk-drag-finish}.}
    @entry[:all]{If set, specifies that all default actions should be taken.}
  @end{table}
  @see-function{gdk-drag-status}
  @see-function{gtk-drag-get-data}
  @see-function{gtk-drag-finish}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTargetFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTargetFlags" gtk-target-flags
  (:export t
   :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-target-flags atdoc:*external-symbols*)
 "@version{2013-4-17}
  @begin{short}
    The @sym{gtk-target-flags} enumeration is used to specify constraints on an
    entry in a @code{GtkTargetTable}.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkTargetFlags\" gtk-target-flags
  (:export t
   :type-initializer \"gtk_target_flags_get_type\")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))
  @end{pre}
  @begin[code]{table}
    @entry[:same-app]{If this is set, the target will only be selected for drags
      within a single application.}
    @entry[:same-widget]{If this is set, the target will only be selected for
      drags within a single widget.}
    @entry[:other-app]{If this is set, the target will not be selected for drags
      within a single application.}
    @entry[:other-widget]{If this is set, the target will not be selected for
      drags within a single widget.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set ()
;;;
;;; void gtk_drag_dest_set (GtkWidget *widget,
;;;                         GtkDestDefaults flags,
;;;                         const GtkTargetEntry *targets,
;;;                         gint n_targets,
;;;                         GdkDragAction actions);
;;;
;;; Sets a widget as a potential drop destination, and adds default behaviors.
;;;
;;; The default behaviors listed in flags have an effect similar to installing
;;; default handlers for the widget's drag-and-drop signals ("drag-motion",
;;; "drag-drop", ...). They all exist for convenience. When passing
;;; GTK_DEST_DEFAULT_ALL for instance it is sufficient to connect to the
;;; widget's "drag-data-received" signal to get primitive, but consistent
;;; drag-and-drop support.
;;;
;;; Things become more complicated when you try to preview the dragged data, as
;;; described in the documentation for "drag-motion". The default behaviors
;;; described by flags make some assumptions, that can conflict with your own
;;; signal handlers. For instance GTK_DEST_DEFAULT_DROP causes invokations of
;;; gdk_drag_status() in the context of "drag-motion", and invokations of
;;; gtk_drag_finish() in "drag-data-received". Especially the later is dramatic,
;;; when your own "drag-motion" handler calls gtk_drag_get_data() to inspect the
;;; dragged data.
;;;
;;; There's no way to set a default action here, you can use the "drag-motion"
;;; callback for that. Here's an example which selects the action to use
;;; depending on whether the control key is pressed or not:
;;;
;;; static void
;;; drag_motion (GtkWidget *widget,
;;;              GdkDragContext *context,
;;;              gint x,
;;;              gint y,
;;;              guint time)
;;; {
;;;   GdkModifierType mask;
;;;
;;;   gdk_window_get_pointer (gtk_widget_get_window (widget),
;;;                           NULL, NULL, &mask);
;;;   if (mask & GDK_CONTROL_MASK)
;;;     gdk_drag_status (context, GDK_ACTION_COPY, time);
;;;   else
;;;     gdk_drag_status (context, GDK_ACTION_MOVE, time);
;;; }
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; flags :
;;;     which types of default drag behavior to use
;;;
;;; targets :
;;;     a pointer to an array of GtkTargetEntrys indicating the drop types that
;;;     this widget will accept, or NULL. Later you can access the list with
;;;     gtk_drag_dest_get_target_list() and gtk_drag_dest_find_target()
;;;
;;; n_targets :
;;;     the number of entries in targets
;;;
;;; actions :
;;;     a bitmask of possible actions for a drop onto this widget.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_proxy ()
;;;
;;; void gtk_drag_dest_set_proxy (GtkWidget *widget,
;;;                               GdkWindow *proxy_window,
;;;                               GdkDragProtocol protocol,
;;;                               gboolean use_coordinates);
;;;
;;; Sets this widget as a proxy for drops to another window.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; proxy_window :
;;;     the window to which to forward drag events
;;;
;;; protocol :
;;;     the drag protocol which the proxy_window accepts (You can use
;;;     gdk_drag_get_protocol() to determine this)
;;;
;;; use_coordinates :
;;;     If TRUE, send the same coordinates to the destination, because it is an
;;;     embedded subwindow.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_unset ()
;;;
;;; void gtk_drag_dest_unset (GtkWidget *widget);
;;;
;;; Clears information about a drop destination set with gtk_drag_dest_set().
;;; The widget will no longer receive notification of drags.
;;;
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_find_target ()
;;;
;;; GdkAtom gtk_drag_dest_find_target (GtkWidget *widget,
;;;                                    GdkDragContext *context,
;;;                                    GtkTargetList *target_list);
;;;
;;; Looks for a match between the supported targets of context and the
;;; dest_target_list, returning the first matching target, otherwise returning
;;; GDK_NONE. dest_target_list should usually be the return value from
;;; gtk_drag_dest_get_target_list(), but some widgets may have different valid
;;; targets for different parts of the widget; in that case, they will have to
;;; implement a drag_motion handler that passes the correct target list to this
;;; function.
;;;
;;; widget :
;;;     drag destination widget
;;;
;;; context :
;;;     drag context
;;;
;;; target_list :
;;;     list of droppable targets, or NULL to use gtk_drag_dest_get_target_list
;;;     (widget)
;;;
;;; Returns :
;;;     first target that the source offers and the dest can accept, or GDK_NONE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_target_list ()
;;;
;;; GtkTargetList * gtk_drag_dest_get_target_list (GtkWidget *widget);
;;;
;;; Returns the list of targets this widget can accept from drag-and-drop.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the GtkTargetList, or NULL if none
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_target_list ()
;;;
;;; void gtk_drag_dest_set_target_list (GtkWidget *widget,
;;;                                     GtkTargetList *target_list);
;;;
;;; Sets the target types that this widget can accept from drag-and-drop. The
;;; widget must first be made into a drag destination with gtk_drag_dest_set().
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; target_list :
;;;     list of droppable targets, or NULL for none
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_text_targets ()
;;;
;;; void gtk_drag_dest_add_text_targets (GtkWidget *widget);
;;;
;;; Add the text targets supported by GtkSelection to the target list of the
;;; drag destination. The targets are added with info = 0. If you need another
;;; value, use gtk_target_list_add_text_targets() and
;;; gtk_drag_dest_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_image_targets ()
;;;
;;; void gtk_drag_dest_add_image_targets (GtkWidget *widget);
;;;
;;; Add the image targets supported by GtkSelection to the target list of the
;;; drag destination. The targets are added with info = 0. If you need another
;;; value, use gtk_target_list_add_image_targets() and
;;; gtk_drag_dest_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_uri_targets ()
;;;
;;; void gtk_drag_dest_add_uri_targets (GtkWidget *widget);
;;;
;;; Add the URI targets supported by GtkSelection to the target list of the drag
;;; destination. The targets are added with info = 0. If you need another value,
;;; use gtk_target_list_add_uri_targets() and gtk_drag_dest_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_track_motion ()
;;;
;;; void gtk_drag_dest_set_track_motion (GtkWidget *widget,
;;;                                      gboolean track_motion);
;;;
;;; Tells the widget to emit "drag-motion" and "drag-leave" events regardless of
;;; the targets and the GTK_DEST_DEFAULT_MOTION flag.
;;;
;;; This may be used when a widget wants to do generic actions regardless of the
;;; targets that the source offers.
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; track_motion :
;;;     whether to accept all targets
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_track_motion ()
;;;
;;; gboolean gtk_drag_dest_get_track_motion (GtkWidget *widget);
;;;
;;; Returns whether the widget has been configured to always emit "drag-motion"
;;; signals.
;;;
;;; widget :
;;;     a GtkWidget that's a drag destination
;;;
;;; Returns :
;;;     TRUE if the widget always emits "drag-motion" events
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_finish" gtk-drag-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-17}
  @argument[context]{the drag context}
  @argument[success]{a flag indicating whether the drop was successful}
  @argument[del]{a flag indicating whether the source should delete the original
    data. This should be @arg{true} for a move.}
  @argument[time]{the timestamp from the \"drag-drop\" signal}
  Informs the drag source that the drop is finished, and that the data of the
  drag will no longer be required."
  (context (g-object gdk-drag-context))
  (success :boolean)
  (del :boolean)
  (time :uint32))

(export 'gtk-drag-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_get_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_get_data" gtk-drag-get-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-23}
  @argument[widget]{the widget that will receive the \"drag-data-received\"
    signal}
  @argument[context]{the drag context}
  @argument[target]{the target (form of the data) to retrieve}
  @argument[time]{a timestamp for retrieving the data. This will generally be
    the time received in a \"drag-motion\" or \"drag-drop\" signal.}
  @begin{short}
    Gets the data associated with a drag.
  @end{short}
  When the data is received or the retrieval fails, GTK+ will emit a
  \"drag-data-received\" signal. Failure of the retrieval is indicated by the
  length field of the selection_data signal parameter being negative. However,
  when the funcion @sym{gtk-drag-get-data} is called implicitely because the
  @code{GTK_DEST_DEFAULT_DROP} was set, then the widget will not receive
  notification of failed drops."
  (widget (g-object gtk-widget))
  (context (g-object gdk-drag-context))
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gtk-drag-get-data)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_get_source_widget ()
;;;
;;; GtkWidget * gtk_drag_get_source_widget (GdkDragContext *context);
;;;
;;; Determines the source widget for a drag.
;;;
;;; context :
;;;     a (destination side) drag context
;;;
;;; Returns :
;;;     if the drag is occurring within a single application, a pointer to the
;;;     source widget. Otherwise, NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_highlight ()
;;;
;;; void gtk_drag_highlight (GtkWidget *widget);
;;;
;;; Draws a highlight around a widget. This will attach handlers to "draw", so
;;; the highlight will continue to be displayed until gtk_drag_unhighlight() is
;;; called.
;;;
;;; widget :
;;;     a widget to highlight
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_unhighlight ()
;;;
;;; void gtk_drag_unhighlight (GtkWidget *widget);
;;;
;;; Removes a highlight set by gtk_drag_highlight() from a widget.
;;;
;;; widget :
;;;     a widget to remove the highlight from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_begin ()
;;;
;;; GdkDragContext * gtk_drag_begin (GtkWidget *widget,
;;;                                  GtkTargetList *targets,
;;;                                  GdkDragAction actions,
;;;                                  gint button,
;;;                                  GdkEvent *event);
;;;
;;; Initiates a drag on the source side. The function only needs to be used when
;;; the application is starting drags itself, and is not needed when
;;; gtk_drag_source_set() is used.
;;;
;;; The event is used to retrieve the timestamp that will be used internally to
;;; grab the pointer. If event is NULL, then GDK_CURRENT_TIME will be used.
;;; However, you should try to pass a real event in all cases, since that can be
;;; used by GTK+ to get information about the start position of the drag, for
;;; example if the event is a GDK_MOTION_NOTIFY.
;;;
;;; Generally there are three cases when you want to start a drag by hand by
;;; calling this function:
;;;
;;; 1. During a "button-press-event" handler, if you want to start a drag
;;;    immediately when the user presses the mouse button. Pass the event that
;;;    you have in your "button-press-event" handler.
;;;
;;; 2. During a "motion-notify-event" handler, if you want to start a drag when
;;;    the mouse moves past a certain threshold distance after a button-press.
;;;    Pass the event that you have in your "motion-notify-event" handler.
;;;
;;; 3. During a timeout handler, if you want to start a drag after the mouse
;;;    button is held down for some time. Try to save the last event that you
;;;    got from the mouse, using gdk_event_copy(), and pass it to this function
;;;    (remember to free the event with gdk_event_free() when you are done). If
;;;    you can really not pass a real event, pass NULL instead.
;;;
;;; widget :
;;;     the source widget.
;;;
;;; targets :
;;;     The targets (data formats) in which the source can provide the data.
;;;
;;; actions :
;;;     A bitmask of the allowed drag actions for this drag.
;;;
;;; button :
;;;     The button the user clicked to start the drag.
;;;
;;; event :
;;;     The event that triggered the start of the drag.
;;;
;;; Returns :
;;;     the context for this drag
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_widget ()
;;;
;;; void gtk_drag_set_icon_widget (GdkDragContext *context,
;;;                                GtkWidget *widget,
;;;                                gint hot_x,
;;;                                gint hot_y);
;;;
;;; Changes the icon for a widget to a given widget. GTK+ will not destroy the
;;; icon, so if you don't want it to persist, you should connect to the
;;; "drag-end" signal and destroy it yourself.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; widget :
;;;     a toplevel window to use as an icon.
;;;
;;; hot_x :
;;;     the X offset within widget of the hotspot.
;;;
;;; hot_y :
;;;     the Y offset within widget of the hotspot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_pixbuf ()
;;;
;;; void gtk_drag_set_icon_pixbuf (GdkDragContext *context,
;;;                                GdkPixbuf *pixbuf,
;;;                                gint hot_x,
;;;                                gint hot_y);
;;;
;;; Sets pixbuf as the icon for a given drag.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; pixbuf :
;;;     the GdkPixbuf to use as the drag icon.
;;;
;;; hot_x :
;;;     the X offset within widget of the hotspot.
;;;
;;; hot_y :
;;;     the Y offset within widget of the hotspot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_stock ()
;;;
;;; void gtk_drag_set_icon_stock (GdkDragContext *context,
;;;                               const gchar *stock_id,
;;;                               gint hot_x,
;;;                               gint hot_y);
;;;
;;; Sets the icon for a given drag from a stock ID.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; stock_id :
;;;     the ID of the stock icon to use for the drag.
;;;
;;; hot_x :
;;;     the X offset within the icon of the hotspot.
;;;
;;; hot_y :
;;;     the Y offset within the icon of the hotspot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_surface ()
;;;
;;; void gtk_drag_set_icon_surface (GdkDragContext *context,
;;;                                 cairo_surface_t *surface);
;;;
;;; Sets surface as the icon for a given drag. GTK+ retains references for the
;;; arguments, and will release them when they are no longer needed.
;;;
;;; To position the surface relative to the mouse, use
;;; cairo_surface_set_device_offset() on surface. The mouse cursor will be
;;; positioned at the (0,0) coordinate of the surface.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; surface :
;;;     the surface to use as icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_name ()
;;;
;;; void gtk_drag_set_icon_name (GdkDragContext *context,
;;;                              const gchar *icon_name,
;;;                              gint hot_x,
;;;                              gint hot_y);
;;;
;;; Sets the icon for a given drag from a named themed icon. See the docs for
;;; GtkIconTheme for more details. Note that the size of the icon depends on the
;;; icon theme (the icon is loaded at the symbolic size GTK_ICON_SIZE_DND), thus
;;; hot_x and hot_y have to be used with care.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; icon_name :
;;;     name of icon to use
;;;
;;; hot_x :
;;;     the X offset of the hotspot within the icon
;;;
;;; hot_y :
;;;     the Y offset of the hotspot within the icon
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_gicon ()
;;;
;;; void gtk_drag_set_icon_gicon (GdkDragContext *context,
;;;                               GIcon *icon,
;;;                               gint hot_x,
;;;                               gint hot_y);
;;;
;;; Sets the icon for a given drag from the given icon. See the documentation
;;; for gtk_drag_set_icon_name() for more details about using icons in drag and
;;; drop.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;;
;;; icon :
;;;     a GIcon
;;;
;;; hot_x :
;;;     the X offset of the hotspot within the icon
;;;
;;; hot_y :
;;;     the Y offset of the hotspot within the icon
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_default ()
;;;
;;; void gtk_drag_set_icon_default (GdkDragContext *context);
;;;
;;; Sets the icon for a particular drag to the default icon.
;;;
;;; context :
;;;     the context for a drag. (This must be called with a context for the
;;;     source side of a drag)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_check_threshold ()
;;;
;;; gboolean gtk_drag_check_threshold (GtkWidget *widget,
;;;                                    gint start_x,
;;;                                    gint start_y,
;;;                                    gint current_x,
;;;                                    gint current_y);
;;;
;;; Checks to see if a mouse drag starting at (start_x, start_y) and ending at
;;; (current_x, current_y) has passed the GTK+ drag threshold, and thus should
;;; trigger the beginning of a drag-and-drop operation.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; start_x :
;;;     X coordinate of start of drag
;;;
;;; start_y :
;;;     Y coordinate of start of drag
;;;
;;; current_x :
;;;     current X coordinate
;;;
;;; current_y :
;;;     current Y coordinate
;;;
;;; Returns :
;;;     TRUE if the drag threshold has been passed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set ()
;;;
;;; void gtk_drag_source_set (GtkWidget *widget,
;;;                           GdkModifierType start_button_mask,
;;;                           const GtkTargetEntry *targets,
;;;                           gint n_targets,
;;;                           GdkDragAction actions);
;;;
;;; Sets up a widget so that GTK+ will start a drag operation when the user
;;; clicks and drags on the widget. The widget must have a window.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; start_button_mask :
;;;     the bitmask of buttons that can start the drag
;;;
;;; targets :
;;;     the table of targets that the drag will support, may be NULL
;;;
;;; n_targets :
;;;     the number of items in targets
;;;
;;; actions :
;;;     the bitmask of possible actions for a drag from this widget
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_pixbuf ()
;;;
;;; void gtk_drag_source_set_icon_pixbuf (GtkWidget *widget, GdkPixbuf *pixbuf);
;;;
;;; Sets the icon that will be used for drags from a particular widget from a
;;; GdkPixbuf. GTK+ retains a reference for pixbuf and will release it when it
;;; is no longer needed.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; pixbuf :
;;;     the GdkPixbuf for the drag icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_stock ()
;;;
;;; void gtk_drag_source_set_icon_stock (GtkWidget *widget,
;;;                                      const gchar *stock_id);
;;;
;;; Sets the icon that will be used for drags from a particular source to a
;;; stock icon.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; stock_id :
;;;     the ID of the stock icon to use
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_name ()
;;;
;;; void gtk_drag_source_set_icon_name (GtkWidget *widget,
;;;                                     const gchar *icon_name);
;;;
;;; Sets the icon that will be used for drags from a particular source to a
;;; themed icon. See the docs for GtkIconTheme for more details.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; icon_name :
;;;     name of icon to use
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_gicon ()
;;;
;;; void gtk_drag_source_set_icon_gicon (GtkWidget *widget, GIcon *icon);
;;;
;;; Sets the icon that will be used for drags from a particular source to icon.
;;; See the docs for GtkIconTheme for more details.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; icon :
;;;     A GIcon
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_unset ()
;;;
;;; void gtk_drag_source_unset (GtkWidget *widget);
;;;
;;; Undoes the effects of gtk_drag_source_set().
;;;
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_target_list ()
;;;
;;; void gtk_drag_source_set_target_list (GtkWidget *widget,
;;;                                       GtkTargetList *target_list);
;;;
;;; Changes the target types that this widget offers for drag-and-drop. The
;;; widget must first be made into a drag source with gtk_drag_source_set().
;;;
;;; widget :
;;;     a GtkWidget that's a drag source
;;;
;;; target_list :
;;;     list of draggable targets, or NULL for none
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_get_target_list ()
;;;
;;; GtkTargetList * gtk_drag_source_get_target_list (GtkWidget *widget);
;;;
;;; Gets the list of targets this widget can provide for drag-and-drop.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the GtkTargetList, or NULL if none
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_text_targets ()
;;;
;;; void gtk_drag_source_add_text_targets (GtkWidget *widget);
;;;
;;; Add the text targets supported by GtkSelection to the target list of the
;;; drag source. The targets are added with info = 0. If you need another value,
;;; use gtk_target_list_add_text_targets() and
;;; gtk_drag_source_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's is a drag source
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_image_targets ()
;;;
;;; void gtk_drag_source_add_image_targets (GtkWidget *widget);
;;;
;;; Add the writable image targets supported by GtkSelection to the target list
;;; of the drag source. The targets are added with info = 0. If you need another
;;; value, use gtk_target_list_add_image_targets() and
;;; gtk_drag_source_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's is a drag source
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_uri_targets ()
;;;
;;; void gtk_drag_source_add_uri_targets (GtkWidget *widget);
;;;
;;; Add the URI targets supported by GtkSelection to the target list of the drag
;;; source. The targets are added with info = 0. If you need another value, use
;;; gtk_target_list_add_uri_targets() and gtk_drag_source_set_target_list().
;;;
;;; widget :
;;;     a GtkWidget that's is a drag source
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.drag-and-drop.lisp -------------------------------------
