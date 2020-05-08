;;; ----------------------------------------------------------------------------
;;; gtk.drag-and-drop.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; Drag and Drop
;;;
;;;     Functions for controlling drag and drop handling
;;;
;;; Types and Values
;;;
;;;     GtkDestDefaults
;;;     GtkTargetFlags  --> gtk.selections.lisp
;;;     GtkDragResult
;;;
;;; Functions
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
;;;     gtk_drag_begin_with_coordinates
;;;     gtk_drag_cancel
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
    The @sym{gtk-dest-defaults} flags specifies the various types of action that
    will be taken on behalf of the user for a drag destination site.
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
;;; gtk_drag_dest_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_set" %gtk-drag-dest-set) :void
  (widget (g-object gtk-widget))
  (flags gtk-dest-defaults)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-drag-dest-set (widget flags targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[flags]{which types of type @symbol{gtk-dest-defaults} of default
    drag behavior to use}
  @argument[targets]{a list of @class{gtk-target-entry}s indicating the drop
    types that this @arg{widget} will accept, or @code{nil}. Later you can
    access the list with the functions @fun{gtk-drag-dest-get-target-list} and
    @fun{gtk-drag-dest-find-target}}
  @argument[actions]{a bitmask of possible actions for a drop onto
    this @arg{widget}}
  @begin{short}
    Sets a @arg{widget} as a potential drop destination, and adds default
    behaviors.
  @end{short}

  The default behaviors listed in @arg{flags} have an effect similar to
  installing default handlers for the @arg{widget}'s drag-and-drop signals
  (\"drag-motion\", \"drag-drop\", ...). They all exist for convenience. When
  passing @code{:all} for instance it is sufficient to connect to the
  @arg{widget}'s \"drag-data-received\" signal to get primitive, but consistent
  drag-and-drop support.

  Things become more complicated when you try to preview the dragged data, as
  described in the documentation for the \"drag-motion\" signal. The default
  behaviors described by @arg{flags} make some assumptions, that can conflict
  with your own signal handlers. For instance @code{:drop} causes invokations of
  the function @fun{gdk-drag-status} in the context of the \"drag-motion\"
  signal, and invokations of the function @fun{gtk-drag-finish} in the
  \"drag-data-received\" handler. Especially the later is dramatic, when your
  own \"drag-motion\" handler calls the function @fun{gtk-drag-get-data} to
  inspect the dragged data.

  There is no way to set a default action here, you can use the the
  \"drag-motion\" callback for that. Here is an example which selects the action
  to use depending on whether the control key is pressed or not:
  @begin{pre}
 static void
 drag_motion (GtkWidget *widget,
              GdkDragContext *context,
              gint x,
              gint y,
              guint time)
 {
   GdkModifierType mask;

   gdk_window_get_pointer (gtk_widget_get_window (widget),
                           NULL, NULL, &mask);
   if (mask & GDK_CONTROL_MASK)
     gdk_drag_status (context, GDK_ACTION_COPY, time);
   else
     gdk_drag_status (context, GDK_ACTION_MOVE, time);
 @}
  @end{pre}
  @see-function{gtk-drag-dest-get-target-list}
  @see-function{gtk-drag-dest-find-target}
  @see-function{gdk-drag-status}
  @see-function{gtk-drag-finish}
  @see-function{gtk-drag-get-data}"
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-drag-dest-set widget
                        flags
                        targets-ptr
                        n-targets
                        actions)))

(export 'gtk-drag-dest-set)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_proxy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_set_proxy" gtk-drag-dest-set-proxy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[proxy-window]{the @class{gdk-window} object to which to forward drag
    events}
  @argument[protocol]{the drag protocol of type @symbol{gdk-drag-protocol} which
    the @arg{proxy-window} accepts, you can use the @fun{gdk-drag-get-protocol}
    function to determine this}
  @argument[use-coordinates]{if @emph{true}, send the same coordinates to the
    destination, because it is an embedded subwindow}
  @begin{short}
    Sets the widget as a proxy for drops to another window.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-dest-set-proxy} function has been deprecated since version
    3.22 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-widget}
  @see-class{gdk-window}
  @see-symbol{gdk-drag-protocol}
  @see-function{gdk-drag-get-protocol}"
  (widget (g-object gtk-widget))
  (proxy-window (g-object gdk-window))
  (protocol gdk-drag-protocol)
  (use-coordinates :boolean))

(export 'gtk-drag-dest-set-proxy)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_unset" gtk-drag-dest-unset) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @begin{short}
    Clears information about a drop destination set with the function
    @fun{gtk-drag-dest-set}.
  @end{short}
  The widget will no longer receive notification of drags.
  @see-class{gtk-widget}
  @see-function{gtk-drag-dest-set}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-unset)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_find_target ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_find_target" %gtk-drag-dest-find-target)
    gdk-atom-as-string
  (widget (g-object gtk-widget))
  (context (g-object gdk-drag-context))
  (target-list (g-boxed-foreign gtk-target-list)))

(defun gtk-drag-dest-find-target (widget context &optional (target-list nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{drag destination widget}
  @argument[context]{drag context}
  @argument[target-list]{list of droppable targets, or @code{nil} to use the
    function @fun{gtk-drag-dest-get-target-list}}
  @begin{return}
    First target that the source offers and the dest can accept,
    or @code{:none}.
  @end{return}
  @begin{short}
    Looks for a match between the supported targets of context and the
    @arg{target-list}, returning the first matching target, otherwise returning
    @code{:none}.
  @end{short}
  @arg{target-list} should usually be the return value from the function
  @fun{gtk-drag-dest-get-target-list}, but some widgets may have different valid
  targets for different parts of the widget; in that case, they will have to
  implement a \"drag-motion\" handler that passes the correct target list to
  this function.
  @see-class{gtk-widget}
  @see-class{gdk-drag-context}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-dest-get-target-list}"
  (%gtk-drag-dest-find-target widget context target-list))

(export 'gtk-drag-dest-find-target)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_target_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_get_target_list" gtk-drag-dest-get-target-list)
    (g-boxed-foreign gtk-target-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @return{The @class{gtk-target-list}, or @code{nil} if none.}
  Returns the list of targets this widget can accept from drag-and-drop.
  @see-class{gtk-widget}
  @see-class{gtk-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-get-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_target_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_set_target_list" gtk-drag-dest-set-target-list) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @argument[target-list]{list of droppable targets, or @code{nil} for none}
  @begin{short}
    Sets the target types that this widget can accept from drag-and-drop.
  @end{short}
  The widget must first be made into a drag destination with the function
  @fun{gtk-drag-dest-set}.
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-dest-set}"
  (widget (g-object gtk-widget))
  (target-list (g-boxed-foreign gtk-target-list)))

(export 'gtk-drag-dest-set-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_text_targets" gtk-drag-dest-add-text-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the text targets supported by the selection to the target list of the
    drag destination.
  @end{short}
  The targets are added with info = 0. If you need another value, use the
  functions @fun{gtk-target-list-add-text-targets} and
  @fun{gtk-drag-dest-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-text-targets}
  @see-function{gtk-drag-dest-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_image_targets" gtk-drag-dest-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-3}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the image targets supported by GtkSelection to the target list
    of the drag destination.
  @end{short}
  The targets are added with info = 0. If you need another value, use the
  functions @fun{gtk-target-list-add-image-targets} and
  @fun{gtk-drag-dest-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-image-targets}
  @see-function{gtk-drag-dest-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_uri_targets" gtk-drag-dest-add-uri-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the URI targets supported by the selection to the target list of the
    drag destination.
  @end{short}
  The targets are added with info = 0. If you need another value, use the
  functions @fun{gtk-target-list-add-uri-targets} and
  @fun{gtk-drag-dest-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-uri-targets}
  @see-function{gtk-drag-dest-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-uri-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_track_motion ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_set_track_motion" gtk-drag-dest-set-track-motion) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @argument[track-motion]{whether to accept all targets}
  @begin{short}
    Tells the widget to emit \"drag-motion\" and \"drag-leave\" events
    regardless of the targets and the @code{:motion} flag.
  @end{short}

  This may be used when a widget wants to do generic actions regardless of the
  targets that the source offers.
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget))
  (track-motion :boolean))

(export 'gtk-drag-dest-set-track-motion)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_track_motion ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_get_track_motion" gtk-drag-dest-get-track-motion)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @return{@em{True} if the widget always emits \"drag-motion\" events.}
  @begin{short}
    Returns whether the widget has been configured to always emit
    \"drag-motion\" signals.
  @end{short}
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-get-track-motion)

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_get_source_widget" gtk-drag-get-source-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-17}
  @argument[context]{a destination side drag context}
  @return{If the drag is occurring within a single application, a pointer to
    the source widget. Otherwise, @code{nil}.}
  Determines the source widget for a drag."
  (context (g-object gdk-drag-context)))

(export 'gtk-drag-get-source-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_highlight ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_highlight" gtk-drag-highlight) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-7}
  @argument[widget]{a @class{gtk-widget} object to highlight}
  @begin{short}
    Draws a highlight around a widget.
  @end{short}
  This will attach handlers to \"draw\", so the highlight will continue to be
  displayed until the function @fun{gtk-drag-unhighlight} is called.
  @see-function{gtk-drag-unhighlight}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-highlight)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_unhighlight ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_unhighlight" gtk-drag-unhighlight) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-7}
  @argument[widget]{a @class{gtk-widget} object to remove the highlight from}
  @begin{short}
    Removes a highlight set by the function @fun{gtk-drag-highlight} from a
    widget.
  @end{short}
  @see-function{gtk-drag-highlight}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-unhighlight)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_begin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_begin" gtk-drag-begin) (g-object gdk-drag-context)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{the source widget}
  @argument[targets]{the targets (data formats) in which the source can provide
    the data}
  @argument[actions]{a bitmask of the allowed drag actions for this drag}
  @argument[button]{the button the user clicked to start the drag}
  @argument[event]{the event that triggered the start of the drag}
  @return{The context for this drag.}
  @begin{short}
    Initiates a drag on the source side.
  @end{short}
  The function only needs to be used when the application is starting drags
  itself, and is not needed when the function @fun{gtk-drag-source-set} is used.

  The event is used to retrieve the timestamp that will be used internally to
  grab the pointer. If event is @code{nil}, then @var{+gdk-current-time+} will
  be used. However, you should try to pass a real event in all cases, since that
  can be used by GTK+ to get information about the start position of the drag,
  for example if the event is a @code{:motion-notify}.

  Generally there are three cases when you want to start a drag by hand by
  calling this function:
  @begin{enumeration}
    @begin{entry}
      During a \"button-press-event\" handler, if you want to start a drag
      immediately when the user presses the mouse button. Pass the event that
      you have in your \"button-press-event\" handler.
    @end{entry}
    @begin{entry}
      During a \"motion-notify-event\" handler, if you want to start a drag when
      the mouse moves past a certain threshold distance after a button-press.
      Pass the event that you have in your \"motion-notify-event\" handler.
    @end{entry}
    @begin{entry}
      During a timeout handler, if you want to start a drag after the mouse
      button is held down for some time. Try to save the last event that you
      got from the mouse, using the function @fun{gdk-event-copy}, and pass it
      to this function, remember to free the event with the function
      @fun{gdk-event-free} when you are done. If you can really not pass a real
      event, pass @code{nil} instead.
    @end{entry}
  @end{enumeration}
  @begin[Warning]{dictionary}
    The @sy{gtk-drag-begin} function has been deprecated since version 3.10 and
    should not be used in newly-written code. Use the
    @fun{gtk-drag-begin-with-coordinates} instead.
  @end{dictionary}
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-symbol{gdk-drag-action}
  @see-class{gdk-event}
  @see-function{gtk-drag-source-set}
  @see-function{gdk-event-copy}
  @see-function{gdk-event-free}
  @see-variable{+gdk-current-time+}"
  (widget (g-object gtk-widget))
  (targets (g-boxed-foreign gtk-target-list))
  (actions gdk-drag-action)
  (button :int)
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-drag-begin)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_begin_with_coordinates ()
;;;
;;; GdkDragContext *
;;; gtk_drag_begin_with_coordinates (GtkWidget *widget,
;;;                                  GtkTargetList *targets,
;;;                                  GdkDragAction actions,
;;;                                  gint button,
;;;                                  GdkEvent *event,
;;;                                  gint x,
;;;                                  gint y);
;;;
;;; Initiates a drag on the source side. The function only needs to be used when
;;; the application is starting drags itself, and is not needed when
;;; gtk_drag_source_set() is used.
;;;
;;; The event is used to retrieve the timestamp that will be used internally to
;;; grab the pointer. If event is NULL, then GDK_CURRENT_TIME will be used.
;;; However, you should try to pass a real event in all cases, since that can be
;;; used to get information about the drag.
;;;
;;; Generally there are three cases when you want to start a drag by hand by
;;; calling this function:
;;;
;;; During a “button-press-event” handler, if you want to start a drag
;;; immediately when the user presses the mouse button. Pass the event that you
;;; have in your “button-press-event” handler.
;;;
;;; During a “motion-notify-event” handler, if you want to start a drag when the
;;; mouse moves past a certain threshold distance after a button-press. Pass the
;;; event that you have in your “motion-notify-event” handler.
;;;
;;; During a timeout handler, if you want to start a drag after the mouse button
;;; is held down for some time. Try to save the last event that you got from the
;;; mouse, using gdk_event_copy(), and pass it to this function (remember to
;;; free the event with gdk_event_free() when you are done). If you really
;;; cannot pass a real event, pass NULL instead.
;;;
;;; widget :
;;;     the source widget
;;;
;;; targets :
;;;     The targets (data formats) in which the source can provide the data
;;;
;;; actions :
;;;     A bitmask of the allowed drag actions for this drag
;;;
;;; button :
;;;     The button the user clicked to start the drag
;;;
;;; event :
;;;     The event that triggered the start of the drag, or NULL if none can be
;;;     obtained.
;;;
;;; x :
;;;     The initial x coordinate to start dragging from, in the coordinate space
;;;     of widget . If -1 is passed, the coordinates are retrieved from event or
;;;     the current pointer position
;;;
;;; y :
;;;     The initial y coordinate to start dragging from, in the coordinate space
;;;     of widget . If -1 is passed, the coordinates are retrieved from event or
;;;     the current pointer position
;;;
;;; Returns :
;;;     the context for this drag.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_cancel ()
;;;
;;; void gtk_drag_cancel (GdkDragContext *context);
;;;
;;; Cancels an ongoing drag operation on the source side.
;;;
;;; If you want to be able to cancel a drag operation in this way, you need to
;;; keep a pointer to the drag context, either from an explicit call to
;;; gtk_drag_begin_with_coordinates(), or by connecting to “drag-begin”.
;;;
;;; If context does not refer to an ongoing drag operation, this function does
;;; nothing.
;;;
;;; If a drag is cancelled in this way, the result argument of “drag-failed” is
;;; set to GTK_DRAG_RESULT_ERROR .
;;;
;;; context :
;;;     a GdkDragContext, as e.g. returned by gtk_drag_begin_with_coordinates()
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_widget" gtk-drag-set-icon-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag, this must be called with a context
    for the source side of a drag}
  @argument[widget]{a toplevel window to use as an icon}
  @argument[hot-x]{the x offset within widget of the hotspot}
  @argument[hot-y]{the y offset within widget of the hotspot}
  @begin{short}
    Changes the icon for a widget to a given widget.
  @end{short}
  GTK+ will not destroy the icon, so if you do not want it to persist, you
  should connect to the \"drag-end\" signal and destroy it yourself.
  @see-class{gdk-drag-context}
  @see-class{gtk-widget}"
  (context (g-object gdk-drag-context))
  (widget (g-object gtk-widget))
  (hot-x :int)
  (hot-y :int))

(export 'gtk-drag-set-icon-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_pixbuf" gtk-drag-set-icon-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-3}
  @argument[context]{the context of type @class{gdk-drag-context} for a drag,
    this must be called with a context for the source side of a drag}
  @argument[pixbuf]{the @class{gdk-pixbuf} to use as the drag icon}
  @argument[hot-x]{the x offset within widget of the hotspot}
  @argument[hot-y]{the y offset within widget of the hotspot}
  @short{Sets @arg{pixbuf} as the icon for a given drag.}
  @see-class{gdk-drag-context}
  @see-class{gdk-pixbuf}"
  (context (g-object gdk-drag-context))
  (pixbuf (g-object gdk-pixbuf))
  (hot-x :int)
  (hot-y :int))

(export 'gtk-drag-set-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_stock" gtk-drag-set-icon-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag. This must be called with a context
    for the source side of a drag.}
  @argument[stock-id]{the ID of the stock icon to use for the drag}
  @argument[hot-x]{the x offset within the icon of the hotspot}
  @argument[hot-y]{the y offset within the icon of the hotspot}
  @begin{short}
    Sets the icon for a given drag from a stock ID.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-set-icon-stock} function has been deprecated since version
    3.10 and should not be used in newly-written code. Use the
    @fun{gtk-drag-set-icon-name} instead.
  @end{dictionary}
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (stock-id :string)
  (hot-x :int)
  (hot-y :int))

(export 'gtk-drag-set-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_surface" gtk-drag-set-icon-surface) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag. This must be called with a context
    for the source side of a drag.}
  @argument[surface]{the surface to use as icon}
  @begin{short}
    Sets @arg{surface} as the icon for a given drag.
  @end{short}
  GTK+ retains references for the arguments, and will release them when they are
  no longer needed.

  To position the surface relative to the mouse, use the function
  @fun{cairo-surface-set-device-offset} on surface. The mouse cursor will be
  positioned at the (0,0) coordinate of the surface.
  @see-class{gdk-drag-context}
  @see-symbol{cairo-surface-t}
  @see-function{cairo-surface-set-device-offset}"
  (context (g-object gdk-drag-context))
  (surface (:pointer (:struct cairo-surface-t))))

(export 'gtk-drag-set-icon-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_name" gtk-drag-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag. This must be called with a context
    for the source side of a drag.}
  @argument[icon-name]{name of icon to use}
  @argument[hot-x]{the x offset of the hotspot within the icon}
  @argument[hot-y]{the y offset of the hotspot within the icon}
  @begin{short}
    Sets the icon for a given drag from a named themed icon.
  @end{short}
  See the docs for @class{gtk-icon-theme} for more details. Note that the size
  of the icon depends on the icon theme, the icon is loaded at the symbolic size
  @code{:dnd}, thus @arg{hot-x} and @arg{hot-y} have to be used with care.
  @see-class{gdk-drag-context}
  @see-class{gtk-icon-theme}"
  (context (g-object gdk-drag-context))
  (icon-name :string)
  (hot-x :int)
  (hot-y :int))

(export 'gtk-drag-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_gicon" gtk-drag-set-icon-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag. This must be called with a context
    for the source side of a drag.}
  @argument[icon]{a @class{g-icon} object}
  @argument[hot-x]{the x offset of the hotspot within the icon}
  @argument[hot-y]{the y offset of the hotspot within the icon}
  @begin{short}
    Sets the icon for a given drag from the given icon.
  @end{short}
  See the documentation for the function @fun{gtk-drag-set-icon-name} for more
  details about using icons in drag and drop.
  @see-class{gdk-drag-context}
  @see-class{g-icon}
  @see-function{gtk-drag-set-icon-name}"
  (context (g-object gdk-drag-context))
  (icon (g-object g-icon))
  (hot-x :int)
  (hot-y :int))

(export 'gtk-drag-set-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_default" gtk-drag-set-icon-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[context]{the context for a drag. This must be called with a context
    for the source side of a drag.}
  Sets the icon for a particular drag to the default icon.
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context)))

(export 'gtk-drag-set-icon-default)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_check_threshold ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_check_threshold" gtk-drag-check-threshold) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[start-x]{x coordinate of start of drag}
  @argument[start-y]{y coordinate of start of drag}
  @argument[current-x]{current x coordinate}
  @argument[current-y]{current y coordinate}
  @return{@emph{True} if the drag threshold has been passed.}
  Checks to see if a mouse drag starting at (@arg{start-x}, @arg{start-y}) an
  ending at (@arg{current-x}, @arg{current-y}) has passed the GTK+ drag
  threshold, and thus should trigger the beginning of a drag-and-drop operation.
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget))
  (start-x :int)
  (start-y :int)
  (current-x :int)
  (current-y :int))

(export 'gtk-drag-check-threshold)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set" %gtk-drag-source-set) :void
  (widget (g-object gtk-widget))
  (start-button-mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-drag-source-set (widget start-button-mask targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[start-button-mask]{the bitmask of buttons that can start the drag}
  @argument[targets]{the list of targets that the drag will support,
    may be @code{nil}}
  @argument[actions]{the bitmask of possible actions for a drag from this
    widget}
  Sets up a widget so that GTK+ will start a drag operation when the user
  clicks and drags on the @arg{widget}. The @arg{widget} must have a window."
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-drag-source-set widget
                          start-button-mask
                          targets-ptr
                          n-targets
                          actions)))

(export 'gtk-drag-source-set)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_pixbuf" gtk-drag-source-set-icon-pixbuf)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[pixbuf]{the @class{gdk-pixbuf} for the drag icon}
  @begin{short}
    Sets the icon that will be used for drags from a particular @arg{widget}
    from a @class{gdk-pixbuf}.
  @end{short}
  GTK+ retains a reference for @arg{pixbuf} and will release it when it is no
  longer needed.
  @see-function{gtk-drag-source-set-icon-stock}
  @see-function{gtk-drag-source-set-icon-name}
  @see-function{gtk-drag-source-set-icon-gicon}"
  (widget (g-object gtk-widget))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-drag-source-set-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_stock" gtk-drag-source-set-icon-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[stock-id]{a string with the ID of the stock icon to use}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to a
    stock icon.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-source-set-icon-stock} function has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the
    @fun{gtk-drag-source-set-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget))
  (stock-id :string))

(export 'gtk-drag-source-set-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_name" gtk-drag-source-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[icon_name]{name of icon to use}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to a
    themed icon.
  @end{short}
  See the docs for @class{gtk-icon-theme} for more details.
  @see-class{gtk-widget}
  @see-class{gtk-icon-theme}"
  (widget (g-object gtk-widget))
  (icon-name :string))

(export 'gtk-drag-source-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_gicon" gtk-drag-source-set-icon-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to
    @arg{icon}.
  @end{short}
  See the docs for @class{gtk-icon-theme} for more details.
  @see-class{gtk-widget}
  @see-class{g-icon}
  @see-class{gtk-icon-theme}"
  (widget (g-object gtk-widget))
  (icon (g-object g-icon)))

(export 'gtk-drag-source-set-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_unset" gtk-drag-source-unset) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} object}
  Undoes the effects of the function @fun{gtk-drag-source-set}.
  @see-class{gtk-widget}
  @see-function{gtk-drag-source-set}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-unset)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_target_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_target_list" gtk-drag-source-set-target-list)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} that is a drag source}
  @argument[target-list]{list of draggable targets, or @code{nil} for none}
  @begin{short}
    Changes the target types that this widget offers for drag-and-drop.
  @end{short}
  The widget must first be made into a drag source with the function
  @fun{gtk-drag-source-set}.
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-source-set}
  @see-function{gtk-drag-source-get-target-list}"
  (widget (g-object gtk-widget))
  (target-list (g-boxed-foreign gtk-target-list)))

(export 'gtk-drag-source-set-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_get_target_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_get_target_list" gtk-drag-source-get-target-list)
    (g-boxed-foreign gtk-target-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} object}
  @return{The @class{gtk-target-list}, or @code{nil} if none.}
  @begin{short}
    Gets the list of targets this widget can provide for drag-and-drop.
  @end{short}
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-source-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-get-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_text_targets" gtk-drag-source-add-text-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} that is a drag source}
  @begin{short}
    Add the text targets supported by GtkSelection to the target list
    of the drag source.
  @end{short}
  The targets are added with info = 0. If you need another value, use the
  functions @fun{gtk-target-list-add-text-targets} and
  @fun{gtk-drag-source-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-text-targets}
  @see-function{gtk-drag-source-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_image_targets" gtk-drag-source-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-3}
  @argument[widget]{a @class{gtk-widget} that is a drag source}
  @begin{short}
    Add the writable image targets supported by GtkSelection to the
    target list of the drag source.
  @end{short}
  The targets are added with info = 0. If you need another value, use
  the function @fun{gtk-target-list-add-image-targets} and
  @fun{gtk-drag-source-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-image-targets}
  @see-function{gtk-drag-source-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_uri_targets" gtk-drag-source-add-uri-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-20}
  @argument[widget]{a @class{gtk-widget} that is a drag source}
  @begin{short}
    Add the URI targets supported by GtkSelection to the target list of
    the drag source.
  @end{short}
  The targets are added with info = 0. If you need another value, use the
  functions @fun{gtk-target-list-add-uri-targets} and
  @fun{gtk-drag-source-set-target-list}.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-uri-targets}
  @see-function{gtk-drag-source-set-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-uri-targets)

;;; --- End of file gtk.drag-and-drop.lisp -------------------------------------
