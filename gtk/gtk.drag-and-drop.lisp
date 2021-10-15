;;; ----------------------------------------------------------------------------
;;; gtk.drag-and-drop.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.

;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
(setf (gethash 'gtk-dest-defaults atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-dest-defaults atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    The @sym{gtk-dest-defaults} flags specifies the various types of action
    that will be taken on behalf of the user for a drag destination site.
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
    @entry[:motion]{If set for a widget, GTK, during a drag over this widget
      will check if the drag matches the list of possible targets and actions
      of the widget. GTK will then call the @fun{gdk-drag-status} function as
      appropriate.}
    @entry[:highlight]{If set for a widget, GTK will draw a highlight on the
      widget as long as a drag is over this widget and the widget drag format
      and action are acceptable.}
    @entry[:drop]{If set for a widget, when a drop occurs, GTK will will check
      if the drag matches the list of possible targets and actions of the
      widget. If so, GTK will call the @fun{gtk-drag-data} function on behalf
      of the widget. Whether or not the drop is successful, GTK will call the
      @fun{gtk-drag-finish} function. If the action was a move, then if the
      drag was successful @em{true} will be passed for the delete parameter to
      the @fun{gtk-drag-finish} function.}
    @entry[:all]{If set, specifies that all default actions should be taken.}
  @end{table}
  @see-function{gdk-drag-status}
  @see-function{gtk-drag-data}
  @see-function{gtk-drag-finish}")

;;; ----------------------------------------------------------------------------
;;; enum GtkDragResult
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkDragResult" gtk-drag-result
  (:export t
   :type-initializer "gtk_drag_result_get_type")
  (:success 0)
  (:no-target 1)
  (:user-cancelled 2)
  (:timeout-expired 3)
  (:grab-broken 4)
  (:error 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-drag-result atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-drag-result atdoc:*external-symbols*)
 "@version{2021-10-3}
  @begin{short}
    Gives an indication why a drag operation failed.
  @end{short}
  The value can by obtained by connecting to the \"drag-failed\" signal of a
  @class{gtk-widget} object.
  @begin{pre}
(define-g-enum \"GtkDragResult\" gtk-drag-result
  (:export t
   :type-initializer \"gtk_drag_result_get_type\")
  (:success 0)
  (:no-target 1)
  (:user-cancelled 2)
  (:timeout-expired 3)
  (:grab-broken 4)
  (:error 5))
  @end{pre}
  @begin[code]{table}
    @entry[:success]{The drag operation was successful.}
    @entry[:no-target]{No suitable drag target.}
    @entry[:user-cancelled]{The user cancelled the drag operation.}
    @entry[:timeout-expired]{The drag operation timed out.}
    @entry[:grab-broken]{The pointer or keyboard grab used for the drag
      operation was broken.}
    @entry[:error]{The drag operation failed due to some unspecified error.}
  @end{table}
  @see-class{gtk-widget}")

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
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[flags]{a @symbol{gtk-dest-defaults} bitmask with the default drag
    behavior to use}
  @argument[targets]{a list of target entries indicating the drop types that
    @arg{widget} will accept, or @code{nil}, later you can access the list with
    the @fun{gtk-drag-dest-target-list} and @fun{gtk-drag-dest-find-target}
    functions}
  @argument[actions]{a @symbol{gdk-drag-action} bitmask of possible actions for
    a drop onto the widget}
  @begin{short}
    Sets a widget as a potential drop destination, and adds default behaviors.
  @end{short}

  The default behaviors listed in the @arg{flags} argument have an effect
  similar to installing default handlers \"drag-motion\", \"drag-drop\", ...
  for the drag and drop signals of the widget. They all exist for
  convenience. When passing the @code{:all} value for instance it is sufficient
  to connect to the \"drag-data-received\" signal of the widget to get
  primitive, but consistent drag and drop support.

  Things become more complicated when you try to preview the dragged data, as
  described in the documentation for the \"drag-motion\" signal. The default
  behaviors described by the @arg{flags} argument make some assumptions, that
  can conflict with your own signal handlers. For instance the @code{:drop}
  value causes invokations of the @fun{gdk-drag-status} function in the drag
  context of the \"drag-motion\" signal, and invokations of the
  @fun{gtk-drag-finish} function in the \"drag-data-received\" handler.
  Especially the later is dramatic, when your own \"drag-motion\" handler calls
  the @fun{gtk-drag-data} function to inspect the dragged data.

  There is no way to set a default action here, you can use the the
  \"drag-motion\" callback function for that. Here is an example which selects
  the action to use depending on whether the @kbd{Control} key is pressed or
  not:
  @begin{pre}
(defun drag-motion (widget context x y time)
  (declare (ignore x y))
  (let* ((seat (gdk-display-default-seat (gtk-widget-display widget)))
         (device (gdk-seat-pointer seat)))
    (multiple-value-bind (window x y mask)
        (gdk-window-device-position (gtk-widget-window widget) device)
      (if (member :control-mask mask)
          (gdk-drag-status context :copy time)
          (gdk-drag-status context :move time)))))
  @end{pre}
  @see-class{gtk-widget}
  @see-symbol{gtk-dest-defaults}
  @see-symbol{gtk-drag-action}
  @see-function{gtk-drag-dest-target-list}
  @see-function{gtk-drag-dest-find-target}
  @see-function{gdk-drag-status}
  @see-function{gtk-drag-finish}
  @see-function{gtk-drag-data}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr '(:struct %gtk-target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (mem-aptr targets-ptr
                                       '(:struct %gtk-target-entry) i)
            for entry = (pop targets)
            do (with-foreign-slots ((target flags info)
                                    target-ptr
                                    (:struct %gtk-target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%gtk-drag-dest-set widget flags targets-ptr n-targets actions))))

(export 'gtk-drag-dest-set)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_set_proxy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_set_proxy" gtk-drag-dest-set-proxy) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[window]{a @class{gdk-window} object to which to forward drag
    events}
  @argument[protocol]{the drag protocol of type @symbol{gdk-drag-protocol}
    which @arg{window} accepts, you can use the @fun{gdk-drag-context-protocol}
    function to determine this}
  @argument[coordinates]{if @em{true}, send the same coordinates to the
    destination, because it is an embedded subwindow}
  @begin{short}
    Sets the widget as a proxy for drops to another window.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-drag-dest-set-proxy} has been deprecated since version
    3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-widget}
  @see-class{gdk-window}
  @see-symbol{gdk-drag-protocol}
  @see-function{gdk-drag-context-protocol}"
  (widget (g-object gtk-widget))
  (window (g-object gdk-window))
  (protocol gdk-drag-protocol)
  (coordinates :boolean))

(export 'gtk-drag-dest-set-proxy)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_unset" gtk-drag-dest-unset) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @begin{short}
    Clears information about a drop destination set with the
    @fun{gtk-drag-dest-set} function.
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
  (tlist (g-boxed-foreign gtk-target-list)))

(defun gtk-drag-dest-find-target (widget context &optional (tlist nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} drag destination widget}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[tlist]{a @class{gtk-target-list} instance with the droppable
    targets, or @code{nil}, the default value, to use the
    @fun{gtk-drag-dest-target-list} function}
  @begin{return}
    First target that the source offers and the destination can accept,
    or the @code{:none} value.
  @end{return}
  @begin{short}
    Looks for a match between the supported targets of the drag context and the
    target list, returning the first matching target, otherwise returning the
    @code{:none} value.
  @end{short}
  The target list should usually be the return value from the
  @fun{gtk-drag-dest-target-list} function, but some widgets may have different
  valid targets for different parts of the widget. In that case, they will have
  to implement a \"drag-motion\" handler that passes the correct target list to
  this function.
  @see-class{gtk-widget}
  @see-class{gdk-drag-context}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-dest-target-list}"
  (%gtk-drag-dest-find-target widget context tlist))

(export 'gtk-drag-dest-find-target)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_target_list ()
;;; gtk_drag_dest_set_target_list () -> gtk-drag-target-list
;;; ----------------------------------------------------------------------------

(defun (setf gtk-drag-dest-target-list) (tlist widget)
  (foreign-funcall "gtk_drag_dest_set_target_list"
                   (g-object gtk-widget) widget
                   (g-boxed-foreign gtk-target-list) tlist
                   :void)
  tlist)

(defcfun ("gtk_drag_dest_get_target_list" gtk-drag-dest-target-list)
    (g-boxed-foreign gtk-target-list)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @syntax[]{(gtk-dag-dest-target-list widget) => tlist}
  @syntax[]{(setf (gtk-drag-dest-target-list widget) tlist)}
  @argument[widget]{a @class{gtk-widget} object that is a drag destination}
  @argument[tlist]{a @symbol{gtk-target-list} instance of droppable targets,
    or @code{nil} for none}
  @begin{short}
    The @sym{gtk-drag-dest-target-list} function returns the list of targets
    this widget can accept from drag and drop.
  @end{short}
  The @sym{(setf gtk-drag-dest-target-list)} function sets the target types.
  The widget must first be made into a drag destination with the
  @fun{gtk-drag-dest-set} function.
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-dest-set}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_text_targets" gtk-drag-dest-add-text-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the text targets supported by the selection to the target list of the
    drag destination.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-text-targets} and
  @fun{gtk-drag-dest-target-list} functions.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-text-targets}
  @see-function{gtk-drag-dest-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_image_targets" gtk-drag-dest-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the image targets supported by the selection to the target list of the
    drag destination.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-image-targets} and
  @fun{gtk-drag-dest-target-list} functions.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-image-targets}
  @see-function{gtk-drag-dest-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_dest_add_uri_targets" gtk-drag-dest-add-uri-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @begin{short}
    Add the URI targets supported by the selection to the target list of the
    drag destination.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-uri-targets} and
  @fun{gtk-drag-dest-target-list} functions.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-uri-targets}
  @see-function{gtk-drag-dest-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-add-uri-targets)
;;; ----------------------------------------------------------------------------
;;; gtk_drag_dest_get_track_motion ()
;;; gtk_drag_dest_set_track_motion () -> gtk-drag-dest-track-motion
;;; ----------------------------------------------------------------------------

(defun (setf gtk-drag-dest-track-motion) (motion widget)
  (foreign-funcall "gtk_drag_dest_set_track_motion"
                   (g-object gtk-widget) widget
                   :boolean motion
                   :void)
  motion)

(defcfun ("gtk_drag_dest_get_track_motion" gtk-drag-dest-track-motion) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @syntax[]{(gtk-drag-dest-track-motion widget) => motion}
  @syntax[]{(setf (gtk-drag-dest-track-motion widget) motion)}
  @argument[widget]{a @class{gtk-widget} that is a drag destination}
  @argument[motion]{a boolean whether to accept all targets}
  @begin{short}
    The @sym{gtk-drag-dest-track-motion} function returns whether the widget
    has been configured to always emit \"drag-motion\" signals.
  @end{short}
  The @sym{(setf gtk-drag-dest-track-motion)} function tells the widget to emit
  \"drag-motion\" and \"drag-leave\" events regardless of the targets and the
  @code{:motion} flag.

  This may be used when a widget wants to do generic actions regardless of the
  targets that the source offers.
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-dest-track-motion)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_finish ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_finish" gtk-drag-finish) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{the @class{gdk-drag-context} object}
  @argument[success]{a boolean indicating whether the drop was successful}
  @argument[delete]{a boolean indicating whether the source should delete the
    original data, this should be @em{true} for a move}
  @argument[time]{an unsigned integer with the timestamp from the \"drag-drop\"
    signal}
  @begin{short}
    Informs the drag source that the drop is finished, and that the data of the
    drag will no longer be required.
  @end{short}
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context))
  (success :boolean)
  (delete :boolean)
  (time :uint32))

(export 'gtk-drag-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_get_data () -> gtk-drag-data
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_get_data" gtk-drag-data) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{the @class{gtk-widget} object that will receive the
   \"drag-data-received\" signal}
  @argument[context]{a @class{gdk-drag-context} object}
  @argument[target]{an atom as a string with the target, form of the data, to
    retrieve}
  @argument[time]{an unsigned integer with the timestamp for retrieving the
    data, this will generally be the time received in a \"drag-motion\" or
    \"drag-drop\" signal}
  @begin{short}
    Gets the data associated with a drag.
  @end{short}
  When the data is received or the retrieval fails, GTK will emit a
  \"drag-data-received\" signal. Failure of the retrieval is indicated by the
  length field of the @arg{selection} signal parameter being negative. However,
  when the @sym{gtk-drag-data} function is called implicitely because the
  @code{:drop} flag was set, then the widget will not receive notification of
  failed drops.
  @see-class{gtk-widget}
  @see-class{gdk-drag-context}
  @see-symbol{gdk-atom}"
  (widget (g-object gtk-widget))
  (context (g-object gdk-drag-context))
  (target gdk-atom-as-string)
  (time :uint32))

(export 'gtk-drag-data)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_get_source_widget () -> gtk-drag-source-widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_get_source_widget" gtk-drag-source-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object with the destination
    side drag context}
  @return{If the drag is occurring within a single application, returns a
    pointer to the source widget, otherwise @code{nil}.}
  @begin{short}
    Determines the source widget for a drag.
  @end{short}
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context)))

(export 'gtk-drag-source-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_highlight ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_highlight" gtk-drag-highlight) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object to highlight}
  @begin{short}
    Draws a highlight around a widget.
  @end{short}
  This will attach handlers to the \"draw\" handler, so the highlight will
  continue to be displayed until the @fun{gtk-drag-unhighlight} function is
  called.
  @see-class{gtk-widget}
  @see-function{gtk-drag-unhighlight}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-highlight)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_unhighlight ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_unhighlight" gtk-drag-unhighlight) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object to remove the highlight from}
  @begin{short}
    Removes a highlight set by the @fun{gtk-drag-highlight} function from a
    widget.
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-drag-highlight}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-unhighlight)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_begin ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_begin" gtk-drag-begin) (g-object gdk-drag-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} source widget}
  @argument[targets]{a @class{gtk-target-list} instance with the targets, data
  formats, in which the source can provide the data}
  @argument[actions]{a @symbol{gdk-drag-action} bitmask of the allowed drag
    actions for this drag}
  @argument[button]{an integer with the button the user clicked to start the
    drag}
  @argument[event]{the @class{gdk-event} event that triggered the start of the
    drag}
  @return{The @class{gdk-drag-context} object for this drag.}
  @begin{short}
    Initiates a drag on the source side.
  @end{short}
  The function only needs to be used when the application is starting drags
  itself, and is not needed when the @fun{gtk-drag-source-set} function is used.

  The @arg{event} argument is used to retrieve the timestamp that will be used
  internally to grab the pointer. If the @arg{event} argument is @code{nil},
  then the @var{+gdk-current-time+} value will be used. However, you should try
  to pass a real event in all cases, since that can be used by GTK to get
  information about the start position of the drag, for example if the event is
  a @code{:motion-notify} event.

  Generally there are three cases when you want to start a drag by hand by
  calling this function:
  @begin{enumerate}
    @begin{item}
      During a \"button-press-event\" handler, if you want to start a drag
      immediately when the user presses the mouse button. Pass the event that
      you have in your \"button-press-event\" handler.
    @end{item}
    @begin{item}
      During a \"motion-notify-event\" handler, if you want to start a drag when
      the mouse moves past a certain threshold distance after a button press.
      Pass the event that you have in your \"motion-notify-event\" handler.
    @end{item}
    @begin{item}
      During a timeout handler, if you want to start a drag after the mouse
      button is held down for some time. Try to save the last event that you
      got from the mouse, using the @fun{gdk-event-copy} function, and pass it
      to this function. If you can really not pass a real event, pass the
      @code{nil} value instead.
    @end{item}
  @end{enumerate}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-begin} function has been deprecated since version 3.10
    and should not be used in newly written code. Use the
    @fun{gtk-drag-begin-with-coordinates} function instead.
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_begin_with_coordinates" gtk-drag-begin-with-coordinates)
    (g-object gdk-drag-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{the @class{gtk-widget} source widget}
  @argument[targets]{a @class{gtk-target-list} instance with the targets, data
    formats, in which the source can provide the data}
  @argument[actions]{a @symbol{gdk-drag-action} bitmask of the allowed drag
    actions for this drag}
  @argument[button]{an integer with the button the user clicked to start the
    drag}
  @argument[event]{a @class{gdk-event} event that triggered the start of the
    drag, or @code{nil} if none can be obtained}
  @argument[x]{an integer with the initial x coordinate to start dragging from,
    in the coordinate space of the widget, if -1 is passed, the coordinates are
    retrieved from event or the current pointer position}
  @argument[y]{an integer with the initial y coordinate to start dragging from,
    in the coordinate space of the widget, if -1 is passed, the coordinates are
    retrieved from event or the current pointer position}
  @return{The @class{gdk-drag-context} object for this drag.}
  @begin{short}
    Initiates a drag on the source side.
  @end{short}
  The function only needs to be used when the application is starting drags
  itself, and is not needed when the @fun{gtk-drag-source-set} function is used.

  The @arg{event} argument is used to retrieve the timestamp that will be used
  internally to grab the pointer. If the @arg{event} argument is @code{nil},
  then the @var{+gdk-current-time+} value will be used. However, you should try
  to pass a real event in all cases, since that can be used to get information
  about the drag.

  Generally there are three cases when you want to start a drag by hand by
  calling this function:
  @begin{enumerate}
    @begin{item}
      During a \"button-press-event\" handler, if you want to start a drag
      immediately when the user presses the mouse button. Pass the event that
      you have in your \"button-press-event\" handler.
    @end{item}
    @begin{item}
      During a \"motion-notify-event\" handler, if you want to start a drag
      when the mouse moves past a certain threshold distance after a button
      press. Pass the event that you have in your \"motion-notify-event\"
      handler.
    @end{item}
    @begin{item}
      During a timeout handler, if you want to start a drag after the mouse
      button is held down for some time. Try to save the last event that you
      got from the mouse, using the @fun{gdk-event-copy} function, and pass it
      to this function. If you really cannot pass a real event, pass the
      @code{nil} value instead.
    @end{item}
  @end{enumerate}
  @see-class{gdk-drag-context}
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-symbol{gdk-drag-action}
  @see-class{gdk-event}
  @see-function{gtk-drag-source-set}"
  (widget (g-object gtk-widget))
  (targets (g-boxed-foreign gtk-target-list))
  (actions gdk-drag-action)
  (button :int)
  (event (g-boxed-foreign gdk-event))
  (x :int)
  (y :int))

(export 'gtk-drag-begin-with-coordinates)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_cancel ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_cancel" gtk-drag-cancel) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object, as e.g. returned by
    the @fun{gtk-drag-begin-with-coordinates} function}
  @begin{short}
    Cancels an ongoing drag operation on the source side.
  @end{short}

  If you want to be able to cancel a drag operation in this way, you need to
  keep a pointer to the drag context, either from an explicit call to the
  @fun{gtk-drag-begin-with-coordinates} function, or by connecting to the
  \"drag-begin\" signal.

  If the @arg{context} argument does not refer to an ongoing drag operation,
  this function does nothing.

  If a drag is cancelled in this way, the result argument of the \"drag-failed\"
  handler is set to the @code{:error} value.
  @see-class{gdk-drag-context}
  @see-function{gtk-drag-begin-with-coordinates}"
  (context (g-object gdk-drag-context)))

(export 'gtk-drag-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_widget" gtk-drag-set-icon-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{the @class{gdk-drag-context} object for a drag, this must
    be called with a drag context for the source side of a drag}
  @argument[widget]{a @class{gtk-widget} toplevel window to use as an icon}
  @argument[x]{an integer with the x offset within widget of the hotspot}
  @argument[y]{an integer with the y offset within widget of the hotspot}
  @begin{short}
    Changes the icon for a widget to a given widget.
  @end{short}
  GTK will not destroy the icon, so if you do not want it to persist, you
  should connect to the \"drag-end\" signal and destroy it yourself.
  @see-class{gdk-drag-context}
  @see-class{gtk-widget}"
  (context (g-object gdk-drag-context))
  (widget (g-object gtk-widget))
  (x :int)
  (y :int))

(export 'gtk-drag-set-icon-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_pixbuf" gtk-drag-set-icon-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @argument[pixbuf]{a @class{gdk-pixbuf} object to use as the drag icon}
  @argument[x]{an integer with the x offset within widget of the hotspot}
  @argument[y]{an integer with the y offset within widget of the hotspot}
  @begin{short}
    Sets the pixbuf as the icon for a given drag.
  @end{short}
  @see-class{gdk-drag-context}
  @see-class{gdk-pixbuf}"
  (context (g-object gdk-drag-context))
  (pixbuf (g-object gdk-pixbuf))
  (x :int)
  (y :int))

(export 'gtk-drag-set-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_stock" gtk-drag-set-icon-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @argument[stock]{a string with the ID of the stock icon to use for the drag}
  @argument[x]{an integer with the x offset within the icon of the hotspot}
  @argument[y]{an integer with the y offset within the icon of the hotspot}
  @begin{short}
    Sets the icon for a given drag from a stock ID.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-set-icon-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk-drag-set-icon-name} function instead.
  @end{dictionary}
  @see-class{gdk-drag-context}
  @see-function{gtk-drag-set-icon-name}"
  (context (g-object gdk-drag-context))
  (stock :string)
  (x :int)
  (y :int))

(export 'gtk-drag-set-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_surface" gtk-drag-set-icon-surface) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @argument[surface]{a @symbol{cairo-surface-t} surface to use as icon}
  @begin{short}
    Sets the Cairo surface as the icon for a given drag.
  @end{short}
  GTK retains references for the arguments, and will release them when they
  are no longer needed.

  To position the surface relative to the mouse, use the
  @fun{cairo-surface-set-device-offset} function on the surface. The mouse
  cursor will be positioned at the (0,0) coordinate of the surface.
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
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @argument[name]{a string with the name of the icon to use}
  @argument[x]{an integer with the x offset of the hotspot within the icon}
  @argument[y]{an integer with the y offset of the hotspot within the icon}
  @begin{short}
    Sets the icon for a given drag from a named themed icon.
  @end{short}
  See the docs for the @class{gtk-icon-theme} implementation for more details.
  Note that the size of the icon depends on the icon theme, the icon is loaded
  at the @code{:dnd} symbolic size, thus the @arg{x} and @arg{y} arguments have
  to be used with care.
  @see-class{gdk-drag-context}
  @see-class{gtk-icon-theme}"
  (context (g-object gdk-drag-context))
  (name :string)
  (x :int)
  (y :int))

(export 'gtk-drag-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_gicon" gtk-drag-set-icon-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @argument[icon]{a @class{g-icon} object}
  @argument[x]{an integer with the x offset of the hotspot within the icon}
  @argument[y]{an integer with the y offset of the hotspot within the icon}
  @begin{short}
    Sets the icon for a given drag from the given icon.
  @end{short}
  See the documentation for the @fun{gtk-drag-set-icon-name} function for more
  details about using icons in drag and drop.
  @see-class{gdk-drag-context}
  @see-class{g-icon}
  @see-function{gtk-drag-set-icon-name}"
  (context (g-object gdk-drag-context))
  (icon (g-object g-icon))
  (x :int)
  (y :int))

(export 'gtk-drag-set-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_set_icon_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_set_icon_default" gtk-drag-set-icon-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[context]{a @class{gdk-drag-context} object for a drag, this must be
    called with a drag context for the source side of a drag}
  @begin{short}
    Sets the icon for a particular drag to the default icon.
  @end{short}
  @see-class{gdk-drag-context}"
  (context (g-object gdk-drag-context)))

(export 'gtk-drag-set-icon-default)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_check_threshold ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_check_threshold" gtk-drag-check-threshold) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[start-x]{an integer with the x coordinate of start of drag}
  @argument[start-y]{an integer with the y coordinate of start of drag}
  @argument[current-x]{an integer with the current x coordinate}
  @argument[current-y]{an integer with the current y coordinate}
  @return{@em{True} if the drag threshold has been passed.}
  @begin{short}
    Checks to see if a mouse drag starting at (@arg{start-x}, @arg{start-y})
    and ending at (@arg{current-x}, @arg{current-y}) has passed the GTK drag
    threshold, and thus should trigger the beginning of a drag and drop
    operation.
  @end{short}
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
  (mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-drag-source-set (widget mask targets actions)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[mask]{a @symbol{gdk-modifier-type} bitmask of buttons that can
    start the drag}
  @argument[targets]{a list of target entries that the drag will support, may
    be @code{nil}}
  @argument[actions]{a @symbol{gdk-drag-action} bitmask of possible actions
    for a drag from this widget}
  @begin{short}
    Sets up a widget so that GTK will start a drag operation when the user
    clicks and drags on the widget.
  @end{short}
  The widget must have a GDK window.
  @see-class{gtk-widget}
  @see-symbol{gdk-modifier-type}
  @see-symbol{gdk-drag-action}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr '(:struct %gtk-target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (mem-aptr targets-ptr
                                       '(:struct %gtk-target-entry) i)
            for entry = (pop targets)
            do (with-foreign-slots ((target flags info)
                                    target-ptr
                                    (:struct %gtk-target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%gtk-drag-source-set widget mask targets-ptr n-targets actions))))

(export 'gtk-drag-source-set)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_pixbuf" gtk-drag-source-set-icon-pixbuf)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[pixbuf]{the @class{gdk-pixbuf} object for the drag icon}
  @begin{short}
    Sets the icon that will be used for drags from a particular widget from a
    pixbuf.
  @end{short}
  @see-class{gtk-widget}
  @see-class{gdk-pixbuf}
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
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[stock]{a string with the ID of the stock icon to use}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to a
    stock icon.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-drag-source-set-icon-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk-drag-source-set-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk-widget}
  @see-function{gtk-drag-source-set-icon-name}"
  (widget (g-object gtk-widget))
  (stock :string))

(export 'gtk-drag-source-set-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_name" gtk-drag-source-set-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[name]{a string with the name of icon to use}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to a
    themed icon.
  @end{short}
  See the docs for the @class{gtk-icon-theme} implementation for more details.
  @see-class{gtk-widget}
  @see-class{gtk-icon-theme}"
  (widget (g-object gtk-widget))
  (name :string))

(export 'gtk-drag-source-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_set_icon_gicon" gtk-drag-source-set-icon-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Sets the icon that will be used for drags from a particular source to
    @arg{icon}.
  @end{short}
  See the docs for the @class{gtk-icon-theme} implementation for more details.
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
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object}
  @begin{short}
    Undoes the effects of the @fun{gtk-drag-source-set} function.
  @end{short}
  @see-class{gtk-widget}
  @see-function{gtk-drag-source-set}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-unset)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_target_list ()
;;; gtk_drag_source_get_target_list () -> gtk-drag-source-target-list
;;; ----------------------------------------------------------------------------

(defun (setf gtk-drag-source-target-list) (tlist widget)
  (foreign-funcall "gtk_drag_source_set_target_list"
                   (g-object gtk-widget) widget
                   (g-boxed-foreign gtk-target-list) tlist
                   :void)
  tlist)

(defcfun ("gtk_drag_source_get_target_list" gtk-drag-source-target-list)
    (g-boxed-foreign gtk-target-list)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @syntax[]{(gtk-drag-source-target-list widget) => tlist}
  @syntax[]{(setf (gtk-drag-source-target-list widget) tlist)}
  @argument[widget]{a @class{gtk-widget} that is a drag source}
  @argument[tlist]{a  @class{gtk-target-list} instance with the draggable
    targets, or @code{nil} for none}
  @begin{short}
    The @sym{gtk-drag-source-target-list} function gets the list of targets
    this widget can provide for drag and drop.
  @end{short}
  The @sym{(setf gtk-drag-source-target-list)} function changes the target
  types.

  The widget must first be made into a drag source with the
  @fun{gtk-drag-source-set} function.
  @see-class{gtk-widget}
  @see-class{gtk-target-list}
  @see-function{gtk-drag-source-set}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-target-list)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_text_targets" gtk-drag-source-add-text-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object that is a drag source}
  @begin{short}
    Add the text targets supported by the selection to the target list of the
    drag source.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-text-targets} and
  @fun{gtk-drag-source-target-list} functions.
  @see-class{gtk-widget}
  @see-class{gtk-selection-data}
  @see-function{gtk-target-list-add-text-targets}
  @see-function{gtk-drag-source-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_image_targets" gtk-drag-source-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object that is a drag source}
  @begin{short}
    Add the writable image targets supported by the selection to the target
    list of the drag source.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-image-targets} and
  @fun{gtk-drag-source-target-list} functions.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-image-targets}
  @see-function{gtk-drag-source-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_drag_source_add_uri_targets" gtk-drag-source-add-uri-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-10-3}
  @argument[widget]{a @class{gtk-widget} object that is a drag source}
  @begin{short}
    Add the URI targets supported by the selection to the target list of the
    drag source.
  @end{short}
  The targets are added with the 0 value for info. If you need another value,
  use the @fun{gtk-target-list-add-uri-targets} and
  @fun{gtk-drag-source-target-list} functions.
  @see-class{gtk-widget}
  @see-function{gtk-target-list-add-uri-targets}
  @see-function{gtk-drag-source-target-list}"
  (widget (g-object gtk-widget)))

(export 'gtk-drag-source-add-uri-targets)

;;; --- End of file gtk.drag-and-drop.lisp -------------------------------------
