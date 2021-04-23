;;; ----------------------------------------------------------------------------
;;; gtk.gesture.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GtkGesture
;;;
;;;     Base class for gestures
;;;
;;; Types and Values
;;;
;;;     GtkGesture
;;;     GtkEventSequenceState
;;;
;;; Functions
;;;
;;;     gtk_gesture_get_device
;;;     gtk_gesture_get_window                             Accessor
;;;     gtk_gesture_set_window                             Accessor
;;;     gtk_gesture_is_active
;;;     gtk_gesture_is_recognized
;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state
;;;     gtk_gesture_set_state
;;;     gtk_gesture_get_sequences
;;;     gtk_gesture_handles_sequence
;;;     gtk_gesture_get_last_updated_sequence
;;;     gtk_gesture_get_last_event
;;;     gtk_gesture_get_point
;;;     gtk_gesture_get_bounding_box
;;;     gtk_gesture_get_bounding_box_center
;;;     gtk_gesture_group
;;;     gtk_gesture_ungroup
;;;     gtk_gesture_get_group
;;;     gtk_gesture_is_grouped_with
;;;
;;; Properties
;;;
;;;          guint    n-points                  Read / Write / Construct Only
;;;      GdkWindow*   window                    Read / Write
;;;
;;; Signals
;;;
;;;           void    begin                     Run Last
;;;           void    cancel                    Run Last
;;;           void    end                       Run Last
;;;           void    sequence-state-changed    Run Last
;;;           void    update                    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ├── GtkGestureSingle
;;;             ├── GtkGestureRotate
;;;             ╰── GtkGestureZoom
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkEventSequenceState
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkEventSequenceState" gtk-event-sequence-state
  (:export t
   :type-initializer "gtk_event_sequence_state_get_type")
  (:none 0)
  (:claimed 1)
  (:denied 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-event-sequence-state atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-event-sequence-state atdoc:*external-symbols*)
 "@version{2020-9-10}
  @begin{short}
    Describes the state of a @class{gdk-event-sequence} in a
    @class{gtk-gesture}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkEventSequenceState\" gtk-event-sequence-state
  (:export t
   :type-initializer \"gtk_event_sequence_state_get_type\")
  (:none 0)
  (:claimed 1)
  (:denied 2))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{The sequence is handled, but not grabbed.}
    @entry[:claimed]{The sequence is handled and grabbed.}
    @entry[:denied]{The sequence is denied.}
  @end{table}
  @see-class{gtk-gesture}
  @see-class{gdk-event-sequence}")

;;; ----------------------------------------------------------------------------
;;; struct GtkGesture
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGesture" gtk-gesture
  (:superclass gtk-event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_get_type")
  ((n-points
    gtk-gesture-n-points
    "n-points" "guint" t t)
   (window
    gtk-gesture-window
    "window" "GdkWindow" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture 'type)
 "@version{2020-9-10}
  @begin{short}
    @sym{gtk-gesture} is the base object for gesture recognition, although this
    object is quite generalized to serve as a base for multi-touch gestures, it
    is suitable to implement single-touch and pointer-based gestures (using the
    special @code{nil} @class{gdk-event-sequence} value for these).
  @end{short}

  The number of touches that a @sym{gtk-gesture} need to be recognized is
  controlled by the @code{n-points} property, if a gesture is keeping track of
  less or more than that number of sequences, it won't check whether the gesture
  is recognized.

  As soon as the gesture has the expected number of touches, the gesture will
  run the \"check\" signal regularly on input events until the gesture is
  recognized, the criteria to consider a gesture as \"recognized\" is left to
  @sym{gtk-gesture} subclasses.

  A recognized gesture will then emit the following signals:

  @begin{itemize}
    @item{\"begin\" when the gesture is recognized.}
    @item{A number of \"update\", whenever an input event is processed.}
    @item{\"end\" when the gesture is no longer recognized.}
  @end{itemize}

  @subheading{Event propagation}
  In order to receive events, a gesture needs to either set a propagation phase
  through the function @fun{gtk-event-controller-propagation-phase}, or feed
  those manually through the function @fun{gtk-event-controller-handle-event}.

  In the capture phase, events are propagated from the toplevel down to the
  target widget, and gestures that are attached to containers above the widget
  get a chance to interact with the event before it reaches the target.

  After the capture phase, GTK+ emits the traditional \"button-press-event\",
  \"button-release-event\", \"touch-event\", etc signals. Gestures with the
  @code{:phase-target} phase are fed events from the default \"event\" handlers.

  In the bubble phase, events are propagated up from the target widget to the
  toplevel, and gestures that are attached to containers above the widget get a
  chance to interact with events that have not been handled yet.

  @subheading{States of a sequence}
  Whenever input interaction happens, a single event may trigger a cascade of
  @class{gtk-gesture}s, both across the parents of the widget receiving the
  event and in parallel within an individual widget. It is a responsibility of
  the widgets using those gestures to set the state of touch sequences
  accordingly in order to enable cooperation of gestures around the
  @class{gdk-event-sequences} triggering those.

  Within a widget, gestures can be grouped through the function
  @fun{gtk-gesture-group}, grouped gestures synchronize the state of sequences,
  so calling the function @fun{gtk-gesture-set-sequence-state} on one will
  effectively propagate the state throughout the group.

  By default, all sequences start out in the @code{:none} state, sequences in
  this state trigger the gesture event handler, but event propagation will
  continue unstopped by gestures.

  If a sequence enters into the @code{:denied} state, the gesture group will
  effectively ignore the sequence, letting events go unstopped through the
  gesture, but the \"slot\" will still remain occupied while the touch is
  active.

  If a sequence enters in the @code{:claimed} state, the gesture group will
  grab all interaction on the sequence, by:
  @begin{itemize}
    @item{Setting the same sequence to @code{:denied} on every other gesture
      group within the widget, and every gesture on parent widgets in the
      propagation chain.}
    @item{calling \"cancel\" on every gesture in widgets underneath in the
      propagation chain.}
    @item{Stopping event propagation after the gesture group handles the event.}
  @end{itemize}
  Note: if a sequence is set early to @code{:claimed} on
  @code{:touch-begin}/@code{:button-press} (so those events are captured before
  reaching the event widget, this implies @code{:phase-capture}), one similar
  event will emulated if the sequence changes to @code{:denied}. This way event
  coherence is preserved before event propagation is unstopped again.

  Sequence states can't be changed freely, see
  @fun{gtk-gesture-set-sequence-state} to know about the possible lifetimes of
  a @class{gdk-event-sequence}.

  @subheading{Touchpad gestures}
  On the platforms that support it, @sym{gtk-gesture} will handle transparently
  touchpad gesture events. The only precautions users of @sym{gtk-gesture}
  should do to enable this support are:
  @begin{itemize}
    @item{Enabling GDK_TOUCHPAD_GESTURE_MASK on their @class{gdk-windows}}
    @item{If the gesture has @code{:phase-none}, ensuring events of type
      GDK_TOUCHPAD_SWIPE and GDK_TOUCHPAD_PINCH are handled by the
      @sym{gtk-gesture}}
  @end{itemize}
  @begin[Signal Details]{dictionary}
    @subheading{The \"begin\" signal}
      @begin{pre}
 lambda (gesture sequence)    : Run Last
      @end{pre}
      This signal is emitted when the gesture is recognized. This means the
      number of touch sequences matches @code{n-points}, and the \"check\"
      handler(s) returned @em{true}. Note: These conditions may also happen when
      an extra touch (e.g. a third touch on a 2-touches gesture) is lifted, in
      that situation sequence will not pertain to the current set of active
      touches, so do not rely on this being true.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk-event-sequence} that made the gesture
          to be recognized.}
      @end{table}
    @subheading{The \"cancel\" signal}
      @begin{pre}
 lambda (gesture sequence)    : Run Last
      @end{pre}
      This signal is emitted whenever a sequence is cancelled. This usually
      happens on active touches when the function
      @fun{gtk-event-controller-reset} is called on gesture (manually, due to
      grabs ...), or the individual sequence was claimed by parent widgets'
      controllers (see the function @fun{gtk-gesture-set-sequence-state}).
      @arg{gesture} must forget everything about sequence as a reaction to this
      signal.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk-event-sequence} that was cancelled.}
      @end{table}
    @subheading{The \"end\" signal}
      @begin{pre}
 lambda (gesture sequence)    : Run Last
      @end{pre}
      This signal is emitted when gesture either stopped recognizing the event
      sequences as something to be handled (the \"check\" handler returned
      @code{nil}), or the number of touch sequences became higher or lower than
      @code{n-points}. Note: sequence might not pertain to the group of
      sequences that were previously triggering recognition on gesture (i.e. a
      just pressed touch sequence that exceeds @code{n-points}). This situation
      may be detected by checking through the function
      @fun{gtk-gesture-handles-sequence}.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk-event-sequence} that made gesture
          recognition to finish.}
      @end{table}
    @subheading{The \"sequence-state-changed\" signal}
      @begin{pre}
 lambda (gesture sequence state)    : Run Last
      @end{pre}
      This signal is emitted whenever a sequence state changes. See the function
      @fun{gtk-gesture-set-sequence-state} to know more about the expectable
      sequence lifetimes.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk-event-sequence} that was cancelled.}
        @entry[state]{The new sequence state.}
      @end{table}
    @subheading{The \"update\" signal}
      @begin{pre}
 lambda (gesture sequence)    : Run Last
      @end{pre}
      This signal is emitted whenever an event is handled while the gesture is
      recognized. @arg{sequence} is guaranteed to pertain to the set of active
      touches.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture} object which received the signal.}
        @entry[sequence]{The @class{gdk-event-sequence} that was updated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-gesture-n-points}
  @see-slot{gtk-gesture-window}
  @see-class{gtk-event-controller}
  @see-class{gtk-gesture-single}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-gesture-n-points ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "n-points" 'gtk-gesture) 't)
 "The @code{n-points} property of type @code{:int} (Read / Write) @br{}
  The number of touch points that trigger recognition on this gesture. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1 @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-n-points atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-n-points 'function)
 "@version{2020-9-15}
  @syntax[]{(gtk-gesture-n-points object) => n-points)}
  @syntax[]{(setf (gtk-gesture-n-points object) n-points)}
  @argument[object]{a @class{gtk-gesture} object}
  @argument[n-points]{the number of touch points}
  @begin{short}
    Accessor of the @slot[gtk-gesture]{n-points} slot of the
    @class{gtk-gesture} class.
  @end{short}

  The number of touch points that trigger recognition on this gesture.
  @see-class{gtk-gesture}")

;;; --- gtk-gesture-window -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "window" 'gtk-gesture) 't)
 "The @code{window} property of type @class{GdkWindow} (Read / Write) @br{}
  If non-@code{nil}, the gesture will only listen for events that happen on this
  @class{gdk-window}, or a child of it.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-window atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-window 'function)
 "@version{2020-9-15}
  @syntax[]{(gtk-gesture-window object) => window)}
  @syntax[]{(setf (gtk-gesture-window object) window)}
  @argument[object]{a @class{gtk-gesture} object}
  @argument[window]{a @class{gdk-window} object}
  @begin{short}
    Accessor of the @slot[gtk-gesture]{window} slot of the
    @class{gtk-gesture} class.
  @end{short}

  The slot access function @sym{gtk-gesture-window} returns the user-defined
  window that receives the events handled by the gesture. The slot access
  function @sym{(setf gtk-gesture-window)} sets a specific window to receive
  events about, so gesture will effectively handle only events targeting window,
  or a child of it. @arg{window} must pertain to the function
  @fun{gtk-event-controller-widget}.
  @see-class{gtk-gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_device ()
;;;
;;; GdkDevice * gtk_gesture_get_device (GtkGesture *gesture);
;;;
;;; Returns the master GdkDevice that is currently operating on gesture , or
;;; NULL if the gesture is not being interacted.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     a GdkDevice, or NULL.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_active ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_is_active" gtk-gesture-is-active) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture} object}
  @return{@em{True} if @arg{gesture} is active.}
  @begin{short}
    Returns @em{true} if the gesture is currently active.
  @end{short}
  A gesture is active meanwhile there are touch sequences interacting with it.
  @see-class{gtk-gesture}"
  (gesture (g-object gtk-gesture)))

(export 'gtk-gesture-is-active)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_recognized ()
;;;
;;; gboolean
;;; gtk_gesture_is_recognized (GtkGesture *gesture);
;;;
;;; Returns TRUE if the gesture is currently recognized. A gesture is recognized
;;; if there are as many interacting touch sequences as required by gesture ,
;;; and “check” returned TRUE for the sequences being currently interpreted.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     TRUE if gesture is recognized
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequence_state ()
;;;
;;; GtkEventSequenceState
;;; gtk_gesture_get_sequence_state (GtkGesture *gesture,
;;;                                 GdkEventSequence *sequence);
;;;
;;; Returns the sequence state, as seen by gesture .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence
;;;
;;; Returns :
;;;     The sequence state in gesture
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_gesture_set_sequence_state ()
;;;
;;; gboolean
;;; gtk_gesture_set_sequence_state (GtkGesture *gesture,
;;;                                 GdkEventSequence *sequence,
;;;                                 GtkEventSequenceState state);
;;;
;;; Sets the state of sequence in gesture . Sequences start in state
;;; GTK_EVENT_SEQUENCE_NONE, and whenever they change state, they can never go
;;; back to that state. Likewise, sequences in state GTK_EVENT_SEQUENCE_DENIED
;;; cannot turn back to a not denied state. With these rules, the lifetime of an
;;; event sequence is constrained to the next four:
;;;
;;; None
;;; None → Denied
;;; None → Claimed
;;; None → Claimed → Denied
;;;
;;; Note: Due to event handling ordering, it may be unsafe to set the state on
;;; another gesture within a “begin” signal handler, as the callback might be
;;; executed before the other gesture knows about the sequence. A safe way to
;;; perform this could be:
;;;
;;; static void
;;; first_gesture_begin_cb (GtkGesture       *first_gesture,
;;;                         GdkEventSequence *sequence,
;;;                         gpointer          user_data)
;;; {
;;;   gtk_gesture_set_sequence_state (first_gesture, sequence, GTK_EVENT_SEQUENCE_CLAIMED);
;;;   gtk_gesture_set_sequence_state (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED);
;;; }
;;;
;;; static void
;;; second_gesture_begin_cb (GtkGesture       *second_gesture,
;;;                          GdkEventSequence *sequence,
;;;                          gpointer          user_data)
;;; {
;;;   if (gtk_gesture_get_sequence_state (first_gesture, sequence) == GTK_EVENT_SEQUENCE_CLAIMED)
;;;     gtk_gesture_set_sequence_state (second_gesture, sequence, GTK_EVENT_SEQUENCE_DENIED);
;;; }
;;;
;;; If both gestures are in the same group, just set the state on the gesture
;;; emitting the event, the sequence will be already be initialized to the
;;; group's global state when the second gesture processes the event.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence
;;;
;;; state :
;;;     the sequence state
;;;
;;; Returns :
;;;     TRUE if sequence is handled by gesture , and the state is changed
;;;     successfully
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_set_state ()
;;;
;;; gboolean
;;; gtk_gesture_set_state (GtkGesture *gesture,
;;;                        GtkEventSequenceState state);
;;;
;;; Sets the state of all sequences that gesture is currently interacting with.
;;; See gtk_gesture_set_sequence_state() for more details on sequence states.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; state :
;;;     the sequence state
;;;
;;; Returns :
;;;     TRUE if the state of at least one sequence was changed successfully
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequences ()
;;;
;;; GList *
;;; gtk_gesture_get_sequences (GtkGesture *gesture);
;;;
;;; Returns the list of GdkEventSequences currently being interpreted by
;;; gesture .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     A list of GdkEventSequences, the list elements are owned by GTK+ and
;;;     must not be freed or modified, the list itself must be deleted through
;;;     g_list_free().
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_handles_sequence ()
;;;
;;; gboolean
;;; gtk_gesture_handles_sequence (GtkGesture *gesture,
;;;                              GdkEventSequence *sequence);
;;;
;;; Returns TRUE if gesture is currently handling events corresponding to
;;; sequence .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence or NULL.
;;;
;;; Returns :
;;;     TRUE if gesture is handling sequence , FALSE otherwise
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_updated_sequence ()
;;;
;;; GdkEventSequence *
;;; gtk_gesture_get_last_updated_sequence (GtkGesture *gesture);
;;;
;;; Returns the GdkEventSequence that was last updated on gesture .
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     The last updated sequence.
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_event ()
;;;
;;; const GdkEvent *
;;; gtk_gesture_get_last_event (GtkGesture *gesture,
;;;                            GdkEventSequence *sequence);
;;;
;;; Returns the last event that was processed for sequence .
;;;
;;; Note that the returned pointer is only valid as long as the sequence is
;;; still interpreted by the gesture . If in doubt, you should make a copy of
;;; the event.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence.
;;;
;;; Returns :
;;;     The last event from sequence .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_point ()
;;;
;;; gboolean
;;; gtk_gesture_get_point (GtkGesture *gesture,
;;;                        GdkEventSequence *sequence,
;;;                        gdouble *x,
;;;                        gdouble *y);
;;;
;;; If sequence is currently being interpreted by gesture , this function
;;; returns TRUE and fills in x and y with the last coordinates stored for that
;;; event sequence. The coordinates are always relative to the widget
;;; allocation.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; sequence :
;;;     a GdkEventSequence, or NULL for pointer events.
;;;
;;; x :
;;;     return location for X axis of the sequence coordinates.
;;;
;;; y :
;;;     return location for Y axis of the sequence coordinates.
;;;
;;; Returns :
;;;     TRUE if sequence is currently interpreted
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box ()
;;;
;;; gboolean
;;; gtk_gesture_get_bounding_box (GtkGesture *gesture,
;;;                               GdkRectangle *rect);
;;;
;;; If there are touch sequences being currently handled by gesture , this
;;; function returns TRUE and fills in rect with the bounding box containing all
;;; active touches. Otherwise, FALSE will be returned.
;;;
;;; Note: This function will yield unexpected results on touchpad gestures.
;;; Since there is no correlation between physical and pixel distances, these
;;; will look as if constrained in an infinitely small area, rect width and
;;; height will thus be 0 regardless of the number of touchpoints.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;; rect :
;;;     bounding box containing all active touches.
;;;
;;; Returns :
;;;     TRUE if there are active touches, FALSE otherwise
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box_center ()
;;;
;;; gboolean
;;; gtk_gesture_get_bounding_box_center (GtkGesture *gesture,
;;;                                      gdouble *x,
;;;                                      gdouble *y);
;;;
;;; If there are touch sequences being currently handled by gesture , this
;;; function returns TRUE and fills in x and y with the center of the bounding
;;; box containing all active touches. Otherwise, FALSE will be returned.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; x :
;;;     X coordinate for the bounding box center.
;;;
;;; y :
;;;     Y coordinate for the bounding box center.
;;;
;;; Returns :
;;;     FALSE if no active touches are present, TRUE otherwise
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_group ()
;;;
;;; void
;;; gtk_gesture_group (GtkGesture *group_gesture,
;;;                    GtkGesture *gesture);
;;;
;;; Adds gesture to the same group than group_gesture . Gestures are by default
;;; isolated in their own groups.
;;;
;;; When gestures are grouped, the state of GdkEventSequences is kept in sync
;;; for all of those, so calling gtk_gesture_set_sequence_state(), on one will
;;; transfer the same value to the others.
;;;
;;; Groups also perform an "implicit grabbing" of sequences, if a
;;; GdkEventSequence state is set to GTK_EVENT_SEQUENCE_CLAIMED on one group,
;;; every other gesture group attached to the same GtkWidget will switch the
;;; state for that sequence to GTK_EVENT_SEQUENCE_DENIED.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; group_gesture :
;;;     GtkGesture to group gesture with
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_ungroup ()
;;;
;;; void gtk_gesture_ungroup (GtkGesture *gesture);
;;;
;;; Separates gesture into an isolated group.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_group ()
;;;
;;; GList * gtk_gesture_get_group (GtkGesture *gesture);
;;;
;;; Returns all gestures in the group of gesture
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; Returns :
;;;     The list of GtkGestures, free with g_list_free().
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_grouped_with ()
;;;
;;; gboolean
;;; gtk_gesture_is_grouped_with (GtkGesture *gesture,
;;;                              GtkGesture *other);
;;;
;;; Returns TRUE if both gestures pertain to the same group.
;;;
;;; gesture :
;;;     a GtkGesture
;;;
;;; other :
;;;     another GtkGesture
;;;
;;; Returns :
;;;     whether the gestures are grouped
;;;
;;; Since 3.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.gesture.lisp -------------------------------------------
