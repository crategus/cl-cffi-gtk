;;; ----------------------------------------------------------------------------
;;; gdk.frame-clock.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2016 - 2020 Dieter Kaiser
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
;;; Frame clock
;;;
;;;     Frame clock syncs painting to a window or display
;;;
;;; Types and Values
;;;
;;;     GdkFrameClock
;;;     GdkFrameClockPhase
;;;
;;; Functions
;;;
;;;     gdk_frame_clock_get_frame_time
;;;     gdk_frame_clock_request_phase
;;;     gdk_frame_clock_begin_updating
;;;     gdk_frame_clock_end_updating
;;;     gdk_frame_clock_get_frame_counter
;;;     gdk_frame_clock_get_history_start
;;;     gdk_frame_clock_get_timings
;;;     gdk_frame_clock_get_current_timings
;;;     gdk_frame_clock_get_refresh_info
;;;
;;; Signals
;;;
;;;     void    after-paint      Run Last
;;;     void    before-paint     Run Last
;;;     void    flush-events     Run Last
;;;     void    layout           Run Last
;;;     void    paint            Run Last
;;;     void    resume-events    Run Last
;;;     void    update           Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;      ╰── GdkFrameClock
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkFrameClockPhase
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFrameClockPhase" gdk-frame-clock-phase
  (:export t
   :type-initializer "gdk_frame_clock_phase_get_type")
  (:none 0)
  (:flush-events 1)
  (:before-paint 2)
  (:update 3)
  (:layout 4)
  (:paint 5)
  (:resume-events 6)
  (:after-paint 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-frame-clock-phase atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gdk-frame-clock-phase atdoc:*external-symbols*)
 "@version{2020-11-12}
  @begin{short}
    The @sym{gdk-frame-clock-phase} enumeration is used to represent the
    different paint clock phases that can be requested.
  @end{short}
  The elements of the enumeration correspond to the signals of the
  @class{gdk-frame-clock} class.
  @begin{pre}
(define-g-enum \"GdkFrameClockPhase\" gdk-frame-clock-phase
  (:export t
   :type-initializer \"gdk_frame_clock_phase_get_type\")
  (:none 0)
  (:flush-events 1)
  (:before-paint 2)
  (:update 3)
  (:layout 4)
  (:paint 5)
  (:resume-events 6)
  (:after-paint 7))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No phase.}
    @entry[:flush-events]{Corresponds to the signal \"flush-events\".
      Should not be handled by applications.}
    @entry[:before-paint]{Corresponds to the signal \"before-paint\".
      Should not be handled by applications.}
    @entry[:update]{Corresponds to the signal \"update\".}
    @entry[:layout]{Corresponds to the signal \"layout\".}
    @entry[:paint]{Corresponds to the signal \"paint\".}
    @entry[:resume-events]{Corresponds tol the signal \"resume-events\".
      Should not be handled by applications.}
    @entry[:after-paint]{Corresponds to the signal \"after-paint\".
      Should not be handled by applications.}
  @end{table}
  @see-class{gdk-frame-clock}")

;;; ----------------------------------------------------------------------------
;;; GdkFrameClock
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkFrameClock" gdk-frame-clock
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_frame_clock_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-frame-clock 'type)
 "@version{2020-11-12}
  @begin{short}
    A @sym{gdk-frame-clock} object tells the application when to update and
    repaint a window.
  @end{short}
  This may be synced to the vertical refresh rate of the monitor, for example.
  Even when the frame clock uses a simple timer rather than a hardware-based
  vertical sync, the frame clock helps because it ensures everything paints at
  the same time (reducing the total number of frames). The frame clock can also
  automatically stop painting when it knows the frames will not be visible, or
  scale back animation framerates.

  The @sym{gdk-frame-clock} class is designed to be compatible with an
  OpenGL-based implementation or with @code{mozRequestAnimationFrame} in
  Firefox, for example.

  A frame clock is idle until someone requests a frame with the function
  @fun{gdk-frame-clock-request-phase}. At some later point that makes sense for
  the synchronization being implemented, the clock will process a frame and
  emit signals for each phase that has been requested. See the signals of the
  @sym{gdk-frame-clock} class for documentation of the phases. The clock phase
  @code{:update} and the \"update\" signal are most interesting for application
  writers, and are used to update the animations, using the frame time given by
  the function @fun{gdk-frame-clock-frame-time}.

  The frame time is reported in microseconds and generally in the same
  timescale as the system monotonic time. The frame time does not advance
  during the time a frame is being painted, and outside of a frame, an
  attempt is made so that all calls to the function
  @fun{gdk-frame-clock-frame-time} that are called at a \"similar\" time get
  the same value. This means that if different animations are timed by looking
  at the difference in time between an initial value from the function
  @fun{gdk-frame-clock-frame-time} and the value inside the \"update\" signal
  of the clock, they will stay exactly synchronized.
  @begin[Signal Details]{dictionary}
    @subheading{The \"after-paint\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal ends processing of the frame. Applications should generally
      not handle this signal.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"before-paint\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal begins processing of the frame. Applications should generally
      not handle this signal.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"flush-events\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal is used to flush pending motion events that are being batched
      up and compressed together. Applications should not handle this signal.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"layout\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal is emitted as the second step of toolkit and application
      processing of the frame. Any work to update sizes and positions of
      application elements should be performed. GTK normally handles this
      internally.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"paint\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal is emitted as the third step of toolkit and application
      processing of the frame. The frame is repainted. GDK normally handles
      this internally and produces expose events, which are turned into GTK
      \"draw\" signals.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"resume-events\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal is emitted after processing of the frame is finished, and is
      handled internally by GTK to resume normal event processing. Applications
      should not handle this signal.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
    @subheading{The \"update\" signal}
      @begin{pre}
 lambda (clock)    :run-last
      @end{pre}
      The signal is emitted as the first step of toolkit and application
      processing of the frame. Animations should be updated using the
      @fun{gdk-frame-clock-frame-time} function. Applications can connect
      directly to this signal, or use the @fun{gtk-widget-add-tick-callback}
      function as a more convenient interface.
      @begin[code]{table}
        @entry[clock]{The @sym{gdk-frame-clock} object emitting the signal.}
      @end{table}
  @end{dictionary}
  @see-class{gdk-frame-timings}")

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_frame_time () -> gdk-frame-clock-frame-time
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_frame_time" gdk-frame-clock-frame-time) :int64
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[clock]{a @class{gdk-frame-clock} object}
  @return{An integer with a timestamp in microseconds.}
  @begin{short}
    Gets the time that should currently be used for animations.
  @end{short}
  Inside the processing of a frame, it is the time used to compute the animation
  position of everything in a frame. Outside of a frame, it is the time of the
  conceptual \"previous frame\", which may be either the actual previous frame
  time, or if that is too old, an updated time.
  @see-class{gdk-frame-clock}"
  (clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-frame-time)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_request_phase ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_request_phase" gdk-frame-clock-request-phase) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @argument[phase]{the phase of type @symbol{gdk-frame-clock-phase} that is
    requested}
  @begin{short}
    Asks the frame clock to run a particular phase.
  @end{short}
  The signal corresponding to the requested phase will be emitted the next time
  the frame clock processes. Multiple calls to the function
  @sym{gdk-frame-clock-request-phase} will be combined together and only one
  frame processed. If you are displaying animated content and want to
  continually request the @code{:update} phase for a period of time, you should
  use the function @fun{gdk-frame-clock-begin-updating} instead, since this
  allows GTK to adjust system parameters to get maximally smooth animations.
  @see-class{gdk-frame-clock}
  @see-symbol{gdk-frame-clock-phase}
  @see-function{gdk-frame-clock-begin-updating}"
  (frame-clock (g-object gdk-frame-clock))
  (phase gdk-frame-clock-phase))

(export 'gdk-frame-clock-request-phase)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_begin_updating ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_begin_updating" gdk-frame-clock-begin-updating) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Starts updates for an animation.
  @end{short}
  Until a matching call to the function @fun{gdk-frame-clock-end-updating} is
  made, the frame clock will continually request a new frame with the
  @code{:update} phase. This function may be called multiple times and frames
  will be requested until the function @fun{gdk-frame-clock-end-updating} is
  called the same number of times.
  @see-class{gdk-frame-clock}
  @see-function{gdk-frame-clock-end-updating}"
  (frame-clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-begin-updating)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_end_updating ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_end_updating" gdk-frame-clock-end-updating) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{short}
    Stops updates for an animation.
  @end{short}
  See the documentation for the function @fun{gdk-frame-clock-begin-updating}.
  @see-class{gdk-frame-clock}
  @see-function{gdk-frame-clock-begin-updating}"
  (frame-clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-end-updating)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_frame_counter () -> gdk-frame-clock-frame-counter
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_frame_counter" gdk-frame-clock-frame-counter)
    :int64
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{return}
    Inside frame processing, the unsigned integer value of the frame counter for
    the current frame. Outside of frame processing, the frame counter for the
    last frame.
  @end{return}
  @begin{short}
    A frame clock maintains a 64-bit counter that increments for each frame
    drawn.
  @end{short}
  @see-class{gdk-frame-clock}"
  (frame-clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-frame-counter)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_history_start () -> gdk-frame-history-start
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_history_start" gdk-frame-clock-history-start)
    :int64
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{return}
    The unsigned integer frame counter value for the oldest frame that is
    available in the internal frame history of the frame clock.
  @end{return}
  @begin{short}
    The frame clock internally keeps a history of @class{gdk-frame-timings}
    objects for recent frames that can be retrieved with the function
    @fun{gdk-frame-clock-timings}.
  @end{short}
  The set of stored frames is the set from the counter values given by the
  function @sym{gdk-frame-clock-history-start} and the function
  @fun{gdk-frame-clock-frame-counter}, inclusive.
  @see-class{gdk-frame-clock}
  @see-class{gdk-frame-timings}
  @see-function{gdk-frame-clock-timings}
  @see-function{gdk-frame-clock-frame-counter}"
  (frame-clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-history-start)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_timings () -> gdk-frame-clock-timings
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_timings" gdk-frame-clock-timings)
    (g-boxed-foreign gdk-frame-timings)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @argument[frame-counter]{an unsigned integer frame counter value identifying
    the frame to be received}
  @begin{return}
    The @class{gdk-frame-timings} structure for the specified frame, or
    @code{nil} if it is not available. See the function
    @fun{gdk-frame-clock-history-start}.
  @end{return}
  @begin{short}
    Retrieves a @fun{gdk-frame-timings} structure holding timing information for
    the current frame or a recent frame.
  @end{short}
  The @class{gdk-frame-timings} object may not yet be complete. See the function
  @fun{gdk-frame-timings-complete}.
  @see-class{gdk-frame-clock}
  @see-class{gdk-frame-timings}
  @see-function{gdk-frame-clock-history-start}
  @see-function{gdk-frame-timings-complete}"
  (frame-clock (g-object gdk-frame-clock))
  (frame-counter :int64))

(export 'gdk-frame-clock-timings)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_current_timings () -> gdk-frame-clock-current-timings
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_current_timings" gdk-frame-clock-current-timings)
    (g-boxed-foreign gdk-frame-timings)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @begin{return}
    The @class{gdk-frame-timings} structure for the frame currently being
    processed, or even no frame is being processed, for the previous frame.
    Before any frames have been procesed, returns @code{nil}.
  @end{return}
  @begin{short}
    Gets the frame timings for the current frame.
  @end{short}
  @see-class{gdk-frame-clock}
  @see-class{gdk-frame-timings}"
  (frame-clock (g-object gdk-frame-clock)))

(export 'gdk-frame-clock-current-timings)

;;; ----------------------------------------------------------------------------
;;; gdk_frame_clock_get_refresh_info () -> gdk-frame-clock-refresh-info
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_frame_clock_get_refresh_info" %gdk-frame-clock-refresh-info)
    :void
  (frame-clock (g-object gdk-frame-clock))
  (base-time :int64)
  (refresh-interval (:pointer :int64))
  (presentation-time-return (:pointer :int64)))

(defun gdk-frame-clock-refresh-info (frame-clock base-time)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-12}
  @argument[frame-clock]{a @class{gdk-frame-clock} object}
  @argument[base-time]{an integer base time for determining a presentaton time}
  @begin{return}
    @code{refresh-interval-return} -- an integer with the determined refresh
    interval, or @code{nil}, a default refresh interval of 1/60th of a second
    will be stored if no history is present @br{}
    @code{presentation-time-return} -- an integer with the next candidate
    presentation time after the given base time, 0 will be will be stored if no
    history is present
  @end{return}
  @begin{short}
    Using the frame history stored in the frame clock, finds the last known
    presentation time and refresh interval, and assuming that presentation times
    are separated by the refresh interval, predicts a presentation time that is
    a multiple of the refresh interval after the last presentation time, and
    later than @arg{base-time}.
  @end{short}
  @see-class{gdk-frame-clock}"
  (with-foreign-objects ((refresh-interval :int64)
                         (presentation-time-return :int64))
    (%gdk-frame-clock-refresh-info frame-clock
                                   base-time
                                   refresh-interval
                                   presentation-time-return)
    (values (mem-ref refresh-interval :int64)
            (mem-ref presentation-time-return :int64))))

(export 'gdk-frame-clock-refresh-info)

;;; --- End of file gdk.frame-clock.lisp ---------------------------------------
