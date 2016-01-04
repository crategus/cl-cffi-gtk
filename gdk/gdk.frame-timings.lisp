;;; ----------------------------------------------------------------------------
;;; gdk.frame-timings.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2016 Dieter Kaiser
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
;;; Frame timings
;;;
;;; Object holding timing information for a single frame
;;;
;;; Types and Values
;;;
;;;     GdkFrameTimings
;;;
;;; Functions
;;;
;;;     gdk_frame_timings_ref
;;;     gdk_frame_timings_unref
;;;     gdk_frame_timings_get_frame_counter
;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkFrameTimings
;;; ----------------------------------------------------------------------------

#+gdk-3-8
(define-g-boxed-opaque gdk-frame-timings "GdkFrameTimings"
  :alloc (error "GdkFrameTimings can not be created from Lisp side."))

#+(and gdk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gdk-frame-timings atdoc:*class-name-alias*) "CStruct"
      (documentation 'gdk-frame-timings 'type)
 "@version{2016-1-3}
  @begin{short}
    A @sym{gdk-frame-timings} object holds timing information for a single frame
    of the application’s displays.
  @end{short}
  To retrieve @sym{gdk-frame-timings} objects, use the functions
  @fun{gdk-frame-clock-get-timings} or
  @fun{gdk-frame-clock-get-current-timings}. The information in
  @sym{gdk-frame-timings} is useful for precise synchronization of video with
  the event or audio streams, and for measuring quality metrics for the
  application’s display, such as latency and jitter.
  @begin{pre}
(define-g-boxed-opaque gdk-frame-timings \"GdkFrameTimings\"
  :alloc (error \"GdkFrameTimings can not be created from Lisp side.\"))
  @end{pre}
  Since 3.8
  @see-class{gdk-frame-clock}
  @see-function{gdk-frame-clock-get-timings}
  @see-function{gdk-frame-clock-get-current-timings}")

#+gdk-3-8
(export (boxed-related-symbols 'gdk-frame-timings))

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_ref ()
;;;
;;; GdkFrameTimings *
;;; gdk_frame_timings_ref (GdkFrameTimings *timings);
;;;
;;; Increases the reference count of timings .
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     timings
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_unref ()
;;;
;;; void
;;; gdk_frame_timings_unref (GdkFrameTimings *timings);
;;;
;;; Decreases the reference count of timings . If timings is no longer
;;; referenced, it will be freed.
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_frame_counter ()
;;;
;;; gint64
;;; gdk_frame_timings_get_frame_counter (GdkFrameTimings *timings);
;;;
;;; Gets the frame counter value of the GdkFrameClock when this this frame was
;;; drawn.
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     the frame counter value for this frame
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_complete ()
;;;
;;; gboolean
;;; gdk_frame_timings_get_complete (GdkFrameTimings *timings);
;;;
;;; The timing information in a GdkFrameTimings is filled in incrementally as
;;; the frame as drawn and passed off to the window system for processing and
;;; display to the user. The accessor functions for GdkFrameTimings can return 0
;;; to indicate an unavailable value for two reasons: either because the
;;; information is not yet available, or because it isn't available at all. Once
;;; gdk_frame_timings_get_complete() returns TRUE for a frame, you can be
;;; certain that no further values will become available and be stored in the
;;; GdkFrameTimings.
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     TRUE if all information that will be available for the frame has been
;;;     filled in.
;;;
;;; Since: 3.8
;;;-----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_frame_time ()
;;;
;;; gint64
;;; gdk_frame_timings_get_frame_time (GdkFrameTimings *timings);
;;;
;;; Returns the frame time for the frame. This is the time value that is
;;; typically used to time animations for the frame. See
;;; gdk_frame_clock_get_frame_time().
;;;
;;; Parameters
;;;
;;; timings
;;;     A GdkFrameTimings
;;;
;;; Returns
;;;     the frame time for the frame, in the timescale of g_get_monotonic_time()
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_presentation_time ()
;;;
;;; gint64
;;; gdk_frame_timings_get_presentation_time (GdkFrameTimings *timings);
;;;
;;; Returns the presentation time. This is the time at which the frame became
;;; visible to the user.
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     the time the frame was displayed to the user, in the timescale of
;;;     g_get_monotonic_time(), or 0 if no presentation time is available. See
;;;     gdk_frame_timings_get_complete()
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_refresh_interval ()
;;;
;;; gint64
;;; gdk_frame_timings_get_refresh_interval (GdkFrameTimings *timings);
;;;
;;; Gets the natural interval between presentation times for the display that
;;; this frame was displayed on. Frame presentation usually happens during the
;;; "vertical blanking interval".
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     the refresh interval of the display, in microseconds, or 0 if the
;;;     refresh interval is not available. See gdk_frame_timings_get_complete().
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_frame_timings_get_predicted_presentation_time ()
;;;
;;; gint64
;;; gdk_frame_timings_get_predicted_presentation_time (GdkFrameTimings *timings)
;;;
;;; Gets the predicted time at which this frame will be displayed. Although no
;;; predicted time may be available, if one is available, it will be available
;;; while the frame is being generated, in contrast to
;;; gdk_frame_timings_get_presentation_time(), which is only available after the
;;; frame has been presented. In general, if you are simply animating, you
;;; should use gdk_frame_clock_get_frame_time() rather than this function, but
;;; this function is useful for applications that want exact control over
;;; latency. For example, a movie player may want this information for
;;; Audio/Video synchronization.
;;;
;;; Parameters
;;;
;;; timings
;;;     a GdkFrameTimings
;;;
;;; Returns
;;;     The predicted time at which the frame will be presented, in the
;;;     timescale of g_get_monotonic_time(), or 0 if no predicted presentation
;;;     time is available.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.frame-timings.lisp -------------------------------------
