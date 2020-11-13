(def-suite gdk-frame-clock :in gdk-suite)
(in-suite gdk-frame-clock)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameClockPhase



;;;     GdkFrameClock

;;; --- Signals ----------------------------------------------------------------

;;;     void  after-paint      Run Last
;;;     void  before-paint     Run Last
;;;     void  flush-events     Run Last
;;;     void  layout           Run Last
;;;     void  paint            Run Last
;;;     void  resume-events    Run Last
;;;     void  update           Run Last

;;; --- Functions --------------------------------------------------------------

;;;     gdk-frame-clock-frame-time

(test gdk-frame-clock-frame-time
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))
      (is (typep frame-clock 'gdk-frame-clock))
      (is (integerp (gdk-frame-clock-frame-time frame-clock))))))

;;;     gdk-frame-clock-request-phase

(test gdk-frame-clock-request-phase
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))

      (is-false (gdk-frame-clock-request-phase frame-clock :update))

)))

;;;     gdk-frame-clock-begin-updating
;;;     gdk-frame-clock-end-updating

(test gdk-frame-clock-updating
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))

      (is-false (gdk-frame-clock-begin-updating frame-clock))
      (is-false (gdk-frame-clock-end-updating frame-clock))

)))

;;;     gdk-frame-clock-frame-counter

(test gdk-frame-clock-frame-counter
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))

      (is (integerp (gdk-frame-clock-frame-counter frame-clock)))

)))

;;;     gdk-frame-clock-history-start

(test gdk-frame-clock-frame-counter
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))

      (is (integerp (gdk-frame-clock-history-start frame-clock)))

)))

;;;     gdk-frame-clock-timings

(test gdk-frame-clock-timings
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))

      (is (typep (gdk-frame-clock-timings frame-clock 0) 'gdk-frame-timings))

)))

;;;     gdk-frame-clock-current-timings

(test gdk-frame-clock-current-timings
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))
      (is (typep frame-clock 'gdk-frame-clock))

      (is (typep (gdk-frame-clock-current-timings frame-clock) 'gdk-frame-timings))

)))

;;;     gdk-frame-clock-refresh-info

(test gdk-frame-clock-refresh-info
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (is-false (gtk-widget-realize window))
    (let ((frame-clock (gtk-widget-frame-clock window)))
      (is (typep frame-clock 'gdk-frame-clock))

      (is (every #'integerp
                 (multiple-value-list
                   (gdk-frame-clock-refresh-info frame-clock 0)))))))

;;; 2020-11-11
