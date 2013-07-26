
;; We need GTK+ to have a button widget.
(asdf:load-system :cl-cffi-gtk)

(def-suite gobject-signals :in gobject-suite)
(in-suite gobject-signals)

(defvar *message* nil)

(test gobject-signals
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         (setf *message* "Signal 'clicked' for button")
                         t))))
    ;; list-signals
    (is (equal '(170 165 166 167 168 169)
               (mapcar #'signal-info-id (list-signals "GtkButton"))))
    (is (equal '("activate" "pressed" "released" "clicked" "enter" "leave")
               (mapcar #'signal-info-name (list-signals "GtkButton"))))
    (is (equal '((:RUN-FIRST :ACTION)
                 (:RUN-FIRST)
                 (:RUN-FIRST)
                 (:RUN-FIRST :ACTION)
                 (:RUN-FIRST)
                 (:RUN-FIRST))
               (mapcar #'signal-info-flags (list-signals "GtkButton"))))
    (is (equal '(nil nil nil nil nil nil)
               (mapcar #'signal-info-param-types (list-signals "GtkButton"))))
    (is (equal '(nil nil nil nil nil nil)
               (mapcar #'signal-info-detail (list-signals "GtkButton"))))

    ;; g-signal-query
    (let ((query (g-signal-query 165)))
      (is (= 165 (signal-info-id query)))
      (is (equal "pressed" (signal-info-name query)))
      (is (equal "GtkButton" (g-type-name (signal-info-owner-type query))))
      (is (equal '(:run-first) (signal-info-flags query)))
      (is (equal "void" (g-type-name (signal-info-return-type query))))
      (is-false (signal-info-param-types query))
      (is-false (signal-info-detail query)))

    ;; g-signal-lookup
    (is (= 165 (g-signal-lookup "pressed" "GtkButton")))
    (is (= 166 (g-signal-lookup "released" "GtkButton")))
    (is (= 167 (g-signal-lookup "clicked" "GtkButton")))
    (is (= 168 (g-signal-lookup "enter" "GtkButton")))
    (is (= 169 (g-signal-lookup "leave" "GtkButton")))
    (is (= 170 (g-signal-lookup "activate" "GtkButton")))

    ;; g-signal-name
    (is (equal "pressed" (g-signal-name 165)))
    (is (equal "released" (g-signal-name 166)))
    (is (equal "clicked" (g-signal-name 167)))
    (is (equal "enter" (g-signal-name 168)))
    (is (equal "leave" (g-signal-name 169)))
    (is (equal "activate" (g-signal-name 170)))

    ;; g-signal-list-ids
    (is-false (g-signal-list-ids "gboolean"))
    (is (equal '(1) (g-signal-list-ids "GObject")))
    (is (equal '(170 165 166 167 168 169) (g-signal-list-ids "GtkButton")))

    ;; g-signal-emit
    ;; The signal handler writes a message in the global variable *message*.
    ;; We emit the signal and check the value of *message*.
    (is-false (setf *message* nil))
    (g-signal-emit button "clicked")
    (is (equal "Signal 'clicked' for button" *message*))

    ;; g-signal-handler-is-connected
    (is-true (g-signal-handler-is-connected button handler-id))

    ;; g-signal-has-handler-pending
    (is-true (g-signal-has-handler-pending button 167 (null-pointer) t))
    (is-true (g-signal-has-handler-pending button 167 (null-pointer) nil))

    ;; Block and unblock a signal handler
    (g-signal-handler-block button handler-id)
    (is-true (g-signal-has-handler-pending button 167 (null-pointer) t))
    (is-false (g-signal-has-handler-pending button 167 (null-pointer) nil))
    (g-signal-handler-unblock button handler-id)
    (is-true (g-signal-has-handler-pending button 167 (null-pointer) t))
    (is-true (g-signal-has-handler-pending button 167 (null-pointer) nil))

    ;; g-signal-handler-find
    (is (= handler-id (g-signal-handler-find button signal-id)))

;    (assert-true (g-object-signal-handlers button))


    ;; Get complete signal info for a signal
;    (assert-false (g-signal-parse-name "GtkButton" "clicked"))
;    (let ((info (g-signal-parse-name "GtkButton" "clicked")))
;      (assert-false (signal-info-id info))
;      (assert-false (signal-info-name info))
;      (assert-false (signal-info-owner-type info))
;      (assert-false (signal-info-flags info))
;      (assert-false (signal-info-return-type info))
;      (assert-false (signal-info-param-types info))
;      (assert-false (signal-info-detail info)))
))

