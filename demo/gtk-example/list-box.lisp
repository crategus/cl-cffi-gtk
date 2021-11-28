;;;; List Box
;;;;
;;;; GtkListBox allows lists with complicated layouts, using
;;;; regular widgets supporting sorting and filtering.

(in-package :gtk-example)

(defclass gtk-message (g-object)
  ((id :initform 0
       :accessor gtk-message-id)
   (name :initform ""
         :accessor gtk-message-name)
   (nick :initform ""
         :accessor gtk-message-nick)
   (message :initform ""
            :accessor gtk-message-message)
   (time :initform 0
         :accessor gtk-message-time)
   (reply :initform 0
          :accessor gtk-message-reply)
   (resent :initform ""
           :accessor gtk-message-resent)
   (n-favorites :initform 0
                :accessor gtk-message-n-favorites)
   (n-reshares :initform 0
               :accessor gtk-message-n-reshares))
  (:g-type-name . "GtkMessage")
  (:metaclass gobject-class))

(register-object-type-implementation "GtkMessage"            ; name
                                     gtk-message             ; class
                                     "GObject"               ; parent
                                     nil                     ; interfaces
                                     nil)                    ; properties

(defun gtk-message-new (str)
  (let ((msg (make-instance 'gtk-message))
        (entries (split-sequence:split-sequence #\| str)))

    (format t "~&~%~a~%" entries)

    (setf (gtk-message-id msg) (parse-integer (pop entries) :junk-allowed t))
    (setf (gtk-message-name msg) (pop entries))
    (setf (gtk-message-nick msg) (pop entries))
    (setf (gtk-message-message msg) (pop entries))
    (setf (gtk-message-time msg) (parse-integer (pop entries)))

    (when entries
      (setf (gtk-message-reply msg) (parse-integer (pop entries))))
    (when entries
      (setf (gtk-message-resent msg) (pop entries)))
    (when entries
      (setf (gtk-message-n-favorites msg) (parse-integer (pop entries))))
    (when entries
      (setf (gtk-message-n-reshares msg) (parse-integer (pop entries))))

    (format t "~a~%" (gtk-message-id msg))
    (format t "~a~%" (gtk-message-name msg))
    (format t "~a~%" (gtk-message-nick msg))
    (format t "~a~%" (gtk-message-message msg))
    (format t "~a~%" (gtk-message-time msg))
    (format t "~a~%" (gtk-message-reply msg))
    (format t "~a~%" (gtk-message-resent msg))
    (format t "~a~%" (gtk-message-n-favorites msg))
    (format t "~a~%" (gtk-message-n-reshares msg))

    msg
))

(defclass gtk-message-row (gtk-list-box-row)
  ((message :initarg :message
            :accessor gtk-message-row-message)
   (details-revealer :accessor gtk-message-row-details-revealer)
   (avatar-image :accessor gtk-message-row-avatar-image)
   (extra-buttons-box :accessor gtk-message-row-extra-buttons-box)
   (content-label :accessor gtk-message-row-content-label)
   (source-name :accessor gtk-message-row-source-name)
   (source-nick :accessor gtk-message-row-source-nick)
   (short-time-label :accessor gtk-message-row-short-time-label)
   (detailed-time-label :accessor gtk-message-row-detailed-time-label)
   (resent-box :accessor gtk-message-row-resent-box)
   (resent-by-button :accessor gtk-message-row-resent-by-button)
   (n-favorites-label :accessor gtk-message-row-n-favorites-label)
   (n-reshares-label :accessor gtk-message-row-n-reshares-label)
   (expand-button :accessor gtk-message-row-expand-button))
  (:g-type-name . "GtkMessageRow")
  (:metaclass gobject-class))

(register-object-type-implementation "GtkMessageRow"         ; name
                                     gtk-message-row         ; class
                                     "GtkListBoxRow"         ; parent
                                     nil                     ; interfaces
                                     nil)                    ; properties

(defun gtk-message-row-expand (row)
  (let* ((revealer (gtk-message-row-details-revealer row))
         (expand (not (gtk-revealer-reveal-child revealer)))
         (button (gtk-message-row-expand-button row)))
    (setf (gtk-revealer-reveal-child revealer) expand)
    (if expand
        (setf (gtk-button-label button) "Hide")
        (setf (gtk-button-label button) "Expand"))))

(defun gtk-message-row-update (row)
  (let ((message (gtk-message-row-message row)))

    (format t "~&in GTK-MESSAGE-ROW-UPDATE ~a~%" message)
    (format t "  details-revealer : ~a~%" (gtk-message-row-details-revealer row))
    (format t "      avatar-image : ~a~%" (gtk-message-row-avatar-image row))
    (format t " extra-buttons-box : ~a~%" (gtk-message-row-extra-buttons-box row))

    (gtk-image-set-from-file (gtk-message-row-avatar-image row)
                             (sys-path "apple-red.png"))

    (setf (gtk-label-label (gtk-message-row-source-name row))
          (gtk-message-name message))
    (setf (gtk-label-label (gtk-message-row-source-nick row))
          (gtk-message-nick message))
    (setf (gtk-label-label (gtk-message-row-content-label row))
          (gtk-message-message message))

    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time (gtk-message-time message))
      (setf (gtk-label-label (gtk-message-row-short-time-label row))
            (format nil "~a.~a.~a" date month (+ year 70)))
      (setf (gtk-label-label (gtk-message-row-detailed-time-label row))
            (format nil "~a:~a:~a - ~a.~a,~a"
                        hour minute second date month year)))

    (let ((n-favorites-label (gtk-message-row-n-favorites-label row))
          (n-favorites (gtk-message-n-favorites message)))
      (setf (gtk-widget-visible n-favorites-label) (not (= 0 n-favorites)))
      (gtk-label-set-markup n-favorites-label (format nil "~a" n-favorites)))

    (let ((n-reshares-label (gtk-message-row-n-reshares-label row))
          (n-reshares (gtk-message-n-reshares message)))
      (gtk-label-set-markup n-reshares-label (format nil "~a" n-reshares)))

))

(defun gtk-message-row-new (message)
  (let* ((builder (gtk-builder-new-from-file (sys-path "list-box.ui")))
         (row (gtk-builder-object builder "messagerow")))

    (setf (gtk-message-row-message row) message)

    (setf (gtk-message-row-details-revealer row)
          (gtk-builder-object builder "details_revealer"))
    (setf (gtk-message-row-avatar-image row)
          (gtk-builder-object builder "avatar_image"))
    (setf (gtk-message-row-extra-buttons-box row)
          (gtk-builder-object builder "extra_buttons_box"))
    (setf (gtk-message-row-content-label row)
          (gtk-builder-object builder "content_label"))
    (setf (gtk-message-row-source-name row)
          (gtk-builder-object builder "source_name"))
    (setf (gtk-message-row-source-nick row)
          (gtk-builder-object builder "source_nick"))
    (setf (gtk-message-row-short-time-label row)
          (gtk-builder-object builder "short_time_label"))
    (setf (gtk-message-row-detailed-time-label row)
          (gtk-builder-object builder "detailed_time_label"))
    (setf (gtk-message-row-n-favorites-label row)
          (gtk-builder-object builder "n_favorites_label"))
    (setf (gtk-message-row-n-reshares-label row)
          (gtk-builder-object builder "n_reshares_label"))
    (setf (gtk-message-row-expand-button row)
          (gtk-builder-object builder "expand-button"))

    (gtk-message-row-update row)

    (g-signal-connect (gtk-builder-object builder "expand-button") "clicked"
        (lambda (button)
          (format t "in EXPAND_CLICKED: ~a~%" button)
          (gtk-message-row-expand row)))

    (g-signal-connect (gtk-builder-object builder "reshare-button") "clicked"
        (lambda (button)
          (let ((message (gtk-message-row-message row)))
            (incf (gtk-message-n-reshares message))
            (gtk-message-row-update row))))

    (g-signal-connect (gtk-builder-object builder "favorite-button") "clicked"
        (lambda (button)
          (let ((message (gtk-message-row-message row)))
            (incf (gtk-message-n-favorites message))
            (gtk-message-row-update row))))

    row
))

(defun example-list-box (&optional application)
  (within-main-loop
    (let ((avatar-pixbuf-other (gdk-pixbuf-new-from-file
                                 (sys-path "apple-red.png")))
          (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :application application
                                  :title "Example List Box"
                                  :default-width 600
                                  :default-height 400))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :spacing 12))
          (label (make-instance  'gtk-label
                                 :label "Messages from GTK and friends"))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :never
                                   :vscrollbar-policy :automatic))
          (listbox (make-instance 'gtk-list-box
                                  :activate-on-single-click nil))
         )

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-container-add window vbox)
      (gtk-box-pack-start vbox label :expand nil)
      (gtk-box-pack-start vbox scrolled)
      (gtk-container-add scrolled listbox)

;      (g-signal-connect listbox "row-activated"
;                        (lambda (listbox row)
;                          (declare (ignore listbox))
;                          (gtk-message-row-expand row)))

      (with-open-file (stream (sys-path "list-box-message.txt"))
        (do ((msg nil) (row nil)
             (line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
          (setf msg (gtk-message-new line))
          (setf row (gtk-message-row-new msg))

          (format t "~a~%" (gtk-label-label (gtk-message-row-source-name row)))
          (format t "~a~%" (gtk-label-label (gtk-message-row-source-nick row)))
          (format t "~a~%" (gtk-label-label (gtk-message-row-content-label row)))

          (gtk-widget-show-all row)
          (gtk-container-add listbox row)
          ))

      ;; Show the window.
      (gtk-widget-show-all window))))
