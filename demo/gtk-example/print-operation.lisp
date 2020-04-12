(in-package :gtk-example)

(defun begin-print (operation context)
  (declare (ignore context))
  (format t "in signal begin-print~%")
  (setf (gtk-print-operation-n-pages operation) 1))

(defvar *header-height* (floor (/ (* 20 72) 25.4)))
(defvar *header-gap*    (floor (/ (* 3 72) 25.4)))

(defun draw-page (operation context page-nr)
  (declare (ignore operation page-nr))
  (let ((text-height 0)
        (cr (gtk-print-context-get-cairo-context context))
        (width (floor (gtk-print-context-width context)))
        (layout (gtk-print-context-create-pango-layout context)))
    ;; Print a grey colored header
    (cairo-rectangle cr 0 0 width *header-height*)
    (cairo-set-source-rgb cr 0.9 0.9 0.9)
    (cairo-fill cr)
    ;; Set the font and text to print
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string "sans 14"))
    (setf (pango-layout-text layout) "Title")
    (setf (pango-layout-width layout) (* width +pango-scale+))
    (setf (pango-layout-alignment layout) :center)
    ;; Get the height of the text
    (multiple-value-bind (width height)
        (pango-layout-get-size layout)
      (declare (ignore width))
      (setf text-height (/ height +pango-scale+)))
    ;; Set color to black and center the text in header
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-move-to cr 0 (floor (/ (- *header-height* text-height) 2)))
    (pango-cairo-show-layout cr layout)))

(defun draw-page-2 (operation context page-nr)
  (declare (ignore operation page-nr))
  (let ((cr (gtk-print-context-get-cairo-context context))
        (layout (gtk-print-context-create-pango-layout context)))

    ;; Draw a red rectangle, as wide as the paper (inside the margins)
    (cairo-set-source-rgb cr 1.0 0 0)
    (cairo-rectangle cr 0 0 (gtk-print-context-width context) 50)
    (cairo-fill cr)

    ;; Draw some lines
    (cairo-move-to cr 20 10)
    (cairo-line-to cr 40 20)
    (cairo-arc cr 60 60 20 0 3.14)
    (cairo-line-to cr 80 20)

    (cairo-set-source-rgb cr 0 0 0)
    (cairo-set-line-width cr 5)
    (cairo-set-line-cap cr :round)
    (cairo-set-line-join cr :round)

    (cairo-stroke cr)

    ;; Draw some text
    (setf (pango-layout-text layout) "Hello World! Printing is easy")
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string "sans 28"))
    (cairo-move-to cr 30 20)
    (pango-cairo-layout-path cr layout)

    ;; Font Outline
    (cairo-set-source-rgb cr 0.93 1.0 0.47)
    (cairo-set-line-width cr 0.5)
    (cairo-stroke-preserve cr)

    ;; Font Fill
    (cairo-set-source-rgb cr 0 0.0 1.0)
    (cairo-fill cr)))

(defvar *print-settings* nil)

(defun do-print-operation (window)
  (let ((response nil)
        (print (gtk-print-operation-new)))
    ;; Connect signal handlers for the print operation
    (g-signal-connect print "draw-page" #'draw-page-2)
    (g-signal-connect print "begin-print" #'begin-print)
    ;; Restore the print settings
    (when *print-settings*
      (setf (gtk-print-operation-print-settings print) *print-settings*))
    ;; Perform the print operation
    (setf response (gtk-print-operation-run print :print-dialog window))
    ;; Check the response and save the print settings
    (when (eq :apply response)
      (setf *print-settings* (gtk-print-operation-print-settings print)))))

(defun example-print-operation ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel
                                             :default-width 400
                                             :default-height 320
                                             :border-width 24))
          (button (make-instance 'gtk-button :label "Do Print")))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (do-print-operation window)))

      (gtk-container-add window button)
      (gtk-widget-show-all window))))

(defun example-print-run-page-setup-dialog ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window :type :toplevel
                                             :default-width 400
                                             :default-height 320
                                             :border-width 24))
          (button (make-instance 'gtk-button :label "Do Print"))
          (page-setup (make-instance 'gtk-page-setup))
          (settings (make-instance 'gtk-print-settings)))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (setf page-setup
                                (gtk-print-run-page-setup-dialog window
                                                                 page-setup
                                                                 settings))))

      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;; TODO: gtk-print-run-page-setup-dialog-async does not work.

;(defun done-cb (page-setup)
;  (format t "done-cb called : ~A~%" page-setup))

;(defun example-print-run-page-setup-dialog-async ()
;  (within-main-loop
;    (let ((window (make-instance 'gtk-window :type :toplevel
;                                             :default-width 400
;                                             :default-height 320
;                                             :border-width 24))
;          (button (make-instance 'gtk-button :label "Do Print"))
;          (page-setup (make-instance 'gtk-page-setup))
;          (settings (make-instance 'gtk-print-settings)))

;      (g-signal-connect window "destroy"
;                        (lambda (widget)
;                          (declare (ignore widget))
;                          (leave-gtk-main)))

;      (g-signal-connect button "clicked"
;                        (lambda (widget)
;                          (declare (ignore widget))
;                          (setf page-setup
;                                (gtk-print-run-page-setup-dialog-async window
;                                                                       page-setup
;                                                                       settings
;                                                                       #'done-cb))))

;      (gtk-container-add window button)
;      (gtk-widget-show-all window))))

