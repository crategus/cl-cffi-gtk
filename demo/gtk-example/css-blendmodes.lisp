;;;; Theming/CSS Blend Modes - 2021-11-5
;;;;
;;;; You can blend multiple backgrounds using the CSS blend modes available.

(in-package #:gtk-example)

(defparameter +blend-modes+ '(("Color"         "color")
                              ("Color (burn)"  "color-burn")
                              ("Color (dodge)" "color-dodge")
                              ("Darken"        "darken")
                              ("Difference"    "difference")
                              ("Exclusion"     "exclusion")
                              ("Hard Light"    "hard-light")
                              ("Hue"           "hue")
                              ("Lighten"       "lighten")
                              ("Luminosity"    "luminosity")
                              ("Multiply"      "multiply")
                              ("Normal"        "normal")
                              ("Overlay"       "overlay")
                              ("Saturate"      "saturate")
                              ("Screen"        "screen")
                              ("Soft Light"    "soft-light")))

(defun update-css-for-blend-mode (provider blend-mode)
  (let ((str (read-file (sys-path "css-blendmodes.css"))))
    (setq str (format nil str blend-mode blend-mode blend-mode))
    (print str)
    (gtk-css-provider-load-from-data provider str)))

(defun example-css-blendmodes (&optional application)
  (within-main-loop
    (let* ((builder (gtk-builder-new-from-file (sys-path "css-blendmodes.ui")))
           (provider (make-instance 'gtk-css-provider))
           (listbox (make-instance 'gtk-list-box))
           (window (gtk-builder-object builder "window")))
      (setf (gtk-window-application window) application)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Setup the CSS provider for window
      (gtk-style-context-add-provider-for-screen (gdk-screen-default)
                                                 provider
                                                 +gtk-style-provider-priority-application+)
      ;; Signal handler for listbox
      (g-signal-connect listbox "row-activated"
                        (lambda (listbox row)
                          (declare (ignore listbox))
                          (let ((blend-mode (second (elt +blend-modes+
                                                         (gtk-list-box-row-index row)))))
                            (update-css-for-blend-mode provider blend-mode))))
      ;; Fill the list box
      (let ((normal-row nil))
        (dolist (blend-modes +blend-modes+)
          (let ((row (make-instance 'gtk-list-box-row)))
             (gtk-container-add row
                                (make-instance 'gtk-label
                                               :label (first blend-modes)
                                               :xalign 0.0d0))
             (gtk-container-add listbox row)
             (when (string= "normal" (second blend-modes))
               (setq normal-row row))))
        ;; Select the "normal" row
        (gtk-list-box-select-row listbox normal-row)
        (g-signal-emit normal-row "activate")
        (gtk-widget-grab-focus normal-row))
      ;; Add listbox to scrolled window
      (gtk-container-add (gtk-builder-object builder "scrolledwindow") listbox)
      ;; Show the window
      (gtk-widget-show-all window))))
