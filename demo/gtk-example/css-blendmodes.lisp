;;;; Theming/CSS Blend Modes - 2021-11-27
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
      (gtk-style-context-add-provider-for-screen
                                      (gdk-screen-default)
                                      provider
                                      +gtk-style-provider-priority-application+)
      ;; Signal handler for listbox
      (g-signal-connect listbox "row-activated"
          (lambda (listbox row)
            (declare (ignore listbox))
            (let* ((mode (second (elt +blend-modes+
                                      (gtk-list-box-row-index row))))
                   (str (format nil
                                (read-file (sys-path "css-blendmodes.css"))
                        mode mode mode)))
              (gtk-css-provider-load-from-data provider str))))
      ;; Fill the list box
      (let ((normal nil))
        (dolist (mode +blend-modes+)
          (let ((row (make-instance 'gtk-list-box-row)))
             (gtk-container-add row
                                (make-instance 'gtk-label
                                               :label (first mode)
                                               :xalign 0.0d0))
             (gtk-container-add listbox row)
             (when (string= "normal" (second mode))
               (setq normal row))))
        ;; Select the "normal" row
        (gtk-list-box-select-row listbox normal)
        (g-signal-emit normal "activate")
        (gtk-widget-grab-focus normal))
      ;; Add listbox to scrolled window
      (gtk-container-add (gtk-builder-object builder "scrolledwindow") listbox)
      ;; Show the window
      (gtk-widget-show-all window))))
