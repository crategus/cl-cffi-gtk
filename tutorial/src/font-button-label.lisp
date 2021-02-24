;;;; Font Button Label

(in-package :gtk-tutorial)

(defun font-filter (family face)
  (declare (ignore face))
  (member (pango-font-family-name family)
          '("Purisa" "Sans" "Serif" "Times New Roman")
          :test #'equal))

(defun example-font-button-label ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Font Chooser Button"
                                 :type :toplevel
                                 :border-width 18
                                 :default-width 300
                                 :default-height 100))
          (provider (gtk-css-provider-new))
          (box (make-instance 'gtk-box
                              :orientation :horizontal
                              :spacing 12))
          (label (make-instance 'gtk-label
                                :label "Font Button"
                                :use-markup t))
          (button (make-instance 'gtk-font-button)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect button "font-set"
         (lambda (chooser)
           (let* (;; Get the font description
                  (desc (gtk-font-chooser-font-desc chooser))
                  ;; Get font informations from the font description
                  (family (pango-font-description-family desc))
                  (size (pango-pixels (pango-font-description-size desc)))
                  (style (pango-font-description-style desc))
                  (weight (pango-font-description-weight desc))
                  (variant (pango-font-description-variant desc))
                  (stretch (pango-font-description-stretch desc))
                  ;; Write the CSS string
                  (css-label (format nil "label { font-family : ~a; ~
                                                  font-weight : ~a; ~
                                                  font-style : ~a; ~
                                                  font-variant : ~a; ~
                                                  font-stretch : ~a; ~
                                                  font-size : ~apx; }"
                                         family
                                         weight
                                         style
                                         variant
                                         stretch
                                         size))
                  ;; Get the style of the label
                  (context (gtk-widget-style-context label)))
             ;; Update the font of the label
             (gtk-css-provider-load-from-data provider css-label)
             (gtk-style-context-add-provider context
                                             provider
                                             +gtk-style-provider-priority-user+))))
      ;; Set a filter function to select fonts for the font chooser
      (gtk-font-chooser-set-filter-func button #'font-filter)
      ;; Pack the widgets
      (gtk-box-pack-start box button)
      (gtk-box-pack-start box label)
      (gtk-container-add window box)
      ;; Show the widgets
      (gtk-widget-show-all window))))
