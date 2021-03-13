;;;; Combo Box Text

(in-package :gtk-example)

(defun example-combo-box-text ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :border-width 12
                                 :title "Example Combo Box Text"))
          (vbox1 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (vbox2 (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 6))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 24))
          (label (make-instance 'gtk-label :label "Label"))
          (combo (make-instance 'gtk-combo-box-text
                                :has-entry t)))
      ;; Setup the combo box
      (gtk-combo-box-text-append-text combo "First entry")
      (gtk-combo-box-text-append-text combo "Second entry")
      (gtk-combo-box-text-append-text combo "Third entry")
      ;; Combo box selection has changed
      (g-signal-connect combo "changed"
          (lambda (object)
            (let ((value (gtk-combo-box-text-active-text object)))
              (gtk-label-set-markup label
                                    (format nil "<tt>~a</tt>" value)))))
      ;; Select the first entry of the combo box
      (setf (gtk-combo-box-active combo) 0)
      ;; Setup the entry for the combo box
      (let ((entry (gtk-bin-child combo)))
        (setf (gtk-entry-primary-icon-name entry) "list-add")
        (setf (gtk-entry-primary-icon-tooltip-text entry) "Add to Combo Box")
        (setf (gtk-entry-secondary-icon-name entry) "list-remove")
        (setf (gtk-entry-secondary-icon-tooltip-text entry)
              "Remove from Combo Box")
        ;; Toggle the primary and secondary icons of the entry
        (g-signal-connect entry "focus-in-event"
            (lambda (widget event)
              (declare (ignore event))
              (setf (gtk-entry-primary-icon-sensitive widget) t)
              (setf (gtk-entry-secondary-icon-sensitive widget) nil)))
        (g-signal-connect entry "focus-out-event"
            (lambda (widget event)
              (declare (ignore event))
              (setf (gtk-entry-primary-icon-sensitive widget) nil)
              (setf (gtk-entry-secondary-icon-sensitive widget) t)))
        ;; One of the icons of the entry has been pressed
        (g-signal-connect entry "icon-press"
            (lambda (object pos event)
              (declare (ignore event))
              (if (eq :primary pos)
                  (let ((text (gtk-entry-text object)))
                    (gtk-combo-box-text-append-text combo text))
                  (let ((active (gtk-combo-box-active combo)))
                    (gtk-combo-box-text-remove combo active)
                    (setf (gtk-combo-box-active combo) active))))))
      ;; Pack and show widgets
      (gtk-box-pack-start vbox1 (make-instance 'gtk-label
                                               :xalign 0
                                               :use-markup t
                                               :label "<b>Select item</b>")
                                :expand nil)
      (gtk-box-pack-start vbox1 combo)
      (gtk-box-pack-start hbox vbox1)
      (gtk-box-pack-start vbox2 (make-instance 'gtk-label
                                               :xalign 0
                                               :use-markup t
                                               :label "<b>Activated item</b>")
                                 :expand nil)
      (gtk-box-pack-start vbox2 label)
      (gtk-box-pack-start hbox vbox2)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
