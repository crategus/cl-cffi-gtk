;;;; Example Clipboard - 2021-11-19

(in-package :gtk-example)

(defun button-press-event-handler (image event num)

  (format t "BUTTON-PRESS-EVENT-HANDLER~%")
  (format t "  button : ~a~%" (gdk-event-button-button event))

  (unless (= 1 (gdk-event-button-button event))
    (let ((menu (gtk-menu-new)))

      (let ((item (gtk-menu-item-new-with-mnemonic "_Copy")))
        (gtk-menu-shell-append menu item)
        (g-signal-connect item "activate"
                          (lambda (widget)
                            (format t "COPY activated ~a~%" widget)
                            (let ((pixbuf (get-pixbuf-from-image image))
                                  (clipboard (gtk-clipboard-get "CLIPBOARD")))
                              (gtk-clipboard-set-image clipboard pixbuf))))
        (when (= 2 num)
          (setf (gtk-widget-sensitive item) nil))
      )

      (let ((item (gtk-menu-item-new-with-mnemonic "_Paste")))
        (gtk-menu-shell-append menu item)
        (g-signal-connect item "activate"
                          (lambda (widget)
                            (format t "PASTE activated ~a~%" widget)
                            (let* ((clipboard (gtk-clipboard-get "CLIPBOARD"))
                                   (pixbuf (gtk-clipboard-wait-for-image clipboard)))
                              (when pixbuf
                                (gtk-image-set-from-pixbuf image pixbuf)))))
        (when (= 1 num)
          (setf (gtk-widget-sensitive item) nil))
      )

      (let ((item (gtk-menu-item-new-with-mnemonic "_Clear")))
        (gtk-menu-shell-append menu item)
        (g-signal-connect item "activate"
            (lambda (widget)
              (declare (ignore widget))
              (format t "CLEAR activated~%")
              (gtk-image-set-from-icon-name image "broken-image" :dialog)))
        (when (= 1 num)
          (setf (gtk-widget-sensitive item) nil)))

      (gtk-widget-show-all menu)
      (gtk-menu-popup-at-pointer menu event)
    )
))

(defun example-clipboard (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Clipboard"
                                 :application application))
          (vbox (make-instance 'gtk-box
                               :border-width 6
                               :orientation :vertical))

          (hbox1 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))
          (entry1 (make-instance 'gtk-entry))
          (button1 (make-instance 'gtk-button
                                  :label "Copy"))

          (hbox2 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))
          (entry2 (make-instance 'gtk-entry))
          (button2 (make-instance 'gtk-button
                                  :label "Paste"))

          (hbox3 (make-instance 'gtk-box
                                :spacing 6
                                :border-width 6
                                :orientation :horizontal))
          (selection nil))

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Copy button clicked
      (g-signal-connect button1 "clicked"
          (lambda (widget)
            (declare (ignore widget))
            (format t "Copy button clicked.~%")
            ;; Get the clipboard
            (let ((clipboard (gtk-widget-clipboard entry1 "CLIPBOARD")))
              ;; Set clipboard text
              (gtk-clipboard-set-text clipboard (gtk-entry-text entry1)))))

      ;; Paste button clicked
      (g-signal-connect button2 "clicked"
          (lambda (widget)
            (declare (ignore widget))
            (format t "Paste button clicked.~%")
            ;; Get the clipboard
            (let ((clipboard (gtk-widget-clipboard entry2 "CLIPBOARD")))
              ;; Request the contents of the clipboard, contents_received will
              ;; be called when we do get the contents.
              (gtk-clipboard-request-text clipboard
                                          ;;gtk-clipboard-text-received-func
                                          (lambda (clipboard text)
                                            (declare (ignore clipboard))
                                            (when text
                                              (setf (gtk-entry-text entry2)
                                                    text)))))))

      ;; Pack and show the widgets
      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :margin-top 6
                                         :label
                                         "<b>Copy text in Clipboard</b>"))

      (gtk-box-pack-start hbox1 entry1)
      (gtk-box-pack-start hbox1 button1)
      (gtk-box-pack-start vbox hbox1)

      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :margin-top 6
                                         :label
                                         "<b>Paste text from Clipboard</b>"))

      (gtk-box-pack-start hbox2 entry2)
      (gtk-box-pack-start hbox2 button2)
      (gtk-box-pack-start vbox hbox2)

      (gtk-box-pack-start vbox
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :xalign 0.0
                                         :margin-top 12
                                         :label
                                         "<b>Transfer image via Clipbord</b>"))

      ;; Create the first image
      (let ((image (gtk-image-new-from-icon-name "dialog-warning" :dialog))
            (ebox (make-instance 'gtk-event-box)))
        (gtk-container-add ebox image)
        (gtk-container-add hbox3 ebox)

        ;; make ebox a drag source
        (gtk-drag-source-set ebox :button1-mask nil :copy)
        (gtk-drag-source-add-image-targets ebox)

        (g-signal-connect ebox "drag-begin"
            (lambda (widget context)
              (declare (ignore widget))
              (format t "in DRAG-BEGIN~%")
              (let ((pixbuf (get-pixbuf-from-image image)))
                (gtk-drag-set-icon-pixbuf context pixbuf 0 0))
              nil))

        (g-signal-connect ebox "drag-data-get"
            (lambda (widget context data info time)
              (declare (ignore widget context info time))
              (format t "in DRAG-DATA-GET~%")
              (let ((pixbuf (get-pixbuf-from-image image)))
                (when (setf (gtk-selection-data-pixbuf data) pixbuf)
                  ;; Workaround: Save data in global selection
                  (setf selection (gtk-selection-data-copy data))))
              nil))

        ;; Context menu on ebox
        (g-signal-connect ebox "button-press-event"
                               (lambda (widget event)
                                 (declare (ignore widget))
                                 (button-press-event-handler image event 1))))

      ;; Create the second image
      (let ((image (gtk-image-new-from-icon-name "broken-image" :dialog))
            (ebox (make-instance 'gtk-event-box)))
        (gtk-container-add ebox image)
        (gtk-container-add hbox3 ebox)

        ;; Accept drops on ebox
        (gtk-widget-add-events ebox :all-events-mask)
        (gtk-drag-dest-set ebox '(:motion :highlight) nil :copy)
        (gtk-drag-dest-add-image-targets ebox)

        (g-signal-connect ebox "drag-drop"
           (lambda (widget context x y time)
             (declare (ignore x y))
             (format t "in DRAG-DROP~%")
             (gtk-drag-data widget context "image/png" time)
             ;; Return true for successful drop
             t))

        (g-signal-connect ebox "drag-data-received"
            (lambda (widget context x y data info time)
              (declare (ignore widget x y info))
              (format t "in DRAG-DATA-RECEIVED~%")
              ;; Workaround: Copy data from global selection
              (setf data (gtk-selection-data-copy selection))
              (let ((pixbuf (gtk-selection-data-pixbuf data)))
                (when pixbuf
                  (gtk-image-set-from-pixbuf image pixbuf))
                (gtk-drag-finish context nil nil time))))

        ;; Context menu on ebox
        (g-signal-connect ebox "button-press-event"
                               (lambda (widget event)
                                 (declare (ignore widget))
                                 (button-press-event-handler image event 2))))

      (gtk-box-pack-start vbox hbox3)
      (gtk-container-add window vbox)

      (gtk-widget-show-all window))))
