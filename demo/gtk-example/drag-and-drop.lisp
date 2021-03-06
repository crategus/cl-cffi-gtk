;;;; Drag and Drop - 2021-3-22

(in-package #:gtk-example)

;; Define a list of data types called "targets" that a destination widget will
;; accept. The string type is arbitrary, and negotiated between DnD widgets by
;; the developer. An enum or GQuark can serve as the integer target id.

(defvar target-int32 0)
(defvar target-string 1)
(defvar target-rootwin 2)

(defvar target-list
        (list (gtk-target-entry-new :target "INTEGER"
                                    :flags 0
                                    :info target-int32)
              (gtk-target-entry-new :target "STRING"
                                    :flags 0
                                    :info target-string)
              (gtk-target-entry-new :target "text/plain"
                                    :flags 0
                                    :info target-string)
              (gtk-target-entry-new :target "application/x-rootwindow-drop"
                                    :flags 0
                                    :info target-rootwin)))


(defun example-drag-and-drop ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Drag and Drop"
                                 :type :toplevel
                                 :default-width 450
                                 :default-height 50))
          (hbox (make-instance 'gtk-box
                               :orientation :horizontal
                               :spacing 6))
          (coin-source (make-instance 'gtk-button
                                      :label "[coins]"))
          (well-dest (make-instance 'gtk-label
                                    :label "[a well]"))
          (directions-label (make-instance 'gtk-label
                                           :label
                                           "drag a coin and drop it")))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Make the "well label" a DnD destination
      (gtk-drag-dest-set well-dest             ; widget that will accept a drop
                         '(::all) ; default actions for dest Dnd
                         target-list           ; list of targets to support
                         '(:copy))             ; what to with dropped data

      ;; Make the "coin-button" a DnD source
      (gtk-drag-source-set coin-source         ; widget will be dragable
                           '(:button1-mask)    ; modifier that will start drag
                           target-list         ; lists of targets to support
                           '(:copy))           ; what to do with dropped data

      ;; Connect all possible destination signals

      ;; Emitted when the user releases (drops) the selection. It should check
      ;; that the drop is over a valid part of the widget (if its a complex
      ;; widget), and itself to return true if the operation should continue.
      ;; Next choose the target type it wishes to ask the source for. Finally
      ;; call gtk_drag_get_data which will emit "drag-data-get" on the source.
      (g-signal-connect well-dest "drag-drop"
          (lambda (widget context x y time)
            (let ((targets (gdk-drag-context-list-targets context))
                  (is-valid-drop t))
              (declare (ignorable is-valid-drop))

              (format t "~%DRAG-DROP~%")
              (format t "   widget : ~a~%" widget)
              (format t "  context : ~a~%" context)
              (format t "     x, y : ~a, ~a~%" x y)
              (format t "     time : ~a~%" time)

              (format t "   actions : ~a~%" (gdk-drag-context-actions context))
              (format t " suggested : ~a~%" (gdk-drag-context-suggested-action context))
              (format t "  selected : ~a~%" (gdk-drag-context-selected-action context))
              (format t "   targets : ~a~%" (gdk-drag-context-list-targets context))
              (format t "    device : ~a~%" (gdk-drag-context-device context))
              (format t "    source : ~a~%" (gdk-drag-context-source-window context))
              (format t "      dest : ~a~%" (gdk-drag-context-dest-window context))
              (format t "  protocol : ~a~%" (gdk-drag-context-protocol context))

              (gtk-drag-data widget context "STRING" time)

              t)))

      (g-signal-connect well-dest "drag-data-received"
          (lambda (widget context x y selection info time)
            (format t "~%DRAG-DATA-RECEIVED~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)
            (format t "     x, y : ~a, ~a~%" x y)
            (format t "selection : ~a~%" selection)
            (format t "     info : ~a~%" info)
            (format t "     time : ~a~%" time)
      ))

      (g-signal-connect well-dest "drag-leave"
          (lambda (widget context time)
            (format t "~%DRAG-LEAVE~%")
            (format t "    widget : ~a~%" widget)
            (format t "   context : ~a~%" context)
            (format t "      time : ~a~%" time)

            (format t "   actions : ~a~%" (gdk-drag-context-actions context))
            (format t " suggested : ~a~%" (gdk-drag-context-suggested-action context))
            (format t "  selected : ~a~%" (gdk-drag-context-selected-action context))
            (format t "   targets : ~a~%" (gdk-drag-context-list-targets context))
            (format t "    device : ~a~%" (gdk-drag-context-device context))
            (format t "    source : ~a~%" (gdk-drag-context-source-window context))
            (format t "      dest : ~a~%" (gdk-drag-context-dest-window context))
            (format t "  protocol : ~a~%" (gdk-drag-context-protocol context))
      ))

      (g-signal-connect well-dest "drag-motion"
          (lambda (widget context x y time)
             (declare (ignore widget context x y time))
;            (format t "~%DRAG-MOTION~%")
;            (format t "    widget : ~a~%" widget)
;            (format t "   context : ~a~%" context)
;            (format t "      x, y : ~a~%" x y)
;            (format t "      time : ~a~%" time)
      ))

      ;; Connect all possible source signals

      ;; Emitted when DnD begins. This is often used to present custom graphics.
      (g-signal-connect coin-source "drag-begin"
          (lambda (widget context)
            (let ((name (gtk-widget-name widget)))
              (format t "~%DRAG-BEGIN for ~A~%" name)
              (format t "    widget : ~a~%" widget)
              (format t "   context : ~a~%" context)

              (format t "   actions : ~a~%" (gdk-drag-context-actions context))
              (format t " suggested : ~a~%" (gdk-drag-context-suggested-action context))
              (format t "  selected : ~a~%" (gdk-drag-context-selected-action context))
              (format t "   targets : ~a~%" (gdk-drag-context-list-targets context))
              (format t "    device : ~a~%" (gdk-drag-context-device context))
              (format t "    source : ~a~%" (gdk-drag-context-source-window context))
              (format t "      dest : ~a~%" (gdk-drag-context-dest-window context))
              (format t "  protocol : ~a~%" (gdk-drag-context-protocol context))
      )))

      ;; Emitted when DnD ends. This is used to clean up any leftover data.
      (g-signal-connect coin-source "drag-end"
          (lambda (widget context)
            (let ((name (gtk-widget-name widget)))
              (format t "~%DRAG-END for ~A~%" name)
              (format t "   widget : ~a~%" widget)
              (format t "  context : ~a~%" context)
      )))

      ;; Emitted when the destination requests data from the source via
      ;; gtk_drag_get_data. It should attempt to provide its data in the form
      ;; requested in the target_type passed to it from the destination. If it
      ;; cannot, it should default to a "safe" type such as a string or text,
      ;; even if only to print an error. Then use gtk_selection_data_set to put
      ;; the source data into the allocated selection_data object, which will
      ;; then be passed to the destination. This will cause "drag-data-received"
      ;; to be emitted on the destination. GdkSelectionData is based on X's
      ;; selection mechanism which, via X properties, is only capable of storing
      ;; data in blocks of 8, 16, or 32 bit units.
      (g-signal-connect coin-source "drag-data-get"
          (lambda (widget context selection target time)

            (format t "~%DRAG-DATA-GET~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)
            (format t "selection : ~a~%" selection)
            (format t "   target : ~a~%" target)
            (format t "     time : ~a~%" time)

            (cond ((= target-int32 target)
                   (format t "found INTEGER target~%")
                   (with-foreign-object (integer-data :int)
                     (setf (mem-ref integer-data :int) 999)
                     (gtk-selection-data-set selection
                                             (gtk-selection-data-target selection)
                                             32 ; DWORD
                                             integer-data
                                             (foreign-type-size :int))

                     (setf (gtk-selection-data-format selection) 32)
                     (setf (gtk-selection-data-data selection) integer-data)
                     (setf (gtk-selection-data-length selection) (foreign-type-size :int))

                     (format t "selection : ~a~%" selection)

                  ))
                  ((= target-string target)
                   (format t "found STRING target~%")
                   (setf (gtk-selection-data-text selection) "text message")
                   (format t "selection : ~a~%" selection)
                  )
                  (t
                   (format t "found no target~%")))
            t))

      (g-signal-connect coin-source "drag-data-delete"
          (lambda (widget context)
            (format t "~%DRAG-DATA-DELETE~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)
      ))

      ;; Pack and show the widgets
      (gtk-container-add window hbox)
      (gtk-container-add hbox coin-source)
      (gtk-container-add hbox directions-label)
      (gtk-container-add hbox well-dest)
      (gtk-widget-show-all window))))
