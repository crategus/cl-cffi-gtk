;;;; Drag and Drop - 2021-10-1

;; TODO: Finish this example

(in-package #:gtk-example)

;; Define a list of data types called "targets" that a destination widget will
;; accept. The string type is arbitrary, and negotiated between DnD widgets by
;; the developer. An enum or GQuark can serve as the integer target id.

(defvar target-int32 0)
(defvar target-string 1)
(defvar target-rootwin 2)

(defvar targets
        (list (list "INTEGER" 0 target-int32)
              (list "STRING" 0 target-string)
              (list "text/plain" 0 target-string)
              (list "application/x-rootwindow-drop" 0 target-rootwin)))

(defun print-context (context)
  (format t "~%")
  (format t "   actions : ~a~%" (gdk-drag-context-actions context))
  (format t " suggested : ~a~%" (gdk-drag-context-suggested-action context))
  (format t "  selected : ~a~%" (gdk-drag-context-selected-action context))
  (format t "   targets : ~a~%" (gdk-drag-context-list-targets context))
  (format t "    device : ~a~%" (gdk-drag-context-device context))
  (format t "    source : ~a~%" (gdk-drag-context-source-window context))
  (format t "      dest : ~a~%" (gdk-drag-context-dest-window context))
  (format t "  protocol : ~a~%" (gdk-drag-context-protocol context)))

(defun print-selection (selection)
  (format t "~%")
  (format t " selection : ~a~%" (gtk-selection-data-selection selection))
  (format t "    target : ~a~%" (gtk-selection-data-target selection))
  (format t " data-type : ~a~%" (gtk-selection-data-data-type selection))
  (format t "    format : ~a~%" (gtk-selection-data-format selection))
  (format t "      data : ~a~%" (gtk-selection-data-data selection))
  (format t "    length : ~a~%" (gtk-selection-data-length selection))
  (format t "   display : ~a~%" (gtk-selection-data-display selection)))

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
                                           "drag a coin and drop it"))
          ;; Workaround: Global variable to store the selection data
          (selection nil))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Make the "well label" a DnD destination
      (gtk-drag-dest-set well-dest             ; widget that will accept a drop
                         '(:motion :highlight) ; default actions for dest Dnd
                         targets               ; list of targets to support
                         '(:copy))             ; what to with dropped data

      ;; Make the "coin-button" a DnD source
      (gtk-drag-source-set coin-source         ; widget will be dragable
                           '(:button1-mask)    ; modifier that will start drag
                           targets             ; lists of targets to support
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
              (declare (ignorable targets is-valid-drop))

              (format t "~%DRAG-DROP~%")
              (format t "   widget : ~a~%" widget)
              (format t "  context : ~a~%" context)
              (format t "     x, y : ~a, ~a~%" x y)
              (format t "     time : ~a~%" time)

              (print-context context)

              (gtk-drag-data widget context "STRING" time)

              t)))

      ;; Emitted when the data has been received from the source. It should
      ;; check the GtkSelectionData sent by the source, and do something with
      ;; it. Finally it needs to finish the operation by calling
      ;; gtk_drag_finish, which will emit the "data-delete" signal if told to.
      (g-signal-connect well-dest "drag-data-received"
          (lambda (widget context x y data info time)

            (format t "~%DRAG-DATA-RECEIVED~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)
            (format t "     x, y : ~a, ~a~%" x y)
            (format t "selection : ~a~%" data)
            (format t "     info : ~a~%" info)
            (format t "     time : ~a~%" time)

            (print-context context)

            ;; Workaround: Copy selection data from the global
            (setf data (gtk-selection-data-copy selection))
            (print-selection data)

            (let ((success nil) (delete nil))

              ;; Deal with what we are given from source
              (when (and (not (null-pointer-p (gtk-selection-data-data data)))
                         (>= (gtk-selection-data-length data) 0))
                (format t "DnD data received.~%")
              )

              (unless success
                (format t "DnD data transfer failed.~%"))
              (gtk-drag-finish context success delete time)
      )))

      ;; Emitted when a drag leaves the destination
      (g-signal-connect well-dest "drag-leave"
          (lambda (widget context time)

            (format t "~%DRAG-LEAVE~%")
            (format t "    widget : ~a~%" widget)
            (format t "   context : ~a~%" context)
            (format t "      time : ~a~%" time)

            (print-context context)
      ))

      ;; Emitted when a drag is over the destination
      (g-signal-connect well-dest "drag-motion"
          (lambda (widget context x y time)
             (declare (ignore widget context x y time))
;            (format t "~%DRAG-MOTION~%")
;            (format t "    widget : ~a~%" widget)
;            (format t "   context : ~a~%" context)
;            (format t "      x, y : ~a~%" x y)
;            (format t "      time : ~a~%" time)

            ;; Fancy stuff here. This signal spams the console something
            ;; horrible.
      ))

      ;; Connect all possible source signals

      ;; Emitted when DnD begins. This is often used to present custom graphics.
      (g-signal-connect coin-source "drag-begin"
          (lambda (widget context)
            (format t "~%DRAG-BEGIN~%")
            (format t "    widget : ~a~%" widget)
            (format t "   context : ~a~%" context)

            (print-context context)

            nil))

      ;; Emitted when DnD ends. This is used to clean up any leftover data.
      (g-signal-connect coin-source "drag-end"
          (lambda (widget context)
            (format t "~%DRAG-END~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)

            (print-context context)
      ))

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
          (lambda (widget context data target time)

            (format t "~%DRAG-DATA-GET~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)
            (format t "selection : ~a~%" data)
            (format t "   target : ~a~%" target)
            (format t "     time : ~a~%" time)

            (print-context context)

            (cond ((= target-int32 target)
                   (format t "found INTEGER target~%")
                   (with-foreign-object (integer-data :int)
                     (setf (mem-ref integer-data :int) 999)
                     (gtk-selection-data-set selection
                                             (gtk-selection-data-target selection)
                                             32 ; DWORD
                                             integer-data
                                             (foreign-type-size :int))
                  ))
                  ((= target-string target)
                   (format t "found STRING target~%")
                   (setf (gtk-selection-data-text selection) "text message")
                   (print-selection selection)
                  )
                  (t
                   (format t "found no target~%")))
            ;; Workaround: Store selection data in a global
            (setf selection (gtk-selection-data-copy data))
            (print-selection data)
               
            nil))

      ;; Emitted after "drag-data-received" is handled, and gtk_drag_finish is
      ;; called with the "delete" parameter set to TRUE (when DnD is
      ;; GDK_ACTION_MOVE).
      (g-signal-connect coin-source "drag-data-delete"
          (lambda (widget context)

            (format t "~%DRAG-DATA-DELETE~%")
            (format t "   widget : ~a~%" widget)
            (format t "  context : ~a~%" context)

            (print-context context)

            ;; We are not moving or deleting anything here.
      ))

      ;; Pack and show the widgets
      (gtk-container-add window hbox)
      (gtk-container-add hbox coin-source)
      (gtk-container-add hbox directions-label)
      (gtk-container-add hbox well-dest)
      (gtk-widget-show-all window))))
