
(def-suite gtk-selections :in gtk-suite)
(in-suite gtk-selections)

(defvar *verbose-gtk-selections* nil)

;;;   GtkSelectionData

(test gtk-selection-data
  ;; Type checks
  (is-false (g-type-is-object "GtkSelectionData"))
  (is-false (g-type-is-abstract "GtkSelectionData"))
  (is-true  (g-type-is-derived "GtkSelectionData"))
  (is-false (g-type-is-fundamental "GtkSelectionData"))
  (is-true  (g-type-is-value-type "GtkSelectionData"))
  (is-true  (g-type-has-value-table "GtkSelectionData"))
  (is-false (g-type-is-classed "GtkSelectionData"))
  (is-false (g-type-is-instantiatable "GtkSelectionData"))
  (is-true  (g-type-is-derivable "GtkSelectionData"))
  (is-false (g-type-is-deep-derivable "GtkSelectionData"))
  (is-false (g-type-is-interface "GtkSelectionData"))

  ;; Check the fundamental type
  (is (equal (gtype "GBoxed") (g-type-fundamental "GtkSelectionData")))

  ;; Check some more GType information
  (is (equal (gtype "GBoxed") (g-type-parent "GtkSelectionData")))
  (is (= 2 (g-type-depth "GtkSelectionData")))
  (is (equal (gtype "GtkSelectionData")
             (g-type-next-base "GtkSelectionData" "GBoxed")))
  (is-false (g-type-is-a "GtkSelectionData" "GtkWidget"))
  (is-false (g-type-is-a "GtkSelectionData" "GtkContainer"))
  (is-false (g-type-is-a "GtkSelectionData" "gboolean"))
  (is-true  (g-type-is-a "GtkSelectionData" "GBoxed"))
)

(test make-gtk-selection-data
  (let ((selection (make-gtk-selection-data)))
    (is-false (gtk-selection-data-selection selection))
    (is-false (gtk-selection-data-target selection))
    (is-false (gtk-selection-data-type selection))
    (is (eql 0 (gtk-selection-data-format selection)))
    (is-true (null-pointer-p (gtk-selection-data-data selection)))
    (is (eql 0 (gtk-selection-data-length selection)))
    (is-false (gtk-selection-data-display selection))))

;;;     GtkTargetFlags  <-- gtk.drag-and-drop.lisp
;;;     GtkTargetEntry
;;;     GtkTargetList

;;;     gtk_target_entry_new
;;;     gtk_target_entry_copy
;;;     gtk_target_entry_free
;;;     gtk_target_list_new
;;;     gtk_target_list_ref
;;;     gtk_target_list_unref
;;;     gtk_target_list_add
;;;     gtk_target_list_add_table
;;;     gtk_target_list_add_text_targets
;;;     gtk_target_list_add_image_targets
;;;     gtk_target_list_add_uri_targets
;;;     gtk_target_list_add_rich_text_targets
;;;     gtk_target_list_remove
;;;     gtk_target_list_find
;;;     gtk_target_table_free
;;;     gtk_target_table_new_from_list

;;;   gtk_selection_owner_set

(test gtk-selection-owner-set.1
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-get-window widget)))
      ;; Check the presence of a gkd-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "PRIMARY"
                                        +gdk-current-time+)))))

(test gtk-selection-owner-set.2
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-get-window widget)))
      ;; Check the presence of a gkd-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "SECONDARY"
                                        +gdk-current-time+)))))

(test gtk-selection-owner-set.3
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-get-window widget)))
      ;; Check the presence of a gkd-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "CLIPBOARD"
                                        +gdk-current-time+)))))

;;;   gtk_selection_owner_set_for_display

(test gtk-selection-owner-set-for-display
  (let ((display (gdk-display-get-default))
        (widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-get-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))

      (g-signal-connect widget "selection-clear-event"
         (lambda (widget event)
           (when *verbose-gtk-selections*
             (format t "~&SELECTION-CLEAR-EVENT ~A~%" widget)
             (format t "    event = ~A~%" event))))

      (is-true (gtk-selection-owner-set-for-display display
                                                    widget
                                                    "PRIMARY"
                                                    +gdk-current-time+))
      ;; Get the owner and check it is eql to window
      (is (eql window (gdk-selection-owner-get "PRIMARY")))

      ;; This call emits the "selection-clear-event"
      (is-true (gtk-selection-owner-set-for-display display
                                                    nil
                                                    "PRIMARY"
                                                    +gdk-current-time+))
)))

;;;   gtk_selection_add_target

(test gtk-selection-add-target
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-get-window widget)))
      ;; Check the presence of a gkd-window
      (is (eq 'gdk-window (type-of window)))

      (gtk-selection-add-target widget "PRIMARY" "STRING" 0)
)))

;;;     gtk_selection_add_targets

;;;   gtk_selection_clear_targets

(test gtk-selection-clear-targets
  (let ((widget (make-instance 'gtk-window :type :toplevel)))

    (g-signal-connect widget "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore time))
         (format t "~&Signal SELECTION-RECEIVED for ~A~%" widget)
         (format t "  selection = ~A~%" selection-data)
         (format t "targets = ~A~%" (gtk-selection-data-get-targets selection-data))
    ))
    
    (gtk-widget-realize widget)
    (gtk-selection-owner-set widget "CLIPBOARD" +gdk-current-time+)
    (gtk-selection-clear-targets widget "CLIPBOARD")
    (gtk-selection-convert widget "CLIPBOARD" "TARGETS" +gdk-current-time+)

))


;;;   gtk_selection_convert

(test gtk-selection-convert
  (let ((window (make-instance 'gtk-window :type :toplevel))
        (button (make-instance 'gtk-button)))

    (gtk-container-add window button)
    (gtk-widget-realize window)

    (g-signal-connect button "selection-received"
       (lambda (widget selection-data time)
         (format t "~&Signal SELECTION-RECEIVED for ~A~%" widget)
         (format t "  selection = ~A~%" selection-data)
         (format t "  time      = ~A~%" time)
         (format t "targets = ~A~%" (gtk-selection-data-get-targets selection-data))
    ))
    
;    (gtk-selection-add-target widget "PRIMARY" "STRING" 0)
    (gtk-selection-convert button "CLIPBOARD" "TARGETS" +gdk-current-time+)

))

;;;     gtk_selection_data_set

;;;   gtk_selection_data_set_text

(test gtk-selection-data-set-text
  (let ((window (make-instance 'gtk-window :type :toplevel)))

    (gtk-widget-realize window)

    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
         (gtk-selection-data-set-text selection-data "some text")
         (format t "~&GTK-SELECTION-DATA-SET-TEXT ~A~%" selection-data)
         (is (equal "some text" (gtk-selection-data-get-text selection-data)))
         (is (equal "CLIPBOARD"
                    (gtk-selection-data-get-selection selection-data)))
         (is-true (pointerp (gtk-selection-data-get-data selection-data)))
         (is (= 9 (gtk-selection-data-get-length selection-data)))
         (is (equal "COMPOUND_TEXT" (gtk-selection-data-get-data-type selection-data)))
         (is (eq 'gdk-display
                 (type-of (gtk-selection-data-get-display selection-data))))
         (is (= 8 (gtk-selection-data-get-format selection-data)))
         (is (equal "TEXT"
                    (gtk-selection-data-get-target selection-data)))
         (multiple-value-bind (length data)
             (gtk-selection-data-get-data-with-length selection-data)
           (is (= 9 length))
           (is (pointerp data)))))

    (gtk-selection-convert window "CLIPBOARD" "TEXT" +gdk-current-time+)

))

;;;   gtk_selection_data_get_text

#+nil
(test gtk-selection-data-get-text
  (let ((selection (make-gtk-selection-data :selection "PRIMARY"
                                            :target "STRING"
                                            :type "STRING"
                                            :display (gdk-display-get-default))))

    (gtk-selection-data-set-text selection "text")
    (is (equal "text" (gtk-selection-data-get-text selection)))))

;;;     gtk_selection_data_set_pixbuf
;;;     gtk_selection_data_get_pixbuf
;;;     gtk_selection_data_set_uris
;;;     gtk_selection_data_get_uris
;;;     gtk_selection_data_get_targets

;;;   gtk_selection_data_targets_include_image

(test gtk-selection-data-targets-include-image
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-realize window)
    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
           (is-true (gtk-selection-data-targets-include-image selection-data nil))))
    (gtk-selection-convert window "CLIPBOARD" "TEXT" +gdk-current-time+)))

;;;   gtk_selection_data_targets_include_text

(test gtk-selection-data-targets-include-text
  (let ((window (make-instance 'gtk-window :type :toplevel)))

    (gtk-widget-realize window)

    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
           (is-true (gtk-selection-data-targets-include-text selection-data))))


    (gtk-selection-convert window "CLIPBOARD" "TEXT" +gdk-current-time+)))

;;;     gtk_selection_data_targets_include_uri
;;;     gtk_selection_data_targets_include_rich_text

;;;   gtk_selection_data_get_selection
;;;   gtk_selection_data_get_data
;;;   gtk_selection_data_get_length
;;;   gtk_selection_data_get_data_with_length
;;;   gtk_selection_data_get_data_type
;;;   gtk_selection_data_get_display
;;;   gtk_selection_data_get_format
;;;   gtk_selection_data_get_target

(test gtk-selection-data-get
  (let ((window (make-instance 'gtk-window :type :toplevel)))

    (gtk-widget-realize window)

    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
         (is (equal "CLIPBOARD"
                    (gtk-selection-data-get-selection selection-data)))
         (is-true (pointerp (gtk-selection-data-get-data selection-data)))
         (is (= 12 (gtk-selection-data-get-length selection-data)))
         (is (equal "ATOM" (gtk-selection-data-get-data-type selection-data)))
         (is (eq 'gdk-display
                 (type-of (gtk-selection-data-get-display selection-data))))
         (is (= 32 (gtk-selection-data-get-format selection-data)))
         (is (equal "TARGETS"
                    (gtk-selection-data-get-target selection-data)))
         (multiple-value-bind (length data)
             (gtk-selection-data-get-data-with-length selection-data)
           (is (= 12 length))
           (is (pointerp data)))))

    (gtk-selection-convert window "CLIPBOARD" "TARGETS" +gdk-current-time+)))

;;;     gtk_targets_include_image
;;;     gtk_targets_include_text
;;;     gtk_targets_include_uri
;;;     gtk_targets_include_rich_text

;;;     gtk_selection_remove_all
;;;     gtk_selection_data_copy
;;;     gtk_selection_data_free

