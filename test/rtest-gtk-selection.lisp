(def-suite gtk-selection :in gtk-suite)
(in-suite gtk-selection)

(defvar *verbose-gtk-selection* nil)

;;;   GtkTargetFlags  <-- gtk.drag-and-drop.lisp

(test gtk-target-flags
  ;; Check the type
  (is (g-type-is-flags "GtkTargetFlags"))
  ;; Check the registered name
  (is (eq 'gtk-target-flags
          (registered-flags-type "GtkTargetFlags")))
  ;; Check the type initializer
  (is (eq (gtype "GtkTargetFlags")
          (gtype (foreign-funcall "gtk_target_flags_get_type" g-size))))
  ;; Check the names
  (is (equal '("GTK_TARGET_SAME_APP" "GTK_TARGET_SAME_WIDGET"
               "GTK_TARGET_OTHER_APP" "GTK_TARGET_OTHER_WIDGET")
             (mapcar #'flags-item-name
                     (get-flags-items "GtkTargetFlags"))))
  ;; Check the values
  (is (equal '(1 2 4 8)
             (mapcar #'flags-item-value
                     (get-flags-items "GtkTargetFlags"))))
  ;; Check the nick names
  (is (equal '("same-app" "same-widget" "other-app" "other-widget")
             (mapcar #'flags-item-nick
                     (get-flags-items "GtkTargetFlags"))))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkTargetFlags"
                              GTK-TARGET-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_target_flags_get_type")
                              (:SAME-APP 1)
                              (:SAME-WIDGET 2)
                              (:OTHER-APP 4)
                              (:OTHER-WIDGET 8))
             (get-g-type-definition "GtkTargetFlags"))))

;;;   GtkTargetEntry

;;;   GtkTargetList

(test gtk-target-list-structure
  ;; Type check
  (is (g-type-is-a (gtype "GtkTargetList") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GtkTargetList")
          (gtype (foreign-funcall "gtk_target_list_get_type" g-size)))))

;;;     GtkSelectionData

(test gtk-selection-data-structure
  ;; Type check
  (is (g-type-is-a (gtype "GtkSelectionData") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GtkSelectionData")
          (gtype (foreign-funcall "gtk_selection_data_get_type" g-size)))))

;;;   gtk_target_entry_new
;;;   gtk_target_entry_copy
;;;   gtk_target_entry_free

;;;   gtk_target_list_new

(test gtk-target-list-new
  (let ((tlist (gtk-target-list-new '(("text/html" :none 0)
                                      ("STRING" :none 1)
                                      ("number" :none 2)
                                      ("image/jpeg" :none 3)
                                      ("text/uri-list" :none 4)))))
    (is (= 0 (gtk-target-list-find tlist "text/html")))
    (is (= 1 (gtk-target-list-find tlist "STRING")))
    (is (= 2 (gtk-target-list-find tlist "number")))
    (is (= 3 (gtk-target-list-find tlist "image/jpeg")))
    (is (= 4 (gtk-target-list-find tlist "text/uri-list")))))

;;;   gtk_target_list_ref
;;;   gtk_target_list_unref

;;;   gtk_target_list_add

(test gtk-target-list-add
  (let ((tlist (gtk-target-list-new)))
    ;; Add the target entries
    (gtk-target-list-add tlist "text/html" 0 0)
    (gtk-target-list-add tlist "STRING" 0 1)
    (gtk-target-list-add tlist "number" 0 2)
    (gtk-target-list-add tlist "image/jpeg" 0 3)
    (gtk-target-list-add tlist "text/uri-list" 0 4)
    ;; Check the added target entries
    (is (= 0 (gtk-target-list-find tlist "text/html")))
    (is (= 1 (gtk-target-list-find tlist "STRING")))
    (is (= 2 (gtk-target-list-find tlist "number")))
    (is (= 3 (gtk-target-list-find tlist "image/jpeg")))
    (is (= 4 (gtk-target-list-find tlist "text/uri-list")))
    ;; Check for an unknown target entry
    (is-false (gtk-target-list-find tlist "unknown"))))

;;;   gtk_target_list_add_table

(test gtk-target-list-add-table
  (let ((tlist (gtk-target-list-new)))
    (gtk-target-list-add-table tlist '(("text/html" :none 0)
                                       ("STRING" :none 1)
                                       ("number" :none 2)
                                       ("image/jpeg" :none 3)
                                       ("text/uri-list" :none 4)))
    (is (= 0 (gtk-target-list-find tlist "text/html")))
    (is (= 1 (gtk-target-list-find tlist "STRING")))
    (is (= 2 (gtk-target-list-find tlist "number")))
    (is (= 3 (gtk-target-list-find tlist "image/jpeg")))
    (is (= 4 (gtk-target-list-find tlist "text/uri-list")))))

;;;   gtk_target_list_add_text_targets

(test gtk-target-list-add-text-targets
  (let ((tlist (gtk-target-list-new)))
    (gtk-target-list-add-text-targets tlist 0)
    (is (equal #-win32
               '("UTF8_STRING"
                 "COMPOUND_TEXT"
                 "TEXT"
                 "STRING"
                 "text/plain;charset=utf-8"
                 "text/plain")
               #+win32
               '("UTF8_STRING"
                 "COMPOUND_TEXT"
                 "TEXT"
                 "STRING"
                 "text/plain;charset=utf-8"
                 "text/plain;charset=CP1252"
                 "text/plain")
               (mapcar #'first
                       (gtk-target-table-new-from-list tlist))))))

;;;   gtk_target_list_add_image_targets

(test gtk-target-list-add-image-targets.1
  (let ((tlist (gtk-target-list-new)))
    (gtk-target-list-add-image-targets tlist 0 t)
    (is (equal '("application/ico" "image/bmp" "image/ico" "image/icon"
                 "image/jpeg" "image/png" "image/tiff"
                 "image/vnd.microsoft.icon" "image/x-bmp" "image/x-ico"
                 "image/x-icon" "image/x-MS-bmp" "image/x-win-bitmap"
                 "text/ico")
               (sort (mapcar #'first
                             (gtk-target-table-new-from-list tlist))
                     #'string-lessp)))))

(test gtk-target-list-add-image-targets.2
  (let ((tlist (gtk-target-list-new)))
    (gtk-target-list-add-image-targets tlist 0 nil)
    (is (equal #-win32
               '("application/ico" "application/x-navi-animation" "image/bmp"
                 "image/gif" "image/ico" "image/icon" "image/jpeg" "image/png"
                 "image/qtif" "image/svg" "image/svg+xml"
                 "image/svg+xml-compressed" "image/svg-xml" "image/tiff"
                 "image/vnd.adobe.svg+xml" "image/vnd.microsoft.icon"
                 "image/x-bmp" "image/x-icns" "image/x-ico" "image/x-icon"
                 "image/x-MS-bmp" "image/x-portable-anymap"
                 "image/x-portable-bitmap" "image/x-portable-graymap"
                 "image/x-portable-pixmap" "image/x-quicktime" "image/x-tga"
                 "image/x-win-bitmap" "image/x-wmf" "image/x-xbitmap"
                 "image/x-xpixmap" "text/ico" "text/xml-svg")
               #+win32
               '("application/emf" "application/ico" "application/x-emf"
                 "application/x-navi-animation" "image/bmp" "image/emf"
                 "image/gif" "image/ico" "image/icon" "image/jpeg" "image/png"
                 "image/qtif" "image/svg" "image/svg+xml"
                 "image/svg+xml-compressed" "image/svg-xml" "image/tiff"
                 "image/vnd.adobe.svg+xml" "image/vnd.microsoft.icon"
                 "image/wmf" "image/x-bmp" "image/x-emf" "image/x-icns"
                 "image/x-ico" "image/x-icon" "image/x-mgx-emf" "image/x-MS-bmp"
                 "image/x-portable-anymap" "image/x-portable-bitmap"
                 "image/x-portable-graymap" "image/x-portable-pixmap"
                 "image/x-quicktime" "image/x-tga" "image/x-win-bitmap"
                 "image/x-wmf" "image/x-wmf" "image/x-xbitmap" "image/x-xpixmap"
                 "text/ico" "text/xml-svg")
               (sort (mapcar #'first
                             (gtk-target-table-new-from-list tlist))
                     #'string-lessp)))))

;;;   gtk_target_list_add_uri_targets

(test gtk-target-list-add-uri-targets
  (let ((target-list (gtk-target-list-new)))
    (gtk-target-list-add-uri-targets target-list 0)
    (is (equal '("text/uri-list")
               (mapcar #'first
                       (gtk-target-table-new-from-list target-list))))))

;;;   gtk_target_list_add_rich_text_targets

(test gtk-target-list-add-rich-text-targets.1
  (let ((target-list (gtk-target-list-new))
        (buffer (make-instance 'gtk-text-buffer)))
    (gtk-target-list-add-rich-text-targets target-list 0 nil buffer)
    (is (equal '("application/x-gtk-text-buffer-rich-text")
               (mapcar #'first
                       (gtk-target-table-new-from-list target-list))))))

(test gtk-target-list-add-rich-text-targets.2
  (let ((target-list (gtk-target-list-new))
        (buffer (make-instance 'gtk-text-buffer)))
    (gtk-target-list-add-rich-text-targets target-list 0 t buffer)
    (is (equal '()
               (mapcar #'first
                       (gtk-target-table-new-from-list target-list))))))

;;;   gtk_target_list_remove
;;;   gtk_target_list_find

(test gtk-target-list-find
  (let ((target-list (gtk-target-list-new)))
    (gtk-target-list-add target-list "text/html" 0 1)
    (is (= 1 (gtk-target-list-find target-list "text/html")))
    (gtk-target-list-remove target-list "text/html")
    (is-false (gtk-target-list-find target-list "text/html"))))

;;;   gtk_target_table_free

;;;   gtk_target_table_new_from_list

(test gtk-target-table-new-from-list
  (let ((tlist (gtk-target-list-new)))

    (gtk-target-list-add tlist "text/html" 0 0)
    (gtk-target-list-add tlist "text/plain" :same-app 1)
    (gtk-target-list-add tlist "text" '(:same-app :same-widget) 2)
    (is (equal '(("text/html" nil 0)
                 ("text/plain" (:same-app) 1)
                 ("text" (:same-app :same-widget) 2))
               (gtk-target-table-new-from-list tlist)))

    (gtk-target-list-remove tlist "text/html")
    (gtk-target-list-remove tlist "text/plain")
    (gtk-target-list-remove tlist "text")
    (is (equal '()
               (gtk-target-table-new-from-list tlist)))

    (gtk-target-list-add-text-targets tlist 99)
    (is (equal #-win32
               '(("UTF8_STRING" NIL 99)
                 ("COMPOUND_TEXT" NIL 99)
                 ("TEXT" NIL 99)
                 ("STRING" NIL 99)
                 ("text/plain;charset=utf-8" NIL 99)
                 ("text/plain" NIL 99))
               #+win32
               '(("UTF8_STRING" nil 99)
                 ("COMPOUND_TEXT" nil 99)
                 ("TEXT" nil 99)
                 ("STRING" nil 99)
                 ("text/plain;charset=utf-8" nil 99)
                 ("text/plain;charset=CP1252" nil 99)
                 ("text/plain" nil 99))
               (gtk-target-table-new-from-list tlist)))))

;;;   gtk_selection_owner_set

(test gtk-selection-owner-set.1
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "PRIMARY"
                                        +gdk-current-time+)))))

(test gtk-selection-owner-set.2
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "SECONDARY"
                                        +gdk-current-time+)))))

(test gtk-selection-owner-set.3
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))
      (is-true (gtk-selection-owner-set widget
                                        "CLIPBOARD"
                                        +gdk-current-time+)))))

;;;   gtk_selection_owner_set_for_display

#+nil
(test gtk-selection-owner-set-for-display
  (let ((display (gdk-display-default))
        (widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))

      (g-signal-connect widget "selection-clear-event"
         (lambda (widget event)
           (when *verbose-gtk-selection*
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
                                                    +gdk-current-time+)))))

;;;   gtk_selection_add_target

(test gtk-selection-add-target
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))

      (gtk-selection-add-target widget "PRIMARY" "TEXT" 0)
)))

;;;   gtk_selection_add_targets

(test gtk-selection-add-targets
  (let ((widget (make-instance 'gtk-window :type :toplevel)))
    ;; Realize the toplevel widget to create a gdk-window
    (gtk-widget-realize widget)
    (let ((window (gtk-widget-window widget)))
      ;; Check the presence of a gdk-window
      (is (eq 'gdk-window (type-of window)))

      (gtk-selection-add-targets widget
                                 "PRIMARY"
                                 '(("TEXT" 0) ("PIXBUF" 1) ("IMAGE" 2)))
)))

;;;   gtk_selection_clear_targets

#+nil
(test gtk-selection-clear-targets
  (let ((widget (make-instance 'gtk-window :type :toplevel)))

    (g-signal-connect widget "selection-received"
       (lambda (widget selection-data time)
         (declare (ignorable widget selection-data time))
;         (format t "~&Signal SELECTION-RECEIVED for ~A~%" widget)
;         (format t "  selection = ~A~%" selection-data)
;         (format t "targets = ~A~%" (gtk-selection-data-targets selection-data))
    ))

    (gtk-widget-realize widget)
    (gtk-selection-owner-set widget "CLIPBOARD" +gdk-current-time+)
    (gtk-selection-clear-targets widget "CLIPBOARD")
    (gtk-selection-convert widget "CLIPBOARD" "TARGETS" +gdk-current-time+)

))

;;;   gtk_selection_convert

#+nil
(test gtk-selection-convert
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-realize window)

    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
          (declare (ignorable widget selection-data time))
;         (format t "~&Signal SELECTION-RECEIVED for ~A~%" widget)
;         (format t "  selection = ~A~%" selection-data)
;         (format t "    targets = ~A~%" (gtk-selection-data-targets selection-data))
    ))

    (gtk-selection-add-target window "CLIPBOARD" "STRING" 0)
    (gtk-selection-convert window "CLIPBOARD" "TARGETS" +gdk-current-time+)

))

;;;   gtk_selection_data_set

;;;   gtk_selection_data_set_text

#+nil
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
                                            :display (gdk-display-default))))

    (gtk-selection-data-set-text selection "text")
    (is (equal "text" (gtk-selection-data-get-text selection)))))

;;;     gtk_selection_data_set_pixbuf
;;;     gtk_selection_data_get_pixbuf
;;;     gtk_selection_data_set_uris
;;;     gtk_selection_data_get_uris
;;;     gtk_selection_data_get_targets

;;;   gtk_selection_data_targets_include_image

#+nil
(test gtk-selection-data-targets-include-image
  (let ((window (make-instance 'gtk-window :type :toplevel)))
    (gtk-widget-realize window)
    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
           (is-true (gtk-selection-data-targets-include-image selection-data nil))))
    (gtk-selection-convert window "CLIPBOARD" "BITMAP" +gdk-current-time+)))

;;;   gtk_selection_data_targets_include_text

#+nil
(test gtk-selection-data-targets-include-text
  (let ((window (make-instance 'gtk-window :type :toplevel)))

    (gtk-widget-realize window)

    (g-signal-connect window "selection-received"
       (lambda (widget selection-data time)
         (declare (ignore widget time))
           (is-true (gtk-selection-data-targets-include-text selection-data))))


    (gtk-selection-convert window "CLIPBOARD" "TEXT" +gdk-current-time+)))

;;;   gtk_selection_data_targets_include_uri
;;;   gtk_selection_data_targets_include_rich_text

;;;   gtk_selection_data_get_selection
;;;   gtk_selection_data_get_data
;;;   gtk_selection_data_get_length
;;;   gtk_selection_data_get_data_with_length
;;;   gtk_selection_data_get_data_type
;;;   gtk_selection_data_get_display
;;;   gtk_selection_data_get_format
;;;   gtk_selection_data_get_target

#+nil
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

(test gtk-targets-include-image
  (is-false (gtk-targets-include-image '() nil))
  (is-true (gtk-targets-include-image '("application/ico" "image/bmp"
                                        "image/ico" "image/icon"
                                        "image/jpeg" "image/png" "image/tiff"
                                        "image/vnd.microsoft.icon" "image/x-bmp"
                                        "image/x-ico" "image/x-icon"
                                        "image/x-MS-bmp" "image/x-win-bitmap"
                                        "text/ico") nil)))

;;;     gtk_targets_include_text
;;;     gtk_targets_include_uri
;;;     gtk_targets_include_rich_text

;;;     gtk_selection_remove_all
;;;     gtk_selection_data_copy
;;;     gtk_selection_data_free

;;; 2021-9-28
