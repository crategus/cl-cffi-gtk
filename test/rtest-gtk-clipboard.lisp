(def-suite gtk-clipboard :in gtk-suite)
(in-suite gtk-clipboard)

(defparameter *verbose-gtk-clipboard* nil)

;;; Types and Values

;;; --- GtkClipboard -----------------------------------------------------------

(test gtk-clipboard-class
  ;; Type check
  (is (g-type-is-object "GtkClipboard"))
  ;; Check the registered name
  (is (eq 'gtk-clipboard
          (registered-object-type-by-name "GtkClipboard")))
  ;; Check the type initializer
  (is (eq (gtype "GtkClipboard")
          (gtype (foreign-funcall "gtk_clipboard_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkClipboard")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkClipboard"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkClipboard"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkClipboard"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkClipboard" GTK-CLIPBOARD
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_clipboard_get_type")
                       NIL)
             (get-g-type-definition "GtkClipboard"))))

;;; Signals

;;;     void    owner-change    Run First

;;; Functions
;;;
;;;     GtkClipboardReceivedFunc
;;;     GtkClipboardTextReceivedFunc
;;;     GtkClipboardImageReceivedFunc
;;;     GtkClipboardTargetsReceivedFunc
;;;     GtkClipboardRichTextReceivedFunc
;;;     GtkClipboardURIReceivedFunc
;;;     GtkClipboardGetFunc
;;;     GtkClipboardClearFunc
;;;
;;;     gtk-clipboard-get

(test gtk-clipbard-get
  (is (typep (gtk-clipboard-get "CLIPBOARD") 'gtk-clipboard))
  (is (typep (gtk-clipboard-get "PRIMARY") 'gtk-clipboard))
  (is (typep (gtk-clipboard-get "SECONDARY") 'gtk-clipboard)))

;;;     gtk-clipboard-for-display

(test gtk-clipboard-get-for-display
  (let ((display (gdk-display-default)))
    (is (typep (gtk-clipboard-for-display display "CLIPBOARD") 'gtk-clipboard))
    (is (typep (gtk-clipboard-for-display display "PRIMARY") 'gtk-clipboard))
    (is (typep (gtk-clipboard-for-display display "SECONDARY") 'gtk-clipboard))))

;;;     gtk_clipboard_get_display

(test gtk-clipboard-display
  (let ((clipboard (gtk-clipboard-get "CLIPBOARD")))
    (is (typep (gtk-clipboard-display clipboard) 'gdk-display))))

;;;     gtk_clipboard_get_default ()

(test gtk-clipboard-default
  (is (typep (gtk-clipboard-default (gdk-display-default)) 'gtk-clipboard)))

;;;     gtk_clipboard_set_with_data
;;;     gtk_clipboard_set_with_owner
;;;     gtk_clipboard_get_owner

;;;     gtk_clipboard_clear

;;;     gtk_clipboard_set_text
;;;     gtk_clipboard_request_text

(test gtk-clipboard-set-text
  (flet ((request-text (clipboard text)
           (is (typep clipboard 'gtk-clipboard))
           (is (string= text "This is text."))))
  (let ((clipboard (gtk-clipboard-get "CLIPBOARD")))
    (is-false (gtk-clipboard-set-text clipboard "This is text."))
    (is-false (gtk-clipboard-request-text clipboard #'request-text)))))

;;;     gtk_clipboard_set_image
;;;     gtk_clipboard_request_image

(test gtk-clipboard-set-image
  (flet ((request-image (clipboard  pixbuf)
           (is (typep clipboard 'gtk-clipboard))
           (is (typep pixbuf 'gdk-pixbuf))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-IMAGE:~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t "    pixbuf : ~a~%" pixbuf))))
  (let ((clipboard (gtk-clipboard-get "CLIPBOARD"))
        (pixbuf (gtk-icon-theme-load-icon (gtk-icon-theme-default)
                                          "gtk-ok"
                                          48
                                          0)))
    (is (typep pixbuf 'gdk-pixbuf))
    (gtk-clipboard-set-image clipboard pixbuf)
    (gtk-clipboard-request-image clipboard #'request-image))))

;;;     gtk_clipboard_request_contents

(test gtk-clipboard-request-contents.1
  (flet ((request-contents (clipboard selection)
           (is (typep clipboard 'gtk-clipboard))
           (is (typep selection 'gtk-selection-data))
           (is (string= "This is text."
                        (gtk-selection-data-text selection)))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-CONTENS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection))))
  (let ((clipboard (gtk-clipboard-get "CLIPBOARD")))
    (is-false (gtk-clipboard-set-text clipboard "This is text."))
    (is-false (gtk-clipboard-request-contents clipboard
                                              "STRING"
                                              #'request-contents)))))

(test gtk-clipboard-request-contents.2
  (flet ((request-contents (clipboard selection)
           (is (typep clipboard 'gtk-clipboard))
           (is (typep selection 'gtk-selection-data))
           ;; FIXME: This should be true. It works for text, not for a pixbuf.
           (is-false (typep (gtk-selection-data-pixbuf selection) 'gdk-pixbuf))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-CONTENS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection))))
  (let ((clipboard (gtk-clipboard-get "CLIPBOARD"))
        (pixbuf (gtk-icon-theme-load-icon (gtk-icon-theme-default)
                                          "gtk-ok"
                                          48
                                          0)))
    (is-false (gtk-clipboard-set-image clipboard pixbuf))
    (is-false (gtk-clipboard-request-contents clipboard
                                              "PIXMAP"
                                              #'request-contents)))))

;;;     gtk_clipboard_request_targets

(test gtk-clipboard-request-targets
  (flet ((request-targets (clipboard atoms n-atoms)
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-TARGETS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t "     atoms : ~a~%" atoms)
             (format t "   n-atoms : ~a~%" n-atoms)
             (with-foreign-object (targets-ar 'gdk-atom-as-string n-atoms)
               (loop for i from 0 below n-atoms
                     do (format t "    target : ~a~%"
                                  (mem-aref targets-ar
                                            'gdk-atom-as-string i)))))))

  (let ((clipboard (gtk-clipboard-get "CLIPBOARD")))

;    (is-false (gtk-clipboard-set-text clipboard "This is some text."))
    (is-false (gtk-clipboard-request-targets clipboard #'request-targets))

)))

;;;     gtk_clipboard_request_rich_text



;;;     gtk_clipboard_request_uris
;;;     gtk_clipboard_wait_for_contents
;;;     gtk_clipboard_wait_for_text
;;;     gtk_clipboard_wait_for_image
;;;     gtk_clipboard_wait_for_rich_text
;;;     gtk_clipboard_wait_for_uris
;;;     gtk_clipboard_wait_is_text_available
;;;     gtk_clipboard_wait_is_image_available
;;;     gtk_clipboard_wait_is_rich_text_available
;;;     gtk_clipboard_wait_is_uris_available
;;;     gtk_clipboard_wait_for_targets
;;;     gtk_clipboard_wait_is_target_available
;;;     gtk_clipboard_set_can_store
;;;     gtk_clipboard_store

;;;     gtk_clipboard_get_selection

(test gtk-clipboard-selection
  (let ((clipboard (gtk-clipboard-default (gdk-display-default))))
    (is (string= "CLIPBOARD" (gtk-clipboard-selection clipboard)))))

;;; 2021-3-28
