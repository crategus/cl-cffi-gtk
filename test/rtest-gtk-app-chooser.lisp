(def-suite gtk-app-chooser :in gtk-suite)
(in-suite gtk-app-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooser

(test gtk-app-chooser-interface
  ;; Type check
  (is-true (g-type-is-interface "GtkAppChooser"))
  ;; Check the registered name
  (is (eq 'gtk-app-chooser
          (registered-object-type-by-name "GtkAppChooser")))
  ;; Get the names of the interface properties.
  (is (equal '("content-type")
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GtkAppChooser"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkAppChooser"
                                  GTK-APP-CHOOSER
                                  (:EXPORT T :TYPE-INITIALIZER "gtk_app_chooser_get_type")
                                  (CONTENT-TYPE GTK-APP-CHOOSER-CONTENT-TYPE "content-type" "gchararray" T NIL))
             (get-g-type-definition "GtkAppChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-properties
  (let ((chooser (make-instance 'gtk-app-chooser-button)))
    (is-false (gtk-app-chooser-content-type chooser)))
  (let ((chooser (make-instance 'gtk-app-chooser-button
                                :content-type "plain/text")))
    (is (string= "plain/text" (gtk-app-chooser-content-type chooser)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_get_app_info

(test gtk-app-chooser-app-info
  (let ((chooser (make-instance 'gtk-app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk-app-chooser-app-info chooser))))

;;;     gtk_app_chooser_refresh

(test gtk-app-chooser-refresh
  (let ((chooser (make-instance 'gtk-app-chooser-button
                                :content-type "plain/text")))
    (is-false (gtk-app-chooser-refresh chooser))))

