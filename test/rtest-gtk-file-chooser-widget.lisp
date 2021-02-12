(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

;;; --- Properties -------------------------------------------------------------

;;;     gboolean   search-mode                Read / Write
;;;        gchar*  subtitle                   Read

(test gtk-file-chooser-widget-properties.1
  (let ((chooser (make-instance 'gtk-file-chooser-widget)))
    (is-false (gtk-file-chooser-widget-search-mode chooser))
    (is-false (gtk-file-chooser-widget-subtitle chooser))))

(test gtk-file-chooser-widget-properties.2
  (let ((chooser (make-instance 'gtk-file-chooser-widget
                                :search-mode t)))
    (is-true (gtk-file-chooser-widget-search-mode chooser))
    (is (string= "Suchen" (gtk-file-chooser-widget-subtitle chooser)))))

;;; --- Signals ----------------------------------------------------------------

;;;         void   desktop-folder             Action
;;;         void   down-folder                Action
;;;         void   home-folder                Action
;;;         void   location-popup             Action
;;;         void   location-popup-on-paste    Action
;;;         void   location-toggle-popup      Action
;;;         void   places-shortcut            Action
;;;         void   quick-bookmark             Action
;;;         void   recent-shortcut            Action
;;;         void   search-shortcut            Action
;;;         void   show-hidden                Action
;;;         void   up-folder                  Action

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_widget_new

(test gtk-file-chooser-widget-new
  (is (typep (gtk-file-chooser-widget-new :open) 'gtk-file-chooser-widget))
  (is (typep (gtk-file-chooser-widget-new :save) 'gtk-file-chooser-widget)))

;;; 2021-1-26
