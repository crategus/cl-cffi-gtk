;;;; File Chooser Button

(in-package :gtk-tutorial)

(defun show-file-chooser-info (label chooser)
  (let ((filename (gtk-file-chooser-filename chooser))
        (uri (gtk-file-chooser-uri chooser))
        (current-folder (gtk-file-chooser-current-folder chooser))
        (current-folder-uri (gtk-file-chooser-current-folder-uri chooser))
        (preview-filename (gtk-file-chooser-preview-filename chooser))
        (preview-uri (gtk-file-chooser-preview-uri chooser)))
    (setf (gtk-label-label label)
          (format nil "<small><tt>~{~20@a ~@a~%~}</tt></small>"
                      (list "Filename :" filename
                            "URI :" uri
                            "Current Folder :" current-folder
                            "Current Folder URI :" current-folder-uri
                            "Preview Filename :" preview-filename
                            "Preview URI :" preview-uri)))))

(defun example-file-chooser-button ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example File Chooser Button"
                                  :type :toplevel
                                  :border-width 24
                                  :default-width 300
                                  :default-height 100))
           (info-box (make-instance 'gtk-box
                                    :orientation :vertical
                                    :valign :start
                                    :spacing 12))
           (info-label (make-instance 'gtk-label
                                      :use-markup t))
           (filter-all (make-instance 'gtk-file-filter))
           (filter-image (make-instance 'gtk-file-filter))
           (filter-text (make-instance 'gtk-file-filter))
           (hbox (make-instance 'gtk-box
                                :orientation :horizontal
                                :expand nil
                                :spacing 36
                                :halign :start))
           (vbox (make-instance 'gtk-box
                                :orientation :vertical
                                :expand nil
                                :spacing 6
                                :valign :start))
           (button-folder (make-instance 'gtk-file-chooser-button
                                         :action :select-folder))
           (button-open (make-instance 'gtk-file-chooser-button
                                       :action :open)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect button-folder "file-set"
                        (lambda (chooser)
                          (show-file-chooser-info info-label chooser)
                          (setf (gtk-file-chooser-filename button-open)
                                (gtk-file-chooser-filename chooser))))

      (g-signal-connect button-open "file-set"
                        (lambda (chooser)
                          (show-file-chooser-info info-label chooser)
                          (setf (gtk-file-chooser-filename button-folder)
                                (gtk-file-chooser-current-folder chooser))))
      ;; Set some filters on the file chooser
      (setf (gtk-file-filter-name filter-all) "All Files")
      (gtk-file-filter-add-pattern filter-all "*")
      (gtk-file-chooser-add-filter button-open filter-all)
      ;; Filter for Image Files
      (setf (gtk-file-filter-name filter-image) "Image Files")
      (gtk-file-filter-add-mime-type filter-image "image/*")
      (gtk-file-chooser-add-filter button-open filter-image)
      ;; Filter for Text Files
      (setf (gtk-file-filter-name filter-text) "Text Files")
      (gtk-file-filter-add-mime-type filter-text "text/*")
      (gtk-file-chooser-add-filter button-open filter-text)
      ;; Pack and show the widgets
      (gtk-box-pack-start vbox (make-instance 'gtk-label
                                             :label "<b>Select Folder</b>"
                                             :use-markup t
                                             :halign :start))
      (gtk-box-pack-start vbox button-folder)
      (gtk-box-pack-start vbox (make-instance 'gtk-label
                                             :label "<b>Select File</b>"
                                             :use-markup t
                                             :margin-top 6
                                             :halign :start))
      (gtk-box-pack-start vbox button-open)
      (gtk-box-pack-start hbox vbox)
      (gtk-box-pack-start info-box
                          (make-instance 'gtk-label
                                         :use-markup t
                                         :label
                                         "<b>Selected Folder and File</b>"))
      (gtk-box-pack-start info-box info-label)
      (gtk-box-pack-start hbox info-box)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
