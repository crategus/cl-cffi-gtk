;;;; Example Text Entry Completion
;;;;
;;;; GtkEntryCompletion provides a mechanism for adding support for completion
;;;; in GtkEntry.

(in-package :gtk-example)

(defun create-completion-model ()
  (let ((store (make-instance 'gtk-list-store
                              :column-types '("gchararray"))))
    (gtk-list-store-set store (gtk-list-store-append store) "Gnome")
    (gtk-list-store-set store (gtk-list-store-append store) "total")
    (gtk-list-store-set store (gtk-list-store-append store) "totally")
    store))

;; TODO: There is a problem with a popup-completion
;; The popup does not get the correct height to show the completions.

(defun example-text-entry-completion ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Entry Buffer"
                                 :border-width 12
                                 :default-width 400))
          (hbox (make-instance 'gtk-grid
                               :orientation :horizontal))
          (vbox (make-instance 'gtk-grid
                               :orientation :vertical)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add vbox
                         (make-instance 'gtk-label
                                        :label "<b>Entry with Completion</b>"
                                        :halign :start
                                        :margin-bottom 3
                                        :use-markup t))
      (let* (;; Create the completion object
             (completion (make-instance 'gtk-entry-completion
                                        :text-column 0
                                        :inline-completion t
                                        :popup-completion nil
                                        :model (create-completion-model)))
             ;; Create the entry with a completion
             (entry (make-instance 'gtk-entry
                                   :completion completion)))
        (gtk-container-add vbox entry))
      (gtk-container-add hbox vbox)
      (gtk-container-add hbox
                         (make-instance 'gtk-label
                                        :valign :start
                                        :use-markup t
                                        :margin-top 12
                                        :margin-left 12
                                        :label
                                        (format nil
                                                "Try writing <b>total</b> or ~%~
                                                 <b>Gnome</b> for example.~%~%")))
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
