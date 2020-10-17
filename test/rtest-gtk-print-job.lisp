(def-suite gtk-print-job :in gtk-suite)
(in-suite gtk-print-job)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintJob

(test gtk-print-job-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPrintJob"))
  ;; Check the registered name
  (is (eq 'gtk-print-job
          (registered-object-type-by-name "GtkPrintJob")))
  ;; Check the type initializer
  (is (string= "GtkPrintJob"
               (g-type-name (gtype (foreign-funcall "gtk_print_job_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkPrintJob")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkPrintJob"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkPrintJob"))))
  ;; Check the class properties
  (is (equal '("page-setup" "printer" "settings" "title" "track-print-status")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkPrintJob"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintJob" GTK-PRINT-JOB
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_job_get_type")
                       ((PAGE-SETUP GTK-PRINT-JOB-PAGE-SETUP "page-setup"
                         "GtkPageSetup" T NIL)
                        (PRINTER GTK-PRINT-JOB-PRINTER "printer" "GtkPrinter" T
                         NIL)
                        (SETTINGS GTK-PRINT-JOB-SETTINGS "settings"
                         "GtkPrintSettings" T NIL)
                        (TITLE GTK-PRINT-JOB-TITLE "title" "gchararray" T NIL)
                        (TRACK-PRINT-STATUS GTK-PRINT-JOB-TRACK-PRINT-STATUS
                         "track-print-status" "gboolean" T T)))
             (get-g-type-definition "GtkPrintJob"))))

;;; --- Properties -------------------------------------------------------------

;; TODO: We cannot create an instance of GTKPrintJob.

;(test gtk-print-job-properties
;  (let ((job (make-instance 'gtk-print-job
;                            :page-setup (gtk-page-setup-new)
;                            :printer (gtk-printer-new "Printer" nil nil)
;                            :settings (gtk-print-settings-new)
;                            :title "Print Job"
;                            :track-print-status t)))
    ;; page-setup
;    (is-false (gtk-print-job-page-setup job))
    ;; printer
;    (is-false (gtk-print-job-printer job))
    ;; settings
;    (is-false (gtk-print-job-settings job))
    ;; title
;    (is-false (gtk-print-job-title job))
    ;; track-print-status
;    (is-false (gtk-print-job-track-print-status job))
;))


;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_job_new
;;;     gtk_print_job_get_settings
;;;     gtk_print_job_get_printer
;;;     gtk_print_job_get_title
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_set_track_print_status
;;;     gtk_print_job_get_track_print_status
;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages
;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges
;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set
;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies
;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale
;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up
;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout
;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate
;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate
;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse

