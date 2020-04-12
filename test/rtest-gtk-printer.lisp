(def-suite gtk-printer :in gtk-suite)
(in-suite gtk-printer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrinter

(test gtk-printer-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPrinter"))
  ;; Check the registered name
  (is (eq 'gtk-printer
          (registered-object-type-by-name "GtkPrinter")))
  ;; Check the type initializer
  (is (string= "GtkPrinter"
               (g-type-name (gtype (foreign-funcall "gtk_printer_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkPrinter")))
  ;; Check the children
  (is (equal '("GtkPrinterCups" "GtkPrinterCloudprint")
             (mapcar #'gtype-name (g-type-children "GtkPrinter"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkPrinter"))))
  ;; Check the class properties
  (is (equal '("accepting-jobs" "accepts-pdf" "accepts-ps" "backend" "icon-name" "is-virtual"
               "job-count" "location" "name" "paused" "state-message")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkPrinter"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrinter" GTK-PRINTER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_printer_get_type")
                       ((ACCEPTING-JOBS GTK-PRINTER-ACCEPTING-JOBS
                         "accepting-jobs" "gboolean" T NIL)
                        (ACCEPTS-PDF GTK-PRINTER-ACCEPTS-PDF "accepts-pdf"
                         "gboolean" T NIL)
                        (ACCEPTS-PS GTK-PRINTER-ACCEPTS-PS "accepts-ps"
                         "gboolean" T NIL)
                        (BACKEND GTK-PRINTER-BACKEND "backend"
                         "GtkPrintBackend" T NIL)
                        (ICON-NAME GTK-PRINTER-ICON-NAME "icon-name"
                         "gchararray" T NIL)
                        (IS-VIRTUAL GTK-PRINTER-IS-VIRTUAL "is-virtual"
                         "gboolean" T NIL)
                        (JOB-COUNT GTK-PRINTER-JOB-COUNT "job-count" "gint" T
                         NIL)
                        (LOCATION GTK-PRINTER-LOCATION "location" "gchararray"
                         T NIL)
                        (NAME GTK-PRINTER-NAME "name" "gchararray" T NIL)
                        (PAUSED GTK-PRINTER-PAUSED "paused" "gboolean" T NIL)
                        (STATE-MESSAGE GTK-PRINTER-STATE-MESSAGE
                         "state-message" "gchararray" T NIL)))
             (get-g-type-definition "GtkPrinter"))))

;;;     GtkPrintBackend

(test gtk-print-backend-class
  ;; Type check
  (is-true  (g-type-is-object "GtkPrintBackend"))
  ;; Check the registered name
  (is (eq 'gtk-print-backend
          (registered-object-type-by-name "GtkPrintBackend")))
  ;; Check the type initializer
  (is (string= "GtkPrintBackend"
               (g-type-name (gtype (foreign-funcall "gtk_print_backend_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GObject") (g-type-parent "GtkPrintBackend")))
  ;; Check the children
  (is (equal '("GtkPrintBackendFile" "GtkPrintBackendCups" "GtkPrintBackendCloudprint")
             (mapcar #'gtype-name (g-type-children "GtkPrintBackend"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'gtype-name (g-type-interfaces "GtkPrintBackend"))))
  ;; Check the class properties
  (is (equal '("status")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkPrintBackend"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintBackend" GTK-PRINT-BACKEND
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_backend_get_type")
                       ((STATUS GTK-PRINT-BACKEND-STATUS "status" "gint" T T)))
             (get-g-type-definition "GtkPrintBackend"))))

;;; --- Properties -------------------------------------------------------------

;;;        gboolean   accepting-jobs      Read
;;;        gboolean   accepts-pdf         Read / Write / Construct Only
;;;        gboolean   accepts-ps          Read / Write / Construct Only
;;; GtkPrintBackend*  backend             Read / Write / Construct Only
;;;           gchar*  icon-name           Read
;;;        gboolean   is-virtual          Read / Write / Construct Only
;;;            gint   job-count           Read
;;;           gchar*  location            Read
;;;           gchar*  name                Read / Write / Construct Only
;;;        gboolean   paused              Read
;;;           gchar*  state-message       Read

(test gtk-printer-properties
  (let* ((backend (make-instance 'gtk-print-backend))
         (printer (make-instance 'gtk-printer
                                 :name "myPrinter"
                                 :backend backend
                                 :is-virtual t)))
    ;; accepting-jobs
    (is-true (gtk-printer-accepting-jobs printer))
    (signals (error) (setf (gtk-printer-accepting-jobs printer) nil))
    ;; accepts-pdf
    (is-false (gtk-printer-accepts-pdf printer))
    ;; accepts-ps
    (is-true (gtk-printer-accepts-ps printer))
    ;; backend
    (is (eq 'gtk-print-backend (type-of (gtk-printer-backend printer))))
    ;; icon-name
    (is (string= "printer" (gtk-printer-icon-name printer)))
    (signals (error) (setf (gtk-printer-icon-name printer) nil))
    ;; is-virtual
    (is-true (gtk-printer-is-virtual printer))
    ;; job-count
    (is (= 0 (gtk-printer-job-count printer)))
    (signals (error) (setf (gtk-printer-job-count printer) 10))
    ;; location
    (is (string= "" (gtk-printer-location printer)))
    (signals (error) (setf (gtk-printer-location printer) "test"))
    ;; name
    (is (string= "myPrinter" (gtk-printer-name printer)))
    ;; paused
    (is-false (gtk-printer-paused printer))
    (signals (error) (setf (gtk-printer-paused printer) t))
    ;; state-message
    (is (string= "" (gtk-printer-state-message printer)))
    (signals (error) (setf (gtk-printer-state-message printer) "test"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_printer_new
;;;     gtk_printer_get_description
;;;     gtk_printer_is_active
;;;     gtk_printer_is_paused
;;;     gtk_printer_is_accepting_jobs
;;;     gtk_printer_is_virtual
;;;     gtk_printer_is_default
;;;     gtk_printer_accepts_ps
;;;     gtk_printer_accepts_pdf
;;;     gtk_printer_list_papers
;;;     gtk_printer_compare
;;;     gtk_printer_has_details
;;;     gtk_printer_request_details
;;;     gtk_printer_get_capabilities
;;;     gtk_printer_get_default_page_size
;;;     gtk_printer_get_hard_margins
;;;     gtk_enumerate_printers

