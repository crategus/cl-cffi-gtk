(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Type check
  (is (g-type-is-object "GtkSpinner"))
  ;; Check the registered name
  (is (eq 'gtk-spinner
          (registered-object-type-by-name "GtkSpinner")))
  ;; Check the type initializer
  (is (eq (gtype "GtkSpinner")
          (gtype (foreign-funcall "gtk_spinner_get_type" g-type))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkSpinner")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkSpinner"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkSpinner"))))
  ;; Check the class properties
  (is (equal '("active")
             (list-class-property-names "GtkSpinner")))
  ;; Check the style properties.
  (is (equal '()
             (list-class-style-property-names "GtkSpinner")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSpinner" GTK-SPINNER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((ACTIVE GTK-SPINNER-ACTIVE "active" "gboolean" T T)))
             (get-g-type-definition "GtkSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-spinner-properties
  (let ((spinner (make-instance 'gtk-spinner)))
    (is-false (gtk-spinner-active spinner))
    (is-true (setf (gtk-spinner-active spinner) t))
    (is-true (gtk-spinner-active spinner))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spinner_new

(test gtk-spinner-new
  (is (eq 'gtk-spinner (type-of (gtk-spinner-new)))))

;;;     gtk_spinner_start
;;;     gtk_spinner_end

(test gtk-spinner-start
  (let ((spinner (gtk-spinner-new)))
    (is-false (gtk-spinner-active spinner))
    (is-false (gtk-spinner-start spinner))
    (is-true (gtk-spinner-active spinner))
    (is-false (gtk-spinner-stop spinner))
    (is-false (gtk-spinner-active spinner))))

;;; 2021-10-19
