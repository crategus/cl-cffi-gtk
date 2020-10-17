(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Type check
  (is-true  (g-type-is-object "GtkSpinner"))
  ;; Check the registered name
  (is (eq 'gtk-spinner
          (registered-object-type-by-name "GtkSpinner")))
  ;; Check the type initializer
  (is (string= "GtkSpinner"
               (g-type-name (gtype (foreign-funcall "gtk_spinner_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkWidget") (g-type-parent "GtkSpinner")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkSpinner"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkSpinner"))))
  ;; Check the class properties
  (is (equal '("active" "app-paintable" "can-default" "can-focus" "composite-child"
               "double-buffered" "events" "expand" "focus-on-click" "halign" "has-default"
               "has-focus" "has-tooltip" "height-request" "hexpand" "hexpand-set" "is-focus"
               "margin" "margin-bottom" "margin-end" "margin-left" "margin-right"
               "margin-start" "margin-top" "name" "no-show-all" "opacity" "parent"
               "receives-default" "scale-factor" "sensitive" "style" "tooltip-markup"
               "tooltip-text" "valign" "vexpand" "vexpand-set" "visible" "width-request"
               "window")
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkSpinner"))
                          #'string-lessp)))
  ;; Check the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging")
             (mapcar #'g-param-spec-name
                     (gtk-widget-class-list-style-properties "GtkSpinner"))))
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

