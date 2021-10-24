(def-suite gtk-invisible :in gtk-suite)
(in-suite gtk-invisible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInvisible

(test gtk-invisible-class
  ;; Type check
  (is (g-type-is-object "GtkInvisible"))
  ;; Check the registered name
  (is (eq 'gtk-invisible
          (registered-object-type-by-name "GtkInvisible")))
  ;; Check the type initializer
  (is (eq (gtype "GtkInvisible")
          (gtype (foreign-funcall "gtk_invisible_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkInvisible")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkInvisible"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkInvisible"))))
  ;; Check the class properties
  (is (equal '("screen")
             (list-class-property-names "GtkInvisible")))
  ;; Check the style properties
  (is (equal '()
             (list-class-style-property-names "GtkInvisible")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkInvisible" GTK-INVISIBLE
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_invisible_get_type")
                       ((SCREEN GTK-INVISIBLE-SCREEN "screen" "GdkScreen" T T)))
             (get-g-type-definition "GtkInvisible"))))

;;; --- Properties -------------------------------------------------------------

;;;     GdkScreen*  screen    Read / Write

;;; --- Functions --------------------------------------------------------------

;;;     gtk_invisible_new
;;;     gtk_invisible_new_for_screen
;;;     gtk_invisible_set_screen                           Accessor
;;;     gtk_invisible_get_screen                           Accessor

;;; --- 2021-10-18 -------------------------------------------------------------
