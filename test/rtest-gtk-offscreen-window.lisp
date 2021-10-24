(def-suite gtk-offscreen-window :in gtk-suite)
(in-suite gtk-offscreen-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOffscreenWindow

(test gtk-container-class
  ;; Type check
  (is (g-type-is-object "GtkOffscreenWindow"))
  ;; Check the registered name
  (is (eq 'gtk-offscreen-window
          (registered-object-type-by-name "GtkOffscreenWindow")))
  ;; Check the type initializer
  (is (eq (gtype "GtkOffscreenWindow")
          (gtype (foreign-funcall "gtk_offscreen_window_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWindow")
          (g-type-parent "GtkOffscreenWindow")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkOffscreenWindow"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkOffscreenWindow"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkOffscreenWindow")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-class-style-property-names "GtkOffscreenWindow")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkOffscreenWindow")))
  ;; Check the class definition
  (is (equal '()
             (get-g-type-definition "GtkOffscreenWindow"))))

;;; --- Functions --------------------------------------------------------------
;;;
;;;     gtk_offscreen_window_new
;;;     gtk_offscreen_window_get_surface
;;;     gtk_offscreen_window_get_pixbuf

;;; 2021-10-18
