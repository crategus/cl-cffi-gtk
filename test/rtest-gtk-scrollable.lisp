(def-suite gtk-scrollable :in gtk-suite)
(in-suite gtk-scrollable)

;;;     GtkScrollablePolicy

(test gtk-scrollable-policy
  ;; Check the type
  (is-true (g-type-is-enum "GtkScrollablePolicy"))
  ;; Check the registered name
  (is (eql 'gtk-scrollable-policy (gobject::registered-enum-type "GtkScrollablePolicy")))
  ;; Check the names
  (is (equal '("GTK_SCROLL_MINIMUM" "GTK_SCROLL_NATURAL")
             (mapcar #'gobject::enum-item-name
                     (gobject::get-enum-items "GtkScrollablePolicy"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'gobject::enum-item-value
                     (gobject::get-enum-items "GtkScrollablePolicy"))))
  ;; Check the nick names
  (is (equal '("minimum" "natural")
             (mapcar #'gobject::enum-item-nick
                     (gobject::get-enum-items "GtkScrollablePolicy"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkScrollablePolicy"
                              GTK-SCROLLABLE-POLICY
                  (:EXPORT T :TYPE-INITIALIZER "gtk_scrollable_policy_get_type")
                  (:MINIMUM 0)
                  (:NATURAL 1))
             (gobject::get-g-type-definition "GtkScrollablePolicy"))))

;;;     GtkScrollable

(test gtk-scrollable-interface
  ;; Type check
  (is-true (g-type-is-interface "GtkScrollable"))
  ;; Check the registered name
  (is (eq 'gtk-scrollable
          (registered-object-type-by-name "GtkScrollable")))
  ;; Get the names of the interface properties.
  (is (equal '("hadjustment" "hscroll-policy" "vadjustment" "vscroll-policy")
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GtkScrollable"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkScrollable"
                                  GTK-SCROLLABLE
      (:EXPORT T :TYPE-INITIALIZER "gtk_scrollable_get_type")
      (HADJUSTMENT GTK-SCROLLABLE-HADJUSTMENT "hadjustment" "GtkAdjustment" T T)
      (HSCROLL-POLICY GTK-SCROLLABLE-HSCROLL-POLICY "hscroll-policy"
                                                    "GtkScrollablePolicy" T T)
      (VADJUSTMENT GTK-SCROLLABLE-VADJUSTMENT "vadjustment" "GtkAdjustment" T T)
      (VSCROLL-POLICY GTK-SCROLLABLE-VSCROLL-POLICY "vscroll-policy"
                                                    "GtkScrollablePolicy" T T))
             (get-g-type-definition "GtkScrollable"))))

;;; --- gtk-scrollable-properties ----------------------------------------------

(test gtk-scrollable-properties
  (let ((scrollable (make-instance 'gtk-layout)))
    (is (eq 'gtk-adjustment (type-of (gtk-scrollable-hadjustment scrollable))))
    (is (eq 'gtk-adjustment (type-of (gtk-scrollable-vadjustment scrollable))))
    (is (eq :minimum (gtk-scrollable-hscroll-policy scrollable)))
    (is (eq :minimum (gtk-scrollable-vscroll-policy scrollable)))))

;;; --- gtk-scrollable-border --------------------------------------------------

(test gtk-scrollable-border
  (let ((scrollable (make-instance 'gtk-layout)))
    (is (eq 'gtk-border (type-of (gtk-scrollable-border scrollable))))))

