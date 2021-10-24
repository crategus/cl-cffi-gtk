(def-suite gtk-level-bar :in gtk-suite)
(in-suite gtk-level-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLevelBarMode

(test gtk-level-bar-mode
  ;; Check the type
  (is (g-type-is-enum "GtkLevelBarMode"))
  ;; Check the type initializer
  (is (eq (gtype "GtkLevelBarMode")
          (gtype (foreign-funcall "gtk_level_bar_mode_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-level-bar-mode
          (registered-enum-type "GtkLevelBarMode")))
  ;; Check the names
  (is (equal '("GTK_LEVEL_BAR_MODE_CONTINUOUS" "GTK_LEVEL_BAR_MODE_DISCRETE")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkLevelBarMode"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkLevelBarMode"))))
  ;; Check the nick names
  (is (equal '("continuous" "discrete")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkLevelBarMode"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkLevelBarMode"
                             GTK-LEVEL-BAR-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_level_bar_mode_get_type")
                             (:CONTINUOUS 0)
                             (:DISCRETE 1))
             (get-g-type-definition "GtkLevelBarMode"))))

;;;     GtkLevelBar

(test gtk-level-bar-class
  ;; Type check
  (is (g-type-is-object "GtkLevelBar"))
  ;; Check the registered name
  (is (eq 'gtk-level-bar
          (registered-object-type-by-name "GtkLevelBar")))
  ;; Check the type initializer
  (is (eq (gtype "GtkLevelBar")
          (gtype (foreign-funcall "gtk_level_bar_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkLevelBar")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkLevelBar"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (mapcar #'g-type-name (g-type-interfaces "GtkLevelBar"))))
  ;; Check the class properties
  (is (equal '("inverted" "max-value" "min-value" "mode" "orientation" "value")
             (list-class-property-names "GtkLevelBar")))
  ;; Check the style properties.
  (is (equal '("min-block-height" "min-block-width")
             (list-class-style-property-names "GtkLevelBar")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkLevelBar" GTK-LEVEL-BAR
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_level_bar_get_type")
                       ((INVERTED GTK-LEVEL-BAR-INVERTED "inverted" "gboolean"
                         T T)
                        (MAX-VALUE GTK-LEVEL-BAR-MAX-VALUE "max-value"
                         "gdouble" T T)
                        (MIN-VALUE GTK-LEVEL-BAR-MIN-VALUE "min-value"
                         "gdouble" T T)
                        (MODE GTK-LEVEL-BAR-MODE "mode" "GtkLevelBarMode" T T)
                        (VALUE GTK-LEVEL-BAR-VALUE "value" "gdouble" T T)))
             (get-g-type-definition "GtkLevelBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-level-bar-properties
  (let ((level-bar (make-instance 'gtk-level-bar)))
    ;; inverted
    (is-false (gtk-level-bar-inverted level-bar))
    (is-true (setf (gtk-level-bar-inverted level-bar) t))
    (is-true (gtk-level-bar-inverted level-bar))
    ;; max-value
    (is (= 1.0d0 (gtk-level-bar-max-value level-bar)))
    (is (= 5.0d0 (setf (gtk-level-bar-max-value level-bar) 5.0)))
    (is (= 5.0d0 (gtk-level-bar-max-value level-bar)))
    ;; min-value
    (is (= 0.0d0 (gtk-level-bar-min-value level-bar)))
    (is (= 1.0d0 (setf (gtk-level-bar-min-value level-bar) 1.0)))
    (is (= 1.0d0 (gtk-level-bar-min-value level-bar)))
    ;; mode
    (is (eq :continuous (gtk-level-bar-mode level-bar)))
    (is (eq :discrete (setf (gtk-level-bar-mode level-bar) :discrete)))
    (is (eq :discrete (gtk-level-bar-mode level-bar)))
    ;; value (not 0,0d0 because the min-value is 1.0d0)
    (is (= 1.0d0 (gtk-level-bar-value level-bar)))
    (is (= 2.0d0 (setf (gtk-level-bar-value level-bar) 2.0)))
    (is (= 2.0d0 (gtk-level-bar-value level-bar)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-level-bar-style-properties
  (let ((level-bar (make-instance 'gtk-level-bar)))
    (is (= 3 (gtk-widget-style-property level-bar "min-block-height")))
    (is (= 3 (gtk-widget-style-property level-bar "min-block-width")))))

;;; --- Signals ----------------------------------------------------------------

(defvar *verbose-gtk-level-bar* nil)

#+nil
(test gtk-level-bar-offset-changed-signal
  (let* ((message nil)
         (level-bar (gtk-level-bar-new-for-interval 0.0 10.0))
         ;; Connect a signal handler
         (handler-id (g-signal-connect level-bar "offset-changed"
                       (lambda (widget name)
                         (declare (ignore widget))
                         (when *verbose-gtk-level-bar*
                           (format t "~&Signal 'offset-changed' for level bar.~%"))
                         (is (string= "high" name))
                         (is (= 0.75 (gtk-level-bar-offset-value level-bar name)))
                         (setf message "Signal 'offset-changed' for level bar")
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (is-false (g-signal-emit level-bar "offset-changed" "high"))
    (is (string= "Signal 'offset-changed' for level bar" message))
    (is-false (g-signal-handler-disconnect level-bar handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_level_bar_new

(test gtk-level-bar-new
  (is (eq 'gtk-level-bar (type-of (gtk-level-bar-new)))))

;;;     gtk_level_bar_new_for_interval

(test gtk-level-bar-new-for-interval
  (let ((level-bar (gtk-level-bar-new-for-interval 1.0 2.0)))
    (is (= 1.0d0 (gtk-level-bar-min-value level-bar)))
    (is (= 2.0d0 (gtk-level-bar-max-value level-bar)))))

;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value

(test gtk-level-bar-add-offset-value
  (let ((level-bar (gtk-level-bar-new-for-interval 0.0 10.0)))
    (is-false (gtk-level-bar-add-offset-value level-bar "half" 0.5))
    (is (= 0.5d0 (gtk-level-bar-offset-value level-bar "half")))
    (is-false (gtk-level-bar-remove-offset-value level-bar "half"))
    (is-false (gtk-level-bar-offset-value level-bar "half"))))

;;; 2021-10-19
