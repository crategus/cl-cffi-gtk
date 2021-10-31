;;; Lisp Utilities for the testsuite

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(let ((eps-factor 1.0d-2))
  (defun approx-equal (x y)
    (or (< (abs (- x y)) eps-factor)
        (< (abs (- x y)) (* eps-factor (max (abs x) (abs y)))))))

;; A sorted list of the class property names without inherited properties
(defun list-class-property-names (gtype)
  (sort (set-difference (mapcar #'g-param-spec-name
                                (g-object-class-list-properties gtype))
                        (mapcar #'g-param-spec-name
                                (g-object-class-list-properties
                                  (g-type-parent gtype)))
                        :test #'string=)
        #'string<))

;; A sorted list of the class style property names without inherited properties
(defun list-class-style-property-names (gtype)
  (sort (set-difference (mapcar #'g-param-spec-name
                                (gtk-widget-class-list-style-properties gtype))
                        (mapcar #'g-param-spec-name
                                (gtk-widget-class-list-style-properties
                                  (g-type-parent gtype)))
                        :test #'string=)
        #'string<))

;; A sorted list of the class child property names
(defun list-class-child-property-names (gtype)
  (sort (mapcar #'g-param-spec-name
                (gtk-container-class-list-child-properties gtype))
        #'string<))

(defun print-foreign-gobjects-weak ()
  (format t "~&List of foreign gobjects WEAK~&")
  (loop for key being the hash-keys of gobject::*foreign-gobjects-weak*
        using (hash-value value)
        do (format t "key: ~a   ~a~%" key value)))

(defun list-foreign-gobjects-weak ()
  (loop for key being the hash-keys of gobject::*foreign-gobjects-weak*
        using (hash-value value)
        collect value))

(defun print-foreign-gobjects-strong ()
  (format t "~&List of foreign gobjects STRONG~%")
  (loop for key being the hash-keys of gobject::*foreign-gobjects-strong*
        using (hash-value value)
        do (format t "key: ~a   ~a~%" key value)))

(defun list-foreign-gobjects-strong ()
  (loop for key being the hash-keys of gobject::*foreign-gobjects-strong*
        using (hash-value value)
        collect value))

(defun print-registered-object-types ()
  (format t "~&List of registered object types~%")
  (loop for key being the hash-keys of gobject::*registered-object-types*
        using (hash-value value)
        do (format t "key: ~a   ~a~%" key value)))

;;; 2021-10-29
