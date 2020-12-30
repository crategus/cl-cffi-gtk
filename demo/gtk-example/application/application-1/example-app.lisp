
(in-package :gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *debug-subclass* t))

;(defclass example-app (gtk-application)
;  ()
;  (:metaclass gobject-class)
;  (:g-type-name . "ExampleApp"))

(trace gobject::make-instance)
(trace gobject::class-init)
(trace gobject::instance-init)
(trace gobject::install-properties)
(trace gobject::%g-object-class-install-property)

(trace gobject::G-OBJECT-PROPERTY)
(trace gobject::object-property-get)


(define-foreign-g-object-class "ExampleApp" example-app
  (:superclass gtk-application
   :export t
   :interfaces nil
   :type-initializer nil)
  ((count
    example-app-count
    "count" "gint" t t)))

;(eval-when (:compile-toplevel :load-toplevel :execute)
;(register-object-type-implementation "ExampleApp"
;                                     example-app
;                                     "GtkApplication"
;                                     nil
;                                     nil))

