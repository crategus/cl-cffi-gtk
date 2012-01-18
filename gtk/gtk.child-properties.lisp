(in-package :gtk)

(defcfun gtk-container-child-get-property :void
  (container g-object)
  (child g-object)
  (property-name :string)
  (value (:pointer g-value)))

(export 'gtk-container-child-get-property)

(defcfun gtk-container-child-set-property :void
  (container g-object)
  (child g-object)
  (property-name :string)
  (value (:pointer g-value)))

(export 'gtk-container-child-set-property)

(defcfun gtk-container-class-find-child-property :pointer
  (class :pointer)
  (property-name :string))

(export 'gtk-container-class-find-child-property)

(defun container-child-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
         (let ((g-param-spec (gtk-container-class-find-child-property class property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'container-child-property-info)

(defun container-call-get-property (container child property-name type)
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue (gtype type))
    (gtk-container-child-get-property container child property-name gvalue)
    (prog1 (parse-g-value gvalue)
      (g-value-unset gvalue))))

(defun container-call-set-property (container child property-name new-value type)
  (with-foreign-object (gvalue 'g-value)
    (set-g-value gvalue new-value (gtype type) :zero-g-value t)
    (gtk-container-child-set-property container child property-name gvalue)
    (g-value-unset gvalue)
    (values)))

(export '(container-call-get-property container-call-set-property))

(defmacro define-child-property (container-type
                                  property-name property-gname
                                  property-type readable writable export)
  (when (stringp container-type)
    (setf container-type (registered-object-type-by-name container-type)))
  `(progn
     ,@(when readable
             (list `(defun ,property-name (container child)
                      (assert (typep container ',container-type))
                      (container-call-get-property container
                                                   child
                                                   ,property-gname
                                                   ,property-type))))
     ,@(when writable
             (list `(defun (setf ,property-name) (new-value container child)
                      (assert (typep container ',container-type))
                      (container-call-set-property container
                                                   child
                                                   ,property-gname
                                                   new-value
                                                   ,property-type))))
     ,@(when export
             (list `(export ',property-name)))))

(defcfun gtk-container-class-list-child-properties (:pointer (:pointer g-param-spec))
  (class (:pointer g-object-class))
  (n-properties (:pointer :int)))

(defun container-class-child-properties (g-type)
  (setf g-type (gtype g-type))
  (let ((g-class (g-type-class-ref g-type)))
    (unwind-protect
         (with-foreign-object (n-properties :uint)
           (let ((params (gtk-container-class-list-child-properties g-class n-properties)))
             (unwind-protect
                  (loop
                     for i from 0 below (mem-ref n-properties :uint)
                     for param = (mem-aref params :pointer i)
                     collect (parse-g-param-spec param))
               (g-free params))))
      (g-type-class-unref g-class))))

(defun child-property-name (type-name property-name package-name)
  (intern (format nil "~A-CHILD-~A" (symbol-name (registered-object-type-by-name type-name)) (string-upcase property-name)) (find-package package-name)))

(defun generate-child-properties (&optional (type-root "GtkContainer") (package-name "GTK"))
  (setf type-root (gtype type-root))
  (append (loop
             for property in (container-class-child-properties type-root)
             collect
               `(define-child-property
                    ,(gtype-name type-root)
                    ,(child-property-name (gtype-name type-root) (g-class-property-definition-name property) package-name)
                  ,(g-class-property-definition-name property)
                  ,(gtype-name (g-class-property-definition-type property))
                  ,(g-class-property-definition-readable property)
                  ,(g-class-property-definition-writable property)
                  t))
          (loop
             for subclass in (g-type-children type-root)
           appending (generate-child-properties subclass package-name))))
