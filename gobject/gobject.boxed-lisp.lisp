;;; ----------------------------------------------------------------------------
;;; gobject.boxed-lisp.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; Garbage Collection for GBoxed objects

(defvar *gboxed-gc-hooks-lock* (make-recursive-lock "gboxed-gc-hooks-lock"))
(defvar *gboxed-gc-hooks* nil) ; pointers to objects to be freed

(defun activate-gboxed-gc-hooks ()
  (with-recursive-lock-held (*gboxed-gc-hooks-lock*)
    (when *gboxed-gc-hooks*
      (log-for :gc "activating gc hooks for boxeds: ~A~%" *gboxed-gc-hooks*)
      (loop
         for (pointer type) in *gboxed-gc-hooks*
         do (boxed-free-fn type pointer))
      (setf *gboxed-gc-hooks* nil))))

;(defcallback gboxed-idle-gc-hook :boolean ((data :pointer))
;  (declare (ignore data))
;  (activate-gboxed-gc-hooks)
;  nil)

(defun register-gboxed-for-gc (type pointer)
  (with-recursive-lock-held (*gboxed-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gboxed-gc-hooks*))))
      (push (list pointer type) *gboxed-gc-hooks*)
      (unless locks-were-present
        (log-for :gc "adding gboxed idle-gc-hook to main loop~%")
        (g-idle-add #'activate-gboxed-gc-hooks)))))
;        (glib::%g-idle-add (callback gboxed-idle-gc-hook) (null-pointer))))))

;;; ----------------------------------------------------------------------------

;; Global Hash-Table to store the structure info for a gtype

(defvar *g-type-name->g-boxed-foreign-info* (make-hash-table :test 'equal))

;;; ----------------------------------------------------------------------------

;; Get the structure info for a name from the property list

(defun get-g-boxed-foreign-info (name)
  (get name 'g-boxed-foreign-info))

;; Get the structure info for a gtype from a Hash-Table
;; e.g. (get-g-boxed-foreign-info-for-gtype (gtype "GdkCursor"))

(defun get-g-boxed-foreign-info-for-gtype (type)
  (or (gethash (gtype-name (gtype type))
               *g-type-name->g-boxed-foreign-info*)
      (error "Unknown GBoxed type '~A'"
             (gtype-name (gtype type)))))

;;; ----------------------------------------------------------------------------

;; Helper functions to create an internal symbol

(defun generated-cstruct-name (symbol)
  (intern (format nil "~A-CSTRUCT" (symbol-name symbol))
          (symbol-package symbol)))

(defun generated-cunion-name (symbol)
  (intern (format nil "~A-CUNION" (symbol-name symbol))
          (symbol-package symbol)))

;;; ----------------------------------------------------------------------------

;; Helper function used by the method boxed-copy-fn

(defun memcpy (target source bytes)
  (iter (for i from 0 below bytes)
        (setf (mem-aref target :uchar i)
              (mem-aref source :uchar i))))

;;; ----------------------------------------------------------------------------

;; Define the base type g-boxed-foreign
;;
;; This type is specialized further to:
;;    g-boxed-opaque-foreign-type
;;    g-boxed-cstruct-foreign-type
;;    g-boxed-variant-cstruct-foreign-type

(define-foreign-type g-boxed-foreign-type ()
  ((info :initarg :info
         :accessor g-boxed-foreign-info
         :initform (error "info must be specified"))
   (return-p :initarg :return-p
             :accessor g-boxed-foreign-return-p
             :initform nil))
  (:actual-type :pointer))

(defgeneric make-foreign-type (info &key return-p))

(define-parse-method g-boxed-foreign (name &rest options)
  (let ((info (get-g-boxed-foreign-info name)))
    (assert info nil "Unknown foreign GBoxed type ~A" name)
    (make-foreign-type info
                       :return-p (member :return options))))

(export 'g-boxed-foreign)

;;; ----------------------------------------------------------------------------

;;; A generic function which is implemented by the derived classes

(defgeneric cleanup-translated-object-for-callback (foreign-type
                                                    converted-object
                                                    native-object))

;;; This method is not further implemented and always returns TRUE

(defgeneric has-callback-cleanup (foreign-type))

(defmethod has-callback-cleanup ((type g-boxed-foreign-type))
  t)

;;; ----------------------------------------------------------------------------

(defstruct g-boxed-info
  name
  type)

;;; ----------------------------------------------------------------------------

;; Definition of generic functions to copy and to free a boxed object

(defgeneric boxed-copy-fn (type-info native)
  (:method (type-info native)
           (g-boxed-copy (g-boxed-info-type type-info) native)))

(defgeneric boxed-free-fn (type-info native)
  (:method (type-info native)
           (g-boxed-free (g-boxed-info-type type-info) native)))

;;; ----------------------------------------------------------------------------
;;;
;;; Imlementation of g-boxed-opaque-foreign-type
;;;
;;; ----------------------------------------------------------------------------

(defstruct (g-boxed-opaque-wrapper-info (:include g-boxed-info))
  alloc
  free)

(define-foreign-type boxed-opaque-foreign-type (g-boxed-foreign-type) ())

(defclass g-boxed-opaque ()
  ((pointer :initarg :pointer
            :initform nil
            :accessor g-boxed-opaque-pointer)))

(defgeneric pointer (object))

(defmethod pointer ((object g-boxed-opaque))
  (g-boxed-opaque-pointer object))

(defmethod make-foreign-type ((info g-boxed-opaque-wrapper-info) &key return-p)
  (make-instance 'boxed-opaque-foreign-type
                 :info info
                 :return-p return-p))

(defmethod translate-to-foreign (proxy (type boxed-opaque-foreign-type))
  (if (null proxy)
      (null-pointer)
      (prog1
        (g-boxed-opaque-pointer proxy)
        (when (g-boxed-foreign-return-p type)
          (tg:cancel-finalization proxy)
          (setf (g-boxed-opaque-pointer proxy) nil)))))

(defmethod free-translated-object
    (native (type boxed-opaque-foreign-type) param)
  (declare (ignore native type param)))

(defun make-boxed-free-finalizer (type pointer)
  (lambda () (register-gboxed-for-gc type pointer)))

(defmethod translate-from-foreign
    (native (foreign-type boxed-opaque-foreign-type))
  (let* ((type (g-boxed-foreign-info foreign-type))
         (proxy (make-instance (g-boxed-info-name type)
                               :pointer native)))
    proxy))

(defmethod cleanup-translated-object-for-callback
    ((type boxed-opaque-foreign-type) proxy native)
  (declare (ignore native))
  (tg:cancel-finalization proxy)
  (setf (g-boxed-opaque-pointer proxy) nil))

;;; ----------------------------------------------------------------------------

(defmacro define-g-boxed-opaque (name
                                 gtype
                                 &key (alloc (error "Alloc must be specified")))
  (let ((native-copy (gensym "NATIVE-COPY-"))
        (instance (gensym "INSTANCE-")))
    `(progn
       (defclass ,name (g-boxed-opaque) ())
       (defmethod initialize-instance
                  :after ((,instance ,name) &key &allow-other-keys)
         (unless (g-boxed-opaque-pointer ,instance)
           (let ((,native-copy ,alloc))
             (setf (g-boxed-opaque-pointer ,instance) ,native-copy)
             (finalize ,instance
                       (make-boxed-free-finalizer (get ',name
                                                       'g-boxed-foreign-info)
                                                  ,native-copy)))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'g-boxed-foreign-info)
               (make-g-boxed-opaque-wrapper-info :name ',name
                                                 :type ,gtype)
               (gethash ,gtype *g-type-name->g-boxed-foreign-info*)
               (get ',name 'g-boxed-foreign-info))))))

;;; ----------------------------------------------------------------------------
;;;
;;; Imlementation of g-boxed-cstruct-foreign-type
;;;
;;; ----------------------------------------------------------------------------

(defstruct cstruct-description
  name
  slots)

(defmethod make-load-form ((object cstruct-description) &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; ----------------------------------------------------------------------------

(defstruct (g-boxed-cstruct-wrapper-info (:include g-boxed-info))
  cstruct-description)

;;; ----------------------------------------------------------------------------

(defstruct cstruct-slot-description
  name
  type
  count
  initform
  inline-p)

(defmethod make-load-form ((object cstruct-slot-description)
                           &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; ----------------------------------------------------------------------------

(defstruct (cstruct-inline-slot-description (:include cstruct-slot-description))
  boxed-type-name)

;;; ----------------------------------------------------------------------------

(defmethod make-load-form ((object cstruct-inline-slot-description)
                           &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defclass boxed-cstruct-foreign-type (g-boxed-foreign-type) ())

(defun parse-cstruct-slot (slot)
  (destructuring-bind (name type &key count initform inline) slot
    (if inline
        (make-cstruct-inline-slot-description :name name
                                              :type (list :union (generated-cunion-name type))
                                              :count count
                                              :initform initform
                                              :inline-p inline
                                              :boxed-type-name type)
        (make-cstruct-inline-slot-description :name name
                                              :type type
                                              :count count
                                              :initform initform
                                              :inline-p inline))))

(defun parse-cstruct-definition (name slots)
  (make-cstruct-description :name name
                            :slots (mapcar #'parse-cstruct-slot slots)))

;;; ----------------------------------------------------------------------------

(defmacro define-g-boxed-cstruct (name gtype &body slots)
  (let ((cstruct-description (parse-cstruct-definition name slots)))
    `(progn
       (defstruct ,name
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for initform = (cstruct-slot-description-initform slot))
                 (collect (list name initform))))
       (defcstruct ,(generated-cstruct-name name)
         ,@(iter (for slot in (cstruct-description-slots cstruct-description))
                 (for name = (cstruct-slot-description-name slot))
                 (for type = (cstruct-slot-description-type slot))
                 (for count = (cstruct-slot-description-count slot))
                 (collect `(,name ,type ,@(when count `(:count ,count))))))
       (defcunion ,(generated-cunion-name name)
         (,name (:struct ,(generated-cstruct-name name))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'g-boxed-foreign-info)
               (make-g-boxed-cstruct-wrapper-info :name ',name
                                                  :type ,gtype
                                                  :cstruct-description
                                                  ,cstruct-description)
               (gethash ,gtype *g-type-name->g-boxed-foreign-info*)
               (get ',name 'g-boxed-foreign-info)
               (get ',name 'structure-constructor)
               ',(intern (format nil "MAKE-~A" (symbol-name name))
                         (symbol-package name)))))))

;;; ----------------------------------------------------------------------------

(defmethod make-foreign-type ((info g-boxed-cstruct-wrapper-info) &key return-p)
  (make-instance 'boxed-cstruct-foreign-type
                 :info info
                 :return-p return-p))

(defmethod boxed-copy-fn ((info g-boxed-cstruct-wrapper-info) native)
  (if (g-boxed-info-type info)
      (g-boxed-copy (g-boxed-info-type info) native)
      (let ((copy
             (foreign-alloc (generated-cstruct-name (g-boxed-info-name info)))))
        (memcpy copy
                native
                (foreign-type-size
                  (list :struct (generated-cstruct-name (g-boxed-info-name info)))))
        copy)))

(defmethod boxed-free-fn ((info g-boxed-cstruct-wrapper-info) native)
  (if (g-boxed-info-type info)
      (g-boxed-free (g-boxed-info-type info) native)
      (foreign-free native)))

;;; ----------------------------------------------------------------------------

(defun copy-slots-to-native (proxy native description)
  (iter (with cstruct-type = (generated-cstruct-name
                               (cstruct-description-name description)))
        (for slot in (cstruct-description-slots description))
        (for slot-name = (cstruct-slot-description-name slot))
        (cond
          ((cstruct-slot-description-count slot)
           (iter (with ptr = (foreign-slot-pointer native
                                                   (list :struct cstruct-type)
                                                   slot-name))
                 (with array = (slot-value proxy slot-name))
                 (for i from 0 below (cstruct-slot-description-count slot))
                 (setf (mem-aref ptr (cstruct-slot-description-type slot) i)
                       (aref array i))))
          ((cstruct-slot-description-inline-p slot)
           (let ((info
                  (get-g-boxed-foreign-info
                       (cstruct-inline-slot-description-boxed-type-name slot))))
             (copy-slots-to-native
                      (slot-value proxy slot-name)
                      (foreign-slot-pointer native
                                            (list :struct cstruct-type)
                                            slot-name)
                      (g-boxed-cstruct-wrapper-info-cstruct-description info))))
          (t
           (setf (foreign-slot-value native (list :struct cstruct-type) slot-name)
                 (slot-value proxy slot-name))))))

(defun create-structure (structure-name)
  (let ((constructor (get structure-name 'structure-constructor)))
    (assert constructor nil "Don't know how to create structure of type ~A"
            structure-name)
    (funcall constructor)))

(defun copy-slots-to-proxy (proxy native description)
  (iter (with cstruct-type = (generated-cstruct-name
                               (cstruct-description-name description)))
        (for slot in (cstruct-description-slots description))
        (for slot-name = (cstruct-slot-description-name slot))
        (cond
          ((cstruct-slot-description-count slot)
           (setf (slot-value proxy slot-name)
                 (make-array (list (cstruct-slot-description-count slot))))
           (iter (with ptr = (foreign-slot-pointer native
                                                   (list :struct cstruct-type)
                                                   slot-name))
                 (with array = (slot-value proxy slot-name))
                 (for i from 0 below (cstruct-slot-description-count slot))
                 (setf (aref array i)
                       (mem-aref ptr (cstruct-slot-description-type slot) i))))
          ((cstruct-slot-description-inline-p slot)
           (let ((info (get-g-boxed-foreign-info
                         (cstruct-inline-slot-description-boxed-type-name slot))))
             (setf (slot-value proxy slot-name)
                   (create-structure
                     (cstruct-inline-slot-description-boxed-type-name slot)))
             (copy-slots-to-proxy
                      (slot-value proxy slot-name)
                      (foreign-slot-pointer native
                                            (list :struct cstruct-type)
                                            slot-name)
                      (g-boxed-cstruct-wrapper-info-cstruct-description info))))
          (t (setf (slot-value proxy slot-name)
                   (foreign-slot-value native (list :struct cstruct-type) slot-name))))))

;;; ----------------------------------------------------------------------------

(defmethod translate-to-foreign (proxy (type boxed-cstruct-foreign-type))
  (if (null proxy)
      (null-pointer)
      (let* ((info (g-boxed-foreign-info type))
             (native-struct-type
               (generated-cstruct-name (g-boxed-info-name info))))
        (with-foreign-object (native-struct (list :struct native-struct-type))
          (copy-slots-to-native
                        proxy
                        native-struct
                        (g-boxed-cstruct-wrapper-info-cstruct-description info))
          (values (boxed-copy-fn info native-struct)
                  proxy)))))

(defmethod free-translated-object (native-struct
                                   (type boxed-cstruct-foreign-type)
                                   proxy)
  (when proxy
    (let ((info (g-boxed-foreign-info type)))
      (copy-slots-to-proxy
                        proxy
                        native-struct
                        (g-boxed-cstruct-wrapper-info-cstruct-description info))
      (boxed-free-fn info native-struct))))

(defmethod translate-from-foreign
    (native-struct (type boxed-cstruct-foreign-type))
  (unless (null-pointer-p native-struct)
    (let* ((info (g-boxed-foreign-info type))
           (proxy-struct-type (g-boxed-info-name info))
           (proxy (create-structure proxy-struct-type)))
      (copy-slots-to-proxy
                        proxy
                        native-struct
                        (g-boxed-cstruct-wrapper-info-cstruct-description info))
      (when (g-boxed-foreign-return-p type)
        (boxed-free-fn info native-struct))
      proxy)))

;;; ----------------------------------------------------------------------------

(defmethod cleanup-translated-object-for-callback
    ((type boxed-cstruct-foreign-type) proxy native-structure)
  (when proxy
    (let ((info (g-boxed-foreign-info type)))
      (copy-slots-to-native
                     proxy
                     native-structure
                     (g-boxed-cstruct-wrapper-info-cstruct-description info)))))

;;; ----------------------------------------------------------------------------

(defstruct var-structure
  name
  parent
  slots
  discriminator-slot
  variants
  resulting-cstruct-description)

(defstruct var-structure-variant
  discriminating-values
  structure)

(defmethod make-load-form ((object var-structure) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-variant) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun var-struct-all-slots (struct)
  (when struct
    (append (var-struct-all-slots (var-structure-parent struct))
            (var-structure-slots struct))))

(defun all-structures (structure)
  (append (iter (for variant in (var-structure-variants structure))
                (appending (all-structures (var-structure-variant-structure variant))))
          (list structure)))

(defun parse-variant-structure-definition (name slots &optional parent)
  (iter (with result = (make-var-structure :name name
                                           :parent parent
                                           :slots nil
                                           :discriminator-slot nil
                                           :variants nil))
        (for slot in slots)
        (if (eq :variant (first slot))
            (progn
              (when (var-structure-discriminator-slot result)
                (error "Structure has more than one discriminator slot"))
              (setf (var-structure-discriminator-slot result)
                    (second slot)
                    (var-structure-variants result)
                    (parse-variants result (nthcdr 2 slot))))
            (push (parse-cstruct-slot slot) (var-structure-slots result)))
        (finally (setf (var-structure-slots result)
                       (reverse (var-structure-slots result)))
                 (unless parent
                   (set-variant-result-structure result))
                 (return result))))

(defun set-variant-result-structure (var-structure)
  (setf (var-structure-resulting-cstruct-description var-structure)
        (make-cstruct-description
         :name
         (var-structure-name var-structure)
         :slots
         (append
          (when (var-structure-parent var-structure)
            (cstruct-description-slots
              (var-structure-resulting-cstruct-description
                (var-structure-parent var-structure))))
          (var-structure-slots var-structure))))
  (iter (for variant in (var-structure-variants var-structure))
        (for child-var-structure = (var-structure-variant-structure variant))
        (set-variant-result-structure child-var-structure)))

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun parse-variants (parent variants)
  (iter (for (options variant-name . slots) in variants)
        (for variant =
             (make-var-structure-variant
              :discriminating-values (ensure-list options)
              :structure
              (parse-variant-structure-definition variant-name slots parent)))
        (collect variant)))

(defun generate-cstruct-1 (struct)
  `(defcstruct ,(generated-cstruct-name (cstruct-description-name struct))
     ,@(iter (for slot in (cstruct-description-slots struct))
             (collect `(,(cstruct-slot-description-name slot)
                         ,(cstruct-slot-description-type slot)
                         ,@(when (cstruct-slot-description-count slot)
                             `(:count ,(cstruct-slot-description-count slot))))))))

(defun generate-c-structures (structure)
  (iter (for str in (all-structures structure))
        (for cstruct = (var-structure-resulting-cstruct-description str))
        (collect (generate-cstruct-1 cstruct))))

(defun generate-variant-union (struct)
  `(defcunion ,(generated-cunion-name (var-structure-name struct))
     ,@(iter (for str in (all-structures struct))
             (collect `(,(var-structure-name str)
                         (:struct ,(generated-cstruct-name (var-structure-name str))))))))

(defun generate-structure-1 (str)
  (let ((name (var-structure-name str)))
    `(progn
       (defstruct ,(if (var-structure-parent str)
                       `(,(var-structure-name str)
                          (:include
                            ,(var-structure-name (var-structure-parent str))
                            (,(var-structure-discriminator-slot (var-structure-parent str))
                              ,(first (var-structure-variant-discriminating-values
                                        (find str
                                              (var-structure-variants
                                                (var-structure-parent str))
                                              :key #'var-structure-variant-structure))))))
                       `,(var-structure-name str))
         ,@(iter (for slot in (var-structure-slots str))
                 (collect `(,(cstruct-slot-description-name slot)
                             ,(cstruct-slot-description-initform slot)))))
       (setf (get ',name 'structure-constructor)
             ',(intern (format nil "MAKE-~A" (symbol-name name))
                       (symbol-package name))))))

(defun generate-structures (str)
  (iter (for variant in (reverse (all-structures str)))
        (collect (generate-structure-1 variant))))

(defun generate-native-type-decision-procedure-1 (str proxy-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(var-structure-resulting-cstruct-description str))
      `(typecase ,proxy-var
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-name v-str)
                             ,(generate-native-type-decision-procedure-1 v-str proxy-var))))
         (,(var-structure-name str)
          (values ',(var-structure-resulting-cstruct-description str))))))

(defun generate-proxy-type-decision-procedure-1 (str native-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(var-structure-name str)
               ',(var-structure-resulting-cstruct-description str))
      `(case (foreign-slot-value ,native-var
                                 '(:struct ,(generated-cstruct-name (var-structure-name str)))
                                 ',(var-structure-discriminator-slot str))
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-variant-discriminating-values variant)
                             ,(generate-proxy-type-decision-procedure-1
                               v-str
                               native-var))))
         (t (values ',(var-structure-name str)
                    ',(var-structure-resulting-cstruct-description str))))))

(defun generate-proxy-type-decision-procedure (str)
  (let ((native (gensym "NATIVE-")))
    `(lambda (,native)
       (declare (ignorable ,native))
       ,(generate-proxy-type-decision-procedure-1 str native))))

(defun generate-native-type-decision-procedure (str)
  (let ((proxy (gensym "PROXY-")))
    `(lambda (,proxy)
       (declare (ignorable ,proxy))
       ,(generate-native-type-decision-procedure-1 str proxy))))

(defun compile-proxy-type-decision-procedure (str)
  (compile nil (generate-proxy-type-decision-procedure str)))

(defun compile-native-type-decision-procedure (str)
  (compile nil (generate-native-type-decision-procedure str)))

;;; ----------------------------------------------------------------------------

;; Type boxed-variant-cstruct-foreign-type

(defstruct (g-boxed-variant-cstruct-info (:include g-boxed-info))
  root
  native-type-decision-procedure
  proxy-type-decision-procedure)

(defmethod make-load-form
    ((object g-boxed-variant-cstruct-info) &optional env)
  (make-load-form-saving-slots object :environment env))

(define-foreign-type boxed-variant-cstruct-foreign-type (g-boxed-foreign-type)
  ())

(defmethod make-foreign-type ((info g-boxed-variant-cstruct-info) &key return-p)
  (make-instance 'boxed-variant-cstruct-foreign-type
                   :info info
                   :return-p return-p))

(defmacro define-g-boxed-variant-cstruct (name g-type-name &body slots)
  (let* ((structure (parse-variant-structure-definition name slots)))
    `(progn
      ,@(generate-c-structures structure)
      ,(generate-variant-union structure)
      ,@(generate-structures structure)
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (get ',name 'g-boxed-foreign-info)
                   (make-g-boxed-variant-cstruct-info
                            :name ',name
                            :type ,g-type-name
                            :root ,structure
                            :native-type-decision-procedure
                            ,(generate-native-type-decision-procedure structure)
                            :proxy-type-decision-procedure
                            ,(generate-proxy-type-decision-procedure structure))
                    (gethash ,g-type-name
                              *g-type-name->g-boxed-foreign-info*)
                    (get ',name 'g-boxed-foreign-info))))))

(defun decide-native-type (info proxy)
  (funcall (g-boxed-variant-cstruct-info-native-type-decision-procedure info)
            proxy))

(defmethod boxed-copy-fn ((info g-boxed-variant-cstruct-info) native)
  (if (g-boxed-info-type info)
      (g-boxed-copy (g-boxed-info-type info) native)
      (let ((copy (foreign-alloc
                    (generated-cunion-name (g-boxed-info-name info)))))
        (memcpy copy
                native
                (foreign-type-size
                  (list :struct (generated-cunion-name (g-boxed-info-name info)))))
        copy)))

(defmethod boxed-free-fn ((info g-boxed-variant-cstruct-info) native)
  (if (g-boxed-info-type info)
      (g-boxed-free (g-boxed-info-type info) native)
      (foreign-free native)))

(defmethod translate-to-foreign
    (proxy (foreign-type boxed-variant-cstruct-foreign-type))
  (if (null proxy)
      (null-pointer)
      (let* ((type (g-boxed-foreign-info foreign-type))
             (description (decide-native-type type proxy)))
        (with-foreign-object
            (native-struct (list :union (generated-cunion-name
                             (var-structure-name
                               (g-boxed-variant-cstruct-info-root type)))))
          (copy-slots-to-native proxy native-struct description)
          (values (boxed-copy-fn type native-struct) proxy)))))

(defun decide-proxy-type (info native-struct)
  (funcall (g-boxed-variant-cstruct-info-proxy-type-decision-procedure info)
  native-struct))

(defmethod free-translated-object
    (native (foreign-type boxed-variant-cstruct-foreign-type) proxy)
  (when proxy
    (let ((type (g-boxed-foreign-info foreign-type)))
      (multiple-value-bind (actual-struct cstruct-description)
          (decide-proxy-type type native)
        (unless (eq (type-of proxy) actual-struct)
          (restart-case
              (error "Expected type of boxed variant structure ~A and actual ~
                       type ~A do not match"
                     (type-of proxy) actual-struct)
            (skip-parsing-values () (return-from free-translated-object))))
        (copy-slots-to-proxy proxy native cstruct-description)
        (boxed-free-fn type native)))))

(defmethod translate-from-foreign (native
                              (foreign-type boxed-variant-cstruct-foreign-type))
  (unless (null-pointer-p native)
    (let ((type (g-boxed-foreign-info foreign-type)))
      (multiple-value-bind (actual-struct cstruct-description)
          (decide-proxy-type type native)
        (let ((proxy (create-structure actual-struct)))
          (copy-slots-to-proxy proxy native cstruct-description)
          (when (g-boxed-foreign-return-p foreign-type)
            (boxed-free-fn type native))
          proxy)))))

(defmethod cleanup-translated-object-for-callback
    ((foreign-type boxed-variant-cstruct-foreign-type) proxy native)
  (when proxy
    (let ((type (g-boxed-foreign-info foreign-type)))
      (let ((cstruct-description (decide-native-type type proxy)))
        (copy-slots-to-native proxy native cstruct-description)))))

;;; ----------------------------------------------------------------------------

(defgeneric boxed-parse-g-value (gvalue-ptr info))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type-numeric (eql (gtype "GBoxed")))
                                   parse-kind)
  (declare (ignore parse-kind))
  (if (g-type= (g-value-type gvalue-ptr) (g-type-strv))
      (convert-from-foreign (g-value-get-boxed gvalue-ptr)
                            '(g-strv :free-from-foreign nil))
      (let ((boxed-type
             (get-g-boxed-foreign-info-for-gtype (g-value-type gvalue-ptr))))
        (boxed-parse-g-value gvalue-ptr boxed-type))))

;;; ----------------------------------------------------------------------------

(defgeneric boxed-set-g-value (gvalue-ptr info proxy))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type-numeric (eql (gtype "GBoxed")))
                                value)
  (if (g-type= (g-value-type gvalue-ptr) (g-type-strv))
      (g-value-set-boxed gvalue-ptr
                         (convert-to-foreign value
                                             '(g-strv
                                               :free-from-foreign nil)))
      (let ((boxed-type
             (get-g-boxed-foreign-info-for-gtype (g-value-type gvalue-ptr))))
        (boxed-set-g-value gvalue-ptr boxed-type value))))

;;; ----------------------------------------------------------------------------

(defmethod boxed-parse-g-value (gvalue-ptr (info g-boxed-cstruct-wrapper-info))
  (translate-from-foreign (g-value-get-boxed gvalue-ptr)
                          (make-foreign-type info :return-p nil)))

(defmethod boxed-set-g-value (gvalue-ptr
                              (info g-boxed-cstruct-wrapper-info)
                              proxy)
  (g-value-take-boxed gvalue-ptr
                     (translate-to-foreign proxy
                                           (make-foreign-type info
                                                              :return-p nil))))

(defmethod boxed-parse-g-value (gvalue-ptr (info g-boxed-variant-cstruct-info))
  (translate-from-foreign (g-value-get-boxed gvalue-ptr)
                          (make-foreign-type info :return-p nil)))

(defmethod boxed-set-g-value (gvalue-ptr
                              (info g-boxed-variant-cstruct-info)
                              proxy)
  (g-value-take-boxed gvalue-ptr
                    (translate-to-foreign proxy
                                          (make-foreign-type info
                                                             :return-p nil))))

(defmethod boxed-parse-g-value (gvalue-ptr (info g-boxed-opaque-wrapper-info))
  (translate-from-foreign (boxed-copy-fn info (g-value-get-boxed gvalue-ptr))
                          (make-foreign-type info :return-p nil)))

(defmethod boxed-set-g-value (gvalue-ptr
                              (info g-boxed-opaque-wrapper-info)
                              proxy)
  (g-value-set-boxed gvalue-ptr
                     (translate-to-foreign proxy
                                           (make-foreign-type info
                                                              :return-p nil))))

;;; ----------------------------------------------------------------------------

(defmacro define-boxed-opaque-accessor (boxed-name accessor-name
                                        &key type reader writer)
  (let ((var (gensym))
        (n-var (gensym)))
    `(progn
       ,@(when reader
           (list (etypecase reader
                   (symbol `(defun ,accessor-name (,var)
                              (funcall ,reader ,var)))
                   (string `(defcfun (,accessor-name ,reader) ,type
                              (,var (g-boxed-foreign ,boxed-name)))))))
       ,@(when writer
           (list (etypecase reader
                   (symbol `(defun (setf ,accessor-name) (,n-var ,var)
                              (funcall ,reader ,n-var ,var)))
                   (string `(defun (setf ,accessor-name) (,n-var ,var)
                              (foreign-funcall ,writer
                                               (g-boxed-foreign ,boxed-name)
                                               ,var
                                               ,type
                                               ,n-var
                                               :void)))))))))

;;; ----------------------------------------------------------------------------

(defun boxed-related-symbols (name)
  (let ((info (get-g-boxed-foreign-info name)))
    (etypecase info
      (g-boxed-cstruct-wrapper-info
       (append
         (list name
               (intern (format nil "MAKE-~A" (symbol-name name)))
               (intern (format nil "COPY-~A" (symbol-name name))))
         (iter (for slot in (cstruct-description-slots
                              (g-boxed-cstruct-wrapper-info-cstruct-description info)))
               (for slot-name = (cstruct-slot-description-name slot))
               (collect (intern (format nil "~A-~A"
                                        (symbol-name name)
                                        (symbol-name slot-name)))))))
      (g-boxed-opaque-wrapper-info
       (list name))
      (g-boxed-variant-cstruct-info
       (append
         (list name)
         (iter (for var-struct in (all-structures (g-boxed-variant-cstruct-info-root info)))
               (for s-name = (var-structure-name var-struct))
               (for cstruct-description = (var-structure-resulting-cstruct-description var-struct))
               (appending (append
                            (list s-name)
                            (list (intern (format nil "MAKE-~A" (symbol-name s-name)))
                                  (intern (format nil "COPY-~A" (symbol-name s-name))))
                            (iter (for slot in (cstruct-description-slots cstruct-description))
                                  (for slot-name = (cstruct-slot-description-name slot))
                                  (collect (intern (format nil "~A-~A"
                                                           (symbol-name s-name)
                                                           (symbol-name slot-name)))))))))))))

;;; ----------------------------------------------------------------------------

(defun copy-boxed-slots-to-foreign (struct
                                    native-ptr
                                    &optional (gtype (and struct
                                                          (type-of struct))))
  (when struct
    (copy-slots-to-native struct
                          native-ptr
                          (g-boxed-cstruct-wrapper-info-cstruct-description
                                            (get-g-boxed-foreign-info gtype)))))

(define-compiler-macro copy-boxed-slots-to-foreign (&whole whole structure
                                                           native-ptr
                                                    &optional type)
  (if (and type
           (constantp type))
      (let* ((type-r (eval type))
             (f-i (get-g-boxed-foreign-info type-r)))
        (unless f-i
          (warn "Unknown foreign GBoxed type ~S" type-r)
          (return-from copy-boxed-slots-to-foreign whole))
        (unless (typep f-i 'g-boxed-cstruct-wrapper-info)
          (warn "Foreign GBoxed type ~S is not a C structure wrapper" type-r)
          (return-from copy-boxed-slots-to-foreign whole))
        `(when ,structure
           (copy-slots-to-native
            ,structure
            ,native-ptr
            (load-time-value
              (g-boxed-cstruct-wrapper-info-cstruct-description
                (get-g-boxed-foreign-info ',type-r))))))
      whole))

;;; ----------------------------------------------------------------------------

(defmacro with-foreign-boxed-array ((n-var array-var type values-seq)
                                    &body body)
  (let ((values-seq-1 (gensym "VALUES-SEQ-"))
        (cstruct (generated-cstruct-name type))
        (x (gensym "X-"))
        (i (gensym "I-")))
    `(let* ((,values-seq-1 ,values-seq)
            (,n-var (length ,values-seq-1)))
       (with-foreign-object (,array-var '(:struct ,cstruct) ,n-var)
         (let ((,i 0))
           (map nil (lambda (,x)
                      (copy-boxed-slots-to-foreign
                       ,x
                       (inc-pointer ,array-var
                                    (* ,i (foreign-type-size '(:struct ,cstruct))))
                       ',type)
                      (incf ,i))
                ,values-seq-1))
         ,@body))))

;;; --- End of file gobject.boxed-lisp.lisp ------------------------------------
