;;; ----------------------------------------------------------------------------
;;; gobject.g-value.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;
;;; Generic values
;;;
;;;     A polymorphic type that can hold values of any other type
;;;
;;; Types and Values
;;;
;;;     GValue
;;;
;;; Functions
;;;
;;;     G_VALUE_INIT                                       not implemented
;;;     G_VALUE_HOLDS
;;;     G_VALUE_TYPE
;;;     G_VALUE_TYPE_NAME
;;;     G_TYPE_IS_VALUE
;;;     G_TYPE_IS_VALUE_ABSTRACT
;;;     G_IS_VALUE                                         not implemented
;;;     G_TYPE_VALUE
;;;     G_TYPE_VALUE_ARRAY                                 not implemented

;;;     g_value_init
;;;     g_value_copy
;;;     g_value_reset
;;;     g_value_unset
;;;     g_value_init_from_instance
;;;     g_value_set_instance
;;;     g_value_fits_pointer                               not implemented
;;;     g_value_peek_pointer                               not implemented
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     g_value_transform
;;;     GValueTransform
;;;     g_value_register_transform_func
;;;     g_strdup_value_contents
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GValue
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------

;; A generic function for getting the value of a g-value structure.

(defgeneric parse-g-value-for-type (gvalue-ptr gtype parse-kind))

(defmethod parse-g-value-for-type :around (gvalue-ptr gtype parse-kind)
  (declare (ignorable gvalue-ptr parse-kind))
  (assert (typep gtype '(or gtype nil)))
  (call-next-method))

(defmethod parse-g-value-for-type (gvalue-ptr gtype parse-kind)
  (if (eq gtype (g-type-fundamental gtype))
      (call-next-method)
      (parse-g-value-for-type gvalue-ptr
                              (g-type-fundamental gtype)
                              parse-kind)))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-pointer+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (g-value-pointer gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-param+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (parse-g-param-spec (g-value-param gvalue-ptr)))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-object+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (g-value-object gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr
                                   (type (eql (gtype +g-type-interface+)))
                                   parse-kind)
  (declare (ignore parse-kind))
  (g-value-object gvalue-ptr))

;;; ----------------------------------------------------------------------------
;;; parse-g-value (gvalue parse-kind)
;;;
;;; Parses the g-value structure and returns the corresponding Lisp object.
;;; This is a more general function which replaces the functions g-value-...
;;; The function is not part of the GObject library.
;;;
;;; Note:
;;;     It might be more consistent to name this function like the
;;;     corresponding functions as g-value-get ()
;;;
;;; value :
;;;     a C pointer to the GValue structure
;;;
;;; return :
;;;     value contained in the GValue structure. Type of value depends
;;;     on GValue type
;;; ----------------------------------------------------------------------------

(defun parse-g-value (gvalue &key (parse-kind :get-property))
  (let* ((gtype (g-value-type gvalue))
         (fundamental-type (g-type-fundamental gtype)))
    (ev-case fundamental-type
      ((gtype +g-type-invalid+)
       (error "GValue is of invalid type (~A)" (gtype-name gtype)))
      ((gtype +g-type-none+) nil)
      ((gtype +g-type-char+) (g-value-char gvalue))
      ((gtype +g-type-uchar+) (g-value-uchar gvalue))
      ((gtype +g-type-boolean+) (g-value-boolean gvalue))
      ((gtype +g-type-int+) (g-value-int gvalue))
      ((gtype +g-type-uint+) (g-value-uint gvalue))
      ((gtype +g-type-long+) (g-value-long gvalue))
      ((gtype +g-type-ulong+) (g-value-ulong gvalue))
      ((gtype +g-type-int64+) (g-value-int64 gvalue))
      ((gtype +g-type-uint64+) (g-value-uint64 gvalue))
      ((gtype +g-type-enum+) (parse-g-value-enum gvalue))
      ((gtype +g-type-flags+) (parse-g-value-flags gvalue))
      ((gtype +g-type-float+) (g-value-float gvalue))
      ((gtype +g-type-double+) (g-value-double gvalue))
      ((gtype +g-type-string+) (g-value-string gvalue))
      ((gtype +g-type-variant+) (g-value-variant gvalue))
      (t (parse-g-value-for-type gvalue gtype parse-kind)))))

;;; ----------------------------------------------------------------------------

;;; A generic function for setting the value of a GValue structure.

(defgeneric set-gvalue-for-type (gvalue-ptr type value))

(defmethod set-gvalue-for-type :around (gvalue-ptr type value)
  (declare (ignorable gvalue-ptr value))
  (assert (typep type '(or gtype null)))
  (call-next-method))

(defmethod set-gvalue-for-type (gvalue-ptr type value)
  (if (eq type (g-type-fundamental type))
      (call-next-method)
      (set-gvalue-for-type gvalue-ptr (g-type-fundamental type) value)))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-pointer+)))
                                value)
  (setf (g-value-pointer gvalue-ptr) value))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-param+)))
                                value)
  (declare (ignore gvalue-ptr value))
  (error "Setting of GParam is not implemented"))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-object+)))
                                value)
  (setf (g-value-object gvalue-ptr) value))

(defmethod set-gvalue-for-type (gvalue-ptr
                                (type (eql (gtype +g-type-interface+)))
                                value)
  (setf (g-value-object gvalue-ptr) value))

;;; ----------------------------------------------------------------------------
;;; set-g-value (gvalue value type zero-g-value unset-g-value g-value-init)
;;;
;;; Assigns the GValue structure gvalue the value value of GType type. This is
;;; a more general function which replaces the functions (setf g-value-...)
;;; The function is not part of the GObject library.
;;;
;;; Note :
;;;     It might be more consistent to name this function like the
;;;     corresponding functions as g-value-set ()
;;;
;;; gvalue :
;;;     a C pointer to the GValue structure
;;;
;;; value :
;;;     a Lisp object that is to be assigned
;;;
;;; type :
;;;     a GType that is to be assigned
;;;
;;; zero-g-value :
;;;     a boolean specifying whether GValue should be zero-initialized before
;;;     assigning. See g-value-zero.
;;;
;;; unset-g-value :
;;;     a boolean specifying whether GValue should be 'unset' before assigning.
;;;     See g-value-unset. The 'true' value should not be passed to both
;;;     zero-g-value and unset-g-value arguments
;;;
;;; g-value-init :
;;;     a boolean specifying where GValue should be initialized
;;; ----------------------------------------------------------------------------

(defun set-g-value (gvalue value type &key zero-g-value
                                           unset-g-value
                                           (g-value-init t))
  (setf type (gtype type))
  (cond (zero-g-value (%g-value-zero gvalue))
        (unset-g-value (g-value-unset gvalue)))
  (when g-value-init (g-value-init gvalue type))
  (let ((fundamental-type (g-type-fundamental type)))
    (ev-case fundamental-type
      ((gtype +g-type-invalid+) (error "Invalid type (~A)" type))
      ((gtype +g-type-none+) nil)
      ((gtype +g-type-char+) (setf (g-value-char gvalue) value))
      ((gtype +g-type-uchar+) (setf (g-value-uchar gvalue) value))
      ((gtype +g-type-boolean+) (setf (g-value-boolean gvalue) value))
      ((gtype +g-type-int+) (setf (g-value-int gvalue) value))
      ((gtype +g-type-uint+) (setf (g-value-uint gvalue) value))
      ((gtype +g-type-long+) (setf (g-value-long gvalue) value))
      ((gtype +g-type-ulong+) (setf (g-value-ulong gvalue) value))
      ((gtype +g-type-int64+) (setf (g-value-int64 gvalue) value))
      ((gtype +g-type-uint64+) (setf (g-value-uint64 gvalue) value))
      ((gtype +g-type-enum+) (set-gvalue-enum gvalue value))
      ((gtype +g-type-flags+) (set-gvalue-flags gvalue value))
      ((gtype +g-type-float+)
       (unless (realp value) (error "~A is not a real number" value))
       (setf (g-value-float gvalue) (coerce value 'single-float)))
      ((gtype +g-type-double+)
       (unless (realp value) (error "~A is not a real number" value))
       (setf (g-value-double gvalue) (coerce value 'double-float)))
      ((gtype +g-type-string+) (setf (g-value-string gvalue) value))
      ((gtype +g-type-variant+) (setf (g-value-variant gvalue) value))
      (t (set-gvalue-for-type gvalue type value)))))

;;; ----------------------------------------------------------------------------
;;; GValue
;;; ----------------------------------------------------------------------------

(defcunion g-value-data
  (:int :int)
  (:uint :uint)
  (:long :long)
  (:ulong :ulong)
  (:int64 :int64)
  (:uint64 :uint64)
  (:float :float)
  (:double :double)
  (:pointer :pointer))

;;; ----------------------------------------------------------------------------

;; The generalized calculation of the size and offset works for sbcl and ccl on
;;  a 32-bit Linux. Check this for more system.

#-windows
(defcstruct (g-value :size
                     ;; Generalized caluclation of the size of the structure
                     #.(+ (foreign-type-size 'g-type)
                          (* 2 (foreign-type-size '(:union g-value-data)))))
  (:type g-type)
  (:data (:union g-value-data)
         ;; Generalized calculation of the offset
         :offset #.(foreign-type-size 'g-type) :count 2)) ; Not a pointer. Is this correct?

#+windows
(defcstruct g-value
  (:type g-type)
  (:data (:union g-value-data) :count 2)) ; Not a pointer. Is this correct?

#+cl-cffi-gtk-documentation
(setf (gethash 'g-value atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-value atdoc:*external-symbols*)
 "@version{2013-6-5}
  @begin{short}
    The @sym{g-value} structure is basically a variable container that consists
    of a type identifier and a specific value of that type. The type identifier
    within a @sym{g-value} structure always determines the type of the
    associated value.
  @end{short}
  To create a undefined @sym{g-value} structure, simply create a zero-filled
  @sym{g-value} structure. To initialize the @sym{g-value}, use the
  @fun{g-value-init} function. A @sym{g-value} cannot be used until it is
  initialized. The basic type operations (such as freeing and copying) are
  determined by the @symbol{g-type-value-table} associated with the type ID
  stored in the @sym{g-value}. Other @sym{g-value} operations (such as
  converting values between types) are provided by this interface.

  The code in the example program below demonstrates @sym{g-value}'s features.
  @begin{pre}
;; A transformation from an integer to a string
(defcallback int2string :void ((src-value (:pointer g-value))
                               (dest-value (:pointer g-value)))
  (if (eql (g-value-int src-value) 42)
      (setf (g-value-string dest-value) \"An important number\")
      (setf (g-value-string dest-value) \"What is that?\")))

(defun example-g-value ()
  ;; Declare two variables of type g-value.
  (with-foreign-objects ((value1 'g-value) (value2 'g-value))

    ;; Initialization, setting and reading a value of type g-value
    (g-value-init value1 +g-type-string+)
    (setf (g-value-string value1) \"string\")
    (format t \"value1 = ~A~%\" (g-value-string value1))
    (format t \"type   = ~A~%\" (g-value-type value1))
    (format t \"name   = ~A~%~%\" (g-value-type-name value1))

    ;; The same in one step with the Lisp extension set-g-value
    (set-g-value value2 \"a second string\" +g-type-string+ :zero-g-value t)
    (format t \"value2 = ~A~%\" (parse-g-value value2))
    (format t \"type   = ~A~%\" (g-value-type value2))
    (format t \"name   = ~A~%~%\" (g-value-type-name value2))

    ;; Reuse value1 for an integer value.
    (g-value-unset value1)
    (g-value-init value1 +g-type-int+)
    (setf (g-value-int value1) 42)
    (format t \"value1 = ~A~%\" (parse-g-value value1))
    (format t \"type   = ~A~%\" (g-value-type value1))
    (format t \"name   = ~A~%~%\" (g-value-type-name value1))

    ;; The types integer and string are transformable.
    (assert (g-value-type-transformable +g-type-int+ +g-type-string+))

    ;; Transform value1 of type integer into value2 which is a string
    (g-value-transform value1 value2)
    (format t \"value1 = ~A~%\" (parse-g-value value1))
    (format t \"value2 = ~A~%~%\" (parse-g-value value2))

    ;; Some test functions.
    (assert (g-value-holds value1 +g-type-int+))
    (format t \"value-holds is ~A~%\" (g-value-holds value1 +g-type-int+))
    (format t \"is-value is ~A~%~%\" (g-type-is-value +g-type-int+))

    ;; Reuse value2 again for a string.
    (g-value-unset value2)
    (g-value-init value2 +g-type-string+)
    (setf (g-value-string value2) \"string\")
    (format t \"value2 = ~A~%\" (parse-g-value value2))

    ;; Register the transformation int2string
    (g-value-register-transform-func +g-type-int+
                                     +g-type-string+
                                     (callback int2string))
    ;; Try the transformation
    (g-value-transform value1 value2)
    (format t \"value2 = ~A~%~%\" (parse-g-value value2))))
  @end{pre}
  The data within the @sym{g-value} structure has protected scope: it is
  accessible only to functions within a @symbol{g-type-value-table} structure,
  or implementations of the @sym{g-value-*} API. That is, code portions which
  implement new fundamental types. @sym{g-value} users cannot make any
  assumptions about how data is stored within the 2 element data union, and the
  @class{g-type} member should only be accessed through the @fun{g-value-type}
  function.
  @see-function{g-value-init}
  @see-symbol{g-type-value-table}
  @see-class{g-type}
  @see-function{g-value-type}")

(export 'g-value)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_INIT
;;;
;;; #define G_VALUE_INIT  { 0, { { 0 } } }
;;;
;;; A GValue must be initialized before it can be used. This macro can be used
;;; as initializer instead of an explicit { 0 } when declaring a variable, but
;;; it cannot be assigned to a variable.
;;;
;;;   GValue value = G_VALUE_INIT;
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS()
;;; ----------------------------------------------------------------------------

(defun g-value-holds (value type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{a @symbol{g-value} structure}
  @argument[type]{a @class{g-type} value}
  @return{@arg{True} if @arg{value} holds the @arg{type}.}
  @begin{short}
    Checks if @arg{value} holds or contains a value of @arg{type}. This
    function will also check for a value not @code{NULL} and issue a warning if
    the check fails.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (setq value
       (with-foreign-object (value 'g-value)
         (g-value-init value \"gint\")))
=> #.(SB-SYS:INT-SAP #XB7910FE8)
 (g-value-holds value \"gint\")
=> T
 (g-value-holds value \"gboolean\")
=> NIL
    @end{pre}
  @end{dictionary}"
  (g-type= type (g-value-type value)))

(export 'g-value-holds)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-value-type (value)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{a @symbol{g-value} structure}
  @return{The @class{g-type} of @arg{value}.}
  @begin{short}
    Get the type identifier of @arg{value}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (setq value
       (with-foreign-object (value 'g-value)
         (g-value-init value \"gint\")))
=> #.(SB-SYS:INT-SAP #XB7910FE8)
 (g-value-type value)
=> #S(GTYPE :NAME \"gint\" :%ID 24)
    @end{pre}
  @end{dictionary}"
  (foreign-slot-value value '(:struct g-value) :type))

(export 'g-value-type)

;;; ----------------------------------------------------------------------------
;;; G_VALUE_TYPE_NAME()
;;; ----------------------------------------------------------------------------

(defun g-value-type-name (value)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{a @symbol{g-value} structure}
  @return{The type name of @arg{value}.}
  @begin{short}
    Gets the the type name of @arg{value}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (setq value
       (with-foreign-object (value 'g-value)
         (g-value-init value \"gint\")))
=> #.(SB-SYS:INT-SAP #XB7910FE8)
 (g-value-type-name value)
=> \"gint\"
    @end{pre}
  @end{dictionary}"
  (g-type-name (g-value-type value)))

(export 'g-value-type-name)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE()
;;; ----------------------------------------------------------------------------

(defun g-type-is-value (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[type]{a @class{g-type}}
  @return{Whether @arg{type} is suitable as a @symbol{g-value} type.}
  @begin{short}
    Checks whether the passed in @arg{type} ID can be used for
    @fun{g-value-init}.
  @end{short}
  That is, this function checks whether this @arg{type} provides an
  implementation of the @symbol{g-type-value-table} functions required for a
  type to create a @symbol{g-value} of.

  This function is equivalent to @fun{g-type-is-value-type}.
  @see-function{g-value-init}
  @see-function{g-type-is-value-type}"
  (%g-type-check-is-value-type type))

(export 'g-type-is-value)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_ABSTRACT()
;;; ----------------------------------------------------------------------------

(defun g-type-is-value-abstract (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is an abstract value type.}
  @begin{short}
    Checks if @arg{type} is an abstract value type. An abstract value type
    introduces a value table, but can not be used for @fun{g-value-init} and is
    normally used as an abstract base type for derived value types.
  @end{short}
  @see-function{g-value-init}"
  (%g-type-test-flags type :value-abstract))

(export 'g-type-is-value-abstract)

;;; ----------------------------------------------------------------------------
;;; G_IS_VALUE()
;;;
;;; #define G_IS_VALUE(value) (G_TYPE_CHECK_VALUE (value))
;;;
;;; Checks if value is a valid and initialized GValue structure.
;;;
;;; value :
;;;     A GValue structure.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE
;;; ----------------------------------------------------------------------------

(defun g-type-value ()
 "@version{2013-6-9}
  @return{The type ID of @symbol{g-value}.}
  The type ID of the @symbol{g-value} type which is a boxed type, used to pass
  around pointers to @symbol{g-value}'s.
  @see-symbol{g-value}"
  (cffi:foreign-funcall "g_value_get_type" g-type))

(glib-init::at-init nil (g-type-value))

(export 'g-type-value)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VALUE_ARRAY
;;;
;;; #define G_TYPE_VALUE_ARRAY (g_value_array_get_type ())
;;;
;;; Warning
;;;
;;; G_TYPE_VALUE_ARRAY has been deprecated since version 2.32 and should not be
;;; used in newly-written code. Use GArray instead of GValueArray
;;;
;;; The type ID of the "GValueArray" type which is a boxed type, used to pass
;;; around pointers to GValueArrays.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_init ()
;;; ----------------------------------------------------------------------------

;; Called from g-value-init to initialize a GValue to zero

(defun %g-value-zero (value)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  This function is called from the function @fun{g-value-init} to initialize
  the @symbol{g-value} structure with zeros. @arg{value} is a C pointer to the
  @symbol{g-value} structure.
  @see-symbol{g-value}
  @see-function{g-value-init}"
  (loop
     for i from 0 below (foreign-type-size '(:struct g-value))
     do (setf (mem-ref value :uchar i) 0)))

(defcfun ("g_value_init" %g-value-init) (:pointer (:struct g-value))
  (value (:pointer (:struct g-value)))
  (gtype g-type))

(defun g-value-init (value &optional (gtype nil))
 #+cl-cffi-gtk-documentation
 "@version{2020-10-11}
  @argument[value]{an uninitialized @symbol{g-value} structure}
  @argument[gtype]{the @class{g-type} @arg{value} should hold values of}
  @return{The @symbol{g-value} structure that has been passed in.}
  @begin{short}
    Initializes @arg{value} with the default value of @arg{gtype}.
  @end{short}
  @see-symbol{g-value}
  @see-class{g-type}"
  (cond ((null gtype)
         (%g-value-zero value))
        (t
         (%g-value-zero value)
         (%g-value-init value gtype)))
  value)

(export 'g-value-init)

;;; ----------------------------------------------------------------------------
;;; g_value_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_copy" g-value-copy) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[src-value]{an initialized @symbol{g-value} structure}
  @argument[dest-value]{an initialized @symbol{g-value} structure of the same
    type as @arg{src-value}}
  Copies the value of @arg{src-value} into @arg{dest-value}."
  (src-value (:pointer (:struct g-value)))
  (dst-value (:pointer (:struct g-value))))

(export 'g-value-copy)

;;; ----------------------------------------------------------------------------
;;; g_value_reset ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_reset" g-value-reset) (:pointer (:struct g-value))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{an initialized @symbol{g-value} structure}
  @return{The @symbol{g-value} structure that has been passed in.}
  Clears the current value in @arg{value} and resets it to the default value
  as if the value had just been initialized."
  (value (:pointer (:struct g-value))))

(export 'g-value-reset)

;;; ----------------------------------------------------------------------------
;;; g_value_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_unset" g-value-unset) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{an initialized @symbol{g-value} structure}
  @begin{short}
    Clears the current value in @arg{value} and \"unsets\" the type, this
    releases all resources associated with this @symbol{g-value}.
  @end{short}
  An unset value is the same as an uninitialized (zero-filled) @symbol{g-value}
  structure."
  (value (:pointer (:struct g-value))))

(export 'g-value-unset)

;;; ----------------------------------------------------------------------------
;;; g_value_init_from_instance ()
;;;
;;; void
;;; g_value_init_from_instance (GValue *value, gpointer instance);
;;;
;;; Initializes and sets value from an instantiatable type via the value_table's
;;; collect_value() function.
;;;
;;; Note: The value will be initialised with the exact type of instance . If you
;;; wish to set the value 's type to a different GType (such as a parent class
;;; GType), you need to manually call g_value_init() and g_value_set_instance().
;;;
;;; value:
;;;     An uninitialized GValue structure.
;;;
;;; instance:
;;;     the instance.
;;;
;;; Since 2.42
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_instance ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_instance" g-value-set-instance) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{an initialized @symbol{g-value} structure}
  @argument[instance]{the instance}
  Sets @arg{value} from an instantiatable type via the @arg{value_table}'s
  @arg{collect_value()} function."
  (value (:pointer (:struct g-value)))
  (instance :pointer))

(export 'g-value-set-instance)

;;; ----------------------------------------------------------------------------
;;; g_value_fits_pointer ()
;;;
;;; gboolean g_value_fits_pointer (const GValue *value);
;;;
;;; Determines if value will fit inside the size of a pointer value. This is an
;;; internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     An initialized GValue structure.
;;;
;;; Returns :
;;;     TRUE if value will fit inside a pointer value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_peek_pointer ()
;;;
;;; gpointer g_value_peek_pointer (const GValue *value);
;;;
;;; value :
;;;     An initialized GValue structure.
;;;
;;; Returns :
;;;     the value contents as pointer. This function asserts that
;;;     g_value_fits_pointer() returned TRUE for the passed in value. This is an
;;;     internal function introduced mainly for C marshallers.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_type_compatible ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_type_compatible" g-value-type-compatible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[src-type]{source type to be copied}
  @argument[dest-type]{destination type for copying}
  @return{@em{True} if @fun{g-value-copy} is possible with @arg{src-type} and
  @arg{dest-type}.}
  Returns whether a @symbol{g-value} of type @arg{src-type} can be copied into
  a @symbol{g-value} of type @arg{dest-type}.
  @see-function{g-value-copy}"
  (src-type g-type)
  (dest-type g-type))

(export 'g-value-type-compatible)

;;; ----------------------------------------------------------------------------
;;; g_value_type_transformable ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_type_transformable" g-value-type-transformable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[src-type]{source type}
  @argument[dest-type]{target type}
  @return{@code{True} if the transformation is possible, @code{nil} otherwise.}
  Check whether @fun{g-value-transform} is able to transform values of type
  @arg{src-type} into values of type @arg{dest-type}.
  @see-function{g-value-transform}"
  (src-type g-type)
  (dest-type g-type))

(export 'g-value-type-transformable)

;;; ----------------------------------------------------------------------------
;;; g_value_transform ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_transform" g-value-transform) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[src-value]{source value}
  @argument[dest-value]{target value}
  @return{Whether a transformation rule was found and could be applied. Upon
    failing transformations, @arg{dest-value} is left untouched.}
  @begin{short}
    Tries to cast the contents of @arg{src-value} into a type appropriate to
    store in @arg{dest-value}, e. g. to transform a @var{+g-type-int+} value
    into a @var{+g-type-float+} value.
  @end{short}
  Performing transformations between value types might incur precision
  lossage. Especially transformations into strings might reveal seemingly
  arbitrary results and should not be relied upon for production code (such as
  rcfile value or object property serialization)."
  (src-value (:pointer (:struct g-value)))
  (dest-value (:pointer (:struct g-value))))

(export 'g-value-transform)

;;; ----------------------------------------------------------------------------
;;; GValueTransform ()
;;;
;;; void (*GValueTransform) (const GValue *src_value, GValue *dest_value);
;;;
;;; The type of value transformation functions which can be registered with
;;; g_value_register_transform_func().
;;;
;;; src_value :
;;;     Source value.
;;;
;;; dest_value :
;;;     Target value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_register_transform_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_register_transform_func" g-value-register-transform-func)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[src-type]{source type}
  @argument[dest-type]{target type}
  @argument[transform-func]{a function which transforms values of type
    @arg{src-type} into value of type @arg{dest-type}}
  @begin{short}
    Registers a value transformation function for use in
    @fun{g-value-transform}.
  @end{short}
  A previously registered transformation function for @arg{src-type} and
  @arg{dest-type} will be replaced.
  @see-function{g-value-transform}"
  (src-type g-type)
  (dest-type g-type)
  (transform-func :pointer))

(export 'g-value-register-transform-func)

;;; ----------------------------------------------------------------------------
;;; g_strdup_value_contents ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_strdup_value_contents" g-strdup-value-contents) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{@symbol{g-value} which contents are to be described}
  @return{Newly allocated string.}
  @begin{short}
    Return a newly allocated string, which describes the contents of a
    @symbol{g-value}.
  @end{short}
  The main purpose of this function is to describe @symbol{g-value} contents for
  debugging output, the way in which the contents are described may change
  between different GLib versions."
  (value (:pointer (:struct g-value))))

(export 'g-strdup-value-contents)

;;; --- End of file gobject.g-value.lisp ---------------------------------------
