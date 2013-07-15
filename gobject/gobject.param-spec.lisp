;;; ----------------------------------------------------------------------------
;;; gobject.param-spec.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.36.2. See <http://www.gtk.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; GParamSpec
;;;
;;; Metadata for parameter specifications
;;;
;;; Synopsis
;;;
;;;     GParamFlags
;;;     GParamSpec
;;;     GParamSpecClass
;;;
;;;     G_TYPE_IS_PARAM
;;;     G_PARAM_SPEC
;;;     G_IS_PARAM_SPEC
;;;     G_PARAM_SPEC_CLASS
;;;     G_IS_PARAM_SPEC_CLASS
;;;     G_PARAM_SPEC_GET_CLASS
;;;     G_PARAM_SPEC_TYPE
;;;     G_PARAM_SPEC_TYPE_NAME
;;;     G_PARAM_SPEC_VALUE_TYPE
;;;
;;;     G_PARAM_READWRITE
;;;     G_PARAM_STATIC_STRINGS
;;;     G_PARAM_MASK
;;;     G_PARAM_USER_SHIFT
;;;
;;;     g_param_spec_ref
;;;     g_param_spec_unref
;;;     g_param_spec_sink
;;;     g_param_spec_ref_sink
;;;     g_param_value_set_default
;;;     g_param_value_defaults
;;;     g_param_value_validate
;;;     g_param_value_convert
;;;     g_param_values_cmp
;;;     g_param_spec_get_name
;;;     g_param_spec_get_nick
;;;     g_param_spec_get_blurb
;;;     g_param_spec_get_qdata
;;;     g_param_spec_set_qdata
;;;     g_param_spec_set_qdata_full
;;;     g_param_spec_steal_qdata
;;;     g_param_spec_get_redirect_target
;;;     g_param_spec_internal
;;;
;;;     GParamSpecTypeInfo
;;;
;;;     g_param_type_register_static
;;;
;;;     GParamSpecPool
;;;
;;;     g_param_spec_pool_new
;;;     g_param_spec_pool_insert
;;;     g_param_spec_pool_remove
;;;     g_param_spec_pool_lookup
;;;     g_param_spec_pool_list
;;;     g_param_spec_pool_list_owned
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; enum GParamFlags
;;; ----------------------------------------------------------------------------

(defbitfield g-param-flags
  (:readable #.(ash 1 0))
  (:writable #.(ash 1 1))
  (:construct #.(ash 1 2))
  (:construct-only #.(ash 1 3))
  (:lax-validation #.(ash 1 4))
  (:static-name #.(ash 1 5))
  (:static-nick #.(ash 1 6))
  (:static-blurb #.(ash 1 7))
  (:deprecated #.(ash 1 31)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-flags atdoc:*symbol-name-alias*) "Bitfield"
      (gethash 'g-param-flags atdoc:*external-symbols*)
 "@version{2013-2-7}
  @begin{short}
    Through the @sym{g-param-flags} flag values, certain aspects of parameters
    can be configured.
  @end{short}
  @begin{pre}
(defbitfield g-param-flags
  (:readable #.(ash 1 0))
  (:writable #.(ash 1 1))
  (:construct #.(ash 1 2))
  (:construct-only #.(ash 1 3))
  (:lax-validation #.(ash 1 4))
  (:static-name #.(ash 1 5))
  (:static-nick #.(ash 1 6))
  (:static-blurb #.(ash 1 7))
  (:deprecated #.(ash 1 31)))
  @end{pre}
  @begin[code]{table}
    @entry[:readable]{the parameter is readable}
    @entry[:writable]{the parameter is writable}
    @entry[:construct]{the parameter will be set upon object construction}
    @entry[:construct-only]{the parameter will only be set upon object
      construction}
    @entry[:lax-validation]{upon parameter conversion (see
      g_param_value_convert()) strict validation is not required}
    @entry[:static-name]{the string used as name when constructing the parameter
      is guaranteed to remain valid and unmodified for the lifetime of the
      parameter. Since 2.8}
    @entry[:static-nick]{the string used as nick when constructing the parameter
      is guaranteed to remain valid and unmmodified for the lifetime of the
      parameter. Since 2.8}
    @entry[:static-blurb]{the string used as blurb when constructing the
      parameter is guaranteed to remain valid and unmodified for the lifetime of
      the parameter. Since 2.8}
    @entry[:deprecated]{the parameter is deprecated and will be removed in a
      future version. A warning will be generated if it is used while running
      with @code{G_ENABLE_DIAGNOSTIC=1}. Since 2.26}
  @end{table}")

(export 'g-param-flags)

;;; ----------------------------------------------------------------------------
;;; struct GParamSpec
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec
  (:type-instance (:pointer (:struct g-type-instance)))
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:flags g-param-flags)
  (:value-type g-type)
  (:owner-type g-type))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec atdoc:*external-symbols*)
 "@version{2013-7-14}
  @begin{short}
    @sym{g-param-spec} is an object structure that encapsulates the metadata
    required to specify parameters, such as e. g. @class{g-object} properties.
  @end{short}

  Parameter names need to start with a letter (a-z or A-Z). Subsequent
  characters can be letters, numbers or a '-'. All other characters are
  replaced by a '-' during construction. The result of this replacement is
  called the canonical name of the parameter.
  @begin{pre}
(defcstruct g-param-spec
  (:type-instance (:pointer (:struct g-type-instance)))
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:flags g-param-flags)
  (:value-type g-type)
  (:owner-type g-type))
  @end{pre}
  @begin[code]{table}
    @entry[:type-instance]{Private @symbol{g-type-instance} portion.}
    @entry[:name]{Name of this parameter: always an interned string.}
    @entry[:flags]{The @symbol{g-param-flags} flags for this parameter.}
    @entry[:value-type]{The @symbol{g-value} type for this parameter.}
    @entry[:owner-type]{The @class{g-type} that uses (introduces) this
      parameter.}
  @end{table}
  @see-symbol{g-type-instance}
  @see-symbol{g-param-flags}
  @see-symbol{g-value}
  @see-class{g-type}")

(export 'g-param-spec)

;;; ----------------------------------------------------------------------------

;; Corresponding Lisp structure describing a property of a GObject class.

(defstruct param-spec
  name
  type
  readable
  writable
  constructor
  constructor-only
  owner-type)

(defmethod print-object ((instance param-spec) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream
                "PROPERTY ~A ~A . ~A (flags:~@[~* readable~]~@[~* writable~]~@[~* constructor~]~@[~* constructor-only~])"
                (gtype-name (param-spec-type instance))
                (param-spec-owner-type instance)
                (param-spec-name instance)
                (param-spec-readable instance)
                (param-spec-writable instance)
                (param-spec-constructor instance)
                (param-spec-constructor-only instance)))))

;; Transform a value of the C type GParamSpec to Lisp type param-spec

(defun parse-g-param-spec (param)
  (let ((flags (foreign-slot-value param '(:struct g-param-spec) :flags)))
    (make-param-spec
        :name (foreign-slot-value param '(:struct g-param-spec) :name)
        :type (foreign-slot-value param '(:struct g-param-spec) :value-type)
        :readable (not (null (member :readable flags)))
        :writable (not (null (member :writable flags)))
        :constructor (not (null (member :construct flags)))
        :constructor-only (not (null (member :construct-only flags)))
        :owner-type (foreign-slot-value param '(:struct g-param-spec) :owner-type))))

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecClass
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-class
  (:type-class (:pointer (:struct g-type-class)))
  (:value-type g-type)
  (:finalize :pointer)
  (:value-set-default :pointer)
  (:value-validate :pointer)
  (:values-cmp :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-class atdoc:*external-symbols*)
 "@version{2013-2-7}
  @begin{short}
    The class structure for the @symbol{g-param-spec} type.
  @end{short}
  Normally, @symbol{g-param-spec} classes are filled by
  @fun{g-param-type-register-static}.
  @begin{pre}
(defcstruct g-param-spec-class
  (:type-class (:pointer (:struct g-type-class)))
  (:value-type g-type)
  (:finalize :pointer)
  (:value-set-default :pointer)
  (:value-validate :pointer)
  (:values-cmp :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{the parent class}
    @entry[:value-type]{the GValue type for this parameter}
    @entry[:finalize]{The instance finalization function (optional), should
      chain up to the finalize method of the parent class.}
    @entry[:value-set-default]{Resets a value to the default value for this type
      (recommended, the default is @fun{g-value-reset}), see
      @fun{g-param-value-set-default}.}
    @entry[:value-validate]{Ensures that the contents of value comply with the
      specifications set out by this type (optional), see
      @fun{g-param-value-validate}.}
    @entry[:value-cmp]{Compares value1 with value2 according to this type
      (recommended, the default is @code{memcmp()}), see
      @fun{g-param-values-cmp}.}
  @end{table}")

(export 'g-param-spec-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_PARAM()
;;; ----------------------------------------------------------------------------

(defun g-type-is-param (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[type]{a @class{g-type} ID}
  @begin{short}
    Checks whether type \"is a\" @code{G_TYPE_PARAM}.
  @end{short}"
  (eql (gtype-id (g-type-fundamental gtype)) +g-type-param+))

(export 'g-type-is-param)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC()
;;;
;;; #define G_PARAM_SPEC(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec), G_TYPE_PARAM, GParamSpec))
;;;
;;; Casts a derived GParamSpec object (e.g. of type GParamSpecInt) into a
;;; GParamSpec object.
;;;
;;; pspec :
;;;     a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC()
;;; ----------------------------------------------------------------------------

(defun g-is-param-spec (pspec)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[spec]{a GParamSpec}
  @begin{short}
    Checks whether pspec \"is a\" valid GParamSpec structure of type
    G_TYPE_PARAM or derived.
  @end{short}"
  (g-type-is-param (g-type-from-instance pspec)))

(export 'g-is-param-spec)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_CLASS()
;;;
;;; #define G_PARAM_SPEC_CLASS(pclass)
;;;         (G_TYPE_CHECK_CLASS_CAST ((pclass), G_TYPE_PARAM, GParamSpecClass))
;;;
;;; Casts a derived GParamSpecClass structure into a GParamSpecClass structure.
;;;
;;; pclass :
;;;     a valid GParamSpecClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_CLASS()
;;;
;;; #define G_IS_PARAM_SPEC_CLASS(pclass)
;;;         (G_TYPE_CHECK_CLASS_TYPE ((pclass), G_TYPE_PARAM))
;;;
;;; Checks whether pclass "is a" valid GParamSpecClass structure of type
;;; G_TYPE_PARAM or derived.
;;;
;;; pclass :
;;;     a GParamSpecClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_GET_CLASS()
;;;
;;; #define G_PARAM_SPEC_GET_CLASS(pspec)
;;;         (G_TYPE_INSTANCE_GET_CLASS ((pspec), G_TYPE_PARAM, GParamSpecClass))
;;;
;;; Retrieves the GParamSpecClass of a GParamSpec.
;;;
;;; pspec :
;;;     a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-param-spec-type (pspec)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @short{Retrieves the GType of this pspec.}"
  (g-type-from-instance pspec))

(export 'g-param-spec-type)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_TYPE_NAME()
;;; ----------------------------------------------------------------------------

(defun g-param-spec-type-name (pspec)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @short{Retrieves the @class{g-type} name of this @arg{pspec}.}"
  (g-type-name (g-param-spec-type pspec)))

(export 'g-param-spec-type-name)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VALUE_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-param-spec-value-type (pspec)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @short{Retrieves the GType to initialize a GValue for this parameter.}"
  (foreign-slot-value pspec '(:struct g-param-spec) :value-type))

(export 'g-param-spec-value-type)

;;; ----------------------------------------------------------------------------
;;; G_PARAM_READWRITE
;;;
;;; #define G_PARAM_READWRITE (G_PARAM_READABLE | G_PARAM_WRITABLE)
;;;
;;; GParamFlags value alias for G_PARAM_READABLE | G_PARAM_WRITABLE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_STATIC_STRINGS
;;;
;;; #define G_PARAM_STATIC_STRINGS
;;;         (G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB)
;;;
;;; GParamFlags value alias for G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
;;; G_PARAM_STATIC_BLURB.
;;;
;;; Since 2.13.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_MASK
;;;
;;; #define G_PARAM_MASK (0x000000ff)
;;;
;;; Mask containing the bits of GParamSpec.flags which are reserved for GLib.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_USER_SHIFT
;;;
;;; #define G_PARAM_USER_SHIFT (8)
;;;
;;; Minimum shift count to be used for user defined flags, to be stored in
;;; GParamSpec.flags. The maximum allowed is 30 + G_PARAM_USER_SHIFT.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref ()
;;;
;;; GParamSpec * g_param_spec_ref (GParamSpec *pspec);
;;;
;;; Increments the reference count of pspec.
;;;
;;; pspec :
;;;     a valid GParamSpec
;;;
;;; Returns :
;;;     the GParamSpec that was passed into this function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_unref" g-param-spec-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @short{Decrements the reference count of a pspec.}"
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-unref)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_sink ()
;;;
;;; void g_param_spec_sink (GParamSpec *pspec);
;;;
;;; The initial reference count of a newly created GParamSpec is 1, even though
;;; no one has explicitly called g_param_spec_ref() on it yet. So the initial
;;; reference count is flagged as "floating", until someone calls
;;; g_param_spec_ref (pspec); g_param_spec_sink (pspec); in sequence on it,
;;; taking over the initial reference count (thus ending up with a pspec that
;;; has a reference count of 1 still, but is not flagged "floating" anymore).
;;;
;;; pspec :
;;;     a valid GParamSpec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ref_sink ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_ref_sink" g-param-spec-ref-sink)
    (:pointer (:struct g-param-spec))
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @return{the GParamSpec that was passed into this function}
  @short{Convenience function to ref and sink a GParamSpec.}@break{}
  Since 2.10"
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-ref-sink)

;;; ----------------------------------------------------------------------------
;;; g_param_value_set_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_value_set_default" g-param-value-set-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @argument[value]{a GValue of correct type for pspec}
  @short{Sets value to its default value as specified in pspec.}"
  (pspec (:pointer (:struct g-param-spec)))
  (value (:pointer (:struct g-value))))

(export 'g-param-value-set-default)

;;; ----------------------------------------------------------------------------
;;; g_param_value_defaults ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_value_defaults" g-param-value-defaults) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @argument[value]{a GValue of correct type for pspec}
  @return{Whether value contains the canonical default for this pspec.}
  @begin{short}
    Checks whether value contains the default value as specified in pspec.
  @end{short}"
  (pspec (:pointer (:struct g-param-spec)))
  (value (:pointer (:struct g-value))))

(export 'g-param-value-defaults)

;;; ----------------------------------------------------------------------------
;;; g_param_value_validate ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_value_validate" g-param-value-validate) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @argument[value]{a @symbol{g-value} of correct type for @arg{pspec}}
  @return{Whether modifying value was necessary to ensure validity.}
  @begin{short}
    Ensures that the contents of value comply with the specifications set out by
    pspec.
  @end{short}
  For example, a GParamSpecInt might require that integers stored in
  value may not be smaller than -42 and not be greater than +42. If value
  contains an integer outside of this range, it is modified accordingly, so
  the resulting value will fit into the range -42 .. +42."
  (pspec (:pointer (:struct g-param-spec)))
  (value (:pointer (:struct g-value))))

(export 'g-param-value-validate)

;;; ----------------------------------------------------------------------------
;;; g_param_value_convert ()
;;;
;;; gboolean g_param_value_convert (GParamSpec *pspec,
;;;                                 const GValue *src_value,
;;;                                 GValue *dest_value,
;;;                                 gboolean strict_validation);
;;;
;;; Transforms src_value into dest_value if possible, and then validates
;;; dest_value, in order for it to conform to pspec. If strict_validation is
;;; TRUE this function will only succeed if the transformed dest_value complied
;;; to pspec without modifications.
;;;
;;; See also g_value_type_transformable(), g_value_transform() and
;;; g_param_value_validate().
;;;
;;; pspec :
;;;     a valid GParamSpec
;;;
;;; src_value :
;;;     souce GValue
;;;
;;; dest_value :
;;;     destination GValue of correct type for pspec
;;;
;;; strict_validation :
;;;     TRUE requires dest_value to conform to pspec without modifications
;;;
;;; Returns :
;;;     TRUE if transformation and validation were successful, FALSE otherwise
;;;     and dest_value is left untouched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_values_cmp ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_values_cmp" g-param-values-cmp) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-21}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @argument[value1]{a @symbol{g-value} of correct type for @arg{pspec}}
  @argument[value2]{a @symbol{g-value} of correct type for @arg{pspec}}
  @return{-1, 0 or +1, for a less than, equal to or greater than result}
  Compares @arg{value1} with @arg{value2} according to pspec, and return -1, 0
  or +1, if @arg{value1} is found to be less than, equal to or greater than
  value2, respectively."
  (pspec (:pointer (:struct g-param-spec)))
  (value1 (:pointer (:struct g-value)))
  (value2 (:pointer (:struct g-value))))

(export 'g-param-values-cmp)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_get_name" g-param-spec-get-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @return{The name of pspec.}
  @begin{short}
    Get the name of a GParamSpec.
  @end{short}@break{}
  The name is always an \"interned\" string (as per g_intern_string()). This
  allows for pointer-value comparisons."
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-get-name)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_nick ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_get_nick" g-param-spec-get-nick) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @return{The nickname of @arg{pspec}.}
  @short{Get the nickname of a GParamSpec.}"
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-get-nick)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_blurb ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_get_blurb" g-param-spec-get-blurb) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @argument[pspec]{a valid @symbol{g-param-spec}}
  @return{The short description of @arg{pspec}.}
  @short{Get the short description of a GParamSpec.}"
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-get-blurb)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_qdata ()
;;;
;;; gpointer g_param_spec_get_qdata (GParamSpec *pspec, GQuark quark);
;;;
;;; Gets back user data pointers stored via g_param_spec_set_qdata().
;;;
;;; pspec :
;;;     a valid GParamSpec
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; Returns :
;;;     the user data pointer set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_set_qdata ()
;;;
;;; void g_param_spec_set_qdata (GParamSpec *pspec, GQuark quark, gpointer data)
;;;
;;; Sets an opaque, named pointer on a GParamSpec. The name is specified through
;;; a GQuark (retrieved e.g. via g_quark_from_static_string()), and the pointer
;;; can be gotten back from the pspec with g_param_spec_get_qdata(). Setting a
;;; previously set user data pointer, overrides (frees) the old pointer set,
;;; using NULL as pointer essentially removes the data stored.
;;;
;;; pspec :
;;;     the GParamSpec to set store a user data pointer
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; data :
;;;     an opaque user data pointer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_set_qdata_full ()
;;;
;;; void g_param_spec_set_qdata_full (GParamSpec *pspec,
;;;                                   GQuark quark,
;;;                                   gpointer data,
;;;                                   GDestroyNotify destroy);
;;;
;;; This function works like g_param_spec_set_qdata(), but in addition, a void
;;; (*destroy) (gpointer) function may be specified which is called with data as
;;; argument when the pspec is finalized, or the data is being overwritten by a
;;; call to g_param_spec_set_qdata() with the same quark.
;;;
;;; pspec :
;;;     the GParamSpec to set store a user data pointer
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; data :
;;;     an opaque user data pointer
;;;
;;; destroy :
;;;     function to invoke with data as argument, when data needs to be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_steal_qdata ()
;;;
;;; gpointer g_param_spec_steal_qdata (GParamSpec *pspec, GQuark quark);
;;;
;;; Gets back user data pointers stored via g_param_spec_set_qdata() and removes
;;; the data from pspec without invoking its destroy() function (if any was
;;; set). Usually, calling this function is only required to update user data
;;; pointers with a destroy notifier.
;;;
;;; pspec :
;;;     the GParamSpec to get a stored user data pointer from
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; Returns :
;;;     the user data pointer set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_get_redirect_target ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_get_redirect_target" g-param-spec-get-redirect-target)
    (:pointer (:struct g-param-spec))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-2}
  @argument[pspec]{a @symbol{g-param-spec} structure}
  @begin{return}
    paramspec to which requests on this paramspec should be redirected, or
    @code{nil} if none.
  @end{return}
  @begin{short}
    If the paramspec redirects operations to another paramspec, returns that
    paramspec.
  @end{short}
  Redirect is used typically for providing a new implementation of a property in
  a derived type while preserving all the properties from the parent type.
  Redirection is established by creating a property of type
  @code{GParamSpecOverride}. See @fun{g-object-class-override-property} for an
  example of the use of this capability.

  Since 2.4"
  (pspec (:pointer (:struct g-param-spec))))

(export 'g-param-spec-get-redirect-target)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_internal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_internal" g-param-spec-internal) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-6-20}
  @argument[param-type]{the @class{g-type} for the property; must be derived
    from @code{G_TYPE_PARAM}}
  @argument[name]{the canonical name of the property}
  @argument[nick]{the nickname of the property}
  @argument[blurb]{a short description of the property}
  @argument[flags]{a combination of @symbol{g-param-flags}}
  @return{A newly allocated @symbol{g-param-spec} instance.}
  @begin{short}
    Creates a new @symbol{g-param-spec} instance.
  @end{short}

  A property @arg{name} consists of segments consisting of ASCII letters and
  digits, separated by either the '-' or '_' character. The first character of
  a property @arg{name} must be a letter. Names which violate these rules lead
  to undefined behaviour.

  When creating and looking up a @symbol{g-param-spec}, either separator can be
  used, but they cannot be mixed. Using '-' is considerably more efficient and
  in fact required when using property names as detail strings for signals.

  Beyond the @arg{name}, @symbol{g-param-spec}'s have two more descriptive
  strings associated with them, the @arg{nick}, which should be suitable for use
  as a label for the property in a property editor, and the @arg{blurb}, which
  should be a somewhat longer description, suitable for e. g. a tooltip. The
  @arg{nick} and @arg{blurb} should ideally be localized."
  (param-type g-type)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(export 'g-param-spec-internal)

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecTypeInfo
;;;
;;; struct GParamSpecTypeInfo {
;;;   /* type system portion */
;;;   guint16     instance_size;                              /* obligatory */
;;;   guint16     n_preallocs;                                /* optional */
;;;   void        (*instance_init)     (GParamSpec   *pspec); /* optional */
;;;
;;;   /* class portion */
;;;   GType       value_type;                                 /* obligatory */
;;;   void        (*finalize)          (GParamSpec   *pspec); /* optional */
;;;   void        (*value_set_default) (GParamSpec   *pspec,  /* recommended */
;;;                                     GValue       *value);
;;;   gboolean    (*value_validate)    (GParamSpec   *pspec,  /* optional */
;;;                                     GValue       *value);
;;;   gint        (*values_cmp)        (GParamSpec   *pspec,  /* recommended */
;;;                                     const GValue *value1,
;;;                                     const GValue *value2);
;;; };
;;;
;;; This structure is used to provide the type system with the information
;;; required to initialize and destruct (finalize) a parameter's class and
;;; instances thereof. The initialized structure is passed to the
;;; g_param_type_register_static() The type system will perform a deep copy of
;;; this structure, so its memory does not need to be persistent across
;;; invocation of g_param_type_register_static().
;;;
;;; guint16 instance_size;
;;;     Size of the instance (object) structure.
;;;
;;; guint16 n_preallocs;
;;;     Prior to GLib 2.10, it specified the number of pre-allocated (cached)
;;;     instances to reserve memory for (0 indicates no caching). Since
;;;     GLib 2.10, it is ignored, since instances are allocated with the slice
;;;     allocator now.
;;;
;;; instance_init ()
;;;     Location of the instance initialization function (optional).
;;;
;;; GType value_type;
;;;     The GType of values conforming to this GParamSpec
;;;
;;; finalize ()
;;;     The instance finalization function (optional).
;;;
;;; value_set_default ()
;;;     Resets a value to the default value for pspec (recommended, the default
;;;     is g_value_reset()), see g_param_value_set_default().
;;;
;;; value_validate ()
;;;     Ensures that the contents of value comply with the specifications set
;;;     out by pspec (optional), see g_param_value_validate().
;;;
;;; values_cmp ()
;;;     Compares value1 with value2 according to pspec (recommended, the default
;;;     is memcmp()), see g_param_values_cmp().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_type_register_static ()
;;;
;;; GType g_param_type_register_static (const gchar *name,
;;;                                     const GParamSpecTypeInfo *pspec_info);
;;;
;;; Registers name as the name of a new static type derived from G_TYPE_PARAM.
;;; The type system uses the information contained in the GParamSpecTypeInfo
;;; structure pointed to by info to manage the GParamSpec type and its
;;; instances.
;;;
;;; name :
;;;     0-terminated string used as the name of the new GParamSpec type.
;;;
;;; pspec_info :
;;;     The GParamSpecTypeInfo for this GParamSpec type.
;;;
;;; Returns :
;;;     The new type identifier.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GParamSpecPool
;;;
;;; typedef struct _GParamSpecPool GParamSpecPool;
;;;
;;; A GParamSpecPool maintains a collection of GParamSpecs which can be quickly
;;; accessed by owner and name. The implementation of the GObject property
;;; system uses such a pool to store the GParamSpecs of the properties all
;;; object types.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_new ()
;;;
;;; GParamSpecPool * g_param_spec_pool_new (gboolean type_prefixing);
;;;
;;; Creates a new GParamSpecPool.
;;;
;;; If type_prefixing is TRUE, lookups in the newly created pool will allow to
;;; specify the owner as a colon-separated prefix of the property name, like
;;; "GtkContainer:border-width". This feature is deprecated, so you should
;;; always set type_prefixing to FALSE.
;;;
;;; type_prefixing :
;;;     Whether the pool will support type-prefixed property names.
;;;
;;; Returns :
;;;     a newly allocated GParamSpecPool
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_insert ()
;;;
;;; void g_param_spec_pool_insert (GParamSpecPool *pool,
;;;                                GParamSpec *pspec,
;;;                                GType owner_type);
;;;
;;; Inserts a GParamSpec in the pool.
;;;
;;; pool :
;;;     a GParamSpecPool.
;;;
;;; pspec :
;;;     the GParamSpec to insert
;;;
;;; owner_type :
;;;     a GType identifying the owner of pspec
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_remove ()
;;;
;;; void g_param_spec_pool_remove (GParamSpecPool *pool, GParamSpec *pspec);
;;;
;;; Removes a GParamSpec from the pool.
;;;
;;; pool :
;;;     a GParamSpecPool
;;;
;;; pspec :
;;;     the GParamSpec to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_lookup ()
;;;
;;; GParamSpec * g_param_spec_pool_lookup (GParamSpecPool *pool,
;;;                                        const gchar *param_name,
;;;                                        GType owner_type,
;;;                                        gboolean walk_ancestors);
;;;
;;; Looks up a GParamSpec in the pool.
;;;
;;; pool :
;;;     a GParamSpecPool
;;;
;;; param_name :
;;;     the name to look for
;;;
;;; owner_type :
;;;     the owner to look for
;;;
;;; walk_ancestors :
;;;     If TRUE, also try to find a GParamSpec with param_name owned by an
;;;     ancestor of owner_type.
;;;
;;; Returns :
;;;     The found GParamSpec, or NULL if no matching GParamSpec was found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_list ()
;;;
;;; GParamSpec ** g_param_spec_pool_list (GParamSpecPool *pool,
;;;                                       GType owner_type,
;;;                                       guint *n_pspecs_p);
;;;
;;; Gets an array of all GParamSpecs owned by owner_type in the pool.
;;;
;;; pool :
;;;     a GParamSpecPool
;;;
;;; owner_type :
;;;     the owner to look for
;;;
;;; n_pspecs_p :
;;;     return location for the length of the returned array
;;;
;;; Returns :
;;;     a newly allocated array containing pointers to all GParamSpecs owned by
;;;     owner_type in the pool
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pool_list_owned ()
;;;
;;; GList * g_param_spec_pool_list_owned (GParamSpecPool *pool,
;;;                                       GType owner_type);
;;;
;;; Gets an GList of all GParamSpecs owned by owner_type in the pool.
;;;
;;; pool :
;;;     a GParamSpecPool
;;;
;;; owner_type :
;;;     the owner to look for
;;;
;;; Returns :
;;;     a GList of all GParamSpecs owned by owner_type in the poolGParamSpecs
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.param-spec.lisp ------------------------------------
