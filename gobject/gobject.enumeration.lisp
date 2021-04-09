;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; Enumeration and Flag Types
;;;
;;;     Enumeration and flags types
;;;
;;; Types and Values
;;;
;;;     GEnumClass
;;;     GFlagsClass
;;;     GEnumValue
;;;     GFlagsValue
;;;
;;; Functions
;;;
;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME
;;;     G_TYPE_IS_ENUM
;;;     G_ENUM_CLASS
;;;     G_IS_ENUM_CLASS
;;;     G_TYPE_IS_FLAGS
;;;     G_FLAGS_CLASS
;;;     G_IS_FLAGS_CLASS
;;;     G_FLAGS_CLASS_TYPE
;;;     G_FLAGS_CLASS_TYPE_NAME
;;;
;;;     g_enum_get_value
;;;     g_enum_get_value_by_name
;;;     g_enum_get_value_by_nick
;;;     g_enum_to_string
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;     g_flags_to_string
;;;     g_enum_register_static
;;;     g_flags_register_static
;;;     g_enum_complete_type_info
;;;     g_flags_complete_type_info
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;; Enums

(defvar *registered-enum-types* (make-hash-table :test 'equal))

(defun register-enum-type (name type)
  (setf (gethash name *registered-enum-types*) type))

(defun registered-enum-type (name)
  (gethash name *registered-enum-types*))

;;; ----------------------------------------------------------------------------
;;; define-g-enum
;;;
;;; Defines a GEnum type for enumeration. Generates the corresponding CFFI
;;; definition.
;;;
;;; Example:
;;;
;;; (define-g-enum "GdkGrabStatus" grab-status ()
;;;   :success
;;;   :already-grabbed
;;;   :invalid-time
;;;   :not-viewable
;;;   :frozen)
;;;
;;; (define-g-enum "GdkExtensionMode" gdk-extension-mode
;;;    (:export t
;;;     :type-initializer "gdk_extension_mode_get_type")
;;;    (:none 0)
;;;    (:all 1)
;;;    (:cursor 2))
;;;
;;; g-name :
;;;     a string. Specifies the GEnum name
;;;
;;; name :
;;;     a symbol. Names the enumeration type.
;;;
;;; export :
;;;     a boolean. If true, name will be exported.
;;;
;;; type-initializer :
;;;     a NIL or a string or a function designator. If non-NIL, specifies the
;;;     function that initializes the type: string specifies a C function that
;;;     returns the GType value and function designator specifies the Lisp
;;;     function.
;;;
;;; values :
;;;     values for enum. Each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of
;;;     enumeration, and integer-value is an C integer for enumeration item.
;;;     If integer-value is not specified, it is generated automatically
;;;    (see CFFI manual)
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement the key argument :allow-undeclared-values

(defmacro define-g-enum (g-name name (&key (export t)
                                           type-initializer)
                                     &body values)
  `(progn
     (defcenum (,name :int) ,@values)
     (register-enum-type ,g-name ',name)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init::at-init ()
                   ,(type-initializer-call type-initializer))))))

(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (foreign-symbol-pointer ,type-initializer)
                 (foreign-funcall-pointer
                     (foreign-symbol-pointer ,type-initializer) ()
                     g-size)
                 (warn "Type initializer '~A' is not available"
                       ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

;; parse-g-value-enum  is called from the function parse-g-value.

(defun parse-g-value-enum (gvalue)
  (let* ((gtype (g-value-type gvalue))
         (enum-type (registered-enum-type (gtype-name gtype))))
    (unless enum-type
      (error "Enum ~A is not registered" (gtype-name gtype)))
    (convert-from-foreign (g-value-enum gvalue) enum-type)))

;; This function is called from set-g-value to set a GEnum Value.

(defun set-g-value-enum (gvalue value)
  (let* ((gtype (g-value-type gvalue))
         (enum-type (registered-enum-type (gtype-name gtype))))
    (unless enum-type
      (error "Enum ~A is not registered" (gtype-name gtype)))
    (setf (g-value-enum gvalue) (convert-to-foreign value enum-type))))

;;; ----------------------------------------------------------------------------
;;; struct GEnumValue
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-enum-value atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-enum-value atdoc:*external-symbols*)
 "@version{2021-4-7}
  @begin{short}
    A structure which contains a single enum value, its name, and its nickname.
  @end{short}
  @begin{pre}
(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{pre}
  @begin[code]{table}
    @entry[:value]{The enum value.}
    @entry[:name]{The name of the value.}
    @entry[:nick]{The nickname of the value.}
  @end{table}
  @see-symbol{g-enum-class}")

(export 'g-enum-value)

;;; ----------------------------------------------------------------------------
;;; struct GEnumClass
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-class
  (:type-class (:pointer (:struct g-type-class)))
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer (:struct g-enum-value))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-enum-class atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-enum-class atdoc:*external-symbols*)
 "@version{2021-4-7}
  @begin{short}
    The class of an enumeration type holds information about its possible
    values.
  @end{short}
  @begin{pre}
(defcstruct g-enum-class
  (:type-class (:pointer (:struct g-type-class)))
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{The parent class.}
    @entry[:minimum]{The smallest possible value.}
    @entry[:maximum]{The largest possible value.}
    @entry[:n-values]{The number of possible values.}
    @entry[:values]{An array of @symbol{g-enum-value} structures describing the
      individual values.}
  @end{table}
  @see-symbol{g-enum-value}")

(export 'g-enum-class)

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of g-type-from-class.

(defun g-enum-class-type (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-enum-class} structure}
  @return{The @class{g-type} ID for @arg{class}.}
  @begin{short}
    Get the type identifier from a given @symbol{g-enum-class} structure.
  @end{short}
  @see-symbol{g-enum-class}
  @see-class{g-type}"
  (g-type-from-class class))

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE_NAME
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implemenation. We do not export this function.
;; It is a call of g-type-name and g-type-from-class.

(defun g-enum-class-type-name (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-enum-class} structure}
  @return{A string with the type name.}
  @begin{short}
    Get the type name from a given @symbol{g-enum-class} structure.
  @end{short}
  @see-symbol{g-enum-class}"
  (g-type-name (g-type-from-class class)))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ENUM
;;; ----------------------------------------------------------------------------

(declaim (inline g-type-is-enum))

(defun g-type-is-enum (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-7}
  @argument[gtype]{a @class{g-type} ID}
  @return{@em{True} if @arg{gtype} is a \"GEnum\" type.}
  @begin{short}
    Checks whether @arg{gtype} is a \"GEnum\" type.
  @end{short}
  @see-class{g-type}"
  (eq (g-type-fundamental gtype) (gtype "GEnum")))

(export 'g-type-is-enum)

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS()
;;;
;;; #define G_ENUM_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_ENUM, GEnumClass))
;;;
;;; Casts a derived GEnumClass structure into a GEnumClass structure.
;;;
;;; class :
;;;     a valid GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_ENUM_CLASS
;;; ----------------------------------------------------------------------------

;; not exported

(defun g-is-enum-class (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-enum-class} structure}
  @begin{short}
    Checks whether class is a valid @symbol{g-enum-class} structure of type
    @var{+g-type-enum+} or derived.
  @end{short}
  @see-symbol{g-enum-class}
  @see-variable{+g-type-enum+}"
  (g-type-check-class-type class +g-type-enum+))

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value ()
;;;
;;; GEnumValue * g_enum_get_value (GEnumClass *enum_class, gint value);
;;;
;;; Returns the GEnumValue for a value.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; value :
;;;     the value to look up
;;;
;;; Returns :
;;;     the GEnumValue for value, or NULL if value is not a member of the
;;;     enumeration
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_name ()
;;;
;;; GEnumValue * g_enum_get_value_by_name (GEnumClass *enum_class,
;;;                                        const gchar *name);
;;;
;;; Looks up a GEnumValue by name.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; name :
;;;     the name to look up
;;;
;;; Returns :
;;;     the GEnumValue with name name, or NULL if the enumeration does not have
;;;     a member with that name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value_by_nick ()
;;;
;;; GEnumValue * g_enum_get_value_by_nick (GEnumClass *enum_class,
;;;                                        const gchar *nick);
;;;
;;; Looks up a GEnumValue by nickname.
;;;
;;; enum_class :
;;;     a GEnumClass
;;;
;;; nick :
;;;     the nickname to look up
;;;
;;; Returns :
;;;     the GEnumValue with nickname nick, or NULL if the enumeration does not
;;;     have a member with that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_to_string ()
;;;
;;; gchar *
;;; g_enum_to_string (GType g_enum_type, gint value);
;;;
;;; Pretty-prints value in the form of the enum’s name.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;;
;;; g_enum_type :
;;;     the type identifier of a GEnumClass type
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     a newly-allocated text string.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;; Flags

(defvar *registered-flags-types* (make-hash-table :test 'equal))

(defun register-flags-type (name type)
  (setf (gethash name *registered-flags-types*) type))

(defun registered-flags-type (name)
  (gethash name *registered-flags-types*))

;;; ----------------------------------------------------------------------------
;;; define-g-flags
;;;
;;; Defines a GFlags type for enumeration that can combine its values.
;;; Generates corresponding CFFI definition. Values of this type are lists of
;;; keywords that are combined.
;;;
;;; Example:
;;;
;;; (define-g-flags \"GdkWindowState\" window-state ()
;;;   (:withdrawn 1)
;;;   (:iconified 2) (:maximized 4) (:sticky 8) (:fullscreen 16)
;;;   (:above 32) (:below 64))
;;;
;;; g-name :
;;;     a string. Specifies the GEnum name
;;;
;;; name :
;;;     a symbol. Names the enumeration type.
;;;
;;; export :
;;;     a boolean. If true, name will be exported.
;;;
;;; type-initializer :
;;;     a  NIL or a string or a function designator. If non-NIL, specifies the
;;;     function that initializes the type: string specifies a C function that
;;;     returns the GType value and function designator specifies the Lisp
;;;     function.
;;;
;;; values :
;;;     values for flags. Each value is a keyword or a list
;;;     (keyword integer-value). keyword corresponds to Lisp value of a flag,
;;;     and integer-value is an C integer for flag. If integer-value is not
;;;     specified, it is generated automatically (see CFFI manual)
;;; ----------------------------------------------------------------------------

(defmacro define-g-flags (g-name name (&key (export t) type-initializer)
                                       &body values)
  `(progn
     (defbitfield ,name ,@values)
     (register-flags-type ,g-name ',name)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib-init::at-init ()
                   ,(type-initializer-call type-initializer))))))

;; parse-g-value-flags is called from the function parse-g-value.

(defun parse-g-value-flags (gvalue)
  (let* ((gtype (g-value-type gvalue))
         (flags-type (registered-flags-type (gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (gtype-name gtype)))
    (convert-from-foreign (g-value-flags gvalue) flags-type)))

;; This function is called from set-g-value to set a GFlag value.

(defun set-g-value-flags (gvalue value)
  (let* ((gtype (g-value-type gvalue))
         (flags-type (registered-flags-type (gtype-name gtype))))
    (unless flags-type
      (error "Flags ~A is not registered." (gtype-name gtype)))
    (setf (g-value-flags gvalue) (convert-to-foreign value flags-type))))

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-value
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-flags-value atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-flags-value atdoc:*external-symbols*)
 "@version{2021-4-7}
  @begin{short}
    A structure which contains a single flags value, its name, and its nickname.
  @end{short}
  @begin{pre}
(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))
  @end{pre}
  @begin[code]{table}
    @entry[:value]{The flags value.}
    @entry[:name]{The name of the value.}
    @entry[:nick]{The nickname of the value.}
  @end{table}
  @see-symbol{g-flags-class}")

(export 'g-flags-value)

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-class
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-class
  (:type-class (:pointer (:struct g-type-class)))
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer (:struct g-flags-value))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-flags-class atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-flags-class atdoc:*external-symbols*)
 "@version{2021-4-7}
  @begin{short}
    The class of a flags type holds information about its possible values.
  @end{short}
  @begin{pre}
(defcstruct g-flags-class
  (:type-class (:pointer (:struct g-type-class)))
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{The parent class.}
    @entry[:mask]{A mask covering all possible values.}
    @entry[:n-values]{The number of possible values.}
    @entry[:values]{An array of @symbol{g-flags-value} structures describing
      the individual values.}
  @end{table}
  @see-symbol{g-flags-value}")

(export 'g-flags-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FLAGS
;;; ----------------------------------------------------------------------------

(declaim (inline g-type-is-flags))

(defun g-type-is-flags (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-7}
  @argument[type]{a @class{g-type} ID}
  @return{@em{True} if @arg{type} is a \"GFlags\" type.}
  @begin{short}
    Checks whether @arg{type} is a \"GFlags\" type.
  @end{short}
  @see-class{g-type}"
  (eq (g-type-fundamental gtype) (gtype "GFlags")))

(export 'g-type-is-flags)

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS
;;;
;;; #define G_FLAGS_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_FLAGS, GFlagsClass))
;;;
;;; Casts a derived GFlagsClass structure into a GFlagsClass structure.
;;;
;;; class :
;;;     a valid GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_FLAGS_CLASS
;;; ----------------------------------------------------------------------------

;; not exported

(defun g-is-flags-class (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-flags-class} structure}
  @begin{short}
    Checks whether @arg{class} is a valid @symbol{g-enum-class} structure
    of type @var{+g-type-flags+} or derived.
  @end{short}
  @see-symbol{g-flags-class}
  @see-variable{+g-type-flags+}"
  (g-type-check-class-type class +g-type-flags+))

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. We do not export this function.
;; It is a call of g-type-from-class.

(defun g-flags-class-type (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-flags-class} structure}
  @return{The @class{g-type} ID for @arg{class}.}
  @begin{short}
    Get the type identifier from a given @symbol{g-flags-class} structure.
  @end{short}
  @see-symbol{g-flags-class}
  @see-class{g-type}"
  (g-type-from-class class))

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implemenation. We do not export this function.
;; It is a call of g-type-name and g-type-from-class.

(defun g-flags-class-type-name (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-16}
  @argument[class]{a @symbol{g-flags-class} structure}
  @return{A string with the type name.}
  @begin{short}
    Get the type name from a given @symbol{g-flags-class} structure.
  @end{short}
  @see-symbol{g-flags-class}"
  (g-type-name (g-type-from-class class)))

;;; ----------------------------------------------------------------------------
;;; g_flags_get_first_value ()
;;;
;;; GFlagsValue * g_flags_get_first_value (GFlagsClass *flags_class,
;;;                                        guint value);
;;;
;;; Returns the first GFlagsValue which is set in value.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     the first GFlagsValue which is set in value, or NULL if none is set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_name ()
;;;
;;; GFlagsValue * g_flags_get_value_by_name (GFlagsClass *flags_class,
;;;                                          const gchar *name);
;;;
;;; Looks up a GFlagsValue by name.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; name :
;;;     the name to look up
;;;
;;; Returns :
;;;     the GFlagsValue with name name, or NULL if there is no flag with that
;;;     name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_get_value_by_nick ()
;;;
;;; GFlagsValue * g_flags_get_value_by_nick (GFlagsClass *flags_class,
;;;                                          const gchar *nick);
;;;
;;; Looks up a GFlagsValue by nickname.
;;;
;;; flags_class :
;;;     a GFlagsClass
;;;
;;; nick :
;;;     the nickname to look up
;;;
;;; Returns :
;;;     the GFlagsValue with nickname nick, or NULL if there is no flag with
;;;     that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_to_string ()
;;;
;;; gchar *
;;; g_flags_to_string (GType flags_type, guint value);
;;;
;;; Pretty-prints value in the form of the flag names separated by | and sorted.
;;; Any extra bits will be shown at the end as a hexadecimal number.
;;;
;;; This is intended to be used for debugging purposes. The format of the
;;; output may change in the future.
;;;
;;; flags_type :
;;;     the type identifier of a GFlagsClass type
;;;
;;; value :
;;;     the value
;;;
;;; Returns :
;;;     a newly-allocated text string.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_register_static ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. This function is not
;; exported.

(defcfun ("g_enum_register_static" g-enum-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[name]{a string used as the name of the new type}
  @argument[static-values]{an array of @symbol{g-enum-value} structs for the
    possible enumeration values. The array is terminated by a struct with all
    members being 0. @class{g-object} keeps a reference to the data, so it
    cannot be stack-allocated.}
  @return{The new type identifier.}
  @begin{short}
    Registers a new static enumeration type with the name name.
  @end{short}

  It is normally more convenient to let glib-mkenums generate a
  @code{my_enum_get_type()} function from a usual C enumeration definition than
  to write one yourself using @sym{g-enum-register-static}."
  (name :string)
  (static-values (:pointer (:struct g-enum-value))))

;;; ----------------------------------------------------------------------------
;;; g_flags_register_static ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation. This function is not
;; exported.

(defcfun ("g_flags_register_static" g-flags-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[name]{a string used as the name of the new type}
  @argument[static-values]{an array of @symbol{g-flags-value} structs for the
    possible flags values. The array is terminated by a struct with all members
    being 0. @class{g-object} keeps a reference to the data, so it cannot be
    stack-allocated.}
  @return{The new type identifier.}
  @begin{short}
    Registers a new static flags type with the name name.
  @end{short}

  It is normally more convenient to let @code{glib-mkenums} generate a
  @code{my_flags_get_type()} function from a usual C enumeration definition
  than to write one yourself using @sym{g-flags-register-static}."
  (name :string)
  (static-values (:pointer (:struct g-flags-value))))

;;; ----------------------------------------------------------------------------
;;; g_enum_complete_type_info ()
;;;
;;; void g_enum_complete_type_info (GType g_enum_type,
;;;                                 GTypeInfo *info,
;;;                                 const GEnumValue *const_values);
;;;
;;; This function is meant to be called from the complete_type_info function of
;;; a GTypePlugin implementation, as in the following example:
;;;
;;;   static void
;;;   my_enum_complete_type_info (GTypePlugin     *plugin,
;;;                               GType            g_type,
;;;                               GTypeInfo       *info,
;;;                               GTypeValueTable *value_table)
;;;   {
;;;     static const GEnumValue values[] = {
;;;       { MY_ENUM_FOO, "MY_ENUM_FOO", "foo" },
;;;       { MY_ENUM_BAR, "MY_ENUM_BAR", "bar" },
;;;       { 0, NULL, NULL }
;;;     };
;;;
;;;     g_enum_complete_type_info (type, info, values);
;;;   }
;;;
;;; g_enum_type :
;;;     the type identifier of the type being completed
;;;
;;; info :
;;;     the GTypeInfo struct to be filled in
;;;
;;; const_values :
;;;     An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_complete_type_info ()
;;;
;;; void g_flags_complete_type_info (GType g_flags_type,
;;;                                  GTypeInfo *info,
;;;                                  const GFlagsValue *const_values);
;;;
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation, see the example for
;;; g_enum_complete_type_info() above.
;;;
;;; g_flags_type :
;;;     the type identifier of the type being completed
;;;
;;; info :
;;;     the GTypeInfo struct to be filled in
;;;
;;; const_values :
;;;     An array of GFlagsValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------
