;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.32.4. See http://www.gtk.org
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------

;; Enums

(defvar *registered-enum-types* (make-hash-table :test 'equal))

(defun register-enum-type (name type)
  (setf (gethash name *registered-enum-types*) type))

(defun registered-enum-type (name)
  (gethash name *registered-enum-types*))

;; Flags

(defvar *registered-flags-types* (make-hash-table :test 'equal))

(defun register-flags-type (name type)
  (setf (gethash name *registered-flags-types*) type))

(defun registered-flags-type (name)
  (gethash name *registered-flags-types*))

;;; ----------------------------------------------------------------------------
;;; define-g-enum
;;;
;;; Defines a GEnum type for enumeration. Generates corresponding CFFI
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

(defmacro define-g-enum (g-name name (&key (export t) type-initializer)
                                      &body values)
  `(progn
     (defcenum ,name ,@values)
     (register-enum-type ,g-name ',name)
     ,@(when export
         (list `(export ',name
                        (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
         (list `(glib::at-init () ,(type-initializer-call type-initializer))))))

(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (foreign-symbol-pointer ,type-initializer)
                 (foreign-funcall-pointer
                  (foreign-symbol-pointer ,type-initializer) ()
                  %g-type)
                 (warn "Type initializer '~A' is not available"
                       ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

;;; ----------------------------------------------------------------------------
;;; struct GEnumValue
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(export 'g-enum-value)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-enum-value atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-enum-value atdoc:*external-symbols*)
 "@version{2013-2-6}
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
    @entry[:value]{the enum value}
    @entry[:name]{the name of the value}
    @entry{:nick]{the nickname of the value}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GEnumClass
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-class
  (:type-class g-type-class)
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))

(export 'g-enum-class)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-enum-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-enum-class atdoc:*external-symbols*)
 "@version{2013-2-6}
  @begin{short}
    The class of an enumeration type holds information about its possible
    values.
  @end{short}
  @begin{pre}
(defcstruct g-enum-class
  (:type-class g-type-class)
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{the parent class}
    @entry[:minimum]{the smallest possible value.}
    @entry[:maximum]{the largest possible value.}
    @entry[:n-values]{the number of possible values.}
    @entry[:values]{an array of @symbol{g-enum-value} structs describing the
      individual values.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-value
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(export 'g-flags-value)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-flags-value atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-flags-value atdoc:*external-symbols*)
 "@version{2013-2-6}
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
    @entry[:value]{the flags value}
    @entry[:name]{the name of the value}
    @entry[:nick]{the nickname of the value}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; Struct g-flags-class
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-class
  (:type-class g-type-class)
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))

(export 'g-flags-class)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-flags-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-flags-class atdoc:*external-symbols*)
 "@version{2013-2-6}
  @begin{short}
    The class of a flags type holds information about its possible values.
  @end{short}
  @begin{pre}
(defcstruct g-flags-class
  (:type-class g-type-class)
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:type-class]{the parent class}
    @entry[:mask]{a mask covering all possible values.}
    @entry[:n-values]{the number of possible values.}
    @entry[:values]{an array of @symbol{g-flags-value} structs describing the
      individual values.}
  @end{table}")

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
         (list `(glib::at-init () ,(type-initializer-call type-initializer))))))

;;; ----------------------------------------------------------------------------
;;; g-enum-class-type
;;; ----------------------------------------------------------------------------

(declaim (inline g-enum-class-type))

(defun g-enum-class-type (class)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[class]{a @symbol{g-enum-class}}
  @return{the @class{g-type}}
  @begin{short}
    Get the type identifier from a given @symbol{g-enum-class} structure.
  @end{short}"
  (g-type-from-class class))

(export 'g-enum-class-type)

;;; ----------------------------------------------------------------------------
;;; g-enum-class-type-name
;;; ----------------------------------------------------------------------------

(declaim (inline g-enum-class-type-name))

(defun g-enum-class-type-name (class)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[class]{a @symbol{g-enum-class}}
  @return{The type name.}
  @begin{short}
    Get the static type name from a given GEnumClass structure.
  @end{short}"
  (g-type-name (g-type-from-class class)))

(export 'g-enum-class-type-name)

;;; ----------------------------------------------------------------------------
;;; g-type-is-enum
;;; ----------------------------------------------------------------------------

(declaim (inline g-type-is-enum))

(defun g-type-is-enum (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[type]{a @class{g-type} ID.}
  @return{@arg{true} if type \"is a\" @variable{+g-type-enum+}.}
  @begin{short}
    Checks whether type \"is a\" @variable{+g-type-enum}.
  @end{short}"
  (eql (g-type-fundamental gtype) +g-type-enum+))

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
;;; G_IS_ENUM_CLASS()
;;;
;;; #define G_IS_ENUM_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_ENUM))
;;;
;;; Checks whether class "is a" valid GEnumClass structure of type G_TYPE_ENUM
;;; or derived.
;;;
;;; class :
;;;     a GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g-type-is-flags
;;; ----------------------------------------------------------------------------

(declaim (inline g-type-is-flags))

(defun g-type-is-flags (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[type]{a @class{g-type} ID.}
  @return{@arg{true} if type \"is a\" @variable{+g-type-flags+}.}
  @begin{short}
    Checks whether type \"is a\" @variable{+g-type-flags+}.
  @end{short}"
  (eql (g-type-fundamental gtype) +g-type-flags+))

(export 'g-type-is-flags)

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS()
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
;;; G_IS_FLAGS_CLASS()
;;;
;;; #define G_IS_FLAGS_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_FLAGS))
;;;
;;; Checks whether class "is a" valid GFlagsClass structure of type G_TYPE_FLAGS
;;; or derived.
;;;
;;; class :
;;;     a GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g-flags-class-type
;;; ----------------------------------------------------------------------------

(declaim (inline g-flags-class-type))

(defun g-flags-class-type (class)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[class]{a @symbol{g-flags-class}}
  @retrun{the @class{g-type}}
  @begin{short}
    Get the type identifier from a given @symbol{g-flags-class} structure.
  @end{short}"
  (g-type-from-class class))

(export 'g-flags-class-type)

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME()
;;; ----------------------------------------------------------------------------

(declaim (inline g-flags-class-type-name))

(defun g-flags-class-type-name (class)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[class]{a @symbol{g-flags-class}}
  @return{The type name.}
  @begin{short}
    Get the static type name from a given @symbol{g-flags-class} structure.
  @end{short}"
  (g-type-name (g-type-from-class class)))

(export 'g-flags-class-type-name)

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

;; parse-g-value-enum replaces the function g_enum_get_value.
;; It is called from the function parse-g-value.
;; TODO: Should we implement g-enum-get-value to be more complete?

(defun parse-g-value-enum (gvalue)
  (let* ((gtype (g-value-type gvalue))
         (enum-type (registered-enum-type (g-type-name gtype))))
    (unless enum-type
      (error "Enum ~A is not registered" (g-type-name gtype)))
    (convert-from-foreign (g-value-get-enum gvalue) enum-type)))

;; This function is called from set-g-value to set a GEnum Value.

(defun set-gvalue-enum (gvalue value)
  (let* ((type (g-value-type gvalue))
         (type-name (gtype-name type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (g-value-set-enum gvalue (convert-to-foreign value enum-type))))

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
;;;     the GEnumValue with name name, or NULL if the enumeration doesn't have a
;;;     member with that name
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
;;;     the GEnumValue with nickname nick, or NULL if the enumeration doesn't
;;;     have a member with that nickname
;;; ----------------------------------------------------------------------------

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

;; parse-g-value-flags replaces the function g_flags_get_value.
;; It is called from the function parse-g-value.
;; TODO: Should we implement g-flags-get-first-value to be more complete?

(defun parse-g-value-flags (gvalue)
  (let* ((type (g-value-type gvalue))
         (type-name (gtype-name type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-flags gvalue) flags-type)))

;; This function is called from set-g-value to set a GFlag value.

(defun set-gvalue-flags (gvalue value)
  (let* ((type (g-value-type gvalue))
         (type-name (gtype-name type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (g-value-set-flags gvalue (convert-to-foreign value flags-type))))

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
;;; g-enum-register-static
;;; ----------------------------------------------------------------------------

(defcfun ("g_enum_register_static" g-enum-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[name]{A nul-terminated string used as the name of the new type.}
  @argument[static-values]{An array of @symbol{g-enum-value} structs for the
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
  (static-values (:pointer g-enum-value)))

(export 'g-enum-register-static)

;;; ----------------------------------------------------------------------------
;;; g_flags_register_static ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_flags_register_static" g-flags-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @argument[name]{A nul-terminated string used as the name of the new type.}
  @argument[static_values]{An array of GFlagsValue structs for the possible
    flags values. The array is terminated by a struct with all members being 0.
    @class{g-object} keeps a reference to the data, so it cannot be
    stack-allocated.}
  @return{The new type identifier.}
  @begin{short}
    Registers a new static flags type with the name name.
  @end{short}

  It is normally more convenient to let glib-mkenums generate a
  @code{my_flags_get_type()} function from a usual C enumeration definition than
  to write one yourself using @fun{g-flags-register-static}."
  (name :string)
  (static-values (:pointer g-flags-value)))

(export 'g-flags-register-static)

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
;;;     An array of GEnumValue structs for the possible enumeration values. The
;;;     array is terminated by a struct with all members being 0.
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
;;;     An array of GFlagsValue structs for the possible enumeration values. The
;;;     array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------