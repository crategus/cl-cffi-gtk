;;; ----------------------------------------------------------------------------
;;; gobject.enumeration.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GObject 2.30.2 Reference Manual. See http://www.gtk.org
;;;
;;; ----------------------------------------------------------------------------
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
;;; Synopsis
;;; 
;;;     g-enum-value
;;;     g-enum-class
;;;
;;;     g-flags-value
;;;     g-flags-class
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
;;;
;;;     g-enum-register-static
;;;
;;;     g_enum_complete_type_info
;;;
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;
;;;     g-flags-register-static
;;;
;;;     g_flags_complete_type_info
;;; 
;;; Description
;;; 
;;; The GLib type system provides fundamental types for enumeration and flags
;;; types. (Flags types are like enumerations, but allow their values to be
;;; combined by bitwise or). A registered enumeration or flags type associates
;;; a name and a nickname with each allowed value, and the methods
;;; g_enum_get_value_by_name(), g_enum_get_value_by_nick(),
;;; g_flags_get_value_by_name() and g_flags_get_value_by_nick() can look up
;;; values by their name or nickname. When an enumeration or flags type is
;;; registered with the GLib type system, it can be used as value type for
;;; object properties, using g_param_spec_enum() or g_param_spec_flags().
;;; 
;;; GObject ships with a utility called glib-mkenums that can construct
;;; suitable type registration functions from C enumeration definitions.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------

;; Enums

(defvar *registered-enum-types* (make-hash-table :test 'equal))

(defun register-enum-type (name type)
  (setf (gethash name *registered-enum-types*) type))

(defun registered-enum-type (name)
  (gethash name *registered-enum-types*))

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
         (list `(at-init () ,(type-initializer-call type-initializer))))))

(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (foreign-symbol-pointer ,type-initializer)
                 (foreign-funcall-pointer
                  (foreign-symbol-pointer ,type-initializer) ()
                  g-type)
                 (warn "Type initializer '~A' is not available"
                       ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

(defun enum-value->definition (enum-value)
  (let ((value-name (intern (lispify-name (enum-item-nick enum-value))
                            (find-package :keyword)))
        (numeric-value (enum-item-value enum-value)))
    `(,value-name ,numeric-value)))

(defun get-g-enum-definition (type &optional lisp-name-package)
  (when (and (stringp type)
             (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (g-type (gtype type))
         (g-name (gtype-name g-type))
         (name (g-name->name g-name))
         (items (get-enum-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-enum ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'enum-value->definition items))))

;;; ----------------------------------------------------------------------------
;;; struct GEnumValue
;;; 
;;; struct GEnumValue {
;;;   gint	 value;
;;;   const gchar *value_name;
;;;   const gchar *value_nick;
;;; };
;;; 
;;; A structure which contains a single enum value, its name, and its nickname.
;;; 
;;; gint value;
;;; 	the enum value
;;;
;;; const gchar *value_name;
;;; 	the name of the value
;;;
;;; const gchar *value_nick;
;;; 	the nickname of the value
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-value
  (:value :int)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(export 'g-enum-value)

;;; ----------------------------------------------------------------------------
;;; struct GEnumClass
;;; 
;;; struct GEnumClass {
;;;   GTypeClass  g_type_class;
;;; 
;;;   gint	      minimum;
;;;   gint	      maximum;
;;;   guint	      n_values;
;;;   GEnumValue *values;
;;; };
;;; 
;;; The class of an enumeration type holds information about its possible
;;; values.
;;; 
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; gint minimum;
;;; 	the smallest possible value.
;;; 
;;; gint maximum;
;;; 	the largest possible value.
;;; 
;;; guint n_values;
;;; 	the number of possible values.
;;; 
;;; GEnumValue *values;
;;; 	an array of GEnumValue structs describing the individual values.
;;; ----------------------------------------------------------------------------

(defcstruct g-enum-class
  (:type-class g-type-class)
  (:minimum :int)
  (:maximum :int)
  (:n-values :uint)
  (:values (:pointer g-enum-value)))

(export 'g-enum-class)

;; "A structure describing a single enumeration item.
;;
;; See accessor functions:
;;    enum-item-name
;;    enum-item-value
;;    enum-item-nick

(defstruct enum-item
  name
  value
  nick)

;; Gets the list of enum items that belong to GEnum type type
;;
;; type :
;;     a string or an integer specifying GEnum type
;; return :
;;     a list of enum-item objects

(defun get-enum-items (type)
  (assert (g-type-is-a type +g-type-enum+))
  (let ((g-class (g-type-class-ref type)))
    (unwind-protect
      (loop
        with n = (foreign-slot-value g-class 'g-enum-class :n-values)
        with values = (foreign-slot-value g-class 'g-enum-class :values)
        for i from 0 below n
        for enum-value = (mem-aref values 'g-enum-value i)
        collect (make-enum-item
                  :name (foreign-slot-value enum-value 'g-enum-value :name)
                  :value (foreign-slot-value enum-value 'g-enum-value :value)
                 :nick (foreign-slot-value enum-value 'g-enum-value :nick)))
      (g-type-class-unref g-class))))

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
         (list `(at-init () ,(type-initializer-call type-initializer))))))

(defun flags-value->definition (flags-value)
  (let ((value-name (intern (lispify-name (flags-item-nick flags-value))
                            (find-package :keyword)))
        (numeric-value (flags-item-value flags-value)))
    `(,value-name ,numeric-value)))

(defun get-g-flags-definition (type &optional lisp-name-package)
  (when (and (stringp type) (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (g-type (gtype type))
         (g-name (gtype-name g-type))
         (name (g-name->name g-name))
         (items (get-flags-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-flags ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'flags-value->definition items))))

;;; ----------------------------------------------------------------------------
;;; struct GFlagsValue
;;; 
;;; struct GFlagsValue {
;;;   guint	 value;
;;;   const gchar *value_name;
;;;   const gchar *value_nick;
;;; };
;;; 
;;; A structure which contains a single flags value, its name, and its nickname.
;;; 
;;; guint value;
;;; 	the flags value
;;; 
;;; const gchar *value_name;
;;; 	the name of the value
;;; 
;;; const gchar *value_nick;
;;; 	the nickname of the value
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-value
  (:value :uint)
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:nick (:string :free-from-foreign nil :free-to-foreign nil)))

(export 'g-flags-value)

;;; ----------------------------------------------------------------------------
;;; struct GFlagsClass
;;; 
;;; struct GFlagsClass {
;;;   GTypeClass   g_type_class;
;;;   
;;;   guint	       mask;
;;;   guint	       n_values;
;;;   GFlagsValue *values;
;;; };
;;; 
;;; The class of a flags type holds information about its possible values.
;;; 
;;; GTypeClass g_type_class;
;;; 	the parent class
;;; 
;;; guint mask;
;;; 	a mask covering all possible values.
;;; 
;;; guint n_values;
;;; 	the number of possible values.
;;; 
;;; GFlagsValue *values;
;;; 	an array of GFlagsValue structs describing the individual values.
;;; ----------------------------------------------------------------------------

(defcstruct g-flags-class
  (:type-class g-type-class)
  (:mask :uint)
  (:n-values :uint)
  (:values (:pointer g-flags-value)))

(export 'g-flags-class)

;; A structure describing a single flags item.

(defstruct flags-item
  name
  value
  nick)

;; Gets the list of flags items that belong to GFlags type type
;; type is a string or an integer specifying GFlags type.
;; Returns a list of flags-item objects

(defun get-flags-items (type)
  (assert (g-type-is-a type +g-type-flags+))
  (let ((g-class (g-type-class-ref type)))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-flags-class :n-values)
            with values = (foreign-slot-value g-class 'g-flags-class :values)
            for i from 0 below n
            for flags-value = (mem-aref values 'g-flags-value i)
            collect (make-flags-item
                     :name (foreign-slot-value flags-value 'g-flags-value
                                               :name)
                     :value (foreign-slot-value flags-value 'g-flags-value
                                                :value)
                     :nick (foreign-slot-value flags-value 'g-flags-value
                                               :nick)))
      (g-type-class-unref g-class))))

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE()
;;; 
;;; #define G_ENUM_CLASS_TYPE(class) (G_TYPE_FROM_CLASS (class))
;;; 
;;; Get the type identifier from a given GEnumClass structure.
;;; 
;;; class :
;;; 	a GEnumClass
;;; 
;;; Returns :
;;; 	the GType
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS_TYPE_NAME()
;;; 
;;; #define G_ENUM_CLASS_TYPE_NAME(class)
;;;         (g_type_name (G_ENUM_CLASS_TYPE (class)))
;;; 
;;; Get the static type name from a given GEnumClass structure.
;;; 
;;; class :
;;; 	a GEnumClass
;;; 
;;; Returns :
;;; 	the type name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ENUM()
;;; 
;;; #define G_TYPE_IS_ENUM(type) (G_TYPE_FUNDAMENTAL (type) == G_TYPE_ENUM)
;;; 
;;; Checks whether type "is a" G_TYPE_ENUM.
;;; 
;;; type :
;;; 	a GType ID.
;;; 
;;; Returns :
;;; 	TRUE if type "is a" G_TYPE_ENUM.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ENUM_CLASS()
;;; 
;;; #define G_ENUM_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class),
;;;                              G_TYPE_ENUM, GEnumClass))
;;; 
;;; Casts a derived GEnumClass structure into a GEnumClass structure.
;;; 
;;; class :
;;; 	a valid GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_ENUM_CLASS()
;;; 
;;; #define G_IS_ENUM_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),
;;;                                 G_TYPE_ENUM))
;;; 
;;; Checks whether class "is a" valid GEnumClass structure of type G_TYPE_ENUM
;;; or derived.
;;; 
;;; class :
;;; 	a GEnumClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FLAGS()
;;; 
;;; #define G_TYPE_IS_FLAGS(type) (G_TYPE_FUNDAMENTAL (type) == G_TYPE_FLAGS)
;;; 
;;; Checks whether type "is a" G_TYPE_FLAGS.
;;; 
;;; type :
;;; 	a GType ID.
;;; 
;;; Returns :
;;; 	TRUE if type "is a" G_TYPE_FLAGS.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS()
;;; 
;;; #define G_FLAGS_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class),
;;;                               G_TYPE_FLAGS, GFlagsClass))
;;; 
;;; Casts a derived GFlagsClass structure into a GFlagsClass structure.
;;; 
;;; class :
;;; 	a valid GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_FLAGS_CLASS()
;;; 
;;; #define G_IS_FLAGS_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),
;;;                                  G_TYPE_FLAGS))
;;; 
;;; Checks whether class "is a" valid GFlagsClass structure of type
;;; G_TYPE_FLAGS or derived.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE()
;;; 
;;; #define G_FLAGS_CLASS_TYPE(class) (G_TYPE_FROM_CLASS (class))
;;; 
;;; Get the type identifier from a given GFlagsClass structure.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; 
;;; Returns :
;;; 	the GType
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_FLAGS_CLASS_TYPE_NAME()
;;; 
;;; #define G_FLAGS_CLASS_TYPE_NAME(class)
;;;         (g_type_name (G_FLAGS_CLASS_TYPE (class)))
;;; 
;;; Get the static type name from a given GFlagsClass structure.
;;; 
;;; class :
;;; 	a GFlagsClass
;;; 
;;; Returns :
;;; 	the type name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_enum_get_value ()
;;; 
;;; GEnumValue * g_enum_get_value (GEnumClass *enum_class, gint value);
;;; 
;;; Returns the GEnumValue for a value.
;;; 
;;; enum_class :
;;; 	a GEnumClass
;;; 
;;; value :
;;; 	the value to look up
;;; 
;;; Returns :
;;; 	the GEnumValue for value, or NULL if value is not a member of the
;;;     enumeration
;;; ----------------------------------------------------------------------------

;; parse-g-value-enum replaces the function g_enum_get_value.
;; It is called from the function parse-g-value.
;; TODO: Should we implement g-enum-get-value to be more complete?

(defun parse-g-value-enum (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-enum gvalue) enum-type)))

;; This function is called from set-g-value to set a GEnum Value.

(defun set-gvalue-enum (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
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
;;; 	a GEnumClass
;;; 
;;; name :
;;; 	the name to look up
;;; 
;;; Returns :
;;; 	the GEnumValue with name name, or NULL if the enumeration doesn't have
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
;;; 	a GEnumClass
;;; 
;;; nick :
;;; 	the nickname to look up
;;; 
;;; Returns :
;;; 	the GEnumValue with nickname nick, or NULL if the enumeration doesn't
;;;     have a member with that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g-enum-register-static (name static-values)
;;; 
;;; GType g_enum_register_static (const gchar *name,
;;;                               const GEnumValue *const_static_values);
;;; 
;;; Registers a new static enumeration type with the name name.
;;; 
;;; It is normally more convenient to let glib-mkenums generate a
;;; my_enum_get_type() function from a usual C enumeration definition than to
;;; write one yourself using g_enum_register_static().
;;; 
;;; name :
;;; 	A nul-terminated string used as the name of the new type.
;;; 
;;; static-values :
;;; 	An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0. GObject
;;;     keeps a reference to the data, so it cannot be stack-allocated.
;;; 
;;; Returns :
;;; 	The new type identifier.
;;; ----------------------------------------------------------------------------

(defcfun ("g_enum_register_static" g-enum-register-static) g-type-designator
  (name :string)
  (static-values (:pointer g-enum-value)))

(export 'g-enum-register-static)

;;; ----------------------------------------------------------------------------
;;; g_enum_complete_type_info ()
;;; 
;;; void g_enum_complete_type_info (GType g_enum_type,
;;;                                 GTypeInfo *info,
;;;                                 const GEnumValue *const_values);
;;; 
;;; This function is meant to be called from the complete_type_info() function
;;; of a GTypePlugin implementation, as in the following example:
;;; 
;;;  1 static void
;;;  2 my_enum_complete_type_info (GTypePlugin     *plugin,
;;;  3                             GType            g_type,
;;;  4                             GTypeInfo       *info,
;;;  5                             GTypeValueTable *value_table)
;;;  6 {
;;;  7   static const GEnumValue values[] = {
;;;  8     { MY_ENUM_FOO, "MY_ENUM_FOO", "foo" },
;;;  9     { MY_ENUM_BAR, "MY_ENUM_BAR", "bar" },
;;; 10     { 0, NULL, NULL }
;;; 11   };
;;; 12 
;;; 13   g_enum_complete_type_info (type, info, values);
;;; 14 }
;;; 
;;; g_enum_type :
;;; 	the type identifier of the type being completed
;;; 
;;; info :
;;; 	the GTypeInfo struct to be filled in
;;; 
;;; const_values :
;;; 	An array of GEnumValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
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
;;; 	a GFlagsClass
;;; 
;;; value :
;;; 	the value
;;; 
;;; Returns :
;;; 	the first GFlagsValue which is set in value, or NULL if none is set
;;; ----------------------------------------------------------------------------

;; parse-g-value-flags replaces the function g_flags_get_value.
;; It is called from the function parse-g-value.
;; TODO: Should we implement g-flags-get-first-value to be more complete?

(defun parse-g-value-flags (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-flags gvalue) flags-type)))

;; This function is called from set-g-value to set a GFlag value.

(defun set-gvalue-flags (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (gtype-name g-type))
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
;;; 	a GFlagsClass
;;; 
;;; name :
;;; 	the name to look up
;;; 
;;; Returns :
;;; 	the GFlagsValue with name name, or NULL if there is no flag with that
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
;;; 	a GFlagsClass
;;; 
;;; nick :
;;; 	the nickname to look up
;;; 
;;; Returns :
;;; 	the GFlagsValue with nickname nick, or NULL if there is no flag with
;;;     that nickname
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_flags_register_static ()
;;; 
;;; GType g_flags_register_static (const gchar *name,
;;;                                const GFlagsValue *const_static_values);
;;; 
;;; Registers a new static flags type with the name name.
;;; 
;;; It is normally more convenient to let glib-mkenums generate a
;;; my_flags_get_type() function from a usual C enumeration definition than to
;;; write one yourself using g_flags_register_static().
;;; 
;;; name :
;;; 	A nul-terminated string used as the name of the new type.
;;; 
;;; static-values :
;;; 	An array of GFlagsValue structs for the possible flags values. The
;;;     array is terminated by a struct with all members being 0. GObject keeps
;;;     a reference to the data, so it cannot be stack-allocated.
;;; 
;;; Returns :
;;; 	The new type identifier.
;;; ----------------------------------------------------------------------------

(defcfun ("g_flags_register_static" g-flags-register-static) g-type-designator
  (name :string)
  (static-values (:pointer g-flags-value)))

(export 'g-flags-register-static)

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
;;; 	the type identifier of the type being completed
;;; 
;;; info :
;;; 	the GTypeInfo struct to be filled in
;;; 
;;; const_values :
;;; 	An array of GFlagsValue structs for the possible enumeration values.
;;;     The array is terminated by a struct with all members being 0.
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.enumeration.lisp -----------------------------------
