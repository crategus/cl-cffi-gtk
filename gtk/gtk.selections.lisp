;;; ----------------------------------------------------------------------------
;;; gtk.selections.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Selections
;;;
;;;     Functions for handling inter-process communication via selections
;;;
;;; Types and Values
;;;
;;;     GtkSelectionData
;;;     GtkTargetFlags  <-- gtk.drag-and-drop.lisp
;;;     GtkTargetEntry
;;;     GtkTargetList
;;;     GtkTargetPair
;;;
;;; Functions
;;;
;;;     gtk_target_entry_new
;;;     gtk_target_entry_copy
;;;     gtk_target_entry_free
;;;
;;;     gtk_target_list_new
;;;     gtk_target_list_ref
;;;     gtk_target_list_unref
;;;     gtk_target_list_add
;;;     gtk_target_list_add_table
;;;     gtk_target_list_add_text_targets
;;;     gtk_target_list_add_image_targets
;;;     gtk_target_list_add_uri_targets
;;;     gtk_target_list_add_rich_text_targets
;;;     gtk_target_list_remove
;;;     gtk_target_list_find
;;;
;;;     gtk_target_table_free
;;;     gtk_target_table_new_from_list
;;;
;;;     gtk_selection_owner_set
;;;     gtk_selection_owner_set_for_display
;;;     gtk_selection_add_target
;;;     gtk_selection_add_targets
;;;     gtk_selection_clear_targets
;;;     gtk_selection_convert
;;;
;;;     gtk_selection_data_set
;;;     gtk_selection_data_set_text
;;;     gtk_selection_data_get_text
;;;     gtk_selection_data_set_pixbuf
;;;     gtk_selection_data_get_pixbuf
;;;     gtk_selection_data_set_uris
;;;     gtk_selection_data_get_uris
;;;     gtk_selection_data_get_targets
;;;     gtk_selection_data_targets_include_image
;;;     gtk_selection_data_targets_include_text
;;;     gtk_selection_data_targets_include_uri
;;;     gtk_selection_data_targets_include_rich_text
;;;     gtk_selection_data_get_selection
;;;     gtk_selection_data_get_data
;;;     gtk_selection_data_get_length
;;;     gtk_selection_data_get_data_with_length
;;;     gtk_selection_data_get_data_type
;;;     gtk_selection_data_get_display
;;;     gtk_selection_data_get_format
;;;     gtk_selection_data_get_target
;;;
;;;     gtk_targets_include_image
;;;     gtk_targets_include_text
;;;     gtk_targets_include_uri
;;;     gtk_targets_include_rich_text
;;;
;;;     gtk_selection_remove_all
;;;     gtk_selection_data_copy
;;;     gtk_selection_data_free
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── GtkSelectionData
;;;     ╰── GtkTargetList
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkTargetFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTargetFlags" gtk-target-flags
  (:export t
   :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-flags atdoc:*symbol-name-alias*)
      "Flags"
      (gethash 'gtk-target-flags atdoc:*external-symbols*)
 "@version{2021-3-26}
  @begin{short}
    The @sym{gtk-target-flags} flags is used to specify constraints on
    a @class{gtk-target-entry} instance.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkTargetFlags\" gtk-target-flags
  (:export t
   :type-initializer \"gtk_target_flags_get_type\")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))
  @end{pre}
  @begin[code]{table}
    @entry[:same-app]{If this is set, the target will only be selected for
      drags within a single application.}
    @entry[:same-widget]{If this is set, the target will only be selected for
      drags within a single widget.}
    @entry[:other-app]{If this is set, the target will not be selected for
      drags within a single application.}
    @entry[:other-widget]{If this is set, the target will not be selected for
      drags within a single widget.}
  @end{table}
  @see-class{gtk-target-entry}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTargetEntry
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-target-entry "GtkTargetEntry"
  (target :string :initform 0)
  (flags gtk-target-flags :initform 0)
  (info :uint :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry atdoc:*class-name-alias*)
      "Boxed CStruct"
      (documentation 'gtk-target-entry 'type)
 "@version{2021-3-21}
  @begin{short}
    A @sym{gtk-target-entry} structure represents a single type of data
    that can be supplied for by a widget for a selection or received during
    drag-and-drop.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gtk-target-entry \"GtkTargetEntry\"
  (target :string :initform 0)
  (flags gtk-target-flags :initform 0)
  (info :uint :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[target]{A string representation of the target type.}
    @entry[flags]{The @symbol{gtk-target-flags} flags for DND.}
    @entry[info]{An application-assigned integer ID which will get passed
      as a parameter to e.g. the \"selection-get\" signal. It allows the
      application to identify the target type without extensive string
      compares.}
  @end{table}
  @see-slot{gtk-target-entry-target}
  @see-slot{gtk-target-entry-flags}
  @see-slot{gtk-target-entry-info}
  @see-symbol{gtk-target-flags}")

(export 'gtk-target-entry)

;;; ----------------------------------------------------------------------------
;;; Constructors for GtkTargetEntry
;;; ----------------------------------------------------------------------------

;; not exported

#+cl-cffi-gtk-documentation
(setf (documentation 'make-gtk-target-entry 'function)
 "@version{2013-8-27}
  Returns an object of type @class{gtk-target-entry}.
  @see-class{gtk-target-entry}")

#+cl-cffi-gtk-documentation
(setf (documentation 'copy-gtk-target-entry 'function)
 "@version{2013-8-27}
  Returns an object of type @class{gtk-target-entry}.
  @see-class{gtk-target-entry}")

;;; ----------------------------------------------------------------------------
;;; Accessors of GtkTargetEntry
;;; ----------------------------------------------------------------------------

;;; --- gtk-target-entry-target ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-target-entry-target 'function)
 "@version{2021-3-21}
  @syntax[]{(gtk-target-entry-target instance) => target}
  @syntax[]{(setf (gtk-target-entry-target instance) target)}
  @argument[instance]{a @class{gtk-target-entry} instance}
  @argument[target]{a string representation of the target type}
  @begin{short}
    Accessor for the @code{target} slot of the @class{gtk-target-entry}
    structure.
  @end{short}
  @see-class{gtk-target-entry}")

(export 'gtk-target-entry-target)

;;; --- gtk-target-entry-flags -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-flags atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-target-entry-flags 'function)
 "@version{2021-3-26}
  @syntax[]{(gtk-target-entry-flags instance) => flags}
  @syntax[]{(setf (gtk-target-entry-flags instance) flags)}
  @argument[instance]{a @class{gtk-target-entry} instance}
  @argument[flags]{set of @symbol{gtk-target-flags} flags}
  @begin{short}
    Accessor for the @code{flags} slot of the @class{gtk-target-entry}
    structure.
  @end{short}
  @see-class{gtk-target-entry}
  @see-symbol{gtk-target-flags}")

(export 'gtk-target-entry-flags)

;;; --- gtk-target-entry-info --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-entry-info atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-target-entry-info 'function)
 "@version{2021-3-21}
  @syntax[]{(gtk-target-entry-info instance) => info}
  @syntax[]{(setf (gtk-target-entry-info instance) info)}
  @argument[instance]{a @class{gtk-target-entry} instance}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @begin{short}
    Accessor for the @code{info} slot of the @class{gtk-target-entry} structure.
  @end{short}
  @see-class{gtk-target-entry}")

(export 'gtk-target-entry-info)

;;; ----------------------------------------------------------------------------
;;; GtkTargetList
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_target_list_get_type" g-size))

(define-g-boxed-opaque gtk-target-list "GtkTargetList"
  :alloc (%gtk-target-list-new (null-pointer) 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-target-list atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-target-list 'type)
 "@version{2021-3-26}
  @begin{short}
    A @sym{gtk-target-list} structure is used to represent the same information
    as a table of @class{gtk-target-entry} instances, but in an efficient form.
  @end{short}
  This structure should be treated as opaque.
  @begin{pre}
(define-g-boxed-opaque gtk-target-list \"GtkTargetList\"
  :alloc (%gtk-target-list-new (null-pointer) 0))
  @end{pre}
  @see-class{gtk-target-entry}")

(export 'gtk-target-list)

;;; ----------------------------------------------------------------------------
;;; struct GtkTargetPair
;;;
;;; struct GtkTargetPair {
;;;   GdkAtom   target;
;;;   guint     flags;
;;;   guint     info;
;;; };
;;;
;;; A GtkTargetPair is used to represent the same information as a table of
;;; GtkTargetEntry, but in an efficient form.
;;;
;;; Members
;;;
;;; GdkAtom target;
;;; GdkAtom representation of the target type
;;;
;;; guint flags;
;;; GtkTargetFlags for DND
;;;
;;; guint info;
;;; an application-assigned integer ID which will get passed as a parameter to
;;; e.g the “selection-get” signal. It allows the application to identify the
;;; target type without extensive string compares.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkSelectionData
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-selection-data "GtkSelectionData"
  (selection gdk-atom-as-string :initform nil)
  (target gdk-atom-as-string :initform nil)
  (type gdk-atom-as-string :initform nil)
  (format :int :initform 0)
  (data :pointer :initform (null-pointer))
  (length :int :initform 0)
  (display (g-object gdk-display) :initform nil))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data atdoc:*class-name-alias*)
      "CStruct"
      (documentation 'gtk-selection-data 'type)
 "@version{2021-3-21}
  @short{}
  @begin{pre}
(define-g-boxed-cstruct gtk-selection-data \"GtkSelectionData\"
  (selection gdk-atom-as-string :initform nil)
  (target gdk-atom-as-string :initform nil)
  (type gdk-atom-as-string :initform nil)
  (format :int :initform 0)
  (data :pointer :initform (null-pointer))
  (length :int :initform 0)
  (display (g-object gdk-display) :initform nil))
  @end{pre}
  @see-slot{gtk-selection-data-selection}
  @see-slot{gtk-selection-data-target}
  @see-slot{gtk-selection-data-type}
  @see-slot{gtk-selection-data-format}
  @see-slot{gtk-selection-data-data}
  @see-slot{gtk-selection-data-length}
  @see-slot{gtk-selection-data-display}")

(export 'gtk-selection-data)

;;; ----------------------------------------------------------------------------
;;; Constructors of GtkSelectionData
;;; ----------------------------------------------------------------------------

;;; --- gtk-selection-data-new -------------------------------------------------

(declaim (inline gtk-selection-data-new))

(defun gtk-selection-data-new (&key (selection "NONE")
                                    (target "NONE")
                                    (type "NONE")
                                    (format 0)
                                    (data (null-pointer))
                                    (length 0)
                                    (display nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[selection]{a @symbol{gdk-atom} as a string with the selection}
  @argument[target]{a @symbol{gdk-atom} as a string with the target}
  @argument[type]{a @symbol{gdk-atom} as a string with the data type of the
    selection}
  @argument[format]{an integer with the format of the selection}
  @argument[data]{a pointer with the raw data of the selection}
  @argument[length]{an integer with the length of the selection data}
  @argument[display]{a @class{gdk-display} object of the selection}
  @begin{short}
    Returns a @class{gtk-selection-data} instance.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gdk-display}
  @see-symbol{gdk-atom}
  @see-function{gtk-selection-data-copy}"
  (make-gtk-selection-data :selection selection
                           :target target
                           :type type
                           :format format
                           :data data
                           :length length
                           :display display))

(export 'gtk-selection-data-new)

;;; --- gtk-selection-data-copy ------------------------------------------------

(declaim (inline gtk-selection-data-copy))

(defun gtk-selection-data-copy (selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-23}
  @begin{short}
    Copies a @class{gtk-selection-data} instance.
  @end{short}
  @see-class{gtk-selection-data}
  @see-function{gtk-selection-data-new}"
  (copy-gtk-selection-data selection))

(export 'gtk-selection-data-copy)

;;; ----------------------------------------------------------------------------
;;; Accessors of GtkSelectionData
;;; ----------------------------------------------------------------------------

;;; --- gtk-selection-data-selection -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-selection atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-selection 'function)
 "@version{2021-3-23}
  @syntax[]{(gtk-selection-data-selection instance) => selection}
  @syntax[]{(setf (gtk-selection-data-selection instance) selection)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[selection]{a string with the selection}
  @begin{short}
    Accessor of the @code{selection} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  The function @sym{gtk-selection-data-selection} retrieves the selection
  of the selection data. The function @sym{(setf gtk-selection-data-selection)}
  sets the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-selection)

;;; --- gtk-selection-data-target ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-target atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-target 'function)
 "@version{2021-3-23}
  @syntax[]{(gtk-selection-data-target instance) => target}
  @syntax[]{(setf (gtk-selection-data-target instance) target)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[target]{a string with the target}
  @begin{short}
    Accessor of the @code{target} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  The target of the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-target)

;;; --- gtk-selection-data-type ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-type 'function)
 "@version{2021-3-23}
  @syntax[]{(gtk-selection-data-type instance) => type}
  @syntax[]{(setf (gtk-selection-data-type instance) type)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[type]{a string with the type of the selection}
  @begin{short}
    Accessor of the @code{type} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  Retrieves the data type of the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-type)

;;; --- gtk-selection-data-format ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-format atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-format 'function)
 "@version{2021-3-23}
  @syntax[]{(gtk-selection-data-format instance) => format}
  @syntax[]{(setf (gtk-selection-data-format instance) format)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[format]{an integer with the format}
  @begin{short}
    Accessor of the @code{format} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  Retrieves the format of the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-format)

;;; --- gtk-selection-data-data ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-data atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-data 'function)
 "@version{2021-3-26}
  @syntax[]{(gtk-selection-data-data instance) => data}
  @syntax[]{(setf (gtk-selection-data-data instance) data)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[data]{a pointer to the raw data}
  @begin{short}
    Accessor of the @code{data} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  Retrieves the raw data of the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-data)

;;; --- gtk-selection-data-length ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-length atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-length 'function)
 "@version{2021-3-26}
  @syntax[]{(gtk-selection-data-length instance) => length}
  @syntax[]{(setf (gtk-selection-data-length instance) length)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[length]{an integer with the length of the data}
  @begin{short}
    Accessor of the @code{length} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  The length of the data of the selection.
  @see-class{gtk-selection-data}")

(export 'gtk-selection-data-length)

;;; --- gtk-selection-data-display ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-selection-data-display atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-selection-data-display 'function)
 "@version{2021-3-26}
  @syntax[]{(gtk-selection-data-display instance) => display}
  @syntax[]{(setf (gtk-selection-data-display instance) display)}
  @argument[instance]{a @class{gtk-selection-data} instance}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Accessor of the @code{display} slot of the @class{gtk-selection-data}
    structure.
  @end{short}

  The display of the selection.
  @see-class{gtk-selection-data}
  @see-class{gdk-display}")

(export 'gtk-selection-data-display)

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-target-entry-new))

(defun gtk-target-entry-new (&key target flags info)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[target]{a string identifier for target}
  @argument[flags]{set of @symbol{gtk-target-flags} flags}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @begin{return}
    A new @class{gtk-target-entry} instance.
  @end{return}
  @begin{short}
    Makes a new @class{gtk-target-entry} instance.
  @end{short}
  @see-class{gtk-target-entry}
  @see-symbol{gtk-target-flags}"
  (make-gtk-target-entry :target target :flags flags :info info))

(export 'gtk-target-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-target-entry-copy))

(defun gtk-target-entry-copy (target)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[target]{a @class{gtk-target-entry} instance}
  @begin{return}
    A copy of @arg{target}.
  @end{return}
  @begin{short}
    Makes a copy of a @class{gtk-target-entry} instance and its data.
  @end{short}
  @see-class{gtk-target-entry}"
  (copy-gtk-target-entry target))

(export 'gtk-target-entry-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_target_entry_free ()
;;; ----------------------------------------------------------------------------

;; not implemented

#+nil
(defcfun ("gtk_target_entry_free" gtk-target-entry-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-19}
  @argument[data]{a @class{gtk-target-entry} structure}
  Frees a @class{gtk-target-entry} structure returned from the functions
  @fun{gtk-target-entry-new} or @fun{gtk-target-entry-copy}.
  @see-class{gtk-target-entry}
  @see-function{gtk-target-entry-new}
  @see-function{gtk-target-entry-copy}"
  (data (g-boxed-foreign gtk-target-entry)))

#+nil
(export 'gtk-target-entry-free)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_new" %gtk-target-list-new)
    (g-boxed-foreign gtk-target-list)
  (targets :pointer)
  (n-targets :uint))

(defun gtk-target-list-new (&optional (targets nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[targets]{a list of @class{gtk-target-entry} instances}
  @return{The new @class{gtk-target-list} instance.}
  @begin{short}
    Creates a new @class{gtk-target-list} from a list of
    @class{gtk-target-entry} instances
  @end{short}
  @see-class{gtk-target-entry}
  @see-class{gtk-target-list}"
  (if targets
      (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
        (%gtk-target-list-new targets-ptr n-targets))
      (%gtk-target-list-new (null-pointer) 0)))

(export 'gtk-target-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_ref ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gtk_target_list_ref" gtk-target-list-ref)
    (g-boxed-foreign gtk-target-list)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[target-list]{a @class{gtk-target-list}}
  @return{The passed in @class{gtk-target-list}.}
  Increases the reference count of a @class{gtk-target-list} by one.
  @see-class{gtk-target-list}"
  (target-list (g-boxed-foreign gtk-target-list)))

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_unref ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gtk_target_list_unref" gtk-target-list-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-21}
  @argument[list]{a @class{gtk-target-list}}
  @begin{short}
    Decreases the reference count of a @class{gtk-target-list} by one.
  @end{short}
  If the resulting reference count is zero, frees the list.
  @see-class{gtk-target-list-unref}"
  (target-list (g-boxed-foreign gtk-target-list)))

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add" gtk-target-list-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[target]{a string for the interned atom representing the target}
  @argument[flags]{the @symbol{gtk-target-flags} flags for this target}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @begin{short}
    Appends another target to a @class{gtk-target-list} instance.
  @end{short}
  @see-class{gtk-target-list}
  @see-symbol{gtk-target-flags}"
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom-as-string)
  (flags gtk-target-flags)
  (info :uint))

(export 'gtk-target-list-add)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_table ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_table" %gtk-target-list-add-table) :void
  (list (g-boxed-foreign gtk-target-list))
  (targets :pointer)
  (n-targets :uint))

(defun gtk-target-list-add-table (list targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[targets]{a list of @class{gtk-target-entry} instances}
  @begin{short}
    Prepends a list of @class{gtk-target-entry} instances to a target list.
  @end{short}
  @see-class{gtk-target-list}
  @see-class{gtk-target-entry}"
  (when targets
    (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
      (%gtk-target-list-add-table list targets-ptr n-targets))))

(export 'gtk-target-list-add-table)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_text_targets" gtk-target-list-add-text-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @begin{short}
    Appends the text targets supported by a selection to the target list.
  @end{short}
  All targets are added with the same info.
  @see-class{gtk-target-list}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint))

(export 'gtk-target-list-add-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_image_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_image_targets" gtk-target-list-add-image-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @argument[writable]{a boolean whether to add only targets for which GTK
    knows how to convert a pixbuf into the format}
  @begin{short}
    Appends the image targets supported by a selection to the target list.
  @end{short}
  All targets are added with the same info.
  @see-class{gtk-target-list}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint)
  (writeable :boolean))

(export 'gtk-target-list-add-image-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_uri_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_uri_targets" gtk-target-list-add-uri-targets)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @begin{short}
    Appends the URI targets supported by a selection to the target list.
  @end{short}
  All targets are added with the same info.
  @see-class{gtk-target-list}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint))

(export 'gtk-target-list-add-uri-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_add_rich_text_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_add_rich_text_targets"
           gtk-target-list-add-rich-text-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[info]{an unsigned integer ID that will be passed back to the
    application}
  @argument[deserializable]{if @em{true}, then deserializable rich text formats
    will be added, serializable formats otherwise}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{short}
    Appends the rich text targets registered with the functions
    @fun{gtk-text-buffer-register-serialize-format} or
    @fun{gtk-text-buffer-register-deserialize-format} to the target list.
  @end{short}
  All targets are added with the same info.
  @see-class{gtk-target-list}
  @see-class{gtk-text-buffer}
  @see-function{gtk-text-buffer-register-serialize-format}
  @see-function{gtk-text-buffer-register-deserialize-format}"
  (list (g-boxed-foreign gtk-target-list))
  (info :uint)
  (deserializable :boolean)
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-target-list-add-rich-text-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_remove" gtk-target-list-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-21}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[target]{a string with for the interned atom representing the target}
  @begin{short}
    Removes a target from a target list.
  @end{short}
  @see-class{gtk-target-list}"
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom-as-string))

(export 'gtk-target-list-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_target_list_find ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_list_find" %gtk-target-list-find) :boolean
  (list (g-boxed-foreign gtk-target-list))
  (target gdk-atom-as-string)
  (info :pointer))

(defun gtk-target-list-find (list target)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[list]{a @class{gtk-target-list} instance}
  @argument[target]{a string with for the interned atom representing the target
    to search for}
  @begin{return}
    Application info as an unsigned integer for target, or @code{nil}.
  @end{return}
  @begin{short}
    Looks up a given target in a @class{gtk-target-list} instance.
  @end{short}
  @see-class{gtk-target-list}"
  (with-foreign-object (info :uint)
    (when (%gtk-target-list-find list target info)
      (mem-ref info :uint))))

(export 'gtk-target-list-find)

;;; ----------------------------------------------------------------------------
;;; gtk_target_table_free ()
;;;
;;; void gtk_target_table_free (GtkTargetEntry *targets, gint n_targets);
;;;
;;; This function frees a target table as returned by
;;; gtk_target_table_new_from_list()
;;;
;;; targets :
;;;     a GtkTargetEntry array
;;;
;;; n_targets :
;;;     the number of entries in the array
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_target_table_new_from_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_target_table_new_from_list" %gtk-target-table-new-from-list)
    :pointer
  (list (g-boxed-foreign gtk-target-list))
  (n-targets (:pointer :int)))

(defun gtk-target-table-new-from-list (list)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[list]{a @class{gtk-target-list} instance}
  @return{A list of @class{gtk-target-entry} instances.}
  @begin{short}
    This function creates a @class{gtk-target-entry} list that contains the
    same targets as the passed list.
  @end{short}
  @see-class{gtk-target-list}
  @see-class{gtk-target-entry}"
  (with-foreign-object (n-targets :int)
    (let* ((targets (%gtk-target-table-new-from-list list n-targets))
           (n-targets (mem-ref n-targets :int))
           (type-size (foreign-type-size '(:struct gtk-target-entry-cstruct))))
      (prog1
        (iter (for i from 0 below n-targets)
              (for target =
                   (convert-from-foreign (inc-pointer targets (* i type-size ))
                                         '(g-boxed-foreign gtk-target-entry)))
              (collect target))
        (g-free targets)))))

(export 'gtk-target-table-new-from-list)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_owner_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_owner_set" gtk-selection-owner-set) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[widget]{a @class{gtk-widget} object, or @code{nil}}
  @argument[selection]{an interned @symbol{gdk-atom} as a stting representing
    the selection to claim}
  @argument[time]{an unsigned integer with the timestamp with which to claim
    the selection}
  @return{@em{True} if the operation succeeded.}
  @begin{short}
    Claims ownership of a given selection for a particular widget, or, if
    @arg{widget} is @code{nil}, release ownership of the selection.
  @end{short}
  @see-class{gtk-widget}
  @see-symbol{gdk-atom}
  @see-function{gtk-selection-owner-set-for-display}"
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (time :uint))

(export 'gtk-selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_owner_set_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_owner_set_for_display"
           gtk-selection-owner-set-for-display) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[display]{the @class{gdk-display} object where the selection is set}
  @argument[widget]{new selection owner, a @class{gtk-widget} object,
    or @code{nil}}
  @argument[selection]{an interned @symbol{gdk-atom} as a string representing
    the selection to claim}
  @argument[time]{an unsigned integer with the timestamp with which to claim
    the selection}
  @return{@em{True} if the operation succeeded.}
  @begin{short}
    Claim ownership of a given selection for a particular widget, or, if
    @arg{widget} is @code{nil}, release ownership of the selection.
  @end{short}
  @see-class{gdk-display}
  @see-class{gtk-widget}
  @see-symbol{gdk-atom}
  @see-function{gtk-selection-owner-set}"
  (display (g-object gdk-display))
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (time :uint))

(export 'gtk-selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_add_target ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_add_target" gtk-selection-add-target) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[selection]{a @symbol{gdk-atom} as a string with the selection}
  @argument[target]{a @symbol{gdk-atom} as a string with the target to add}
  @argument[info]{a unsigned integer which will be passed back to the
    application}
  @begin{short}
    Appends a specified @arg{target} to the list of supported targets for a
    given @arg{widget} and @arg{selection}.
  @end{short}
  @see-class{gtk-widget}
  @see-symbol{gdk-atom}
  @see-function{gtk-selection-add-targets}"
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (info :uint))

(export 'gtk-selection-add-target)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_add_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_add_targets" %gtk-selection-add-targets) :void
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (targets :pointer)
  (n-targets :uint))

(defun gtk-selection-add-targets (widget selection targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[selection]{a @symbol{gdk-atom} as a string with the selection}
  @argument[targets]{a list of @class{gtk-target-entry} instances to add}
  @begin{short}
    Prepends a table of targets to the list of supported targets for a given
    widget and selection.
  @end{short}
  @see-class{gtk-widget}
  @see-class{gtk-target-entry}
  @see-symbol{gdk-atom}"
  (when targets
    (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
      (%gtk-selection-add-targets widget selection targets-ptr n-targets))))

(export 'gtk-selection-add-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_clear_targets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_clear_targets" gtk-selection-clear-targets) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-23}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[selection]{a @symbol{gdk-atom} as a string representing a selection}
  @begin{short}
    Remove all targets registered for the given @arg{selection} for the widget.
  @end{short}
  @see-class{gtk-widget}
  @see-symbol{gdk-atom}"
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string))

(export 'gtk-selection-clear-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_convert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_convert" gtk-selection-convert) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[widget]{the @class{gtk-widget} object which acts as requestor}
  @argument[selection]{a @symbol{gdk-atom} as a string which selection to get}
  @argument[target]{a @symbol{gdk-atom} as a string with the form of
    information desired}
  @argument[time]{an unsigned integer with the time of request, usually of
    triggering event, in emergency, you could use the @var{+gdk-current-time+}
    value}
  @begin{return}
    @em{True} if requested succeeded, @em{false} if we could not process
    request, e.g., there was already a request in process for this widget.
  @end{return}
  @begin{short}
    Requests the contents of a selection.
  @end{short}
  When received, a \"selection-received\" signal will be generated.
  @see-class{gtk-selection-data}
  @see-class{gtk-widget}
  @see-symbol{gdk-atom}
  @see-variable{+gdk-current-time+}"
  (widget (g-object gtk-widget))
  (selection gdk-atom-as-string)
  (target gdk-atom-as-string)
  (time :uint))

(export 'gtk-selection-convert)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_set" gtk-selection-data-set) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[type]{a @symbol{gdk-atom} as a string with the type of selection
    data}
  @argument[format]{an integer with the format (number of bits in a unit)}
  @argument[data]{a pointer to the data (will be copied)}
  @argument[length]{an integer with the length of the data}
  @begin{short}
    Stores new data into a @class{gtk-selection-data} instance.
  @end{short}
  Should only be called from a selection handler callback. Zero-terminates
  the stored data.
  @see-class{gtk-selection-data}
  @see-symbol{gdk-atom}"
  (selection (g-boxed-foreign gtk-selection-data))
  (type gdk-atom-as-string)
  (format :int)
  (data :pointer)
  (length :int))

(export 'gtk-selection-data-set)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_text ()
;;; gtk_selection_data_set_text () -> gtk-selection-data-text
;;; ----------------------------------------------------------------------------

(defun (setf gtk-selection-data-text) (text selection)
  (when (foreign-funcall "gtk_selection_data_set_text"
                         (g-boxed-foreign gtk-selection-data) selection
                         :string text
                         :int -1
                         :boolean)
    text))

(defcfun ("gtk_selection_data_get_text" gtk-selection-data-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @syntax[]{(gtk-selection-data-text selection) => text}
  @syntax[]{(setf (gtk-selection-data-text selection) text)}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[text]{a UTF-8 string}
  @begin{short}
    The function @sym{gtk-selection-data-text} gets the contents of the
    selection data as a UTF-8 string.
  @end{short}
  The function @sym{(setf gtk-selection-data-text)} sets the contents of the
  selection. The string is converted to the form determined by the function
  @fun{gtk-selection-data-target}.
  @see-class{gtk-selection-data}
  @see-function{gtk-selection-data-target}"
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_pixbuf ()
;;; gtk_selection_data_set_pixbuf () -> gtk-selection-data-pixbuf
;;; ----------------------------------------------------------------------------

(defun (setf gtk-selection-data-pixbuf) (pixbuf selection)
  (when (foreign-funcall "gtk_selection_data_set_pixbuf"
                         (g-boxed-foreign gtk-selection-data) selection
                         (g-object gdk-pixbuf) pixbuf
                         :boolean)
    pixbuf))

(defcfun ("gtk_selection_data_get_pixbuf" gtk-selection-data-pixbuf)
    (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @syntax[]{(gtk-selection-data-pixbuf selection) => pixbuf}
  @syntax[]{(setf (gtk-selection-data-pixbuf selection) pixbuf)}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @begin{short}
    The function @sym{gtk-selection-data-pixbuf} gets the contents of the
    selection data as a @class{gdk-pixbuf} object.
  @end{short}
  The function @sym{(setf gtk-selection-data-pixbuf)} sets the contents of the
  selection.

  The pixbuf is converted to the form determined by the target of the selection.
  @see-class{gtk-selection-data}
  @see-class{gdk-pixbuf}"
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_uris ()
;;; gtk_selection_data_set_uris () -> gtk-selection-data-uris
;;; ----------------------------------------------------------------------------

(defun (setf gtk-selection-data-uris) (uris selection)
  (when (foreign-funcall "gtk_selection_data_set_uris"
                         (g-boxed-foreign gtk-selection-data) selection
                         g-strv uris
                         :boolean)
    uris))

(defcfun ("gtk_selection_data_get_uris" gtk-selection-data-uris) g-strv
 #+cl-cffi-gtk-documentation
 "@version{2023-3-26}
  @syntax[]{(gtk-selection-data-uris selection) => uris}
  @syntax[]{(setf (gtk-selection-data-uris selection) uris)}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[uris]{a list of strings holding URIs}
  @begin{short}
    The @sym{gtk-selection-data-uris} gets the contents of the selection data
    as a list of URIs.
  @end{short}
  The function @sym{(setf gtk-selection-data-uris)} sets the contents of the
  selection.

  The string is converted to the form determined by the function
  @fun{gtk-selection-data-target}.
  @see-class{gtk-selection-data}
  @see-function{gtk-selection-data-target}"
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_targets () -> gtk-selection-data-targets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_get_targets" %gtk-selection-data-targets) :boolean
  (selection (g-boxed-foreign gtk-selection-data))
  (targets (:pointer gdk-atom-as-string))
  (n-atoms (:pointer :int)))

(defun gtk-selection-data-targets (selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @return{A list of @symbol{gdk-atom} targets as strings.}
  @begin{short}
    Gets the contents of the selection as a list of targets.
  @end{short}
  This can be used to interpret the results of getting the standard \"TARGETS\"
  target that is always supplied for any selection.
  @see-class{gtk-selection-data}
  @see-symbol{gdk-atom}"
  (with-foreign-objects ((targets-ptr :pointer) (n-atoms :int))
    (when (%gtk-selection-data-targets selection targets-ptr n-atoms)
      (let ((result nil))
        (loop for i from 0 below (mem-ref n-atoms :int)
              do (push (mem-aref targets-ptr 'gdk-atom-as-string i) result))
        (nreverse result)))))

(export 'gtk-selection-data-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_targets_include_image"
           gtk-selection-data-targets-include-image) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[writable]{a boolean whether to accept only targets for which GTK+
    knows how to convert a pixbuf into the format}
  @begin{return}
    @em{True} if @arg{selection} holds a list of targets, and a suitable target
    for images is included, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Given a @class{gtk-selection-data} instance holding a list of targets,
    determines if any of the targets can be used to provide a @class{gdk-pixbuf}
    object.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gdk-pixbuf}"
  (selection (g-boxed-foreign gtk-selection-data))
  (writeable :boolean))

(export 'gtk-selection-data-targets-include-image)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_targets_include_text"
           gtk-selection-data-targets-include-text) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-23}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @begin{return}
    @em{True} if @arg{selection} holds a list of targets, and a suitable
    target for text is included, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Given a @class{gtk-selection-data} object holding a list of targets,
    determines if any of the targets can be used to provide text.
  @end{short}
  @see-class{gtk-selection-data}"
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-targets-include-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_targets_include_uri"
           gtk-selection-data-targets-include-uri) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @begin{return}
    @em{True} if @arg{selection} holds a list of targets, and a suitable
    target for URI lists is included, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Given a @class{gtk-selection-data} instance holding a list of targets,
    determines if any of the targets can be used to provide a list
    or URIs.
  @end{short}
  @see-class{gtk-selection-data}"
  (selection (g-boxed-foreign gtk-selection-data)))

(export 'gtk-selection-data-targets-include-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_targets_include_rich_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_targets_include_rich_text"
           gtk-selection-data-targets-include-rich-text) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @begin{return}
    @em{True} if @arg{selection} holds a list of targets, and a suitable
    target for rich text is included, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Given a @class{gtk-selection-data} instance holding a list of targets,
    determines if any of the targets can be used to provide rich text.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gtk-text-buffer}"
  (selection (g-boxed-foreign gtk-selection-data))
  (buffer (g-object gtk-text-buffer)))

(export 'gtk-selection-data-targets-include-rich-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_get_data_with_length ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_data_get_data_with_length"
          %gtk-selection-data-data-with-length) :pointer
  (selection (g-boxed-foreign gtk-selection-data))
  (length (:pointer :int)))

(defun gtk-selection-data-data-with-length (selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-23}
  @argument[selection]{a @class{gtk-selection-data} instance}
  @begin{return}
    @code{length} -- an integer with the length of the data segment @br{}
    @code{data} -- a pointer to the raw data of the selection, see the function
    @fun{gtk-selection-data-data}
  @end{return}
  @begin{short}
    Retrieves the raw data of the selection along with its length.
  @end{short}
  @see-class{gtk-selection-data}
  @see-function{gtk-selection-data-data}"
  (with-foreign-object (length-ptr :int)
    (let ((data (%gtk-selection-data-data-with-length selection
                                                      length-ptr)))
      (let ((length (mem-ref length-ptr :int)))
        (if (> length 0)
            (values length data)
            (values length nil))))))

(export 'gtk-selection-data-data-with-length)

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_image ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_targets_include_image" %gtk-targets-include-image) :boolean
  (targets :pointer)
  (n-targets :int)
  (writable :boolean))

(defun gtk-targets-include-image (targets writable)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[targets]{a list of @symbol{gdk-atom} as strings}
  @argument[writable]{a boolean whether to accept only targets for which GTK+
    knows how to convert a pixbuf into the format}
  @return{@em{True} if @arg{targets} include a suitable target for images,
    otherwise @em{false}.}
  @begin{short}
    Determines if any of the targets in @arg{targets} can be used to provide a
    @class{gdk-pixbuf} object.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gdk-atom}
  @see-class{gdk-pixbuf}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ar :pointer n-targets)
      (loop for i from 0 below n-targets
            for target in targets
            do (setf (mem-aref targets-ar 'gdk-atom-as-string i) target))
      (%gtk-targets-include-image targets-ar n-targets writable))))

(export 'gtk-targets-include-image)

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_targets_include_text" %gtk-targets-include-text) :boolean
  (targets :pointer)
  (n-targets :int))

(defun gtk-targets-include-text (targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[targets]{a list of @symbol{gdk-atom} as strings}
  @return{@em{True} if @arg{targets} include a suitable target for text,
    otherwise @em{false}.}
  @begin{short}
    Determines if any of the targets in @arg{targets} can be used to provide
    text.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gkd-atom}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ar :pointer n-targets)
      (loop for i from 0 below n-targets
            for target in targets
            do (setf (mem-aref targets-ar 'gdk-atom-as-string i) target))
      (%gtk-targets-include-text targets-ar n-targets))))

(export 'gtk-targets-include-text)

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_targets_include_uri" %gtk-targets-include-uri) :boolean
  (targets :pointer)
  (n-targets :int))

(defun gtk-targets-include-uri (targets)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[targets]{a list of @symbol{gdk-atom} as strings}
  @return{@em{True} if @arg{targets} include a suitable target for URI lists,
    otherwise @em{false}.}
  @begin{short}
    Determines if any of the targets in @arg{targets} can be used to provide
    an URI list.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gkd-atom}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ar :pointer n-targets)
      (loop for i from 0 below n-targets
            for target in targets
            do (setf (mem-aref targets-ar 'gdk-atom-as-string i) target))
      (%gtk-targets-include-uri targets-ar n-targets))))

(export 'gtk-targets-include-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_targets_include_rich_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_targets_include_rich_text" %gtk-targets-include-rich-text)
    :boolean
  (targets :pointer)
  (n-targets :int)
  (buffer (g-object gtk-text-buffer)))

(defun gtk-targets-include-rich-text (targets buffer)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[targets]{a list of @symbol{gdk-atom} as strings}
  @argument[buffer]{a @class{gtk-text-buffer} object}
  @return{@em{True} if @arg{targets} include a suitable target for rich text,
    otherwise @em{false}.}
  @begin{short}
    Determines if any of the targets in @arg{targets} can be used to provide
    rich text.
  @end{short}
  @see-class{gtk-selection-data}
  @see-class{gkd-atom}
  @see-class{gtk-text-buffer}"
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ar :pointer n-targets)
      (loop for i from 0 below n-targets
            for target in targets
            do (setf (mem-aref targets-ar 'gdk-atom-as-string i) target))
      (%gtk-targets-include-rich-text targets-ar n-targets buffer))))

(export 'gtk-targets-include-rich-text)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_selection_remove_all" gtk-selection-remove-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-26}
  @argument[widget]{a @class{gtk-widget} object}
  @begin{short}
    Removes all handlers and unsets ownership of all selections for a widget.
  @end{short}
  Called when @arg{widget} is being destroyed. This function will not generally
  be called by applications.
  @see-class{gtk-selection-data}
  @see-class{gtk-widget}"
  (widget (g-object gtk-widget)))

(export 'gtk-selection-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_selection_data_free ()
;;;
;;; void gtk_selection_data_free (GtkSelectionData *data);
;;;
;;; Frees a GtkSelectionData structure returned from gtk_selection_data_copy().
;;;
;;; data :
;;;     a pointer to a GtkSelectionData structure.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.selections.lisp ----------------------------------------
