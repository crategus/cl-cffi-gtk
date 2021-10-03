;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.68 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
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
;;; This file contains several type definitions and functions, which are
;;; needed for the implemenation of the GTK library, but are not fully
;;; implemented.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Basic Types
;;;
;;;     Standard GLib types, defined for ease-of-use and portability
;;;
;;; Only the following types are implemented:
;;;
;;;     gsize
;;;     gssize
;;;     goffset
;;; ----------------------------------------------------------------------------
;;;
;;; Memory Allocation
;;;
;;;  The following functions for general memory handling are implemented:
;;;
;;;     g_malloc
;;;     g_free
;;; ----------------------------------------------------------------------------
;;;
;;; String Utility Functions
;;;
;;;     Various string-related functions
;;;
;;; Implemented is:
;;;
;;;     GString
;;;     GStrv
;;;
;;;     g_strdup                                           not exported
;;; ----------------------------------------------------------------------------
;;;
;;; Doubly-Linked Lists
;;;
;;;     Linked lists containing integer values or pointers to data, with the
;;;     ability to iterate over the list in both directions
;;;
;;; Implemented is:
;;;
;;;     GList
;;;
;;;     g_list_free                                        not exported
;;;     g_list_next                                        not exported
;;;     g_list_append                                      not exported
;;; ----------------------------------------------------------------------------
;;;
;;; Singly-Linked Lists
;;;
;;;     Linked lists containing integer values or pointers to data, limited to
;;;     iterating over the list in one direction
;;;
;;; Implemented is:
;;;
;;;     GSList
;;;
;;;     g_slist_alloc                                      not exported
;;;     g_slist_free                                       not exported
;;;     g_slist_next                                       not exported
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; gsize
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond ((cffi-features:cffi-feature-p :x86-64) (defctype g-size :uint64))
        ((cffi-features:cffi-feature-p :x86)    (defctype g-size :ulong))
        ((cffi-features:cffi-feature-p :ppc32)  (defctype g-size :uint32))
        ((cffi-features:cffi-feature-p :ppc64)  (defctype g-size :uint64))
        (t
         (error "Can not define 'g-size', unknown CPU architecture ~
                (known are x86 and x86-64)"))))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-size 'type)
 "@version{2021-4-9}
  @begin{short}
    An unsigned integer type of the result of the sizeof operator,
    corresponding to the @code{size_t} type defined in C99.
  @end{short}
  This type is wide enough to hold the numeric value of a pointer, so it is
  usually 32 bit wide on a 32 bit platform and 64 bit wide on a 64 bit platform.
  Values of this type can range from 0 to @code{G_MAXSIZE}.
  @see-type{g-ssize}
  @see-type{g-offset}")

(export 'g-size)

;;; ----------------------------------------------------------------------------
;;; gssize
;;; ----------------------------------------------------------------------------

(defctype g-ssize :long)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-ssize 'type)
 "@version{2021-4-9}
  @begin{short}
    A signed variant of the @type{g-size} type, corresponding to the
    @code{ssize_t} type defined on most platforms.
  @end{short}
  Values of this type can range from @code{G_MINSSIZE} to @code{G_MAXSSIZE}.
  @see-type{g-size}
  @see-type{g-offset}")

(export 'g-ssize)

;;; ----------------------------------------------------------------------------
;;; goffset
;;; ----------------------------------------------------------------------------

(defctype g-offset :uint64)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-offset 'type)
 "@version{2021-4-9}
  @begin{short}
    A signed integer type that is used for file offsets, corresponding to the
    C99 @code{off64_t} type.
  @end{short}
  Values of this type can range from @code{G_MINOFFSET} to @code{G_MAXOFFSET}.
  @see-type{g-size}
  @see-type{g-ssize}")

(export 'g-offset)

;;; ----------------------------------------------------------------------------
;;; g_malloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_malloc" g-malloc) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2021-9-28}
  @argument[n-bytes]{a @type{g-size} type with the number of bytes to allocate}
  @return{A foreign pointer to the allocated memory.}
  @begin{short}
    Allocates @arg{n-bytes} bytes of memory.
  @end{short}
  If @arg{n-bytes} is 0 the @sym{g-malloc} function returns a foreign
  @code{null}-pointer.
  @see-type{g-size}
  @see-function{g-free}"
  (n-bytes g-size))

(export 'g-malloc)

;;; ----------------------------------------------------------------------------
;;; g_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_free" g-free) :void
 "@version{2021-9-28}
  @argument[mem]{a foreign pointer to the memory to free}
  @begin{short}
    Frees the memory pointed to by the @arg{mem} foreign pointer.
  @end{short}
  If the @arg{mem} argument is a @code{null}-pointer the @sym{g-free} function
  simply returns.
  @see{g-malloc}"
  (mem :pointer))

(export 'g-free)

;;; ----------------------------------------------------------------------------
;;; GString
;;; ----------------------------------------------------------------------------

;; A type that it almost like :string but uses g_malloc and g_free

(define-foreign-type g-string-type ()
  ((free-from-foreign :initarg :fff
                      :reader g-string-type-fff
                      :initform nil)
   (free-to-foreign :initarg :ftf
                    :reader g-string-type-ftf
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-string (&key (free-from-foreign nil) (free-to-foreign t))
  (make-instance 'g-string-type
                 :fff free-from-foreign
                 :ftf free-to-foreign))

(defmethod translate-to-foreign (value (type g-string-type))
  (%g-strdup value))

(defmethod translate-from-foreign (value (type g-string-type))
  (prog1
    (convert-from-foreign value '(:string :free-from-foreign nil))
    (when (g-string-type-fff type)
      (g-free value))))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-string 'type)
 "@version{2021-4-8}
  @begin{short}
    A type that is almost like the foreign CFFI type @code{:string} but uses
    the GLib functions @fun{g-malloc} and @fun{g-free} to allocate and free
    memory.
  @end{short}

  The @sym{g-string} type performs automatic conversion between Lisp and C
  strings. Note that, in the case of functions the converted C string will
  have dynamic extent, i.e. it will be automatically freed after the foreign
  function returns.

  In addition to Lisp strings, this type will accept foreign pointers and pass
  them unmodified.

  A method for free-translated-object is specialized for this type. So, for
  example, foreign strings allocated by this type and passed to a foreign
  function will be freed after the function returns.
  @see-type{g-strv}")

(export 'g-string)

;;; ----------------------------------------------------------------------------
;;; GStrv
;;; ----------------------------------------------------------------------------

;;; Another string type g-strv

(define-foreign-type g-strv-type ()
  ((free-from-foreign :initarg :free-from-foreign
                      :initform t
                      :reader g-strv-type-fff)
   (free-to-foreign :initarg :free-to-foreign
                    :initform t
                    :reader g-strv-type-ftf))
  (:actual-type :pointer))

(define-parse-method g-strv (&key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'g-strv-type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (value (type g-strv-type))
  (unless (null-pointer-p value)
    (prog1
      (iter (for i from 0)
            (for str-ptr = (mem-aref value :pointer i))
            (until (null-pointer-p str-ptr))
            (collect (convert-from-foreign str-ptr
                                           '(:string :free-from-foreign nil)))
            (when (g-strv-type-fff type)
              (g-free str-ptr)))
      (when (g-strv-type-fff type)
        (g-free value)))))

(defmethod translate-to-foreign (str-list (type g-strv-type))
  (let* ((n (length str-list))
         (result (g-malloc (* (1+ n) (foreign-type-size :pointer)))))
    (iter (for i from 0)
          (for str in str-list)
          (setf (mem-aref result :pointer i) (%g-strdup str)))
    (setf (mem-aref result :pointer n) (null-pointer))
    result))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-strv 'type)
 "@version{2021-4-9}
  @begin{short}
    This type represents and performs automatic conversion between a list of
    Lisp strings and an array of C strings of the @type{g-string} type.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(setq str (convert-to-foreign (list \"Hello\" \"World\") 'g-strv))
=> #.(SB-SYS:INT-SAP #X01541998)
(convert-from-foreign str 'g-strv)
=> (\"Hello\" \"World\")
    @end{pre}
  @end{dictionary}
  @see-type{g-string}")

(export 'g-strv)

;;; ----------------------------------------------------------------------------
;;; g_strdup ()                                            not exported
;;; ----------------------------------------------------------------------------

;; We do not export this function. It is internal for the implementation of
;; the types g-string and g-strv.

(defcfun ("g_strdup" %g-strdup) :pointer
 #+cl-cffi-gtk-documentation
 "@argument[str]{the string to duplicate}
  @return{a newly-allocated copy of @arg{str}}
  @short{Duplicates a string.}
  If @arg{str} is @code{NULL} it returns @code{NULL}. The returned string
  should be freed with @fun{g-free} when no longer needed."
  (str (:string :free-to-foreign t)))

;;; ----------------------------------------------------------------------------
;;; GList
;;; ----------------------------------------------------------------------------

(defcstruct %g-list
  (data :pointer)
  (next :pointer)
  (prev :pointer))

(define-foreign-type g-list-type ()
  ((type :reader g-list-type-type
         :initarg :type
         :initform :pointer)
   (free-from-foreign :reader g-list-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader g-list-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-list (type &key (free-from-foreign t)
                                       (free-to-foreign t))
  (make-instance 'g-list-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type g-list-type))
  (prog1
    (iter (for c initially pointer then (%g-list-next c))
          (until (null-pointer-p c))
          (collect (convert-from-foreign (foreign-slot-value c
                                                             '(:struct %g-list)
                                                             'data)
                                         (g-list-type-type type))))
    (when (g-list-type-free-from-foreign type)
      (%g-list-free pointer))))

;; TODO: This implemention only allows pointers as list elements, but not
;;       Lisp objects.

(defmethod translate-to-foreign (lst (type g-list-type))
  (let ((nlst (null-pointer)))
    (iter (for data in lst)
          (setq nlst (%g-list-append nlst data)))
    nlst))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-list 'type)
 "@version{2021-4-8}
  @begin{short}
    The @sym{g-list} type represents a C doubly-linked list with elements of
    the @code{GList} structure.
  @end{short}
  The @sym{g-list} type performs automatic conversion from a C list to a Lisp
  list. The conversion from a Lisp list to a C reprensentation is implemented
  only for foreign pointers, but not Lisp objects.
  @see-type{g-slist}")

(export 'g-list)

;;; ----------------------------------------------------------------------------
;;; g_list_free ()                                         not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_free" %g-list-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-24}
  @argument[lst]{a @code{GList}}
  @begin{short}
    Frees all of the memory used by a GList. The freed elements are returned to
    the slice allocator.
  @end{short}
  @begin[Note]{dictionary}
    If list elements contain dynamically-allocated memory, you should either
    use @code{g_list_free_full()} or free them manually first.
  @end{dictionary}"
  (lst (:pointer (:struct %g-list))))

;;; ----------------------------------------------------------------------------
;;; g_list_next ()                                         not exported
;;; ----------------------------------------------------------------------------

(defun %g-list-next (lst)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-24}
  @argument[list]{an element in a GList.}
  @return{the next element, or NULL if there are no more elements.}
  @short{A convenience macro to get the next element in a GList.}"
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '(:struct %g-list) 'next)))

;;; ----------------------------------------------------------------------------
;;; g_list_append ()
;;;
;;; GList * g_list_append (GList *list, gpointer data);
;;;
;;; Adds a new element on to the end of the list.
;;;
;;; Note
;;;
;;; The return value is the new start of the list, which may have changed, so
;;; make sure you store the new value.
;;;
;;; Note
;;;
;;; Note that g_list_append() has to traverse the entire list to find the end,
;;; which is inefficient when adding multiple elements. A common idiom to avoid
;;; the inefficiency is to prepend the elements and reverse the list when all
;;; elements have been added.
;;;
;;; /* Notice that these are initialized to the empty list. */
;;; GList *list = NULL, *number_list = NULL;
;;;
;;; /* This is a list of strings. */
;;; list = g_list_append (list, "first");
;;; list = g_list_append (list, "second");
;;;
;;; /* This is a list of integers. */
;;; number_list = g_list_append (number_list, GINT_TO_POINTER (27));
;;; number_list = g_list_append (number_list, GINT_TO_POINTER (14));
;;;
;;; list :
;;;     a pointer to a GList
;;;
;;; data :
;;;     the data for the new element
;;;
;;; Returns :
;;;     the new start of the GList
;;; ----------------------------------------------------------------------------

;;; GList * g_list_append (GList *list, gpointer data);

(defcfun ("g_list_append" %g-list-append) (:pointer (:struct %g-list))
  (list (:pointer (:struct %g-list)))
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GSList
;;; ----------------------------------------------------------------------------

(defcstruct %g-slist
  (data :pointer)
  (next :pointer))

(define-foreign-type g-slist-type ()
  ((type :reader g-slist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader g-slist-type-free-from-foreign
                      :initarg :free-from-foreign
                      :initform t)
   (free-to-foreign :reader g-slist-type-free-to-foreign
                    :initarg :free-to-foreign
                    :initform t))
  (:actual-type :pointer))

(define-parse-method g-slist (type &key (free-from-foreign t)
                                        (free-to-foreign t))
  (make-instance 'g-slist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defmethod translate-from-foreign (pointer (type g-slist-type))
  (prog1
    (iter (for c initially pointer then (%g-slist-next c))
          (until (null-pointer-p c))
          (collect (convert-from-foreign (foreign-slot-value c
                                                             '(:struct %g-slist)
                                                             'data)
                                         (g-slist-type-type type))))
    (when (g-slist-type-free-from-foreign type)
      (%g-slist-free pointer))))

(defmethod translate-to-foreign (lst (type g-slist-type))
  (let ((result (null-pointer))
        last)
    (iter (for item in lst)
          (for n = (%g-slist-alloc))
          (for ptr = (convert-to-foreign item (g-slist-type-type type)))
          (setf (foreign-slot-value n '(:struct %g-slist) 'data) ptr)
          (setf (foreign-slot-value n '(:struct %g-slist) 'next) (null-pointer))
          (when last
            (setf (foreign-slot-value last '(:struct %g-slist) 'next) n))
          (setf last n)
          (when (first-iteration-p)
            (setf result n)))
    result))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-slist 'type)
 "@version{2021-4-8}
  @begin{short}
    The @sym{g-slist} type represents a C singly-linked list with elements of
    @code{GSList} structure.
  @end{short}
  The type @sym{g-slist} performs automatic conversion from a C list to a Lisp
  list.
  @see-type{g-list}")

(export 'g-slist)

;;; ----------------------------------------------------------------------------
;;; g_slist_alloc ()                                       not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_alloc" %g-slist-alloc) (:pointer (:struct %g-slist))
 #+cl-cffi-gtk-documentation
 "@return{a pointer to the newly-allocated GSList element.}
  @short{Allocates space for one GSList element.}
  It is called by the g_slist_append(), g_slist_prepend(), g_slist_insert() and
  g_slist_insert_sorted() functions and so is rarely used on its own.")

;;; ----------------------------------------------------------------------------
;;; g_slist_free ()                                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_free" %g-slist-free) :void
 #+cl-cffi-gtk-documentation
 "@argument[lst]{a GSList}
  @short{Frees all of the memory used by a GSList.}
  The freed elements are returned to the slice allocator.
  @begin[Note]{dictionary}
    If list elements contain dynamically-allocated memory, you should either
    use g_slist_free_full() or free them manually first.
  @end{dictionary}"
  (lst (:pointer (:struct %g-slist))))

;;; ----------------------------------------------------------------------------
;;; g_slist_next ()                                        not exported
;;; ----------------------------------------------------------------------------

(defun %g-slist-next (lst)
 #+cl-cffi-gtk-documentation
 "@argument[slist]{an element in a GSList.}
  @return{the next element, or NULL if there are no more elements.}
  @short{A convenience macro to get the next element in a GSList.}"
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '(:struct %g-slist) 'next)))

;;; --- End of file glib.misc.lisp ---------------------------------------------
