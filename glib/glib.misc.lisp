;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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
;;;
;;; This file contains several type definitions and functions, which are
;;; needed for the implemenation of the GTK library, but are not fully
;;; implemented.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Basic Types
;;; 
;;; Standard GLib types, defined for ease-of-use and portability
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
;;; The following functions for the general memory-handling are implemented:
;;;
;;;     g_malloc
;;;     g_free
;;; ----------------------------------------------------------------------------
;;;
;;; Date and Time Functions
;;; 
;;; Calendrical calculations and miscellaneous time stuff
;;;
;;; Only the following struct is implemented:
;;;
;;;     GTimeVal
;;; ----------------------------------------------------------------------------
;;;
;;; String Utility Functions - Various string-related functions
;;;
;;; Implemented is:
;;; 
;;;     GString
;;;     GStrv
;;;
;;;     g_strdup        *not exported*
;;; ----------------------------------------------------------------------------
;;; 
;;; Doubly-Linked Lists
;;;
;;; Linked lists containing integer values or pointers to data, with the ability
;;; to iterate over the list in both directions
;;;
;;; Implemented is:
;;;
;;;     GList
;;;
;;;     g_list_free     *not exported*
;;;     g_list_next     *not exported*
;;; ----------------------------------------------------------------------------
;;;
;;; Singly-Linked Lists
;;; 
;;; Linked lists containing integer values or pointers to data, limited to
;;; iterating over the list in one direction
;;;
;;; Implemented is:
;;;
;;;     GSList
;;;
;;;     g_slist_alloc   *not exported*
;;;     g_slist_free    *not exported*
;;;     g_slist_next    *not exported*
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; gsize
;;; 
;;; typedef unsigned long gsize;
;;; 
;;; An unsigned integer type of the result of the sizeof operator,
;;; corresponding to the size_t type defined in C99. This type is wide enough
;;; to hold the numeric value of a pointer, so it is usually 32bit wide on a
;;; 32bit platform and 64bit wide on a 64bit platform. Values of this type can
;;; range from 0 to G_MAXSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond ((cffi-features:cffi-feature-p :x86-64) (defctype g-size :uint64))
        ((cffi-features:cffi-feature-p :x86)    (defctype g-size :ulong))
        ((cffi-features:cffi-feature-p :ppc32)  (defctype g-size :uint32))
        ((cffi-features:cffi-feature-p :ppc64)  (defctype g-size :uint64))
        (t
         (error "Can not define 'g-size', unknown CPU architecture ~
                (known are x86 and x86-64)"))))

(export 'g-size)

;;; ----------------------------------------------------------------------------
;;; gssize
;;; 
;;; typedef signed long gssize;
;;; 
;;; A signed variant of gsize, corresponding to the ssize_t defined on most
;;; platforms. Values of this type can range from G_MINSSIZE to G_MAXSSIZE.
;;; 
;;; To print or scan values of this type, use G_GSIZE_MODIFIER and/or
;;; G_GSSIZE_FORMAT.
;;; ----------------------------------------------------------------------------

(defctype g-ssize :long)

(export 'g-ssize)

;;; ----------------------------------------------------------------------------
;;; goffset
;;; 
;;; typedef gint64 goffset;
;;; 
;;; A signed integer type that is used for file offsets, corresponding to the
;;; C99 type off64_t. Values of this type can range from G_MINOFFSET to
;;; G_MAXOFFSET.
;;; 
;;; To print or scan values of this type, use G_GOFFSET_MODIFIER and/or
;;; G_GOFFSET_FORMAT.
;;;
;;; Since: 2.14
;;; ----------------------------------------------------------------------------

(defctype g-offset :uint64)

(export 'g-offset)

;;; ----------------------------------------------------------------------------
;;; g_malloc ()
;;; 
;;; gpointer g_malloc (gsize n_bytes)
;;; 
;;; Allocates n_bytes bytes of memory. If n_bytes is 0 it returns NULL.
;;; 
;;; n-bytes :
;;;     the number of bytes to allocate
;;; 
;;; Returns :
;;;     a pointer to the allocated memory
;;; ----------------------------------------------------------------------------

(defcfun ("g_malloc" g-malloc) :pointer
  (n-bytes g-size))

(export 'g-malloc)

;;; ----------------------------------------------------------------------------
;;; g_free ()
;;; 
;;; void g_free (gpointer mem)
;;; 
;;; Frees the memory pointed to by mem. If mem is NULL it simply returns.
;;; 
;;; mem :
;;;     the memory to free
;;; ----------------------------------------------------------------------------

(defcfun ("g_free" g-free) :void
  (mem :pointer))

(export 'g-free)

;;; ----------------------------------------------------------------------------
;;; GTimeVal
;;; 
;;; struct GTimeVal {
;;;   glong tv_sec;
;;;   glong tv_usec;
;;; };
;;; 
;;; Represents a precise time, with seconds and microseconds. Similar to the
;;; struct timeval returned by the gettimeofday() UNIX call.
;;; 
;;; GLib is attempting to unify around the use of 64bit integers to represent
;;; microsecond-precision time. As such, this type will be removed from a
;;; future version of GLib.
;;; 
;;; glong tv_sec;
;;;     seconds
;;; 
;;; glong tv_usec;
;;;     microseconds
;;; ----------------------------------------------------------------------------

(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))

(export 'g-time-val)

;;; ----------------------------------------------------------------------------
;;; g_get_current_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_current_time" %g-get-current-time) :void
  (result (:pointer g-time-val)))

(defun g-get-current-time ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @return{A @type{g-time-val} structure.}
  @begin{short}
    Equivalent to the UNIX @code{gettimeofday()} function, but portable.
  @end{short}

  You may find @fun{g-get-real-time} to be more convenient.
  @see-function{g-get-real-time}"
  (with-foreign-object (result 'g-time-val)
    (when (%g-get-current-time result)
      result)))

(export 'g-get-current-time)

;;; ----------------------------------------------------------------------------
;;; g_get_monotonic_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_monotonic_time" g-get-monotonic-time) :int64
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @return{The monotonic time, in microseconds.}
  @short{Queries the system monotonic time, if available.}

  On POSIX systems with @code{clock_gettime()} and @code{CLOCK_MONOTONIC} this
  call is a very shallow wrapper for that. Otherwise, we make a best effort that
  probably involves returning the wall clock time (with at least microsecond
  accuracy, subject to the limitations of the OS kernel).

  It's important to note that @code{POSIX CLOCK_MONOTONIC} does not count time
  spent while the machine is suspended.

  On Windows, \"limitations of the OS kernel\" is a rather substantial
  statement. Depending on the configuration of the system, the wall clock time
  is updated as infrequently as 64 times a second (which is approximately every
  16 ms). Also, on XP (but not on Vista or later) the monotonic clock is locally
  monotonic, but may differ in exact value between processes due to timer wrap
  handling.

  Since 2.28")

(export 'g-get-monotonic-time)

;;; ----------------------------------------------------------------------------
;;; g_get_real_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_real_time" g-get-real-time) :int64
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @return{The number of microseconds since January 1, 1970 UTC.}
  @short{Queries the system wall-clock time.}

  This call is functionally equivalent to @fun{g-get-current-time} except that
  the return value is often more convenient than dealing with a
  @type{g-time-val}.

  You should only use this call if you are actually interested in the real
  wall-clock time. @fun{g-get-monotonic-time} is probably more useful for
  measuring intervals.

  Since 2.28
  @see-function{g-get-monotonic-time}")

(export 'g-get-real-time)

;;; ----------------------------------------------------------------------------
;;; GString
;;; 
;;; struct GString {
;;;   gchar  *str;
;;;   gsize   len;    
;;;   gsize   allocated_len;
;;; };
;;; 
;;; The GString struct contains the public fields of a GString.
;;; 
;;; gchar *str;
;;;     points to the character data. It may move as text is added. The str
;;;     field is null-terminated and so can be used as an ordinary C string.
;;; 
;;; gsize len;
;;;     contains the length of the string, not including the terminating nul
;;;     byte.
;;; 
;;; gsize allocated_len;
;;;     the number of bytes that can be stored in the string before it needs to
;;;     be reallocated. May be larger than len.
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
  (g-strdup value))

(defmethod translate-from-foreign (value (type g-string-type))
  (prog1
    (convert-from-foreign value '(:string :free-from-foreign nil))
    (when (g-string-type-fff type)
      (g-free value))))

(export 'g-string)

;;; ----------------------------------------------------------------------------
;;; GStrv
;;; 
;;; typedef gchar** GStrv;
;;; 
;;; A C representable type name for G_TYPE_STRV.
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
          (setf (mem-aref result :pointer i) (g-strdup str)))
    (setf (mem-aref result :pointer n) (null-pointer))
    result))

(export 'g-strv)

;;; ----------------------------------------------------------------------------
;;; g_strdup ()
;;; 
;;; gchar * g_strdup (const gchar *str)
;;; 
;;; Duplicates a string. If str is NULL it returns NULL. The returned string
;;; should be freed with g_free() when no longer needed.
;;; 
;;; str :
;;;     the string to duplicate
;;; 
;;; Returns :
;;;     a newly-allocated copy of str
;;; ----------------------------------------------------------------------------

(defcfun ("g_strdup" g-strdup) :pointer
  (str (:string :free-to-foreign t)))

;;; ----------------------------------------------------------------------------
;;; GList
;;; 
;;; struct GList {
;;;   gpointer data;
;;;   GList   *next;
;;;   GList   *prev;
;;; };
;;; 
;;; The GList struct is used for each element in a doubly-linked list.
;;; 
;;; gpointer data;
;;;     holds the element's data, which can be a pointer to any kind of data,
;;;     or any integer value using the Type Conversion Macros.
;;; 
;;; GList *next;
;;;     contains the link to the next element in the list.
;;; 
;;; GList *prev;
;;;     contains the link to the previous element in the list.
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
    (iter (for c initially pointer then (g-list-next c))
          (until (null-pointer-p c))
          (collect (convert-from-foreign (foreign-slot-value c '%g-list 'data)
                                         (g-list-type-type type))))
    (when (g-list-type-free-from-foreign type)
      (g-list-free pointer))))

(export 'g-list)

;;; ----------------------------------------------------------------------------
;;; g_list_free ()
;;; 
;;; void g_list_free (GList *list)
;;; 
;;; Frees all of the memory used by a GList. The freed elements are returned to
;;; the slice allocator.
;;; 
;;; Note
;;; 
;;; If list elements contain dynamically-allocated memory, you should either
;;; use g_list_free_full() or free them manually first.
;;; 
;;; lst :
;;;     a GList
;;; ----------------------------------------------------------------------------

(defcfun ("g_list_free" g-list-free) :void
  (lst (:pointer %g-list)))

;;; ----------------------------------------------------------------------------
;;; g_list_next ()
;;; 
;;; #define g_list_next(list)
;;; 
;;; A convenience macro to get the next element in a GList.
;;; 
;;; list :
;;;     an element in a GList.
;;; 
;;; Returns :
;;;     the next element, or NULL if there are no more elements.
;;; ----------------------------------------------------------------------------

(defun g-list-next (lst)
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '%g-list 'next)))

;;; ----------------------------------------------------------------------------
;;; GSList
;;; 
;;; struct GSList {
;;;   gpointer data;
;;;   GSList  *next;
;;; };
;;; 
;;; The GSList struct is used for each element in the singly-linked list.
;;; 
;;; gpointer data;
;;;     holds the element's data, which can be a pointer to any kind of data,
;;;     or any integer value using the Type Conversion Macros.
;;; 
;;; GSList *next;
;;;     contains the link to the next element in the list.
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
    (iter (for c initially pointer then (g-slist-next c))
          (until (null-pointer-p c))
          (collect (convert-from-foreign (foreign-slot-value c '%g-slist 'data)
                                         (g-slist-type-type type))))
    (when (g-slist-type-free-from-foreign type)
      (g-slist-free pointer))))

(defmethod translate-to-foreign (lst (type g-slist-type))
  (let ((result (null-pointer))
        last)
    (iter (for item in lst)
          (for n = (g-slist-alloc))
          (for ptr = (convert-to-foreign item (g-slist-type-type type)))
          (setf (foreign-slot-value n '%g-slist 'data) ptr)
          (setf (foreign-slot-value n '%g-slist 'next) (null-pointer))
          (when last
            (setf (foreign-slot-value last '%g-slist 'next) n))
          (setf last n)
          (when (first-iteration-p)
            (setf result n)))
    result))

(export 'g-slist)

;;; ----------------------------------------------------------------------------
;;; g_slist_alloc ()
;;; 
;;; GSList * g_slist_alloc (void)
;;; 
;;; Allocates space for one GSList element. It is called by the
;;; g_slist_append(), g_slist_prepend(), g_slist_insert() and
;;; g_slist_insert_sorted() functions and so is rarely used on its own.
;;; 
;;; Returns :
;;;     a pointer to the newly-allocated GSList element.
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_alloc" g-slist-alloc)
  (:pointer %g-slist))

;;; ----------------------------------------------------------------------------
;;; g_slist_free ()
;;; 
;;; void g_slist_free (GSList *list)
;;; 
;;; Frees all of the memory used by a GSList. The freed elements are returned
;;; to the slice allocator.
;;; 
;;; Note
;;; 
;;; If list elements contain dynamically-allocated memory, you should either
;;; use g_slist_free_full() or free them manually first.
;;; 
;;; lst :
;;;     a GSList
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_free" g-slist-free) :void
  (lst (:pointer %g-slist)))

;;; ----------------------------------------------------------------------------
;;; g_slist_next ()
;;; 
;;; #define g_slist_next(slist)
;;; 
;;; A convenience macro to get the next element in a GSList.
;;; 
;;; slist :
;;;     an element in a GSList.
;;; 
;;; Returns :
;;;     the next element, or NULL if there are no more elements.
;;; ----------------------------------------------------------------------------

(defun g-slist-next (lst)
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '%g-slist 'next)))

;;; --- End of file glib.misc.lisp ---------------------------------------------
