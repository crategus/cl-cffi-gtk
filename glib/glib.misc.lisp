;;; ----------------------------------------------------------------------------
;;; glib.misc.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GLib 2.36.3 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Only the following struct and functions are implemented:
;;;
;;;     GTimeVal
;;;
;;;     g_get_current_time
;;;     g_get_monotonic_time
;;;     g_get_real_time
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
;;;
;;; Threads
;;;
;;; Portable support for threads, mutexes, locks, conditions and thread private
;;; data
;;;
;;; Implemented is:
;;;
;;;     g-mutex
;;;     g-cond
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; gsize
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+x86-64 (defctype g-size :uint64)
  #+x86    (defctype g-size :ulong)
  #+ppc32  (defctype g-size :uint32)
  #+ppc64  (defctype g-size :uint64)
  #+arm64  (defctype g-size :uint64)
  #-(or x86-64 x86 ppc32 ppc64 arm64)
  (error "Can not define '~A', unknown CPU architecture" 'g-size))

(export 'g-size)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-size 'type)
 "@version{2013-7-22}
  @begin{short}
    An unsigned integer type of the result of the sizeof operator,
    corresponding to the @code{size_t} type defined in C99.
  @end{short}
  This type is wide enough to hold the numeric value of a pointer, so it is
  usually 32 bit wide on a 32 bit platform and 64 bit wide on a 64 bit platform.
  Values of this type can range from 0 to @code{G_MAXSIZE}.
  @see-type{g-ssize}
  @see-type{g-offset}")

;;; ----------------------------------------------------------------------------
;;; gssize
;;; ----------------------------------------------------------------------------

(defctype g-ssize :long)

(export 'g-ssize)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-ssize 'type)
 "@version{2013-7-22}
  @begin{short}
    A signed variant of @type{g-size}, corresponding to the @code{ssize_t}
    defined on most platforms.
  @end{short}
  Values of this type can range from @code{G_MINSSIZE} to @code{G_MAXSSIZE}.
  @see-type{g-size}
  @see-type{g-offset}")

;;; ----------------------------------------------------------------------------
;;; goffset
;;; ----------------------------------------------------------------------------

(defctype g-offset :uint64)

(export 'g-offset)

#+cl-cffi-gtk-documentation
(setf (documentation 'g-offset 'type)
 "@version{2013-7-22}
  @begin{short}
    A signed integer type that is used for file offsets, corresponding to the
    C99 type @code{off64_t}.
  @end{short}
  Values of this type can range from @code{G_MINOFFSET} to @code{G_MAXOFFSET}.

  Since: 2.14
  @see-type{g-size}
  @see-type{g-ssize}")

;;; ----------------------------------------------------------------------------
;;; g_malloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_malloc" g-malloc) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @argument[n-bytes]{the number of bytes to allocate}
  @return{A foreign pointer to the allocated memory.}
  Allocates @arg{n-bytes} bytes of memory. If @arg{n-bytes} is 0 @sym{g-malloc}
  returns a foreign @code{null}-pointer.
  @see{g-free}"
  (n-bytes g-size))

(export 'g-malloc)

;;; ----------------------------------------------------------------------------
;;; g_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_free" g-free) :void
 "@version{2013-7-22}
  @argument[mem]{a foreign pointer to the memory to free}
  Frees the memory pointed to by the foreign pointer @arg{mem}. If @arg{mem}
  is a @code{null}-pointer @sym{g-free} simply returns.
  @see{g-malloc}"
  (mem :pointer))

(export 'g-free)

;;; ----------------------------------------------------------------------------
;;; GTimeVal
;;; ----------------------------------------------------------------------------

(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-time-val atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-time-val 'type)
 "@version{2013-7-22}
  @begin{short}
    Represents a precise time, with seconds and microseconds.
  @end{short}
  Similar to the struct @code{timeval} returned by the @code{gettimeofday()}
  UNIX call.

  GLib is attempting to unify around the use of 64bit integers to represent
  microsecond-precision time. As such, this type will be removed from a
  future version of GLib.
  @begin{pre}
(defcstruct g-time-val
  (tv-sec :long)
  (tv-usec :long))
  @end{pre}
  @begin[code]{table}
    @entry[tv-sec]{seconds}
    @entry[tv-usec]{microseconds}
  @end{table}")

(export 'g-time-val)

;;; ----------------------------------------------------------------------------
;;; g_get_current_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_current_time" %g-get-current-time) :void
  (result (:pointer (:struct g-time-val))))

(defun g-get-current-time ()
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @return{The values seconds and microseconds from the @type{g-time-val}
    structure.}
  @begin{short}
    Equivalent to the UNIX @code{gettimeofday()} function, but portable.
  @end{short}

  You may find @fun{g-get-real-time} to be more convenient.
  @begin[Example]{dictionary}
    @begin{pre}
 (multiple-value-bind (seconds microseconds)
     (g-get-current-time)
   (format t \"seconds = ~A, microseconds = ~A~%\"
             seconds microseconds))
=> seconds = 1374489588, microseconds = 442027
    @end{pre}
  @end{dictionary}
  @see-type{g-time-val}
  @see-function{g-get-real-time}
  @see-function{g-get-monotonic-time}"
  (with-foreign-object (result '(:struct g-time-val))
    (%g-get-current-time result)
    (values (foreign-slot-value result '(:struct g-time-val) 'tv-sec)
            (foreign-slot-value result '(:struct g-time-val) 'tv-usec))))

(export 'g-get-current-time)

;;; ----------------------------------------------------------------------------
;;; g_get_monotonic_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_monotonic_time" g-get-monotonic-time) :int64
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @return{The monotonic time, in microseconds.}
  @short{Queries the system monotonic time, if available.}

  On POSIX systems with @code{clock_gettime()} and @code{CLOCK_MONOTONIC} this
  call is a very shallow wrapper for that. Otherwise, we make a best effort that
  probably involves returning the wall clock time (with at least microsecond
  accuracy, subject to the limitations of the OS kernel).

  It is important to note that @code{POSIX CLOCK_MONOTONIC} does not count time
  spent while the machine is suspended.

  On Windows, \"limitations of the OS kernel\" is a rather substantial
  statement. Depending on the configuration of the system, the wall clock time
  is updated as infrequently as 64 times a second (which is approximately every
  16 ms). Also, on XP (but not on Vista or later) the monotonic clock is locally
  monotonic, but may differ in exact value between processes due to timer wrap
  handling.

  Since 2.28
  @see-function{g-get-current-time}
  @see-function{g-get-real-time}")

(export 'g-get-monotonic-time)

;;; ----------------------------------------------------------------------------
;;; g_get_real_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_get_real_time" g-get-real-time) :int64
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @return{The number of microseconds since January 1, 1970 UTC.}
  @short{Queries the system wall-clock time.}

  This call is functionally equivalent to @fun{g-get-current-time} except that
  the return value is often more convenient than dealing with a value list
  of seconds and microseconds.

  You should only use this call if you are actually interested in the real
  wall-clock time. @fun{g-get-monotonic-time} is probably more useful for
  measuring intervals.

  Since 2.28
  @see-function{g-get-current-time}
  @see-function{g-get-monotonic-time}")

(export 'g-get-real-time)

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
 "@version{2013-7-22}
  @begin{short}
    A type that is almost like the foreign CFFI type @code{:string} but uses the
    GLib functions @fun{g-malloc} and @fun{g-free} to allocate and free memory.
  @end{short}

  The @sym{g-string} type performs automatic conversion between Lisp and C
  strings. Note that, in the case of functions the converted C string will
  have dynamic extent (i. e. it will be automatically freed after the foreign
  function returns).

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
 "@version{2013-7-22}
  @begin{short}
    This type represents and performs automatic conversion between a list of
    Lisp strings and an array of C strings of type @type{g-string}.
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
;;; g_strdup ()
;;; ----------------------------------------------------------------------------

;; We do not export this function. It is internal for the implementation of
;; the types g-string and g-strv.

(defcfun ("g_strdup" %g-strdup) :pointer
 #+cl-cffi-gtk-documentation
 "@argument[str]{the string to duplicate}
  @return{a newly-allocated copy of @arg{str}}
  @short{Duplicates a string.}
  If @arg{str} is @code{NULL} it returns @code{NULL}. The returned string should
  be freed with @fun{g-free} when no longer needed."
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
 "@version{2013-7-22}
  @begin{short}
    The @sym{g-list} type represents a C doubly-linked list with elements of
    the type @code{GList} structure.
  @end{short}
  The type @sym{g-list} performs automatic conversion from a C list to a Lisp
  list. The conversion from a Lisp list to a C reprensentation is implemented
  only for foreign pointers, but not Lisp objects.")

(export 'g-list)

;;; ----------------------------------------------------------------------------
;;; g_list_free ()
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
;;; g_list_next ()
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
          (collect (convert-from-foreign (foreign-slot-value c '(:struct %g-slist) 'data)
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
 "@version{2013-7-22}
  @begin{short}
    The @sym{g-slist} type represents a C singly-linked list with elements of
    type @code{GSList} structure.
  @end{short}
  The type @sym{g-slist} performs automatic conversion from a C list to a Lisp
  list.")

(export 'g-slist)

;;; ----------------------------------------------------------------------------
;;; g_slist_alloc ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_slist_alloc" %g-slist-alloc) (:pointer (:struct %g-slist))
 #+cl-cffi-gtk-documentation
 "@return{a pointer to the newly-allocated GSList element.}
  @short{Allocates space for one GSList element.}
  It is called by the g_slist_append(), g_slist_prepend(), g_slist_insert() and
  g_slist_insert_sorted() functions and so is rarely used on its own.")

;;; ----------------------------------------------------------------------------
;;; g_slist_free ()
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
;;; g_slist_next ()
;;; ----------------------------------------------------------------------------

(defun %g-slist-next (lst)
 #+cl-cffi-gtk-documentation
 "@argument[slist]{an element in a GSList.}
  @return{the next element, or NULL if there are no more elements.}
  @short{A convenience macro to get the next element in a GSList.}"
  (if (null-pointer-p lst)
      (null-pointer)
      (foreign-slot-value lst '(:struct %g-slist) 'next)))

;;; ----------------------------------------------------------------------------
;;; union GMutex
;;;
;;; union _GMutex
;;; {
;;;   /*< private >*/
;;;   gpointer p;
;;;   guint i[2];
;;; };
;;;
;;; The GMutex struct is an opaque data structure to represent a mutex (mutual
;;; exclusion). It can be used to protect data against shared access. Take for
;;; example the following function:
;;;
;;; Example 2. A function which will not work in a threaded environment
;;;
;;;   int
;;;   give_me_next_number (void)
;;;   {
;;;     static int current_number = 0;
;;;
;;;     /* now do a very complicated calculation to calculate the new
;;;      * number, this might for example be a random number generator
;;;      */
;;;     current_number = calc_next_number (current_number);
;;;
;;;     return current_number;
;;;   }
;;;
;;; It is easy to see that this won't work in a multi-threaded application.
;;; There current_number must be protected against shared access. A GMutex can
;;; be used as a solution to this problem:
;;;
;;; Example 3. Using GMutex to protected a shared variable
;;;
;;;   int
;;;   give_me_next_number (void)
;;;   {
;;;     static GMutex mutex;
;;;     static int current_number = 0;
;;;     int ret_val;
;;;
;;;     g_mutex_lock (&mutex);
;;;     ret_val = current_number = calc_next_number (current_number);
;;;     g_mutex_unlock (&mutex);
;;;
;;;     return ret_val;
;;;   }
;;;
;;; Notice that the GMutex is not initialised to any particular value. Its
;;; placement in static storage ensures that it will be initialised to
;;; all-zeros, which is appropriate.
;;;
;;; If a GMutex is placed in other contexts (eg: embedded in a struct) then it
;;; must be explicitly initialised using g_mutex_init().
;;;
;;; A GMutex should only be accessed via g_mutex_ functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-mutex)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-mutex atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-mutex 'type)
 "@version{2013-7-21}
  The @sym{g-mutex} structure is an opaque data structure to represent a mutex
  (mutual exclusion).")

(export 'g-mutex)

;;; ----------------------------------------------------------------------------
;;; struct GCond
;;;
;;; struct GCond {
;;; };
;;;
;;; The GCond struct is an opaque data structure that represents a condition.
;;; Threads can block on a GCond if they find a certain condition to be false.
;;; If other threads change the state of this condition they signal the GCond,
;;; and that causes the waiting threads to be woken up.
;;;
;;; Consider the following example of a shared variable. One or more threads can
;;; wait for data to be published to the variable and when another thread
;;; publishes the data, it can signal one of the waiting threads to wake up to
;;; collect the data.
;;;
;;; Example 6.  Using GCond to block a thread until a condition is satisfied
;;;
;;;   gpointer current_data = NULL;
;;;   GMutex data_mutex;
;;;   GCond data_cond;
;;;
;;;   void
;;;   push_data (gpointer data)
;;;   {
;;;     g_mutex_lock (&data_mutex);
;;;     current_data = data;
;;;     g_cond_signal (&data_cond);
;;;     g_mutex_unlock (&data_mutex);
;;;   }
;;;
;;;   gpointer
;;;   pop_data (void)
;;;   {
;;;     gpointer data;
;;;
;;;     g_mutex_lock (&data_mutex);
;;;     while (!current_data)
;;;       g_cond_wait (&data_cond, &data_mutex);
;;;     data = current_data;
;;;     current_data = NULL;
;;;     g_mutex_unlock (&data_mutex);
;;;
;;;     return data;
;;;   }
;;;
;;; Whenever a thread calls pop_data() now, it will wait until current_data is
;;; non-NULL, i.e. until some other thread has called push_data().
;;;
;;; The example shows that use of a condition variable must always be paired
;;; with a mutex. Without the use of a mutex, there would be a race between the
;;; check of current_data by the while loop in pop_data and waiting.
;;; Specifically, another thread could set pop_data after the check, and signal
;;; the cond (with nobody waiting on it) before the first thread goes to sleep.
;;; GCond is specifically useful for its ability to release the mutex and go to
;;; sleep atomically.
;;;
;;; It is also important to use the g_cond_wait() and g_cond_wait_until()
;;; functions only inside a loop which checks for the condition to be true. See
;;; g_cond_wait() for an explanation of why the condition may not be true even
;;; after it returns.
;;;
;;; If a GCond is allocated in static storage then it can be used without
;;; initialisation. Otherwise, you should call g_cond_init() on it and
;;; g_cond_clear() when done.
;;;
;;; A GCond should only be accessed via the g_cond_ functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-cond)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-cond atdoc:*type-name-alias*) "CStruct"
      (documentation 'g-cond 'type)
 "@version{2013-7-21}
  The @sym{g-cond} structure is an opaque data structure that represents a
  condition.")

(export 'g-cond)

(defcstruct %g-hash-table)

(define-foreign-type g-hash-table-type ()
  ((key-type :reader g-hash-table-type-key-type :initarg :key-type :initform :pointer)
   (value-type :reader g-hash-table-type-value-type :initarg :value-type :initform :pointer)
   (test :reader g-hash-table-type-test :initarg :test :initform 'eql))
  (:actual-type :pointer))

(define-parse-method g-hash-table (key-type value-type &key (test 'eql))
  (make-instance 'g-hash-table-type
                 :key-type key-type
                 :value-type value-type
                 :test test))

(defcfun ("g_hash_table_new_full" %g-hash-table-new-full) (:pointer (:struct %g-hash-table))
  (hash-func :pointer)
  (key-equal-func :pointer)
  (key-destroy-func :pointer)
  (value-destroy-func :pointer))

(defun %g-hash-table-new (&key (key-type :string))
  (let ((null (null-pointer)))
    (multiple-value-bind (equal hash)
        (ecase key-type
          (:string (values
                    (foreign-symbol-pointer "g_str_equal")
                    (foreign-symbol-pointer "g_str_hash")))
          (:int (values
                 (foreign-symbol-pointer "g_int_equal")
                 (foreign-symbol-pointer "g_int_equal")))
          (:int64 (values
                   (foreign-symbol-pointer "g_int64_equal")
                   (foreign-symbol-pointer "g_int64_equal")))
          ((NIL :pointer) (values null null))
          (:double (values
                    (foreign-symbol-pointer "g_double_equal")
                    (foreign-symbol-pointer "g_double_equal"))))
      (%g-hash-table-new-full equal hash null null))))

(defcfun ("g_hash_table_unref" %g-hash-table-unref) :void
  (hash-table (:pointer (:struct %g-hash-table))))

(defcfun ("g_hash_table_insert" %g-hash-table-insert) :boolean
  (hash-table (:pointer (:struct %g-hash-table)))
  (key :pointer)
  (value :pointer))

(defcstruct %g-hash-table-iter
  (dummy1 :pointer)
  (dummy2 :pointer)
  (dummy3 :pointer)
  (dummy4 :int)
  (dummy5 :boolean)
  (dummy6 :pointer))

(defcfun ("g_hash_table_iter_init" %g-hash-table-iter-init) :void
  (iter (:pointer (:struct %g-hash-table-iter)))
  (hash-table (:pointer (:struct %g-hash-table))))

(defcfun ("g_hash_table_iter_next" %g-hash-table-iter-next) :boolean
  (iter (:pointer (:struct %g-hash-table-iter)))
  (key :pointer)
  (value :pointer))

(defmethod translate-from-foreign (pointer (type g-hash-table-type))
  (with-foreign-objects ((iter '(:struct %g-hash-table-iter))
                         (foreign-key :pointer)
                         (foreign-value :pointer))
    (%g-hash-table-iter-init iter pointer)
    (let ((result (make-hash-table :test (g-hash-table-type-test type))))
      (iterate
        (while (%g-hash-table-iter-next iter foreign-key foreign-value))
        (let ((key (convert-from-foreign
                    (mem-ref foreign-key :pointer)
                    (g-hash-table-type-key-type type)))
              (value (convert-from-foreign
                      (mem-ref foreign-value :pointer)
                      (g-hash-table-type-value-type type))))
          (setf (gethash key result) value)))
      result)))

(defmethod translate-to-foreign (value (type g-hash-table-type))
  (let ((result (%g-hash-table-new :key-type (g-hash-table-type-key-type type))))
    (iterate
      (for (key value) in-hashtable value)
      (let ((foreign-key
              (convert-to-foreign key (g-hash-table-type-key-type type)))
            (foreign-value
              (convert-to-foreign value (g-hash-table-type-value-type type))))
        (%g-hash-table-insert result foreign-key foreign-value)))
    result))

(export 'g-hash-table)

;;; --- End of file glib.misc.lisp ---------------------------------------------
