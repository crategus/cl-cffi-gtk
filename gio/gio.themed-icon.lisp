;;; ----------------------------------------------------------------------------
;;; gio.themed-icon.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.36.3. The latest version of this documentation can be found
;;; on-line at <http://library.gnome.org/devel/gio/unstable/>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; GThemedIcon
;;;
;;; Icon theming support
;;;
;;; Synopsis
;;;
;;;     GThemedIcon
;;;
;;;     g_themed_icon_new
;;;     g_themed_icon_new_from_names
;;;     g_themed_icon_new_with_default_fallbacks
;;;     g_themed_icon_prepend_name
;;;     g_themed_icon_append_name
;;;     g_themed_icon_get_names
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GThemedIcon
;;;
;;; Implemented Interfaces
;;;
;;; GThemedIcon implements GIcon.
;;;
;;; Properties
;;;
;;;   "name"                     gchar*               : Write / Construct Only
;;;   "names"                    GStrv                : Read / Write / Construct
;;;   "use-default-fallbacks"    gboolean             : Read / Write / Construct
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GThemedIcon
;;;
;;; typedef struct _GThemedIcon GThemedIcon;
;;;
;;; An implementation of GIcon for themed icons.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GThemedIcon" g-themed-icon
  (:superclass g-object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_themed_icon_get_type")
  ((name
    g-themed-icon-name
    "name" "gchar" nil t)
   (names
    g-themed-icon-names
    "names" "GStrv" t t)
   (use-default-fallbacks
    g-themed-icon-use-default-fallbacks
    "use-default-fallbacks" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-themed-icon 'type)
 "@version{2013-7-18}
  @begin{short}
    @sym{g-themed-icon} is an implementation of @class{g-icon} that supports
    icon themes.
  @end{short}
  @sym{g-themed-icon} contains a list of all of the icons present in an icon
  theme, so that icons can be looked up quickly. @sym{g-themed-icon} does not
  provide actual pixmaps for icons, just the icon names. Ideally something like
  the function @fun{gtk-icon-theme-choose-icon} should be used to resolve the
  list of names so that fallback icons work nicely with themes that inherit
  other themes.
  @see-slot{g-themed-icon-name}
  @see-slot{g-themed-icon-names}
  @see-slot{g-themed-icon-use-default-fallbacks}
  @see-class{g-icon}
  @see-function{gtk-icon-theme-choose-icon}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-themed-icon) 't)
 "The @code{\"name\"} property of type @code{:string}
  (Write / Construct Only) @br{}
  The icon name. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "names" 'g-themed-icon) 't)
 "The @code{\"names\"} property of type @type{g-strv}
  (Read / Write / Construct Only) @br{}
  A list of icon names.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-default-fallbacks"
                                               'g-themed-icon) 't)
 "The @code{\"use-default-fallbacks\"} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  Whether to use the default fallbacks found by shortening the icon name at '-'
  characters. If the @code{\"names\"} array has more than one element, ignores
  any past the first. @br{}
  For example, if the icon name was \"gnome-dev-cdrom-audio\", the array would
  become
  @begin{pre}
'(\"gnome-dev-cdrom-audio\"
  \"gnome-dev-cdrom\"
  \"gnome-dev\"
  \"gnome\")
  @end{pre}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-name 'function)
 "@version{2013-7-18}
  Accessor of the slot @code{\"name\"} of the @class{g-themed-icon}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-names atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-names 'function)
 "@version{2013-7-18}
  Accessor of the slot @code{\"names\"} of the @class{g-themed-icon}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-use-default-fallbacks
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-use-default-fallbacks 'function)
 "@version{2013-7-18}
  Accessor of the slot @code{\"use-default-fallbacks\"} of the
  @class{g-themed-icon} class.")

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-themed-icon-new))

(defun g-themed-icon-new (icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon-name]{a string containing an icon name}
  @return{A new @class{g-themed-icon}.}
  Creates a new themed icon for iconname.
  @see-function{g-themed-icon-new-from-names}
  @see-function{g-themed-icon-new-with-default-fallbacks}"
  (make-instance 'g-themed-icon
                 :name icon-name))

(export 'g-themed-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_from_names ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-themed-icon-new-from-names))

(defun g-themed-icon-new-from-names (&rest icon-names)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon-names]{a list of strings containing icon names}
  @return{A new @class{g-themed-icon}.}
  Creates a new themed icon for @arg{icon-names}.
  @see-function{g-themed-icon-new}
  @see-function{g-themed-icon-new-with-default-fallbacks}"
  (make-instance 'g-themed-icon
                 :names (if (listp (first icon-names))
                            (first icon-names)
                            icon-names)))

(export 'g-themed-icon-new-from-names)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_with_default_fallbacks ()
;;; ----------------------------------------------------------------------------

(defun g-themed-icon-new-with-default-fallbacks (icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon-name]{a string containing an icon name}
  @return{A new @class{g-themed-icon}.}
  @begin{short}
    Creates a new themed icon for @arg{icon-name}, and all the names that can be
    created by shortening iconname at '-' characters.
  @end{short}

  In the following example, @code{icon1} and @code{icon2} are equivalent:
  @begin{pre}
(let* ((names (list \"gnome-dev-cdrom-audio\"
                    \"gnome-dev-cdrom\"
                    \"gnome-dev\"
                    \"gnome\"))
       (icon1 (g-themed-icon-new-from-names names))
       (icon2 (g-themed-icon-new-with-default-fallbacks
                \"gnome-dev-cdrom-audio\")))
  ... )
  @end{pre}
  @see-function{g-themed-icon-new}
  @see-function{g-themed-icon-new-from-names}"
  (make-instance 'g-themed-icon
                 :name icon-name
                 :use-default-fallbacks t))

(export 'g-themed-icon-new-with-default-fallbacks)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_prepend_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_themed_icon_prepend_name" g-themed-icon-prepend-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon]{a @class{g-themed-icon} object}
  @argument[icon-name]{name of icon to prepend to list of icons from within
    @arg{icon}}
  @begin{short}
    Prepend a name to the list of icons from within @arg{icon}.
  @end{short}
  
  @subheading{Note}
    Note that doing so invalidates the hash computed by prior calls to
    @fun{g-icon-hash}.

  Since 2.18
  @see-function{g-icon-hash}
  @see-function{g-themed-icon-append-name}"
  (icon (g-object g-themed-icon))
  (icon-name :string))

(export 'g-themed-icon-prepend-name)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_append_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_themed_icon_append_name" g-themed-icon-append-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon]{a @class{g-themed-icon} object}
  @argument[icon-name]{name of icon to append to list of icons from within
    @arg{icon}}
  @begin{short}
    Append a name to the list of icons from within @arg{icon}.
  @end{short}

  @subheading{Note}
    Note that doing so invalidates the hash computed by prior calls to
    @fun{g-icon-hash}.
  @see-function{g-icon-hash}
  @see-function{g-themed-icon-prepend-name}"
  (icon (g-object g-themed-icon))
  (icon-name :string))

(export 'g-themed-icon-append-name)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_get_names ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-themed-icon-get-names))

(defun g-themed-icon-get-names (icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-19}
  @argument[icon]{a @class{g-themed-icon} object}
  @return{A list of icon names.}
  Gets the names of icons from within @arg{icon}."
  (g-themed-icon-names icon))

(export 'g-themed-icon-get-names)

;;; --- gio.themed-icon.lisp ---------------------------------------------------
