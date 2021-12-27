;;; ----------------------------------------------------------------------------
;;; gio.themed-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2021 Dieter Kaiser
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
;;;     Icon theming support
;;;
;;; Types and Values
;;;
;;;     GThemedIcon
;;;
;;; Functions
;;;
;;;     g_themed_icon_new
;;;     g_themed_icon_new_from_names
;;;     g_themed_icon_new_with_default_fallbacks
;;;     g_themed_icon_prepend_name
;;;     g_themed_icon_append_name
;;;     g_themed_icon_get_names
;;;
;;; Properties
;;;
;;;        char*   name                         Write / Construct Only
;;;       GStrv    names                        Read / Write / Construct Only
;;;    gboolean    use-default-fallbacks    	Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GThemedIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GThemedIcon implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GThemedIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GThemedIcon" g-themed-icon
  (:superclass g-object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_themed_icon_get_type")
  ((name
    g-themed-icon-name
    "name" "gchararray" nil t)
   (names
    g-themed-icon-names
    "names" "GStrv" t t)
   (use-default-fallbacks
    g-themed-icon-use-default-fallbacks
    "use-default-fallbacks" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-themed-icon 'type)
 "@version{*2021-4-27}
  @begin{short}
    The @sym{g-themed-icon} class is an implementation of the @class{g-icon}
    interface that supports icon themes.
  @end{short}
  The @sym{g-themed-icon} class contains a list of all of the icons present in
  an icon theme, so that icons can be looked up quickly. The @sym{g-themed-icon}
  class does not provide actual pixmaps for icons, just the icon names. Ideally
  something like the function @fun{gtk-icon-theme-choose-icon} should be used to
  resolve the list of names so that fallback icons work nicely with themes that
  inherit other themes.
  @see-slot{g-themed-icon-name}
  @see-slot{g-themed-icon-names}
  @see-slot{g-themed-icon-use-default-fallbacks}
  @see-class{g-icon}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- g-themed-icon-name -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-themed-icon) 't)
 "The @code{name} property of type @code{:string} (Write / Construct Only) @br{}
  The icon name. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-name 'function)
 "@version{2021-4-15}
  @syntax[]{(g-themed-icon-name object) => name}
  @syntax[]{(setf g-themed-icon-name object) name)}
  @argument[object]{a @class{g-themed-icon} object}
  @argument[name]{a string with the icon name}
  @begin{short}
    Accessor of the @slot[g-themed-icon]{name} slot of the @class{g-themed-icon}
    class.
  @end{short}
  @see-class{g-themed-icon}")

;;; --- g-themed-icon-names ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "names" 'g-themed-icon) 't)
 "The @code{names} property of type @type{g-strv}
  (Read / Write / Construct Only) @br{}
  A list of strings with the icon names.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-names atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-names 'function)
 "@version{2021-4-15}
  @syntax[]{(g-themed-icon-names object) => names}
  @syntax[]{(setf g-themed-icon-names object) names)}
  @argument[object]{a @class{g-themed-icon} object}
  @argument[names]{a list of strings with icon names}
  @begin{short}
    Accessor of the @slot[g-themed-icon]{names} slot of the
    @class{g-themed-icon} class.
  @end{short}

  Gets the names of icons from within @arg{object}.
  @see-class{g-themed-icon}")

;;; --- g-themed-icon-use-default-fallbacks ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-default-fallbacks"
                                               'g-themed-icon) 't)
 "The @code{use-default-fallbacks} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  Whether to use the default fallbacks found by shortening the icon name at '-'
  characters. If the @code{names} list has more than one element, ignores
  any past the first. For example, if the icon name was
  \"gnome-dev-cdrom-audio\", the list would become
  @begin{pre}
'(\"gnome-dev-cdrom-audio\"
  \"gnome-dev-cdrom\"
  \"gnome-dev\"
  \"gnome\")
  @end{pre}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-themed-icon-use-default-fallbacks
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-themed-icon-use-default-fallbacks 'function)
 "@version{2021-4-15}
  @syntax[]{(g-themed-icon-use-default-fallbacks object) => setting}
  @syntax[]{(setf g-themed-icon-use-default-fallbacks object) setting)}
  @argument[object]{a @class{g-themed-icon} object}
  @argument[setting]{a boolean whether to use default fallbacks}
  @begin{short}
    Accessor of the @slot[g-themed-icon]{use-default-fallbacks} slot of the
    @class{g-themed-icon} class.
  @end{short}

  Whether to use the default fallbacks found by shortening the icon name at '-'
  characters. If the @code{names} list has more than one element, ignores
  any past the first. For example, if the icon name was
  \"gnome-dev-cdrom-audio\", the list would become
  @begin{pre}
'(\"gnome-dev-cdrom-audio\"
  \"gnome-dev-cdrom\"
  \"gnome-dev\"
  \"gnome\")
  @end{pre}
  @see-class{g-themed-icon}")

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-themed-icon-new))

(defun g-themed-icon-new (name)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-17}
  @argument[name]{a string containing an icon name}
  @return{A new @class{g-themed-icon} object.}
  @begin{short}
    Creates a new themed icon for the icon name.
  @end{short}
  @see-class{g-themed-icon}
  @see-function{g-themed-icon-new-from-names}
  @see-function{g-themed-icon-new-with-default-fallbacks}"
  (make-instance 'g-themed-icon
                 :name name))

(export 'g-themed-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_from_names ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-themed-icon-new-from-names))

(defun g-themed-icon-new-from-names (&rest names)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[names]{a list of strings containing icon names}
  @return{A new @class{g-themed-icon} object.}
  @begin{short}
    Creates a new themed icon for @arg{names}.
  @end{short}
  @see-class{g-themed-icon}
  @see-function{g-themed-icon-new}
  @see-function{g-themed-icon-new-with-default-fallbacks}"
  (make-instance 'g-themed-icon
                 :names (if (listp (first names))
                            (first names)
                            names)))

(export 'g-themed-icon-new-from-names)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_new_with_default_fallbacks ()
;;; ----------------------------------------------------------------------------

(defun g-themed-icon-new-with-default-fallbacks (name)
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-22}
  @argument[name]{a string containing an icon name}
  @return{A new @class{g-themed-icon} object.}
  @begin{short}
    Creates a new themed icon for @arg{name}, and all the names that can
    be created by shortening iconname at '-' characters.
  @end{short}
  @begin[Example]{dictionary}
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
  @end{dictionary}
  @see-class{g-themed-icon}
  @see-function{g-themed-icon-new}
  @see-function{g-themed-icon-new-from-names}"
  (make-instance 'g-themed-icon
                 :name name
                 :use-default-fallbacks t))

(export 'g-themed-icon-new-with-default-fallbacks)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_prepend_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_themed_icon_prepend_name" g-themed-icon-prepend-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[icon]{a @class{g-themed-icon} object}
  @argument[name]{a string with the name of the icon to prepend to list of
    icons from within @arg{icon}}
  @begin{short}
    Prepend a name to the list of icons from within @arg{icon}.
  @end{short}
  Note that doing so invalidates the hash computed by prior calls to the
  function @fun{g-icon-hash}.
  @see-class{g-themed-icon}
  @see-function{g-icon-hash}
  @see-function{g-themed-icon-append-name}"
  (icon (g-object g-themed-icon))
  (name :string))

(export 'g-themed-icon-prepend-name)

;;; ----------------------------------------------------------------------------
;;; g_themed_icon_append_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_themed_icon_append_name" g-themed-icon-append-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-15}
  @argument[icon]{a @class{g-themed-icon} object}
  @argument[name]{a string with the name of the icon to append to list of icons
    from within @arg{icon}}
  @begin{short}
    Append a name to the list of icons from within @arg{icon}.
  @end{short}
  Note that doing so invalidates the hash computed by prior calls to the
  function @fun{g-icon-hash}.
  @see-class{g-themed-icon}
  @see-function{g-icon-hash}
  @see-function{g-themed-icon-prepend-name}"
  (icon (g-object g-themed-icon))
  (name :string))

(export 'g-themed-icon-append-name)

;;; --- gio.themed-icon.lisp ---------------------------------------------------
