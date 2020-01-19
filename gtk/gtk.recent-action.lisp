;;; ----------------------------------------------------------------------------
;;; gtk.recent-action.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkRecentAction
;;;
;;;     An action of which represents a list of recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentAction
;;;
;;; Functions
;;;
;;;     gtk_recent_action_new
;;;     gtk_recent_action_new_for_manager
;;;     gtk_recent_action_get_show_numbers                 Accessor
;;;     gtk_recent_action_set_show_numbers                 Accessor
;;;
;;; Properties
;;;
;;;     gboolean   show-numbers    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ╰── GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentAction implements GtkBuildable and GtkRecentChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentAction" gtk-recent-action
  (:superclass gtk-action
   :export t
   :interfaces ("GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_action_get_type")
  ((show-numbers
    gtk-recent-action-show-numbers
    "show-numbers" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-recent-action 'type)
 "@version{2013-6-2}
  @begin{short}
    A @sym{gtk-recent-action} represents a list of recently used files, which
    can be shown by widgets such as @class{gtk-recent-chooser-dialog} or
    @class{gtk-recent-chooser-menu}.
  @end{short}

  To construct a submenu showing recently used files, use a
  @sym{gtk-recent-action} as the action for a <menuitem>. To construct a menu
  toolbutton showing the recently used files in the popup menu, use a
  @sym{gtk-recent-action} as the action for a <toolitem> element.
  @see-slot{gtk-recent-action-show-numbers}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-numbers"
                                               'gtk-recent-action) 't)
 "The @code{show-numbers} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the items should be displayed with a number. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-recent-action-show-numbers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-recent-action-show-numbers 'function)
 "@version{2019-6-6}
  @argument[object]{a @class{gtk-recent-action} object}
  @argument[show-numbers]{@em{true} if the shown items should be numbered}
  @begin{short}
    Accessor of the @slot[gtk-recent-action]{show-numbers} slot of the
    @class{gtk-recent-action} class.
  @end{short}

  The @sym{gtk-recent-action-show-numbers} slot access function
  sets whether a number should be added to the items shown by the widgets
  representing action. The numbers are shown to provide a unique character for
  a mnemonic to be used inside the menu item's label. Only the first ten items
  get a number to avoid clashes.
  @see-class{gtk-recent-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_action_new" gtk-recent-action-new) (g-object gtk-action)
 "@version{2019-6-6}
  @argument[name]{a unique name for the action}
  @argument[label]{the label displayed in menu items and on buttons, or
    @code{nil}}
  @argument[tooltip]{a tooltip for the action, or @code{nil}}
  @argument[stock-id]{the stock icon to display in widgets representing the
    action, or @code{nil}}
  @return{The newly created @class{gtk-recent-action}.}
  @begin{short}
    Creates a new @class{gtk-recent-action} object.
  @end{short}
  To add the action to a @class{gtk-action-group} and set the accelerator for
  the action, call the @fun{gtk-action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @sym{gtk-recent-action-new} function has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-recent-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string))

(export 'gtk-recent-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new_for_manager ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_action_new_for_manager"
           gtk-recent-action-new-for-manager) (g-object gtk-action)
 "@version{2019-6-6}
  @argument[name]{a unique name for the action}
  @argument[label]{the label displayed in menu items and on buttons, or
    @code{nil}}
  @argument[tooltip]{a tooltip for the action, or @code{nil}}
  @argument[stock-id]{the stock icon to display in widgets representing the
    action, or @code{nil}}
  @argument[manager]{a @class{gtk-recent-manager}, or @code{nil} for using the
    default @class{gtk-recent-manager}}
  @return{The newly created @class{gtk-recent-action}.}
  @begin{short}
    Creates a new @class{gtk-recent-action} object.
  @end{short}
  To add the action to a @class{gtk-action-group} and set the accelerator for
  the action, call the @fun{gtk-action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @sym{gtk-recent-action-new-manager} function has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-recent-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string)
  (manager (g-object gtk-recent-manager)))

(export 'gtk-recent-action-new-for-manager)

;;; --- End of file gtk.recent-action.lisp -------------------------------------
