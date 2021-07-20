;;; ----------------------------------------------------------------------------
;;; gtk.activatable.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkActivatable
;;;
;;;     An interface for activatable widgets
;;;
;;; Types and Values
;;;
;;;     GtkActivatable
;;;
;;; Functions
;;;
;;;     gtk_activatable_do_set_related_action
;;;     gtk_activatable_get_related_action
;;;     gtk_activatable_get_use_action_appearance
;;;     gtk_activatable_sync_action_properties
;;;     gtk_activatable_set_related_action
;;;     gtk_activatable_set_use_action_appearance
;;;
;;; Properties
;;;
;;;     GtkAction*   related-action           Read / Write
;;;      gboolean    use-action-appearance    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkActivatable
;;;
;;; Prerequisites
;;;
;;;     GtkActivatable requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GtkActivatable is implemented by GtkButton, GtkCheckButton,
;;;     GtkCheckMenuItem, GtkColorButton, GtkFontButton, GtkImageMenuItem,
;;;     GtkLinkButton, GtkLockButton, GtkMenuButton, GtkMenuItem,
;;;     GtkMenuToolButton, GtkModelButton, GtkRadioButton, GtkRadioMenuItem,
;;;     GtkRadioToolButton, GtkRecentChooserMenu, GtkScaleButton,
;;;     GtkSeparatorMenuItem, GtkSeparatorToolItem, GtkSwitch,
;;;     GtkTearoffMenuItem, GtkToggleButton, GtkToggleToolButton,
;;;     GtkToolButton, GtkToolItem and GtkVolumeButton.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActivatable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActivatable" gtk-activatable
  (:export t
   :type-initializer "gtk_activatable_get_type")
  (related-action
   gtk-activatable-related-action
   "related-action" "GtkAction" t t)
  (use-action-appearance
   gtk-activatable-use-action-appearance
   "use-action-appearance" "gboolean" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-activatable 'type)
 "@version{2021-7-20}
  @begin{short}
    Activatable widgets can be connected to a @class{gtk-action} object and
    reflects the state of its action.
  @end{short}
  A activatable widget can also provide feedback through its action, as they
  are responsible for activating their related actions.
  @begin[Warning]{dictionary}
    The @sym{gtk-activatable} interface has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-slot{gtk-activatable-related-action}
  @see-slot{gtk-activatable-use-action-appearance}
  @see-class{gtk-action}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-activatable-related-action -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "related-action"
                                               'gtk-activatable) 't)
 "The @code{related-action} property of type @class{gtk-action} (Read / Write)
  @br{}
  The action that the activatable will activate and receive updates from for
  various states and possibly appearance.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable-related-action atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-activatable-related-action 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-activatable-related-action object) => action}
  @syntax[]{(setf (gtk-activatable-related-action object) action)}
  @argument[activatable]{a @class{gtk-activatable} widget}
  @argument[action]{a @class{gtk-action} object to set}
  @begin{short}
    Accessor of the @slot[gtk-activatable]{related-action} slot of the
    @class{gtk-activatable} class.
  @end{short}

  The slot access function @sym{gtk-activatable-related-action} gets the
  related action. The slot access function
  @sym{(setf gtk-activatable-related-action)} sets the related action.
  @begin[Warning]{dictionary}
    The function @sym{gtk-activatable-related-action} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-activatable}
  @see-class{gtk-action}")

;;; --- gtk-activatable-use-action-appearance ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-action-appearance"
                                               'gtk-activatable) 't)
 "The @code{use-action-appearance} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the activatable should reset its layout and appearance when setting
  the related action or when the action changes appearance. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-activatable-use-action-appearance
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-activatable-use-action-appearance 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-activatable-use-action-appearance object) => action}
  @syntax[]{(setf (gtk-activatable-use-action-appearance object) action)}
  @argument[activatable]{a @class{gtk-activatable} widget}
  @argument[use-appearance]{a boolean whether to use the actions appearance}
  @begin{short}
    Accessor of the @slot[gtk-activatable]{use-action-appearance} slot of the
    @class{gtk-activatable} class.
  @end{short}

  The slot access function @sym{gtk-activatable-use-action-appearance} gets
  whether the activatable should reset its layout and appearance when setting
  the related action or when the action changes appearance. The slot access
  function @sym{(setf gtk-activatable-use-action-appearance)} sets whether this
  activatable should reset its layout and appearance.
  @begin[Warning]{dictionary}
    The function @sym{gtk-activatable-use-action-appearance} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-activatable}")

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_do_set_related_action ()               not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_activatable_do_set_related_action"
           gtk-activatable-do-set-related-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-22}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[action]{the @class{gtk-action} object to set}
  @begin{short}
    This is a utility function for @class{gtk-activatable} interface
    implementors.
  @end{short}

  When implementing the @class{gtk-activatable} interface you must call this
  when handling changes of the @slot[gtk-activatable]{related-action} property,
  and you must also use this to break references in @code{GObject->dispose()}.

  This function adds a reference to the currently set related action for you,
  it also makes sure the @code{GtkActivatable->update()} method is called when
  the related @class{gtk-action} object properties change and registers to the
  action's proxy list.
  @begin[Note]{dictionary}
    Be careful to call this before setting the local copy of the
    @class{gtk-action} object property, since this function uses
    @fun{gtk-activatable-get-related-action} to retrieve the previous action.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The function @sym{gtk-activatable-do-set-related-action} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-activatable}
  @see-class{gtk-action}
  @see-function{gtk-activatable-get-related-action}"
  (activatable (g-object gtk-activatable))
  (action (g-object gtk-action)))

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_sync_action_properties ()              not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_activatable_sync_action_properties"
           gtk-activatable-sync-action-properties) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-1-22}
  @argument[activatable]{a @class{gtk-activatable} object}
  @argument[action]{the related @class{gtk-action} or @code{nil}}
  @begin{short}
    This is called to update the @arg{activatable} completely, this is called
    internally when the @slot[gtk-activatable]{related-action} property is set
    or unset and by the implementing class when
    @slot[gtk-activatable]{use-action-appearance} changes.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-activatable-sync-action-properties} has been
    deprecated since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-activatable}
  @see-class{gtk-action}"
  (activatable (g-object gtk-activatable))
  (action (g-object gtk-action)))

;;; --- End of file gtk.activatable.lisp ---------------------------------------
