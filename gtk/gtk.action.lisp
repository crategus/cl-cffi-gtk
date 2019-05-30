;;; ----------------------------------------------------------------------------
;;; gtk.action.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.8 and modified to document the Lisp binding to the GTK library.
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
;;;    GtkAction
;;;
;;;    An action which can be triggered by a menu or toolbar item
;;;
;;;    Synopsis
;;;
;;;    GtkAction
;;;
;;;    gtk_action_new
;;;    gtk_action_get_name
;;;    gtk_action_is_sensitive
;;;    gtk_action_get_sensitive
;;;    gtk_action_set_sensitive
;;;    gtk_action_is_visible
;;;    gtk_action_get_visible
;;;    gtk_action_set_visible
;;;    gtk_action_activate
;;;    gtk_action_create_icon
;;;    gtk_action_create_menu_item
;;;    gtk_action_create_tool_item
;;;    gtk_action_create_menu
;;;    gtk_action_get_proxies
;;;    gtk_action_connect_accelerator
;;;    gtk_action_disconnect_accelerator
;;;    gtk_action_block_activate
;;;    gtk_action_unblock_activate
;;;    gtk_action_get_always_show_image
;;;    gtk_action_set_always_show_image
;;;    gtk_action_get_accel_path
;;;    gtk_action_set_accel_path
;;;    gtk_action_get_accel_closure
;;;    gtk_action_set_accel_group
;;;    gtk_action_set_label
;;;    gtk_action_get_label
;;;    gtk_action_set_short_label
;;;    gtk_action_get_short_label
;;;    gtk_action_set_tooltip
;;;    gtk_action_get_tooltip
;;;    gtk_action_set_stock_id
;;;    gtk_action_get_stock_id
;;;    gtk_action_set_gicon
;;;    gtk_action_get_gicon
;;;    gtk_action_set_icon_name
;;;    gtk_action_get_icon_name
;;;    gtk_action_set_visible_horizontal
;;;    gtk_action_get_visible_horizontal
;;;    gtk_action_set_visible_vertical
;;;    gtk_action_get_visible_vertical
;;;    gtk_action_set_is_important
;;;    gtk_action_get_is_important
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class GtkAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAction" gtk-action
  (:superclass g-object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_action_get_type")
  ((action-group
    gtk-action-action-group
    "action-group" "GtkActionGroup" t t)
   (always-show-image
    gtk-action-always-show-image
    "always-show-image" "gboolean" t t)
   (gicon
    gtk-action-gicon
    "gicon" "GIcon" t t)
   (hide-if-empty
    gtk-action-hide-if-empty
    "hide-if-empty" "gboolean" t t)
   (icon-name
    gtk-action-icon-name
    "icon-name" "gchararray" t t)
   (is-important
    gtk-action-is-important
    "is-important" "gboolean" t t)
   (label
    gtk-action-label
    "label" "gchararray" t t)
   (name
    gtk-action-name
    "name" "gchararray" t nil)
   (sensitive
    gtk-action-sensitive
    "sensitive" "gboolean" t t)
   (short-label
    gtk-action-short-label
    "short-label" "gchararray" t t)
   (stock-id
    gtk-action-stock-id
    "stock-id" "gchararray" t t)
   (tooltip
    gtk-action-tooltip
    "tooltip" "gchararray" t t)
   (visible
    gtk-action-visible
    "visible" "gboolean" t t)
   (visible-horizontal
    gtk-action-visible-horizontal
    "visible-horizontal" "gboolean" t t)
   (visible-overflown
    gtk-action-visible-overflown
    "visible-overflown" "gboolean" t t)
   (visible-vertical
    gtk-action-visible-vertical
    "visible-vertical" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-action 'type)
 "@version{2013-6-2}
  @begin{short}
    Actions represent operations that the user can be perform, along with some
    information how it should be presented in the interface. Each action
    provides methods to create icons, menu items and toolbar items representing
    itself.
  @end{short}

  As well as the callback that is called when the action gets activated, the
  following also gets associated with the action:
  @begin{itemize}
    @item{a name (not translated, for path lookup)}
    @item{a label (translated, for display)}
    @item{an accelerator}
    @item{whether label indicates a stock ID}
    @item{a tooltip (optional, translated)}
    @item{a toolbar label (optional, shorter than label)}
  @end{itemize}
  The action will also have some state information:
  @begin{itemize}
    @item{visible (shown/hidden)}
    @item{sensitive (enabled/disabled)}
  @end{itemize}
  Apart from regular actions, there are toggle actions, which can be toggled
  between two states and radio actions, of which only one in a group can be in
  the \"active\" state. Other actions can be implemented as @sym{gtk-action}
  subclasses.

  Each action can have one or more proxy widgets. To act as an action proxy,
  widget needs to implement the @class{gtk-activatable} interface. Proxies
  mirror the state of the action and should change when the action's state
  changes. Properties that are always mirrored by proxies are @code{sensitive}
  and @code{visible}. @code{gicon}, @code{icon-name}, @code{label},
  @code{short-label} and @code{stock-id} properties are only mirorred if proxy
  widget has the @slot[gtk-acivatable]{use-action-appearance} property set to
  @arg{true}.

  When the proxy is activated, it should activate its action.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (action)   : No Recursion
      @end{pre}
      The \"activate\" signal is emitted when the action is activated.
      @begin[code]{table}
        @entry[action]{The @sym{gtk-action}.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-action-action-group}
  @see-slot{gtk-action-always-show-image}
  @see-slot{gtk-action-gicon}
  @see-slot{gtk-action-hide-if-empty}
  @see-slot{gtk-action-icon-name}
  @see-slot{gtk-action-is-important}
  @see-slot{gtk-action-label}
  @see-slot{gtk-action-name}
  @see-slot{gtk-action-sensitive}
  @see-slot{gtk-action-short-label}
  @see-slot{gtk-action-stock-id}
  @see-slot{gtk-action-tooltip}
  @see-slot{gtk-action-visible}
  @see-slot{gtk-action-visible-horizontal}
  @see-slot{gtk-action-visible-overflown}
  @see-slot{gtk-action-visible-vertical}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-action-action-group ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-group" 'gtk-action) 't)
 "The @code{action-group} property of type @class{gtk-action-group}
  (Read / Write) @br{}
  The @class{gtk-action-group} this @sym{gtk-action} is associated with, or
  @code{nil} (for internal use).")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-action-group 'function)
 "@version{2013-8-11}
  Accessor of the @slot[gtk-action]{action-group} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}")

;;; --- gtk-action-always-show-image -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-action) 't)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @arg{true}, the action's menu item proxies will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show their image, if
  available. Use this property if the menu item would be useless or hard to use
  without their image. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-always-show-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-always-show-image 'function)
 "@version{2013-8-11}
  Accessor of the @slot[gtk-action]{always-show-image} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-always-show-image}
  @see-function{gtk-action-set-always-show-image}")

;;; --- gtk-action-gicon -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-action) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  The @class{g-icon} displayed in the @sym{gtk-action}. Note that the stock icon
  is preferred, if the @code{stock-id} property holds the ID of an existing
  stock icon. This is an appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @arg{true}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-gicon 'function)
 "@version{2013-8-11}
  Accessor of the @slot[gtk-action]{gicon} slot of the @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-gicon}
  @see-function{gtk-action-set-gicon}")

;;; --- gtk-action-hide-if-empty -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hide-if-empty" 'gtk-action) 't)
 "The @code{hide-if-empty} property of type @code{:boolean}
  (Read / Write) @br{}
  When @arg{true}, empty menu proxies for this action are hidden. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-hide-if-empty atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-hide-if-empty 'function)
 "@version{2013-8-11}
  Accessor of the @slot[gtk-action]{hide-if-empty} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}")

;;; --- gtk-action-icon-name ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-action) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. Note that the stock icon is
  preferred, if the @code{stock-id} property holds the ID of an existing
  stock icon, and the @class{g-icon} is preferred if the @code{gicon}
  property is set. This is an appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @arg{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-icon-name 'function)
 "@version{2013-8-11}
  Accessor of the @slot[gtk-action]{icon-name} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-icon-name}
  @see-function{gtk-action-set-icon-name}")

;;; --- gtk-action-is-important ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-important" 'gtk-action) 't)
 "The @code{is-important} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the action is considered important. When @arg{true}, toolitem proxies
  for this action show text in @code{:both-horiz} mode. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-is-important atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-is-important 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{is-important} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-is-important}
  @see-function{gtk-action-set-is-important}")

;;; --- gtk-action-label -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-action) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label used for menu items and buttons that activate this action. If the
  label is @code{nil}, GTK+ uses the stock label specified via the
  @code{stock-id} property. This is an appearance property and thus only
  applies if the @slot[gtk-activatable]{use-action-appearance} is @arg{true}.
  @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-label 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{label} slot of the @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-label}
  @see-function{gtk-action-set-label}")

;;; --- gtk-action-name --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-action) 't)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct) @br{}
  A unique name for the action. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-name 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{name} slot of the @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-name}")

;;; --- gtk-action-sensitive ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sensitive" 'gtk-action) 't)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is enabled. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-sensitive 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{sensitive} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-sensitive}
  @see-function{gtk-action-set-sensitive}")

;;; --- gtk-action-short-label -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "short-label" 'gtk-action) 't)
 "The @code{short-label} property of type @code{:string}
  (Read / Write) @br{}
  A shorter label that may be used on toolbar buttons. This is an appearance
  property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @arg{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-short-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-short-label 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{short-label} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-short-label}
  @see-function{gtk-action-set-short-label}")

;;; --- gtk-action-stock-id ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-id" 'gtk-action) 't)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed in widgets representing this action. This is an
  appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} is @arg{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-stock-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-stock-id 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{stock-id} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-stock-id}
  @see-function{gtk-action-set-stock-id}")

;;; --- gtk-action-tooltip -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tooltip" 'gtk-action) 't)
 "The @code{tooltip} property of type @code{:string} (Read / Write) @br{}
  A tooltip for this action. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-tooltip atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-tooltip 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{tooltip} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-tooltip}
  @see-function{gtk-action-set-tooltip}")

;;; --- gtk-action-visible -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-action) 't)
 "The @code{visible} property of type  @code{:boolean} (Read / Write) @br{}
  Whether the action is visible. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{visible} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible}
  @see-function{gtk-action-set-visible}")

;;; --- gtk-action-visible-horizontal ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-horizontal"
                                               'gtk-action) 't)
 "The @code{visible-horizontal} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-horizontal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-horizontal 'function)
 "@version{2013-12-10}
    Accessor of the @slot[gtk-action]{visible-horizontal} slot of the
    @class{gtk-action} class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible-horizontal}
  @see-function{gtk-action-set-visible-horizontal}")

;;; --- gtk-action-visible-overflow --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-overflown"
                                               'gtk-action) 't)
 "The @code{visible-overflown} property of type @code{:boolean}
  (Read / Write) @br{}
  When @arg{true}, toolitem proxies for this action are represented in the
  toolbar overflow menu. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-overflown atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-overflown 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{visible-overflown} slot of the
  @class{gtk-action} class.
  @see-class{gtk-action}")

;;; --- gtk-action-visible-vertical --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-vertical"
                                               'gtk-action) 't)
 "The @code{visible-vertical} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: @arg{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-vertical atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-vertical 'function)
 "@version{2013-12-10}
  Accessor of the @slot[gtk-action]{visible-vertical} of the @class{gtk-action}
  class.
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible-vertical}
  @see-function{gtk-action-set-visible-vertical}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-new))

(defun gtk-action-new (name &optional (label nil) (tooltip nil) (stock-id nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[name]{a unique name for the action}
  @argument[label]{the label displayed in menu items and on buttons,
    or @code{nil}}
  @argument[tooltip]{a tooltip for the action, or @code{nil}}
  @argument[stock-id]{the stock icon to display in widgets representing the
    action, or @code{nil}}
  @return{A new @class{gtk-action} object.}
  @begin{short}
    Creates a new @class{gtk-action} object.
  @end{short}
  To add the action to a @class{gtk-action-group} and set the accelerator for
  the action, call the @fun{gtk-action-group-add-action} function.
  See the section called \"UI Definitions\" for information on allowed action
  names.
  @see-class{gtk-action}
  @see-class{gtk-action-group}
  @see-function{gtk-action-group-add-action}"
  (make-instance 'gtk-action
                 :name name
                 :label (if label label (null-pointer))
                 :tooltip (if tooltip tooltip (null-pointer))
                 :stock-id (if stock-id stock-id (null-pointer))))

(export 'gtk-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-name))

(defun gtk-action-get-name (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[action]{the action object}
  @return{The name of the action.}
  @short{Returns the name of the action.}
  @see-class{gtk-action}"
  (gtk-action-name action))

(export 'gtk-action-get-name)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_sensitive" gtk-action-is-sensitive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[action]{the action object}
  @return{@arg{True} if the action and its associated action group are both
    sensitive.}
  @short{Returns whether the action is effectively sensitive.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-sensitive}
  @see-function{gtk-action-set-sensitive}"
  (action (g-object gtk-action)))

(export 'gtk-action-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_sensitive ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-sensitive))

(defun gtk-action-get-sensitive (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[action]{the action object}
  @return{@em{True} if the @arg{action} itself is sensitive.}
  @begin{short}
    Returns whether the action itself is sensitive. Note that this does
    not necessarily mean effective sensitivity. See the function
    @fun{gtk-action-is-sensitive} for that.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-is-sensitive}
  @see-function{gtk-action-set-sensitive}"
  (gtk-action-sensitive action))

(export 'gtk-action-get-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_sensitive ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-sensitive))

(defun gtk-action-set-sensitive (action sensitive)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[action]{the action object}
  @argument[sensitive]{@em{true} to make the action sensitive}
  @begin{short}
    Sets the @code{sensitive} property of the action to sensitive.
  @end{short}
  Note that this does not necessarily mean effective sensitivity. See the
  @fun{gtk-action-is-sensitive} function for that.
  @see-class{gtk-action}
  @see-function{gtk-action-is-sensitive}
  @see-function{gtk-action-get-sensitive}"
  (setf (gtk-action-sensitive action) sensitive))

(export 'gtk-action-set-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_visible" gtk-action-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @return{@arg{True} if the action and its associated action group are
    both visible.}
  @short{Returns whether the action is effectively visible.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible}
  @see-function{gtk-action-set-visible}"
  (action (g-object gtk-action)))

(export 'gtk-action-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-visible))

(defun gtk-action-get-visible (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @return{@em{True} if the action itself is visible.}
  @begin{short}
    Returns whether the action itself is visible.
  @end{short}
  Note that this does not necessarily mean effective visibility.
  See the @fun{gtk-action-is-sensitive} function for that.
  @see-class{gtk-action}
  @see-function{gtk-action-set-visible}
  @see-function{gtk-action-is-sensitive}"
  (gtk-action-visible action))

(export 'gtk-action-get-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-visible))

(defun gtk-action-set-visible (action visible)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @argument[visible]{@em{true} to make the action visible}
  @begin{short}
    Sets the @code{visible} property of the action to visible.
  @end{short}
  Note that this does not necessarily mean effective visibility.
  See the @fun{gtk-action-is-visible} function for that.
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible}
  @see-function{gtk-action-is-visible}"
  (setf (gtk-action-visible action) visible))

(export 'gtk-action-set-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_activate" gtk-action-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[action]{the action object}
  @begin{short}
    Emits the \"activate\" signal on the specified action, if it is not
    insensitive. This gets called by the proxy widgets when they get activated.
  @end{short}

  It can also be used to manually activate an action.
  @see-class{gtk-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_icon" gtk-action-create-icon) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-10}
  @argument[action]{the action object}
  @argument[icon-size]{the size of type @symbol{gtk-icon-size} of the icon that
    should be created}
  @return{A widget that displays the icon for this action.}
  @begin{short}
    This function is intended for use by action implementations to create icons
    displayed in the proxy widgets.
  @end{short}
  @see-class{gtk-action}
  @see-class{gtk-widget}
  @see-symbol{gtk-icon-size}"
  (action (g-object gtk-action))
  (icon-size gtk-icon-size))

(export 'gtk-action-create-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu_item" gtk-action-create-menu-item)
    (g-object gtk-image-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{the action object}
  @return{A menu item connected to the action.}
  @short{Creates a menu item widget that proxies for the given action.}
  @see-class{gtk-action}
  @see-class{gtk-image-menu-item}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_tool_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_tool_item" gtk-action-create-tool-item)
    (g-object gtk-tool-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{the action object}
  @return{A toolbar item connected to the action.}
  @short{Creates a toolbar item widget that proxies for the given action.}
  @see-class{gtk-action}
  @see-class{gtk-tool-button}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-tool-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu" gtk-action-create-menu) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{a @class{gtk-action} object}
  @return{The menu item provided by the action, or @code{NULL}.}
  @begin{short}
    If action provides a @class{gtk-menu} widget as a submenu for the menu item
    or the toolbar item it creates, this function returns an instance of that
    menu.
  @end{short}
  @see-class{gtk-action}
  @see-class{gtk-menu}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_proxies ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_proxies" gtk-action-get-proxies)
    (g-slist g-object :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{the action object}
  @return{A list of proxy widgets.}
  @short{Returns the proxy widgets for an action.}
  See also the function @fun{gtk-activatable-get-related-action}.
  @see-class{gtk-action}
  @see-function{gtk-activatable-get-related-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-get-proxies)

;;; ----------------------------------------------------------------------------
;;; gtk_action_connect_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_connect_accelerator" gtk-action-connect-accelerator) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{a @class{gtk-action} object}
  @begin{short}
    Installs the accelerator for action if action has an accel path and group.
  @end{short}
  See the functions @fun{gtk-action-set-accel-path} and
  @fun{gtk-action-set-accel-group}.

  Since multiple proxies may independently trigger the installation of the
  accelerator, the action counts the number of times this function has been
  called and does not remove the accelerator until the function
  @fun{gtk-action-disconnect-accelerator} has been called as many times.
  @see-class{gtk-action}
  @see-function{gtk-action-set-accel-path}
  @see-function{gtk-action-set-accel-group}
  @see-function{gtk-action-disconnect-accelerator}"
  (action (g-object gtk-action)))

(export 'gtk-action-connect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_disconnect_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_disconnect_accelerator" gtk-action-disconnect-accelerator)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{a @class{gtk-action} object}
  @begin{short}
    Undoes the effect of one call to the function
    @fun{gtk-action-connect-accelerator}.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-connect-accelerator}"
  (action (g-object gtk-action)))

(export 'gtk-action-disconnect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_block_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_block_activate" gtk-action-block-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{a @class{gtk-action} object}
  @short{Disable activation signals from the action.}

  This is needed when updating the state of your proxy @class{gtk-activatable}
  widget could result in calling the function @fun{gtk-action-activate}, this
  is a convenience function to avoid recursing in those cases (updating toggle
  state for instance).
  @see-class{gtk-action}
  @see-function{gtk-action-activate}"
  (action (g-object gtk-action)))

(export 'gtk-action-block-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_unblock_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_unblock_activate" gtk-action-unblock-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{a @class{gtk-action} object}
  @short{Reenable activation signals from the action.}
  @see-class{gtk-action}
  @see-function{gtk-action-block-activate}"
  (action (g-object gtk-action)))

(export 'gtk-action-unblock-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_always_show_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-always-show-image))

(defun gtk-action-get-always-show-image (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[action]{a @class{gtk-action} object}
  @return{@em{True} if the menu item proxies will always show their image.}
  @begin{short}
    Returns whether action's menu item proxies will ignore the
    @slot[gtk-settings]{gtk-menu-images} setting and always show their image,
    if available.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-set-always-show-image}"
  (gtk-action-always-show-image action))

(export 'gtk-action-get-always-show-image)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_always_show_image ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-always-show-image))

(defun gtk-action-set-always-show-image (action always-show)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[action]{a @class{gtk-action} object}
  @argument[always-show]{@em{true} if menu item proxies should always show their
    image}
  @begin{short}
    Sets whether @arg{action}'s menu item proxies will ignore the
    @slot[gtk-settings]{gtk-menu-images} setting and always show their image,
    if available.
  @end{short}

  Use this if the menu item would be useless or hard to use without their
  image.
  @see-class{gtk-action}
  @see-function{gtk-action-get-always-show-image}"
  (setf (gtk-action-always-show-image action) always-show))

(export 'gtk-action-set-always-show-image)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_accel_path" gtk-action-get-accel-path) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @return{The accel path for this action, or @code{nil} if none is set.}
  @short{Returns the accel path for this action.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-accel-path}"
  (action (g-object gtk-action)))

(export 'gtk-action-get-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_set_accel_path" gtk-action-set-accel-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{the action object}
  @argument[accel-path]{the accelerator path}
  @begin{short}
    Sets the accel path for this action.
  @end{short}
  All proxy widgets associated with the action will have this accel path, so
  that their accelerators are consistent.
  @see-class{gtk-action}
  @see-function{gtk-action-get-accel-path}"
  (action (g-object gtk-action))
  (accel-path :string))

(export 'gtk-action-set-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_closure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_accel_closure" gtk-action-get-accel-closure)
    (:pointer (:struct g-closure))
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @begin{return}
    The accel closure for this action. The returned closure is owned by GTK+
    and must not be unreffed or modified.
  @end{return}
  @short{Returns the accel closure for this action.}
  @see-class{gtk-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-get-accel-closure)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_set_accel_group" gtk-action-set-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-12-11}
  @argument[action]{the action object}
  @argument[accel-group]{a @class{gtk-accel-group} or @code{nil}}
  @begin{short}
    Sets the @class{gtk-accel-group} in which the accelerator for this action
    will be installed.
  @end{short}
  @see-class{gtk-action}
  @see-class{gtk-accel-group}"
  (action (g-object gtk-action))
  (accel-group (g-object gtk-accel-group)))

(export 'gtk-action-set-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-label))

(defun gtk-action-set-label (action label)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[label]{the label text to set}
  @short{Sets the label of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-label}"
  (setf (gtk-action-label action) label))

(export 'gtk-action-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-label))

(defun gtk-action-get-label (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{The label text.}
  @short{Gets the label text of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-label}"
  (gtk-action-label action))

(export 'gtk-action-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_short_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-short-label))

(defun gtk-action-set-short-label (action short-label)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[short-label]{the label text to set}
  @short{Sets a shorter label text on @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-short-label}"
  (setf (gtk-action-short-label action) short-label))

(export 'gtk-action-set-short-label)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_short_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-short-label))

(defun gtk-action-get-short-label (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{The short label text.}
  @short{Gets the short label text of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-short-label}"
  (gtk-action-short-label action))

(export 'gtk-action-get-short-label)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_tooltip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-tooltip))

(defun gtk-action-set-tooltip (action tooltip)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[tooltip]{the tooltip text}
  @short{Sets the tooltip text on @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-tooltip}"
  (setf (gtk-action-tooltip action) tooltip))

(export 'gtk-action-set-tooltip)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_tooltip ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-tooltip))

(defun gtk-action-get-tooltip (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{The tooltip text.}
  @short{Gets the tooltip text of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-tooltip}"
  (gtk-action-tooltip action))

(export 'gtk-action-get-tooltip)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_stock_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-stock-id))

(defun gtk-action-set-stock-id (action stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[stock-id]{the stock ID}
  @short{Sets the stock ID on @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-stock-id}"
  (setf (gtk-action-stock-id action) stock-id))

(export 'gtk-action-set-stock-id)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_stock_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-stock-id))

(defun gtk-action-get-stock-id (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{The stock ID.}
  @short{Gets the stock ID of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-stock-id}"
  (gtk-action-stock-id action))

(export 'gtk-action-get-stock-id)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-gicon))

(defun gtk-action-set-gicon (action icon)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[action]{a @class{gtk-action} object}
  @argument[icon]{the @class{g-icon} to set}
  @begin{short}
    Sets the icon of action.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-get-gicon}"
  (setf (gtk-action-gicon action) icon))

(export 'gtk-action-set-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-gicon))

(defun gtk-action-get-gicon (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-11}
  @argument[action]{a @class{gtk-action} object}
  @return{The @arg{action}'s @class{g-icon} if one is set.}
  @begin{short}
    Gets the @class{g-icon} of @arg{action}.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-set-gicon}"
  (gtk-action-gicon action))

(export 'gtk-action-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-icon-name))

(defun gtk-action-set-icon-name (action icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[icon-name]{the icon name to set}
  @short{Sets the icon name on @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-icon-name}"
  (setf (gtk-action-icon-name action) icon-name))

(export 'gtk-action-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-icon-name))

(defun gtk-action-get-icon-name (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{The icon name.}
  @short{Gets the icon name of @arg{action}.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-icon-name}"
  (gtk-action-icon-name action))

(export 'gtk-action-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible_horizontal ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-visible-horizontal))

(defun gtk-action-set-visible-horizontal (action visible-horizontal)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[visible-horizontal]{whether the action is visible horizontally}
  @short{Sets whether @arg{action} is visible when horizontal.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible-horizontal}"
  (setf (gtk-action-visible-horizontal action) visible-horizontal))

(export 'gtk-action-set-visible-horizontal)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible_horizontal ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-visible-horizontal))

(defun gtk-action-get-visible-horizontal (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{Whether @arg{action} is visible when horizontal.}
  @short{Checks whether @arg{action} is visible when horizontal.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-visible-horizontal}"
  (gtk-action-visible-horizontal action))

(export 'gtk-action-get-visible-horizontal)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_visible_vertical ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-visible-vertical))

(defun gtk-action-set-visible-vertical (action visible-vertical)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @argument[visible-vertical]{whether the action is visible vertically}
  @short{Sets whether @arg{action} is visible when vertical.}
  @see-class{gtk-action}
  @see-function{gtk-action-get-visible-vertical}"
  (setf (gtk-action-visible-vertical action) visible-vertical))

(export 'gtk-action-set-visible-vertical)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_visible_vertical ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-visible-vertical))

(defun gtk-action-get-visible-vertical (action)
 #+cl-cffi-gtk-documentation
 "@version{2014-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{Whether @arg{action} is visible when horizontal.}
  @short{Checks whether @arg{action} is visible when horizontal.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-visible-vertical}"
  (gtk-action-visible-vertical action))

(export 'gtk-action-get-visible-vertical)

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_is_important ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-set-is-important))

(defun gtk-action-set-is-important (action is-important)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{the action object}
  @argument[is-important]{@em{true} to make the action important}
  @begin{short}
    Sets whether the action is important, this attribute is used primarily by
    toolbar items to decide whether to show a label or not.
  @end{short}
  @see-class{gtk-action}
  @see-function{gtk-action-get-is-important}"
  (setf (gtk-action-is-important action) is-important))

(export 'gtk-action-set-is-important)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_is_important ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-get-is-important))

(defun gtk-action-get-is-important (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[action]{a @class{gtk-action} object}
  @return{Whether @arg{action} is important.}
  @short{Checks whether @arg{action} is important or not.}
  @see-class{gtk-action}
  @see-function{gtk-action-set-is-important}"
  (gtk-action-is-important action))

(export 'gtk-action-get-is-important)

;;; --- End of file gtk.action.lisp --------------------------------------------
