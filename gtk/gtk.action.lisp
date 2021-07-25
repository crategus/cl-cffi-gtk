;;; ----------------------------------------------------------------------------
;;; gtk.action.lisp
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
;;; GtkAction
;;;
;;;     A deprecated action which can be triggered by a menu or toolbar item.
;;;
;;; Types and Values
;;;
;;;     GtkAction
;;;
;;; Functions
;;;
;;;     gtk_action_new
;;;     gtk_action_get_name                                Accessor
;;;     gtk_action_is_sensitive
;;;     gtk_action_get_sensitive                           Accessor
;;;     gtk_action_set_sensitive                           Accessor
;;;     gtk_action_is_visible
;;;     gtk_action_get_visible                             Accessor
;;;     gtk_action_set_visible                             Accessor
;;;     gtk_action_activate
;;;     gtk_action_create_icon
;;;     gtk_action_create_menu_item
;;;     gtk_action_create_tool_item
;;;     gtk_action_create_menu
;;;     gtk_action_get_proxies
;;;     gtk_action_connect_accelerator
;;;     gtk_action_disconnect_accelerator
;;;     gtk_action_block_activate
;;;     gtk_action_unblock_activate
;;;     gtk_action_get_always_show_image                   Accessor
;;;     gtk_action_set_always_show_image                   Accessor
;;;     gtk_action_get_accel_path
;;;     gtk_action_set_accel_path
;;;     gtk_action_get_accel_closure
;;;     gtk_action_set_accel_group
;;;     gtk_action_set_label                               Accessor
;;;     gtk_action_get_label                               Accessor
;;;     gtk_action_set_short_label                         Accessor
;;;     gtk_action_get_short_label                         Accessor
;;;     gtk_action_set_tooltip                             Accessor
;;;     gtk_action_get_tooltip                             Accessor
;;;     gtk_action_set_stock_id                            Accessor
;;;     gtk_action_get_stock_id                            Accessor
;;;     gtk_action_set_gicon                               Accessor
;;;     gtk_action_get_gicon                               Accessor
;;;     gtk_action_set_icon_name                           Accessor
;;;     gtk_action_get_icon_name                           Accessor
;;;     gtk_action_set_visible_horizontal                  Accessor
;;;     gtk_action_get_visible_horizontal                  Accessor
;;;     gtk_action_set_visible_vertical                    Accessor
;;;     gtk_action_get_visible_vertical                    Accessor
;;;     gtk_action_set_is_important                        Accessor
;;;     gtk_action_get_is_important                        Accessor
;;;
;;; Properties
;;;
;;;     GtkActionGroup*   action-group          Read / Write
;;;           gboolean    always-show-image     Read / Write / Construct
;;;              GIcon*   gicon                 Read / Write
;;;           gboolean    hide-if-empty         Read / Write
;;;              gchar*   icon-name             Read / Write
;;;           gboolean    is-important          Read / Write
;;;              gchar*   label                 Read / Write
;;;              gchar*   name                  Read / Write / Construct
;;;           gboolean    sensitive             Read / Write
;;;              gchar*   short-label           Read / Write
;;;              gchar*   stock-id              Read / Write
;;;              gchar*   tooltip               Read / Write
;;;           gboolean    visible               Read / Write
;;;           gboolean    visible-horizontal    Read / Write
;;;           gboolean    visible-overflown     Read / Write
;;;           gboolean    visible-vertical      Read / Write
;;;
;;; Signals
;;;
;;;               void    activate              No Recursion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ├── GtkToggleAction
;;;         ╰── GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAction implements GtkBuildable.
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
 "@version{*2021-7-24}
  @begin{short}
    Actions represent operations that the user can be perform, along with some
    information how it should be presented in the interface.
  @end{short}
  Each action provides methods to create icons, menu items and toolbar items
  representing itself.

  As well as the callback function that is called when the action gets
  activated, the following also gets associated with the action:
  @begin{itemize}
    @item{a name, not translated, for path lookup}
    @item{a label, translated, for display}
    @item{an accelerator}
    @item{whether label indicates a stock ID}
    @item{a tooltip, optional, translated}
    @item{a toolbar label, optional, shorter than label}
  @end{itemize}
  The action will also have some state information:
  @begin{itemize}
    @item{visible, shown/hidden}
    @item{sensitive, enabled/disabled}
  @end{itemize}
  Apart from regular actions, there are toggle actions, which can be toggled
  between two states and radio actions, of which only one in a group can be in
  the \"active\" state. Other actions can be implemented as @sym{gtk-action}
  subclasses.

  Each action can have one or more proxy widgets. To act as an action proxy,
  the widget needs to implement the @class{gtk-activatable} interface. Proxies
  mirror the state of the action and should change when the state of the action
  changes. Properties that are always mirrored by proxies are @code{sensitive}
  and @code{visible}. The @code{gicon}, @code{icon-name}, @code{label},
  @code{short-label} and @code{stock-id} properties are only mirorred if the
  proxy widget has the @slot[gtk-activatable]{use-action-appearance} property
  set to @em{true}.

  When the proxy is activated, it should activate its action.
  @begin[Warning]{dictionary}
    The @sym{gtk-action} class has been deprecated since GTK 3.10. Use
    the @class{g-action} interface instead, and associate actions with
    @class{gtk-actionable} widgets. Use the @class{g-menu-model} class for
    creating menus with the function @fun{gtk-menu-new-from-model}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (action)    :no-recurse
      @end{pre}
      The signal is emitted when the action is activated.
      @begin[code]{table}
        @entry[action]{The @sym{gtk-action} object which received the signal.}
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
  @see-slot{gtk-action-visible-vertical}
  @see-class{gtk-action-group}
  @see-class{gtk-activatable}
  @see-class{g-action}
  @see-class{gtk-actionable}
  @see-class{g-menu-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-action-action-group ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-group" 'gtk-action) 't)
 "The @code{action-group} property of type @class{gtk-action-group}
  (Read / Write) @br{}
  The action group the action is associated with, or @code{nil} for internal
  use.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-action-group 'function)
 "@version{2021-7-20}
  @syntax[]{gtk-action-action-group object) => group}
  @syntax[]{(setf (gtk-action-action-group object) group)}
  @argument[object]{a @class{gtk-action} object}
  @argument[group]{a @class{gtk-action-group} object}
  @begin{short}
    Accessor of the @slot[gtk-action]{action-group} slot of the
    @class{gtk-action} class.
  @end{short}

  The action group the action is associated with, or @code{nil} for internal
  use.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-action-group} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-action-group}")

;;; --- gtk-action-always-show-image -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-action) 't)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item proxies of the action will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show their image, if
  available. Use this property if the menu item would be useless or hard to use
  without their image. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-always-show-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-always-show-image 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-always-show-image object) => always-show}
  @syntax[]{(setf (gtk-action-always-show-image object) always-show)}
  @argument[object]{a @class{gtk-action} object}
  @argument[always-show]{@em{true} if menu item proxies should always show
    their image}
  @begin{short}
    Accessor of the @slot[gtk-action]{always-show-image} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-always-show-image} returns whether
  the action menu item proxies will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show their image,
  if available. The slot access function
  @sym{(setf gtk-action-always-show-image)} sets the property.

  Use this if the menu item would be useless or hard to use without their
  image.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-always-show-image} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-settings-gtk-menu-images}")

;;; --- gtk-action-gicon -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-action) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  The icon displayed in the action. Note that the stock icon is preferred, if
  the @code{stock-id} property holds the ID of an existing stock icon. This is
  an appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-gicon 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-gicon object) => icon}
  @syntax[]{(setf (gtk-action-gicon object) icon)}
  @argument[object]{a @class{gtk-action} object}
  @argument[icon]{a @class{g-icon} object}
  @begin{short}
    Accessor of the @slot[gtk-action]{gicon} slot of the @class{gtk-action}
    class.
  @end{short}

  The slot access function @sym{gtk-action-gicon} gets the icon of the action.
  The slot access function @sym{(setf gtk-action-gicon)} sets the icon.

  The icon displayed in the action. Note that the stock icon is preferred, if
  the @slot[gtk-action]{stock-id} property holds the ID of an existing stock
  icon. This is an appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-gicon} has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{g-icon}
  @see-function{gtk-action-stock-id}
  @see-function{gtk-activatable-use-action-appearance}")

;;; --- gtk-action-hide-if-empty -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hide-if-empty" 'gtk-action) 't)
 "The @code{hide-if-empty} property of type @code{:boolean} (Read / Write) @br{}
  When @em{true}, empty menu proxies for this action are hidden. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-hide-if-empty atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-hide-if-empty 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-hide-if-empty object) => hide-if-empty}
  @syntax[]{(setf (gtk-action-hide-if-empty object) hide-if-empty)}
  @argument[object]{a @class{gtk-action} object}
  @argument[hide-if-empty]{a boolean whether empty menu proxies are hidden}
  @begin{short}
    Accessor of the @slot[gtk-action]{hide-if-empty} slot of the
    @class{gtk-action} class.
  @end{short}

  When @em{true}, empty menu proxies for this action are hidden.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-hide-if-empty} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; --- gtk-action-icon-name ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-action) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. Note that the stock icon is
  preferred, if the @code{stock-id} property holds the ID of an existing stock
  icon, and the @class{g-icon} object is preferred if the @code{gicon} property
  is set. This is an appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-icon-name 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-icon-name object) => name}
  @syntax[]{(setf (gtk-action-icon-name object) name)}
  @argument[object]{a @class{gtk-action} object}
  @argument[name]{a string with the icon name to set}
  @begin{short}
    Accessor of the @slot[gtk-action]{icon-name} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-icon-name} gets the icon name of the
  action. The slot access function @sym{(setf gtk-action-icon-name)} sets the
  icon name.

  The name of the icon from the icon theme. Note that the stock icon is
  preferred, if the @slot[gtk-action]{stock-id} property holds the ID of an
  existing stock icon, and the @class{g-icon} object is preferred if the
  @slot[gtk-action]{gicon} property is set. This is an appearance property and
  thus only applies if the @slot[gtk-activatable]{use-action-appearance}
  property is @em{true}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-icon-name} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-action-gicon}
  @see-function{gtk-action-stock-id}
  @see-function{gtk-activatable-use-action-appearance}")

;;; --- gtk-action-is-important ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-important" 'gtk-action) 't)
 "The @code{is-important} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is considered important. When @em{true}, toolitem proxies
  for this action show text in @code{:both-horiz} mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-is-important atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-is-important 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-is-important object) => is-important}
  @syntax[]{(setf (gtk-action-is-important object) is-important)}
  @argument[object]{a @class{gtk-action} object}
  @argument[is-important]{@em{true} to make the action important}
  @begin{short}
    Accessor of the @slot[gtk-action]{is-important} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-is-important} checks whether the
  action is important or not. The slot access function
  @sym{(setf gtk-action-is-important)} sets whether the action is important.
  This attribute is used primarily by toolbar items to decide whether to show a
  label or not. When @em{true}, toolitem proxies for this action show text in
  @code{:both-horiz} mode.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-is-important} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-symbol{gtk-toolbar-style}")

;;; --- gtk-action-label -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-action) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label used for menu items and buttons that activate this action. If the
  label is @code{nil}, GTK uses the stock label specified via the
  @code{stock-id} property. This is an appearance property and thus
  only applies if the @slot[gtk-activatable]{use-action-appearance} property is
  @em{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-label 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-label object) => label}
  @syntax[]{(setf (gtk-action-label object) label)}
  @argument[object]{a @class{gtk-action} object}
  @argument[label]{a string with the label text to set}
  @begin{short}
    Accessor of the @slot[gtk-action]{label} slot of the @class{gtk-action}
    class.
  @end{short}

  The slot access function @sym{gtk-action-label} gets the label text of the
  action. The slot access function @sym{(setf gtk-action-label)} sets the label.

  The label used for menu items and buttons that activate this action. If the
  label is @code{nil}, GTK uses the stock label specified via the
  @slot[gtk-action]{stock-id} property. This is an appearance property and thus
  only applies if the @slot[gtk-activatable]{use-action-appearance} property is
  @em{true}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-label} has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-action-stock-id}
  @see-function{gtk-activatable-use-action-appearance}")

;;; --- gtk-action-name --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-action) 't)
 "The @code{name} property of type @code{:string} (Read / Write / Construct)
  @br{}
  A unique name for the action. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-name 'function)
 "@version{*2021-7-24}
  @syntax[]{(gtk-action-name object) => name}
  @argument[object]{a @class{gtk-action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Accessor of the @slot[gtk-action]{name} slot of the @class{gtk-action}
    class.
  @end{short}

  The slot access function @sym{gtk-action-label} returns the unique name of
  the action.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-name} has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; --- gtk-action-sensitive ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "sensitive" 'gtk-action) 't)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is enabled. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-sensitive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-sensitive 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-sensitive object) => sensitive}
  @syntax[]{(setf (gtk-action-sensitive object) sensitive)}
  @argument[object]{a @class{gtk-action} object}
  @argument[sensitive]{@em{true} to make the action sensitive}
  @begin{short}
    Accessor of the @slot[gtk-action]{sensitive} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-label} returns whether the action
  itself is sensitive. The slot access function @sym{(setf gtk-action-label)}
  sets the sensitivity.

  Note that this does not necessarily mean effective sensitivity. See the
  function @fun{gtk-action-is-sensitive} for that.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-sensitive} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-action-is-sensitive}")

;;; --- gtk-action-short-label -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "short-label" 'gtk-action) 't)
 "The @code{short-label} property of type @code{:string} (Read / Write) @br{}
  A shorter label that may be used on toolbar buttons. This is an appearance
  property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-short-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-short-label 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-short-label object) => short-label}
  @syntax[]{(setf (gtk-action-short-label object) short-label)}
  @argument[object]{a @class{gtk-action} object}
  @argument[short-label]{a string with the label text to set}
  @begin{short}
    Accessor of the @slot[gtk-action]{short-label} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-short-label} gets the short label
  text of the action. The slot access function
  @sym{(setf gtk-action-short-label)} sets a shorter label text.

  A shorter label that may be used on toolbar buttons. This is an appearance
  property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-short-label} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-activatable-use-action-appearance}")

;;; --- gtk-action-stock-id ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-id" 'gtk-action) 't)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed in widgets representing this action. This is an
  appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-stock-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-stock-id 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-stock-id object) => stock-id}
  @syntax[]{(setf (gtk-action-stock-id object) stock-id)}
  @argument[object]{a @class{gtk-action} object}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk-action]{stock-id} slot of the @class{gtk-action}
    class.
  @end{short}

  The slot access function @sym{gtk-action-stock-id} gets the stock ID of the
  action. The slot access function @sym{(setf gtk-action-stock-id)} sets the
  stock ID.

  The stock icon displayed in widgets representing the action. This is an
  appearance property and thus only applies if the
  @slot[gtk-activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-stock-id} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-activatable-use-action-appearance}")

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
 "@version{2021-7-20}
  @syntax[]{(gtk-action-tooltip object) => tooltip}
  @syntax[]{(setf (gtk-action-tooltip object) tooltip)}
  @argument[object]{a @class{gtk-action} object}
  @argument[tooltip]{a string with the tooltip text}
  @begin{short}
    Accessor of the @slot[gtk-action]{tooltip} slot of the @class{gtk-action}
    class.
  @end{short}

  The slot access function @sym{gtk-action-tooltip} gets the tooltip text of
  the action. The slot access function @sym{(setf gtk-action-tooltip)} sets the
  tooltip text.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-tooltip} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; --- gtk-action-visible -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible" 'gtk-action) 't)
 "The @code{visible} property of type  @code{:boolean} (Read / Write) @br{}
  Whether the action is visible. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-visible object) => visible}
  @syntax[]{(setf (gtk-action-visible object) visible)}
  @argument[object]{a @class{gtk-action} object}
  @argument[visible]{@em{true} to make the action visible}
  @begin{short}
    Accessor of the @slot[gtk-action]{visible} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-visible} returns whether the action
  itself is visible. The slot access function @sym{(setf gtk-action-visible)}
  sets the visibility.

  Note that this does not necessarily mean effective visibility. See the
  function @fun{gtk-action-is-visible} for that.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-visible} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-function{gtk-action-is-visible}")

;;; --- gtk-action-visible-horizontal ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-horizontal"
                                               'gtk-action) 't)
 "The @code{visible-horizontal} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-horizontal atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-horizontal 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-visible-horizontal object) => visible}
  @syntax[]{(setf (gtk-action-visible-horizontal object) visible)}
  @argument[object]{a @class{gtk-action} object}
  @argument[visible]{@em{true} to make the action visible horizontally}
  @begin{short}
    Accessor of the @slot[gtk-action]{visible-horizontal} slot of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-visible-horizontal} checks whether
  the action is visible when horizontal. The slot access function
  @sym{(setf gtk-action-visible-horizontal)} sets the visibility.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-visible-horizontal} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; --- gtk-action-visible-overflow --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-overflown"
                                               'gtk-action) 't)
 "The @code{visible-overflown} property of type @code{:boolean} (Read / Write)
  @br{}
  When @em{true}, toolitem proxies for this action are represented in the
  toolbar overflow menu. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-overflown atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-overflown 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-visible-overflow object) => visible}
  @syntax[]{(setf (gtk-action-visible-overflow object) visible)}
  @argument[object]{a @class{gtk-action} object}
  @argument[visible]{a boolean whether a toolbar overflow menu is shown}
  @begin{short}
    Accessor of the @slot[gtk-action]{visible-overflown} slot of the
    @class{gtk-action} class.
  @end{short}

  When @em{true}, toolitem proxies for this action are represented in the
  toolbar overflow menu.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-visible-overflown} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; --- gtk-action-visible-vertical --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-vertical"
                                               'gtk-action) 't)
 "The @code{visible-vertical} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-visible-vertical atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-visible-vertical 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-action-visible-vertical object) => visible}
  @syntax[]{(setf (gtk-action-visible-vertical object) visible)}
  @argument[object]{a @class{gtk-action} object}
  @argument[visible]{@em{true} to make the action visible vertically}
  @begin{short}
    Accessor of the @slot[gtk-action]{visible-vertical} of the
    @class{gtk-action} class.
  @end{short}

  The slot access function @sym{gtk-action-visible-vertical} checks whether
  the action is visible when vertical. The slot access function
  @sym{(setf gtk-action-visible-vertical)} sets the visibility.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-visible-vertical} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-new))

(defun gtk-action-new (name &optional (label nil) (tooltip nil) (stock-id nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[name]{a string with a unique name for the action}
  @argument[label]{a string with the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string with a tooltip for the action, or @code{nil}}
  @argument[stock-id]{a string with the stock icon to display in widgets
    representing the action, or @code{nil}}
  @return{A new @class{gtk-action} object.}
  @begin{short}
    Creates a new action.
  @end{short}
  To add the action to a @class{gtk-action-group} object and set the accelerator
  for the action, call the function @fun{gtk-action-group-add-action}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-new} has been deprecated since version 3.10
    and should not be used in newly written code. Use the @class{g-action}
    interface instead, associating it to a widget with the
    @class{gtk-actionable} interface or creating a @class{gtk-menu} widget with
    the function @fun{gtk-menu-new-from-model}.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-action-group}
  @see-class{gtk-menu}
  @see-class{g-action}
  @see-class{g-menu-model}
  @see-function{gtk-action-group-add-action}
  @see-function{gtk-actionable}
  @see-function{gtk-menu-new-from-model}
  @see-function{g-action-enabled}"
  (make-instance 'gtk-action
                 :name name
                 :label (if label label (null-pointer))
                 :tooltip (if tooltip tooltip (null-pointer))
                 :stock-id (if stock-id stock-id (null-pointer))))

(export 'gtk-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_sensitive" gtk-action-is-sensitive) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{@em{True} if the action and its associated action group are both
    sensitive.}
  @short{Returns whether the action is effectively sensitive.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-is-sensitive} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the function
    @fun{g-action-enabled} on a @class{g-action} object instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{g-action}
  @see-function{gtk-action-sensitive}
  @see-function{g-action-enabled}"
  (action (g-object gtk-action)))

(export 'gtk-action-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_is_visible" gtk-action-is-visible) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{@em{True} if the action and its associated action group are
    both visible.}
  @short{Returns whether the action is effectively visible.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-is-visible} has been deprecated since version
    3.10 and should not be used in newly written code. Use the @class{g-action}
    interface instead, and control and monitor the state of
    @class{gtk-actionable} widgets directly.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{g-action}
  @see-class{gtk-actionable}"
  (action (g-object gtk-action)))

(export 'gtk-action-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_activate" gtk-action-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @begin{short}
    Emits the \"activate\" signal on the specified action, if it is not
    insensitive.
  @end{short}
  This gets called by the proxy widgets when they get activated. It can also be
  used to manually activate an action.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-activate} has been deprecated since version
    3.10 and should not be used in newly written code. Use the function
    @fun{g-action-group-activate-action} on a @class{g-action} object instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{g-action}
  @see-function{g-action-group-activate-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_icon" gtk-action-create-icon) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @argument[size]{a value of the @symbol{gtk-icon-size} enumeration for the
    size of the icon that should be created}
  @return{A @class{gtk-widget} object that displays the icon for this action.}
  @begin{short}
    This function is intended for use by action implementations to create icons
    displayed in the proxy widgets.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-create-icon} has been deprecated since version
    3.10 and should not be used in newly written code. Use the function
    @fun{g-menu-item-set-icon} to set an icon on a @class{g-menu-item} object,
    or the function @fun{gtk-container-add} to add a @class{gtk-image} widget
    to a @class{gtk-button} widget.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-widget}
  @see-class{g-menu-item}
  @see-class{gtk-image}
  @see-class{gtk-button}
  @see-symbol{gtk-icon-size}
  @see-function{g-menu-item-set-icon}
  @see-function{gtk-container-add}"
  (action (g-object gtk-action))
  (size gtk-icon-size))

(export 'gtk-action-create-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu_item" gtk-action-create-menu-item)
    (g-object gtk-image-menu-item)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{A @class{gtk-image-menu-item} widget connected to the action.}
  @short{Creates a menu item widget that proxies for the given action.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-create-menu-item} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the function
    @fun{g-menu-item-new} and associate it with a @class{g-action} object
    instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-image-menu-item}
  @see-class{g-action}
  @see-function{g-menu-item-new}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_tool_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_tool_item" gtk-action-create-tool-item)
    (g-object gtk-tool-button)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{A @class{gtk-tool-button} widget connected to the action.}
  @short{Creates a toolbar item that proxies for the given action.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-create-tool-item} has been deprecated since
    version 3.10 and should not be used in newly written code. Use a
    @class{gtk-tool-item} widget and associate it with a @class{g-action}
    object using the function @fun{gtk-actionable-action-name} instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-tool-button}
  @see-class{gtk-tool-item}
  @see-class{g-action}
  @see-function{gtk-actionable-action-name}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-tool-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_create_menu" gtk-action-create-menu) g-object
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{The @class{gtk-menu} widget provided by the action, or @code{nil}.}
  @begin{short}
    If the action provides a @class{gtk-menu} widget as a submenu for the menu
    item or the toolbar item it creates, this function returns an instance of
    that menu.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-create-menu} has been deprecated since version
    3.10 and should not be used in newly written code. Use the @class{g-action}
    interface and the @class{g-menu-model} object instead, and create a
    @class{gtk-menu} widget with the function @fun{gtk-menu-new-from-model}.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-menu}
  @see-class{g-action}
  @see-class{g-menu-model}
  @see-function{gtk-menu-new-from-model}"
  (action (g-object gtk-action)))

(export 'gtk-action-create-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_proxies () -> gtk-action-proxies
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_proxies" gtk-action-proxies)
    (g-slist g-object :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @return{A list of @class{gtk-widget} proxy widgets.}
  @short{Returns the proxy widgets for the action.}
  See also the function @fun{gtk-activatable-related-action}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-proxies} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-widget}
  @see-function{gtk-activatable-related-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-proxies)

;;; ----------------------------------------------------------------------------
;;; gtk_action_connect_accelerator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_connect_accelerator" gtk-action-connect-accelerator) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @begin{short}
    Installs the accelerator for the action if the action has an accel path and
    group.
  @end{short}
  See the functions @fun{gtk-action-accel-path} and
  @fun{gtk-action-set-accel-group}.

  Since multiple proxies may independently trigger the installation of the
  accelerator, the action counts the number of times this function has been
  called and does not remove the accelerator until the function
  @fun{gtk-action-disconnect-accelerator} has been called as many times.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-connect-accelerator} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g-action} interface and the accelerator group on an associated
    @class{gtk-menu} widget instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-menu}
  @see-class{g-action}
  @see-function{gtk-action-accel-path}
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
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @begin{short}
    Undoes the effect of one call to the function
    @fun{gtk-action-connect-accelerator}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-disconnect-accelerator} has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @class{g-action} interface and the accelerator group on an associated
    @class{gtk-menu} widget instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-menu}
  @see-class{g-action}
  @see-function{gtk-action-connect-accelerator}"
  (action (g-object gtk-action)))

(export 'gtk-action-disconnect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_block_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_block_activate" gtk-action-block-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @short{Disable activation signals from the action.}

  This is needed when updating the state of your @class{gtk-activatable} proxy
  widget could result in calling the function @fun{gtk-action-activate}, this
  is a convenience function to avoid recursing in those cases, updating toggle
  state for instance.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-block-activate} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the function
    @fun{g-simple-action-enabled} to disable the @class{g-simple-action} object
    instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-activatable}
  @see-class{g-simple-action}
  @see-function{gtk-action-activate}
  @see-function{g-simple-action-enbaled}"
  (action (g-object gtk-action)))

(export 'gtk-action-block-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_unblock_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_unblock_activate" gtk-action-unblock-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @short{Reenable activation signals from the action.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-unblock-activate} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the function
    @fun{g-simple-action-enabled} to enable the @class{g-simple-action} object
    instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{g-simple-action}
  @see-function{gtk-action-block-activate}
  @see-function{g-simple-action-enabled}"
  (action (g-object gtk-action)))

(export 'gtk-action-unblock-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_path ()
;;; gtk_action_set_accel_path () -> gtk-action-accel-path
;;; ----------------------------------------------------------------------------

(defun (setf gtk-action-accel-path) (path action)
  (foreign-funcall "gtk_action_set_accel_path"
                   (g-object gtk-action) action
                   :string path
                   :void)
  path)

(defcfun ("gtk_action_get_accel_path" gtk-action-accel-path) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @syntax[]{(gtk-action-accel-path action) => path}
  @syntax[]{(setf (gtk-action-accel-path action) path)}
  @argument[action]{a @class{gtk-action} object}
  @argument[path]{a string with the accelerator path}
  @begin{short}
    Accessor of the accel path for the action.
  @end{short}

  The function @sym{gtk-action-accel-path} returns the accel path for this
  action, or @code{nil} if none is set. The function
  @sym{(setf gtk-action-accel-path)} sets the accel path.

  All proxy widgets associated with the action will have this accel path, so
  that their accelerators are consistent.
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-accel-path} has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @class{g-action} interface and the accelerator path on an associated
    @class{gtk-menu} widget instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-menu}
  @see-class{g-action}"
  (action (g-object gtk-action)))

(export 'gtk-action-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_closure ()                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_get_accel_closure" gtk-action-accel-closure)
    (:pointer (:struct g-closure))
 #+cl-cffi-gtk-documentation
 "@version{2021-7-6}
  @argument[action]{a @class{gtk-action} object}
  @begin{return}
    The accel closure for this action.
  @end{return}
  The returned closure is owned by GTK and must not be unreffed or modified.
  @short{Returns the accel closure for this action.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-get-accel-closure} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g-action} inferface and @class{gtk-menu} widget instead, which have
    no equivalent for getting the accel closure.
  @end{dictionary}
  @see-class{gtk-action}"
  (action (g-object gtk-action)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_set_accel_group" gtk-action-set-accel-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-action} object}
  @argument[group]{a @class{gtk-accel-group} object or @code{nil}}
  @begin{short}
    Sets the accel group in which the accelerator for this action will be
    installed.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-action-set-accel-group} has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g-action} interface and the accelerator group on an associated
    @class{gtk-menu} widget instead.
  @end{dictionary}
  @see-class{gtk-action}
  @see-class{gtk-accel-group}
  @see-class{gtk-menu}
  @see-class{g-action}"
  (action (g-object gtk-action))
  (group (g-object gtk-accel-group)))

(export 'gtk-action-set-accel-group)

;;; --- End of file gtk.action.lisp --------------------------------------------
