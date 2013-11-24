;;; ----------------------------------------------------------------------------
;;; gtk.ui-manager.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkUIManager
;;;
;;; Constructing menus and toolbars from an XML description
;;;
;;; Synopsis
;;;
;;;     GtkUIManager
;;;
;;;     gtk_ui_manager_new
;;;     gtk_ui_manager_set_add_tearoffs
;;;     gtk_ui_manager_get_add_tearoffs
;;;     gtk_ui_manager_insert_action_group
;;;     gtk_ui_manager_remove_action_group
;;;     gtk_ui_manager_get_action_groups
;;;     gtk_ui_manager_get_accel_group
;;;     gtk_ui_manager_get_widget
;;;     gtk_ui_manager_get_toplevels
;;;     gtk_ui_manager_get_action
;;;     gtk_ui_manager_add_ui_from_resource
;;;     gtk_ui_manager_add_ui_from_string
;;;     gtk_ui_manager_add_ui_from_file
;;;     gtk_ui_manager_new_merge_id
;;;
;;;     GtkUIManagerItemType
;;;
;;;     gtk_ui_manager_add_ui
;;;     gtk_ui_manager_remove_ui
;;;     gtk_ui_manager_get_ui
;;;     gtk_ui_manager_ensure_update
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(defstruct ui-d
  class
  props
  children
  expansion
  var
  initform
  initializer)

(defstruct ui-prop
  name
  value)

(defstruct ui-child
  v
  props)

(defun parse-ui-props (list)
  ;; list is ({:prop value}* rest)
  (iter (for x first list then (cddr x))
        (while (keywordp (first x)))
        (for (name value) = x)
        (collect (make-ui-prop :name name :value value) into props)
        (finally (return (values props x)))))

(defun parse-ui-children (list)
  ;; list is (child*)
  ;; child is {ui {:prop value}*}
  (iter (while list)
        (for child = (if (eq :expr (first (first list)))
                         (make-ui-d :var (second (first list)))
                         (parse-ui-description (first list))))
        (for (values props rest) = (parse-ui-props (rest list)))
        (setf list rest)
        (collect (make-ui-child :v child :props props))))

(defun parse-ui-description (description)
  ;; description is (class {:prop value}* child*)
  ;; child is {ui {:prop value}*}
  (let ((class (first description)))
    (multiple-value-bind (props rest) (parse-ui-props (rest description))
      (let ((children (parse-ui-children rest)))
        (make-ui-d :class class :props props :children children)))))

(defun get-ui-d-var (d)
  (let ((prop (find :var (ui-d-props d) :key #'ui-prop-name)))
    (if prop
        (ui-prop-value prop)
        (gensym (format nil "~A-" (symbol-name (ui-d-class d)))))))

(defun get-ui-d-initform (d)
  `(make-instance ',(ui-d-class d)
                  ,@(iter (for prop in (ui-d-props d))
                          (unless (eq (ui-prop-name prop) :var)
                            (appending (list (ui-prop-name prop)
                                             (ui-prop-value prop)))))))

(defgeneric pack-child (container child &key))

(defmethod pack-child ((w gtk-container) child &key)
  (gtk-container-add w child))

(defmethod pack-child ((b gtk-box) child &key (expand t)
                                              (fill t)
                                              (padding 0) pack-type position)
  (gtk-box-pack-start b child :expand expand :fill fill :padding padding)
  (when pack-type
    (setf (gtk-box-child-pack-type b child) pack-type))
  (when position
    (setf (gtk-box-child-position b child) position)))

(defmethod pack-child ((p gtk-paned) child &key (resize 'default) (shrink t))
  (if (null (gtk-paned-get-child1 p))
      (gtk-paned-pack1 p
                       child
                       :resize (if (eq resize 'default) nil resize)
                       :shrink shrink)
      (gtk-paned-pack2 p
                       child
                       :resize (if (eq resize 'default) t resize)
                       :shrink shrink)))

(defmethod pack-child ((table gtk-table) child &key
                       left right top bottom
                       (x-options '(:expand :fill))
                       (y-options '(:expand :fill))
                       (x-padding 0) (y-padding 0))
  (unless left
    (error "left is a mandatory child property for table packing"))
  (unless right
    (error "right is a mandatory child property for table packing"))
  (unless top
    (error "top is a mandatory child property for table packing"))
  (unless bottom
    (error "bottom is a mandatory child property for table packing"))
  (gtk-table-attach table child
                    left
                    right
                    top
                    bottom
                    :x-options x-options
                    :y-options y-options
                    :x-padding x-padding
                    :y-padding y-padding))

(defmethod pack-child ((w gtk-tree-view) child &key)
  (gtk-tree-view-append-column w child))

(defmethod pack-child ((w gtk-tree-view-column) child
                       &key (expand t) attributes)
  (gtk-tree-view-column-pack-start w child :expand expand)
  (iter (for a on attributes by #'cddr)
        (gtk-tree-view-column-add-attribute w
                                            child
                                            (first a)
                                            (second a))))

(defmethod pack-child ((b gtk-toolbar) child &key (expand 'default)
                                              (homogeneous 'default))
  (gtk-toolbar-insert b child -1)
  (unless (eq expand 'default)
    (container-call-set-property b child "expand" expand +g-type-boolean+))
  (unless (eq homogeneous 'default)
    (container-call-set-property b
                                 child
                                 "homogeneous" homogeneous
                                 +g-type-boolean+)))

(defun set-ui-expansion-1 (d)
  (when (ui-d-class d)
    ;; only direct-vars do not have class
    (setf (ui-d-var d) (get-ui-d-var d)
          (ui-d-initform d) (get-ui-d-initform d))))

(defun set-ui-expansion (description)
  (iter (for child in (ui-d-children description))
        (set-ui-expansion (ui-child-v child)))
  (set-ui-expansion-1 description))

(defun flattened-ui-descriptions (d)
  (cons d
        (iter (for child in (ui-d-children d))
              (when (ui-d-class (ui-child-v child))
                (appending (flattened-ui-descriptions (ui-child-v child)))))))

(defmacro let-ui (ui-description &body body)
  (let* ((description (parse-ui-description ui-description))
         (items (flattened-ui-descriptions description)))
    (set-ui-expansion description)
    `(let (,@(iter (for item in items)
                   (collect (list (ui-d-var item)
                                  (ui-d-initform item)))))
       ,@(iter (for item in items)
               (appending (iter (for child in (ui-d-children item))
                                (for child-var = (ui-d-var (ui-child-v child)))
                                (let ((props
                                       (iter (for p in (ui-child-props child))
                                             (appending (list (ui-prop-name p)
                                                              (ui-prop-value p))))))
                                  (collect (list* 'pack-child
                                                  (ui-d-var item)
                                                  child-var props))))))
       ,@body)))

;;; ----------------------------------------------------------------------------
;;; struct GtkUIManager
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkUIManager" gtk-ui-manager
  (:superclass g-object
    :export t
    :interfaces ("GtkBuildable"))
  ((add-tearoffs
    gtk-ui-manager-add-tearoffs
    "add-tearoffs" "gboolean" t t)
   (ui
    gtk-ui-manager-ui
    "ui" "gchararray" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-ui-manager 'type)
 "@version{2013-6-2}
  @begin{short}
    A @sym{gtk-ui-manager} constructs a user interface (menus and toolbars) from
    one or more UI definitions, which reference actions from one or more action
    groups.
  @end{short}

  @subheading{UI Definitions}
    The UI definitions are specified in an XML format which can be roughly
    described by the following DTD.

  @subheading{Note}
    Do not confuse the @sym{gtk-ui-manager} UI Definitions described here with
    the similarly named @class{gtk-builder} UI Definitions.
    @begin{pre}
   <!ELEMENT ui          (menubar|toolbar|popup|accelerator)* >
   <!ELEMENT menubar     (menuitem|separator|placeholder|menu)* >
   <!ELEMENT menu        (menuitem|separator|placeholder|menu)* >
   <!ELEMENT popup       (menuitem|separator|placeholder|menu)* >
   <!ELEMENT toolbar     (toolitem|separator|placeholder)* >
   <!ELEMENT placeholder (menuitem|toolitem|separator|placeholder|menu)* >
   <!ELEMENT menuitem     EMPTY >
   <!ELEMENT toolitem     (menu?) >
   <!ELEMENT separator    EMPTY >
   <!ELEMENT accelerator  EMPTY >
   <!ATTLIST menubar      name                      #IMPLIED
                          action                    #IMPLIED >
   <!ATTLIST toolbar      name                      #IMPLIED
                          action                    #IMPLIED >
   <!ATTLIST popup        name                      #IMPLIED
                          action                    #IMPLIED
                          accelerators (true|false) #IMPLIED >
   <!ATTLIST placeholder  name                      #IMPLIED
                          action                    #IMPLIED >
   <!ATTLIST separator    name                      #IMPLIED
                          action                    #IMPLIED
                          expand       (true|false) #IMPLIED >
   <!ATTLIST menu         name                      #IMPLIED
                          action                    #REQUIRED
                          position     (top|bot)    #IMPLIED >
   <!ATTLIST menuitem     name                      #IMPLIED
                          action                    #REQUIRED
                          position     (top|bot)    #IMPLIED
                          always-show-image (true|false) #IMPLIED >
   <!ATTLIST toolitem     name                      #IMPLIED
                          action                    #REQUIRED
                          position     (top|bot)    #IMPLIED >
   <!ATTLIST accelerator  name                      #IMPLIED
                          action                    #REQUIRED >
    @end{pre}
    There are some additional restrictions beyond those specified in the DTD,
    e. g. every toolitem must have a toolbar in its anchestry and every menuitem
    must have a menubar or popup in its anchestry. Since a @code{GMarkup} parser
    is used to parse the UI description, it must not only be valid XML, but
    valid @code{GMarkup}.

    If a name is not specified, it defaults to the action. If an action is not
    specified either, the element name is used. The name and action attributes
    must not contain '/' characters after parsing (since that would mess up path
    lookup) and must be usable as XML attributes when enclosed in doublequotes,
    thus they must not '\"' characters or references to the \" entity.

    @b{Example:} A UI definition
    @begin{pre}
   <span style=\"color: red\"><ui>
     <span style=\"color: red\"><menubar>
       <span style=\"color: red\"><menu>
         <span style=\"color: red\"><menuitem></menuitem></span>
         <span style=\"color: red\"><placeholder></placeholder></span>
       </menu></span>
       <span style=\"color: red\"><menu>
         <span style=\"color: red\"><menuitem></menuitem></span>
         <span style=\"color: red\"><menuitem></menuitem></span>
         <span style=\"color: red\"><menuitem></menuitem></span>
         <span style=\"color: red\"><menuitem></menuitem></span>
       </menu></span>
     </menubar></span>
     <span style=\"color: red\"><toolbar>
       <span style=\"color: red\"><placeholder>
         <span style=\"color: red\"><separator></separator></span>
         <span style=\"color: red\"><toolitem></toolitem></span>
         <span style=\"color: red\"><toolitem></toolitem></span>
         <span style=\"color: red\"><toolitem></toolitem></span>
         <span style=\"color: red\"><toolitem></toolitem></span>
         <span style=\"color: red\"><separator></separator></span>
       </placeholder></span>
     </toolbar></span>
   </ui></span>
    @end{pre}
    The constructed widget hierarchy is very similar to the element tree of the
    XML, with the exception that placeholders are merged into their parents. The
    correspondence of XML elements to widgets should be almost obvious:
    @begin[code]{table}
      @entry[menubar]{a @class{gtk-menu-bar}}
      @entry[toolbar]{a @class{gtk-toolbar}}
      @entry[popup]{a toplevel @class{gtk-menu}}
      @entry[menu]{a @class{gtk-menu} attached to a menuitem}
      @entry[menuitem]{a @class{gtk-menu-item} subclass, the exact type depends
        on the action}
      @entry[toolitem]{a @class{gtk-tool-item} subclass, the exact type depends
        on the action. Note that toolitem elements may contain a menu element,
        but only if their associated action specifies a
        @class{gtk-menu-tool-button} as proxy.}
      @entry[separator]{a @class{gtk-separator-menu-item} or
        @class{gtk-separator-tool-item}}
      @entry[accelerator]{a keyboard accelerator}
    @end{table}
    The \"position\" attribute determines where a constructed widget is
    positioned wrt. to its siblings in the partially constructed tree. If it is
    \"top\", the widget is prepended, otherwise it is appended.

  @subheading{UI Merging}
    The most remarkable feature of @sym{gtk-ui-manager} is that it can
    overlay a set of menuitems and toolitems over another one, and demerge
    them later.

    Merging is done based on the names of the XML elements. Each element is
    identified by a path which consists of the names of its anchestors,
    separated by slashes. For example, the menuitem named \"Left\" in the
    example above has the path /ui/menubar/JustifyMenu/Left and the toolitem
    with the same name has path /ui/toolbar1/JustifyToolItems/Left.

  @subheading{Accelerators}
    Every action has an accelerator path. Accelerators are installed together
    with menuitem proxies, but they can also be explicitly added with
    <accelerator> elements in the UI definition. This makes it possible to have
    accelerators for actions even if they have no visible proxies.

  @subheading{Smart Separators}
    The separators created by @sym{gtk-ui-manager} are \"smart\", i. e. they
    do not show up in the UI unless they end up between two visible menu or tool
    items. Separators which are located at the very beginning or end of the menu
    or toolbar containing them, or multiple separators next to each other, are
    hidden. This is a useful feature, since the merging of UI elements from
    multiple sources can make it hard or impossible to determine in advance
    whether a separator will end up in such an unfortunate position.

    For separators in toolbars, you can set expand=\"true\" to turn them from a
    small, visible separator to an expanding, invisible one. Toolitems following
    an expanding separator are effectively right-aligned.

  @subheading{Empty Menus}
    Submenus pose similar problems to separators inconnection with merging. It
    is impossible to know in advance whether they will end up empty after
    merging. @sym{gtk-ui-manager} offers two ways to treat empty submenus:
    @begin{itemize}
      @begin{item}
        Make them disappear by hiding the menu item they are attached to.
      @end{item}
      @begin{item}
        Add an insensitive \"Empty\" item.
      @end{item}
    @end{itemize}
    The behaviour is chosen based on the @code{\"hide-if-empty\"} property of
    the action to which the submenu is associated.

  @subheading{GtkUIManager as GtkBuildable}
    The @sym{gtk-ui-manager} implementation of the @class{gtk-buildable}
    interface accepts @class{gtk-action-group} objects as <child> elements in
    UI definitions.

    A @sym{gtk-ui-manager} UI definition as described above can be embedded in
    an @sym{gtk-ui-manager} <object> element in a @class{gtk-builder} UI
    definition.

    The widgets that are constructed by a @sym{gtk-ui-manager} can be embedded
    in other parts of the constructed user interface with the help of the
    \"constructor\" attribute. See the example below.

    @b{Example:} An embedded @sym{gtk-ui-manager} UI definition
    @begin{pre}
   <object class=\"GtkUIManager\" id=\"uiman\">
     <child>
       <object class=\"GtkActionGroup\" id=\"actiongroup\">
         <child>
           <object class=\"GtkAction\" id=\"file\">
             <property name=\"label\">_File</property>
           </object>
         </child>
       </object>
     </child>
     <ui>
       <menubar name=\"menubar1\">
         <menu action=\"file\">
         </menu>
       </menubar>
     </ui>
   </object>
   <object class=\"GtkWindow\" id=\"main-window\">
     <child>
       <object class=\"GtkMenuBar\" id=\"menubar1\" constructor=\"uiman\"/>
     </child>
   </object>
    @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"actions-changed\" signal}
      @begin{pre}
 lambda (mangager)   : No Recursion
      @end{pre}
      The \"actions-changed\" signal is emitted whenever the set of actions
      changes.
      @begin[code]{table}
        @entry[manager]{A @sym{gtk-ui-manager}.}
      @end{table}
      Since 2.4

    @subheading{The \"add-widget\" signal}
      @begin{pre}
 lambda (manager widget)   : No Recursion
      @end{pre}
      The \"add-widget\" signal is emitted for each generated menubar and
      toolbar. It is not emitted for generated popup menus, which can be
      obtained by the @fun{gtk-ui-manager-get-widget} function.
      @begin[code]{table}
        @entry[manager]{A @sym{gtk-ui-manager}.}
        @entry[widget]{The added widget.}
      @end{table}
      Since 2.4

    @subheading{The \"connect-proxy\" signal}
      @begin{pre}
 lambda (manager action proxy)   : No Recursion
      @end{pre}
      The \"connect-proxy\" signal is emitted after connecting a proxy to an
      action in the group.
      This is intended for simple customizations for which a custom action class
      would be too clumsy, e. g. showing tooltips for menuitems in the
      statusbar.
      @begin[code]{table}
        @entry[manager]{The ui manager.}
        @entry[action]{The action.}
        @entry[proxy]{The proxy.}
      @end{table}
      Since 2.4

    @subheading{The \"disconnect-proxy\" signal}
      @begin{pre}
 lambda (manager action proxy)   : No Recursion
      @end{pre}
      The \"disconnect-proxy\" signal is emitted after disconnecting a proxy
      from an action in the group.
      @begin[code]{table}
        @entry[manager]{The ui manager.}
        @entry[action]{The action.}
        @entry[proxy]{The proxy.}
      @end{table}
      Since 2.4

    @subheading{The \"post-activate\" signal}
      @begin{pre}
 lambda (manager action)   : No Recursion
      @end{pre}
      The \"post-activate\" signal is emitted just after the action is
      activated.
      This is intended for applications to get notification just after any
      action is activated.
      @begin[code]{table}
        @entry[manager]{The ui manager.}
        @entry[action]{The action.}
      @end{table}
      Since 2.4

    @subheading{The \"pre-activate\" signal}
      @begin{pre}
 lambda (manager action)   : No Recursion
      @end{pre}
      The \"pre-activate\" signal is emitted just before the action is
      activated.
      This is intended for applications to get notification just before any
      action is activated.
      @begin[code]{table}
        @entry[manager]{The ui manager.}
        @entry[action]{The action.}
      @end{table}
      Since 2.4
  @end{dictionary}
  @see-slot{gtk-ui-manager-add-tearoffs}
  @see-slot{gtk-ui-manager-ui}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "add-tearoffs"
                                               'gtk-ui-manager) 't)
 "The @code{\"add-tearoffs\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @subheading{Warning}
    The @code{\"add-tearoffs\"} property has been deprecated since version 3.4
    and should not be used in newly-written code. Tearoff menus are deprecated
    and should not be used in newly written code.

  The \"add-tearoffs\" property controls whether generated menus have tearoff
  menu items.
  Note that this only affects regular menus. Generated popup menus never have
  tearoff menu items. @br{}
  Default value: @code{nil} @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ui" 'gtk-ui-manager) 't)
 "The @code{\"ui\"} property of type @code{:string} (Read) @br{}
  An XML string describing the merged UI. @br{}
  Default value: \"<ui>\n</ui>\n\"")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-ui-manager-add-tearoffs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-ui-manager-add-tearoffs 'function)
 "@version{2013-3-27}
  Accessor of the slot \"add-tearoffs\" of the @class{gtk-ui-manager} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-ui-manager-ui atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-ui-manager-ui 'function)
 "@version{2013-3-27}
  Accessor of the slot \"ui\" of the @class{gtk-ui-manager} class.")

;;; ----------------------------------------------------------------------------
;;; enum GtkUIManagerItemType
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkUIManagerItemType" gtk-ui-manager-item-type
  (:export t
   :type-initializer "gtk_ui_manager_item_type_get_type")
  (:auto 0)
  (:menubar 1)
  (:menu 2)
  (:toolbar 4)
  (:placeholder 8)
  (:popup 16)
  (:menuitem 32)
  (:toolitem 64)
  (:separator 128)
  (:accelerator 256)
  (:popup-with-accels 512))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-ui-manager-item-type atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-ui-manager-item-type atdoc:*external-symbols*)
 "@version{2013-6-2}
  @begin{short}
    These enumeration values are used by the @fun{gtk-ui-manager-add-ui}
    function to determine what UI element to create.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkUIManagerItemType\" gtk-ui-manager-item-type
  (:export t
   :type-initializer \"gtk_ui_manager_item_type_get_type\"))
  (:auto 0)
  (:menubar 1)
  (:menu 2)
  (:toolbar 4)
  (:placeholder 8)
  (:popup 16)
  (:menuitem 32)
  (:toolitem 64)
  (:separator 128)
  (:accelerator 256)
  (:popup-with-accels 512))
  @end{pre}
  @begin[code]{table}
    @entry[:auto]{Pick the type of the UI element according to context.}
    @entry[:menubar]{Create a menubar.}
    @entry[:menu]{Create a menu.}
    @entry[:toolbar]{Create a toolbar.}
    @entry[:placeholder]{Insert a placeholder.}
    @entry[:popup]{Create a popup menu.}
    @entry[:menuitem]{Create a menuitem.}
    @entry[:toolitem]{Create a toolitem.}
    @entry[:separator]{Create a separator.}
    @entry[:accelerator]{Install an accelerator.}
    @entry[:popup-with-accels]{Same as @code{:popup}, but the actions'
      accelerators are shown.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_new ()
;;;
;;; GtkUIManager * gtk_ui_manager_new (void);
;;;
;;; Creates a new ui manager object.
;;;
;;; Returns :
;;;     a new ui manager object.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_set_add_tearoffs ()
;;;
;;; void gtk_ui_manager_set_add_tearoffs (GtkUIManager *manager,
;;;                                       gboolean add_tearoffs);
;;;
;;; Warning
;;;
;;; gtk_ui_manager_set_add_tearoffs has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Tearoff menus are deprecated and
;;; should not be used in newly written code.
;;;
;;; Sets the "add_tearoffs" property, which controls whether menus generated by
;;; this GtkUIManager will have tearoff menu items.
;;;
;;; Note that this only affects regular menus. Generated popup menus never have
;;; tearoff menu items.
;;;
;;; manager :
;;;     a GtkUIManager
;;;
;;; add_tearoffs :
;;;     whether tearoff menu items are added
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_add_tearoffs ()
;;;
;;; gboolean gtk_ui_manager_get_add_tearoffs (GtkUIManager *manager);
;;;
;;; Warning
;;;
;;; gtk_ui_manager_get_add_tearoffs has been deprecated since version 3.4 and
;;; should not be used in newly-written code. Tearoff menus are deprecated and
;;; should not be used in newly written code.
;;;
;;; Returns whether menus generated by this GtkUIManager will have tearoff menu
;;; items.
;;;
;;; manager :
;;;     a GtkUIManager
;;;
;;; Returns :
;;;     whether tearoff menu items are added
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_insert_action_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_insert_action_group"
          gtk-ui-manager-insert-action-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[action-group]{the action group to be inserted}
  @argument[pos]{the position at which the group will be inserted}
  @begin{short}
    Inserts an action group into the list of action groups associated with
    manager. Actions in earlier groups hide actions with the same name in later
    groups.
  @end{short}

  If @arg{pos} is larger than the number of action groups in manager, or
  negative, @arg{action-group} will be inserted at the end of the internal list.

  Since 2.4"
  (ui-manager g-object)
  (action-group g-object)
  (pos :int))

(export 'gtk-ui-manager-insert-action-group)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_remove_action_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_remove_action_group"
          gtk-ui-manager-remove-action-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[action-group]{the action group to be removed}
  @begin{short}
    Removes an action group from the list of action groups associated with
    manager.
  @end{short}

  Since 2.4"
  (ui-manager g-object)
  (action-group g-object))

(export 'gtk-ui-manager-remove-action-group)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_action_groups ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_get_action_groups" gtk-ui-manager-action-groups)
    (g-list g-object :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @begin{return}
    A list of action groups.
  @end{return}
  @short{Returns the list of action groups associated with manager.}

  Since 2.4"
  (ui-manager g-object))

(export 'gtk-ui-manager-action-groups)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_accel_group ()
;;;
;;; GtkAccelGroup * gtk_ui_manager_get_accel_group (GtkUIManager *manager);
;;;
;;; Returns the GtkAccelGroup associated with manager.
;;;
;;; manager :
;;;     a GtkUIManager object
;;;
;;; Returns :
;;;     the GtkAccelGroup
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_get_widget" gtk-ui-manager-get-widget) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[path]{a path}
  @begin{return}
    The widget found by following the @arg{path}, or @code{nil} if no widget
    was found.
  @end{return}
  @begin{short}
    Looks up a widget by following a @arg{path}. The @arg{path} consists of the
    names specified in the XML description of the UI. separated by '/'. Elements
    which do not have a name or action attribute in the XML (e. g. <popup>) can
    be addressed by their XML element name (e. g. \"popup\"). The root element
    (\"/ui\") can be omitted in the @arg{path}.
  @end{short}

  Note that the widget found by following a path that ends in a <menu> element
  is the menuitem to which the menu is attached, not the menu itmanager.

  Also note that the widgets constructed by a UI manager are not tied to the
  lifecycle of the UI manager. If you add the widgets returned by this
  function to some container or explicitly ref them, they will survive the
  destruction of the UI manager.

  Since 2.4
  @see-class{gtk-ui-manager}"
  (ui-manager g-object)
  (path :string))

(export 'gtk-ui-manager-get-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_toplevels ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_get_toplevels" gtk-ui-manager-toplevels)
    (g-slist g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[types]{specifies the types of toplevel widgets to include. Allowed
    types are @code{:menubar}, @code{:toolbar} and @code{:popup}}
  @begin{return}
    A newly-allocated list of all toplevel widgets of the requested types.
  @end{return}
  @begin{short}
    Obtains a list of all toplevel widgets of the requested types.
  @end{short}

  Since 2.4"
  (ui-manager g-object)
  (types gtk-ui-manager-item-type))

(export 'gtk-ui-manager-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_get_action" gtk-ui-manager-action) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[path]{a path}
  @begin{return}
    The action whose proxy widget is found by following the @arg{path},
    or @code{nil} if no widget was found.
  @end{return}
  @begin{short}
    Looks up an action by following a @arg{path}.
  @end{short}
  See the @fun{gtk-ui-manager-get-widget} function for more information about
  paths.

  Since 2.4
  @see-function{gtk-ui-manager-get-widget}"
  (ui-manager g-object)
  (path :string))

(export 'gtk-ui-manager-action)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_add_ui_from_resource ()
;;;
;;; guint gtk_ui_manager_add_ui_from_resource (GtkUIManager *manager,
;;;                                            const gchar *resource_path,
;;;                                            GError **error);
;;;
;;; Parses a resource file containing a UI definition and merges it with the
;;; current contents of manager.
;;;
;;; manager :
;;;     a GtkUIManager object
;;;
;;; resource_path :
;;;     the resource path of the file to parse
;;;
;;; error :
;;;     return location for an error
;;;
;;; Returns :
;;;     The merge id for the merged UI. The merge id can be used to unmerge the
;;;     UI with gtk_ui_manager_remove_ui(). If an error occurred, the return
;;;     value is 0.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_add_ui_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_add_ui_from_string"
          %gtk-ui-manager-add-ui-from-string)
    :uint
  (ui-manager g-object)
  (buffer :string)
  (length g-ssize)
  (error :pointer))

(defun gtk-ui-manager-add-ui-from-string (ui-manager buffer)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[buffer]{the string to parse}
  @begin{return}
    The merge ID for the merged UI. The merge ID can be used to unmerge the
    UI with the @fun{gtk-ui-manager-remove-ui} function. If an error occurred,
    the return value is 0.
  @end{return}
  @begin{short}
    Parses a string containing a UI definition and merges it with the current
    contents of manager. An enclosing <ui> element is added if it is missing.
  @end{short}

  Since 2.4"
  (with-g-error (err)
    (%gtk-ui-manager-add-ui-from-string ui-manager buffer -1 err)))

(export 'gtk-ui-manager-add-ui-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_add_ui_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_add_ui_from_file" %gtk-ui-manager-add-ui-from-file)
    :uint
  (ui-manager g-object)
  (file-name :string)
  (error :pointer))

(defun gtk-ui-manager-add-ui-from-file (ui-manager filename)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[filename]{the name of the file to parse}
  @begin{return}
    The merge ID for the merged UI. The merge ID can be used to unmerge the
    UI with the @fun{gtk-ui-manager-remove-ui} function. If an error occurred,
    the return value is 0.
  @end{return}
  @begin{short}
    Parses a file containing a UI definition and merges it with the current
    contents of manager.
  @end{short}

  Since 2.4"
  (with-g-error (err)
    (%gtk-ui-manager-add-ui-from-file ui-manager filename err)))

(export 'gtk-ui-manager-add-ui-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_new_merge_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_new_merge_id" gtk-ui-manager-new-merge-id) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @return{An unused merge ID.}
  @begin{short}
    Returns an unused merge ID, suitable for use with the
    @fun{gtk-ui-manager-add-ui} function.
  @end{short}

  Since 2.4"
  (ui-manager g-object))

(export 'gtk-ui-manager-new-merge-id)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_add_ui ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_add_ui" gtk-ui-manager-add-ui) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[merge-id]{the merge ID for the merged UI, see the
    @fun{gtk-ui-manager-new-merge-id} function}
  @argument[path]{a path}
  @argument[name]{the name for the added UI element}
  @argument[action]{the name of the action to be proxied, or @code{nil} to add
    a separator}
  @argument[type]{the type of UI element to add}
  @argument[top]{if @em{true}, the UI element is added before its siblings,
    otherwise it is added after its siblings}
  @begin{short}
    Adds a UI element to the current contents of manager.
  @end{short}

  If type is @code{:auto}, GTK+ inserts a menuitem, toolitem or separator if
  such an element can be inserted at the place determined by @arg{path}.
  Otherwise type must indicate an element that can be inserted at the place
  determined by @arg{path}.

  If @arg{path} points to a menuitem or toolitem, the new element will be
  inserted before or after this item, depending on top.

  Since 2.4"
  (ui-manager g-object)
  (merge-id :uint)
  (path :string)
  (name :string)
  (action :string)
  (type gtk-ui-manager-item-type)
  (top :boolean))

(export 'gtk-ui-manager-add-ui)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_remove_ui ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_remove_ui" gtk-ui-manager-remove-ui) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @argument[merge-id]{a merge ID as returned by the
    @fun{gtk-ui-manager-add-ui-from-string} function}
  @begin{short}
    Unmerges the part of managers content identified by @arg{merge-id}.
  @end{short}

  Since 2.4
  @see-function{gtk-ui-manager-add-ui-from-string}"
  (ui-manager g-object)
  (merge-id :uint))

(export 'gtk-ui-manager-remove-ui)

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_get_ui ()
;;;
;;; gchar * gtk_ui_manager_get_ui (GtkUIManager *manager);
;;;
;;; Creates a UI definition of the merged UI.
;;;
;;; manager :
;;;     a GtkUIManager
;;;
;;; Returns :
;;;     A newly allocated string containing an XML representation of the merged
;;;     UI.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_ui_manager_ensure_update ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_ui_manager_ensure_update" gtk-ui-manager-ensure-update) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[manager]{a @class{gtk-ui-manager} object}
  @begin{short}
    Makes sure that all pending updates to the UI have been completed.
  @end{short}

  This may occasionally be necessary, since @class{gtk-ui-manager} updates the
  UI in an idle function. A typical example where this function is useful is to
  enforce that the menubar and toolbar have been added to the main window before
  showing it:
  @begin{pre}
   gtk_container_add (GTK_CONTAINER (window), vbox);
   g_signal_connect (merge, \"add-widget\",
                     G_CALLBACK (add_widget), vbox);
   gtk_ui_manager_add_ui_from_file (merge, \"my-menus\");
   gtk_ui_manager_add_ui_from_file (merge, \"my-toolbars\");
   gtk_ui_manager_ensure_update (merge);
   gtk_widget_show (window);
  @end{pre}
  Since 2.4"
  (ui-manager g-object))

(export 'gtk-ui-manager-ensure-update)

;;; --- End of file gtk.ui-manager.lisp ----------------------------------------
