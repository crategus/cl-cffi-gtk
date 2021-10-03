;;; ----------------------------------------------------------------------------
;;; gtk.image-menu-item.lisp
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
;;; GtkImageMenuItem
;;;
;;;     A deprecated widget for a menu item with an icon.
;;;
;;; Types and Values
;;;
;;;     GtkImageMenuItem
;;;
;;; Functions
;;;
;;;     gtk_image_menu_item_set_image                      Accessor
;;;     gtk_image_menu_item_get_image                      Accessor
;;;     gtk_image_menu_item_new
;;;     gtk_image_menu_item_new_from_stock
;;;     gtk_image_menu_item_new_with_label
;;;     gtk_image_menu_item_new_with_mnemonic
;;;     gtk_image_menu_item_get_use_stock                  Accessor
;;;     gtk_image_menu_item_set_use_stock                  Accessor
;;;     gtk_image_menu_item_get_always_show_image          Accessor
;;;     gtk_image_menu_item_set_always_show_image          Accessor
;;;     gtk_image_menu_item_set_accel_group                Accessor
;;;
;;; Properties
;;;
;;;     GtkAccelGroup*  accel-group          Write
;;;          gboolean   always-show-image    Read / Write / Construct
;;;         GtkWidget*  image                Read / Write
;;;          gboolean   use-stock            Read / Write / Construct
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkImageMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImageMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkImageMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkImageMenuItem" gtk-image-menu-item
  (:superclass gtk-menu-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_image_menu_item_get_type")
  ((accel-group
    gtk-image-menu-item-accel-group
    "accel-group" "GtkAccelGroup" nil t)
   (always-show-image
    gtk-image-menu-item-always-show-image
    "always-show-image" "gboolean" t t)
   (image
    gtk-image-menu-item-image
    "image" "GtkWidget" t t)
   (use-stock
    gtk-image-menu-item-use-stock
    "use-stock" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-image-menu-item 'type)
 "@version{2021-7-21}
  @begin{short}
    A @sym{gtk-image-menu-item} widget is a menu item which has an icon next to
    the text label.
  @end{short}
  This is functionally equivalent to:
  @begin{pre}
(defun create-image-menu-item ()
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
                            :spacing 6))
        (icon (make-instance 'gtk-image
                             :icon-name \"folder-music-symbolic\"
                             :icon-size 1))
        (label (make-instance 'gtk-label
                              :label \"Music\"))
        (menu-item (make-instance 'gtk-menu-item)))
    (gtk-container-add box icon)
    (gtk-container-add box label)
    (gtk-container-add menu-item box)
    menu-item))
  @end{pre}
  Note that the user may disable display of menu icons using the
  @slot[gtk-settings]{gtk-menu-images} setting, so make sure to still fill in
  the text label. If you want to ensure that your menu items show an icon you
  are strongly encouraged to use a @class{gtk-menu-item} widget with a
  @class{gtk-image} widget instead.

  Furthermore, if you would like to display keyboard accelerator, you must pack
  the accel label into the box using the @fun{gtk-box-pack-end} function and
  align the label, otherwise the accelerator will not display correctly. The
  following code snippet adds a keyboard accelerator to the menu item, with a
  key binding of the @kbd{Ctrl+M} key:
  @begin{pre}
(defun create-image-menu-item-with-accel ()
  (let ((box (make-instance 'gtk-box
                            :orientation :horizontal
                            :spacing 6))
        (icon (make-instance 'gtk-image
                             :icon-name \"folder-music-symbolic\"
                             :icon-size 1))
        (label (make-instance 'gtk-accel-label
                              :label \"Music\"
                              :use-underline t
                              :xalign 0.0))
        (menu-item (make-instance 'gtk-menu-item))
        (accel-group (make-instance 'gtk-accel-group)))
    (gtk-widget-add-accelerator menu-item
                                \"activate\"
                                accel-group
                                (gdk-keyval-from-name \"M\")
                                :control-mask
                                :visible)
    (setf (gtk-accel-label-accel-widget label) menu-item)
    (gtk-container-add box icon)
    (gtk-box-pack-end box label :expand t :fill t :padding 0)
    (gtk-container-add menu-item box)
    menu-item))
  @end{pre}
  @begin[Warning]{dictionary}
    The @sym{gtk-image-menu-item} class has been deprecated since GTK 3.10. If
    you want to display an icon in a menu item, you should use the
    @class{gtk-menu-item} widget and pack a @class{gtk-box} widget with a
    @class{gtk-image} widget and a @class{gtk-label} widget instead. You should
    also consider using the @class{gtk-builder} object and the XML
    @class{g-menu} description for creating menus, by following the
    @class{g-menu} guide. You should consider using icons in menu items only
    sparingly, and for \"objects\" or \"nouns\" elements only, like bookmarks,
    files, and links; \"actions\" or \"verbs\" should not have icons.
  @end{dictionary}
  @see-slot{gtk-image-menu-item-accel-group}
  @see-slot{gtk-image-menu-item-always-show-image}
  @see-slot{gtk-image-menu-item-image}
  @see-slot{gtk-image-menu-item-use-stock}
  @see-class{gtk-menu-item}
  @see-class{g-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-image-menu-item-accel-group ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accel-group"
                                               'gtk-image-menu-item) 't)
 "The @code{accel-group} property of type @class{gtk-accel-group}
  (Write) @br{}
  The accelerator group to use for stock accelerator keys.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-accel-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-accel-group 'function)
 "@version{2021-7-21}
  @syntax[]{(setf (gtk-image-menu-item-accel-group object) group)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[group]{a @class{gtk-accel-group} object}
  @begin{short}
    Accessor of @slot[gtk-image-menu-item]{accel-group} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  Specifies an accelerator group to add the menu items accelerator to, this
  only applies to stock items so a stock item must already be set. Make sure to
  call the @fun{gtk-image-menu-item-use-stock} and @fun{gtk-menu-item-label}
  functions with a valid stock item first.

  If you want this menu item to have changeable accelerators then you should
  not need this. See the function @fun{gtk-image-menu-item-new-from-stock}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-accel-group} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-class{gtk-accel-group}
  @see-function{gtk-image-menu-item-use-stock}
  @see-function{gtk-menu-item-label}
  @see-function{gtk-image-menu-item-new-from-stock}")

;;; --- gtk-image-menu-item-always-show-image ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "always-show-image"
                                               'gtk-image-menu-item) 't)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show the image, if
  available. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-always-show-image
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-always-show-image 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-image-menu-item-always-show-image object) => always-show}
  @syntax[]{(setf (gtk-image-menu-item-always-show-image object) always-show)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[always-show]{@em{true} if the menu item should always show the
    image}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{always-show-image} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  If @em{true}, the menu item will ignore the
  @slot[gtk-settings]{gtk-menu-images} setting and always show the image, if
  available. Use this property if the menu item would be useless or hard to use
  without the image.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-always-show-image} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-function{gtk-settings-gtk-menu-images}")

;;; --- gtk-image-menu-item-image ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image"
                                               'gtk-image-menu-item) 't)
 "The @code{image} property of type @class{gtk-widget} (Read / Write) @br{}
  Child widget to appear next to the menu text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-image 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-image-menu-item-image object) => image}
  @syntax[]{(setf (gtk-image-menu-item-image object) image)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[image]{a @class{gtk-widget} object to set as the image for the menu
    item}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{image} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  The slot access function @sym{gtk-image-menu-item-image} gets the widget that
  is currently set as the image of the menu item. The slot access function
  @sym{(setf gtk-image-menu-item-image)} sets the image. Note that it depends
  on the @slot[gtk-settings]{gtk-menu-images} setting whether the image will
  be displayed or not.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-image} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-class{gtk-widget}
  @see-function{gtk-settings-gtk-menu-images}")

;;; --- gtk-image-menu-item-use-stock ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-stock"
                                               'gtk-image-menu-item) 't)
 "The @code{use-stock} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the label set in the menu item is used as a stock ID to select
  the stock item for the item. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-menu-item-use-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-menu-item-use-stock 'function)
 "@version{2021-7-21}
  @syntax[]{(gtk-image-menu-item-use-stock object) => use-stock}
  @syntax[]{(setf (gtk-image-menu-item-use-stock object) use-stock)}
  @argument[object]{a @class{gtk-image-menu-item} widget}
  @argument[use-stock]{@em{true} if the menu item should use a stock item}
  @begin{short}
    Accessor of the @slot[gtk-image-menu-item]{use-stock} slot of the
    @class{gtk-image-menu-item} class.
  @end{short}

  If @em{true}, the label set in the menu item is used as a stock ID to select
  the stock item for the item.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-use-stock} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-menu-item-new))

(defun gtk-image-menu-item-new ()
 "@version{2021-7-21}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item with an empty label.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-new} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}"
  (make-instance 'gtk-image-menu-item))

(export 'gtk-image-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_from_stock"
           gtk-image-menu-item-new-from-stock) (g-object gtk-image-menu-item)
 "@version{2021-7-21}
  @argument[stock-id]{a string with the name of the stock item}
  @argument[group]{the @class{gtk-accel-group} object to add the menu items
    accelerator to, or @code{nil}}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing the image and text from a stock
    item.
  @end{short}

  If you want this menu item to have changeable accelerators, then pass in
  @code{nil} for @arg{group}. Next call the function
  @fun{gtk-menu-item-accel-path} with an appropriate path for the menu item,
  use the function @code{gtk_stock_lookup()} to look up the standard accelerator
  for the stock item, and if one is found, call the function
  @fun{gtk-accel-map-add-entry} to register it.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-new-from-stock} has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-class{gtk-accel-group}
  @see-function{gtk-menu-item-accel-path}
  @see-function{gtk-accel-map-add-entry}"
  (stock-id :string)
  (group (g-object gtk-accel-group)))

(export 'gtk-image-menu-item-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_with_label"
           gtk-image-menu-item-new-with-label) (g-object gtk-image-menu-item)
 "@version{2021-7-21}
  @argument[label]{a string with the text of the menu item}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing a label.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-new-with-label} has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    function @fun{gtk-menu-item-new-with-label} instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-function{gtk-menu-item-new-with-label}"
  (label :string))

(export 'gtk-image-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_menu_item_new_with_mnemonic"
           gtk-image-menu-item-new-with-mnemonic) (g-object gtk-image-menu-item)
 "@version{2021-7-21}
  @argument[label]{a string with the text of the menu item, with an underscore
    in front of the mnemonic character}
  @return{A new @class{gtk-image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing a label.
  @end{short}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the menu item.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-menu-item-new-with-mnemonic} has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    function @fun{gtk-menu-item-new-with-mnemonic} instead.
  @end{dictionary}
  @see-class{gtk-image-menu-item}
  @see-function{gtk-label-new-with-mnemonic}
  @see-function{gtk-menu-item-new-with-mnemonic}"
  (label :string))

(export 'gtk-image-menu-item-new-with-mnemonic)

;;; --- End of file gtk.image-menu-item.lisp -----------------------------------
