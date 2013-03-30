;;; ----------------------------------------------------------------------------
;;; gtk.image.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkImage
;;;
;;; A widget displaying an image
;;;
;;; Synopsis
;;;
;;;     GtkImage
;;;     GtkImageType
;;;
;;;     gtk_image_get_icon_set
;;;     gtk_image_get_pixbuf
;;;     gtk_image_get_stock
;;;     gtk_image_get_animation
;;;     gtk_image_get_icon_name
;;;     gtk_image_get_gicon
;;;     gtk_image_get_storage_type
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_icon_set
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_stock
;;;     gtk_image_new_from_animation
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_icon_set
;;;     gtk_image_set_from_pixbuf
;;;     gtk_image_set_from_stock
;;;     gtk_image_set_from_animation
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;     gtk_image_clear
;;;     gtk_image_new
;;;     gtk_image_set_pixel_size
;;;     gtk_image_get_pixel_size
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkImage
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkImage" gtk-image
  (:superclass gtk-misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_image_get_type")
  ((file
    gtk-image-file
    "file" "gchararray" t t)
   (gicon
    gtk-image-gicon
    "gicon" "GIcon" t t)
   (icon-name
    gtk-image-icon-name
    "icon-name" "gchararray" t t)
   (icon-set
    gtk-image-icon-set
    "icon-set" "GtkIconSet" t t)
   (icon-size
    gtk-image-icon-size
    "icon-size" "gint" t t)
   (pixbuf
    gtk-image-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-animation
    gtk-image-pixbuf-animation
    "pixbuf-animation" "GdkPixbufAnimation" t t)
   (pixel-size
    gtk-image-pixel-size
    "pixel-size" "gint" t t)
   (stock
    gtk-image-stock
    "stock" "gchararray" t t)
   (storage-type
    gtk-image-storage-type
    "storage-type" "GtkImageType" t nil)
   (use-fallback
    gtk-image-use-fallback
    "use-fallback" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-image 'type)
 "@version{2013-2-1}
  @begin{short}
    The @sym{gtk-image} widget displays an image. Various kinds of object can be
    displayed as an image; most typically, you would load a @class{gdk-pixbuf}
    (\"pixel buffer\") from a file, and then display that.
  @end{short}
  There's a convenience function to do this, @fun{gtk-image-new-from-file}, used
  as follows:
  @begin{pre}
 GtkWidget *image;
 image = gtk_image_new_from_file (\"myfile.png\");
  @end{pre}
  If the file isn't loaded successfully, the image will contain a
  \"broken image\" icon similar to that used in many web browsers. If you want
  to handle errors in loading the file yourself, for example by displaying an
  error message, then load the image with @fun{gdk-pixbuf-new-from-file}, then
  create the @sym{gtk-image} with @fun{gtk-image-new-from-pixbuf}.

  The image file may contain an animation, if so the @sym{gtk-image} will
  display an animation of type @class{gdk-pixbuf-animation} instead of a static
  image.

  @sym{gtk-image} is a subclass of @class{gtk-misc}, which implies that you can
  align it (center, left, right) and add padding to it, using @class{gtk-misc}
  methods.

  @sym{gtk-image} is a \"no window\" widget (has no @class{gdk-window} of its
  own), so by default does not receive events. If you want to receive events on
  the image, such as button clicks, place the image inside a
  @class{gtk-event-box}, then connect to the event signals on the event box.

  @b{Example:} Handling button press events on a @sym{gtk-image}.
  @begin{pre}
 static gboolean
 button_press_callback (GtkWidget      *event_box,
                        GdkEventButton *event,
                        gpointer        data)
 {
   g_print (\"Event box clicked at coordinates %f,%f\n\",
            event->x, event->y);

   /* Returning TRUE means we handled the event, so the signal
    * emission should be stopped (don't call any further
    * callbacks that may be connected). Return FALSE
    * to continue invoking callbacks.
    */
   return TRUE;
 @}

 static GtkWidget*
 create_image (void)
 {
   GtkWidget *image;
   GtkWidget *event_box;

   image = gtk_image_new_from_file (\"myfile.png\");

   event_box = gtk_event_box_new ();

   gtk_container_add (GTK_CONTAINER (event_box), image);

   g_signal_connect (G_OBJECT (event_box),
                     \"button_press_event\",
                     G_CALLBACK (button_press_callback),
                     image);

   return image;
 @}
  @end{pre}
  When handling events on the event box, keep in mind that coordinates in the
  image may be different from event box coordinates due to the alignment and
  padding settings on the image (see @class{gtk-misc}). The simplest way to
  solve this is to set the alignment to 0.0 (left/top), and set the padding to
  zero. Then the origin of the image will be the same as the origin of the event
  box.

  Sometimes an application will want to avoid depending on external data
  files, such as image files. GTK+ comes with a program to avoid this, called
  @code{gdk-pixbuf-csource}. This library allows you to convert an image into a
  C variable declaration, which can then be loaded into a @class{gdk-pixbuf}
  using @fun{gdk-pixbuf-new-from-inline}.
  @see-slot{gtk-image-file}
  @see-slot{gtk-image-gicon}
  @see-slot{gtk-image-icon-name}
  @see-slot{gtk-image-icon-set}
  @see-slot{gtk-image-icon-size}
  @see-slot{gtk-image-pixbuf}
  @see-slot{gtk-image-pixbuf-animation}
  @see-slot{gtk-image-pixel-size}
  @see-slot{gtk-image-stock}
  @see-slot{gtk-image-storage-type}
  @see-slot{gtk-image-use-fallback}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-image) 't)
 "The @code{\"file\"} property of type @code{:string} (Read / Write)@br{}
  Filename to load and display. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-image) 't)
 "The @code{\"gicon\"} property of type @class{g-icon} (Read / Write)@br{}
  The @class{g-icon} displayed in the @sym{gtk-image}. For themed icons, If the
  icon theme is changed, the image will be updated automatically. @br{}
 Since 2.14")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-image) 't)
 "The @code{\"icon-name\"} property of type @code{:string} (Read / Write)@br{}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically. @br{}
  Default value: @code{nil}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-set" 'gtk-image) 't)
 "The @code{\"icon-set\"} property of type @class{gtk-icon-set}
  (Read / Write)@br{}
  Icon set to display.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-image) 't)
 "The @code{\"icon-size\"} property of type @code{:int} (Read / Write)@br{}
  Symbolic size to use for stock icon, icon set or named icon. @br{}
  Allowed values: >= 0@br{}
  Default value: 4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-image) 't)
 "The @code{\"pixbuf\"} property of type @class{gdk-pixbuf} (Read / Write)@br{}
  A @class{gdk-pixbuf} to display.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-animation"
                                               'gtk-image) 't)
 "The @code{\"pixbuf-animation\"} property of type @class{gdk-pixbuf-animation}
  (Read / Write)@br{}
  @class{gdk-pixbuf-animation} to display.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixel-size" 'gtk-image) 't)
 "The @code{\"pixel-size\"} property of type @code{:int} (Read / Write)@br{}
  The @code{\"pixel-size\"} property can be used to specify a fixed size
  overriding the @code{\"icon-size\"} property for images of type
  @code{:icon-name}.@br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1@br{}
  Since 2.6")
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-image) 't)
 "The @code{\"stock\"} property of type @code{:string} (Read / Write)@br{}
  Stock ID for a stock image to display. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "storage-type" 'gtk-image) 't)
 "The @code{\"storage-type\"} property of type @symbol{gtk-image-type}
  (Read)@br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-fallback" 'gtk-image) 't)
 "The @code{\"use-fallback\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the icon displayed in the @sym{gtk-image} will use standard icon names
  fallback. The value of this property is only relevant for images of type
  @code{:icon-name} and @code{:gicon}. @br{}
  Default value: @code{nil}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-file 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"file\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-gicon 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"gicon\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-name 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-name\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-set 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-set\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-size 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"icon-size\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"pixbuf\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf-animation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf-animation 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"pixbuf-animation\"} of the @class{gtk-image}
  class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixel-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixel-size 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"pixel-size\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-stock 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"stock\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-storage-type 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"storage-type\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-use-fallback atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-use-fallback 'function)
 "@version{2013-3-27}
  Accessor of the slot @code{\"use-fallback\"} of the @class{gtk-image} class.")

;;; ----------------------------------------------------------------------------
;;; enum GtkImageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkImageType" gtk-image-type
  (:export t
   :type-initializer "gtk_image_type_get_type")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-image-type atdoc:*external-symbols*)
 "@version{2013-2-1}
  @begin{short}
    Describes the image data representation used by a GtkImage.
  @end{short}
  If you want to get the image from the widget, you can only get the
  currently-stored representation. e.g. if the gtk_image_get_storage_type()
  returns GTK_IMAGE_PIXBUF, then you can call gtk_image_get_pixbuf() but not
  gtk_image_get_stock(). For empty images, you can request any storage type
  (call any of the \"get\" functions), but they will all return NULL values.
  @begin{pre}
(define-g-enum \"GtkImageType\" gtk-image-type
  (:export t
   :type-initializer \"gtk_image_type_get_type\")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6))
  @end{pre}
  @begin[code]{table}
    @entry[:empty]{there is no image displayed by the widget}
    @entry[:pixbuf]{the widget contains a GdkPixbuf}
    @entry[:stock]{the widget contains a stock icon name (see Stock Items(3))}
    @entry[:icon-set]{the widget contains a GtkIconSet}
    @entry[:animation]{the widget contains a GdkPixbufAnimation}
    @entry[:icon-name]{the widget contains a named icon. This image type was
      added in GTK+ 2.6}
    @entry[:gicon]{the widget contains a GIcon. This image type was added in
      GTK+ 2.14}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-icon-set))

(defun gtk-image-get-icon-set (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{icon-set} -- a @class{gtk-icon-set}, or @code{nil}@br{}
    @code{size} -- a stock icon size, or @code{nil}
  @end{return}
  Gets the icon set and size being displayed by the @class{gtk-image}. The
  storage type of the image must be @code{:empty} or @code{:icon-set} (see
  @fun{gtk-image-get-storage-type}).
  @see-function{gtk-image-get-storage-type}"
  (values (gtk-image-icon-set image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_pixbuf ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-pixbuf))

(defun gtk-image-get-pixbuf (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a @class{gtk-image} widget}
  @return{The displayed pixbuf, or @code{nil} if the image is empty.}
  Gets the @class{gdk-pixbuf} being displayed by the @class{gtk-image}. The
  storage type of the image must be @code{:empty} or @code{:pixbuf} (see
  @fun{gtk-image-get-storage-type}). The caller of this function does not own a
  reference to the returned pixbuf.
  @fun{gtk-image-get-storage-type}"
  (gtk-image-pixbuf image))

(export 'gtk-image-get-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-stock))

(defun gtk-image-get-stock (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a @class{gtk-image} object}
  @begin{return}
    @code{stock-id} -- a stock icon name, or @code{nil}@br{}
    @code{size} -- a stock icon size, or @code{nil}
  @end{return}
  Gets the stock icon name and size being displayed by the GtkImage. The
  storage type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_STOCK (see
  gtk_image_get_storage_type()). The returned string is owned by the GtkImage
  and should not be freed."
  (values (gtk-image-stock image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_get_animation" gtk-image-get-animation)
    (g-object gdk-pixbuf-animation)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @return{The displayed animation, or NULL if the image is empty.}
  Gets the GdkPixbufAnimation being displayed by the GtkImage. The storage
  type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_ANIMATION (see
  gtk_image_get_storage_type()). The caller of this function does not own a
  reference to the returned animation."
  (image (g-object gtk-image)))

(export 'gtk-image-get-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-icon-name))

(defun gtk-image-get-icon-name (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @begin{return}
    @code{icon-name} -- an icon name, or NULL @br{}
    @code{size} -- an icon size, or NULL
  @end{return}
  @begin{short}
    Gets the icon name and size being displayed by the GtkImage. The storage
    type of the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_ICON_NAME (see
    gtk_image_get_storage_type()). The returned string is owned by the GtkImage
    and should not be freed.
  @end{short}

  Since 2.6"
  (values (gtk-image-icon-name image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-gicon))

(defun gtk-image-get-gicon (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @begin{return}
    @code{gicon} -- a GIcon, or NULL @br{}
    @code{size} -- an icon size, or NULL
  @end{return}
  @begin{short}
    Gets the GIcon and size being displayed by the GtkImage. The storage type of
    the image must be GTK_IMAGE_EMPTY or GTK_IMAGE_GICON (see
    gtk_image_get_storage_type()). The caller of this function does not own a
    reference to the returned GIcon.
  @end{short}

  Since 2.14"
  (values (gtk-image-gicon image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_storage_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-storage-type))

(defun gtk-image-get-storage-type (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @return{image representation being used}
  Gets the type of representation being used by the GtkImage to store image
  data. If the GtkImage has no image data, the return value will be
  GTK_IMAGE_EMPTY."
  (gtk-image-storage-type image))

(export 'gtk-image-get-storage-type)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_file" gtk-image-new-from-file)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[filename]{a filename}
  @return{a new GtkImage}
  @begin{short}
    Creates a new GtkImage displaying the file filename. If the file isn't found
    or can't be loaded, the resulting GtkImage will display a \"broken image\"
    icon. This function never returns NULL, it always returns a valid GtkImage
    widget.
  @end{short}

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use
  gdk_pixbuf_new_from_file() to load the file yourself, then create the
  GtkImage from the pixbuf. (Or for animations, use
  gdk_pixbuf_animation_new_from_file()).

  The storage type (gtk_image_get_storage_type()) of the returned image is not
  defined, it will be whatever is appropriate for displaying the file."
  (filename :string))

(export 'gtk-image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-new-from-icon-set))

(defun gtk-image-new-from-icon-set (icon-set size)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[icon_set]{a GtkIconSet}
  @argument[size]{a stock icon size}
  @return{a new GtkImage}
  @begin{short}
    Creates a GtkImage displaying an icon set. Sample stock sizes are
    GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. Instead of using this
    function, usually it's better to create a GtkIconFactory, put your icon sets
    in the icon factory, add the icon factory to the list of default factories
    with gtk_icon_factory_add_default(), and then use
    gtk_image_new_from_stock(). This will allow themes to override the icon you
    ship with your application.
  @end{short}

  The GtkImage does not assume a reference to the icon set; you still need to
  unref it if you own references. GtkImage will add its own reference rather
  than adopting yours."
  (make-instance 'gtk-image
                 :icon-set icon-set
                 :icon-size size
                 :storage-type :icon-set))

(export 'gtk-image-new-from-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-new-from-pixbuf))

(defun gtk-image-new-from-pixbuf (pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[pixbuf]{a GdkPixbuf, or NULL}
  @return{a new GtkImage}
  @begin{short}
    Creates a new GtkImage displaying pixbuf. The GtkImage does not assume a
    reference to the pixbuf; you still need to unref it if you own references.
    GtkImage will add its own reference rather than adopting yours.
  @end{short}

  Note that this function just creates an GtkImage from the pixbuf. The
  GtkImage created will not react to state changes. Should you want that, you
  should use gtk_image_new_from_icon_set()."
  (make-instance 'gtk-image
                 :pixbuf pixbuf
                 :storage-type :pixbuf))

(export 'gtk-image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_stock" gtk-image-new-from-stock)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[stock_id]{a stock icon name}
  @argument[size]{a stock icon size}
  @return{a new GtkImage displaying the stock icon}
  Creates a GtkImage displaying a stock icon. Sample stock icon names are
  GTK_STOCK_OPEN, GTK_STOCK_QUIT. Sample stock sizes are GTK_ICON_SIZE_MENU,
  GTK_ICON_SIZE_SMALL_TOOLBAR. If the stock icon name isn't known, the image
  will be empty. You can register your own stock icon names, see
  gtk_icon_factory_add_default() and gtk_icon_factory_add()."
  (stock-id :string)
  (size gtk-icon-size))

(export 'gtk-image-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_animation ()
;;;
;;; GtkWidget * gtk_image_new_from_animation (GdkPixbufAnimation *animation);
;;;
;;; Creates a GtkImage displaying the given animation. The GtkImage does not
;;; assume a reference to the animation; you still need to unref it if you own
;;; references. GtkImage will add its own reference rather than adopting yours.
;;;
;;; Note that the animation frames are shown using a timeout with
;;; G_PRIORITY_DEFAULT. When using animations to indicate busyness, keep in mind
;;; that the animation will only be shown if the main loop is not busy with
;;; something that has a higher priority.
;;;
;;; animation :
;;;     an animation
;;;
;;; Returns :
;;;     a new GtkImage widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name ()
;;;
;;; GtkWidget * gtk_image_new_from_icon_name (const gchar *icon_name,
;;;                                           GtkIconSize size);
;;;
;;; Creates a GtkImage displaying an icon from the current icon theme. If the
;;; icon name isn't known, a "broken image" icon will be displayed instead. If
;;; the current icon theme is changed, the icon will be updated appropriately.
;;;
;;; icon_name :
;;;     an icon name
;;;
;;; size :
;;;     a stock icon size
;;;
;;; Returns :
;;;     a new GtkImage displaying the themed icon
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon ()
;;;
;;; GtkWidget * gtk_image_new_from_gicon (GIcon *icon, GtkIconSize size);
;;;
;;; Creates a GtkImage displaying an icon from the current icon theme. If the
;;; icon name isn't known, a "broken image" icon will be displayed instead. If
;;; the current icon theme is changed, the icon will be updated appropriately.
;;;
;;; icon :
;;;     an icon
;;;
;;; size :
;;;     a stock icon size
;;;
;;; Returns :
;;;     a new GtkImage displaying the themed icon
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file ()
;;;
;;; void gtk_image_set_from_file (GtkImage *image, const gchar *filename);
;;;
;;; See gtk_image_new_from_file() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; filename :
;;;     a filename or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_set ()
;;;
;;; void gtk_image_set_from_icon_set (GtkImage *image,
;;;                                   GtkIconSet *icon_set,
;;;                                   GtkIconSize size);
;;;
;;; See gtk_image_new_from_icon_set() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; icon_set :
;;;     a GtkIconSet
;;;
;;; size :
;;;     a stock icon size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf ()
;;;
;;; void gtk_image_set_from_pixbuf (GtkImage *image, GdkPixbuf *pixbuf);
;;;
;;; See gtk_image_new_from_pixbuf() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; pixbuf :
;;;     a GdkPixbuf or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_stock ()
;;;
;;; void gtk_image_set_from_stock (GtkImage *image,
;;;                                const gchar *stock_id,
;;;                                GtkIconSize size);
;;;
;;; See gtk_image_new_from_stock() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; stock_id :
;;;     a stock icon name
;;;
;;; size :
;;;     a stock icon size
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_animation" gtk-image-set-from-animation) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @argument[animation]{the GdkPixbufAnimation}
  Causes the GtkImage to display the given animation (or display nothing, if
  you set the animation to NULL)."
  (image (g-object gtk-image))
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-set-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name ()
;;;
;;; void gtk_image_set_from_icon_name (GtkImage *image,
;;;                                    const gchar *icon_name,
;;;                                    GtkIconSize size);
;;;
;;; See gtk_image_new_from_icon_name() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; icon_name :
;;;     an icon name
;;;
;;; size :
;;;     an icon size
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon ()
;;;
;;; void gtk_image_set_from_gicon (GtkImage *image,
;;;                                GIcon *icon,
;;;                                GtkIconSize size);
;;;
;;; See gtk_image_new_from_gicon() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; icon :
;;;     an icon
;;;
;;; size :
;;;     an icon size
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_clear" gtk-image-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-8}
  @argument[image]{a @class{gtk-image} object}
  @short{Resets the image to be empty.}

 Since 2.8"
  (image (g-object gtk-image)))

(export 'gtk-image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-new))

(defun gtk-image-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @return{a newly created GtkImage widget}
  Creates a new empty GtkImage widget."
  (make-instance 'gtk-image))

(export 'gtk-image-new)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_pixel_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-set-pixel-size))

(defun gtk-image-set-pixel-size (image pixel-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @argument[pixel_size]{the new pixel size}
  @begin{short}
    Sets the pixel size to use for named icons. If the pixel size is set to a
    value != -1, it is used instead of the icon size set by
    gtk_image_set_from_icon_name().
  @end{short}

  Since 2.6"
  (setf (gtk-image-pixel-size image) pixel-size))

(export 'gtk-image-set-pixel-size)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_pixel_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-pixel-size))

(defun gtk-image-get-pixel-size (image)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[image]{a GtkImage}
  @return{the pixel size used for named icons}
  @short{Gets the pixel size used for named icons.}

  Since 2.6"
  (gtk-image-pixel-size image))

(export 'gtk-imgage-get-pixel-size)

;;; --- End of file gtk.image.lisp ---------------------------------------------
