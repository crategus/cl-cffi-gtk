;;; ----------------------------------------------------------------------------
;;; gtk.image.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; GtkImage
;;;
;;;     A widget displaying an image
;;;
;;; Types and Values
;;;
;;;     GtkImage
;;;     GtkImageType
;;;
;;; Functions
;;;
;;;     gtk_image_get_icon_set                             * deprecated
;;;     gtk_image_get_pixbuf                                 
;;;     gtk_image_get_stock                                * deprecated
;;;     gtk_image_get_animation                              
;;;     gtk_image_get_icon_name                              
;;;     gtk_image_get_gicon                                  
;;;     gtk_image_get_storage_type                           
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_icon_set                        * deprecated
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_stock                           * deprecated
;;;     gtk_image_new_from_animation
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_new_from_resource
;;;     gtk_image_new_from_surface ()
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_icon_set                        * deprecated
;;;     gtk_image_set_from_pixbuf
;;;     gtk_image_set_from_stock                           * deprecated
;;;     gtk_image_set_from_animation
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;     gtk_image_set_from_resource
;;;     gtk_image_set_from_surface ()
;;;     gtk_image_clear
;;;     gtk_image_new
;;;     gtk_image_set_pixel_size
;;;     gtk_image_get_pixel_size
;;;
;;; Properties
;;;
;;;                  gchar*  file                Read / Write
;;;                  GIcon*  gicon               Read / Write
;;;                  gchar*  icon-name           Read / Write
;;;             GtkIconSet*  icon-set            Read / Write
;;;                   gint   icon-size           Read / Write
;;;              GdkPixbuf*  pixbuf              Read / Write
;;;     GdkPixbufAnimation*  pixbuf-animation    Read / Write
;;;                   gint   pixel-size          Read / Write
;;;                  gchar*  resource            Read / Write
;;;                  gchar*  stock               Read / Write
;;;           GtkImageType   storage-type        Read
;;;           CairoSurface*  surface             Read / Write
;;;               gboolean   use-fallback        Read / Write

;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkImage
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImage implements AtkImplementorIface and GtkBuildable.
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
   #+gtk-3-8
   (resource
    gtk-image-resource
    "resource" "gchararray" t t)
   (stock
    gtk-image-stock
    "stock" "gchararray" t t)
   (storage-type
    gtk-image-storage-type
    "storage-type" "GtkImageType" t nil)
   #+gtk-3-10
   (surface
    gtk-image-surface
    "surface" "CairoSurface" t t)
   (use-fallback
    gtk-image-use-fallback
    "use-fallback" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-image 'type)
 "@version{2013-4-21}
  @begin{short}
    The @sym{gtk-image} widget displays an image. Various kinds of object can be
    displayed as an image; most typically, you would load a @class{gdk-pixbuf}
    object (\"pixel buffer\") from a file, and then display that.
  @end{short}
  There is a convenience function to do this, @fun{gtk-image-new-from-file},
  used as follows:
  @begin{pre}
 GtkWidget *image;
 image = gtk_image_new_from_file (\"myfile.png\");
  @end{pre}
  If the file is not loaded successfully, the image will contain a
  \"broken image\" icon similar to that used in many web browsers. If you want
  to handle errors in loading the file yourself, for example by displaying an
  error message, then load the image with the function
  @fun{gdk-pixbuf-new-from-file}, then create the @sym{gtk-image} with
  the function @fun{gtk-image-new-from-pixbuf}.

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
  using the function @fun{gdk-pixbuf-new-from-inline}.
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
  @see-slot{gtk-image-surface}
  @see-slot{gtk-image-use-fallback}
  @see-class{gtk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-image-file ---------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "file" 'gtk-image) 't)
 "The @code{file} property of type @code{:string} (Read / Write) @br{}
  Filename to load and display. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-file atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-file 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{file} of the @class{gtk-image} class.
  @see-class{gtk-image}")

;;; --- gtk-image-gicon --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'gtk-image) 't)
 "The @code{gicon} property of type @class{g-icon} (Read / Write) @br{}
  The @class{g-icon} displayed in the @sym{gtk-image}. For themed icons, If the
  icon theme is changed, the image will be updated automatically.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-gicon 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{gicon} of the @class{gtk-image} class.
  @see-class{gtk-image}")

;;; --- gtk-image-icon-name ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-image) 't)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-name 'function)
 "@version{2013-9-11}
  Accessor of the slot @slot[gtk-image]{icon-name} of the @class{gtk-image}
  class.
  @see-class{gtk-image}
  @see-function{gtk-image-get-icon-name}")

;;; --- gtk-image-icon-set -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-set" 'gtk-image) 't)
 "The @code{icon-set} property of type @class{gtk-icon-set}
  (Read / Write) @br{}
  Icon set to display. @br{}
  @b{Warning:} @code{icon-set} has been deprecated since version 3.10 and should
  not be used in newly-written code. Use @code{icon-name} instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-set 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{icon-set} of the @class{gtk-image}
  class.
  @see-class{gtk-image}
  @see-function{gtk-image-get-icon-set}")

;;; --- gtk-image-icon-size ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-size" 'gtk-image) 't)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  Symbolic size to use for stock icon, icon set or named icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 4")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-icon-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-icon-size 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{icon-size} of the @class{gtk-image}
  class.
  @see-class{gtk-image}")

;;; --- gtk-image-pixbuf -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf" 'gtk-image) 't)
 "The @code{pixbuf} property of type @class{gdk-pixbuf} (Read / Write) @br{}
  A @class{gdk-pixbuf} to display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf 'function)
 "@version{2014-3-7}
  @argument[object]{a @class{gtk-image} widget}
  @begin{short}
    Accessor of the slot @slot[gtk-image]{pixbuf} of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-pixbuf} gets the @class{gdk-pixbuf}
  object being displayed by the @class{gtk-image}.

  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:pixbuf}, see the slot accessfunction @fun{gtk-image-storage-type}.
  The caller of this function does not own a reference to the returned pixbuf.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}")

;;; --- gtk-image-pixbuf-animation ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixbuf-animation"
                                               'gtk-image) 't)
 "The @code{pixbuf-animation} property of type @class{gdk-pixbuf-animation}
  (Read / Write) @br{}
  @class{gdk-pixbuf-animation} to display.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixbuf-animation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixbuf-animation 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{pixbuf-animation} of the
  @class{gtk-image} class.
  @see-class{gtk-image}
  @see-function{gtk-image-get-animation}")

;;; --- gtk-image-pixel-size ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixel-size" 'gtk-image) 't)
 "The @code{pixel-size} property of type @code{:int} (Read / Write) @br{}
  The @code{pixel-size} property can be used to specify a fixed size
  overriding the @code{\"icon-size\"} property for images of type
  @code{:icon-name}. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-pixel-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-pixel-size 'function)
 "@version{2014-3-7}
  @argument[object]{a @class{gtk-image} widget}
  @argument[pixel-size]{the new pixel size}
  @syntax[]{(gtk-image-pixel-size object) => pixel-size}
  @syntax[]{(setf (gtk-image-pixel-size object) pixel-size)}
  @begin{short}
    Accessor of the slot @slot[gtk-image]{pixel-size} of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-pixel-size} sets the pixel size used
  for named icons.

  The slot access function @sym{(setf gtk-image-pixel-size)} sets the pixel size
  to use for named icons. If the pixel size is set to a value not equal to -1,
  it is used instead of the icon size set by the function
  @fun{gtk-image-set-from-icon-name}.
  @see-class{gtk-image}
  @see-function{gtk-image-set-from-icon-name}")

;;; --- gtk-image-resource -----------------------------------------------------

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "resource" 'gtk-image) 't)
 "The @code{\"resource\"} property of type @code{:string} (Read / Write) @br{}
  A path to a resource file to display. @br{}
  Default value: @code{nil} @br{}
  Since 3.8")

#+(and gtk-3-8 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-image-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-resource 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{stock} of the @class{gtk-image} class.
  @see-class{gtk-image}")

;;; --- gtk-image-stock --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock" 'gtk-image) 't)
 "The @code{stock} property of type @code{:string} (Read / Write) @br{}
  Stock ID for a stock image to display. @br{}
  @b{Warning:} @code{stock} has been deprecated since version 3.10 and should
  not be used in newly-written code. Use @code{icon-name} instead. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-stock atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-stock 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{stock} of the @class{gtk-image} class.
  @see-class{gtk-image}
  @see-function{gtk-image-get-stock}")

;;; --- gtk-image-storage-type -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "storage-type" 'gtk-image) 't)
 "The @code{storage-type} property of type @symbol{gtk-image-type}
  (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-storage-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-storage-type 'function)
 "@version{2014-3-7}
  @argument[object]{a @class{gtk-image} widget}
  @begin{short}
    Accessor of the slot @slot[gtk-image]{storage-type} of the @class{gtk-image}
    class.
  @end{short}

  The slot access function @sym{gtk-image-storage-type} gets the type of
  representation being used by the @class{gtk-image} widget to store image data.
  If the @class{gtk-image} widget has no image data, the return value will be
  @code{:empty}.
  @see-class{gtk-image}
  @see-symbol{gtk-image-type}")

;;; --- gtk-image-surface ------------------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "surface" 'gtk-image) 't)
 "The @code{surface} property of type @code{:string} (Read / Write) @br{}
  A @symbol{cairo-surface-t} to display.")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-image-surface atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-surface 'function)
 "@version{2014-10-27}
  Accessor of the slot @slot[gtk-image]{surface} of the @class{gtk-image} class.
  Since 3.10
  @see-class{gtk-image}
  @see-symbol{cairo-surface-t}")

;;; --- gtk-image-use-fallback -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-fallback" 'gtk-image) 't)
 "The @code{\"use-fallback\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the icon displayed in the @sym{gtk-image} will use standard icon names
  fallback. The value of this property is only relevant for images of type
  @code{:icon-name} and @code{:gicon}. @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-use-fallback atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-image-use-fallback 'function)
 "@version{2014-3-7}
  Accessor of the slot @slot[gtk-image]{use-fallback} of the @class{gtk-image}
  class.
  @see-class{gtk-image}")

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
  (:gicon 6)
  #+gtk-3-10
  (:surface 7)
  )

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-image-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-image-type atdoc:*external-symbols*)
 "@version{2014-10-27}
  @begin{short}
    Describes the image data representation used by a @class{gtk-image}.
  @end{short}
  If you want to get the image from the widget, you can only get the
  currently stored representation. e. g. if the slot access function
  @fun{gtk-image-storage-type} returns @code{:pixbuf}, then you can call the
  function @fun{gtk-image-pixbuf} but not @fun{gtk-image-get-stock}. For
  empty images, you can request any storage type (call any of the \"get\"
  functions), but they will all return @code{nil} values.
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
  (:gicon 6)
  (:surface 7))
  @end{pre}
  @begin[code]{table}
    @entry[:empty]{There is no image displayed by the widget.}
    @entry[:pixbuf]{The widget contains a @class{gdk-pixbuf} object.}
    @entry[:stock]{The widget contains a stock icon name.}
    @entry[:icon-set]{The widget contains a @class{gtk-icon-set} object.}
    @entry[:animation]{The widget contains a @class{gdk-pixbuf-animation}
      object.}
    @entry[:icon-name]{The widget contains a named icon. This image type was
      added in GTK+ 2.6.}
    @entry[:gicon]{The widget contains a @class{g-icon}. This image type was
      added in GTK+ 2.14.}
    @entry[:surface]{The widget contains a @symbol{cairo-surface-t}. This
      image type was added in GTK+ 3.10.}
  @end{table}
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-class{gdk-pixbuf-animation}
  @see-class{gtk-icon-set}
  @see-class{g-icon}
  @see-symbol{cairo-surface-t}
  @see-function{gtk-image-storage-type}
  @see-function{gtk-image-pixbuf}
  @see-function{gtk-image-get-stock}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-icon-set))

(defun gtk-image-get-icon-set (image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{icon-set} -- a @class{gtk-icon-set} object, or @code{nil} @br{}
    @code{size} -- a stock icon size, or @code{nil}
  @end{return}
  @begin{short}
    Gets the icon set and size being displayed by the @class{gtk-image}.
  @end{short}
  The storage type of the image must be @code{:empty} or @code{:icon-set}. See
  the slot access function @fun{gtk-image-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-get-icon-set} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-get-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-class{gtk-icon-set}
  @see-function{gtk-image-get-icon-name}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-icon-set image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-stock))

(defun gtk-image-get-stock (image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{stock-id} -- a stock icon name, or @code{nil} @br{}
    @code{size} -- a stock icon size of type @symbol{gtk-icon-size},
                   or @code{nil}
  @end{return}
  @begin{short}
    Gets the stock icon name and size being displayed by the @class{gtk-image}
    widget.
  @end{short}
  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:stock}, see the slot access function @fun{gtk-image-storage-type}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-get-stock} has been deprecated since version
    3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-get-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-symbol{gtk-icon-size}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}
  @see-function{gtk-image-get-icon-name}"
  (values (gtk-image-stock image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_get_animation" gtk-image-get-animation)
    (g-object gdk-pixbuf-animation)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @return{The displayed animation, or @code{nil} if the image is empty.}
  @begin{short}
    Gets the @class{gdk-pixbuf-animation} object being displayed by the
    @class{gtk-image} widget.
  @end{short}
  The @symbol{gtk-image-type} storage type of the image must be @code{:empty}
  or @code{:animation}, see the slot access function
  @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}
  @see-symbol{gtk-image-type}
  @see-function{gtk-image-storage-type}"
  (image (g-object gtk-image)))

(export 'gtk-image-get-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-icon-name))

(defun gtk-image-get-icon-name (image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} object}
  @begin{return}
    @code{icon-name} -- an icon name, or @code{nil} @br{}
    @code{size} -- an icon size, or @code{nil}
  @end{return}
  @begin{short}
    Gets the icon name and size being displayed by the @class{gtk-image} object.
  @end{short}
  The storage type of the image must be @code{:empty} or @code{:icon-name},
  see the slot access function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-icon-name image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_gicon ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-get-gicon))

(defun gtk-image-get-gicon (image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @begin{return}
    @code{gicon} -- a @class{g-icon} object, or @code{nil} @br{}
    @code{size} -- an icon size, or @code{nil}
  @end{return}
  @begin{short}
    Gets the @class{g-icon} object and @arg{size} being displayed by the
    @class{gtk-image} object.
  @end{short}
  The storage type of the image must be @code{:empty} or @code{:gicon}, see the
  slot access function @fun{gtk-image-storage-type}.
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-function{gtk-image-storage-type}"
  (values (gtk-image-gicon image)
          (gtk-image-icon-size image)))

(export 'gtk-image-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_file" gtk-image-new-from-file)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[filename]{a filename}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates a new @class{gtk-image} displaying the file filename.
  @end{short}
  If the file is not found or cannot be loaded, the resulting @class{gtk-image}
  object will display a \"broken image\" icon. This function never returns
  @code{nil}, it always returns a valid @class{gtk-image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the function
  @fun{gdk-pixbuf-new-from-file} to load the file yourself, then create the
  @class{gtk-image} object from the pixbuf. Or for animations, use the function
  @fun{gdk-pixbuf-animation-new-from-file}.

  The storage type, see the slot access function @fun{gtk-image-storage-type},
  of the returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk-image}
  @see-function{gdk-pixbuf-new-from-file}
  @see-function{gdk-pixbuf-animation-new-from-file}
  @see-function{gtk-image-storage-type}"
  (filename :string))

(export 'gtk-image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_icon_set" gtk-image-new-from-icon-set)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[icon-set]{a @class{gtk-icon-set} object}
  @argument[size]{a stock icon size}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates a @class{gtk-image} object displaying an icon set.
  @end{short}
  Sample stock sizes are @code{:menu}, @code{:small-toolbar}. Instead of using
  this function, usually it is better to create a @class{gtk-icon-factory}, put
  your icon sets in the icon factory, add the icon factory to the list of
  default factories with the function @fun{gtk-icon-factory-add-default}, and
  then use the function @fun{gtk-image-new-from-stock}. This will allow themes
  to override the icon you ship with your application.

  The @class{gtk-image} object does not assume a reference to the icon set; you
  still need to unref it if you own references. @class{gtk-image} object will
  add its own reference rather than adopting yours.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-new-from-icon-set} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-new-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-class{gtk-icon-factory}
  @see-function{gtk-icon-factory-add-default}
  @see-function{gtk-image-new-from-icon-name}
  @see-function{gtk-image-new-from-stock}"
  (icon-set (g-boxed-foreign gtk-icon-set))
  (icon-size :int))

(export 'gtk-image-new-from-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_pixbuf" gtk-image-new-from-pixbuf)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[pixbuf]{a @class{gdk-pixbuf} object, or @code{nil}}
  @return{A new @class{gtk-image} object.}
  @begin{short}
    Creates a new @class{gtk-image} object displaying pixbuf.
  @end{short}

  Note that this function just creates an @class{gtk-image} object from the
  pixbuf. The @class{gtk-image} object created will not react to state changes.
  Should you want that, you should use the function
  @fun{gtk-image-new-from-icon-name}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-function{gtk-image-new-from-icon-name}"
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_stock" gtk-image-new-from-stock)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[stock-id]{a stock icon name}
  @argument[size]{a stock icon size from the @symbol{gtk-icon-size} enumeration}
  @return{A new @class{gtk-image} widget displaying the stock icon.}
  @begin{short}
    Creates a @class{gtk-image} widget displaying a stock icon.
  @end{short}
  Sample stock icon names are @code{\"gtk-open\"}, @code{\"gtk-quit\"}. Sample
  stock sizes are @code{:menu}, @code{:small-toolbar}. If the stock icon name
  is not known, the image will be empty. You can register your own stock icon
  names, see the functions @fun{gtk-icon-factory-add-default} and
  @fun{gtk-icon-factory-add}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-new-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-new-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-icon-factory-add}
  @see-function{gtk-icon-factory-add-default}"
  (stock-id :string)
  (size gtk-icon-size))

(export 'gtk-image-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_animation" gtk-image-new-from-animation)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[animation]{an animation of type @class{gdk-pixbuf-animation}}
  @return{A new @class{gtk-image} widget.}
  @begin{short}
    Creates a @class{gtk-image} displaying the given animation.
  @end{short}

  Note that the animation frames are shown using a timeout with
  @code{:default}. When using animations to indicate busyness, keep in mind
  that the animation will only be shown if the main loop is not busy with
  something that has a higher priority.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}"
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-new-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_icon_name" gtk-image-new-from-icon-name)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[icon-name]{an icon name}
  @argument[size]{a stock icon size}
  @return{A new @class{gtk-image} widget displaying the themed icon.}
  @begin{short}
    Creates a @class{gtk-image} widget displaying an icon from the current icon
    theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk-image}
  @see-function{gtk-image-set-from-icon-name}"
  (icon-name :string)
  (size gtk-icon-size))

(export 'gtk-image-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_gicon" gtk-image-new-from-gicon)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[icon]{an icon of type @class{g-icon}}
  @argument[size]{a stock icon size of type @symbol{gtk-icon-size}}
  @return{A new @class{gtk-image} displaying the themed icon.}
  @begin{short}
    Creates a @class{gtk-image} displaying an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-symbol{gtk-icon-size}"
  (icon (g-object g-icon))
  (size gtk-icon-size))

(export 'gtk-image-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_new_from_resource" gtk-image-new-from-resource)
    (g-object gtk-image)
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[resource-path]{a resource path}
  @return{A new @class{gtk-image} widget}
  @begin{short}
    Creates a new @class{gtk-image} displaying the resource file
    @arg{resource-path}.
  @end{short}
  If the file is not found or can not be loaded, the resulting @class{gtk-image}
  will display a \"broken image\" icon. This function never returns @code{nil},
  it always returns a valid @class{gtk-image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the function
  @fun{gdk-pixbuf-new-from-file} to load the file yourself, then create the
  @class{gtk-image} from the pixbuf. Or for animations, use the function
  @fun{gdk-pixbuf-animation-new-from-file}.

  The storage type, see the function @fun{gtk-image-storage-type}, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.

  Since 3.4
  @see-class{gtk-image}
  @see-function{gdk-pixbuf-new-from-file}
  @see-function{gdk-pixbuf-animation-new-from-file}"
  (resource-path :string))

(export 'gtk-image-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_surface ()
;;;
;;; GtkWidget *
;;; gtk_image_new_from_surface (cairo_surface_t *surface);
;;;
;;; Creates a new GtkImage displaying surface . The GtkImage does not assume a
;;; reference to the surface; you still need to unref it if you own references.
;;; GtkImage will add its own reference rather than adopting yours.
;;;
;;; surface :
;;;     a cairo_surface_t, or NULL.
;;;
;;; Returns :
;;;     a new GtkImage
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_file" gtk-image-set-from-file) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[image]{a @class{gtk-image} widget}
  @argument[filename]{a filename or @code{nil}}
  See the function @fun{gtk-image-new-from-file} for details.
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-file}"
  (image (g-object gtk-image))
  (filename :string))

(export 'gtk-image-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_icon_set" gtk-image-set-from-icon-set) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon-set]{a @class{gtk-icon-set} structure}
  @argument[size]{a stock icon size}
  @begin{short}
    See the function @fun{gtk-image-new-from-icon-set} for details.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-set-from-icon-set} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-set-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-icon-set}
  @see-function{gtk-image-set-from-icon-name}"
  (image (g-object gtk-image))
  (icon-set (g-boxed-foreign gtk-icon-set))
  (size gtk-icon-size))

(export 'gtk-image-set-from-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_pixbuf" gtk-image-set-from-pixbuf) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf} object or @code{nil}}
  See the function @fun{gtk-image-new-from-pixbuf} for details.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf}
  @see-function{gtk-image-new-from-pixbuf}"
  (image (g-object gtk-image))
  (pixbuf (g-object gdk-pixbuf)))

(export 'gtk-image-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_stock" gtk-image-set-from-stock) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @argument[stock-id]{a stock icon name}
  @argument[size]{a stock icon size}
  @begin{short}
    See the function @fun{gtk-image-new-from-stock} for details.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-image-set-from-stock} has been deprecated since
    version 3.10 and should not be used in newly-written code. Use the function
    @fun{gtk-image-set-from-icon-name} instead.
  @end{dictionary}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-stock}
  @see-function{gtk-image-set-from-icon-name}"
  (image (g-object gtk-image))
  (stock-id :string)
  (size gtk-icon-size))

(export 'gtk-image-set-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_animation ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_animation" gtk-image-set-from-animation) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} object}
  @argument[animation]{the @class{gdk-pixbuf-animation} object}
  Causes the @class{gtk-image} object to display the given animation, or display
  nothing, if you set the animation to @code{nil}.
  @see-class{gtk-image}
  @see-class{gdk-pixbuf-animation}"
  (image (g-object gtk-image))
  (animation (g-object gdk-pixbuf-animation)))

(export 'gtk-image-set-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_icon_name" gtk-image-set-from-icon-name) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon-name]{an icon name}
  @argument[size]{an icon size}
  @begin{short}
    See the function @fun{gtk-image-new-from-icon-name} for details.
  @end{short}
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-icon-name}"
  (image (g-object gtk-image))
  (icon-name :string)
  (size gtk-icon-size))

(export 'gtk-image-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_gicon" gtk-image-set-from-gicon) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[image]{a @class{gtk-image} widget}
  @argument[icon]{an icon}
  @argument[size]{an icon size}
  @begin{short}
    See the funcion @fun{gtk-image-new-from-gicon} for details.
  @end{short}
  @see-class{gtk-image}
  @see-class{g-icon}
  @see-symbol{gtk-icon-size}
  @see-function{gtk-image-new-from-gicon}"
  (image (g-object gtk-image))
  (icon (g-object g-icon))
  (size gtk-icon-size))

(export 'gtk-image-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_set_from_resource" gtk-image-set-from-resource) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-24}
  @argument[image]{a @class{gtk-image} widget}
  @argument[resource-path]{a resource path or @code{nil}}
  See the function @fun{gtk-image-new-from-resource} for details.
  @see-class{gtk-image}
  @see-function{gtk-image-new-from-resource}"
  (image (g-object gtk-image))
  (resource-path :string))

(export 'gtk-image-set-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_surface ()
;;;
;;; void
;;; gtk_image_set_from_surface (GtkImage *image,
;;;                             cairo_surface_t *surface);
;;;
;;; See gtk_image_new_from_surface() for details.
;;;
;;; image :
;;;     a GtkImage
;;;
;;; surface :
;;;     a cairo_surface_t or NULL.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_image_clear" gtk-image-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @argument[image]{a @class{gtk-image} object}
  @short{Resets the image to be empty.}
 @see-class{gtk-image}"
  (image (g-object gtk-image)))

(export 'gtk-image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-image-new))

(defun gtk-image-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-10-27}
  @return{A newly created @class{gtk-image} object.}
  Creates a new empty @class{gtk-image} widget.
  @see-class{gtk-image}"
  (make-instance 'gtk-image))

(export 'gtk-image-new)

;;; --- End of file gtk.image.lisp ---------------------------------------------
