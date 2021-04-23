;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.animation.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; Animations
;;;
;;;     Animated images.
;;;
;;; Types and Values
;;;
;;;     GdkPixbufAnimation
;;;     GdkPixbufAnimationIter
;;;     GdkPixbufSimpleAnim
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_animation_new_from_file
;;;     gdk_pixbuf_animation_new_from_resource
;;;     gdk_pixbuf_animation_new_from_stream
;;;     gdk_pixbuf_animation_new_from_stream_async
;;;     gdk_pixbuf_animation_new_from_stream_finish
;;;     gdk_pixbuf_animation_ref
;;;     gdk_pixbuf_animation_unref
;;;     gdk_pixbuf_animation_get_width
;;;     gdk_pixbuf_animation_get_height
;;;     gdk_pixbuf_animation_get_iter
;;;     gdk_pixbuf_animation_is_static_image
;;;     gdk_pixbuf_animation_get_static_image
;;;     gdk_pixbuf_animation_iter_advance
;;;     gdk_pixbuf_animation_iter_get_delay_time
;;;     gdk_pixbuf_animation_iter_on_currently_loading_frame
;;;     gdk_pixbuf_animation_iter_get_pixbuf
;;;
;;;     gdk_pixbuf_simple_anim_new
;;;     gdk_pixbuf_simple_anim_add_frame
;;;     gdk_pixbuf_simple_anim_set_loop
;;;     gdk_pixbuf_simple_anim_get_loop
;;;
;;; Properties
;;;
;;;      gboolean    loop    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GdkPixbufAnimation
;;;     │   ╰── GdkPixbufSimpleAnim
;;;     ╰── GdkPixbufAnimationIter
;;;
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufAnimationIter
;;;
;;; struct GdkPixbufAnimationIter;
;;;
;;; An opaque struct representing an iterator which points to a certain position
;;; in an animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkPixbufSimpleAnim
;;;
;;; typedef struct _GdkPixbufSimpleAnim GdkPixbufSimpleAnim;
;;;
;;; An opaque struct representing a simple animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPixbufAnimation
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkPixbufAnimation" gdk-pixbuf-animation
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_animation_get_type")
  ((loop
    gdk-pixbuf-animation-loop
    "loop" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-pixbuf-animation 'type)
 "@version{2020-11-22}
  @begin{short}
    The @sym{gdk-pixbuf} library provides a simple mechanism to load and
    represent animations.
  @end{short}
  An animation is conceptually a series of frames to be displayed over time.
  The animation may not be represented as a series of frames internally. For
  example, it may be stored as a sprite and instructions for moving the sprite
  around a background. To display an animation you do not need to understand
  its representation, however; you just ask @class{gdk-pixbuf} what should be
  displayed at a given point in time.
  @see-slot{gdk-pixbuf-animation-loop}
  @see-class{gdk-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "loop" 'gdk-pixbuf-animation) 't)
 "The @code{loop} property of type @code{:boolean} (Read / Write) @br{}
  Whether the animation should loop when it reaches the end. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-pixbuf-animation-loop atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gdk-pixbuf-animation-loop 'function)
 "@version{2020-11-22}
  @syntax[]{(gdk-pixbuf-animation-loop object) => loop}
  @syntax[]{(setf (gdk-pixbuf-animation-loop object) loop)}
  @argument[object]{a @class{gdk-pixbuf-animation} structure}
  @argument[loop]{a boolean whether the animation should loop}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf-animation]{loop} of the
    @class{gdk-pixbuf-animation} class.
  @end{short}

  Whether the animation should loop when it reaches the end.
  @see-class{gdk-pixbuf-animation}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_animation_new_from_file"
          %gdk-pixbuf-animation-new-from-file) (g-object gdk-pixbuf-animation)
  (filename :string)
  (error :pointer))

(defun gdk-pixbuf-animation-new-from-file (filename)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @argument[filename]{a string with the name of file to load, in the GLib file
    name encoding}
  @begin{return}
    A newly created animation with a reference count of 1, or @code{nil} if any
    of several error conditions ocurred: the file could not be opened, there
    was no loader for the file's format, there was not enough memory to
    allocate the image buffer, or the image file contained invalid data.
  @end{return}
  @begin{short}
    Creates a new animation by loading it from a file.
  @end{short}
  The file format is detected automatically. If the file's format does not
  support multi-frame images, then an animation with a single frame will be
  created. Possible errors are in the @code{GDK_PIXBUF_ERROR} and
  @code{G_FILE_ERROR} domains.
  @see-class{gdk-pixbuf-animation}"
  (with-g-error (err)
    (%gdk-pixbuf-animation-new-from-file filename err)))

(export 'gdk-pixbuf-animation-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_resource ()
;;;
;;; GdkPixbufAnimation * gdk_pixbuf_animation_new_from_resource
;;;                                                  (const char *resource_path,
;;;                                                   GError **error);
;;;
;;; Creates a new pixbuf animation by loading an image from an resource.
;;;
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set.
;;;
;;; resource_path :
;;;     the path of the resource file
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly-created animation, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer, the
;;;     stream contained invalid data, or the operation was cancelled.
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream ()
;;;
;;; GdkPixbufAnimation * gdk_pixbuf_animation_new_from_stream
;;;                                                  (GInputStream *stream,
;;;                                                   GCancellable *cancellable,
;;;                                                   GError **error);
;;;
;;; Creates a new animation by loading it from an input stream.
;;;
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the GDK_PIXBUF_ERROR and
;;; G_IO_ERROR domains.
;;;
;;; The stream is not closed.
;;;
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly-created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer, the
;;;     stream contained invalid data, or the operation was cancelled.
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream_async ()
;;;
;;; void gdk_pixbuf_animation_new_from_stream_async
;;;                                               (GInputStream *stream,
;;;                                                GCancellable *cancellable,
;;;                                                GAsyncReadyCallback callback,
;;;                                                gpointer user_data);
;;;
;;; Creates a new animation by asynchronously loading an image from an input
;;; stream.
;;;
;;; For more details see gdk_pixbuf_new_from_stream(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_animation_new_from_stream_finish() to get the
;;; result of the operation.
;;;
;;; stream :
;;;     a GInputStream from which to load the animation
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the the pixbuf is loaded
;;;
;;; user_data :
;;;     the data to pass to the callback function
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream_finish ()
;;;
;;; GdkPixbufAnimation * gdk_pixbuf_animation_new_from_stream_finish
;;;                                                 (GAsyncResult *async_result,
;;;                                                  GError **error);
;;;
;;; Finishes an asynchronous pixbuf animation creation operation started with
;;; gdk_pixbuf_animation_new_from_stream_async().
;;;
;;; async_result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GdkPixbufAnimation or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_ref ()
;;;
;;; GdkPixbufAnimation * gdk_pixbuf_animation_ref
;;;                                              (GdkPixbufAnimation *animation)
;;;
;;; Warning
;;;
;;; gdk_pixbuf_animation_ref has been deprecated since version 2.0 and should
;;; not be used in newly-written code. Use g_object_ref().
;;;
;;; Adds a reference to an animation.
;;;
;;; animation :
;;;     An animation.
;;;
;;; Returns :
;;;     The same as the animation argument.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_unref ()
;;;
;;; void gdk_pixbuf_animation_unref (GdkPixbufAnimation *animation);
;;;
;;; Warning
;;;
;;; gdk_pixbuf_animation_unref has been deprecated since version 2.0 and should
;;; not be used in newly-written code. Use g_object_unref().
;;;
;;; Removes a reference from an animation.
;;;
;;; animation :
;;;     An animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_width ()
;;;
;;; int gdk_pixbuf_animation_get_width (GdkPixbufAnimation *animation);
;;;
;;; Queries the width of the bounding box of a pixbuf animation.
;;;
;;; animation :
;;;     An animation.
;;;
;;; Returns :
;;;     Width of the bounding box of the animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_height ()
;;;
;;; int gdk_pixbuf_animation_get_height (GdkPixbufAnimation *animation);
;;;
;;; Queries the height of the bounding box of a pixbuf animation.
;;;
;;; animation :
;;;     An animation.
;;;
;;; Returns :
;;;     Height of the bounding box of the animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_iter ()
;;;
;;; GdkPixbufAnimationIter * gdk_pixbuf_animation_get_iter
;;;                                              (GdkPixbufAnimation *animation,
;;;                                               const GTimeVal *start_time);
;;;
;;; Get an iterator for displaying an animation. The iterator provides the
;;; frames that should be displayed at a given time. It should be freed after
;;; use with g_object_unref().
;;;
;;; start_time would normally come from g_get_current_time(), and marks the
;;; beginning of animation playback. After creating an iterator, you should
;;; immediately display the pixbuf returned by
;;; gdk_pixbuf_animation_iter_get_pixbuf(). Then, you should install a timeout
;;; (with g_timeout_add()) or by some other mechanism ensure that you'll update
;;; the image after gdk_pixbuf_animation_iter_get_delay_time() milliseconds.
;;; Each time the image is updated, you should reinstall the timeout with the
;;; new, possibly-changed delay time.
;;;
;;; As a shortcut, if start_time is NULL, the result of g_get_current_time()
;;; will be used automatically.
;;;
;;; To update the image (i.e. possibly change the result of
;;; gdk_pixbuf_animation_iter_get_pixbuf() to a new frame of the animation),
;;; call gdk_pixbuf_animation_iter_advance().
;;;
;;; If you're using GdkPixbufLoader, in addition to updating the image after
;;; the delay time, you should also update it whenever you receive the
;;; area_updated signal and
;;; gdk_pixbuf_animation_iter_on_currently_loading_frame() returns TRUE. In this
;;; case, the frame currently being fed into the loader has received new data,
;;; so needs to be refreshed. The delay time for a frame may also be modified
;;; after an area_updated signal, for example if the delay time for a frame is
;;; encoded in the data after the frame itself. So your timeout should be
;;; reinstalled after any area_updated signal.
;;;
;;; A delay time of -1 is possible, indicating "infinite."
;;;
;;; animation :
;;;     a GdkPixbufAnimation
;;;
;;; start_time :
;;;     time when the animation starts playing. [allow-none]
;;;
;;; Returns :
;;;     an iterator to move over the animation. [transfer full]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_is_static_image ()
;;;
;;; gboolean gdk_pixbuf_animation_is_static_image
;;;                                              (GdkPixbufAnimation *animation)
;;;
;;; If you load a file with gdk_pixbuf_animation_new_from_file() and it turns
;;; out to be a plain, unanimated image, then this function will return TRUE.
;;; Use gdk_pixbuf_animation_get_static_image() to retrieve the image.
;;;
;;; animation :
;;;     a GdkPixbufAnimation
;;;
;;; Returns :
;;;     TRUE if the "animation" was really just an image
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_static_image ()
;;; -> gdk-pixbuf-animation-static-image
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_animation_get_static_image"
           gdk-pixbuf-animation-static-image) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-22}
  @argument[animation]{a @class{gdk-pixbuf-animation} structure}
  @return{Unanimated @class{gdk-pixbuf} image representing the animation.}
  @begin{short}
    If an animation is really just a plain image, has only one frame, this
    function returns that image.
  @end{short}
  If the animation is an animation, this function returns a reasonable thing to
  display as a static unanimated image, which might be the first frame, or
  something more sophisticated. If an animation has not loaded any frames yet,
  this function will return @code{nil}.
  @see-class{gdk-pixbuf}
  @see-class{gdk-pixbuf-animation}"
  (animation (g-object gdk-pixbuf-animation)))

(export 'gdk-pixbuf-animation-static-image)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_advance ()
;;;
;;; gboolean gdk_pixbuf_animation_iter_advance (GdkPixbufAnimationIter *iter,
;;;                                             const GTimeVal *current_time);
;;;
;;; Possibly advances an animation to a new frame. Chooses the frame based on
;;; the start time passed to gdk_pixbuf_animation_get_iter().
;;;
;;; current_time would normally come from g_get_current_time(), and must be
;;; greater than or equal to the time passed to gdk_pixbuf_animation_get_iter(),
;;; and must increase or remain unchanged each time
;;; gdk_pixbuf_animation_iter_get_pixbuf() is called. That is, you can't go
;;; backward in time; animations only play forward.
;;;
;;; As a shortcut, pass NULL for the current time and g_get_current_time() will
;;; be invoked on your behalf. So you only need to explicitly pass current_time
;;; if you're doing something odd like playing the animation at double speed.
;;;
;;; If this function returns FALSE, there's no need to update the animation
;;; display, assuming the display had been rendered prior to advancing; if TRUE,
;;; you need to call gdk_pixbuf_animation_iter_get_pixbuf() and update the
;;; display with the new pixbuf.
;;;
;;; iter :
;;;     a GdkPixbufAnimationIter
;;;
;;; current_time :
;;;     current time. [allow-none]
;;;
;;; Returns :
;;;     TRUE if the image may need updating
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_get_delay_time ()
;;;
;;; int gdk_pixbuf_animation_iter_get_delay_time (GdkPixbufAnimationIter *iter)
;;;
;;; Gets the number of milliseconds the current pixbuf should be displayed, or
;;; -1 if the current pixbuf should be displayed forever. g_timeout_add()
;;; conveniently takes a timeout in milliseconds, so you can use a timeout to
;;; schedule the next update.
;;;
;;; iter :
;;;     an animation iterator
;;;
;;; Returns :
;;;     delay time in milliseconds (thousandths of a second)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_on_currently_loading_frame ()
;;;
;;; gboolean gdk_pixbuf_animation_iter_on_currently_loading_frame
;;;                                               (GdkPixbufAnimationIter *iter)
;;;
;;; Used to determine how to respond to the area_updated signal on
;;; GdkPixbufLoader when loading an animation. area_updated is emitted for an
;;; area of the frame currently streaming in to the loader. So if you're on the
;;; currently loading frame, you need to redraw the screen for the updated area.
;;;
;;; iter :
;;;     a GdkPixbufAnimationIter
;;;
;;; Returns :
;;;     TRUE if the frame we are on is partially loaded, or the last frame.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_get_pixbuf ()
;;;
;;; GdkPixbuf * gdk_pixbuf_animation_iter_get_pixbuf
;;;                                               (GdkPixbufAnimationIter *iter)
;;;
;;; Gets the current pixbuf which should be displayed; the pixbuf might not be
;;; the same size as the animation itself (gdk_pixbuf_animation_get_width(),
;;; gdk_pixbuf_animation_get_height()). This pixbuf should be displayed for
;;; gdk_pixbuf_animation_iter_get_delay_time() milliseconds. The caller of this
;;; function does not own a reference to the returned pixbuf; the returned
;;; pixbuf will become invalid when the iterator advances to the next frame,
;;; which may happen anytime you call gdk_pixbuf_animation_iter_advance(). Copy
;;; the pixbuf to keep it (don't just add a reference), as it may get recycled
;;; as you advance the iterator.
;;;
;;; iter :
;;;     an animation iterator
;;;
;;; Returns :
;;;     the pixbuf to be displayed. [transfer none]
;;; GdkPixbufSimpleAnim
;;;
;;; typedef struct _GdkPixbufSimpleAnim GdkPixbufSimpleAnim;
;;;
;;; An opaque struct representing a simple animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_new ()
;;;
;;; GdkPixbufSimpleAnim * gdk_pixbuf_simple_anim_new (gint width,
;;;                                                   gint height,
;;;                                                   gfloat rate);
;;;
;;; Creates a new, empty animation.
;;;
;;; width :
;;;     the width of the animation
;;;
;;; height :
;;;     the height of the animation
;;;
;;; rate :
;;;     the speed of the animation, in frames per second
;;;
;;; Returns :
;;;     a newly allocated GdkPixbufSimpleAnim
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_add_frame ()
;;;
;;; void gdk_pixbuf_simple_anim_add_frame (GdkPixbufSimpleAnim *animation,
;;;                                        GdkPixbuf *pixbuf);
;;;
;;; Adds a new frame to animation. The pixbuf must have the dimensions specified
;;; when the animation was constructed.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; pixbuf :
;;;     the pixbuf to add
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_set_loop ()
;;;
;;; void gdk_pixbuf_simple_anim_set_loop (GdkPixbufSimpleAnim *animation,
;;;                                       gboolean loop);
;;;
;;; Sets whether animation should loop indefinitely when it reaches the end.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; loop :
;;;     whether to loop the animation
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_get_loop ()
;;;
;;; gboolean gdk_pixbuf_simple_anim_get_loop (GdkPixbufSimpleAnim *animation);
;;;
;;; Gets whether animation should loop indefinitely when it reaches the end.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; Returns :
;;;     TRUE if the animation loops forever, FALSE otherwise
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.animation.lisp ----------------------------------
