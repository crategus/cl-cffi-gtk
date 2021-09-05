
(in-package :gio)

;;;GFileIcon

;;;GFileIcon — Icons pointing to an image file
;;;Functions

;;;GIcon *	g_file_icon_new ()
;;;GFile *	g_file_icon_get_file ()

;;;Properties

;;;GFile *	file
;;;Types and Values

;;; 	GFileIcon
;;;Object Hierarchy

;;;    GObject
;;;    ╰── GFileIcon
;;;Implemented Interfaces

;;;GFileIcon implements GIcon and GLoadableIcon.

;;;Includes

;;;#include <gio/gio.h>
;;;Description

;;;GFileIcon specifies an icon by pointing to an image file to be used as icon.

;;; ----------------------------------------------------------------------------
;;; g_file_icon_new ()
;;;
;;; GIcon *
;;; g_file_icon_new (GFile *file);
;;;
;;; Creates a new icon for a file.
;;;
;;; file :
;;;     a GFile
;;;
;;; Returns :
;;;     a GIcon for the given file , or NULL on error.
;;; ----------------------------------------------------------------------------

(defcfun ("g_file_icon_new" g-file-icon-new) (g-object g-icon)
  (file (g-object g-file)))

(export 'g-file-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_file_icon_get_file ()
;;;
;;; GFile *
;;; g_file_icon_get_file (GFileIcon *icon);
;;;
;;; Gets the GFile associated with the given icon .
;;;
;;; icon :
;;;     a GIcon
;;;
;;; Returns :
;;;     a GFile
;;; ----------------------------------------------------------------------------

;;;Types and Values

;;;GFileIcon

;;;typedef struct _GFileIcon GFileIcon;
;;;Gets an icon for a GFile. Implements GLoadableIcon.

;;;Property Details

;;;The “file” property

;;;  “file”                     GFile *
;;;The file containing the icon.

;;;Owner: GFileIcon

;;;Flags: Read / Write / Construct Only

;;;See Also

;;;GIcon, GLoadableIcon

