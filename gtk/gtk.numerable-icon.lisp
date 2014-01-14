
;;; GtkNumerableIcon
;;;
;;; A GIcon that allows numbered emblems
;;;
;;; Synopsis
;;;
;;;     GtkNumerableIcon
;;;
;;;     gtk_numerable_icon_new
;;;     gtk_numerable_icon_new_with_style_context
;;;     gtk_numerable_icon_get_background_gicon
;;;     gtk_numerable_icon_set_background_gicon
;;;     gtk_numerable_icon_get_background_icon_name
;;;     gtk_numerable_icon_set_background_icon_name
;;;     gtk_numerable_icon_get_count
;;;     gtk_numerable_icon_set_count
;;;     gtk_numerable_icon_get_label
;;;     gtk_numerable_icon_set_label
;;;     gtk_numerable_icon_get_style_context
;;;     gtk_numerable_icon_set_style_context
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GEmblemedIcon
;;;          +----GtkNumerableIcon
;;;
;;; Implemented Interfaces
;;;
;;; GtkNumerableIcon implements GIcon.
;;; Properties
;;;
;;;   "background-icon"          GIcon*                : Read / Write
;;;   "background-icon-name"     gchar*                : Read / Write
;;;   "count"                    gint                  : Read / Write
;;;   "label"                    gchar*                : Read / Write
;;;   "style-context"            GtkStyleContext*      : Read / Write
;;;
;;; Description
;;;
;;; GtkNumerableIcon is a subclass of GEmblemedIcon that can show a number or
;;; short string as an emblem. The number can be overlayed on top of another
;;; emblem, if desired.
;;;
;;; It supports theming by taking font and color information from a provided
;;; GtkStyleContext; see gtk_numerable_icon_set_style_context().
;;;
;;; Example 43. Typical numerable icons
;;;
;;;
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkNumerableIcon
;;;
;;; struct GtkNumerableIcon;
;;; ----------------------------------------------------------------------------



;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new ()
;;;
;;; GIcon *             gtk_numerable_icon_new              (GIcon *base_icon);
;;;
;;; Creates a new unthemed GtkNumerableIcon.
;;;
;;; base_icon :
;;;     a GIcon to overlay on
;;;
;;; Returns :
;;;     a new GIcon. [transfer full]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new_with_style_context ()
;;;
;;; GIcon *             gtk_numerable_icon_new_with_style_context
;;;                                                         (GIcon *base_icon,
;;;                                                          GtkStyleContext *context);
;;;
;;; Creates a new GtkNumerableIcon which will themed according to the passed GtkStyleContext. This is a convenience constructor that calls gtk_numerable_icon_set_style_context() internally.
;;;
;;; base_icon :
;;;     a GIcon to overlay on
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; Returns :
;;;     a new GIcon. [transfer full]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_background_gicon ()
;;;
;;; GIcon *             gtk_numerable_icon_get_background_gicon
;;;                                                         (GtkNumerableIcon *self);
;;;
;;; Returns the GIcon that was set as the base background image, or NULL if there's none. The caller of this function does not own a reference to the returned GIcon.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; Returns :
;;;     a GIcon, or NULL. [transfer none]
;;;
;;; Since 3.0
;;; gtk_numerable_icon_set_background_gicon ()
;;;
;;; void                gtk_numerable_icon_set_background_gicon
;;;                                                         (GtkNumerableIcon *self,
;;;                                                          GIcon *icon);
;;;
;;; Updates the icon to use icon as the base background image. If icon is NULL, self will go back using style information or default theming for its background image.
;;;
;;; If this method is called and an icon name was already set as background for the icon, icon will be used, i.e. the last method called between gtk_numerable_icon_set_background_gicon() and gtk_numerable_icon_set_background_icon_name() has always priority.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; icon :
;;;     a GIcon, or NULL. [allow-none]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_background_icon_name ()
;;;
;;; const gchar *       gtk_numerable_icon_get_background_icon_name
;;;                                                         (GtkNumerableIcon *self);
;;;
;;; Returns the icon name used as the base background image, or NULL if there's none.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; Returns :
;;;     an icon name, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_background_icon_name ()
;;;
;;; void                gtk_numerable_icon_set_background_icon_name
;;;                                                         (GtkNumerableIcon *self,
;;;                                                          const gchar *icon_name);
;;;
;;; Updates the icon to use the icon named icon_name from the current icon theme as the base background image. If icon_name is NULL, self will go back using style information or default theming for its background image.
;;;
;;; If this method is called and a GIcon was already set as background for the icon, icon_name will be used, i.e. the last method called between gtk_numerable_icon_set_background_icon_name() and gtk_numerable_icon_set_background_gicon() has always priority.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; icon_name :
;;;     an icon name, or NULL. [allow-none]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_count ()
;;;
;;; gint                gtk_numerable_icon_get_count        (GtkNumerableIcon *self);
;;;
;;; Returns the value currently displayed by self.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; Returns :
;;;     the currently displayed value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_count ()
;;;
;;; void                gtk_numerable_icon_set_count        (GtkNumerableIcon *self,
;;;                                                          gint count);
;;;
;;; Sets the currently displayed value of self to count.
;;;
;;; The numeric value is always clamped to make it two digits, i.e. between -99 and 99. Setting a count of zero removes the emblem. If this method is called, and a label was already set on the icon, it will automatically be reset to NULL before rendering the number, i.e. the last method called between gtk_numerable_icon_set_count() and gtk_numerable_icon_set_label() has always priority.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; count :
;;;     a number between -99 and 99
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_label ()
;;;
;;; const gchar *       gtk_numerable_icon_get_label        (GtkNumerableIcon *self);
;;;
;;; Returns the currently displayed label of the icon, or NULL.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; Returns :
;;;     the currently displayed label
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_label ()
;;;
;;; void                gtk_numerable_icon_set_label        (GtkNumerableIcon *self,
;;;                                                          const gchar *label);
;;;
;;; Sets the currently displayed value of self to the string in label. Setting an empty label removes the emblem.
;;;
;;; Note that this is meant for displaying short labels, such as roman numbers, or single letters. For roman numbers, consider using the Unicode characters U+2160 - U+217F. Strings longer than two characters will likely not be rendered very well.
;;;
;;; If this method is called, and a number was already set on the icon, it will automatically be reset to zero before rendering the label, i.e. the last method called between gtk_numerable_icon_set_label() and gtk_numerable_icon_set_count() has always priority.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; label :
;;;     a short label, or NULL. [allow-none]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_get_style_context ()
;;;
;;; GtkStyleContext *   gtk_numerable_icon_get_style_context
;;;                                                         (GtkNumerableIcon *self);
;;;
;;; Returns the GtkStyleContext used by the icon for theming, or NULL if there's none.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; Returns :
;;;     a GtkStyleContext, or NULL. This object is internal to GTK+ and should not be unreffed. Use g_object_ref() if you want to keep it around. [transfer none]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_set_style_context ()
;;;
;;; void                gtk_numerable_icon_set_style_context
;;;                                                         (GtkNumerableIcon *self,
;;;                                                          GtkStyleContext *style);
;;;
;;; Updates the icon to fetch theme information from the given GtkStyleContext.
;;;
;;; self :
;;;     a GtkNumerableIcon
;;;
;;; style :
;;;     a GtkStyleContext
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-icon" property
;;;
;;;   "background-icon"          GIcon*                : Read / Write
;;;
;;; The icon for the number emblem background.
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-icon-name" property
;;;
;;;   "background-icon-name"     gchar*                : Read / Write
;;;
;;; The icon name for the number emblem background.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "count" property
;;;
;;;   "count"                    gint                  : Read / Write
;;;
;;; The count of the emblem currently displayed.
;;;
;;; Allowed values: [-99,99]
;;;
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;;
;;;   "label"                    gchar*                : Read / Write
;;;
;;; The label to be displayed over the icon.
;;;
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-context" property
;;;
;;;   "style-context"            GtkStyleContext*      : Read / Write
;;;
;;; The style context to theme the icon appearance.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.numerable-icon.lisp ------------------------------------
