;;; ----------------------------------------------------------------------------
;;; gtk.style-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK+ Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; GtkStyleProvider
;;;
;;; Interface to provide style information to GtkStyleContext
;;;
;;; Synopsis
;;;
;;;     GtkStyleProviderIface
;;;     GtkStyleProvider
;;;
;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;     gtk_style_provider_get_icon_factory
;;;     gtk_style_provider_get_style
;;;     gtk_style_provider_get_style_property
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GtkStyleProvider
;;;
;;; Known Implementations
;;;
;;; GtkStyleProvider is implemented by GtkCssProvider and GtkSettings.
;;;
;;; Description
;;;
;;; GtkStyleProvider is an interface used to provide style information to a
;;; GtkStyleContext. See gtk_style_context_add_provider() and
;;; gtk_style_context_add_provider_for_screen().
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStyleProviderIface
;;;
;;; struct GtkStyleProviderIface {
;;;   GTypeInterface g_iface;
;;;
;;;   GtkStyleProperties * (* get_style) (GtkStyleProvider *provider,
;;;                                       GtkWidgetPath    *path);
;;;
;;;   gboolean (* get_style_property) (GtkStyleProvider *provider,
;;;                                    GtkWidgetPath    *path,
;;;                                    GtkStateFlags     state,
;;;                                    GParamSpec       *pspec,
;;;                                    GValue           *value);
;;;
;;;   GtkIconFactory * (* get_icon_factory) (GtkStyleProvider *provider,
;;;                      GtkWidgetPath    *path);
;;; };
;;;
;;; GTypeInterface g_iface;
;;;
;;;
;;; get_style ()
;;;     Gets a set of style information that applies to a widget path.
;;;
;;; get_style_property ()
;;;     Gets the value of a widget style property that applies to a widget path.
;;;
;;; get_icon_factory ()
;;;     Gets the icon factory that applies to a widget path.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkStyleProvider
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkStyleProvider" gtk-style-provider
  (:export t
   :type-initializer "gtk_style_provider_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-style-provider atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-style-provider 'type)
 "@version{2013-7-29}
  @sym{gtk-style-provider} is an interface used to provide style information to
  a @class{gtk-style-context}. See the functions
  @fun{gtk-style-context-add-provider} and
  @fun{gtk-style-context-add-provider-for-screen}.
  @see-class{gtk-style-context}
  @see-function{gtk-style-context-add-provider}
  @see-function{gtk-style-context-add-provider-for-screen}")

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;
;;; #define GTK_STYLE_PROVIDER_PRIORITY_FALLBACK      1
;;;
;;; The priority used for default style information that is used in the absence
;;; of themes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;
;;; #define GTK_STYLE_PROVIDER_PRIORITY_THEME     200
;;;
;;; The priority used for style information provided by themes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;
;;; #define GTK_STYLE_PROVIDER_PRIORITY_SETTINGS    400
;;;
;;; The priority used for style information provided via GtkSettings.
;;;
;;; This priority is higher than GTK_STYLE_PROVIDER_PRIORITY_THEME to let
;;; settings override themes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;
;;; #define GTK_STYLE_PROVIDER_PRIORITY_APPLICATION 600
;;;
;;; A priority that can be used when adding a GtkStyleProvider for
;;; application-specific style information.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; #define GTK_STYLE_PROVIDER_PRIORITY_USER        800
;;;
;;; The priority used for the style information from ~/.gtk-3.0.css.
;;;
;;; You should not use priorities higher than this, to give the user the last
;;; word.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_icon_factory ()
;;;
;;; GtkIconFactory * gtk_style_provider_get_icon_factory
;;;                                                 (GtkStyleProvider *provider,
;;;                                                  GtkWidgetPath *path);
;;;
;;; Returns the GtkIconFactory defined to be in use for path, or NULL if none is
;;; defined.
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; path :
;;;     GtkWidgetPath to query
;;;
;;; Returns :
;;;     The icon factory to use for path, or NULL.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style ()
;;;
;;; GtkStyleProperties * gtk_style_provider_get_style 
;;;                                                 (GtkStyleProvider *provider,
;;;                                                  GtkWidgetPath *path);
;;;
;;; Returns the style settings affecting a widget defined by path, or NULL if
;;; provider doesn't contemplate styling path.
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; path :
;;;     GtkWidgetPath to query
;;;
;;; Returns :
;;;     a GtkStyleProperties containing the style settings affecting path.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style_property ()
;;;
;;; gboolean gtk_style_provider_get_style_property (GtkStyleProvider *provider,
;;;                                                 GtkWidgetPath *path,
;;;                                                 GtkStateFlags state,
;;;                                                 GParamSpec *pspec,
;;;                                                 GValue *value);
;;;
;;; Looks up a widget style property as defined by provider for the widget
;;; represented by path.
;;;
;;; provider :
;;;     a GtkStyleProvider
;;;
;;; path :
;;;     GtkWidgetPath to query
;;;
;;; state :
;;;     state to query the style property for
;;;
;;; pspec :
;;;     The GParamSpec to query
;;;
;;; value :
;;;     return location for the property value
;;;
;;; Returns :
;;;     TRUE if the property was found and has a value, FALSE otherwise
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk-style-provider.lisp ------------------------------------
