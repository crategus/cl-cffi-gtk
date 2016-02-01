#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GTK_TYPE_CUSTOM_WINDOW (gtk_custom_window_get_type ())
#define GTK_CUSTOM_WINDOW(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_CUSTOM_WINDOW, GtkCustomWindow))
#define GTK_CUSTOM_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_CUSTOM_WINDOW, GtkCustomWindowClass))

typedef struct _GtkCustomWindowClass GtkCustomWindowClass;
typedef struct _GtkCustomWindow GtkCustomWindow;

struct _GtkCustomWindow
{
  GtkWindow window;
};

struct _GtkCustomWindowClass
{
  GtkWindowClass parent_class;
};

GDK_AVAILABLE_IN_ALL
GType gtk_custom_window_get_type (void) G_GNUC_CONST;

#define GTK_TYPE_ANOTHER_CUSTOM_WINDOW (gtk_another_custom_window_get_type ())

G_DEFINE_AUTOPTR_CLEANUP_FUNC(GtkCustomWindow, g_object_unref)

typedef struct _GtkAnotherCustomWindow GtkAnotherCustomWindow;
typedef struct _GtkAnotherCustomWindowClass GtkAnotherCustomWindowClass;

struct _GtkAnotherCustomWindow
{
  GtkCustomWindow window;
};

struct _GtkAnotherCustomWindowClass
{
  GtkCustomWindowClass parent_class;
};

GDK_AVAILABLE_IN_ALL
GType gtk_another_custom_window_get_type (void) G_GNUC_CONST;

G_DEFINE_AUTOPTR_CLEANUP_FUNC(GtkAnotherCustomWindow, g_object_unref)

G_END_DECLS
