#include "subclass.h"

#include <stdio.h>

GType gtk_custom_window_get_type (void) {
  return g_type_from_name("CustomWindow");
}

G_DEFINE_TYPE (GtkAnotherCustomWindow, gtk_another_custom_window, GTK_TYPE_CUSTOM_WINDOW)

static void
gtk_another_custom_window_set_focus (GtkWindow *window, GtkWidget *focus)
{
  fprintf(stderr, "Hello from AnotherCustomWindow set focus.\n");

  GTK_WINDOW_CLASS (gtk_another_custom_window_parent_class)->set_focus (window, focus);
}

static void
gtk_another_custom_window_class_init (GtkAnotherCustomWindowClass *klass)
{
  fprintf(stderr, "Hello from AnotherCustomWindow class init.\n");

  GtkWindowClass *window_class = (GtkWindowClass*) klass;

  window_class->set_focus = gtk_another_custom_window_set_focus;
}

static void
gtk_another_custom_window_init (GtkAnotherCustomWindow *window)
{
  fprintf(stderr, "Hello from AnotherCustomWindow init.\n");
}
