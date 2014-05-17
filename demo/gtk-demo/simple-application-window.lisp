
(defun demo-simple-application-window ()
  (within-main-loop
    (let ((app (gtk-application-new "demo.application-window" :none))
          (builder (gtk-builder-new)))
      (gtk-builder-add-from-string builder
                                   (format nil
                                           "<interface> ~
                                              <menu id='menubar'> ~
                                                <submenu label='_Edit'> ~
                                                  <item label='_Copy' action='win.copy'/> ~
                                                  <item label='_Paste' action='win.paste'/> ~
                                                </submenu> ~
                                              </menu> ~
                                            </interface>"))
      (setf (gtk-application-menubar app)
            (gtk-builder-get-object builder "menubar"))
      (let ((window (gtk-application-window-new app)))
        (gtk-widget-show-all window)
        ))))

#|
 app = gtk_application_new ();


 builder = gtk_builder_new ();
 gtk_builder_add_from_string (builder,
    "<interface>"
    "  <menu id='menubar'>"
    "    <submenu label='_Edit'>"
    "      <item label='_Copy' action='win.copy'/>"
    "      <item label='_Paste' action='win.paste'/>"
    "    </submenu>"
    "  </menu>"
    "</interface>");
  gtk_application_set_menubar
                (G_APPLICATION (app),
                 G_MENU_MODEL (gtk_builder_get_object (builder, "menubar")));
 g_object_unref (builder);


 ...


 window = gtk_application_window_new (app);

|#
