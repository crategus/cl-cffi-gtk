(in-package :gdk)

(defcfun ("gdk_x11_window_get_xid" gdk-x11-window-get-xid) :pointer
  (window (g-object gdk-window)))

(export 'gdk-x11-window-get-xid)
