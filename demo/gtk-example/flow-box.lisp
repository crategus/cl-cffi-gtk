;;;; Flow Box - 2021-12-5
;;;;
;;;; GtkFlowBox allows flexible and responsive grids which reflow as needed
;;;; and support sorting and filtering.
;;;;
;;;; The children of a GtkFlowBox are regular widgets.

(in-package :gtk-example)

(defparameter *colors*
  '("AliceBlue"       "AntiqueWhite"     "AntiqueWhite1"   "AntiqueWhite2"
    "AntiqueWhite3"   "AntiqueWhite4"    "Aqua"            "Aquamarine"
    "Aquamarine1"     "Aquamarine2"      "Aquamarine3"     "Aquamarine4"
    "Azure"           "Azure1"           "Azure2"          "Azure3"
    "Azure4"          "Beige"            "Bisque"          "Bisque1"
    "Bisque2"         "Bisque3"          "Bisque4"         "black"
    "BlanchedAlmond"  "Blue"             "Blue1"           "Blue2"
    "Blue3"           "Blue4"            "BlueViolet"      "Brown"
    "Brown1"          "Brown2"           "Brown3"          "Brown4"
    "Burlywood"       "Burlywood1"       "Burlywood2"      "Burlywood3"
    "Burlywood4"      "CadetBlue"        "CadetBlue1"      "CadetBlue2"
    "CadetBlue3"      "CadetBlue4"       "Chartreuse"      "Chartreuse1"
    "Chartreuse2"     "Chartreuse3"      "Chartreuse4"     "Chocolate"
    "Chocolate1"      "Chocolate2"       "Chocolate3"      "Chocolate4"
    "Coral"           "Coral1"           "Coral2"          "Coral3"
    "Coral4"          "CornflowerBlue"   "Cornsilk"        "Cornsilk1"
    "Cornsilk2"       "Cornsilk3"        "Cornsilk4"       "crimson"
    "Cyan"            "Cyan1"            "Cyan2"           "Cyan3"
    "Cyan4"           "DarkBlue"         "DarkCyan"        "DarkGoldenrod"
    "DarkGoldenrod1"  "DarkGoldenrod2"   "DarkGoldenrod3"  "DarkGoldenrod4"
    "DarkGray"        "DarkGreen"        "DarkGrey"        "DarkKhaki"
    "DarkMagenta"     "DarkOliveGreen"   "DarkOliveGreen1" "DarkOliveGreen2"
    "DarkOliveGreen3" "DarkOliveGreen4"  "DarkOrange"      "DarkOrange1"
    "DarkOrange2"     "DarkOrange3"      "DarkOrange4"     "DarkOrchid"
    "DarkOrchid1"     "DarkOrchid2"      "DarkOrchid3"     "DarkOrchid4"
    "DarkRed"         "DarkSalmon"       "DarkSeaGreen"    "DarkSeaGreen1"
    "DarkSeaGreen2"   "DarkSeaGreen3"    "DarkSeaGreen4"   "DarkSlateBlue"
    "DarkSlateGray"   "DarkSlateGray1"   "DarkSlateGray2"  "DarkSlateGray3"
    "DarkSlateGray4"  "DarkSlateGrey"    "DarkTurquoise"   "DarkViolet"
    "DeepPink"        "DeepPink1"        "DeepPink2"       "DeepPink3"
    "DeepPink4"       "DeepSkyBlue"      "DeepSkyBlue1"    "DeepSkyBlue2"
    "DeepSkyBlue3"    "DeepSkyBlue4"     "DimGray"         "DimGrey"
    "DodgerBlue"      "DodgerBlue1"      "DodgerBlue2"     "DodgerBlue3"
    "DodgerBlue4"     "firebrick"        "Firebrick1"      "Firebrick2"
    "Firebrick3"      "Firebrick4"       "FloralWhite"     "ForestGreen"
    "Fuchsia"         "Gainsboro"        "GhostWhite"      "Gold"
    "Gold1"           "Gold2"            "Gold3"           "Gold4"
    "Goldenrod"       "Goldenrod1"       "Goldenrod2"      "Goldenrod3"
    "Goldenrod4"      "Gray"             "Gray0"           "Gray1"
    "Gray2"           "Gray3"            "Gray4"           "Gray5"
    "Gray6"           "Gray7"            "Gray8"           "Gray9"
    "Gray10"          "Gray11"           "Gray12"          "Gray13"
    "Gray14"          "Gray15"           "Gray16"          "Gray17"
    "Gray18"          "Gray19"           "Gray20"          "Gray21"
    "Gray22"          "Gray23"           "Gray24"          "Gray25"
    "Gray26"          "Gray27"           "Gray28"          "Gray29"
    "Gray30"          "Gray31"           "Gray32"          "Gray33"
    "Gray34"          "Gray35"           "Gray36"          "Gray37"
    "Gray38"          "Gray39"           "Gray40"          "Gray41"
    "Gray42"          "Gray43"           "Gray44"          "Gray45"
    "Gray46"          "Gray47"           "Gray48"          "Gray49"
    "Gray50"          "Gray51"           "Gray52"          "Gray53"
    "Gray54"          "Gray55"           "Gray56"          "Gray57"
    "Gray58"          "Gray59"           "Gray60"          "Gray61"
    "Gray62"          "Gray63"           "Gray64"          "Gray65"
    "Gray66"          "Gray67"           "Gray68"          "Gray69"
    "Gray70"          "Gray71"           "Gray72"          "Gray73"
    "Gray74"          "Gray75"           "Gray76"          "Gray77"
    "Gray78"          "Gray79"           "Gray80"          "Gray81"
    "Gray82"          "Gray83"           "Gray84"          "Gray85"
    "Gray86"          "Gray87"           "Gray88"          "Gray89"
    "Gray90"          "Gray91"           "Gray92"          "Gray93"
    "Gray94"          "Gray95"           "Gray96"          "Gray97"
    "Gray98"          "Gray99"           "Gray100"
    "Green"           "Green1"           "Green2"          "Green3"
    "Green4"          "GreenYellow"
    "Honeydew"        "Honeydew1"        "Honeydew2"       "Honeydew3"
    "Honeydew4"       "HotPink"          "HotPink1"        "HotPink2"
    "HotPink3"        "HotPink4"         "IndianRed"       "IndianRed1"
    "IndianRed2"      "IndianRed3"       "IndianRed4"      "Indigo"
    "ivory"           "Ivory1"           "Ivory2"          "Ivory3"
    "Ivory4"          "Khaki"            "Khaki1"          "Khaki2"
    "Khaki3"          "Khaki4"           "Lavender"        "LavenderBlush"
    "LavenderBlush1"  "LavenderBlush2"   "LavenderBlush3"  "LavenderBlush4"
    "LawnGreen"       "LemonChiffon"     "LemonChiffon1"   "LemonChiffon2"
    "LemonChiffon3"   "LemonChiffon4"    "LightBlue"       "LightBlue1"
    "LightBlue2"      "LightBlue3"       "LightBlue4"      "LightCoral"
    "LightCyan"       "LightCyan1"       "LightCyan2"      "LightCyan3"
    "LightCyan4"      "LightGoldenrod"   "LightGoldenrod1" "LightGoldenrod2"
    "LightGoldenrod3" "LightGoldenrod4"  "LightGoldenrodYellow"
    "LightGray"       "LightGreen"       "LightGrey"       "LightPink"
    "LightPink1"      "LightPink2"       "LightPink3"      "LightPink4"
    "LightSalmon"     "LightSalmon1"     "LightSalmon2"    "LightSalmon3"
    "LightSalmon4"    "LightSeaGreen"    "LightSkyBlue"    "LightSkyBlue1"
    "LightSkyBlue2"   "LightSkyBlue3"    "LightSkyBlue4"   "LightSlateBlue"
    "LightSlateGray"  "LightSlateGrey"   "LightSteelBlue"  "LightSteelBlue1"
    "LightSteelBlue2" "LightSteelBlue3"  "LightSteelBlue4" "LightYellow"
    "LightYellow1"    "LightYellow2"     "LightYellow3"    "LightYellow4"
    "Lime"            "LimeGreen"        "Linen"           "Magenta"
    "Magenta1"        "Magenta2"         "Magenta3"        "Magenta4"
    "Maroon"          "Maroon1"          "Maroon2"         "Maroon3"
    "Maroon4"         "MediumAquamarine" "MediumBlue"      "MediumOrchid"
    "MediumOrchid1"   "MediumOrchid2"    "MediumOrchid3"   "MediumOrchid4"
    "MediumPurple"    "MediumPurple1"    "MediumPurple2"   "MediumPurple3"
    "MediumPurple4"   "MediumSeaGreen"   "MediumSlateBlue" "MediumSpringGreen"
    "MediumTurquoise" "MediumVioletRed"  "MidnightBlue"    "MintCream"
    "MistyRose"       "MistyRose1"       "MistyRose2"      "MistyRose3"
    "MistyRose4"      "Moccasin"         "NavajoWhite"     "NavajoWhite1"
    "NavajoWhite2"    "NavajoWhite3"     "NavajoWhite4"    "navy"
    "NavyBlue"        "OldLace"          "Olive"           "OliveDrab"
    "OliveDrab1"      "OliveDrab2"       "OliveDrab3"      "OliveDrab4"
    "Orange"          "Orange1"          "Orange2"         "Orange3"
    "Orange4"         "OrangeRed"        "OrangeRed1"      "OrangeRed2"
    "OrangeRed3"      "OrangeRed4"       "Orchid"          "Orchid1"
    "Orchid2"         "Orchid3"          "Orchid4"         "PaleGoldenrod"
    "PaleGreen"       "PaleGreen1"       "PaleGreen2"      "PaleGreen3"
    "PaleGreen4"      "PaleTurquoise"    "PaleTurquoise1"  "PaleTurquoise2"
    "PaleTurquoise3"  "PaleTurquoise4"   "PaleVioletRed"   "PaleVioletRed1"
    "PaleVioletRed2"  "PaleVioletRed3"   "PaleVioletRed4"  "PapayaWhip"
    "PeachPuff"       "PeachPuff1"       "PeachPuff2"      "PeachPuff3"
    "PeachPuff4"      "Peru"             "Pink"            "Pink1"
    "Pink2"           "Pink3"            "Pink4"           "Plum"
    "Plum1"           "Plum2"            "Plum3"           "Plum4"
    "PowderBlue"      "Purple"           "Purple1"         "Purple2"
    "Purple3"         "Purple4"          "Red"             "Red1"
    "Red2"            "Red3"             "Red4"            "RosyBrown"
    "RosyBrown1"      "RosyBrown2"       "RosyBrown3"      "RosyBrown4"
    "RoyalBlue"       "RoyalBlue1"       "RoyalBlue2"      "RoyalBlue3"
    "RoyalBlue4"      "SaddleBrown"      "Salmon"          "Salmon1"
    "Salmon2"         "Salmon3"          "Salmon4"         "SandyBrown"
    "SeaGreen"        "SeaGreen1"        "SeaGreen2"       "SeaGreen3"
    "SeaGreen4"       "seashell"         "seashell1"       "seashell2"
    "seashell3"       "seashell4"        "Sienna"          "Sienna1"
    "Sienna2"         "Sienna3"          "Sienna4"         "Silver"
    "SkyBlue"         "SkyBlue1"         "SkyBlue2"        "SkyBlue3"
    "SkyBlue4"        "SlateBlue"        "SlateBlue1"      "SlateBlue2"
    "SlateBlue3"      "SlateBlue4"       "SlateGray"       "SlateGray1"
    "SlateGray2"      "SlateGray3"       "SlateGray4"      "SlateGrey"
    "Snow"            "Snow1"            "Snow2"           "Snow3"
    "Snow4"           "SpringGreen"      "SpringGreen1"    "SpringGreen2"
    "SpringGreen3"    "SpringGreen4"     "SteelBlue"       "SteelBlue1"
    "SteelBlue2"      "SteelBlue3"       "SteelBlue4"      "Tan"
    "Tan1"            "Tan2"             "Tan3"            "Tan4"
    "Teal"            "Thistle"          "Thistle1"        "Thistle2"
    "Thistle3"        "Thistle4"         "Tomato"          "Tomato1"
    "Tomato2"         "Tomato3"          "Tomato4"         "Turquoise"
    "Turquoise1"      "Turquoise2"       "Turquoise3"      "Turquoise4"
    "Violet"          "VioletRed"        "VioletRed1"      "VioletRed2"
    "VioletRed3"      "VioletRed4"       "Wheat"           "Wheat1"
    "Wheat2"          "Wheat3"           "Wheat4"          "White"
    "WhiteSmoke"      "Yellow"           "Yellow1"         "Yellow2"
    "Yellow3"         "Yellow4"          "YellowGreen"
))

(defun color-swatch-new (color)
  (let ((button (make-instance 'gtk-button
                               :name (string-downcase color)))
        (vbox (make-instance 'gtk-box
                             :orientation :vertical))
        (area (make-instance 'gtk-drawing-area
                             :width-request 24
                             :height-request 24))
        (label (make-instance 'gtk-label
                              :label color)))
    (g-signal-connect area "draw"
        (lambda (widget cr)
        (let ((cr (pointer cr))
              (rgba (gdk-rgba-parse color)))
          (when rgba
            (gdk-cairo-set-source-rgba cr rgba)
            (cairo-paint cr))
          (cairo-destroy cr))))
    (gtk-box-pack-start vbox area)
    (gtk-box-pack-start vbox label)
    (gtk-container-add button vbox)
    button))

(defun example-flow-box (&optional (application nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                  :title "Example Flow Box"
                                  :type :toplevel
                                  :application application
                                  :default-width 600
                                  :default-height 400))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical))
          (entry (make-instance 'gtk-search-entry))
          (scrolled (make-instance 'gtk-scrolled-window
                                   :hscrollbar-policy :never
                                   :vscrollbar-policy :automatic))
          (flowbox (make-instance 'gtk-flow-box
                                  :valign :start
                                  :max-children-per-line 30
                                  :selection-mode :none)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect entry "search-changed"
          (lambda (entry)
             (declare (ignore entry))
             (gtk-flow-box-invalidate-filter flowbox)))
      (gtk-flow-box-set-filter-func flowbox
          (lambda (child)
            (let* ((button (gtk-bin-child child))
                   (color (gtk-widget-name button))
                   (text (gtk-entry-text entry)))
              (search (string-downcase text) color))))
      (dolist (color *colors*)
        (gtk-container-add flowbox (color-swatch-new color)))
      (gtk-box-pack-start vbox entry :expand nil)
      (gtk-container-add scrolled flowbox)
      (gtk-box-pack-start vbox scrolled)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
