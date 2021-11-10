;;;; Color Chooser Palette - 2021-11-5

(in-package #:gtk-example)

(defparameter *color-chooser-palette-ui*
"<interface>
   <object class='GtkGrid' id='action-grid'>
     <property name='column-spacing'>12</property>
     <property name='row-spacing'>6</property>
     <property name='valign'>1</property>
     <child>
       <object class='GtkLabel'>
         <property name='label'>&lt;b&gt;Add a palette&lt;/b&gt;</property>
         <property name='use-markup'>T</property>
         <property name='halign'>1</property>
       </object>
       <packing>
         <property name='left-attach'>0</property>
         <property name='top-attach'>0</property>
         <property name='width'>1</property>
       </packing>
     </child>
     <child>
       <object class='GtkComboBoxText' id='combo-add-palette'>
         <property name='halign'>1</property>
         <items>
            <item>Clear</item>
            <item>Red Colors</item>
            <item>Pink Colors</item>
            <item>Orange Colors</item>
            <item>Yellow Colors</item>
            <item>Purple Colors</item>
            <item>Green Colors</item>
            <item>Blue Colors</item>
            <item>Brown Colors</item>
            <item>White Colors</item>
            <item>Gray Colors</item>
         </items>
       </object>
       <packing>
         <property name='left-attach'>0</property>
         <property name='top-attach'>1</property>
         <property name='width'>1</property>
       </packing>
     </child>
   </object>
 </interface>")

(defparameter *html-color-names*
  '(("Red Colors" .
     (
      ("LightSalmon"           #xFFA07A    "rgb(255, 160, 122)")
      ("Salmon"                #xFA8072    "rgb(250, 128, 114)")
      ("DarkSalmon"            #xE9967A    "rgb(233, 150, 122)")
      ("LightCoral"            #xF08080    "rgb(240, 128, 128)")
      ("IndianRed"             #xCD5C5C    "rgb(205,  92,  92)")
      ("Crimson"               #xDC143C    "rgb(220,  20,  60)")
      ("Red"                   #xFF0000    "rgb(255,   0,   0)")
      ("FireBrick"             #xB22222    "rgb(178,  34,  34)")
      ("DarkRed"               #x8B0000    "rgb(139,   0,   0)")))
    ("Pink Colors" .
     (("Pink"                  #xFFC0CB    "rgb(255, 192, 203)")
      ("LightPink"             #xFFB6C1    "rgb(255, 182, 193)")
      ("HotPink"               #xFF69B4    "rgb(255, 105, 180)")
      ("DeepPink"              #xFF1493    "rgb(255,  20, 147)")
      ("PaleVioletRed"         #xDB7093    "rgb(219, 112, 147)")
      ("MediumVioletRed"       #xC71585    "rgb(199,  21, 133)")))
    ("Orange Colors" .
     (
      ("Orange"                #xFFA500    "rgb(255, 165,   0)")
      ("DarkOrange"            #xFF8C00    "rgb(255, 140,   0)")
      ("Coral"                 #xFF7F50    "rgb(255, 127,  80)")
      ("Tomato"                #xFF6347    "rgb(255,  99,  71)")
      ("OrangeRed"             #xFF4500    "rgb(255,  69,   0)")))
    ("Yellow Colors" .
     (("Gold"                  #xFFD700    "rgb(255, 215,   0)")
      ("Yellow"                #xFFFF00    "rgb(255, 255,   0)")
      ("LightYellow"           #xFFFFE0    "rgb(255, 255, 224)")
      ("LemonChiffon"          #xFFFACD    "rgb(255, 250, 205)")
      ("LightGoldenrodYellow"  #xFAFAD2    "rgb(250, 250, 210)")
      ("PapayaWhip"            #xFFEFD5    "rgb(255, 239, 213)")
      ("Moccasin"              #xFFE4B5    "rgb(255, 228, 181)")
      ("PeachPuff"             #xFFDAB9    "rgb(255, 218, 185)")
      ("PaleGoldenrod"         #xEEE8AA    "rgb(238, 232, 170)")
      ("Khaki"                 #xF0E68C    "rgb(240, 230, 140)")
      ("DarkKhaki"             #xBDB76B    "rgb(189, 183, 107)")))
    ("Purple Colors" .
     (("Lavender"              #xE6E6FA    "rgb(230, 230, 250)")
      ("Thistle"               #xD8BFD8    "rgb(216, 191, 216)")
      ("Plum"                  #xDDA0DD    "rgb(221, 160, 221)")
      ("Orchid"                #xDA70D6    "rgb(218, 112, 214)")
      ("Violet"                #xEE82EE    "rgb(238, 130, 238)")
      ("Fuchsia"               #xFF00FF    "rgb(255,   0, 255)")
      ("Magenta"               #xFF00FF    "rgb(255,   0, 255)")
      ("MediumOrchid"          #xBA55D3    "rgb(186,  85, 211)")
      ("DarkOrchid"            #x9932CC    "rgb(153,  50, 204)")
      ("DarkViolet"            #x9400D3    "rgb(148,   0, 211)")
      ("BlueViolet"            #x8A2BE2    "rgb(138,  43, 226)")
      ("DarkMagenta"           #x8B008B    "rgb(139,   0, 139)")
      ("Purple"                #x800080    "rgb(128,   0, 128)")
      ("MediumPurple"          #x9370DB    "rgb(147, 112, 219)")
      ("MediumSlateBlue"       #x7B68EE    "rgb(123, 104, 238)")
      ("SlateBlue"             #x6A5ACD    "rgb(106,  90, 205)")
      ("DarkSlateBlue"         #x483D8B    "rgb( 72,  61, 139)")
      ("RebeccaPurple"         #x663399    "rgb(102,  51, 153)")
      ("Indigo"                #x4B0082    "rgb( 75,   0, 130)")))
    ("Green Colors" .
     (("GreenYellow"           #xADFF2F    "rgb(173, 255,  47)")
      ("Chartreuse"            #x7FFF00    "rgb(127, 255,   0)")
      ("LawnGreen"             #x7CFC00    "rgb(124, 252,   0)")
      ("Lime"                  #x00FF00    "rgb(  0, 255,   0)")
      ("LimeGreen"             #x32CD32    "rgb( 50, 205,  50)")
      ("PaleGreen"             #x98FB98    "rgb(152, 251, 152)")
      ("LightGreen"            #x90EE90    "rgb(144, 238, 144)")
      ("MediumSpringGreen"     #x00FA9A    "rgb(  0, 250, 154)")
      ("SpringGreen"           #x00FF7F    "rgb(  0, 255, 127)")
      ("MediumSeaGreen"        #x3CB371    "rgb( 60, 179, 113)")
      ("SeaGreen"              #x2E8B57    "rgb( 46, 139,  87)")
      ("ForestGreen"           #x228B22    "rgb( 34, 139,  34)")
      ("Green"                 #x008000    "rgb(  0, 128,   0)")
      ("DarkGreen"             #x006400    "rgb(  0, 100,   0)")
      ("YellowGreen"           #x9ACD32    "rgb(154, 205,  50)")
      ("OliveDrab"             #x6B8E23    "rgb(107, 142,  35)")
      ("Olive"                 #x808000    "rgb(128, 128,   0)")
      ("DarkOliveGreen"        #x556B2F    "rgb( 85, 107,  47)")
      ("MediumAquamarine"      #x66CDAA    "rgb(102, 205, 170)")
      ("DarkSeaGreen"          #x8FBC8B    "rgb(143, 188, 139)")
      ("LightSeaGreen"         #x20B2AA    "rgb( 32, 178, 170)")
      ("DarkCyan"              #x008B8B    "rgb(  0, 139, 139)")
      ("Teal"                  #x008080    "rgb(  0, 128, 128)")))
    ("Blue Colors" .
     (("Aqua"                  #x00FFFF    "rgb(  0, 255, 255)")
      ("Cyan"                  #x00FFFF    "rgb(  0, 255, 255)")
      ("LightCyan"             #xE0FFFF    "rgb(224, 255, 255)")
      ("PaleTurquoise"         #xAFEEEE    "rgb(175, 238, 238)")
      ("Aquamarine"            #x7FFFD4    "rgb(127, 255, 212)")
      ("Turquoise"             #x40E0D0    "rgb( 64, 224, 208)")
      ("MediumTurquoise"       #x48D1CC    "rgb( 72, 209, 204)")
      ("DarkTurquoise"         #x00CED1    "rgb(  0, 206, 209)")
      ("CadetBlue"             #x5F9EA0    "rgb( 95, 158, 160)")
      ("SteelBlue"             #x4682B4    "rgb( 70, 130, 180)")
      ("LightSteelBlue"        #xB0C4DE    "rgb(176, 196, 222)")
      ("PowderBlue"            #xB0E0E6    "rgb(176, 224, 230)")
      ("LightBlue"             #xADD8E6    "rgb(173, 216, 230)")
      ("SkyBlue"               #x87CEEB    "rgb(135, 206, 235)")
      ("LightSkyBlue"          #x87CEFA    "rgb(135, 206, 250)")
      ("DeepSkyBlue"           #x00BFFF    "rgb(  0, 191, 255)")
      ("DodgerBlue"            #x1E90FF    "rgb( 30, 144, 255)")
      ("CornflowerBlue"        #x6495ED    "rgb(100, 149, 237)")
      ("MediumSlateBlue"       #x7B68EE    "rgb(123, 104, 238)")
      ("RoyalBlue"             #x4169E1    "rgb( 65, 105, 225)")
      ("Blue"                  #x0000FF	   "rgb(  0,   0, 255)")
      ("MediumBlue"            #x0000CD    "rgb(  0,   0, 205)")
      ("DarkBlue"              #x00008B    "rgb(  0,   0, 139)")
      ("Navy"                  #x000080    "rgb(  0,   0, 128)")
      ("MidnightBlue"          #x191970    "rgb( 25,  25, 112)")))
    ("Brown Colors" .
     (("Cornsilk"              #xFFF8DC    "rgb(255, 248, 220)")
      ("BlanchedAlmond"        #xFFEBCD    "rgb(255, 235, 205)")
      ("Bisque"                #xFFE4C4    "rgb(255, 228, 196)")
      ("NavajoWhite"           #xFFDEAD    "rgb(255, 222, 173)")
      ("Wheat"                 #xF5DEB3    "rgb(245, 222, 179)")
      ("BurlyWood"             #xDEB887    "rgb(222, 184, 135)")
      ("Tan"                   #xD2B48C    "rgb(210, 180, 140)")
      ("RosyBrown"             #xBC8F8F    "rgb(188, 143, 143)")
      ("SandyBrown"            #xF4A460    "rgb(244, 164,  96)")
      ("Goldenrod"             #xDAA520    "rgb(218, 165,  32)")
      ("DarkGoldenrod"         #xB8860B    "rgb(184, 134,  11)")
      ("Peru"                  #xCD853F    "rgb(205, 133,  63)")
      ("Chocolate"             #xD2691E    "rgb(210, 105,  30)")
      ("SaddleBrown"           #x8B4513    "rgb(139,  69,  19)")
      ("Sienna"                #xA0522D    "rgb(160,  82,  45)")
      ("Brown"                 #xA52A2A    "rgb(165,  42,  42)")
      ("Maroon"                #x800000    "rgb(128,   0,   0)")))
    ("White Colors" .
     (("White"                 #xFFFFFF    "rgb(255, 255, 255)")
      ("Snow"                  #xFFFAFA    "rgb(255, 250, 250)")
      ("HoneyDew"              #xF0FFF0    "rgb(240, 255, 240)")
      ("MintCream"             #xF5FFFA    "rgb(245, 255, 250)")
      ("Azure"                 #xF0FFFF    "rgb(240, 255, 255)")
      ("AliceBlue"             #xF0F8FF    "rgb(240, 248, 255)")
      ("GhostWhite"            #xF8F8FF    "rgb(248, 248, 255)")
      ("WhiteSmoke"            #xF5F5F5    "rgb(245, 245, 245)")
      ("SeaShell"              #xFFF5EE    "rgb(255, 245, 238)")
      ("Beige"                 #xF5F5DC    "rgb(245, 245, 220)")
      ("OldLace"               #xFDF5E6    "rgb(253, 245, 230)")
      ("FloralWhite"           #xFFFAF0    "rgb(255, 250, 240)")
      ("Ivory"                 #xFFFFF0    "rgb(255, 255, 240)")
      ("AntiqueWhite"          #xFAEBD7    "rgb(250, 235, 215)")
      ("Linen"                 #xFAF0E6    "rgb(250, 240, 230)")
      ("LavenderBlush"         #xFFF0F5    "rgb(255, 240, 245)")
      ("MistyRose"             #xFFE4E1    "rgb(255, 228, 225)")))
    ("Gray Colors" .
     (("Gainsboro"             #xDCDCDC    "rgb(220, 220, 220)")
      ("LightGray"             #xD3D3D3    "rgb(211, 211, 211)")
      ("Silver"                #xC0C0C0    "rgb(192, 192, 192)")
      ("DarkGray"              #xA9A9A9    "rgb(169, 169, 169)")
      ("Gray"                  #x808080    "rgb(128, 128, 128)")
      ("DimGray"               #x696969    "rgb(105, 105, 105)")
      ("LightSlateGray"        #x778899    "rgb(119, 136, 153)")
      ("SlateGray"             #x708090    "rgb(112, 128, 144)")
      ("DarkSlateGray"         #x2F4F4F    "rgb( 47,  79,  79)")
      ("Black"                 #x000000    "rgb(  0,   0,   0)")))))

(defun example-color-chooser-palette (&optional application)
  (within-main-loop
    (let* ((colors-per-line 9)
           (window (make-instance 'gtk-window
                                  :application application
                                  :title "Example Color Chooser Palette"
                                  :border-width 12
                                  :default-width 400))
           (builder (gtk-builder-new-from-string *color-chooser-palette-ui*))
           (action-grid (gtk-builder-object builder "action-grid"))
           (combo-add-palette (gtk-builder-object builder "combo-add-palette"))
           (hbox (make-instance 'gtk-box
                                :orientation :horizontal
                                :spacing 24))
           (color-chooser (make-instance 'gtk-color-chooser-widget)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect combo-add-palette "changed"
          (lambda (combo)
            (let* ((palette (gtk-combo-box-text-active-text combo))
                   (colors (mapcar #'first
                                   (cdr (assoc palette *html-color-names*
                                               :test #'equal)))))
            (format t "~%")
            (format t "Palette : ~a~%" palette)
            (format t "Colors  : ~a~%" colors)
            (gtk-color-chooser-add-palette color-chooser
                                           :horizontal
                                           colors-per-line
                                           (mapcar #'gdk-rgba-parse
                                                   colors)))))
      (setf (gtk-combo-box-active combo-add-palette) 0)
      (gtk-box-pack-start hbox color-chooser)
      (gtk-box-pack-start hbox action-grid)
      (gtk-container-add window hbox)
      (gtk-widget-show-all window))))
