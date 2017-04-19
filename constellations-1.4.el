;;; constellations.el --- Constellation tools.
;; Copyright 2005-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.4
;; Keywords: convenience, astronomy
;; URL: https://github.com/davep/constellations.el

;; constellations.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; constellations.el provides a constellation abbreviation lookup tool. I
;; can never remember the abbreviations for most constellations so, when I
;; was writing my observing logs (http://www.astronomer.me.uk/logs/) I found
;; this little tool handy -- saved me from having to remember.
;;
;; The latest constellations.el is always available from:
;;
;;   <URL:https://github.com/davep/constellations.el>

;;; Code:

(defvar constellations-names '(("Andromeda"           . "And")
                               ("Antlia"              . "Ant")
                               ("Apus"                . "Aps")
                               ("Aquarius"            . "Aqr")
                               ("Aquila"              . "Aql")
                               ("Ara"                 . "Ara")
                               ("Aries"               . "Ari")
                               ("Auriga"              . "Aur")
                               ("Boötes"              . "Boo")
                               ("Caelum"              . "Cae")
                               ("Camelopardalis"      . "Cam")
                               ("Cancer"              . "Cnc")
                               ("Canes Venatici"      . "CVn")
                               ("Canis Major"         . "CMa")
                               ("Canis Minor"         . "CMi")
                               ("Capricornus"         . "Cap")
                               ("Carina"              . "Car")
                               ("Cassiopeia"          . "Cas")
                               ("Centaurus"           . "Cen")
                               ("Cepheus"             . "Cep")
                               ("Cetus"               . "Cet")
                               ("Chamaeleon"          . "Cha")
                               ("Circinus"            . "Cir")
                               ("Columba"             . "Col")
                               ("Coma Berenices"      . "Com")
                               ("Corona Australis"    . "CrA")
                               ("Corona Borealis"     . "CrB")
                               ("Corvus"              . "Crv")
                               ("Crater"              . "Crt")
                               ("Crux"                . "Cru")
                               ("Cygnus"              . "Cyg")
                               ("Delphinus"           . "Del")
                               ("Dorado"              . "Dor")
                               ("Draco"               . "Dra")
                               ("Equuleus"            . "Eql")
                               ("Eridanus"            . "Eri")
                               ("Fornax"              . "For")
                               ("Gemini"              . "Gem")
                               ("Grus"                . "Gru")
                               ("Hercules"            . "Her")
                               ("Horologium"          . "Hor")
                               ("Hydra"               . "Hya")
                               ("Hydrus"              . "Hyi")
                               ("Indus"               . "Ind")
                               ("Lacerta"             . "Lac")
                               ("Leo"                 . "Leo")
                               ("Leo Minor"           . "LMi")
                               ("Lepus"               . "Lep")
                               ("Libra"               . "Lib")
                               ("Lupus"               . "Lup")
                               ("Lynx"                . "Lyn")
                               ("Lyra"                . "Lyr")
                               ("Mensa"               . "Men")
                               ("Microscopium"        . "Mic")
                               ("Monoceros"           . "Mon")
                               ("Musca"               . "Mus")
                               ("Norma"               . "Nor")
                               ("Octans"              . "Oct")
                               ("Ophiuchus"           . "Oph")
                               ("Orion"               . "Ori")
                               ("Pavo"                . "Pav")
                               ("Pegasus"             . "Peg")
                               ("Perseus"             . "Per")
                               ("Phoenix"             . "Phe")
                               ("Pictor"              . "Pic")
                               ("Pisces"              . "Psc")
                               ("Piscis Austrinus"    . "PsA")
                               ("Puppis"              . "Pup")
                               ("Pyxis"               . "Pyx")
                               ("Reticulum"           . "Ret")
                               ("Sagitta"             . "Sge")
                               ("Sagittarius"         . "Sgr")
                               ("Scorpius"            . "Sco")
                               ("Sculptor"            . "Scl")
                               ("Scutum"              . "Sct")
                               ("Serpens"             . "Ser")
                               ("Sextans"             . "Sex")
                               ("Taurus"              . "Tau")
                               ("Telescopium"         . "Tel")
                               ("Triangulum"          . "Tri")
                               ("Triangulum Australe" . "TrA")
                               ("Tucana"              . "Tuc")
                               ("Ursa Major"          . "UMa")
                               ("Ursa Minor"          . "UMi")
                               ("Vela"                . "Vel")
                               ("Virgo"               . "Vir")
                               ("Volans"              . "Vol")
                               ("Vulpecula"           . "Vul"))
  "List of constellations and their abbreviations.")

;;;###autoload
(defun constellations-insert-abbr (name)
  "Given constellation name NAME, insert its abbreviation."
  (interactive (list (completing-read "Name: " constellations-names)))
  (when name
    (let ((const (assoc name constellations-names)))
      (when const
        (insert (cdr const))))))

;;;###autoload
(defun constellations-insert-name (abbr)
  "Given constellation abbreviation ABBR, insert its name."
  (interactive (list (completing-read "Abbreviation: " (mapcar #'cdr constellations-names))))
  (when abbr
    (let ((const (assoc abbr (mapcar #'(lambda (const)
                                         (cons (cdr const) (car const)))
                                     constellations-names))))
      (when const
        (insert (cdr const))))))

(provide 'constellations)

;;; constellations.el ends here
