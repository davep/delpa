;;; fasta.el --- Mode for working with fasta files -*- lexical-binding: t -*-
;; Copyright 2018-2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: files, dna, fasta, bioinformatics
;; URL: https://github.com/davep/fasta.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; fasta.el implements a simple major mode for viewing and editing fasta
;; files. It's not designed to be anything too clever, I just wrote it to
;; make fasta files look nicer in Emacs.

;;; Code:

(defgroup fasta nil
  "Mode for working with FASTA files."
  :group 'text)

(defface fasta-header-face
  '((t :inherit font-lock-function-name-face))
  "Face to use when showing a \"header\" line."
  :group 'fasta)

(defface fasta-comment-face
  '((t :inherit font-lock-comment-face))
  "Face to use when showing a comment line."
  :group 'fasta)

(defface fasta-adenine-face
  '((((background dark)) :foreground "#8dd4e8")
    (t :foreground "#4b5cc4"))
  "Face to use when showing adenine."
  :group 'fasta)

(defface fasta-cytosine-face
  '((((background dark)) :foreground "#ff0000")
    (t :foreground "#bb0000"))
  "Face to use when showing cytosine."
  :group 'fasta)

(defface fasta-guanine-face
  '((((background dark)) :foreground "#00c000")
    (t :foreground "#00c000"))
  "Face to use when showing guanine."
  :group 'fasta)

(defface fasta-thymine-face
  '((((background dark)) :foreground "#bbbb00")
    (t :foreground "#969600"))
  "Face to use when showing thymine."
  :group 'fasta)

(defface fasta-uracil-face
  '((((background dark)) :foreground "#cc9900")
    (t :foreground "#cc9900"))
  "Face to use when showing uracil."
  :group 'fasta)

(defface fasta-purine-face
  '((((background dark)) :foreground "#2e8b57")
    (t :foreground "#2e8b57"))
  "Face to use when showing a purine."
  :group 'fasta)

(defface fasta-pyrimidine-face
  '((((background dark)) :foreground "#ff8c00")
    (t :foreground "#ff8c00"))
  "Face to use when showing a pyrimidine."
  :group 'fasta)

(defface fasta-ketone-face
  '((((background dark)) :foreground "#bbc000")
    (t :foreground "#96c000"))
  "Face to use when showing a ketone."
  :group 'fasta)

(defface fasta-amine-face
  '((((background dark)) :foreground "#ff5cc4")
    (t :foreground "#bb5cc4"))
  "Face to use when showing an amine."
  :group 'fasta)

(defface fasta-strong-face
  '((((background dark)) :foreground "#ffc000")
    (t :foreground "#bbc000"))
  "Face to use when showing strong interaction."
  :group 'fasta)

(defface fasta-weak-face
  '((((background dark)) :foreground "#8dbb00")
    (t :foreground "#4b9500"))
  "Face to use when showing weak interaction."
  :group 'fasta)

(defface fasta-not-adenine-face
  '((((background dark)) :foreground "black" :background "#8dd4e8")
    (t :foreground "white" :background "#4b5cc4"))
  "Face to use when showing not adenine."
  :group 'fasta)

(defface fasta-not-cytosine-face
  '((t :foreground "white" :background "#e00000"))
  "Face to use when showing not cytosine."
  :group 'fasta)

(defface fasta-not-guanine-face
  '((((background dark)) :foreground "black" :background "#00c000")
    (t :background "#00c000"))
  "Face to use when showing not guanine."
  :group 'fasta)

(defface fasta-not-thymine-uracil-face
  '((((background dark)) :foreground "black" :background "#bbbb00")
    (t :foreground "white" :background "#969600"))
  "Face to use when showing not thymine/uracil."
  :group 'fasta)

(defface fasta-nucleic-acid-face
  '((t :inherit default))
  "Face to use when showing a nucleic acid."
  :group 'fasta)

(defface fasta-gap-face
  '((t :inherit font-lock-preprocessor-face))
  "Face to use when showing a gap."
  :group 'fasta)

(defface fasta-translation-stop-face
  '((t :inherit font-lock-doc-face))
  "Face to use when showing a translation stop."
  :group 'fasta)

(defvar fasta-font-lock
  '(("^>.*$" . 'fasta-header-face)
    ("^;.*$" . 'fasta-comment-face)
    ("A"     . 'fasta-adenine-face)
    ("C"     . 'fasta-cytosine-face)
    ("G"     . 'fasta-guanine-face)
    ("T"     . 'fasta-thymine-face)
    ("U"     . 'fasta-uracil-face)
    ("R"     . 'fasta-purine-face)
    ("Y"     . 'fasta-pyrimidine-face)
    ("K"     . 'fasta-ketone-face)
    ("M"     . 'fasta-amine-face)
    ("S"     . 'fasta-strong-face)
    ("W"     . 'fasta-weak-face)
    ("B"     . 'fasta-not-adenine-face)
    ("D"     . 'fasta-not-cytosine-face)
    ("H"     . 'fasta-not-guanine-face)
    ("V"     . 'fasta-not-thymine-uracil-face)
    ("N"     . 'fasta-nucleic-acid-face)
    ("-"     . 'fasta-gap-face)
    ("\\."   . 'fasta-gap-face)
    ("\\*"   . 'fasta-translation-stop-face))
  "Font lock rules for `fasta-mode'.")

(defun fasta-comment-line-p ()
  "Are we on a comment line?"
  (save-excursion
    (setf (point) (line-beginning-position))
    (looking-at-p "^[>;]")))

(defun fasta-sequence-line-p ()
  "Are we on a line of sequence data?"
  (save-excursion
    (setf (point) (line-beginning-position))
    (not (or (fasta-comment-line-p) (looking-at-p "^[[:space:]]*$")))))

(defun fasta-beginning-of-sequence ()
  "Move `point' to the start of the current sequence."
  (interactive)
  (setf (point) (line-beginning-position))
  (while (not (or (fasta-comment-line-p) (bobp)))
    (forward-line -1)))

(defun fasta-end-of-sequence ()
  "Move `point' to the end of the current sequence."
  (interactive)
  (when (fasta-comment-line-p)
    (forward-line))
  (while (and (fasta-sequence-line-p) (not (eobp)))
    (forward-line)))

(defvar fasta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'fasta-beginning-of-sequence)
    (define-key map (kbd "C-c C-e") #'fasta-end-of-sequence)
    map)
  "Local keymap for `fasta-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.f\\(asta\\|na\\|fn\\|aa\\|rn\\)\\'" . fasta-mode))

;;;###autoload
(define-derived-mode fasta-mode fundamental-mode "fasta"
  "Major mode for editing fasta files."
  (setq font-lock-defaults '(fasta-font-lock)))

(provide 'fasta)

;;; fasta.el ends here
