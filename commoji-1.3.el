;;; commoji.el --- Quickly insert a commit emoji -*- lexical-binding: t -*-
;; Copyright 2018 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/commoji.el
;; Package-Requires: ((emacs "24"))
;;
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
;; commoji.el provides a command for quickly and easily finding and
;; inserting a commit emoji into a git commit message. This code was
;; inspired by https://gist.github.com/parmentf/035de27d6ed1dce0b36a and
;; similar ideas.

;;; Code:

(defconst commoji-emoji
  '(("Initial commit"             . tada)
    ("Version tag"                . bookmark)
    ("New feature"                . sparkles)
    ("Bugfix"                     . bug)
    ("Metadata"                   . card_index)
    ("Documentation"              . books)
    ("Documenting source code"    . bulb)
    ("Performance"                . racehorse)
    ("Cosmetic"                   . lipstick)
    ("Tests"                      . rotating_light)
    ("Adding a test"              . white_check_mark)
    ("Test data"                  . clipboard)
    ("General update"             . zap)
    ("Improve format/structure"   . art)
    ("Code tidying"               . art)
    ("Refactor code"              . hammer)
    ("Removing code/files"        . fire)
    ("Continuous Integration"     . green_heart)
    ("Security"                   . lock)
    ("Upgrading dependencies"     . arrow_up)
    ("Downgrading dependencies"   . arrow_down)
    ("Lint"                       . shirt)
    ("Translation"                . alien)
    ("Text"                       . pencil)
    ("Critical hotfix"            . ambulance)
    ("Deploying stuff"            . rocket)
    ("Fixing on MacOS"            . apple)
    ("Fixing on Linux"            . penguin)
    ("Fixing on Windows"          . checkered_flag)
    ("Work in progress"           . construction)
    ("Adding CI build system"     . construction_worker)
    ("Analytics or tracking code" . chart_with_upwards_trend)
    ("Removing a dependency"      . heavy_minus_sign)
    ("Adding a dependency"        . heavy_plus_sign)
    ("Docker"                     . whale)
    ("Configuration files"        . wrench)
    ("Build system"               . wrench)
    ("Merging branches"           . twisted_rightwards_arrows)
    ("Bad code / need improv."    . hankey)
    ("Reverting changes"          . rewind)
    ("Breaking changes"           . boom)
    ("Code review changes"        . ok_hand)
    ("Accessibility"              . wheelchair))
  "List of commit descriptions and emoji that go with them.

Based on https://gist.github.com/parmentf/035de27d6ed1dce0b36a")

;;;###autoload
(defun commoji (commit-type)
  "Insert a commit message emoji.

COMMIT-TYPE is the description of the commit type to work off.

Completions come from `commoji-emoji'."
  (interactive
   (list
    (ido-completing-read "Commit type: " (mapcar #'car commoji-emoji) nil t)))
  (when commit-type
    (let ((emoji (assoc commit-type commoji-emoji)))
      (when emoji
        (insert (format ":%s: " (cdr emoji)))))))

(provide 'commoji)

;;; commoji.el ends here
