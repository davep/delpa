;;; commoji.el --- Quickly insert a commit emoji -*- lexical-binding: t -*-
;; Copyright 2018-2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.5
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
  '(("Accessibility"              . wheelchair)
    ("Adding CI build system"     . construction_worker)
    ("Adding a dependency"        . heavy_plus_sign)
    ("Adding a test"              . white_check_mark)
    ("Adding an Easter egg"       . egg)
    ("Adding logs"                . loud_sound)
    ("Adding/updating .gitignore" . see_no_evil)
    ("Analytics or tracking code" . chart_with_upwards_trend)
    ("Architectural changes"      . building_construction)
    ("Bad code / need improv."    . hankey)
    ("Breaking changes"           . boom)
    ("Bugfix"                     . bug)
    ("Build system"               . wrench)
    ("Code review changes"        . ok_hand)
    ("Code tidying"               . art)
    ("Configuration files"        . wrench)
    ("Continuous Integration"     . green_heart)
    ("Correcting/fixing typo"     . pencil)
    ("Cosmetic"                   . lipstick)
    ("Critical hotfix"            . ambulance)
    ("Database work"              . card_file_box)
    ("Deploying stuff"            . rocket)
    ("Docker"                     . whale)
    ("Documentation"              . books)
    ("Documenting source code"    . bulb)
    ("Downgrading dependencies"   . arrow_down)
    ("Experimenting"              . alembic)
    ("Fixing on Linux"            . penguin)
    ("Fixing on MacOS"            . apple)
    ("Fixing on Windows"          . checkered_flag)
    ("General update"             . zap)
    ("Improve format/structure"   . art)
    ("Initial commit"             . tada)
    ("Lint"                       . shirt)
    ("Merging branches"           . twisted_rightwards_arrows)
    ("Metadata"                   . card_index)
    ("Mocking"                    . clown_face)
    ("Moving/renaming files"      . truck)
    ("New feature"                . sparkles)
    ("Performance"                . racehorse)
    ("Pinning dependencies"       . pushpin)
    ("Refactor code"              . hammer)
    ("Removing a dependency"      . heavy_minus_sign)
    ("Removing code/files"        . fire)
    ("Removing logs"              . mute)
    ("Responsive design"          . iphone)
    ("Reverting changes"          . rewind)
    ("Security"                   . lock)
    ("Test data"                  . clipboard)
    ("Tests"                      . rotating_light)
    ("Text"                       . pencil)
    ("Translation"                . alien)
    ("Upgrading dependencies"     . arrow_up)
    ("Version tag"                . bookmark)
    ("Work in progress"           . construction))
  "List of commit descriptions and emoji that go with them.

Originally based on:
https://gist.github.com/parmentf/035de27d6ed1dce0b36a

Further inspired by:
https://gitmoji.carloscuesta.me/")

;;;###autoload
(defun commoji (commit-type)
  "Insert a commit message emoji.

COMMIT-TYPE is the description of the commit type to work off.

Completions come from `commoji-emoji'."
  (interactive
   (list
    (completing-read "Commit type: " (mapcar #'car commoji-emoji) nil t)))
  (when commit-type
    (let ((emoji (assoc commit-type commoji-emoji)))
      (when emoji
        (insert (format ":%s: " (cdr emoji)))))))

(provide 'commoji)

;;; commoji.el ends here
