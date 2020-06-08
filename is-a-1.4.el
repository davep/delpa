;;; is-a.el --- Tools for testing the environment we're running in
;; Copyright 2017-2020 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.4
;; Keywords: convenience
;; URL: https://github.com/davep/is-a.el

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
;; is-a.el provides tools for testing the environment we're running in.
;;
;; Note that these tests aren't intended to be comprehensive. They're just
;; good enough to cover the range of machines and environments I own and
;; use.

;;; Code:

;;;###autoload
(defconst is-a-win32-p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

;;;###autoload
(defconst is-a-linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

;;;###autoload
(defconst is-a-linux-x-p
  (and (display-graphic-p) is-a-linux-p)
  "Are we running under X on a GNU/Linux system?")

;;;###autoload
(defconst is-a-linux-terminal-p
  (and (not (display-graphic-p)) is-a-linux-p)
  "Are we running on GNU/Linux, in a terminal?")

;;;###autoload
(defconst is-a-macOS-p
  (eq system-type 'darwin)
  "Are we running on some form of macOS?")

;;;###autoload
(defconst is-a-macOS-terminal-p
  (and is-a-macOS-p (not (display-graphic-p)))
  "Are we running in an macOS terminal?")

;;;###autoload
(defconst is-a-macOS-window-p
  (and is-a-macOS-p (not is-a-macOS-terminal-p))
  "Are we running in an macOS window?")

;;;###autoload
(defconst is-a-macOS-dark-mode-window-p
  (and is-a-macOS-window-p (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle") "Dark\n"))
  "Are we running in a macOS window in dark mode?")

;;;###autoload
(defconst is-a-unix-p
  (or is-a-linux-p is-a-macOS-p)
  "Are we on some form of Unix?")

;;;###autoload
(defconst is-a-unix-window-p
  (and is-a-unix-p (display-graphic-p))
  "Are we on some form of Unix and in a graphical environment?")

;;;###autoload
(defconst is-a-unix-terminal-p
  (and is-a-unix-p (not (display-graphic-p)))
  "Are we on some form of Unix but not running as a graphical app?")

;;;###autoload
(defconst is-a-root-user-p
  (and is-a-unix-p (zerop (user-uid)))
  "Are we running as root?")

(provide 'is-a)

;;; is-a.el ends here
