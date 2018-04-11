;;; beeminder-settings.el --- Settings for the beeminder extension.

;; Copyright (C) 2014-2018 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains settings used by the beeminder module.

;;; Code:

(defgroup beeminder nil
  "Emacs interface for the Beeminder API."
  :group 'processes
  :prefix "beeminder-")

(defcustom beeminder-username nil
  "Your Beeminder username."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-auth-token nil
  "Your Beeminder API key."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-goal-org-tags ":GOAL:BEEMINDER:"
  "Tags that will be applied to inserted goal headlines."
  :group 'beeminder
  :type '(string))

(defcustom beeminder-properties
  '((slug .       "beeminder")
    (pledge .     "beeminder-pledge")
    (goal_type .  "beeminder-type")
    (goalval .    "beeminder-target")
    (lane .       "beeminder-lane")
    (value .      "beeminder-value")
    (progress .   "beeminder-progress")
    (updated_at . "beeminder-updated-at"))
  "Alist mapping property names for Beeminder goals.

The key should be the symbol that the Beeminder API returns, and
the value should be the name of the property updated in Org."
  :group 'beeminder
  :type '(repeat
          (cons
           (symbol "Symbol")
           (string "Property name"))))


(provide 'beeminder-settings)
;;; beeminder-settings.el ends here
