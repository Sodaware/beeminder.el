;;; beeminder-client-faces.el --- Beeminder client face definitions. -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020 Phil Newton

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

;; This file contains face definitions that are used by Beeminder client modes.

;;; Code:

;; --------------------------------------------------
;; -- Base styles

(defgroup beeminder-client-faces nil
  "Font faces in beeminder client major modes."
  :tag   "beeminder"
  :group 'beeminder-client)


;; --------------------------------------------------
;; -- Section styles

(defface beeminder-section-headline
  '((t (:bold t)))
  "Default face for section headers."
  :group 'beeminder-client-faces)

(defface beeminder-section-counter
  '((t (:bold nil)))
  "Default face for numbers that appear in section headers."
  :group 'beeminder-client-faces)


;; --------------------------------------------------
;; -- Indicator styles

(defface beeminder-indicator-derailed
  '((t (:bold t :background "red" :foreground "white")))
  "Default face for '!!!' indicator in goal status table."
  :group 'beeminder-client-faces)

(defface beeminder-indicator-in-red
  '((t (:foreground "red")))
  "Default face for ' !!' indicator in goal status table."
  :group 'beeminder-client-faces)

(defface beeminder-indicator-in-orange
  '((t (:foreground "orange")))
  "Default face for '  !' indicator in goal status table."
  :group 'beeminder-client-faces)

(defface beeminder-indicator-in-clear
  '((t (:bold nil)))
  "Default face for 'everything is okay' indicator in goal status table."
  :group 'beeminder-client-faces)

(defface beeminder-indicator-fresh
  '((t (:foreground "ForestGreen")))
  "Default face for checkmark indicator in goal status table."
  :group 'beeminder-client-faces)


;; --------------------------------------------------
;; -- Deadline date styles

(defface beeminder-deadline-derailed
  '((t (:foreground "red" :bold y)))
  "Default face for deadline date of derailed goal."
  :group 'beeminder-client-faces)


;; --------------------------------------------------
;; -- Table styles

(defface beeminder-client-table-header
  '((((class color) (min-colors 16) (background light))
     (:background "grey90" :underline t :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:background "grey30" :underline t :weight bold))
    (((class color) (min-colors 8))
     (:background "cyan" :foreground "black" :underline t :weight bold))
    (t (:inverse-video t)))
  "Face for beeminder client table headers."
  :group 'beeminder-client-faces)

(provide 'beeminder-client-faces)
;;; beeminder-client-faces.el ends here
