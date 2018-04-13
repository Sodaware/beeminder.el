;;; beeminder.el --- Emacs interface for Beeminder -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>
;; Keywords: beeminder
;; URL: http://www.philnewton.net/code/beeminder-el/
;; Created: March 22nd, 2014
;; Version: 1.1.0
;; Package-Requires: ((org "7"))
;;
;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA

;;; Commentary:

;; beeminder.el provides a simple way for Emacs to interact with the Beeminder
;; API.  It's pretty basic at the moment, but can be used to fetch and submit
;; data.

;; Please set `beeminder-username' and `beeminder-auth-token' before using.

;; You can find your auth token by logging in to Beeminder and then visiting the
;; following URI: https://www.beeminder.com/api/v1/auth_token.json

;; Load beeminder.el with (require 'beeminder) after your Org is set up.

;;; Keyboard bindings:

;; We recommend binding the commands to the C-c b prefix

;; C-c b g    - Insert your goals as an org-mode list
;; C-c b m    - Display username in message line

;; You can use C-c C-x p (org-set-property) to add the beeminder
;; property to projects or tasks that are associated with beeminder
;; goals.  Set it to the identifier of your goal (the short name that's
;; in the URL).
;;
;; By default, completing those tasks will log one point.  You can set
;; the beeminder-value property to "prompt" in order to interactively
;; specify the value whenever you complete the task.  Set
;; beeminder-value to "time-today" in order to log the time you
;; clocked today (see "Clocking work time" in the Org manual).
;;
;; To do so, add these to your init.el:

;; (global-set-key "\C-cba" 'beeminder-add-data)
;; (global-set-key "\C-cbw" 'beeminder-whoami)
;; (global-set-key "\C-cbg" 'beeminder-my-goals-org)
;; (global-set-key "\C-cbr" 'beeminder-refresh-goal)

;;; TODO:

;; [todo] - Replace goalval with the "math_is_hard" values

;;; Code:

;; Dependencies

(require 'json)
(require 'org)
(require 'url-http)

(require 'beeminder-settings) ;; Settings and properties for module.
(require 'beeminder-api)      ;; API function wrappers and helpers.
(require 'beeminder-client)   ;; Interactive functions.
(require 'beeminder-org)      ;; org-mode functions.

(defvar org-state)
(defvar url-http-end-of-headers)

(provide 'beeminder)
;;; beeminder.el ends here
