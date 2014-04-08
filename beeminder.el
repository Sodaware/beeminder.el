;;; beeminder.el --- Emacs interface for Beeminder

;; Copyright (C) 2014 Phil Newton <phil@sodaware.net>

;; Author: Phil Newton <phil@sodaware.net>
;; Keywords: beeminder
;; URL: http://www.philnewton.net/code/beeminder-el/
;; Created: March 22nd, 2014
;; Version: 1.0.0

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

;;; Keyboard bindings:

;; We recommend binding the commands to the C-c b prefix

;; C-c b g    - Insert your goals as an org-mode list
;; C-c b m    - Display username in message line

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


;; Configuration

(defgroup beeminder nil
  "Emacs interface for the Beeminder API"
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

(defvar beeminder-request-data nil
  "An assoc list of parameter names to values.")

(defconst beeminder-v1-api
  "https://www.beeminder.com/api/v1/"
  "The endpoint for version 1.0 of the Beeminder API.")


;; org-mode hooks

(defun beeminder-on-org-task-completed ()
  "Fires when an 'org-mode' task is marked as DONE."
  ;; Only fire if task is complete and a beeminder task
  (when (and (org-entry-is-done-p) (org-entry-get (point) "beeminder" t))

      ;; If "value" property set, use that as the data, otherwise default to 1
      (let* ((datapoint (or (org-entry-get (point) "value" t) 1))
	     (title (nth 4 (org-heading-components)))
	     (goal (org-entry-get (point) "beeminder" t)))

	;; Send to beeminder
	(beeminder-add-data goal datapoint title))))

(add-hook 'org-after-todo-state-change-hook 'beeminder-on-org-task-completed)


;; Functions

(defun beeminder-whoami ()
  "Displays the Beeminder username for your auth token."
  (interactive)
  (let ((result (beeminder-fetch (format "users/me.json?auth_token=%s" beeminder-auth-token))))
    (message "Your Beeminder username: %s" (assoc-default 'username result))))

(defun beeminder-my-goals ()
  "Displays your goals in the Message buffer (kind of useless)."
  (interactive)
  (message
   "%s"
   (mapconcat (lambda (goal)
		(format "Goal: %s" (assoc-default 'title goal)))
	      (beeminder-fetch-goals beeminder-username)
	      "\n")))

(defun beeminder-refresh-goal ()
  "Fetch data for the current goal headline and update it."
  (interactive)

  ;; Get the goal at current point
  (when (org-entry-get (point) "beeminder" t)

    (let* ((goal (org-entry-get (point) "beeminder" t))
	   ;; Get the updated goal from Beeminder
	   (result (beeminder-fetch-goal beeminder-username goal)))

      ;; Update properties
      (org-entry-put (point) "pledge" (format "%s" (cdr (assoc 'pledge result))))
      (org-entry-put (point) "type"   (cdr (assoc 'goal_type result)))
      (org-entry-put (point) "target" (format "%s" (cdr (assoc 'goalval result))))
      (org-entry-put (point) "lane"   (format "%s" (cdr (assoc 'lane result))))

      ;; Update deadline
      (org-deadline nil (format-time-string "%Y-%m-%d %a %H:%M" (seconds-to-time (assoc-default 'goaldate result)))))))


;; ORG-Mode stuff

(defun beeminder-my-goals-org ()
  "Insert your Beeminder goals as an 'org-mode' headline list."
  (interactive)

  ;; Insert the main headline
  (insert
   (format "* Beeminder goals for %s\n" beeminder-username)
   (mapconcat
    (lambda (goal)
      ;; Insert the goal name and tags
      (format (concat "** TODO %s %s\n"
		      "  DEADLINE: <%s>\n"
		      "   :PROPERTIES:\n"
		      "   :beeminder: %s\n"
		      "   :type:   %s\n"
		      "   :pledge: %s\n"
		      "   :target: %s\n"
		      "   :STYLE: habit\n"
		      "   :END:\n")
	      (assoc-default 'title goal)
	      beeminder-goal-org-tags
	      (format-time-string
	       "%Y-%m-%d %a %H:%M"
	       (seconds-to-time (assoc-default 'losedate goal)))
	      (assoc-default 'slug goal)
	      (assoc-default 'goal_type goal)
	      (assoc-default 'pledge goal)
	      (assoc-default 'goalval goal)))
    (beeminder-fetch-goals beeminder-username)
    "\n")))

;; Main API Endpoints

(defun beeminder-fetch-goals (&optional username)
  "Fetch a list of all goals for a single USERNAME."
  (beeminder-fetch
   (format "users/%s/goals.json?auth_token=%s" (or username beeminder-username) beeminder-auth-token)))

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-fetch
   (format "users/%s/goals/%s.json?auth_token=%s" username goal beeminder-auth-token)))

(defun beeminder-add-data (goal value comment)
  "Update Beeminder GOAL with VALUE and COMMENT."
  (interactive "MGoal: \nnValue: \nMComment: \n")
  (let ((result
	 ;; Send the request
	 (beeminder-post
	  (format "users/%s/goals/%s/datapoints.json" beeminder-username goal)
	  (format "auth_token=%s&value=%s&comment=%s"
		  beeminder-auth-token
		  value
		  (url-hexify-string comment)))))
    ;; Show what happened
    (message
     "Data added at %s"
     (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time (assoc-default 'timestamp result))))))


;; GET/POST Helpers

(defun beeminder-fetch (action)
  "Perform ACTION on the Beeminder API."
  (let* ((action (if (symbolp action) (symbol-name action) action))
	 (url (format "%s%s" beeminder-v1-api action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
	(kill-buffer)))))

;;;###autoload
(defun beeminder-post (action args)
  "Perform a POST request to ACTION with ARGS."
  (let* ((url-request-method "POST")
	 (url-request-data args)
	 (url (format "%s%s" beeminder-v1-api action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
	(kill-buffer)))))


(provide 'beeminder)
;;; beeminder.el ends here
