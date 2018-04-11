;;; beeminder-client.el --- Emacs client interface for Beeminder.

;; Copyright (C) 2014-2016 Phil Newton

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

;; This file contains a simple Beeminder client.

;;; Code:

;; Dependencies

;; Interactive functions.

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

(defun beeminder-whoami ()
  "Display the Beeminder username for your auth token."
  (interactive)
  ;; TODO: Display an error if not logged in.
  (let ((result (beeminder-me)))
    (if result
        (message "Your Beeminder username: %s" (assoc-default 'username result))
        (warning "beeminder-auth-token is invalid or empty."))))

(defun beeminder-my-goals ()
  "Display your goals in the Message buffer (kind of useless)."
  (interactive)
  (message
   "%s"
   (mapconcat (lambda (goal)
                (format "Goal: %s" (assoc-default 'title goal)))
              (beeminder-fetch-goals beeminder-username)
              "\n")))



(provide 'beeminder-client)
;;; beeminder-client.el ends here
