;;; beeminder-client.el --- Emacs client interface for Beeminder. -*- lexical-binding: t; -*-

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

(require 'beeminder)


;; --------------------------------------------------
;; -- Legacy interactive functions.

(defun beeminder-add-data (goal value comment)
  "Update Beeminder GOAL with VALUE and COMMENT."
  (interactive "MGoal: \nnValue: \nMComment: \n")
  (let ((result
         ;; Send the request.
         (beeminder--post
          (beeminder--create-endpoint
           (format "users/%s/goals/%s/datapoints" beeminder-username goal))
          (beeminder--build-post-body
           (list :auth_token beeminder-auth-token
                 :value      value
                 :comment    (url-hexify-string comment))))))
    ;; Show what happened.
    (message
     "Data added at %s"
     (format-time-string "%Y-%m-%d %a %H:%M:%S"
                         (seconds-to-time (assoc-default 'timestamp result))))))

(defun beeminder-whoami ()
  "Display the Beeminder username for your auth token."
  (interactive)
  (let ((result (beeminder-me)))
    (if (beeminder--api-valid-response-p result)
        (message "Your Beeminder username: %s" (assoc-default 'username result))
        (error "Beeminder error: beeminder-auth-token is invalid or empty"))))

(defun beeminder-my-goals ()
  "Display your goals in the Message buffer (kind of useless)."
  (interactive)
  (message
   "%s"
   (mapconcat (lambda (goal)
                (format "Goal: %s" (assoc-default 'title goal)))
              (beeminder-fetch-goals beeminder-username)
              "\n")))


;; --------------------------------------------------
;; -- Deprecated functions

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-get-user-goal username goal))


(provide 'beeminder-client)
;;; beeminder-client.el ends here
