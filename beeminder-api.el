;;; beeminder-api.el --- API Wrapper functions for Beeminder -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Phil Newton

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

;; This file contains functions for working with the Beeminder API.  These can
;; be used by other libraries to interact with Beeminder.

;; See http://api.beeminder.com/#beeminder-api-reference for official Beeminder
;; API documentation.

;;; Code:

;; Dependencies

(require 'beeminder)
(require 'json)
(require 'url-http)

(defvar url-http-end-of-headers)


;; --------------------------------------------------
;; -- Configuration

(defconst beeminder-v1-api-endpoint
  "https://www.beeminder.com/api/v1/"
  "The endpoint for version 1.0 of the Beeminder API.")


;; --------------------------------------------------
;; -- API endpoints - users

(defun beeminder-user-info (username)
  "Retrieve information and a list of goalnames for the user USERNAME."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s" username)
                   (list :auth_token beeminder-auth-token))))

(defun beeminder-me ()
  "Get the username associated with the current auth token."
  (beeminder-user-info "me"))


;; --------------------------------------------------
;; -- API endpoints - goals

(defun beeminder-get-user-goal (username goal)
  "Get goal details for USERNAME's GOAL."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s/goals/%s" username goal)
                   (list :auth_token beeminder-auth-token))))

(defun beeminder-get-user-goals (username)
  "Get a list of all goals for USERNAME."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s/goals" username)
                   (list :auth_token beeminder-auth-token))))

(defun beeminder-fetch-goals (&optional username)
  "Fetch a list of all goals for the global user, or USERNAME if supplied."
  (beeminder-get-user-goals (or username beeminder-username)))

(defun beeminder-refresh-goal-graph (username goal)
  "Force a refresh of the USERNAME's GOAL graph."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s/goals/%s/refresh_graph" username goal))))

(defun beeminder-short-circuit (goal)
  "Cause a failure of GOAL.

ATTENTION: This will increase the pledge level of GOAL and charge
the user their current pledge level."
  (beeminder--post (beeminder--create-endpoint
                    (format "users/%s/goals/%s/shortcircuit" beeminder-username goal))
                   (beeminder--build-post-body
                    (list :auth_token beeminder-auth-token))))

(defun beeminder-stepdown (goal)
  "Decrease GOAL's pledge level subject to the akrasia horizon."
  (beeminder--post (beeminder--create-endpoint
                    (format "users/%s/goals/%s/stepdown" beeminder-username goal))
                   (beeminder--build-post-body
                    (list :auth_token beeminder-auth-token))))

(defun beeminder-cancel-stepdown (goal)
  "Cancel a pending stepdown of GOAL's pledge."
  (beeminder--post (beeminder--create-endpoint
                    (format "users/%s/goals/%s/cancel_stepdown" beeminder-username goal))
                   (beeminder--build-post-body
                    (list :auth_token beeminder-auth-token))))


;; --------------------------------------------------
;; -- API endpoints - datapoints

(defun beeminder-get-datapoints (username goal)
  "Get the list of datapoints for USERNAME's GOAL."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s/goals/%s/datapoints" username goal))))


;; --------------------------------------------------
;; -- Deprecated functions

(defun beeminder-fetch-goal (username goal)
  "Fetch data for USERNAME's GOAL."
  (beeminder-get-user-goal username goal))


;; --------------------------------------------------
;; -- Goal helpers

(defun beeminder--goal-derailed-p (goal)
  "Check if GOAL is derailed."
  (eq t (assoc-default 'lost goal)))

(defun beeminder--goal-in-red-p (goal)
  "Check if GOAL is in the red."
  (eq :red (beeminder--goal-dot-color goal)))

(defun beeminder--goal-in-orange-p (goal)
  "Check if GOAL is in the orange."
  (eq :orange (beeminder--goal-dot-color goal)))

(defun beeminder--goal-fresh-p (goal)
  "Get the fresh indicator for GOAL.

GOAL is fresh if it had data submitted today."
  (beeminder--goal-data-added-after-p goal (parse-time-string (format-time-string "%F 00:00:00"))))

(defun beeminder--goal-dot-color (goal)
  "Get the dot color for GOAL."
  (let ((color (* (assoc-default 'yaw goal) (assoc-default 'lane goal))))
    (cond ((>  color  1) :green)
          ((=  color  1) :blue)
          ((=  color -1) :orange)
          ((<= color -2) :red))))

(defun beeminder--goal-data-added-after-p (goal timestamp)
  "Test if GOAL's last datapoint was entered after TIMESTAMP."
  (let ((goal-timestamp (format-time-string "%s" (seconds-to-time (assoc-default 'lastday goal))))
        (test-timestamp (format-time-string "%s" (apply 'encode-time timestamp))))
    (> (string-to-number goal-timestamp)
       (string-to-number test-timestamp))))


;; --------------------------------------------------
;; -- URL helpers

(defun beeminder--create-endpoint (path &optional query-vars)
  "Build an endpoint to the api using PATH and optional QUERY-VARS."
  (format "%s%s.json%s"
          beeminder-v1-api-endpoint
          path
          (beeminder--build-query query-vars)))

(defun beeminder--build-query (query-vars)
  "Build a query string using QUERY-VARS and prepend it with a `?` symbol.

QUERY-VARS should be a list of symbols and their corresponding values.

For example (:key value :other-key value) will generate the following string:
 ?key=value&other-key=value"
  (if (null query-vars)
      ""
      (progn (let (query-string)
               (dolist (var query-vars)
                 (if (symbolp var)
                     (setq query-string (concat query-string (substring (symbol-name var) 1) "="))
                     (setq query-string (format "%s%s&" query-string var))))
               (concat "?" (substring query-string 0 -1))))))


;; --------------------------------------------------
;; -- Request Helpers

(defun beeminder--build-post-body (query-vars)
  "Build a post-compatible query string using QUERY-VARS.

QUERY-VARS should be a list of symbols and their corresponding values.

For example (:key value :other-key value) will generate the following string:
 key=value&other-key=value"
  (if (null query-vars)
      ""
      (progn (let (query-string)
               (dolist (var query-vars)
                 (if (symbolp var)
                     (setq query-string (concat query-string (substring (symbol-name var) 1) "="))
                     (setq query-string (format "%s%s&" query-string var))))
               (substring query-string 0 -1)))))

(defun beeminder--api-error-p (result)
  "Check if RESULT is an api error."
  (assoc-default 'errors result))

(defun beeminder--api-valid-response-p (result)
  "Check if RESULT is valid."
  (not (beeminder--api-error-p result)))

(defun beeminder--get (url)
  "Perform a GET request to URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer))))

(defun beeminder--post (url args)
  "Perform a POST request to URL with ARGS."
  (let ((url-request-method "POST")
        (url-request-data args))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(provide 'beeminder-api)
;;; beeminder-api.el ends here
