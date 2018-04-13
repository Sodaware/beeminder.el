;;; beeminder-api.el --- API Wrapper functions for Beeminder -*- lexical-binding: t; -*-

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
                   (format "users/%s/goals" username))))

(defun beeminder-create-goal (parameters)
  "Create a Goal for the current global user with a list of PARAMETERS."
  (let ((required-params (list :slug :title :goal_type :goaldate :goalval :rate :initval))
        (optional-params (list :secret :datapublic :datasource :dryrun)))
    ;; TODO: Check only required + optional parameters have been set.
    (beeminder--post (beeminder--create-endpoint
                      (format "users/%s/goals" beeminder-username))
                     (list :auth_token beeminder-auth-token))))

(defun beeminder-fetch-goals (&optional username)
  "Fetch a list of all goals for the global user, or USERNAME if supplied."
  (let ((user (or username beeminder-username)))
    (beeminder--get (beeminder--create-endpoint
                     (format "users/%s/goals" user)
                     (list :auth_token beeminder-auth-token)))))

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
                   (list :auth_token beeminder-auth-token)))

(defun beeminder-stepdown (goal)
  "Decrease GOAL's pledge level subject to the akrasia horizon."
  (beeminder--post (beeminder--create-endpoint
                    (format "users/%s/goals/%s/stepdown" beeminder-username goal))
                   (list :auth_token beeminder-auth-token)))

(defun beeminder-cancel-stepdown (goal)
  "Cancel a pending stepdown of GOAL's pledge."
  (beeminder--post (beeminder--create-endpoint
                    (format "users/%s/goals/%s/cancel_stepdown" beeminder-username goal))
                   (list :auth_token beeminder-auth-token)))


;; --------------------------------------------------
;; -- API endpoints - datapoints

(defun beeminder-get-datapoints (username goal)
  "Get the list of datapoints for USERNAME's GOAL."
  (beeminder--get (beeminder--create-endpoint
                   (format "users/%s/goals/%s/datapoints" beeminder-username goal))))


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

;;;###autoload
(defun beeminder--get (action)
  "Perform a GET request to ACTION."
  (let* ((action (if (symbolp action) (symbol-name action) action))
         (url (format "%s%s" beeminder-v1-api-endpoint action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

;;;###autoload
(defun beeminder--post (action args)
  "Perform a POST request to ACTION with ARGS."
  (let* ((url-request-method "POST")
         (url-request-data args)
         (url (format "%s%s" beeminder-v1-api-endpoint action)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(provide 'beeminder-api)
;;; beeminder-api.el ends here
