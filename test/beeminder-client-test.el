;; beeminder-client-test.el --- Tests for beeminder-client file.

;;; Commentary:

;; Tests for `beeminder-client.el`.  Mostly front-end facing stuff.

;;; Code:


;; --------------------------------------------------
;; -- beeminder-goals

(ert-deftest beeminder-client-test/beeminder-goals-creates-new-buffer-if-doesnt-exist ()
  (with-mock
   (stub beeminder--initialize-goals-buffer)
   (stub beeminder-goals-mode)
   (let ((beeminder-username "test_user"))
     (should-not (get-buffer "beeminder: test_user"))
     (beeminder-goals)
     (should (get-buffer "beeminder: test_user")))))


;; --------------------------------------------------
;; -- beeminder--initialize-goals-buffer

(ert-deftest beeminder-client-test/initialize-goals-buffer-inserts-headlines ()
  (with-mock
   (stub beeminder--get)
   (with-temp-buffer
     (let ((beeminder-username "test_user"))
       (beeminder--initialize-goals-buffer)
       (should (string= "Beeminder goals for: test_user" (buffer-line-contents 1)))
       (should (string= "Active Goals (2)" (buffer-line-contents 4)))
       (should (string= "Archived Goals (0)" (buffer-line-contents 9)))))))


;; --------------------------------------------------
;; -- beeminder--goal-status-indicator

;; Returns 4 spaces for goals that are on target and have no data today.
(ert-deftest beeminder-client-test/goal-status-empty-by-default ()
  (should (string= "    " (beeminder--goal-status-indicator nil)))
  )

;; Char 1 contains a ! if goal is derailed.
;; Char 2 contains a ! if goal is going to derail today
;; Char 3 contains a ! if goal is going to derail today or tomorrow
;; Returns a checkmark at the end if goal has had data submitted.


(ert-deftest beeminder-client-test/-goals ()
  )


;; --------------------------------------------------
;; -- beeminder-add-data

(ert-deftest beeminder-client-test/add-data-submits-data ()
  (let ((beeminder-auth-token "ABCDEF")
        (beeminder-username   "test_user")
        (inhibit-message      t))
    (with-mock
     (mock-post "users/test_user/goals/example_goal/datapoints.json"
                (list :auth_token "ABCDEF"
                      :value       1.5
                      :comment     "example%20comment")
                "add-data-response.json")
     (beeminder-add-data "example_goal" 1.5 "example comment")
     (should (string= "Data added at 2012-07-29 Sun 12:00:00" (last-message))))))


;; --------------------------------------------------
;; -- beeminder-whoami

(ert-deftest beeminder-client-test/whoami-displays-warning-when-no-auth-token ()
  (let ((beeminder-auth-token ""))
    (with-mock
     (mock-invalid-get "users/me.json" "incorrect_token.json")
     (should-error (beeminder-whoami)))))

(ert-deftest beeminder-client-test/whoami-displays-username-when-valid ()
  (let ((beeminder-auth-token "ABCDEF")
        (inhibit-message t))
    (with-mock
     (mock-get "users/me.json" "users-test_user.json")
     (beeminder-whoami)
     (should (string= "Your Beeminder username: test_user" (last-message))))))


;; --------------------------------------------------
;; -- beeminder-my-goals

(ert-deftest beeminder-client-test/my-goals-displays-goals ()
  (let ((beeminder-auth-token "ABCDEF")
        (beeminder-username "example")
        (inhibit-message t))
    (with-mock
     (mock-get "users/example/goals.json" "user-example-goals.json")
     (beeminder-my-goals)
     (should (string= "Goal: Example Goal" (last-message))))))


;;; beeminder-client-test.el ends here
