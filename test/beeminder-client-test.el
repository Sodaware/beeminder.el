;; beeminder-client-test.el --- Tests for beeminder-client file.

;;; Commentary:

;; Tests for `beeminder-client.el`.  Mostly front-end facing stuff.

;;; Code:

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
