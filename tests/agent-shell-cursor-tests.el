;;; agent-shell-cursor-tests.el --- Tests for agent-shell-cursor -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-cursor)

;;; Code:

(ert-deftest agent-shell-cursor--make-text-content-block-test ()
  "Test `agent-shell-cursor--make-text-content-block'."
  (should (equal (agent-shell-cursor--make-text-content-block "hello")
                 '((type . "content")
                   (content (type . "text") (text . "hello")))))
  (should-not (agent-shell-cursor--make-text-content-block ""))
  (should-not (agent-shell-cursor--make-text-content-block nil)))

(ert-deftest agent-shell-cursor--content-from-raw-output-test ()
  "Test `agent-shell-cursor--content-from-raw-output'."
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((error . "permission denied")))
                 (list '((type . "content")
                         (content (type . "text") (text . "permission denied"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((content . "file contents")))
                 (list '((type . "content")
                         (content (type . "text") (text . "file contents"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "hello")
                    (stderr . "warning")
                    (exitCode . 0)))
                 (list '((type . "content")
                         (content (type . "text")
                                  (text . "```\nExit code: 0\n\nhello\n\nwarning\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "done")))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\ndone\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((exitCode . 1)))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\nExit code: 1\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)
                    (truncated . t)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches (truncated)"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((resultCount . 7)))
                 (list '((type . "content")
                         (content (type . "text") (text . "7 results"))))))
  (should-not (agent-shell-cursor--content-from-raw-output nil))
  (should-not (agent-shell-cursor--content-from-raw-output '((unknown . "value")))))

(ert-deftest agent-shell-cursor--notification-adapter-test ()
  "Test `agent-shell-cursor--notification-adapter'."
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ((type . "content")
                                       (content (type . "text")
                                                (text . "keep me"))))
                           (rawOutput (stdout . "ignored")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   '((type . "content")
                     (content (type . "text") (text . "keep me"))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ())
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "in_progress")
                           (rawOutput (error . "permission denied")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "permission denied")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (content . "file contents")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "file contents")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (totalMatches . 42)
                                      (truncated . t)))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "42 matches (truncated)")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (unknown . "value")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "agent_message_chunk")
                           (content . "hello"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   "hello")))
  (let ((notification
         '((method . "session/request")
           (params (foo . "bar")))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal notification
                   '((method . "session/request")
                     (params (foo . "bar")))))))

(ert-deftest agent-shell--adapt-notification-cursor-integration-test ()
  "Test `agent-shell--adapt-notification' with Cursor config."
  (let* ((config (agent-shell-cursor-make-agent-config))
         (state (agent-shell--make-state :agent-config config))
         (notification
          '((method . "session/update")
            (params (update (sessionUpdate . "tool_call_update")
                            (status . "completed")
                            (rawOutput (stdout . "hello")))))))
    (agent-shell--adapt-notification :state state :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```"))))))))

(ert-deftest agent-shell-cursor--update-todos-replace-and-merge-test ()
  "Replace todos, then merge updates by ID."
  (let ((state (agent-shell--make-state))
        published)
    (cl-letf (((symbol-function 'agent-shell-cursor--publish-todos)
               (lambda (updated-state)
                 (setq published (copy-tree (map-elt updated-state :todos))))))
      (should
       (agent-shell-cursor--update-todos
        `((:state . ,state)
          (:acp-notification
           (method . "cursor/update_todos")
           (params
            (merge . :json-false)
            (todos . [((id . "one") (content . "First") (status . "pending"))
                      ((id . "two") (content . "Second") (status . "in_progress"))]))))))
      (should (equal published
                     '(((:id . "one") (:content . "First") (:status . "pending"))
                       ((:id . "two") (:content . "Second") (:status . "in_progress")))))
      (agent-shell-cursor--update-todos
       `((:state . ,state)
         (:acp-notification
          (method . "cursor/update_todos")
          (params
           (merge . t)
           (todos . [((id . "one") (content . "First") (status . "completed"))
                     ((id . "three") (title . "Third") (status . "canceled"))])))))
      (should (equal (map-elt state :todos)
                     '(((:id . "one") (:content . "First") (:status . "completed"))
                       ((:id . "two") (:content . "Second") (:status . "in_progress"))
                       ((:id . "three") (:content . "Third") (:status . "cancelled"))))))))

(ert-deftest agent-shell-cursor--update-todos-request-test ()
  "Acknowledge a Cursor todo request after the state update."
  (let ((state (agent-shell--make-state))
        response)
    (cl-letf (((symbol-function 'agent-shell-cursor--publish-todos) #'ignore)
              ((symbol-function 'acp-send-response)
               (lambda (&rest args)
                 (setq response (plist-get args :response)))))
      (should
       (agent-shell-cursor--request-handler
        `((:state . ,state)
          (:acp-request
           (id . 0)
           (method . "cursor/update_todos")
           (params
            (toolCallId . "call_3gAx9U34jeFxKkZMUvNiqQO4")
            (todos . [((id . "traffic-demo-1")
                       (content . "Inspect the Agent Shell request traffic")
                       (status . "in_progress"))
                      ((id . "traffic-demo-2")
                       (content . "Verify the plan update payload")
                       (status . "pending"))
                      ((id . "traffic-demo-3")
                       (content . "Confirm that task status changes appear")
                       (status . "pending"))])
            (merge))))))
      (should (= 0 (map-elt response :request-id)))
      (should (hash-table-p (map-elt response :result)))
      (should (= 3 (length (map-elt state :todos))))
      (should (equal (map-elt (car (map-elt state :todos)) :status)
                     "in_progress")))))

(ert-deftest agent-shell-cursor--publish-todos-test ()
  "Render todo progress and emit the current todo state."
  (let ((state (agent-shell--make-state))
        fragment
        event)
    (map-put! state :todos
              '(((:id . "one") (:content . "First") (:status . "completed"))
                ((:id . "two") (:content . "Second") (:status . "in_progress"))))
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (setq fragment args)))
              ((symbol-function 'agent-shell--emit-event)
               (lambda (&rest args) (setq event args))))
      (agent-shell-cursor--publish-todos state)
      (should (equal (plist-get fragment :label-left) "Todos"))
      (should (equal (plist-get fragment :label-right) "1/2 completed"))
      (should (string-match-p "First" (plist-get fragment :body)))
      (should (string-match-p "Second" (plist-get fragment :body)))
      (should (eq (plist-get event :event) 'todo-update))
      (should (equal (map-elt (cadr (map-elt (plist-get event :data) :todos))
                              :status)
                     "in_progress")))))

(provide 'agent-shell-cursor-tests)
;;; agent-shell-cursor-tests.el ends here
