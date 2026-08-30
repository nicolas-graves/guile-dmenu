(use-modules (guile-dmenu mcp)
             (srfi srfi-64))

(test-begin "dmenu MCP server")

(define (field object name)
  (cdr (or (assoc name object) (assoc (string->symbol name) object))))

(let* ((response (handle-mcp-message
                  '((jsonrpc . "2.0") (id . 1) (method . "initialize")
                    (params . ()))))
       (payload (field response "result")))
  (test-equal "initialize reports the MCP protocol"
    "2025-06-18" (field payload "protocolVersion")))

(let* ((response (handle-mcp-message
                  '((jsonrpc . "2.0") (id . 2) (method . "tools/list")
                    (params . ()))))
       (tools (field (field response "result") "tools")))
  (test-equal "question tool is advertised"
    "ask_questions" (field (vector-ref tools 0) "name")))

(parameterize
    ((mcp-question-handler
      (lambda (arguments)
        '((status . "answered")
          (answers . #(((id . "choice") (answer . "yes"))))))))
  (let* ((message
          '((jsonrpc . "2.0")
            (id . 3)
            (method . "tools/call")
            (params
             . ((name . "ask_questions")
                (arguments
                 . ((questions
                     . #(((id . "choice")
                          (prompt . "Continue?")
                          (options
                           . #(((id . "yes") (label . "Yes"))
                               ((id . "no") (label . "No")))))))))))))
         (response (handle-mcp-message message))
         (payload (field response "result"))
         (structured (field payload "structuredContent")))
    (test-equal "tool call returns structured answers"
      "answered" (field structured "status"))
    (test-assert "successful tool call is not an error"
      (not (field payload "isError")))))

(parameterize ((mcp-question-handler
                (lambda (arguments)
                  (sleep 5)
                  '((status . "answered") (answers . #())))))
  (let* ((input
          (string-append
           "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/call\",\"params\":{\"name\":\"ask_questions\",\"arguments\":{}}}\n"
           "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"ping\",\"params\":{}}\n"
           "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":10}}\n"))
         (output (open-output-string)))
    (call-with-input-string input
      (lambda (port) (run-mcp-server port output)))
    (let ((text (get-output-string output)))
      (test-assert "a pending question does not block protocol traffic"
        (string-contains text "\"id\":11"))
      (test-assert "cancelled questions emit no stale response"
        (not (string-contains text "\"id\":10"))))))

(test-end "dmenu MCP server")
