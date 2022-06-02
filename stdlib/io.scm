
(define socket-port (socket-server->i/o-port 9002))
(set-current-repl-prompt! "")
(set-current-output-port! socket-port)
(set-current-input-port! socket-port)

;; revert
; (set-current-output-port! *default-output-port*)
; (set-current-input-port! *default-input-port*)
; (set-current-repl-prompt! "> ")
