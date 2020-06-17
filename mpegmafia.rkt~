#lang racket
(require rsound)

(define (play filepath)
  (define input-pstream (make-pstream))
  (define input-rsound (rs-read filepath))
  (pstream-play input-pstream input-rsound)
  (values input-pstream input-rsound))

(define (pause input-pstream init-frame)
  (define last-frame-played (pstream-current-frame input-pstream))
  (stop)
  (values (+ last-frame-played init-frame) input-pstream))

(define (continue song last-frame-played  input-file)
  (displayln last-fram-played)
  (define end-frame (rs-frames input-file))
  (define input-sound (clip input-file last-frame-played end-frame))
  (define input-pstream (make-pstream))
  (pstream-play input-pstream input-sound)
  (values input-pstream input-sound))

(define (input-loop)
  (display "P->pause, C->continue")
  (define command (read-line))
  (cond [(string=? command "P")
         (set!-values (last-frame-played input-pstream) (pause input-pstream last-frame-played))
         (displayln last-fram-played)
         (input-loop)]
        [(string=? command "C")
         (set!-values (input-pstream input-rsound) (continue song last-frame-played input-rsound))
         (input-loop)]
        [(string=? command "exit")  (displayln "exited successfully...")]        
        [else (displayln "unknown command") (input-loop)]))

(define song "riptide.wav")
(define-values (input-pstream input-rsound) (play song))
(define last-frame-played 0)
(input-loop)