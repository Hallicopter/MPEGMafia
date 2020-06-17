#lang racket
(require rsound)
(require charterm)
(require raart)

; This creates the initail rsound
; for a song, this rsound is passed around
; so the whole song doesn't have to be
; decoded from the file everytime.
(define (play filepath)
  (define input-pstream (make-pstream))
  (define input-rsound (rs-read filepath))
  (pstream-play input-pstream input-rsound)
  (values input-pstream input-rsound))

; Simple pause function
; stops any sound on any audio channel
(define (pause input-pstream)
  (define last-frame-played (pstream-current-frame input-pstream))
  (stop)
  (values last-frame-played input-pstream))


; Continues to play the current song
; if the is-playing flag is #f
(define (continue song last-frame-played  input-file)
  (define end-frame (rs-frames input-file))
  (define input-sound (clip input-file last-frame-played end-frame))
  (define input-pstream (make-pstream))
  (pstream-play input-pstream input-sound)
  (values input-pstream input-sound))

; Creates channel to communcate current
; frame info for the progress bar, WIP
(define frame-pos-channel (make-channel))

; Progress bar thread
(define progress-bar (thread (lambda ()
                               (let loop ([counter 0])
                                 (define buf (make-cached-buffer 4 100))
                                 (draw buf (vappend2 (hline counter)
                                                     (text "Playing song. Press p to pause, c to continue, e to exit.")
                                                     #:halign 'left))
                                 (sleep 0.2)
                                 (cond [(equal? 100 counter)
                                        #f]
                                       [else (loop (add1 counter))])))))
                                   
                                   

; Infinite loop for taking input
(define (input-loop)
  (with-charterm
      (let ((command (charterm-read-key)))
        (cond [(char=? command #\p)
               (set!-values (last-frame-played input-pstream) (pause input-pstream))
               (set! is-playing #f)
               (input-loop)]
              [(and (char=? command #\c) (not is-playing))
               (set!-values (input-pstream input-rsound) (continue song last-frame-played input-rsound))
               (set! is-playing #t)
               (input-loop)]
              [(char=? command #\e)  (displayln "exited successfully...")]        
              [else (displayln "unknown command") (input-loop)]))))

; Babble for testing purposes

(define song "riptide.wav")
(define is-playing #t)
(define-values (input-pstream input-rsound) (play song))
(define last-frame-played 0)
(input-loop)