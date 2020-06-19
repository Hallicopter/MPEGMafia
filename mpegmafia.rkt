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
  (set! played-frame-count (+ played-frame-count last-frame-played))
  (values last-frame-played input-pstream))


; Continues to play the current song
; if the is-playing flag is #f
(define (continue last-frame-played  input-file)
  (define end-frame (rs-frames input-file))
  (define input-sound (clip input-file last-frame-played end-frame))
  (define input-pstream (make-pstream))
  (pstream-play input-pstream input-sound)
  (values input-pstream input-sound))

; Scrub forward
; Moves forward by 10,000 frames (~0.25s)
(define (forward last-frame-played input-rsound input-pstream)
  (set! played-frame-count ( + (+ played-frame-count (pstream-current-frame input-pstream)) 20000))
  (stop)
  (define end-frame (rs-frames input-rsound))
  (define forwarded-rsound (clip input-rsound (+ last-frame-played 20000) end-frame))
  (set! input-pstream (make-pstream))
  (pstream-play input-pstream forwarded-rsound)
  (values input-pstream forwarded-rsound (+ last-frame-played 20000)))

; Scrub backward
; Moves backward by 10,000 frames (~0.25s)
(define (backward last-frame-played input-pstream song)
  (set! played-frame-count ( - (+ played-frame-count (pstream-current-frame input-pstream)) 60000))
  (stop)
  (define end-frame (rs-read-frames song))
  (define input-sound (rs-read/clip song (- last-frame-played 60000) end-frame))
  (set! input-pstream (make-pstream))
  (pstream-play input-pstream input-sound)
  (values input-pstream input-sound (- last-frame-played 60000)))
  
; Creates channel to communcate current
; frame info for the progress bar, WIP
(define frame-pos-channel (make-channel))

; Define the terminal buffer for progress bar
(define buf (make-cached-buffer 4 100))

; Progress bar thread
(define progress-bar (thread (lambda ()
                               (let loop ()
                                 (define counter (channel-get frame-pos-channel))
                                 
                                 (draw buf (vappend (text (string-append (format "~v" counter) " sec"))
                                                    (hline counter)
                                                    (text "Playing song. Press p to pause, c to continue, e to exit.")
                                                    #:halign 'left))
                                 (sleep 0.4)
                                 (cond [(equal? 100 counter)
                                        #f]
                                       [else (loop)])))))

; Infinite loop for taking input
(define (input-loop)
  (with-charterm
      (let ((command (charterm-read-key)))
        (cond [(char=? command #\p)
               (set!-values (last-frame-played input-pstream) (pause input-pstream))
               (set! is-playing #f)
               (input-loop)]
              [(and (char=? command #\c) (not is-playing))
               (set!-values (input-pstream input-rsound) (continue last-frame-played input-rsound))
               (set! is-playing #t)
               (input-loop)]
              [(and (char=? command #\j) is-playing)
               (set!-values
                (input-pstream input-rsound last-frame-played)
                (backward (+ played-frame-count (pstream-current-frame input-pstream)) input-pstream song))
               (set! is-playing #t)
               (input-loop)]
              [(and (char=? command #\k) is-playing)
               (set!-values (input-pstream input-rsound last-frame-played) (forward (pstream-current-frame input-pstream)  input-rsound input-pstream))
               (set! is-playing #t)
               (input-loop)]
              [(char=? command #\e)  (displayln "\nExited successfully...")]        
              [else (displayln "unknown command") (input-loop)]))))

  

; Babble for testing purposes
(define song "riptide.wav")
(define is-playing #t)
(define-values (input-pstream input-rsound) (play song))
(define last-frame-played 0)
(define played-frame-count 0)

; Thread to update progress bar channel
(define update-progress-bar (thread (lambda ()
                                      (let loop ()
                                        (define counter (round
                                                         (/ (+ played-frame-count (pstream-current-frame input-pstream)) 44100)))
                                        (channel-put frame-pos-channel counter)
                                        (sleep 1)
                                        (loop )))))
(input-loop)