#lang racket
(require net/http-easy)
(require xml)
(require xml/path)
(require net/url)

(struct podcast (title link))

; Helper function to remove ampersands
(define (remove-ampersand str)
  (apply string
         (map (λ (c) (cond [(char=? c #\&)
                            #\a]
                           [else c]))
              (string->list str))))


; Returns episode list given podcast rss
(define (get-podcast-list rss-path)
  (define res
    (get rss-path))

  (define rss-xexpr (xml->xexpr (document-element
                                 (read-xml (open-input-string
                                            (remove-ampersand (bytes->string/locale (response-body res))))))))
  (define ep-list (se-path*/list '(item title) rss-xexpr))
  (define ep-links (se-path*/list '(item enclosure #:url) rss-xexpr))
  (map (λ (title link)
         (define pod (podcast title link))
         pod ) ep-list ep-links))

; Downloads the podcast, and saves it locally
(define (download-podcast podcast)
  (define output-file (string-append (podcast-title podcast) ".mp3"))
  (call-with-output-file output-file
    (lambda (p) (display (port->bytes (get-pure-port #:redirections 10 (string->url (podcast-link podcast)))) p))
    #:exists 'replace))
