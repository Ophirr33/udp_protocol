#! /proj/racket/plt-released/recent/bin/racket
#lang racket/base

(require json)
(require racket/date)
(require racket/string)
(require racket/udp)
;; log the given message
(define (log str)
  (write (string-append (now) " " str "\n") (current-error-port)))


;; get the time as hour:minute:second:microsecond
(define (now)
  (define (upcast n)
    (if (integer? n) (inexact->exact n)
        (upcast (* 10 n))))
  (let* ([milli (current-inexact-milliseconds)]
         [d (current-date)]
         [micro   (number->string (upcast(- milli (floor milli))))]
         [seconds (number->string (date-second d))]
         [minutes (number->string (date-minute d))]
         [hours   (number->string (date-hour d))])
    (string-append hours ":" minutes ":" seconds ":" micro)))

; define values
(define-values (msg-size timeout udp-ip udp-port)
  (values 1500 30 "127.0.0.1" 0))

; Set up the socket and bind to localhost/ephemeral port
(define socket (udp-open-socket))
(udp-bind! socket udp-ip udp-port)
; Get port we bound to
(let-values ([(local-address local-port remote-address remote-port)
              (udp-addresses socket #t)])
    (log (string-append "[bound] " (number->string local-port))))



; Now listen for packets
(let loop ([data-read (make-bytes msg-size)])
  (define-values (bytes-received address port)
    (udp-receive! socket data-read 0 msg-size))
  (if bytes-received
      (with-handlers  ([exn:fail? (lambda (exn) (log "[recv corrupt packet]"))])
        (let ([decoded (string->jsexpr data-read)])

          ; If the EOF flag is set, exit
          (when (hash-ref decoded 'end)
            (begin (log "[completed]")
                   (exit 0)))

          (when (hash-ref decoded 'data)
            ;; IF we received data, we assume it's in order
            (begin (log (string-append "[recv data] "
                                       (number->string (hash-ref decoded 'sequence))
                                       " ("
                                       (number->string (string-length (hash-ref decoded 'data)))
                                       ") ACCEPTED (in-order)"))
                   (write (hash-ref decoded 'data))
                   (flush-output (current-output-port))))
         
          ;; Send back an ack to the sender          
          (define msg (jsexpr->string (hash 'ack (+ (hash-ref decoded 'sequence)
                                                     (string-length (hash-ref decoded 'data))))))
          (log (string-append "ABOUT TO SEND " msg))
          (unless (udp-send-to* socket address port data-read)
              (log "[error] unable to fully send packet"))
          (loop data-read)
          ))
      (begin (log "[error] timeout")
             (exit -1))))

(udp-close socket)
