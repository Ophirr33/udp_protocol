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


(define-values (msg-size data-size timeout sequence) (values 15 1000 30 0))
(define args (current-command-line-arguments))
;; define ip and port as a pair
(define udp-ip/port
  (let ([lst (string-split (vector-ref args 0) ":")])
    (cons (car lst) (string->number (cadr lst)))))
;; initialize the socket
(define socket (udp-open-socket))
;; clients connect, servers bind
(udp-connect! socket (car udp-ip/port) (cdr udp-ip/port))

(define (send-next-packet)
  (let* ([data (read-bytes data-size)]
         [msg (hash 'sequence sequence 'data (bytes->string/utf-8 data) 'ack #f 'end #f)]
         [len (bytes-length data)])
    (if (> len 0)
        (begin (set! sequence (+ sequence len))
               (udp-send socket (string->bytes/utf-8 (jsexpr->string msg)))
               (log (string-append "[send data] "
                                   (number->string sequence)
                                   " ("
                                   (number->string len)
                                   ")"))
               #t)
        #f)))
    

(send-next-packet)

(let loop ([data-read (make-bytes msg-size)])
  (log "ABOUT TO SLEEP")
  (define-values (bytes-received address port)
    (udp-receive!* socket data-read 0 msg-size))
  (if bytes-received
      (with-handlers  ([exn:fail? (lambda (exn) (log "[recv corrupt packet]"))])
        (let ([decoded (string->jsexpr data-read)])
          ;; If there is an ack, send next packet
          (if (= (hash-ref decoded 'ack) sequence)
              (begin (log (string-append "[recv ack] " (number->string sequence)))
                     ; Try to send next packet; break if no more data
                     (when (not (send-next-packet))
                       (exit 0)))
              (loop data-read))))
      (begin (log "[error] timeout")
             (exit -1))))
              

      
(udp-send socket
          (string->bytes/utf-8
           (jsexpr->string (hash 'end #t 'data "" 'sequence sequence 'ack #f))))

(udp-close socket)
(exit 0)
