#lang racket/base

(require debugging/client
         racket/format
         racket/gui/easy)

(define (labelled label v)
  (hpanel
   #:stretch '(#t #f)
   (hpanel
    #:stretch '(#f #t)
    #:min-size '(100 #f)
    (text label))
   v))

(define (info-tab c)
  (define info (get-info c))
  (vpanel
   (labelled "OS:" (text (~a (hash-ref info 'os*))))
   (labelled "Racket VM:" (text (~a (hash-ref info 'vm))))
   (labelled "Racket Version:" (text (~a (hash-ref info 'version))))))

(define (memory-tab c)
  (define @total (obs (get-memory-use c)))
  (define @counts (obs (get-object-counts c)))
  (vpanel
   (hpanel
    #:stretch '(#t #f)
    (labelled "Memory use:" (text (obs-map @total ~MiB)))
    (button "Reload..." (λ ()
                          (obs-update! @total (λ (_) (get-memory-use c)))
                          (obs-update! @counts (λ (_) (get-object-counts c))))))
   (table
    '("Kind" "Count" "Size")
    (obs-map @counts list->vector)
    #:entry->row (λ (entry)
                   (vector
                    (~a (car entry))
                    (~a (cadr entry))
                    (~MiB (cddr entry)))))))

(define (run c)
  (define @tab (obs "Info"))
  (render
   (window
    #:size '(400 300)
    #:title "Remote Debugger"
    (tabs
     '("Info" "Memory")
     (λ (event choices selection)
       (case event
         [(select)
          (obs-update! @tab (λ (_) (list-ref choices selection)))]))
     (case-view @tab
       [("Info")
        (info-tab c)]
       [("Memory")
        (memory-tab c)]
       [else
        (text "Unreachable.")])))))

(define (~MiB b)
  (~a (~r #:precision '(= 2) (/ b 1024 1024)) "MB"))

(module+ test
  (run (connect)))
