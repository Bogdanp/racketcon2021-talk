#lang slideshow/widescreen

;; Prelude ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require pict
         (prefix-in gui: racket/gui)
         (prefix-in rge: racket/gui/easy)
         racket/sandbox
         slideshow/code
         slideshow/text)

(set-margin! 20)
(current-main-font "Helvetica")
(current-code-font "Dank Mono")
(current-titlet
 (lambda (s)
   (colorize (title s)
             (current-title-color))))

(define (hexcolor n)
  (list
   (bitwise-and (arithmetic-shift n -16) #xFF)
   (bitwise-and (arithmetic-shift n  -8) #xFF)
   (bitwise-and (arithmetic-shift n   0) #xFF)))

(define-syntax-rule (define-color id s)
  (define-syntax-rule (id e)
    (colorize e (hexcolor s))))

(define-color white #xFFFFFF)
(define-color red #xBF0006)

(current-slide-assembler
 (let ([old (current-slide-assembler)])
   (lambda (t sep p)
     (define bg
       (inset (white (filled-rectangle 1360 766))
              (- margin)))
     (refocus (ct-superimpose bg (old t sep p)) bg))))

(define-syntax-rule (demo name e ...)
  (slide
   #:name (format "demo ~a" name)
   (caps (big (t "Demo")))
   e ...))

(define (title s)
  (bold (big (t s))))

(slide
 #:name "title"
 (red (title "Declarative GUIs"))
 (small (t "Bogdan Popa")))

(slide
 #:name "racket/gui"
 (title "racket/gui")
 'next
 (item "Part of the main Racket distribution.")
 'next
 (item "Cross-platform.")
 'next
 (item "Powerful & flexible.")
 'next
 (item "Imperative.")
 'next
 (item "Unopinionated re. state management."))

(slide
 #:name "gui-easy"
 (title "gui-easy")
 'next
 (item "Built on top of" (code racket/gui) ".")
 'next
 (item "GUIs are constructed as trees of regular function calls.")
 'next
 (item "State is propagated to views through Observables.")
 'next
 (item "Less flexible than" (code racket/gui) "."))

(define (run-code stx)
  (define ns
    (let ([ns (make-base-namespace)])
      (begin0 ns
        (for ([mod (in-list '(racket/gui/base racket/gui/easy))])
          (namespace-attach-module (current-namespace) mod ns))
        (for ([mod (in-list '(racket/format))])
          (namespace-require mod ns)))))
  (let/cc esc
    (parameterize ([uncaught-exception-handler
                    (lambda (e)
                      ((error-display-handler) (exn-message e) e)
                      (esc))])
      (call-with-trusted-sandbox-configuration
       (lambda ()
         (parameterize ([gui:current-eventspace (gui:make-eventspace)]
                        [current-namespace ns])
           (eval (syntax->datum stx))))))))

(define-exec-code/scale 0.6 (rg-example-pict rg-example _rg-example-str)
  (require racket/gui)

  code:blank
  (define count 0)
  (define (update-count! f)
    (set! count (f count))
    (send message set-label (~a count)))

  code:blank
  (define frame (new frame% [label "Counter"]))
  (define panel (new horizontal-panel% [parent frame]))
  (define minus-button
    (new button%
         [label "-"]
         [parent panel]
         [callback (λ (self event)
                     (update-count! sub1))]))
  (define message
    (new message%
         [label "0"]
         [parent panel]
         [stretchable-width #t]))
  (define plus-button
    (new button%
         [label "+"]
         [parent panel]
         [callback (λ (self event)
                     (update-count! add1))]))

  code:blank
  (send frame show #t))

(define-exec-code/scale 0.6 (rge-example-pict rge-example _rge-example-str)
  (require racket/gui/easy)
  code:blank
  (define @count (obs 0))
  code:blank
  (render
   (window
    #:title "Counter"
    (hpanel
     (button "-" (λ () (obs-update! @count sub1)))
     (text (obs-map @count ~a))
     (button "+" (λ () (obs-update! @count add1)))))))

(slide
 #:name "counter"
 (title "Counter")
 (scale
  (ht-append
   20
   (vc-append
    20
    rg-example-pict
    (clickback (t "Run") (λ () (run-code rg-example))))
   (vc-append
    20
    rge-example-pict
    (clickback (t "Run") (λ () (run-code rge-example)))))
  0.75))

(define-syntax-rule (code/small e0 e ...)
  (parameterize ([get-current-code-font-size (λ () 20)])
    (code e0 e ...)))

(slide
 #:name "views"
 (title "Views")
 'next
 (item "Regular Racket functions that combine to form the GUI hierarchy.")
 'next
 (item "Know how to respond to Observable value changes.")
 'next
 (ht-append
  40
  (code/small (text @message))
  (code/small (choice
               #:selection @selection
               '("a" "b" "c")))
  (code/small (canvas
               @data
               (λ (dc data)
                 ...)))))

(slide
 #:name "observables"
 (title "Observables")
 'alts
 (list
  (list
   (para "An" (code obs) "is like a" (code box) " that broadcasts changes to observer functions.")
   'next
   (para
    (code/small
     (define @count (obs 0))))
   'next
   (para
    (code/small
     (obs-observe! @count (λ (v) (printf "observer-1: count changed to ~s~n" v)))
     (obs-observe! @count (λ (v) (printf "observer-2: count changed to ~s~n" v)))))
   'next
   (para
    (code/small
     (obs-update! @count add1)))
   'next
   (para
    (code/small
     (code:comment "observer-1: count changed to 1")
     (code:comment "observer-2: count changed to 1"))))
  (list
   (para "Observables may be mapped.")
   (para
    (code/small
     (define @count-squares
       (obs-map @count (λ (v) (* v v))))
     code:blank
     (obs-observe! @count-squares
                   (λ (v) (printf "observer-3: count-squares changed to~s~n" v)))))
   'next
   (para
    (code/small
     (obs-update! @count add1)))
   'next
   (para
    (code/small
     (code:comment "observer-1: count changed to 2")
     (code:comment "observer-2: count changed to 2")
     (code:comment "observer-3: count-squares changed to 4"))))
  (list
   (para "Mapped observables can't be updated.")
   'next
   (para
    (code/small
     (obs-update! @count-squares add1)))
   'next
   (para
    (code/small
     (code:comment "obs-update!: contract violation")
     (code:comment "  expected: a non-derived observable")
     (code:comment "  ..."))))))

(slide
 #:name "custom views"
 (title "Custom Views")
 'alts
 (list
  (list
   (para "Views implement the" (code view<%>) "interface.")
   'next
   (para
    (code/small
     (is-a? (text "hi") view<%>) (code:comment "=> #t")))
   'next
   (para
    (hc-append
     (code/small
      (define view<%>
        (interface ()
          [dependencies (->m (listof obs?))]
          [create (->m (is-a?/c area<%>) (is-a?/c area<%>))]
          [update (->m (is-a?/c area<%>) obs? any/c void?)]
          [destroy (->m (is-a?/c area<%>) void?)])))
     (inset
      (scale (bitmap "lifecycle.png") 0.20)
      100 0))))
  (list
   (hc-append
    20
    (code/small
     (define my-text%
       (class* object% (view<%>)
         (init-field @msg)
         (super-new)

         code:blank
         (define/public (dependencies)
           (list @msg))

         code:blank
         (define/public (create parent)
           (new message%
                [parent parent]
                [label (obs-peek @msg)]))

         code:blank
         (define/public (update v what val)
           (when (equal? what @msg)
             (send v set-label val)))

         code:blank
         (define/public (destroy v)
           (void)))))
    (code/small
     (define (my-text @msg)
       (new my-text% [@msg @msg]))

     code:blank
     (render
      (window
       (my-text (obs-map @counter ~a)))))))))

(slide
 #:name "demo"
 (title "Demo"))

(slide
 #:name "fin"
 (title "Thanks!")
 (red (t "defn.io")))
