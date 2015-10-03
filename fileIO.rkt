#lang racket
(provide (all-defined-out))

(struct point (x y) #:inspector #f)

(define in-file-name "/Users/jonengelbert/projects/locate-in-region/points.txt")
(define out-file-name "/Users/jonengelbert/projects/locate-in-region/points-out.txt")
(define region-file-name "/Users/jonengelbert/projects/locate-in-region/region-definitions.txt")
(define region-file-name-small "/Users/jonengelbert/projects/locate-in-region/region-def-small.txt")
(define test-region-out-file-name "/Users/jonengelbert/projects/locate-in-region/region-definitions-out.txt")
;(define in (open-input-file in-file-name))
;(define out (open-output-file out-file-name #:exists 'replace))
;(define loc-names '())


(define (read-classification-points in-file-name)
  (define lines (file->lines in-file-name))
  (define pt-names (map (lambda (l) (first  (regexp-split #rx":" l))) lines))
  (define pt-value-pairs (map (lambda (l) (second  (regexp-split #rx":" l))) lines))
  (define pts (map (lambda (pt-str) (list
                                     (string->number (string-trim (first (regexp-split #rx"," pt-str))))
                                     (string->number (string-trim (second (regexp-split #rx"," pt-str))))))  pt-value-pairs))
  (list pt-names pts)
  )

(define (write-classification-points pt-names points out-file-name)
  (define out (open-output-file out-file-name #:exists 'replace))
  (for ([name pt-names][pt points])
    (fprintf out "~a: ~a,~a\n" name (first pt) (last pt))
    )
  (close-output-port out)
  )
  
(define (region-name? str)
  (regexp-match #rx".*:" str)
  )

;{ to trim the : from the line with the region name}
(define (trimlast str) (substring str 0 (sub1 (string-length str))))


(define (process-region-points region-point-strs)
  (define region-points (list))
  (for/list ([pt-str region-point-strs])
    ;    (print "  pt-str is string: ")
    ;    (println (string? pt-str))
    ;    (print "  pt-str is list ")
    ;    (println (list? pt-str))
    (set! region-points
          (cons (list
           (string->number (string-trim (first (regexp-split #rx"," pt-str))))
           (string->number (string-trim (second (regexp-split #rx"," pt-str))))
           )
          region-points)
          )
    )
  region-points
  )

(define (read-regions in-file-name)
  (define regions (make-weak-hash))
  (define lines (file->lines in-file-name))
  (define region-point-strs (list))
  (define region-name '(""))
  (define pt-str "")
  (for/list ([l lines])
    (if (regexp-match #rx".*:" l)
        (
         begin
          (if (not (null? region-point-strs))
              (begin
                (hash-set! regions (string-trim region-name) (process-region-points region-point-strs))
                )
              #f)
          (set! region-name (first (regexp-split #rx":" (first (regexp-match #rx".*:" l)))))
          (set! region-point-strs (list))
          )
        (if (regexp-match #rx".*,.*" l)
            (
             begin
              (set! pt-str (regexp-match #rx".*,.*" l))
              (set! region-point-strs
                    (if (not (eq? pt-str #f))
                        (cons (first pt-str) region-point-strs)
                        (list)
                        )
                    )
              )
            #f)
        )
    )
  (if (not (null? region-point-strs))
      (hash-set! regions (string-trim region-name) (process-region-points region-point-strs))
      #f)
  regions
  )

(define (write-regions regions out-file-name)
  (define out (open-output-file out-file-name #:exists 'replace))
  (for ([(reg-name pt-list) regions])
    (fprintf out "~a:\n" reg-name)
    (for ([pt pt-list])
      (fprintf out "      ~a,~a\n" (first pt) (last pt))
      )
    )
  (close-output-port out)
  )

(define (is-left v0 v1 p)
  (> (-(*(-(first v1)(first v0)) (-(second p)(second v0)))  (*(-(first p)(first v0)) (-(second v1)(second v0)))) 0)
  )

(define (is-right v0 v1 p)
  (< (-(*(-(first v1)(first v0)) (-(second p)(second v0)))  (*(-(first p)(first v0)) (-(second v1)(second v0)))) 0)
  )

(define (cross-up v0 v1 p)
  (and (<= (second v0) (second p))
       (> (second v1)  (second p))
       )
  )

(define (cross-down v0 v1 p)
  (and (> (second v0) (second p))
       (<= (second v1)  (second p))
       )
  )

(define (accumulate_wn_for_edge edge P wn)
  (if (cross-up (first edge) (last edge) P)
      (if (is-left (first edge) (last edge) P)
          (set! wn (+ 1 wn))
          #f)
      (if (cross-down (first edge) (last edge) P)
          (if (is-right (first edge) (last edge) P)
              (set! wn (- 1 wn))
              #f)
          #f)
      )
  wn)

(define (wn_PnPoly P v-list)
  (define wn 0)
  (define v-prev #f)
  (for ([v v-list])
    (cond
      [(not v-prev)
       (let ([edge (list (last v-list) v)])
         (set! wn (accumulate_wn_for_edge edge P wn))
         )
       ]

      [else
       (let ([edge (list v-prev v)])
         (set! wn (accumulate_wn_for_edge edge P wn))
         )
       ]
      )
    (set! v-prev v)
    )
  ;(print "wn: ")
  ;(println wn)
  wn
  )

(define (point-in-region-polygon p region-points)
   (odd? (wn_PnPoly p region-points))
  )

(define (point-in-region-by-name p regions region-name)
  (odd? (wn_PnPoly p (hash-ref regions region-name)))
  )

(define (find-enclosing-region-stub pt-in regions)
  #t
  )

(define (find-enclosing-region pt-in regions)
  (define solution "")
  (for ([(region-name region-points) regions])
          (begin
            ;(print "region-name:")
            ;(println region-name)
            (if (point-in-region-polygon pt-in region-points)
                (begin
                  ;(print "solution: ")
                  ;(print pt-in)
                  ;(println region-name)
                  (set! solution region-name)
                  )
                #f)
            )
          )
  solution
  )

(define (solve-enclosing-regions point-file-name region-file-name)
  (define points-in (last (read-classification-points point-file-name)))
  (define regions (read-regions region-file-name))
  (define solution-set (list))
  (define solution "")
  (for ([pt-in points-in])
    (begin
      ;(print "pt-in")
      ;(println pt-in)
      (set! solution "")
      (for ([(region-name region-points) regions])
        ;(print "region: ")
        ;(println region-name)
        ;(println region-points)
        (if (point-in-region-polygon pt-in region-points)
            (begin
              (set! solution region-name); what if the solution already was found... then two regions that the point is in.
              )
            #f)
        )
      (set! solution-set (cons solution solution-set))
      )
    )
  (reverse solution-set)
  )
  


;  (define region-names (filter (lambda (l) (regexp-match #rx".*:" l)) lines))
;  (define region-value-pairs (filter (lambda (l) (regexp-match #rx".*,.*" l)) lines))
;  (define pts (map (lambda (pt-str) (list
;                                     (string->number (string-trim (first (regexp-split #rx"," pt-str))))
;                                   (string->number (string-trim (second (regexp-split #rx"," pt-str))))))  region-value-pairs))
;  (list region-names pts)
;)

;(define (points->file name-points file)
;  (display-lines-to-file name-points
;                         file
;                         #:exists 'replace
;                         #:mode 'text))

;(define class-points (list))
(define regions (read-regions region-file-name))
;(read-classification-points in-file-name)
(define class-points (read-classification-points in-file-name))
(write-classification-points (first class-points) (last class-points) out-file-name)
(write-regions regions test-region-out-file-name)
(define pt-in (list -85.646282 42.912051))
;(point-in-region (list -85.646282 42.912051) regions " Alger Heights")
;(point-in-region (list -85.646282 42.912051) regions " Baxter")
(solve-enclosing-regions in-file-name region-file-name)