#lang racket
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
          ;          (print "region-point-strs: ")
          ;          (if (not (null? region-point-strs))
          ;              (println (first region-point-strs))
          ;              #f)
          (if (not (null? region-point-strs))
              (begin
                ;                (print "region-point-strs")
                ;                (println region-name)
                ;                (println (first region-point-strs))
                (hash-set! regions region-name (process-region-points region-point-strs))
                )
              #f)
          (set! region-name (first (regexp-split #rx":" (first (regexp-match #rx".*:" l)))))
          (set! region-point-strs (list))
          )
        (if (regexp-match #rx".*,.*" l)
            (
             begin
              (set! pt-str (regexp-match #rx".*,.*" l))
              ;         (print "line, pt-str: " )
              ;         (println l)
              ;         (println pt-str)
              (set! region-point-strs
                    (if (not (eq? pt-str #f))
                        (cons (first pt-str) region-point-strs)
                        (list)
                        )
                    )
              ;              (if (not (null? region-point-strs))
              ;                 (println (first region-point-strs))
              ;                #f)
              )
            #f)
        )
    )
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

(define (is-left p0 p1 p2)
  (((first p1)-(first p0)) * ((second p2)-(second p0)) - ((first p2)-(first p0)) * ((second p1)-(second p0)) > 0)
  )

(define (is-right p0 p1 p2)
  (((first p1)-(first p0)) * ((second p2)-(second p0)) - ((first p2)-(first p0)) * ((second p1)-(second p0)) < 0)
  )

;(define (cross-up P, 


(define (wn_PnPoly P v-list)
  (define wn 0)
  (define v-prev #f)
  (for ([v v-list])
    (cond
      [(not v-prev) #f]
      [else
       (let ([edge (list v v-prev)])
             (if (cross-up P (first edge) (last edge))
                 (if (is-left P (first edge) (last edge))
                     (set! wn (+ 1 wn))
                     #f)
                 #f)
             (if (cross-down P edge)
                 (if (is-right P (first edge) (last edge))
                     (set! wn (- 1 wn))
                     #f)
                 #f)
         )
       ]
      )
    (set! v-prev v)
    )
  )


;  (define region-names (filter (lambda (l) (regexp-match #rx".*:" l)) lines))
;  (define region-value-pairs (filter (lambda (l) (regexp-match #rx".*,.*" l)) lines))
;  (define pts (map (lambda (pt-str) (list
;                                     (string->number (string-trim (first (regexp-split #rx"," pt-str))))
;                                   (string->number (string-trim (second (regexp-split #rx"," pt-str))))))  region-value-pairs))
;  (list region-names pts)
;)

(define (points->file name-points file)
  (display-lines-to-file name-points
                         file
                         #:exists 'replace
                         #:mode 'text))

;(define class-points (list))
(define regions (read-regions region-file-name-small))
;(read-classification-points in-file-name)
(define class-points (read-classification-points in-file-name))
(write-classification-points (first class-points) (last class-points) out-file-name)
(write-regions regions test-region-out-file-name)
