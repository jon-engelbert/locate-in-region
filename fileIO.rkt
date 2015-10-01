#lang racket
(define in (open-input-file "/Users/jonengelbert/projects/locate-in-region/points.txt"))
(define out (open-output-file "/Users/jonengelbert/projects/locate-in-region/points-out.txt" #:exists 'replace))
(define loc-names '())
(define (read-classification-points in-file-name)
  (define lines (file->lines in-file-name))
  (define pt-names (map (lambda (l) (first  (regexp-split #rx":" l))) lines))
  (define pt-value-pairs (map (lambda (l) (second  (regexp-split #rx":" l))) lines))
  (define pts (map (lambda (pt-str) (list
                                     (string->number (string-trim (first (regexp-split #rx"," pt-str))))
                                   (string->number (string-trim (second (regexp-split #rx"," pt-str))))))  pt-value-pairs))
  (list pt-names pts)
)

(define (region-name? str)
  (regexp-match #rx".*:" str)
  )

;{ to trim the : from the line with the region name}
(define (trimlast str) (substring str 0 (sub1 (string-length str))))


(define (read-regions in-file-name)
  (define regions (list))
  (define lines (file->lines in-file-name))
  (define region (make-weak-hash))
  (define region-point-strs (list))
  (define region-name #f)
  (for/list ([l lines])
    (if (regexp-match #rx".*:" l)
        (
         (if (region-name)
            (process-region-points region-point-strs region)
         (cons region regions)
         (set region-name (regexp-match #rx".*:" l))
         (set region-point-strs (list))
         (set region (hash region-name (region-point-strs)))           )
         )
        (
         (let ([pt-str (regexp-match #rx".*,.*" l)])
           (if (pt-str)
               (cons pt-str region-point-strs)
               (set region-point-strs (list))
               )
           )
         )
        )
    )
  )
               
(define (process-region-points region-point-strs region)
  (define region-points (list))
  (for/list ([pt-str region-point-strs])
   (cons (list
               (string->number (string-trim (first (regexp-split #rx"," pt-str))))
               (string->number (string-trim (second (regexp-split #rx"," pt-str))))
               )
         region-points)
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

(read-regions "/Users/jonengelbert/projects/locate-in-region/region-definitions.txt")
