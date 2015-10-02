#lang racket
(require rackunit)
(require "fileIO.rkt")

(define file-tests
  (test-suite
   "tests for fileIO.rkt"
   (test-case
    "cross-up pass"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (cross-up v0 v1 p1))
        (check-true (cross-up v0 v1 p2))
      )
    )
   (test-case
    "cross-up fail"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (not (cross-up v1 v0 p1)))
        (check-true (not (cross-up v1 v0 p2)))
      )
    )
   (test-case
    "cross-down pass"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (not (cross-down v0 v1 p1)))
        (check-true (not (cross-down v0 v1 p2)))
      )
    )
   (test-case
    "cross-down fail"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (cross-down v1 v0 p1))
        (check-true (cross-down v1 v0 p2))
      )
    )
   (test-case
    "is-left pass"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (is-left v0 v1 p1))
      )
    )
   (test-case
    "is-left fail"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (not (is-left v0 v1 p2)))
      )
    )
   (test-case
    "is-right pass"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (is-right v0 v1 p2))
      )
    )
   (test-case
    "is-right fail"
    (let ([ v0 (list 0 0)]
          [v1 (list 0 10)]
          [p1 (list -1 6)]
          [p2 (list 1 6)])
        (check-true (not (is-right v0 v1 p1)))
      )
    )

   (test-case
    "read/write classification points"
    (define class-points (read-classification-points in-file-name))
    (write-classification-points (first class-points) (last class-points) out-file-name)
    (define class-points2 (read-classification-points out-file-name))
    (check-equal? class-points class-points2)
    )

   (test-case
    "read/write regions"
    (define regions (read-regions region-file-name-small))
    (write-regions regions test-region-out-file-name)
    (define regions2 (read-regions test-region-out-file-name))
    (check-equal? regions regions2)
    )
   )
  )

  (require rackunit/text-ui)
  (run-tests file-tests)