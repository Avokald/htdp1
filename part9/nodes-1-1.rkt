;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname nodes-1-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (key val l r))
;; BST (Binary Search Tree) is one:
;; - false
;; - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;; - key is the node key
;; - val is the node value
;; - r and l are right and left subtrees
;; INVARIANT: for a given node:
;; Key is > than all the keys in the l(eft) child
;; Key is < than all the keys in the r(ight) child
;; The same key never appears in a single tree
(define BST0 false)
(define BST1 (make-node 34 "hle" false false))
(define BST2 (make-node 43 "ehl" (make-node 37 "jur" false (make-node 41 "nem" false false)) false))
(define BST3 (make-node 35 "asz" BST1 BST2))


(define (fn-for-bst bst)
  (cond [(false? bst) (...)]
        [else
         (... (node-key bst)              ; Integer
              (node-val bst)              ; String
              (fn-for-bst (node-l bst))   ; BST
              (fn-for-bst (node-r bst)))])) ; BST

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self-reference: (node-l bst) has type BST
;; - self-reference: (node-r bst) has type BST

;; BST node-key -> String
;; Searches BST for the item which key is equal to node-key
(check-expect (lookup-key BST0 21) false)
(check-expect (lookup-key BST1 0) false)
(check-expect (lookup-key BST2 41) "nem")
(check-expect (lookup-key BST3 37) "jur")

;(define (lookup-key BST key) "") ; stub

(define (lookup-key BST key)
  (cond [(false? BST) false]
        [else
         (cond [(= (node-key BST) key)
                (node-val BST)]
               [(< (node-key BST) key)
                (lookup-key (node-r BST) key)]
               [(> (node-key BST) key)
                (lookup-key (node-l BST) key)])]))

