;;;; exercises on binary trees and graph structures

;;; Example 1: 104. Maximum Depth of Binary Tree
;;; Given the root of a binary tree, find the length of the longest path from the root to a leaf.

(defstruct (tree-node (:constructor make-tree-node (content left right)))
  (content nil)
  (left nil :type (or null tree-node))
  (right nil :type (or null tree-node)))

(defun mk-trnode (content &key left right)
  (let ((left-node nil) (right-node nil))
    (if (typep left 'tree-node)
        (setf left-node left)
        (setf left-node (make-tree-node left nil nil)))
    (if (typep right 'tree-node)
        (setf right-node right)
        (setf right-node (make-tree-node right nil nil))) 
    (let ((trn (make-tree-node content left-node right-node))) 
      (values trn))))

;; helper function to ensure object is a tree node
(declaim (inline ensure-node))
(defun ensure-node (node-or-content)
  (if (typep node-or-content 'tree-node)
      node-or-content
      (make-tree-node node-or-content nil nil)))

;; children are passed in as a list with the format '(left-child right-child)
;; for further descendents, pass '((left-child (descendents)) (right-child (descendents))) 
(defun add-children (trnode children)
  (declare (type tree-node trnode) (type list children))
  (let ((left-side (car children)) (right-side (if (consp (cdr children)) (cadr children) (cdr children)))) 
    (setf (tree-node-left trnode) (if (consp left-side) (ensure-node (car left-side)) (ensure-node left-side)))
    (setf (tree-node-right trnode) (if (consp right-side) (ensure-node (car right-side)) (ensure-node right-side)))
    (when (consp left-side) 
      (add-children (tree-node-left trnode) (cdr left-side)))
    (when (consp right-side)
      (add-children (tree-node-right trnode) (cdr right-side)))))


(defun max-depth (trnode)
  (declare (type tree-node trnode))
  (let ((unexplored nil) (depth 0))
    (labels ((recurse-depth (tn accum unex-nodes)
                (if (null (tree-node-left tn))
                  (if (null (tree-node-right tn))
                      (values accum)
                      (recurse-depth (tree-node-right tn) (1+ accum) unex-nodes))
                  (progn 
                    (when (tree-node-right tn)
                      ;; unexplored nodes are stored as dotted lists, node . level
                      (push (cons (tree-node-right tn) accum) unex-nodes)) 
                    (recurse-depth (tree-node-left tn) (1+ accum) unex-nodes)))))
      (setf depth (recurse-depth trnode 1 unexplored))
      (loop while unexplored
            do (let* ((node-dot-level (car unex-nodes)) (nd (first node-dot-level)) (level (second node-dot-level)))
                 (setf depth (max depth (recurse-depth nd level unexplored))))))
    (values depth)))

(setf *root* (mk-trnode 0))
(add-children *root* '((1 ((3 (7 8)) 4)) 
                       (2 (5 6))))
(max-depth *root*)


;;; Example 2: 112. Path Sum
;; 
;; Given the root of a binary tree and an integer targetSum, 
;; return true if there exists a path from the root to a leaf such that the sum of the nodes on the path is equal to targetSum, 
;; and return false otherwise.
