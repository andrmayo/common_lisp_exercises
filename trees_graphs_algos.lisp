;;;; exercises on binary trees and graph structures
;;;; DFS binary tree exercises

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
      (add-children (tree-node-left trnode) (cadr left-side)))
    (when (consp right-side)
      (add-children (tree-node-right trnode) (cadr right-side)))))

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

(defparameter *root* (mk-trnode 0))
(add-children *root* '((1 ((3 (7 8)) 4)) 
                       (2 (5 6))))
(max-depth *root*)

;;; Example 2: 112. Path Sum
;; 
;; Given the root of a binary tree and an integer targetSum, 
;; return true if there exists a path from the root to a leaf such that the sum of the nodes on the path is equal to targetSum, 
;; and return false otherwise.

;; note: function below not written to be tail-call optimized

(defun check-branch-sum (rt target)
  (declare (type tree-node rt) (type number target))
  (labels (
           (recurse-nodes-dfs (nd target accum)
             (progn 
               (setf accum (+ accum (tree-node-content nd)))
               (when (eql accum target)
                   (return-from recurse-nodes-dfs t))
               (when (and (null (tree-node-left nd)) (null (tree-node-right nd)))
                 (return-from recurse-nodes-dfs nil))
               (let ((sum-found-p nil) (left-nd (tree-node-left nd)) (right-nd (tree-node-right nd)))
                 (when left-nd
                   (setf sum-found-p (recurse-nodes-dfs left-nd target accum)))
                 (if (and (null sum-found-p) right-nd) 
                     (recurse-nodes-dfs right-nd target accum)
                     sum-found-p)))))
    (values (recurse-nodes-dfs rt target 0))))

(check-branch-sum *root* 12)

;;; Example 3: 1448. Count Good Nodes in Binary Tree

;; Given the root of a binary tree, find the number of nodes that are good.
;; A node is good if the path between the root and the node has no nodes with a greater value.

(defun count-good-nodes (rt)
  (declare (type tree-node rt))
  (labels ((recurse-nodes (nd last-val depth nd-stack deepest) 
             (progn 
               (when (or (null last-val) (>= (tree-node-content nd) last-val))
                  (progn 
                    (when (tree-node-right nd)
                      (push (cons (list (tree-node-right nd) (tree-node-content nd)) (1+ depth)) nd-stack))
                    (when (tree-node-left nd)
                      (push (cons (list (tree-node-left nd) (tree-node-content nd)) (1+ depth)) nd-stack))
                    (setf deepest (max deepest depth))))
               (if nd-stack 
                  (let ((stack-item (pop nd-stack))) 
                    (recurse-nodes (car stack-item) (cadr stack-item) (cdr (cdr stack-item)) nd-stack deepest)) 
                  deepest))))
    (recurse-nodes rt nil 1 nil 0)))

;;; Example 4: 100. Same Tree
;; 
;; Given the roots of two binary trees p and q, check if they are the same tree. 
;; Two binary trees are the same tree if they are structurally identical and the nodes have the same values.xample 4: 100. Same Tree
;; 
;; Given the roots of two binary trees p and q, check if they are the same tree. 
;; Two binary trees are the same tree if they are structurally identical and the nodes have the same values.

(defun sametree-p (rt1 rt2)
  (declare (type tree-node rt1) (type tree-node rt2))
  (labels ((recurse-dfs (rt1 rt2 node-stack1 node-stack2)
             (let ((rt1-left (tree-node-left rt1)) (rt1-right (tree-node-right rt1)) (rt2-left (tree-node-left rt2)) (rt2-right (tree-node-right rt2)))
               (when (not (equal (tree-node-content rt1) (tree-node-content rt2)))
                 (return-from sametree-p nil))
               (when (and (null rt1-left) rt2-left)
                 (return-from sametree-p nil))
               (when (and (null rt1-right) rt2-right)
                 (return-from sametree-p nil))
               (when rt1-right
                 (when (null rt2-right)
                   (return-from sametree-p nil))
                 (push rt1-right node-stack1)
                 (push rt2-right node-stack2))
               (when rt1-left
                 (when (null rt2-left)
                   (return-from sametree-p nil))
                 (push rt1-left node-stack1)
                 (push rt2-left node-stack2))
               (if (and (null node-stack1) (null node-stack2))
                   (return-from sametree-p t)
                   (recurse-dfs (pop node-stack1) (pop node-stack2) node-stack1 node-stack2)))))
           (recurse-dfs rt1 rt2 nil nil)))


(defparameter *root1* (mk-trnode 0))
(add-children *root1* '((1 ((3 (7 8)) 4)) 
                       (2 (5 6))))

(defparameter *root2* (mk-trnode 0))
(add-children *root2* '((1 ((3 (7 8)) 4)) 
                       (2 (5 6))))

(sametree-p *root1* *root2*)

;;; Bonus example: 236. Lowest Common Ancestor of a Binary Tree
;; 
;; G9iven the root of a binary tree and two nodes p and q that are in the tree,
;; return the lowest common ancestor (LCA) of the two nodes.
;; The LCA is the lowest node in the tree that has both p and q as descendants (note: a node is a descendant of itself).

;; this function is not tail-call optimized; number of calls on callstack is O(m), where m is the deepest part of 
;; the tree
(defun lca (rt p q)
  (declare (type tree-node rt) (type tree-node p) (type tree-node q))
  (labels ((recurse-nodes (rt p q) 
             (let ((left (tree-node-left rt)) (right (tree-node-right rt)) (left-ans nil) (right-ans nil))
               (when (eq rt p)
                 (return-from recurse-nodes 'p))
               (when (eq rt q)
                 (return-from recurse-nodes 'q))
               (when left 
                 (setf left-ans (recurse-nodes left p q)))
               (when right 
                 (setf right-ans (recurse-nodes right p q)))
               (if (or (eql left-ans 'p) (eql left-ans 'q))
                   (if (or (eql right-ans 'p) (eql right-ans 'q))
                       (return-from lca rt)
                       left-ans)
                   (when (or (eql right-ans 'p) (eql right-ans 'q))
                       right-ans))))) 
    (if (or (eq rt p) (eq rt q)) 
        rt 
        (recurse-nodes rt p q))))

(defparameter *p* (tree-node-left (tree-node-left (tree-node-left *root*))))
(defparameter *q* (tree-node-right (tree-node-left *root*)))
(lca *root* *p* *q*)

;;;; BFS binary tree exercises

;; Example 1: 199. Binary Tree Right Side View
;; 
;; Given the root of a binary tree, imagine yourself standing on the right side of it.
;; Return the values of the nodes you can see ordered from top to bottom.

(defun right-side-view (root)
  (declare (type tree-node root))
  (let ((stack (list root)))
    (labels ((bfs (stack ans)
               (if (null stack)
                   (reverse ans)
                   (progn 
                     (push (car stack) ans)
                     (bfs (process-layer (reverse stack) nil) ans))))
             (process-layer (old-stack new-stack) 
               (if (null old-stack)
                   new-stack
                   (let* ((cur-node (pop old-stack)) (left (tree-node-left cur-node)) (right (tree-node-right cur-node))) 
                     (process-layer 
                       old-stack
                       (append (if right (list right) nil) 
                               (if left (list left) nil) 
                               new-stack))))))
      (bfs stack nil))))

(right-side-view *root*)

;;; Example 2: 515. Find Largest Value in Each Tree Row
;; Given the root of a binary tree, return an array of the largest value in each row of the tree.

(defun bfs-helper (old-stack new-stack) 
  (if (null old-stack) 
      new-stack 
      (let* ((cur-node (pop old-stack)) (left (tree-node-left cur-node)) (right (tree-node-right cur-node))) 
        (bfs-helper 
          old-stack 
          (append (if right (list right) nil) 
                  (if left (list left) nil) 
                  new-stack)))))

(defun rowwise-max-bfs (root)
  (declare (type tree-node root))
  (labels ((get-max-node (stack ans) 
             (if stack 
                 (progn 
                   (push (apply #'max (mapcar #'tree-node-content stack)) ans)
                   (get-max-node (bfs-helper stack nil) ans))
                 (reverse ans))))
      (get-max-node (list root) nil)))

(rowwise-max-bfs *root*)

;;; Example 1: 938. Range Sum of BST
;; Given the root node of a binary search tree and two integers low and high, 
;; return the sum of values of all nodes with a value in the inclusive range [low, high].

(defun range-sum-bst (root low high)
  (declare (type tree-node root) (type number low) (type number high))
  (labels ((descend (low high stack accum)
             (if (null stack)
                 accum 
                 (let* ((node (pop stack)) (left (tree-node-left node)) (right (tree-node-right node))) 
                   (when right 
                     (let ((right-val (tree-node-content right))) 
                       (when (and (<= right-val high) (>= right-val low)) 
                         (setf accum (+ accum right-val)) 
                         (push right stack)))) 
                   (when left 
                     (let ((left-val (tree-node-content left))) 
                       (when (and (<= left-val high) (>= left-val low)) 
                         (setf accum (+ accum left-val)) 
                         (push left stack))))))))
    (descend low high (list root) 0)))



;;; Example 2: 530. Minimum Absolute Difference in BST
;; Given the root of a BST, return the minimum absolute difference 
;; between the values of any two different nodes in the tree.

(defun dfs-helper (node old-stack new-stack)
  (declare (type tree-node node) (type list old-stack) (type list new-stack))
  (let ((left (tree-node-left node)) (right (tree-node-right node)))
    (when (not (eq left (car old-stack)))
      (when left
        (push left old-stack)))
    (push node old-stack)
    (when (not (eq right (car new-stack)))
      (when right
        (push right old-stack)))
    (when (eq (car old-stack) node)
        (push (pop old-stack) new-stack))
    (if (null old-stack) 
        new-stack
        (dfs-helper (car old-stack) (cdr old-stack) new-stack))))


(defun get-least-diff-adjac (stack)
  (labels ((recurse-elts (num-stack prev least-diff)
             (progn
                (setf least-diff (min least-diff (abs (- prev (car num-stack)))))
                (if (null (cdr num-stack))
                    least-diff
                    (recurse-elts (cdr num-stack) (car num-stack) least-diff)))))
    (recurse-elts (cdr stack) (car stack) (abs (- (car stack) (cadr stack))))))

(defun min-abs-diff (root)
  (declare (type tree-node root))
  (let ((stack (dfs-helper root nil nil)))
    (get-least-diff-adjac (mapcar #'tree-node-content stack))))

(defparameter *bst-root* (mk-trnode 9))
(add-children *bst-root* '((5 (1 7)) 15))

(min-abs-diff *bst-root*)

;;; Example 3: 98. Validate Binary Search Tree
;; Given the root of a binary tree, determine if it is a valid BST.
