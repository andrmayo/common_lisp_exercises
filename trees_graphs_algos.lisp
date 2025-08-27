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

(defun bst-p (root)
  (declare (type tree-node root))
  (labels ((recurse-dfs (nd stack smallest largest)
             (let ((val (tree-node-content nd)) (left (tree-node-left nd)) (right (tree-node-right nd)))
               (let ((left-val (if left (tree-node-content left) (1- val))) (right-val (if right (tree-node-content right) (1+ val))))
                 (when (not (and (< left-val val) (> right-val val))) 
                   (return-from bst-p nil))
                 (when (or (and smallest (<= val smallest)) (and largest (>= val largest)))
                   (return-from bst-p nil))
                 (when left
                   (push (list left smallest val) stack))
                 (when right
                   (push (list right val largest) stack))
                 (let* ((new-item (pop stack)) (new-node (car new-item)) (new-smallest (second new-item)) (new-largest (third new-item)))
                   (if (null new-item)
                       (return-from bst-p t)
                       (recurse-dfs new-node stack new-smallest new-largest)))))))
    (recurse-dfs root nil nil nil)))


;;; Example 1: 547. Number of Provinces
;; 
;; There are n cities. A province is a group of directly or indirectly connected cities and no other cities outside of the group. 
;; You are given an n x n matrix isConnected where isConnected[i][j] = isConnected[j][i] = 1 if the ith city 
;; and the jth city are directly connected, and isConnected[i][j] = 0 otherwise. Return the total number of provinces.

(defun num-provinces (isConnected)
  (declare (type (array integer (* *)) isConnected))
  (let ((table (make-hash-table))) 
    (loop for i below (array-dimension isConnected 0) do 
          (loop for j from (1+ i) below (array-dimension isConnected 1) do 
                (when (eql (aref isConnected i j) 1) 
                  (setf (gethash i table) (cons j (gethash i table nil)))
                  (setf (gethash j table) (cons i (gethash j table nil))))))
    (labels ((dfs (nd seen table)
               (loop for i in (gethash nd table) do
                     (when (null (gethash i seen)) 
                       (setf (gethash i seen) t)
                       (dfs i seen table)))))
      (let ((seen-vals (make-hash-table)) (ans 0))
        (loop for i below (array-dimension isConnected 0) do 
              (when (null (gethash i seen-vals)) 
                 (setf ans (1+ ans)) 
                 (setf (gethash i seen-vals) t) 
                 (dfs i seen-vals table)))
        (values ans)))))
      

(defparameter *adj-matrix* (make-array '(5 5) :initial-contents '((0 1 0 1 1) (1 0 0 0 0) (0 0 0 0 0) (1 0 0 0 1) (1 0 0 1 0))))

(num-provinces *adj-matrix*)

;;; Example 2: 200. Number of Islands
;; 
;; Given an m x n 2D binary grid which represents a map of 1 (land)
;; and 0 (water), return the number of islands. 
;; An island is surrounded by water 
;; and is formed by connecting adjacent land cells horizontally or vertically.

(defun new-island-explore (islands seen stack)
  (declare (type (array integer (* *)) islands) (type hash-table seen) (type list stack))
  (let ((coords (pop stack)))
    (if (null coords) 
        seen
        (let ((i (car coords)) (j (cadr coords)))
          (if (or (< i 0) (< j 0) (>= i (array-dimension islands 0)) (>= j (array-dimension islands 1)))
              (new-island-explore islands seen stack)
              (if (eql 0 (aref islands i j))
                  (new-island-explore islands seen stack)
                  (progn 
                    (setf (gethash (list i j) seen) t)
                    (when (null (gethash (list (1+ i) j) seen)) 
                      (push (list (1+ i) j) stack))
                    (when (null (gethash (list (1- i) j) seen)) 
                      (push (list (1- i) j) stack))
                    (when (null (gethash (list i (1+ j)) seen)) 
                      (push (list i (1+ j)) stack))
                    (when (null (gethash (list i (1- j)) seen)) 
                      (push (list i (1- j)) stack))
                    (new-island-explore islands seen stack))))))))

(defun num-islands (islands)
  (declare (type (array integer (* *)) islands))
  (let ((seen (make-hash-table :test #'equal)) (m (array-dimension islands 0)) (n (array-dimension islands 1)) (accum 0))
    (loop for i below m do 
          (loop for j below n do 
                (progn 
                  (if (or (gethash (list i j) seen) (eql (aref islands i j) 0))
                      accum 
                      (progn 
                        (setf accum (1+ accum))
                        (new-island-explore islands seen (list (list i j))))))))
    accum))

(defparameter *islands* (make-array '(5 7) 
                                    :initial-contents 
                                    '((1 0 0 0 0 0 1)
                                      (1 1 1 0 0 1 1)
                                      (1 0 0 0 0 0 0)
                                      (0 0 1 1 0 0 0)
                                      (0 0 0 0 0 0 0))))

(num-islands *islands*)

;;; Example 3: 1466. Reorder Routes to Make All Paths Lead to the City Zero
;; 
;; There are n cities numbered from 0 to n - 1 and n - 1 roads 
;; such that there is only one way to travel between two different cities.
;; Roads are represented by connections where connections[i] = [x, y] 
;; represents a road from city x to city y. The edges are directed.
;; You need to swap the direction of some edges so that every city
;; can reach city 0. Return the minimum number of swaps needed.

(defun reorder-routes (roads)
  (declare (type cons roads))
  (labels ((map-edges (connections table)
             (let ((edge (car connections)))
               (setf (gethash (car edge) table) (cons (cdr edge) (gethash (car edge) table)))
               (setf (gethash (cdr edge) table) (cons (car edge) (gethash (cdr edge) table)))
               (if (null (cdr connections))
                   table
                   (map-edges (cdr connections) table))))
           (reverse-roads (roads table seen stack &optional (accum 0))
             (let ((city (pop stack)))
               (if (null city)
                   accum 
                   (if (gethash city seen) 
                       (reverse-roads roads table seen stack accum)
                       (progn 
                         (setf (gethash city seen) t)
                         (loop for neighbour in (gethash city table) do 
                               (when (null (gethash neighbour seen)) 
                                 (if (member (cons neighbour city) roads :test #'equal) 
                                     (push neighbour stack) 
                                     (progn 
                                       (setf accum (1+ accum)) 
                                       (push neighbour stack))))) 
                         (reverse-roads roads table seen stack accum))))))) 
    (let ((table (map-edges roads (make-hash-table)))) 
      (reverse-roads roads table (make-hash-table) (list 0)))))


;; answer: '((1 0) (3 1) (2 4) (4 3))
(defparameter *roads* '((0 . 1) (1 . 3) (2 . 4) (4 . 3)))
(reorder-routes *roads*)

;;; Example 1: 1091. Shortest Path in Binary Matrix
;; 
;; Given an n x n binary matrix grid, return the length of the shortest clear path in the matrix. 
;; If there is no clear path, return -1. 
;; A clear path is a path from the top-left cell (0, 0) to the bottom-right cell (n - 1, n - 1) such that all visited cells are 0. 
;; You may move 8-directionally (up, down, left, right, or diagonally).

(defun valid-clear-p (matrix i j)
  (declare (type (array integer (* *)) matrix) (type integer i) (type integer j))
  (and 
    (>= i 0) 
    (>= j 0) 
    (< i (array-dimension matrix 0)) 
    (< j (array-dimension matrix 1)) 
    (eql (aref matrix i j) 0)))

(defun add-path-nodes (matrix i j stack path paths seen)
  (declare (type (array integer (* *)) matrix) (type integer i) (type integer j) (type list stack))
  (progn 
    (loop for coords in '((0 . 1) (0 . -1) (1 . 0) (-1 . 0) (1 . 1) (-1 . -1) (1 . -1) (-1 . 1)) do 
          (let ((k (+ i (car coords))) (l (+ j (cdr coords)))) 
            (when (and (not (gethash (cons k l) seen)) (valid-clear-p matrix k l)) 
              (setf (gethash (cons k l) seen) t)
              (push (cons k l) stack) 
              (push (+ path 1) paths))))
    (values stack paths)))

(defun clear-path-bfs (matrix stack paths &optional (seen (make-hash-table :test #'equal)))
  (let ((coords (pop stack)) (path (pop paths)) (i nil) (j nil))
    (if (null coords)
        (return-from clear-path-bfs -1)
        (setf 
          i (car coords)
          j (cdr coords)))
    (if (and (eql i (1- (array-dimension matrix 0))) (eql j (1- (array-dimension matrix 1))))
        (if (eql (aref matrix i j) 0)
            path 
            -1)
         (progn 
          (multiple-value-setq (stack paths) (add-path-nodes matrix i j stack path paths seen)) 
          (clear-path-bfs matrix stack paths seen)))))

(defun get-shortest-clear-path (matrix) 
  (declare (type (array integer (* *)) matrix))
  (if (eql 1 (aref matrix 0 0))
      -1 
      (let ((stack (list (cons 0 0))) (paths (list 1))) 
        (clear-path-bfs matrix stack paths))))


(defparameter *clear-path* (make-array '(5 5) :initial-contents 
                                       '((0 1 1 0 0)
                                         (1 0 0 1 0)
                                         (0 1 1 1 0)
                                         (0 1 1 0 0)
                                         (1 0 0 1 0))))

(get-shortest-clear-path *clear-path*)

;;; copying over binary tree node code for convenience

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

(defparameter *root* (mk-trnode 0))
(add-children *root* '((1 ((3 (7 8)) 4)) 
                       (2 (5 6))))
;;; Example 2: 863. All Nodes Distance K in Binary Tree
;; 
;; Given the root of a binary tree, a target node target in the tree, and an integer k, 
;; return an array of the values of all nodes that have a distance k from the target node.
 
(defun k-process-layer (node-stack target parents &optional (new-stack nil))
  (declare (type list node-stack) (type tree-node target) (type hash-table parents) (type list new-stack))
  (let ((cur-node (pop node-stack)))
    (if (null cur-node)
        new-stack 
        (let ((left (tree-node-left cur-node)) (right (tree-node-right cur-node)))
          (when left 
            (setf (gethash left parents) cur-node))
          (when right 
            (setf (gethash right parents) cur-node))
          (when (eq left target)
            (return-from k-process-layer left))
          (when (eq right target)
            (return-from k-process-layer right))
          (when right
            (push right new-stack))
          (when left 
            (push left new-stack))
          (k-process-layer node-stack target parents new-stack)))))

(defun k-recurse-layers (node-stack target parents)
  (let ((next-layer (k-process-layer node-stack target parents)))
    (if (typep next-layer 'tree-node)
        next-layer
        (if (null next-layer)
            nil 
            (k-recurse-layers next-layer target parents)))))

(defun k-get-nodes (stack-by-level k parents seen ans)
  (declare (type list stack-by-level) (type integer k) (type hash-table parents) (type hash-table seen) (type list ans))
  (let* ((cur-item (pop stack-by-level)) (cur-node (if cur-item (car cur-item) nil)) (cur-k (if cur-item (cdr cur-item) nil)))
    (if (null cur-item)
        ans 
        (if (> cur-k k)
            (k-get-nodes stack-by-level k parents seen ans)
            (let ((left (tree-node-left cur-node)) (right (tree-node-right cur-node)) (parent (gethash cur-node parents)))
              (push cur-node ans)
              (when (and right (not (gethash right seen)))
                (push (cons right (1+ cur-k)) stack-by-level)
                (setf (gethash right seen) t))
              (when (and left (not (gethash left seen))) 
                (push (cons left (1+ cur-k)) stack-by-level)
                (setf (gethash left seen) t))
              (when (and parent (not (gethash parent seen)))
                (push (cons parent (1+ cur-k)) stack-by-level)
                (setf (gethash parent seen) t))
              (k-get-nodes stack-by-level k parents seen ans))))))
                

(defun k-bfs (root target k)
  (declare (type tree-node root) (type tree-node target) (type integer k))
  (let ((parents (make-hash-table)) (dest nil) (seen (make-hash-table)))
    (setf dest (k-recurse-layers (list root) target parents))
    (when (null dest)
      (return-from k-bfs nil))
    (setf (gethash dest seen) t)
    (k-get-nodes (list (cons dest 0)) k parents seen nil)))

(defparameter *target-node* (tree-node-left (tree-node-left *root*)))
(k-bfs *root* *target-node* 2)

;;; Example 4: 1293. Shortest Path in a Grid with Obstacles Elimination
;; 
;; You are given an m x n integer matrix grid where each cell is either 0 (empty) or 1 (obstacle). 
;; You can move up, down, left, or right from and to an empty cell in one step. 
;; Return the minimum number of steps to walk from the upper left corner to the lower right corner 
;; given that you can eliminate at most k obstacles. If it is not possible, return -1.

(defun valid-coords-p (coords matrix seen)
  (if (gethash coords seen)
      nil 
      (let ((i (car coords)) (j (cdr coords)))
        (if (or (< i 0) (< j 0) (>= i (array-dimension matrix 0)) (>= j (array-dimension matrix 1)))
            nil 
            t))))

(defun process-cell-kmap (coords matrix l k k-map path seen)
  (declare (type list coords) (type (array integer (* *)) matrix) (type integer l) (type integer k) (type hash-table k-map) (type list path)) ; coords is dotted list
  (loop for step in (list (cons 1 0) (cons -1 0) (cons 0 -1) (cons 0 1)) do 
        (let ((i (+ (car coords) (car step))) (j (+ (cdr coords) (cdr step))))
          (when (and (eql i (1- (array-dimension matrix 0))) (eql j (1- (array-dimension matrix 1))))
            (if (eql (aref matrix i j) 1)
                (return-from process-cell-kmap -1)
                (return-from process-cell-kmap (cons (cons i j) path))))
          (when (valid-coords-p (cons i j) matrix seen) 
            (setf (gethash coords seen) t) 
            (if (eql (aref matrix i j) 0) 
                (setf (gethash l k-map) (cons (cons (cons i j) path) (gethash l k-map))) 
                (when (< l k) (setf (gethash (1+ l) k-map) (cons (cons (cons i j) path) (gethash (1+ l) k-map))))))))
  l)

(defun recurse-cells (matrix l k k-map seen)
  (declare (type (array integer (* *)) matrix) (type integer l) (type integer k) (type hash-table k-map) (type hash-table seen))
  (let ((path (pop (gethash l k-map))))
    (if (null path)
        (if (< l k)
            (recurse-cells matrix (1+ l) k k-map seen)
            -1) 
        (let ((output (process-cell-kmap (car path) matrix l k k-map path seen))) 
          (if (eql output -1) 
              -1 
              (if (typep output 'cons) 
                  (1- (length output))
                  (recurse-cells matrix output k k-map seen)))))))

         ; process all paths with obstabcle number l before calling proceed-cells with (1+ l)

(defun nav-obstacles (matrix k)
  (declare (type (array integer (* *)) matrix) (type integer k))
  (let ((k-map (make-hash-table :test #'equal)) (seen (make-hash-table :test #'equal)))
    (setf (gethash 0 k-map) (list (list (cons 0 0))))
    (setf (gethash (cons 0 0) seen) t)
    (recurse-cells matrix 0 k k-map seen)))

(defparameter *k-obstacles* 
  (make-array '(5 6) 
              :initial-contents '((0 1 0 0 0 0)
                                  (1 1 0 1 1 1)
                                  (1 1 1 1 1 1)
                                  (0 1 0 1 1 1)
                                  (1 0 0 0 0 0))))

(nav-obstacles *k-obstacles* 2)


;;; Example 5: 1129. Shortest Path with Alternating Colors
;; 
;; You are given a directed graph with n nodes labeled from 0 to n - 1. Edges are red or blue in this graph. 
;; You are given redEdges and blueEdges, where redEdges[i] and blueEdges[i] both have the format [x, y] indicating an edge from x to y in the respective color. 
;; Return an array ans of length n, where answer[i] is the length of the shortest path from 0 to i where edge colors alternate, or -1 if no path exists.

;; map red to 0 and blue to 1 
;; f(x) = 1 - x gives the alternating colour

(defun add-edge-type (edges type-prefix edge-map)
  (declare (type hash-table edge-map))
  (if (null edges)
      edge-map
      (let* ((edge (car edges)) (parent (car edge)) (child (cdr edge)) (key (cons type-prefix parent))) 
        (setf (gethash key edge-map) (cons child (gethash key edge-map))) 
        (add-edge-type (cdr edges) type-prefix edge-map))))

;; for a given starting path, starting colour to look for, and target node, 
;; this finds the length of the shortest path to node target, if there is one, and the path,
;; otherwise returns -1 and nil
(defun recur-alt (paths target colour edge-map &optional (seen (make-hash-table))) ; colour is colour of edge we're looking for
  (declare (type list paths) (type fixnum target colour) (type hash-table edge-map seen))
  (when (null paths) 
    (return-from recur-alt (values -1 nil)))
  (let ((new-paths nil))
    (loop while paths do 
          (let* ((path (pop paths)) (linked-nodes (gethash (cons colour (car path)) edge-map)))
            (loop for node in linked-nodes do 
                  (if (eql node target) 
                      (return-from recur-alt (values (1+ (length path)) (cons node path)))
                      (when (null (gethash node seen)) 
                        (setf (gethash node seen) t) 
                        (push (cons node path) new-paths))))))
    (recur-alt new-paths target (- 1 colour) edge-map seen)))

(defun shortest-alt-i-path (edge-map i)
  (declare (type hash-table edge-map) (type fixnum i))
  (when (eql i 0)
    (return-from shortest-alt-i-path (values 1 (list 0))))
  (let ((red-paths (list (list 0))) (blue-paths (list (list 0))))
    (multiple-value-bind (red-path-len red-path) (recur-alt red-paths i 0 edge-map)
      (multiple-value-bind (blue-path-len blue-path) (recur-alt blue-paths i 1 edge-map)
        (when (eql red-path-len -1)
          (if (eql blue-path-len -1)
              (return-from shortest-alt-i-path (values -1 nil))
              (return-from shortest-alt-i-path (values blue-path-len blue-path))))
        (when (eql blue-path-len -1)
          (return-from shortest-alt-i-path (values red-path-len red-path)))
        (if (<= red-path-len blue-path-len)
            (values red-path-len red-path)
            (values blue-path-len blue-path))))))

;; red-edges and blue-edges are lists with elements of format (x . y)
;; n is the number of nodes, where nodes are labelled over interval [0 ... n)
;; returns a list of lengths of shortest paths
(defun shortest-alt-paths (red-edges blue-edges n)
  (declare (type list red-edges blue-edges) (type fixnum n))
  (let ((edge-map (make-hash-table :test #'equal)) (ans nil))
    (setf edge-map (add-edge-type red-edges 0 edge-map)) ; red are 0
    (setf edge-map (add-edge-type blue-edges 1 edge-map)) ; blue are 1
    (setf *edge-map* edge-map)
    (loop for i below n do 
          (push (shortest-alt-i-path edge-map i) ans))
    (reverse ans)))

(defparameter *red-edges* (list (cons 0 1) (cons 2 3) (cons 4 5)))
(defparameter *blue-edges* (list (cons 1 2) (cons 3 4) (cons 5 6)))
(defparameter *n* 7)
;; should return (1 2 3 4 5 6 7)
(shortest-alt-paths *red-edges* *blue-edges* *n*)

