;; stack and queue algorithms

;; Example 1: 20. Valid Parentheses

;; Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid. The string is valid if all open brackets are closed by the same type of closing bracket in the correct order, and each closing bracket closes exactly one open bracket.

;; For example, s = "({})" and s = "(){}[]" are valid, but s = "(]" and s = "({)}" are not valid.

(defun is-left-bracket (input)
  (declare (type character input))
  (let ((as-list (cons (char "()" 0) '(#\{ #\[)))) 
    (if (member input as-list)
        t
        nil)))

(defun get-match (ch)
  (declare (type character ch))
  (if (char= ch (char "()" 0)) 
      (char "()" 1)
      (if (char= ch #\{)
          #\}
          (if (char= ch #\[)
              #\]))))

(defun valid-parenth? (input)
  (declare (type string input))
  (let ((char-list (coerce input 'list)) (is-valid t) (seen nil))
    (loop while (and char-list is-valid) do
          (let ((ch (pop char-list)))
            (if (is-left-bracket ch)
                (push ch seen)
                (if (not (char= ch (get-match (pop seen))))
                    (setf is-valid nil)))))
    (if (> (length seen) 0)
        (setf is-valid nil))
    is-valid))

(valid-parenth? "{[]{}}")

;;; Example 2: 1047. Remove All Adjacent Duplicates In String
;; You are given a string s. Continuously remove duplicates (two of the same character beside each other) until you can't anymore. Return the final string after this. 
;; For example, given s = "abbaca", you can first remove the "bb" to get "aaca". Next, you can remove the "aa" to get "ca". This is the final answer.

(defun remove-dupl-str (input)
  (declare (type string input))
  (let ((char-list (coerce input 'list)) (seen nil))
    (loop while char-list do
          (let ((ch1 (pop char-list)) (ch2 (pop seen)))
            (if (null ch2)
                (push ch1 seen)
                (if (not (char= ch1 ch2))
                    (progn
                      (push ch2 seen)
                      (push ch1 seen))))))
    (coerce (reverse seen) 'string)))

(remove-dupl-str "addaswqeeasd")

;; Example 3: 844. Backspace String Compare
;; Given two strings s and t, return true if they are equal when both are typed into empty text editors. '#' means a backspace character.
;; For example, given s = "ab#c" and t = "ad#c", return true. Because of the backspace, the strings are both equal to "ac".

(defun backspace-str-cmp (input1 input2)
  (declare (type string input1) (type string input2))
  (let ((char-list1 nil) (char-list2 nil))
    (loop for ch across input1 do
          (if (char= ch #\#)
              (pop char-list1)
              (push ch char-list1)))
    (loop for ch across input2 do
          (if (char= ch #\#)
              (pop char-list2)
              (push ch char-list2)))
    (equal char-list1 char-list2)))

;;; implementation of FIFO queue as a struct

(defstruct (queue (:constructor make-queue (&optional initial-contents)))
  (elements nil :type list)
  (tail nil :type (or null cons)))

(defmethod initialize-instance :after ((q queue) &key initial-contents)
  (when initial-contents
    (dolist (item initial-contents)
      (enqueue item q))))

(defun enqueue (item q)
  (let ((new-cons (list item)))
    (if (null (queue-elements q))
        (setf (queue-elements q) new-cons
              (queue-tail q) new-cons)
        (setf (cdr (queue-tail q)) new-cons
              (queue-tail q) new-cons))))

(defun dequeue (q)
  (if (null (queue-elements q)) 
      (values nil nil) ; first nil reflects absence of element to return, second nil failure of dequeue
      (let ((item (car (queue-elements q))))
        (setf (queue-elements q) (cdr (queue-elements q)))
        (when (null (queue-elements q))
          (setf (queue-tail q) nil))
        (values item t)))) ; t indicates successful dequeue

(defun queue-empty-p (q)
  (null (queue-elements q)))

(setf q-example (make-queue))
(enqueue 10 q-example)
(enqueue 20 q-example)
(dequeue q-example)

;;; Example: 933. Number of Recent Calls
;; Implement the RecentCounter class. It should support ping(int t), 
;; which records a call at time t, and then returns an integer 
;; representing the number of calls that have happened in the range [t - 3000, t]. 
;; Calls to ping will have increasing t.

(defstruct (recent-counter 
             (:include queue) 
             (:constructor make-recent-counter (&key initial-contents trange))) 
  (trange 3000))

(defmethod initialize-instance :after ((rc recent-counter) &key initial-contents trange)
  (progn 
    (when initial-contents 
      (dolist (item initial-contents)
        (enqueue item rc)))
    (when trange 
      (setf (recent-counter-trange rc) trange))))

(defun ping (time rc)
  (declare (type integer time))
  (progn 
    (enqueue time rc)
    (let ((cur (car (queue-elements rc))))
      (loop while (< cur (- time (recent-counter-trange rc))) do 
            (progn 
              (dequeue rc)
              (setf cur (car (queue-elements rc))))))
    (length (queue-elements rc))))


(setf counter (make-recent-counter))
(ping 5020 counter)

;;; Example 1: 739. Daily Temperatures

;; Given an array of integers temperatures that represents the daily temperatures, 
;; return an array answer such that answer[i] is the number of days you have to wait after the ith day to get a warmer temperature. 
;; If there is no future day that is warmer, have answer[i] = 0 instead.

(defun days-til-hotter (temps)
  (declare (type (array integer (*)) temps))
  (let* ((len (length temps))
        (ans (make-array len :element-type 'integer :initial-element 0))
        (stack nil))
    (loop for i below len do
          (progn 
            (loop while (and stack (< (svref temps (car stack)) (svref temps i))) do
                (setf (svref ans (car stack)) (- i (pop stack))))
            (push i stack)))
    ans))


(setf temperatures (make-array 5 :initial-contents '(40 35 32 37 50)))
(days-til-hotter temperatures)

;;; implement a deque as a class,
;;; drawing on https://github.com/pcmendes1973/cl-deque/blob/master/cl-deque.lisp

(defclass node ()
  ((content 
     :accessor content
     :initarg :content)
   (prev
     :accessor prev
    :initform nil)
   (next
     :accessor next
     :initform nil))
  (:documentation "Node class for deque"))

(defun make-node (content &key prev next)
  "creates node linking backwards and forwards"
  (let ((n (make-instance 'node :content content)))
    (when prev 
      (setf (next prev) n (prev n) prev))
    (when next
      (setf (next n) next (prev next) n))
    (values n)))

(defun copy-node (node)
  (make-node (content node) :prev (prev node) :next (next node)))

(defun bind-nodes (a b)
  "Bind nodes a and b, with b placed after a"
  (setf (next a) b (prev b) a))

(defmethod print-object ((obj node) stream)
  "Prints a node object with format:
  
  <NODE content sole|first|middle|last>

  where sole means the node is not linked to other nodes,
  first, middle, and last mean the node is first, middle, or last in a list"
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((content content)
                     (next next)
                     (prev prev))
        obj
      (format stream "~a ~:[~:[sole~;first~]~;~:[last~;middle~]~]" content prev next))))

(defun print-list (nodes &key from-end)  
  "Prints out the items of a linked list in separate lines"
  (let ((direction (if from-end 'prev 'next)))
    (loop for i = nodes then (slot-value i direction)
          while i do (pprint i))))

(defmacro do-linked-list ((var nodes &key from-end) &body body)
  (let ((i (gensym)))
    "Iterates over linked list 'nodes'  in either direction"
    `(loop for ,i = ,nodes
           then (,(if from-end 'prev 'next) ,i)
           while ,i
           do (let ((,var (content ,i))) (progn ,@body)))))

(defun make-linked-list (nodes)
  "Creates a doubly linked list from a common list. Returns
pointers to the first and last elements in the list and the
number of nodes in the list."
  (if nodes 
      (loop with 1st = (make-node (first nodes))
            for i in nodes
            for j = 1st then (make-node i :prev j)
            counting t into n
            finally (return (values 1st j n)))
      (values nil nil 0)))

;;; deque class

(defclass deque ()
  ((element-count
     :accessor element-count 
     :initarg :element-count 
     :type integer)
   (first-element
     :accessor first-element
     :initform nil)
   (last-element 
     :accessor last-element 
     :initform nil))
  (:documentation "deque class"))


(defmethod print-object ((obj deque) stream)
    "Prints a deque object. Output has the format:

<DEQUE :elements <element-count> :contents (first ... last)>"
    (print-unreadable-object (obj stream :type t)    
      (with-accessors ((first first-element)
                       (last last-element)
                       (c element-count)
                       (p pointer))
          obj
        (format stream "~[empty~:;:elements ~:*~d :content ~:*(~[~;~a~;~a ~a~:;~a ... ~a~])~]"
                c
                (if first (content first))
                (if last (content last))))))

(defun make-deque (&optional contents)
  "Deque constructor: takes a list and creates deque with elements in the same order"
  (multiple-value-bind (first last n) 
      (make-linked-list contents)
    (let ((d (make-instance 'deque :element-count n)))
      (setf (first-element d) first
            (last-element d) last)
      (values d))))

;;; inlining deque helper functions 

(declaim (inline add-first-element remove-single-element))

(defmethod add-first-element ((obj deque) element)
  "Add element to empty deque"
  (let ((new-node (make-node element)))
    (setf (element-count obj) 1 
          (first-element obj) new-node
          (last-element obj) new-node)))

(defmethod remove-single-element ((obj deque))
  "Empties a deque containing one element"
  (setf (element-count obj) 0
        (first-element obj) nil 
        (last-element obj) nil))

(defmethod empty-deque-p ((obj deque))
  "returns t for empty deque"
  (zerop (element-count obj)))

(defmethod append-element ((obj deque) element)
  "Add element to end of deque, and return deque"
  (if (empty-deque-p obj)
      (add-first-element obj element)
      (progn 
        (make-node element :prev (last-element obj))
        (incf (element-count obj))
        (setf (last-element obj) (next (last-element obj)))))
  (values obj))

;;; Functions for appending, prepending and removing elements from
;;; either end of the deque.
(defmethod prepend-element ((obj deque) element)
  "Add one element to the start of a deque. Return the enlarged deque."
  (if (zerop (element-count obj))
      (add-first-element obj element)
      (progn (make-node element :next (first-element obj))
             (incf (element-count obj))
             (setf (first-element obj)
                    (prev (first-element obj)))))
  (values obj))


(defmethod pop-last ((obj deque))
  "Remove one element from the end of a deque. Return the shortened deque."
  (let ((result (unless (zerop (element-count obj))
                  (content (last-element obj)))))
    (case (element-count obj)
      (0
       (values nil nil))
      (1
       (remove-single-element obj)
       (values result t))
      (otherwise
       (setf (last-element obj) (prev (last-element obj))
             (next (last-element obj)) nil)
       (decf (element-count obj))
       (values result t)))))

(defmethod pop-first ((obj deque))
  "Remove one element from the start of a deque. Return the shortened deque."
  (let ((result (unless (zerop (element-count obj))
                  (content (first-element obj)))))
    (case (element-count obj)
      (0
       (values nil nil))
      (1
       (remove-single-element obj)
       (values result t))
      (otherwise
       (setf (first-element obj) (next (first-element obj))
             (prev (first-element obj)) nil)
       (decf (element-count obj))
       (values result t)))))
   

;;; Example 2: 239. Sliding Window Maximum
 
;; Given an integer array nums and an integer k, there is a sliding window of size k that moves from the very left to the very right. 
;; For each window, find the maximum element in the window.

;; For example, given nums = [1, 3, -1, -3, 5, 3, 6, 7], k = 3, return [3, 3, 5, 5, 6, 7]. 

;; The first window is [1, 3, -1] and the last window is [3, 6, 7]

(defun max-window (nums k)
  (declare (type (array integer (*)) nums) (type integer k))
  (let ((window (make-deque)) (ans (make-array (+ (- (length nums) k) 1))))
    (loop for num across nums 
          for i below (length nums)
          ;; in each loop, first pop from left if index of leftmost element is out-of-window
          ;; then remove all elements from right that are lower than current element
          ;; then if window is the right size, add leftmost element to ans array
          do (progn 
            (when (and (first-element window) (<= (content (first-element window)) (- i k)))
                (pop-first window))
            (loop while (and 
                   (last-element window) 
                   (< (svref nums (content (last-element window))) num))
                do (pop-last window) 
                finally (append-element window i)) ; append index, rather than num
            (when (>= i (- k 1))
              (setf (svref ans (+ (- i k) 1)) (svref nums (content (first-element window)))))))
    (values ans)))
                   
(max-window (make-array 8 :initial-contents '(1 3 -1 -3 5 3 6 7)) 3)

;;; Example 3: 1438. Longest Continuous Subarray With Absolute Diff Less Than or Equal to Limit
;; Given an array of integers nums and an integer limit, 
;; return the size of the longest subarray such that the absolute difference between any two elements of this subarray is less than or equal to limit.

(defun long-contin-subarr (nums limit)
  (declare (type (array integer (*)) nums) (type integer limit))
  (let ((monotone-incr (make-deque)) (monotone-decr (make-deque)) (max-size 0) (left 0))
    (loop for num across nums
          for i below (length nums)
          do (progn 
               (loop while (and (last-element monotone-incr) (> (svref nums (content (last-element monotone-incr))) num))
                 do (pop-last monotone-incr)
                 finally (append-element monotone-incr i))
               (loop while (and (last-element monotone-decr) (< (svref nums (content (last-element monotone-decr))) num))
                 do (pop-last monotone-decr)
                 finally (append-element monotone-decr i))
               (loop while (> (abs (- (svref nums (content (first-element monotone-decr))) 
                                 (svref nums (content (first-element monotone-incr)))))
                         limit)
                      do (if (< (content (first-element monotone-decr)) (content (first-element monotone-incr))) 
                             (setf left (+ 1 (pop-first monotone-decr))) 
                             (setf left (+ 1 (pop-first monotone-incr)))))
               (setf max-size (max max-size (+ 1 (- i left))))))
    (values max-size)))

(setf *contin-arr-input* '(1 2 3 -10 5 2 -3 20 18 17 20 22 23 21 17 15 23 -30))
(long-contin-subarr (make-array (length *contin-arr-input*) :initial-contents *contin-arr-input*) 5)
