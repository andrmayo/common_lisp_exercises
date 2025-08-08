
;; some exercises on arrays, strings, and sliding windows

;; below has time complexity O(nm)
(defun string_subseq? (s search_space) ; return subseq of search_space beginning at match if string s is a subsequence of search_space, no gaps
  (if (> (length s) (length search_space))
      nil
      (if (string= s (subseq search_space 0 (length s)))
          search_space
          (string_subseq? s (subseq search_space 1)))))

;; version of prev function that allows gaps, with time_complexity O(m)
(defun gapped_string_subseq? (s search_space)
  (if (eql (length s) 0)
    t
    (if (> (length s) (length search_space))
        nil
        (if (eql (char s 0) (char search_space 0))
            (gapped_string_subseq (subseq s 1) (subseq search_space 1))
            (gapped_string_subseq s (subseq search_space 1))))))


(defun build_prefix (nums accum)
  (if (null (cdr nums))
      (list (+ accum (car nums)))
      (let ((new_accum (+ accum (car nums))))
        (cons new_accum (build_prefix (cdr nums) new_accum)))))

(defun prefix (nums)
  (build_prefix nums 0))

(defun eval_query (nums query limit pref-list)
  (< (+ (- (nth (cdr query) pref-list) (nth (car query) pref-list)) (nth (car query) nums)) limit))

(defun answer_queries_recur (nums queries limit pref-list)
  (let ((query (car queries)))
    (if (null query)
        nil
        (cons (eval_query nums query limit pref-list) (answer_queries_recur nums (cdr queries) limit pref-list)))))

(defun answer_queries (nums queries limit)
    (answer_queries_recur nums queries limit (prefix nums)))
    
(answer_queries '(1 2 3 4 5) '((0 . 1) (0 . 2) (2 . 4)) 7)

;; Given an integer array nums, find the number of ways to split the array into two parts 
;; so that the first section has a sum greater than or equal to the sum of the second section. 
;; The second section should have at least one number.

(defun comp-split (pref-list)
  (>= (car pref-list) (- (car (last pref-list)) (car pref-list))))

(defun count-splits (pref-list)
  (if (null (cdr pref-list))
      0
      (if (comp-split pref-list)
          (+ 1 (count-splits (cdr pref-list)))
          (count-splits (cdr pref-list)))))

(defun ways-to-split (nums)
  (count-splits (prefix nums)))

;; hash table algorithms

;; Given an array of integers nums and an integer target, return indices of two numbers such that they add up to target. 
;; You cannot use the same index twice.

;; takes a list of integers and a hash table, adds entries mapping values in list to indices
(defun map-int-to-index (nums table index)
  (declare (type list nums) (type hash-table table) (type integer index))
  (if (null (cdr nums))
      (setf (gethash (car nums) table) index)
      (progn
        (setf (gethash (car nums) table) index)
        (map-int-to-index (cdr nums) table (+ index 1)))))

;; function to recurse through list looking for list's complement in a hashtable,
;; return (index1 . index2) if found, else (-1 . -1)
(defun find-complement (nums table target)
  (declare (type list nums) (type hash-table table) (type integer target))
  (let ((value (gethash (- target (car nums)) table)))
    (if (null (cdr nums))
        (if value
            (cons (gethash (car nums) table) value)
            '(-1 . -1))
        (if value
            (cons (gethash (car nums) table) value)
            (find-complement (cdr nums) table target)))))

(defun two-sum (nums target)
  (declare (type list nums) (type integer target))
  (let ((table (make-hash-table)))
    (progn 
      (map-int-to-index nums table 0)
      (find-complement nums table target))))

;; Given a string s, return the first character to appear twice.
;; It is guaranteed that the input will have a duplicate character.

(defun find-repeat-char (input table)
  (declare (type hash-table table))
  (if (null input)
      nil
      (if (gethash (car input) table)
          (car input)
          (progn 
            (setf (gethash (car input) table) t)
            (find-repeat-char (cdr input) table)))))

(defun repeated-character (input)
  (declare (type string input))
  (let ((table (make-hash-table)) (listed-input (coerce input 'list)))
    (find-repeat-char listed-input table)))

;; Given an integer array nums, find all the numbers x in nums that satisfy the following: x + 1 is not in nums, and x - 1 is not in nums.
;; If a valid number x appears multiple times, you only need to include it in the answer once.

(defun add-to-set (nums table)
  (declare (type list nums) (type hash-table table))
  (if (null (cdr nums))
      (setf (gethash (car nums) table) t)
      (progn 
        (setf (gethash (car nums) table) t)
        (add-to-set (cdr nums) table))))

(defun recurse-incr-decr (nums table &optional answers)
  (declare (type hash-table table))
  (if (null nums)
      answers
      (if (and (gethash (+ (car nums) 1) table) (gethash (- (car nums) 1) table))
          (progn 
            (push (car nums) answers)
            (recurse-incr-decr (cdr nums) table answers))
          (recurse-incr-decr (cdr nums) table answers))))


(defun check-incr-decr (nums)
  (declare (type list nums))
  (let ((table (make-hash-table)))
    (progn 
      (add-to-set nums table)
      (recurse-incr-decr nums table))))

;; You are given a string s and an integer k. Find the length of the longest substring that contains at most k distinct characters.
;; For example, given s = "eceba" and k = 2, return 3. The longest substring with at most 2 distinct characters is "ece".

(defun substring-k (str_input k)
  (declare (type string str_input) (type integer k))
  (let ((table (make-hash-table)) (left 0) (unique 0) (char-accum 0) (max-chars 0))
    (loop for charact across str_input
          do (progn
               (if (eql (gethash charact table 0) 0)
                   (setf unique (+ unique 1)))
               (setf char-accum (+ char-accum 1))
               (setf (gethash charact table) (+ (gethash charact table 0) 1))
               (loop while (> unique k)
                      do (progn
                           (if (eql (setf (gethash (char str_input left) table) (- (gethash (char str_input left) table) 1)) 0) 
                               (setf unique (- unique 1)))
                           (setf left (+ left 1))
                           (setf char-accum (- char-accum 1))))
               (setf max-chars (max max-chars char-accum))))
    max-chars))

;; Given a binary array nums (i.e. a vector), return the maximum length of a contiguous subarray with an equal number of 0 and 1.
(defun max-contig-equal (nums)
  (declare (type vector nums))
  (let ((count 0) (table (make-hash-table)) (ans 0))
    (setf (gethash count table) -1)
    (loop for num across nums
          for i from 0 do 
            (progn 
              (if (eql num 0)
                  (setf count (- count 1))
                  (setf count (+ count 1)))
              (if (null (gethash count table))
                  (setf (gethash count table) i)
                  (setf ans (max ans (- i (gethash count table)))))))
    ans))

(setf *bin-vec* (coerce '(1 0 0 1 0) 'vector))
(max-contig-equal *bin-vec*)

;; Given an array (or list) of strings strs, group the anagrams together.
;; For example, given strs = ["eat","tea","tan","ate","nat","bat"], return [["bat"],["nat","tan"],["ate","eat","tea"]].

(defun recurse-for-anagrams (word-list table &optional anagrams)
  (if (null word-list)
      anagrams
      (let ((key (sort (copy-seq (car word-list)) #'char-lessp)))
        (if (null (gethash key table))
            (setf anagrams (cons key anagrams)))
        (setf 
          (gethash key table) 
          (cons (car word-list) (gethash key table)))
        (recurse-for-anagrams (cdr word-list) table anagrams))))

(defun recurse-anagram-keys (table anagrams &optional anagrams-all)
  (if (null anagrams)
      anagrams-all
      (recurse-anagram-keys 
        table 
        (cdr anagrams)
        (setf 
          anagrams-all 
          (cons (gethash (car anagrams) table) anagrams-all)))))


(defun group-anagrams (word-list)
  (declare (type list word-list))
  (let ((table (make-hash-table :test #'equal)))
    (let ((anagrams (recurse-for-anagrams word-list table)))
      (recurse-anagram-keys table anagrams))))
    

(setf *word-list* '("eat" "tea" "tan" "ate" "nat" "bat"))
(group-anagrams *word-list*)

;; Given an integer array cards, find the length of the shortest subarray that contains at least one duplicate. 
;; If the array has no duplicates, return -1. This one I'm doing with an array rather than a list.

(defun shortest-array-len (cards)
  (declare (type (array integer (*)) cards))
  (let ((table (make-hash-table)) (min-interv nil))
    (loop for num across cards
          for i from 0 do
          (if (null (gethash num table))
              (setf (gethash num table) i)
              (let ((cur-interv (+ 1 (- i (gethash num table)))))
                (if (null min-interv)
                    (setf min-interv cur-interv)
                    (setf min-interv (min min-interv cur-interv)))
                (setf (gethash num table) i))))
    min-interv))

;; Max Sum of a Pair With Equal Sum of Digits
;; Given an array of integers nums, find the maximum value of nums[i] + nums[j], 
;; where nums[i] and nums[j] have the same digit sum (the sum of their individual digits). 
;; Return -1 if there is no pair of numbers with the same digit sum.

(defun get-digit-sum (num &optional (digit-sum 0))
  (declare (type integer num))
  (if (eq num 0)
      digit-sum
      (get-digit-sum (truncate (abs num) 10) (+ (rem num 10) digit-sum))))

(defun max-digit-sum-pair (nums)
  (declare (type (array integer (*)) nums))
  (let ((table (make-hash-table)) (max-pair-sum -1))
    (loop for num across nums do
          (let ((cur-digit-sum (get-digit-sum num)))
            (if (gethash cur-digit-sum table)
                (progn
                  (setf max-pair-sum (max max-pair-sum (+ num (gethash cur-digit-sum table))))
                  (setf (gethash cur-digit-sum table) (max (gethash cur-digit-sum table) num)))
                (setf (gethash cur-digit-sum table) num))))
    max-pair-sum))

(defparameter *digit-sum-list* '(121 34343 211 32323 19 29 82))
(defparameter *digit-sum-array* (make-array (length *digit-sum-list*) :initial-contents *digit-sum-list*))
(max-digit-sum-pair *digit-sum-array*)

;;; Equal Row and Column Pairs
;; Given an n x n matrix grid, return the number of pairs (R, C) where R is a row and C is a column,
;; and R and C are equal if we consider them as 1D arrays. 
;; note: because CL hash tables allow for mutable keys, it should be possible (unlike with Python)
;; to simply use vectors as as keys, provided make-hash-table is passed :test #'equal.

(defun count-equal-rows-columns (arr)
  (declare (type (array integer (* *))))
  (let* ((table (make-hash-table :test #'equalp)) (dims (array-dimensions arr)) (nrows (first dims)) (ncols (second dims)) (accum 0))
    (loop for i below nrows do
          (let ((row (make-array ncols :element-type (array-element-type arr))))
            (loop for j below ncols do 
                  (setf (aref row j) (aref arr i j)))
            (format t "row ~a is: ~a~%" i row)
            (if (null (gethash row table))
                (setf (gethash row table) t)
                (progn 
                  (format t "duplicate encountered: ~a~%" row)
                  (setf accum (+ accum 1))))))
    accum))

(defparameter *square-array* (make-array '(3 3) :initial-contents '((1 2 3) (1 2 3) (2 2 2))))
(count-equal-rows-columns *square-array*)

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

                 


