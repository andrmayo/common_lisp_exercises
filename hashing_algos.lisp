;;; hash table algorithms

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
