;;; some exercises on arrays, strings, and sliding windows

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

