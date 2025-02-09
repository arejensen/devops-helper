(in-package #:devops-helper)

(defun filter-and-sort (regex lst)
  "Return a list of strings from LST that match REGEX,
sorted alphabetically."
  (let ((filtered (remove-if-not (lambda (s)
                                   (cl-ppcre:scan regex s))
                                 lst)))
    (sort filtered #'string>)))

(defun increment-last-number (s)
  "Return S with its last number incremented by one, preserving the number’s width.
If no number is found, return S unchanged."
  (let ((i (1- (length s))))
    ;; Find the index of the last digit in the string.
    (loop while (and (>= i 0)
                     (not (digit-char-p (char s i))))
          do (decf i))
    (if (< i 0)
        s  ; No digit found: return the original string.
        (let ((end (1+ i)))
          ;; Find the beginning of the contiguous digit sequence.
          (loop while (and (> i 0)
                           (digit-char-p (char s (1- i))))
                do (decf i))
          (let* ((start i)
                 (num-str (subseq s start end))
                 (number (parse-integer num-str))
                 (incremented (1+ number))
                 (new-num-str (format nil "~v,'0d" (length num-str) incremented)))
            ;; Rebuild the string.
            (concatenate 'string (subseq s 0 start)
                         new-num-str
                         (subseq s end)))))))

(defun bump-tag (regex repo)
  "Bumps the tag matching regex in repo to a new version.
E.g. if you match a tag like 1.9.2-pre0001, the new tag will be 1.9.2-pre0002."
  (let ((tag-name (increment-last-number (first (filter-and-sort regex (legit:tags repo))))))
    (legit:with-chdir (repo)
      (legit:git-tag :tags tag-name :annotate t :message tag-name))))
