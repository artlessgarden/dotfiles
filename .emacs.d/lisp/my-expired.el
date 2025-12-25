
(require 'cl-lib)

(defun my/parse-date (s)
  "把 \"12_05\" 解析成 (12 5)。"
  (when (string-match "^\\([0-9]+\\)_\\([0-9]+\\)$" s)
    (list (string-to-number (match-string 1 s))
          (string-to-number (match-string 2 s)))))

(defun my/days-until (month day)
  "距离当前日期还有多少天（负数表示已过期）。"
  (let* ((year   (string-to-number (format-time-string "%Y")))
         (target (ignore-errors (encode-time 0 0 0 day month year))))
    (if (null target)
        999999
      (floor (/ (float-time (time-subtract target (current-time)))
                86400)))))

(defun my/find-all-expiry ()
  "返回所有月_日，附带 (行文本)。每个元素是 plist：(:str :pos :days :line)"
  (let (results)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\b[0-9]+_[0-9]+\\b" nil t)
        (let* ((str  (match-string 0))
               (pos  (match-beginning 0))
               (md   (my/parse-date str)))
          (when md
            (let* ((month (nth 0 md))
                   (day   (nth 1 md))
                   (days  (my/days-until month day))
                   (line  (string-trim
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
              (push (list :str str
                          :pos pos
                          :days days
                          :line line)
                    results))))))
    ;; 排序
    (cl-sort results #'< :key (lambda (it) (plist-get it :days)))))

(defun my/add-one-month (date-str)
  "\"12_05\" → 加一个月 → \"01_05\"。"
  (let* ((md    (my/parse-date date-str))
         (month (nth 0 md))
         (day   (nth 1 md))
         (year  (string-to-number (format-time-string "%Y")))
         (time  (encode-time 0 0 0 day (1+ month) year)))
    (format "%02d_%02d"
            (string-to-number (format-time-string "%m" time))
            (string-to-number (format-time-string "%d" time)))))

(defun my/renew-expiry ()
  "选择某个 月_日 所在行 → 续费一个月。"
  (interactive)
  (let ((items (my/find-all-expiry)))
    (if (null items)
        (message "没找到 月_日 格式的日期。")
      (let* ((choices
              ;; 每一项显示： 12_05（3 天后） | 整行内容
              (mapcar (lambda (it)
                        (cons (format "%s  (%d 天后)  |  %s"
                                      (plist-get it :str)
                                      (plist-get it :days)
                                      (plist-get it :line))
                              it))
                      items))
             (choice-key
              (completing-read "选择续费的日期: "
                               (mapcar #'car choices)
                               nil t))
             (record  (cdr (assoc choice-key choices)))
             (old-str (plist-get record :str))
             (pos     (plist-get record :pos))
             (new-str (my/add-one-month old-str)))
        ;; 替换
        (save-excursion
          (goto-char pos)
          (delete-char (length old-str))
          (insert new-str))
        (message "续费完成：%s → %s" old-str new-str)))))


(provide 'my-expired)
