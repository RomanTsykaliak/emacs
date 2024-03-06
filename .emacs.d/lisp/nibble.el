;;; nibble --- sum up all the callories consumed in the last 7 days
;;;
;;; Copyright (C) 2018, 2019, 2022 Roman Tsykaliak
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Commentary:
;;; 06092050nibble(+ 1000)
;;; 06101222nibble(+ 300)
;;; 06101510nibble(+ 400)
;;; 06101902nibble(+ 200)
;;; 06101946nibble(+ 100)
;;; 06111034nibble(+ 300)
;;; 06111600nibble(+ 400)
;;; 06111810nibble(+ 300)
;;; 06121034nibble(+ 500)
;;; 06121528nibble(+ 500)
;;; 06130956nibble(+ 250)
;;; 06131455nibble(+ 250)
;;; 06131758nibble(+ 250)
;;; 06132212nibble(+ 250)
;;; 06140909nibble(+ 300)
;;; 06141134nibble(+ 400)
;;; 06141720nibble(+ 150)
;;; 06142237nibble(+ 150)
;;; 06151132nibble(+ 700)
;;; 06152353nibble(+ 300)
;;; 06161027nibble(+ 100)
;;; 06161246nibble(+ 500)
;;; 06161716nibble(+ 200)
;;; 06162030nibble(+ 200)
;;; 06172050nibble(+ 1000)
;;; 06181222nibble(+ 300)
;;; 06181510nibble(+ 400)
;;; 06181902nibble(+ 200)
;;; 06181946nibble(+ 100)
;;; 06191034nibble(+ 300)
;;; 06191600nibble(+ 400)
;;; 06191810nibble(+ 300)
;;; 06201034nibble(+ 500)
;;; 06201528nibble(+ 500)
;;; 06210956nibble(+ 250)
;;; 06211455nibble(+ 250)
;;; 06211758nibble(+ 250)
;;; 06212212nibble(+ 250)
;;; 06220909nibble(+ 300)
;;; 06221134nibble(+ 400)
;;; 06221720nibble(+ 150)
;;; 06222237nibble(+ 150)
;;; 06231132nibble(+ 700)
;;; 06232353nibble(+ 300)
;;; 06241027nibble(+ 100)
;;; 06241246nibble(+ 500)
;;; 06241716nibble(+ 200)
;;; 06242030nibble(+ 200)
;;; (nibble)"Today 1000 LastWeek 7000 Average 1000 Weeks (7000 7000)"

;;; Code:
(defun nibble ()
  "Search for ‘nibble’ tags in a day and evaluate the expressions."
  (interactive)
  (let* ((tag "nibble")
         (max-mmdd "1232") (max-hhmm "2359") (min-str "0000") (week 7) (cur-week 0)
         (sum 0) (days 0) (per-day) (last-week) (per-week 0) (prev-week 0) (list-week)
         (mmdd min-str) (hhmm min-str)
         (pre-mmdd max-mmdd) (pre-hhmm max-hhmm))
    (save-excursion
      (while (re-search-backward (concat "^\\([0-9]\\{4\\}\\)\\([0-9]\\{4\\}\\)" tag "") nil t)
        ;; (insert (format "while day is %d" days))
        (setq mmdd (match-string-no-properties 1))
        (setq hhmm (match-string-no-properties 2))
        (if (string= mmdd pre-mmdd)
            () ;; check hours; below
          (if (string-lessp mmdd pre-mmdd) ;; else if
              (progn (setq pre-mmdd mmdd)
                     (setq pre-hhmm max-hhmm)
                     (setq days (1+ days))
                     ;; (insert (format "\npre-mmdd %s day %1d"
                     ;;                 pre-mmdd days))
                     ) ;; check hours; below
            (error "Error: date %s is bigger then previous %s"
                   mmdd pre-mmdd)))
        ;; here date is less or equal to pre-date; check hours
        (if (string< hhmm pre-hhmm)
            (setq pre-hhmm hhmm)
            ;; (progn (setq pre-hhmm hhmm)
            ;;        (insert (format "\npre-hhmm %s hhmm %s"
            ;;                        pre-hhmm hhmm)))
          (error (concat "Error: time %s is bigger or equal to "
                         "previous %s at date %s")
                 hhmm pre-hhmm mmdd))
        ;; count calories for the current day
        ;; (insert (format "\neval-region for day %1d" days))
        (let*
            ((raw-result
              (with-output-to-string
                (eval-region
                 (goto-char (match-end 0))
                 (progn (skip-chars-forward "^") (point))
                 standard-output)))
             (result (when (>= (length raw-result) 1)
                       (substring raw-result 1 -1))))
          ;; (insert (format "result %s" result))
          (when result ;; nil when eval-region empty
            (setq sum (+ sum (string-to-number result)))))
        (goto-char (match-beginning 0))
        (when (= days 1) (setq per-day sum)) ;; for a day sum
        (if (= cur-week (- (1- days) (% (1- days) week))) ;; each 7 days, days start with 1
            (setq per-week sum)                 ;; sum will be higher by one rotation
          (progn (setq cur-week (- (1- days) (% (1- days) week)))
                 (push (- per-week prev-week) list-week)
                 (setq prev-week per-week)))
                 ;; (insert (format "%d %d %d" cur-week prev-week per-week))))
        (when (= days 7) (setq last-week sum)) ;; get last week sum
      ;;(insert (format "\nsum %5d" sum))))
      ))
    (message "Today %d LastWeek %d Average %d Weeks %s"
             per-day (or last-week 0) (/ sum days) (or (princ list-week) ""))))

(provide 'nibble)
;;; nibble.el ends here
