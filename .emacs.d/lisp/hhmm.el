;;; hhmm --- calculate the time spent on a TAG
;;;
;;; Copyright (C) 2017, 2021 Roman Tsykaliak
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
;;; 
;;; 11070000refuseNA
;;; 11070800awokenNA
;;; 11070830nibbleNA
;;; 11070930scrawlNA
;;; 11071030investNA
;;; 11071200labourNA
;;; 11071300nibbleNA
;;; 11071800labourNA
;;; 11071830nibbleNA
;;; 11071900hobnobNA
;;; 11072000expendNA
;;; 11072100peruseNA
;;; 11072200pursueNA
;;; 11070000refuseNA
;;; M-x hhmm-region-wrap
;;; "awoken 0800"
;;; "expend 0100"
;;; "nibble 0200"
;;; "scrawl 0100"
;;; "peruse 0100"
;;; "pursue 0100"
;;; "hobnob 0030"
;;; "labour 0630"
;;; "invest 0100"
;;; "refuse 0200"
;;; "AMOUNT 2400"

;;; Code:
(defun hhmm-sub (lhhmm rhhmm)
  "Subtract two strings LHHMM and RHHMM which represent time HHMM."
  ;; (or (and
  ;;      (save-match-data (string-match "^[0-9]+[0-9]\\{2\\}$" lhhmm))
  ;;      (save-match-data (string-match "^[0-9]+[0-9]\\{2\\}$" rhhmm)))
  ;;     (error "Wrong: %s or %s" lhhmm rhhmm))
  (let* ((lhh (string-to-number (substring lhhmm 0 -2)))
         (lmm (string-to-number (substring lhhmm -2 nil)))
         (rhh (string-to-number (substring rhhmm 0 -2)))
         (rmm (string-to-number (substring rhhmm -2 nil)))
         (mm (if (and (> 60 lmm) (> 60 rmm))
                 (- (if (< lmm rmm)
                        (progn (setq lhh (1- lhh)) (+ 60 lmm))
                      lmm)
                    rmm)
               (error "Wrong: %s>=60 or %s>=60" lmm rmm)))
         (hh (if (> 0 (- lhh rhh))
                 (error "Wrong: %s < %s" lhhmm rhhmm)
               (- lhh rhh)))
         (result (format "%02d%02d" hh mm)))
    ;;(insert (format "lhh %d lmm %d rhh %d rmm %d mm %d hh %d"
    ;;                lhh lmm rhh rmm mm hh))
    result))

(defun hhmm-add (lhhmm rhhmm)
  "Add two strings LHHMM and RHHMM which represent time HHMM."
  ;; (save-match-data
  ;;   (or (and
  ;;        (string-match "\\(^[0-9]+\\)\\([0-9]\\{2\\}\\)$" lhhmm)
  ;;        (string-match "\\(^[0-9]+\\)\\([0-9]\\{2\\}\\)$" rhhmm))
  ;;       (error "Wrong: %s or %s" lhhmm rhhmm)))
  (let* ((lhh (string-to-number (substring lhhmm 0 -2)))
         (lmm (string-to-number (substring lhhmm -2 nil)))
         (rhh (string-to-number (substring rhhmm 0 -2)))
         (rmm (string-to-number (substring rhhmm -2 nil)))
         (mm (if (and (> 60 lmm) (> 60 rmm))
                 (if (>= (+ lmm rmm) 60)
                     (progn (setq lhh (1+ lhh)) (- (+ lmm rmm) 60))
                   (+ lmm rmm))
               (error "Wrong: %s>=60 or %s>=60" lmm rmm)))
         (hh (+ lhh rhh))
         (result (format "%02d%02d" hh mm)))
    ;;(insert (format "lhh %d lmm %d rhh %d rmm %d mm %d hh %d"
    ;;                lhh lmm rhh rmm mm hh))
    result))

(defun hhmm-span (lhhmm rhhmm)
  "Time spent between two times LHHMM and RHHMM in a one day."
  ;; (save-match-data
  ;;   (or (and
  ;;        (string-match "^[0-9]\\{4\\}$" lhhmm)
  ;;        (string-match "^[0-9]\\{4\\}$" rhhmm))
  ;;       (error "Wrong: %s or %s" lhhmm rhhmm)))
  (let* ((lhh (string-to-number (substring lhhmm 0 -2)))
         (lmm (string-to-number (substring lhhmm -2 nil)))
         (rhh (string-to-number (substring rhhmm 0 -2)))
         (rmm (string-to-number (substring rhhmm -2 nil)))
         (result (if (and (> 24 lhh) (> 60 lmm) (> 24 rhh) (> 60 rmm))
                     (if (string< lhhmm rhhmm)
                         (hhmm-sub rhhmm lhhmm)
                       (hhmm-sub (hhmm-add "2400" rhhmm) lhhmm))
                   (error "Wrong: %s>=24 or %s>=60 or %s>=24 or %s>=60"
                          lhh lmm rhh rmm))))
    result))

(defun hhmm-region (beginning end &optional tag)
  "In the region from BEGINNING to END get a time spent on a TAG."
  (interactive "r")
  (or tag (setq tag "labour"))
  (message "Counting time spent on a '%s'" tag)
  (save-excursion
    (goto-char beginning)
    (let ((lhhmm) (rhhmm) (result "0000") (ctag))
      (while (and (< (point) end)
                  (re-search-forward
                   (concat "^[0-9]\\{4\\}\\([0-9]\\{4\\}\\)"
                           tag ".*$") end t))
        (setq rhhmm (match-string-no-properties 1))
        (save-excursion
          (save-match-data
            (setq lhhmm rhhmm)
            (setq ctag "")
            (search-backward "\n")
            (while (and (string= rhhmm lhhmm)
                        (not (string= ctag tag))
                        (re-search-backward
                         (concat "^[0-9]\\{4\\}\\([0-9]\\{4\\}\\)"
                                 "\\(.*?\\)" ".*$") beginning t))
              (setq lhhmm (match-string-no-properties 1))
              (setq ctag (match-string-no-properties 2))
              (when (not (string= rhhmm lhhmm))
                (setq result (hhmm-add result
                                       (hhmm-span lhhmm rhhmm))))))))
      (message "%s %6s" tag result))))

(defun hhmm-region-wrap (beginning end)
  "Print time spent on tags from BEGINNING to END."
  (interactive "r")
  (let ((sum "0000") (tmp))
    (message "Counting time spent ...")
    (message "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\nAMOUNT %6s"
             (progn (setq tmp (hhmm-region beginning end "awoken"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "expend"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "nibble"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "scrawl"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "peruse"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "pursue"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "hobnob"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "labour"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "invest"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             (progn (setq tmp (hhmm-region beginning end "refuse"))
                    (setq sum (hhmm-add sum (car (last (split-string tmp))))) tmp)
             sum)))

(provide 'hhmm)
;;; hhmm.el ends here
