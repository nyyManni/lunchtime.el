;;; ttymenu.el --- read restaurant menus in Emacs.

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman at gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 20170214.1
;; Version: 0.1

;; This file is NOT part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:

;; ttymenu.el provides an interface and a major mode for reading restaurant
;; menus within Emacs.

;;; Code:

(require 'request)

(defvar ttymenu--menu-alist ()
  "List containing menu data.")

(defvar ttymenu--day-offset 0
  "How many days from now to fetch menus.")


(defvar ttymenu--menu-cache (make-hash-table :test 'equal)
  "Cached menu data to avoid extra API calls.")

(defun ttymenu--get-data (date-str)
  "HTTP GET to the API.  DATE-STR: ISO-format datestring."
  (setq ttymenu--menu-alist
        (if (gethash date-str ttymenu--menu-cache)
            (gethash date-str ttymenu--menu-cache)
          
          (let* ((menu-response (request
                                 (concat "https://api.ruoka.xyz/" date-str)
                                 :parser (lambda ()
                                           (let ((json-object-type 'plist))
                                             (json-read)))
                                 :sync t))

                 (menu-data (request-response-data menu-response)))
            (puthash date-str (plist-get menu-data :restaurants) ttymenu--menu-cache)))))
  
(defun ttymenu--get-datetime-from-offset (days)
  (let* ((time (current-time))
         (up (first time))
         (seconds (+ (second time) (* 60 60 24 days)))
         (rest (cddr time)))
    (cons up (cons seconds rest))))

(defun ttymenu--get-time-str-offset (days)
  (format-time-string "%F" (ttymenu--get-datetime-from-offset days)))

(defun ttymenu--insert-header ()
  (let* ((weekdays [nil "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])
         (date (ttymenu--get-datetime-from-offset ttymenu--day-offset))
         (weekday (aref weekdays (string-to-int (format-time-string "%u" date)))))
   (insert (concat "  TUT Menu  -  "
                   weekday
                   (format-time-string ", %d. of %B %Y" date)
                   "\n\n"))))

(defun --insert-utf-8 (str)
  (insert (decode-coding-string str 'utf-8)))

(defun ttymenu--list-contents (meal)
  (--insert-utf-8 (concat )))

(defun pad-price (price)
  (if (= (length price) 4)
      (concat " " price)
    price))

(defun ttymenu--list-content (content)
  (--insert-utf-8
   (concat "  - " (replace-regexp-in-string "\r\n" "" (plist-get content :name)) "\n")))

(defun ttymenu--list-meal-details (meal)
  "List details of MEAL."
  (let* ((item (concat " " (downcase (decode-coding-string (plist-get meal :name) 'utf-8))))
         (item-len (length item))
         (prices (mapconcat 'pad-price (plist-get meal :prices) "  "))
         (prices-len (length prices))
         (padding-len (- (- (window-total-width) 4) (+ prices-len item-len))))
    (insert (concat item (make-string padding-len ? ) prices "\n")))
  (mapc 'ttymenu--list-content (plist-get meal :contents)))

(defun ttymenu--list-meals (restaurant)
  "List all meals from RESTAURANT."
  (let* ((item (concat "= " (decode-coding-string (plist-get restaurant :name) 'utf-8) " "))
         (item-len (length item))
         (padding-len (- (- (window-total-width) 4) item-len)))
    (insert (concat item (make-string padding-len ?=) "=\n")))
  (mapc
   (lambda (subrestaurant)
     (let* ((item (concat "-- " (decode-coding-string (plist-get subrestaurant :name) 'utf-8) " "))
            (padding-len (- (- (window-total-width) 4) (length item))))
       (--insert-utf-8 (concat item (make-string padding-len ?-) "-\n")))
     ;; (--insert-utf-8 (concat "   " (plist-get subrestaurant :name) "\n"))
     (mapc 'ttymenu--list-meal-details (plist-get subrestaurant :meals)))
   (plist-get restaurant :menus)))

(defun ttymenu--redraw ()
  (with-current-buffer (get-buffer-create "*TUT menu*")
    (let ((inhibit-read-only t))
      ;; Insert contents
      (erase-buffer)
      (ttymenu--insert-header)
      (mapc 'ttymenu--list-meals ttymenu--menu-alist))

    ;; Run the menu in org-mode
    (let ((mode 'ttymenu-mode))
      (funcall mode))

    (beginning-of-buffer)

    ;; Switch to the buffer
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun ttymenu-display-menus ()
  "Open a new buffer showing current menu."
  (interactive)

  (setq ttymenu--day-offset 0)

  ;; Refresh the data from server
  (ttymenu--get-data
   (ttymenu--get-time-str-offset ttymenu--day-offset))
  (ttymenu--redraw))

(defun ttymenu-next-day ()
  (interactive)
  (progn
    (setq ttymenu--day-offset (+ ttymenu--day-offset 1))
    (ttymenu--get-data
     (ttymenu--get-time-str-offset ttymenu--day-offset))
    (ttymenu--redraw)))

(defun ttymenu-previous-day ()
  (interactive)
  (progn
    (setq ttymenu--day-offset (- ttymenu--day-offset 1))
    (ttymenu--get-data
     (ttymenu--get-time-str-offset ttymenu--day-offset))
    (ttymenu--redraw)))

(defun ttymenu-close ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defvar ttymenu-mode-map
  (let ((map (make-sparse-keymap))) map))

;;;###autoload
(define-derived-mode ttymenu-mode special-mode "TUT Menu"
  "Fetch Tampere Universiry of Technology restaurant menus."
  (local-set-key (kbd "l") 'ttymenu-next-day)
  (local-set-key (kbd "h") 'ttymenu-previous-day)
  (add-hook 'window-configuration-change-hook #'ttymenu--redraw t t))

(add-hook 'ttymenu-mode-hook
  (lambda () 
    (font-lock-add-keywords nil 
      '(("^  TUT Menu .*$" . font-lock-warning-face)
        ("^= .*" . font-lock-builtin-face)
        ("^-- .*" . font-lock-constant-face)
	("^ \\(.*\\)       \\([0-9][0-9, ]+[0-9]\\)$"
	 (1 font-lock-comment-face) (2 font-lock-string-face))))))

(provide 'ttymenu)
;;; ttymenu.el ends here
