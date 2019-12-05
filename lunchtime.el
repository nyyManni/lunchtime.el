;;; lunctime.el --- read restaurant menus in Emacs.

;; Copyright (C) 2018 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman at gmail.com>
;; Package-Requires: ((emacs "25.1") (request))
;; Version: 0.1

;; This file is NOT part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:

;; lunchtime.el provides an interface and a major mode for reading restaurant
;; menus within Emacs. For each restaurant menu source there has to exist a
;; parser that transforms the information fetched from the API into a unified
;; format used in this package. An example of the format can be seen below:

;; (((name . "Restaurant 1")
;;   (subrestaurants . (((name . "Subrestaurant 1")
;;                       (menus . (((name . "Menu Option 1")
;;                                  (menu . ("Some food" "More some food"))
;;                                  (prices . ("4,95" "10,20")))
;;                                 ((name . "Menu Option 2")
;;                                  (menu . ("Some other food"))
;;                                  (prices . ("4,95"))))))
;;                      ((name . "Subrestaurant 2")
;;                       (menus . (((name . "Menu Option 1")
;;                                  (menu . ("Some food" "More some food"))
;;                                  (prices . ("4,95" "10,20")))
;;                                 ((name . "Menu Option 2")
;;                                  (menu . ("Some other food"))
;;                                  (prices . ("4,95")))))))))
;;  ((name . "Restaurant 2")
;;   (subrestaurants . ((name . "Lunch")
;;                      (menus . (((name . "Menu Option 1")
;;                                 (menu . ("Some food" "More some food"))
;;                                 (prices . ("4,95" "10,20")))
;;                                ((name . "Menu Option 2")
;;                                 (menu . ("Some other food"))
;;                                 (prices . ("4,95")))))))))


;; Sample configuration (Hermia 5 restaurant):

;; (lunchtime-define-restaurant
;;  "https://www.sodexo.fi/ruokalistat/output/daily_json/134/%Y/%m/%d/en"
;;  `(((name . ,(assoc-recursive lunchtime-response-data 'meta 'ref_title))
;;     (subrestaurants
;;      .
;;      (((name . "Lounas") ;; Sodexo has only one restaurant per menu item
;;        (menus . ,(mapcar
;;                   (lambda (item)
;;                     `((name . ,(assoc-recursive item 'category))
;;                       (prices . (,(assoc-recursive item 'price)))
;;                       (menu . (,(assoc-recursive item 'title_fi)))))
;;                   (assoc-recursive lunchtime-response-data 'courses)))))))))

;;; Code:

(require 'request)

(defvar lunchtime-parsers-alist ()
  "Contains a list of urls and parsers for defined restaurants.")

(defvar *lunchtime-cache* (make-hash-table :test 'equal)
  "Cached data for all the restaurants. The data in cache is in parsed format.")

(defvar lunchtime--date-offset 0
  "Specifies how many days from today the menu is displayed.
Use 0 for today, -1 for yesterday, 1 for tomorrow etc.")

(defvar lunchtime--restaurant-offset 0
  "Used in folding N items when redisplaying.")


(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYS in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  (if (stringp alist)
      (decode-coding-string alist 'utf-8)
    alist))

(defmacro lunchtime-define-restaurant (url &rest body)
  "Define a new restaurant data stucture parser and introduce it to luchtime.

The restaurant will get its data from URL which is an HTTP url with string
formatting characters used in `format-date-string'. The parsing will be done in
BODY, where `lunchtime-response-data' will contain data received from the API."
  `(add-to-list 'lunchtime-parsers-alist
                (cons ,url
                      (lambda (lunchtime-response-data) ,@body))))


(defun lunchtime-get-menus (offset)
  "Return all menus on OFFSET days from now.

Data is fetched only if it is not in cache."
  (apply #'append
         (mapcar
          (lambda (r)
            (condition-case nil
                (let* ((url-template (car r))
                       (parser (lambda ()
                                 (funcall (cdr r)
                                          (let ((json-array-type 'list))
                                            (json-read)))))
                       (formatted-url (format-time-string
                                       url-template
                                       (time-add (current-time)
                                                 (days-to-time offset)))))

                  (or (gethash formatted-url *lunchtime-cache*)
                      (puthash formatted-url
                               (let ((response (request formatted-url
                                                        :sync t
                                                        :headers `(("Content-Type"
                                                                    . "application/json"))
                                                        :parser parser)))
                                 (unless (= (request-response-status-code response) 200)
                                   (error "Failed to fetch menu info"))
                                 (request-response-data response))
                               *lunchtime-cache*)))

                (error nil)))
          lunchtime-parsers-alist)))

(defun lunchtime--insert-title (restaurant)
  "Insert the name of RESTAURANT as a buffer-wide string."
  (let* ((item (concat "= " (assoc-recursive restaurant 'name) " "))
         (item-len (length item))
         (padding-len (- (window-total-width) 4 item-len)))
    (insert (concat item (make-string padding-len ?=) "=\n"))))


(defun lunchtime--insert-subtitle (subrestaurant)
  "Insert the name of SUBRESTAURANT."
  (let* ((item (concat "-- "  (assoc-recursive subrestaurant 'name) " "))
         (padding-len (- (window-total-width) 4 (length item))))
    (insert (concat item (make-string padding-len ?-) "-\n"))))


(defun lunchtime--insert-menu-title (meal)
  "Insert name of the MEAL."
  (let* ((item (concat " " (capitalize (downcase (or (assoc-recursive meal 'name)
                                                     "")))))
         (item-len (length item))
         (prices (mapconcat
                  (lambda (p) (format "%+5s" p))
                  (assoc-recursive meal 'prices) "  "))
         (prices-len (length prices))
         (padding-len (- (window-total-width) 4 prices-len item-len)))
    (insert (concat item (make-string padding-len ? ) prices "\n"))))

(defun lunchtime--insert-details (restaurant)
  "Insert the name of RESTAURANT and it's lunch details below it."
  (lunchtime--insert-title restaurant)
  (dolist (subrestaurant (assoc-recursive restaurant 'subrestaurants))
    (lunchtime--insert-subtitle subrestaurant)
    (dolist (menu (assoc-recursive subrestaurant 'menus))
      (lunchtime--insert-menu-title menu)
      (dolist (content (assoc-recursive menu 'menu))
        (insert (format "  - %s\n" content))))))

(defun lunchtime--insert-header ()
  "Insert topmost header of the buffer with current date."
  (insert (format-time-string "  LunchTime Menu - %A, %d. %B %Y\n\n"
                              (time-add (current-time)
                                        (days-to-time lunchtime--date-offset)))))

(defun lunchtime--redraw ()
  "Redraw the buffer, fetch data from remote if needed."
  (with-current-buffer (get-buffer-create "*LunchTime menu*")

    (let ((data (lunchtime-get-menus lunchtime--date-offset))
          (inhibit-read-only t))
      (erase-buffer)
      (when (< lunchtime--restaurant-offset 0)
        (setq lunchtime--restaurant-offset 0))

      (when (>= lunchtime--restaurant-offset (length data))
        (setq lunchtime--restaurant-offset (- (length data) 1)))

      (lunchtime--insert-header)
      ;; Every entry until restaurant-offset is inserted as 'folded'.
      (mapc 'lunchtime--insert-title
            (subseq data 0 lunchtime--restaurant-offset))
      ;; The rest are inserted completely.
      (mapc 'lunchtime--insert-details
            (subseq data lunchtime--restaurant-offset)))

    (let ((mode 'lunchtime-mode))
      (funcall mode))
    (beginning-of-buffer)

    (pop-to-buffer (current-buffer))))


;;;###autoload
(defun lunchtime-display-menus ()
  "Open a new buffer showing todays menu."
  (interactive)

  (setq lunchtime--date-offset 0)
  (setq lunchtime--restaurant-offset 0)

  (lunchtime--redraw))

(defun lunchtime-next-day ()
  "Move forwards one day."
  (interactive)
  (setq lunchtime--date-offset (+ lunchtime--date-offset 1))
  (lunchtime--redraw))

(defun lunchtime-previous-day ()
  "Move backwards one day."
  (interactive)
  (setq lunchtime--date-offset (- lunchtime--date-offset 1))
  (lunchtime--redraw))

(defun lunchtime-close ()
  "Close the buffer (and window)."
  (interactive)
  (kill-this-buffer)
  (unless (one-window-p t)
    (delete-window)))

(defun lunchtime-next-restaurant ()
  "Fold one more item."
  (interactive)
  (setq lunchtime--restaurant-offset (+ lunchtime--restaurant-offset 1))
  (lunchtime--redraw))

(defun lunchtime-previous-restaurant ()
  "Unfold one more item."
  (interactive)
  (setq lunchtime--restaurant-offset (- lunchtime--restaurant-offset 1))
  (lunchtime--redraw))

(defvar lunchtime-mode-map
  (let ((map (make-sparse-keymap))) map))

;;;###autoload
(define-derived-mode lunchtime-mode special-mode "LunchTime Menu"
  "Fetch Tampere Universiry of Technology restaurant menus."
  (local-set-key (kbd "l") 'lunchtime-next-day)
  (local-set-key (kbd "h") 'lunchtime-previous-day)
  (add-hook 'window-configuration-change-hook #'lunchtime--redraw t t))

(defun lunchtime--lunchtime-hook ()
  (setq imenu-generic-expression '((nil "^= \\(.+\\) ====+" 1)))
  (font-lock-add-keywords nil 
    '(("^  LunchTime Menu .*$" . font-lock-warning-face)
      ("^= .*" . font-lock-builtin-face)
      ("^-- .*" . font-lock-constant-face)
      ("^ \\(.*\\)       \\([$0-9][0-9, €]+[0-9] *[€]?\\)$"
       (1 font-lock-comment-face) (2 font-lock-string-face)))))
(add-hook 'lunchtime-mode-hook #'lunchtime--lunchtime-hook)


(provide 'lunchtime)
;;; lunchtime.el ends here
