;;; nepali-calendar-test.el --- Tests for the Nepali calendar -*- lexical-binding: t; -*-

;; Run: cd ~/.emacs.d && /Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch \
;;        -L modules/personal -l tests/nepali-calendar-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nepali-calendar)

(defun nepali-test--abs (month day year)
  (calendar-absolute-from-gregorian (list month day year)))

(ert-deftest nepali-new-years-fall-on-1-baisakh ()
  (should (equal (my/nepali-from-absolute (nepali-test--abs 4 14 2025)) '(2082 1 1)))
  (should (equal (my/nepali-from-absolute (nepali-test--abs 4 14 2026)) '(2083 1 1))))

(ert-deftest nepali-conversion-round-trips ()
  (dolist (abs (number-sequence (nepali-test--abs 1 1 2020) (nepali-test--abs 12 31 2030) 37))
    (should (= abs (my/nepali-to-absolute (my/nepali-from-absolute abs))))))

(ert-deftest nepali-tithi-is-the-one-at-sunrise ()
  ;; 4 Nov 2026, 18 Kartik 2083: Kartik Krishna Dashami until 11:19.
  (let ((p (my/nepali-panchang (nepali-test--abs 11 4 2026))))
    (should (= (plist-get p :tithi) 25))
    (should (= (plist-get p :month) 6))
    (should-not (plist-get p :adhik))))

(ert-deftest nepali-adhik-month-is-found ()
  ;; 2026 has Adhik Jestha, from the 17 May new moon to 15 June.
  (should (plist-get (my/nepali-panchang (nepali-test--abs 6 1 2026)) :adhik))
  (should-not (plist-get (my/nepali-panchang (nepali-test--abs 7 1 2026)) :adhik)))

(ert-deftest nepali-festivals-match-the-2083-holiday-list ()
  ;; Government holiday list for 2083 BS, as BS dates.
  (let ((festivals (my/nepali-festivals (my/nepali-to-absolute '(2083 1 1))
                                        (my/nepali-to-absolute '(2083 12 30)))))
    (pcase-dolist (`(,name ,bs) '(("Buddha Jayanti, Ubhauli" (2083 1 18))
                                  ("Janai Purnima"           (2083 5 12))
                                  ("Gai Jatra"               (2083 5 13))
                                  ("Krishna Janmashtami"     (2083 5 19))
                                  ("Haritalika Teej"         (2083 5 29))
                                  ("Indra Jatra"             (2083 6 9))
                                  ("Ghatasthapana"           (2083 6 25))
                                  ("Kaag Tihar"              (2083 7 21))
                                  ("Laxmi Puja"              (2083 7 22))
                                  ("Govardhan Puja"          (2083 7 24))
                                  ("Bhai Tika"               (2083 7 25))
                                  ("Chhath"                  (2083 7 29))
                                  ("Udhauli"                 (2083 9 9))
                                  ("Sonam Lhosar"            (2083 10 24))
                                  ("Basanta Panchami"        (2083 10 28))
                                  ("Maha Shivaratri"         (2083 11 22))
                                  ("Gyalpo Lhosar"           (2083 11 25))
                                  ("Holi (hills)"            (2083 12 7))
                                  ("Holi (Terai)"            (2083 12 8))
                                  ("Ghode Jatra"             (2083 12 23))))
      (should (equal (list name (my/nepali-from-absolute
                                 (car (rassoc name festivals))))
                     (list name bs))))))

(ert-deftest nepali-evening-festivals-follow-sunset ()
  ;; 2082: amavasya held both sunsets; Laxmi Puja took the second.
  ;; Chhath's shashthi began after sunrise on the 27th.
  (let ((festivals (my/nepali-festivals (nepali-test--abs 10 15 2025)
                                        (nepali-test--abs 10 31 2025))))
    (should (= (car (rassoc "Laxmi Puja" festivals)) (nepali-test--abs 10 21 2025)))
    (should (= (car (rassoc "Chhath" festivals)) (nepali-test--abs 10 27 2025)))))

(ert-deftest nepali-month-view-starts-on-today ()
  (with-temp-buffer
    (my/nepali-calendar-mode)
    (my/nepali-calendar-today)
    (should (= (get-text-property (point) 'my/nepali-day)
               (calendar-absolute-from-gregorian (calendar-current-date))))))

;;; nepali-calendar-test.el ends here
