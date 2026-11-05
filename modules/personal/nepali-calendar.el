;;; nepali-calendar.el --- Bikram Sambat dates, tithis and festivals -*- lexical-binding: t; -*-

;; The Nepali calendar the way a patro shows it: the Bikram Sambat date,
;; the tithi and lunar month, and the festivals.  Three parts, each from
;; the source that decides it in Nepal:
;;
;; - BS dates come from the published month-length table (1975-2100 BS,
;;   from the nepali-datetime package).  BS months are solar, 29 to 32
;;   days, and the official lengths are a committee's table, so they are
;;   looked up, not computed.
;; - Tithis and lunar months are computed: Meeus's lunar and solar
;;   longitudes, the tithi in force at sunrise in Kathmandu, the month
;;   named by the sidereal (Lahiri) sign the sun is in at the new moon
;;   that opens it, Adhik when no sankranti falls inside it.  Months are
;;   amanta inside; shown purnimanta, as Nepal names them, so the dark
;;   half before Laxmi Puja reads Kartik Krishna.
;; - Festivals are rules on those: a lunar month and tithi, or a fixed BS
;;   or AD date.  A festival falls on the day whose sunrise (or, for
;;   night festivals, midnight) the tithi holds; a tithi that holds no
;;   sunrise falls on the day it begins.  The panchang committee settles
;;   a few edge cases otherwise, so a festival can be a day off the
;;   printed patro in some year.
;;
;; C-c g n opens `my/nepali-calendar', a BS month grid with each day's
;; tithi and the month's festivals.  In M-x calendar, p n shows the
;; Nepali date at point, the echo follows the cursor, and the festivals
;; join the holidays (h, and the marks).  `my/nepali-diary-entry' is the
;; same line for a diary sexp, so %%(my/nepali-diary-entry) in an agenda
;; file puts it on each day of the Org agenda.

;;; Code:

(require 'calendar)
(require 'solar)
(require 'cl-lib)

(defvar displayed-month)
(defvar displayed-year)
(defvar calendar-holidays)

;;; Bikram Sambat dates

(defconst my/nepali--month-lengths
  '(
    (1975 31 31 32 32 31 30 30 29 30 29 30 30)
    (1976 31 32 31 32 31 30 30 30 29 29 30 31)
    (1977 30 32 31 32 31 30 30 30 29 30 29 31)
    (1978 31 31 32 31 31 31 30 29 30 29 30 30)
    (1979 31 31 32 32 31 30 30 29 30 29 30 30)
    (1980 31 32 31 32 31 30 30 30 29 29 30 31)
    (1981 31 31 31 32 31 31 29 30 30 29 30 30)
    (1982 31 31 32 31 31 31 30 29 30 29 30 30)
    (1983 31 31 32 32 31 30 30 29 30 29 30 30)
    (1984 31 32 31 32 31 30 30 30 29 29 30 31)
    (1985 31 31 31 32 31 31 29 30 30 29 30 30)
    (1986 31 31 32 31 31 31 30 29 30 29 30 30)
    (1987 31 32 31 32 31 30 30 29 30 29 30 30)
    (1988 31 32 31 32 31 30 30 30 29 29 30 31)
    (1989 31 31 31 32 31 31 29 30 30 29 30 30)
    (1990 31 31 32 31 31 31 30 29 30 29 30 30)
    (1991 31 32 31 32 31 30 30 30 29 29 30 30)
    (1992 31 32 31 32 31 30 30 30 29 30 29 31)
    (1993 31 31 32 31 31 31 30 29 30 29 30 30)
    (1994 31 31 32 31 31 31 30 29 30 29 30 30)
    (1995 31 32 31 32 31 30 30 30 29 29 30 30)
    (1996 31 32 31 32 31 30 30 30 29 30 29 31)
    (1997 31 31 32 31 31 31 30 29 30 29 30 30)
    (1998 31 31 32 31 31 31 30 29 30 29 30 30)
    (1999 31 32 31 32 31 30 30 30 29 29 30 31)
    (2000 30 32 31 32 31 30 30 30 29 30 29 31)
    (2001 31 31 32 31 31 31 30 29 30 29 30 30)
    (2002 31 31 32 32 31 30 30 29 30 29 30 30)
    (2003 31 32 31 32 31 30 30 30 29 29 30 31)
    (2004 30 32 31 32 31 30 30 30 29 30 29 31)
    (2005 31 31 32 31 31 31 30 29 30 29 30 30)
    (2006 31 31 32 32 31 30 30 29 30 29 30 30)
    (2007 31 32 31 32 31 30 30 30 29 29 30 31)
    (2008 31 31 31 32 31 31 29 30 30 29 29 31)
    (2009 31 31 32 31 31 31 30 29 30 29 30 30)
    (2010 31 31 32 32 31 30 30 29 30 29 30 30)
    (2011 31 32 31 32 31 30 30 30 29 29 30 31)
    (2012 31 31 31 32 31 31 29 30 30 29 30 30)
    (2013 31 31 32 31 31 31 30 29 30 29 30 30)
    (2014 31 31 32 32 31 30 30 29 30 29 30 30)
    (2015 31 32 31 32 31 30 30 30 29 29 30 31)
    (2016 31 31 31 32 31 31 29 30 30 29 30 30)
    (2017 31 31 32 31 31 31 30 29 30 29 30 30)
    (2018 31 32 31 32 31 30 30 29 30 29 30 30)
    (2019 31 32 31 32 31 30 30 30 29 30 29 31)
    (2020 31 31 31 32 31 31 30 29 30 29 30 30)
    (2021 31 31 32 31 31 31 30 29 30 29 30 30)
    (2022 31 32 31 32 31 30 30 30 29 29 30 30)
    (2023 31 32 31 32 31 30 30 30 29 30 29 31)
    (2024 31 31 31 32 31 31 30 29 30 29 30 30)
    (2025 31 31 32 31 31 31 30 29 30 29 30 30)
    (2026 31 32 31 32 31 30 30 30 29 29 30 31)
    (2027 30 32 31 32 31 30 30 30 29 30 29 31)
    (2028 31 31 32 31 31 31 30 29 30 29 30 30)
    (2029 31 31 32 31 32 30 30 29 30 29 30 30)
    (2030 31 32 31 32 31 30 30 30 29 29 30 31)
    (2031 30 32 31 32 31 30 30 30 29 30 29 31)
    (2032 31 31 32 31 31 31 30 29 30 29 30 30)
    (2033 31 31 32 32 31 30 30 29 30 29 30 30)
    (2034 31 32 31 32 31 30 30 30 29 29 30 31)
    (2035 30 32 31 32 31 31 29 30 30 29 29 31)
    (2036 31 31 32 31 31 31 30 29 30 29 30 30)
    (2037 31 31 32 32 31 30 30 29 30 29 30 30)
    (2038 31 32 31 32 31 30 30 30 29 29 30 31)
    (2039 31 31 31 32 31 31 29 30 30 29 30 30)
    (2040 31 31 32 31 31 31 30 29 30 29 30 30)
    (2041 31 31 32 32 31 30 30 29 30 29 30 30)
    (2042 31 32 31 32 31 30 30 30 29 29 30 31)
    (2043 31 31 31 32 31 31 29 30 30 29 30 30)
    (2044 31 31 32 31 31 31 30 29 30 29 30 30)
    (2045 31 32 31 32 31 30 30 29 30 29 30 30)
    (2046 31 32 31 32 31 30 30 30 29 29 30 31)
    (2047 31 31 31 32 31 31 30 29 30 29 30 30)
    (2048 31 31 32 31 31 31 30 29 30 29 30 30)
    (2049 31 32 31 32 31 30 30 30 29 29 30 30)
    (2050 31 32 31 32 31 30 30 30 29 30 29 31)
    (2051 31 31 31 32 31 31 30 29 30 29 30 30)
    (2052 31 31 32 31 31 31 30 29 30 29 30 30)
    (2053 31 32 31 32 31 30 30 30 29 29 30 30)
    (2054 31 32 31 32 31 30 30 30 29 30 29 31)
    (2055 31 31 32 31 31 31 30 29 30 29 30 30)
    (2056 31 31 32 31 32 30 30 29 30 29 30 30)
    (2057 31 32 31 32 31 30 30 30 29 29 30 31)
    (2058 30 32 31 32 31 30 30 30 29 30 29 31)
    (2059 31 31 32 31 31 31 30 29 30 29 30 30)
    (2060 31 31 32 32 31 30 30 29 30 29 30 30)
    (2061 31 32 31 32 31 30 30 30 29 29 30 31)
    (2062 31 31 31 32 31 31 29 30 29 30 29 31)
    (2063 31 31 32 31 31 31 30 29 30 29 30 30)
    (2064 31 31 32 32 31 30 30 29 30 29 30 30)
    (2065 31 32 31 32 31 30 30 30 29 29 30 31)
    (2066 31 31 31 32 31 31 29 30 30 29 29 31)
    (2067 31 31 32 31 31 31 30 29 30 29 30 30)
    (2068 31 31 32 32 31 30 30 29 30 29 30 30)
    (2069 31 32 31 32 31 30 30 30 29 29 30 31)
    (2070 31 31 31 32 31 31 29 30 30 29 30 30)
    (2071 31 31 32 31 31 31 30 29 30 29 30 30)
    (2072 31 32 31 32 31 30 30 29 30 29 30 30)
    (2073 31 32 31 32 31 30 30 30 29 29 30 31)
    (2074 31 31 31 32 31 31 30 29 30 29 30 30)
    (2075 31 31 32 31 31 31 30 29 30 29 30 30)
    (2076 31 32 31 32 31 30 30 30 29 29 30 30)
    (2077 31 32 31 32 31 30 30 30 29 30 29 31)
    (2078 31 31 31 32 31 31 30 29 30 29 30 30)
    (2079 31 31 32 31 31 31 30 29 30 29 30 30)
    (2080 31 32 31 32 31 30 30 30 29 29 30 30)
    (2081 31 32 31 32 31 30 30 30 29 30 29 31)
    (2082 31 31 32 31 31 31 30 29 30 29 30 30)
    (2083 31 31 32 31 31 31 30 29 30 29 30 30)
    (2084 31 31 32 31 31 30 30 30 29 30 30 30)
    (2085 31 32 31 32 30 31 30 30 29 30 30 30)
    (2086 30 32 31 32 31 30 30 30 29 30 30 30)
    (2087 31 31 32 31 31 31 30 29 30 30 30 30)
    (2088 30 31 32 32 30 31 30 30 29 30 30 30)
    (2089 30 32 31 32 31 30 30 30 29 30 30 30)
    (2090 30 32 31 32 31 30 30 30 29 30 30 30)
    (2091 31 31 32 31 31 31 30 30 29 30 30 30)
    (2092 30 31 32 32 31 30 30 30 29 30 30 30)
    (2093 30 32 31 32 31 30 30 30 29 30 30 30)
    (2094 31 31 32 31 31 30 30 30 29 30 30 30)
    (2095 31 31 32 31 31 31 30 29 30 30 30 30)
    (2096 30 31 32 32 31 30 30 29 30 29 30 30)
    (2097 31 32 31 32 31 30 30 30 29 30 30 30)
    (2098 31 31 32 31 31 31 29 30 29 30 29 31)
    (2099 31 31 32 31 31 31 30 29 29 30 30 30)
    (2100 31 32 31 32 30 31 30 29 30 29 30 30))
  "Days in each month of each BS year: (YEAR BAISAKH ... CHAITRA).")

(defconst my/nepali--epoch (calendar-absolute-from-gregorian '(4 13 1918))
  "Absolute date of 1 Baisakh 1975 BS, the table's first day.")

(defconst my/nepali-month-names
  ["Baisakh" "Jestha" "Asar" "Shrawan" "Bhadra" "Asoj"
   "Kartik" "Mangsir" "Poush" "Magh" "Falgun" "Chaitra"]
  "BS solar months.")

(defconst my/nepali-lunar-month-names
  ["Baisakh" "Jestha" "Asar" "Shrawan" "Bhadra" "Ashwin"
   "Kartik" "Mangsir" "Poush" "Magh" "Falgun" "Chaitra"]
  "Lunar months, indexed by the sidereal sign the sun is in when they open.")

(defconst my/nepali-tithi-names
  ["Pratipada" "Dwitiya" "Tritiya" "Chaturthi" "Panchami" "Shashthi"
   "Saptami" "Ashtami" "Navami" "Dashami" "Ekadashi" "Dwadashi"
   "Trayodashi" "Chaturdashi" "Purnima"]
  "Tithis of a paksha; the fifteenth of the dark half is Aunsi.")

(defun my/nepali--year-lengths (year)
  "Month lengths of BS YEAR, or an error outside the table."
  (or (cdr (assq year my/nepali--month-lengths))
      (user-error "BS %d is outside the table (1975-2100)" year)))

(defun my/nepali-to-absolute (date)
  "Absolute date of the BS DATE, a list (YEAR MONTH DAY)."
  (pcase-let ((`(,year ,month ,day) date))
    (+ my/nepali--epoch
       (cl-loop for (y . lengths) in my/nepali--month-lengths
                while (< y year) sum (apply #'+ lengths))
       (apply #'+ (seq-take (my/nepali--year-lengths year) (1- month)))
       (1- day))))

(defun my/nepali-from-absolute (abs)
  "BS date (YEAR MONTH DAY) of absolute date ABS, or nil outside the table."
  (let ((days (- abs my/nepali--epoch)))
    (when (>= days 0)
      (cl-loop for (year . lengths) in my/nepali--month-lengths
               for len = (apply #'+ lengths)
               if (< days len)
               return (cl-loop for l in lengths for month from 1
                               if (< days l) return (list year month (1+ days))
                               do (cl-decf days l))
               else do (cl-decf days len)))))

(defun my/nepali-month-length (year month)
  "Days in BS MONTH of YEAR."
  (nth (1- month) (my/nepali--year-lengths year)))

;;; Sun and moon
;;
;; Moments are absolute dates with a fraction of the day, in UT.

(defconst my/nepali--utc-offset (/ 5.75 24) "Nepal time ahead of UT, in days.")

(defun my/nepali--centuries (moment)
  "Julian centuries of dynamical time from J2000 to the UT MOMENT."
  (let* ((year (+ 2000 (/ (- moment 730120.5) 365.25)))
         (u (- year 2000))
         ;; Espenak and Meeus's Delta T fit for 2005-2050, in seconds.
         (delta-t (+ 62.92 (* 0.32217 u) (* 0.005589 u u))))
    (/ (- (+ moment (/ delta-t 86400.0)) 730120.5) 36525.0)))

(defsubst my/nepali--sin (deg) (sin (degrees-to-radians deg)))

(defun my/nepali--nutation (c)
  "Nutation in longitude at C centuries, to 0.5 arcseconds, in degrees."
  (let ((omega (- 125.04452 (* 1934.136261 c)))
        (sun (+ 280.4665 (* 36000.7698 c)))
        (moon (+ 218.3165 (* 481267.8813 c))))
    (/ (+ (* -17.20 (my/nepali--sin omega))
          (* -1.32 (my/nepali--sin (* 2 sun)))
          (* -0.23 (my/nepali--sin (* 2 moon)))
          (* 0.21 (my/nepali--sin (* 2 omega))))
       3600)))

(defun my/nepali--sun-longitude (moment)
  "Apparent tropical longitude of the sun at MOMENT, in degrees (Meeus 25)."
  (let* ((c (my/nepali--centuries moment))
         (l0 (+ 280.46646 (* 36000.76983 c) (* 0.0003032 c c)))
         (m (+ 357.52911 (* 35999.05029 c) (* -0.0001537 c c)))
         (center (+ (* (- 1.914602 (* 0.004817 c) (* 0.000014 c c))
                       (my/nepali--sin m))
                    (* (- 0.019993 (* 0.000101 c)) (my/nepali--sin (* 2 m)))
                    (* 0.000289 (my/nepali--sin (* 3 m))))))
    (mod (+ l0 center -0.00569 (my/nepali--nutation c)) 360)))

(defconst my/nepali--moon-terms
  '((0 0 1 0 6288774) (2 0 -1 0 1274027) (2 0 0 0 658314) (0 0 2 0 213618)
    (0 1 0 0 -185116) (0 0 0 2 -114332) (2 0 -2 0 58793) (2 -1 -1 0 57066)
    (2 0 1 0 53322) (2 -1 0 0 45758) (0 1 -1 0 -40923) (1 0 0 0 -34720)
    (0 1 1 0 -30383) (2 0 0 -2 15327) (0 0 1 2 -12528) (0 0 1 -2 10980)
    (4 0 -1 0 10675) (0 0 3 0 10034) (4 0 -2 0 8548) (2 1 -1 0 -7888)
    (2 1 0 0 -6766) (1 0 -1 0 -5163) (1 1 0 0 4987) (2 -1 1 0 4036)
    (2 0 2 0 3994) (4 0 0 0 3861) (2 0 -3 0 3665) (0 1 -2 0 -2689)
    (2 0 -1 2 -2602) (2 -1 -2 0 2390) (1 0 1 0 -2348) (2 -2 0 0 2236)
    (0 1 2 0 -2120) (0 2 0 0 -2069) (2 -2 -1 0 2048) (2 0 1 -2 -1773)
    (2 0 0 2 -1595) (4 -1 -1 0 1215) (0 0 2 2 -1110) (3 0 -1 0 -892)
    (2 1 1 0 -810) (4 -1 -2 0 759) (0 2 -1 0 -713) (2 2 -1 0 -700)
    (2 1 -2 0 691) (2 -1 0 -2 596) (4 0 1 0 549) (0 0 4 0 537)
    (4 -1 0 0 520) (1 0 -2 0 -487) (2 1 0 -2 -399) (0 0 2 -2 -381)
    (1 1 1 0 351) (3 0 -2 0 -340) (4 0 -3 0 330) (2 -1 2 0 327)
    (0 2 1 0 -323) (1 1 -1 0 299) (2 0 3 0 294))
  "Meeus table 47.A: multiples of D, M, M', F and the sine coefficient.")

(defun my/nepali--moon-longitude (moment)
  "Apparent longitude of the moon at MOMENT, in degrees (Meeus 47)."
  (let* ((c (my/nepali--centuries moment))
         (c2 (* c c)) (c3 (* c2 c)) (c4 (* c3 c))
         (lp (+ 218.3164477 (* 481267.88123421 c) (* -0.0015786 c2)
                (/ c3 538841) (/ c4 -65194000)))
         (d (+ 297.8501921 (* 445267.1114034 c) (* -0.0018819 c2)
               (/ c3 545868) (/ c4 -113065000)))
         (m (+ 357.5291092 (* 35999.0502909 c) (* -0.0001536 c2)
               (/ c3 24490000)))
         (mp (+ 134.9633964 (* 477198.8675055 c) (* 0.0087414 c2)
                (/ c3 69699) (/ c4 -14712000)))
         (f (+ 93.2720950 (* 483202.0175233 c) (* -0.0036539 c2)
               (/ c3 -3526000) (/ c4 863310000)))
         (e (- 1 (* 0.002516 c) (* 0.0000074 c2)))
         (a1 (+ 119.75 (* 131.849 c)))
         (a2 (+ 53.09 (* 479264.290 c)))
         (sum (+ (cl-loop for (kd km kmp kf coeff) in my/nepali--moon-terms
                          sum (* coeff (expt e (abs km))
                                 (my/nepali--sin (+ (* kd d) (* km m)
                                                    (* kmp mp) (* kf f)))))
                 (* 3958 (my/nepali--sin a1))
                 (* 1962 (my/nepali--sin (- lp f)))
                 (* 318 (my/nepali--sin a2)))))
    (mod (+ lp (/ sum 1e6) (my/nepali--nutation c)) 360)))

(defun my/nepali--elongation (moment)
  "Degrees the moon is ahead of the sun at MOMENT, 0 to 360."
  (mod (- (my/nepali--moon-longitude moment) (my/nepali--sun-longitude moment))
       360))

(defun my/nepali--elongation-moment (target guess)
  "Moment near GUESS when the elongation reaches TARGET degrees."
  (let ((moment guess))
    (dotimes (_ 10 moment)
      (cl-incf moment (/ (- (mod (+ (- target (my/nepali--elongation moment))
                                    180)
                                 360)
                            180)
                         12.19)))))

(defun my/nepali--ayanamsa (moment)
  "Lahiri ayanamsa at MOMENT, in degrees."
  (+ 23.857 (* 1.39697 (my/nepali--centuries moment))))

(defun my/nepali--sign (moment)
  "Sidereal sign the sun is in at MOMENT, 0 (Mesh) to 11 (Meen)."
  (floor (mod (- (my/nepali--sun-longitude moment) (my/nepali--ayanamsa moment))
              360)
         30))

;;; Lunar months and tithis

(defvar my/nepali--months nil
  "Cache of computed lunar months, (START END INDEX ADHIK).")

(defun my/nepali--new-moon-before (moment)
  "Moment of the last new moon at or before MOMENT."
  (let ((nm (my/nepali--elongation-moment
             0 (- moment (/ (my/nepali--elongation moment) 12.19)))))
    (if (> nm moment)
        (my/nepali--elongation-moment 0 (- nm 29.53))
      nm)))

(defun my/nepali--lunar-month (moment)
  "The amanta month holding MOMENT, as (START END INDEX ADHIK).
INDEX is 0 for Baisakh; ADHIK is non-nil when no sankranti falls in it."
  (or (cl-find-if (lambda (month) (and (<= (car month) moment)
                                       (< moment (cadr month))))
                  my/nepali--months)
      (let* ((start (my/nepali--new-moon-before moment))
             (end (my/nepali--elongation-moment 0 (+ start 29.53)))
             (sign (my/nepali--sign start))
             (month (list start end sign (= sign (my/nepali--sign end)))))
        (push month my/nepali--months)
        month)))

(defun my/nepali--sun-event (abs event)
  "Moment of EVENT, `sunrise' or `sunset', in Kathmandu on the civil day ABS."
  (let* ((calendar-latitude 27.7172)
         (calendar-longitude 85.324)
         (calendar-time-zone 345)
         (calendar-standard-time-zone-name "NPT")
         (calendar-daylight-savings-starts nil)
         (calendar-daylight-savings-ends nil)
         (times (solar-sunrise-sunset (calendar-gregorian-from-absolute abs)))
         (hours (car (if (eq event 'sunset) (cadr times) (car times)))))
    (- (+ abs (/ (or hours (if (eq event 'sunset) 18.0 6.0)) 24))
       my/nepali--utc-offset)))

(defun my/nepali--sunrise (abs)
  "Moment of sunrise in Kathmandu on the Nepal civil day ABS."
  (my/nepali--sun-event abs 'sunrise))

(defun my/nepali--local-day (moment)
  "Nepal civil day (absolute date) holding the UT MOMENT."
  (floor (+ moment my/nepali--utc-offset)))

(defun my/nepali--clock (moment)
  "Nepal time of MOMENT as HH:MM."
  (let ((minutes (round (* 1440 (- (+ moment my/nepali--utc-offset)
                                   (my/nepali--local-day moment))))))
    (format "%02d:%02d" (/ minutes 60) (% minutes 60))))

(defun my/nepali-panchang (abs)
  "Lunar day of absolute date ABS, at sunrise in Kathmandu, as a plist.
:tithi 1-30 (16-30 the dark half), :month the purnimanta month index,
:adhik, and :ends, the moment the tithi ends."
  (let* ((sunrise (my/nepali--sunrise abs))
         (tithi (1+ (floor (my/nepali--elongation sunrise) 12)))
         (month (my/nepali--lunar-month sunrise)))
    (list :tithi tithi
          :month (if (and (> tithi 15) (not (nth 3 month)))
                     (% (1+ (nth 2 month)) 12)
                   (nth 2 month))
          :adhik (nth 3 month)
          :ends (my/nepali--elongation-moment (* 12 tithi) (+ sunrise 0.5)))))

(defun my/nepali-tithi-name (tithi)
  "Name of TITHI (1-30) with its paksha, e.g. Krishna Navami."
  (format "%s %s" (if (> tithi 15) "Krishna" "Shukla")
          (if (= tithi 30) "Aunsi"
            (aref my/nepali-tithi-names (1- (if (> tithi 15) (- tithi 15) tithi))))))

(defun my/nepali-tithi-short (tithi)
  "Name of TITHI alone, the paksha left to the reader."
  (cond ((= tithi 30) "Aunsi")
        (t (aref my/nepali-tithi-names (1- (if (> tithi 15) (- tithi 15) tithi))))))

;;; Festivals

(defvar my/nepali-lunar-festivals
  '(("Ram Navami"            11  9)
    ("Mata Tirtha Aunsi"     11 30)
    ("Buddha Jayanti, Ubhauli" 0 15)
    ("Guru Purnima"           2 15)
    ("Nag Panchami"           3  5)
    ("Janai Purnima"          3 15)
    ("Gai Jatra"              3 16)
    ("Krishna Janmashtami"    3 23)
    ("Kushe Aunsi"            3 30)
    ("Haritalika Teej"        4  3)
    ("Rishi Panchami"         4  5)
    ("Indra Jatra"            4 14)
    ("Ghatasthapana"          5  1)
    ("Fulpati"                5  7)
    ("Maha Ashtami"           5  8)
    ("Maha Navami"            5  9)
    ("Vijaya Dashami"         5 10)
    ("Kojagrat Purnima"       5 15)
    ("Kaag Tihar"             5 28)
    ("Kukur Tihar"            5 29)
    ("Laxmi Puja"             5 30 sunset)
    ("Govardhan Puja"         6  1)
    ("Bhai Tika"              6  2)
    ("Chhath"                 6  6 sunset)
    ("Udhauli"                7 15)
    ("Sonam Lhosar"           9  1)
    ("Basanta Panchami"       9  5)
    ("Maha Shivaratri"        9 29 midnight)
    ("Gyalpo Lhosar"         10  1)
    ("Holi (hills)"          10 15 sunset)
    ("Holi (Terai)"          10 15)
    ("Ghode Jatra"           10 30))
  "Lunar festivals: (NAME MONTH TITHI [WHEN]).
MONTH is the amanta month, 0 Baisakh to 11 Chaitra, so a dark-half
festival sits in the month before the one Nepal names it by: Laxmi
Puja, Kartik Krishna Aunsi, is Ashwin 30 here.  WHEN is `sunrise'
(nil) for the day whose sunrise the tithi holds, `sunset' for the last
day whose sunset it holds (evening pujas), `midnight' for the night it
holds.  The Lhosars follow the Tibetan calendar, whose leap months
differ, so in some years they land a month off.")

(defvar my/nepali-solar-festivals
  '(("Nepali New Year"       1  1)
    ("Republic Day"          2 15)
    ("Saune Sankranti"       4  1)
    ("Constitution Day"      6  3)
    ("Tamu Lhosar"           9 15)
    ("Prithvi Jayanti"       9 27)
    ("Maghe Sankranti"      10  1)
    ("Martyrs' Day"         10 16)
    ("Democracy Day"        11  7))
  "Festivals on fixed BS dates: (NAME MONTH DAY), MONTH 1 for Baisakh.")

(defvar my/nepali-gregorian-festivals
  '(("Women's Day" 3 8)
    ("Christmas"  12 25))
  "Public holidays on fixed AD dates: (NAME MONTH DAY).")

(defun my/nepali--festival-day (start end when)
  "Civil day of the tithi from START to END, kept at WHEN.
See `my/nepali-lunar-festivals'.  A tithi that holds no sunrise (or
sunset, or midnight) falls on the day it begins."
  (let* ((first (my/nepali--local-day start))
         (days (cl-loop for day from first to (1+ first)
                        for ref = (pcase when
                                    ('midnight (- (1+ day) my/nepali--utc-offset))
                                    ('sunset (my/nepali--sun-event day 'sunset))
                                    (_ (my/nepali--sunrise day)))
                        when (and (<= start ref) (< ref end)) collect day)))
    (cond ((null days) first)
          ((eq when 'sunset) (car (last days)))
          (t (car days)))))

(defun my/nepali-festivals (from to)
  "Festivals on absolute dates FROM to TO, as a list of (ABS . NAME)."
  (let (result)
    (let ((moment (- from 30)))
      (while (< moment (+ to 1))
        (pcase-let ((`(,start ,end ,index ,adhik)
                     (my/nepali--lunar-month moment)))
          (unless adhik
            (dolist (festival my/nepali-lunar-festivals)
              (seq-let (name month tithi when) festival
               (when (= month index)
                (let* ((guess (+ start (* (1- tithi) (/ (- end start) 30))))
                       (day (my/nepali--festival-day
                             (my/nepali--elongation-moment (* 12 (1- tithi)) guess)
                             (if (= tithi 30) end
                               (my/nepali--elongation-moment (* 12 tithi)
                                                             (+ guess 1)))
                             when)))
                  (when (<= from day to) (push (cons day name) result)))))))
          (setq moment (+ end 0.01)))))
    (cl-loop for day from from to to
             for bs = (my/nepali-from-absolute day)
             for (_ month dom) = (calendar-gregorian-from-absolute day)
             do (pcase-dolist (`(,name ,m ,d) my/nepali-solar-festivals)
                  (when (equal (cdr bs) (list m d)) (push (cons day name) result)))
             do (pcase-dolist (`(,name ,m ,d) my/nepali-gregorian-festivals)
                  (when (and (= month m) (= dom d)) (push (cons day name) result))))
    (sort result (lambda (a b) (< (car a) (car b))))))

;;; Text

(defun my/nepali-date-string (abs)
  "One line for absolute date ABS: BS date, tithi, lunar month, festivals."
  (let* ((bs (my/nepali-from-absolute abs))
         (p (my/nepali-panchang abs))
         (festivals (mapcar #'cdr (my/nepali-festivals abs abs))))
    (concat
     (if bs
         (pcase-let ((`(,y ,m ,d) bs))
           (format "%d %s %d BS" d (aref my/nepali-month-names (1- m)) y))
       "Outside the BS table")
     (format " · %s%s %s (until %s)"
             (if (plist-get p :adhik) "Adhik " "")
             (aref my/nepali-lunar-month-names (plist-get p :month))
             (my/nepali-tithi-name (plist-get p :tithi))
             (my/nepali--clock (plist-get p :ends)))
     (if festivals (concat " · " (string-join festivals ", ")) ""))))

(defun my/nepali-calendar-print-date ()
  "Show the Nepali date of the date under the cursor in the calendar."
  (interactive)
  (message "%s" (my/nepali-date-string
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t)))))

(defun my/nepali-calendar-echo ()
  "Echo the Nepali date as the calendar cursor moves."
  (when-let ((date (calendar-cursor-to-date)))
    (message "%s" (my/nepali-date-string
                   (calendar-absolute-from-gregorian date)))))

(defun my/nepali-calendar-holidays ()
  "The Nepali festivals in the calendar's three displayed months.
An entry for `calendar-holidays'."
  (let ((m1 displayed-month) (y1 displayed-year)
        (m2 displayed-month) (y2 displayed-year))
    (calendar-increment-month m1 y1 -1)
    (calendar-increment-month m2 y2 1)
    (mapcar (lambda (festival)
              (list (calendar-gregorian-from-absolute (car festival))
                    (cdr festival)))
            (my/nepali-festivals
             (calendar-absolute-from-gregorian (list m1 1 y1))
             (calendar-absolute-from-gregorian
              (list m2 (calendar-last-day-of-month m2 y2) y2))))))

(defun my/nepali-diary-entry ()
  "The Nepali date line for a diary sexp: %%(my/nepali-diary-entry)."
  (with-no-warnings
    (my/nepali-date-string (calendar-absolute-from-gregorian date))))

(with-eval-after-load 'holidays
  (add-to-list 'calendar-holidays '(my/nepali-calendar-holidays) t))

(with-eval-after-load 'calendar
  (keymap-set calendar-mode-map "p n" #'my/nepali-calendar-print-date)
  (add-hook 'calendar-move-hook #'my/nepali-calendar-echo))

;;; Month view

(defvar-local my/nepali--shown nil "BS (YEAR MONTH) the month view shows.")

(defconst my/nepali--cell-width 12)

(defun my/nepali--cell (abs festival-days today)
  "The two lines of ABS's cell in the month grid."
  (let* ((bs (my/nepali-from-absolute abs))
         (tithi (plist-get (my/nepali-panchang abs) :tithi))
         (greg (calendar-gregorian-from-absolute abs))
         (red (or (= (calendar-day-of-week greg) 6) (memq abs festival-days)))
         (num (propertize (format "%-3d" (nth 2 bs))
                          'face (cond ((= abs today) '(:inherit (calendar-today bold)))
                                      (red '(:inherit (holiday bold)))
                                      (t 'bold))))
         (ad (propertize (format "%s %d"
                                 (calendar-month-name (car greg) t) (cadr greg))
                         'face 'shadow))
         (tname (my/nepali-tithi-short tithi)))
    (mapcar (lambda (s)
              (propertize (truncate-string-to-width s my/nepali--cell-width nil ?\s)
                          'my/nepali-day abs))
            (list (concat num (make-string (max 1 (- my/nepali--cell-width 4
                                                     (length ad)))
                                           ?\s)
                          ad)
                  (propertize tname 'face
                              (if (memq tithi '(15 30))
                                  'font-lock-keyword-face
                                'font-lock-comment-face))))))

(defun my/nepali--render ()
  "Draw the month in `my/nepali--shown'."
  (pcase-let* ((`(,year ,month) my/nepali--shown)
               (first (my/nepali-to-absolute (list year month 1)))
               (last (+ first (my/nepali-month-length year month) -1))
               (festivals (my/nepali-festivals first last))
               (festival-days (mapcar #'car festivals))
               (today (calendar-absolute-from-gregorian (calendar-current-date)))
               (inhibit-read-only t)
               (g1 (calendar-gregorian-from-absolute first))
               (g2 (calendar-gregorian-from-absolute last))
               (p1 (my/nepali-panchang first))
               (p2 (my/nepali-panchang last)))
    (erase-buffer)
    (insert (propertize (format "%s %d" (aref my/nepali-month-names (1- month)) year)
                        'face '(:inherit bold :height 1.3))
            (propertize (format "   %s %d – %s %d, %d"
                                (calendar-month-name (car g1) t) (cadr g1)
                                (calendar-month-name (car g2) t) (cadr g2)
                                (nth 2 g2))
                        'face 'shadow)
            "\n"
            (propertize (format "%s %s – %s %s\n\n"
                                (aref my/nepali-lunar-month-names (plist-get p1 :month))
                                (if (> (plist-get p1 :tithi) 15) "Krishna" "Shukla")
                                (aref my/nepali-lunar-month-names (plist-get p2 :month))
                                (if (> (plist-get p2 :tithi) 15) "Krishna" "Shukla"))
                        'face 'shadow))
    (dolist (name '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
      (insert (propertize (truncate-string-to-width name my/nepali--cell-width nil ?\s)
                          'face (if (equal name "Sat") 'holiday 'shadow))))
    (insert "\n")
    (let ((day (- first (calendar-day-of-week (calendar-gregorian-from-absolute first)))))
      (while (<= day last)
        (let ((cells (cl-loop for d from day below (+ day 7)
                              collect (if (<= first d last)
                                          (my/nepali--cell d festival-days today)
                                        (let ((blank (make-string my/nepali--cell-width ?\s)))
                                          (list blank blank))))))
          (insert (mapconcat #'car cells) "\n" (mapconcat #'cadr cells) "\n\n"))
        (cl-incf day 7)))
    (when festivals
      (insert (propertize "Festivals\n" 'face 'bold))
      (pcase-dolist (`(,abs . ,name) festivals)
        (let ((g (calendar-gregorian-from-absolute abs)))
          (insert (propertize
                   (format "  %2d  %s %2d  %s  %s\n"
                           (nth 2 (my/nepali-from-absolute abs))
                           (calendar-month-name (car g) t) (cadr g)
                           (calendar-day-name g t)
                           (propertize name 'face 'holiday))
                   'my/nepali-day abs)))))
    (goto-char (point-min))
    (when-let ((pos (text-property-any (point-min) (point-max) 'my/nepali-day
                                       (if (<= first today last) today first))))
      (goto-char pos))))

(defun my/nepali-calendar-forward (n)
  "Show the BS month N months on."
  (interactive "p")
  (pcase-let* ((`(,year ,month) my/nepali--shown)
               (index (+ (* 12 year) (1- month) n)))
    (setq my/nepali--shown (list (/ index 12) (1+ (% index 12))))
    (my/nepali--render)))

(defun my/nepali-calendar-backward (n)
  "Show the BS month N months back."
  (interactive "p")
  (my/nepali-calendar-forward (- n)))

(defun my/nepali-calendar-today ()
  "Show this BS month."
  (interactive)
  (setq my/nepali--shown
        (seq-take (my/nepali-from-absolute
                   (calendar-absolute-from-gregorian (calendar-current-date)))
                  2))
  (my/nepali--render))

(defun my/nepali-calendar-describe ()
  "Show the full Nepali date of the day at point."
  (interactive)
  (if-let ((abs (get-text-property (point) 'my/nepali-day)))
      (message "%s · %s" (my/nepali-date-string abs)
               (calendar-date-string (calendar-gregorian-from-absolute abs)))
    (user-error "No day at point")))

(defvar-keymap my/nepali-calendar-mode-map
  "n" #'my/nepali-calendar-forward
  "p" #'my/nepali-calendar-backward
  ">" #'my/nepali-calendar-forward
  "<" #'my/nepali-calendar-backward
  "." #'my/nepali-calendar-today
  "t" #'my/nepali-calendar-today
  "RET" #'my/nepali-calendar-describe)

(define-derived-mode my/nepali-calendar-mode special-mode "Patro"
  "BS month view: n/p months, t today, RET the day at point."
  (setq truncate-lines t)
  (add-hook 'post-command-hook
            (lambda () (when (get-text-property (point) 'my/nepali-day)
                         (my/nepali-calendar-describe)))
            nil t))

(defun my/nepali-calendar ()
  "Open the Nepali calendar on this BS month."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*Nepali Calendar*"))
  (my/nepali-calendar-mode)
  (my/nepali-calendar-today))

(provide 'nepali-calendar)
;;; nepali-calendar.el ends here
