;;; Package: date-calc.lisp
;;; Heiko Schroeter, Dec 2005
;;;
;;; Ver 0.2 ALPHA
;;; License: GNU, Version 2, June 1991
;;;
;;; Legal issues:
;;; -------------
;;; This package with all its parts is
;;; Copyright (c) 2005 by Heiko Schroeter.

;;; This package is free software; you can use, modify and redistribute
;;; under the "GNU General Public License" and the "Artistic License".

;;; This package is intended as a date-calc module for "everyday" purposes. It is not intended
;;; , nor claims to be,
;;; a bullet proofed implementation of 'scientific' datum calculus.

;;; Parts taken from DateCalc.el (EMACS, Doug Alcorn, <doug@lathi.net>, Ver. 0.1, 2003)
;;; and the
;;; Perl Package "Date::Calc" Version 5.4,Copyright (c) 1995 - 2004 by Steffen Beyer.

;;; Some Documentation strings are only slightly edited from DateCalc.el

;;; THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
;;; WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

;;; The following routines are a sidestep for CL Day Of Week (DOW) conformance.
;;; (See Hyperspec 25.1.4.1 X3J13).
;;; "An integer between 0 and 6, inclusive; 0 means Monday, 1 means Tuesday, and so on; 6 means Sunday."
;;; PERLs Date::Calc module range is from 1(Monday) to 7(Sunday).


;;;  CL conform                     ,Perl' Conform
;;;  ----------                     --------------
;;;  cl-day-of-week                 day-of-week
;;;  cl-weeks-in-year               weeks-in-year
;;;  cl-check-business-p            check-business-p
;;;  cl-nth-weekday-of-month-year   nth-weekday-of-month-year
;;;  cl-standard-to-business        standard-to-business
;;;  cl-business-to-standard        business-to-standard
;;;  cl-system-clock                system-clock
;;;  cl-decode-day-of-week          decode-day-of-week

;;; Pls report bugs to schroete @ iup physik uni-bremen de


(in-package #:cl-user)

(defpackage #:date-calc
  (:use #:cl)
  (:export #:*language*
	   #:decode-day-of-week
	   #:cl-decode-day-of-week
           #:decode-month
	   #:decode-language
	   #:iso-lc
	   #:iso-uc
	   #:year-to-days
	   #:fixed-window
	   #:center
	   #:valid-year-p
	   #:valid-month-p
	   #:leap-year
	   #:leap-year-p
	   #:days-in-month
	   #:days-in-year
	   #:check-date
	   #:check-business-p
	   #:check-time-p
	   #:day-of-year
	   #:date-to-days
	   #:day-of-week
	   #:weeks-in-year
	   #:delta-days
	   #:week-number
	   #:week-of-year
	   #:add-delta-days
	   #:monday-of-week
	   #:nth-weekday-of-month-year
	   #:standard-to-business
	   #:business-to-standard
	   #:delta-hms
	   #:delta-dhms
	   #:delta-ymd
	   #:delta-ymdhms
	   #:normalize-dhms
	   #:add-delta-dhms
	   #:add-year-month
	   #:add-delta-ym
	   #:add-delta-ymd
	   #:add-delta-ymdhms
	   #:system-clock
	   #:cl-system-clock
	   #:gmtime
	   #:localtime
	   #:today
	   #:yesterday
	   #:tomorrow
	   #:now
	   #:today-and-now
	   #:this-year
	   #:date-to-text
	   #:date-to-text-long
	   #:cl-day-of-week
	   #:cl-weeks-in-year
	   #:cl-check-business-p
	   #:cl-nth-weekday-of-month-year
	   #:cl-standard-to-business
	   #:cl-business-to-standard))

(pushnew :date-calc *features*)
(in-package #:date-calc)

;;;; Parameters
(defparameter year-of-epoc 70 "Year of reference (epoc)")
(defparameter century-of-epoc 1900 "Century of reference (epoc)")
(defparameter eopoc (+ year-of-epoc century-of-epoc) "reference year (epoc)")

(defparameter days-in-year-arr (make-array '(2 13) :initial-contents
						     '((0 31 59 90 120 151 181 212 243 273 304 334 365)
						       (0 31 60 91 121 152 182 213 244 274 305 335 366))))

(defparameter days-in-month-arr (make-array '(2 13) :initial-contents
						      '((0 31 28 31 30 31 30 31 31 30 31 30 31)
							(0 31 29 31 30 31 30 31 31 30 31 30 31))))

(defparameter languages 11)
(defparameter *language* 1) ; Default English

;; (defconstant num-of-lingos (1+ languages))

(defparameter month-to-text (make-hash-table))
(setf (gethash 0 month-to-text)
      #("???" "???" "???" "???"
	"???" "???" "???" "???"
	"???" "???" "???" "???" "???"))
(setf (gethash 1 month-to-text)
      #("???" "January" "February" "March"
	"April" "May" "June" "July" "August"
	"September" "October" "November" "December"))
(setf (gethash 2 month-to-text)
      #("???" "janvier" "fevrier" "mars"
	"avril" "mai" "juin" "juillet" "aout"
	"septembre" "octobre" "novembre" "decembre"))
(setf (gethash 3 month-to-text)
      #("???" "Januar" "Februar" "Maerz"
	"April" "Mai" "Juni" "Juli" "August"
	"September" "Oktober" "November" "Dezember"))
(setf (gethash 4 month-to-text)
      #("???" "enero" "febrero" "marzo"
	"abril" "mayo" "junio" "julio" "agosto"
	"septiembre" "octubre" "noviembre" "diciembre"))
(setf (gethash 5 month-to-text)
      #("???" "janeiro" "fevereiro" "marco"
	"abril" "maio" "junho" "julho" "agosto"
	"setembro" "outubro" "novembro" "dezembro"))
(setf (gethash 6 month-to-text)
      #("???" "januari" "februari" "maart"
	"april" "mei" "juni" "juli" "augustus"
	"september" "october" "november" "december"))
(setf (gethash 7 month-to-text)
      #("???" "Gennaio" "Febbraio" "Marzo"
	"Aprile" "Maggio" "Giugno" "Luglio" "Agosto"
	"Settembre" "Ottobre" "Novembre" "Dicembre"))
(setf (gethash 8 month-to-text)
      #("???" "januar" "februar" "mars"
	"april" "mai" "juni" "juli" "august"
	"september" "oktober" "november" "desember"))
(setf (gethash 9 month-to-text)
      #("???" "januari" "februari" "mars"
	"april" "maj" "juni" "juli" "augusti"
	"september" "oktober" "november" "december"))
(setf (gethash 10 month-to-text)
      #("???" "januar" "februar" "marts"
	"april" "maj" "juni" "juli" "august"
	"september" "oktober" "november" "december"))
(setf (gethash 11 month-to-text)
      #("???" "tammikuu" "helmikuu" "maaliskuu"
	"huhtikuu" "toukokuu" "kesaekuu" "heinaekuu"
	"elokuu" "syyskuu" "lokakuu" "marraskuu" "joulukuu"))

(defparameter day-of-week-to-text (make-hash-table))
(setf (gethash 0 day-of-week-to-text)
      #("???" "???" "???" "???" "???" "???" "???" "???"))
(setf (gethash 1 day-of-week-to-text)
      #("???" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(setf (gethash 2 day-of-week-to-text)
      #("???" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi" "Dimanche"))
(setf (gethash 3 day-of-week-to-text)
      #("???" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag" "Sonntag"))
(setf (gethash 4 day-of-week-to-text)
      #("???" "Lunes" "Martes" "Miercoles" "Jueves" "Viernes" "Sabado" "Domingo"))
(setf (gethash 5 day-of-week-to-text)
      #("???" "Segunda-feira" "Terca-feira" "Quarta-feira" "Quinta-feira" "Sexta-feira" "Sabado" "Domingo"))
(setf (gethash 6 day-of-week-to-text)
      #("???" "Maandag" "Dinsdag" "Woensdag" "Donderdag" "Vrijdag" "Zaterdag" "Zondag"))
(setf (gethash 7 day-of-week-to-text)
      #("???" "Lunedi" "Martedi" "Mercoledi" "Giovedi" "Venerdi" "Sabato" "Domenica"))
(setf (gethash 8 day-of-week-to-text)
      #("???" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "loerdag" "soendag"))
(setf (gethash 9 day-of-week-to-text)
      #("???" "mandag" "tisdag" "onsdag" "torsdag" "fredag" "loerdag" "soendag"))
(setf (gethash 10 day-of-week-to-text)
      #("???" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "loerdag" "soendag"))
(setf (gethash 11 day-of-week-to-text)
      #("???" "maanantai" "tiistai" "keskiviikko" "torstai" "perjantai" "lauantai" "sunnuntai"))

(defparameter day-of-week-abbreviation (make-hash-table))
(setf (gethash  0 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  1 day-of-week-abbreviation) #("??" "Mon" "Tue" "Wen" "Thu" "Fri" "Sat" "Sun"))
(setf (gethash  2 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  3 day-of-week-abbreviation) #("??" "Mo" "Di" "Mi" "Do" "Fr" "Sa" "So"))
(setf (gethash  4 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  5 day-of-week-abbreviation) #("???" "2" "3" "4" "5" "6" "Sam" "Dom"))
(setf (gethash  6 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  7 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  8 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash  9 day-of-week-abbreviation) #("??" "Mo" "Ti" "On" "To" "Fr" "Lo" "So"))
(setf (gethash 10 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))
(setf (gethash 11 day-of-week-abbreviation) #("" "" "" "" "" "" "" ""))

(defparameter long-format (make-array '(12) :initial-contents
  '(("~A, ~A ~A ~A" 10)                     ;   0  Default, the second value describes order:
    ("~A, ~A ~A ~A" 10)                     ;   1  English    11=DMY 10=MDY see #'date-to-text-long     
    ("~A ~A ~A ~A"  10)                     ;   2  Francais    
    ("~A, den ~A ~A ~A" 11)                 ;   3  Deutsch     
    ("~A, ~A de ~A de ~A" 10)               ;   4  Espanol     
    ("~A, dia ~A de ~A de ~A" 10)           ;   5  Portugues   
    ("~A, ~A ~A ~A" 10)                     ;   6  Nederlands  
    ("~A, ~A ~A ~A" 10)                     ;   7  Italiano    
    ("~A, ~A. ~A ~A" 10)                    ;   8  Norsk       
    ("~A, ~A ~A ~A" 10)                     ;   9  Svenska     
    ("~A, ~A. ~A ~A" 10)                    ;  10  Dansk       
    ("~A, ~A. ~A ta ~A" 10))))              ;  11  suomi       

(defparameter language-to-text
  (vector "???" "English" "Francais" "Deutsch" "Espanol"
	  "Portugues" "Nederlands" "Italiano" "Norsk"
	  "Svenska" "Dansk" "suomi"))

;;;; Functions
(defun decode-day-of-week (str)
  "Returns number of weekday. STR can partially name the Weekday. DOW is not CL conform."
  (let ((week-vector (gethash *language* day-of-week-to-text))
	(i 0))
    (loop for weekday across week-vector
	  until (search str weekday :test #'char-equal)
	  do (incf i)
	  finally (return (if (<= i 7) i nil)))))

(defun cl-decode-day-of-week (str)
  "Returns number of weekday. STR can partially name the Weekday. DOW is CL conform."
  (let ((week-vector (gethash *language* day-of-week-to-text))
	(i 0))
    (loop for weekday across week-vector
	  until (search str weekday :test #'char-equal)
	  do (incf i)
	  finally (return (if (<= i 7) (1- i) nil)))))

(defun decode-month (str)
  "Returns number of month. STR can partially name the month. Computes a (search ...:test #'char-equal)."
  (let ((month-vector (gethash *language* month-to-text))
	(i 0))
    (loop for month across month-vector
	  until (search str month :test #'char-equal)
	  do (incf i)
	  finally (return (if (<= i 12) i nil)))))

(defun decode-language (num)
  "Returns the Language of number NUM."
  (svref language-to-text num))

(defun iso-lc (char)
  "Returns lower case CHAR."
  (char-downcase char))

(defun iso-uc (char)
  "Returns upper case CHAR."
  (char-upcase char))

(defun year-to-days (year)
  "Returns the number of days for YEAR since 1 Jan 1."
  (+ (- (+ (* year 365) (ash year -2))
	(floor (/ (ash year -2) 25)))
     (ash (floor (/ (ash year -2) 25)) -2)))

(defun fixed-window (year)
  "Convert two digit YEAR to four digit YEAR; YEAR<=70 -> 2000+YEAR; YEAR<100&&>70  -> 1900+YEAR."
  (if (and (> year 70) (< year 100))
      (+ 1900 year)
      (+ 2000 year)))

(defun center (string width)
  "Return a string that is WIDTH long with STRING centered in it."
  (let* ((pad (- width (length string)))
	 (lpad (truncate pad 2))
	 (rpad (- pad (truncate pad 2))))
    (if (<= pad 0)
	string
	(concatenate 'string (make-string lpad :initial-element #\Space) string (make-string rpad :initial-element #\Space)))))

(defun normalize-time (dd dh dm ds)
"Internal fn for normalize-dhms. Returns the normalized (values DD DH DM DS)."
  (values (+ dd (floor (+ dh (floor (+ dm (floor ds 60)) 60)) 24)) ; dd
	  (- (+ dh (floor (+ dm (floor ds 60)) 60))
	     (* (floor (+ dh (floor (+ dm (floor ds 60)) 60)) 24) 24)) ; dh
	  (- (+ dm (floor ds 60)) (* (floor (+ dm (floor ds 60)) 60) 60)) ;dm
	  (- ds (* (floor ds 60) 60)))) ;ds

(defun normalize-ranges (dd dh dm ds)
"Internal fn for normalize-dhms. Returns the normalized (values DD DH DM DS). This function prevents overflow errors on systems with short longs (e.g. 32-bits) (If need be for CL ???)."
  (normalize-time (+ dd (floor dh 24))
			      (+ (- dh (* (floor dh 24) 24)) (floor dm 60))
			      (- dm (* (floor dm 60) 60))
			      ds))

(defun normalize-signs (dd dh dm ds)
"Internal fn for normalize-dhms."
  (let* ((quot (floor ds 86400))
	 (ds1 (- ds (* quot 86400)))
	 (dd1 (+ dd quot)))
    (setq dh 0 dm 0)
    (if (not (= dd1 0))
	(if (> dd1 0)
	    (when (< ds 0)
	      (setq ds1 (+ ds 86400)
		    dd1 (1- dd1)))
	    (when (> ds 0)
	      (setq ds1 (- ds 86400)
		    dd1 (1+ dd1)))))
    (if (not (= ds1 0))
	(normalize-time dd1 dh dm ds1)
	(values dd1 dh dm ds1))))

(defun valid-year-p (year) (>= year 1))
(defun valid-month-p (month) (and month (>= month 1) (<= month 12)))

(defun leap-year (year)
  "This function returns 1 if the given YEAR is a leap year and 0 otherwise."
  (if (or (and (zerop (mod year 4))
	       (not (zerop (mod year 100))))
	  (zerop (mod year 400)))
      1
      0))

(defun leap-year-p (year)
  "This function returns t if the given YEAR is a leap year and nil otherwise."
  (if (or (and (zerop (mod year 4))
	       (not (zerop (mod year 100))))
	  (zerop (mod year 400)))
      t
      nil))

(defun days-in-month (year month)
  "This function returns the number of days in the given MONTH of the given YEAR."
  (if (and (valid-year-p year)
	   (valid-month-p month))
      (aref days-in-month-arr (leap-year year) month)))

(defun days-in-year (year &optional month)
  "This function returns the number of days in the given YEAR and optional MONTH. If MONTH is [1..12], return the number of days in that YEAR as of the last of that MONTH."
  (aref days-in-year-arr (leap-year year) (if (and month (>= month 0) (<= month 12))
								  month
								  12)))

(defun check-date (year month day)
  "This function returns t if the given three numerical values YEAR MONTH DAY constitute a valid date, and nil otherwise."
  (and (valid-year-p year)
       (valid-month-p month)
       (>= day 1)
       (<= day (days-in-month year month))))

(defun check-time-p (hour min sec)
  "This function returns t if the given three numerical values HOUR MIN SEC constitute a valid time, and nil otherwise."
    (and (>= hour 0) (< hour 24)
	 (>= min 0) (< min 60)
	 (>= sec 0) (< sec 60)))

(defun day-of-year (year month day)
  "This function returns the sum of the number of days in the months starting with January up to and including MONTH in
    the given year YEAR. 0 on failure."
  (if (check-date year month day)
      (+ day (aref days-in-year-arr (leap-year year) (1- month)))
      0))

(defun date-to-days (year month day)
  "This function returns the (absolute) number of the day of the given date, where counting starts at the 1.Jan 1."
  (if (check-date year month day)
      (+ (year-to-days (1- year))
	 (day-of-year year month day))
      0))

(defun day-of-week (year month day)
  "This function returns the DOW of YEAR MONTH DAY. DOW not CL conform."
  (let ((days (date-to-days year month day)))
    (if (> days 0)
	(1+ (mod (1- days) 7))
	days)))

(defun cl-day-of-week (year month day)
  "This function returns the DOW of YEAR MONTH DAY. DOW CL conform."
  (let ((days (date-to-days year month day)))
    (if (> days 0)
	(mod (1- days) 7)
	days)))

(defun weeks-in-year (year)
  "This function returns the number of weeks in the given YEAR, i.e., either 52 or 53."
  (if (or (= 4 (day-of-week year 1 1))
	  (= 4 (day-of-week year 12 31)))
      53 52))

(defun cl-weeks-in-year (year)
  "This function returns the number of weeks in the given YEAR for CL DOW conform numbering (Monday=0)., i.e., either 52 or 53."
  (if (or (= 3 (cl-day-of-week year 1 1))
	  (= 3 (cl-day-of-week year 12 31)))
      53 52))

(defun check-business-p (year week dow)
    "This function returns true if the given three numerical values YEAR WEEK DOW constitute a valid date in business format, and nil otherwise. Beware that this function does NOT compute whether a given date is a business day (i.e., Monday to Friday)! To do so, use (< (day-of-week year month day) 6) instead. DOW not CL conform."
  (and (>= year 1)
       (>= week 1)
       (<= week (weeks-in-year year))
       (>= dow 1)
       (<= dow 7)))

(defun cl-check-business-p (year week dow)
    "This function returns true if the given three numerical values YEAR WEEK DOW constitute a valid date in business format for CL (Monday=0), and nil otherwise. DOW is CL conform."
  (and (>= year 1)
       (>= week 1)
       (<= week (weeks-in-year year))
       (>= dow 0)
       (<= dow 6)))

(defun delta-days (year1 month1 day1 year2 month2 day2)
  "This function returns the difference in days between Y1 M1 D1 and Y2 M2 D2."
  (- (date-to-days year2 month2 day2)
     (date-to-days year1 month1 day1)))

(defun week-number (year month day)
  "This function returns the number of the week of the given Y M D lies in. If the given date lies in the LAST week of the PREVIOUS year, 0 is returned."
  (let ((first-jan (1- (day-of-week year 1 1))))
    (if (< first-jan 4)
	(1+ (truncate (+ first-jan (delta-days year 1 1 year month day)) 7))
	(+ 0 (truncate (+ first-jan (delta-days year 1 1 year month day)) 7))))) ; + 0..-> only return one value

(defun week-of-year (year month day)
  "Return (values week year) where week is the week number of YEAR"
    (if (not (check-date year month day))
	nil
	(progn
	  (let ((week (week-number year month day)))
	    (if (= week 0)
		(values (weeks-in-year (1- year)) year)
		(progn
		  (if (> week (weeks-in-year year))
		      (values 1 (1+ year))
		      (values week year))))))))

(defun add-delta-days (year month day delta)
  "This function returns (values year month day) such that it is YEAR MONTH DAY plus DELTA days"
;; Be careful when changing things in this fn ! Side effects !
;; Fairly direct port from the PERL routine. Pretty imperative style.
  (let* ((days (+ (date-to-days year month day) delta))
	 (y1 (round (/ days 365.2425)))
	 (d1 (- days (year-to-days y1))))
    (when (> days 0)
      (progn
	(if (< d1 1)
	    (setf d1 (- days (year-to-days (1- y1)))) ; then
	    (setf y1 (1+ y1)))		; else
	(if (> d1 (days-in-year y1))
	    (setf d1 (- d1 (days-in-year y1))
		  y1 (1+ y1)))
	(loop for index downfrom 12 to 1
	      until (> d1 (days-in-year y1 index))
	      finally (return (values y1 (1+ index) (- d1 (days-in-year y1 index))))))))) ; index=month just one to low here after until, thats why (1+ index) as return value

(defun monday-of-week (week year)
  "Return (values year month day) where month and day correspond to the Monday of WEEK in YEAR"
  (let ((erst (1- (day-of-week year 1 1))))
    (if (< erst 4)
	(add-delta-days year 1 1 (- (* (1- week) 7) erst))
	(add-delta-days year 1 1 (- (* week 7) erst)))))

(defun nth-weekday-of-month-year (year month dow n)
  "This function returns the (year month day) of the N-th day of week DOW in the given MONTH and YEAR; such as, for example, the 3rd Thursday of a given month and year. DOW is not CL conform."
  (when (and (check-date year month 1) ; check params
	     (>= dow 1) (<= dow 7)
	     (> n 0) (< n 5))
    (let* ((erst (day-of-week year month 1))
	   (tow (if (< dow erst)
		    (+ dow 7)
		    dow)))
      (multiple-value-bind (y m d)
	  (add-delta-days year month 1 (+ (- tow erst) (* (1- n) 7)))
	(when (= month m)
	  (values y m d))))))

(defun cl-nth-weekday-of-month-year (year month dow n)
  "This function returns the (year month day) of the N-th day of week DOW in the given MONTH and YEAR; such as, for example, the 3rd Thursday of a given month and year. DOW is CL conform."
  (when (and (check-date year month 1) ; check params
	     (>= dow 0) (<= dow 6)
	     (> n 0) (< n 5))
    (let* ((erst (cl-day-of-week year month 1))
	   (tow (if (< dow erst)
		    (+ dow 7)
		    dow)))
      (multiple-value-bind (y m d)
	  (add-delta-days year month 1 (+ (- tow erst) (* (1- n) 7)))
	(when (= month m)
	  (values y m d))))))

(defun standard-to-business (year month day)
  "This function converts a given date from standard notation YEAR MONTH DAY to business notation year week dow. DOW is not CL conform."
  (multiple-value-bind (week y) (week-of-year year month day)
    (when week
      (values y week (day-of-week year month day)))))

(defun cl-standard-to-business (year month day)
  "This function converts a given date from standard notation YEAR MONTH DAY to business notation year week day of week. DOW is CL conform."
  (multiple-value-bind (week y) (week-of-year year month day)
    (when week
      (values y week (cl-day-of-week year month day)))))


(defun business-to-standard (year week dow)
  "This function converts a given date from business notation YEAR WEEK DOW to standard notation year month day. DOW is not CL conform."
  (when (check-business-p year week dow)
    (let* ((erst (day-of-week year 1 1))
	   (delta (+ (- dow erst) (* 7 (1- (+ week (if (> erst 4) 1 0)))))))
      (add-delta-days year 1 1 delta))))

(defun cl-business-to-standard (year week dow)
  "This function converts a given date from business notation YEAR WEEK DOW to standard notation year month day. DOW is CL conform."
  (when (cl-check-business-p year week dow)
    (let* ((erst (cl-day-of-week year 1 1))
	   (delta (+ (- dow erst) (* 7 (1- (+ week (if (> erst 4) 1 0)))))))
      (add-delta-days year 1 1 delta))))

(defun delta-hms (hour1 min1 sec1 hour2 min2 sec2)
  "This function returns the difference of H1 M1 S1 and H2 M2 S2 in (values d h m s)."
  (when (and (check-time-p hour1 min1 sec1)
	     (check-time-p hour2 min2 sec2))
    (normalize-signs 0 0 0 (- (+ sec2 (* 60 (+ min2 (* 60 hour2))))
					(+ sec1 (* 60 (+ min1 (* 60 hour1))))))))

(defun delta-dhms (year1 month1 day1 hour1 min1 sec1 year2 month2 day2 hour2 min2 sec2)
  "Returns the difference in (values d h m s) between the two given dates with times (Y1 M1 D1 H1 MIN1 SEC1 and Y2 M2 D2 H2 MIN2 SEC2)."
  (let ((dd (delta-days year1 month1 day1 year2 month2 day2)))
    (multiple-value-bind (d h m s) (delta-hms hour1 min1 sec1 hour2 min2 sec2)
      (if d
	  (values (+ d dd) h m s)
	  (values d h m s)))))

(defun delta-ymd (year1 month1 day1 year2 month2 day2)
  "This function returns the difference (values YEAR MONTH DAYS) between the two dates Y1M1D1 and Y2M2D2."
  (if (and (check-date year1 month1 day1)
	   (check-date year2 month2 day2))
      (values (- year2 year1)(- month2 month1)(- day2 day1))
      nil))

(defun delta-ymdhms (year1 month1 day1 hour1 min1 sec1
				 year2 month2 day2 hour2 min2 sec2)
  "This function returns the difference (values YEAR MONTH DAYS HOUR MINUTE SEC) between
the two dates Y1 M1 D1 H1 MI1 S1 and Y2 M2 D2 H2 MI2 S2."
  (multiple-value-bind (y m d) (delta-ymd year1 month1 day1 year2 month2 day2)
    (when y
      (multiple-value-bind (dd hh mm ss)
	  (delta-hms hour1 min1 sec1 hour2 min2 sec2)
	(when dd
	  (values y m (+ dd d) hh mm ss))))))

(defun normalize-dhms (day hour min sec)
  "This function takes four arbitrary values for days, hours, minutes and seconds (which may have different signs) and renormalizes them so that the values for hours, minutes and seconds will lie in the ranges [-23..23], [-59..59] and [-59..59], respectively, and so that they have the same sign."
  (multiple-value-bind (dd dh dm ds) (normalize-ranges day hour min sec)
    (when ds
      (normalize-signs dd dm dh (+ ds (* 60 (+ dm (* 60 dh))))))))

(defun add-delta-dhms (year month day hour min sec dd dh dm ds)
  "This function serves to add a days, hours, minutes and seconds offset to a given date and time (YEAR MONTH DAY HOUR MINUTE SECOND DDAY DHOUR DMINUTE DSECOND), in order to answer questions like \"today and now plus 7 days but minus 5 hours and then plus 30 minutes, what date and time gives that?\". Returns: (values y m d h min sec)"
  (when (and (check-date year month day)
	     (check-time-p hour min sec))
    (multiple-value-bind (d1 h1 m1 s1) (normalize-ranges dd dh dm ds)
      (when d1
	(progn
	  (let ((s2 (+ s1 (* 60 (+ m1 (* 60 h1))) (+ sec (* 60 (+ min (* 60 hour)))))))
	    (when (= 0 s2)
		(multiple-value-bind (yy mm ddd) (add-delta-days year month day d1)
		  (values yy mm ddd 0 0 0)))
	    (when (< s2 0)
	      (multiple-value-bind (dd1 ss2) (truncate s2 86400)
		(multiple-value-bind (ddd hh mm ss) (normalize-time (+ d1 dd1) 0 0 ss2)
		  (multiple-value-bind (yy mmm dddd) (add-delta-days year month day ddd)
		    (values yy mmm dddd hh mm ss)))))
	    (when (> s2 0)
	      (multiple-value-bind (ddd hh mm ss) (normalize-time d1 0 0 s2)
		(multiple-value-bind (yy mmm dddd) (add-delta-days year month day ddd)
		  (values yy mmm dddd hh mm ss))))))))))

(defun add-year-month (year month dy dm)
  "This function adds DYEAR and DMONTH offset to YEAR and MONTH."
  (let ((mt (+ month dm)))
    (if (> mt 0)
	(multiple-value-bind (jahre monate) (truncate (1- mt) 12)
	  (values (+ jahre (+ year dy)) (1+ monate)))
	(multiple-value-bind (jahre monate) (truncate mt 12)
	  (values (+ (+ year dy) jahre -1) (+ 12 monate))))))

(defun add-delta-ym (year month day dy dm)
  "This function adds DYEAR and DMONTH offset to YEAR MONTH DAY."
  (when (check-date year month day)
    (multiple-value-bind (jahr monat) (add-year-month year month dy dm)
      (values jahr monat day))))

(defun add-delta-ymd (year month day dy dm dd)
  "This function adds DYEAR DMONTH and DDAY offset to YEAR MONTH DAY."
  (when (check-date year month day)
      (multiple-value-bind (jahr monat tag) (add-delta-ym year month day dy dm)
	(when jahr
	  (add-delta-days jahr monat tag dd)))))

(defun add-delta-ymdhms (year month day hour min sec dyear dmonth dday dh dm ds)
  "This function is the same as add-delta-ymd except that a time offset may be given in addition to the year, month and day offset"
  (multiple-value-bind (jahr monat) (add-year-month year month dyear dmonth)
    (when jahr
      (add-delta-dhms jahr monat 1 hour min sec (+ dday (1- day)) dh dm ds))))

(defun system-clock (gmt time)
  "This function returns (values year month day hour min sec doy dow dst) based on current system clock. DOW is not CL conform."
  (multiple-value-bind (second minute hour day month year dow daylight-p dst)
      (decode-universal-time time)
    (declare (ignorable daylight-p))
    (let ((doy (day-of-year year month day)))
      (if gmt
	  (multiple-value-bind (jahr monat tag std min sek)
	      (add-delta-dhms year month day hour minute second 0 0 dst 0)
	    (values jahr monat tag std min sek doy (1+ dow) dst))
	  (values year month day hour minute second doy (1+ dow) dst)))))

(defun cl-system-clock (gmt time)
  "This function returns (values year month day hour min sec doy dow dst) based on current system clock. DOW is CL conform."
  (multiple-value-bind (second minute hour day month year dow daylight-p dst)
      (decode-universal-time time)
    (declare (ignorable daylight-p))
    (let ((doy (day-of-year year month day)))
      (if gmt
	  (multiple-value-bind (jahr monat tag std min sek)
	      (add-delta-dhms year month day hour minute second 0 0 dst 0)
	    (values jahr monat tag std min sek doy dow dst))
	  (values year month day hour minute second doy dow dst)))))

;;;;;;; Add gmt flag
(defun gmtime ()
  (system-clock t (get-universal-time)))

(defun localtime ()
  (system-clock nil (get-universal-time)))

(defun today ()
  "This function returns (year month day) for today."
  (multiple-value-bind (sec minute hour day month year) (get-decoded-time)
    (declare (ignorable sec minute hour))
    (values year month day)))

(defun yesterday ()
  (multiple-value-bind (jahr monat tag) (today)
    (add-delta-days jahr monat tag -1)))

(defun tomorrow ()
  (multiple-value-bind (jahr monat tag) (today)
    (add-delta-days jahr monat tag 1)))

(defun now ()
  "This function returns (hour minute second) for right now."
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (values hour minute second)))

(defun today-and-now ()
  "This function returns (year month day hour minute second) for the current date and time."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (values year month day hour minute second)))

(defun this-year ()
  "This function returns the current year in localtime."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignorable second minute hour day month))
    year))

(defun date-to-text (year month day)
  "Return a pretty print string of YEAR MONTH DAY in DOW-TXT(SHORT) DAY MONTH YEAR with a little bit of sorting for language."
  (let ((prn (first (aref long-format *language*)))) ; get print format
    (multiple-value-bind (a b c) ; What order is the date DMY , MDY ....
	(let ((k (second (aref long-format *language*))))
	  (case k   ; return the order of DMY
	    (10 (values month day year))
	    (11 (values day month year))
	    (otherwise (values month day year)))) ; return english by default
      (format nil prn  ; make the return string
	      (svref (gethash *language* day-of-week-abbreviation) ; Get Name of Weekday
		     (day-of-week year month day))
	      a b c))))

(defun date-to-text-long (year month day)
  "Return a pretty print string of YEAR MONTH DAY in DOW-TXT(LONG) DAY MONTH YEAR with a little bit of sorting for language."
  (let ((prn (first (aref long-format *language*)))) ; get print format
    (multiple-value-bind (a b c) ; What order is the date DMY , MDY ....
	(let ((k (second (aref long-format *language*))))
	  (case k   ; return the order of DMY
	    (10 (values month day year))
	    (11 (values day month year))
	    (otherwise (values month day year)))) ; return english by default
      (format nil prn  ; make the return string
	      (svref (gethash *language* day-of-week-to-text) ; Get Name of Weekday
		     (day-of-week year month day))
	      a b c))))

