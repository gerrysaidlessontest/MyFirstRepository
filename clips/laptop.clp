(deftemplate laptop
  (slot name)
  (slot budget)
  (slot usage)
  (slot portability))

(deftemplate preference
  (slot category)
  (slot value))

(deftemplate match-score
  (slot laptop-name)
  (slot score)
  (multislot scored-on))

(deffacts laptops
  (laptop (name "Alienware X16") (budget high) (usage gaming) (portability no))
  (laptop (name "ASUS ROG Zephyrus") (budget high) (usage gaming) (portability yes))
  (laptop (name "Dell G15") (budget medium) (usage gaming) (portability no))
  (laptop (name "MacBook Air M2") (budget medium) (usage office) (portability yes))
  (laptop (name "HP Pavilion") (budget low) (usage study) (portability no))
  (laptop (name "Lenovo ThinkPad X1") (budget high) (usage office) (portability yes))
  (laptop (name "MacBook Pro 16") (budget high) (usage design) (portability no)))

(deffacts control
  (need-recommendation))

(defrule ask-budget
  (not (preference (category budget)))
  =>
  (printout t "What is your budget? (low / medium / high): ")
  (bind ?ans (read))
  (assert (preference (category budget) (value ?ans))))

(defrule ask-usage
  (not (preference (category usage)))
  =>
  (printout t "What is your main usage? (gaming / office / design / study): ")
  (bind ?ans (read))
  (assert (preference (category usage) (value ?ans))))

(defrule ask-portability
  (not (preference (category portability)))
  =>
  (printout t "Do you need high portability? (yes / no): ")
  (bind ?ans (read))
  (assert (preference (category portability) (value ?ans))))

(defrule initialize-scores
  ?l <- (laptop (name ?n))
  =>
  (assert (match-score (laptop-name ?n) (score 0) (scored-on))))

(defrule score-budget
  (laptop (name ?n) (budget ?b))
  (preference (category budget) (value ?b))
  ?m <- (match-score (laptop-name ?n) (score ?s) (scored-on $?tags))
  (test (not (member$ budget ?tags)))
  =>
  (modify ?m (score (+ ?s 1)) (scored-on (create$ ?tags budget))))

(defrule score-usage
  (laptop (name ?n) (usage ?u))
  (preference (category usage) (value ?u))
  ?m <- (match-score (laptop-name ?n) (score ?s) (scored-on $?tags))
  (test (not (member$ usage ?tags)))
  =>
  (modify ?m (score (+ ?s 1)) (scored-on (create$ ?tags usage))))

(defrule score-portability
  (laptop (name ?n) (portability ?p))
  (preference (category portability) (value ?p))
  ?m <- (match-score (laptop-name ?n) (score ?s) (scored-on $?tags))
  (test (not (member$ portability ?tags)))
  =>
  (modify ?m (score (+ ?s 1)) (scored-on (create$ ?tags portability))))

(defrule show-best
  (declare (salience -10))
  ?f <- (need-recommendation)
  ?max <- (match-score (laptop-name ?n1) (score ?s1))
  (not (match-score (score ?s2&:(> ?s2 ?s1))))
  =>
  (printout t crlf "💡 Best Match: " ?n1 " (score: " ?s1 " out of 3)" crlf)
  (retract ?f))
