(deftemplate sky
   (slot color))

(deftemplate weather
   (slot condition))

; Rule: If sky is grey, remove old sky and weather facts, set grey sky and ugly weather
(defrule update-sky-to-grey
   ?s <- (sky (color ?))
   ?w <- (weather (condition ?))
   (test (neq ?s (sky (color grey))))
   =>
   (retract ?s)
   (retract ?w)
   (assert (sky (color grey)))
   (assert (weather (condition ugly))))

; Rule: If sky is blue, remove old sky and weather facts, set blue sky and nice weather
(defrule update-sky-to-blue
   ?s <- (sky (color ?))
   ?w <- (weather (condition ?))
   (test (neq ?s (sky (color blue))))
   =>
   (retract ?s)
   (retract ?w)
   (assert (sky (color blue)))
   (assert (weather (condition nice))))

; If no sky exists, and blue is asserted, create nice weather
(defrule init-blue-sky
   (not (sky))
   (sky (color blue))
   =>
   (assert (weather (condition nice))))

; If no sky exists, and grey is asserted, create ugly weather
(defrule init-grey-sky
   (not (sky))
   (sky (color grey))
   =>
   (assert (weather (condition ugly))))
