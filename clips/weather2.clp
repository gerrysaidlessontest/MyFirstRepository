; -------------------------------
; Templates
; -------------------------------

; modify this clips application so that apart from the weather
; you explain that the sun is out if the sky is blue, and there 
; is no sun if the sky is grey.
; Just FYI my email address is gerrysaid@gmail.com

(deftemplate sky
  (slot color))

(deftemplate weather
  (slot condition))

(deftemplate set-sky
  (slot to-color))

(deftemplate prompt-next
  (slot value))

(deftemplate exit-flag)

; -------------------------------
; Ask user for sky color
; -------------------------------

(defrule ask-user
  ?prompt <- (prompt-next (value ready))
  =>
  (retract ?prompt)
  (printout t crlf "What is the sky color? (blue or grey, or type exit): ")
  (bind ?c (read))
  (if (eq ?c exit) then
    (assert (exit-flag))
    (printout t "Exiting." crlf)
  else
    (assert (set-sky (to-color ?c)))))

; -------------------------------
; Set or update sky fact
; -------------------------------

(defrule init-sky
  ?cmd <- (set-sky (to-color ?newColor))
  (not (sky))
  =>
  (retract ?cmd)
  (assert (sky (color ?newColor))))

(defrule update-sky
  ?cmd <- (set-sky (to-color ?newColor))
  ?old <- (sky (color ?oldColor&:(neq ?oldColor ?newColor)))
  =>
  (retract ?old)
  (retract ?cmd)
  (assert (sky (color ?newColor))))

(defrule discard-duplicate-sky-command
  ?cmd <- (set-sky (to-color ?newColor))
  (sky (color ?newColor)) ; already correct, no update needed
  =>
  (retract ?cmd))

; -------------------------------
; Weather updates
; -------------------------------

(defrule weather-nice
  (sky (color blue))
  ?w <- (weather (condition ?c&:(neq ?c nice)))
  =>
  (retract ?w)
  (assert (weather (condition nice))))

(defrule weather-ugly
  (sky (color grey))
  ?w <- (weather (condition ?c&:(neq ?c ugly)))
  =>
  (retract ?w)
  (assert (weather (condition ugly))))

(defrule init-weather-nice
  (sky (color blue))
  (not (weather))
  =>
  (assert (weather (condition nice))))

(defrule init-weather-ugly
  (sky (color grey))
  (not (weather))
  =>
  (assert (weather (condition ugly))))

; -------------------------------
; Loop to ask again unless exiting
; -------------------------------

(defrule repeat-loop
  (sky)
  (weather)
  (not (exit-flag))
  (not (prompt-next))
  =>
  (assert (prompt-next (value ready))))