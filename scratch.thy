seq
  (letrec
    (fac/2066
       (function _v/2065
         (let
           (n/2064 = _v/2065
            _v/2067 = (apply (field 0 (global Fac.links!)) (- n/2064 1)))
           (* n/2064 _v/2067))))
    (setfield_imm 0 (global Fac.links!) fac/2066))

(setglobal Fac!
  (letrec
    (fac/1002 (function n/1003 (* n/1003 (apply fac/1002 (- n/1003 1)))))
    (makeblock 0 fac/1002)))
