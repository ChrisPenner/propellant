Should I propagate contradictory values or nah?
How do I notify cells about "kick-out" and "bring-in"?
TMS is responsible for not propagating contradictions; but should instead report the nogood supports.

a = (T) F
b = (T) F

a & a == not b

a = T (yay)

True == not True -- NOOO
reject (a=T, b=T)
