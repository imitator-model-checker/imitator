../../bin/imitator deadlock1.imi deadlock.imiprop

Old:
Final positive constraint guaranteeing deadlock-freeness:
   p >= 0

New (correct):
Final positive constraint guaranteeing deadlock-freeness:
   3 >= p
 & p >= 0


../../bin/imitator deadlock1.imi deadlock.imiprop

Old:
Final positive constraint guaranteeing deadlock-freeness:
   q >= 0
 & p >= 0

New (correct):
Final positive constraint guaranteeing deadlock-freeness:
   2 >= q
 & p >= 0
 & q >= 0
 & 3 >= p


Old:
Final positive constraint guaranteeing deadlock-freeness:
  p >= 0

New:
Final positive constraint guaranteeing deadlock-freeness:
  p >= 1

CORRECT answer would be: p>=2

