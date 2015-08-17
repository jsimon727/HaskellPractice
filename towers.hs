type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 
  let 
    step1move = hanoi(n-1) a c b 
    step2move = hanoi(a, b) 
    step3move = hanoi(n-1) c b a
  in
  step1move ++ step2move ++ step3move
