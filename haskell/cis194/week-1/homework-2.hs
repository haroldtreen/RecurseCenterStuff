type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start, end)]
hanoi n start end temp =
    let nMinusOne = n - 1
        topToBuffer = hanoi nMinusOne start temp end
        bottomToEnd = hanoi 1 start end temp
        bufferToEnd = hanoi nMinusOne temp end start
    in  topToBuffer ++ bottomToEnd ++ bufferToEnd
