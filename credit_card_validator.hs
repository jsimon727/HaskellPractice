toDigits :: Integer -> [Integer]

toDigits 0 = []
toDigits n = reverse(toDigitsRev(n))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther(x:y:zs) = x : (2 * y) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits(x : xs)  = (x `mod` 10) + (x `div` 10) + sumDigits xs 


validate :: Integer -> Bool

validate(n) = ((sumDigits(doubleEveryOther(toDigitsRev(n)))) `mod` 10) == 0






