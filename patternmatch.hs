lucky :: Int -> String
lucky 7 = "Lucky Numebr Seven!!"
lucky x = "Sorry"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Not between 1 and 3"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)


charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph" 
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 


firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]


bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal."
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, conguratulations!"
  where bmi = weight / height ^ 2 
        skinny = 18.5
        normal = 25.0
        fat = 30.0


initals :: String -> String -> String
initals firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


cylinder :: Double -> Double -> Double
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea