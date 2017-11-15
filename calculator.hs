import Data.Char
import Data.List.Split

add :: (Float, (Float, [[Char]])) -> (Float, [[Char]])
sub :: (Float, (Float, [[Char]])) -> (Float, [[Char]])
mul :: (Float, Float) -> Float
prt :: (Float, Float) -> Float

add (a, (b, expr)) = (a + b, expr)
sub (a, (b, expr)) = (a - b, expr)
mul (a, b) = a * b
prt (a, b) = a / b

toFloat :: [Char] -> Float
toFloat x = read x :: Float

nNum :: ([[Char]]) -> Float
nNum (expr) = toFloat(head(tail(expr)))

sCalc :: ([[Char]]) -> (Float, [[Char]])
sCalc (expr) | null(tail expr) = (toFloat(head expr), [])
              | head(tail expr) == "+" = (toFloat(head expr), tail expr)
              | head(tail expr) == "-" = (toFloat(head expr), tail expr)
              | head(tail expr) == "*" = sCalc(expr)
              | head(tail expr) == "/" = sCalc(expr)
              | otherwise = (0, [])
              
fCalc :: (Float, [[Char]]) -> (Float, [[Char]])
fCalc (num, expr) | null expr = (num, [])
                    | head expr == "+" = fCalc(add(num, fCalc(tail expr)))
                    | head expr == "-" = fCalc(sub(num, fCalc(tail expr)))
                    | head expr == "*" = fCalc(mul(num, nNum expr), tail(tail expr))
                    | head expr == "/" = fCalc(prt(num, nNum expr), tail(tail expr))
                    | otherwise = (num, [])


input :: [[Char]] -> (Float, [[Char]])
input (expr) = fCalc(toFloat(head expr), tail expr)

calculate :: [Char] -> (Float, [[Char]])
calculate (expr) = input(splitOn " " expr)
