-- acc - accumulator
-- m*  - main *
-- c*  - current *
-- n*  - next *
-- f*  - first *
-- s*  - second *
-- opr - operation
-- num - number

import Data.Char
import Data.List.Split
import Data.Tuple

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 
calculate :: [Char] -> (Float, [[Char]])
calculate (expr) = mInput(splitOn " " expr)

mInput :: [[Char]] -> (Float, [[Char]])
mInput (expr) = fInput(toFloat(head expr), tail expr)

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 
fInput :: (Float, [[Char]]) -> (Float, [[Char]])
fInput (fAcc, expr) | null expr = (fAcc, [])
                   | null cutExpr = fCalcF(fAcc, cOpr, (nNum, expr))
                   | nOpr == "+" || nOpr == "-" = fCalc(fAcc, cOpr, (nNum, expr))
                   | nOpr == "*" || nOpr == "/" = fCalcF(fAcc, cOpr, sInput(fAcc, expr))
                   | otherwise = (fAcc, [])
                   where
                   cutExpr = tail(tail expr)
                   cOpr =         head expr
                   nNum = toFloat(head(tail expr))
                   nOpr =         head(tail(tail expr))

fCalc :: (Float, [Char], (Float, [[Char]])) -> (Float, [[Char]])
fCalc (fAcc, cOpr, (nNum, expr)) | null expr = (fAcc, [])
                  | null cutExpr = fCalcF(fAcc, cOpr, (nNum, expr))
                  | cOpr == "+" = fInput(add(fAcc, (nNum, cutExpr)))
                  | cOpr == "-" = fInput(sub(fAcc, (nNum, cutExpr)))
                  | cOpr == "*" = fInput(mul(fAcc, nNum), cutExpr)
                  | cOpr == "/" = fInput(prt(fAcc, nNum), cutExpr)
                  | otherwise = (fAcc, [])
                  where
                  cutExpr = tail(tail expr)
                  nOpr =         head(tail(tail expr))
                  
fCalcF :: (Float, [Char], (Float, [[Char]])) -> (Float, [[Char]])
fCalcF (fAcc, cOpr, (nNum, expr)) | cOpr == "+" = add(fAcc, (nNum, tail(tail expr)))
                                  | cOpr == "-" = sub(fAcc, (nNum, tail(tail expr)))
                                  | cOpr == "*" = fInput(mul(fAcc, nNum), tail(tail expr))
                                  | cOpr == "/" = fInput(prt(fAcc, nNum), tail(tail expr))
                                  | otherwise = (fAcc, [])
                                  where
                                  cOpr =         head expr
                                  nNum = toFloat(head(tail expr))
   
 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

sInput :: (Float, [[Char]]) -> (Float, [[Char]])
sInput (sAcc, expr) | null expr = (sAcc, expr)
                    | null cutExpr = sCalcF(sAcc, expr)
                    | nOpr == "+" || nOpr == "-" = sCalcF(sAcc, expr)
                    | nOpr == "*" || nOpr == "/" = sCalc (sAcc, expr)
                    | otherwise = (sAcc, [])
                    where
                    cutExpr = tail(tail expr)
                    nOpr    = head(tail(tail expr))
             
sCalc :: (Float, [[Char]]) -> (Float, [[Char]])
sCalc (sAcc, expr) | null expr = (sAcc, [])
                   | cOpr == "*" = sInput(mul(sAcc, nNum), cutExpr)
                   | cOpr == "/" = sInput(prt(sAcc, nNum), cutExpr)
                   | otherwise = (sAcc, [])
                   where
                   cutExpr = tail(tail expr)
                   cOpr    =         head expr
                   nNum    = toFloat(head(tail expr))
                  
sCalcF :: (Float, [[Char]]) -> (Float, [[Char]])
sCalcF (sAcc, expr) | null expr = (sAcc, [])
                    | cOpr == "*" = (mul(sAcc, nNum), cutExpr)
                    | cOpr == "/" = (prt(sAcc, nNum), cutExpr)
                    | otherwise = (sAcc, [])
                    where
                    cutExpr = tail(tail expr)
                    cOpr    =         head expr
                    nNum    = toFloat(head(tail expr))


             
             

                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  