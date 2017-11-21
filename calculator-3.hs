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
mul :: (Float, (Float, [[Char]])) -> (Float, [[Char]])
prt :: (Float, (Float, [[Char]])) -> (Float, [[Char]])

add (a, (b, expr)) = (a + b, expr)
sub (a, (b, expr)) = (a - b, expr)
mul (a, (b, expr)) = (a * b, expr)
prt (a, (b, expr)) = (a / b, expr)

toFloat :: [Char] -> Float
toFloat x = read x :: Float

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 
calculate :: [Char] -> Float
calculate (expr) = fst(mInput(splitOn " " expr))

mInput :: [[Char]] -> (Float, [[Char]])
mInput (expr) = fInput(toFloat(head expr), tail expr)

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 
fInput :: (Float, [[Char]]) -> (Float, [[Char]])
fInput (fAcc, expr) | null expr                  = (fAcc, [])
                    | null cutExpr               = fCalcF(fAcc, cOpr, (nNum, cutExpr))
                    | nOpr == "+" || nOpr == "-" = fCalc (fAcc, cOpr, (nNum, cutExpr))
                    | nOpr == "*" || nOpr == "/" = fCalc (fAcc, cOpr, sInput(nNum, cutExpr))
                    | otherwise                  = (fAcc, [])
                    where
                    cutExpr = tail(tail expr)
                    cOpr    =         head expr
                    nNum    = toFloat(head(tail expr))
                    nOpr    =         head(tail(tail expr))

fCalc :: (Float, [Char], (Float, [[Char]])) -> (Float, [[Char]])
fCalc (fAcc, cOpr, (nNum, expr)) | null expr    = fCalcF(fAcc, cOpr, (nNum, expr))
                                 | cOpr == "+"  = fInput(add(fAcc, (nNum, expr)))
                                 | cOpr == "-"  = fInput(sub(fAcc, (nNum, expr)))
                                 | cOpr == "*"  = fInput(mul(fAcc, (nNum, expr)))
                                 | cOpr == "/"  = fInput(prt(fAcc, (nNum, expr)))
                                 | otherwise    = (fAcc, [])
                  
fCalcF :: (Float, [Char], (Float, [[Char]])) -> (Float, [[Char]])
fCalcF (fAcc, cOpr, (nNum, expr)) | cOpr == "+" = add(fAcc, (nNum, []))
                                  | cOpr == "-" = sub(fAcc, (nNum, []))
                                  | cOpr == "*" = mul(fAcc, (nNum, []))
                                  | cOpr == "/" = prt(fAcc, (nNum, []))
                                  | otherwise   = (fAcc, [])
   
 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

sInput :: (Float, [[Char]]) -> (Float, [[Char]])
sInput (sAcc, expr) | null expr                  = (sAcc, expr)
                    | null cutExpr               = sCalcF(sAcc, expr)
                    | nOpr == "+" || nOpr == "-" = sCalcF(sAcc, expr)
                    | nOpr == "*" || nOpr == "/" = sCalc (sAcc, expr)
                    | otherwise                  = (sAcc, [])
                    where
                    cutExpr = tail(tail expr)
                    nOpr    = head(cutExpr)
             
sCalc :: (Float, [[Char]]) -> (Float, [[Char]])
sCalc (sAcc, expr) | null expr   = (sAcc, [])
                   | cOpr == "*" = sInput(mul(sAcc, (nNum, cutExpr)))
                   | cOpr == "/" = sInput(prt(sAcc, (nNum, cutExpr)))
                   | otherwise   = (sAcc, [])
                   where
                   cutExpr = tail(tail expr)
                   cOpr    = head expr
                   cNum    = toFloat(head expr)
                   nNum    = toFloat(head(tail expr))
                  
sCalcF :: (Float, [[Char]]) -> (Float, [[Char]])
sCalcF (sAcc, expr) | null expr   = (sAcc, [])
                    | cOpr == "*" = mul(sAcc, (nNum, cutExpr))
                    | cOpr == "/" = prt(sAcc, (nNum, cutExpr))
                    | otherwise   = (sAcc, [])
                    where
                    cutExpr = tail(tail expr)
                    cOpr    = head expr
                    nNum    = toFloat(head(tail expr))


             
             

                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  