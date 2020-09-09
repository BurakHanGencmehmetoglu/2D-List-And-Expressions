module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse 
    ) where

import Expression
import Parser
import Data.List




--Helper function for gridMap function.
helpergridMap [] _ = []
helpergridMap list uzunluk = [take uzunluk list] ++ (helpergridMap (drop uzunluk list) uzunluk)



--We apply given function to the each element of 2D List.
--Example runs : 
--gridMap (*5) [[1, 2], [3, 4], [5, 6]]
--[[5,10],[15,20],[25,30]]
--gridMap ("hello "++) [["world", "haskell", "ceng242"]]
--[["hello world","hello haskell","hello ceng242"]]

gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap (f) x = result where
                        length1=length (x!!0)
                        result1= [z | y<-x,z<-y]
                        result2= map f result1
                        result= helpergridMap result2 length1
                        



--Helper function for gridMapIf function.
helpergridMapIf _ _ [] = []
helpergridMapIf predicate function (x:xs) = if predicate x then [function x] ++ helpergridMapIf predicate function xs else [x] ++ helpergridMapIf predicate function xs



--We apply given function to the each element of 2D List if the element satisfies the given condition.
--Example runs :
--gridMapIf (<10) (*2) [[1, 2, 3], [11, 12, 13]]
--[[2,4,6],[11,12,13]]
--gridMapIf (<0) negate [[1], [-2], [3], [-4]]
--[[1],[2],[3],[4]]

gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
gridMapIf predicate function list = result where 
                                            length1 = length (list!!0)
                                            result1 = [z | y<-list,z<-y]
                                            result2 = helpergridMapIf predicate function result1
                                            result=helpergridMap result2 length1 








f (Just n) = n

--Helper function for evalExpr to do parameter matching over Expression type.
func (UnaryOperation Minus (Leaf (Variable x))) list = Leaf (Constant (-1*(f (lookup x list)))) 
func (UnaryOperation Minus (Leaf (Constant x))) _ = Leaf (Constant (-x)) 
func (Leaf (Constant x)) _ = Leaf (Constant x)
func (Leaf (Variable x)) list = Leaf (Constant (f (lookup x list)))
func (BinaryOperation Plus (Leaf (Constant x)) (UnaryOperation Minus (Leaf (Variable y)))) list = Leaf (Constant (x-f (lookup y list)))
func (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Constant x))) list = Leaf (Constant (x-f (lookup y list)))
func (BinaryOperation Plus (Leaf (Variable x)) (UnaryOperation Minus (Leaf (Variable y)))) list = Leaf (Constant (f (lookup x list)-f (lookup y list)))
func (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Variable x))) list = Leaf (Constant (f (lookup x list)-f (lookup y list)))
func (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (UnaryOperation Minus (Leaf (Variable x)))) list = Leaf (Constant (-f (lookup x list)-f (lookup y list)))
func (BinaryOperation Times (Leaf (Constant x)) (UnaryOperation Minus (Leaf (Variable y)))) list = Leaf (Constant (-x*f (lookup y list)))
func (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Constant x))) list = Leaf (Constant (-x*f (lookup y list)))
func (BinaryOperation Times (Leaf (Variable x)) (UnaryOperation Minus (Leaf (Variable y)))) list = Leaf (Constant (-(f (lookup x list)*f (lookup y list))))
func (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Variable x))) list = Leaf (Constant (-(f (lookup x list)*f (lookup y list))))
func (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (UnaryOperation Minus (Leaf (Variable x)))) list = Leaf (Constant (f (lookup x list)*f (lookup y list)))
func (BinaryOperation Plus (Leaf (Constant x)) (Leaf (Constant y)))  _ = Leaf (Constant (x+y))
func (BinaryOperation Times (Leaf (Constant x)) (Leaf (Constant y))) _ = Leaf (Constant (x*y))
func (BinaryOperation Plus (Leaf (Constant x)) (Leaf (Variable y))) list = Leaf (Constant (x+f (lookup y list)))
func (BinaryOperation Times (Leaf (Constant x)) (Leaf (Variable y))) list = Leaf (Constant (x*f (lookup y list)))
func (BinaryOperation Plus (Leaf (Variable y)) (Leaf (Constant x))) list = Leaf (Constant (x+f (lookup y list)))
func (BinaryOperation Times (Leaf (Variable y)) (Leaf (Constant x))) list = Leaf (Constant (x*f (lookup y list)))
func (BinaryOperation Plus (Leaf (Variable x)) (Leaf (Variable y)))  list = Leaf (Constant (f (lookup x list) +f (lookup y list)))
func (BinaryOperation Times (Leaf (Variable x)) (Leaf (Variable y)))  list =  Leaf (Constant (f (lookup x list) *f (lookup y list)))
func (BinaryOperation Plus (e1) (e2)) list = func (BinaryOperation Plus (func e1 list) (func e2 list)) list 
func (BinaryOperation Times (e1) (e2)) list = func (BinaryOperation Times (func e1 list) (func e2 list)) list
func (UnaryOperation Minus (e1)) list = func (UnaryOperation Minus (func e1 list)) list
func2 (Leaf (Constant x)) =x




--We evaluate the given expression.
--Example runs :
--evalExpr [] $ parse "-(3+5*7)"
--38
--evalExpr [("b", 10)] $ parse "-(3+5*7)+b*b"
--62
--evalExpr [("b", 10), ("var", 1)] $ parse "-(3+5*7)+b*b+(5*var)"
--67
--evalExpr [("x", -2), ("y", 0), ("z", 1)] $ parse "x*y*z + x*x + 5*z"
--9

evalExpr :: [(String, Int)] -> ExprV -> Int
evalExpr list x = func2 (func x list)




--Parameter matching for getvars function.
helperfunc (UnaryOperation Minus (Leaf (Variable x))) = [x]
helperfunc (UnaryOperation Minus (Leaf (Constant x))) = [] 
helperfunc (Leaf (Constant x)) = []
helperfunc (Leaf (Variable x)) = [x]
helperfunc (BinaryOperation Plus (Leaf (Constant x)) (UnaryOperation Minus (Leaf (Variable y)))) = [y]
helperfunc (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Constant x))) = [y]
helperfunc (BinaryOperation Plus (Leaf (Variable x)) (UnaryOperation Minus (Leaf (Variable y)))) = [x,y]
helperfunc (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Variable x))) = [x,y]
helperfunc (BinaryOperation Plus (UnaryOperation Minus (Leaf (Variable y))) (UnaryOperation Minus (Leaf (Variable x)))) = [x,y]
helperfunc (BinaryOperation Times (Leaf (Constant x)) (UnaryOperation Minus (Leaf (Variable y)))) = [y]
helperfunc (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Constant x))) = [y]
helperfunc (BinaryOperation Times (Leaf (Variable x)) (UnaryOperation Minus (Leaf (Variable y)))) = [x,y]
helperfunc (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (Leaf (Variable x))) = [x,y]
helperfunc (BinaryOperation Times (UnaryOperation Minus (Leaf (Variable y))) (UnaryOperation Minus (Leaf (Variable x))))= [x,y]
helperfunc (BinaryOperation Plus (Leaf (Constant x)) (Leaf (Constant y))) = []
helperfunc (BinaryOperation Times (Leaf (Constant x)) (Leaf (Constant y))) = []
helperfunc (BinaryOperation Plus (Leaf (Constant x)) (Leaf (Variable y))) = [y]
helperfunc (BinaryOperation Times (Leaf (Constant x)) (Leaf (Variable y))) = [y]
helperfunc (BinaryOperation Plus (Leaf (Variable x)) (Leaf (Constant y))) = [x]
helperfunc (BinaryOperation Times (Leaf (Variable x)) (Leaf (Constant y))) = [x]
helperfunc (BinaryOperation Plus (Leaf (Variable x)) (Leaf (Variable y)))  = [x,y]
helperfunc (BinaryOperation Times (Leaf (Variable x)) (Leaf (Variable y))) = [x,y]
helperfunc (BinaryOperation Plus (e1) (e2)) = (helperfunc e1) ++ (helperfunc e2) 
helperfunc (BinaryOperation Times (e1) (e2)) = (helperfunc e1) ++ (helperfunc e2) 
helperfunc (UnaryOperation Minus (e1)) = helperfunc e1


--Helper for getVars function.
helpergetvars [] = []
helpergetvars (x:xs) = if elem x xs then helpergetvars xs else [x] ++ helpergetvars xs



--We return all the variables in Expression as a list of string.
--Exanple runs : 
--getVars $ parse "3+5+8*-10"
--[]
--getVars $ parse "x+7*x+-x*5"
--["x"]
--getVars $ parse "myVar*a+(-y*y*z)"
--["a","myVar","y","z"]
--getVars $ parse "3*17+-a*x+ceng242"
--["a","ceng242","x"]

getVars :: ExprV -> [String]
getVars x = sort (helpergetvars (helperfunc x)) 



--Helper function for evalDeriv.
helpderiv (UnaryOperation Minus (Leaf (Variable x))) list a = if a==x then -1 else 0
helpderiv (UnaryOperation Minus (Leaf (Constant x))) _ _ = 0
helpderiv (Leaf (Constant x)) _ _ = 0
helpderiv (Leaf (Variable x)) list a = if a==x then 1 else 0  
helpderiv (BinaryOperation Plus (e1) (e2)) list a = (helpderiv e1 list a) + (helpderiv e2 list a) 
helpderiv (BinaryOperation Times (e1) (e2)) list a = (evalExpr list e1)*(helpderiv e2 list a) + (evalExpr list e2)*(helpderiv e1 list a)
helpderiv (UnaryOperation Minus (e1)) list a = -(helpderiv e1 list a)



--We will evaluate first derivative of expression with respect to given variable.
--Example runs :
--evalDeriv [("x", 5)] "x" $ parse "x*x*x"
--75
--evalDeriv [("x", 5), ("y", 3)] "x" $ parse "x*x*x + x*y"
--78
--evalDeriv [("y", 0), ("z", 2), ("myVar", 5)] "x" $ parse "3+y+z+5*myVar"
--0
--evalDeriv [("x", 12), ("y", 5), ("z", 13)] "z" $ parse "x*x + y*y + -z*z"
--26

evalDeriv :: [(String, Int)] -> String -> ExprV -> Int
evalDeriv list karakter e1 =  helpderiv e1 list karakter
