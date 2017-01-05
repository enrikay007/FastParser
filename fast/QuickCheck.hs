
import FastParser
import FastAST
import SimpleParse
import System.IO
import Control.Applicative hiding (IntConst)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Char(ord,isUpper, isLower, isAlphaNum, isDigit,isAlpha,isLetter)
import System.IO

   
      
--------testing library
newtype TestName = TestName Name
                    deriving Show



nameGen :: Gen Name
nameGen = do 
   len <- choose (1,20)
   nameN len

    where charGen = elements $ ['a'..'z']++['A'..'B']++['_']
          
          
          nameN :: Int -> Gen Name
          nameN 0 = return []
          nameN n | n > 0 = do c <- charGen 
                               cs <- nameN (n-1)
                               return (c:cs)

instance Arbitrary TestName where
  arbitrary = do s <- nameGen
                 return (TestName s)
                                
prop_PPP(TestName s) = 
    case parseEof str s of
    [(result,"")] -> result == s
    _            -> False
    

    
    

                    
newtype TestExprString  = TES String
                        deriving Show
  
numberGen:: Gen String
numberGen = do
   i <- choose (0,1000 ::Int)
   j <- choose (0,1000 ::Int)
   return (show  i {-++ "." ++ show j-})

   
unOpGen, binOpGen :: Gen String
unOpGen = elements ["width", "height"]
binOpGen = elements ["+", "*"]

exprStrGen :: Int -> Gen String
exprStrGen  0 = numberGen
exprStrGen n|n > 0 = do
  op <- binOpGen
  e1 <- exprStrGen (n `div` 2)
  e2 <- exprStrGen (n `div` 2)
  return (e1 ++ op ++ e2)
 
 
prop_Precedence s =
  case parseEof expr s of
    [(e, "")] -> foo e
  where
    foo :: Expr -> Bool
    foo (Plus e1 e2) = Plus e1 e2 `validPrec` e1 &&
                      Plus e1 e2 `validPrec` e2 &&
                      foo e1 && foo e2
    
    foo (Times e1 e2) = Times e1 e2 `validPrec` e1 &&
                       Times e1 e2 `validPrec` e2 &&
                       foo e1 && foo e2
    foo (IntConst _) = True
    foo _ = False
 
validPrec :: Expr -> Expr -> Bool
validPrec (Plus _ _) (Plus _ _) = True
validPrec (Plus _ _) (Times _ _) = True
validPrec (Plus _ _) (IntConst _) = True
validPrec (Times _ _) (Times _ _) = True
validPrec (Times _ _) (IntConst _) = True
validPrec _ _ = False
   
   
   
   
   
   
   
   
   
   
