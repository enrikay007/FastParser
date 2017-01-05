module FastParser
      ( integer
       ,name
       ,str
       ,Error
       ,pattern
       ,expr
       ,exprs
       ,params
       ,cases'
       ,case'
       ,args
       ,recvDecl
       ,namedMethodDecl
       ,namedMethodDecls
       ,constructorDecl
       ,classDecl
       ,parseString
       ,parseFile
       ,program
       )
       where

import FastAST

import SimpleParse
import Data.Char
import Data.Char(ord,isUpper, isLower, isAlphaNum, isDigit,isAlpha,isLetter)
import System.IO
import Control.Applicative hiding (Const)

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
-- type Error = ()
data Error=Error deriving Show

parseString :: String -> Either Error Prog
parseString s = 
	case parseEof program s of
		[]->Left Error
		[(p,_)]->Right p

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filePath = 
    do f <- readFile filePath
       let p = parseString f in return p

program :: Parser [ClassDecl]
program = many1 classDecl 

  


underScore :: Parser Char
underScore =  do
                c <- char '_'
                return c
  
  

letter :: Parser Char
letter = do a <- satisfy isAlpha
            return a

                    

a_token :: Parser a -> Parser a
a_token p = do {a <- p; spaces; return a}
                  
anyChar :: Parser Char
anyChar = satisfy $ \_ -> True                  

intNum :: Parser [Char]
intNum = do
	nr<-many1 digit1 
	return nr 
              
digit:: Parser Char
digit = satisfy isDigit
           
                  


digit1:: Parser Char
digit1 = satisfy isDigit

keyword :: String -> Parser ()
keyword s = do _ <- symbol s
               notFollowedBy $ satisfy constituent


constituent :: Char -> Bool
constituent c = isAlphaNum c || c == '_'|| c /= '"'

constituent1 :: Char -> Bool
constituent1 c = isDigit c|| isLetter c || c == '_' {-|| c /= '"'-}


str :: Parser String
str = token $ do  
                schar '"' 
                cs <- many(satisfy strConstant)           
                schar '"'
                return (cs)


strConstant :: Char -> Bool
strConstant c = isAlphaNum c|| c /= '"' {-c == '['|| c == ']'||c == '*'||c == '^'|| c == '"' -}
 	
isSpecial :: Char -> Bool
isSpecial c = {-not (isSpace c) && -} c `notElem` "\"" && not (isDigit c)
      
type Number = Integer
 
    
    
name :: Parser Name
name = token $ do c <- satisfy (isAlpha )
                 -- cs <- many(letter <|> underScore <|> digit1)
                 -- satisfy isSpace
                  cs <- many (satisfy constituent1)	
              --    notFollowedBy (satisfy isAlpha)           
                  if elem (c:cs) keywords then reject
		              else return (c:cs)
		                   where keywords=["self", "class", "new","receive","send","match", "return", "set"]


                       
integer :: Parser Integer
integer =token  (do 
              str <-  num
              return (read str)) 
     
token' :: Parser a -> Parser a
token' p = p <* munch isSpace'
  where isSpace' c = c == ' ' || c == '\t'
  
num=  many digit

-- param :: Parser Name
-- param = params 
    -- <|> reject


-- params = do  
             -- es <- many oneCommaExpr
             -- return (es)
             


-- oneCommaExpr :: Parser Name
-- oneCommaExpr = do symbol ','
                  -- e <- name 
                  -- return e  
-- Parsing parameters


-- params :: Parser [Name]
-- params = do  a<-(token $ name) `sepBy` schar ','
             -- schar ','
             -- return (a)
             
params :: Parser [Name]
params =  (token param) `sepBy` schar ','              
             
             
         -- <++(do p <- many name  
                -- return (p))
-- params' :: Parser [[Name]]
-- params' = many params           

-- Parsing parameters
param =  name
                  
-- param :: Parser Name
-- param = do p <- name
           -- return p
           


-- params :: Parser [Name]                                          
-- params = do  a <- token $ param `sepBy` schar ','
             -- schar ','
             -- return a
            -- <|>
             -- (do a<- many ()
                 -- return a             
              -- )
pattern :: Parser Pattern
pattern = token $ do i <- integer
                     return (ConstInt i)
             
          <|>
          do s <- str
             return (ConstString s)
             
          <|>
          do n <- name
             symbol "("
             p <- params
             symbol ")"
             return (TermPattern n p)              
           
          <|> 
          do n1 <- name
             return (AnyValue n1)          
             
cases' :: Parser [Case]
cases' = token ( do c <-many case'
                    return c)
            <|>    reject                   

case' :: Parser Case
case' = token $ do p <- pattern
                   symbol "->"
                   symbol "{"
                   e <- exprs 
                   symbol "}"
                   return (p, e)

   
-- (a -> b -> a) - this is the first argument which is function. 
bop :: (a -> b -> a) -> Parser a -> Parser [Char] -> Parser b -> Parser a
bop f pa p pb = do 
 a <- pa
 p
 b <- pb
 return (f a b) -- return is return as the parser from a, f return as the a
 
 
-- (a -> b -> a) - this is the first argument which is function. 
-- bop' :: (a -> b -> c-> a) -> Parser a -> Parser Char -> Parser b -> Parser Char-> Parser c->Parser Char -> Parser a
bop' pa  = do  
                      symbol "."
                      b <- name
                      symbol "("
                      c <- args
                      symbol ")"
                      bop' $ CallMethod pa b c 
                  <|> return pa
 -- return is return as the parser from a, f return as the a
-- (a -> b -> a) - this is the first argument which is function. 
bop3 :: (a -> b -> c-> a) -> Parser a -> Parser Char -> Parser b -> Parser Char-> Parser c->Parser Char -> Parser a
bop3 f pa p1 pb p2 pc p3= do 
 a <- pa
 p1
 b <- pb
 p2
 c <- pc
 p3
 return (f a b c) -- return is return as the parser from a, f return as the a


-- (a -> b -> a) - this is the first argument which is function. 
bop1 :: (a -> b -> a) -> Parser [Char] -> Parser a -> Parser [Char]-> Parser b -> Parser a
bop1 f p1 pa p2 pb = do 
 p1
 a <- pa
 p2
 b <- pb
 return (f a b) -- return is return as the parser from a, f return as the a



-- (a -> b -> a) - this is the first argument which is function. 
bop2 :: (a -> a) -> Parser [Char] -> Parser a -> Parser a
bop2 f p pa = do 
 p
 a <- pa
 return (f a) -- return is return as the parser from a, f return as the a

			 
			  

expr :: Parser Expr
expr =  e0 <* spaces

e0 :: Parser Expr
e0 = bop2 Return (symbol "return") e1<|> e1
--chainl1 e1 op0
--
-- e0 :: Parser Expr
-- e0 = bop1 SetVar (symbol "set" ) name (symbol "=" ) e1

e1 :: Parser Expr
e1 = chainl1 e2 op0

e2 :: Parser Expr
e2 = chainl1 e3 op1




-- e3 :: Parser Expr
-- e3 = chainl1 e4 op3


e3 :: Parser Expr
--e2 = chainl1 t op2

-- e3 =  do a <- t  
         -- bop' a
     -- <|> t
e3 =  bop3 CallMethod t (schar '.') name (schar '(') args (schar ')') <|> t
     
t :: Parser Expr
t =  do
			i <- integer
			return (IntConst i)
	<|>
      do
      s <- str
      return (StringConst s)
  <|>
      do
      nr <- name
      return (ReadVar nr)
     
  <|> do
      na <- name
      symbol "("
      arg <- args
      symbol ")"
      return (TermLiteral na arg)
 <|>
     do
     symbol "self"
     return (Self)
     
   -- <|> 
     -- do
     -- symbol "return"
     -- exp <- expr
     -- return (Return exp)
  
 <|>
     do 
     symbol "set"
     symbol "self"
     symbol "."
     n5 <- name
     symbol "="
     exp5 <- expr
     return (SetField n5 exp5)  
 <|>
     do 
     symbol "set"
     n1 <- name
     symbol "="
     exp1 <- expr
     return (SetVar n1 exp1)  
     
  <|>
     do 
     symbol "match"
     exp2 <- expr
     schar '{'
     cases1 <- cases'
     schar '}'
     return (Match exp2 cases1)     
  
  <|> 
     do
     symbol "send"
     schar '('
     exp2 <-expr
     schar ','
     exp3 <- expr
     schar ')'
     return (SendMessage exp2 exp3)
  <|>
      do
      symbol "self"
      symbol "."
      nr7 <- name
      return (ReadField nr7)
  <|>
     do 
     symbol "new"
     n2 <- name
     schar '('
     ags <-args
     schar ')'
     return (New n2 ags)
  <|>
	   (do 
        schar '('
        f <- expr
        schar ')'
        return (f))
  
  
-- op0 :: Parser (Expr -> Expr)
-- op0 =(do
        -- symbol "return"
        -- exp <- expr
        -- return (Return exp))
       
     


op0 :: Parser (Expr -> Expr -> Expr)
op0 = (do _ <- symbol "+"
          return Plus) 
     <|>
     (do _ <- symbol "-"
         return Minus) 
          
          
op1 :: Parser (Expr -> Expr -> Expr)
op1 = (do _ <- symbol "*"
          return Times)                 
        <|>
     (do _ <- symbol "/"
         return DividedBy) 

          
args :: Parser [Expr]          
args = (token expr) `sepBy` schar ',' 


exprs :: Parser [Expr]
exprs =  do a <-(token expr) `sepBy` schar ';'
            schar ';' 
            return a
 

recvDecl :: Parser (Maybe ReceiveDecl)
recvDecl = token  (do s <- option $ symbol "receive"
                      symbol "("
                      r <- param
                      symbol ")"
                      symbol "{"
                      e <- exprs
                      symbol "}"
                      case s of 
                         Nothing -> return Nothing
                         Just _ -> return $ Just $ ReceiveDecl  r e)
                      

                      
namedMethodDecl :: Parser NamedMethodDecl
namedMethodDecl = token $ do m <- name
                             symbol "("
                             ps <- params
                             symbol ")"
                             symbol "{"
                             exp4 <- exprs
                             symbol "}"
                             return (NamedMethodDecl m ( MethodDecl ps exp4))
                            

namedMethodDecls :: Parser [NamedMethodDecl]
namedMethodDecls = token $ many namedMethodDecl
                     

constructorDecl :: Parser (Maybe MethodDecl)
constructorDecl = token $ (do symbol "new"
                              symbol "("
                              pr <- params
                              symbol ")"
                              symbol "{"
                              e9 <- exprs
                              symbol "}"
                              return $ Just $ MethodDecl pr e9)
                         <|> return Nothing
             
classDecl :: Parser ClassDecl
classDecl = token $ do symbol "class"
                       ne <- name
                       symbol "{"
                       con <- constructorDecl
                       nm <- namedMethodDecls
                       rv <- recvDecl
                       symbol "}"
                       return (ClassDecl  ne  con nm rv)

             
-- parens :: Parser a -> Parser a
-- parens = between 
