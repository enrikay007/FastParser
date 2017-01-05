module FastInterpreter
       ( runProg
       , Error (..)
       )
       where

import FastAST

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M



-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

-- | Give the printed representation of a value.
printed :: Value -> String
printed (IntValue x) = show x
printed (StringValue s) = s
printed (ReferenceValue ref) = "#<object " ++ show ref ++ ">"
printed (TermValue (Term sym vs)) =
  sym ++ "(" ++ intercalate ", " (map printed vs) ++ ")"

-- | A key-value store where the keys are of type @k@, and the values
-- are of type @v@.  Used for mapping object references to objects and
-- variable names to values.
type Store k v = M.Map k v

-- | A mapping from object references to objects.
type GlobalStore = Store ObjectReference ObjectState

-- | A mapping from field names to field values.
type ObjectFields = Store Name Value

-- | A mapping from variable names to variable values.
type MethodVariables = Store Name Value

-- | The global state of the program execution.
data GlobalState = GlobalState { classDecl::ClassDecl,
                                 globalstore::GlobalStore,
                                 methodstate::MethodState}
  deriving (Eq,Show)

-- | The state of a single object.
data ObjectState = ObjectState ObjectFields

-- | The state of a method execution.
data MethodState = MethodState Name MethodVariables 

-- | The basic monad in which execution of a Fast program takes place.
-- Maintains the global state, the running output, and whether or not
-- an error has occurred.
data FastM a = FastM {
  runFastM :: Prog -> GlobalState
           -> Either Error (a, GlobalState)
  }

instance Functor FastM where
  fmap = liftM

instance Applicative FastM where
  pure = return
  (<*>) = ap

instance Monad FastM where
  return x = FastM $ \_ globestate -> Right (x, globestate)
  fail er = FastM $ \_ _ -> Left $ Error er
  FastM sf0 >>= f  =
    FastM $ \prog gs -> do (a1,gs1) <- sf0 prog gs
                           let FastM sf1 = f a1
                           (a2,gs2) <- sf1 prog gs
                           return (a2, gs2)
                          
  
-- | Add the 'printed' representation of the value to the output.
printValue :: Value -> FastM ()
printValue value = do s <- getGlobalState
                      putGlobalState s { stdout = outputString msgs : stdout s }

-- | Get the program being executed.
askProg :: FastM Prog
askProg = undefined


getGlobalState :: FastM GlobalState
getGlobalState = FastM (\_ fs -> Right (fs,fs))

putGlobalState :: GlobalState -> FastM ()
putGlobalState n = FastM (\_ _ -> Right (_,n))

modifyGlobalState :: (GlobalState -> GlobalState) -> FastM ()
modifyGlobalState f = FastM(\_ fs -> Right(_, f fs))
throw :: ErrorType -> FastM a

-- Define a function, and handle the error via Left)
throw e = FastM (\_ _ -> Left $Error e)
-- | This function provides the instruction the PC currently points
-- to. If the PC is out of bounds, the MSM halts with an error.



modifyGlobalStore :: (GlobalStore -> GlobalStore) -> FastM ()
modifyGlobalStore f = FastM(\fs _ -> Right(f fs _))





lookupObject :: ObjectReference -> FastM ObjectState
lookupObject n = do
             s <- getGlobalState
             let rs = regs s
             case M.lookup n rs of Nothing -> throw $UnallocatedRegister n 
                                   Just v ->  return v   


setObject :: ObjectReference -> ObjectState -> FastM ()
setObject  m n = do
        s <- getGlobalState
        let rs = regs s
        case Map.lookup n rs of Nothing -> throw $UnallocatedRegister n
                                Just _ -> set s{regs = (M.insert n m rs)}

-- | Get a unique, fresh, never-before used object reference for use
-- to identify a new object.
allocUniqID :: FastM ObjectReference
allocUniqID = do
       s <- getGlobalState
       let rs = regs s
       case Map.lookup n rs of Nothing -> set s{ regs = (M.insert n 0 rs)}
                               Just _ -> throw RegisterAlreadyAllocated 


-- | The monad in which methods (and constructors and receive actions)
-- execute.  Runs on top of 'FastM' - maintains the reference to self,
-- as well as the method variables.
--
-- Note that since FastMethodM runs on top of FastM, a FastMethodM
-- action has access to the global state (through liftFastM).

-- data FastMethodM a = FastMethodM {
  -- runFastMethodM :: ObjectReference -> MethodState
                 -- -> FastM ...
  -- }

instance Functor FastMethodM where
  fmap = liftM

instance Applicative FastMethodM where
  pure = return
  (<*>) = ap

instance Monad FastMethodM where
  return = liftFastM . return
  fail = liftFastM . fail
  (>>=) = undefined

-- | Perform a 'FastM' operation inside a 'FastMethodM'.
liftFastM :: FastM a -> FastMethodM a
liftFastM = undefined

-- | Who are we?
askSelf :: FastMethodM ObjectReference
askSelf = undefined

-- | Add the given name-value associations to the variable store.
bindVars :: [(Name, Value)] -> FastMethodM a -> FastMethodM a
bindVars = undefined


getMethodState :: FastMethodM MethodState
getMethodState = FastM (\_ fs -> Right (fs,fs))

{-
putMethodState :: MethodState -> FastMethodM ()
putMethodState = undefined

getsMethodState :: (MethodState -> a) -> FastMethodM a
getsMethodState f = do s <- getMethodState
                       return $ f s

modifyMethodState :: (MethodState -> MethodState) -> FastMethodM ()
modifyMethodState f = do s <- getMethodState
                         putMethodState $ f s

getObjectState :: FastMethodM ObjectState
getObjectState = undefined

putObjectState :: ObjectState -> FastMethodM ()
putObjectState = undefined

getsObjectState :: (ObjectState -> a) -> FastMethodM a
getsObjectState f = do s <- getObjectState
                       return $ f s

modifyObjectState :: (ObjectState -> ObjectState) -> FastMethodM ()
modifyObjectState f = do s <- getObjectState
                         putObjectState $ f s
-}

-- | Find the declaration of the class with the given name, or cause
-- an error if that name is not a class.
findClassDecl :: Name -> FastM ClassDecl
findClassDecl = undefined

-- | Instantiate the class with the given name, passing the given
-- values to the constructor.
createObject :: Name -> [Value] -> FastM ObjectReference
createObject = undefined

sendMessageTo :: Value -> Value -> FastM Value
sendMessageTo = undefined

-- | Evaluate a method body - the passed arguments are the object in
-- which to run, the initial variable bindings (probably the
-- parameters of the method, constructor or receive action), and the
-- body.  Returns a value and the new state of the object.
evalMethodBody :: ObjectReference
               -> [(Name, Value)]
               -> Exprs
               -> FastM (Value, ObjectState)
evalMethodBody = undefined

evalExprs :: [Expr] -> FastMethodM Value
evalExprs [] = return $ TermValue $ Term "nil" []
evalExprs [e] = evalExpr e
evalExprs (e:es) = evalExpr e >> evalExprs es

evalExpr :: Expr -> FastMethodM Value
evalExpr (Plus a1 a2)= undefined

runProg :: Prog -> Either Error String
runProg prog = undefined
