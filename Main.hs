import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e
evalExpr env (FuncExpr Nothing args block) = return $ Func "" (map show args)
evalExpr env (FuncExpr (Just (Id str)) args block) = return $ Func str (map show args)
	

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (IfSingleStmt expr stmt) =
	evalExpr env expr >> evalStmt env stmt
evalStmt env (IfStmt expr stmt1 stmt2) =
	evalExpr env expr >> evalStmt env stmt1 >> evalStmt env stmt2
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt:stmts)) =
	evalStmt env stmt >> evalStmt env (BlockStmt stmts)
evalStmt env (WhileStmt expr stmt) = 
	evalExpr env expr >> evalStmt env stmt
evalStmt env (DoWhileStmt stmt expr) = 
	evalStmt env stmt >> evalExpr env expr
evalStmt env forstmt =
	evalFor env forstmt
evalStmt env (FunctionStmt id [] block) =
	varDecl env (VarDecl id Nothing) >> evalStmt env (BlockStmt block)
evalStmt env (FunctionStmt id (arg:args) block) =
	varDecl env (VarDecl id (Just (FuncExpr (Just id) (arg:args) block))) >> varDecl env (VarDecl arg Nothing) >> evalStmt env (FunctionStmt id args block) >> evalStmt env (BlockStmt block)
	
--loop ou case
--evalStmt env (BrealStmt Nothing)
--evalStmt env (BrealStmt (Just id))


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--cada caso do For
evalFor :: StateT -> Statement -> StateTransformer Value
evalFor env (ForStmt NoInit Nothing Nothing stmt) =
	evalStmt env stmt
evalFor env (ForStmt (VarInit decls) Nothing Nothing stmt) =
	evalStmt env (VarDeclStmt decls) >> evalStmt env stmt
evalFor env (ForStmt (ExprInit expr) Nothing Nothing stmt) =
	evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt NoInit (Just expr) Nothing stmt) = 
	evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt (VarInit decls) (Just expr) Nothing stmt) =
	evalStmt env (VarDeclStmt decls) >> evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt (ExprInit exprinit) (Just expr) Nothing stmt) =
	evalExpr env exprinit >> evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt NoInit Nothing (Just expr) stmt) = 
	evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt (VarInit decls) Nothing (Just expr) stmt) =
	evalStmt env (VarDeclStmt decls) >> evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt (ExprInit exprinit) Nothing (Just expr) stmt) =
	evalExpr env exprinit >> evalExpr env expr >> evalStmt env stmt
evalFor env (ForStmt NoInit (Just expr1) (Just expr2) stmt) =
	evalExpr env expr1 >> evalExpr env expr2 >> evalStmt env stmt
evalFor env (ForStmt (VarInit decls) (Just expr1) (Just expr2) stmt) =
	evalStmt env (VarDeclStmt decls) >> evalExpr env expr1 >> evalExpr env expr2 >> evalStmt env stmt
evalFor env (ForStmt (ExprInit exprinit) (Just expr1) (Just expr2) stmt) =
	evalExpr env exprinit >> evalExpr env expr1 >> evalExpr env expr2 >> evalStmt env stmt
evalFor env ow = return Nil

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
