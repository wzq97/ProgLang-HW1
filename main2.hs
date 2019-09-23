import PA1Helper
import System.Environment (getArgs)
import Data.Map

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 

-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!

reducer :: Lexp -> Lexp
reducer (Apply (Lambda x t12) v2@(Lambda _ _)) = subst x t12 v2
reducer (Apply v1@(Lambda _ _ ) t2) = let t2' = reducer t2 in Apply v1 t2'
reducer (Apply t1 t2) = let t1' = reducer t1 in Apply t1' t2
reducer _ = error "No rules applies"


-- substitute all string in Lexp to lexp
subst :: String -> Lexp -> Lexp -> Lexp
subst x (Atom v) new
	| x == v = new
	| otherwise = Atom v
subst x (Lambda y t1) new
	| x == y = Lambda y t1
	| x /= y = Apply (Atom y) (subst x t1 new)
subst x (Apply t1 t2) new = Apply (subst x t1 new) (subst x t2 new)

{-
freeVars :: Lexp -> [String]
freeVars (Atom x) = [x]
freeVars (Lambda x t1) = freeVars t1 \\ [x]
freeVars (Apply t1 t2) = freeVars t1 ++ freeVars t2
-}


eta :: Lexp -> Lexp
eta v@(Atom _) = v
eta lexp@(Apply _ _) = lexp 
eta lexp1@(Lambda var lexp2@(Apply exp1 v@(Atom var2))) = if var2 == var then exp1 else lexp1
eta lexp@(Lambda _ _) = lexp

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
