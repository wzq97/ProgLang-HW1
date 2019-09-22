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
reducer lexp1@(Apply lexp2@(Lambda old exp1) new) = (subst exp1 old new)
reducer lexp = lexp
 

subst :: Lexp -> String -> Lexp -> Lexp
subst v@(Atom var1) old new = if var1 == old then new else v
subst lexp@(Lambda var1 exp1) old new = (Lambda var1 (subst exp1 old new))
subst lexp@(Apply exp1 exp2) old new = (Apply (subst exp1 old new) (subst exp2 old new))


eta :: Lexp -> Lexp
eta v@(Atom _) = v
eta lexp@(Apply _ _) = lexp 
eta lexp1@(Lambda var lexp2@(Apply exp1 v@(Atom var2))) = if var2 == var then exp1 else lexp1
eta lexp@(Lambda _ _) = lexp

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input1.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
