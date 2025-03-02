import PA1Helper
import System.Environment (getArgs)

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


-- ////////////////////////////// Helper functions ///////////////////////////////////
-- helper function of reducer, which checks break point and helps with reducer recursion
helper :: Lexp -> Lexp -> Lexp
helper exp1 exp2 = if exp1 == exp2 then exp2 else reducer exp2

-- remove as filter application
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (\v -> v/=x)

-- Haskell representation of lambda expression
freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)

-- helper functrion of alpha renaming. Get a variable name
getNewVar :: String -> String
getNewVar var = var ++ "0"



-- ////////////////////////////// Reduction functions //////////////////////////////
-- reduce lambda expression in different scenario using alpha renaming, beta and eta reduction
reducer :: Lexp -> Lexp
reducer lexp@(Atom _) = lexp
reducer lexp@(Apply (Lambda x y) exp1) = helper lexp (beta y x (reducer exp1))
reducer lexp@(Apply exp1 exp2) = helper lexp (Apply (reducer exp1) (reducer exp2))
reducer lexp@(Lambda x (Apply y z)) = helper lexp (eta lexp)
reducer lexp@(Lambda x exp1) = helper lexp (Lambda x (reducer exp1))

-- rename the variable occurences of a given lambda expression
alpha :: Lexp -> Lexp
alpha lexp@(Atom _) = lexp
alpha lexp@(Lambda old exp) = Lambda (getNewVar old) (beta exp old (Atom (getNewVar old)))
alpha lexp@(Apply e1 e2) = lexp

-- reduce lambda expression by replacing the old String with a new lexp
beta :: Lexp -> String -> Lexp -> Lexp
beta v@(Atom var1) old new = if var1 == old then new else v
beta lexp@(Lambda var1 exp1) old new =
 if var1 /= old && var1 `notElem` (freevars new)
  then (Lambda var1 (beta exp1 old new))
  else if var1 /= old && var1 `elem` (freevars new)
   then beta (alpha lexp) old new
   else lexp
beta lexp@(Apply exp1@(Lambda var1 exp11) exp2) old new = beta (beta exp11 var1 exp2) old new
beta lexp@(Apply exp1 exp2) old new = (Apply (beta exp1 old new) (beta exp2 old new))

-- reduce lambda expression by eta reduction: \x.(E x)->E. 
eta :: Lexp -> Lexp
eta v@(Atom _) = v
eta lexp@(Apply _ _) = lexp 
eta lexp1@(Lambda var lexp2@(Apply exp1 v@(Atom var2))) = 
 if var2 == var && var `notElem` (freevars exp1)
  then exp1
  else lexp1
eta lexp@(Lambda _ _) = lexp


-- /////////////////////////////// Main function ////////////////////////////////
-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
