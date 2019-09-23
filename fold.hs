--foldr'
foldr' :: (a->b->b) -> b -> [a] -> b
foldr' _ u []    = u
foldr' f u (h:t) = f h (foldr' f u t)

{-- Test it with:

foldr' (+) 0 [1,2,3,4]   -- displays 10
foldr' (*) 1 [1,2,3,4]   -- displays 24

--}

-- filter' as foldr' application after eta-reduction
filter' :: (a-> Bool) -> [a] -> [a]
filter' p = foldr' 
            (\h t ->  if p h 
                      then h:t 
                      else t) []

{-- Test it with:

filter''' (\x -> x < 3) [1,2,3,4]   -- displays [1,2]

--}

-- remove as filter application
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter' (\v -> v/=x)

{-- Test it with:

remove 2 [1,2,3,4]

--}

-- remove as filter application using infix notation for /=
remove' :: (Eq a) => a -> [a] -> [a]
remove' x = filter' (/=x)

{-- Test it with:

remove' 2 [1,2,3,4]

--}

-- Haskell representation of lambda expression
data Lexp = Atom String | Lambda String Lexp | Apply Lexp Lexp 

freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)

{-- Test it with:

freevars (Lambda "x" (Atom "x"))
freevars (Atom "x")
freevars (Apply (Lambda "x" (Atom "x")) (Atom "y"))
freevars (Apply (Lambda "x" (Atom "x")) (Atom "x"))
freevars (Lambda "x" (Apply (Atom "x") (Atom "x")))
freevars (Apply (Lambda "x" (Apply (Atom "x") (Atom "x"))) (Lambda "x" (Apply (Atom "x") (Atom "x"))))
--}

iscombinator :: Lexp -> Bool
iscombinator e = freevars e == []

{-- Test it with:

iscombinator (Lambda "x" (Atom "x"))
iscombinator (Atom "x")
iscombinator (Apply (Lambda "x" (Atom "x")) (Atom "y"))
iscombinator (Apply (Lambda "x" (Atom "x")) (Atom "x"))
iscombinator (Lambda "x" (Apply (Atom "x") (Atom "x")))
iscombinator (Apply (Lambda "x" (Apply (Atom "x") (Atom "x"))) (Lambda "x" (Apply (Atom "x") (Atom "x"))))
--}