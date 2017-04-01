-- errors are used instead of functors for this exercise, I may come back and redo it with functors but at the moment
-- the point of this exercise is to implement a lot of the standard library functions

myLast :: [a] -> a
myLast [] = error "invalid input"
myLast (x:[]) = x
myLast (x:xs) = myLast xs 

myButLast :: [a] -> a
myButLast [] = error "invalid input"
myButLast (x:[]) = error "invalid input"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "invalid input"
elementAt (x:xs) n 
    | n < 0 = error "invalid input"
    | n == 0 = x
    | otherwise = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = myReverse ls == ls

data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = 
    case x of 
        [] -> []
        (x:xs) -> (myFlatten x) ++ (myFlatten (List xs))

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:xs) = 
    if x == y
        then compress (y:xs)
        else x : compress (y:xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:xs) = let (first:more):rest = pack xs in
    if x == first
        then (x:first:more):rest
        else [x]:(first:more):rest

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:[]) = [(1, x)]
encode (x:xs) = let (num, el):rest = encode xs in
    if x == el
        then (num+1, el):rest
        else (1, x):(num, el):rest