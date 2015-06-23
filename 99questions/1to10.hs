-- Ex.1 Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Ex.2 Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Ex.3 Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt (xs) x
    | length(xs) >= x && x > 0 = xs !! (x - 1)
    | otherwise = error "Incorrect index"

-- Ex.4  Find the number of elements of a list.
myLength :: [a] -> Int
myLength (xs) = length(xs)
