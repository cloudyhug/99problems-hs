-- 1
mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs
mylast _ = error "mylast"

-- 2
mybutlast :: [a] -> a
mybutlast [x, _] = x
mybutlast (x:xs) = mybutlast xs
mybutlast _ = error "mybutlast"

-- 3
mynth :: Integral i => [a] -> i -> a
mynth (x:xs) 1 = x
mynth (x:xs) n = mynth xs (n - 1)
mynth _ _ = error "mynth"

-- 4
mylength :: Integral i => [a] -> i
mylength l =
    f 0 l
    where
        f n [] = n
        f n (x:xs) = f (n + 1) xs

-- 5
myreverse :: [a] -> [a]
myreverse l =
    f [] l
    where
        f acc [] = acc
        f acc (x:xs) = f (x:acc) xs

-- 6
mypalindrome :: Eq a => [a] -> Bool
mypalindrome [] = True
mypalindrome [_] = True
mypalindrome l =
    let len = mylength l in
    if even len then f (len `div` 2) 0 [] l else g (len `div` 2) 0 [] l
    where
        f n n' acc l@(x:xs) = if n == n' then acc == l else f n (n' + 1) (x:acc) xs
        g n n' acc (x:xs) = if n' == n then acc == xs else g n (n' + 1) (x:acc) xs

-- 7
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f acc x) xs

myflatten :: [[a]] -> [a]
myflatten ll = myfoldl (++) [] ll

-- myfoldr :: (a -> b -> a) -> a -> [b] -> a
-- myfoldr f acc l = myfoldl f acc (myreverse l)

-- 8
mycontains :: Eq a => [a] -> a -> Bool
mycontains [] x = False
mycontains (x':xs) x = if x' == x then True else mycontains xs x

mycompress :: Eq a => [a] -> [a]
mycompress l =
    f [] l
    where
        f acc [] = myreverse acc
        f [] (x:xs) = f [x] xs
        f acc@(a:_) (x:xs) = if x == a then f acc xs else f (x:acc) xs

-- 9
mypack :: Eq a => [a] -> [[a]]
mypack [] = []
mypack l =
    f [] [] l
    where
        f acc current [] = myreverse (current:acc)
        f acc [] (x:xs) = f acc [x] xs
        f acc current@(c:_) (x:xs) = if x == c then f acc (x:current) xs else f (current:acc) [x] xs

-- 10
mymap :: (a -> b) -> [a] -> [b]
mymap f l =
    f' [] l
    where
        f' acc [] = myreverse acc
        f' acc (x:xs) = f' ((f x):acc) xs

myencode :: (Eq a, Integral i) => [a] -> [(i, a)]
myencode l = mymap (\p@(x:_) -> (mylength p, x)) (mypack l)

-- 11
data Encoded i a = One a | Several i a deriving (Show)

myencodeModified :: (Eq a, Integral i) => [a] -> [Encoded i a]
myencodeModified l =
    mymap f (myencode l)
    where
        f (1, x) = One x
        f (n, x) = Several n x

-- 12
mymake :: Integral i => i -> a -> [a]
mymake n x =
    f [] n
    where
        f acc 0 = acc
        f acc n = f (x:acc) (n - 1)

myrevappend :: [a] -> [a] -> [a]
myrevappend [] l = l
myrevappend (x:xs) l = myrevappend xs (x:l)

mydecode :: (Eq a, Integral i) => [Encoded i a] -> [a]
mydecode l =
    myreverse (myfoldl f [] l)
    where
        f acc (One x) = x:acc
        f acc (Several n x) = myrevappend (mymake n x) acc

-- 13
myencodeDirect :: (Eq a, Integral i) => [a] -> [Encoded i a]
myencodeDirect l =
    f [] 0 Nothing l
    where
        f acc _ Nothing [] = myreverse acc
        f acc count (Just x) [] = myreverse ((if count == 1 then One x else Several count x):acc)
        f acc _ Nothing (x:xs) = f acc 1 (Just x) xs
        f acc count (Just c) (x:xs) =
            if x == c then f acc (count + 1) (Just c) xs
            else f ((if count == 1 then One c else Several count c):acc) 1 (Just x) xs

-- 14
mydup :: [a] -> [a]
mydup l =
    f [] l
    where
        f acc [] = myreverse acc
        f acc (x:xs) = f (x:x:acc) xs

-- 15
myrepli :: Integral i => [a] -> i -> [a]
myrepli l n =
    f [] l
    where
        f acc [] = myreverse acc
        f acc (x:xs) = f (myrevappend (mymake n x) acc) xs

-- 16
mydrop :: Integral i => [a] -> i -> [a]
mydrop l n =
    f [] n l
    where
        f acc _ [] = myreverse acc
        f acc k (x:xs) = if k == 1 then f acc n xs else f (x:acc) (k - 1) xs

-- 17
mysplit :: Integral i => [a] -> i -> ([a], [a])
mysplit l n =
    f [] n l
    where
        f acc 0 l = (myreverse acc, l)
        f acc n (x:xs) = f (x:acc) (n - 1) xs

-- 18
mytake :: Integral i => [a] -> i -> [a]
mytake l n =
    f [] n l
    where
        f acc 0 _ = myreverse acc
        f acc n (x:xs) = f (x:acc) (n - 1) xs

myslice :: Integral i => [a] -> i -> i -> [a]
myslice l i j = mytake (snd $ mysplit l (i - 1)) (j - i + 1)

-- 19
myrotate :: Integral i => [a] -> i -> [a]
myrotate l 0 = l
myrotate l n = let (a, b) = if n > 0 then mysplit l n else mysplit l (mylength l + n) in b ++ a

-- 20
myremoveat :: Integral i => [a] -> i -> [a]
myremoveat l n = let (a, (x:xs)) = mysplit l (n - 1) in a ++ xs

-- 21
myinsertat :: Integral i => a -> [a] -> i -> [a]
myinsertat x l n = let (a, b) = mysplit l (n - 1) in a ++ (x:b)

-- 22
myrange :: Integral i => i -> i -> [i]
myrange i j =
    if i < j then f [] i else g [] i
    where
        f acc n = if n == j then myreverse (j:acc) else f (n:acc) (n + 1)
        g acc n = if n == j then myreverse (j:acc) else g (n:acc) (n - 1)

-- 23
myrandgen :: Integral i => i -> i -> i -> i -> i
myrandgen m a c seed = (a * seed + c) `mod` m

myrand :: Integral i => i -> i
myrand = myrandgen (2 ^ 31) 1103515245 12345

myrandrange :: Integral i => i -> i -> i -> (i, i)
myrandrange a b seed =
    let seed' = myrand seed
        n = seed' `mod` (b - a + 1) + a in (n, seed')

seed :: Integral i => i
seed = 14829087

myrndselect :: Integral i => [a] -> i -> [a]
myrndselect l n =
    f [] n l seed
    where
        f acc 0 l s = acc
        f acc n l s =
            let (index, s') = myrandrange 1 (mylength l) s in
            f ((mynth l index):acc) (n - 1) (myremoveat l index) s'

-- 24
mylottoselect :: Integral i => i -> i -> [i]
mylottoselect n m = myrndselect (myrange 1 m) n

-- 25
myrndpermu :: [a] -> [a]
myrndpermu l = myrndselect l (mylength l)

-- 26
mycount :: (Eq a, Integral i) => a -> [a] -> i
mycount x l = myfoldl (\c e -> if e == x then c + 1 else c) 0 l

mycombination :: Integral i => i -> [a] -> [[a]]
mycombination 0 _ = []
mycombination n l =
    let len = mylength l
        final = mymake len 1 in
        f [] (mymake (mylength l) 0) final
        where
            f acc counter final =
                if counter == final then
                    if mycount 1 counter == n then (pick [] counter l):acc else acc
                else if mycount 1 counter == n then
                    f ((pick [] counter l):acc) (incr counter) final
                else f acc (incr counter) final
                where
                    pick acc [] [] = myreverse acc
                    pick acc (c:cs) (x:xs) = if c == 1 then pick (x:acc) cs xs else pick acc cs xs
                    incr counter =
                        g 0 (myreverse counter)
                        where
                            g n (x1:x2:xs) =
                                case (x1, x2) of
                                    (0, 0) -> myreverse (1:0:xs)
                                    (0, 1) -> myreverse (1:1:xs)
                                    (1, 0) -> myreverse ((mymake n 0) ++ (0:1:xs))
                                    (1, 1) -> g (n + 1) (x2:xs)

-- 27