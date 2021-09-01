{- Lab 1
   Authors: Eric Blohm, Pontus Granli Holmberg, Linus Hallberg 
   Lab group: 21
 -}
---------------------------------------------
power :: Int -> Int -> Int
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Int
stepsPower n k = length [0,1..k]


-- B -------------------------
-- power1

power1 :: Int -> Int -> Int
power1 n k 
 | k < 0 = error "power: negative argument"
power1 n k = product(replicate k n)


-- C -------------------------
-- power2

power2 :: (Num a, Integral b) => a -> b -> a
power2 n k 
 | k == 0 = 1
 | even k = (n*n)^ (k `div` 2)
 | odd k = n*(n^(k-1)) 

-- D -------------------------
{- 
Negativ bas med jämna samt ojämn exponent -> kolla om funktionerna kan lagen om dubbel negation.
Kolla då k = 0 
<Describe your test cases here>

 -}

-- comparePower1

comparePower1 :: Int -> Int -> Bool
comparePower1 n k 
 | power1 n k == power n k = True 
 | otherwise = False 

-- comparePower2
comparePower2 :: Int -> Int -> Bool
comparePower2 n k 
 | power2 n k == power n k = True 
 | otherwise = False 

-- Test functions: 

