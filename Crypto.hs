module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m n 
--check special condition: n = 0 
        | n == 0 = m
        | otherwise = gcd n (m `mod` n)

phi :: Int -> Int --can use a helper function also
--use list comprehension to get a list with elements range from 1 to m and fits the confition gcd a m == 1 
phi m = length [a|a<-[1..m], gcd a m == 1]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a b
--check base case of the recursion : b=0
        | b == 0 = (1,0)
        --based on the math equation to get u' v' are result of computeCoeffs with inputs b and r 
        | otherwise = (v', (u'- q*v'))
              where (q,r) = quotRem a b
                    (u',v') = computeCoeffs b r

-- Inverse of a modulo m
inverse :: Int -> Int -> Int 
inverse a m 
        | gcd a m == 1 = 
        -- not use if then else, try again  
          if u < 0 
              then m+u
          else u 
          --base on the math equation, u is obtained by calling the computeCoeff function with a and m as inputs 
          where (u,_) = computeCoeffs a m 
         -- not 1, then return error message 
        | otherwise = null (or gcd /= 1) = error("no inverse of "++ show a ++ `mod` ++ show m ++ "exists")

-- Calculates (a^k mod m)
#check this again
modPow :: Int -> Int -> Int -> Int
modPow a k m 
--check for base case of the recursion where k = 0 
        | k == 0 = 1 `mod` m 
        | even k = modPow c j m
        --to reduce the volume of calculation use recursion here instead of putting in formula 
        | otherwise = (a * modPow c j m) `mod` m --modpow a (k-1) m*a`mod`m
        where c = ((a `mod` m)^2) `mod` m -- or a*a`mod`m
              j = k `div` 2 

-- Returns the smallest integer that is coprime with phi
smallestCoPrimeOf :: Int -> Int
--use list comprehension to get the smallest coprime from a list of number from 2 and coprime to a
#use head then once they hv the first one they will return so not generate the entire list
#want it to generte the entire list then needa define it 
smallestCoPrimeOf a = head [b|b<-[2..], gcd a b == 1]

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
--calculate e d n based on steps given on spec 
genKeys p q = ((e, n ), (d, n ))
        where n = p*q
              e = smallestCoPrimeOf ((p - 1)*(q - 1))
              d = inverse e ((p-1)*(q-1))
#             m = (p-1)*(q-1) -> coz prev just repeating this operation, can now replace others with m

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e,n) = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d,n) = modPow c d n


-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt x 
        | x =='a' = 0
        | otherwise = ord x - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar n = chr(ord 'a' + n)

-- "adds" two letters
add :: Char -> Char -> Char
add n m = toChar x
        where x = (toInt n + toInt m) `mod` ((ord 'z' - ord 'a') + 1)

-- "substracts" two letters
substract :: Char -> Char -> Char
substract n m 
#this can use mod as well(?), check it out
        | y>=0 = toChar y
        | otherwise = toChar ((ord 'z' - ord 'a' + 1) + y)
        where y = toInt n - toInt m

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
ecbEncrypt :: Char -> String -> String
ecbEncrypt k m = map (add k) m
        --using map to add k to each char in m 
        --where is redundant here 

ecbDecrypt :: Char -> String -> String
#instead of `subtract`, can use flip subtract
ecbDecrypt k m = map (`substract` k) m
        --using map to substract each char by k in m 
       

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt k iv m
#in terms of string, use ""
#do not use head and tail here, use m:ms instead, so:
        | length (x:xs) == 0 = ""
        | otherwise = c' : cbcEncrypt k c' (tail xs)
        where c' = add k (add (head x) iv)

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt k iv m 
        | length (x:xs) == 0 = ""
        | otherwise = x : cbcDecrypt k c (xs)
        where x = (c `substract` k) `substract` iv  
              c = head s 


