module Main where
--import  qualified Data.ByteString as BS
--import  qualified Data.Text as T
--import  qualified Data.Text.Encoding as T
--import  Data.ByteString.UTF8 as U
--import System.Random
import Data.Char
import Debug.Trace (trace)

main :: IO()

sum' (x:xs)
 | xs == [] = x
 | otherwise = sum' xs + x

sum'' [] = 0
sum'' (x:xs) = x  + sum'' xs

product' (x:xs)
 | xs == [] = x
 | otherwise = product' xs * x

take' n (x:xs)
 | xs == [] = [x]
 | n < 1 = []
 | otherwise = x : take' (n-1) xs

take'' _ [] = []
take'' n _ | n < 1= []
take'' n (x:xs) = x:take'' (n-1) xs

drop' n (x:xs)
 | n== 0 = x:xs
 | n >= 0 && n <= length(xs) = drop'(n-1) xs

drop'' _ [] = []
drop'' n xs | n < 1 = xs
drop'' n (x:xs) = drop'' (n-1) xs

reverse' (x:xs)
 | length(xs)==0 = x:xs -- くっつけなおして返却
 | otherwise = reverse'(xs) ++ [x]  -- 先頭を末尾にくっつける

-- Q6
-- p,q
-- ax + by = c の傾きは、 -a/b
-- 垂線：bx − ay = d
preLinePlot (a,b,c, loop, fn)
 | loop == 0 = []
 | loop > 0 = preLinePlot2(a,b,c,0, loop, fn)

preLinePlot2 (a,b,c, counter, loop,  fn)
 | counter > loop = []
 | otherwise = fn(a,b,c,counter) : preLinePlot2(a,b,c, counter+1, loop, fn)

preLine (a,b,c,x) = (c - (a * x)) / b

preFormula(a,b,c) = "y = (" ++ show c ++ " - (" ++ show a ++ " * x)) / " ++ show b

split [] = []
split (x:xs) = show x ++ '\n' : split xs

toStringList [] = []
toStringList (x:xs) = show x ++ '\n' : toStringList xs

printListLn (x:xs) =  do
   putStrLn  $ show x
   printListLn xs
printListLn [] = putStr ""

rot13chr x
  | ord 'a' <= ord x && ord x <= ord 'm' || ord 'A' <= ord x && ord  x <= ord 'M' = chr $ ord x + 13
  | ord 'n' <= ord  x && ord  x <= ord 'z' || ord 'N' <= ord x && ord  x <= ord 'Z' = chr $ ord x - 13
  | otherwise = x

rot13(x:xs)
 | xs == [] = [x]
 | otherwise = rot13chrResult:rot13Result
  where
    rot13chrResult= rot13chr x
    rot13Result = rot13 xs

bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort xs = ys ++ bubblesort zs -- ys,zsともに遅延評価
    where
        -- foldr 関数 初期値 対象
        (ys, zs) = foldr bubble ([], []) xs
        -- 戻り値は(ソート済みの領域, 未ソートの領域)
        -- bubble :: (Ord a) => a -> ([a], [a]) -> ([a], [a])
        bubble x ([], []) = ([x], []) -- 1要素目
        bubble x (p:ps, qs)
            | p < x     = ([p], x:ps ++ qs)
            | otherwise = (x:p:ps, qs)

-- 1から20までの値を3個出して、それがピタゴラスの定理を満たせばOK。満たさなければNG.
-- c^2 = a^2 + b^2
-- cを決めれば、必然的にa,bは求まる？
-- [1..20] これがcの範囲
-- c ^ 2 = a^2 + b^2を満たすa,bを見つけるには、
-- 上記を満たすまで再帰でa,bの総当たりをする。a,bについてもリスト内包表記ができる。
-- (a,b)を全組合せ作り出すことも可能。
abpattern = [(a,b,c) | a<-[1..20], b<-[1..20], c<-[1..20], c^2 == a^2+b^2]
  
data OrderStatus = ORDERING | ORDERED | CANNCELED deriving (Show, Enum)

data Point = Point Int Int deriving Show

offset (Point x1 y1) (Point x2 y2) =
    Point (x1 + x2) (y1 + y2)

data B = B Int
data A = A Int
f:: A->B
f A = B
id :: B->B
id B = B


main = do
    let a = Point 2 3
        b = Point 1 1
        c = offset a b
    print c