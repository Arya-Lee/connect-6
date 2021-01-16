module AIwhite where
import Data.List
import Data.Char
import System.IO
import Data.Time
import Control.Monad (when)
import AIblack (get_part_before_right_braket,get_part_after_left_braket,printchess,f)--为了避免函数被重复定义，调用了AIblack中定义的函数
------------------------------------------------------------------------
dup :: String -> Int -> String 
dup a 1 = a
dup a 2 = a ++ a
dup a n = dup a (n-1) ++ a --copy String
------------------------------------------------------------------------
blackpoint = "########"
blackground = "#......."
white = "#1111111" 
black = "#0000000" --basic import
----------------------------------------------------------------------
row :: (Int,Int) -> Int -> Int -> String
row (x,y) v l  | (l-1) `mod` 4 == 0                      = dup blackpoint 15 ++ "#"
               | (l-1) `mod` 4 /= 0,(x,y) == (0,0)       = dup blackground 15 ++ "#"
               | (l-1) `mod` 4 /= 0,y == 1,v == 2        = if l < 4 * x + 1 && l > 4 * x - 3 then white ++ dup blackground 14 ++ "#" else dup blackground 15 ++ "#"                
               | (l-1) `mod` 4 /= 0,y >= 2,y < 15,v == 2 = if l < 4 * x + 1 && l > 4 * x - 3 then dup blackground (y-1) ++ white ++ dup blackground (15-y) ++ "#" else dup blackground 15 ++ "#"
               | (l-1) `mod` 4 /= 0,y == 15,v == 2       = if l < 4 * x + 1 && l > 4 * x - 3 then dup blackground (y-1) ++ white ++ "#" else dup blackground 15 ++ "#"                                  --White chess
                         
               | (l-1) `mod` 4 /= 0,y == 1,v == 1        = if l < 4 * x + 1 && l > 4 * x - 3 then black ++ dup blackground 14 ++ "#" else dup blackground 15 ++ "#"
               | (l-1) `mod` 4 /= 0,y >= 2,y < 15,v == 1 = if l < 4 * x + 1 && l > 4 * x - 3 then dup blackground (y-1) ++ black ++ dup blackground (15-y) ++ "#" else dup blackground 15 ++ "#"        --black chess
               | (l-1) `mod` 4 /= 0,y == 15,v == 1       = if l < 4 * x + 1 && l > 4 * x - 3 then dup blackground (y-1) ++ black ++ "#" else dup blackground 15 ++ "#" 
-------------------------------------------------------------------------
{-f :: (Int, Int) -> Int -> Int -> String                 
f (x,y) v 1 = row (x,y) v 1 
f (x,y) v 2 = row (x,y) v 1 ++ "\n" ++ row (x,y) v 2
f (x,y) v l = f (x,y) v (l-1) ++ "\n" ++ row (x,y) v l-}
----------------------------------------------------------------------
fil e = if e == 145 then 35 else if e == 1270 then 10 else e

strnumb q = map ord q                                                                  -- strnumb "###" = [35,35,35]
strnumbs q = map (+(-45)) (strnumb q)                                                  -- strnumbs "###" = [-10,-10,-10] , strnumbs "..." = [1,1,1]
zipWiths q w = zipWith (*) (strnumbs q) (strnumbs w)                                   -- zipWiths "###" "..." = [-10,-10,-10]
fils q w = map fil $ map (+45) $ zipWiths q w                                          -- files "###" "..." = [35,35,35]
strdo q w = map chr $ fils q w                                                         -- String combination,mapchar "###" "..." = "###"
printss q w = putStrLn $ strdo q w                                                      -- print , puts "###" "..." = ###
------------------------------------------------------------------------
strdos [] = error"########"
strdos [x] = x
strdos (x:xs) = strdo x (strdos xs)                                                    --strdos ["#..",".#.","..#"] = "###"
prints x = putStrLn $ strdos x                                                        --prints ["#..",".#.","..#"] = ###
--printchess a b =  prints $ concat [a,b]                                                     -- print blackchess whitechess
------------------------------------------------------------------------
qsort :: Ord a => [a] -> [a]     
qsort []=[]
qsort (h:t)=(qsort(filter (<h) t) )++(num (h:t)) ++(qsort(filter (>h) t))
num (h:t)=filter (==h) (h:t)


list_n :: Int -> [a] -> a
list_n n a = last $ take n a

evens :: Integral a => a -> a
evens n = if even n then n else n+1



xb1 :: [(a, b)] -> a
xb1 l=fst $ l!!0
xb2 :: [(a, b)] -> a
xb2 l=fst $ l!!1
yb1 :: [(a, b)] -> b
yb1 l=snd $ l!!0
yb2 :: [(a, b)] -> b
yb2 l=snd $ l!!1

transfertotwobits x
   |x>10||x==10 =show x
   |x<10 ="0"++ show x                                     
ai_white=do
  mdatablack <- readFile "black.csv"                                                         --read blackchess
  mdatawhite <- readFile "white.csv"                                                         --read whitechess
  currentTime <-getCurrentTime
  let aab = lines mdatablack     
      aaw = lines mdatawhite
      everyonein :: (Foldable t, Eq a) => [a] -> t a -> Bool
      everyonein [] ta = True
      everyonein (s:st) ta = s `elem` ta && everyonein st ta
      positions :: Eq a => a -> [a] -> [Int]
      positions x xs = [pos | (x',pos) <- zip xs [1..length xs],x' == x]

      m :: Int -> String
      p :: Int -> String
      m n = aab !! n
      p q = aaw !! q
      
      lb = length aab 
      lw = length aaw 
      
      tbh :: Int -> Int
      tbmin :: Int -> Int
      tbs :: Int -> Int
      tb :: Int -> Int
      tbh n = read $ take 2 $ m n :: Int
      tbmin n = read $ take 2 $ (tails $ m n) !! 3 :: Int
      tbs n = read $ take 2 $ (tails $ m n) !! 6 :: Int
      tb n = 3600 * tbh n + 60 * tbmin n + tbs n   --blackchess time
      
      twh :: Int -> Int
      twmin :: Int -> Int
      tws :: Int -> Int
      tw :: Int -> Int
      twh q = read $ take 2 $ p q :: Int
      twmin q = read $ take 2 $ (tails $ p q) !! 3 :: Int
      tws q = read $ take 2 $ (tails $ p q) !! 6 :: Int
      tw q = 3600 * twh q + 60 * twmin q + tws q   --whitechess time
      
      xb :: Int -> Int -> Int
      yb :: Int -> Int -> Int
      xw :: Int -> Int -> Int
      yw :: Int -> Int -> Int
      
       
      move_of_black n 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ m (n-1)
      move_of_black n 1=get_part_before_right_braket $ get_part_after_left_braket $ m (n-1) 
      
      ordered_pairs_of_black n j="("++move_of_black n j++")"
      
      xb n j=if n == 1 && j == 2 then error"empty" else fst (read $ordered_pairs_of_black n j ::(Int,Int))
      yb n j=if n == 1 && j == 2 then error"empty" else snd (read $ordered_pairs_of_black n j ::(Int,Int))
      
      move_of_white q 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ p (q-1)
      move_of_white q 1=get_part_before_right_braket $ get_part_after_left_braket $ p (q-1) 
      
      ordered_pairs_of_white q j="("++move_of_white q j++")"
      
      xw q j=fst (read $ordered_pairs_of_white q j ::(Int,Int))
      yw q j=snd (read $ordered_pairs_of_white q j ::(Int,Int))
      -------------------------------------------------------------------------------
                                           --List of white chess strings                                      
      chess_list_b = if lb == 1 then [(xb 1 1,yb 1 1)] else [(xb 1 1,yb 1 1)] ++ [(x,y) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_w = [(x,y) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      
      bla = [f (x,y) v l | (x,y) <- chess_list_b,v <- [1],l <- [61]]
      whi = [f (x,y) v l | (x,y) <- chess_list_w,v <- [2],l <- [61]]
      
      x_chess n j s 
            | s == 0 = xb n j
            | s == 1 = xw n j
            | s == 2 = 0
            |otherwise = 0
      y_chess n j s
            | s == 0 = yb n j
            | s == 1 = yw n j
            | s == 2 = 0
            | otherwise = 0
      --------------------------------------------------------------------------------------------------------
      tailss 1 a = a
      tailss 2 a = tail a
      tailss m a = tail $ tailss (m-1) a
      
      initss 1 a = a
      initss 2 a = init a
      initss m a = init $ initss (m-1) a
      
      last_n 1 a =[last $ initss 1 a]
      last_n n a =[last $ initss n a] ++ last_n (n-1) a
      
      step_count_white n = if mod n 2 == 0 then div n 2 else div n 2 + 1
      step_count_black n = if mod (n-1) 2 == 0 then div (n-1) 2 + 1 else div n 2 + 1
      chess_list_b_n n = take (2*n-1) chess_list_b
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      com_bwe (x1,y1,z1) (x2,y2,z2) = if x1 == x2 && y1 == y2 && z1 == 2 then (x2,y2,z2)
                                 else if x1 == x2 && y1 == y2 && z2 == 2 then (x1,y1,z1)
                                 else if (x1 /= x2 || y1 /= y2) then (x2,y2,z2)
                                 else error"Repeated move"
      com_bwer (x1,y1,z1) (x2,y2,z2) = if x1 == x2 && y1 == y2 && z1 == 2 then (x2,y2,z1)
                                 else if x1 == x2 && y1 == y2 && z2 == 2 then (x1,y1,z2)
                                 else if (x1 /= x2 || y1 /= y2) then (x2,y2,z2)
                                 else error"Repeated move"
      com_bwet (x1,y1,z1) (x2,y2,z2) = if x1 == x2 && y1 == y2 && z1 == 2 then (x2,y2,z2)
                                 else if x1 == x2 && y1 == y2 && z2 == 2 then (x1,y1,z1)                         
                                 else (x2,y2,z2)                                                    
      chess_empty = [(x,y,s) | x <- [1..15], y <- [1..15] ,s <- [2]]
      
      --chess_com :: (Integer,Int,Integer) -> [(Integer,Int,Integer)] -> [(Integer,Int,Integer)]
      chess_com (x,y,z) b = map (com_bwe (x,y,z)) b
      
      chess_coms [x] b = chess_com x b
      chess_coms (x:xs) b = chess_coms xs (chess_com x b) 
      
      chess_comr (x,y,z) b = map (com_bwer (x,y,z)) b
      
      chess_comrs [x] b = chess_comr x b
      chess_comrs (x:xs) b = chess_comrs xs (chess_comr x b)
      
      chess_comt (x,y,z) b = map (com_bwet (x,y,z)) b
      
      chess_comts [x] b = chess_comt x b
      chess_comts (x:xs) b = chess_comts xs (chess_comt x b)
      
      chess_list_b_s = if lb == 1 then [(xb 1 1,yb 1 1,0)] else [(xb 1 1,yb 1 1,0)] ++ [(x,y,0) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_w_s = [(x,y,1) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      
      chess_list_b_s_n n = take (2*n-1) chess_list_b_s
      chess_list_w_s_n n = take (2*n) chess_list_w_s
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      aaa0_b_0 n j n0 = [(xb n j,y,s) | y <- [(yb n j - (n0-1))..(yb n j + (n0-1))],s <- [0]]
      aaa1_b_0 n j n0 = [(xb n j,y,s) | y <- [(yb n j - (n0-1))..(yb n j + (n0-1))],s <- [1]]
      aaa2_b_0 n j n0 = [(xb n j,y,s) | y <- [(yb n j - (n0-1))..(yb n j + (n0-1))],s <- [2]]
      
      redefine_list_n_0_b n0 n1 m n j = take n1 $ tailss m $ aaa0_b_0 n j n0 --n0 == n1
      redefine_list_n_0_w n0 n1 m n j = take n1 $ tailss m $ aaa1_b_0 n j n0
      redefine_list_n_0_e n0 n1 m n j = take n1 $ tailss m $ aaa2_b_0 n j n0 
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      aaa0_b_90 n j n0 = [(x,yb n j,s) | x <- [(xb n j - (n0-1))..(xb n j + (n0-1))],s <- [0]]
      aaa1_b_90 n j n0 = [(x,yb n j,s) | x <- [(xb n j - (n0-1))..(xb n j + (n0-1))],s <- [1]]
      aaa2_b_90 n j n0 = [(x,yb n j,s) | x <- [(xb n j - (n0-1))..(xb n j + (n0-1))],s <- [2]]
      
      redefine_list_n_90_b n0 n1 m n j = take n1 $ tailss m $ aaa0_b_90 n j n0 --n0 == n1
      redefine_list_n_90_w n0 n1 m n j = take n1 $ tailss m $ aaa1_b_90 n j n0
      redefine_list_n_90_e n0 n1 m n j = take n1 $ tailss m $ aaa2_b_90 n j n0 
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      list_x_black_45 n j n0 = [ x | x <- [(xb n j + (n0-1)),(xb n j + (n0-2))..(xb n j - (n0-1))]]
      list_y_black_45 n j n0 = [ y | y <- [(yb n j-(n0-1))..(yb n j+(n0-1))]]
      redefine_list_b_45 n j n0 = zip (list_x_black_45 n j n0) (list_y_black_45 n j n0)
      
      aaa0_b_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_45 n j n0), s <- [0]]
      aaa1_b_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_45 n j n0), s <- [1]]
      aaa2_b_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_45 n j n0), s <- [2]]
      
      redefine_list_n_45_b n0 n1 m n j = take n1 $ tailss m $ aaa0_b_45 n j n0 --n0 == n1
      redefine_list_n_45_w n0 n1 m n j = take n1 $ tailss m $ aaa1_b_45 n j n0
      redefine_list_n_45_e n0 n1 m n j = take n1 $ tailss m $ aaa2_b_45 n j n0 
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      list_x_black_135 n j n0 = [ x | x <- [(xb n j - (n0-1))..(xb n j + (n0-1))]]
      list_y_black_135 n j n0 = [ y | y <- [(yb n j - (n0-1))..(yb n j + (n0-1))]]
      redefine_list_b_135 n j n0 = zip (list_x_black_135 n j n0) (list_y_black_135 n j n0)
      
      aaa0_b_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_135 n j n0), s <- [0]]
      aaa1_b_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_135 n j n0), s <- [1]]
      aaa2_b_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_b_135 n j n0), s <- [2]]
      
      redefine_list_n_135_b n0 n1 m n j = take n1 $ tailss m $ aaa0_b_135 n j n0 --n0 == n1
      redefine_list_n_135_w n0 n1 m n j = take n1 $ tailss m $ aaa1_b_135 n j n0
      redefine_list_n_135_e n0 n1 m n j = take n1 $ tailss m $ aaa2_b_135 n j n0  
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      aaa0_w_0 n j n0 = [(xw n j,y,s) | y <- [(yw n j - (n0-1))..(yw n j + (n0-1))],s <- [0]]
      aaa1_w_0 n j n0 = [(xw n j,y,s) | y <- [(yw n j - (n0-1))..(yw n j + (n0-1))],s <- [1]]
      aaa2_w_0 n j n0 = [(xw n j,y,s) | y <- [(yw n j - (n0-1))..(yw n j + (n0-1))],s <- [2]]
      
      redefine_list_n_0_b_w n0 n1 m n j = take n1 $ tailss m $ aaa0_w_0 n j n0 --n0 == n1
      redefine_list_n_0_w_w n0 n1 m n j = take n1 $ tailss m $ aaa1_w_0 n j n0
      redefine_list_n_0_e_w n0 n1 m n j = take n1 $ tailss m $ aaa2_w_0 n j n0 
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      aaa0_w_90 n j n0 = [(x,yw n j,s) | x <- [(xw n j - (n0-1))..(xw n j + (n0-1))],s <- [0]]
      aaa1_w_90 n j n0 = [(x,yw n j,s) | x <- [(xw n j - (n0-1))..(xw n j + (n0-1))],s <- [1]]
      aaa2_w_90 n j n0 = [(x,yw n j,s) | x <- [(xw n j - (n0-1))..(xw n j + (n0-1))],s <- [2]]
      
      redefine_list_n_90_b_w n0 n1 m n j = take n1 $ tailss m $ aaa0_w_90 n j n0 --n0 == n1
      redefine_list_n_90_w_w n0 n1 m n j = take n1 $ tailss m $ aaa1_w_90 n j n0
      redefine_list_n_90_e_w n0 n1 m n j = take n1 $ tailss m $ aaa2_w_90 n j n0 
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      list_x_white_45 n j n0 = [ x | x <- [(xw n j + (n0-1)),(xw n j + (n0-2))..(xw n j - (n0-1))]]
      list_y_white_45 n j n0 = [ y | y <- [(yw n j-(n0-1))..(yw n j+(n0-1))]]
      redefine_list_w_white_45 n j n0 = zip (list_x_white_45 n j n0) (list_y_white_45 n j n0)
      
      aaa0_w_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_45 n j n0), s <- [0]]
      aaa1_w_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_45 n j n0), s <- [1]]
      aaa2_w_45 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_45 n j n0), s <- [2]]
      
      redefine_list_n_45_b_w n0 n1 m n j = take n1 $ tailss m $ aaa0_w_45 n j n0 --n0 == n1
      redefine_list_n_45_w_w n0 n1 m n j = take n1 $ tailss m $ aaa1_w_45 n j n0
      redefine_list_n_45_e_w n0 n1 m n j = take n1 $ tailss m $ aaa2_w_45 n j n0
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      list_x_white_135 n j n0 = [ x | x <- [(xw n j-(n0-1))..(xw n j+(n0-1))]]
      list_y_white_135 n j n0 = [ y | y <- [(yw n j-(n0-1))..(yw n j+(n0-1))]]
      redefine_list_w_white_135 n j n0 = zip (list_x_white_135 n j n0) (list_y_white_135 n j n0)
      
      aaa0_w_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_135 n j n0), s <- [0]]
      aaa1_w_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_135 n j n0), s <- [1]]
      aaa2_w_135 n j n0 = [(x,y,s) | (x,y) <- (redefine_list_w_white_135 n j n0), s <- [2]]
      
      redefine_list_n_135_b_w n0 n1 m n j = take n1 $ tailss m $ aaa0_w_135 n j n0 --n0 == n1
      redefine_list_n_135_w_w n0 n1 m n j = take n1 $ tailss m $ aaa1_w_135 n j n0
      redefine_list_n_135_e_w n0 n1 m n j = take n1 $ tailss m $ aaa2_w_135 n j n0  
      -------------------------------------------------------------------------------------------------------------------------------------------------------
      list_b = take (2*lb-1) chess_list_b_s 
      list_w = take (2*lw) chess_list_w_s
      list_com = list_b ++ list_w
      redefine_list_com = chess_coms (list_com) chess_empty
      ------------------------------------------------------------------------------------------------------------------------------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------
      um a b = if a + b == 5 then 0 else b 
      list_um n a = filter (/=0) $ map (um n) a
      
      step_count s = if s == 0 then step_count_black else step_count_white
      
      ext (x,y,z) = if z == 2 then [(x,y)] else []
      
      ext_list [x] = ext x
      ext_list (x:xs) = ext x ++ ext_list xs
      
      shan x y = if x == y then [] else [y]
      shans a [b]= shan a b
      shans a (x:xs) = shan a x ++ shans a xs 
      
      shans_list [a] b = shans a b
      shans_list (x:xs) b =  shans_list xs $ shans x b  
      
      x_s s n j 
        |s == 0 = xb n j
        |s == 1 = xw n j
      y_s s n j
        |s == 0 = yb n j
        |s == 1 = yw n j
      chess_s s n j= (x_s s n j,y_s s n j,s) 
      list_chess s u n j
        |u == 0 = [(x_s s n j+u0,y_s s n j+u0,s)|u0 <- [u]] --huo1 need
        |u >= 1 && u <= 3 =  [(x_s s n j-4+u0,y_s s n j-4+u0,s)|u0<- [u,4]] --2
        |u >= 4 && u <= 6 =  [(x_s s n j-4+u0,y_s s n j-4+u0,s)|u0<- [4,u+1]] --2
      sel_huo u a
        |u == 1 = [list_n 1 a] ++ [list_n 6 a]  
        |u == 2 = [list_n 1 a] ++ [list_n 7 a]
        |u == 3 = [list_n 2 a] ++ [list_n 7 a]
      u_list_sel_huo m 
        |m == 1 = [1]
        |m == 2 = [2,3]
        |m >=3 && m <= 5 = [1..3]
        |m == 6 = [1,2]
        |m == 7 = [3]
        
      mu_position_huo5_list = [(m,u)|m <-[1..7],u <- u_list_sel_huo m] 
      x_take (x,y) = x
      y_take (x,y) = y  
      ----------------------------------------------------------------------------------------------------huo  
      menglong_list a = [list_n 1 a] ++ [list_n 3 a] ++ [list_n 5 a] 
      ---------------------------------------------------------------------------------------------------------menglong
      sel_mian v 
        |v == 1 = 8
        |v == 2 = 0
        |v == 3 = 1
      sel1_mian v
        |v == 1 = 1
        |v == 2 = 0
        |v == 3 = 8
      sel_list_take n a = if n > 0 then [list_n n a] else take n a
      sel1_list_take n a = if n > 0 then [list_n n a] else [list_n 1 a] ++ [list_n 8 a]
      list5_n n a = take 5 $ drop (5*n-5) a
      
      empty_lists = [(x,y) | x <- [1..15] ,y<- [1..15]] 
      --rem_boa :: (Int, Int, c) -> [(Int, Int, c)]
      rem_boa (x1,y1,z1) = if everyonein [(x1,y1)] empty_lists then [(x1,y1,z1)] else []  
      rembs [x] = rem_boa x
      rembs (x:xs) = rem_boa x ++ rembs xs
      ppp=[(2,-4,1),(3,-3,0),(4,-2,0),(5,-1,0),(6,0,0),(7,1,2),(8,2,0)]
      rembs_n n a = rembs (take n a ++ last_n n a) 
      chongzu n a = rembs (take n a ++ last_n n a) ++ shans_list (take n a ++ last_n n a) a
      list_uv v 
        |v == 1 = [1..4]
        |v == 2 = [3,4]
        |v == 3 = [3..6]
      
      find_uv_mian4 a = positions 1 a
      find_u_mian4 a = if mod (head $ find_uv_mian4 a) 5 == 0 then div (head $ find_uv_mian4 a) 5 else div (head $ find_uv_mian4 a) 5 + 1
      find_u1_mian4 a = list_n (head $ positions 1 $ list5_n (find_u_mian4 a) a) (shans (find_u_mian4 a) [1..6])
      
      find_uv_mian3 a = if (length $ positions 1 a) >= 1 then head $ positions 1 a else 0
      find_v_mian3 a 
        |find_uv_mian3 a >= 1 && find_uv_mian3 a <= 80 = 1
        |find_uv_mian3 a >= 81 && find_uv_mian3 a <= 120 = 2
        |find_uv_mian3 a >= 121 && find_uv_mian3 a <= 200 = 3
      
      find_u_mian3 v a 
        |v == 1 = if mod (find_uv_mian3 a) 20 == 0 then div (find_uv_mian3 a) 20 else div (find_uv_mian3 a) 20 + 1
        |v == 2 = if mod (find_uv_mian3 a) 20 == 0 then div (find_uv_mian3 a) 20 - 2 else div (find_uv_mian3 a) 20 - 1
        |v == 3 = if mod (find_uv_mian3 a) 20 == 0 then div (find_uv_mian3 a) 20 - 4 else div (find_uv_mian3 a) 20 - 3
      does_find_u1_mian3 a = if mod (mod (find_uv_mian3 a) 20) 4 == 0 then div (mod (find_uv_mian3 a) 20) 4 else div (mod (find_uv_mian3 a) 20) 4 + 1
      find_u1_mian3 a = list_n (does_find_u1_mian3 a) (shans (find_u_mian3  (find_v_mian3 a) a) [1..6])
      does_find_u2_mian3 a = if mod (mod (find_uv_mian3 a) 20) 4 == 0 then mod (mod (find_uv_mian3 a) 20) 4 + 4 else mod (mod (find_uv_mian3 a) 20) 4
      find_u2_mian3 a = list_n (does_find_u2_mian3 a) (shans_list [find_u_mian3 (find_v_mian3 a) a,find_u1_mian3 a] [1..6])
      --------------------------------------------------------------------------------------------------------mian
      ------------------------------------------------------------------------------------------------------
      aaa0_b a n j n0 
        |a == 1 = aaa0_b_135 n j n0
        |a == 2 = aaa0_b_45 n j n0
        |a == 3 = aaa0_b_90 n j n0
        |a == 4 = aaa0_b_0 n j n0
      aaa1_b a n j n0
        |a == 1 = aaa1_b_135 n j n0
        |a == 2 = aaa1_b_45 n j n0
        |a == 3 = aaa1_b_90 n j n0
        |a == 4 = aaa1_b_0 n j n0
      aaa2_b a n j n0
        |a == 1 = aaa2_b_135 n j n0
        |a == 2 = aaa2_b_45 n j n0
        |a == 3 = aaa2_b_90 n j n0
        |a == 4 = aaa2_b_0 n j n0
      redefine_list_n_b a n0 n1 m n j
        |a == 1 = redefine_list_n_135_b n0 n1 m n j
        |a == 2 = redefine_list_n_45_b n0 n1 m n j
        |a == 3 = redefine_list_n_90_b n0 n1 m n j
        |a == 4 = redefine_list_n_0_b n0 n1 m n j  
      
      redefine_list_n_w a n0 n1 m n j
        |a == 1 = redefine_list_n_135_w n0 n1 m n j
        |a == 2 = redefine_list_n_45_w n0 n1 m n j
        |a == 3 = redefine_list_n_90_w n0 n1 m n j
        |a == 4 = redefine_list_n_0_w n0 n1 m n j 
      
      redefine_list_n_e a n0 n1 m n j
        |a == 1 = redefine_list_n_135_e n0 n1 m n j
        |a == 2 = redefine_list_n_45_e n0 n1 m n j
        |a == 3 = redefine_list_n_90_e n0 n1 m n j
        |a == 4 = redefine_list_n_0_e n0 n1 m n j 
      ----------------------------------------------
      aaa0_w a n j n0 
        |a == 1 = aaa0_w_135 n j n0
        |a == 2 = aaa0_w_45 n j n0
        |a == 3 = aaa0_w_90 n j n0
        |a == 4 = aaa0_w_0 n j n0
      aaa1_w a n j n0
        |a == 1 = aaa1_w_135 n j n0
        |a == 2 = aaa1_w_45 n j n0
        |a == 3 = aaa1_w_90 n j n0
        |a == 4 = aaa1_w_0 n j n0
      aaa2_w a n j n0
        |a == 1 = aaa2_w_135 n j n0
        |a == 2 = aaa2_w_45 n j n0
        |a == 3 = aaa2_w_90 n j n0
        |a == 4 = aaa2_w_0 n j n0
      
      redefine_list_n_b_w a n0 n1 m n j
        |a == 1 = redefine_list_n_135_b_w n0 n1 m n j
        |a == 2 = redefine_list_n_45_b_w n0 n1 m n j
        |a == 3 = redefine_list_n_90_b_w n0 n1 m n j
        |a == 4 = redefine_list_n_0_b_w n0 n1 m n j
      
      redefine_list_n_w_w a n0 n1 m n j
        |a == 1 = redefine_list_n_135_w_w n0 n1 m n j
        |a == 2 = redefine_list_n_45_w_w n0 n1 m n j
        |a == 3 = redefine_list_n_90_w_w n0 n1 m n j
        |a == 4 = redefine_list_n_0_w_w n0 n1 m n j
      
      redefine_list_n_e_w a n0 n1 m n j
        |a == 1 = redefine_list_n_135_e_w n0 n1 m n j
        |a == 2 = redefine_list_n_45_e_w n0 n1 m n j
        |a == 3 = redefine_list_n_90_e_w n0 n1 m n j
        |a == 4 = redefine_list_n_0_e_w n0 n1 m n j
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      menglong s a q m n j
        |s == 0 && q == 3 = chess_coms (menglong_list $ redefine_list_n_b a (q+2) (q+2) m n j) (redefine_list_n_e a (q+4) (q+4) (m+1) n j)
        |s == 1 && q == 3 = chess_coms (menglong_list $ redefine_list_n_w_w a (q+2) (q+2) m n j) (redefine_list_n_e_w a (q+4) (q+4) (m+1) n j)
        |otherwise = []
      
      judge_menglong s a q m n j = if everyonein (menglong s a q m n j) (redefine_list_com) then 1 else 0
      
      judge_menglong_list_m s a q n j
        |q == 3 = [judge_menglong s a q m n j|m <- [1,3,5]]
        |otherwise = []
      
      does_judge_menglong_list_m s a q n j = if (length $ positions 1 $ judge_menglong_list_m s a q n j) >= 1 then 1 else 0
      
      judge_menglong_list_mnj s a q 
        |s == 0 = [does_judge_menglong_list_m s a q n j| n <- [1],j <- [1]] ++ [does_judge_menglong_list_m s a q n j|n <- [2..lb],j <- [1,2]]
        |s == 1 = [does_judge_menglong_list_m s a q n j|n <- [1..lw],j <- [1,2]]
      
      nj_menglong s a q
        |q == 3 = if (length $ positions 1 $ judge_menglong_list_mnj s a q) >= 1 then head $ positions 1 $ judge_menglong_list_mnj s a q else 0
        |otherwise = 0
      
      n_menglong s a q = if nj_menglong s a q <= 0 then 0 else step_count s $ nj_menglong s a q
      
      j_menglong s a q 
        |s == 0 = if nj_menglong s a q <= 0 then 0 else if nj_menglong s a q == 1 then 1 else if mod (nj_menglong s a q) 2 == 0 then 1 else 2
        |s == 1 = if nj_menglong s a q <= 0 then 0 else if mod (nj_menglong s a q) 2 == 0 then 2 else 1 
      
      m_menglong s a q
        |q == 3 = if nj_menglong s a q <= 0 then 0 else if (length $ positions 1 $ judge_menglong_list_m s a q (n_menglong s a q) (j_menglong s a q)) >= 1 then 2 * (head $ positions 1 $ judge_menglong_list_m s a q (n_menglong s a q) (j_menglong s a q)) - 1 else 0
        |otherwise = 0
      
      zhenxia_menglong s a q 
        |q == 3 = if nj_menglong s a q >= 1 then take 2 $ ext_list $ tail $ menglong s a q (m_menglong s a q) (n_menglong s a q) (j_menglong s a q) else []
        |otherwise = []  
      -----------------------------------------------------------------------------------------------
      
      
      
      huo s a u q m n j  
        |s == 0 && q == 5 = chess_comrs (sel_huo u (redefine_list_n_e a (q+2) (q+2) m n j)) (redefine_list_n_b a (q+2) (q+2) m n j)
        |s == 0 && q == 4 = chess_coms (redefine_list_n_b a q q m n j) (redefine_list_n_e a (q+4) (q+4) (m+2) n j)
        |s == 0 && q == 3 = chess_coms (chess_comrs [list_n u (redefine_list_n_e a (q+1) (q+1) m n j)] (redefine_list_n_b a (q+1) (q+1) m n j)) (redefine_list_n_e a (q+5) (q+5) (m+2) n j)
        |s == 0 && q == 2 = chess_coms (chess_coms (list_chess s u n j) (redefine_list_n_e a (q+2) (q+2) m n j)) (redefine_list_n_e a (q+6) (q+6) (m+2) n j)
        |s == 0 && q == 1 = chess_coms (chess_coms (list_chess s u n j) (redefine_list_n_e a (q+3) (q+3) m n j)) (redefine_list_n_e a (q+7) (q+7) (m+2) n j)
        
        |s == 1 && q == 5 = chess_comrs (sel_huo u (redefine_list_n_e_w a (q+2) (q+2) m n j)) (redefine_list_n_w_w a (q+2) (q+2) m n j)
        |s == 1 && q == 4 = chess_coms (redefine_list_n_w_w a q q m n j) (redefine_list_n_e_w a (q+4) (q+4) (m+2) n j)
        |s == 1 && q == 3 = chess_coms (chess_comrs [list_n u (redefine_list_n_e_w a (q+1) (q+1) m n j)] (redefine_list_n_w_w a (q+1) (q+1) m n j)) (redefine_list_n_e_w a (q+5) (q+5) (m+2) n j)
        |s == 1 && q == 2 = chess_coms (chess_coms (list_chess s u n j) (redefine_list_n_e_w a (q+2) (q+2) m n j)) (redefine_list_n_e_w a (q+6) (q+6) (m+2) n j)
        |s == 1 && q == 1 = chess_coms (chess_coms (list_chess s u n j) (redefine_list_n_e_w a (q+3) (q+3) m n j)) (redefine_list_n_e_w a (q+7) (q+7) (m+2) n j)
      
      
      judge_huo s a u q m n j = if everyonein (huo s a u q m n j) (redefine_list_com) then 1 else 0
      
      --judge_nj_huo s a q x = if (length $ positions 1 $ list_n x $ judge_huo_list_mnj s a q) >= 1 then x else 0
      
      judge_huo_list_m s a u q n j
        |q == 4 = [judge_huo s a u q m n j|m <- [1..q]]
        |q == 3 = [judge_huo s a u q m n j|m <- (list_um u [1..4])]
        |q == 2 && u >= 1 && u <= 3 = [judge_huo s a u q m n j|m <- [1..u]]
        |q == 2 && u >= 4 && u <= 6 = [judge_huo s a u q m n j|m <- [u-2..4]]
        |q == 1 && u == 0 = [judge_huo s a u q m n j|m <- [1..4]]
      
      judge_huo_list_ms s a u q n j = if (length $ positions 1 $ judge_huo_list_m s a u q n j) >= 1 then 1 else 0
      
      judge_huo_list_m_us s a q n j
        |q == 5 = [judge_huo s a u q m n j|m <-[1..7],u <- u_list_sel_huo m]
        |q == 3 = [judge_huo_list_ms s a u0 q n j|u0 <- [1..4]]
        |q == 2 = [judge_huo_list_ms s a u0 q n j|u0 <- [1..3]] ++ [judge_huo_list_ms s a u0 q n j|u0 <- [4..6]]
        |otherwise = [judge_huo_list_ms s a u0 q n j|u0 <- [0]]
      --taio zheng huo 5
      judge_huo_list_m_u s a q n j 
        |q == 3 = [judge_huo_list_m s a u0 q n j|u0 <- [1..4]]
        |q == 2 = [judge_huo_list_m s a u0 q n j|u0 <- [1..3]] ++ [judge_huo_list_m s a u0 q n j|u0 <- [4..6]]
        |otherwise = [judge_huo_list_m s a u0 q n j|u0 <- [0]]
        
      does_judge_huo_m_u s a q i n j = if (length $ positions 1 (list_n i (judge_huo_list_m_u s a q n j))) >= 1 then i else 0 
      u_judge_huo_list_m_u s a q n j
        | q == 3 = if (length $ filter (>=1) [ does_judge_huo_m_u s a q i n j | i <- [1..4]])  >= 1 then head $ filter (>=1) [ does_judge_huo_m_u s a q i n j | i <- [1..4]] else 0
        | q == 2 = if (length $ filter (>=1) [ does_judge_huo_m_u s a q i n j | i <- [1..6]])  >= 1 then head $ filter (>=1) [ does_judge_huo_m_u s a q i n j | i <- [1..6]] else 0
        | otherwise = 0
      
      judge_huo_list_mnj s a q 
        |s == 0 = [judge_huo_list_m_us s a q 1 1] ++ [judge_huo_list_m_us s a q n j|n <- [2..lb],j <-[1,2]]
        |s == 1 = [judge_huo_list_m_us s a q n j|n <- [1..lw],j <-[1,2]]
         
      judge_nj_huo s a q x = if (length $ positions 1 $ list_n x $ judge_huo_list_mnj s a q) >= 1 then x else 0
      
      nj_huo s a q 
        |s == 0 = if (length $ filter (>=1) [judge_nj_huo s a q x | x <- [1..2*lb-1]]) >= 1 then head $ filter (>=1) [judge_nj_huo s a q x | x <- [1..2*lb-1]] else 0
        |s == 1 = if (length $ filter (>=1) [judge_nj_huo s a q x | x <- [1..2*lw]]) >= 1 then head $ filter (>=1) [judge_nj_huo s a q x | x <- [1..2*lw]] else 0
      
      n_huo s a q = step_count s $ nj_huo s a q 
      
      j_huo s a q 
        |s == 0 = if nj_huo s a q == 1 then 1 else if mod (nj_huo s a q) 2 == 0 then 1 else 2
        |s == 1 = if mod (nj_huo s a q) 2 == 0 then 2 else 1
      
      mu_list s a q = if (length $ positions 1 $ judge_huo_list_m_us s a q (n_huo s a q) (j_huo s a q)) >= 1 then head $ positions 1 $ judge_huo_list_m_us s a q (n_huo s a q) (j_huo s a q) else 0
      
      u_huo s a q 
        |q == 5 = y_take $ list_n (mu_list s a q) mu_position_huo5_list
        |q == 3 = if (length $ filter (>=1) [ does_judge_huo_m_u s a q i (n_huo s a q) (j_huo s a q) | i <- [1..4]])>=1 then head $ filter (>=1) [ does_judge_huo_m_u s a q i (n_huo s a q) (j_huo s a q) | i <- [1..4]] else 0
        |q == 2 = if (length $ filter (>=1) [ does_judge_huo_m_u s a q i (n_huo s a q) (j_huo s a q) | i <- [1..6]])>=1 then head $ filter (>=1) [ does_judge_huo_m_u s a q i (n_huo s a q) (j_huo s a q) | i <- [1..6]] else 0
        |otherwise = 0
         
      m_huo s a q 
        |q == 5 = x_take $ list_n (mu_list s a q) mu_position_huo5_list
        |q == 3 || q == 2 = if (length $ positions 1 (list_n (u_judge_huo_list_m_u s a q (n_huo s a q) (j_huo s a q)) (judge_huo_list_m_u s a q (n_huo s a q) (j_huo s a q)))) >= 1 then head $ positions 1 (list_n (u_judge_huo_list_m_u s a q (n_huo s a q) (j_huo s a q)) (judge_huo_list_m_u s a q (n_huo s a q) (j_huo s a q))) else 0
        |otherwise = if (length $ positions 1 (judge_huo_list_m s a 0 q (n_huo s a q) (j_huo s a q))) >= 1 then head $ positions 1 (judge_huo_list_m s a 0 q (n_huo s a q) (j_huo s a q)) else 0
      
      middle4 s a u q m n j 
        |s == 0 && q == 5 = chess_comrs (sel_huo u (redefine_list_n_e a (q+2) (q+2) m n j)) (redefine_list_n_b a (q+2) (q+2) m n j)
        |s == 0 && q == 4 = chess_coms (redefine_list_n_b a q q m n j) (redefine_list_n_e a (q+2) (q+2) (m+1) n j)
        |s == 0 && q == 3 = chess_comrs [list_n u (redefine_list_n_e a (q+1) (q+1) m n j)] (redefine_list_n_b a (q+1) (q+1) m n j)
        |s == 0 && q == 2 = chess_coms (list_chess s u n j) (redefine_list_n_e a (q+2) (q+2) m n j)
        |s == 0 && q == 1 = chess_coms (list_chess s u n j) (redefine_list_n_e a (q+3) (q+3) m n j)
        
        |s == 1 && q == 5 = chess_comrs (sel_huo u (redefine_list_n_e_w a (q+2) (q+2) m n j)) (redefine_list_n_w_w a (q+2) (q+2) m n j)
        |s == 1 && q == 4 = chess_coms (redefine_list_n_w_w a q q m n j) (redefine_list_n_e_w a (q+4) (q+4) (m+2) n j)
        |s == 1 && q == 3 = chess_comrs [list_n u (redefine_list_n_e_w a (q+1) (q+1) m n j)] (redefine_list_n_w_w a (q+1) (q+1) m n j)  
        |s == 1 && q == 2 = chess_coms (list_chess s u n j) (redefine_list_n_e_w a (q+2) (q+2) m n j)  
        |s == 1 && q == 1 = chess_coms (list_chess s u n j) (redefine_list_n_e_w a (q+3) (q+3) m n j)  
         
      xiaqi4 s a q = middle4 s a (u_huo s a q) q (m_huo s a q) (n_huo s a q) (j_huo s a q)
      zhenxia s a q 
        |q == 3 = if nj_huo s a q >= 1 then take 1 $ ext_list $ xiaqi4 s a q else []
        |otherwise = if nj_huo s a q >= 1 then take 2 $ ext_list $ xiaqi4 s a q else []
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      bbbbb_5 a u q m n j =  chess_comrs [list_n u (redefine_list_n_e a (q+1) (q+1) m n j)] (redefine_list_n_b a (q+1) (q+1) m n j)
      bbbbb_4 a u u1 q m n j = chess_comrs [list_n u1 $ (redefine_list_n_e a (q+2) (q+2) m n j)] (bbbbb_5 a u (q+1) m n j)
      bbbbb_1 a u q n j = chess_coms [list_n (7-u) (redefine_list_n_b a (q+5) (q+5) u n j)] (redefine_list_n_e a (q+5) (q+5) (7-u) n j)
      bbbbb_2 a u u1 q n j = chess_coms [list_n u1 $ (redefine_list_n_b a (q+4) (q+4) (7-u) n j)] (bbbbb_1 a u (q-1) n j)
      bbbbb_3 a u u1 u2 q n j = chess_comts [list_n u2 $ (redefine_list_n_b a (q+3) (q+3) (7-u) n j)] (bbbbb_2 a u u1 (q-1) n j)
      wwwww_5 a u q m n j =  chess_comrs [list_n u (redefine_list_n_e_w a (q+1) (q+1) m n j)] (redefine_list_n_w_w a (q+1) (q+1) m n j)
      wwwww_4 a u u1 q m n j = chess_comrs [list_n u1 $ (redefine_list_n_e_w a (q+2) (q+2) m n j)] (wwwww_5 a u (q+1) m n j)
      wwwww_1 a u q n j = chess_coms [list_n (7-u) (redefine_list_n_w_w a (q+5) (q+5) u n j)] (redefine_list_n_e_w a (q+5) (q+5) (7-u) n j)
      wwwww_2 a u u1 q n j = chess_coms [list_n u1 $ (redefine_list_n_w_w a (q+4) (q+4) (7-u) n j)] (wwwww_1 a u (q-1) n j)
      wwwww_3 a u u1 u2 q n j = chess_comts [list_n u2 $ (redefine_list_n_w_w a (q+3) (q+3) (7-u) n j)] (wwwww_2 a u u1 (q-1) n j)
      
      mian s a v u u1 u2 q m n j
        |s == 0 && q == 5 = chongzu 1 (sel_list_take (sel_mian v) (redefine_list_n_w a (q+3) (q+3) (m+1) n j) ++ chess_comrs [list_n u (redefine_list_n_e a (q+1) (q+1) m n j)] (redefine_list_n_b a (q+1) (q+1) m n j))
        |s == 0 && q == 4 = chongzu 1 (sel_list_take (sel_mian v) (redefine_list_n_w a (q+4) (q+4) (m+1) n j) ++ chess_comrs [list_n u1 $ (redefine_list_n_e a (q+2) (q+2) m n j)] (bbbbb_5 a u (q+1) m n j))
        |s == 0 && q == 3 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_w a (q+5) (q+5) (8-u) n j) ++ chess_comts [list_n u2 $ (redefine_list_n_b a (q+3) (q+3) (7-u) n j)] (bbbbb_2 a u u1 (q-1) n j))
        |s == 0 && q == 2 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_w a (q+6) (q+6) (8-u) n j) ++ chess_coms [list_n u1 $ (redefine_list_n_b a (q+4) (q+4) (7-u) n j)] (bbbbb_1 a u (q-1) n j))
        |s == 0 && q == 1 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_w a (q+7) (q+7) (8-u) n j) ++ chess_coms [list_n (7-u) (redefine_list_n_b a (q+5) (q+5) u n j)] (redefine_list_n_e a (q+5) (q+5) (7-u) n j))
        
        |s == 1 && q == 5 = chongzu 1 (sel_list_take (sel_mian v) (redefine_list_n_b_w a (q+3) (q+3) (m+1) n j) ++ chess_comrs [list_n u (redefine_list_n_e_w a (q+1) (q+1) m n j)] (redefine_list_n_w_w a (q+1) (q+1) m n j))
        |s == 1 && q == 4 = chongzu 1 (sel_list_take (sel_mian v) (redefine_list_n_b_w a (q+4) (q+4) (m+1) n j) ++ chess_comrs [list_n u1 $ (redefine_list_n_e_w a (q+2) (q+2) m n j)] (wwwww_5 a u (q+1) m n j))
        |s == 1 && q == 3 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_b_w a (q+5) (q+5) (8-u) n j) ++ chess_comts [list_n u2 $ (redefine_list_n_w_w a (q+3) (q+3) (7-u) n j)] (wwwww_2 a u u1 (q-1) n j))
        |s == 1 && q == 2 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_b_w a (q+6) (q+6) (8-u) n j) ++ chess_coms [list_n u1 $ (redefine_list_n_w_w a (q+4) (q+4) (7-u) n j)] (wwwww_1 a u (q-1) n j))
        |s == 1 && q == 1 = chongzu 1 (sel1_list_take (sel1_mian v) (redefine_list_n_b_w a (q+7) (q+7) (8-u) n j) ++ chess_coms [list_n (7-u) (redefine_list_n_w_w a (q+5) (q+5) u n j)] (redefine_list_n_e_w a (q+5) (q+5) (7-u) n j))
      
      judge_mian s a v u u1 u2 q m n j = if everyonein (mian s a v u u1 u2 q m n j) (redefine_list_com) then 1 else 0 
      
      judge_mian_list_uv s a q m n j
        |q == 5 || q == 1 = [judge_mian s a v u u1 u2 q m n j|v <- [1..3],u <- [2*v-1,2*v],u1 <- [0],u2 <- [0]]
        |q == 4 || q == 2 = [judge_mian s a v u u1 u2 q m n j|v <- [1..3],u <- [2*v-1,2*v],u1 <- shans u [1..6],u2 <- [0]]
        |q == 3 = [judge_mian s a v u u1 u2 q m n j|v <- [1..3],u <- list_uv v,u1 <- shans u [1..6],u2 <- shans_list [u,u1] [1..6]]
      
      judge_mian_list_uvm s a q n j
        |q == 5 = [judge_mian_list_uv s a q m n j|m <-[1..6]]
        |q == 4 = [judge_mian_list_uv s a q m n j|m <-[1..6]]
        |otherwise = [judge_mian_list_uv s a q m n j|m <-[0]]
        
      judge_mian_list_uvf s a q m n j = if (length $ positions 1 $ judge_mian_list_uv s a q m n j) >= 1 then 1 else 0
      
      judge_mian_list_uvmf s a q n j
        |q == 5 = [judge_mian_list_uvf s a q m n j|m <-[1..6]]
        |q == 4 = [judge_mian_list_uvf s a q m n j|m <-[1..6]]
        |otherwise = [judge_mian_list_uvf s a q m n j|m <-[0]]
      
      dose_judge_mian_list_uvmf s a q n j = if (length $ positions 1 $ judge_mian_list_uvmf s a q n j) >= 1 then 1 else 0
      
      judge_mian_list_mnj s a q 
        |s == 0 = [dose_judge_mian_list_uvmf s a q 1 1] ++ [dose_judge_mian_list_uvmf s a q n j|n <- [2..lb],j <-[1,2]]
        |s == 1 = [dose_judge_mian_list_uvmf s a q n j|n <- [1..lw],j <-[1,2]]
        
      nj_mian s a q = if (length $ positions 1 $ judge_mian_list_mnj s a q) >= q then head $ positions 1 $ judge_mian_list_mnj s a q else 0
        
      n_mian s a q = step_count s $ nj_mian s a q
      j_mian s a q 
        |s == 0 = if nj_mian s a q == 1 then 1 else if mod (nj_mian s a q) 2 == 0 then 1 else 2
        |s == 1 = if mod (nj_mian s a q) 2 == 0 then 2 else 1
      
      m_mian s a q 
        |q == 5 = if (length $ positions 1 $ judge_mian_list_uvmf s a q (n_mian s a q) (j_mian s a q)) >= 1 then head $ positions 1 $ judge_mian_list_uvmf s a q (n_mian s a q) (j_mian s a q) else 0
        |q == 4 = if (length $ positions 1 $ judge_mian_list_uvmf s a q (n_mian s a q) (j_mian s a q)) >= 1 then head $ positions 1 $ judge_mian_list_uvmf s a q (n_mian s a q) (j_mian s a q) else 0
        |otherwise = 0
      
      u_mian s a q
        |q == 5 = if (length $ positions 1 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q)) >= 1 then head $ positions 1 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q) else 0
        |q == 4 = if (length $ positions 1 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q)) >= 1 then find_u_mian4 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q) else 0
        |q == 3 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_u_mian3 (find_v_mian3 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) (judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) else 0
        |q == 2 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_u_mian4 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
        |q == 1 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then head $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
      
      u1_mian s a q
        |q == 5 || q == 1 = 0
        |q == 4 = if (length $ positions 1 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q)) >= 1 then find_u1_mian4 $ judge_mian_list_uv s a q (m_mian s a q) (n_mian s a q) (j_mian s a q) else 0
        |q == 3 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_u1_mian3 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
        |q == 2 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_u1_mian4 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
      
      u2_mian s a q
        |q == 3 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_u2_mian3 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
        |otherwise = 0  
        
      v_mian s a q
        |q == 5 || q == 4 || q == 1 || q == 2 = if mod (u_mian s a q) 2 == 0 then div (u_mian s a q) 2 else div (u_mian s a q) 2 + 1
        |q == 3 = if (length $ positions 1 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q)) >= 1 then find_v_mian3 $ judge_mian_list_uv s a q 0 (n_mian s a q) (j_mian s a q) else 0
    
      zhenmian s a q 
        |s == 0 && q == 5 = if nj_mian s a q >= 1 then ext_list $ bbbbb_5 a (u_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 4 = if nj_mian s a q >= 1 then ext_list $ bbbbb_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 3 = if nj_mian s a q >= 1 then take 2 $ ext_list $ bbbbb_3 a (u_mian s a q) (u1_mian s a q) (u2_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 2 = if nj_mian s a q >= 1 then take 2 $ ext_list $ bbbbb_2 a (u_mian s a q) (u1_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 1 = if nj_mian s a q >= 1 then take 2 $ ext_list $ bbbbb_1 a (u_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 5 = if nj_mian s a q >= 1 then ext_list $ wwwww_5 a (u_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 4 = if nj_mian s a q >= 1 then ext_list $ wwwww_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 3 = if nj_mian s a q >= 1 then take 2 $ ext_list $ wwwww_3 a (u_mian s a q) (u1_mian s a q) (u2_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 2 = if nj_mian s a q >= 1 then take 2 $ ext_list $ wwwww_2 a (u_mian s a q) (u1_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 1 = if nj_mian s a q >= 1 then take 2 $ ext_list $ wwwww_1 a (u_mian s a q) q (n_mian s a q) (j_mian s a q) else []
      
      
      
      
       
      dumian s a q
        |s == 0 && q == 5 = if nj_mian s a q >= 1 then ext_list $ bbbbb_5 a (u_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 4 = if nj_mian s a q >= 1 && u_mian s a q == 4 && u1_mian s a q == 6 then [head $ ext_list $ bbbbb_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else if nj_mian s a q >= 1 && u_mian s a q == 3 && u1_mian s a q == 1 then [last $ ext_list $ bbbbb_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else if nj_mian s a q >= 1 then [head $ ext_list $ bbbbb_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else []
        |s == 0 && q == 3 = if nj_mian s a q >= 1 then take 1 $ ext_list $ bbbbb_3 a (u_mian s a q) (u1_mian s a q) (u2_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 2 = if nj_mian s a q >= 1 then take 2 $ ext_list $ bbbbb_2 a (u_mian s a q) (u1_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 0 && q == 1 = if nj_mian s a q >= 1 then take 2 $ ext_list $ bbbbb_1 a (u_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 5 = if nj_mian s a q >= 1 then ext_list $ wwwww_5 a (u_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 4 = if nj_mian s a q >= 1 && u_mian s a q == 4 && u1_mian s a q == 6 then [head $ ext_list $ wwwww_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else if nj_mian s a q >= 1 && u_mian s a q == 3 && u1_mian s a q == 1 then [last $ ext_list $ wwwww_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else if nj_mian s a q >= 1 then [head $ ext_list $ wwwww_4 a (u_mian s a q) (u1_mian s a q) q (m_mian s a q) (n_mian s a q) (j_mian s a q)] else []
        |s == 1 && q == 3 = if nj_mian s a q >= 1 then take 1 $ ext_list $ wwwww_3 a (u_mian s a q) (u1_mian s a q) (u2_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 2 = if nj_mian s a q >= 1 then take 2 $ ext_list $ wwwww_2 a (u_mian s a q) (u1_mian s a q) q (n_mian s a q) (j_mian s a q) else []
        |s == 1 && q == 1 = if nj_mian s a q >= 1 then take 2 $ ext_list $ wwwww_1 a (u_mian s a q) q (n_mian s a q) (j_mian s a q) else []
      
--      wins s = [nj_lian s a q| a <- [1..4],q <- [6,7]]
--      winss s = length $ filter (>=1) $ wins s
      --pan duan shu ying
      ookk s z a q 
        |z == 1 = nj_huo s a q
        |z == 2 = nj_mian s a q
        |z == 3 = nj_menglong s a q
      
      xiaqiqi s z a q
        |z == 1 = zhenxia s a q 
        |z == 2 = if q /= 4 then zhenmian s a q else dumian s a q
        |z == 3 = zhenxia_menglong s a q
      
      
      ookkf s z a q = if ookk s z a q >= 1 then 1 else 0
      
      ookkl s z q = [ookk s z a q|a <-[1..4]]
      ookklf s z q = [ookkf s z a q|a <-[1..4]]
      ------------------------------------------------------hei qi pan duan luo zi
      
      a_ookk s z q = if (length $ positions 1 $ ookklf s z q) >= 1 then head $ positions 1 $ ookklf s z q else 0
      
      nj_ookk s z q = if (length $ positions 1 $ ookklf s z q) >= 1 then head $ filter (>=1) $ ookkl s z q else 0
      
      
      ttt s z q  = xiaqiqi s z (a_ookk s z q) q
      sss z q = if q == 5 then 1 else 0
      
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------       

 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      www = if nj_ookk 1 1 5 >= 1 then ttt 1 1 5                                                             --xia
       else if nj_ookk 1 1 4 >= 1 then ttt 1 1 4                                                             --xia    
       else if nj_ookk 1 2 5 >= 1 then ttt 1 2 5                                                             --xia
       else if nj_ookk 1 2 4 >= 1 then ttt 1 2 4                                                             --xia   
       else if nj_ookk 0 1 5 >= 1 then ttt 0 1 5                                                             -- w huo 5  du
       else if nj_ookk 0 1 4 >= 1 then ttt 0 1 4                                                             -- w huo 4  du  
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 2 4 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 2 4)    --b mian 5 && b mian 4
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 1 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 1 3)    --b mian 5 && w huo 3
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 3 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 3 3)    --                  menglong 3
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 2 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 2 3)    --                  mian 3 
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 1 2 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 1 2)    --                  huo 2
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 2 2 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 2 2)    --                  mian 2  
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 1 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 1 3)    --b mian 5 && b huo 3
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 3 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 3 3)    --                  menglong 3
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 2 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 2 3)    --                  mian 3
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 1 2 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 1 2)    --                  huo 2
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 2 2 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 2 2)    --                  mian 2
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 1 1)    --b mian 5 && w huo 1     
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 1 2 1 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 2 1)    --                  mian 1
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 1 1 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 1 1)    --b mian 5 && b huo 1
       else if nj_ookk 0 2 5 >= 1 && nj_ookk 0 2 1 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 2 1)    --                  mian 1
      --b mian 4 mian 4 
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 1 3 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 1 3)    --b mian 4 && w huo 3
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 3 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 1 3 3)    --                  menglong 3 
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 2 3 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 2 3)    --                  mian 3 
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 1 2 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 1 2)    --                  huo 2
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 2 2 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 2 2)    --                  mian 2  
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 1 3 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 1 3)    --b mian 4 && b huo 3
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 3 3 >= 1 then (take 1 $ ttt 0 2 5) ++ (take 1 $ ttt 0 3 3)    --                  menglong 3 
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 2 3 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 2 3)    --                  mian 3
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 1 2 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 1 2)    --                  huo 2
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 2 2 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 2 2)    --                  mian 2
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 1 1)    --b mian 4 && w huo 1     
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 1 2 1 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 1 2 1)    --                  mian 1
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 1 1 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 1 1)    --b mian 4 && b huo 1
       else if nj_ookk 0 2 4 >= 1 && nj_ookk 0 2 1 >= 1 then (take 1 $ ttt 0 2 4) ++ (take 1 $ ttt 0 2 1)    --                  mian 1
      --w huo 3 huo 3 
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 3 3 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 3 3)    -- w huo 3 && w menglong 3
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 2 3 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 2 3)    -- w huo 3 && w mian 3
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 1 2 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 1 2)    --                     huo 2
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 0 1 3 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 0 1 3)    --                 b huo 3 
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 0 2 3 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 0 2 3)    --                     mian 3
      --b huo 3 huo 3 
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 3 3 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 3 3)    -- b huo 3 && b menglong 3 
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 2 3 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 2 3)    -- b huo 3 && b mian 3
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 1 2 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 1 2)    --                  huo 2   
        
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 2 2 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 2 2)    -- w huo3 &&  w  mian 2
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 2 2 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 2 2)    -- b huo3 &&  b  mian 2
      
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 1 1)    -- w huo3 &&  w  huo 1
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 1 1 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 1 1)    -- b huo3 &&  b  huo 1 
       
       else if nj_ookk 1 1 3 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 1 1 3) ++ (take 1 $ ttt 1 1 1)    -- w huo3 &&  w  mian 1
       else if nj_ookk 0 1 3 >= 1 && nj_ookk 0 1 1 >= 1 then (take 1 $ ttt 0 1 3) ++ (take 1 $ ttt 0 1 1)    -- b huo3 &&  b  mian 1
      
       else if nj_ookk 1 3 3 >= 1 then ttt 1 3 3                                                             --w menglong 3 
       else if nj_ookk 1 1 2 >= 1 then ttt 1 1 2                                                             --w huo 2
      --w mian 3 mian 3
       else if nj_ookk 0 3 3 >= 1 && nj_ookk 1 1 2 >= 1 then (take 1 $ ttt 0 3 3) ++ (take 1 $ ttt 1 1 2)    --b menglong 3 && w huo 2 
       else if nj_ookk 0 3 3 >= 1 && nj_ookk 1 2 3 >= 1 then (take 1 $ ttt 0 3 3) ++ (take 1 $ ttt 1 2 3)    --b menglong 3 && w mian 3
       else if nj_ookk 0 3 3 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 0 3 3) ++ (take 1 $ ttt 1 1 1)    --b menglong 3 && w huo 1
       else if nj_ookk 0 3 3 >= 1 && nj_ookk 1 2 2 >= 1 then (take 1 $ ttt 0 3 3) ++ (take 1 $ ttt 1 2 2)    --b menglong 3 && w mian 2 
       
       else if nj_ookk 0 1 2 >= 1 && nj_ookk 1 2 3 >= 1 then (take 1 $ ttt 0 1 2) ++ (take 1 $ ttt 1 2 3)    --b huo 2 && w mian 3
       else if nj_ookk 0 1 2 >= 1 && nj_ookk 1 1 1 >= 1 then (take 1 $ ttt 0 1 2) ++ (take 1 $ ttt 1 1 1)    --b huo 2 && w huo 1
       else if nj_ookk 0 1 2 >= 1 && nj_ookk 1 2 2 >= 1 then (take 1 $ ttt 0 1 2) ++ (take 1 $ ttt 1 2 2)    --b huo 2 && w mian 2
      --b mian 3 mian 3 du liang ge
       else if nj_ookk 0 3 3 >= 1 then ttt 0 3 3                                                             --b menglong 3  
       else if nj_ookk 0 1 2 >= 1 then ttt 0 1 2                                                             --b huo 2 
       
       else if nj_ookk 1 1 1 >= 1 then ttt 1 1 1                                                             --w huo 1 
       else if nj_ookk 0 1 1 >= 1 && nj_ookk 1 2 1 >= 1 then ttt 0 1 1 ++ ttt 1 2 1                          --b huo 1 && w mian 1
      
       else if nj_ookk 1 2 1 >= 1 then ttt 1 2 1                                                             --w mian 1 
       else if nj_ookk 0 2 1 >= 1 then ttt 0 2 1                                                             --b mian 1 
      
       else if (length $ ext_list redefine_list_com) >= 1 then take 2 $ ext_list redefine_list_com
       else [] 
      
      www1 = if nj_ookk 1 2 5 >= 1 then take 1 $ ext_list redefine_list_com else []
      
      www_out = if length www == 1 then  www ++ www1  -- zhi xia 1 zi shi jia yi zi
           else www
      
      pairs_of_white 1=(xb1 www_out,yb1 www_out) --通过xb1，yb1函数取出白棋所落的第一个子的位置
      pairs_of_white 2=(xb2 www_out,yb2 www_out) --通过xb2，yb2函数取出白棋所落的第二个子的位置
      time0=floor $ utctDayTime currentTime ::Int
      hh_0=div time0 3600
      hh=hh_0+8
      mm=div (time0-3600*hh_0) 60
      ss=time0-3600*hh_0-60*mm
      timeNow=transfertotwobits hh ++"/"++transfertotwobits mm ++ "/"++transfertotwobits ss
      m_white=timeNow++","++(show $ lw+1)++",\""++(show $ pairs_of_white 1)++"\",\""++(show $ pairs_of_white 2)++"\"\n"
  when (length m_white > 0) $ do
        appendFile "white.csv" m_white  --将输出结果存入white.csv文件中
           
  printchess bla whi     --写入数据后在终端上打印棋盘
  putStrLn "white :"
  print www_out    --显示白方落子位置














