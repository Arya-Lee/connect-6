module Judge where    
import Data.List
import Data.Char
import System.IO
import System.Directory
import AIblack
import AIwhite
import BlackFirstline
import WhiteFirstline
------------------------------------------------------------------------
--Judge.hs文件调用了其余文件，是代码的主要部分
tailss 1 a = a
tailss 2 a = tail a
tailss m a = tail $ tailss (m-1) a

initss 1 a = a
initss 2 a = init a
initss m a = init $ initss (m-1) a

last_n 1 a =[last $ initss 1 a]
last_n n a =[last $ initss n a] ++ last_n (n-1) a

redefine_six_list_m m a = take 6 $ tailss m a

step_count_white n = if mod n 2 == 0 then div n 2 else div n 2 + 1

everyonein :: (Foldable t, Eq a) => [a] -> t a -> Bool
everyonein [] ta = True
everyonein (s:st) ta = s `elem` ta && everyonein st ta
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [pos | (x',pos) <- zip xs [1..length xs],x' == x]--找出等于xs 中x的位置
------------------------------------------------------------------------
judge a b c d = if a>2||b>2||c>2||d>2  then 1 else 0
--用于判断横，竖，斜向上，斜向下三个方向上是否连成六子 
main :: IO()
main = do   --交互模块，读取用户的终端输入选择作为黑方或是白方
    putStrLn "Input 'b' or 'w' to choose your color (b:black w: white) "
    appendFile "white.csv" ("")
    appendFile "black.csv" ("")
    character <- getChar
    tmp <- getLine
    if character == 'w' 
      then do
          whiteFirstline --因为第一行基本不需要经过AI分析，故将该模块单独列出
          whiteloop   --在加入第一行后，进入循环判断过程      
    else 
      if character == 'b' 
      then do
          blackFirstline
          blackloop 
        else initializationError

initializationError = do
    tmp <- getChar
    putStrLn "Wrong argument!"  
    main
      
blackloop=do
  mdatablack1 <- readFile "black.csv"
  mdatawhite1 <- readFile "white.csv"
  let aab = lines mdatablack1                                                                 
      aaw = lines mdatawhite1
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
      tb n = 3600 * tbh n + 60 * tbmin n + tbs n                                             --blackchess time
      twh :: Int -> Int
      twmin :: Int -> Int
      tws :: Int -> Int
      tw :: Int -> Int
      twh q = read $ take 2 $ p q :: Int
      twmin q = read $ take 2 $ (tails $ p q) !! 3 :: Int
      tws q = read $ take 2 $ (tails $ p q) !! 6 :: Int
      tw q = 3600 * twh q + 60 * twmin q + tws q
      
  if mdatablack1== ""||mdatawhite1 ==""             --为避免两个终端不能同时输入而出现错误，需要等待两方csv文件均出现第一行
  then blackloop
  else do                                           --whitechess time
  let move_of_black n 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ m (n-1)
      move_of_black n 1=get_part_before_right_braket $ get_part_after_left_braket $ m (n-1)       
      --(for the second x or y should apply the function twice                                    
      ordered_pairs_of_black n j="("++move_of_black n j++")"
      --ordered_pairs_of_black n 1="("++move_of_black n 1++")"
      --ordered_pairs_of_black n 2="("++move_of_black n 2++")"
      xb n j=fst (read $ordered_pairs_of_black n j ::(Int,Int))
     -- xb n 1=fst (read $ordered_pairs_of_black n 1 ::(Int,Int))
      --xb n 2=fst (read $ordered_pairs_of_black n 2 ::(Int,Int))
      yb n j=snd (read $ordered_pairs_of_black n j ::(Int,Int))
      --yb n 1=snd (read $ordered_pairs_of_black n 1 ::(Int,Int))
     -- yb n 1=snd (read $ordered_pairs_of_black n 2 ::(Int,Int) 
      move_of_white q 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ p (q-1)
      move_of_white q 1=get_part_before_right_braket $ get_part_after_left_braket $ p (q-1) 
      --(for the second x or y should apply the two functions twice
      ordered_pairs_of_white q j="("++move_of_white q j++")"
      --ordered_pairs_of_white n 1="("++move_of_white q 1++")"
      --ordered_pairs_of_white n 2="("++move_of_white q 2++")"
      
      xw q j=fst (read $ordered_pairs_of_white q j ::(Int,Int))
      --xw q 1=fst (read $ordered_pairs_of_white q 1 ::(Int,Int))
      --xw q 2=fst (read $ordered_pairs_of_white q 2 ::(Int,Int))
      yw q j=snd (read $ordered_pairs_of_white q j ::(Int,Int)) 
      chess_list_black = if lb == 1 then [(xb 1 1,yb 1 1)] else [(xb 1 1,yb 1 1)] ++ [(x,y) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_white = [(x,y) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      chess_list_white_n n = take (2*n) chess_list_white 
---------------------------------------------------------------------------------------------以下是通过对四个方向的棋形综合，判断是否连成六子  
      redefine_list_11_0_white n j = [(xw n j,y) | y <- [(yw n j - 5)..(yw n j + 5)]]
      redefine_list_six_0_white n j m = redefine_six_list_m m $ redefine_list_11_0_white n j
      single_figure_connect6_0_white n j m = if everyonein (redefine_list_six_0_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_0_white n j = [single_figure_connect6_0_white n j m | m <- [1..6]]
      figure_connect6_0_white n j = if everyonein [1] (list_figure_connect6_0_white n j) then 1 else 0
      list_connect6_0_white = [figure_connect6_0_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_0_white] =if positions 1 list_connect6_0_white == [] then [0] else [ head $ positions 1 list_connect6_0_white]
      step_count_connect6_0_white = step_count_white position_connect6_0_white
----------------------------------------------
      redefine_list_11_90_white n j = [(x,yw n j) | x <- [(xw n j - 5)..(xw n j + 5)]]
      redefine_list_six_90_white n j m = redefine_six_list_m m $ redefine_list_11_90_white n j
      single_figure_connect6_90_white n j m = if everyonein (redefine_list_six_90_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_90_white n j = [single_figure_connect6_90_white n j m | m <- [1..6]]
      figure_connect6_90_white n j = if everyonein [1] (list_figure_connect6_90_white n j) then 1 else 0
      list_connect6_90_white = [figure_connect6_90_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_90_white] = if positions 1 list_connect6_90_white == [] then [0] else [head $ positions 1 list_connect6_90_white] --need modify
      step_count_connect6_90_white = step_count_white position_connect6_90_white
----------------------------------------------------------------
      list_45_x_white n j = [ x | x <- [(xw n j + 5),(xw n j + 4)..(xw n j - 5)]]
      list_45_y_white n j = [ y | y <- [(yw n j-5)..(yw n j+5)]]
      redefine_list_11_45_white n j = zip (list_45_x_white n j) (list_45_y_white n j)
      redefine_list_six_45_white n j m = redefine_six_list_m m $ redefine_list_11_45_white n j
      single_figure_connect6_45_white n j m = if everyonein (redefine_list_six_45_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_45_white n j = [single_figure_connect6_45_white n j m | m <- [1..6]]
      figure_connect6_45_white n j = if everyonein [1] (list_figure_connect6_45_white n j) then 1 else 0
      list_connect6_45_white = [figure_connect6_45_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_45_white] =if positions 1 list_connect6_45_white == [] then [0] else [ head $ positions 1 list_connect6_45_white]
      step_count_connect6_45_white = step_count_white position_connect6_45_white
--------------------------------------------------------------------
      list_135_x_white n j = [ x | x <- [(xw n j - 5)..(xw n j + 5)]]
      list_135_y_white n j = [ y | y <- [(yw n j - 5)..(yw n j + 5)]]
      redefine_list_11_135_white n j = zip (list_135_x_white n j) (list_135_y_white n j)
      redefine_list_six_135_white n j m = redefine_six_list_m m $ redefine_list_11_135_white n j
      single_figure_connect6_135_white n j m = if everyonein (redefine_list_six_135_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_135_white n j = [single_figure_connect6_135_white n j m | m <- [1..6]]
      figure_connect6_135_white n j = if everyonein [1] (list_figure_connect6_135_white n j) then 1 else 0
      list_connect6_135_white = [figure_connect6_135_white n j | n <- [1..lw],j <- [1,2]]

      [position_connect6_135_white] =if positions 1 list_connect6_135_white == [] then [0] else [ head $ positions 1     list_connect6_135_white]
      step_count_connect6_135_white = step_count_white position_connect6_135_white
--------------------------------------------------------------------------------------------------------------------------------
      step_count_black n = if mod (n-1) 2 == 0 then div (n-1) 2 + 1 else div n 2 + 1
      chess_list_black_n n = take (2*n-1) chess_list_black
      redefine_list_11_0_black n j = [(xb n j,y) | y <- [(yb n j - 5)..(yb n j + 5)]]
 
      redefine_list_six_0_black n j m = redefine_six_list_m m $ redefine_list_11_0_black n j
      single_figure_connect6_0_black n j m = if everyonein (redefine_list_six_0_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_0_black n j = [single_figure_connect6_0_black n j m | m <- [1..6]]
      figure_connect6_0_black n j = if everyonein [1] (list_figure_connect6_0_black n j) then 1 else 0
      list_connect6_0_black =[figure_connect6_0_black 1 1] ++ [figure_connect6_0_black n j | n <- [2..lb],j <- [1,2]]

      [position_connect6_0_black] =if positions 1 list_connect6_0_black == [] then [0] else [ head $ positions 1     list_connect6_0_black]
      step_count_connect6_0_black = step_count_black position_connect6_0_black
------------------------------------------------------
      redefine_list_11_90_black n j = [(x,yb n j) | x <- [(xb n j - 5)..(xb n j + 5)]]
      redefine_list_six_90_black n j m = redefine_six_list_m m $ redefine_list_11_90_black n j
      single_figure_connect6_90_black n j m = if everyonein (redefine_list_six_90_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_90_black n j = [single_figure_connect6_90_black n j m | m <- [1..6]]
      figure_connect6_90_black n j = if everyonein [1] (list_figure_connect6_90_black n j) then 1 else 0
      list_connect6_90_black = [figure_connect6_90_black 1 1] ++ [figure_connect6_90_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_90_black] = if positions 1 list_connect6_90_black == [] then [0] else [ head $ positions 1 list_connect6_90_black] --need modify
      step_count_connect6_90_black = step_count_black position_connect6_90_black
----------------------------------------------------------------
      list_45_x_black n j = [ x | x <- [(xb n j + 5),(xb n j + 4)..(xb n j - 5)]]
      list_45_y_black n j = [ y | y <- [(yb n j-5)..(yb n j+5)]]
      redefine_list_11_45_black n j = zip (list_45_x_black n j) (list_45_y_black n j)
      redefine_list_six_45_black n j m = redefine_six_list_m m $ redefine_list_11_45_black n j
      single_figure_connect6_45_black n j m = if everyonein (redefine_list_six_45_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_45_black n j = [single_figure_connect6_45_black n j m | m <- [1..6]]
      figure_connect6_45_black n j = if everyonein [1] (list_figure_connect6_45_black n j) then 1 else 0
      list_connect6_45_black = [figure_connect6_45_black 1 1] ++ [figure_connect6_45_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_45_black] =if positions 1 list_connect6_45_black == [] then [0] else [head $ positions 1 list_connect6_45_black]
      step_count_connect6_45_black = step_count_black position_connect6_45_black
--------------------------------------------------------------------
      list_135_x_black n j = [ x | x <- [(xb n j - 5)..(xb n j + 5)]]  
      list_135_y_black n j = [ y | y <- [(yb n j - 5)..(yb n j + 5)]]
      redefine_list_11_135_black n j = zip (list_135_x_black n j) (list_135_y_black n j)

      redefine_list_six_135_black n j m = redefine_six_list_m m $ redefine_list_11_135_black n j
      single_figure_connect6_135_black n j m = if everyonein (redefine_list_six_135_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_135_black n j = [single_figure_connect6_135_black n j m | m <- [1..6]]
      figure_connect6_135_black n j = if everyonein [1] (list_figure_connect6_135_black n j) then 1 else 0
      list_connect6_135_black = [figure_connect6_135_black 1 1] ++ [figure_connect6_135_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_135_black] =if positions 1 list_connect6_135_black == [] then [0] else [ head $positions 1           list_connect6_135_black]
      step_count_connect6_135_black = step_count_black position_connect6_135_black
      
      
      chess_list_b = if lb == 1 then [(xb 1 1,yb 1 1)] else [(xb 1 1,yb 1 1)] ++ [(x,y) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_w = [(x,y) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      
      bla = [f (x,y) v l | (x,y) <- chess_list_b,v <- [1],l <- [61]]
      whi = [f (x,y) v l | (x,y) <- chess_list_w,v <- [2],l <- [61]]
-------------------------------------------------------------------------------------------------
  if judge step_count_connect6_0_white step_count_connect6_45_white step_count_connect6_90_white step_count_connect6_135_white==1  then
                putStrLn "white wins"
    else if judge step_count_connect6_0_black step_count_connect6_45_black step_count_connect6_90_black step_count_connect6_135_black==1 then 
                putStrLn "black wins"
    else if lb==lw && judge step_count_connect6_0_white step_count_connect6_45_white step_count_connect6_90_white step_count_connect6_135_white/=1 then do
               --作为黑方循环，选择当白方与自身csv文件行数相同且未分胜负时落子
               (tempName,tempHandle)<- openTempFile".""temp"
               hPutStr tempHandle mdatablack1
               hClose tempHandle
               removeFile "black.csv"
               renameFile tempName "black.csv"
               --为解决文件被重复读取所引起的文件繁忙的问题，创建了一个临时文件，将csv文件的内容存入，并使之替代原文件
               ai_black 
               blackloop
   else blackloop
--以下whiteloop与blackloop结构相似        
whiteloop=do
  mdatablack <- readFile "black.csv"                                                         --read blackchess
  mdatawhite <- readFile "white.csv"
  let aab = lines mdatablack                                                                 
      aaw = lines mdatawhite
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
      tb n = 3600 * tbh n + 60 * tbmin n + tbs n                                             --blackchess time
      twh :: Int -> Int
      twmin :: Int -> Int
      tws :: Int -> Int
      tw :: Int -> Int
      twh q = read $ take 2 $ p q :: Int
      twmin q = read $ take 2 $ (tails $ p q) !! 3 :: Int
      tws q = read $ take 2 $ (tails $ p q) !! 6 :: Int
      tw q = 3600 * twh q + 60 * twmin q + tws q                                             --whitechess time
  if mdatablack== ""||mdatawhite ==""
  then whiteloop
  else do                                           --whitechess time
  let move_of_black n 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ m (n-1)
      move_of_black n 1=get_part_before_right_braket $ get_part_after_left_braket $ m (n-1)       
      --(for the second x or y should apply the function twice                                    
      ordered_pairs_of_black n j="("++move_of_black n j++")"
      --ordered_pairs_of_black n 1="("++move_of_black n 1++")"
      --ordered_pairs_of_black n 2="("++move_of_black n 2++")"
      xb n j=fst (read $ordered_pairs_of_black n j ::(Int,Int))
     -- xb n 1=fst (read $ordered_pairs_of_black n 1 ::(Int,Int))
      --xb n 2=fst (read $ordered_pairs_of_black n 2 ::(Int,Int))
      yb n j=snd (read $ordered_pairs_of_black n j ::(Int,Int))
      --yb n 1=snd (read $ordered_pairs_of_black n 1 ::(Int,Int))
     -- yb n 1=snd (read $ordered_pairs_of_black n 2 ::(Int,Int)   --------------------------------------------------------------------------------------------------------                                         
      move_of_white q 2=get_part_before_right_braket $ get_part_after_left_braket $ get_part_after_left_braket $ p (q-1)
      move_of_white q 1=get_part_before_right_braket $ get_part_after_left_braket $ p (q-1) 
      --(for the second x or y should apply the two functions twice
      ordered_pairs_of_white q j="("++move_of_white q j++")"
      --ordered_pairs_of_white n 1="("++move_of_white q 1++")"
      --ordered_pairs_of_white n 2="("++move_of_white q 2++")"
      
      xw q j=fst (read $ordered_pairs_of_white q j ::(Int,Int))
      --xw q 1=fst (read $ordered_pairs_of_white q 1 ::(Int,Int))
      --xw q 2=fst (read $ordered_pairs_of_white q 2 ::(Int,Int))
      yw q j=snd (read $ordered_pairs_of_white q j ::(Int,Int)) 
      chess_list_black = if lb == 1 then [(xb 1 1,yb 1 1)] else [(xb 1 1,yb 1 1)] ++ [(x,y) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_white = [(x,y) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      chess_list_white_n n = take (2*n) chess_list_white 
      
----------------------------------------------------------------------------------------------------------------------------------------
      step_count_black n = if mod (n-1) 2 == 0 then div (n-1) 2 + 1 else div n 2 + 1
      chess_list_black_n n = take (2*n-1) chess_list_black
      redefine_list_11_0_black n j = [(xb n j,y) | y <- [(yb n j - 5)..(yb n j + 5)]]
 
      redefine_list_six_0_black n j m = redefine_six_list_m m $ redefine_list_11_0_black n j
      single_figure_connect6_0_black n j m = if everyonein (redefine_list_six_0_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_0_black n j = [single_figure_connect6_0_black n j m | m <- [1..6]]
      figure_connect6_0_black n j = if everyonein [1] (list_figure_connect6_0_black n j) then 1 else 0
      list_connect6_0_black =[figure_connect6_0_black 1 1] ++ [figure_connect6_0_black n j | n <- [2..lb],j <- [1,2]]

      [position_connect6_0_black] =if positions 1 list_connect6_0_black == [] then [0] else [ head $ positions 1     list_connect6_0_black]
      step_count_connect6_0_black = step_count_black position_connect6_0_black
------------------------------------------------------
      redefine_list_11_90_black n j = [(x,yb n j) | x <- [(xb n j - 5)..(xb n j + 5)]]
      redefine_list_six_90_black n j m = redefine_six_list_m m $ redefine_list_11_90_black n j
      single_figure_connect6_90_black n j m = if everyonein (redefine_list_six_90_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_90_black n j = [single_figure_connect6_90_black n j m | m <- [1..6]]
      figure_connect6_90_black n j = if everyonein [1] (list_figure_connect6_90_black n j) then 1 else 0
      list_connect6_90_black = [figure_connect6_90_black 1 1] ++ [figure_connect6_90_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_90_black] = if positions 1 list_connect6_90_black == [] then [0] else [ head $ positions 1 list_connect6_90_black] --need modify
      step_count_connect6_90_black = step_count_black position_connect6_90_black
----------------------------------------------------------------
      list_45_x_black n j = [ x | x <- [(xb n j + 5),(xb n j + 4)..(xb n j - 5)]]
      list_45_y_black n j = [ y | y <- [(yb n j-5)..(yb n j+5)]]
      redefine_list_11_45_black n j = zip (list_45_x_black n j) (list_45_y_black n j)
      redefine_list_six_45_black n j m = redefine_six_list_m m $ redefine_list_11_45_black n j
      single_figure_connect6_45_black n j m = if everyonein (redefine_list_six_45_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_45_black n j = [single_figure_connect6_45_black n j m | m <- [1..6]]
      figure_connect6_45_black n j = if everyonein [1] (list_figure_connect6_45_black n j) then 1 else 0
      list_connect6_45_black = [figure_connect6_45_black 1 1] ++ [figure_connect6_45_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_45_black] =if positions 1 list_connect6_45_black == [] then [0] else [head $ positions 1 list_connect6_45_black]
      step_count_connect6_45_black = step_count_black position_connect6_45_black
--------------------------------------------------------------------
      list_135_x_black n j = [ x | x <- [(xb n j - 5)..(xb n j + 5)]]  
      list_135_y_black n j = [ y | y <- [(yb n j - 5)..(yb n j + 5)]]
      redefine_list_11_135_black n j = zip (list_135_x_black n j) (list_135_y_black n j)

      redefine_list_six_135_black n j m = redefine_six_list_m m $ redefine_list_11_135_black n j
      single_figure_connect6_135_black n j m = if everyonein (redefine_list_six_135_black n j m) (chess_list_black_n n) then 1 else 0
      list_figure_connect6_135_black n j = [single_figure_connect6_135_black n j m | m <- [1..6]]
      figure_connect6_135_black n j = if everyonein [1] (list_figure_connect6_135_black n j) then 1 else 0
      list_connect6_135_black = [figure_connect6_135_black 1 1] ++ [figure_connect6_135_black n j | n <- [2..lb],j <- [1,2]]
      [position_connect6_135_black] =if positions 1 list_connect6_135_black == [] then [0] else [ head $positions 1           list_connect6_135_black]
      step_count_connect6_135_black = step_count_black position_connect6_135_black
-------------------------------------------------------------------------------------------------  
      redefine_list_11_0_white n j = [(xw n j,y) | y <- [(yw n j - 5)..(yw n j + 5)]]
      redefine_list_six_0_white n j m = redefine_six_list_m m $ redefine_list_11_0_white n j
      single_figure_connect6_0_white n j m = if everyonein (redefine_list_six_0_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_0_white n j = [single_figure_connect6_0_white n j m | m <- [1..6]]
      figure_connect6_0_white n j = if everyonein [1] (list_figure_connect6_0_white n j) then 1 else 0
      list_connect6_0_white = [figure_connect6_0_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_0_white] =if positions 1 list_connect6_0_white == [] then [0] else [ head $ positions 1 list_connect6_0_white]
      step_count_connect6_0_white = step_count_white position_connect6_0_white
----------------------------------------------
      redefine_list_11_90_white n j = [(x,yw n j) | x <- [(xw n j - 5)..(xw n j + 5)]]
      redefine_list_six_90_white n j m = redefine_six_list_m m $ redefine_list_11_90_white n j
      single_figure_connect6_90_white n j m = if everyonein (redefine_list_six_90_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_90_white n j = [single_figure_connect6_90_white n j m | m <- [1..6]]
      figure_connect6_90_white n j = if everyonein [1] (list_figure_connect6_90_white n j) then 1 else 0
      list_connect6_90_white = [figure_connect6_90_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_90_white] = if positions 1 list_connect6_90_white == [] then [0] else [head $ positions 1 list_connect6_90_white] --need modify
      step_count_connect6_90_white = step_count_white position_connect6_90_white
----------------------------------------------------------------
      list_45_x_white n j = [ x | x <- [(xw n j + 5),(xw n j + 4)..(xw n j - 5)]]
      list_45_y_white n j = [ y | y <- [(yw n j-5)..(yw n j+5)]]
      redefine_list_11_45_white n j = zip (list_45_x_white n j) (list_45_y_white n j)
      redefine_list_six_45_white n j m = redefine_six_list_m m $ redefine_list_11_45_white n j
      single_figure_connect6_45_white n j m = if everyonein (redefine_list_six_45_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_45_white n j = [single_figure_connect6_45_white n j m | m <- [1..6]]
      figure_connect6_45_white n j = if everyonein [1] (list_figure_connect6_45_white n j) then 1 else 0
      list_connect6_45_white = [figure_connect6_45_white n j | n <- [1..lw],j <- [1,2]]
      [position_connect6_45_white] =if positions 1 list_connect6_45_white == [] then [0] else [ head $ positions 1 list_connect6_45_white]
      step_count_connect6_45_white = step_count_white position_connect6_45_white
--------------------------------------------------------------------
      list_135_x_white n j = [ x | x <- [(xw n j - 5)..(xw n j + 5)]]
      list_135_y_white n j = [ y | y <- [(yw n j - 5)..(yw n j + 5)]]
      redefine_list_11_135_white n j = zip (list_135_x_white n j) (list_135_y_white n j)
      redefine_list_six_135_white n j m = redefine_six_list_m m $ redefine_list_11_135_white n j
      single_figure_connect6_135_white n j m = if everyonein (redefine_list_six_135_white n j m) (chess_list_white_n n) then 1 else 0
      list_figure_connect6_135_white n j = [single_figure_connect6_135_white n j m | m <- [1..6]]
      figure_connect6_135_white n j = if everyonein [1] (list_figure_connect6_135_white n j) then 1 else 0
      list_connect6_135_white = [figure_connect6_135_white n j | n <- [1..lw],j <- [1,2]]

      [position_connect6_135_white] =if positions 1 list_connect6_135_white == [] then [0] else [ head $ positions 1     list_connect6_135_white]
      step_count_connect6_135_white = step_count_white position_connect6_135_white
      
      chess_list_b = if lb == 1 then [(xb 1 1,yb 1 1)] else [(xb 1 1,yb 1 1)] ++ [(x,y) | (x,y) <- [(xb n j,yb n j) | n <- [2..lb],j <- [1,2]]]
      chess_list_w = [(x,y) | (x,y) <- [(xw q j,yw q j) | q <- [1..lw],j <- [1,2]]]
      
      bla = [f (x,y) v l | (x,y) <- chess_list_b,v <- [1],l <- [61]]
      whi = [f (x,y) v l | (x,y) <- chess_list_w,v <- [2],l <- [61]]
----------------------------------------------------------------------------------------------------      
  if judge step_count_connect6_0_black step_count_connect6_45_black step_count_connect6_90_black step_count_connect6_135_black==1 then do
      putStrLn "black wins"
      printchess bla whi
      else if judge step_count_connect6_0_white step_count_connect6_45_white step_count_connect6_90_white step_count_connect6_135_white==1  then do
                putStrLn "white wins"
                printchess bla whi
      else if judge step_count_connect6_0_black step_count_connect6_45_black step_count_connect6_90_black step_count_connect6_135_black/=1&&lb-lw==1 then do
      ai_white              --作为白方方循环，选择当黑方比自身csv文件行数多出一行且未分胜负时落子
      whiteloop
      else whiteloop
  
   
