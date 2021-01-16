module WhiteFirstline where
import Data.List
import Data.Char
import System.IO
import Data.Time
import Control.Monad (when)
transfertotwobits x
   |x>10||x==10 =show x
   |x<10 ="0"++ show x
--用于后面的落子时间输出，当时，分，秒中的任一一个值小于10时，在其前加0补位，形成hh/mm/ss的输出格式  
whiteFirstline=do
   mdatablack <-readFile "black.csv"  --由于白方后手，需读取黑方落子
   currentTime <-getCurrentTime
   let get_part_before_right_braket s = if head s==')' then ""
                                     else head s: (get_part_before_right_braket $tail s)
       get_part_after_left_braket s = if head s=='(' then tail s 
                                   else get_part_after_left_braket $tail s
       move_of_black1=get_part_before_right_braket $ get_part_after_left_braket $ mdatablack
       ordered_pairs_of_black1="("++move_of_black1++")"
       x1=fst (read $ordered_pairs_of_black1::(Int,Int))
       y1=snd (read $ordered_pairs_of_black1::(Int,Int))
       time0=floor $ utctDayTime currentTime ::Int
       hh_0=div time0 3600
       hh=hh_0+8
       mm=div (time0-3600*hh_0) 60
       ss=time0-3600*hh_0-60*mm
       timeNow=transfertotwobits hh ++"/"++transfertotwobits mm ++ "/"++transfertotwobits ss
       m_white=timeNow++","++(show 1)++",\""++(show (x1+1,y1) )++"\",\""++(show(x1,y1+1))++"\"\n"   --将白方的第一步棋设定为对黑方第一步棋的堵截
   when (length m_white > 0) $
        appendFile "white.csv" m_white
   putStrLn "white :"
   print [(x1+1,y1),(x1,y1+1)]   --输出落子位置
      
