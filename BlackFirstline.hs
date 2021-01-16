module BlackFirstline where
import Data.List
import Data.Char
import System.IO
import Data.Time
import Control.Monad (when)
transfertotwobits x
   |x>10||x==10 =show x
   |x<10 ="0"++ show x
--用于后面的落子时间输出，当时，分，秒中的任一一个值小于10时，在其前加0补位，形成hh/mm/ss的输出格式  
blackFirstline=do
   currentTime <-getCurrentTime
   let get_part_before_right_braket s = if head s==')' then ""
                                     else head s: (get_part_before_right_braket $tail s)
       get_part_after_left_braket s = if head s=='(' then tail s 
                                   else get_part_after_left_braket $tail s
       time0=floor $ utctDayTime currentTime ::Int
       hh_0=div time0 3600
       hh=hh_0+8
       mm=div (time0-3600*hh_0) 60
       ss=time0-3600*hh_0-60*mm
       timeNow=transfertotwobits hh ++"/"++transfertotwobits mm ++ "/"++transfertotwobits ss
       m_black=timeNow++","++(show 1)++",\""++(show (8,8) )++"\",\n"    --我们设计的代码中黑方第一次落子在（8，8）点
   when (length m_black > 0) $
        appendFile "black.csv" m_black
   putStrLn "black :"
   print [(8,8)]   --输出落子
   
