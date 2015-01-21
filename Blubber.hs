{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Blubber where



class Test a where
    foo :: a -> Int

instance Test Int where
    foo _ = 2

instance Test a where
    foo _ = 66

jsf
test1 = foo (2::Int) -- chooses more specific instance
-- test2 = foo 22 -- fails to compile. not so in repl with let
--test3 = foo (3::Num a => a) -- same here


{-
*Blubber> test1
2
*Blubber> foo 22
66
*Blubber> let test2 = foo 22
*Blubber> test2
66
*Blubber> 
-}
