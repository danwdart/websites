module Util.Triple (t1, t2, t3) where 

-- Because I can't be bothered with lenses for now.

t1 :: (a, b, c) -> a
t1 (a, _, _) = a

t2 :: (a, b, c) -> b
t2 (_, b, _) = b

t3 :: (a, b, c) -> c
t3 (_, _, c) = c