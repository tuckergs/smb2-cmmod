
module Helpers where

noDups :: Eq a => [a] -> [a]
noDups ls
  = [ c | (c,i) <- zip ls [0..((length ls)-1)],
    (i == head [ j | 
               j <- [0..((length ls)-1)],
               c == ls !! j 
               ])
    ]

brutalLookup :: Eq a => a -> [(a,b)] -> b
brutalLookup ele store = case lookup ele store of
  Just b -> b
  Nothing -> error "brutalLookup failed. You shouldn\'t be seeing this. TwixNinja sucks. You should beat him up."

brutalUncons :: [a] -> (a,[a])
brutalUncons [] = error "brutalUncons failed. You shouldn\'t be seeing this. TwixNinja sucks. You should beat him up." 
brutalUncons (x:xs) = (x,xs)
