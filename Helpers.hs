
module Helpers where

noDups :: Eq a => [a] -> [a]
noDups ls
  = [ c | (c,i) <- zip ls [0..((length ls)-1)],
    (i == head [ j | 
               j <- [0..((length ls)-1)],
               c == ls !! j 
               ])
    ]
