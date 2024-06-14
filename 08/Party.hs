
module Party where

import Employee
import Data.Tree
import Data.List
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0

instance Semigroup GuestList where
    (GL es1 f1) <> (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 | gl1 < gl2 = gl2
                | otherwise = gl1


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node n ns) = f n (map (treeFold f) ns)


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss xs = (glCons boss (mconcat (map snd xs)), mconcat (map fst xs))


maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


fmtEL :: [Employee] -> String
fmtEL = unlines . sort . map empName

fmtGL :: GuestList -> String
fmtGL (GL gls fun) = "Total fun: " ++ show fun ++ "\n" ++ fmtEL gls

main :: IO ()
main = do
    txt <- readFile "company.txt"
    putStr . fmtGL . maxFun . read $ txt
