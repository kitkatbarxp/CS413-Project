{-# LANGUAGE GADTs #-}

data Op a where 
  I      :: Int -> Op Int 
  IL     :: [Int] -> Op [Int]
  AddI   :: Op Int -> Op Int -> Op Int

  D      :: Double -> Op Double
  DL     :: [Double] -> Op [Double]
  AddD   :: Op Double -> Op Double -> Op Double

instance (Show a) => Show (Op a) where
  show (I n) = show n
  show (IL l) = show l
  show (AddI o1 o2) = "Add " ++ show o1 ++ show o2

  show (D n) = show n
  show (DL l) = show l
  show (AddD o1 o2) = "Add " ++ show o1 ++ show o2

eval :: Op a -> a
eval (IL l) = l
eval (I n) = n
eval (AddI o1 o2) = eval o1 + eval o2
eval (DL l) = l
eval (D n) = n
eval (AddD o1 o2) = eval o1 + eval o2

data Expr = MapI (Op [Int]) (Op Int -> Op Int) (Op [Int]) 
          | MapD (Op [Double]) (Op Double -> Op Double) (Op [Double])

instance Show Expr where
  show (MapI a b c) = "Map" ++ show a ++ show c
  show (MapD a b c) = "Map" ++ show a ++ show c

run :: Expr -> Expr
run a@(MapI _ _ (IL [])) = a
run a@(MapD _ _ (DL [])) = a
run (MapI (cur) (exp) (args)) = MapI (IL ((eval cur) ++ [eval(exp(I (head (eval args))))])) (exp) (IL (drop 1 (eval args)))
run (MapD (cur) (exp) (args)) = MapD (DL ((eval cur) ++ [eval(exp(D (head (eval args))))])) (exp) (DL (drop 1 (eval args)))

m = MapI (IL []) (AddI (I 10)) (IL [1,2,3,4])
m' = MapD (DL []) (AddD (D 10)) (DL [1,2,3,4])

