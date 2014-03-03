module Far (
) where


data Far a = Arr (Int -> a)
           | Update Int a Far
           | Post (a -> b) Far
           | Pre (Int -> Int) Far
           | Empty

data Ind a = Individual [a]
           | Mut [Int] Ind
           | Cross (Int, Int, Int) Ind

data Pop a = Population [Ind a]
           | Tourned [Int] Pop
