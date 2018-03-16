{-
  Задача 1 (4 балла).
  Напишите две реализации (рекурсивную и с использованием стандартных функций) функции,
  которая создаёт список с заданным количеством элементов арифметической прогрессии
  по заданным первому элементу и разности.
-}

import Data.List

arifmRec::Int->Int->Int->[Int]
arifmRec 0 _ _ = []
arifmRec 1 a1 _ = [a1]
arifmRec n a1 d = step n a1 d [a1]
  where
    step 1 _ _ xs = xs
    step n a1 d acc = step (n-1) (a1+d) d (acc++[a1+d])


arifmStd::Int->Int->Int->[Int]
arifmStd  n a1 d = take n [a1, (a1+d) ..]
