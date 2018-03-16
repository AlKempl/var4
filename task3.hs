{-
  Задача 3 (10 баллов).
  Дан текстовый файл stock.txt, содержащий содержащий информацию по
  различным партиям товаров на складе: название товара, имеющееся
  количество, цена за единицу.

  1) Разработайте алгебраический тип данных Item для хранения информации
     о партии товара.
  2) (2 б.) Напишите функцию, которая загружает данные из файла с заданным
     именем в список значений типа Item.
  3) (1 б.) Напишите функцию, которая по списку Item и  наименованию товара
     возвращает суммарную стоимость всех партий данного товара (стоимость
     партии вычисляется как количество, умноженное на цену за единицу).
  4) (4 б.) Напишите функцию, которая по списку Item и пределу количества
     возвращает информацию по всем партиям меньшего размера.
  5) (3 б.) Напишите основную программу, которая анализирует параметры
     командной строки, на их основе вызывает функцию из пунктов 3) или 4)
     и печатает результаты.
-}

import System.Environment
import Data.List
import Data.Char
import System.IO

main = do
  [fname, itemName] <- getArgs
  items <- loadData fname
  print $ getBatchPriceByGood itemName items

data Item = Item String Int Double
  deriving (Show)


itemName :: Item->String
itemName (Item name _ _) = name
--
itemCount :: Item->Int
itemCount (Item _ count _) = count
--
itemPrice :: Item->Double
itemPrice (Item  _ _ price) = price

str2item :: String -> Item
str2item = buildItem.words
  where
    buildItem (title:count:price:xs) = Item title (read count) (read price)
    buildItem _ = error "incorrect input data"


loadData :: FilePath -> IO [Item]
loadData fname = do
  fc <- readFile fname
  return $ map str2item $ lines fc

getBatchPriceByGood :: String->[Item]->Double
getBatchPriceByGood name items = foldl (\acc x -> ((itemPrice x) * (fromIntegral (itemCount x)) + acc)) 0.0 (filter (\x-> (itemName x == name)) items)
