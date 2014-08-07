import Data.Char
import Data.List
import Data.String.Utils

first_position = 1

isbn10_validator :: String -> Bool

isbn10_validator number = (generated_digit == last_digit)
    where
        cleansed_isbn = isbn_cleaner number
        generated_digit = digit_generator (init(digits(cleansed_isbn))) first_position 0
        last_digit = last cleansed_isbn

digit_generator :: [Int] -> Int -> Int -> Char

digit_generator [] count res
    | ((mod res 11) == 10) = 'X'
    | otherwise = intToDigit (mod res 11)

digit_generator (x:xs) count res = digit_generator xs incremented_count result
    where
        incremented_count = count+1
        result = res+(x*count)

digits :: String -> [Int]
digits x = map digitToInt x

isbn_cleaner :: String -> String
isbn_cleaner isbn = filter (\x -> notElem x [' ','-']) isbn

random_isbn = "0471958697"
isbn_with_an_x = "0-8044-2957-X"