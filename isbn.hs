import Data.Char

isbn10_validator :: String -> Bool

isbn10_validator number = (generated_digit == last_digit)
    where
        generated_digit = digit_generator (init(digits(number))) 1 0
        last_digit = last(digits(number))

digit_generator :: [Int] -> Int -> Int -> Int
digit_generator [] count res = mod res 11
digit_generator (x:xs) count res = digit_generator xs incremented_count result
    where
        incremented_count = count+1
        result = res+(x*count)


digits :: String -> [Int]
digits x = map digitToInt x

dummy_isbn = "0471958697"
