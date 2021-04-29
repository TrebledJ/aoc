import Data.List (isPrefixOf, (\\))
import Text.Parsec
import Utils

main :: IO ()
main = readFile "input.txt" >>= print . solve . parse notes ""

-- Returns true if all values satisfy at least one field.
isTicketValid :: [Field] -> Ticket -> Bool
isTicketValid fs t = all (\val -> any (valid val) fs) t

-- Checks if a ticket column satisfies a field.
isColumnValid :: [Ticket] -> Int -> Field -> Bool
isColumnValid ts col f = all (\t -> valid (t !! col) f) ts

-- For each column, collect the list of valid fields.
reduceTickets :: [Ticket] -> [Field] -> Int -> [[Field]]
reduceTickets ts fs ncol = map (\i -> filter (isColumnValid ts i) fs) [0..ncol-1]

-- If the list of fields contains a given field, remove said field from the list.
remove :: [Field] -> Field -> [Field]
remove fs f = fs \\ [f]

-- Reduce the aggregate list, so that each column only corresponds to one (distinct) field.
reduceFields :: [[Field]] -> [Field]
reduceFields fss
    | all ((== 1) . length) fss = concat fss
    | otherwise = reduceFields $ foldr (\fs acc -> if length fs == 1 then map (\fs' -> if length fs' /= 1 then fs' `remove` (head fs) else fs') acc else acc) fss fss

-- Returns the value of a particular field on a ticket.
getValues
  :: [Field] -- ^ The column-to-field mapping.
  -> Ticket -- ^ The ticket to query.
  -> (String -> Bool) -- ^ The predicate for querying fields.
  -> [Int] -- ^ The value of the corresponding field.
getValues fs t p = [ v | (f', v) <- zip fs t, p $ fst f' ]

solve :: Either ParseError Notes -> Int
solve (Left err) = error $ "error: " ++ show err
solve (Right (Notes fs t ts)) = product $ getValues fields t ("departure" `isPrefixOf`)
  where validTickets = filter (isTicketValid fs) ts
        columns = length t
        fields = reduceFields $ reduceTickets validTickets fs columns
