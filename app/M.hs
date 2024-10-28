module M where

--- trying to learn more stuff about monads

test :: IO ()
test = do
	putStrLn "ffasdfa"


-- testWODo :: String -> IO ()
-- testWODo =  do
--   x <- action1
--   y <- action2 x
--   action3 x y

-- Desugared
-- action1 <<= (\x -> action2 x <<= (\y action3 x y))

-- optional parameters?
matoa :: Maybe a -> a -> a
matoa (Just s) _ = s
matoa Nothing def = def


-- this is an overkill way to have the behaviour that i want
class Matoable a where
    matoa :: a -> String

instance Matoable (Maybe String) where
    matoa (Just s) = s
    matoa Nothing = ""

instance Matoable (Maybe String, String) where
    matoa (Just s, _) = s
    matoa (Nothing, def) = def

-- here are some examples
-- test :: IO ()
-- test = do
--     -- Using with just Maybe String
--     print $ matoa (Just "hello")     -- prints: "hello"
--     print $ matoa Nothing            -- prints: ""
--     
--     -- Using with Maybe String and default
--     print $ matoa (Just "hello", "default")   -- prints: "hello"
--     print $ matoa (Nothing, "default")        -- prints: "default"
