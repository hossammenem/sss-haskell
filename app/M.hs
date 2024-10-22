module M where

--- trying to learn more stuff about monads

test :: IO ()
test s = do
	putStrLn "ffasdfa"


testWODo :: String -> IO ()
testWODo =  do
  x <- action1
  y <- action2 x
  action3 x y

-- Desugared
action1 <<= (\x -> action2 x <<= (\y action3 x y))
