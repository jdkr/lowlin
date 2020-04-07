module Test where


data Spec = Spec {sName :: !String, sPassed :: !Bool} deriving Show


(=~=) :: (Ord a, Floating a) => a -> a -> Bool
(=~=) x1 x2 = abs (x1 - x2) < 1e-8


test :: [Spec] -> IO ()
test specs = do
    let failedSpecs = filter (\spec-> not (sPassed spec)) specs
    case failedSpecs of
        [] -> putStrLn $ "All specs passed"
        otherwise -> do
            putStrLn $ "failed specs: " ++ show failedSpecs;
            error "test-suite failed"


