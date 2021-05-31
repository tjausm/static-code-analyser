module PrettyPrinter where
    import AttributeGrammar

    printFlowList :: [Flow] -> String 
    printFlowList xs = "Flow: \n" ++ f xs
        where
            f (f1:f2:f3:xs) = printFlow f1 ++ space ++ printFlow f2 ++ space ++ printFlow f3 ++ "\n" ++ f xs
            f (f1:xs) = case xs of
                [] -> printFlow f1
                xs -> printFlow f1 ++ space ++ f xs
            f _ = ""
            space = " - "

    printFlow :: Flow -> String 
    printFlow (Intra (n,x)) = "(" ++ show n ++ "," ++ show x ++ ")"
    printFlow (Inter (n,x)) = "(" ++ show n ++ ";" ++ show x ++ ")"
    printFlow (Over  (n,x)) = "(" ++ show n ++ "-" ++ show x ++ ")"
