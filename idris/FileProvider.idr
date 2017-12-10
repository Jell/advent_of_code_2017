module FileProvider
%default total
%access export

readString : String -> IO (Provider String)
readString filename = do
  Right text <- readFile filename
        | Left f => pure (Error $ (show f)++" (file: "++filename++")")
  pure $ Provide text
