module Main
    (
    main
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set (Set, fromList)
import HW3.Action (HIO (..), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyEvalError, prettyParseError, prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Console.Repline (CompleterStyle (Word), HaskelineT, WordCompleter, evalRepl)
import Prettyprinter.Render.Terminal (putDoc)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "" -> loop
        Just input -> do
          let parsed = parse input
          liftIO $ case parsed of
            Left err -> putDoc . prettyParseError $ err
            Right expr -> do
              res <- runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              putDoc (case res of
                Left err  -> prettyEvalError err
                Right val -> prettyValue val) <> putStrLn ""
          loop
