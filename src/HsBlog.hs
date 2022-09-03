module HsBlog where

import qualified HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn $ process "Default title" content
    [input, output] -> do
      content <- getContents
      isExist <- doesDirectoryExist output
      let writeResult = writeFile output (process input content)
      if isExist
        then whenIO confirm writeResult
        else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title txt =
  Html.render $ HsBlog.Convert.convert title $ Markup.parse txt

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n"
            *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
