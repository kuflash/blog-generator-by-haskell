el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

body_ :: String -> String
body_ = el "body"

h1_ :: String -> String
h1_ = el "h1"

p_ :: String -> String
p_ = el "p"

makeHtml :: String -> String -> String
makeHtml title content =
  html_ (head_ (title_ title) <> body_ content)

myhtml :: String
myhtml = makeHtml "My page title" (h1_ "I'm a header" <> p_ "I'm a paragraph")

main :: IO ()
main = putStrLn myhtml
