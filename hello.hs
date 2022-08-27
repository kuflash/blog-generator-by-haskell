newtype Html = Html String

newtype Structure = Structure String

type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure first) (Structure second) =
  Structure (first <> second)

render :: Html -> String
render (Html html) = html

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html
    ( el
        "html"
        ( el "head" (el "title" title)
            <> el "body" content
        )
    )

h1_ :: String -> Structure
h1_ = Structure . el "h1"

p_ :: String -> Structure
p_ = Structure . el "p"

myhtml :: Html
myhtml =
  html_
    "Page title"
    ( append_
        (h1_ "I'm a heading")
        ( append_
            (p_ "I'm a paragraph one")
            (p_ "I'm a paragraph two")
        )
    )

main :: IO ()
main = putStrLn (render myhtml)
