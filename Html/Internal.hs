module Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (<>) a b =
    Structure (getStructureString a <> getStructureString b)

getStructureString :: Structure -> String
getStructureString (Structure content) = content

render :: Html -> String
render (Html html) = html

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)