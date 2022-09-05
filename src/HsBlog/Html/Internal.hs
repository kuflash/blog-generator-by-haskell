module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

instance Semigroup Structure where
  (<>) a b =
    Structure (getStructureString a <> getStructureString b)

instance Monoid Structure where
  mempty = Structure ""

getStructureString :: Structure -> String
getStructureString (Structure content) = content

getContentString :: Content -> String
getContentString (Content content) = content

render :: Html -> String
render (Html html) = html

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

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

h_ :: Natural -> Content -> Structure
h_ level = Structure . el ("h" <> show level) . escape . getContentString

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)