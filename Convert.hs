module Convert where

import qualified Html
import qualified Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading level txt ->
      Html.h_ level txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock code ->
      Html.code_ (unlines code)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
