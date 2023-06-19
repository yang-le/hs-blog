module HsBlog.Convert where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap converStructure

converStructure :: Markup.Structure -> Html.Structure
converStructure structure = case structure of
    Markup.Heading n txt -> Html.h_ n txt
    Markup.Paragraph p -> Html.p_ p
    Markup.UnorderedList list -> Html.ul_ $ map Html.p_ list
    Markup.OrderedList list -> Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list -> Html.code_ (unlines list)
