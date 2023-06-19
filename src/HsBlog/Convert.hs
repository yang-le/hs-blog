module HsBlog.Convert
  ( convert,
  )
where

import HsBlog.Html
import HsBlog.Markup

convert :: Title -> Document -> Html
convert title = html_ title . foldMap converStructure

converStructure :: HsBlog.Markup.Structure -> HsBlog.Html.Structure
converStructure structure = case structure of
  Heading n txt -> h_ n txt
  Paragraph p -> p_ p
  UnorderedList list -> ul_ $ map p_ list
  OrderedList list -> ol_ $ map p_ list
  CodeBlock list -> code_ (unlines list)
