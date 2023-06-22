module HsBlog.Convert
  ( convert,
    convertStructure,
  )
where

import HsBlog.Html
import HsBlog.Markup

convert :: Title -> Document -> Html
convert title = html_ title . foldMap convertStructure

convertStructure :: HsBlog.Markup.Structure -> HsBlog.Html.Structure
convertStructure structure = case structure of
  Heading n txt -> h_ n (txt_ txt)
  Paragraph p -> p_ (txt_ p)
  UnorderedList list -> ul_ $ map (p_ . txt_) list
  OrderedList list -> ol_ $ map (p_ . txt_) list
  CodeBlock list -> code_ (unlines list)
