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
  Heading n txt -> h_ n (txt_ txt)
  Paragraph p -> p_ (txt_ p)
  UnorderedList list -> ul_ $ map (p_ . txt_) list
  OrderedList list -> ol_ $ map (p_ . txt_) list
  CodeBlock list -> code_ (unlines list)

buildIndex :: [(FilePath, Document)] -> Html
buildIndex list =
  html_
    "Blog"
    ( h_ 1 (link_ "index.html" (txt_ "Blog"))
        <> h_ 2 (txt_ "Posts")
        <> foldMap buildIndexEntry list
    )

buildIndexEntry :: (FilePath, Document) -> HsBlog.Html.Structure
buildIndexEntry (path, doc) = case doc of
  Heading 1 t : article ->
    h_ 3 (link_ path (txt_ t))
      <> foldMap converStructure (take 3 article)
      <> p_ (link_ path (txt_ "..."))
  _ -> h_ 3 (link_ path (txt_ path))
