module HsBlog.Html.Internal
  ( Html,
    Head,
    Structure,
    Content,
    html_,
    p_,
    h_,
    ul_,
    ol_,
    code_,
    txt_,
    link_,
    image_,
    stylesheet_,
    title_,
    meta_,
    render,
  )
where

import Numeric.Natural (Natural)

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

newtype Head = Head String

instance Semigroup Structure where
  (<>) (Structure c1) (Structure c2) = Structure (c1 <> c2)

instance Monoid Structure where
  mempty = Structure ""

instance Semigroup Content where
  (<>) (Content c1) (Content c2) = Content (c1 <> c2)

instance Monoid Content where
  mempty = Content ""

instance Semigroup Head where
  (<>) (Head c1) (Head c2) = Head (c1 <> c2)

instance Monoid Head where
  mempty = Head ""

getStructureString :: Structure -> String
getStructureString (Structure str) = str

render :: Html -> String
render (Html str) = str

html_ :: Head -> Structure -> Html
html_ (Head head) (Structure body) =
  Html
    ( el
        "html"
        ( el "head" head
            <> el "body" body
        )
    )

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: Content -> Structure
p_ (Content c) = Structure (el "p" c)

h_ :: Natural -> Content -> Structure
h_ n (Content c) = Structure (el ("h" <> show n) c)

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ href (Content c) = Content ("<a href=\"" <> escape href <> "\">" <> escape c <> "</a>")

image_ :: FilePath -> Content
image_ src = Content ("<img src=\"" <> escape src <> "\" />")

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ src = Head ("<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape src <> "\" />")

meta_ :: String -> String -> Head
meta_ name content = Head ("<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\" />")

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar c = case c of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      _ -> [c]
