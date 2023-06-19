module HsBlog.Html.Internal where
import Numeric.Natural ( Natural )

newtype Html = Html String
newtype Structure = Structure String
type Title = String

instance Semigroup Structure where
    (<>) (Structure c1) (Structure c2) = Structure (c1 <> c2)

instance Monoid Structure where
    mempty = empty_

render :: Html -> String
render (Html str) = str

empty_ :: Structure
empty_ = Structure ""

html_ :: Title -> Structure -> Html
html_ title (Structure body) = Html
    (el "html"
        (el "head"
            (el "title" (escape title)) <> el "body" body
        )
    )

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape = concatMap escapeChar where
    escapeChar c = case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
