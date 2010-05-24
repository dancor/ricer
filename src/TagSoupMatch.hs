module TagSoupMatch where

import Text.HTML.TagSoup

-- why are these hidden in tagsoup now?

anyAttr :: ((str,str) -> Bool) -> [Attribute str] -> Bool
anyAttr = any

-- | match an opening tag
tagOpen :: (str -> Bool) -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | match a opening tag's name literally
tagOpenLit :: Eq str => str -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpenLit name = tagOpen (name==)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
tagOpenAttrNameLit :: Eq str => str -> str -> (str -> Bool) -> Tag str -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))
