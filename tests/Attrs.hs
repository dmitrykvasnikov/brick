{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Attribute merging with the underline shape, underline color, and
-- overline fields, and overline in theme files.
module Attrs
  ( main
  )
where

import Control.Exception (finally)
import Data.Bits ((.|.))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import Test.QuickCheck

import Brick.AttrMap
import Brick.Themes

genMaybeDefault :: Gen a -> Gen (V.MaybeDefault a)
genMaybeDefault g = oneof [pure V.Default, pure V.KeepCurrent, V.SetTo <$> g]

genColor :: Gen V.Color
genColor = elements [V.red, V.ISOColor 12, V.Color240 100, V.RGBColor 239 101 96]

newtype A = A V.Attr
    deriving (Show)

instance Arbitrary A where
    arbitrary = fmap A $ V.Attr
        <$> genMaybeDefault (elements [V.bold, V.underline, V.overline, V.italic .|. V.overline])
        <*> genMaybeDefault genColor
        <*> genMaybeDefault genColor
        <*> genMaybeDefault (elements ["https://a.example", "https://b.example"])
        <*> genMaybeDefault (elements [minBound .. maxBound])
        <*> genMaybeDefault genColor

-- | What merging should give for a field: the more specific value if
-- it is set, else the less specific one if that is set.
expected :: V.MaybeDefault a -> V.MaybeDefault a -> V.MaybeDefault a
expected _ v@(V.SetTo _) = v
expected v@(V.SetTo _) _ = v
expected _ v = v

-- | 'mergeWithDefault' merges the attribute into the map's default.
merge :: V.Attr -> V.Attr -> V.Attr
merge d a = mergeWithDefault a (attrMap d [])

prop_mergeUnderlineStyle :: A -> A -> Bool
prop_mergeUnderlineStyle (A d) (A a) =
    V.attrUnderlineStyle (merge d a) == expected (V.attrUnderlineStyle d) (V.attrUnderlineStyle a)

prop_mergeUnderlineColor :: A -> A -> Bool
prop_mergeUnderlineColor (A d) (A a) =
    V.attrUnderlineColor (merge d a) == expected (V.attrUnderlineColor d) (V.attrUnderlineColor a)

prop_mergeColorsAndURL :: A -> A -> Bool
prop_mergeColorsAndURL (A d) (A a) =
    let m = merge d a
    in V.attrForeColor m == expected (V.attrForeColor d) (V.attrForeColor a)
       && V.attrBackColor m == expected (V.attrBackColor d) (V.attrBackColor a)
       && V.attrURL m == expected (V.attrURL d) (V.attrURL a)

prop_mergeStylesCombine :: A -> A -> Bool
prop_mergeStylesCombine (A d) (A a) =
    V.attrStyle (merge d a) == case (V.attrStyle d, V.attrStyle a) of
        (V.SetTo s, V.SetTo t) -> V.SetTo (s .|. t)
        (s, t) -> expected s t

-- | A lookup merges along the name: the shape comes from the more
-- specific name, the color and the overline from the less specific one.
prop_lookupAlongName :: Bool
prop_lookupAlongName =
    let m = attrMap V.defAttr
            [ (attrName "link", V.defAttr `V.withUnderlineColor` V.blue `V.withStyle` V.overline)
            , (attrName "link" <> attrName "visited", V.defAttr `V.withUnderlineStyle` V.DottedUnderline)
            ]
        a = attrMapLookup (attrName "link" <> attrName "visited") m
    in V.attrUnderlineStyle a == V.SetTo V.DottedUnderline
       && V.attrUnderlineColor a == V.SetTo V.blue
       && V.attrStyle a == V.SetTo (V.overline .|. V.underline)

theme :: Theme
theme = newTheme V.defAttr [(attrName "x", V.defAttr)]

-- | Run the action with a temporary file holding the given text.
withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content act = do
    dir <- getTemporaryDirectory
    (path, h) <- openTempFile dir "brick-theme.ini"
    hPutStr h content >> hClose h
    act path `finally` removeFile path

prop_themeFileReadsOverline :: Property
prop_themeFileReadsOverline = once $ ioProperty $
    withTempFile "[other]\nx.style = [overline, bold]\n" $ \path -> do
        result <- loadCustomizations path theme
        return $ case result of
            Left _ -> False
            Right t -> V.attrStyle (attrMapLookup (attrName "x") (themeToAttrMap t))
                       == V.SetTo (V.overline .|. V.bold)

prop_themeFileWritesOverline :: Property
prop_themeFileWritesOverline = once $ ioProperty $
    withTempFile "" $ \path -> do
        saveTheme path $ newTheme V.defAttr [(attrName "x", V.defAttr `V.withStyle` V.overline)]
        content <- readFile path
        length content `seq` return (T.pack "x.style = overline" `T.isInfixOf` T.pack content)

return []

main :: IO Bool
main = $quickCheckAll
