{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IHaskell.Display.Diagrams
  ( IHaskellBackend (..)
  , diagram, diagram'
  , module Diagrams.Prelude
#ifdef FSVG
  , SVG (..)
  , svgDiagram, svgDiagram'
#endif
#ifdef FRASTERIFIC
  , Rasterific (..)
  , rasterificDiagram, rasterificDiagram'
#endif
  ) where

import Diagrams.Prelude
#ifdef FSVG
import Diagrams.Backend.SVG
import Lucid.Svg
#endif
#ifdef FRASTERIFIC
import Codec.Picture
import Codec.Picture.Types (dropTransparency, convertPixel)
import Data.Char
import Data.Word
import Diagrams.Backend.Rasterific
import Data.ByteString.Lazy (toStrict)
#endif

-- import Data.Typeable
-- import GHC.Exts (Constraint)
import IHaskell.Display

defaultSpec2D :: Num n => SizeSpec V2 n
defaultSpec2D = mkHeight 350

-- | Display a diagram using the given backend token.
diagram :: (InSpace V2 n b, IHaskellBackend b n) => b -> Diagram b -> IO Display
diagram _ = displayDiagram

-- | Display a diagram using the given backend token and 'SizeSpec'.
diagram' :: IHaskellBackend b (N b) => b -> SizeSpec (V b) (N b) -> Diagram b -> IO Display
diagram' _ = displayDiagram'

class Num (N b) => IHaskellBackend b n where
  displayDiagram :: (V b ~ V2, Num n) => QDiagram b V2 n Any -> IO Display
  displayDiagram = displayDiagram' defaultSpec2D

  displayDiagram' :: SizeSpec (V b) n -> QDiagram b (V b) n Any -> IO Display

instance (V b ~ V2, IHaskellBackend b n, Num n) => IHaskellDisplay (QDiagram b V2 n Any) where
  display = displayDiagram

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

#ifdef FSVG
instance SVGFloat n => IHaskellBackend SVG n where
  displayDiagram' szSpec = display . svgDiagram' szSpec

-- | Render diagram using given 'SizeSpec2D'.
svgDiagram' :: SVGFloat n => SizeSpec V2 n -> QDiagram SVG V2 n Any -> DisplayData
svgDiagram' szSpec dia = svg $ toListOf each rendered
  where
    rendered = renderText $ renderDia SVG (SVGOptions szSpec [] "ihaskell-svg") dia

-- | Rendering hint.
svgDiagram :: Diagram SVG -> DisplayData
svgDiagram = svgDiagram' defaultSpec2D
#endif

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------

#ifdef FRASTERIFIC
instance TypeableFloat n => IHaskellBackend Rasterific n where
  displayDiagram' szSpec = display . rasterificDiagram' "png" 80 szSpec

-- | Render diagram using given 'SizeSpec2D'. Only supports jpeg and png formats.
rasterificDiagram' :: TypeableFloat n
                   => String -> Word8 -> SizeSpec V2 n -> QDiagram Rasterific V2 n Any -> DisplayData
rasterificDiagram' format q szSpec dia = encode img
  where
    encode | isAny ["jpg","jpeg"] = jpg w h . base64 . toStrict . encodeJpg
           | otherwise            = png w h . base64 . toStrict . encodePng
    isAny = any (== map toLower format)
    --
    img   = renderDia Rasterific (RasterificOptions szSpec) dia
    (w,h) = (imageWidth img, imageHeight img)
    q'    = min 100 q
    --
    encodeJpg = encodeJpegAtQuality q' . pixelMap (convertPixel . dropTransparency)

-- | Rendering hint.
rasterificDiagram :: Diagram Rasterific -> DisplayData
rasterificDiagram = rasterificDiagram' "png" 80 (mkHeight 350)
#endif

