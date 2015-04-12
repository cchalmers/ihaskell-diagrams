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
  , getDefSize, setDefSize
  , module Diagrams.Prelude
#ifdef FSVG
  -- * SVG helpers
  , SVG (..)
  , svgDiagram, svgDiagram'
#endif
#ifdef FRASTERIFIC
  -- * Rasterific helpers
  , Rasterific (..)
  , pngDiagram, pngDiagram'
  , jpgDiagram, jpgDiagram'
#endif
#ifdef FCAIRO
  -- * Cairo helpers
  , Cairo (..)
  , svgCDiagram, svgCDiagram'
  , pngCDiagram, pngCDiagram'
#endif
  ) where

import Diagrams.Prelude
import Data.IORef
import System.IO.Unsafe
#ifdef FSVG
import Diagrams.Backend.SVG as SVG
import Lucid.Svg
#endif
#ifdef FRASTERIFIC
import Codec.Picture
import Codec.Picture.Types (dropTransparency, convertPixel)
import Data.Word
import Diagrams.Backend.Rasterific
import Data.ByteString.Lazy (toStrict)
#endif
#ifdef FCAIRO
import Diagrams.Backend.Cairo as Cairo
import Data.ByteString.Char8 as B (readFile, unpack)
#endif

import IHaskell.Display

-- This is a reference to the default size that can be changed.
defaultSize :: IORef (SizeSpec V2 Int)
defaultSize = unsafePerformIO $ newIORef (mkHeight 350)
{-# NOINLINE defaultSize #-}

-- | Get the current default size for rendering an ihaskell diagram.
getDefSize :: Num n => IO (SizeSpec V2 n)
getDefSize = readIORef defaultSize <&> fmap fromIntegral

-- | Set the default size for ihaskell diagrams. This will only hold for
--   the current ihaskell session. The default size is @'mkHeight' 350@.
setDefSize :: SizeSpec V2 Int -> IO ()
setDefSize = writeIORef defaultSize

-- | Display a diagram using the given backend token.
diagram :: (InSpace V2 n b, IHaskellBackend b n) => b -> Diagram b -> IO Display
diagram _ = displayDiagram

-- | Display a diagram using the given backend token and 'SizeSpec'.
diagram' :: IHaskellBackend b (N b) => b -> SizeSpec (V b) (N b) -> Diagram b -> IO Display
diagram' _ = displayDiagram'

-- | Class for backends that can be displayed in ihaskell.
class Num (N b) => IHaskellBackend b n where
  displayDiagram :: (V b ~ V2, Num n) => QDiagram b V2 n Any -> IO Display
  displayDiagram d = getDefSize >>= \sz -> displayDiagram' sz d

  displayDiagram' :: SizeSpec (V b) n -> QDiagram b (V b) n Any -> IO Display

instance (V b ~ V2, IHaskellBackend b n, Num n) => IHaskellDisplay (QDiagram b V2 n Any) where
  display = displayDiagram

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

#ifdef FSVG
instance SVGFloat n => IHaskellBackend SVG n where
  displayDiagram' szSpec = display . svgDiagram' szSpec

-- | Render a SVG diagram to svg 'DisplayData' using given 'SizeSpec'.
svgDiagram' :: SVGFloat n => SizeSpec V2 n -> QDiagram SVG V2 n Any -> DisplayData
svgDiagram' szSpec dia = svg $ toListOf each rendered
  where
    rendered = renderText $ renderDia SVG.SVG (SVGOptions szSpec [] "ihaskell-svg") dia

-- | Render a SVG diagram to svg 'DisplayData'.
svgDiagram :: Diagram SVG -> IO DisplayData
svgDiagram d = getDefSize <&> \sz -> svgDiagram' sz d
#endif

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------

#ifdef FRASTERIFIC
instance TypeableFloat n => IHaskellBackend Rasterific n where
  displayDiagram' szSpec = display . pngDiagram' szSpec

-- | Render a Rasterific diagram using given 'SizeSpec' to jpg 'DisplayData'.
pngDiagram' :: TypeableFloat n
            => SizeSpec V2 n -> QDiagram Rasterific V2 n Any -> DisplayData
pngDiagram' szSpec dia = png w h . base64 . toStrict $ encodePng img
  where
    img   = renderDia Rasterific (RasterificOptions szSpec) dia
    (w,h) = (imageWidth img, imageHeight img)

-- | Render a Rasterific diagram using given jpg quality (between 0 and
--   100) and 'SizeSpec' to jpg 'DisplayData'.
jpgDiagram' :: TypeableFloat n
            => Word8 -> SizeSpec V2 n -> QDiagram Rasterific V2 n Any -> DisplayData
jpgDiagram' q szSpec dia = png w h . base64 . toStrict $ encodeJpg img
  where
    img   = renderDia Rasterific (RasterificOptions szSpec) dia
    (w,h) = (imageWidth img, imageHeight img)
    q'    = min 100 q
    --
    encodeJpg = encodeJpegAtQuality q' . pixelMap (convertPixel . dropTransparency)

-- | Render a Rasterific diagram to jpg 'DisplayData'.
jpgDiagram :: Diagram Rasterific -> IO DisplayData
jpgDiagram d = getDefSize <&> \sz -> jpgDiagram' 80 sz d

-- | Render a Rasterific diagram to png 'DisplayData'.
pngDiagram :: Diagram Rasterific -> IO DisplayData
pngDiagram d = getDefSize <&> \sz -> pngDiagram' sz d
#endif

------------------------------------------------------------------------
-- Cairo
------------------------------------------------------------------------

#ifdef FCAIRO
-- | Renders Cairo SVGs.
instance (n ~ Double) => IHaskellBackend Cairo n where
  displayDiagram' szSpec = display . svgCDiagram' szSpec

cairoData :: OutputType -> SizeSpec V2 Double -> Diagram Cairo -> IO DisplayData
cairoData format sz d = do
  let filename = "diagram." ++ extension format
      V2 w h = fst $ sizeAdjustment sz (boundingBox d)

  switchToTmpDir

  -- Cairo doesn't appear to have an easy way to retrive the raw data
  -- without writing to file.
  renderCairo filename sz d
  imgData <- B.readFile filename
  return $ case format of
    PNG -> png (floor w) (floor h) $ base64 imgData
    _   -> svg $ unpack imgData
  where
    extension Cairo.SVG = "svg"
    extension Cairo.PNG = "png"
    extension _         = error "cairoData: unsupported format for DisplayData"

-- | Render a Rasterific diagram using given 'SizeSpec' to jpg 'DisplayData'.
pngCDiagram' :: SizeSpec V2 Double -> Diagram Cairo -> IO DisplayData
pngCDiagram' = cairoData PNG

-- | Render a Rasterific diagram using given 'SizeSpec' to jpg 'DisplayData'.
svgCDiagram' :: SizeSpec V2 Double -> Diagram Cairo -> IO DisplayData
svgCDiagram' = cairoData Cairo.SVG

-- | Render a Cairo diagram to png 'DisplayData'.
pngCDiagram :: Diagram Cairo -> IO DisplayData
pngCDiagram d = getDefSize >>= \sz -> pngCDiagram' sz d


svgCDiagram :: Diagram Cairo -> IO DisplayData
svgCDiagram d = getDefSize >>= \sz -> svgCDiagram' sz d
#endif

