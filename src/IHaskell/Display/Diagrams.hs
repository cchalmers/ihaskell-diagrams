{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE TypeApplications            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IHaskell.Display.Diagrams
  -- ( IHaskellBackend (..)
    -- diagram, diagram'
  ( getDefSize, setDefSize
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
#ifdef FHTML5
  -- * Cairo helpers
  , Html5 (..)
  , canvasDiagram, canvasDiagram'
#endif
#ifdef FCAIRO
  -- * Cairo helpers
  , Cairo (..)
  , svgCDiagram, svgCDiagram'
  , pngCDiagram, pngCDiagram'
#endif
  ) where

import Plots hiding (display)

import Diagrams.Prelude
import Data.IORef
import System.IO.Unsafe
import Data.Text (Text)
import Data.Unique
import IHaskell.Types (DisplayData (..), MimeType (MimeSvg, MimeHtml))

import Data.ByteString.Lazy
import Codec.Picture

-- #ifdef FSVG
import Diagrams.Backend.SVG as SVG
-- import Lucid.Svg
import Graphics.Svg
-- #endif

#ifdef FRASTERIFIC
import Codec.Picture
import Codec.Picture.Types (dropTransparency, convertPixel)
import Data.Word
import Diagrams.Backend.Rasterific
import Data.ByteString.Lazy (toStrict)
#endif
#ifdef FHTML5
import Diagrams.Backend.Html5 as Html5
import Data.Text.Lazy.Builder
#endif
-- #ifdef FCAIRO
import qualified Diagrams.Backend.Cairo as Cairo
import qualified Data.ByteString.Char8 as B (readFile, unpack)
-- #endif

import IHaskell.Display

-- This is a reference to the default size, which can be changed.
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

-- | Certain backends need a unique
unique :: IO Int
unique = hashUnique <$> newUnique

-- | Convert svg 'Text' to 'DisplayData'. (This is what 'svg' should
--   probably be.)
svg' :: Text -> DisplayData
svg' = DisplayData MimeHtml

-- | Convert svg 'Text' to 'DisplayData'. (This is what 'html' should
--   probably be.)
html' :: Text -> DisplayData
html' = DisplayData MimeHtml

-- | Display a diagram using the given backend token.
-- diagram :: (InSpace V2 n b, IHaskellBackend b n) => b -> Diagram b -> IO Display
-- diagram _ = displayDiagram

-- | Display a diagram using the given backend token and 'SizeSpec'.
-- diagram' :: IHaskellBackend b (N b) => b -> SizeSpec (V b) (N b) -> Diagram b -> IO Display
-- diagram' _ = displayDiagram'

-- | Class for backends that can be displayed in ihaskell.
-- class Num (N b) => IHaskellBackend b n where
--   displayDiagram :: (V b ~ V2, Num n) => QDiagram b V2 n Any -> IO Display
--   displayDiagram d = getDefSize >>= \sz -> displayDiagram' sz d

  -- displayDiagram' :: SizeSpec (V b) n -> QDiagram b (V b) n Any -> IO Display

instance (v ~ V2, n ~ Double, m ~ Any) => IHaskellDisplay (QDiagram v n m) where
  -- display d = Display . pure <$> svgDiagram d
  display d = Display . pure <$> (getDefSize >>= \sz -> cairoPng sz d)

instance IHaskellDisplay (Axis V2) where
  display = display . renderAxis

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

-- | Render a SVG diagram to svg 'DisplayData' using given 'SizeSpec'.
svgDiagram' :: SizeSpec V2 Int -> Diagram V2 -> DisplayData
svgDiagram' szSpec dia = svg' (view strict rendered)
  where
    rendered = renderText . view _3 $ renderDiaT opts dia
    opts = mkOptions @SVG szSpec

svgDiagram :: Diagram V2 -> IO DisplayData
svgDiagram d = getDefSize <&> \sz -> svgDiagram' sz d

#ifdef FSVG
instance SVGFloat n => IHaskellBackend SVG n where
  displayDiagram' szSpec = display . svgDiagram' szSpec

-- | Render a SVG diagram to svg 'DisplayData'.
#endif

------------------------------------------------------------------------
-- Rasterific
------------------------------------------------------------------------

-- TODO: support 2x for high res displays

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
-- HTML5
------------------------------------------------------------------------

-- TODO: support 2x for high res displays

#ifdef FHTML5
-- | Renders Cairo SVGs.
instance (n ~ Double) => IHaskellBackend Html5 n where
  displayDiagram' szSpec = display . canvasDiagram' szSpec

-- | Render a Html5 diagram using given 'SizeSpec' to jpg 'DisplayData'.
canvasDiagram' :: SizeSpec V2 Double -> Diagram Html5 -> IO DisplayData
canvasDiagram' szSpec dia = unique <&> html' . view strict . rendered
  where
    rendered i = toLazyText $ renderDia Html5 (Html5Options szSpec False nm) dia
      where nm = "id" ++ show i

-- | Render a Html5 diagram using the default 'SizeSpec' from 'getDefSize'.
canvasDiagram :: Diagram Html5 -> IO DisplayData
canvasDiagram d = getDefSize >>= \sz -> canvasDiagram' sz d
#endif

------------------------------------------------------------------------
-- Cairo
------------------------------------------------------------------------

-- #ifdef FCAIRO
-- | Renders Cairo SVGs.
-- instance (n ~ Double) => IHaskellBackend Cairo n where
--   displayDiagram' szSpec = display . svgCDiagram' szSpec

cairoPng :: SizeSpec V2 Double -> Diagram V2 -> IO DisplayData
cairoPng sz dia = do
  img <- Cairo.rasterDia (fmap ceiling sz) dia
  let w = imageWidth img
      h = imageHeight img
  pure . png w h . base64 . toStrict $ encodePng img
  -- where
  -- let filename = "diagram.png"
  --     -- V2 w h = fst $ sizeAdjustment sz (boundingBox d)

  -- saveDiagram
  -- switchToTmpDir

  -- -- Cairo doesn't appear to have an easy way to retrive the raw data
  -- -- without writing to file.
  -- renderCairo filename sz d
  -- imgData <- B.readFile filename
  -- return $ case format of
  --   PNG -> png (floor w) (floor h) $ base64 imgData
  --   _   -> svg $ unpack imgData
-- --   where
-- --     extension Cairo.SVG = "svgo
-- --     extension Cairo.PNG = "png"
-- --     extension _         = error "cairoData: unsupported format for DisplayData"

-- pngCDiagram :: Diagram Cairo -> IO DisplayData
-- pngCDiagram = getDefSize <&> cairoData PNG

-- -- | Render a Rasterific diagram using given 'SizeSpec' to jpg 'DisplayData'.
-- pngCDiagram' :: SizeSpec V2 Double -> Diagram Cairo -> IO DisplayData
-- pngCDiagram' = cairoData PNG

-- -- | Render a Rasterific diagram using given 'SizeSpec' to jpg 'DisplayData'.
-- svgCDiagram' :: SizeSpec V2 Double -> Diagram Cairo -> IO DisplayData
-- svgCDiagram' = cairoData Cairo.SVG

-- -- | Render a Cairo diagram to png 'DisplayData'.
-- pngCDiagram :: Diagram Cairo -> IO DisplayData
-- pngCDiagram d = getDefSize >>= \sz -> pngCDiagram' sz d

-- svgCDiagram :: Diagram Cairo -> IO DisplayData
-- svgCDiagram d = getDefSize >>= \sz -> svgCDiagram' sz d
-- -- #endif

