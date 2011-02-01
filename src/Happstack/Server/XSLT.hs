{-# LANGUAGE TemplateHaskell, FlexibleInstances , UndecidableInstances,
             DeriveDataTypeable, MultiParamTypeClasses, CPP, ScopedTypeVariables,
    ScopedTypeVariables #-}
-- | Functions to allow you to use XSLT to transform your output. To use this, you would generally design your happstack application to output XML. The xslt filter will then run an external tool which performs the tranforms. The transformed result will then be sent to the http client as the Response.
--
-- NOTE: This module is currently looking for a maintainer. If you want to improve XSLT support in Happstack, please volunteer!
module Happstack.Server.XSLT
    (xsltFile, xsltString, {- xsltElem, -} xsltFPS, xsltFPSIO, XSLPath,
     xslt, doXslt, xsltproc,saxon,procFPSIO,procLBSIO,XSLTCommand,XSLTCmd
    ) where


import System.Log.Logger

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8           as B
import Happstack.Server.SimpleHTTP

import Happstack.Server.Types
-- import Happstack.Server.MinHaXML
import Happstack.Util.Common(runCommand)
import Control.Exception.Extensible(bracket,try,SomeException)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory(removeFile)
import System.Environment(getEnv)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
-- import Text.XML.HaXml.Verbatim(verbatim)
import Happstack.Data hiding (Element)

logMX :: Priority -> String -> IO ()
logMX = logM "Happstack.Server.XSLT"

type XSLPath = FilePath

$(deriveAll [''Show,''Read,''Default, ''Eq, ''Ord]
   [d|
       data XSLTCmd = XSLTProc | Saxon 
    |]
   )

xsltCmd :: XSLTCmd
           -> XSLPath
           -> FilePath
           -> FilePath
           -> (FilePath, [String])
xsltCmd XSLTProc = xsltproc'
xsltCmd Saxon = saxon'
{-
-- | Uses 'xsltString' to transform the given XML 'Element' into a
-- a 'String'.    
xsltElem :: XSLPath -> Element -> String
xsltElem xsl = xsltString xsl . verbatim
-}

procLBSIO :: XSLTCmd -> XSLPath -> L.ByteString -> IO L.ByteString
procLBSIO xsltp' xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    let xsltp = xsltCmd xsltp'
    L.hPut sh inp
    hClose sh
    hClose dh
    xsltFileEx xsltp xsl sfp dfp
    s <- L.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return s


procFPSIO :: XSLTCommand
             -> XSLPath
             -> [P.ByteString]
             -> IO [P.ByteString]
procFPSIO xsltp xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    mapM_ (P.hPut sh) inp
    hClose sh
    hClose dh
    xsltFileEx xsltp xsl sfp dfp
    s <- P.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return [s]

-- | Performs an XSL transformation with lists of ByteStrings instead of
-- a String.
xsltFPS :: XSLPath -> [P.ByteString] -> [P.ByteString]
xsltFPS xsl = unsafePerformIO . xsltFPSIO xsl

-- | Equivalent to 'xsltFPS' but does not hide the inherent IO of the low-level
-- ByteString operations.
xsltFPSIO :: XSLPath -> [P.ByteString] -> IO [P.ByteString]
xsltFPSIO xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    mapM_ (P.hPut sh) inp
    hClose sh
    hClose dh
    xsltFile xsl sfp dfp
    s <- P.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return [s]

-- | Uses the provided xsl file to transform the given string.
-- This function creates temporary files during its execution, but
-- guarantees their cleanup.
xsltString :: XSLPath -> String -> String
xsltString xsl inp = unsafePerformIO $
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    hPutStr sh inp
    hClose sh
    hClose dh
    xsltFile xsl sfp dfp
    s <- readFileStrict dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return s

-- | Note that the xsl file must have .xsl suffix.
xsltFile :: XSLPath -> FilePath -> FilePath -> IO ()
xsltFile = xsltFileEx xsltproc'

-- | Use @xsltproc@ to transform XML.
xsltproc :: XSLTCmd
xsltproc = XSLTProc
xsltproc' :: XSLTCommand
xsltproc' dst xsl src = ("xsltproc",["-o",dst,xsl,src])


-- | Use @saxon@ to transform XML.
saxon :: XSLTCmd
saxon = Saxon
saxon' :: XSLTCommand
saxon' dst xsl src = ("java -classpath /usr/share/java/saxon.jar",
                     ["com.icl.saxon.StyleSheet"
                     ,"-o",dst,src,xsl])
                        
type XSLTCommand = XSLPath -> FilePath -> FilePath -> (FilePath,[String])
xsltFileEx   :: XSLTCommand -> XSLPath -> FilePath -> FilePath -> IO ()
xsltFileEx xsltp xsl src dst = do
    let msg = (">>> XSLT: Starting xsltproc " ++ unwords ["-o",dst,xsl,src])
    logMX DEBUG msg
    uncurry runCommand $ xsltp dst xsl src
    logMX DEBUG (">>> XSLT: xsltproc done")

-- Utilities

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile str hand = bracket (openTempFile tempDir str) (removeFile . fst) (uncurry hand)

readFileStrict :: FilePath -> IO String
readFileStrict fp = do
    let fseqM [] = return [] 
        fseqM xs = last xs `seq` return xs
    fseqM =<< readFile fp

{-# NOINLINE tempDir #-}
tempDir :: FilePath
tempDir = unsafePerformIO $ tryAny [getEnv "TEMP",getEnv "TMP"] err
    where err = return "/tmp"

tryAny :: [IO a] -> IO a -> IO a
tryAny [] c     = c
tryAny (x:xs) c = either (\(_::SomeException) -> tryAny xs c) return =<< try x

-- | Use @cmd@ to transform XML against @xslPath@.  This function only
-- acts if the content-type is @application\/xml@.
xslt :: (MonadIO m, MonadPlus m, ToMessage r) =>
        XSLTCmd  -- ^ XSLT preprocessor. Usually 'xsltproc' or 'saxon'.
     -> XSLPath      -- ^ Path to xslt stylesheet.
     -> m r -- ^ Affected 'ServerPart's.
     -> m Response
xslt cmd xslPath parts = do
    res <- parts
    if toContentType res == B.pack "application/xml"
        then doXslt cmd xslPath (toResponse res)
        else return (toResponse res)

doXslt :: (MonadIO m) =>
          XSLTCmd -> XSLPath -> Response -> m Response
doXslt cmd xslPath res =
    do new <- liftIO $ procLBSIO cmd xslPath $ rsBody res
       return $ setHeader "Content-Type" "text/html" $
              setHeader "Content-Length" (show $ L.length new) $
              res { rsBody = new }