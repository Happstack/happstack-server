{-# LANGUAGE TemplateHaskell, FlexibleInstances , UndecidableInstances,
             DeriveDataTypeable, MultiParamTypeClasses, CPP, ScopedTypeVariables,
    PatternSignatures #-}
-- | Implement XSLT transformations using xsltproc
module Happstack.Server.XSLT
    (xsltFile, xsltString, xsltElem, xsltFPS, xsltFPSIO, XSLPath,
     xsltproc,saxon,procFPSIO,procLBSIO,XSLTCommand,XSLTCmd
    ) where


import System.Log.Logger

import Happstack.Server.MinHaXML
import Happstack.Util.Common(runCommand)
import Control.Exception.Extensible(bracket,try,SomeException)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory(removeFile)
import System.Environment(getEnv)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Text.XML.HaXml.Verbatim(verbatim)
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

xsltElem :: XSLPath -> Element -> String
xsltElem xsl = xsltString xsl . verbatim


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

xsltFPS :: XSLPath -> [P.ByteString] -> [P.ByteString]
xsltFPS xsl = unsafePerformIO . xsltFPSIO xsl

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
xsltproc' :: XSLTCommand
xsltproc' dst xsl src = ("xsltproc",["-o",dst,xsl,src])
xsltproc :: XSLTCmd
xsltproc = XSLTProc

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
