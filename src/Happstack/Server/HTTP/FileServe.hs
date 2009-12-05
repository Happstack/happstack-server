{-# LANGUAGE FlexibleContexts, Rank2Types #-}
-- |File Serving functions
module Happstack.Server.HTTP.FileServe
    (
     -- * Content-Type \/ Mime-Type
     MimeMap,
     mimeTypes,
     asContentType,
     guessContentType,
     guessContentTypeM,
     -- * Low-Level
     sendFileResponse,     
     lazyByteStringResponse,
     strictByteStringResponse,
     filePathSendFile,
     filePathLazy,
     filePathStrict,
     -- * High-Level
     -- ** Serving a single file
     serveFile,
     serveFileUsing,
     -- ** Serving files from a directory
     fileServe',
     fileServe,
     fileServeLazy,
     fileServeStrict,
     -- * Other
     blockDotFiles,
     defaultIxFiles,
     doIndex,
     doIndex',
     doIndexLazy,
     doIndexStrict,
     errorwrapper,
     isDot
    ) where

import Control.Exception.Extensible (IOException, SomeException, Exception(fromException), bracket, handleJust)
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import Happstack.Server.SimpleHTTP (FilterMonad, ServerMonad(askRq), Request(..), Response(..), WebMonad, toResponse, resultBS, setHeader, forbidden, nullRsFlags, result, require, rsfContentLength, seeOther, ifModifiedSince )
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime)
import System.IO (IOMode(ReadMode), hFileSize, hClose, openBinaryFile)
import System.FilePath ((</>), addTrailingPathSeparator, joinPath, takeExtension)
import System.Log.Logger (Priority(DEBUG), logM)
import System.Time (CalendarTime, toUTCTime)

ioErrors :: SomeException -> Maybe IOException
ioErrors = fromException

errorwrapper :: (MonadIO m, MonadPlus m, FilterMonad Response m) => String -> String -> m Response
errorwrapper binarylocation loglocation
    = require getErrorLog $ \errorLog ->
      return $ toResponse errorLog
    where getErrorLog
                = handleJust ioErrors (const (return Nothing)) $
                do bintime <- getModificationTime binarylocation
                   logtime <- getModificationTime loglocation
                   if (logtime > bintime)
                     then fmap Just $ readFile loglocation
                     else return Nothing

-- * Mime-Type / Content-Type

type MimeMap = Map String String

-- | Ready collection of common mime types.
-- Except for the first two entries, the mappings come from an Ubuntu 8.04 /etc/mime.types file.
mimeTypes :: MimeMap
mimeTypes = Map.fromList [("gz","application/x-gzip"),("cabal","application/x-cabal"),("%","application/x-trash"),("323","text/h323"),("3gp","video/3gpp"),("7z","application/x-7z-compressed"),("abw","application/x-abiword"),("ai","application/postscript"),("aif","audio/x-aiff"),("aifc","audio/x-aiff"),("aiff","audio/x-aiff"),("alc","chemical/x-alchemy"),("art","image/x-jg"),("asc","text/plain"),("asf","video/x-ms-asf"),("asn","chemical/x-ncbi-asn1"),("aso","chemical/x-ncbi-asn1-binary"),("asx","video/x-ms-asf"),("atom","application/atom"),("atomcat","application/atomcat+xml"),("atomsrv","application/atomserv+xml"),("au","audio/basic"),("avi","video/x-msvideo"),("b","chemical/x-molconn-Z"),("bak","application/x-trash"),("bat","application/x-msdos-program"),("bcpio","application/x-bcpio"),("bib","text/x-bibtex"),("bin","application/octet-stream"),("bmp","image/x-ms-bmp"),("boo","text/x-boo"),("book","application/x-maker"),("bsd","chemical/x-crossfire"),("c","text/x-csrc"),("c++","text/x-c++src"),("c3d","chemical/x-chem3d"),("cab","application/x-cab"),("cac","chemical/x-cache"),("cache","chemical/x-cache"),("cap","application/cap"),("cascii","chemical/x-cactvs-binary"),("cat","application/vnd.ms-pki.seccat"),("cbin","chemical/x-cactvs-binary"),("cbr","application/x-cbr"),("cbz","application/x-cbz"),("cc","text/x-c++src"),("cdf","application/x-cdf"),("cdr","image/x-coreldraw"),("cdt","image/x-coreldrawtemplate"),("cdx","chemical/x-cdx"),("cdy","application/vnd.cinderella"),("cef","chemical/x-cxf"),("cer","chemical/x-cerius"),("chm","chemical/x-chemdraw"),("chrt","application/x-kchart"),("cif","chemical/x-cif"),("class","application/java-vm"),("cls","text/x-tex"),("cmdf","chemical/x-cmdf"),("cml","chemical/x-cml"),("cod","application/vnd.rim.cod"),("com","application/x-msdos-program"),("cpa","chemical/x-compass"),("cpio","application/x-cpio"),("cpp","text/x-c++src"),("cpt","application/mac-compactpro"),("crl","application/x-pkcs7-crl"),("crt","application/x-x509-ca-cert"),("csf","chemical/x-cache-csf"),("csh","application/x-csh"),("csm","chemical/x-csml"),("csml","chemical/x-csml"),("css","text/css"),("csv","text/csv"),("ctab","chemical/x-cactvs-binary"),("ctx","chemical/x-ctx"),("cu","application/cu-seeme"),("cub","chemical/x-gaussian-cube"),("cxf","chemical/x-cxf"),("cxx","text/x-c++src"),("d","text/x-dsrc"),("dat","chemical/x-mopac-input"),("dcr","application/x-director"),("deb","application/x-debian-package"),("dif","video/dv"),("diff","text/x-diff"),("dir","application/x-director"),("djv","image/vnd.djvu"),("djvu","image/vnd.djvu"),("dl","video/dl"),("dll","application/x-msdos-program"),("dmg","application/x-apple-diskimage"),("dms","application/x-dms"),("doc","application/msword"),("dot","application/msword"),("dv","video/dv"),("dvi","application/x-dvi"),("dx","chemical/x-jcamp-dx"),("dxr","application/x-director"),("emb","chemical/x-embl-dl-nucleotide"),("embl","chemical/x-embl-dl-nucleotide"),("eml","message/rfc822"),("ent","chemical/x-ncbi-asn1-ascii"),("eps","application/postscript"),("etx","text/x-setext"),("exe","application/x-msdos-program"),("ez","application/andrew-inset"),("fb","application/x-maker"),("fbdoc","application/x-maker"),("fch","chemical/x-gaussian-checkpoint"),("fchk","chemical/x-gaussian-checkpoint"),("fig","application/x-xfig"),("flac","application/x-flac"),("fli","video/fli"),("fm","application/x-maker"),("frame","application/x-maker"),("frm","application/x-maker"),("gal","chemical/x-gaussian-log"),("gam","chemical/x-gamess-input"),("gamin","chemical/x-gamess-input"),("gau","chemical/x-gaussian-input"),("gcd","text/x-pcs-gcd"),("gcf","application/x-graphing-calculator"),("gcg","chemical/x-gcg8-sequence"),("gen","chemical/x-genbank"),("gf","application/x-tex-gf"),("gif","image/gif"),("gjc","chemical/x-gaussian-input"),("gjf","chemical/x-gaussian-input"),("gl","video/gl"),("gnumeric","application/x-gnumeric"),("gpt","chemical/x-mopac-graph"),("gsf","application/x-font"),("gsm","audio/x-gsm"),("gtar","application/x-gtar"),("h","text/x-chdr"),("h++","text/x-c++hdr"),("hdf","application/x-hdf"),("hh","text/x-c++hdr"),("hin","chemical/x-hin"),("hpp","text/x-c++hdr"),("hqx","application/mac-binhex40"),("hs","text/x-haskell"),("hta","application/hta"),("htc","text/x-component"),("htm","text/html"),("html","text/html"),("hxx","text/x-c++hdr"),("ica","application/x-ica"),("ice","x-conference/x-cooltalk"),("ico","image/x-icon"),("ics","text/calendar"),("icz","text/calendar"),("ief","image/ief"),("iges","model/iges"),("igs","model/iges"),("iii","application/x-iphone"),("inp","chemical/x-gamess-input"),("ins","application/x-internet-signup"),("iso","application/x-iso9660-image"),("isp","application/x-internet-signup"),("ist","chemical/x-isostar"),("istr","chemical/x-isostar"),("jad","text/vnd.sun.j2me.app-descriptor"),("jar","application/java-archive"),("java","text/x-java"),("jdx","chemical/x-jcamp-dx"),("jmz","application/x-jmol"),("jng","image/x-jng"),("jnlp","application/x-java-jnlp-file"),("jpe","image/jpeg"),("jpeg","image/jpeg"),("jpg","image/jpeg"),("js","application/x-javascript"),("kar","audio/midi"),("key","application/pgp-keys"),("kil","application/x-killustrator"),("kin","chemical/x-kinemage"),("kml","application/vnd.google-earth.kml+xml"),("kmz","application/vnd.google-earth.kmz"),("kpr","application/x-kpresenter"),("kpt","application/x-kpresenter"),("ksp","application/x-kspread"),("kwd","application/x-kword"),("kwt","application/x-kword"),("latex","application/x-latex"),("lha","application/x-lha"),("lhs","text/x-literate-haskell"),("lsf","video/x-la-asf"),("lsx","video/x-la-asf"),("ltx","text/x-tex"),("lyx","application/x-lyx"),("lzh","application/x-lzh"),("lzx","application/x-lzx"),("m3u","audio/mpegurl"),("m4a","audio/mpeg"),("maker","application/x-maker"),("man","application/x-troff-man"),("mcif","chemical/x-mmcif"),("mcm","chemical/x-macmolecule"),("mdb","application/msaccess"),("me","application/x-troff-me"),("mesh","model/mesh"),("mid","audio/midi"),("midi","audio/midi"),("mif","application/x-mif"),("mm","application/x-freemind"),("mmd","chemical/x-macromodel-input"),("mmf","application/vnd.smaf"),("mml","text/mathml"),("mmod","chemical/x-macromodel-input"),("mng","video/x-mng"),("moc","text/x-moc"),("mol","chemical/x-mdl-molfile"),("mol2","chemical/x-mol2"),("moo","chemical/x-mopac-out"),("mop","chemical/x-mopac-input"),("mopcrt","chemical/x-mopac-input"),("mov","video/quicktime"),("movie","video/x-sgi-movie"),("mp2","audio/mpeg"),("mp3","audio/mpeg"),("mp4","video/mp4"),("mpc","chemical/x-mopac-input"),("mpe","video/mpeg"),("mpeg","video/mpeg"),("mpega","audio/mpeg"),("mpg","video/mpeg"),("mpga","audio/mpeg"),("ms","application/x-troff-ms"),("msh","model/mesh"),("msi","application/x-msi"),("mvb","chemical/x-mopac-vib"),("mxu","video/vnd.mpegurl"),("nb","application/mathematica"),("nc","application/x-netcdf"),("nwc","application/x-nwc"),("o","application/x-object"),("oda","application/oda"),("odb","application/vnd.oasis.opendocument.database"),("odc","application/vnd.oasis.opendocument.chart"),("odf","application/vnd.oasis.opendocument.formula"),("odg","application/vnd.oasis.opendocument.graphics"),("odi","application/vnd.oasis.opendocument.image"),("odm","application/vnd.oasis.opendocument.text-master"),("odp","application/vnd.oasis.opendocument.presentation"),("ods","application/vnd.oasis.opendocument.spreadsheet"),("odt","application/vnd.oasis.opendocument.text"),("oga","audio/ogg"),("ogg","application/ogg"),("ogv","video/ogg"),("ogx","application/ogg"),("old","application/x-trash"),("otg","application/vnd.oasis.opendocument.graphics-template"),("oth","application/vnd.oasis.opendocument.text-web"),("otp","application/vnd.oasis.opendocument.presentation-template"),("ots","application/vnd.oasis.opendocument.spreadsheet-template"),("ott","application/vnd.oasis.opendocument.text-template"),("oza","application/x-oz-application"),("p","text/x-pascal"),("p7r","application/x-pkcs7-certreqresp"),("pac","application/x-ns-proxy-autoconfig"),("pas","text/x-pascal"),("pat","image/x-coreldrawpattern"),("patch","text/x-diff"),("pbm","image/x-portable-bitmap"),("pcap","application/cap"),("pcf","application/x-font"),("pcf.Z","application/x-font"),("pcx","image/pcx"),("pdb","chemical/x-pdb"),("pdf","application/pdf"),("pfa","application/x-font"),("pfb","application/x-font"),("pgm","image/x-portable-graymap"),("pgn","application/x-chess-pgn"),("pgp","application/pgp-signature"),("php","application/x-httpd-php"),("php3","application/x-httpd-php3"),("php3p","application/x-httpd-php3-preprocessed"),("php4","application/x-httpd-php4"),("phps","application/x-httpd-php-source"),("pht","application/x-httpd-php"),("phtml","application/x-httpd-php"),("pk","application/x-tex-pk"),("pl","text/x-perl"),("pls","audio/x-scpls"),("pm","text/x-perl"),("png","image/png"),("pnm","image/x-portable-anymap"),("pot","text/plain"),("ppm","image/x-portable-pixmap"),("pps","application/vnd.ms-powerpoint"),("ppt","application/vnd.ms-powerpoint"),("prf","application/pics-rules"),("prt","chemical/x-ncbi-asn1-ascii"),("ps","application/postscript"),("psd","image/x-photoshop"),("py","text/x-python"),("pyc","application/x-python-code"),("pyo","application/x-python-code"),("qt","video/quicktime"),("qtl","application/x-quicktimeplayer"),("ra","audio/x-pn-realaudio"),("ram","audio/x-pn-realaudio"),("rar","application/rar"),("ras","image/x-cmu-raster"),("rd","chemical/x-mdl-rdfile"),("rdf","application/rdf+xml"),("rgb","image/x-rgb"),("rhtml","application/x-httpd-eruby"),("rm","audio/x-pn-realaudio"),("roff","application/x-troff"),("ros","chemical/x-rosdal"),("rpm","application/x-redhat-package-manager"),("rss","application/rss+xml"),("rtf","application/rtf"),("rtx","text/richtext"),("rxn","chemical/x-mdl-rxnfile"),("sct","text/scriptlet"),("sd","chemical/x-mdl-sdfile"),("sd2","audio/x-sd2"),("sda","application/vnd.stardivision.draw"),("sdc","application/vnd.stardivision.calc"),("sdd","application/vnd.stardivision.impress"),("sdf","application/vnd.stardivision.math"),("sds","application/vnd.stardivision.chart"),("sdw","application/vnd.stardivision.writer"),("ser","application/java-serialized-object"),("sgf","application/x-go-sgf"),("sgl","application/vnd.stardivision.writer-global"),("sh","application/x-sh"),("shar","application/x-shar"),("shtml","text/html"),("sid","audio/prs.sid"),("sik","application/x-trash"),("silo","model/mesh"),("sis","application/vnd.symbian.install"),("sisx","x-epoc/x-sisx-app"),("sit","application/x-stuffit"),("sitx","application/x-stuffit"),("skd","application/x-koan"),("skm","application/x-koan"),("skp","application/x-koan"),("skt","application/x-koan"),("smi","application/smil"),("smil","application/smil"),("snd","audio/basic"),("spc","chemical/x-galactic-spc"),("spl","application/futuresplash"),("spx","audio/ogg"),("src","application/x-wais-source"),("stc","application/vnd.sun.xml.calc.template"),("std","application/vnd.sun.xml.draw.template"),("sti","application/vnd.sun.xml.impress.template"),("stl","application/vnd.ms-pki.stl"),("stw","application/vnd.sun.xml.writer.template"),("sty","text/x-tex"),("sv4cpio","application/x-sv4cpio"),("sv4crc","application/x-sv4crc"),("svg","image/svg+xml"),("svgz","image/svg+xml"),("sw","chemical/x-swissprot"),("swf","application/x-shockwave-flash"),("swfl","application/x-shockwave-flash"),("sxc","application/vnd.sun.xml.calc"),("sxd","application/vnd.sun.xml.draw"),("sxg","application/vnd.sun.xml.writer.global"),("sxi","application/vnd.sun.xml.impress"),("sxm","application/vnd.sun.xml.math"),("sxw","application/vnd.sun.xml.writer"),("t","application/x-troff"),("tar","application/x-tar"),("taz","application/x-gtar"),("tcl","application/x-tcl"),("tex","text/x-tex"),("texi","application/x-texinfo"),("texinfo","application/x-texinfo"),("text","text/plain"),("tgf","chemical/x-mdl-tgf"),("tgz","application/x-gtar"),("tif","image/tiff"),("tiff","image/tiff"),("tk","text/x-tcl"),("tm","text/texmacs"),("torrent","application/x-bittorrent"),("tr","application/x-troff"),("ts","text/texmacs"),("tsp","application/dsptype"),("tsv","text/tab-separated-values"),("txt","text/plain"),("udeb","application/x-debian-package"),("uls","text/iuls"),("ustar","application/x-ustar"),("val","chemical/x-ncbi-asn1-binary"),("vcd","application/x-cdlink"),("vcf","text/x-vcard"),("vcs","text/x-vcalendar"),("vmd","chemical/x-vmd"),("vms","chemical/x-vamas-iso14976"),("vrm","x-world/x-vrml"),("vrml","model/vrml"),("vsd","application/vnd.visio"),("wad","application/x-doom"),("wav","audio/x-wav"),("wax","audio/x-ms-wax"),("wbmp","image/vnd.wap.wbmp"),("wbxml","application/vnd.wap.wbxml"),("wk","application/x-123"),("wm","video/x-ms-wm"),("wma","audio/x-ms-wma"),("wmd","application/x-ms-wmd"),("wml","text/vnd.wap.wml"),("wmlc","application/vnd.wap.wmlc"),("wmls","text/vnd.wap.wmlscript"),("wmlsc","application/vnd.wap.wmlscriptc"),("wmv","video/x-ms-wmv"),("wmx","video/x-ms-wmx"),("wmz","application/x-ms-wmz"),("wp5","application/wordperfect5.1"),("wpd","application/wordperfect"),("wrl","model/vrml"),("wsc","text/scriptlet"),("wvx","video/x-ms-wvx"),("wz","application/x-wingz"),("xbm","image/x-xbitmap"),("xcf","application/x-xcf"),("xht","application/xhtml+xml"),("xhtml","application/xhtml+xml"),("xlb","application/vnd.ms-excel"),("xls","application/vnd.ms-excel"),("xlt","application/vnd.ms-excel"),("xml","application/xml"),("xpi","application/x-xpinstall"),("xpm","image/x-xpixmap"),("xsl","application/xml"),("xtel","chemical/x-xtel"),("xul","application/vnd.mozilla.xul+xml"),("xwd","image/x-xwindowdump"),("xyz","chemical/x-xyz"),("zip","application/zip"),("zmt","chemical/x-mopac-input"),("~","application/x-trash")]


guessContentType :: MimeMap -> FilePath -> Maybe String
guessContentType mimeMap filepath =
    case getExt filepath of
      "" -> Nothing
      ext -> Map.lookup ext mimeMap

guessContentTypeM :: (Monad m) => MimeMap -> (FilePath -> m String)
guessContentTypeM mimeMap filePath = return $ fromMaybe "text/plain" $ guessContentType mimeMap filePath

asContentType :: (Monad m) => String -> (FilePath -> m String)
asContentType = const . return

defaultIxFiles :: [String]
defaultIxFiles= ["index.html","index.xml","index.gif"]

fileNotFound :: (Monad m, FilterMonad Response m) => FilePath -> m Response
fileNotFound fp = return $ result 404 $ "File not found " ++ fp

-- | Similar to 'takeExtension' but does not include the extension separator char
getExt :: FilePath -> String
getExt fp = drop 1 $ takeExtension fp

-- | Prevents files of the form '.foo' or 'bar/.foo' from being served
blockDotFiles :: (Request -> IO Response) -> Request -> IO Response
blockDotFiles fn rq
    | isDot (joinPath (rqPaths rq)) = return $ result 403 "Dot files not allowed."
    | otherwise = fn rq

-- | Returns True if the given String either starts with a . or is of the form
-- "foo/.bar", e.g. the typical *nix convention for hidden files.
isDot :: String -> Bool
isDot = isD . reverse
    where
    isD ('.':'/':_) = True
    isD ['.']       = True
    --isD ('/':_)     = False
    isD (_:cs)      = isD cs
    isD []          = False

-- * Low-level functions for generating a Response

-- | Use sendFile to send the contents of a Handle
sendFileResponse :: String  -- ^ content-type string
                 -> FilePath  -- ^ file path for content to send
                 -> Maybe (CalendarTime, Request) -- ^ mod-time for the handle (MUST NOT be later than server's time of message origination), incoming request (used to check for if-modified-since header)
                 -> Integer -- ^ offset into Handle
                 -> Integer -- ^ number of bytes to send
                 -> Response
sendFileResponse ct filePath mModTime _offset count =
    let res = ((setHeader "Content-Length" (show count)) .
               (setHeader "Content-Type" ct) $ 
               (SendFile 200 Map.empty nullRsFlags{rsfContentLength=False} Nothing filePath 0 count)
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the contents of a Lazy ByteString
lazyByteStringResponse :: String   -- ^ content-type string (e.g. @\"text/plain; charset=utf-8\"@)
                       -> L.ByteString   -- ^ lazy bytestring content to send
                       -> Maybe (CalendarTime, Request) -- ^ mod-time for the bytestring, incoming request (used to check for if-modified-since header)
                       -> Integer -- ^ offset into the bytestring
                       -> Integer -- ^ number of bytes to send (offset + count must be less than or equal to the length of the bytestring)
                       -> Response
lazyByteStringResponse ct body mModTime offset count =
    let res = ((setHeader "Content-Type" ct) $
               resultBS 200 (L.take (fromInteger count) $ (L.drop (fromInteger offset))  body)
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the contents of a Lazy ByteString
strictByteStringResponse :: String   -- ^ content-type string (e.g. @\"text/plain; charset=utf-8\"@)
                         -> S.ByteString   -- ^ lazy bytestring content to send
                         -> Maybe (CalendarTime, Request) -- ^ mod-time for the bytestring, incoming request (used to check for if-modified-since header)
                         -> Integer -- ^ offset into the bytestring
                         -> Integer -- ^ number of bytes to send (offset + count must be less than or equal to the length of the bytestring)
                         -> Response
strictByteStringResponse ct body mModTime offset count =
    let res = ((setHeader "Content-Type" ct) $
               resultBS 200 (L.fromChunks [S.take (fromInteger count) $ S.drop (fromInteger offset) body])
              )
    in case mModTime of
         Nothing -> res
         (Just (modTime, request)) -> ifModifiedSince modTime request res

-- | Send the specified file with the specified mime-type using sendFile()
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathSendFile :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathSendFile contentType fp =
    do handle  <- liftIO $ openBinaryFile fp ReadMode -- garbage collection should close this
       modtime <- liftIO $ getModificationTime fp
       count   <- liftIO $ hFileSize handle
       rq      <- askRq
       return $ sendFileResponse contentType fp (Just (toUTCTime modtime, rq)) 0 count

-- | Send the specified file with the specified mime-type using Lazy ByteStrings
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathLazy :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathLazy contentType fp =
    do handle  <- liftIO $ openBinaryFile fp ReadMode -- garbage collection should close this
       contents <- liftIO $ L.hGetContents handle
       modtime  <- liftIO $ getModificationTime fp
       count    <- liftIO $ hFileSize handle
       rq       <- askRq
       return $ lazyByteStringResponse contentType contents (Just (toUTCTime modtime, rq)) 0 count

-- | Send the specified file with the specified mime-type using Lazy ByteStrings
--
-- NOTE: assumes file exists and is readable by the server. See 'serveFileUsing'.
--
-- WARNING: No security checks are performed.
filePathStrict :: (ServerMonad m, MonadIO m)
                 => String   -- ^ content-type string
                 -> FilePath -- ^ path to file on disk
                 -> m Response
filePathStrict contentType fp =
    do contents <- liftIO $ S.readFile fp
       modtime  <- liftIO $ getModificationTime fp
       count    <- liftIO $ bracket (openBinaryFile fp ReadMode) hClose hFileSize
       rq       <- askRq
       return $ strictByteStringResponse contentType contents (Just (toUTCTime modtime, rq)) 0 count

-- * High-level functions for serving files


-- ** Serve a single file

-- | Serve a single, specified file.
-- 
-- example 1:
-- 
--  Serve using sendfile() and the specified content-type
--
-- > serveFileUsing filePathSendFile (asContentType "image/jpeg") "/srv/data/image.jpg"
--
--
-- example 2:
-- 
--  Serve using a lazy ByteString and the guess the content-type from the extension
-- 
-- > serveFileUsing filePathLazy (guessContentTypeM mimeTypes) "/srv/data/image.jpg"
-- 
-- WARNING: No security checks are performed.
serveFileUsing :: (ServerMonad m, FilterMonad Response m, MonadIO m) 
               => (String -> FilePath -> m Response) -- ^ typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict'
               -> (FilePath -> m String)  -- ^ function for determining content-type of file. Typically 'asContentType' or 'guessContentTypeM'
               -> FilePath -- ^ path to the file to serve
               -> m Response
serveFileUsing serveFn mimeFn fp = 
    do fe <- liftIO $ doesFileExist fp
       if fe
          then do mt <- mimeFn fp
                  serveFn mt fp
          else fileNotFound fp

-- | Alias for 'serveFileUsing' 'filePathSendFile'
serveFile :: (ServerMonad m, FilterMonad Response m, MonadIO m) => (FilePath -> m String) -> FilePath -> m Response
serveFile = serveFileUsing filePathSendFile

-- ** Serve files from a directory

-- | Serve files from a directory and it's subdirectories (parameterizable version)
-- 
-- Parameterize this function to create functions like, 'fileServe', 'fileServeLazy', and 'fileServeStrict'
--
-- You supply:
--
--  1. a low-level function which takes a content-type and 'FilePath' and generates a Response
--  2. a function which determines the content-type from the 'FilePath'
--  3. a list of all the default index files
--
-- NOTE: unlike fileServe, there are no index files by default. See 'defaultIxFiles'.
fileServe' :: ( WebMonad Response m
              , ServerMonad m
              , FilterMonad Response m
              , MonadIO m
              ) 
           => (String -> FilePath -> m Response) -- ^ function which takes a content-type and filepath and generates a response (typically 'filePathSendFile', 'filePathLazy', or 'filePathStrict')
           -> (FilePath -> m String) -- ^ function which returns the mime-type for FilePath
           -> [FilePath]         -- ^ index files if the path is a directory
           -> FilePath           -- ^ file/directory to serve
           -> m Response
fileServe' serveFn mimeFn ixFiles localpath = do
    rq <- askRq
    let safepath = filter (\x->not (null x) && x /= ".." && x /= ".") (rqPaths rq)
        fp = joinPath  (localpath:safepath)
    fe <- liftIO $ doesFileExist fp
    de <- liftIO $ doesDirectoryExist fp
    let status | de   = "DIR"
               | fe   = "file"
               | True = "NOT FOUND"
    liftIO $ logM "Happstack.Server.HTTP.FileServe" DEBUG ("fileServe: "++show fp++" \t"++status)
    if de
        then if last (rqUri rq) == '/'
             then doIndex' serveFn mimeFn (ixFiles++defaultIxFiles) fp
             else do let path' = addTrailingPathSeparator (rqUri rq)
                     seeOther path' (toResponse path')
        else if fe 
                then serveFileUsing serveFn mimeFn fp
                else fileNotFound fp

-- | Serve files from a directory and it's subdirectories (sendFile version). Should perform much better than its predecessors.
fileServe :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServe ixFiles localPath = fileServe' filePathSendFile (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- | Serve files from a directory and it's subdirectories (lazy ByteString version).
-- 
-- May leak file handles.
fileServeLazy :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeLazy ixFiles localPath = fileServe' filePathLazy (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- | Serve files from a directory and it's subdirectories (strict ByteString version). 
fileServeStrict :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
             [FilePath]         -- ^ index files if the path is a directory
          -> FilePath           -- ^ file/directory to serve
          -> m Response
fileServeStrict ixFiles localPath = fileServe' filePathStrict (guessContentTypeM mimeTypes) (ixFiles ++ defaultIxFiles) localPath

-- * Index

doIndex :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndex ixFiles mimeMap localPath = doIndex' filePathSendFile (guessContentTypeM mimeMap) ixFiles localPath

doIndexLazy :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndexLazy ixFiles mimeMap localPath = doIndex' filePathLazy (guessContentTypeM mimeMap) ixFiles localPath

doIndexStrict :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => [String]
        -> MimeMap
        -> String
        -> m Response
doIndexStrict ixFiles mimeMap localPath = doIndex' filePathStrict (guessContentTypeM mimeMap) ixFiles localPath

doIndex' :: (ServerMonad m, FilterMonad Response m, MonadIO m)
        => (String -> FilePath -> m Response)
        -> (FilePath -> m String)
        -> [String]
        -> String
        -> m Response
doIndex' _serveFn _mime  []          _fp = forbidden $ toResponse "Directory index forbidden"
doIndex'  serveFn mimeFn (index:rest) fp =
    do let path = fp </> index
       fe <- liftIO $ doesFileExist path
       if fe 
          then serveFileUsing serveFn mimeFn path 
          else doIndex' serveFn mimeFn rest fp
