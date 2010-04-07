{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances, TypeSynonymInstances #-}


  
module Happstack.Server.MinHaXML where
-- Copyright (c) Happstack.com 2009; (c) HAppS.org 2005. All Rights Reserved.

import Prelude hiding (elem, pi)

import Text.XML.HaXml.Types as Types
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Pretty as Pretty
import Text.XML.HaXml.Verbatim as Verbatim
import Happstack.Util.Common
import System.Time

import Happstack.Data.Xml as Xml
import Happstack.Data.Xml.HaXml

type StyleURL=String
data StyleSheet = NoStyle
                | CSS {styleURL::StyleURL} 
                | XSL {styleURL::StyleURL} deriving (Read,Show)
hasStyleURL :: StyleSheet -> Bool
hasStyleURL NoStyle = False
hasStyleURL _ = True 
type Element = Types.Element

	

isCSS :: StyleSheet -> Bool
isCSS (CSS _)=True
isCSS _ = False
isXSL :: StyleSheet -> Bool
isXSL = not.isCSS

t :: Name -> [(Name, String)] -> CharData -> Types.Element
t=textElem
l :: Name -> [(Name, String)] -> [Types.Element] -> Types.Element
l=listElem
e :: Name -> [(Name, String)] -> Types.Element
e=emptyElem
(</<) :: Name
         -> [(Name, String)]
         -> [Types.Element]
         -> Types.Element
(</<)=l
(<>) :: Name -> [(Name, String)] -> CharData -> Types.Element
(<>)=t



xmlElem :: (t -> [Content])
           -> Name
           -> [(Name, String)]
           -> t
           -> Types.Element
xmlElem f = \name attrs val -> xmlelem name attrs (f val)
	where 
	xmlelem name = Types.Elem name . map (uncurry attr)
	attr name val= (name,AttValue [Left val])

textElem :: Name -> [(Name, String)] -> CharData -> Types.Element
textElem = xmlElem (return.CString True)
emptyElem :: Name -> [(Name, String)] -> Types.Element
emptyElem = \n a->xmlElem id n a []
listElem :: Name
            -> [(Name, String)]
            -> [Types.Element]
            -> Types.Element
listElem = xmlElem $ map CElem

cdataElem :: CharData -> Content
cdataElem = CString  False

--	     Document (simpleProlog xsl) [] $ xmlEscape stdXmlEscaper root
simpleDocOld :: StyleSheet -> Types.Element -> String
simpleDocOld xsl = show . document . 
                flip (Document (simpleProlog xsl) []) [] . xmlStdEscape

simpleDoc :: StyleSheet -> Types.Element -> String
simpleDoc style elem = ("<?xml version='1.0' encoding='UTF-8' ?>\n"++
                      if hasStyleURL style then pi else "") ++
                     (verbatim $ xmlStdEscape elem)
    where typeText=if isCSS style then "text/css" else "text/xsl"
          pi= "<?xml-stylesheet type=\""++ typeText  ++ 
              "\" href=\""++styleURL style++"\" ?>\n"


simpleDoc' :: StyleSheet -> Types.Element -> String
simpleDoc' style elem = (if hasStyleURL style then pi else "") ++
                        (verbatim $ xmlStdEscape elem)
    where typeText=if isCSS style then "text/css" else "text/xsl"
          pi= "<?xml-stylesheet type=\""++ typeText  ++ 
              "\" href=\""++styleURL style++"\" ?>\n"



xmlEscaper :: XmlEscaper
xmlEscaper=stdXmlEscaper
xmlStdEscape :: Types.Element -> Types.Element
xmlStdEscape = xmlEscape stdXmlEscaper
verbim :: (Verbatim a) => a -> String
verbim = verbatim

simpleProlog :: StyleSheet -> Prolog
simpleProlog style = 
    Prolog 
    (Just (XMLDecl "1.0" 
	   (Just $ EncodingDecl "UTF-8") 
	   Nothing -- standalone declaration
	  ))
    [] Nothing
           (if url=="" then [] else [pi])
	where
	pi = PI ("xml-stylesheet", "type=\""++typeText++"\" href=\""++url++"\"")
	typeText = if isCSS style then "text/css" else "text/xsl"
	url=if hasStyleURL style then styleURL style else ""

nonEmpty :: Name -> String -> Maybe Types.Element
nonEmpty name val = if val=="" then Nothing
					else Just $ textElem name [] val

getRoot :: Document -> Types.Element
getRoot (Document _ _ root _) = root

--toXML .< "App" attrs ./>
--toXML .< "App" attrs .> []
data XML a = XML StyleSheet a

class ToElement x where toElement::x->Types.Element
		
instance (ToElement x) => ToElement (Maybe x) where 
    toElement = maybe (emptyElem "Nothing" []) toElement

instance ToElement String where toElement = textElem "String" []
instance ToElement Types.Element where toElement = id
instance ToElement CalendarTime where 
    toElement = recToEl "CalendarTime" 
                [attrFS "year" ctYear
                ,attrFS "month" (fromEnum.ctMonth)
                ,attrFS "day" ctDay
                ,attrFS "hour" ctHour
                ,attrFS "min" ctMin
                ,attrFS "sec" ctSec
                ,attrFS "time" time 
                ] []
        where time = epochPico

instance ToElement Int where toElement = toElement . show
instance ToElement Integer where toElement = toElement . show
instance ToElement Float where toElement = toElement . show
instance ToElement Double where toElement = toElement . show


instance (Xml a) => ToElement a where
    toElement = un . head . map toHaXml . toXml
        where
        un (CElem el) = el
        un _ = error "Case not handled in Xml toElement instance"

wrapElem :: (ToElement x) => Name -> x -> Types.Element
wrapElem tag x= listElem tag [] [toElement x]
elF :: (ToElement b) => Name -> (a -> b) -> a -> Types.Element
elF tag f = wrapElem tag.f 
-- label !<=! field = wrapField label field
attrF :: t1 -> (t -> String) -> t -> (t1, String)
attrF name f rec = (name,quoteEsc $ f rec)
attrFS :: (Show a) => t1 -> (t -> a) -> t -> (t1, String)
attrFS name f rec = (name,quoteEsc $ show $ f rec)
attrFMb :: (a -> String)
           -> String
           -> (a1 -> Maybe a)
           -> a1
           -> (String, String)
attrFMb r name f = maybe ("","") (\x->(name,quoteEsc $ r x)) . f 

--(\x->(name,quoteEsc $ r x)) . f 
--(name,quoteEsc $ show $ f rec)

quoteEsc :: String -> String
quoteEsc [] = []
quoteEsc ('"':list) = "&quot;" ++ quoteEsc list
quoteEsc (x:xs) = x:quoteEsc xs

--quotescape \\ and " \"

recToEl :: Name
           -> [a -> (String, String)]
           -> [a -> Types.Element]
           -> a
           -> Types.Element
recToEl name attrs els rec = listElem name attrs' (revmap rec els)
    where
    attrs' = filter (\ (x,_)->not $ null x) (revmap rec attrs)
listToEl :: (ToElement a) =>
            Name -> [(Name, String)] -> [a] -> Types.Element
listToEl name attrs = listElem name attrs . map toElement 

toAttrs :: t -> [(t1, t -> t2)] -> [(t1, t2)]
toAttrs x = map (\ (s,f)->(s, f x)) 

{--
toElement rules:
1. if the attr is an instance of toElement then it is a child.
2. if it named and is type string then it is shown that way.
3. if it named and has non-string type then use show on the value.
4. if the attributes are not named then use the type as the label and
   make the text child be a show of the object.
--}


newtype ElString = ElString {elString::String} deriving (Eq,Ord,Read,Show)
