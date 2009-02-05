module HAppS.Server.JSON where
import Data.Char
import Data.List

data JSON = JBool Bool | JString String | JInt Int | JFloat Float | 
            JObj [(String,JSON)] | JList [JSON] | JNull


jInt :: Integral a => a -> JSON
jInt = JInt . fromIntegral

class ToJSON a where
    toJSON::a->JSON
instance ToJSON JSON where toJSON=id

jsonToString :: JSON -> String
jsonToString (JBool bool) = map toLower $ show bool
jsonToString (JString string) = show string
jsonToString (JInt int) = show int
jsonToString (JFloat float) = show float
jsonToString (JObj pairs) = '{' : (concat $ (intersperse "," $ map impl pairs) )++"}"
    where
    impl (name,val) = concat [show name,":",jsonToString val]
jsonToString (JList list) = 
    '[':(concat $ (intersperse "," $ map jsonToString list)) ++"]"
jsonToString JNull = "null"
type CallBack=String

data JSONCall x = JCall CallBack x
