{-# LANGUAGE FlexibleInstances #-}
module System.Environment.Extract where

import System.Environment (getEnvironment)

import qualified Data.HashMap.Strict as H
--import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup, (<>))
import Text.Read (readEither)

import Control.Applicative (Applicative, (<*>), pure, Alternative, empty, (<|>), (<$>))
--import Control.Monad

type EnvKey = String
type EnvVal = String
type EnvMap = H.HashMap EnvKey EnvVal
type TypeReport = String
type ValueReport = String

emptyEnvVal :: EnvVal
emptyEnvVal = ""

getEnvValue :: EnvKey -> EnvMap -> Maybe EnvVal
getEnvValue = H.lookup

getEnvDefault :: EnvVal -> EnvKey -> EnvMap -> EnvVal
getEnvDefault = H.lookupDefault

data FailReport =
    MissingKey { frType :: TypeReport, frKey :: EnvKey } |
    MalformedValue { frType :: TypeReport, frKey :: EnvKey, frGot :: EnvVal, frMsg :: Maybe String}
    deriving (Eq, Show)

data SuccessReport =
    LeafValue { vrType :: TypeReport, vrValue :: ValueReport, vrKey :: EnvKey, vrGot :: EnvVal } |
    DefaultValue { vrType :: TypeReport, vrValue :: ValueReport, vrKey :: EnvKey }
    deriving (Eq, Show)

-- | the Result of running an extractor
-- contains reports etc.
-- has 3 varieties
--  - Fail: non-recoverable failure
--  - FailRec: recoverable failure
--  - Success: succesfully extracted value
data Result a = 
    Fail [FailReport] [SuccessReport] |
    FailRec [FailReport] [SuccessReport] |
    Success a [SuccessReport]
    deriving (Eq, Show)

joinFails :: ([FailReport] -> [SuccessReport] -> Result a) -> [FailReport] -> [SuccessReport] -> [FailReport] -> [SuccessReport] -> Result a
joinFails con frs1 srs1 frs2 srs2 = con (frs1 <> frs2) (srs1 <> srs2)

joinSuccessAndFail :: ([FailReport] -> [SuccessReport] -> Result b) -> [FailReport] -> [SuccessReport] -> a -> [SuccessReport] -> Result b
joinSuccessAndFail con frs1 srs1 _ srs2 = con frs1 (srs1 <> srs2)

instance Functor Result where
    fmap f = result Fail FailRec (Success . f)

instance Applicative Result where
    pure v = Success v []
    (<*>) = result onFail onFailRec onSuccess
        where onFail frs srs = result (joinFails Fail frs srs) (joinFails Fail frs srs) (joinSuccessAndFail Fail frs srs)
              onFailRec frs srs = result (joinFails Fail frs srs) (joinFails FailRec frs srs) (joinSuccessAndFail FailRec frs srs)
              onSuccess fun srs1 = result (compMappend srs1 . Fail) (compMappend srs1 . FailRec) (compMappend srs1 . Success . fun)

instance Alternative Result where
    empty = FailRec [] []
    (<|>) = result onFail onFailRec ((const .) . Success)
        where onFail frs srs = result (joinFails Fail frs srs) (joinFails Fail frs srs) (joinSuccessAndFail Fail frs srs)
              onFailRec frs srs = result (joinFails Fail frs srs) (joinFails FailRec frs srs) Success

--i suspect this instance is not valid.
{-
instance Monad Result where
    return = pure
    r >>= f = result Fail FailRec onSuccess r
        where onSuccess v srs = result (compMappend srs . Fail) (compMappend srs . FailRec) (compMappend srs . Success) $ f v

instance MonadPlus Result where
    mzero = empty
    mplus = (<|>)
-}

-- | case analysis for the Result type
result :: ([FailReport] -> [SuccessReport] -> b) -> ([FailReport] -> [SuccessReport] -> b)-> (a -> [SuccessReport] -> b) -> Result a -> b
result f _ _ (Fail frs srs) = f frs srs
result _ f _ (FailRec frs srs) = f frs srs
result _ _ f (Success v srs) = f v srs

singleFail :: FailReport -> Result a
singleFail fr = Fail (pure fr) []

singleFailRec :: FailReport -> Result a
singleFailRec fr = FailRec (pure fr) []


missingKeyFail :: TypeReport -> EnvKey -> Result a
missingKeyFail tr k = singleFailRec $ MissingKey tr k

malformedValueFail :: TypeReport -> EnvKey -> EnvVal -> Result a
malformedValueFail tr k got = singleFail $ MalformedValue tr k got Nothing

malformedValueFailMsg :: TypeReport -> EnvKey -> EnvVal -> String -> Result a
malformedValueFailMsg tr k got msg = singleFail $ MalformedValue tr k got (Just msg)

valueSuccess :: a -> SuccessReport -> Result a
valueSuccess v sr = Success v [sr]

leafSuccess :: TypeReport -> ValueReport -> EnvKey -> EnvVal -> a-> Result a
leafSuccess tr vr k ev v = valueSuccess v $ LeafValue tr vr k ev

defaultSuccess :: TypeReport -> ValueReport -> EnvKey -> a -> Result a
defaultSuccess tr vr k v = valueSuccess v $ DefaultValue tr vr k

-- | the Extractor function is to take a key (prefix) and the environment
-- map, and produce a Result for its type.
-- (<*>) produces extractors that will pass the received key
-- prefix to the component extractors
newtype Extractor a = Extractor { runExtractor :: EnvKey -> EnvMap -> Result a }

instance Functor Extractor where
    fmap f e = Extractor $ \k env -> f <$> runExtractor e k env

instance Applicative Extractor where
    pure v = Extractor $ \_ _ -> pure v
    f <*> e = Extractor $ \k env -> runExtractor f k env <*> runExtractor e k env

{-
instance Monad Extractor where
    return = pure
    e >>= f = Extractor $ \k env -> runExtractor e k env >>= \v -> runExtractor (f v) k env
-}

leafExtraction :: (EnvVal -> Maybe a) -> TypeReport -> (a -> ValueReport) -> EnvKey -> EnvMap -> Result a
leafExtraction p tr vr k env = maybe (missingKeyFail tr k) (parse <*> p) $ getEnvValue k env
    where parse ev = maybe (malformedValueFail tr k ev) (success ev)
          success ev v = leafSuccess tr (vr v) k ev v

leafExtractionMsg :: (EnvVal -> Either String a) -> TypeReport -> (a -> ValueReport) -> EnvKey -> EnvMap -> Result a
leafExtractionMsg p tr vr k env = maybe (missingKeyFail tr k) (parse <*> p) $ getEnvValue k env
    where parse ev = either (malformedValueFailMsg tr k ev) (success ev)
          success ev v = leafSuccess tr (vr v) k ev v

leafExtractor :: (EnvVal -> Maybe a) -> TypeReport -> (a -> ValueReport) -> Extractor a
leafExtractor p tr vr = Extractor $ leafExtraction p tr vr

leafExtractorMsg :: (EnvVal -> Either String a) -> TypeReport -> (a -> ValueReport) -> Extractor a
leafExtractorMsg p tr vr = Extractor $ leafExtractionMsg p tr vr

-- | define an extractor easily
readShowLeafExtractor :: (Read a, Show a) => TypeReport -> Extractor a
readShowLeafExtractor tr = leafExtractorMsg readEither tr show

-- | an Extractable type has an Extractor defined for it
class Extractable e where
    extractor :: Extractor e

-- | a Defaulter can be used to produce a DefaultValue report
data Defaulter a = Defaulter { defaulterTypeReport :: TypeReport, defaulterValueReport :: a -> ValueReport }

mkDefaultReport :: Defaulter a -> EnvKey -> a -> SuccessReport
mkDefaultReport (Defaulter tr vr) k v = DefaultValue tr (vr v) k

mkDefaultSuccess :: Defaulter a -> EnvKey -> a -> Result a
mkDefaultSuccess (Defaulter tr vr)  k v = defaultSuccess tr (vr v) k v

-- | easily define a defaulter
showDefaulter :: Show a => TypeReport -> Defaulter a
showDefaulter tr = Defaulter tr show

-- | an ExtractableDefault type is an Extractable that can also produce
-- defaulting Extractors.
-- minimal definition: defaulter
class Extractable e => ExtractableDefault e where
    defaulter :: Defaulter e

    extractorWithDefault :: e -> Extractor e
    extractorWithDefault v = Extractor $ \k env -> runExtractor extractor k env <|> mkDefaultSuccess defaulter k v

instance Extractable [Char] where
    extractor = leafExtractor Just "[Char]" id

instance ExtractableDefault [Char] where
    defaulter = Defaulter "[Char]" id

instance Extractable Integer where
    extractor = readShowLeafExtractor "Integer"

instance ExtractableDefault Integer where
    defaulter = showDefaulter "Integer"

instance Extractable Bool where
    extractor = readShowLeafExtractor "Bool"

instance ExtractableDefault Bool where
    defaulter = showDefaulter "Bool"

-- | default key section separator
defaultSeparator :: EnvKey
defaultSeparator = "_"

-- | produce a new extractor that searches for a key with the prefix
-- extended by the separator and segment
extractorSep :: Extractor a -> EnvKey -> EnvKey -> Extractor a
extractorSep e sep append = Extractor $ \k env -> runExtractor e (k <> sep <> append) env

-- | produces an extractor that searches for an extended key
extractSep :: Extractable a => EnvKey -> EnvKey -> Extractor a
extractSep = extractorSep extractor

-- | produces a defaulting extractor that searches for an extended key
extractSepDefault :: ExtractableDefault a => a -> EnvKey -> EnvKey -> Extractor a
extractSepDefault = extractorSep . extractorWithDefault

-- | produces an extractor that searches for a key with the specified segment added to the prefix
extract :: Extractable a => EnvKey -> Extractor a
extract = extractSep defaultSeparator

-- | produces a defaulting extractor that searches for the key with the segment added
-- to the prefix
extractDefault :: ExtractableDefault a => a -> EnvKey -> Extractor a
extractDefault = flip extractSepDefault defaultSeparator

extractOn :: Extractor a -> EnvKey -> [(EnvKey, EnvVal)] -> Result a
extractOn e k env = runExtractor e k $ H.fromList env

extractResult :: Extractable a => EnvKey -> [(EnvKey, EnvVal)] -> Result a
extractResult = extractOn extractor

extractOnIO :: Extractor a -> EnvKey -> IO (Result a)
extractOnIO e k = extractOn e k <$> getEnvironment

extractResultIO :: Extractable a => EnvKey -> IO (Result a)
extractResultIO = extractOnIO extractor


compMappend :: Semigroup b => b -> (b -> c) -> b -> c
compMappend l f r = f (l <> r)

infixr 6 .<>
(.<>) :: Semigroup b => b -> (b -> c) -> b -> c
(.<>) = compMappend

