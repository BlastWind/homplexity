{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Classifying messages by severity and filtering them.
module Language.Haskell.Homplexity.Message (
    Log
  , Message
  , Severity (..)
  , severityOptions
  , critical
  , warn
  , info
  , debug
  , message
  , extract
  , MsgResult (..)
  , CodeFragmentPhantom (..)
  ) where

import           Control.Arrow
import           Control.DeepSeq
import           Data.Foldable                      as Foldable
import           Data.Function                      (on)
#if __GLASGOW_HASKELL__ >= 800
import           Data.Semigroup                     (Semigroup (..))
#else
import           Data.Monoid
#endif
import           Data.Sequence                      as Seq
import           HFlags
import           Language.Haskell.Exts              hiding (style)
import           Language.Haskell.TH.Syntax         (Lift (..))
#ifdef HTML_OUTPUT
import           Prelude                            hiding (div, head, id, span)
import           Text.Blaze.Html4.Strict            hiding (map, style)
import           Text.Blaze.Html4.Strict.Attributes hiding (span, title)
#endif
import           Data.Aeson
import           Data.Aeson.Types                   (Pair)
import           Data.Text                          (Text)
import           Data.ByteString.Lazy               (ByteString)
import Language.Haskell.Homplexity.CodeFragment (CodeFragment)
import Data.Typeable (cast)
import GHC.Generics
--import           Text.Blaze.Renderer.Utf8           (renderMarkup)

-- | Keeps a set of messages
newtype Log = Log { unLog :: Seq Message }
  deriving(Monoid
#if __GLASGOW_HASKELL__ >= 800
          ,Semigroup
#endif
                    )

instance NFData Log where
  rnf = rnf . unLog

instance NFData MsgResult where
  rnf (MsgResult msgGranularity msgIdentifier msgMetric msgValue) =
    rnf msgGranularity `seq` rnf msgIdentifier `seq` msgMetric `seq` rnf msgValue 

data CodeFragmentPhantom = forall a. CodeFragment a => CodeFragmentPhantom a
instance Eq CodeFragmentPhantom where
  (CodeFragmentPhantom a) == (CodeFragmentPhantom b) =
    case cast b of
      Just b' -> a == b'
      Nothing -> False
instance Show CodeFragmentPhantom where 
  show (CodeFragmentPhantom a) = show a

data MsgResult = MsgResult { msgGranularity :: !String
                       , msgIdentifier  :: !String
                       , msgMetric      :: !String
                       , msgValue       :: String
                      } deriving (Eq)

instance Show MsgResult where
  show msgResult = show (msgGranularity msgResult) ++ msgIdentifier msgResult ++ show (msgValue msgResult)

-- | Message from analysis
data Message = Message { msgSeverity :: !Severity
                       , msgSrc      :: !SrcLoc
                       , msgResult   :: !MsgResult 

                       }
  deriving (Eq)

instance NFData Message where
  rnf Message {msgSrc=SrcLoc{..},..} =
    rnf msgSeverity `seq` rnf msgResult `seq`
    rnf srcFilename `seq` rnf srcLine `seq` rnf srcColumn

instance ToJSON SrcLoc where
  toJSON srcLoc = Data.Aeson.object
    [ "filename" .= srcFilename srcLoc
    , "line"     .= srcLine srcLoc
    , "column"   .= srcColumn srcLoc
    ]

instance ToJSON MsgResult where
  toJSON (MsgResult {..}) = Data.Aeson.object
    [ "granularity"  .= msgGranularity
    , "identifier"       .= msgIdentifier
    , "metric"           .= msgMetric
    , "value"         .= msgValue
    ]

instance ToJSON Message where
  toJSON message = Data.Aeson.object
    [ "severity" .= msgSeverity message
    , "result"     .= msgResult message
    , "src"      .= msgSrc message
    ]


toText :: Severity -> Text
toText Debug     = "Debug"
toText Info      = "Info"
toText Warning   = "Warning"
toText Critical  = "Critical"

instance ToJSON Severity where
  toJSON = Data.Aeson.String . toText
  toEncoding = toEncoding . toText

instance Show Message where
  showsPrec _ Message {msgSrc=loc@SrcLoc{..}, ..} = shows msgSeverity
                                                  . (':':)
                                                  . (srcFilename++)
                                                  . (':':)
                                                  . shows loc
                                                  -- . shows srcLine
                                                  -- . shows srcColumn
                                                  . (": "++)
                                                  . (show msgResult++)
                                                  . ('\n':)


instance ToMarkup Message where
  toMarkup Message {msgSrc=SrcLoc{..}, ..} =
    p ! classId $
     (toMarkup msgSeverity
       <> string ": "
       <> (a ! href (toValue srcFilename) $ (string srcFilename))
       <> string ": "
       <> string (show msgResult))
    where
      classId = case msgSeverity of
                     Debug    -> class_ "debug"
                     Info     -> class_ "info"
                     Warning  -> class_ "warning"
                     Critical -> class_ "critical"

instance ToMarkup Severity where
  toMarkup Debug    = span   ! class_ "severity" $ string (show Debug)
  toMarkup Info     = span   ! class_ "severity" $ string (show Info)
  toMarkup Warning  = strong ! class_ "severity" $ string (show Warning)
  toMarkup Critical = strong ! class_ "severity" $ string (show Critical)

-- | Message severity
data Severity = Debug
              | Info
              | Warning
              | Critical
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Lift)

instance NFData Severity where
  rnf !_a = ()

-- | String showing all possible values for @Severity@.
severityOptions :: String
severityOptions  = unwords $ map show [minBound..(maxBound::Severity)]

instance FlagType Severity where
  defineFlag n v = defineEQFlag n [| v :: Severity |] "{Debug|Info|Warning|Critical}"

-- | Helper for logging a message with given severity.
message :: Severity -> SrcLoc -> MsgResult -> Log
message msgSeverity msgSrc msgResult = Log $ Seq.singleton Message {..}


-- | TODO: automatic inference of the srcLine
-- | Log a certain error
critical :: SrcLoc -> MsgResult -> Log
critical  = message Critical

-- | Log a warning
warn  ::  SrcLoc -> MsgResult -> Log
warn   = message Warning

-- | Log informational message
info  ::  SrcLoc -> MsgResult -> Log
info   = message Info

-- | Log debugging message
debug ::  SrcLoc -> MsgResult -> Log
debug  = message Debug

-- TODO: check if this is not too slow
msgOrdering ::  Message -> Message -> Ordering
msgOrdering = compare `on` ((srcFilename &&& srcLine) . msgSrc)

-- | Convert @Log@ into ordered sequence (@Seq@).
orderedMessages                  :: Severity -> Log -> Seq Message
orderedMessages severity Log {..} = Seq.unstableSortBy         msgOrdering  $
                                      Seq.filter ((severity<=) . msgSeverity)   unLog

-- | Extract an ordered sequence of messages from the @Log@.
extract ::  Severity -> Log -> [Message]
extract severity = Foldable.toList
                 . orderedMessages severity

instance Show Log where
  showsPrec _ l e = Foldable.foldr  shows e $
                    orderedMessages Debug l
