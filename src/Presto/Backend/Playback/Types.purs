module Presto.Backend.Playback.Types where

import Prelude

import Control.Monad.Eff.Ref (Ref)
import Data.Array as Array
import Data.Bounded (class Bounded, top, bottom)
import Data.Enum (class Enum, succ, pred)
import Data.Eq (class Eq, eq)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON, encodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Presto.Core.Utils.Encoding (defaultEncode, defaultDecode)
import Sequelize.Models.Types (DataType(..))
import Type.Proxy (Proxy)

data RecordingEntry = RecordingEntry EntryReplayingMode String
data GlobalReplayingMode = GlobalNormal | GlobalNoVerify | GlobalNoMocking
data EntryReplayingMode = Normal | NoVerify | NoMock -- | Skip

derive instance modeEq :: Eq EntryReplayingMode
derive instance genericEntryReplayingMode :: Generic EntryReplayingMode _
instance entryReplayingModeEncode :: Encode EntryReplayingMode where encode = defaultEncode
instance entryReplayingModeDecode :: Decode EntryReplayingMode where decode = defaultDecode
instance showEntryReplayingMode :: Show EntryReplayingMode where show = GShow.genericShow
instance ordEntryReplayingMode :: Ord EntryReplayingMode where compare = GOrd.genericCompare


type DisableEntries  = String
-- TODO: it might be Data.Sequence.Ordered is better
type Recording =
  { entries :: Array RecordingEntry
  }

-- N.B. Async and parallel computations are not properly supported.
-- For now, Ref is used, but it's not thread safe.
-- So having a sequential flow is preferred.
type RecorderRuntime =
  { recordingRef :: Ref Recording
   ,disableEntries :: Array DisableEntries
  }

type PlayerRuntime =
  { recording :: Recording
  , disableVerify :: Array DisableEntries
  , disableMocking :: Array DisableEntries
  --, skip :: Array DisableEntries                 --remove this if not used and implemented 
  , stepRef   :: Ref Int
  , errorRef  :: Ref (Maybe PlaybackError)
  }

data PlaybackErrorType
  = UnexpectedRecordingEnd
  | UnknownRRItem
  | MockDecodingFailed
  | ItemMismatch

newtype PlaybackError = PlaybackError
  { errorType :: PlaybackErrorType
  , errorMessage :: String
  }


class (Eq rrItem, Decode rrItem, Encode rrItem) <= RRItem rrItem where
  toRecordingEntry   :: rrItem -> EntryReplayingMode -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String
  isMocked           :: Proxy rrItem -> Boolean

-- Class for conversions of RRItem and native results.
-- Native result can be unencodable completely.
-- TODO: error handling
class (RRItem rrItem) <= MockedResult rrItem native | rrItem -> native where
  parseRRItem :: rrItem -> Maybe native

derive instance genericRecordingEntry :: Generic RecordingEntry _
instance decodeRecordingEntry         :: Decode  RecordingEntry where decode = defaultDecode
instance encodeRecordingEntry         :: Encode  RecordingEntry where encode = defaultEncode
instance eqRecordingEntry             :: Eq      RecordingEntry where eq = GEq.genericEq
instance showRecordingEntry           :: Show    RecordingEntry where show = GShow.genericShow
instance ordRecordingEntry            :: Ord     RecordingEntry where compare = GOrd.genericCompare

derive instance genericPlaybackErrorType :: Generic PlaybackErrorType _
instance decodePlaybackErrorType         :: Decode  PlaybackErrorType where decode = defaultDecode
instance encodePlaybackErrorType         :: Encode  PlaybackErrorType where encode = defaultEncode
instance eqPlaybackErrorType             :: Eq      PlaybackErrorType where eq = GEq.genericEq
instance showPlaybackErrorType           :: Show    PlaybackErrorType where show = GShow.genericShow
instance ordPlaybackErrorType            :: Ord     PlaybackErrorType where compare = GOrd.genericCompare
instance enumPlaybackErrorType           :: Enum    PlaybackErrorType where
  succ = GEnum.genericSucc
  pred = GEnum.genericPred
instance boundedPlaybackErrorType        :: Bounded PlaybackErrorType where
  top = GBounded.genericTop
  bottom = GBounded.genericBottom

derive instance genericPlaybackError :: Generic PlaybackError _
derive instance newtypeConfig        :: Newtype PlaybackError _
instance decodePlaybackError         :: Decode  PlaybackError where decode = defaultDecode
instance encodePlaybackError         :: Encode  PlaybackError where encode = defaultEncode
instance eqPlaybackError             :: Eq      PlaybackError where eq = GEq.genericEq
instance showPlaybackError           :: Show    PlaybackError where show = GShow.genericShow
instance ordPlaybackError            :: Ord     PlaybackError where compare = GOrd.genericCompare

-- Classless types
newtype RRItemDict rrItem native = RRItemDict
  { toRecordingEntry   :: rrItem -> EntryReplayingMode -> RecordingEntry
  , fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  , getTag             :: Proxy rrItem -> String
  , isMocked           :: Proxy rrItem -> Boolean
  , parseRRItem        :: rrItem -> Maybe native
  , mkEntry            :: native -> rrItem
  , compare            :: rrItem -> rrItem -> Boolean
  , encodeJSON         :: rrItem -> String
  }



toRecordingEntry' :: forall rrItem native. RRItemDict rrItem native -> rrItem -> EntryReplayingMode -> RecordingEntry
toRecordingEntry' (RRItemDict d) mode = d.toRecordingEntry mode

fromRecordingEntry' :: forall rrItem native. RRItemDict rrItem native -> RecordingEntry -> Maybe rrItem
fromRecordingEntry' (RRItemDict d) = d.fromRecordingEntry

getTag' :: forall rrItem native. RRItemDict rrItem native -> Proxy rrItem -> String
getTag' (RRItemDict d) = d.getTag

isMocked' :: forall rrItem native. RRItemDict rrItem native -> Proxy rrItem -> Boolean
isMocked' (RRItemDict d) = d.isMocked

parseRRItem' :: forall rrItem native. RRItemDict rrItem native -> rrItem -> Maybe native
parseRRItem' (RRItemDict d) = d.parseRRItem

mkEntry' :: forall rrItem native. RRItemDict rrItem native -> native -> rrItem
mkEntry' (RRItemDict d) = d.mkEntry

compare' :: forall rrItem native. RRItemDict rrItem native -> rrItem -> rrItem -> Boolean
compare' (RRItemDict d) = d.compare

encodeJSON' :: forall rrItem native. RRItemDict rrItem native -> rrItem -> String
encodeJSON' (RRItemDict d) = d.encodeJSON


mkEntryDict :: forall rrItem native. RRItem rrItem => MockedResult rrItem native => (native -> rrItem) -> RRItemDict rrItem native
mkEntryDict mkEntry = RRItemDict
  { toRecordingEntry   : toRecordingEntry
  , fromRecordingEntry : fromRecordingEntry
  , getTag             : getTag
  , isMocked           : isMocked
  , parseRRItem        : parseRRItem
  , mkEntry            : mkEntry
  , compare            : (==)
  , encodeJSON         : encodeJSON
  }
