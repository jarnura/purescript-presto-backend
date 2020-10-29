module Presto.Backend.Language.Test where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free, liftF)
import Data.Exists (Exists, mkExists)
import Presto.Backend.Types (BackendEffects, BackendAff)
import Test.Spec (Spec)
import Test.Spec (Spec, it) as S

data TestMethod next eff s
  = Run (BackendAff eff s) (Aff eff Unit → next)
    | Describe String (BackendAff eff s) (S.Spec eff Unit → next)
  {--SetCache String String (Maybe Milliseconds) (Either Error Unit -> next)--}
    {--| SetCacheWithOpts String String (Maybe Milliseconds) SetOptions (Either Error Boolean -> next)--}
    {--| GetCache String (Either Error (Maybe String) -> next)--}
    {--| KeyExistsCache String (Either Error Boolean -> next)--}
    {--| DelCache String (Either Error Int -> next)--}
    {--| Enqueue String String (Either Error Unit -> next)--}
    {--| Dequeue String (Either Error (Maybe String) -> next)--}
    {--| GetQueueIdx String Int (Either Error (Maybe String) -> next)--}
    {--| Expire String Seconds (Either Error Boolean -> next)--}
    {--| Incr String (Either Error Int -> next)--}
    {--| SetHash String String String (Either Error Boolean -> next)--}
    {--| GetHashKey String String (Either Error (Maybe String) -> next)--}
    {--| PublishToChannel String String (Either Error Int -> next)--}
    {--| Subscribe String (Either Error Unit -> next)--}

    {--| NewMulti (Multi -> next)--}
    {--| SetCacheInMulti String String (Maybe Milliseconds) Multi (Multi -> next)--}
    {--| GetCacheInMulti String Multi (Multi -> next)--}
    {--| DelCacheInMulti String Multi (Multi -> next)--}
    {--| ExpireInMulti String Seconds Multi (Multi -> next)--}
    {--| IncrInMulti String Multi (Multi -> next)--}
    {--| SetHashInMulti String String String Multi (Multi -> next)--}
    {--| GetHashInMulti String String Multi (Multi -> next)--}
    {--| PublishToChannelInMulti String String Multi (Multi -> next)--}
    {--| SubscribeInMulti String Multi (Multi -> next)--}
    {--| EnqueueInMulti String String Multi (Multi -> next)--}
    {--| DequeueInMulti String Multi (Multi -> next)--}
    {--| GetQueueIdxInMulti String Int Multi (Multi -> next)--}
    {--| Exec Multi (Either Error (Array Foreign) -> next)--}
    {--| AddInMulti  String EntryID  (Array Item)  Multi (Either Error Multi -> next)--}

    {--| SetMessageHandler (forall eff. (String -> String -> Eff eff Unit)) (Unit -> next)--}

newtype TestMethodWrapper eff next = TestMethodWrapper (Exists (TestMethod next eff))

type Test eff next = Free (TestMethodWrapper eff) next


type TestSpec eff = Test eff (S.Spec eff Unit)

wrap :: ∀ eff next s. TestMethod next eff s → Test eff  next
wrap = liftF <<< TestMethodWrapper <<< mkExists

run :: ∀ eff next s. (BackendAff eff s) → Test eff (S.Spec eff Unit)
run s = wrap $ Run s (\aff → S.it "converted_spec" aff)

describe :: ∀ eff s. String → (BackendAff eff s) → Test eff (S.Spec eff Unit)
describe desc f = wrap $ Describe desc f id
