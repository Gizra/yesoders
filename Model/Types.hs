module Model.Types where

import Prelude
import Yesod
import Text.Blaze.Html (ToMarkup (..))

data Employment = NotLooking | LookingJob | LookingDevelopers
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Employment"


prettyEmployment :: Employment -> String
prettyEmployment NotLooking = "I am not currently seeking employment or employees"
prettyEmployment LookingJob = "I am looking for a job"
prettyEmployment LookingDevelopers = "I am looking for developers"

instance ToMarkup Employment where toMarkup = toMarkup . prettyEmployment
