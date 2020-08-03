module Domain.Auth
  ( Auth(..)
  )
where

import           ClassyPrelude
import           Text.Regex.PCRE.Heavy
import           Domain.Validation

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email 
mkEmail = validate Email
  [
     regexMatches [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|] "Not a valid email"
  ]

newtype Password = Password { passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [ lengthBetween 5 50 "Should be between 5 and 50"
  , regexMatches [re|\d|] "Should contain number"
  , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
  , regexMatches [re|[a-z]|] "Should contain lowercase letter"
  ]

data Auth = Auth
    { authEmail :: Text
    , authPassword :: Text
    } deriving (Show, Eq)

data RegistrationError
    = RegistrationErrorEmailTaken
    deriving (Show, Eq)
