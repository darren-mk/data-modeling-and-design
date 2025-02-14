module Lib
    ( someFunc
    ) where

import Text.Regex.TDFA ( (=~) )

data MaritalStatus
    = Single
    | Married
    | Divorced
    | Widowed
    | Separated

type FirstName = String
type LastName = String
type SocialSecurityNumber = String
type PhoneNumber = String

data PersonInEditing =
  PersonInEditing
  { firstNameStr :: Maybe String
  , lastNameStr :: Maybe String
  , ssnStr :: Maybe String
  , maritalStatusStr :: Maybe String
  , phoneNumberStr :: Maybe String }

data Person =
  Person
  { firstName :: FirstName
  , lastName :: LastName
  , ssn :: SocialSecurityNumber
  , maritalStatus :: MaritalStatus
  , phoneNumber :: PhoneNumber }

validateName :: String -> Maybe String
validateName s =
    let pattern = "^[A-Z][a-zA-Z' -]{1,98}[a-zA-Z]$"
    in if s =~ pattern then Just s else Nothing

someFunc :: IO ()
someFunc = putStrLn "someFunc!"
