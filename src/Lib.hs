module Lib (someFunc, coercePhoneNumber, coerceSsn, validateName) where

import Data.Char (toLower)
import Text.Regex.Posix ( (=~) )

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

coerceFirstName :: String -> Maybe FirstName
coerceFirstName = validateName

coerceLastName :: String -> Maybe LastName
coerceLastName = validateName

simplifiedDigitsRegex :: String
simplifiedDigitsRegex = "^[0-9-]+$"

coerceDigitsSimplified :: String -> Maybe String
coerceDigitsSimplified s =
  if s =~ simplifiedDigitsRegex then Just s else Nothing

coerceSsn :: String -> Maybe SocialSecurityNumber
coerceSsn = coerceDigitsSimplified

coerceMaritalStatus :: String -> Maybe MaritalStatus
coerceMaritalStatus s = case map toLower s of
  "single" -> Just Single
  "married" -> Just Married
  "divorced" -> Just Divorced
  "widowed" -> Just Widowed
  "separated" -> Just Separated
  _ -> Nothing

coercePhoneNumber :: String -> Maybe PhoneNumber
coercePhoneNumber = coerceDigitsSimplified

validatePersonData :: PersonInEditing -> Maybe Person
validatePersonData pie = do
  fn <- coerceFirstName =<< firstNameStr pie
  ln <- coerceLastName =<< lastNameStr pie
  ssn <- coerceSsn =<< ssnStr pie
  ms <- coerceMaritalStatus =<< maritalStatusStr pie
  pn <- coercePhoneNumber =<< phoneNumberStr pie
  return $ Person fn ln ssn ms pn

prompt :: String -> IO String
prompt message = do
  putStrLn message
  getLine

someFunc :: IO ()
someFunc = do
  fns <- prompt "Enter first name:"
  lns <- prompt "Enter last name:"
  ssns <- prompt "Enter SSN (e.g. 111-22-3333):"
  mss <- prompt "Enter married status (e.g. married, single):"
  pns <- prompt "Enter phone number (e.g. 123-456-7890):"
  let personInEditing = PersonInEditing
                        { firstNameStr = Just fns
                        , lastNameStr = Just lns
                        , ssnStr = Just ssns
                        , maritalStatusStr = Just mss
                        , phoneNumberStr = Just pns }
  let result = validatePersonData personInEditing
  let msg = case result of
        Just person -> 
          "Good, " ++ firstName person ++ " " ++ lastName person 
        Nothing -> "bad!"
  putStrLn msg
