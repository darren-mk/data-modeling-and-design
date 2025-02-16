module Lib (someFunc, coercePhoneNumber) where

import Data.Char (toLower)
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

coerceFirstName :: String -> Maybe FirstName
coerceFirstName = validateName

coerceLastName :: String -> Maybe LastName
coerceLastName = validateName

coerceSsn :: String -> Maybe SocialSecurityNumber
coerceSsn s =
  let pattern = "^\\d{3}-\\d{2}-\\d{4}$"
  in if s =~ pattern then Just s else Nothing

coerceMaritalStatus :: String -> Maybe MaritalStatus
coerceMaritalStatus s = case map toLower s of
  "single" -> Just Single
  "married" -> Just Married
  "divorced" -> Just Divorced
  "widowed" -> Just Widowed
  "separated" -> Just Separated
  _ -> Nothing

coercePhoneNumber :: String -> Maybe PhoneNumber
coercePhoneNumber s =
  let pattern = "^\\+?\\d{1,3}[-.\\s]?\\(?\\d{1,4}\\)?[-.\\s]?\\d{3,4}[-.\\s]?\\d{4}$"
  in if s =~ pattern then Just s else Nothing

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
  ssns <- prompt "Enter SSN:"
  mss <- prompt "Enter married status:"
  pns <- prompt "Enter phone number:"
  let personInEditing = PersonInEditing
                        { firstNameStr = Just fns
                        , lastNameStr = Just lns
                        , ssnStr = Just ssns
                        , maritalStatusStr = Just mss
                        , phoneNumberStr = Just pns }
  let result = validatePersonData personInEditing
  let msg = case result of
        Just person -> 
          "good!" ++ firstName person
        Nothing -> "bad!"
  putStrLn msg
