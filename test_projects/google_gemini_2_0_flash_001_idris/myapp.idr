
module Main

import Data.String
import Data.Maybe
import System
import System.Environment
import Network.HTTP
import Network.HTTP.Types
import Network.URI
import Data.JSON
import Data.JSON.Deriving
import Data.List
import Data.Char

import Database.PostgreSQL

record User = MkUser { id : Int, name : String, email : String }

derive JSONShow User
derive JSONParse User

opts : Options
opts =
  [ ("port", String "8080")
  , ("dbhost", String "host.docker.internal")
  , ("dbport", String "5432")
  , ("dbname", String "complang")
  , ("dbuser", String "testuser")
  , ("dbpass", String $ fromMaybe "" $ getEnv "PGPASSWORD")
  ]

main : IO ()
main = do
  args <- getArgs
  parsedOpts <- parseOpts opts args
  let port = fromString $ fromMaybe "8080" $ lookupOpt "port" parsedOpts
  let dbhost = fromMaybe "host.docker.internal" $ lookupOpt "dbhost" parsedOpts
  let dbport = fromString $ fromMaybe "5432" $ lookupOpt "dbport" parsedOpts
  let dbname = fromMaybe "complang" $ lookupOpt "dbname" parsedOpts
  let dbuser = fromMaybe "testuser" $ lookupOpt "dbuser" parsedOpts
  let dbpass = fromMaybe "" $ lookupOpt "dbpass" parsedOpts
  putStrLn $ "Starting server on port " ++ show port ++ " connecting to " ++ show dbhost ++ ":" ++ show dbport ++ "/" ++ show dbname ++ " as " ++ show dbuser
  dbConn <- connectDB dbhost dbport dbname dbuser dbpass
  runServer port dbConn

lookupOpt : String -> Options -> Maybe String
lookupOpt key opts = do
  val <- lookup key opts
  case val of
    String s => Just s
    _ => Nothing

parseOpts : Options -> List String -> IO Options
parseOpts defaults args = return $ parseArgs defaults args

type Options = List (String, Arg)

data Arg = String String

parseArgs : Options -> List String -> Options
parseArgs opts [] = opts
parseArgs opts (x :: xs) =
  case stringSplit '=' x of
    [key, value] => parseArgs ((key, String value) :: opts) xs
    _ => parseArgs opts xs

dbToString : PQValue -> String
dbToString (PQString s) = s
dbToString _ = ""

dbToInt : PQValue -> Int
dbToInt (PQInt i) = i
dbToInt _ = 0

runServer : Int -> Connection -> IO ()
runServer port dbConn = serve port $ \req -> do
  case requestMethod req of
    GET ->
      case pathInfo req of
        ["users"] => do
          users <- getUsers dbConn
          respondWithJson (encodeJSON users)
        ["users", idStr] => do
          case stringToInt idStr of
            Just id => do
              user <- getUser dbConn id
              case user of
                Just u => respondWithJson (encodeJSON u)
                Nothing => return $ response LNotFound [] ""
            Nothing => return $ response LBadRequest [] "Invalid user ID"
        _ => return $ response LNotFound [] ""
    POST ->
      case pathInfo req of
        ["users"] => do
          body <- getRequestBody req
          case parseJSON body of
            Ok user -> do
              newUserId <- createUser dbConn user.name user.email
              newUser <- getUser dbConn newUserId
              case newUser of
                Just u => respondWithStatus Created $ encodeJSON u
                Nothing => return $ response InternalServerError [] "Failed to create user"
            Error err => return $ response LBadRequest [] err
        _ => return $ response LNotFound [] ""
    PUT ->
      case pathInfo req of
        ["users", idStr] => do
          case stringToInt idStr of
            Just id => do
              body <- getRequestBody req
              case parseJSON body of
                Ok user -> do
                  updated <- updateUser dbConn id user.name user.email
                  if updated
                    then return $ response LOk [] ""
                    else return $ response LNotFound [] ""
                Error err => return $ response LBadRequest [] err
            Nothing => return $ response LBadRequest [] "Invalid user ID"
        _ => return $ response LNotFound [] ""
    DELETE ->
      case pathInfo req of
        ["users", idStr] => do
          case stringToInt idStr of
            Just id => do
              deleted <- deleteUser dbConn id
              if deleted
                then return $ response LOk [] ""
                else return $ response LNotFound [] ""
            Nothing => return $ response LBadRequest [] "Invalid user ID"
        _ => return $ response LNotFound [] ""
    _ => return $ response LMethodNotAllowed [] ""

respondWithJson : String -> IO Response
respondWithJson json = return $ response LOk [("Content-Type", "application/json")] json

getUsers : Connection -> IO (List User)
getUsers dbConn = do
  result <- query dbConn "SELECT id, name, email FROM users"
  case result of
    Ok rows -> return $ map rowToUser rows
    Error err -> do
      putStrLn $ "Error getting users: " ++ err
      return []

getUser : Connection -> Int -> IO (Maybe User)
getUser dbConn id = do
  result <- queryParams dbConn "SELECT id, name, email FROM users WHERE id = ?" [PQInt id]
  case result of
    Ok rows -> return $ case rows of
      [] => Nothing
      (row :: _) => Just $ rowToUser row
    Error err -> do
      putStrLn $ "Error getting user: " ++ err
      return Nothing

createUser : Connection -> String -> String -> IO Int
createUser dbConn name email = do
  result <- queryParams dbConn "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" [PQString name, PQString email]
  case result of
    Ok rows -> return $ case rows of
      (row :: _) => dbToInt (head row)
      _ => -1
    Error err -> do
      putStrLn $ "Error creating user: " ++ err
      return -1

updateUser : Connection -> Int -> String -> String -> IO Bool
updateUser dbConn id name email = do
  result <- queryParams dbConn "UPDATE users SET name = ?, email = ? WHERE id = ?" [PQString name, PQString email, PQInt id]
  case result of
    Ok _ -> return True
    Error err -> do
      putStrLn $ "Error updating user: " ++ err
      return False

deleteUser : Connection -> Int -> IO Bool
deleteUser dbConn id = do
  result <- queryParams dbConn "DELETE FROM users WHERE id = ?" [PQInt id]
  case result of
    Ok _ -> return True
    Error err -> do
      putStrLn $ "Error deleting user: " ++ err
      return False

rowToUser : List PQValue -> User
rowToUser row = MkUser (dbToInt (row !! 0)) (dbToString (row !! 1)) (dbToString (row !! 2))
