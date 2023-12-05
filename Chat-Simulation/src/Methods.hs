{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{- |
  Module     : CustomMethodsCreated
  Maintainer : Naman Srivastava <ec22297@qmul.ac.uk>
  This module has all the methods that are used throughout the application
-}
module Methods (
     usersName,
     sleepMs,
     createUser,
     selectUser,
     randomSelectText,
     constructMessage,
     countReceiver,
     userFunc,
     findMsgByUser,
     findMsgToUser,
     showMsgByUser,
     showMsgToUser
    ) where

import System.Random
import Control.Concurrent (forkIO, threadDelay, newMVar, MVar)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar,modifyMVar_, readMVar)
import Data.Foldable (for_, forM_)
import Control.Monad (forever)
import Data.List (elemIndices)
import qualified Data.Map as Map
import Types

usersName :: [String]
usersName = ["John","Mary","Alice","Bob","David","Stephanie","George","Scott","Emma","Tina"]

{- |
  Module     : sleepMs
  Info       : This module is used to provide delays for synchronization purpose. It uses threadDelay from library Control.Concurrent
-}
sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

{- |
  Module     : sleepMs
  Info       : This module is used to provide usernames from predefined List.
-}
createUser :: Int -> IO User
createUser n = do
    return User {name = usersName !! n}

{- |
  Module     : selectUser
  Info       : This module is used to performs random selection from available users.
-}
selectUser :: [User] -> IO User
selectUser xs = do
  i <- randomRIO (0, length xs - 1)
  let usersButItself = filter (\x -> x /= (i)) [0..9]
  i <- randomRIO (0, (length usersButItself) - 1)
  let j = usersButItself !! i 
  return (xs !! j)

{- |
  Module     : randomSelectText
  Info       : This module is used to performs random selection from available list of chat responses.
-}
randomSelectText :: IO String
randomSelectText = do
  let text = ["Hello", "How are you?", "What's up?", "Goodbye","I'm good, thanks","Hi","Not much, how about you?","Just hanging out","Bye"]
  randomMessage <-randomRIO (0, (length text) - 1)
  message1 <- return $ text !! randomMessage
  return message1

{- |
  Module     : constructMessage
  Info       : This module is used to performs construction of a message packet of data type Msg defined in Types.hs.
-}
constructMessage ::User -> [User] -> IO Msg
constructMessage user users = do 
   receiverUser <- selectUser users
   message2 <- randomSelectText
   return $ Msg (name user) (name receiverUser) message2

{- |
  Module     : countReceiver
  Info       : This module is used to performs construction of a message packet of data type Msg defined in Types.hs.
-}
countReceiver :: [Msg] -> Map.Map String Int
countReceiver messages = 
  -- Create a Map with zero count for each receiver
  let messageCounts = Map.fromList [(receiver m, 0) | m <- messages]
  -- Use 'foldl' to iterate over the messages and increment the count for each receiver in the Map
  in foldl (\acc m -> Map.adjust (+1) (receiver m) acc) messageCounts messages

{- |
  Module     : userFunc
  Info       : This module is used to performs simulation of constructed messages.
-}
userFunc :: User -> [User] -> MVar Int -> MVar [Msg] -> IO ()
userFunc user users msgCount messages = do
   forM_ [1..10] $ \_ -> do
    delay <- randomRIO (1,50)
    sleepMs delay
    message3 <- constructMessage user users 
    msgSent <- takeMVar msgCount
    if msgSent < 100
      then do
        putMVar msgCount (msgSent + 1)
        modifyMVar_ messages (\msgList -> return (message3:msgList))
      else do
        putMVar msgCount msgSent
   putStrLn $ name user ++ " finished sending messages."

{- |
  Module     : findMsgByUser
  Info       : This module is used to find messages by a specific sender by passing List of users and specific User
               as Input argument and producing output as the message containing specific user as sender.
-}
findMsgByUser :: User -> [Msg] -> [Msg]
findMsgByUser user messages = filter (\msg -> sender (msg) == name user) messages

{- |
  Module     : findMsgToUser
  Info       : This module is used to find messages by a specific receiver by passing List of users and specific User
               as Input argument and producing output as the message containing specific user as receiver.
-}
findMsgToUser :: User -> [Msg] -> [Msg]
findMsgToUser user messages = filter (\msg -> receiver (msg) == name user) messages

{- |
  Module     : showMsgByUser
  Info       : This module is used to display messages by a specific sender by passing List of messages and specific User
               as Input argument and producing output as messages containing specific user as sender.
-}
showMsgByUser :: User -> [Msg] -> IO ()
showMsgByUser user messages = do
    let sentMessages = filter (\msg -> sender (msg) == name user) messages
    putStrLn $ "All the messages SENT BY " ++ (name user) ++ " are: "
    mapM_ (\msg -> putStrLn $ "  " ++ (message msg)++" to "++(receiver msg)) sentMessages

{- |
  Module     : showMsgToUser
  Info       : This module is used to display messages for a specific receiver by passing List of messages and specific User
               as Input argument and producing output as messages containing specific user as receiver.
-}
showMsgToUser :: User -> [Msg] -> IO ()
showMsgToUser user messages = do
    let sentMessages = filter (\msg -> receiver (msg) == name user) messages
    putStrLn $ "All the messages SENT TO " ++ (name user) ++ " are: "
    mapM_ (\msg -> putStrLn $ "  " ++ (message msg)++" from "++(sender msg)) sentMessages
