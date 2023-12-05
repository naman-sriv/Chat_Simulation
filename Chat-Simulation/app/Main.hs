{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
   Module     : Main

   Maintainer : Naman Srivastava <ec22297@qmul.ac.uk>

   Main Functionality is as Follows: 
   - Contains a main thread which creates ten “users” (ten values of type User), and spawns ten threads, one for each of these users.
   - Each user thread should behave as follows: at random time intervals, the thread should select one of the other users at random, and send a random message to that user.
   - Simulate 100 messages, and then terminate and output the final count of how many messages each user received.
   
   Additional Feature:
   - Message history of a user; includes sent and received messages.
   
-}
module Main (main) where

import System.IO
import System.IO.Error
import System.Random
import Control.Concurrent (forkIO, threadDelay, newMVar, MVar)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar,modifyMVar_, readMVar)
import Data.Foldable (for_, forM_)
import Control.Monad
import qualified Data.Map as Map
import Methods
import Types

main :: IO ()
main = do
  -- Create 10 Users
  users <- mapM createUser [0..9]
  -- Create an MVar to store the total message count
  msgCount <- newMVar 0
  -- Create an MVar to store all the messages
  messages <- newMVar []
  -- Spawn a thread for all 10 users
  forM_ users $ \user -> do
    forkIO $ userFunc user users msgCount messages
  -- Wait for all threads to finish
  sleepMs 1000
  putStrLn "All threads finished"
  -- Read MVar to collect all the messages sent by user threads
  finalList <- readMVar messages

  putStrLn "  ---------    Select options ---------------------"
  putStrLn "  (1) Message Count for each user"
  putStrLn "  (2) Get history of messages received by each user"
  hSetBuffering stdout NoBuffering
  putStrLn "Choose an option : "
  -- Accept Input from the user.
  option <- tryIOError (readLn :: IO Int)
  case option of
    Left e -> do
      putStrLn ("Warning!!!! Error: " ++ show e)
      putStrLn "Invalid option: Please select a valid option from below menu"
      main
    Right x -> case x of
      1 -> do
        -- Show output containing total number of messages each user received
        sleepMs 500
        let count = countReceiver finalList
        putStrLn "List of users and their message count:"
        mapM_ (\(name1, value) -> putStrLn (name1 ++ " received " ++ show value ++ " messages.")) (Map.toList count)
        sleepMs 500
        main
      2 -> do
        putStrLn "----------------------------------"
        putStrLn "--------- Select Users -----------"
        forM_ [0..length usersName - 1] $ \i -> putStrLn ("   "++show (i+1) ++ ". " ++ usersName !! i)
        putStrLn "Choose an option : "
        -- Accept Input from the user.
        option2 <- tryIOError (readLn :: IO Int)
        case option2 of
          Left e2 -> do
            putStrLn ("Warning!!!! Error: " ++ show e2)
            putStrLn "Invalid option: Please select a valid option from below menu"
            sleepMs 100
            main
          Right x2 -> do
            -- Show Chat History of specific user - Additional Feature
            if x2 >= 1 && x2 <=10
             then do
              let sent = findMsgByUser (users !! (x2-1)) finalList
              showMsgByUser (users !! (x2-1)) sent
              sleepMs 1
              let receive = findMsgToUser (users !! (x2-1)) finalList
              showMsgToUser (users !! (x2-1)) receive
             else do
              putStrLn "Invalid Entry, exiting application!"