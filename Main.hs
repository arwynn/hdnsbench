{-# Language OverloadedStrings #-}

import Network.Socket ( SocketType(..) )
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Data.List (unfoldr)
import Data.Either (lefts)
import Data.DomainDB
import Control.Exception as E

--lookupDomains :: ResolvSeed -> [(Domain,TYPE)] -> IO ()
lookupDomains rs ds =
    forM ds $ \(dn, t) -> do
        withResolver rs $ \resolver -> do
            threadDelay 10
            E.handle err $ DNS.lookup resolver dn t
    where err :: SomeException -> IO (Either DNSError [RDATA])
          err e = print e >> return (Left ServerFailure)

chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
    ds'' <- readDomains "data/alexa-top-2000-domains.txt"
    let ds' = take 300 ds''
        n  = (length ds') `div` 3
        ds = chunks n ds'
        rc = ResolvConf {resolvInfo = RCHostPort "192.168.1.1" 53
                       , resolvTimeout = 10 * 1000 * 1000
                       , resolvRetry = 10
                       , resolvBufsize = 4096
                       , resolvSockType = Stream}
    rs <- makeResolvSeed rc
    putStrLn "Sending requests..."
    res' <- mapConcurrently (lookupDomains rs) ds
    putStrLn "Finished"
    let res = concat res'
        tot = length res
        err = length $ lefts res
    putStrLn "==============================="
    putStrLn $ "Total sent: " ++ show(tot)
    putStrLn $ "Errors: " ++ show(err)
    return ()
