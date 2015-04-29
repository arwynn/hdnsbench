{-# Language OverloadedStrings #-}

import Network.Socket ( SocketType(Stream) )
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)
import Control.Monad
import Control.Concurrent.Async
import Data.List (unfoldr)

lookupDomains :: ResolvSeed -> [(Domain,TYPE)] -> IO ()
lookupDomains rs ds =
    withResolver rs $ \resolver -> do
        forM_ ds $ \dn -> do
            uncurry (DNS.lookup resolver) dn >>= print

chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

main :: IO ()
main = do
    let rc = ResolvConf {resolvInfo = RCHostName "8.8.8.8"
                       , resolvTimeout = 3 * 1000 * 1000
                       , resolvRetry = 3
                       , resolvBufsize = 512
                       , resolvSockType = Stream}
    rs <- makeResolvSeed rc
    _ <- mapConcurrently (lookupDomains rs) ds
    return ()
    where ds = chunks n ds'
          n = (length ds') `div` 3
          ds' = [("www.example.com", A), ("yandex.ru", A), ("google.com", A),
                 ("vk.com", A), ("risk.ru", A), ("rkfke.com", A),
                 ("vk.com", AAAA), ("risk.ru", AAAA), ("rkfke.com", AAAA)]
