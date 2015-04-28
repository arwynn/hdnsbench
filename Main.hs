{-# Language OverloadedStrings #-}

import Network.BSD
import Network.Socket
import qualified Network.DNS as DNS (lookup)
import Network.DNS hiding (lookup)

setSockType rs@(ResolvSeed {addrInfo = ai}) = rs {addrInfo = ai'}
    where ai' = ai {addrSocketType = Stream, addrProtocol = proto}
          proto = getProtocolNumber "tcp"

main :: IO ()
main = do
    let rc = ResolvConf {resolvInfo = RCHostName "8.8.8.8"
                       , resolvTimeout = 3 * 1000 * 1000
                       , resolvRetry = 3
                       , resolvBufsize = 512}
    rs <- setSockType $ makeResolvSeed rc
    withResolver rs $ \resolver -> do
        DNS.lookup resolver "www.example.com" A >>= print
