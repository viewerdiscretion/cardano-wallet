{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Api.Server.TlsSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Server
    ( Listen (..), TlsConfiguration (..), withListeningSocket )
import Control.Tracer
    ( nullTracer )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Default
    ( def )
import Data.Function
    ( (&) )
import Data.X509
    ( CertificateChain (..) )
import Data.X509.CertificateStore
    ( makeCertificateStore )
import Data.X509.File
    ( readKeyFile, readSignedObject )
import Network.Connection
    ( TLSSettings (..) )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (..)
    , ManagerSettings (..)
    , Response
    , defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseStatus
    )
import Network.HTTP.Client.TLS
    ( mkManagerSettings )
import Network.HTTP.Types.Status
    ( Status (..) )
import Network.TLS
    ( AlertDescription (..)
    , ClientHooks (..)
    , ClientParams (..)
    , Credentials (..)
    , Shared (..)
    , Supported (..)
    , TLSError (..)
    , TLSException (..)
    , noSessionManager
    )
import Network.TLS.Extra.Cipher
    ( ciphersuite_default )
import Network.Wai
    ( responseLBS )
import System.FilePath
    ( FilePath, (</>) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldThrow )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Windows
    ( pendingOnWine )
import UnliftIO.Async
    ( async, link )
import UnliftIO.Exception
    ( fromException )

import qualified Cardano.Wallet.Api.Server as Server
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = describe "TLS Client Authentication" $ do
    it "Respond to authenticated client if TLS is enabled" $ do
        pendingOnWine "CertOpenSystemStoreW is failing under Wine"
        withListeningSocket "*" ListenOnRandomPort $ \(Right (port, socket)) -> do
            let tlsSv = TlsConfiguration
                    { tlsCaCert = rootPKI 1 </> "ca.crt"
                    , tlsSvCert = rootPKI 1 </> "server.crt"
                    , tlsSvKey  = rootPKI 1 </> "server.key"
                    }
            let tlsCl = TlsConfiguration
                    { tlsCaCert = rootPKI 1 </> "ca.crt"
                    , tlsSvCert = rootPKI 1 </> "client.crt"
                    , tlsSvKey  = rootPKI 1 </> "client.key"
                    }
            link =<< async
                (Server.start warpSettings nullTracer (Just tlsSv) socket app)

            response <- pingHttps tlsCl port
            responseStatus response `shouldBe` Http.Status
                { statusCode = 200
                , statusMessage = "Ok"
                }

    it "Deny client with wrong certificate if TLS is enabled" $ do
        pendingOnWine "CertOpenSystemStoreW is failing under Wine"
        withListeningSocket "*" ListenOnRandomPort $ \(Right (port, socket)) -> do
            let tlsSv = TlsConfiguration
                    { tlsCaCert = rootPKI 1 </> "ca.crt"
                    , tlsSvCert = rootPKI 1 </> "server.crt"
                    , tlsSvKey  = rootPKI 1 </> "server.key"
                    }
            let tlsCl = TlsConfiguration
                    { tlsCaCert = rootPKI 2 </> "ca.crt"
                    , tlsSvCert = rootPKI 2 </> "client.crt"
                    , tlsSvKey  = rootPKI 2 </> "client.key"
                    }
            link =<< async
                (Server.start warpSettings nullTracer (Just tlsSv) socket app)

            pingHttps tlsCl port `shouldThrow` \case
                HttpExceptionRequest _ (InternalException e) ->
                    case fromException e of
                        Just (Terminated _ _ (Error_Protocol (_,_,alert))) ->
                            alert == CertificateUnknown
                        _ -> False
                _ -> False

    it "Properly deny HTTP connection if TLS is enabled" $ do
        withListeningSocket "*" ListenOnRandomPort $ \(Right (port, socket)) -> do
            let tlsSv = TlsConfiguration
                    { tlsCaCert = rootPKI 1 </> "ca.crt"
                    , tlsSvCert = rootPKI 1 </> "server.crt"
                    , tlsSvKey  = rootPKI 1 </> "server.key"
                    }
            link =<< async
                (Server.start warpSettings nullTracer (Just tlsSv) socket app)

            response <- pingHttp port
            responseStatus response `shouldBe` Http.Status
                { statusCode = 426
                , statusMessage = "Upgrade Required"
                }

--
-- Test Application
--

warpSettings :: Warp.Settings
warpSettings = Warp.defaultSettings
    -- NOTE By default, Warp prints any exception on stdout, which is kinda
    -- annoying...
    & Warp.setOnException (\_ _ -> pure ())

rootPKI :: Int -> FilePath
rootPKI i
    | i `elem` [1,2] = $(getTestData) </> "PKIs" </> show i
    | otherwise = error $ "rootPKI: There's no PKI for index #" <> show i

app :: Wai.Application
app _request respond =
    respond $ responseLBS Http.status200 [] "All your bases are belong to us!"

pingHttp :: Int -> IO (Response ByteString)
pingHttp port = do
    r <- parseRequest $ "GET http://127.0.0.1:" <> show port
    m <- newManager defaultManagerSettings
    httpLbs r m

pingHttps :: TlsConfiguration -> Int -> IO (Response ByteString)
pingHttps tls port = do
    r <- parseRequest $ "GET https://127.0.0.1:" <> show port
    m <- newManager =<< mkHttpsManagerSettings tls
    httpLbs r m

-- | Construct a 'ManagerSettings' for a client application using the given TLS
-- configuration. The 'TlsConfiguration' is slightly _abused_ here as the
-- @tlsSvCert@ and @tlsSvKey@ are actually pointing to **client** credentials.
mkHttpsManagerSettings
    :: TlsConfiguration
    -> IO ManagerSettings
mkHttpsManagerSettings TlsConfiguration{tlsCaCert,tlsSvCert,tlsSvKey} = do
    params <- clientParams
        <$> readSignedObject tlsCaCert
        <*> readCredentials tlsSvCert tlsSvKey
    pure $ mkManagerSettings (TLSSettings params) sockSettings
  where
    sockSettings = Nothing
    clientParams caChain credentials = ClientParams
        { clientUseMaxFragmentLength = Nothing
        , clientServerIdentification = ("127.0.0.1", "")
        , clientUseServerNameIndication = True
        , clientWantSessionResume = Nothing
        , clientShared = clientShared caChain credentials
        , clientHooks = clientHooks credentials
        , clientSupported = clientSupported
        , clientDebug = def
        , clientEarlyData = def
        }

    clientShared caChain credentials = Shared
        { sharedCredentials = Credentials [credentials]
        , sharedCAStore = makeCertificateStore caChain
        , sharedSessionManager = noSessionManager
        , sharedValidationCache = def
        }

    clientHooks credentials = def
        { onCertificateRequest = const . return . Just $ credentials
        , onServerCertificate = \_ _ _ _ -> pure []
        }

    clientSupported = def
        { supportedCiphers = ciphersuite_default
        }

    readCredentials certFile keyFile = (,)
        <$> (CertificateChain <$> readSignedObject certFile)
        <*> (head <$> readKeyFile keyFile)
