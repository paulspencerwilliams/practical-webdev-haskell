module Lib
  ( someFunc
  )
where

import           ClassyPrelude
import qualified Adapter.InMemory.Auth         as M
import           Domain.Auth
import           Control.Monad                  ( MonadFail )
import           Katip
import qualified Adapter.PostgreSQL.Auth  as PG
import qualified Adapter.Redis.Auth as Redis
import Control.Exception.Safe (MonadThrow)

type State = (PG.State, Redis.State, TVar M.State)
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail, KatipContext, Katip, MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state
  = runKatipContextT le () mempty
  . flip runReaderT state 
  . unApp


withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = bracket createLogEnv closeScribes app
 where
  createLogEnv = do
    logEnv       <- initLogEnv "HAuth" "prod"
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace" 
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1" 
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2" 
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" $ asText "12") $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context" 
        katipAddContext (sl "country" $ asText "Singapore") $
          $(logTM) InfoS "Log in ns2.ns3 with userId and country context"


instance AuthRepo App where
  addAuth             = PG.addAuth
  setEmailAsVerified  = PG.setEmailAsVerified
  findUserByAuth      = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession            = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \ redisState ->
      run le (pgState, redisState, mState) action
  where
    redisCfg = "redis://localhost:6379/0"
    pgCfg = PG.Config 
            { PG.configUrl = "postgresql://postgres:postgres@localhost/hauth"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth  = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session         <- login auth
  Just  uId             <- resolveSessionId session
  Just  registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
