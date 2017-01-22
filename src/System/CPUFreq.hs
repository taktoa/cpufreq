{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module System.CPUFreq where

import           Control.Monad

import           Foreign.C.Error
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

import           Data.Word

import           Data.Set                (Set)
import qualified Data.Set                as Set

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS

import           System.CPUFreq.Internal

type CPUIndex    = Int
type Frequency   = Int
type Latency     = Int
type Governor    = ByteString
type Driver      = ByteString
type ElapsedTime = Word64

data Policy
  = MkPolicy
    { minFreq  :: Frequency
    , maxFreq  :: Frequency
    , governor :: Governor
    }
  deriving (Eq, Show, Read)

type Stats = (Frequency, ElapsedTime)

-- | FIXME: doc
cpuExists :: CPUIndex -> IO Bool
cpuExists idx = do errno <- cf_cpu_exists $ fromIntegral idx
                   pure $ errno == eOK

-- | FIXME: doc
getKernelFrequency :: CPUIndex -> IO Frequency
getKernelFrequency = fmap coerceI . cf_get_frequency_kernel . coerceI

-- | FIXME: doc
getHardwareFrequency :: CPUIndex -> IO Frequency
getHardwareFrequency = fmap coerceI . cf_get_frequency_hardware . coerceI

-- | FIXME: doc
getTransitionLatency :: CPUIndex -> IO Latency
getTransitionLatency = fmap coerceI . cf_get_frequency_hardware . coerceI

-- | FIXME: doc
getHardwareLimits :: CPUIndex -> IO (Frequency, Frequency)
getHardwareLimits idx = do minFP <- mallocForeignPtr
                           maxFP <- mallocForeignPtr
                           withForeignPtr minFP $ \minP ->
                             withForeignPtr maxFP $ \maxP -> helper minP maxP
  where
    helper :: Ptr CFFreq -> Ptr CFFreq -> IO (Frequency, Frequency)
    helper minP maxP = do
      checkErrno $ cf_get_hardware_limits (coerceI idx) minP maxP
      minFreq <- peek minP
      maxFreq <- peek maxP
      pure (coerceI minFreq, coerceI maxFreq)

-- | FIXME: doc
getDriver :: CPUIndex -> IO ByteString
getDriver idx = do
  ptr <- cf_get_driver $ coerceI idx
  driver <- BS.packCString ptr
  cf_put_driver ptr
  pure driver

-- | FIXME: doc
getPolicy :: CPUIndex -> IO Policy
getPolicy idx = do
  ptr <- cf_get_policy (coerceI idx)
  (CFPolicy {..}) <- peek ptr
  let (!minFreq, !maxFreq) = (coerceI _p_min, coerceI _p_max)
  !governor <- BS.packCString _p_governor
  let !policy = MkPolicy {..}
  cf_put_policy ptr
  pure policy

-- | FIXME: doc
availableGovernors :: CPUIndex -> IO (Set Governor)
availableGovernors idx = do ptr <- cf_get_available_governors (coerceI idx)
                            res <- helper ptr []
                            cf_put_available_governors ptr
                            pure $ Set.fromList res
  where
    helper :: Ptr CFAvailableGovernors -> [Governor] -> IO [Governor]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAvailableGovernors {..}) <- peek ptr
                               !gov <- BS.packCString _ag_governor
                               helper _ag_next (gov : soFar)

availableFrequencies :: CPUIndex -> IO (Set Frequency)
availableFrequencies idx = do ptr <- cf_get_available_frequencies (coerceI idx)
                              res <- helper ptr []
                              cf_put_available_frequencies ptr
                              pure $ Set.fromList res
  where
    helper :: Ptr CFAvailableFrequencies -> [Frequency] -> IO [Frequency]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAvailableFrequencies {..}) <- peek ptr
                               let freq = coerceI _af_frequency
                               helper _af_next (freq : soFar)

-- | FIXME: doc
affectedCPUs :: CPUIndex -> IO (Set CPUIndex)
affectedCPUs idx = do ptr <- cf_get_affected_cpus (coerceI idx)
                      res <- helper ptr []
                      cf_put_affected_cpus ptr
                      pure $ Set.fromList res
  where
    helper :: Ptr CFAffectedCPUs -> [CPUIndex] -> IO [CPUIndex]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAffectedCPUs {..}) <- peek ptr
                               let cpu = coerceI _ac_cpu
                               helper _ac_next (cpu : soFar)

-- | FIXME: doc
relatedCPUs :: CPUIndex -> IO (Set CPUIndex)
relatedCPUs idx = do ptr <- cf_get_related_cpus (coerceI idx)
                     res <- helper ptr []
                     cf_put_related_cpus ptr
                     pure $ Set.fromList res
  where
    helper :: Ptr CFAffectedCPUs -> [CPUIndex] -> IO [CPUIndex]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFAffectedCPUs {..}) <- peek ptr
                               let cpu = coerceI _ac_cpu
                               helper _ac_next (cpu : soFar)

-- | FIXME: doc
cpuStatistics :: CPUIndex -> ElapsedTime -> IO [Stats]
cpuStatistics idx tot = do ptr <- cf_get_stats (coerceI idx) (coerceI tot)
                           res <- helper ptr []
                           cf_put_stats ptr
                           pure res
  where
    helper :: Ptr CFStats -> [Stats] -> IO [Stats]
    helper ptr soFar = if ptr == nullPtr
                       then pure soFar
                       else do (CFStats {..}) <- peek ptr
                               let freq = coerceI _s_frequency
                               let time = coerceI _s_time_in_state
                               helper _s_next ((freq, time) : soFar)

-- | FIXME: doc
setPolicy :: CPUIndex -> Policy -> IO ()
setPolicy = undefined

-- | FIXME: doc
setMinFreq :: CPUIndex -> Frequency -> IO ()
setMinFreq = undefined

-- | FIXME: doc
setMaxFreq :: CPUIndex -> Frequency -> IO ()
setMaxFreq = undefined

-- | FIXME: doc
setGovernor :: CPUIndex -> Governor -> IO ()
setGovernor = undefined

-- | FIXME: doc
setFrequency :: CPUIndex -> Frequency -> IO ()
setFrequency = undefined

--------------------------------------------------------------------------------

checkErrno :: IO Errno -> IO ()
checkErrno = undefined

coerceI :: (Integral i, Integral i') => i -> i'
coerceI = fromIntegral

{-

import           Control.Concurrent
import           Control.Exception
import           Data.Bits
import           Data.List
import           Data.Monoid
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.CPUFreq.Internal
import           System.IO
import           System.Posix.Signals

-- the opaque void* from pam
data CPamHandle = CPamHandle
-- the handle returned to the app
data PamHandle  = PamHandle (Ptr CPamHandleT, FunPtr ConvFunc) deriving (Show)

data PamMessage = PamMessage { pamString :: String
                             , pamStyle  :: PamStyle }
                deriving (Show, Eq)

data PamResponse = PamResponse String deriving (Show, Eq)

data PAMReturnCode = PAM_SUCCESS | PAM_OPEN_ERR | PAM_SYMBOL_ERR | PAM_SERVICE_ERR | PAM_SYSTEM_ERR | PAM_BUF_ERR | PAM_PERM_DENIED | PAM_AUTH_ERR | PAM_CRED_INSUFFICIENT | PAM_AUTHINFO_UNAVAIL | PAM_USER_UNKNOWN | PAM_MAXTRIES | PAM_NEW_AUTHTOK_REQD deriving (Enum, Show, Eq)

data PamCredFlags = PAM_ESTABLISH_CRED | PAM_DELETE_CRED | PAM_REINITIALIZE_CRED | PAM_REFRESH_CRED deriving (Enum, Show)

data PamStyle = PamPromptEchoOff
              | PamPromptEchoOn
              | PamErrorMsg | PamTextInfo deriving (Show, Eq)

data PAMItem = PAM_TTY { tty :: String }

type PAMConv = [PamMessage] -> IO [PamResponse]

messageFromC :: CPamMessage -> IO PamMessage
messageFromC cmes = (\s -> PamMessage s style) <$> peekCString (msg cmes)
  where
    style = case msg_style cmes
            of 1 -> PamPromptEchoOff
               2 -> PamPromptEchoOn
               3 -> PamErrorMsg
               4 -> PamTextInfo
               a -> error $ "unknown style value: " <> show a

convWrapper :: PAMConv               -- A callback given by the user
            -> CInt                  -- Number of items in the array
            -> Ptr (Ptr ())          -- Array of messages
            -> Ptr (Ptr ())          -- Responses going back out to PAM (?)
            -> Ptr ()                -- Pointer for application data (useless)
            -> IO CInt               -- Status code (0 indicates success)
convWrapper _     c _    _    _  | c <= 0 = return 19 -- an error code?
convWrapper userC c msgs reply _          = do
  p1 <- peek msgs

  let mesArr = castPtr p1 :: Ptr CPamMessage

  -- turn input into array of pointers
  cMessages <- peekArray (fromIntegral c) mesArr

  -- turn array of pointers into array of data's
  messages <- mapM messageFromC cMessages

  replies <- userC messages

  cResponses <- mapM responseToC replies

  respArr <- newArray cResponses

  poke reply $ castPtr respArr

  return 0
  where
    responseToC (PamResponse str) = do
      resp' <- newCString str
      return $ CPamResponse resp' 0

pamStart :: String -> String -> PAMConv -> IO (PamHandle,PAMReturnCode)
pamStart service user convIn = protect $ do
  cService <- newCString service
  cUser <- newCString user

  wrapped <- mkconvFunc $ convWrapper convIn
  let convStructHs = CPamConv wrapped nullPtr

  convPtr <- malloc
  poke convPtr convStructHs

  pamHandlePtr <- malloc
  poke pamHandlePtr nullPtr

  r1 <- c_pam_start cService cUser convPtr pamHandlePtr

  pamHandle <- peek pamHandlePtr
  return (PamHandle (pamHandle, wrapped), toEnum $ fromIntegral r1)

pamAuthenticate :: PamHandle -> CInt -> IO (PAMReturnCode)
pamAuthenticate (PamHandle (hnd,_)) flags = protect $ do
  ret <- c_pam_authenticate hnd flags
  return $ toEnum $ fromIntegral ret

pamSetCred :: PamHandle -> (Bool,PamCredFlags) -> IO PAMReturnCode
pamSetCred (PamHandle (hnd,_)) (silent, flag) = protect $ do
  let flag' = case flag of
        PAM_ESTABLISH_CRED    -> 0x2
        PAM_DELETE_CRED       -> 0x4
        PAM_REINITIALIZE_CRED -> 0x8
        PAM_REFRESH_CRED      -> 0x10
  let silent' = if silent then 0x8000 else 0
  ret <- c_pam_setcred hnd (flag' .|. silent')
  return $ toEnum $ fromIntegral ret

pamEnd :: PamHandle -> Int -> IO PAMReturnCode
pamEnd (PamHandle (hnd,_)) status = protect $ do
  putStrLn "ending"
  hFlush stdout
  ret <- c_pam_end hnd $ fromIntegral status
  return $ toEnum $ fromIntegral ret

protect :: IO a -> IO a
protect act = runInBoundThread go
  where
    go = do
      blockSignals reservedSignals
      act `finally` unblockSignals reservedSignals

pamOpenSession :: PamHandle -> Bool -> IO PAMReturnCode
pamOpenSession (PamHandle (hnd,_)) silent = protect $ do
  ret <- c_pam_open_session hnd $ if silent then 0x8000 else 0
  return $ toEnum $ fromIntegral ret
pamCloseSession :: PamHandle -> Bool -> IO PAMReturnCode
pamCloseSession (PamHandle (hnd,_)) silent = protect $ do
  ret <- c_pam_close_session hnd $ if silent then 0x8000 else 0
  return $ toEnum $ fromIntegral ret

pamSetItem :: PamHandle -> PAMItem -> IO PAMReturnCode
pamSetItem (PamHandle (hnd,_)) item = protect $ do
  let (itemType,func) = getPtr item
  ptr <- func
  let ptr' = castPtr ptr :: Ptr ()
  ret <- c_pam_set_item hnd itemType ptr'
  free ptr
  return $ toEnum $ fromIntegral ret
  where
    getPtr :: PAMItem -> (CInt, IO CString)
    getPtr (PAM_TTY t) = (3, newCString t)

pamGetEnvList :: PamHandle -> IO [(String,String)]
pamGetEnvList (PamHandle (hnd,_)) = protect $ do
  raw <- c_pam_getenvlist hnd
  --middle <- _ $ raw
  middle <- peekArray0 nullPtr raw
  converted <- mapM peekCString middle
  let splitter = splitOn "="
  let
    f :: [String] -> (String,String)
    f (a:b) = (a,intercalate "=" b)
    f []    = error "empty env found"
  let splitUp = map f (map splitter converted)
  return splitUp

-}
