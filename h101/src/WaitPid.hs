
module WaitPid (waitpid) where

import Foreign (Ptr,alloca)
import Foreign.C (CInt(..),throwErrnoIfMinus1)
import System.Posix.Types (CPid(..))

foreign import ccall unsafe "waitpid" c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

waitpid :: CPid -> IO ()
waitpid pid = alloca $ \status -> do
  _ <- throwErrnoIfMinus1 "waitpid" $ c_waitpid pid status 0
  return ()
