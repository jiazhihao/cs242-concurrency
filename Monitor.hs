import Control.Concurrent.MVar
import Data.IORef
import Control.Concurrent
import Control.Monad

data Monitor = Monitor
    { monitorLock :: MVar ()
    , monitorCond :: MVar [MVar ()]
    }

-- | Repeatedly tests @b@ and runs @doit@ if false.
whileM :: IO Bool -> IO () -> IO ()
whileM cond body = undefined

-- | Create a new monitor object, which contains the lock as
-- well as the queue of condition variables threads are waiting on.
newMonitor :: IO Monitor
newMonitor = undefined

-- | Runs a computation within a monitor.
synchronized :: Monitor -> IO a -> IO a
synchronized m doit = undefined

-- | Inside a 'synchronized' block, releases the lock and waits
-- to be notified
wait :: Monitor -> IO ()
wait m = undefined

-- | Notifies the monitor that some conditions may have become true,
-- and wakes up one process.
notify :: Monitor -> IO ()
notify m = undefined

---------------------------------------------------------------------
-- Example code:

data Account = Account {
    withdraw :: Int -> IO (),
    deposit :: Int -> IO ()
}

newAccount :: IO Account
newAccount = do
    m <- newMonitor
    balance <- newIORef 0
    return Account
            { withdraw = \n -> synchronized m $ do
                putStrLn ("Withdrawing " ++ show n)
                whileM (fmap (< n) $ readIORef balance) $ wait m
                curr <- readIORef balance
                writeIORef balance (curr - n)
            , deposit = \n -> synchronized m $ do
                putStrLn ("Depositing " ++ show n)
                curr <- readIORef balance
                writeIORef balance (curr + n)
                notify m
            }

makeAccountWithPendingWithdrawal = do
    a <- newAccount
    forkIO $ do
        withdraw a 20
        putStrLn "Withdrawal approved"
    return a
