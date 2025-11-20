module Types where

import Database.MySQL.Simple (Connection)
import Control.Monad.State (StateT)

-- | The application state now only needs the active connection.
-- mysql-simple handles caching internally.
data AppState = AppState
  { dbConnection :: Connection
  }

type AppM = StateT AppState IO