%\section{Before we get begin}
%
%\begin{frame}[fragile]
%\frametitle{Literate Haskell}
%
%This talk is a valid Haskell source file.
%
%\hfill
%
%\begin{code}
%{-# LANGUAGE FlexibleInstances, OverloadedStrings,
%             FlexibleContexts, TemplateHaskell,
%             LambdaCase, RankNTypes,
%             GeneralizedNewtypeDeriving,
%             NoMonomorphismRestriction #-}
%
%module Before where
%
%import Control.Applicative
%import Control.Monad
%import Control.Monad.Reader
%import Control.Monad.Except
%import Control.Monad.Writer
%import Control.Monad.State
%import Control.Monad.Trans
%import Control.Monad.RWS
%import Control.Lens hiding (Lens, Prism, prism)
%import Data.Text
%type Lens a b = Lens' a b
%type Prism a b = Prism' a b
%prism = prism'
%loadFromDb = undefined
%sendOverNet = undefined
%mightFail = undefined
%couldFail = undefined
%data Err = Err
%data DbConnection = DBC ()
%data MyData = MyData
%type Stuff = ()
%type Things = ()
%data User = User
%data Schema = Schema
%data Port = Port Int
%data Ssl = Ssl
%\end{code}
%\end{frame}


\section{Goals}

\begin{frame}
\frametitle{Motivation}

We need to write programs that
\begin{itemize}
\item pass configuration
\item handle errors
\item maintain some kind of state
\item perform IO
\item write logs
\item \ldots
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Motivation}

We need to write programs that
\begin{itemize}
\item {\bf pass configuration}
\item {\bf handle errors}
\item maintain some kind of state
\item {\bf perform IO}
\item write logs
\item \ldots
\end{itemize}
\end{frame}


\begin{frame}
\frametitle{Goals}

\begin{itemize}
\item<+->
We want compositionality

\item<+->
We want types to help us
%TODO this isn't a goal because it isn't measurable
\end{itemize}
\end{frame}


\section{Disclaimers}

\begin{frame}
\frametitle{}

\begin{center}
  {\huge Warning: Lies}
\end{center}
\end{frame}

%------------------------------------------------

\begin{frame}
\begin{center}
{\huge TRANSFORMERS}
\end{center}
\end{frame}


\begin{frame}
\frametitle{Transformer usage in apps}

Let's focus on two things for a moment:

{\bf Configuration and error handling}

\end{frame}

\begin{frame}[fragile]
\frametitle{Config}

\begin{itemize}

\item<+->[]
\begin{code}
data DbConfig =
  DbConfig {
    dbConn :: DbConnection
  , schema :: Schema
  }
\end{code}

\item<+->[]
\begin{code}
data NetworkConfig =
  NetConfig {
    port :: Port
  , ssl  :: Ssl
  }
\end{code}

\item<+->[]
\begin{code}
data AppConfig =
  AppConfig {
    appDbConfig :: DbConfig
  , appNetConfig :: NetworkConfig
  }
\end{code}

\end{itemize}
\end{frame}

% TODO draw a pretty picture

\begin{frame}[fragile]
\frametitle{Errors}

\begin{itemize}

\item<+->[]
\begin{code}
data DbError =
    QueryError Text
  | InvalidConnection
\end{code}

\item<+->[]
\begin{code}
data NetworkError =
    Timeout Int
  | ServerOnFire
\end{code}

\item<+->[]
\begin{code}
data AppError =
    AppDbError DbError
  | AppNetError NetworkError
\end{code}

\end{itemize}
\end{frame}


% TODO draw another pretty picture


\begin{frame}[fragile]
\frametitle{A monad just for my application}

\begin{itemize}

\item<+->[]
\begin{code}
newtype App a =
  App {
    unApp :: ReaderT AppConfig (ExceptT AppError IO) a
  } deriving (
    Functor,
    Applicative,
    Monad,
    MonadReader AppConfig,
    MonadError AppError,
    MonadIO
  )
\end{code}

\item<+->[]
\ldots How do we use this thing?

\end{itemize}
\end{frame}


\section{MTL Refresher}

\begin{frame}
\begin{center}
{\huge MTL REFRESHER}
\end{center}
\end{frame}


\begin{frame}
\frametitle{mtl}
Idea: Associate with each transformer a typeclass
specifying its operations
\end{frame}


\begin{frame}[fragile]
\frametitle{Reader}

\begin{itemize}

\item<+->[]
\begin{fakecode}
class Monad m => MonadReader r m | m -> r where
\end{fakecode}

\item<+->[]
\begin{fakecode}
    -- Retrieves the monad environment.
    ask   :: m r
    ask = reader id
\end{fakecode}

\item<+->[]
\begin{fakecode}
    -- Retrieves a function of the current environment.
    reader :: (r -> a)
           -> m a
    reader f = do
      r <- ask
      return (f r)
\end{fakecode}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{MonadReader Example}

\begin{code}
getPort :: MonadReader NetworkConfig m
        => m Port
getPort = reader port
\end{code}
\end{frame}


\begin{frame}[fragile]
\frametitle{MonadReader Example}

\begin{fakecode}
getPort :: MonadReader NetConfig m
        => m Port
getPort =
  do cfg <- ask
     return (port cfg)
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{MonadIO}

\begin{fakecode}
class (Monad m) => MonadIO m where
    -- Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{MonadIO Example}

\begin{code}
printM :: MonadIO m
       => String -> m ()
printM s = liftIO (putStrLn s)
\end{code}
\end{frame}


\begin{frame}[fragile]
\frametitle{Errors}

\begin{itemize}

\item<+->[]
\begin{fakecode}
class (Monad m) => MonadError e m | m -> e where
\end{fakecode}

\item<+->[]
\begin{fakecode}
    -- Is used within a monadic computation to
    -- begin exception processing.
    throwError :: e -> m a
\end{fakecode}

\item<+->[]
\begin{fakecode}
    -- A handler function to handle previous errors
    -- and return to normal execution.
    catchError :: m a -> (e -> m a) -> m a
\end{fakecode}

\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{MonadError Example}

\begin{itemize}

\item<+->[]
\begin{code}
mightFail :: MonadError Err m
          => m Int

couldFail :: MonadError Err m
          => m String
\end{code}

\item<+->[]
\begin{code}
maybeFail :: MonadError Err m
          => m (Maybe (Int,String))
maybeFail =
  ( do a <- mightFail
       b <- couldFail
       pure (Just (a,b))
  ) `catchError` (\err -> pure Nothing)
\end{code}

\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Vocabulary}

\begin{itemize}

\item<+->[]
\begin{fakecode}
-- instance MonadReader AppConfig App
ask :: App AppConfig
\end{fakecode}

\item<+->[]
\begin{fakecode}
-- instance MonadError AppError App
throwError :: AppError -> App a
catchError :: App a -> (AppError -> App a) -> App a
\end{fakecode}

\item<+->[]
\begin{fakecode}
-- instance MonadIO App
liftIO :: IO a -> App a
\end{fakecode}

\end{itemize}
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Still Not Good Enough}

\begin{frame}[fragile]
\frametitle{Yet Another Example}

\begin{fakecode}
loadFromDb :: App MyData

sendOverNet :: MyData -> App ()

loadAndSend :: App ()
loadAndSend = loadFromDb >>= sendOverNet
\end{fakecode}

\end{frame}


\begin{frame}[fragile]
\frametitle{Not Good Enough!}
{\color{red}
\begin{fakecode}
loadFromDb :: App MyData

sendOverNet :: MyData -> App ()

loadAndSend :: App ()
loadAndSend = loadFromDb >>= sendOverNet
\end{fakecode}
}

\end{frame}


\begin{frame}[fragile]
\frametitle{Generalise Everything}
\begin{itemize}

\item<+->[]
\begin{code}
loadFromDb :: (MonadReader DbConfig m,
               MonadError  DbError  m,
               MonadIO m)
           => m MyData
\end{code}

\item<+->[]
\begin{code}
sendOverNet :: (MonadReader NetworkConfig m,
                MonadError  NetworkError  m,
                MonadIO m)
            => MyData -> m ()
\end{code}

\item<+->[]
\begin{fakecode}
loadAndSend = loadFromDb >>= sendOverNet
\end{fakecode}

\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Oops}

\begin{verbatim}
    Couldn't match type ‘NetworkConfig’ with ‘DbConfig’
    arising from a functional dependency between constraints:
      ‘MonadReader DbConfig m’
        arising from a use of ‘loadFromDb’ at P.hs:447:15-24
      ‘MonadReader NetworkConfig m’
        arising from a use of ‘sendOverNet’ at P.hs:447:30-40
    In the first argument of ‘(>>=)’, namely ‘loadFromDb’
    In the expression: loadFromDb >>= sendOverNet
    In an equation for ‘loadAndSend’:
        loadAndSend = loadFromDb >>= sendOverNet
\end{verbatim}

\end{frame}

