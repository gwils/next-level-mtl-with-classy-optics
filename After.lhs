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
%             ConstraintKinds, GeneralizedNewtypeDeriving,
%             NoMonomorphismRestriction #-}
%
%module After where
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
%data DbConnection = DBC ()
%data User = User
%data Schema = Schema
%data Port = Port Int
%data Ssl = Ssl
%data MyData = MyData
%newtype App a = App { runApp :: ReaderT AppConfig (ExceptT AppError IO) a }
%                deriving (Functor, Applicative, Monad, MonadIO,
%                          MonadReader AppConfig, MonadError AppError)
%\end{code}
%\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Classy Optics}

\begin{frame}[fragile]
\frametitle{}
\begin{center}
  {\huge Optics}
\end{center}
\end{frame}


\begin{frame}[fragile]
\frametitle{Optics?}
Optics come from {\tt lens} on Hackage.

We're going to talk about {\bf lenses} and {\bf prisms} today.

There are many more optics in {\tt lens}!
\end{frame}


\begin{frame}[fragile]
\frametitle{What is a lens?}
A lens is a getter-setter

It lets us get at one part of a whole

\begin{itemize}
\item<+->[]
\begin{fakecode}
-- basic lens usage
view :: Lens source target
     -> (source -> target) -- getter

set  :: Lens source target
     -> target -> source -> source -- setter
\end{fakecode}

\item<+->[]
\begin{fakecode}
-- Construct a lens
lens :: (source -> target) -- getter
     -> (source -> target -> source) --setter
     -> Lens source target
\end{fakecode}

\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Lenses compose!}

\begin{fakecode}
(.) :: Lens s t -> Lens t u -> Lens s u
id  :: Lens a a
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{Lens examples}

\begin{itemize}
\item<+->[]
\begin{fakecode}
_1 :: Lens (a,b) a
_2 :: Lens (a,b) b
\end{fakecode}

\item<+->[]
\begin{fakecode}
(_1 . _2) :: Lens ((c,d),e) d
\end{fakecode}

\item<+->[]
\begin{fakecode}
 > view _1 (("hello", Nothing), 3)
\end{fakecode}
{\color{blue}
\begin{fakecode}
("hello", Nothing)
\end{fakecode}
}

\item<+->[]
\begin{fakecode}
 > view (_1 . _1) (("hello", Nothing), 3)
\end{fakecode}

{\color{blue}
\begin{fakecode}
"hello"
\end{fakecode}
}
\end{itemize}

\end{frame}


\begin{frame}[fragile]
\frametitle{Lens examples}
\begin{itemize}
\item[]
\begin{fakecode}
_1 :: Lens (a,b) a
_2 :: Lens (a,b) b
\end{fakecode}

\item<+->[]
\begin{fakecode}
(_1 . _2) :: Lens ((c,d),e) d
\end{fakecode}

\item<+->[]
\begin{fakecode}
 > set _2 1000 (("hello", Nothing), 3)
\end{fakecode}
{\color{blue}
\begin{fakecode}
(("hello", Nothing), 1000)
\end{fakecode}
}

\item<+->[]
\begin{fakecode}
 > set (_1 . _2) (Just "lens") (("hello", Nothing), 3)
\end{fakecode}
{\color{blue}
\begin{fakecode}
(("hello", Just "lens"), 3)
\end{fakecode}
}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{What is a prism?}
A prism is like a first-class pattern match

It lets us get at one branch of an ADT

\begin{itemize}
\item<+->[]
\begin{fakecode}
-- basic prism usage
preview :: Prism a b
        -> (a -> Maybe b) -- partial getter

review  :: Prism a b
        -> (b -> a) -- constructor
\end{fakecode}

\item<+->[]
\begin{fakecode}
-- construct a prism
prism :: (target -> source) 
      -> (source -> Maybe target)
      -> Prism source target
\end{fakecode}

\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Prisms compose!}

\begin{fakecode}
(.) :: Prism s t -> Prism t u -> Prism s u
id  :: Prism a a
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{Prism examples}

\begin{itemize}
\item<+->[]
\begin{fakecode}
_Left  :: Prism (Either a b) a
_Right :: Prism (Either a b) b
\end{fakecode}

\item<+->[]
\begin{fakecode}
_Just    :: Prism (Maybe a) a
_Nothing :: Prism (Maybe a) ()
\end{fakecode}

\item<+->[]
\begin{fakecode}
 > preview _Left (Left (Just 4))
\end{fakecode}
{\color{blue}
\begin{fakecode}
(Just (Just 4))
\end{fakecode}
}

\item<+->[]
\begin{fakecode}
 > preview (_Left . _Just) (Left (Just 4))
\end{fakecode}
{\color{blue}
\begin{fakecode}
Just 4
\end{fakecode}
}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\begin{itemize}

\item[]
\begin{fakecode}
_Left  :: Prism (Either a b) a
_Right :: Prism (Either a b) b
\end{fakecode}

\item[]
\begin{fakecode}
_Just    :: Prism (Maybe a) a
_Nothing :: Prism (Maybe a) ()
\end{fakecode}

\item[]
\begin{fakecode}
 > preview _Right (Left (Just 4))
\end{fakecode}
{\color{blue}
\begin{fakecode}
Nothing
\end{fakecode}
}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\begin{itemize}
\item[]
\begin{fakecode}
_Just    :: Prism (Maybe a) a
_Nothing :: Prism (Maybe a) ()
\end{fakecode}

\item<+->[]
\begin{fakecode}
 > review (_Right . _Just) "hello"
\end{fakecode}
{\color{blue}
\begin{fakecode}
Right (Just "hello")
\end{fakecode}
}

\item<+->[]
\begin{fakecode}
 > review (_Just . _Left) 42
\end{fakecode}
{\color{blue}
\begin{fakecode}
Just (Left 42)
\end{fakecode}
}
\end{itemize}

\end{frame}


\begin{frame}[fragile]
\frametitle{}
\begin{center}
  {\huge Classy Optics}
\end{center}
\end{frame}


\begin{frame}
\frametitle{Classy optics?}
Idea: Associate with each type a typeclass full of optics for that type
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Lenses}

\begin{itemize}
\item<+->[]
\begin{fakecode}
data DbConfig =
  DbConfig {
    _dbConn :: DbConnection
  , _schema :: Schema
  }
\end{fakecode}

\item<+->[]
\begin{fakecode}
class HasDbConfig t where
  dbConfig :: Lens t DbConfig
  dbConn   :: Lens t DbConnection
  dbSchema :: Lens t Schema
\end{fakecode}

\item<+->[]
\begin{fakecode}
instance HasDbConfig DbConfig where
  dbConfig = id
  dbConn   =
    lens _dbConn (\d c -> d { _dbConn = c })
  dbSchema =
    lens _dbSchema (\d s -> d { _dbSchema = s})


\end{fakecode}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Lenses}

\begin{itemize}
\item[]
\begin{code}
data DbConfig =
  DbConfig {
    _dbConn :: DbConnection
  , _dbSchema :: Schema
  }
\end{code}

\item[]
\begin{code}
class HasDbConfig t where
  dbConfig :: Lens t DbConfig
  dbConn   :: Lens t DbConnection
  dbSchema :: Lens t Schema
\end{code}
{\color{green}
\begin{code}
  dbConn = dbConfig . dbConn
  dbSchema = dbConfig . dbSchema
\end{code}
}

\item[]
\begin{code}
instance HasDbConfig DbConfig where
  dbConfig = id
  dbConn   =
    lens _dbConn (\d c -> d { _dbConn = c })
  dbSchema =
    lens _dbSchema (\d s -> d { _dbSchema = s})
\end{code}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Lenses}

\begin{itemize}
\item<+->[]
\begin{fakecode}
data NetworkConfig =
  NetConfig {
    _port :: Port
  , _ssl  :: Ssl
  }
\end{fakecode}

\item<+->[]
\begin{fakecode}
class HasNetworkConfig t where
  netConfig :: Lens t NetworkConfig
  netPort   :: Lens t Port
  netSsl    :: Lens t Ssl
\end{fakecode}

\item<+->[]
\begin{fakecode}
instance HasNetworkConfig NetworkConfig where
  netConfig = id
  netPort   =
    lens _port (\n p -> n { _port = p })
  netSsl    =
    lens _ssl (\n s -> n { _ssl = s})


\end{fakecode}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Lenses}

\begin{itemize}
\item[]
\begin{code}
data NetworkConfig =
  NetConfig {
    _port :: Port
  , _ssl  :: Ssl
  }
\end{code}

\item[]
\begin{code}
class HasNetworkConfig t where
  netConfig :: Lens t NetworkConfig
  netPort   :: Lens t Port
  netSsl    :: Lens t Ssl
\end{code}
{\color{green}
\begin{code}
  netPort = netConfig . netPort
  netSsl  = netConfig . netSsl
\end{code}
}

\item[]
\begin{code}
instance HasNetworkConfig NetworkConfig where
  netConfig = id
  netPort   =
    lens _port (\n p -> n { _port = p })
  netSsl    =
    lens _ssl (\n s -> n { _ssl = s})


\end{code}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Lenses}

\begin{itemize}

\item<+->[]
\begin{code}
data AppConfig =
  AppConfig {
    appDbConfig :: DbConfig
  , appNetConfig :: NetworkConfig
  }
\end{code}

\item<+->[]
\begin{code}
instance HasDbConfig AppConfig where
  dbConfig =
    lens appDbConfig 
      (\app db -> app { appDbConfig = db })
\end{code}

\item<+->[]
\begin{code}
instance HasNetworkConfig AppConfig where
  netConfig =
    lens appNetConfig 
      (\app net -> app { appNetConfig = net })
\end{code}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Prisms}

\begin{itemize}

\item<+->[]
\begin{fakecode}
data DbError =
    QueryError Text
  | InvalidConnection
\end{fakecode}

\item<+->[]
\begin{fakecode}
class AsDbError t where
  _DbError     :: Prism t DbError
  _QueryError  :: Prism t Text
  _InvalidConn :: Prism t ()
\end{fakecode}

\item<+->[]
\begin{fakecode}
instance AsDbError DbError where
  _DbError = id
  _QueryError =
    prism QueryError $ \case QueryError t -> Just t
                             _            -> Nothing
  _InvalidConn =
    prism InvalidConnection $
      \case InvalidConnection -> Just ()
            _                 -> Nothing
\end{fakecode}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Prisms}

\begin{itemize}
\item[]
\begin{code}
data DbError =
    QueryError Text
  | InvalidConnection
\end{code}

\item[]
\begin{code}
class AsDbError t where
  _DbError     :: Prism t DbError
  _QueryError  :: Prism t Text
  _InvalidConn :: Prism t ()
\end{code}
{\color{green}
\begin{code}
  _QueryError = _DbError . _QueryError
  _InvalidConn = _DbError . _InvalidConn
\end{code}
}

\item[]
\begin{code}
instance AsDbError DbError where
  _DbError = id
  _QueryError =
    prism QueryError
      $ \case
          QueryError t -> Just t
          _            -> Nothing
  _InvalidConn =
    prism (const InvalidConnection)
      $ \case
          InvalidConnection -> Just ()
          _                 -> Nothing
\end{code}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Prisms}

\begin{itemize}
\item<+->[]
\begin{fakecode}
data NetworkError =
    Timeout Int
  | ServerOnFire
\end{fakecode}

\item<+->[]
\begin{fakecode}
class AsNetworkError t where
  _NetworkError :: Prism t NetworkError
  _Timeout      :: Prism t Int
  _ServerOnFire :: Prism t ()
\end{fakecode}

\item<+->[]
\begin{fakecode}
instance AsNetworkError NetworkError where
  _NetworkError = id
  _Timeout = prism Timeout $ \case Timeout t -> Just t
                                   _         -> Nothing
  _ServerOnFire =
    prism (const ServerOnFire)
      $ \case ServerOnFire -> Just ()
              _            -> Nothing
    
\end{fakecode}
\end{itemize}
\end{frame}


\begin{frame}[fragile]
\frametitle{Classy Prisms}

\begin{itemize}
\item[]
\begin{code}
data NetworkError =
    Timeout Int
  | ServerOnFire
\end{code}

\item[]
\begin{code}
class AsNetworkError t where
  _NetworkError :: Prism t NetworkError
  _Timeout      :: Prism t Int
  _ServerOnFire :: Prism t ()
\end{code}
{\color{green}
\begin{code}
  _Timeout = _NetworkError . _Timeout
  _ServerOnFire = _NetworkError . _ServerOnFire
\end{code}
}
\item[]
\begin{code}
instance AsNetworkError NetworkError where
  _NetworkError = id
  _Timeout = prism Timeout $ \case Timeout t -> Just t
                                   _         -> Nothing
  _ServerOnFire =
    prism (const ServerOnFire)
      $ \case ServerOnFire -> Just ()
              _            -> Nothing
    
\end{code}
\end{itemize}
\end{frame}
\begin{frame}[fragile]
\frametitle{Classy Prisms}

\begin{itemize}

\item<+->[]
\begin{code}
data AppError =
    AppDbError { dbError :: DbError }
  | AppNetError { netError :: NetworkError }
\end{code}

\item<+->[]
\begin{code}
instance AsDbError AppError where
  _DbError =
    prism AppDbError
      $ \case AppDbError dbe -> Just dbe
              _              -> Nothing
\end{code}

\item<+->[]
\begin{code}
instance AsNetworkError AppError where
  _NetworkError =
    prism AppNetError
      $ \case AppNetError ne -> Just ne
              _              -> Nothing
\end{code}
\end{itemize}

\end{frame}


\begin{frame}[fragile]
\frametitle{If that's too much typing\ldots}

\begin{fakecode}
class AsNetworkError t where
  _NetworkError :: Prism t NetworkError
  _Timeout      :: Prism t Int
  _ServerOnFire :: Prism t ()
  _Timeout = _NetworkError . _Timeout
  _ServerOnFire = _NetworkError . _ServerOnFire

instance AsNetworkError NetworkError where
  _NetworkError = id
  _Timeout = prism Timeout $ \case Timeout t -> Just t
                                   _         -> Nothing
  _ServerOnFire =
    prism (const ServerOnFire)
      $ \case ServerOnFire -> Just ()
              _            -> Nothing
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{Template Haskell!}
\begin{fakecode}
makeClassyPrisms ''NetworkError
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{If this is too much typing\ldots}
\begin{fakecode}
class HasDbConfig t where
  dbConfig :: Lens t DbConfig
  dbConn   :: Lens t DbConnection
  dbSchema :: Lens t Schema
  dbConn = dbConfig . dbConn
  dbSchema = dbConfig . dbSchema

instance HasDbConfig DbConfig where
  dbConfig = id
  dbConn   =
    lens _dbConn (\d c -> d { _dbConn = c })
  dbSchema =
    lens _dbSchema (\d s -> d { _dbSchema = s})
\end{fakecode}
\end{frame}


\begin{frame}[fragile]
\frametitle{Template Haskell again!}
\begin{fakecode}
makeClassy ''DbConfig
\end{fakecode}
\end{frame}


\section{Using Classy Optics}

\begin{frame}
\begin{center}
{\huge PUTTING IT ALL TOGETHER}
\end{center}
\end{frame}


\begin{frame}[fragile]
\frametitle{The Pay-off}
\begin{itemize}

\item<+->[]
\begin{code}
loadFromDb :: (MonadError e m, MonadReader r m,
               AsDbError e,    HasDbConfig r,
               MonadIO m)
           => m MyData
\end{code}

\item<+->[]
\begin{code}
sendOverNet :: (MonadError e m, MonadReader r m,
                AsNetworkError e, HasNetworkConfig r,
                MonadIO m)
            => MyData -> m ()
\end{code}

\item<+->[]
\begin{fakecode}
-- Finally!
loadAndSend = loadFromDb >>= sendOverNet
\end{fakecode}

\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{We've done it}
\begin{itemize}

\item<+->[]
\begin{code}
loadAndSend :: (MonadError e m, MonadReader r m,
                AsNetworkError e, HasDbConfig r,
                AsDbError e, HasNetworkConfig r,
                MonadIO m)
            => m ()
loadAndSend = loadFromDb >>= sendOverNet
\end{code}

\item<+->[]
\begin{code}
mainApp :: App ()
mainApp = loadAndSend
\end{code}

\end{itemize}
\end{frame}


\begin{frame}
\frametitle{Wrapping up\ldots}
\begin{itemize}

\item
Abstractions$~>~$Concretions

\item
Typeclass constraints stack up better than monolithic transformers

\item
Lens gives us a compositional vocabulary for talking about data

\end{itemize}
\end{frame}


\begin{frame}[fragile]

\begin{center}

\begin{itemize}
\item<+->[]
{\huge THE END}

\item<+->[]
\ldots or is it?

\end{itemize}
\end{center}
\end{frame}


\begin{frame}
\frametitle{References}

\begin{itemize}

\item
I talked about these things: \\

{\color{blue}
{\tt hackage.haskell.org/package/mtl} \\
{\tt lens.github.io}} \\

\item
I encourage you to also look at these things:
{\color{blue}
{\tt github.com/benkolera/talk-stacking-your-monads/}
{\tt hackage.haskell.org/package/hoist-error}
}

\end{itemize}
\end{frame}


