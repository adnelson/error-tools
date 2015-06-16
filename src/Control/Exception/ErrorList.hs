{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Exception.ErrorList (
    ErrorList(..), EList(..),
    throwError1, throwErrorC, addError1, addErrorC,
    assert, assertC, wrapJust, withHandler, ifErrorDo, ifErrorReturn,
    showError, showError', errorC, oneErrorC, firstSuccess,
    inContext
  ) where

import qualified Prelude as P
import Prelude (($), (.), map, Int, Double, Functor(..), Show(..),
                Eq(..), Bool(..), Integer, Double, Monad(..), flip,
                Maybe(..), Either(..), IO(..))
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Except (ExceptT, MonadError(..), throwError, runExceptT)
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified GHC.Exts as Exts
import Text.Render

class Exts.IsList elist => ErrorList elist where
  addError :: Text -> elist -> elist
  oneError :: Text -> elist

data EList = EList Text [Text] deriving (Show, Eq)

instance Exts.IsList EList where
  type Item EList = Text
  fromList (e:es) = EList e es
  fromList _ = P.error "No main message in error list"
  toList (EList msg msgs) = msg:msgs

instance ErrorList EList where
  addError e (EList e' es) = EList e (e':es)
  oneError e = EList e []

--instance Monoid ErrorList where
--  mempty = ErrorList mempty
--  ErrorList el1 `mappend` ErrorList el2 = ErrorList (el1 <> el2)

instance Render EList where
  render (EList m msgs) = T.unlines $ "Error:" : map (sp <>) (m:msgs)
    where sp = T.replicate 2 " "

-- | Throws a single-message eror list.
throwError1 :: (ErrorList e, MonadError e m) => Text -> m a
throwError1 = throwError . oneError

-- | Concatenates a list of strings and throws them as an error.
throwErrorC :: (ErrorList e, MonadError e m) => [Text] -> m a
throwErrorC = throwError1 . mconcat

-- | Throws a new error with the given string added on.
addError1 :: (ErrorList e, MonadError e m) => Text -> e -> m a
addError1 msg = throwError . addError msg

-- | Throws a new error with the concatenation of the argument added on.
addErrorC :: (ErrorList e, MonadError e m) => [Text] -> e -> m a
addErrorC list = addError1 (mconcat list)

-- | Useful when the handler is more concise than the action.
withHandler :: MonadError e m => (e -> m a) -> m a -> m a
withHandler = flip catchError

-- | Tries something and throws an error if it fails.
inContext :: (ErrorList e, MonadError e m) => Text -> m a -> m a
inContext ctx action = action `catchError` addError1 ctx

-- | Wraps a successful result in a `Just` and a failure in a `Nothing`.
wrapJust :: MonadError e m => m a -> m (Maybe a)
wrapJust action = liftM Just action `ifErrorReturn` Nothing

-- | Performs an action, returning the second argument if it fails.
ifErrorReturn :: MonadError e m => m a -> a -> m a
ifErrorReturn action a = action `ifErrorDo` return a

-- | Specifies what to do if the given action fails.
ifErrorDo :: MonadError e m => m a -> m a -> m a
ifErrorDo action action' = action `catchError` \_ -> action'

-- | If the test is false, throws an error with the given message.
assert :: (ErrorList e, MonadError e m) => Bool -> Text -> m ()
assert True _ = return ()
assert False msg = throwError1 msg

-- | Same as `assert`, but concatenates a text list.
assertC :: (ErrorList e, MonadError e m) => Bool -> [Text] -> m ()
assertC test = assert test . mconcat

-- | Pretty-prints errors that use `Either`.
showError :: (Render e, Render b) => (a -> Either e b) -> a -> IO ()
showError func arg = case func arg of
  Left e -> P.putStrLn $ unpack $ render e
  Right x -> P.putStrLn $ unpack $ render x

-- | Pretty-prints errors that use `Either`, appearing in tuples.
showError' :: (Render e, Render b) => (a -> (Either e b, c)) -> a -> IO ()
showError' func arg = case func arg of
  (Left e, _) -> P.putStrLn $ unpack $ render e
  (Right x, _) -> P.putStrLn $ unpack $ render x

-- | Concatenates a list of `Text` and makes an error out of it.
errorC :: [Text] -> a
errorC = P.error . unpack . mconcat

firstSuccess :: (ErrorList e, MonadError e m) => Text -> [m a] -> m a
firstSuccess msg [] = throwError1 msg
firstSuccess msg (a:as) = a `ifErrorDo` firstSuccess msg as

-- | Pretty-prints an error list. Considers the first item of the list to
-- be the "main" message; subsequent messages are listed after.
--printErrors :: ErrorList e => e -> IO ()
--printErrors (ErrorList err errs) = do
--  P.putStrLn ("Error: " <> unpack err)
--  P.mapM_ (P.putStrLn . unpack . ("  " <>)) errs

-- | Same as `oneError` but concatenates its argument.
oneErrorC :: ErrorList e => [Text] -> e
oneErrorC = oneError . mconcat
