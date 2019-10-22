module Swihs.Query where

import Control.Monad (void)
import Data.Map (Map)
import Swihs.Builder
import Swihs.C hiding (cutQuery, nextSolution, openQuery)
import qualified Swihs.C as C
import Swihs.Term
import Swihs.Types

queryBool :: Term -> IO Bool
queryBool b = do
  fid <- openForeignFrame
  QueryTerm p ref _ <- buildQueryTerm b
  q <- C.openQuery noModule p ref
  r <- C.nextSolution q
  _ <- C.cutQuery q
  closeForeignFrame fid
  pure r

queryOnce :: Term -> IO (Maybe (Map String (Ground Term)))
queryOnce b = do
  fid <- openForeignFrame
  q <- openQuery b
  t <- nextSolution q
  cutQuery q
  closeForeignFrame fid
  pure t

queryList :: Term -> IO [(Map String (Ground Term))]
queryList b = do
  fid <- openForeignFrame
  q <- openQuery b
  t <- solutionList q
  cutQuery q
  closeForeignFrame fid
  pure t

openQuery :: Term -> IO Query
openQuery b = do
  QueryTerm p ref varMap <- buildQueryTerm b
  q <- C.openQuery noModule p ref
  pure $ Query q varMap

cutQuery :: Query -> IO ()
cutQuery (Query q _) = void $ C.cutQuery q

closeQuery :: Query -> IO ()
closeQuery (Query q _) = void $ C.closeQuery q

nextSolution :: Query -> IO (Maybe (Map String (Ground Term)))
nextSolution (Query q varMap) = do
  sol <- C.nextSolution q
  if sol
    then Just <$> traverse getTerm varMap
    else pure Nothing

solutionList :: Query -> IO [(Map String (Ground Term))]
solutionList = unfoldM . nextSolution

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM p = go
  where
    go = do
      res <- p
      case res of
        Nothing -> pure []
        Just x -> do
          xs <- go
          pure (x : xs)
