{-# LANGUAGE CPP, GADTs, DataKinds, KindSignatures, StandaloneDeriving, RankNTypes, DeriveDataTypeable, FlexibleInstances, MagicHash #-}
module Data.FilePath(
    -- * Types
        Path(..)
    ,   From(..)
    ,   PathSegment
    ,   FilePath
    ,   FilePathParseError
    ,   WeakFilePath
    -- * Functions
    ,   filePathParseError
    ,   weakFilePath
    ,   rootFromWeak
    ,   relativeFromWeak
    ,   segString
    ,   mkPathSegment
    ,   (</>)
    ,   rootPath
    ,   relativePath
    ,   parseDirectory
    ,   parseFilePath
    ,   mkDirPath
    ,   mkDirPathSeg
    ,   mkFilePath
    ,   mkFilePathSeg
    ,   dirname
    ,   basename
    ,   basenameSeg
    ,   showp
    -- * Quasi Quoters
    ,   segQ
    ,   dirpathQ
    ,   filepathQ
    ) where

import Prelude hiding ( FilePath, init, last )
import Control.Applicative ( pure )
import Data.Bifunctor ( first )
import Data.Char
import Data.Data
import Data.Either ( partitionEithers )
import Data.Functor ( (<$>) )
import Data.List ( intercalate )
import Data.List.Split ( splitOn, wordsBy )
import Data.List.NonEmpty ( NonEmpty(..), init, last, nonEmpty )
import Data.Maybe ( fromJust )
import Data.Semigroup ( Semigroup(..) )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Types

-- | A PathSegment is any single element of a path...i.e. the stuff between
-- two \'\/\' characters.  Valid path segments cannot contain \'\/\' or control
-- characters.  PathSegments are also semigroups to allow concatenating with
-- prefixes/suffixes.
newtype PathSegment = PathSegment { _segString :: String }
  deriving (Eq,Show,Typeable,Data)

instance Semigroup PathSegment where
  (PathSegment a) <> (PathSegment b) = PathSegment $ a <> b

-- For the motivation behind taking the time and characters to write `segString` instead of just exporting `_segString` pls see <https://github.com/maxpow4h/data-filepath/pull/6>

-- |
-- Every `PathSegment` is a valid string
--
segString :: PathSegment -> String
segString = _segString

-- | Smart constructor for valid PathSegments.  Valid path segments cannot
-- contain front slashes or control characters.
-- This function performs __all__ the checks up front.
--
-- * Is the string non-empty?
-- * Does the string contain forward slashes or control characters?
--
mkPathSegment :: String -> Maybe PathSegment
mkPathSegment s
    | any (\x -> x == '/' || isControl x) s = Nothing
    | null s                                = Nothing
    | otherwise                             = Just $ PathSegment s

-- |
-- If the string is a valid path segment, returns the string otherwise
-- returns the original string unchanged
eitherPathSegment :: String -> Either String PathSegment
eitherPathSegment s = maybe (Left s) pure $ mkPathSegment s

data Path = File | Directory
data From = Root | Relative
data FilePath (a :: From) (b :: Path) where
  RootPath      :: FilePath Root Directory
  RelativePath  :: FilePath Relative Directory
  FilePath      :: FilePath a Directory -> PathSegment -> FilePath a File
  DirectoryPath :: FilePath a Directory -> PathSegment -> FilePath a Directory

instance Eq (FilePath a b) where
    RootPath == RootPath = True
    RelativePath == RelativePath = True
    FilePath a b == FilePath c d = a == c && b == d
    DirectoryPath a b == DirectoryPath c d = a == c && b == d
    _ == _ = False

data FilePathParseError = InvalidPathSegments String (NonEmpty String) deriving (Show, Eq)

filePathParseError
    :: (String -> NonEmpty String -> a)   -- ^ A function to handle the case where the some path segments contained invalid characters
    -> FilePathParseError
    -> a
filePathParseError badSegments e = case e of
    InvalidPathSegments s ps  -> badSegments s ps

-- |
-- For cases where you dont know during compile time what kind of path you will get, for instance when you are parsing a path from the command line
--
data WeakFilePath (a :: Path) =
        WeakRoot (FilePath Root a)
    |   WeakRelative (FilePath Relative a)
        deriving (Show, Eq)

weakFilePath
    :: (FilePath Root a -> b)       -- ^ The handler for when a Root filepath was found
    -> (FilePath Relative a -> b)   -- ^ The handler for when a Relative filepath was found
    -> WeakFilePath a
    -> b
weakFilePath root relative p = case p of
    WeakRoot p'     -> root p'
    WeakRelative p' -> relative p'

rootFromWeak :: WeakFilePath a -> Maybe (FilePath Root a)
rootFromWeak = weakFilePath pure (const Nothing)

relativeFromWeak :: WeakFilePath a -> Maybe (FilePath Relative a)
relativeFromWeak = weakFilePath (const Nothing) pure

-- Path API
rootPath :: FilePath Root Directory
rootPath = RootPath

relativePath :: FilePath Relative Directory
relativePath = RelativePath

infixr 5 </>
(</>) :: FilePath a Directory -> FilePath Relative b -> FilePath a b
p </> RelativePath = p
p </> (DirectoryPath u s) = DirectoryPath (p </> u) s
p </> (FilePath u s) = FilePath (p </> u) s


-- |
-- Takes in a string that is intended to represent a relative path or a root path down the filesystem tree
-- splits on '/'
--
parseDirectory :: String -> Either FilePathParseError (WeakFilePath Directory)
parseDirectory ('/':s) =
    let
        errorHandler :: FilePathParseError -> FilePathParseError
        errorHandler (InvalidPathSegments s' ps) = InvalidPathSegments ('/':s') ps
    in first errorHandler (parseDirectory s) >>= weakFilePath
        (pure . WeakRoot)
        (pure . WeakRoot . (rootPath </>))
parseDirectory "" = pure $ WeakRelative relativePath
parseDirectory s =
    let
        (badSegments, segments) = partitionEithers $ eitherPathSegment <$> wordsBy (== '/') s
        relative :: FilePath Relative Directory
        relative = foldl (\p s' -> p </> mkDirPathSeg s') relativePath segments
    in maybe (pure $ WeakRelative relative) (Left . InvalidPathSegments s) $ nonEmpty badSegments

-- |
-- Takes in a string that is intended to represent a relative path or a root path down the filesystem tree
-- splits on '/'
--
parseFilePath :: String -> Either FilePathParseError (WeakFilePath File)
parseFilePath s =
    let
        dirErrorHandler :: String -> FilePathParseError -> FilePathParseError
        dirErrorHandler fn (InvalidPathSegments _ ps) = maybe (InvalidPathSegments s $ ps <> pure fn) (const $ InvalidPathSegments s ps) $ mkPathSegment fn
    in do
        let ps' = splitOn "/" s
        ps <- maybe (Left $ InvalidPathSegments s $ pure "") pure $ nonEmpty ps'-- This can't actually be empty but the type of `splitOn` is stupid
        dirResult <- first (dirErrorHandler $ last ps) $ parseDirectory $ intercalate "/" $ init ps
        fnRes <- maybe (Left $ InvalidPathSegments s $ pure $ last ps) pure $ fmap mkFilePathSeg $ mkPathSegment $ last ps
        return $ weakFilePath (WeakRoot . (</> fnRes)) (WeakRelative . (</> fnRes)) dirResult

{-# DEPRECATED mkDirPath "Please use `fmap mkDirPathSeg . mkPathSegment` instead" #-}
-- | Smart constructor for directories.  Valid directories must be valid
-- PathSegments and also cannot be empty strings.
--
mkDirPath :: String -> Maybe (FilePath Relative Directory)
mkDirPath = fmap mkDirPathSeg . mkPathSegment

-- | This function basically defines what the `PathSegment` type is semantically.
-- It is string like thing which you can safely create a path from.
-- See `mkPathSegment`, `mkFilePathSeg`
--
mkDirPathSeg :: PathSegment -> FilePath Relative Directory
mkDirPathSeg = DirectoryPath relativePath

{-# DEPRECATED mkFilePath "Please use `fmap mkFilePathSeg . mkPathSegment` instead" #-}
-- | Smart constructor for files.  Valid files must be valid PathSegments and
-- also cannot be empty strings.
mkFilePath :: String -> Maybe (FilePath Relative File)
mkFilePath = fmap mkFilePathSeg . mkPathSegment

mkFilePathSeg :: PathSegment -> FilePath Relative File
mkFilePathSeg = FilePath relativePath

dirname :: FilePath a File -> FilePath a Directory
dirname (FilePath dir _) = dir

basename :: FilePath a File -> String
basename (FilePath _ (PathSegment bname)) = bname

basenameSeg :: FilePath a File -> PathSegment
basenameSeg (FilePath _ bname) = bname

segments :: FilePath a b -> [PathSegment]
segments =
    let
        segments' :: FilePath a b -> [PathSegment]
        segments' RootPath = []
        segments' RelativePath = []
        segments' (DirectoryPath u seg) = seg : segments' u
    in reverse . segments'

showp :: FilePath a b -> String
showp RootPath = "/"
showp p = showPath p
  where
    showPath :: FilePath a b -> String
    showPath RootPath = ""
    showPath RelativePath = "."
    showPath (DirectoryPath u s) = showPath u ++ "/" ++ s
    showPath (FilePath u s)      = showPath u ++ "/" ++ s

-- TODO: could it split the delimiters?
segQ :: QuasiQuoter
segQ = QuasiQuoter qExp qPat (error "path segments are not types") (error "path segments are not decs")
  where
    qExp :: String -> ExpQ
    qExp s = dataToExpQ (const Nothing) (fromJust (mkPathSegment s) :: PathSegment)
    qPat = undefined

-- TODO: could it split the delimiters?
dirpathQ :: QuasiQuoter
dirpathQ = QuasiQuoter qExp qPat (error "dir paths are not types") (error "dir paths are not decs")
  where
    qExp :: String -> ExpQ
    qExp s = dataToExpQ (const Nothing) (fromJust (mkDirPath s) :: FilePath Relative Directory)
    qPat = undefined

filepathQ :: QuasiQuoter
filepathQ = QuasiQuoter qExp qPat (error "file paths are not types") (error "file paths are not decs")
  where
    qExp :: String -> ExpQ
    qExp s = dataToExpQ (const Nothing) (fromJust (mkFilePath s) :: FilePath Relative File)
    qPat = undefined

-- data / typeable
-- deriving instance Show (FilePath a b)
instance Show (FilePath a b) where
  show = showp

instance Data
           (FilePath
              Relative Directory) where
  gfoldl _k_aFi z_aFj RelativePath
    = z_aFj RelativePath
  gfoldl
    k_aFo
    z_aFp
    (DirectoryPath a1_aFq a2_aFr)
    = (z_aFp DirectoryPath `k_aFo` a1_aFq)
       `k_aFo` a2_aFr
  gunfold k_aFs z_aFt c_aFu
    = case constrIndex c_aFu of
        GHC.Types.I# 2# -> z_aFt RelativePath
        GHC.Types.I# 4# -> k_aFs (k_aFs (z_aFt DirectoryPath))
        _ -> error "impossible"
  toConstr RelativePath
    = cRelativePath
  toConstr (DirectoryPath _ _)
    = cDirectoryPath
  dataTypeOf _ = tFilePath

instance Data
           (FilePath
              Root Directory) where
  gfoldl _k_aFv z_aFw RootPath
    = z_aFw RootPath
  gfoldl
    k_aFD
    z_aFE
    (DirectoryPath a1_aFF a2_aFG)
    = (z_aFE DirectoryPath `k_aFD` a1_aFF)
       `k_aFD` a2_aFG
  gunfold k_aFH z_aFI c_aFJ
    = case constrIndex c_aFJ of
        GHC.Types.I# 1# -> z_aFI RootPath
        GHC.Types.I# 4# -> k_aFH (k_aFH (z_aFI DirectoryPath))
        _ -> error "impossible"
  toConstr RootPath
    = cRootPath
  toConstr (DirectoryPath _ _)
    = cDirectoryPath
  dataTypeOf _ = tFilePath

instance Data
           (FilePath
              Relative File) where
  gfoldl k_aFO z_aFP (FilePath a1_aFQ a2_aFR)
    = (z_aFP FilePath `k_aFO` a1_aFQ) `k_aFO` a2_aFR
  gunfold k_aFW z_aFX c_aFY
    = case constrIndex c_aFY of
        GHC.Types.I# 3# -> k_aFW (k_aFW (z_aFX FilePath))
        _ -> error "impossible"
  toConstr (FilePath _ _)
    = cFilePath
  dataTypeOf _ = tFilePath

instance Data
           (FilePath Root File) where
  gfoldl k_aG3 z_aG4 (FilePath a1_aG5 a2_aG6)
    = (z_aG4 FilePath `k_aG3` a1_aG5) `k_aG3` a2_aG6
  gunfold k_aGb z_aGc c_aGd
    = case constrIndex c_aGd of
        GHC.Types.I# 3# -> k_aGb (k_aGb (z_aGc FilePath))
        _ -> error "impossible"
  toConstr (FilePath _ _)
    = cFilePath
  dataTypeOf _ = tFilePath

tFilePath :: DataType
cRootPath :: Constr
cRelativePath :: Constr
cFilePath :: Constr
cDirectoryPath :: Constr
tFilePath
  = mkDataType
      "FilePath"
      [cRootPath, cRelativePath,
       cFilePath, cDirectoryPath]
cRootPath
  = mkConstr
      tFilePath "RootPath" [] Prefix
cRelativePath
  = mkConstr
      tFilePath "RelativePath" [] Prefix
cFilePath
  = mkConstr
      tFilePath "FilePath" [] Prefix
cDirectoryPath
  = mkConstr
      tFilePath "DirectoryPath" [] Prefix

#if __GLASGOW_HASKELL__ == 706
{-# NOINLINE fTyCon #-}
fTyCon :: TyCon
fTyCon = mkTyCon3 "main" "Data.FilePath" "FilePath"

instance Typeable (FilePath Relative Directory) where
  typeOf _ = mkTyConApp fTyCon []

instance Typeable (FilePath Root Directory) where
  typeOf _ = mkTyConApp fTyCon []

instance Typeable (FilePath Relative File) where
  typeOf _ = mkTyConApp fTyCon []

instance Typeable (FilePath Root File) where
  typeOf _ = mkTyConApp fTyCon []
#endif
#if __GLASGOW_HASKELL__ == 708 || __GLASGOW_HASKELL__ == 710
deriving instance Typeable Directory
deriving instance Typeable Relative
deriving instance Typeable Root
deriving instance Typeable File
deriving instance Typeable FilePath
#endif

