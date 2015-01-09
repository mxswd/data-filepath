{-# LANGUAGE CPP, GADTs, DataKinds, KindSignatures, StandaloneDeriving, RankNTypes, DeriveDataTypeable, FlexibleInstances, MagicHash #-}
module Data.FilePath
    (   Path(..)
    ,   From(..)
    ,   FilePath
    ,   (</>)
    ,   rootPath
    ,   relativePath
    ,   mkDirPath
    ,   mkFilePath
    ,   mkRootFilePathBase
    ,   mkFullFilePath
    ,   dirname
    ,   basename
    ,   showp
    ,   dirpathQ
    ,   filepathQ
    ) where

import Prelude hiding (FilePath)
import Data.Data
import Data.Char
import Data.List.Split
import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Types

data Path = File | Directory
data From = Root | Relative
data FilePath (a :: From) (b :: Path) where
  RootPath      :: FilePath Root Directory
  RelativePath  :: FilePath Relative Directory
  FilePath      :: FilePath a Directory -> String -> FilePath a File
  DirectoryPath :: FilePath a Directory -> String -> FilePath a Directory


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

mkDirPath :: String -> Maybe (FilePath Relative Directory)
mkDirPath s = DirectoryPath RelativePath `fmap` mkf s

mkFilePath :: String -> Maybe (FilePath Relative File)
mkFilePath s = FilePath RelativePath `fmap` mkf s

mkRootFilePathBase :: String -> Maybe (FilePath Root Directory)
mkRootFilePathBase ('/':s) = do
  ys <- xs
  return $ foldl (</>) RootPath ys
  where
    ss = splitOn "/" s
    xs = mapM mkDirPath $ filter (not . null) ss
mkRootFilePathBase _ = Nothing -- all full file paths must start from /


mkFullFilePath :: String -> Maybe (FilePath Root File)
mkFullFilePath ('/':s) = do
  y <- x
  ys <- xs
  return $ foldl (</>) RootPath ys </> y
  where
    ss = splitOn "/" s
    xs = mapM mkDirPath $ init ss
    x  = mkFilePath $ last ss
mkFullFilePath _ = Nothing -- all full file paths must start from /

dirname :: FilePath a File -> FilePath a Directory
dirname (FilePath dir _) = dir

basename :: FilePath a File -> String
basename (FilePath _ bname) = bname

showp :: FilePath a b -> String
showp RootPath = ""
showp RelativePath = "."
showp (DirectoryPath u s) = showp u ++ "/" ++ s
showp (FilePath u s) = showp u ++ "/" ++ s

mkf :: String -> Maybe String
mkf "" = Nothing -- an empty string is an invalid file/dir name
mkf s = if any (\x -> x == '/' || isControl x) s
  then Nothing
  else Just s

-- TODO: could it split the delimiters?
dirpathQ :: QuasiQuoter
dirpathQ = QuasiQuoter qExp qPat (error "dir paths are not types") (error "dir paths are not decs")
  where
    qExp :: String -> ExpQ
    qExp s = dataToExpQ (const Nothing) (DirectoryPath RelativePath (fromJust (mkf s)) :: FilePath Relative Directory)
    qPat = undefined

filepathQ :: QuasiQuoter
filepathQ = QuasiQuoter qExp qPat (error "file paths are not types") (error "file paths are not decs")
  where
    qExp :: String -> ExpQ
    qExp s = dataToExpQ (const Nothing) (FilePath RelativePath (fromJust (mkf s)) :: FilePath Relative File)
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
