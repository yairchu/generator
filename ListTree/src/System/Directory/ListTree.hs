-- | Monadic directory tree.
--
-- > -- List of files under "." or subfolders with ".markdown" extension,
-- > -- except for those with a name starting with "_" somewhere in their path. (like "_cache/index.markdown")
-- > markdownFiles :: ListT IO FilePath
-- > markdownFiles
-- >     = filter ((== ".markdown") . takeExtension) -- only take files with a ".markdown" extension
-- >     . lastL                                     -- get the leaves of the tree (files, not directories)
-- >     . scanl1 appendPath                         -- transform tree nodes to filenames including path
-- >     . prune (not . isPrefixOf "_")              -- ignore directories or files whose name starts with "_"
-- >     $ directoryTree "."                         -- directory tree starting at "."
--
-- Module name System.Directory.Tree is a better fit but it is taken by "directory-tree", a read-directory-tree-in-bulk module.
module System.Directory.ListTree
    ( directoryTree
    , appendPath
    ) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.List.Class (cons, fromList)
import Data.List.Tree (prune)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (joinPath)

directoryTree :: MonadIO m => FilePath -> ListT (ListT m) FilePath
directoryTree rootDir =
    prune (`notElem` [".", ".."]) $ do
        dirContentsList <- liftIO $ getDirectoryContents rootDir
        pathName <- lift $ fromList dirContentsList
        cons pathName $ do
            isDir <- liftIO . doesDirectoryExist $ joinPath [rootDir, pathName]
            guard isDir
            directoryTree $ joinPath [rootDir, pathName]

-- | When used with @scanl@ or @scanl1@, transforms tree of filenames to tree of filenames with the paths
appendPath :: FilePath -> FilePath -> FilePath
appendPath x y = joinPath [x, y]
