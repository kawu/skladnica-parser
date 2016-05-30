{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}


-- | Parsing the Skladnica constituency treebank.


module NLP.Skladnica
(
-- * Types
  NID
, IsHead
, Node (..)
, NonTerm (..)
, Term (..)

-- * Parsing
, parseTop
, readTop

-- * Conversion
, DAG
, mkDAG
, Tree (..)
, forest
, simplify
, printChosen

-- * Extraction
, printExtracted
, printShattered
, walkAndRead
) where


import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import qualified Control.Arrow              as Arr
import           Control.Monad              (forM, forM_, guard)
import qualified Control.Monad.State.Strict as E
-- import qualified Control.Exception as Exc

import qualified Data.Foldable              as F
import           Data.List                  (isSuffixOf)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (isJust, maybeToList)
import qualified Data.Set                   as S
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L
import qualified Data.Tree                  as R
import           System.Directory.PathWalk  (pathWalkLazy)
import           System.FilePath            (joinPath)

import qualified Text.HTML.TagSoup          as TagSoup
import           Text.XML.PolySoup          hiding (P, Q)
import qualified Text.XML.PolySoup          as PolySoup

import qualified NLP.Partage.Tree.Other     as O


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


-- -- | The analysed text.
-- type Text = L.Text


-- | Node ID.
type NID = Int


-- | Is it head or not?
data IsHead = HeadYes | HeadNo
    deriving (Show, Eq, Ord)


-- | A label stored in a tree node.
type Label = Either NonTerm Term


-- | A node of the parsed forest.
data Node = Node
    { nid      :: NID
    -- ^ ID of the node.
    , chosen   :: Bool
    , label    :: Label
    , children :: [[(NID, IsHead)]]
    -- ^ Note potential non-determinism!
    } deriving (Show, Eq, Ord)


-- | Non-terminal
data NonTerm = NonTerm
  { cat   :: L.Text
  -- ^ Category
  , morph :: M.Map L.Text L.Text
  } deriving (Show, Eq, Ord)


-- | Terminal
data Term = Term
  { orth :: L.Text
  , base :: L.Text
  , tag  :: L.Text
  } deriving (Show, Eq, Ord)


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Top-level parser
-- topP :: P (Text, [Node])
topP :: P [Node]
topP = concat <$> every' nodesQ


-- | Text (input sentence) extractor.
--  :: Q Text
--  = named "text" `joinR` first (node text)


-- | Nodes parser
nodesQ :: Q [Node]
-- nodesQ = concat <$> (true //> nodeQ)
nodesQ = true //> nodeQ


-- | Node parser
nodeQ :: Q Node
nodeQ = (named "node" *> ((,) <$> nidP <*> attr "chosen")) `join`
    \(theNid, chosenText) -> do
        theLabel <- first labelQ
        childrenAlt <- every' childrenQ
        return $ Node
            { nid = theNid
            , chosen = chosenText == "true"
            , label = theLabel
            , children = childrenAlt }


labelQ :: Q Label
labelQ = (Left <$> nonTermQ) <|> (Right <$> termQ)


-- nonTermQ :: Q NonTerm
-- nonTermQ = named "nonterminal" `joinR` first
--     (named "category" `joinR` first
--         (node text))


nonTermQ :: Q NonTerm
nonTermQ = named "nonterminal" `joinR` do
  cat_ <- first $ named "category" `joinR` first (node text)
  mor_ <- every' $ (named "f" *> attr "type") `join`
    \typ -> ((typ,) <$> first (node text))
  return $ NonTerm
    { cat = cat_
    , morph = M.fromList mor_ }


termQ :: Q Term
termQ = named "terminal" `joinR` do
  orth_ <- first $ named "orth" `joinR` first (node text)
  base_ <- first $ named "base" `joinR` first (node text)
  tag_  <- first $ (named "f" *> hasAttrVal "type" "tag") `joinR` first (node text)
  return $ Term
    { orth = orth_
    , base = base_
    , tag = tag_ }


childrenQ :: Q [(NID, IsHead)]
childrenQ = named "children" `joinR` every' childQ


childQ :: Q (NID, IsHead)
childQ = node $ named "child" *> ((,) <$> nidP <*> isHeadP)


nidP :: PolySoup.Q (TagSoup.Tag L.Text) NID
nidP = read . L.unpack <$> attr "nid"


isHeadP :: PolySoup.Q (TagSoup.Tag L.Text) IsHead
isHeadP =
    let f x = if x == "true" then HeadYes else HeadNo
    in  f <$> attr "head"


-- | Parse an XML string (in the Skladnica format) into a sequence of `Node`s.
parseTop :: L.Text -> [Node]
parseTop =
    F.concat . evalP topP . parseForest . TagSoup.parseTags


-- | Read a Skladnica XML file and parser it into a sequence of `Node`s.
readTop :: FilePath -> IO [Node]
readTop path = parseTop <$> L.readFile path


-------------------------------------------------
-- Tree
-------------------------------------------------


-- | A rose tree with labels assigned to nodes and edges.
data Tree a b = Tree
  { rootLabel :: a
  , subForest :: [(Tree a b, b)] }
  deriving (Show, Eq, Ord)


-- | Map a function over labels attached to tree nodes.
mapFst :: (a -> c) -> Tree a b -> Tree c b
mapFst f t = Tree
  { rootLabel = f (rootLabel t)
  , subForest = map (Arr.first $ mapFst f) (subForest t) }


-- | Simplify a tree to a regular rose tree (i.e., from the containers package).
simplify :: Tree a b -> R.Tree a
simplify Tree{..} = R.Node rootLabel $ map (simplify . fst) subForest


-- | Purge the nodes which satisfy the predicate.
purge :: (a -> Bool) -> R.Tree a -> R.Forest a
purge p t =
  let x = R.rootLabel t
      ts = concatMap (purge p) (R.subForest t)
  in case p x of
    True  -> ts
    False -> [R.Node x ts]


-------------------------------------------------
-- Conversion
-------------------------------------------------

-- | A full parse forest stored in a form of a directed acyclic graph (DAG)
type DAG = M.Map NID Node


-- | Construct DAG from the list of nodes.
mkDAG :: [Node] -> DAG
mkDAG = M.fromList . map (nid &&& id)


-- | Extract the forest of trees encoded in the given DAG.
forest
  :: (Node -> Bool) -- ^ Nodes which do not satisfy the predicate will be ignored
  -> NID            -- ^ Root node
  -> DAG            -- ^ The DAG
  -> [Tree Node IsHead]
forest nodePred rootID dag =
  go rootID
  where
    go i = do
      n@Node{..} <- maybeToList $ M.lookup i dag
      guard $ nodePred n
      if null children then do
        return $ Tree n []
      else do
        -- take one of the children alternatives
        childrenD <- children
        -- for the given children list, determine the
        -- corresponding forests
        sub <- mapM (go . fst) childrenD
        -- the result
        return . Tree n
          $ zip sub
          $ map snd childrenD


-- | Print the chosen (simplified) tree represented in the DAG.
printChosen :: DAG -> IO ()
printChosen dag =
  let t0 = simplify $ forest chosen 0 dag !! 0
      simpLab = L.unpack . either cat orth . label
  in  putStrLn . R.drawTree . fmap simpLab $ t0


-- -- | A version of `Node` adapted to trees (i.e., it doesn't contain information
-- -- already available in the structure of the tree).
-- data TreeNode = TreeNode
--   { nid      :: NID
--   -- ^ ID of the node.
--   , chosen   :: Bool
--   , label    :: Label
--   , children :: [[(NID, IsHead)]]
--   -- ^ Note potential non-determinism!
--   } deriving (Show, Eq, Ord)



-------------------------------------------------
-- Grammar extraction -- determining the status
-------------------------------------------------


-- | A status of the node w.r.t. to its parent.
data Status
  = Trunk -- ^ The node is a part of the trunk (and should be thus
          -- connected to its parent)
  | Modif -- ^ Optional modifier of its parent (should be thus
          -- modeled by adjunction)
  | Arg   -- ^ Obligatory argument (open slot) of its parent
          -- (to be modeled through substitution)
  deriving (Show, Eq, Ord)



-- | A function which decides, based on the labels of the path
-- from the given node up to the root, and the corresponding
-- `IsHead` markers, what is the `Status` of the current node.
status
  :: [(Label, IsHead)] -- ^ Labels on the path from the node to the root
  -> Status
status xs = case xs of
  [] -> Trunk -- not really important what we return in this case
  (curLab, curHead) : (parLab, parHead) : _
    | parLab `is` "fw" &&
      hasAttrIn parLab "tfw"
        ["sentp(że)"]    -> Trunk
    | parLab `is` "fw"   -> Arg
    | parLab `is` "fl"   -> Modif
    | curHead == HeadYes -> Trunk
    | curLab `is` "fno" &&
      parLab `is` "fpm"  -> Arg
    | curLab `is` "zdanie" &&
      parLab `is` "fzd"  -> Arg
    | otherwise          -> Modif
  (curLab, curHead) : []
    | curHead == HeadYes -> Trunk
    | otherwise          -> Modif
  _ -> error $ "unhandled case in `status`: " ++ show xs
  where
    is (Left NonTerm{..}) x = x == cat
    is _ _ = False
    hasAttrIn (Left NonTerm{..}) attr vals = isJust $ do
      val <- M.lookup attr morph
      guard $ val `elem` vals
    hasAttrIn _ _ _ = False


-- | Determine the status of internal tree nodes.
statRoot
  :: Tree Label IsHead           -- ^ Input tree
  -> Tree (Label, Status) IsHead -- ^ Status-labeled tree
statRoot = statTree []


-- | Determine the status of the internal tree nodes.
statTree
  :: [(Label, IsHead)]           -- ^ Trace from the current node to the root
  -> Tree Label IsHead           -- ^ Current node (tree)
  -> Tree (Label, Status) IsHead -- ^ Status-labeled current node (tree)
statTree trace t
   | null (subForest t) = Tree
       -- whatever the leaf, it must be in a trunk
       { rootLabel = (rootLabel t, Trunk)
       , subForest = [] }
   | otherwise =
       let (subTrees0, areHeads0) = unzip (subForest t)
           areHeads =
             -- WARNING! IMPORTANT: a backup strategy which
             -- handles the case when none of the children
             -- is marked as the head.
             if HeadYes `elem` areHeads0
               then areHeads0
               -- the first child is arbitrarily chosen as the head
               else HeadYes : repeat HeadNo
           subTrees = flip map (zip subTrees0 areHeads) $
             \(subTree, isHead) ->
               statTree
                 ((rootLabel subTree, isHead) : trace)
                 subTree
        in Tree { rootLabel =
                    ( rootLabel t
                    , status trace )
                , subForest = zip subTrees areHeads }



-- | Prepare the tree for grammar extraction:
--
--   * Use `statRoot` to obtain the status of nodes
--   * `purge` redundant (e.g., "fw", "fl" and "ff") nodes
--
prepTree
  :: Tree Label IsHead
  -> R.Tree (Label, Status)
prepTree
  = check
  . purge (useless . fst)
  . simplify
  . statRoot
  where
    check [x] = x
    check xs  = error $ "prepTree: purge left " ++ show xs
    useless (Left NonTerm{..}) =
      cat `elem` ["fw", "fl", "ff"]
    useless _ = False


-- | Neighboring label.
data Neighbor x
  = Parent x -- ^ The closest neighbor is the parent
  | Sister x -- ^ The closest neighbor is the sister (or the node itself)
  | None     -- ^ No neighbor
  deriving (Show, Eq, Ord)


-- | For a given list of labels, determine the left-neighbouring
-- trunk non-terminals for the individual input elements.
onLeft
  :: Neighbor NonTerm  -- ^ Last trunk non-terminal on the left
  -> [(Label, Status)] -- ^ The list of labels and their statuses
  -> [Neighbor NonTerm]
onLeft _ [] = []
onLeft prev ((curr, stat) : rest) =
  case (curr, stat) of
    (Left x, Trunk) -> Sister x : onLeft (Sister x) rest
    (_, Modif)      -> prev : onLeft prev rest
    _               -> None : onLeft None rest


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


-- | Again, a standalone Ord instance for rose trees...
deriving instance Ord a => Ord (R.Tree a)


-- | A TAG elementary tree.
type ET = O.Tree L.Text L.Text


-- | Store the ET in the underlying set.
store :: ET -> E.State (S.Set ET) ()
store = E.modify' . S.insert


-- | Top-level `shatter`
topShatter :: R.Tree (Label, Status) -> S.Set ET
topShatter =
  let doit x = shatter x >>= store
  in  flip E.execState S.empty . doit


-- | Shatter a given parsed tree into the set of the component
-- elementary trees.
shatter :: R.Tree (Label, Status) -> E.State (S.Set ET) ET
shatter r
  | null (R.subForest r) = do
      let x = getTerm' r
      return $ R.Node (O.Term x) []
  | otherwise = do
      let rootNT = labelNT . fst $ R.rootLabel r
          childLabs = map R.rootLabel $ R.subForest r
          left = onLeft (Parent rootNT) childLabs
      children' <- fst <$> go rootNT left (R.subForest r)
      return $ R.Node (O.NonTerm $ cat rootNT) children'
  where
    go rootNT [] [] = return ([], Parent rootNT)
    go rootNT (left:lefts) (child:children) = do
      let (childLab, childStat) = R.rootLabel child
      (children', right) <- go rootNT lefts children
      child' <- shatter child
      case (childLab, childStat) of
        (Left x, Trunk)  -> return (child':children', Sister x)
        (Right _, Trunk) -> return (child':children', None)
        (Left x, Arg) -> do
          store child'
          let childNT = O.NonTerm $ cat x
          return (R.Node childNT [] : children', None)
        (Right _, Arg) -> error "shatter.go: obligatory terminal argument!?"
        (_, Modif) -> case (left, right) of
          (Parent x, _) -> (children', right) <$ store (child' `leftMod` x)
          (_, Parent y) -> (children', right) <$ store (child' `rightMod` y)
          (Sister x, _) -> (children', right) <$ store (child' `rightMod` x)
          (_, Sister y) -> (children', right) <$ store (child' `leftMod` y)
          (None, None) -> do
            store (child' `leftMod` rootNT)
            let newChild = R.Node (O.NonTerm $ cat rootNT) children'
            return ([newChild], Sister rootNT)
    go _ _ _ = error "shatter.go: different lengths of the input lists"


-- | Construct a left modifier from a given (initial,
-- but this is not checked!) ET.
leftMod :: ET -> NonTerm -> ET
leftMod t nonTerm = R.Node (O.NonTerm x)
  [ t
  , R.Node (O.Foot x) [] ]
  where x = cat nonTerm


-- | Construct a right modifier from a given (initial,
-- but this is not checked!) ET.
rightMod :: ET -> NonTerm -> ET
rightMod t nonTerm = R.Node (O.NonTerm x)
  [ R.Node (O.Foot x) []
  , t ]
  where x = cat nonTerm


-- -- | Top-level `shatter`
-- topShatter :: Tree Node IsHead -> S.Set ET
-- topShatter =
--   let doit x = shatter x >>= store
--   in  flip E.execState S.empty . doit
--
--
-- -- | Shatter a given parsed tree into the set of the component
-- -- elementary trees.
-- shatter :: Tree Node IsHead -> E.State (S.Set ET) ET
-- shatter r
--   | null (subForest r) = do
--       let x = getTerm' r
--       return $ R.Node (O.Term x) []
--   | otherwise = do
--       let (subTrees0, areHeads) = unzip (subForest r)
--       subTrees <- forM (zip subTrees0 areHeads) $
--         \(t, isHead) -> shatterChild r t isHead
--       let x = Just (getNT' r)
--       return $ R.Node (O.NonTerm x) subTrees
--
--
-- shatterChild
--   :: Tree Node IsHead -- ^ Parent
--   -> Tree Node IsHead -- ^ Child
--   -> IsHead           -- ^ Child is a head?
--   -> E.State (S.Set ET) ET
-- shatterChild parentTree childTree isHead = do
--   childET <- shatter childTree
--   if ( isHead == HeadYes || rootLabel childTree
--              `isObligatory` rootLabel parentTree ) then do
--     -- this should cover the case of a terminal child
--     return childET
--   else if rootLabel childTree `isModifier`
--           rootLabel parentTree then do
--     store $ mkLeftModifier childET
--     let parentNT = Just (getNT' parentTree)
--     return $ R.Node
--       (O.NonTerm parentNT)
--       [R.Node (O.NonTerm Nothing) []]
--   else do
--     -- substitution
--     store childET
--     let childNT  = Just (getNT' childTree)
--     return $ R.Node (O.NonTerm childNT) []


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


data WalkStats = WalkStats
  { gramSet :: S.Set ET
  -- ^ The resulting grammar
  , seenNum :: Int
  -- ^ The number of seen files
  , parsedNum :: Int
  -- ^ The number of parsed files (i.e., for which at least one
  -- parsed tree has been extracted)
  } deriving (Show, Eq, Ord)


showWalkStats WalkStats{..} = do
  putStr "SEEN: " >> print seenNum
  putStr "PARSED: " >> print parsedNum
  putStr "GRAM TREES: " >> print (S.size gramSet)


-- | Extract grammar from all files present (directly or not) in a give directory.
walkAndRead :: FilePath -> IO ()
walkAndRead root = do
  paths <- pathWalkLazy root
  walkStats <- flip E.execStateT emptyStats $ do
    forM_ paths $ \(root, _dirs, files) -> do
      forM_ files $ \file -> do
        E.when ("-s.xml" `isSuffixOf` file) $ do
          procPath $ joinPath [root, file]
  showWalkStats walkStats
  forM_ (S.toList $ gramSet walkStats) $
    putStrLn . R.drawTree . fmap show
  where
    emptyStats = WalkStats S.empty 0 0
    procPath path = do
      E.lift $ putStrLn $ ">>> " ++ path ++ " <<<"
      E.modify' $ \st -> st {seenNum = seenNum st + 1}
      dag <- E.lift $ mkDAG <$> readTop path
      -- putStrLn "" >> putStrLn "# EXTRACTED:" >> putStrLn ""
      -- printExtracted dag
      let est = case forest chosen 0 dag of
            tree : _ -> topShatter . prepTree $ mapFst label tree
            [] -> S.empty
      E.when (S.null est) $ do
        E.lift $ putStrLn "Something went wrong..." -- >> putStrLn ""
      E.modify' $ \st -> st
        { parsedNum = parsedNum st + if S.null est then 0 else 1
        , gramSet = gramSet st `S.union` est }
      E.when (not $ S.null est) $ do
        g <- E.gets gramSet
        let trees = map (fmap show) (S.toList est)
        length (R.drawForest trees) `seq` E.lift $ do
          putStr "Current number of trees: "
          print $ S.size g


-------------------------------------------------
-- Utils
-------------------------------------------------


-- -- | Obtain non-terminal from ET's root.
-- getNT :: ET -> L.Text
-- getNT t =
--   case R.rootLabel t of
--     O.NonTerm (Just x) -> x
--     _ -> error "getNT: invalid input ET"


-- | Obtain non-terminal from source tree's root.
getNT' :: Tree Node b -> L.Text
getNT' t =
  case label (rootLabel t) of
    Left (NonTerm{..}) -> cat
    _ -> error "getNT': invalid source tree"


-- | Extract the non-terminal from the label (raise an error if not possible).
labelNT :: Label -> NonTerm
labelNT x = case x of
  Left x -> x
  _ -> error "labelNT: not a non-terminal"


-- -- | Obtain non-terminal from source tree's root.
-- getTerm' :: Tree Node b -> L.Text
-- getTerm' t =
--   case label (rootLabel t) of
--     Right (Term{..}) -> base
--     _ -> error "getT': invalid source tree"


-- | Obtain non-terminal from source tree's root.
getTerm' :: R.Tree (Label, a) -> L.Text
getTerm' l =
  case fst (R.rootLabel l) of
    Right (Term{..}) -> base
    _ -> error "getT': invalid source tree"


-- | Print the chosen (simplified) tree represented in the DAG.
printExtracted :: DAG -> IO ()
printExtracted dag =
  let tree = forest chosen 0 dag !! 0
      ets  = prepTree $ mapFst label tree
      lab (Left x) = cat x
      lab (Right t) = orth t
   in putStrLn . R.drawTree . fmap (show . Arr.first lab) $ ets


-- | Print the chosen (simplified) tree represented in the DAG.
printShattered :: DAG -> IO ()
printShattered dag =
  let tree = forest chosen 0 dag !! 0
      ets  = topShatter . prepTree $ mapFst label tree
   in putStrLn
        . R.drawForest
        . map (fmap show)
        . S.toList
        $ ets
