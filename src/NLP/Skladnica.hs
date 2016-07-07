{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveFunctor      #-}


-- | Parsing the Skladnica constituency treebank.


module NLP.Skladnica
(
-- * Types
  NID
, IsHead (..)
, Node (..)
, Label
, NonTerm (..)
, Term (..)

-- * Parsing
, parseTop
, readTop

-- * Conversion
-- ** Tree
, Tree
, Edge (..)
, modifyEdge
, modifyNode
, modifyRootEdge
, modifyRootNode
, mapFst
, simplify
, purge
, drawTree
-- ** DAG
, DAG
, mkDAG
, forest
-- ** Utils
, printChosen
) where


import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import qualified Control.Arrow              as Arr
import           Control.Monad              (guard)

import qualified Data.Foldable              as F
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L
import qualified Data.Tree                  as R

import qualified Text.HTML.TagSoup          as TagSoup
import           Text.XML.PolySoup          hiding (P, Q)
import qualified Text.XML.PolySoup          as PolySoup


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


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
  { cat   :: Text
  -- ^ Category
  , morph :: M.Map Text Text
  } deriving (Show, Eq, Ord)


-- | Terminal
data Term = Term
  { orth :: Text
  , base :: Text
  , tag  :: Text
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
    { cat = L.toStrict cat_
    , morph = M.fromList (map strictPair mor_) }
  where
    strictPair = Arr.first L.toStrict . Arr.second L.toStrict


termQ :: Q Term
termQ = named "terminal" `joinR` do
  orth_ <- first $ named "orth" `joinR` first (node text)
  base_ <- first $ named "base" `joinR` first (node text)
  tag_  <- first $ (named "f" *> hasAttrVal "type" "tag") `joinR` first (node text)
  return $ Term
    { orth = L.toStrict orth_
    , base = L.toStrict base_
    , tag = L.toStrict tag_ }


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


-- | Read a Skladnica XML file and parse it into a sequence of `Node`s.
readTop :: FilePath -> IO [Node]
readTop path = parseTop <$> L.readFile path


-------------------------------------------------
-- Tree
-------------------------------------------------


-- | To distinguish edge labels from the node (child) labels.
data Edge a b = Edge
  { edgeLabel :: b
    -- ^ Label assigned to the in-going edge
  , nodeLabel :: a
    -- ^ Label assigned to the node itself
  } deriving (Show, Eq, Ord)


-- | Modify edge label value.
modifyEdge
  :: (b -> c)
  -> Edge a b
  -> Edge a c
modifyEdge = undefined


-- | Modify node label value.
modifyNode
  :: (a -> b)
  -> Edge a c
  -> Edge b c
modifyNode = undefined


-- | Skladnica tree.
type Tree a b = R.Tree (Edge a b)


-- | Modify root's edge label value.
modifyRootEdge
  :: (b -> c)
  -> Tree a b
  -> Tree a c
modifyRootEdge = undefined


-- | Modify root's node label value.
modifyRootNode
  :: (a -> b)
  -> Tree a c
  -> Tree b c
modifyRootNode = undefined


-- | Map a function over labels attached to tree nodes.
mapFst :: (a -> c) -> Tree a b -> Tree c b
mapFst f t = R.Node
  { R.rootLabel =
      let x = R.rootLabel t
      in  x {nodeLabel = f (nodeLabel x)}
  , R.subForest = map (mapFst f) (R.subForest t) }


-- | Simplify a tree to a regular rose tree (i.e., from the containers package).
simplify :: Tree a b -> R.Tree a
simplify R.Node{..} = R.Node
  { R.rootLabel = (nodeLabel rootLabel)
  , R.subForest = map simplify subForest }


-- | Draw the tree.
drawTree :: Tree String String -> String
drawTree = unlines . treeLines


-- | Draw tree lines.
treeLines :: Tree String String -> [String]
treeLines R.Node{..} =
  show rootLabel : concat
    [ map indent
      (treeLines child)
    | child <- subForest ]
  where
    indent = ("  " ++)


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
  go rootID HeadYes
  where
    go i isHead = do
      n@Node{..} <- maybeToList $ M.lookup i dag
      guard $ nodePred n
      if null children then do
        -- return $ Tree n []
        return $ R.Node (Edge isHead n) []
      else do
        -- take one of the children alternatives
        childrenD <- children
        -- for the given children list, determine the
        -- corresponding forests
        sub <- mapM (uncurry go) childrenD
        -- the result
        return $ R.Node (Edge isHead n) sub


-- | Print the chosen (simplified) tree represented in the DAG.
printChosen :: DAG -> IO ()
printChosen dag =
  let t0 = simplify $ forest chosen 0 dag !! 0
      simpLab = T.unpack . either cat orth . label
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


-- -------------------------------------------------
-- -- Utils
-- -------------------------------------------------
-- 
-- 
-- -- -- | Obtain non-terminal from source tree's root.
-- -- getNT' :: Tree Node b -> L.Text
-- -- getNT' t =
-- --   case label (rootLabel t) of
-- --     Left (NonTerm{..}) -> cat
-- --     _ -> error "getNT': invalid source tree"
-- --
-- --
-- -- -- | Extract the non-terminal from the label (raise an error if not possible).
-- -- labelNT :: Label -> NonTerm
-- -- labelNT x = case x of
-- --   Left x -> x
-- --   _ -> error "labelNT: not a non-terminal"
-- --
-- --
-- -- -- | Obtain non-terminal from source tree's root.
-- -- getTerm' :: R.Tree (Label, a) -> L.Text
-- -- getTerm' l =
-- --   case fst (R.rootLabel l) of
-- --     Right (Term{..}) -> base
-- --     _ -> error "getT': invalid source tree"
