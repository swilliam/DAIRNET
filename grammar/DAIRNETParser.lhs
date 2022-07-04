\documentclass[12pt]{article}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{language=Haskell,basicstyle=\small\sffamily,
      columns=flexible,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
      }
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}

     

\title{CCG Parser}
\author{Simon Williams}
\begin{document}
\maketitle

\section{Preamble}
\label{sec:preamble}

\begin{code}
module CCGParser 
(Category(..)
,Lexicon
,Slash(..)
,lexiconListToMap
,parseTokenList
,renderParseTree
,token2chart
) where
\end{code}

\subsection{Imports}
\label{sec:imports}
\begin{code}
import Data.Maybe
import qualified Data.List as L
import qualified  Data.Map as Map
import Control.Applicative
import Control.Monad
import Diagrams.Backend.SVG
{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Text
\end{code}
\section{Data Structures}
\label{sec:data-structures}


\subsection{Categories}
\label{sec:categories}

We'll try and build up the categories from their component
parts. First the slash

\begin{code}
data Slash = Forward | Vert | Back deriving (Eq)
instance Show Slash where
  show Forward = "/"
  show Vert = "|"
  show Back = "\\"
\end{code}

then the root categories

\begin{code}
data AtomicCategory = S | N deriving (Eq, Show, Read)
\end{code}

and finally the category itself

\begin{code}
data Category  a = Leaf a | DerivedCategory Slash (Category a)  (Category a) deriving (Eq)
instance (Show a) => Show (Category a) where
         show (Leaf b) = show b
         show (DerivedCategory  b c d) = "(" ++ show c ++ show b ++ show d ++")"
\end{code}

\subsection{Combinators}
\label{sec:combinators}

\begin{code}
type Combinator a = (ParseInfo a -> ParseInfo a -> Maybe (ParseInfo a))
\end{code}
First the forward combinator
\begin{code}
forward :: (Eq act) =>Combinator act
forward ParseInfo {category=(DerivedCategory Forward c a ),
        lexical=l1}  ParseInfo {category=b,lexical=l2}
        | a==b  = Just ParseInfo {category=c, lexical=head_lex l1 l2 c b,combinator=">"}
          where head_lex l1 l2 c b | c == b = l2 | c /= b = l1
forward _ _ = Nothing
\end{code}
 and the backward
\begin{code}
back :: (Eq act) =>Combinator act
back ParseInfo {category=b,lexical=l2} ParseInfo {category=(DerivedCategory Back c a ), lexical=l1} 
        | a==b  = Just (ParseInfo {category=c, lexical=head_lex l1 l2 c b,combinator="<"})
          where head_lex l1 l2 c b | c == b = l2 | c /= b = l1
back _ _ = Nothing
\end{code}

\subsection{Lexicon}
\label{sec:lexicon}

Lexicon is a map from words to a list of categories

\begin{code}
type Lexicon a b = Map.Map a [Category b]
lexiconListToMap :: (Ord k) => [(k, v)] -> Map.Map k [v]  
lexiconListToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 
\end{code}

An example lexicon is then 

\begin{code}
exampleLexicon =
   [("the", (DerivedCategory Forward (Leaf N) (Leaf N)))
   ,("cat", (Leaf N))
   ,("sat",(DerivedCategory Forward (DerivedCategory Back (Leaf S) (Leaf N)) (Leaf N)))
   ,("sat", (DerivedCategory Back (Leaf S) (Leaf N)))
   ,("on", (DerivedCategory Forward (DerivedCategory Back (DerivedCategory Back (Leaf S) (Leaf N)) (DerivedCategory Back (Leaf S) (Leaf N) ) ) (Leaf N)))
   ,("mat",(Leaf N))
   ,("maple", (Leaf N))
   ,("green", (DerivedCategory Forward (DerivedCategory Back (Leaf S) (Leaf N)) (Leaf N)))
   ,("180",(Leaf N))]
\end{code}
\section{Algorithms}
\label{sec:algorithms}
 A parser takes a list of tokens, a set of combinators and, a lexicon and constructs a parsetree
This is a ParseTree
\begin{code}
type Token = String
data ParseInfo a = ParseInfo { category :: Category a, combinator ::
     String, lexical::String} deriving (Show, Eq)
type ParseTree a = BTree (ParseInfo a) 
getInfo :: ParseTree a -> ParseInfo a
getInfo (BNode a _ _)= a
\end{code}
 and a Parser is then

An example sentence
\begin{code}
sentence = ["the","cat","sat","on","the","mat"]
\end{code}


The lexical field holds the label of the original token for leaf nodes and the head of the combined chart edge other wise

\subsection{Deterministic Chart}
\label{sec:deterministic-chart}
A chart is a list of edges 
\begin{code}
data Edge a = Edge {start_node::Int,end_node::Int,node::ParseTree a} deriving (Show,Eq)
type Chart a = [Edge a]
\end{code}
token span and the top level parsenode

Convert tokens to starting edges in the chart
\begin{code}
token2chart:: (Show a, Ord a) => [a] -> Lexicon a b->Chart b
token2chart  tokens lexicon = concat [ fromJust ( fmap  (map (\c -> (Edge
             {start_node =n, end_node=(succ n), node=(leaf ParseInfo
             {category=c, lexical=show t, combinator="t"})})))
             (Map.lookup t lexicon))
             | (n,t) <- (zip [0..] tokens)]
\end{code}
then combine all possibilities
\begin{code}
updateChart :: (Eq a) => Chart a-> Chart a
updateChart chart
      | chartAdditions `L.union` chart == chart = error "no parse"
      | otherwise = chartAdditions `L.union` chart
      where chartAdditions =  [ Edge {start_node=start_node e1,      
end_node=end_node e2, node=BNode (fromJust nn) (node e1) (node e2)}
                   | e1 <- chart, e2<-chart,
                   end_node e1 == start_node e2,
                   let n1 = getInfo $ node e1, let n2 =  getInfo $ node e2,
                   let nn = forward n1 n2 <|> back n1 n2, isJust nn]
\end{code}

A condition for the end of parsing: the edge covers the whole token list
\begin{code}
fullEdge :: Int-> Edge a-> Bool
fullEdge l e = start_node e == 0 && end_node e == l
\end{code}
chart is full when there is a full edge
\begin{code}
fullChart :: Int-> Chart a-> Bool
fullChart l c = any (fullEdge l) c
\end{code}
then parsing is as simple as
\begin{code}
parseTokenList :: (Eq c,Show a, Ord a) => [a] ->Lexicon a c-> ParseTree c
parseTokenList tl lex = node (head (filter (fullEdge (length tl))  (until (fullChart (length tl))
               updateChart (token2chart tl lex))))
\end{code}

\subsection{Probabilistic Chart}
\label{sec:probabilistic-chart}


\section{Interface}
\label{sec:interface}


\begin{code}
main :: IO ()
main = print Forward
\end{code}

\subsection{Visualisation of Parse Trees}
\label{sec:visu-parse-trees}

\begin{code}
renderParseTree :: (Show a) =>ParseTree a-> Diagram B
renderParseTree t = pad 1.1 . centerXY
       $ renderTree
              (\n -> ((alignedText 0.5 0 (show (category n))) === strutY 1
                  === (alignedText 0.5 0 ((combinator n)++" "++(lexical n)))) <> roundedRect 8 4 0.3 # translateY (-1) # fc white)
              (~~)
              (fromJust  (uniqueXLayout 5 5 t))
\end{code}
\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
