\documentclass[12pt]{article}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
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
\usepackage{graphicx}
\title{CCG Parser for Situation Assesment}
\author{Simon Williams}
\begin{document}
\maketitle

\section{Preamble}
\label{sec:preamble}

\subsection{Imports}
\label{sec:imports}
\begin{code}
import Data.Maybe
import Data.List
import CCGParser
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Tree
import Diagrams.Prelude hiding (E)
\end{code}

\subsection{Intents}
\label{sec:tokens}

The intents are the atomic categories
\begin{code}
data AtomicCategory = Transit| Stop| Surveill| Attack deriving (Eq, Show, Read)
\end{code}


\subsection{Lexicon}
\label{sec:lexicon}

Lexicon is a map from tokens to a list of categories

\begin{code}
surv =
  [("Ahead", (Leaf Transit))
  ,("Ahead", ( DerivedCategory Back (Leaf Transit) (Leaf Transit)))
  ,("Left", (Leaf Transit))
  ,("Left", ( DerivedCategory Back (Leaf Transit) (Leaf Transit)))
  ,("Right", (Leaf Transit))
  ,("Right", ( DerivedCategory Back (Leaf Transit) (Leaf Transit)))
  ,("Stop@loc1", (DerivedCategory Back (DerivedCategory Forward (Leaf Surveill) (Leaf Transit)) (Leaf Transit)))]
grpLex =
  [("loc1",(Leaf Surveill))
  ,("loc2",(Leaf Surveill))
  ,("loc3",(Leaf Surveill))
  ]
\end{code}

Construct the lexicon map
\begin{code}
situationLexicon = lexiconListToMap (surv)
grpLexicon = lexiconListToMap grpLex
\end{code}

\section{Example Track}
\label{sec:example_track}

An Example track
\begin{code}
track = ["Ahead", "Left", "Ahead", "Stop", "Ahead", "Right", "Ahead"]
\end{code}

An example grouping
\begin{code}
areasurv = ["loc1","loc2","loc3"]
\end{code}
\section{Interface}
\label{sec:interface}
\begin{code}
parse = parseTokenList track situationLexicon
tree = renderParseTree parse
areaParse = parseTokenList areasurv grpLexicon
areaTree = renderParseTree areaParse
\end{code}

\begin{code}
main :: IO ()
main = print Forward
\end{code}

\end{document}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***