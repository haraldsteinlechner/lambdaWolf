\title{Abusing Attribute Grammars for Rendering}
\author{
  Harald Steinlechner \thanks{haraldsteinlechner@gmail.com}
}
\date{\today}

\documentclass[11pt]{article}

\usepackage{verbatim}
\usepackage{listings}
%\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
%\lstloadlanguages{Haskell}
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

\long\def\ignore#1{}

\begin{document}
\maketitle

\begin{abstract}
In this report we show an elegant way to implement a basic SceneGraph system utilizing attribute grammars.
Typically SceneGraph implementations use a clever and flexible mechanism to traverse tree like scene representations.
However traversals and attributes are usually defined in a rather ad-hoc way. In this work attribute grammars,
a well defined formalism is utilized to describe traversals as well as attributes.
We will demonstrate basic rendering as well as culling and give an outlook how to extend the systeme with
user input or other event sources.
\end{abstract}

\section{Introduction}

\begin{center} Rendering is solved. SceneGraphs are dead. 
\end{center}


\paragraph{Outline}
The remainder of this article is organized as follows.
Section~\ref{ags} give a short introduction to attribute grammars.
A basic SceneGraph is defiend in~\ref{scenegraph}.~\ref{reactivity}
provides an outlook how to integrate user interaction into the purely
functional approach previously presented.
Finally, Section~\ref{conclusions} gives the conclusions.

\section{A short introduction to Attribute Grammars}\label{ags}

In this work we use the glorious Utrecht Attribute Grammar library~\cite{citeulike:2637235}. The grammar is 
specified as external domain specific language. Similarly to \texttt{Ox} (a extension to yacc parser generators) a 
preprocessor generates all traversals and provides them as pure haskell code. As an example 
consider the following program:

\small
\begin{verbatim}
data Tree
   | Node  left  :: Tree
           right :: Tree
   | Tip   value :: Int

attr Tree
   syn sum :: Int

sem Tree
  | Node  lhs.sum  =  @left.sum + @right.sum
  | Tip   lhs.sum  =  @value
\end{verbatim}
\normalsize

And another slightly more complicated example (stolen from: \cite{citeulike:5397790}): 

\ignore{
\begin{code}
import Data.List
\end{code}
}

\begin{code}
diff :: [Float] -> [Float]
diff xs = map (\x -> x - (avg xs)) xs
 
avg :: [Float] -> Float
avg xs = sum xs / genericLength xs

\end{code}
As we can see, the complete list is traversed 3 times which is suboptimal. Using clever rewriting
it is possible to solve this concrete problem within one single traversal. However this transformation
quickly gets complicated and the simple declarative version mutates to a complicated construct.
Further details are shown in \cite{citeulike:5397790}.

written as attribute grammar:
\small
\verbatiminput{Diff.ag}
\normalsize

\begin{lstlisting}
>> sem_Root (Root $ Cons 5 (Cons 3 Nil)) => Cons 1.0 (Cons (-1.0) Nil)
\end{lstlisting}

It turns out this attribute grammar indeed generates a program which traverses the input list exactly once.
The initial solution is simple, declarative and works well but is suboptimal respective to performance. This situation
may seem familiar to users of SceneGraph systems. Nifty and very general implementations of traversal routines
often fail to evaluate within the minimal amount of traversals. Consider rendering of an arbitrary scene:
\begin{itemize}
    \item Each node needs to compute its childrens bounding boxes. A flexible solution might be to use a special
      traversal which computes all required bounding boxes. Clever implementations might use some lazy evaluation tricks
      to perform the computation only on a minimal set of nodes. However this optimization requires special optimization
      and tuning.
    \item Another traversal might be used to propagate user inputs to leaf nodes. These events may have effects on the 
      position of leaf geometry nodes which makes order of these traversals significant.
    \item Level of detail decisions might be yet another traversal which again interrelates to prior traversals
    \item Asynchronous computation adds another dimension of complexity - now asynchronous events make their way
      through the graph.
\end{itemize}


\section{Our simple SceneGraph implementation}\label{scenegraph}

Table~\ref{attributes} shows a categorization of typical SceneGraph attributes. 

 \begin{tabular}[t]{lrr@{.}l}
    Computing Bounding-Boxes & DM &  5 & 49\\
    Rendering Scene (RenderTraversal) & DM & 10 & 95\\
  \end{tabular}

\section{How about dynamic scenes and external events}\label{reactivity}

\section{Conclusions}\label{conclusions}
We worked hard, and achieved very little.

\bibliographystyle{alpha}
\bibliography{main}

\end{document}
