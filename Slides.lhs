\documentclass{beamer}

\mode<presentation> {

\usetheme{Madrid}
}

\usepackage{graphicx}
\usepackage{booktabs}
\usepackage{hyperref}
\usepackage{apacite}
\usepackage{fancyvrb}
\usepackage{alltt}
\usepackage{listings}

\hypersetup{colorlinks=false}

\setbeamertemplate{bibliography entry title}{}
\setbeamertemplate{bibliography entry location}{}
\setbeamertemplate{bibliography entry note}{}
\setbeamertemplate{itemize items}[default]
\setbeamertemplate{enumerate items}[default]
\beamertemplatenavigationsymbolsempty
\setbeamertemplate{footline}{}

\lstdefinelanguage{haskell}{
  morekeywords={class,instance,where,do,data,newtype,default,deriving,module},
  otherkeywords={<-},
  sensitive=true,
  morecomment=[l]{--},
  morecomment=[n]{\{-}{-\}},
  morestring=[b]",
  morestring=[b]',
  morestring=[b]"""
}

\lstnewenvironment{code}{\lstset{language=haskell,basicstyle=\small}}{}
\lstnewenvironment{fakecode}{\lstset{language=haskell,basicstyle=\small}}{}

\title[Getting the Most Out of Transformers]{Getting the Most Out of Monad Transformers}

\author{George Wilson}
\institute[]
{
Ephox\\
\medskip
\href{george.wilson@ephox.com}{george.wilson@ephox.com}
}

\begin{document}

\begin{frame}
\titlepage
\end{frame}

\input{Before.lhs}
\input{After.lhs}

\end{document}

