(*****************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Universite Paris 13, Sorbonne Paris Cite (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2015/03/24
 * Last modified: 2015/04/23
 *
 ****************************************************************)


(************************************************************
 Constants
************************************************************)
(** Number of different colors in the LaTeX header ***)
let nb_colors = 16




(*** HACK: manually replaced \t with \\t (and similarly for \n and \b and \r) ***)

let latex_header = "
\documentclass[a4paper,10pt]{article}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PACKAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage[T1]{fontenc}

\usepackage[utf8]{inputenc}

\usepackage{subcaption}

% Tikz
\usepackage{tikz}
\usetikzlibrary{arrows,automata}
\\tikzstyle{every node}=[initial text=]
\\tikzstyle{location}=[rectangle, rounded corners, minimum size=12pt, draw=black, inner sep=1.5pt]
\\tikzstyle{invariant}=[draw=black, xshift=1em, inner sep=1pt]


\definecolor{coloract}{rgb}{0.50, 0.70, 0.30}
\definecolor{colorclock}{rgb}{0.4, 0.4, 1}
\definecolor{colordisc}{rgb}{1, 0, 1}
\definecolor{colorloc}{rgb}{0.4, 0.4, 0.65}
\definecolor{colorparam}{rgb}{1, 0.6, 0.0}

% Set of colors
\definecolor{loccolor1}{rgb}{1, 0.3, 0.3}
\definecolor{loccolor2}{rgb}{0.3, 1, 0.3}
\definecolor{loccolor3}{rgb}{0.3, 0.3, 1}
\definecolor{loccolor4}{rgb}{1, 0.3, 1}
\definecolor{loccolor5}{rgb}{1, 1, 0.3}
\definecolor{loccolor6}{rgb}{0.3, 1, 1}
\definecolor{loccolor7}{rgb}{0.9, 0.6, 0.2}
\definecolor{loccolor8}{rgb}{0.7, 0.4, 1}
\definecolor{loccolor9}{rgb}{0.5, 1, 0.75}
\definecolor{loccolor10}{rgb}{0.8, 0.7, 0.6}
\definecolor{loccolor11}{rgb}{0.6, 0.7, 0.8}
\definecolor{loccolor12}{rgb}{0.2, 0.5, 0.9}
\definecolor{loccolor13}{rgb}{0.5, 0.9, 0.2}
\definecolor{loccolor14}{rgb}{0.9, 0.2, 0.5}
\definecolor{loccolor15}{rgb}{0.7, 0.7, 0.7}
\definecolor{loccolor16}{rgb}{0.8, 0.8, 0.5}

\\newcommand{\styleact}[1]{\ensuremath{\\textcolor{coloract}{\mathrm{#1}}}}
\\newcommand{\styleclock}[1]{\ensuremath{\\textcolor{colorclock}{\mathrm{#1}}}}
\\newcommand{\styledisc}[1]{\ensuremath{\\textcolor{colordisc}{\mathrm{#1}}}}
\\newcommand{\styleloc}[1]{\ensuremath{\\textcolor{colorloc}{\mathrm{#1}}}}
\\newcommand{\styleparam}[1]{\ensuremath{\\textcolor{colorparam}{\mathrm{#1}}}}

\\newcommand{\imitator}{\\textsc{Imitator}}

\\title{An IMITATOR model}
\author{" ^ Constants.program_name ^ "}


\\begin{document}

\\thispagestyle{empty}

\\begin{figure}
\\newcommand{\\ratio}{0.5\\textwidth}
 
	\centering
	 
"
