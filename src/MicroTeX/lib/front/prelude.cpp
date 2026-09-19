#include "front/prelude.h"

namespace microtex::front {

// The engine's commands and environments that are written in LaTeX, taken
// from NewCommandMacro::_init_() in lib/macro/macro_def.cpp. The old parser
// expands those itself; the new front end expands these. Environments built
// in C++ (matrix, array, align, ...) are not here: the parser reads them.
std::string_view preludeSource() {
  static constexpr std::string_view source = R"TEX(
\newenvironment{tabular}[1]{\begin{array}{#1}}{\end{array}}
\newenvironment{pmatrix}{\left(\begin{matrix}}{\end{matrix}\right)}
\newenvironment{bmatrix}{\left[\begin{matrix}}{\end{matrix}\right]}
\newenvironment{Bmatrix}{\left\{\begin{matrix}}{\end{matrix}\right\}}
\newenvironment{vmatrix}{\left|\begin{matrix}}{\end{matrix}\right|}
\newenvironment{Vmatrix}{\left\|\begin{matrix}}{\end{matrix}\right\|}
\newenvironment{eqnarray}{\begin{array}{rcl}}{\end{array}}
\newenvironment{cases}{\left\{\begin{array}{@{}ll@{\,}}}{\end{array}\right.}
\newenvironment{rcases}{\left.\begin{array}{@{}ll@{\,}}}{\end{array}\right\}}
\newenvironment{split}{\begin{array}{r@{\;}l}}{\end{array}}
\newenvironment{math}{\(}{\)}
\newenvironment{displaymath}{\[}{\]}
\newenvironment{equation}{\begin{align}}{\end{align}}
\newcommand{\operatorname}[1]{\mathop{\mathrm{#1}}\nolimits }
\newcommand{\substack}[1]{{\scriptstyle\begin{array}{c}#1\end{array}}}
\newcommand{\dfrac}[2]{\genfrac{}{}{1}{}{#1}{#2}}
\newcommand{\tfrac}[2]{\genfrac{}{}{1}{1}{#1}{#2}}
\newcommand{\dbinom}[2]{\genfrac{(}{)}{0pt}{}{#1}{#2}}
\newcommand{\tbinom}[2]{\genfrac{(}{)}{0pt}{1}{#1}{#2}}
\newcommand{\pmod}[1]{\qquad\mathbin{(\mathrm{mod}\ #1)}}
\newcommand{\mod}[1]{\qquad\mathbin{\mathrm{mod}\ #1}}
\newcommand{\pod}[1]{\qquad\mathbin{(#1)}}
\newcommand{\spbreve}{^{\makeatletter\sp@breve\makeatother}}
\newcommand{\spcheck}{^{\vee}}
\newcommand{\spdot}{^{\displaystyle.}}
\newcommand{\d}[1]{\underaccent{\dot}{#1}}
\newcommand{\b}[1]{\underaccent{\bar}{#1}}
\newcommand{\Bra}[1]{\left\langle{#1}\right\vert}
\newcommand{\Ket}[1]{\left\vert{#1}\right\rangle}
\newcommand{\textsuperscript}[1]{{}^{\text{#1}}}
\newcommand{\textsubscript}[1]{{}_{\text{#1}}}
\newcommand{\overbrack}[1]{\overbracket{#1}}
\newcommand{\underbrack}[1]{\underbracket{#1}}
\newcommand{\degree}{^\circ}
\newcommand{\with}{\mathbin{\&}}
\newcommand{\parr}{\mathbin{\rotatebox[origin=c]{180}{\&}}}
\newcommand{\sfrac}[2]{\scalebox{.8}{\raisebox{.5ex}{\raisebox{.45ex}{\numstyle{#1}}\kern-.4ex\nokern\mathslash\nokern\kern-.4ex\raisebox{-.45ex}{\dnomstyle{#2}}}}}
)TEX";
  return source;
}

}  // namespace microtex::front
