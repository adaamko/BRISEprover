# BRISEprover

Tentative prover for the BRISE project - formalisation of natural language with possibly comparing different formalisations

See [here](http://subsell.logic.at/bprover/briseprover/) for the web interface.

## Usage

To use BRISEprover, first make sure you have [SWI-prolog](https://www.swi-prolog.org) installed and download the files. For running BRISEprover on your system only the prolog files (*.pl*) are necessary. If you want to latex the derivation you also need the file *header.sty* and a latex distribution. Put all the files into the directory of your choice, then run swipl and load *deonticprover.pl*.

The main predicate for local usage is `briseprover_local`. Run

> ?- briseprover_local(Fml, Facts, Deontic_assumptions, Superiority_relation, Example_list, Output_format).

where
- `Fml` is the input formula
- `Facts` is a potentially empty list of *factual assumptions* of the form `atom1 and ... and atomN -> atomN+1 or ... or atomN+M` with the `atomI` prolog atoms.
- `Deontic_assumptions` is a list of *deontic assumptions* of the form `Op(Fml1,Fml2)` or `Norm:Op(Fml1,Fml2)` where `Op` is one of the deontic operators `obl`, `for`, `per`, `Norm` is a prolog atom stating the name of a norm, and `Fml1` and `Fml2` are purely propositional formulae, i.e., prolog terms build from prolog atoms, the terms `true` and `false`, the unary constructor `neg` and the binary infix constructors `or`, `and` and `->`. The usual conventions about binding strength of the operators apply, i.e., `neg` binds stronger than the binary connectors, `and` binds stronger than `or` and `->`, and `or` binds stronger than `->`.
- `Superiority_relation` gives potential superiority relation on the list of deontic assumptions in the form of `Norm1 beats Norm2` where `Norm1` and `Norm2` are names of norms 
- `Example_list` is a list of examples of Plandocuments. So far the examples include `plandokument(6963)`, `plandokument(7601)`, `plandokument(7602)` for the respective plandocuments and `bauordnung(b)` for some portions of the Bauordnung.
- `Output_format` is the *output format*, i.e., one of `derivation` and `explanation`.

Running `briseprover_local` with output format `derivation` writes the output into the file *output.tex*. Make sure that the file *mymacros.sty* is in the same directory and run Pdflatex on this file to obtain the pdf.

Running `briseprover_local` with output format `explanation` generates the output file *output.html*. Open this with a browser to see the explanation.


## References and prior work

BRISEprover is based on [deonticProver2.0](http://subsell.logic.at/bprover/deonticProver/version2.0/), an implementation of a sequent-based system for reasoning with deontic assumptions. The theoretical results underlying deonticProver2.0 are available in [this paper](http://www.collegepublications.co.uk/DEON/submission%20Ciabattoni%20Lellmann.pdf). 
