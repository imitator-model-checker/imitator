imitator
========

IMITATOR is an open source software tool to perform automated parameter synthesis for concurrent timed systems.
IMITATOR takes as input a network of parametric timed automata [AHV93], a well-known formalism to specify and verify models of systems where timing constants can be replaced with parameters, i.e., unknown constants.

IMITATOR addresses several variants of the following problem:
_given a concurrent timed system, what are the values of the timing constants that guarantee that the model of the system satisfies some property?_
Specifically, IMITATOR implements parametric safety analysis [AHV93,JLR15], the inverse method  ACEF09,AM15], the behavioral cartography [AF10], and parametric reachability preservation [ALNS15].
Numerous analysis options are available.

IMITATOR is a command-line only tool, but that can output results in graphical form.
Furthermore, a graphical user interface is available in the CosyVerif platform.

IMITATOR was able to verify numerous case studies from the literature and from the industry, such as communication protocols, hardware asynchronous circuits, schedulability problems and various other systems such as coffee machines (probably the most critical systems from a researcher point of view).
Numerous benchmarks are available at IMITATOR Web page.

For more info, please visit www.imitator.fr


_Keywords_: formal verification, model checking, software verification, parameter synthesis


_References_:

[ACEF09] Étienne André, Thomas Chatain, Emmanuelle Encrenaz and Laurent Fribourg. An Inverse Method for Parametric Timed Automata. International Journal of Foundations of Computer Science 20(5), pages 819–836, 2009.

[AF10] Étienne André and Laurent Fribourg. Behavioral Cartography of Timed Automata. In Antonín Kučera and Igor Potapov (eds.), RP’10, LNCS 6227, Springer, pages 76–90, September 2010.

[AFKS12] Étienne André, Laurent Fribourg, Ulrich Kühne and Romain Soulat. IMITATOR 2.5: A Tool for Analyzing Robustness in Scheduling Problems. In Dimitra Giannakopoulou and Dominique Méry (eds.), FM’12, LNCS 7436, Springer, pages 33–36, August 2012.

[AHV93] Rajeev Alur, Thomas A. Henzinger and Moshe Y. Vardi. Parametric real-time reasoning. STOC’93, ACM, pages 592--601, 1993.

[AM15] Étienne André and Nicolas Markey. Language Preservation Problems in Parametric Timed Automata. In Sriram Sankaranarayanan and Enrico Vicario (eds.), FORMATS’15, Springer LNCS, September 2015.

[JLR15] Aleksandra Jovanovic, Didier Lime, Olivier H. Roux. Integer Parameter Synthesis for Real-Time Systems. IEEE Trans. Software Eng. 41(5): 445-461, 2015.
