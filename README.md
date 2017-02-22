imitator
========

[IMITATOR](http://www.imitator.fr) is an open source software tool to perform automated parameter synthesis for concurrent timed systems.
IMITATOR takes as input a network of IMITATOR parametric timed automata (NIPTA): NIPTA are an extension of parametric timed automata [AHV93], a well-known formalism to specify and verify models of systems where timing constants can be replaced with parameters, i.e., unknown constants.

IMITATOR addresses several variants of the following problem:
_given a concurrent timed system, what are the values of the timing constants that guarantee that the model of the system satisfies some property?_
Specifically, IMITATOR implements:
* parametric safety and parametric reachability analysis [AHV93,JLR15],
* parametric deadlock-freeness checking [Andre16],
* cycle-existence synthesis,
* cycle-existence synthesis under the non-Zenoness assumption [ANPS17],
* the inverse method (also known as (robust) language or trace preservation synthesis) [ACEF09,AM15],
* the behavioral cartography [AF10], and
* parametric reachability preservation (PRP and PRPC) [ALNS15].

Numerous analysis options are available.

IMITATOR is able to run in a distributed fashion on a cluster (using the PRPC algorithm).

IMITATOR is mainly a command-line tool, but that can output results in graphical form.
Furthermore, a graphical user interface is available in the [CosyVerif](http://cosyverif.org/) platform.

IMITATOR was able to verify numerous case studies from the literature and from the industry, such as communication protocols, hardware asynchronous circuits, schedulability problems with uncertain periods and various other systems such as coffee machines (probably the most critical systems from a researcher point of view).
Numerous benchmarks are available at the [IMITATOR Web page](http://www.imitator.fr), or on github.

For more info, please visit www.imitator.fr


### Keywords
formal verification, model checking, software verification, parameter synthesis, parametric timed automata


### References

[ACEF09] Étienne André, Thomas Chatain, Emmanuelle Encrenaz and Laurent Fribourg. **An Inverse Method for Parametric Timed Automata**. International Journal of Foundations of Computer Science 20(5), pages 819–836, 2009.

[AF10] Étienne André and Laurent Fribourg. **Behavioral Cartography of Timed Automata**. In Antonín Kučera and Igor Potapov (eds.), RP’10, LNCS 6227, Springer, pages 76–90, September 2010.

[AFKS12] Étienne André, Laurent Fribourg, Ulrich Kühne and Romain Soulat. **IMITATOR 2.5: A Tool for Analyzing Robustness in Scheduling Problems**. In Dimitra Giannakopoulou and Dominique Méry (eds.), FM’12, LNCS 7436, Springer, pages 33–36, August 2012.

[AHV93] Rajeev Alur, Thomas A. Henzinger and Moshe Y. Vardi. **Parametric real-time reasoning**. STOC’93, ACM, pages 592–601, 1993.

[ALNS15] Étienne André, Giuseppe Lipari, Nguyễn Hoàng Gia and Sun Youcheng. **Reachability Preservation Based Parameter Synthesis for Timed Automata**. In Klaus Havelund, Gerard Holzmann, Rajeev Joshi (eds.), NFM’15, LNCS 9058, Springer, pages 50–65, April 2015.

[AM15] Étienne André and Nicolas Markey. **Language Preservation Problems in Parametric Timed Automata**. In Sriram Sankaranarayanan and Enrico Vicario (eds.), FORMATS’15, Springer LNCS, September 2015.

[Andre16] Étienne André. **Parametric Deadlock-Freeness Checking Timed Automata**. In Augusto Cesar Alves Sampaio and Farn Wang (eds.), ICTAC’16, LNCS 9965, Springer, pages 469–478, October 2016.

[ANPS17] Étienne André, Nguyễn Hoàng Gia, Laure Petrucci and Sun Jun. **Parametric model checking timed automata under non-Zenoness assumption**. In Clark Barrett and Temesghen Kahsai (eds.), NFM’17, Springer LNCS, May 2017. To appear.

[JLR15] Aleksandra Jovanovic, Didier Lime, Olivier H. Roux. **Integer Parameter Synthesis for Real-Time Systems**. IEEE Trans. Software Eng. 41(5): 445–461, 2015.
