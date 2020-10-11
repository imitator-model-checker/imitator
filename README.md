imitator
========

![Travis (.org) branch](https://img.shields.io/travis/imitator-model-checker/imitator/master.svg)
![GitHub](https://img.shields.io/github/license/imitator-model-checker/imitator.svg)

[IMITATOR](https://www.imitator.fr) is an open source software tool to perform automated parameter synthesis for concurrent timed systems.

IMITATOR takes as input a network of IMITATOR parametric timed automata (NIPTA): NIPTA are an extension of parametric timed automata [AHV93], a well-known formalism to specify and verify models of systems where timing constants can be replaced with parameters, i.e., unknown constants.
The input formalism can be seen as a subclass of parametric linear hybrid automata, with constant clock rates (which includes stopwatches and multi-rate automata).

IMITATOR addresses several variants of the following problem:
_given a concurrent timed system, what are the values of the timing constants that guarantee that the model of the system satisfies some property?_
Specifically, IMITATOR implements:
* parametric safety and parametric reachability synthesis [AHV93,JLR15],
* minimal-time and minimal-parameter reachability synthesis [ABPP19],
* parametric deadlock-freeness checking [Andre16],
* cycle-existence synthesis [NPP18],
* cycle-existence synthesis under the non-Zenoness assumption [ANPS17],
* the inverse method (also known as (robust) language or trace preservation synthesis) [ACEF09,AS11,AM15],
* the behavioral cartography [AF10], and
* parametric reachability preservation (PRP and PRPC) [ALNS15].

Numerous analysis options are available.

IMITATOR is able to run in a distributed fashion on a cluster (using the PRPC algorithm).

IMITATOR is mainly a command-line tool, but that can output results in graphical form.

IMITATOR was able to verify numerous case studies from the literature and from the industry, such as communication protocols, hardware asynchronous circuits, schedulability problems with uncertain periods and various other systems such as coffee machines (probably the most critical systems from a researcher point of view).
Numerous benchmarks are available at the [IMITATOR Web page](https://www.imitator.fr), or on github.

For more info, please visit [imitator.fr](https://www.imitator.fr)


### Keywords
formal verification, model checking, software verification, parameter synthesis, parametric timed automata


### References

[ABPP19] Étienne André, Vincent Bloemen, Laure Petrucci and Jaco Van de Pol. [**Minimal-Time Synthesis for Parametric Timed Automata**](https://doi.org/10.1007/978-3-030-17465-1_12). In Tomas Vojnar and Lijun Zhang (eds.), TACAS’19, Springer LNCS, April 2019.

[ACEF09] Étienne André, Thomas Chatain, Emmanuelle Encrenaz and Laurent Fribourg. [**An Inverse Method for Parametric Timed Automata**](https://doi.org/10.1142/S0129054109006905). International Journal of Foundations of Computer Science 20(5), pages 819–836, 2009.

[AF10] Étienne André and Laurent Fribourg. [**Behavioral Cartography of Timed Automata**](https://doi.org/10.1007/978-3-642-15349-5_5). In Antonín Kučera and Igor Potapov (eds.), RP’10, LNCS 6227, Springer, pages 76–90, September 2010.

[AFKS12] Étienne André, Laurent Fribourg, Ulrich Kühne and Romain Soulat. [**IMITATOR 2.5: A Tool for Analyzing Robustness in Scheduling Problems**](https://doi.org/10.1007/978-3-642-32759-9_6). In Dimitra Giannakopoulou and Dominique Méry (eds.), FM’12, LNCS 7436, Springer, pages 33–36, August 2012.

[AHV93] Rajeev Alur, Thomas A. Henzinger and Moshe Y. Vardi. [**Parametric real-time reasoning**](https://doi.org/10.1145/167088.167242). STOC’93, ACM, pages 592–601, 1993.

[ALNS15] Étienne André, Giuseppe Lipari, Nguyễn Hoàng Gia and Sun Youcheng. [**Reachability Preservation Based Parameter Synthesis for Timed Automata**](https://doi.org/10.1007/978-3-319-17524-9_5). In Klaus Havelund, Gerard Holzmann, Rajeev Joshi (eds.), NFM’15, LNCS 9058, Springer, pages 50–65, April 2015.

[AM15] Étienne André and Nicolas Markey. [**Language Preservation Problems in Parametric Timed Automata**](https://doi.org/10.1007/978-3-319-22975-1_3). In Sriram Sankaranarayanan and Enrico Vicario (eds.), FORMATS’15, Springer LNCS, September 2015.

[Andre16] Étienne André. [**Parametric Deadlock-Freeness Checking Timed Automata**](https://doi.org/10.1007/978-3-319-46750-4_27). In Augusto Cesar Alves Sampaio and Farn Wang (eds.), ICTAC’16, LNCS 9965, Springer, pages 469–478, October 2016.

[ANPS17] Étienne André, Nguyễn Hoàng Gia, Laure Petrucci and Sun Jun. [**Parametric model checking timed automata under non-Zenoness assumption**](https://doi.org/10.1007/978-3-319-57288-8_3). In Clark Barrett and Temesghen Kahsai (eds.), NFM’17, Springer LNCS 10227, pages 35–51, May 2017.

[AS11] Étienne André and Romain Soulat. [**Synthesis of Timing Parameters Satisfying Safety Properties**](https://doi.org/10.1007/978-3-642-24288-5_5). RP 2011: 31-44, 2011.

[JLR15] Aleksandra Jovanovic, Didier Lime, Olivier H. Roux. [**Integer Parameter Synthesis for Real-Time Systems**](https://doi.org/10.1109/TSE.2014.2357445). IEEE Trans. Software Eng. 41(5): 445–461, 2015.

[NPP18] Hoang Gia Nguyen, Laure Petrucci, Jaco van de Pol. [**Layered and Collecting NDFS with Subsumption for Parametric Timed Automata**](https://doi.org/10.1109/ICECCS2018.2018.00009). ICECCS 2018: 1-9, 2018.
