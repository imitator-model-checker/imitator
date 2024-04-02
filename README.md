imitator
========

[![CI](https://github.com/imitator-model-checker/imitator/actions/workflows/workflow.yml/badge.svg?branch=master)](https://github.com/imitator-model-checker/imitator/actions/workflows/workflow.yml)
![GitHub](https://img.shields.io/github/license/imitator-model-checker/imitator.svg)

[IMITATOR](https://www.imitator.fr) [[Andre21]](https://www.doi.org/10.1007/978-3-030-81685-8_26) is an open source software tool to perform automated parameter synthesis for concurrent timed systems.

IMITATOR takes as input a network of IMITATOR parametric timed automata (NIPTA): NIPTA are an extension of parametric timed automata [[AHV93]](https://www.doi.org/10.1145/167088.167242), a well-known formalism to specify and verify models of systems where timing constants can be replaced with parameters, i.e., unknown constants.
The input formalism can be seen as a subclass of parametric linear hybrid automata, with constant clock rates (which includes stopwatches and multi-rate automata).

IMITATOR addresses several variants of the following problem:
_given a concurrent timed system, what are the values of the timing constants that guarantee that the model of the system satisfies some property?_
Specifically, IMITATOR implements:
* parameter synthesis for a subset of TCTL, including safety, reachability, unavoidability…, and their timed counterpart, such as $E \phi U_[p; p+1) \psi$ [[AHV93]](https://www.doi.org/10.1145/167088.167242) [[JLR15]](https://www.doi.org/10.1109/TSE.2014.2357445),
* minimal-time and minimal-parameter reachability synthesis [[ABPP19]](https://www.doi.org/10.1007/978-3-030-17465-1_12),
* parametric deadlock-freeness checking [[Andre16]](https://www.doi.org/10.1007/978-3-319-46750-4_27),
* cycle-existence synthesis [[NPP18]](https://www.doi.org/10.1109/ICECCS2018.2018.00009) [[AAPP21]](https://www.doi.org/10.1007/978-3-030-72016-2_17),
* cycle-existence synthesis under the non-Zenoness assumption [[ANPS17]](https://www.doi.org/10.1007/978-3-319-57288-8_3),
* the inverse method (also known as (robust) language or trace preservation synthesis) [[ACEF09]](https://www.doi.org/10.1142/S0129054109006905) [[AS11]](https://www.doi.org/10.1007/978-3-642-24288-5_5) [[AM15]](https://www.doi.org/10.1007/978-3-319-22975-1_3),
* the behavioral cartography [[AF10]](https://www.doi.org/10.1007/978-3-642-15349-5_5), and
* parametric reachability preservation (PRP and PRPC) [[ALNS15]](https://www.doi.org/10.1007/978-3-319-17524-9_5).

Numerous analysis options are available.

IMITATOR is able to run in a distributed fashion on a cluster for a selected algorithm (PRPC).

IMITATOR is mainly a command-line tool, but that can output results in graphical form.

IMITATOR was able to verify numerous case studies from the literature and from the industry, such as communication protocols, hardware asynchronous circuits, schedulability problems with uncertain periods and various other systems such as coffee machines (probably the most critical systems from a researcher point of view).
Numerous benchmarks are available in the official [IMITATOR benchmarks library](https://www.imitator.fr/library.html), or on GitHub.

For more info, please visit [imitator.fr](https://www.imitator.fr)


## Keywords
formal verification, model checking, software verification, parameter synthesis, parametric timed automata, TCTL


## References

[AAPP21] Étienne André, Jaime Arias, Laure Petrucci and Jaco van de Pol. [**Iterative Bounded Synthesis for Efficient Cycle Detection in Parametric Timed Automata**](https://www.doi.org/10.1007/978-3-030-72016-2_17). In Jan Friso Groote and Kim G. Larsen (eds.), TACAS’21, Springer LNCS, April 2021.

[ABPP19] Étienne André, Vincent Bloemen, Laure Petrucci and Jaco van de Pol. [**Minimal-Time Synthesis for Parametric Timed Automata**](https://www.doi.org/10.1007/978-3-030-17465-1_12). In Tomas Vojnar and Lijun Zhang (eds.), TACAS’19, Springer LNCS, April 2019.

[ACEF09] Étienne André, Thomas Chatain, Emmanuelle Encrenaz and Laurent Fribourg. [**An Inverse Method for Parametric Timed Automata**](https://www.doi.org/10.1142/S0129054109006905). International Journal of Foundations of Computer Science 20(5), pages 819–836, 2009.

[AF10] Étienne André and Laurent Fribourg. [**Behavioral Cartography of Timed Automata**](https://www.doi.org/10.1007/978-3-642-15349-5_5). In Antonín Kučera and Igor Potapov (eds.), RP’10, LNCS 6227, Springer, pages 76–90, September 2010.

[AHV93] Rajeev Alur, Thomas A. Henzinger and Moshe Y. Vardi. [**Parametric real-time reasoning**](https://www.doi.org/10.1145/167088.167242). STOC’93, ACM, pages 592–601, 1993.

[ALNS15] Étienne André, Giuseppe Lipari, Nguyễn Hoàng Gia and Sun Youcheng. [**Reachability Preservation Based Parameter Synthesis for Timed Automata**](https://www.doi.org/10.1007/978-3-319-17524-9_5). In Klaus Havelund, Gerard Holzmann, Rajeev Joshi (eds.), NFM’15, LNCS 9058, Springer, pages 50–65, April 2015.

[AM15] Étienne André and Nicolas Markey. [**Language Preservation Problems in Parametric Timed Automata**](https://www.doi.org/10.1007/978-3-319-22975-1_3). In Sriram Sankaranarayanan and Enrico Vicario (eds.), FORMATS’15, Springer LNCS, September 2015.

[Andre16] Étienne André. [**Parametric Deadlock-Freeness Checking Timed Automata**](https://www.doi.org/10.1007/978-3-319-46750-4_27). In Augusto Cesar Alves Sampaio and Farn Wang (eds.), ICTAC’16, LNCS 9965, Springer, pages 469–478, October 2016.

[Andre21] Étienne André. [**IMITATOR 3: Synthesis of timing parameters beyond decidability**](https://www.doi.org/10.1007/978-3-030-81685-8_26). In Rustan Leino and Alexandra Silva (eds.), CAV’21, Springer LNCS 12759, pages 1-14, July 2021.

[ANPS17] Étienne André, Nguyễn Hoàng Gia, Laure Petrucci and Sun Jun. [**Parametric model checking timed automata under non-Zenoness assumption**](https://www.doi.org/10.1007/978-3-319-57288-8_3). In Clark Barrett and Temesghen Kahsai (eds.), NFM’17, Springer LNCS 10227, pages 35–51, May 2017.

[AS11] Étienne André and Romain Soulat. [**Synthesis of Timing Parameters Satisfying Safety Properties**](https://www.doi.org/10.1007/978-3-642-24288-5_5). RP 2011: 31-44, 2011.

[JLR15] Aleksandra Jovanovic, Didier Lime, Olivier H. Roux. [**Integer Parameter Synthesis for Real-Time Systems**](https://www.doi.org/10.1109/TSE.2014.2357445). IEEE Transactions on Software Engineering 41(5): 445–461, 2015.

[NPP18] Hoang Gia Nguyen, Laure Petrucci, Jaco van de Pol. [**Layered and Collecting NDFS with Subsumption for Parametric Timed Automata**](https://www.doi.org/10.1109/ICECCS2018.2018.00009). ICECCS 2018: 1-9, 2018.
