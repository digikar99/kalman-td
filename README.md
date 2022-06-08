# Kalman TD

A python and common lisp based implementation of Kalman TD based on

1. [A Unifying Probabilistic View of Associative Learning, Gershman (2015)](https://doi.org/10.1371%2Fjournal.pcbi.1004567)
2. [Kalman Temporal Differences, Geist and Pietquin (2014)](https://doi.org/10.48550/ARXIV.1406.3270) - specifically algorithm 1
3. [Kalman Filter - Wikipedia](https://en.wikipedia.org/wiki/Kalman_filter)

The python version was originally developed as a course project in Computational Cognitive Science (CS786, IIT Kanpur) under Prof. Nisheeth Srivastava, in collaboration with Anish Thankachan and Diksha Yadav. Since then, a [python/simulations_helper.py](./python/simulations_helper.py) has been added to help simplify the simulations. The report (based on the python version) is available [here](./python/report.pdf).

The main interface (similar in python and common lisp) comprises of:
- file: KalmanTD.py / kalman-td.lisp
- class: KalmanTD / kalman-td
- function: predict_reward / predict-reward
- function: update / update
- function: default_kalman_td / default-kalman-td

About 9 simulations covered in Gershman (2015) have been coded up in the following files. These also illustrate example usage of the above classes and functions.
- [python/simulations.ipynb](./python/simulations.ipynb): the full versions without any simplifications
- [common-lisp/simulations.ipynb](./common-lisp/simulations.ipynb): based on an abstraction that separates out stimuli-reward generation and processing from the actual high level experimental paradigm and plotting code
- [python/simulations_using_helper.py](./python/simulations_using_helper.py): originally developed to simplify the full simulations in the simulations.ipynb; this is incomplete.

Recommended way to run the simulations in common lisp is `(in-package :kalman-td)`. In other cases, one may need to play around with `*array-element-type*`.

## Other Implementations

- Original MATLAB implementation: https://github.com/sjgershm/KTD
- Another python implementation: https://github.com/jusporrer/Kalman-TD-Model


