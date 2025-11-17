# Optimization-and-Regularization-Under-Arbitrary-Objectives
Associated files for: Optimization and Regularization Under Arbitrary Objectives. This repository contains the PDF report, as well as the associated presentation pdf.

Abstract:

This study investigates the limitations of applying Markov Chain Monte Carlo (MCMC) methods to arbitrary objective functions, focusing on a two-block MCMC framework which alternates between Metropolis-Hastings and Gibbs sampling. While such approaches are often considered advantageous for enabling data-driven regularization, we show that their performance critically depends on the sharpness of the employed likelihood form. By introducing a sharpness parameter and exploring alternative likelihood formulations proportional to the target objective function, we demonstrate how likelihood curvature governs both in-sample performance and the degree of regularization inferred by the training data. Empirical applications are conducted on reinforcement learning tasks: including a navigation problem and the game of tic-tac-toe.

The study concludes with a separate analysis examining the implications of extreme likelihood sharpness on arbitrary objective functions stemming from the classic game of blackjack, where the first block of the two-block MCMC framework is replaced with an iterative optimization step. The resulting hybrid approach achieves performance nearly identical to the original MCMC framework, indicating that excessive likelihood sharpness effectively collapses posterior mass onto a single dominant mode.
