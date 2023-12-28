# TCP
Welcome to the GitHub repository for the article titled "Change Points Estimation based on the Multivariate Time Series Trend Detection with Applications to the Economic and Environment Data". This repository contains the codes that were used in the article to identify change points (TCPs) and perform simulations and examples.

The primary purpose of sharing these codes is to promote reproducibility and enable readers to utilize the algorithms and replicate the results presented in the article. The repository includes R codes that can be easily accessed and utilized.

Here is a brief overview of the contents available in this repository:

1. Algorithms: The repository includes the implementation of all algorithms used for identifying TCPs in multivariate time series data. The code allows users to apply the algorithms to their own datasets and obtain accurate change point detection results.

2. Simulation: The repository also includes simulation codes used in the article. 

3. Real Data: Additionally, the repository provides codes that demonstrate the application of the TCP identification algorithm to three real datasets discussed in the paper. Users can examine these examples to gain a better understanding of how the algorithm works and its effectiveness in different cases.

4. Example 1: The repository includes an example showcasing a bivariate time series with mirrored univariate time series. This example demonstrates the performance of our algorithm in the presence of two change points and a negative correlation between the variables. In accordance with the point mentioned in the article, we have assigned a negative weight to one of the variables. The PELT algorithm accurately identifies the change points, correctly recognizing two points. Conversely, when the algorithm is implemented with positive coefficients, it fails to distinguish any change points due to the neutralizing effect of the negative correlation between the variables.

We hope that the availability of these codes will facilitate further research and exploration in the field of change point detection in multivariate time series data. Feel free to explore the repository, use the codes, and contribute to the advancement of this research area.

Please note that proper attribution and citation of the original article is appreciated when using these codes for research or academic purposes.

Thank you for your interest in our work, and we hope you find these codes useful.
