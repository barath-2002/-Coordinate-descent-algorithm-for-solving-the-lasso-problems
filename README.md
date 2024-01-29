# -Coordinate-descent-algorithm-for-solving-the-lasso-problems

ST 443: Group Project Instruction
Due by 12 (noon), 8 December, 2023
1 Problems
1.1 Real World Data
The first part of the project is to apply statistical machine learning techniques on some real
datasets. The students are expected to find the data sets they are interested in from any
resource and evaluate the sample performance of different regression or (and) classification
approaches we have covered in class. I suggest the report includes
• Description of the data and the questions that you are interested in answering.
• Review of some of the approaches you tried.
• Summary of the final approach you used and the reason why you chose that approach.
• Summary of the results and conclusion.
1.2 Coordinate descent algorithm for solving the lasso problems
Tseng (1988) [see also Tseng (2001)] considers minimizing functions of the form
f(β1, . . . , βp) = g(β1, . . . , βp) +X
p
j=1
hj (βj ),
where g(·) is differentiable and convex, and the hj (·)’s are convex. Here each βj cannot be
overlapping. The author shows that coordinate descent converges to the minimizer of f. The
project is to apply coordinate descent type of algorithm on penalized regression problems,
e.g. the lasso in (1) and elastic net in (2).
Suppose the p predictors and response are standardized to have mean zero and variance
1. Take the lasso problem in (1) as an example.
1
2n
Xn
i=1
