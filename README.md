# MKT4320BGSU

MKT4320BGSU is an R package developed for use in MKT 4320: Marketing Analytics
at the Schmidthorst College of Business, Bowling Green State University.

The package provides student-friendly wrapper functions and teaching datasets
for applied marketing analytics, with an emphasis on interpretation and
conceptual understanding.

------------------------------------------------------------
INSTALLATION
------------------------------------------------------------

Install the development version from GitHub:

Recommended:
  pak::pak("jeffn/MKT4320BGSU")

Or:
  remotes::install_github("jeffn/MKT4320BGSU", dependencies = TRUE)

------------------------------------------------------------
USAGE
------------------------------------------------------------

Load the package in a standard R session:

  library(MKT4320BGSU)

Most functions are designed to work with fitted model objects created by
students using base R or commonly used modeling packages (e.g., glm,
nnet::multinom). The package focuses on evaluation, interpretation, and
visualization rather than model estimation itself.

------------------------------------------------------------
EXAMPLES
------------------------------------------------------------

Load a teaching dataset:

  data(directmktg)

Fit a logistic regression model:

  fit <- glm(buy ~ age + gender + salary,
             data = directmktg,
             family = binomial)

Evaluate the fitted model:

  eval_logistic(fit)

------------------------------------------------------------
INTENDED AUDIENCE
------------------------------------------------------------

This package is intended primarily for:

- Undergraduate students enrolled in marketing analytics courses
- Instructors teaching marketing research, analytics, or applied modeling

The package prioritizes clear defaults, interpretable output, and pedagogical
clarity over maximum flexibility or computational efficiency.

------------------------------------------------------------
DEVELOPMENT STATUS
------------------------------------------------------------

This package is under active development and is used in live courses.
Function names, arguments, and outputs may evolve between semesters.

Where possible, backward compatibility is maintained. Deprecated functions
emit warnings and direct users to newer alternatives.

This package is not currently intended for submission to CRAN.

------------------------------------------------------------
LICENSE
------------------------------------------------------------

MIT License

Copyright (c) 2025 Jeffrey Meyer

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
