Problem
-------

BNP Paribas Cardif is a global personal insurance provider. They want to
automate the process of determining whether a claim can automatically be
accepted or needs to be checked further. The problem is then to take the
features available to BNP Paribas Cardif and output the probability that
a claim can be accepted.

Data
----

This problem comes with a training data set and a test data set
(<https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/data>).
The training data set contains 133 variables, 21 of which are
categorical variables and 112 of which are numerical variables. 2 of the
categorical variables include the ID and the target (whether or not the
claim is acceptable).

    load(file="train.RData")
    str(train, list.len=10)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    114321 obs. of  133 variables:
    ##  $ ID    : int  3 4 5 6 8 9 12 21 22 23 ...
    ##  $ target: int  1 1 1 1 1 0 0 1 0 1 ...
    ##  $ v1    : num  1.336 NA 0.944 0.797 NA ...
    ##  $ v2    : num  8.73 NA 5.31 8.3 NA ...
    ##  $ v3    : chr  "C" "C" "C" "C" ...
    ##  $ v4    : num  3.92 NA 4.41 4.23 NA ...
    ##  $ v5    : num  7.92 9.19 5.33 11.63 NA ...
    ##  $ v6    : num  2.6 NA 3.98 2.1 NA ...
    ##  $ v7    : num  3.18 NA 3.93 1.99 NA ...
    ##  $ v8    : num  0.0129 2.3016 0.0196 0.1719 NA ...
    ##   [list output truncated]
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 133
    ##   .. ..$ ID    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ target: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ v1    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v2    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v3    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ v4    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v5    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v6    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v7    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ v8    : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. .. [list output truncated]
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

Solution Outline
----------------

1.  Data preprocessing

<!-- -->

1.  For categorical variables, if there are more than 100 possible
    values remove it (i.e. v22 has 900+ possible values)
2.  For each variable, go through and mark each of the outliers
    (lowest/highest 5%) as NA
3.  For each variable, find the average value and replace all NA's with
    that average value

<!-- -->

1.  Feature Engineering (possible methods)

<!-- -->

1.  Separate categorical variables into binary variables for each
    possible value
2.  Do univariate regression to see which variables have the highest
    correlation to acceptable claims

<!-- -->

1.  Model

<!-- -->

1.  Linear Regression

<!-- -->

1.  Cross-validation to tweak model

<!-- -->

1.  k-fold cross validation

Deliverables
------------

1.  Slide deck explaining process
2.  Code
