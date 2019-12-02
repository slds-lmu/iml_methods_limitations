# Limitations of Interpretable Machine Learning Methods

This project explains the limitations of current approaches in interpretable machine learning, such as partial dependence plots (PDP, Accumulated Local Effects (ALE), permutation feature importance, leave-one-covariate out (LOCO) and local interpretable model-agnostic explanations (LIME).
All of those methods can be used to explain the behavior and predictions of trained machine learning models.
The interpretation methods might not work well in the following cases:

- if a model models interactions (e.g. when a random forest is used)
- if features strongly correlate with each other
- if the model does not correctly model causal relationships
- if parameters of the interpretation method are not set correctly

## How this book came about

This book is the result of a student seminar for Master Statistics and Master Data Science at the LMU in the summer semester 2019.
Each student in the seminar wrote about a specific limitation of an interpretable machine learning method.

## How to build the book

Step 0: Prerequisites

Make sure you have git and R up and running on your computer.

Step 1: Clone the repository to your machine

With RStudio: https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN

With command-line:
```
git clone git@github.com:compstat-lmu/iml_methods_limitations.git
```

Step 2: Install dependencies

Start R in the project folder:

```
install.packages("devtools")
devtools::install_dev_deps()
```

Step 3: Render the book (R commands)

```{r}
# HTML
bookdown::render_book('./', 'bookdown::gitbook')
# PDF
bookdown::render_book('./', 'bookdown::pdf_book')
```


