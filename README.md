# Seminar on Limitations of Interpretable Machine Learning Methods

This project explains the limitations of current approaches in interpretable machine learning, such as partial dependence plots (PDP, Accumulated Local Effects (ALE), permutation feature importance, leave-one-covariate out (LOCO) and local interpretable model-agnostic explanations (LIME).
All of those methods can be used to explain the behavior and predictions of trained machine learning models.
The interpretation methods might not work well in the following cases:

- if a model models interactions (e.g. when a random forest is used)
- if features strongly correlate with each other
- if the model does not correctly model causal relationships
- if parameters of the interpretation method are not set correctly

## Get started

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

Step 3: Create a new branch in the repository and push it. Please exchange firstname and lastname with your actual name so that you have your personal branch.

RStudio: In the 'Git' tab click on 'New Branch' and type the branch name (firstname_lastname scheme). Leave the other settings as is.
```
git checkout -b 'firstname_lastname'
git push -u origin firstname_lastname
```

## Workflow

1. Get the latest changes from the main repository `git pull origin master`
1. Make some changes on your machine
1. Check if the PDF compiles with R `bookdown::render_book('./', 'bookdown::pdf_book')`
1. Check if the Website compiles with R `bookdown::render_book('./', 'bookdown::gitbook')`
1. Add changes to be commited `git add your-file.Rmd`
1. Commit your changes `git commit -m 'some meaningful description of your changes'`
1. Push to your Github repository `git push origin firstname_lastname`
1. Create a pull request on github from your branch (firstname_lastname) to the master branch


Additional tips

- If your code relies on some package, make sure it is added it in the DESCRIPTION file as dependency.

## How to cite stuff
Put bib file into book.bib


https://bookdown.org/yihui/bookdown/

