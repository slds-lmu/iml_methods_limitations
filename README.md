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
1. Recommended: Check if the PDF compiles with R `bookdown::render_book('./', 'bookdown::pdf_book')`
1. Recommended: Check if the Website compiles with R `bookdown::render_book('./', 'bookdown::gitbook')`
1. Add changes to be commited `git add your-file.Rmd`
1. Commit your changes `git commit -m 'some meaningful description of your changes'`
1. Push to your Github repository `git push origin firstname_lastname`
1. If you haven't created a pull request already, create a pull request on github from your branch (firstname_lastname) to the master branch
1. In the name of the Pull Request, write "[WIP]" in the end, which stands for work in progress.

If its the end of a sprint, also do the following:
1. Assign your supervisor and another student as reviewers
1. Incorporate reviewer feedback (means repeating steps 1 to 7)
1. Congrats! Your changes are now merged with the master

Additional tips

- If your code relies on some package, make sure it is added it in the DESCRIPTION file as dependency.
- Citations go into book.bib and can be cited in your .Rmd file via `

## Workflow during group phase

- Create a new branch, e.g. "pdp_chapter"
- Open a pull request from pdp_chapter to the master and add "[WIP]" in the end of the pull request title
- Each student of your team directly works on this branch and commits and pushes changes to e.g. pdp_chapter.
- At the end of the group phase, remove the "[WIP]" from the title and assign one of the supervisors as reviewers.

Make sure you don't get merge conflicts when working on the same file.
You can minimize merge conflicts by
- splitting the chapter into subchapter and each working on the subchapters
- dividing the group into some who work on the slides, some on the chapter
- have a good communication and work sequentially on the chapter


## How to cite stuff
Put bib file into book.bib

## Useful Links

- rmarkdown cheatsheet https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf
- More on Bookdown https://bookdown.org/yihui/bookdown/

