One hashtag
Two hashtags
Double star either side of word
Arrow to draw attention to statement
Click [Here](link)
[Title](link)


<img align="left" src="images/vnso_logo.png" width=42%> <img align="right" src="images/vangov_logo.png">

<br><br><br><br><br><br><br><br><br><br><br><br><br>

# A Reproducible Analytical Pipeline for the Vanuatu National Statistics Office Trade Statistics reporting&nbsp;

Producing official statistics for publications is a key function of many teams across Government. It’s a time consuming and meticulous process to ensure that statistics are accurate and timely. With open source software becoming more widely used, there’s now a range of tools and techniques that can be used to reduce production time, whilst maintaining and even improving the quality of the publications.  

> The Reproducible Analytical Pipeline (RAP) is an alternative production methodology for automating the bulk of steps involved in creating a statistical report. 

To know more about the Vanautu National Statistics Office. Click [Here](https://vnso.gov.vu)

The Vanuatu National Statistics Office collects, compiles and disseminates detailed trade data (by commodity category and by trading partner) for international merchandise trade. The Statistics Office data dissemination system, offer free-access to official trade statistics as reported by Vanuatu. Previously it took the Trade Statistics team forty days to produce a monthly report, with the RAP in place it now takes five (reduction of 87.5%). 

> International merchandise trade statistics record all goods which add to, or subtract from, the stock of material resources of a country by entering (as imports) or leaving (as exports) its economic territory.

To know more about International Merchandise Trade Statistics in Vanuatu. Click [Here](https://vnso.gov.vu/index.php/en/statistics-by-topic/trade)

## Resources list
- [Introduction to Reproducible Analytical Pipelines](https://ukgovdatascience.github.io/rap_companion/)
    * Free RAP Course [here](https://www.udemy.com/course/reproducible-analytical-pipelines/) 
    * Blog on RAP [here](https://dataingovernment.blog.gov.uk/2017/03/27/reproducible-analytical-pipeline/)
    * Video of Reproducible analysis using R [here](https://www.youtube.com/watch?v=qvPDE4ppAns) 

- [Introduction to International Merchandise Trade Statistics](https://www.oecd.org/sdd/its/international-merchandise-trade-statistics.htm)
    * Free international merchandise trade Course [here](https://www.unsdglearn.org/courses/e-learning-on-international-merchandise-trade-statistics/) 
    * United Nations Trade Statistics [here](https://unstats.un.org/unsd/trade/default.asp)
    * Video by the World Trade Organisation using R [here](https://www.youtube.com/watch?v=kHQJkeOxAKM) 

![The new Reproducible Analytical Pipeline](images/NewPipeline.png)

## pre-commit workflow installation

The python programming language has leveraged [git's hook functionality](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) to create a [pre-commit](https://pre-commit.com/) library. It's a library that allows us to select particular hooks (tasks) that will be triggered every time we `commit` changes to our repository locally. The R [precommit](https://lorenzwalthert.github.io/precommit/index.html) package was created to help R users have the same functionality. There are many [python](https://pre-commit.com/hooks.html) and [R](https://lorenzwalthert.github.io/precommit/articles/available-hooks.html) that are tailored to these languages.

The current trades statistics repository has been set up with an R precommit workflow, to install it follow these steps:

1. Clone the repository to your local computer if you haven't already - see [help documentation for this](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
2. Open up R (either in the terminal or RStudio) and install the precommit package with: `install.packages("precommit")`
3. Check python 3 is installed on your computer by running `python --version` in the terminal
    - If it isn't see [installation instructions](https://realpython.com/installing-python/)
4. Set the working directory in R to your repository location
5. Run `precommit::use_precommit()` in R and the precommit workflow will be installed. This may take a few minutes the first time
    - Now you are good to go. You can add/remove hooks in the `.pre-commit-config.yaml` file
    - Note that we also have `.lintr` file that has some specific settings for the `lintr` hook
