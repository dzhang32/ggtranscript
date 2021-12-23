## Did you miss the previous step? The one about creating your package
rstudioapi::navigateToFile(usethis::proj_path("dev", "01_create_pkg.R"))

## ********************
## Setup Git and GitHub
## ********************

## Note that Bioconductor doesn't allow *.Rproj files
## So we have to ignore it before anything else
usethis::use_git_ignore("*.Rproj")
usethis::use_git() ## Choose the option to make the commit, then to restart RStudio

## After the restart, continue by connecting your local git repository to
## GitHub. You might want to use the `organisation` and `private` arguments
args(usethis::use_github)

## If this is your first time running use_github(), you might have to also run:
usethis::gh_token_help()
## The above command will suggest that you read more at
## https://usethis.r-lib.org/articles/articles/git-credentials.html
## which contains the latest recommendations by the usethis authors for
## configuring your R to GitHub connection.
usethis::create_github_token()
gitcreds::gitcreds_set()
## Type your GitHub token, not your password! Otherwise you might run into this
## problem: https://github.com/r-lib/usethis/issues/1347

## In some situations, gitcreds::gitcreds_set() might not work. For example,
## gitcreds::gitcreds_set() is not supported on Linux as discussed at
## https://github.com/r-lib/gitcreds/issues/29. In these situations,
## you have to rely on the old workflow of editing your
## .Renviron file with contents like (note the empty line at the end!):
# GITHUB_PAT=YOUR_40_CHARACTERS_TOKEN
#
usethis::edit_r_environ()
## Then re-start your R session.
rstudioapi::restartSession()
## Editing the .Renviron is strongly discouraged now since it stores as
## simple text your GitHub personal access token (PAT) instead of the
## more secure approach provided by gitcreds.

## Now run use_github()
usethis::use_github()
## Follow any prompts, such as running on the terminal:
## git push --set-upstream origin master

## Move to the next step: setting up your package core files
rstudioapi::navigateToFile(usethis::proj_path("dev", "03_core_files.R"))

## This template was made using https://lcolladotor.github.io/biocthis/
