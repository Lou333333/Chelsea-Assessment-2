####
library(usethis)
use_git()
use_github(private=FALSE)
system("git status")
browse_github()
usethis::use_git_remote("origin", url = NULL,
                         overwrite = TRUE)
#### Commit to git