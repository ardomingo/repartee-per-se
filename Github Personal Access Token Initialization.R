#Set github personal access token (PAT)


## set your user name and email:
usethis::use_git_config(user.name = "ardomingo", user.email = "andres.domingo@outlook.com")

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)

## set personal access token:
credentials::set_github_pat("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")
