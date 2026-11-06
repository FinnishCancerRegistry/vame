- Make your own branch for inspection by a human. Do not make a pull request.
- As the first commit in the branch, increment the dev version in DESCRIPTION.
  E.g. 1.2.0 -> 1.2.0.9000 and 1.2.0.9000 -> 1.2.0.9001.
- Write news following the method in the repository. E.g. via
  @codedoc_comment_block news("<fun>", "<date>", "<version>").
  You will be told what the version of the next release will be. If you are not
  told, use the dev version as the version in the news and remind the human
  that you did not know the version of the next release.
- Update documentation in @codedoc_comment_block blocks when needed.
- Update also roxygen blocks when needed. E.g. when the behaviour of an argument
  changes or a new one is added.
- All object names use lowercase_snake_case.
- If it is ever necessary to use a constant, write a function which returns
  the constant.
  + Unless creating the constant takes more than a few seconds for whatever
    reason --- then create the constant as an internal "dataset" via
    dev/01_data.R and usethis::use_data(internal = TRUE).
  + If this is not feasible for some unusual reason, then define the constant
    outside of all functions into the package's own environment in
    UPPERCASE_SNAKE_CASE.
