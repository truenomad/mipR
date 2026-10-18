# Contributing to mipR

## Coding conventions

- Write all package code in R.
- Use the base pipe `|>` only. Do not use `%>%`.
- Call functions from other packages with explicit namespaces, as
  `package::function()`.
- Do not call `library()` or `require()` in package source code. Declare
  dependencies in DESCRIPTION instead.
- Keep lines to a maximum of 80 characters.
- Use snake_case for function and variable names.
- Give objects clear, descriptive names.
- Document every exported function with roxygen2.
- Write unit tests with testthat (third edition) in `tests/testthat/`.
- Use `here::here()` to build paths in project-level development scripts.
  Package code itself should not depend on here.
- Add a dependency only when the code that needs it is added.

## Workflow

- Run `devtools::document()` after changing roxygen comments.
- Run `devtools::check()` before opening a pull request.
- Edit `README.md` directly.
- Add a bullet to `NEWS.md` for any user-facing change.
