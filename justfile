build:
    stack build

# Format every file in the project. Call `just format check` to check formatting instead.
format mode="inplace":
  ormolu -m {{mode}} $(git ls-files | grep '\.hs$')
