# Monthly Budget

For me, by me, to help me keep track of monthly expenses and how they compare to my income for that month. It'd be cool to make this more general purpose, but I built it to specifically work with the CSV file I can download from my personal bank, which details transactions for a specified time period.

The CSV _needs_ to have an _"Amount"_ column of positive and/or negative numbers, indicating cash in vs cash out. These are separated lists of debits and credits, made unique according to the _"Description"_. Duplicates are totaled. A missing _"Description"_ column will be handled with a default value, so total cash out vs cash in can still be compared. With my bank, cashed checks have an empty description, so it fall backs to the _"Transaction Type"_ column which in this case will say _"CHECK"_. The _"Transaction Type"_ is also used to filter out _"TRANSFERS"_ from debits credits. This prevents transferring from checking -> savings as being represented as a debit.

The cash flow report is then written as a CSV file, placed in the Downloads folder. My long-term vision for this application includes saving the CSV files (locally or in a remote db), so it can provide further insight and analysis into how expenses and/or income changed over a period of time. At that point, I'd also like to have a front end to display those data trends.

## Development

- Install [Haskell platform](https://www.haskell.org/platform/) (this will take a while) to get `stack`, `ghc` and `cabal-install`.
  - The recommended Mac installation is with [gchup](https://www.haskell.org/ghcup/) to get ghc and cabal-install, and following the directions [here to get stack](https://docs.haskellstack.org/en/stable/README/). **Note:** _I believe I did this with homebrew originally, I'm sure there are a few ways._
- `stack build` from project root to compile
- `stack exec monthly-budget-exe` from project root to execute the program

## IDE setup for VS CODE

- Install [the official Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) which comes with the language server and the IDE engine. **Note:** ghc, stack and cabal-install must be installed and on the PATH. _See 'Requirements' section._
- Install [Haskell Syntax Highlighting extension](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)

## Language Documentation

Haskell's docs are weird, and sort of scattered. Places I frequent are:

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
  - _Yes, it's a great introduction and tutorial. It's also the place I usually jump back to for a refresher on syntax._
- [Haskell's base Package](https://hackage.haskell.org/package/base)
  - _Which includes_ [Prelude](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html)
- [Hoogle](https://hoogle.haskell.org/)
  - _Haskell API search engine._
