# monthly-budget

For me, by me, to help me keep track of monthly expenses and how they compare to my income for that month. It'd be cool to make this more general purpose, but I built it to specifically work with the CSV file I can download from my personal bank, which details transactions for a specified time period. 

The CSV *needs* to have an _"Amount"_ column of positive and/or negative numbers, indicating cash in vs cash out. These are separated lists of debits and credits, made unique according to the _"Description"_. Duplicates are totaled. A missing _"Description"_ column will be handled with a default value, so total cash out vs cash in can still be compared. With my bank, cashed checks have an empty description, so it fall backs to the _"Transaction Type"_ column which in this case will say _"CHECK"_. The _"Transaction Type"_ is also used to filter out _"TRANSFERS"_ from debits credits. This prevents transferring from checking -> savings as being represented as a debit.

The cash flow report is then written as a CSV file, placed in the Downloads folder. My long-term vision for this application includes saving the CSV files (locally or in a remote db), so it can provide further insight and analysis into how expenses and/or income changed over a period of time. At that point, I'd also like to have a front end to display those data trends.

## Development

* Install Haskell platform (this will take a while) to get `stack`, `ghc` and `cabal-install`. I believe I did this with homebrew, but there are a few ways. Start here: https://www.haskell.org/platform/
* `stack build` from project root to compile
* `stack exec monthly-budget.exe` from project root to execute the program

## IDE setup for VS CODE

* Install `haskell-ide-engine` repo from GH
* Install `Haskell Syntax Highlighting` and `Haskell Language Server`
* Make these changes to settings.json:
```
    "languageServerHaskell.hieExecutablePath": "~/.local/bin/hie",      //--> ide engine binary 
    "languageServerHaskell.hlintOn": true,                              //----> so we don't need a linter extension too
    "languageServerHaskell.trace.server": "messages",                   //---> debugging the IDE I think?
 ```
* I'm sure there was a bunch of other stupid stuff I had to do & figure out that I don't remember.
