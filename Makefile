# Default target
all: clean convert

# Directories
main = ../../src/converter.hs
ex1 = ./Examples/Example\ 1
ex2 = ./Examples/Example\ 2
ex3 = ./Examples/Example\ 3
ex4 = ./Examples/Example\ 4
grocery = ./Examples/Grocery\ Guide

# Clean target to remove output.tex files from specified directories
clean:
	rm -f $(ex1)/output.tex
	rm -f $(ex2)/output.tex
	rm -f $(ex3)/output.tex
	rm -f $(ex4)/output.tex
	rm -f $(grocery)/output.tex

# Convert target to run the main Haskell function with specified inputs
convert:
	cd $(ex1) && echo "ex1.md" | runhaskell $(main)
	cd $(ex2) && echo "ex2.md" | runhaskell $(main)
	cd $(ex3) && echo "ex3.md" | runhaskell $(main)
	cd $(ex4) && echo "ex4.md" | runhaskell $(main)
	cd $(grocery) && echo "groceryGuide.md" | runhaskell $(main)

.PHONY: all clean convert
