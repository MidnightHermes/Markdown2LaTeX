# Haskell-based Markdown to LaTeX Converter  

## Introduction  
This program converts Markdown files \(`.md`\) to LaTeX files \(`.tex`\). The aim is to make LaTeX more accessible, enabling users to write in Markdown -- a language they are familiar with -- and still leverage the advanced formatting power of LaTeX. This approach helps bridge the gap between Markdown’s user-friendliness/intuitiveness and LaTeX's comprehensive typesetting features.
Currently, the program supports only [basic Markdown syntax](https://www.markdownguide.org/basic-syntax/) excluding reference-style links, offering a foundation for future enhancements.

## Pitfalls / Noteworthy Features  
To be clear, all subtle rule/syntax changes/additions made to appease my program will still compile regularly in a normal Markdown compiler. To prove it, this `.md` file has been converted using my converter! See `README.tex` \(The only exception I have found is escape characters in monospace/code as you will see in this file\)
- Because of the pattern matching algorithm when parsing each line, a list item, heading, horizontal rule, or code block must be followed by a return character represented by two spaces \(`  `\) or a newline character represented in the traditional fashion \(`\\n`\).  
- Should any form of error occur when converting, the program will place a \(hopefully\) detailed error message at the top of the `output.tex` file between the prelude and the actually converted body. This should be properly interpreted by any LaTeX compiler as text.  
- Because numbers followed dots \(`\.`\) are used to represent lists, you will have to escape the dot when ending a sentence with a number.  
    + e.g., `I like #1\.` is not valid, but `I like #1\\\.` is.  
- Similarly, because parentheses and braces are used to represent Hyperlinks and Image references, you may need to escape them when using them in text.  

## Installation / Usage  
To install the converter, follow these steps:
1. Clone the repository: `git clone \[repository URL\]`  
2. Navigate to the repository directory: `cd \[repository name\]`  
3. Run the compiler: `make convert`  
    * Run `make clean` to remove all the `output.tex` files (NOTE: The converter overwrites `output.tex` anyways)  
5. Compile your new `output.tex` file!  

## How it Works  
The conversion process involves a step-by-step lexical analysis of the Markdown file. Each line is analyzed and represented in a custom data structure, capturing the syntactic patterns of Markdown \(e.g., headings denoted by hash marks\). This internal representation then undergoes contextual analysis, considering each line’s relation to its neighbors, ensuring coherent and accurate conversion into LaTeX; this is specifically important for multi-line structures such as lists.

## Contributing  
This project is by no means sophisticated or clean, it was simply a final project for one of my courses. That being said, feel free to refine, add to, or alter it in any way! This project follows the GNU General Public License 3\.0 which means you are welcome to distribute the program alongside any of your alterations so long as you credit the original author \(me: Hermès Henry\) and maintain the GNU General Public License 3\.0 as well.

## License  
This project is licensed under the GNU General Public License 3\.0 -- see the `LICENSE` file for details.

## Acknowledgement  
This project was done as part of a final project for a functional programming course taught by Dr. Andrew Polonksy at Appalachian State University; huge thanks to him for everything he taught me!

## Credit / Contact Information  
My name is Hermès Henry and I made this project during my time as a Computer Science student at Appalachian State University.
For more information, contact me by [email](mailto:henryhe@appstate.edu)\.
