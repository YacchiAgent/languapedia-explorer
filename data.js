const languages = [
    {
        id: 'python',
        name: 'Python',
        year: 1991,
        creator: 'Guido van Rossum',
        useCase: 'Data Science, Web, AI',
        description: 'Famous for being easy to read and learn. It powers everything from Netflix to NASA.',
        tags: ['Dynamic', 'Easy', 'Popular'],
        category: 'AI',
        color: '#3776ab',
        example: `# Hello World in Python
# Using an f-string (The 'essence' of Python readability)
name = "World"
print(f"Hello, {name}!")`,
        executionSteps: ['Source (.py)', 'Bytecode', 'PVM', 'Output'],
        history: [
            { year: 1989, event: 'Guido van Rossum begins work at CWI' },
            { year: 1991, event: 'First release: Python 0.9.0' },
            { year: 2000, event: 'Python 2.0: List comprehensions added' },
            { year: 2008, event: 'Python 3.0: Breaking changes for consistency' },
            { year: 2020, event: 'Python overtakes Java in popularity' }
        ],
        mascot: 'assets/python_mascot.png',
        mascotQuote: 'Indent your code, not your life!',
        funEpisode: "Named after Monty Python's Flying Circus."
    },
    {
        id: 'javascript',
        name: 'JavaScript',
        year: 1995,
        creator: 'Brendan Eich',
        useCase: 'Web, Mobile, Server',
        description: 'The language of the web. It makes websites interactive.',
        tags: ['Web', 'Active', 'Fast'],
        category: 'Web',
        color: '#f7df1e',
        example: `// Hello World in JavaScript
// Using a template literal for modern web style
const greet = "World";
console.log(\`Hello, \${greet}!\`);`,
        executionSteps: ['Source', 'Parser', 'JIT (V8)', 'Run'],
        history: [
            { year: 1995, event: 'Created in 10 days at Netscape' },
            { year: 1997, event: 'Became an ECMA Standard' },
            { year: 2005, event: 'AJAX changes the web forever' },
            { year: 2009, event: 'Node.js brings JS to the server' },
            { year: 2015, event: 'ES6 Modernization' }
        ],
        mascot: 'assets/js_mascot.png',
        mascotQuote: 'I run on 98% of all websites.',
        funEpisode: "It was originally called 'Mocha'."
    },
    {
        id: 'html',
        name: 'HTML',
        year: 1993,
        creator: 'Tim Berners-Lee',
        useCase: 'Web Structure',
        description: 'The skeleton of the web. It defines the structure of every page.',
        tags: ['Markup', 'Web', 'Core'],
        category: 'Web',
        color: '#e34f26',
        example: `<!-- Hello World in HTML -->
<!-- The essence is semantic structure -->
<main>
  <h1>Hello, World!</h1>
  <p>This is the web.</p>
</main>`,
        executionSteps: ['Markup', 'DOM Tree', 'Pixel Render', 'Page'],
        history: [
            { year: 1989, event: 'Tim Berners-Lee invents the Web' },
            { year: 1993, event: 'HTML 1.0 formally published' },
            { year: 1995, event: 'HTML 2.0 (Standardized)' },
            { year: 2014, event: 'HTML5 is finalized' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I give the web its shape.',
        funEpisode: "The first website is still online at CERN."
    },
    {
        id: 'css',
        name: 'CSS',
        year: 1996,
        creator: 'Håkon Wium Lie',
        useCase: 'Web Styling',
        description: 'The paint and fashion of the web. It makes HTML look good.',
        tags: ['Style', 'Design', 'Web'],
        category: 'Web',
        color: '#1572b6',
        example: `/* Hello World using CSS */
/* The essence is selecting and styling */
body::before {
  content: "Hello World";
  color: blue;
  font-size: 2rem;
}`,
        executionSteps: ['Style Sheet', 'CSSOM', 'Layout Engine', 'Paint'],
        history: [
            { year: 1994, event: 'Proposed by Håkon Wium Lie' },
            { year: 1996, event: 'CSS Level 1 Released' },
            { year: 1998, event: 'CSS 2 includes positioning' },
            { year: 2011, event: 'CSS 3 Modules (Flexbox, Grid)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Without me, the web is ugly.',
        funEpisode: "Before CSS, people used <table> for layout!"
    },
    {
        id: 'c_lang',
        name: 'C',
        year: 1972,
        creator: 'Dennis Ritchie',
        useCase: 'OS Kernels, Embedded',
        description: 'The mother of modern languages. Fast, powerful, and dangerous.',
        tags: ['Systems', 'Fast', 'Low-level'],
        category: 'Systems',
        color: '#a8b9cc',
        example: `/* Hello World in C */
/* The essence: Direct memory access & efficiency */
#include <stdio.h>

int main() {
    printf("Hello, World!\\n"); // Standard I/O
    return 0;
}`,
        executionSteps: ['Source', 'Compiler', 'Object Code', 'Machine Code'],
        history: [
            { year: 1972, event: 'Created at Bell Labs' },
            { year: 1978, event: 'K&R "The C Programming Language"' },
            { year: 1989, event: 'ANSI C Standard (C89)' },
            { year: 1999, event: 'C99 Standard (new types)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I built the operating system you are using.',
        funEpisode: "Unix was rewritten in C, making it portable."
    },
    {
        id: 'java',
        name: 'Java',
        year: 1995,
        creator: 'James Gosling',
        useCase: 'Enterprise, Android',
        description: 'Reliable, portable, and massive. The backbone of big business.',
        tags: ['Enterprise', 'OOP', 'Portable'],
        category: 'Enterprise',
        color: '#007396',
        example: `// Hello World in Java
// The essence: Everything is a class
public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}`,
        executionSteps: ['Source (.java)', 'Bytecode (.class)', 'JVM', 'Run'],
        history: [
            { year: 1991, event: 'Oak project started' },
            { year: 1995, event: 'Java 1.0 Released' },
            { year: 2004, event: 'Java 5 (Generics added)' },
            { year: 2009, event: 'Oracle acquires Sun Microsystems' },
            { year: 2014, event: 'Java 8 (Lambdas added)' }
        ],
        mascot: 'assets/java_mascot.png',
        mascotQuote: 'Write Once, Run Anywhere.',
        funEpisode: "Named after the coffee consumed by the developers."
    },
    {
        id: 'cpp',
        name: 'C++',
        year: 1985,
        creator: 'Bjarne Stroustrup',
        useCase: 'Game Engines, High Perf',
        description: 'C with superpowers. Used for things that need to be blazingly fast.',
        tags: ['Games', 'Systems', 'Hard'],
        category: 'Systems',
        color: '#00599c',
        example: `// Hello World in C++
// The essence: Objects + Performance
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'CPU'],
        history: [
            { year: 1979, event: 'C with Classes started' },
            { year: 1985, event: 'C++ 1.0 Released' },
            { year: 1998, event: 'Standardized as ISO C++' },
            { year: 2011, event: 'C++11: The Modern Era' }
        ],
        mascot: 'assets/cpp_mascot.png',
        mascotQuote: 'I power your favorite games.',
        funEpisode: "Stroustrup picked C++ because ++ means 'increment' in C."
    },
    {
        id: 'rust',
        name: 'Rust',
        year: 2010,
        creator: 'Graydon Hoare',
        useCase: 'Systems, Security',
        description: 'Memory safety without a garbage collector. The modern systems choice.',
        tags: ['Safe', 'Modern', 'Systems'],
        category: 'Systems',
        color: '#dea584',
        example: `// Hello World in Rust
// The essence: Safety and Macros
fn main() {
    // println! is a macro, safe at compile time
    println!("Hello, World!");
}`,
        executionSteps: ['Source', 'Borrow Check', 'LLVM', 'Binary'],
        history: [
            { year: 2010, event: 'Mozilla announces Rust' },
            { year: 2015, event: 'Rust 1.0 Stable' },
            { year: 2016, event: 'Most Loved language on StackOverflow' },
            { year: 2021, event: 'Google supports Rust in Android' }
        ],
        mascot: 'assets/rust_mascot.png',
        mascotQuote: 'Safety first, speed always.',
        funEpisode: "Mascot is Ferris the Crab (Rustaceans)."
    },
    {
        id: 'go',
        name: 'Go',
        year: 2009,
        creator: 'Google',
        useCase: 'Cloud, Servers',
        description: 'Simple, efficient, and great for concurrency. Built for the cloud.',
        tags: ['Cloud', 'Simple', 'Fast'],
        category: 'Backend',
        color: '#00add8',
        example: `// Hello World in Go
// The essence: Simplicity (no classes)
package main
import "fmt"

func main() {
    fmt.Println("Hello, World!")
}`,
        executionSteps: ['Source', 'Compiler', 'Static Binary', 'Run'],
        history: [
            { year: 2007, event: 'Designed at Google (Pike, Thompson)' },
            { year: 2009, event: 'Open Sourced' },
            { year: 2012, event: 'Go 1.0 Released' },
            { year: 2016, event: 'Used for Docker and Kubernetes' }
        ],
        mascot: 'assets/go_mascot.png',
        mascotQuote: 'Gopher it!',
        funEpisode: "The mascot is a Gopher drawn by Renee French."
    },
    {
        id: 'ruby',
        name: 'Ruby',
        year: 1995,
        creator: 'Yukihiro Matsumoto',
        useCase: 'Startups, Web',
        description: 'Optimized for developer happiness. Reads like English.',
        tags: ['Friendly', 'Web', 'Magic'],
        category: 'Backend',
        color: '#cc342d',
        example: `# Hello World in Ruby
# The essence: Elegant and English-like
puts "Hello, World!"`,
        executionSteps: ['Source', 'Interpreter', 'Bytecode', 'Run'],
        history: [
            { year: 1993, event: 'Matz starts working on Ruby' },
            { year: 1995, event: 'Ruby 0.95 Released' },
            { year: 2005, event: 'Ruby on Rails revolutionizes web dev' },
            { year: 2020, event: 'Ruby 3.0: 3x Faster' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Matz is nice, so we are nice.',
        funEpisode: "Designed to be 'natural', not minimal."
    },
    {
        id: 'swift',
        name: 'Swift',
        year: 2014,
        creator: 'Apple',
        useCase: 'iPhone, Mac',
        description: 'The modern way to build apps for Apple devices.',
        tags: ['Apple', 'Mobile', 'Fast'],
        category: 'Mobile',
        color: '#f05138',
        example: `// Hello World in Swift
// The essence: Safe and clean
print("Hello, World!")`,
        executionSteps: ['Source', 'Compiler', 'Machine Code', 'Run'],
        history: [
            { year: 2010, event: 'Chris Lattner starts project' },
            { year: 2014, event: 'Shock announcement at WWDC' },
            { year: 2015, event: 'Open Sourced' },
            { year: 2019, event: 'SwiftUI Released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Goodbye Objective-C!',
        funEpisode: "Developed in secret for 4 years."
    },
    {
        id: 'kotlin',
        name: 'Kotlin',
        year: 2011,
        creator: 'JetBrains',
        useCase: 'Android, JVM',
        description: 'A better Java. Concise, safe, and fully compatible.',
        tags: ['Android', 'Modern', 'JVM'],
        category: 'Mobile',
        color: '#7f52ff',
        example: `// Hello World in Kotlin
// The essence: Concise (main is a function)
fun main() {
    println("Hello, World!")
}`,
        executionSteps: ['Source', 'Compiler', 'JVM Bytecode', 'Run'],
        history: [
            { year: 2011, event: 'Unveiled by JetBrains' },
            { year: 2016, event: 'Kotlin 1.0 Released' },
            { year: 2017, event: 'Google official Android language' },
            { year: 2019, event: 'Google goes "Kotlin First"' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Java without the boilerplate.',
        funEpisode: "Named after Kotlin Island near St. Petersburg."
    },
    {
        id: 'php',
        name: 'PHP',
        year: 1995,
        creator: 'Rasmus Lerdorf',
        useCase: 'Web Hosting, WordPress',
        description: 'The workhorse of the web. Easy to deploy, everywhere.',
        tags: ['Web', 'Easy', 'Legacy'],
        category: 'Backend',
        color: '#777bb4',
        example: `// Hello World in PHP
// The essence: Mixes directly with HTML
<?php
  echo "Hello, World!";
?>`,
        executionSteps: ['Request', 'Interpreter', 'HTML', 'Browser'],
        history: [
            { year: 1994, event: 'Personal Home Page tools' },
            { year: 1997, event: 'PHP 3 (Zend Engine)' },
            { year: 2004, event: 'Facebook starts using PHP' },
            { year: 2015, event: 'PHP 7 (Huge speed boost)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I power 79% of the web.',
        funEpisode: "It originally stood for 'Personal Home Page'."
    },
    {
        id: 'sql',
        name: 'SQL',
        year: 1974,
        creator: 'IBM',
        useCase: 'Databases',
        description: 'The only language you need to talk to databases.',
        tags: ['Data', 'Standard', 'Query'],
        category: 'Data',
        color: '#003b57',
        example: `-- Hello World in SQL
-- The essence: Declarative Query
SELECT 'Hello, World!' AS greeting;`,
        executionSteps: ['Query', 'Plan', 'Engine', 'Result'],
        history: [
            { year: 1970, event: 'Relational Model published' },
            { year: 1974, event: 'SEQUEL invented at IBM' },
            { year: 1979, event: 'Oracle releases first commercial SQL' },
            { year: 1986, event: 'SQL becomes an ANSI standard' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Data never sleeps.',
        funEpisode: "Originally called SEQUEL, but changed due to trademark."
    },
    {
        id: 'asm',
        name: 'Assembly',
        year: 1949,
        creator: 'David Wheeler',
        useCase: 'Drivers, OS',
        description: 'Talking directly to the hardware. As close to the metal as it gets.',
        tags: ['Hardware', 'Hard', 'Fast'],
        category: 'Systems',
        color: '#6e4c13',
        example: `; Hello World in x86 Assembly
; The essence: Registers and Syscalls
section .data
    msg db 'Hello, World!', 0
section .text
    global _start
_start:
    ; ... syscall setup omitted ...`,
        executionSteps: ['Source', 'Assembler', 'Machine Code', 'CPU'],
        history: [
            { year: 1947, event: 'First Assembly language' },
            { year: 1949, event: 'EDSAC Initial Orders' },
            { year: 1978, event: 'Intel 8086 (x86 birth)' },
            { year: 2003, event: 'x86-64 expands to 64-bit' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I speak for the silicon.',
        funEpisode: "It is specific to exactly one type of processor."
    },
    {
        id: 'lisp',
        name: 'Lisp',
        year: 1958,
        creator: 'John McCarthy',
        useCase: 'AI, Emacs',
        description: 'The second oldest language. Famous for parentheses and AI.',
        tags: ['AI', 'Functional', 'Parens'],
        category: 'Functional',
        color: '#3fb68b',
        example: `;; Hello World in Lisp
;; The essence: Everything is a list
(print "Hello, World!")`,
        executionSteps: ['Source', 'Reader', 'Evaluator', 'Value'],
        history: [
            { year: 1958, event: 'Invented at MIT' },
            { year: 1960, event: 'First Garbage Collector' },
            { year: 1984, event: 'Common Lisp Standard' },
            { year: 1994, event: 'Paul Graham sells Viaweb (Lisp)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: '(((Parentheses)))',
        funEpisode: "Pioneered If-Statements and Recursion."
    },
    {
        id: 'perl',
        name: 'Perl',
        year: 1987,
        creator: 'Larry Wall',
        useCase: 'Scripts, Text Processing',
        description: 'The duct tape of the internet. Great at processing text.',
        tags: ['Scripting', 'Text', 'Old'],
        category: 'Scripting',
        color: '#39457e',
        example: `# Hello World in Perl
# The essence: Tim Toady (TMTOWTDI)
print "Hello, World!\n";`,
        executionSteps: ['Source', 'Compiler', 'Interpreter', 'Run'],
        history: [
            { year: 1987, event: 'Released to comp.sources' },
            { year: 1994, event: 'Perl 5 Released' },
            { year: 1998, event: 'Powered the early DotCom boom' },
            { year: 2015, event: 'Perl 6 released as Raku' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'There is more than one way to do it.',
        funEpisode: "The manual is full of Lord of the Rings quotes."
    },
    {
        id: 'csharp',
        name: 'C#',
        year: 2000,
        creator: 'Anders Hejlsberg',
        useCase: 'Windows, Unity Games',
        description: 'Microsoft\'s flagship. Great for enterprise and gaming.',
        tags: ['Games', 'Windows', 'Modern'],
        category: 'Software',
        color: '#239120',
        example: `// Hello World in C#
// The essence: Namespace & Class
using System;
class Program {
    static void Main() {
        Console.WriteLine("Hello, World!");
    }
}`,
        executionSteps: ['Source', 'Compiler', 'IL Code', 'CLR'],
        history: [
            { year: 2000, event: 'Announced by Microsoft' },
            { year: 2002, event: '.NET Framework 1.0' },
            { year: 2016, event: '.NET Core (Cross platform)' },
            { year: 2020, event: 'Unified .NET 5' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I run Unity.',
        funEpisode: "Originally named 'Cool'."
    },
    {
        id: 'bash',
        name: 'Bash',
        year: 1989,
        creator: 'Brian Fox',
        useCase: 'Linux, DevOps',
        description: 'The command line language. You use it to control servers.',
        tags: ['CLI', 'Linux', 'Scripting'],
        category: 'Scripting',
        color: '#4eaa25',
        example: `# Hello World in Bash
# The essence: Shell Command
echo "Hello, World!"`,
        executionSteps: ['Source', 'Shell Expansion', 'Execution', 'Output'],
        history: [
            { year: 1989, event: 'Released for GNU Project' },
            { year: 1993, event: 'Default shell on Linux' },
            { year: 2002, event: 'MacOS adopts Bash' },
            { year: 2019, event: 'Bash 5.0 Released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'sudo make me a sandwich.',
        funEpisode: "BASH = Bourne Again Shell."
    },
    {
        id: 'scala',
        name: 'Scala',
        year: 2004,
        creator: 'Martin Odersky',
        useCase: 'Big Data, Spark',
        description: 'Java but more powerful. Functional and Concise.',
        tags: ['JVM', 'Data', 'Functional'],
        category: 'Backend',
        color: '#dc322f',
        example: `// Hello World in Scala
// The essence: Object Singleton
object Hello extends App {
  println("Hello, World!")
}`,
        executionSteps: ['Source', 'Compiler', 'JVM Bytecode', 'Run'],
        history: [
            { year: 2004, event: 'First Release on JVM' },
            { year: 2009, event: 'Twitter keeps Scala alive' },
            { year: 2014, event: 'Spark framework explodes' },
            { year: 2021, event: 'Scala 3.0 (Cleaner syntax)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Scalable Language.',
        funEpisode: "Used to build Apache Spark."
    },
    {
        id: 'typescript',
        name: 'TypeScript',
        year: 2012,
        creator: 'Anders Hejlsberg',
        useCase: 'Web Apps',
        description: 'JavaScript with types. catches bugs before you run format.',
        tags: ['Web', 'Typed', 'Modern'],
        category: 'Web',
        color: '#3178c6',
        example: `// Hello World in TypeScript
// The essence: Optional Types
const msg: string = "Hello, World!";
console.log(msg);`,
        executionSteps: ['TS Source', 'Type Check', 'Transpile', 'JS'],
        history: [
            { year: 2012, event: 'Microsoft releases 0.8' },
            { year: 2016, event: 'Angular 2 adopts TypeScript' },
            { year: 2018, event: 'Babel adds TS support' },
            { year: 2020, event: 'Top 5 language on GitHub' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'JavaScript that scales.',
        funEpisode: "Created by the same guy who made C#."
    },
    {
        id: 'r_lang',
        name: 'R',
        year: 1993,
        creator: 'Ihaka & Gentleman',
        useCase: 'Statistics, Data',
        description: 'The power of statistics in a script.',
        tags: ['Stats', 'Data', 'Visual'],
        category: 'Data',
        color: '#276bca',
        example: `# Hello World in R
# The essence: Vectorized
cat("Hello, World!")`,
        executionSteps: ['Script', 'Parser', 'Interpreter', 'Output'],
        history: [{ year: 1993, event: 'Born at U of Auckland' }, { year: 2000, event: 'R 1.0 Release' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Data is beautiful.',
        funEpisode: "Named after its creators Ross and Robert."
    },
    {
        id: 'lua',
        name: 'Lua',
        year: 1993,
        creator: 'Ierusalimschy',
        useCase: 'Game Scripts',
        description: 'Lightweight and embeddable scripting engine.',
        tags: ['Games', 'Fast', 'Small'],
        category: 'Scripting',
        color: '#000080',
        example: `-- Hello World in Lua
-- The essence: Simple Tables
print("Hello, World!")`,
        executionSteps: ['Script', 'Bytecode', 'Lua VM', 'Result'],
        history: [{ year: 1993, event: 'Created in Brazil' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Small yet mighty.',
        funEpisode: "Lua means 'Moon' in Portuguese."
    },
    {
        id: 'dart',
        name: 'Dart',
        year: 2011,
        creator: 'Google',
        useCase: 'Mobile (Flutter)',
        description: 'Optimized for high-quality UI development.',
        tags: ['Flutter', 'Google', 'UI'],
        category: 'Mobile',
        color: '#0175c2',
        example: `// Hello World in Dart
// The essence: UI Focused
void main() {
  print("Hello, World!");
}`,
        executionSteps: ['Source', 'Compiler (AOT)', 'Machine Code', 'Run'],
        history: [{ year: 2011, event: 'GOTO Conference' }, { year: 2018, event: 'Flutter 1.0' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'UI first.',
        funEpisode: "AOT compilation makes Flutter apps very fast!"
    },
    {
        id: 'haskell',
        name: 'Haskell',
        year: 1990,
        creator: 'Committee',
        useCase: 'Academia',
        description: 'Purely functional, lazy, and mathematically sound.',
        tags: ['Pure', 'Lazy', 'Math'],
        category: 'Functional',
        color: '#5e5086',
        example: `-- Hello World in Haskell
-- The essence: IO Monad
main = putStrLn "Hello, World!"`,
        executionSteps: ['Source', 'GHC', 'Executable', 'Run'],
        history: [{ year: 1990, event: 'Haskell 1.0' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Pure functions only.',
        funEpisode: "Named after logician Haskell Curry."
    },
    {
        id: 'prolog',
        name: 'Prolog',
        year: 1972,
        creator: 'Colmerauer',
        useCase: 'Logic, AI',
        description: 'The definitive logic programming language.',
        tags: ['Logic', 'AI', 'Search'],
        category: 'Functional',
        color: '#74283c',
        example: `% Hello World in Prolog
% The essence: Logic Goals
write('Hello, World!'), nl.`,
        executionSteps: ['Goal', 'Inference', 'Match', 'Output'],
        history: [{ year: 1972, event: 'Born in Marseille' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Let logic decide.',
        funEpisode: "Prolog = Programmation en Logique."
    },
    {
        id: 'vhdl',
        name: 'VHDL',
        year: 1981,
        creator: 'US DOD',
        useCase: 'Chip Design',
        description: 'Hardware description for digital logic comparison.',
        tags: ['Hardware', 'FPGA', 'Logic'],
        category: 'Systems',
        color: '#adb2cb',
        example: `-- Hello World in VHDL
-- The essence: Signals and Processes
entity Hello is end;
architecture Behavior of Hello is begin
  process begin report "Hello World"; wait; end process;
end;`,
        executionSteps: ['Source', 'Synthesis', 'Netlist', 'Hardware'],
        history: [{ year: 1981, event: 'Request for Proposal' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Describing silicon.',
        funEpisode: "Used to simulate space shuttle electronics!"
    },
    {
        id: 'pascal',
        name: 'Pascal',
        year: 1970,
        creator: 'Niklaus Wirth',
        useCase: 'Education',
        description: 'Clean, structured, and strictly typed development.',
        tags: ['Clean', 'Safety', 'Formal'],
        category: 'Systems',
        color: '#27336e',
        example: `(* Hello World in Pascal *)
(* The essence: Structure *)
program Hello;
begin
  WriteLn('Hello, World!');
end.`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'Run'],
        history: [{ year: 1970, event: 'Published' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Structured beauty.',
        funEpisode: "Inspired Apple's early Mac OS code!"
    },
    {
        id: 'vb',
        name: 'Visual Basic',
        year: 1991,
        creator: 'Alan Cooper',
        useCase: 'Windows Apps',
        description: 'Revolutionized GUI development with drag-and-drop.',
        tags: ['Easy', 'GUI', 'Windows'],
        category: 'Software',
        color: '#1572b6',
        example: `' Hello World in VB
' The essence: Event Driven
Sub Main()
    MsgBox "Hello, World!"
End Sub`,
        executionSteps: ['Code', 'Compiler', 'Runtime', 'Window'],
        history: [{ year: 1991, event: 'Released' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'If-then-else GUI.',
        funEpisode: "The 'Father of VB' is also the father of UX personas."
    },
    {
        id: 'objective_c',
        name: 'Objective-C',
        year: 1984,
        creator: 'Brad Cox',
        useCase: 'Legacy iOS',
        description: 'The language that powered the first iPhone.',
        tags: ['Apple', 'Object', 'Legacy'],
        category: 'Software',
        color: '#438eff',
        example: `// Hello World in Obj-C
// The essence: Square Brackets
NSLog(@"Hello, World!");`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'Run'],
        history: [{ year: 1984, event: 'Created' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Dynamic messaging.',
        funEpisode: "Steve Jobs licensed it for NeXT."
    },
    {
        id: 'elixir',
        name: 'Elixir',
        year: 2011,
        creator: 'José Valim',
        useCase: 'Real-time Apps',
        description: 'Concurrent and fault-tolerant on the Erlang VM.',
        tags: ['BEAM', 'Web', 'Concurrent'],
        category: 'Backend',
        color: '#4e2a8e',
        example: `# Hello World in Elixir
# The essence: Pipe Operator
IO.puts "Hello, World!"`,
        executionSteps: ['Source', 'Compiler', 'BEAM Bytecode', 'VM'],
        history: [{ year: 2011, event: 'Born' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Fault tolerance.',
        funEpisode: "Discord handles 12M+ concurrent users with Elixir!"
    },
    {
        id: 'smalltalk',
        name: 'Smalltalk',
        year: 1972,
        creator: 'Alan Kay',
        useCase: 'OO Research',
        description: 'The purest approach to object-oriented programming.',
        tags: ['OO', 'Iconic', 'Pure'],
        category: 'Software',
        color: '#596706',
        example: `"Hello World in Smalltalk"
Transcript show: 'Hello, World!'.`,
        executionSteps: ['Image', 'VM', 'Message', 'Result'],
        history: [{ year: 1972, event: 'XEROX PARC' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Everything is an object.',
        funEpisode: "Invented overlapping windows!"
    },
    {
        id: 'fortran',
        name: 'Fortran',
        year: 1957,
        creator: 'John Backus',
        useCase: 'Supercomputing',
        description: 'The first high-level language. Still rules physics.',
        tags: ['Math', 'Fast', 'Ancient'],
        category: 'Data',
        color: '#734f96',
        example: `! Hello World in Fortran
! The essence: Formula Translation
program hello
  print *, 'Hello, World!'
end program hello`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'Run'],
        history: [{ year: 1957, event: 'Released by IBM' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Number crunching king.',
        funEpisode: "Name stands for FORmula TRANslation."
    },
    {
        id: 'cobol',
        name: 'COBOL',
        year: 1959,
        creator: 'Grace Hopper (Grandmother)',
        useCase: 'Banking, Finance',
        description: 'Business oriented language. Runs 95% of ATM swipes.',
        tags: ['Business', 'Old', 'Stable'],
        category: 'Enterprise',
        color: '#005ca5',
        example: `* Hello World in COBOL
PROCEDURE DIVISION.
DISPLAY 'Hello, World!'.
STOP RUN.`,
        executionSteps: ['Source', 'Compiler', 'Mainframe', 'Run'],
        history: [{ year: 1959, event: 'Designed by CODASYL' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I handle the money.',
        funEpisode: "Y2K bug was mostly about COBOL dates!"
    },
    {
        id: 'racket',
        name: 'Racket',
        year: 1995,
        creator: 'PLT',
        useCase: 'Education, Lisp',
        description: 'A programmable programming language. Design your own syntax.',
        tags: ['Lisp', 'Education', 'Macros'],
        category: 'Functional',
        color: '#3c52a1',
        example: `;; Hello World in Racket
#lang racket
(displayln "Hello, World!")`,
        executionSteps: ['Source', 'Macro Expand', 'Bytecode', 'Run'],
        history: [{ year: 1995, event: 'Started as PLT Scheme' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Language for languages.',
        funEpisode: "Excellent for creating new languages."
    },
    {
        id: 'scheme',
        name: 'Scheme',
        year: 1975,
        creator: 'Sussman & Steele',
        useCase: 'Education, AI',
        description: 'Minimalist Lisp. Clean, beautiful, and distinct.',
        tags: ['Minimal', 'Lisp', 'Academic'],
        category: 'Functional',
        color: '#ff6600',
        example: `;; Hello World in Scheme
(display "Hello, World!")`,
        executionSteps: ['Source', 'Interpreter', 'Value', 'Print'],
        history: [{ year: 1975, event: 'MIT AI Lab' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Minimalist perfection.',
        funEpisode: "Guy Steele created Java spec too!"
    },
    {
        id: 'ocaml',
        name: 'OCaml',
        year: 1996,
        creator: 'INRIA',
        useCase: 'Financial, Systems',
        description: 'Industrial strength functional programming.',
        tags: ['ML', 'Functional', 'Fast'],
        category: 'Functional',
        color: '#ec6813',
        example: `(* Hello World in OCaml *)
print_endline "Hello, World!";;`,
        executionSteps: ['Source', 'Bytecode/Native', 'Run', 'Output'],
        history: [{ year: 1996, event: 'Objective Caml born' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Type safety + speed.',
        funEpisode: "Used heavily by Jane Street for trading."
    },
    {
        id: 'zig',
        name: 'Zig',
        year: 2016,
        creator: 'Andrew Kelley',
        useCase: 'Systems Replacement for C',
        description: 'A modern C. No hidden control flow, no hidden allocations.',
        tags: ['Systems', 'Modern', 'C-like'],
        category: 'Systems',
        color: '#ec915c',
        example: `// Hello World in Zig
const std = @import("std");
pub fn main() !void {
    std.debug.print("Hello, World!\\n", .{});
}`,
        executionSteps: ['Source', 'Zig Compiler', 'Binary', 'Run'],
        history: [{ year: 2016, event: 'Development started' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Zero hidden logic.',
        funEpisode: "Can compile C code better than GCC sometimes!"
    },
    {
        id: 'nim',
        name: 'Nim',
        year: 2008,
        creator: 'Andreas Rumpf',
        useCase: 'Systems, Scripting',
        description: 'Python-like syntax with C-like performance.',
        tags: ['Fast', 'Pythonic', 'Systems'],
        category: 'Systems',
        color: '#ffc200',
        example: `# Hello World in Nim
echo "Hello, World!"`,
        executionSteps: ['Source', 'Transpile C', 'Compile C', 'Run'],
        history: [{ year: 2008, event: 'Created' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Efficient expressiveness.',
        funEpisode: "Compiles to C, C++, or JavaScript!"
    },
    {
        id: 'crystal',
        name: 'Crystal',
        year: 2014,
        creator: 'Ary Borenszweig',
        useCase: 'Web, Systems',
        description: 'Ruby syntax but compiled and statically typed.',
        tags: ['Fast', 'Ruby-like', 'Compiled'],
        category: 'Backend',
        color: '#000000',
        example: `# Hello World in Crystal
puts "Hello, World!"`,
        executionSteps: ['Source', 'LLVM IR', 'Binary', 'Run'],
        history: [{ year: 2014, event: 'Released' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Fast as C, Slick as Ruby.',
        funEpisode: "Type inference is so good you rarely type types."
    },
    {
        id: 'solidity',
        name: 'Solidity',
        year: 2014,
        creator: 'Gavin Wood',
        useCase: 'Smart Contracts',
        description: 'The language of Ethereum and Web3.',
        tags: ['Crypto', 'Contract', 'Blockchain'],
        category: 'Software',
        color: '#363636',
        example: `// Hello World in Solidity
contract Hello {
    string public greet = "Hello, World!";
}`,
        executionSteps: ['Source', 'Bytecode', 'EVM', 'Blockchain'],
        history: [{ year: 2014, event: 'Proposed for Ethereum' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Code is law.',
        funEpisode: "A bug in Solidity code cost $50M (The DAO)."
    },
    {
        id: 'powershell',
        name: 'PowerShell',
        year: 2006,
        creator: 'Microsoft',
        useCase: 'Windows Admin',
        description: 'Object-oriented shell for Windows automation.',
        tags: ['cli', 'Microsoft', 'Admin'],
        category: 'Scripting',
        color: '#012456',
        example: `# Hello World in PowerShell
Write-Host "Hello, World!"`,
        executionSteps: ['Script', 'Parser', '.NET Runtime', 'Output'],
        history: [{ year: 2006, event: 'Monad became PowerShell' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Objects in the shell.',
        funEpisode: "Passes objects, not text, between commands."
    },
    {
        id: 'matlab',
        name: 'MATLAB',
        year: 1984,
        creator: 'Cleve Moler',
        useCase: 'Engineering, Math',
        description: 'Matrix laboratory. Essential for engineers and scientists.',
        tags: ['Math', 'Matrix', 'Science'],
        category: 'Data',
        color: '#e16737',
        example: `% Hello World in MATLAB
disp('Hello, World!')`,
        executionSteps: ['Script', 'Interpreter', 'JIT', 'Plot'],
        history: [{ year: 1984, event: 'Founded MathWorks' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Everything is a matrix.',
        funEpisode: "Originally written to give students access to LINPACK."
    },
    {
        id: 'ada',
        name: 'Ada',
        year: 1980,
        creator: 'Jean Ichbiah',
        useCase: 'Aviation, Defense',
        description: 'Designed for safety-critical systems. Planes run on Ada.',
        tags: ['Safety', 'Military', 'Robust'],
        category: 'Systems',
        color: '#02f88c',
        example: `-- Hello World in Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Hello is begin
   Put_Line ("Hello, World!");
end Hello;`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'Control'],
        history: [{ year: 1980, event: 'MIL-STD-1815' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Failure is not an option.',
        funEpisode: "Named after Ada Lovelace, the first programmer."
    },
    {
        id: 'haxe',
        name: 'Haxe',
        year: 2005,
        creator: 'Nicolas Cannasse',
        useCase: 'Games, Cross-platform',
        description: 'The universal language. Transpiles to almost anything.',
        tags: ['Cross-platform', 'Game', 'Flexible'],
        category: 'Software',
        color: '#ea8220',
        example: `// Hello World in Haxe
class Main {
  static function main() {
    trace("Hello, World!");
  }
}`,
        executionSteps: ['Source', 'Transpiler', 'Target Lang', 'Run'],
        history: [{ year: 2005, event: 'Released' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'One code, everywhere.',
        funEpisode: "Can compile to C++, Java, JS, Python, Lua..."
    },
    {
        id: 'd_lang',
        name: 'D',
        year: 2001,
        creator: 'Walter Bright',
        useCase: 'Systems, Games',
        description: 'C++ done right. Powerful but easier to use.',
        tags: ['Systems', 'Fast', 'Clean'],
        category: 'Systems',
        color: '#b03931',
        example: `// Hello World in D
import std.stdio;
void main() {
    writeln("Hello, World!");
}`,
        executionSteps: ['Source', 'Compiler', 'Binary', 'Run'],
        history: [{ year: 2001, event: 'Released' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Systems power, productivity.',
        funEpisode: "Originally called 'Mars'."
    },
    {
        id: 'clojure',
        name: 'Clojure',
        year: 2007,
        creator: 'Rich Hickey',
        useCase: 'Data, Backend',
        description: 'Modern Lisp on the Java Virtual Machine.',
        tags: ['Lisp', 'JVM', 'Functional'],
        category: 'Functional',
        color: '#5881d8',
        example: `;; Hello World in Clojure
(println "Hello, World!")`,
        executionSteps: ['Source', 'Reader', 'JVM Bytecode', 'Run'],
        history: [{ year: 2007, event: 'Released' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Simple made easy.',
        funEpisode: "Emphasizes immutability for concurrency."
    },
    {
        id: 'fsharp',
        name: 'F#',
        year: 2005,
        creator: 'Don Syme',
        useCase: 'Finance, Web',
        description: 'Functional first language on .NET.',
        tags: ['Functional', '.NET', 'Concise'],
        category: 'Functional',
        color: '#b845fc',
        example: `// Hello World in F#
printfn "Hello, World!"`,
        executionSteps: ['Source', 'Compiler', 'IL', 'CLR'],
        history: [{ year: 2005, event: 'Microsoft Research' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Functional .NET.',
        funEpisode: "Derived from OCaml but for .NET."
    },
    {
        id: 'julia',
        name: 'Julia',
        year: 2012,
        creator: 'Bezanson et al',
        useCase: 'Data Science',
        description: 'Fast as C, Dynamic as Python. Scientific computing.',
        tags: ['Science', 'Fast', 'Math'],
        category: 'Data',
        color: '#9558b2',
        example: `# Hello World in Julia
println("Hello, World!")`,
        executionSteps: ['Source', 'JIT (LLVM)', 'Native', 'Run'],
        history: [{ year: 2012, event: 'Launched' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Walks like Python, runs like C.',
        funEpisode: "Designed to solve the 'two language problem'."
    },
    {
        id: 'erlang',
        name: 'Erlang',
        year: 1986,
        creator: 'Ericsson',
        useCase: 'Telecoms',
        description: 'Built for massive concurrency and 99.9999999% uptime.',
        tags: ['Concurrent', 'Reliable', 'Old'],
        category: 'Backend',
        color: '#a90533',
        example: `% Hello World in Erlang
io:format("Hello, World!~n").`,
        executionSteps: ['Source', 'Compiler', 'BEAM VM', 'Run'],
        history: [{ year: 1986, event: 'Proprietary at Ericsson' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Let it crash.',
        funEpisode: "Originally purely for telephone switches."
    },
    {
        id: 'groovy',
        name: 'Groovy',
        year: 2003,
        creator: 'James Strachan',
        useCase: 'Scripting JVM',
        description: 'A dynamic language for the Java Virtual Machine.',
        tags: ['JVM', 'Scripting', 'Dynamic'],
        category: 'Scripting',
        color: '#4298b8',
        example: `// Hello World in Groovy
println "Hello, World!"`,
        executionSteps: ['Source', 'Compiler', 'Bytecode', 'JVM'],
        history: [{ year: 2003, event: 'Created' }],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Supercharged Java.',
        funEpisode: "Jenkins pipelines are written in Groovy."
    }
];
