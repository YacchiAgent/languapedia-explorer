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
    }
];
