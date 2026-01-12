const languages = [
    {
        id: 'python',
        name: 'Python',
        year: 1991,
        creator: 'Guido van Rossum',
        useCase: 'Data Science, Web, AI',
        description: 'A high-level, interpreted programming language known for its readability and vast library ecosystem.',
        tags: ['Dynamic', 'Easy', 'Popular'],
        category: 'AI',
        color: '#3776ab',
        example: `def greet(name):
    return f"Hello, {name}! Welcome to Python."

print(greet("Developer"))`,
        history: [
            { year: 1989, event: 'Conceptualization started' },
            { year: 1991, event: 'Python 0.9.0 released' },
            { year: 2000, event: 'Python 2.0 introduces list comprehensions' },
            { year: 2008, event: 'Python 3.0 released (breaking changes)' }
        ],
        mascot: 'assets/python_mascot.png',
        mascotQuote: 'Indent your code, not your life!',
        funEpisode: "Named after Monty Python's Flying Circus, not the snake!"
    },
    {
        id: 'javascript',
        name: 'JavaScript',
        year: 1995,
        creator: 'Brendan Eich',
        useCase: 'Web, Server, Mobile',
        description: 'The language of the web. Ubiquitous and extremely flexible, powering almost every modern website.',
        tags: ['Async', 'Web', 'Flexible'],
        category: 'Web',
        color: '#f7df1e',
        example: `const greet = (name) => {
    console.log(\`Hello, \${name}! Let's build the web.\`);
};

greet('World');`,
        history: [
            { year: 1995, event: 'Created in 10 days for Netscape' },
            { year: 1997, event: 'ECMAScript standard released' },
            { year: 2009, event: 'Node.js brings JS to the server' },
            { year: 2015, event: 'ES6 (ES2015) major update' }
        ],
        mascot: 'assets/js_mascot.png',
        mascotQuote: 'I might be quirky, but I run the world!',
        funEpisode: "Created in just 10 days by Brendan Eich at Netscape. Originally named Mocha, then LiveScript."
    },
    {
        id: 'rust',
        name: 'Rust',
        year: 2010,
        creator: 'Graydon Hoare (Mozilla)',
        useCase: 'Systems, WebAssembly, Performance',
        description: 'A systems programming language that provides memory safety without garbage collection.',
        tags: ['Safe', 'Fast', 'Modern'],
        category: 'Systems',
        color: '#dea584',
        example: `fn main() {
    let name = "Ferris";
    println!("Hello, {}! Stay safe.", name);
}`,
        history: [
            { year: 2006, event: 'Personal project started' },
            { year: 2010, event: 'Announced by Mozilla' },
            { year: 2015, event: 'Rust 1.0 stable release' },
            { year: 2021, event: 'Rust Foundation formed' }
        ],
        mascot: 'assets/rust_mascot.png',
        mascotQuote: 'Borrow checker is your best friend. Trust me.',
        funEpisode: "The mascot is a crab named Ferris, because Rust users are called 'rustaceans' (crustaceans)."
    },
    {
        id: 'cpp',
        name: 'C++',
        year: 1985,
        creator: 'Bjarne Stroustrup',
        useCase: 'Games, Systems, High Performance',
        description: 'A powerful, efficient language that extends C with object-oriented features.',
        tags: ['Fast', 'Low-level', 'Standard'],
        category: 'Systems',
        color: '#00599c',
        example: `#include <iostream>\nint main() {\n    std::cout << "Hello, C++!" << std::endl;\n    return 0;\n}`,
        history: [
            { year: 1979, event: 'C with Classes development started' },
            { year: 1983, event: 'Renamed to C++' },
            { year: 1998, event: 'C++98 standard released' },
            { year: 2011, event: 'C++11 modernization' }
        ],
        mascot: 'assets/cpp_mascot.png',
        mascotQuote: 'With great power comes great responsibility... and manual memory management.',
        funEpisode: "Bjarne Stroustrup originally intended to name it 'C with Classes'. The name C++ was suggested by Rick Mascitti in 1983."
    },
    {
        id: 'go',
        name: 'Go',
        year: 2009,
        creator: 'Robert Griesemer, Rob Pike, Ken Thompson (Google)',
        useCase: 'Cloud, Infrastructure, Microservices',
        description: 'An open-source language that simplifies building simple, reliable, and efficient software.',
        tags: ['Cloud', 'Concurrent', 'Simple'],
        category: 'Backend',
        color: '#00add8',
        example: `package main\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, Gophers!")\n}`,
        history: [
            { year: 2007, event: 'Design started at Google' },
            { year: 2009, event: 'Open sourced as Go 1.0' },
            { year: 2012, event: 'Go 1.0 released' },
            { year: 2022, event: 'Generics introduced in Go 1.18' }
        ],
        mascot: 'assets/go_mascot.png',
        mascotQuote: 'Keep it simple, keep it fast, keep it concurrent!',
        funEpisode: "The gopher mascot has no name - it's intentionally simple, much like the language itself."
    },
    {
        id: 'java',
        name: 'Java',
        year: 1995,
        creator: 'James Gosling (Sun Microsystems)',
        useCase: 'Enterprise, Android, Finance',
        description: 'A class-based, object-oriented language designed to have as few implementation dependencies as possible.',
        tags: ['Enterprise', 'Reliable', 'Cross-platform'],
        category: 'Enterprise',
        color: '#007396',
        example: `public class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, Java World!");\n    }\n}`,
        history: [
            { year: 1991, event: 'Project Green started' },
            { year: 1995, event: 'Java 1.0 released (Write Once, Run Anywhere)' },
            { year: 2004, event: 'J2SE 5.0 (Generics, Enums)' },
            { year: 2014, event: 'Java 8 (Lambdas, Streams)' }
        ],
        mascot: 'assets/java_mascot.png',
        mascotQuote: 'Object-oriented from head to toe!',
        funEpisode: "Originally named Oak, but the name was already trademarked. Java was chosen over coffee at a local shop."
    },
    {
        id: 'swift',
        name: 'Swift',
        year: 2014,
        creator: 'Chris Lattner (Apple)',
        useCase: 'iOS, macOS, WatchOS',
        description: 'A powerful and intuitive programming language for iOS, iPadOS, macOS, tvOS, and watchOS.',
        tags: ['Mobile', 'Safe', 'Modern'],
        category: 'Mobile',
        color: '#f05138',
        example: `let name = "Developer"\nprint("Hello, \\(name)! Let's build for Apple platforms.")`,
        history: [
            { year: 2010, event: 'Development started' },
            { year: 2014, event: 'Swift 1.0 announced at WWDC' },
            { year: 2015, event: 'Swift 2.0 open sourced' },
            { year: 2024, event: 'Swift 6.0 major update' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Fast, safe, and ready for your next big app.',
        funEpisode: "Developed entirely in secret at Apple for 4 years before its public announcement at WWDC 2014."
    },
    {
        id: 'ruby',
        name: 'Ruby',
        year: 1995,
        creator: 'Yukihiro Matsumoto',
        useCase: 'Web Development (Rails), Scripting',
        description: 'A dynamic, open source programming language with a focus on simplicity and productivity.',
        tags: ['Elegant', 'Friendly', 'Productive'],
        category: 'Backend',
        color: '#cc342d',
        example: `def welcome(name)\n  puts "Hello, #{name}! Happiness is coding in Ruby."\nend\n\nwelcome("Developer")`,
        history: [
            { year: 1993, event: 'Research and development started' },
            { year: 1995, event: 'Ruby 0.95 released' },
            { year: 2005, event: 'Ruby on Rails web framework released' },
            { year: 2020, event: 'Ruby 3.0 released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Ruby is designed for programmer happiness!',
        funEpisode: "Created with the goal of being 'natural' rather than 'simple' - specifically for human happiness."
    },
    {
        id: 'typescript',
        name: 'TypeScript',
        year: 2012,
        creator: 'Anders Hejlsberg (Microsoft)',
        useCase: 'Web, Enterprise, Scalable JS',
        description: 'A typed superset of JavaScript that compiles to plain JavaScript.',
        tags: ['Typed', 'Scale', 'Microsoft'],
        category: 'Web',
        color: '#3178c6',
        example: `interface User {\n  name: string;\n  id: number;\n}\n\nconst user: User = { name: "Dev", id: 1 };`,
        history: [
            { year: 2010, event: 'Internal development at Microsoft started' },
            { year: 2012, event: 'Public release (v0.8)' },
            { year: 2016, event: 'TypeScript 2.0 released' },
            { year: 2023, event: 'TypeScript 5.0 major update' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Static types are a gift to your future self.',
        funEpisode: "Anders Hejlsberg, the creator of TypeScript, also created Turbo Pascal, Delphi, and C#."
    },
    {
        id: 'php',
        name: 'PHP',
        year: 1995,
        creator: 'Rasmus Lerdorf',
        useCase: 'Web Development, Server-side Scripting',
        description: 'A popular general-purpose scripting language that is especially suited to web development.',
        tags: ['Web', 'Scripting', 'Ubiquitous'],
        category: 'Backend',
        color: '#777bb4',
        example: `<?php\necho "Hello, PHP World!";\n?>`,
        history: [
            { year: 1994, event: 'Personal Home Page (PHP) tools created' },
            { year: 1995, event: 'PHP/FI released' },
            { year: 2004, event: 'PHP 5 release (Zend Engine II)' },
            { year: 2020, event: 'PHP 8 introduces JIT' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I power 75% of the web! Not bad, right?',
        funEpisode: "PHP originally stood for 'Personal Home Page' tools. It now stands for the recursive 'PHP: Hypertext Preprocessor'."
    },
    {
        id: 'kotlin',
        name: 'Kotlin',
        year: 2011,
        creator: 'JetBrains',
        useCase: 'Android, Multiplatform, Backend',
        description: 'A modern, cross-platform, statically typed programming language with type inference.',
        tags: ['Modern', 'Android', 'JetBrains'],
        category: 'Mobile',
        color: '#7f52ff',
        example: `fun main() {\n    println("Hello, Kotlin!")\n}`,
        history: [
            { year: 2010, event: 'Development started by JetBrains' },
            { year: 2011, event: 'Project Kotlin unveiled' },
            { year: 2017, event: 'Google announces first-class support for Java on Android' },
            { year: 2019, event: 'Google announces Kotlin-first for Android' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Concise, safe, and interoperable.',
        funEpisode: "Named after Kotlin Island, near St. Petersburg, just like Java was named after an island."
    },
    {
        id: 'csharp',
        name: 'C#',
        year: 2000,
        creator: 'Anders Hejlsberg (Microsoft)',
        useCase: 'Games (Unity), Enterprise, Desktop',
        description: 'A modern, object-oriented, and type-safe programming language developed by Microsoft.',
        tags: ['Unity', 'Microsoft', 'Enterprise'],
        category: 'Software',
        color: '#239120',
        example: `using System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine("Hello, C#!");\n    }\n}`,
        history: [
            { year: 1999, event: 'Cool project started (original name)' },
            { year: 2000, event: 'C# announced with .NET' },
            { year: 2016, event: '.NET Core open sourced' },
            { year: 2023, event: 'C# 12 released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'From enterprise apps to triple-A games, I do it all.',
        funEpisode: "The name C# was inspired by musical notation, where a sharp indicates that the note is higher in pitch."
    },
    {
        id: 'sql',
        name: 'SQL',
        year: 1974,
        creator: 'Donald D. Chamberlin, Raymond F. Boyce (IBM)',
        useCase: 'Database Management, Data Analysis',
        description: 'The standard language for relational database management systems.',
        tags: ['Data', 'Standard', 'Declarative'],
        category: 'Data',
        color: '#003b57',
        example: `SELECT * FROM languages WHERE name = 'SQL';`,
        history: [
            { year: 1970, event: 'E.F. Codd publishes relational model' },
            { year: 1974, event: 'SEQUEL (later SQL) developed at IBM' },
            { year: 1986, event: 'ANSI SQL standard released' },
            { year: 1999, event: 'SQL:1999 adds regex and triggers' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Your data is safe with me.',
        funEpisode: "Donald D. Chamberlin and Raymond F. Boyce originally called it SEQUEL (Structured English Query Language)."
    },
    {
        id: 'r_lang',
        name: 'R',
        year: 1993,
        creator: 'Ross Ihaka, Robert Gentleman',
        useCase: 'Statistics, Data Visualization, Academia',
        description: 'A language and environment for statistical computing and graphics.',
        tags: ['Stats', 'Data', 'Graphics'],
        category: 'Data',
        color: '#276bca',
        example: `print("Hello, R World!")\nx <- c(1, 2, 3, 4)\nplot(x)`,
        history: [
            { year: 1992, event: 'Development started in New Zealand' },
            { year: 2000, event: 'R 1.0.0 released' },
            { year: 2011, event: 'RStudio first released' },
            { year: 2020, event: 'R 4.0.0 major update' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Let the data tell its story.',
        funEpisode: "Named after the first names of its creators, Ross Ihaka and Robert Gentleman."
    },
    {
        id: 'dart',
        name: 'Dart',
        year: 2011,
        creator: 'Lars Bak, Kasper Lund (Google)',
        useCase: 'Mobile (Flutter), Web',
        description: 'A client-optimized language for fast apps on any platform.',
        tags: ['Flutter', 'Fast', 'Google'],
        category: 'Mobile',
        color: '#0175c2',
        example: `void main() {\n  print('Hello, Dart!');\n}`,
        history: [
            { year: 2011, event: 'Unveiled at GOTO conference' },
            { year: 2013, event: 'Dart 1.0 released' },
            { year: 2017, event: 'Flutter framework first alpha' },
            { year: 2023, event: 'Dart 3.0 introduces sound null safety' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Ready to fly with Flutter.',
        funEpisode: "The language was originally codenamed Dash before it was unveiled as Dart."
    },
    {
        id: 'lua',
        name: 'Lua',
        year: 1993,
        creator: 'Roberto Ierusalimschy et al.',
        useCase: 'Game Scripting (Roblox, WoW), Embedding',
        description: 'A powerful, efficient, lightweight, embeddable scripting language.',
        tags: ['Lightweight', 'Games', 'Embedded'],
        category: 'Scripting',
        color: '#000080',
        example: `print("Hello, Lua!")`,
        history: [
            { year: 1993, event: 'Created at PUC-Rio in Brazil' },
            { year: 1994, event: 'Lua 1.1 released' },
            { year: 2005, event: 'Lua 5.1 released (widely used)' },
            { year: 2015, event: 'Lua 5.3 adds integer support' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Small, fast, and easy to fit in.',
        funEpisode: "Developed in Brazil, 'Lua' means 'Moon' in Portuguese."
    },
    {
        id: 'haskell',
        name: 'Haskell',
        year: 1990,
        creator: 'Committee',
        useCase: 'Academia, Finance, Compilers',
        description: 'An advanced, purely functional programming language.',
        tags: ['Functional', 'Pure', 'Academic'],
        category: 'Functional',
        color: '#5e5086',
        example: `main = putStrLn "Hello, Haskell!"`,
        history: [
            { year: 1987, event: 'Committee formed at FPCA' },
            { year: 1990, event: 'Haskell 1.0 released' },
            { year: 1998, event: 'Haskell 98 standard released' },
            { year: 2010, event: 'Haskell 2010 standard released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Purely functional, strictly academic.',
        funEpisode: "Named after the logician Haskell Curry, whose work became the basis for functional programming."
    },
    {
        id: 'cobol',
        name: 'COBOL',
        year: 1959,
        creator: 'CODASYL (Grace Hopper et al.)',
        useCase: 'Banking, Mainframes, Business',
        description: 'A compiled English-like computer programming language designed for business use.',
        tags: ['Business', 'Ancient', 'Reliable'],
        category: 'Enterprise',
        color: '#005a9c',
        example: `IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO-WORLD.\nPROCEDURE DIVISION.\n    DISPLAY 'Hello, COBOL world!'.\n    STOP RUN.`,
        history: [
            { year: 1959, event: 'CODASYL committee formed' },
            { year: 1960, event: 'COBOL-60 released' },
            { year: 1985, event: 'COBOL-85 major standard' },
            { year: 2002, event: 'Object-Oriented COBOL introduced' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'I am the backbone of global finance since 1959.',
        funEpisode: "Grace Hopper, a pioneer in computer science, was one of the key advisors for the development of COBOL."
    },
    {
        id: 'c_lang',
        name: 'C',
        year: 1972,
        creator: 'Dennis Ritchie (Bell Labs)',
        useCase: 'Systems, Operating Systems, Compilers',
        description: 'The foundation of modern computing. A powerful, low-level language that powers almost all OS kernels.',
        tags: ['Fast', 'Low-level', 'Standard'],
        category: 'Systems',
        color: '#a8b9cc',
        example: `#include <stdio.h>\n\nint main() {\n    printf("Hello, C World!\\n");\n    return 0;\n}`,
        history: [
            { year: 1972, event: 'Development at Bell Labs for Unix' },
            { year: 1978, event: 'K&R C book published' },
            { year: 1989, event: 'ANSI C (C89) standard released' },
            { year: 1999, event: 'C99 major update introduced' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'The language that built the world.',
        funEpisode: "The Unix operating system was originally written in assembly, but was rewritten in C soon after."
    },
    {
        id: 'perl',
        name: 'Perl',
        year: 1987,
        creator: 'Larry Wall',
        useCase: 'CGI, Scripting, System Administration',
        description: 'A highly capable, feature-rich programming language with over 30 years of development.',
        tags: ['Powerful', 'Quirky', 'Legacy'],
        category: 'Scripting',
        color: '#39457e',
        example: `use strict;\nuse warnings;\n\nprint "Hello, Perl!\\n";`,
        history: [
            { year: 1987, event: 'Perl 1.0 released for Unix' },
            { year: 1994, event: 'Perl 5.0 introduces modules and objects' },
            { year: 2000, event: 'Perl 6 (now Raku) development starts' },
            { year: 2020, event: 'Perl 7 announced' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'There is more than one way to do it!',
        funEpisode: "Often called 'the Swiss Army Chainsaw of scripting languages' for its power and occasional complexity."
    },
    {
        id: 'scala',
        name: 'Scala',
        year: 2004,
        creator: 'Martin Odersky',
        useCase: 'Big Data, Enterprise, Functional Programming',
        description: 'Combines object-oriented and functional programming in one concise, high-level language.',
        tags: ['Scalable', 'Typed', 'JVM'],
        category: 'Backend',
        color: '#dc322f',
        example: `object HelloWorld extends App {\n  println("Hello, Scala!")\n}`,
        history: [
            { year: 2001, event: 'Design started at EPFL' },
            { year: 2004, event: 'Public release on JVM' },
            { year: 2011, event: 'Scala 2.9 (Parallel collections)' },
            { year: 2021, event: 'Scala 3 released (major redesign)' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Scalable language for scalable systems.',
        funEpisode: "The name Scala is a portmanteau of 'scalable' and 'language'."
    },
    {
        id: 'elixir',
        name: 'Elixir',
        year: 2011,
        creator: 'José Valim',
        useCase: 'Web, Real-time Systems, Distributed Apps',
        description: 'A dynamic, functional language designed for building scalable and maintainable applications.',
        tags: ['Concurrent', 'Fault-tolerant', 'Web'],
        category: 'Backend',
        color: '#4e2a8e',
        example: `defmodule Hello do\n  def world do\n    IO.puts "Hello, Elixir!"\n  end\nend`,
        history: [
            { year: 2011, event: 'First version released' },
            { year: 2012, event: 'Phoenix web framework development starts' },
            { year: 2014, event: 'Elixir 1.0 released' },
            { year: 2022, event: 'Elixir introduces Nx for AI/ML' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Beaming with concurrency!',
        funEpisode: "Built on top of the Erlang VM, it's used by companies like Discord to handle millions of concurrent users."
    },
    {
        id: 'erlang',
        name: 'Erlang',
        year: 1986,
        creator: 'Joe Armstrong et al. (Ericsson)',
        useCase: 'Telecom, Messaging, Distributed Systems',
        description: 'A functional language used to build massively scalable soft real-time systems with requirements on high availability.',
        tags: ['Distributed', 'Reliable', 'Ancient'],
        category: 'Systems',
        color: '#a90533',
        example: `-module(hello).\n-export([world/0]).\nworld() -> io:format("Hello, Erlang!~n").`,
        history: [
            { year: 1986, event: 'Created at Ericsson Computer Science Lab' },
            { year: 1998, event: 'Open sourced by Ericsson' },
            { year: 2000, event: 'OTP (Open Telecom Platform) standardizes' },
            { year: 2024, event: 'OTP 27 major release' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Let it crash, we have supervisors.',
        funEpisode: "Originally developed by Ericsson in 1986 to power international phone switches."
    },
    {
        id: 'fsharp',
        name: 'F#',
        year: 2005,
        creator: 'Don Syme (Microsoft)',
        useCase: 'Data Science, Finance, Enterprise',
        description: 'A functional-first programming language which empowers everyone to write succinct, robust and performant code.',
        tags: ['Functional', '.NET', 'Concise'],
        category: 'Software',
        color: '#378bba',
        example: `printfn "Hello, F# World!"`,
        history: [
            { year: 2002, event: 'Internal research project at Microsoft' },
            { year: 2005, event: 'F# 1.0 released' },
            { year: 2010, event: 'Integrated into Visual Studio' },
            { year: 2023, event: 'F# 8 released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'The best of functional and OO on .NET.',
        funEpisode: "Started as a research project at Microsoft Research, merging functional and object-oriented paradigms."
    },
    {
        id: 'julia',
        name: 'Julia',
        year: 2012,
        creator: 'Jeff Bezanson et al.',
        useCase: 'Scientific Computing, Data Science, AI',
        description: 'A high-level, high-performance, dynamic programming language for numerical computing.',
        tags: ['Scientific', 'Fast', 'Modern'],
        category: 'Data',
        color: '#9558b2',
        example: `println("Hello, Julia!")\nx = [1, 2, 3]\nsum(x)`,
        history: [
            { year: 2009, event: 'Work started on a faster data language' },
            { year: 2012, event: 'Launch of Julia 0.1' },
            { year: 2018, event: 'Julia 1.0 stable release' },
            { year: 2023, event: 'Julia 1.10 major performance update' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Looks like Python, runs like C.',
        funEpisode: "Designed to be as fast as C but as easy to use as Python - often called the 'Holy Grail' of technical computing."
    },
    {
        id: 'fortran',
        name: 'Fortran',
        year: 1957,
        creator: 'John Backus (IBM)',
        useCase: 'Scientific Research, Weather Prediction, Engineering',
        description: 'The grandfather of high-level languages. Still used for high-performance numerical computing.',
        tags: ['Legacy', 'Scientific', 'Fast'],
        category: 'Systems',
        color: '#4d41b1',
        example: `program hello\n  print *, "Hello, Fortran!"\nend program hello`,
        history: [
            { year: 1954, event: 'Proposal for FORTRAN submitted' },
            { year: 1957, event: 'First compiler released' },
            { year: 1977, event: 'FORTRAN 77 standard (very popular)' },
            { year: 2023, event: 'Fortran 2023 standard released' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Calculating the universe since 1957.',
        funEpisode: "The name stands for FORmula TRANslation. It was the world's first high-level programming language."
    },
    {
        id: 'objc',
        name: 'Objective-C',
        year: 1984,
        creator: 'Brad Cox, Tom Love',
        useCase: 'Apple Platforms, Legacy Apps',
        description: 'The main programming language used by Apple for the OS X and iOS operating systems before Swift.',
        tags: ['Apple', 'Legacy', 'OO'],
        category: 'Software',
        color: '#438eff',
        example: `NSLog(@"Hello, Objective-C World!");`,
        history: [
            { year: 1988, event: 'Licensed by NeXT (Steve Jobs)' },
            { year: 1996, event: 'Apple acquires NeXT' },
            { year: 2007, event: 'Objective-C 2.0 released' },
            { year: 2014, event: 'Handover to Swift starts' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'The heart of NeXT and the early iPhone.',
        funEpisode: "The primary language for Apple's iOS and macOS development for over two decades before Swift."
    },
    {
        id: 'groovy',
        name: 'Groovy',
        year: 2003,
        creator: 'James Strachan',
        useCase: 'Build Tools (Gradle), Automation, JVM',
        description: 'A multi-faceted language for the Java platform that enhances developer productivity.',
        tags: ['JVM', 'Scripting', 'Productive'],
        category: 'Backend',
        color: '#4298b8',
        example: `println "Hello, Groovy World!"`,
        history: [
            { year: 2003, event: 'Initial proposal' },
            { year: 2007, event: 'Groovy 1.0 released' },
            { year: 2012, event: 'Groovy 2.0 introduced' },
            { year: 2020, event: 'Groovy 3.0 released by Apache' }
        ],
        mascot: 'assets/placeholder_mascot.png',
        mascotQuote: 'Making Java a bit more groovy.',
        funEpisode: "A dynamic language for the Java platform that was designed to be as much like Java as possible."
    }
];
