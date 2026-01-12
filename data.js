const languages = [
    {
        id: 'python',
        name: 'Python',
        year: 1991,
        creator: 'Guido van Rossum',
        useCase: 'Data Science, Web, AI',
        description: 'A high-level, interpreted programming language known for its readability and vast library ecosystem.',
        tags: ['Dynamic', 'Easy', 'Popular'],
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
        mascotQuote: 'Indent your code, not your life!'
    },
    {
        id: 'javascript',
        name: 'JavaScript',
        year: 1995,
        creator: 'Brendan Eich',
        useCase: 'Web, Server, Mobile',
        description: 'The language of the web. Ubiquitous and extremely flexible, powering almost every modern website.',
        tags: ['Async', 'Web', 'Flexible'],
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
        mascotQuote: 'I might be quirky, but I run the world!'
    },
    {
        id: 'rust',
        name: 'Rust',
        year: 2010,
        creator: 'Graydon Hoare (Mozilla)',
        useCase: 'Systems, WebAssembly, Performance',
        description: 'A systems programming language that provides memory safety without garbage collection.',
        tags: ['Safe', 'Fast', 'Modern'],
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
        mascotQuote: 'Borrow checker is your best friend. Trust me.'
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
        mascotQuote: 'With great power comes great responsibility... and manual memory management.'
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
        mascotQuote: 'Keep it simple, keep it fast, keep it concurrent!'
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
        mascotQuote: 'Object-oriented from head to toe!'
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
        mascotQuote: 'Fast, safe, and ready for your next big app.'
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
        mascotQuote: 'Ruby is designed for programmer happiness!'
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
        mascotQuote: 'Static types are a gift to your future self.'
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
        mascotQuote: 'I power 75% of the web! Not bad, right?'
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
        mascotQuote: 'Concise, safe, and interoperable.'
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
        mascotQuote: 'From enterprise apps to triple-A games, I do it all.'
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
        mascotQuote: 'Your data is safe with me.'
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
        mascotQuote: 'Let the data tell its story.'
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
        mascotQuote: 'Ready to fly with Flutter.'
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
        mascotQuote: 'Small, fast, and easy to fit in.'
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
        mascotQuote: 'Purely functional, strictly academic.'
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
        mascotQuote: 'I am the backbone of global finance since 1959.'
    }
];

// Set categories for initial languages
languages.find(l => l.id === 'python').category = 'AI';
languages.find(l => l.id === 'javascript').category = 'Web';
languages.find(l => l.id === 'rust').category = 'Systems';
languages.find(l => l.id === 'cpp').category = 'Systems';
languages.find(l => l.id === 'go').category = 'Backend';
languages.find(l => l.id === 'java').category = 'Enterprise';
