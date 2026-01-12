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
        mascot: 'assets/swift_placeholder.png', // Using placeholder for now
        mascotQuote: 'Fast, safe, and ready for your next big app.'
    }
];

// Add category to existing ones
languages[0].category = 'AI'; // Python
languages[1].category = 'Web'; // JS
languages[2].category = 'Systems'; // Rust
