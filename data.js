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
        color: '#00599c',
        example: `#include <iostream>
int main() {
    std::cout << "Hello, C++!" << std::endl;
    return 0;
}`,
        history: [
            { year: 1979, event: 'C with Classes development started' },
            { year: 1983, event: 'Renamed to C++' },
            { year: 1998, event: 'C++98 standard released' },
            { year: 2011, event: 'C++11 modernization' }
        ],
        mascot: 'assets/cpp_mascot.png',
        mascotQuote: 'With great power comes great responsibility... and manual memory management.'
    }
];
