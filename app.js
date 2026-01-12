/**
 * Simple SPA Router & View Engine
 */

const App = {
    state: {
        searchQuery: '',
        activeCategory: 'All',
        // Get the repository name from path if we're on GH Pages
        basePath: window.location.pathname.endsWith('/') ? window.location.pathname.slice(0, -1) : window.location.pathname
    },

    init() {
        this.mainElement = document.getElementById('main-content');
        this.navLinks = document.querySelectorAll('.nav-link');
        this.setupEventListeners();
        this.handleRoute();
    },

    setupEventListeners() {
        // Handle back/forward navigation
        window.addEventListener('popstate', () => this.handleRoute());

        // Intercept clicks on internal links
        document.addEventListener('click', (e) => {
            const link = e.target.closest('a');
            if (link && link.getAttribute('href')) {
                const href = link.getAttribute('href');
                if (href.startsWith('/') || href.startsWith('./')) {
                    e.preventDefault();
                    this.navigate(href);
                }
            }
        });

        // Delegate search and filter events
        document.addEventListener('input', (e) => {
            if (e.target.id === 'search-input') {
                this.state.searchQuery = e.target.value.toLowerCase();
                this.applyFilters();
            }
        });

        document.addEventListener('click', (e) => {
            if (e.target.classList.contains('filter-btn')) {
                this.state.activeCategory = e.target.dataset.category;
                this.applyFilters();
            }
        });
    },

    navigate(path) {
        window.history.pushState({}, '', path);
        this.handleRoute();
    },

    handleRoute() {
        const path = window.location.pathname;
        const params = new URLSearchParams(window.location.search);
        const langId = params.get('lang');

        if (langId) {
            this.renderDetail(langId);
        } else {
            this.renderListing();
        }

        this.updateNavState(path);
    },

    updateNavState(path) {
        this.navLinks.forEach(link => {
            if (link.getAttribute('href') === path) {
                link.classList.add('active');
            } else {
                link.classList.remove('active');
            }
        });
    },

    applyFilters() {
        const filtered = languages.filter(lang => {
            const matchesSearch = lang.name.toLowerCase().includes(this.state.searchQuery) ||
                lang.description.toLowerCase().includes(this.state.searchQuery);
            const matchesCategory = this.state.activeCategory === 'All' || lang.category === this.state.activeCategory;
            return matchesSearch && matchesCategory;
        });

        this.updateGrid(filtered);
        this.updateFilterButtons();
    },

    updateFilterButtons() {
        document.querySelectorAll('.filter-btn').forEach(btn => {
            if (btn.dataset.category === this.state.activeCategory) {
                btn.classList.add('active');
            } else {
                btn.classList.remove('active');
            }
        });
    },

    updateGrid(filteredLanguages) {
        const grid = document.querySelector('.languages-grid');
        if (!grid) return;

        if (filteredLanguages.length === 0) {
            grid.innerHTML = `<div class="no-results">No languages found matching your criteria.</div>`;
            return;
        }

        grid.innerHTML = filteredLanguages.map(lang => `
            <a href="./?lang=${lang.id}" class="lang-card" data-id="${lang.id}">
                <div class="card-header">
                    <div class="card-icon" style="color: ${lang.color}">
                        <i data-lucide="code"></i>
                    </div>
                    <span class="card-year">${lang.year}</span>
                </div>
                <h3>${lang.name}</h3>
                <p>${lang.description}</p>
                <div class="card-badge">${lang.category}</div>
                <div class="card-footer">
                    ${lang.tags.map(tag => `<span class="tag">${tag}</span>`).join('')}
                </div>
            </a>
        `).join('');
        lucide.createIcons();
    },

    renderFeatured() {
        const featured = languages.find(l => l.id === 'rust') || languages[0]; // Currently highlight Rust
        return `
            <section class="featured-banner" onclick="App.navigate('./?lang=${featured.id}')">
                <div class="featured-badge">Featured Language</div>
                <div class="featured-content">
                    <div class="featured-text">
                        <h2>${featured.name}</h2>
                        <p>${featured.description}</p>
                    </div>
                    <div class="featured-image" style="background-image: url('${featured.mascot}')"></div>
                </div>
            </section>
        `;
    },

    renderListing() {
        const categories = ['All', ...new Set(languages.map(l => l.category))];

        let html = `
            <section class="hero">
                <h1>LanguaPedia</h1>
                <p>Explore the evolution and secrets of the world's most influential programming languages.</p>

                <div class="controls">
                    <div class="search-bar">
                        <i data-lucide="search"></i>
                        <input type="text" id="search-input" placeholder="Search languages..." value="${this.state.searchQuery}">
                    </div>
                    <div class="filters">
                        ${categories.map(cat => `
                            <button class="filter-btn ${this.state.activeCategory === cat ? 'active' : ''}" data-category="${cat}">${cat}</button>
                        `).join('')}
                    </div>
                </div>
            </section>
            
            ${this.state.searchQuery === '' && this.state.activeCategory === 'All' ? this.renderFeatured() : ''}

            <div class="languages-grid">
        `;

        this.mainElement.innerHTML = html;
        this.applyFilters();
        lucide.createIcons();
    },

    renderDetail(id) {
        const lang = languages.find(l => l.id === id);
        if (!lang) {
            this.mainElement.innerHTML = `<h1>Language not found</h1><a href="./">Back to home</a>`;
            return;
        }

        this.mainElement.innerHTML = `
            <div class="detail-view">
                <button onclick="App.navigate('./')" class="back-button">
                    <i data-lucide="arrow-left"></i> Back to Listing
                </button>
                
                <header class="detail-header">
                    <div class="header-main">
                        <h1 style="border-left: 8px solid ${lang.color}; padding-left: 1rem;">${lang.name}</h1>
                        <p class="creator-info">Created in <strong>${lang.year}</strong> by <strong>${lang.creator}</strong></p>
                    </div>
                    <div class="mascot-section">
                        <div class="mascot-bubble">
                            <p>"${lang.mascotQuote}"</p>
                            <div class="bubble-tail"></div>
                        </div>
                        <div class="mascot-avatar" style="background-image: url('${lang.mascot}')">
                            <!-- Placeholder for generated mascot -->
                            <div class="mascot-placeholder">Mascot</div>
                        </div>
                    </div>
                </header>

                <div class="detail-grid">
                    <section class="info-card">
                        <h2><i data-lucide="info"></i> Overview</h2>
                        <p>${lang.description}</p>
                        <div class="use-cases">
                            <strong>Main Use Cases:</strong>
                            <p>${lang.useCase}</p>
                        </div>
                    </section>

                    <section class="code-card">
                        <h2><i data-lucide="terminal"></i> Example Code</h2>
                        <pre><code>${this.escapeHTML(lang.example)}</code></pre>
                    </section>
                    
                    <section class="diagram-card">
                        <h2><i data-lucide="layout"></i> Architecture/Flow</h2>
                        <div class="diagram-placeholder">
                            <div class="flow-step">Source Code</div>
                            <div class="flow-arrow"><i data-lucide="arrow-right"></i></div>
                            <div class="flow-step highlighted">${lang.id === 'cpp' || lang.id === 'rust' ? 'Compiler' : 'Interpreter'}</div>
                            <div class="flow-arrow"><i data-lucide="arrow-right"></i></div>
                            <div class="flow-step">Executable/Runtime</div>
                        </div>
                    </section>

                    <section class="history-card">
                        <h2><i data-lucide="history"></i> Key History</h2>
                        <div class="timeline">
                            ${lang.history.map(item => `
                                <div class="timeline-item">
                                    <span class="timeline-year">${item.year}</span>
                                    <span class="timeline-event">${item.event}</span>
                                </div>
                            `).join('')}
                        </div>
                    </section>
                </div>
            </div>
        `;
        lucide.createIcons();
    },

    escapeHTML(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
};

document.addEventListener('DOMContentLoaded', () => App.init());
