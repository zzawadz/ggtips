//'mocha', 'chai', 'chance'

describe('StartTest', () => {
    beforeEach(() => {
        cy.visit('http://shiny:3838');
    });
    it('should have text HELLO', () => {
        cy.contains('HELLO');
    });
    it('should display text WORLD when click', () => {
        cy.get('#do').click();
        cy.contains('WORLD', { timeout: 8000 });
    });
});

