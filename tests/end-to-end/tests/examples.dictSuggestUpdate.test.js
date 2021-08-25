const { By, until } = require('selenium-webdriver');
const { loadBrowser } = require('../browserDrivers');
const { startServer, DEFAULT_BASE_URL, LINKS_ROOT } = require('../linksServerRunner');

let driver, linksServer;

beforeAll(async () => {
  // Instantiate browser driver
  driver = await loadBrowser();

  // Start Links server
  linksServer = await startServer(`${LINKS_ROOT}/examples/webserver/examples.links`,
    `${LINKS_ROOT}/examples:${LINKS_ROOT}/examples/games:${LINKS_ROOT}/examples/handlers:${LINKS_ROOT}/examples/dictionary`,
    'config=linksconfig'
  );

  return linksServer;
});

afterAll(async () => {
  await driver.quit();

  process.kill(-linksServer.pid, 'SIGTERM');
});

test('Test dictSuggestUpdate', async () => {
  await driver.get(`${DEFAULT_BASE_URL}/examples/dictionary/dictSuggestUpdate.links`);

  const searchBar = By.xpath('/html/body/form/input');
  const searchResultTable = By.css('#suggestions > div > table');

  const newWordInput = By.xpath('/html/body/div[2]/div/form/table/tbody/tr[1]/td[2]/input');
  const newWordMeaningInput = By.xpath('/html/body/div[2]/div/form/table/tbody/tr[2]/td[2]/textarea');
  const newWordAddButton = By.css('#add > form > table > tbody > tr:nth-child(3) > td > button');

  // Add new word "Dee"
  await driver.findElement(newWordInput).sendKeys('Dee');
  await driver.findElement(newWordMeaningInput).sendKeys('A very important person');
  await driver.findElement(newWordAddButton).click();

  // search Dee
  await driver.findElement(searchBar).sendKeys('Dee');

  // confirm search result showing
  await driver.wait(until.elementLocated(searchResultTable));
  const searchResult = await driver.findElement(searchResultTable);
  const tableItems = await searchResult.findElements(By.xpath('*'))
    .then(elements => elements.map(async (element) => {
      const word = await element.findElement(By.xpath('td[1]'));
      const definition = await element.findElement(By.xpath('td[2]/span'));
      return [await word.getText(), await definition.getText()];
    })).catch(err => {
      throw err;
    });

  Promise.all(tableItems).then(items => {
    expect(items).toContainEqual(['Dee', 'A very important person']);
  });
});