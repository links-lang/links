const { By, Key, until } = require('selenium-webdriver');
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

test('Check factorial up to 64', async () => {
  await driver.get(`${DEFAULT_BASE_URL}/examples/factorial.links`);

  const inputBox = By.xpath('/html/body/form/input[1]');
  const button = By.xpath('/html/body/form/input[2]');

  // Type '64' in the text box
  // Due to refresh, the element need to be located twice.
  await driver.findElement(inputBox).sendKeys('6');
  await driver.findElement(inputBox).sendKeys('4');
  await driver.findElement(button).click();

  const table = By.xpath('/html/body/table/tbody');
  // Find tables
  await driver.wait(until.elementsLocated(table));
  let table = await driver.findElement(By.xpath('/html/body/table/tbody'));

  // Find all rows
  let rows = await table.findElements(By.css('tr'));
  let factAccum = 1;

  for await (const row of rows.map(r => r.findElements(By.css('td')))) {

    // Extract two values from a row
    let i = parseInt(await (row[0].getText()));
    let fact = parseInt(await (row[1].getText()));

    // Accumulate factorial value
    factAccum *= i;

    expect(fact).toBe(factAccum);
  }
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
    .then(elements => elements.map(async element => {
      const word = await element.findElement(By.xpath('td[1]'));
      const definition = await element.findElement(By.xpath('td[2]/span'));
      return [await word.getText(), await definition.getText()];
    }));

  Promise.all(tableItems).then(items => {
    expect(items).toContainEqual(['Dee', 'A very important person']);
  });
});