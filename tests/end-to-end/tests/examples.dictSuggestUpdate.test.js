const { By, error, until, Condition } = require('selenium-webdriver');
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

  const newWordInput = By.xpath('/html/body/div[2]/div/form/table/tbody/tr[1]/td[2]/input');
  const newWordMeaningInput = By.xpath('/html/body/div[2]/div/form/table/tbody/tr[2]/td[2]/textarea');
  const newWordAddButton = By.css('#add > form > table > tbody > tr:nth-child(3) > td > button');
  const allChildren = By.xpath('*');

  // Add new word "Dee"
  await driver.findElement(newWordInput).sendKeys('Dee');
  await driver.findElement(newWordMeaningInput).sendKeys('A very important person');
  await driver.findElement(newWordAddButton).click();

  // search Dee
  await driver.findElement(searchBar).sendKeys('Dee');

  const searchResultTable = By.css('#suggestions > div > table');
  await driver.wait(until.elementLocated(searchResultTable));
  await driver.wait(
    new Condition("until progress bar is complete", async (driver) => {
      try {
        let table = await driver.findElement(searchResultTable);
        return await table.findElements(allChildren);
      } catch (e) {
        if (e instanceof error.StaleElementReferenceError) {
          // ignore
        } else {
          throw e;
        }
      }
    }));

  // confirm search result showing
  const searchResult = await driver.findElement(searchResultTable);
  let tableItems = (await searchResult.findElements(allChildren))
    .map(async (element) => {
      const word = await element.findElement(By.xpath('td[1]'))
        .then(async elem => await elem.getText());
      const definition = await element.findElement(By.xpath('td[2]/span'))
        .then(async elem => await elem.getText());
      return [word, definition];
    });

  let resolvedItems;
  await Promise.all(tableItems).then(items => {
    resolvedItems = items;
  });

  expect(resolvedItems).toContainEqual(['Dee', 'A very important person']);
});