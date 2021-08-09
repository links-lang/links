const { By, Key, until } = require('selenium-webdriver');
const { loadBrowser } = require('../browserDrivers');
const { startServer, DEFAULT_BASE_URL, LINKS_ROOT } = require('../linksServerRunner');

let driver, linksServer;

beforeAll(async () => {
  // Instantiate browser driver
  driver = await loadBrowser();

  // Start Links server
  linksServer = await startServer(`${LINKS_ROOT}/examples/webserver/examples.links`,
    `${LINKS_ROOT}/examples:${LINKS_ROOT}/examples/games:${LINKS_ROOT}/examples/handlers:${LINKS_ROOT}/examples/dictionary`
  );

  return linksServer;
});

afterAll(async () => {
  await driver.quit();
  linksServer.kill('SIGINT');
});

test('Check factorial up to 64', async () => {
  await driver.get(`${DEFAULT_BASE_URL}/examples/factorial.links`);

  // Type '64' in the text box
  // Due to refresh, the element need to be located twice.
  await driver.findElement(By.xpath('/html/body/form/input[1]')).sendKeys('6');
  await driver.findElement(By.xpath('/html/body/form/input[1]')).sendKeys('4');

  await driver.findElement(By.xpath('/html/body/form/input[2]'))
    .click();

  // Find tables
  await driver.wait(until.elementsLocated(By.xpath('/html/body/table/tbody')));
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