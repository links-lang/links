const { By, Key, until } = require('selenium-webdriver');
const { loadBrowser } = require('../browserDrivers');
const { startServer, DEFAULT_BASE_URL, LINKS_ROOT } = require('../linksServerRunner');

let driver, linksServer;

beforeAll(async () => {
  // Instantiate browser driver
  driver = await loadBrowser();

  // Start Links server
  linksServer = await startServer(`${LINKS_ROOT}/examples/webserver/buttons.links`);
  return linksServer;
});

afterAll(async () => {
  await driver.quit();

  process.kill(-linksServer.pid, 'SIGTERM');
});

test('adds 1 + 2 to equal 3', async () => {
  await driver.get(DEFAULT_BASE_URL);

  // Wait for all elements to be located
  Promise.all(['input_0', 'input_1']
    .map(inputName => driver.wait(until.elementsLocated(By.name(inputName)))));

  await driver.findElement(By.name('input_0')).sendKeys('1');
  await driver.findElement(By.name('input_1')).sendKeys('2', Key.ENTER);

  // Wait for the result to load
  await driver.wait(until.urlIs(DEFAULT_BASE_URL + '/#'));

  var elem = await driver.findElement(By.css('body'));
  var output = await elem.getText();

  expect(output.trim()).toBe('3');
});