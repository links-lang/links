const { By, until, Condition, error } = require('selenium-webdriver');
const { loadBrowser } = require('../browserDrivers');
const { startServer, DEFAULT_BASE_URL, LINKS_ROOT } = require('../linksServerRunner');

let driver, linksServer;

beforeAll(async () => {
  // Instantiate browser driver
  driver = await loadBrowser();

  // Start Links server
  linksServer = await startServer(`${LINKS_ROOT}/examples/webserver/progress.links`);
  return linksServer;
});

afterAll(async () => {
  await driver.quit();

  process.kill(-linksServer.pid, 'SIGTERM');
});

test('Count up to 1234', async () => {

  await driver.get(DEFAULT_BASE_URL);

  const inputBox = By.name('lname__g135');
  const outputBar = By.id('bar');
  const submitButton = By.xpath('/html/body/form/input[2]');

  // Wait for all elements to be located
  await driver.wait(until.elementsLocated(inputBox));

  // TODO: try send keys
  // Insert 1234 into the input box
  for (let n = 1; n <= 4; n++) {
    await driver.findElement(inputBox).sendKeys(n);
  }

  await driver.findElement(submitButton).click();

  // Wait for the progress bar to fill up
  await driver.wait(
    new Condition("until progress bar is complete", async (driver) => {
      try {
        let bar = await driver.findElement(outputBar);
        return await bar.getCssValue('width');
      } catch (e) {
        if (e instanceof error.StaleElementReferenceError) {
          // ignore
        } else {
          throw e;
        }
      }
    }));

  const elem = driver.findElement(outputBar);
  const outputText = await elem.getText();
  expect(outputText.trim()).toBe('done counting to 1234');
});