const { Builder, By, Key, until } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');


const URL = 'http://localhost:8080/';
const TIMEOUT = 10000;

// Make browser headless
const options = new Options().headless();

// Build a firefox driver
const driver = new Builder().forBrowser('firefox').setFirefoxOptions(options).build();

test('adds 1 + 2 to equal 3', async () => {
  await driver.get(URL);

  const inputLeft = By.name('input_0');
  const inputRight = By.name('input_1');

  Promise.all(['input_0', 'input_1'].map(inputName => driver.wait(until.elementsLocated(By.name(inputName)), TIMEOUT)));

  await driver.findElement(inputLeft).sendKeys('1');
  await driver.findElement(inputRight).sendKeys('2', Key.ENTER);

  // Wait for the result to load
  await driver.wait(until.urlIs(URL + '#'));

  var elem = await driver.findElement(By.css('body'));
  var output = await elem.getText();

  expect(output.trim()).toBe('3');
});
