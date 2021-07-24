const { Builder, By, Key, until } = require('selenium-webdriver');

const script = require('jest');

const URL = 'http://localhost:8080/';
const TIMEOUT = 10000;

let driver = new Builder().forBrowser('firefox').build();

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
