const { Builder, By, Key, until } = require('selenium-webdriver');

const URL = 'http://localhost:8080/';
const TIMEOUT = 10000;

(async function example() {
  let driver = await new Builder().forBrowser('firefox').build();
  try {
    await driver.get(URL);

    const inputLeft = By.name('input_0');
    const inputRight = By.name('input_1');

    Promise.all(['input_0', 'input_1'].map(inputName => driver.wait(until.elementsLocated(By.name(inputName)), TIMEOUT)));

    await driver.findElement(inputLeft).sendKeys('1');
    await driver.findElement(inputRight).sendKeys('2', Key.ENTER);

    // Wait for the result to load
    await driver.wait(until.urlIs(URL + '#'));

    var elem = await driver.findElement(By.css('body'));
    console.log(' Assert ', await elem.getText());

  } finally {
    await driver.quit();
  }
})();