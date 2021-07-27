const { Builder, By, Key, until } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');


const URL = 'http://localhost:8080/';
const TIMEOUT = 10000;

jest.setTimeout(TIMEOUT);

let driver;

async function startServer() {
  const command = "cd ../../ && ./links examples/webserver/buttons.links";
  const util = require('util');
  const exec = util.promisify(require('child_process').exec);
  try {
    const { stdout, stderr } = await exec(command);
    console.log('stdout:', stdout);
    console.log('stderr:', stderr);
    return;
  } catch (err) {
    console.error(err);
  }
}

beforeAll(async () => {
  startServer();

  require('geckodriver');

  // Make browser headless
  const options = new Options()
    // .headless();

  driver = await new Builder()
    .forBrowser('firefox')
    .setFirefoxOptions(options)
    .build();
});

afterAll(async () => {
  await driver.quit();
})

test('adds 1 + 2 to equal 3', async () => {
  await driver.get(URL);

  const inputLeft = By.name('input_0');
  const inputRight = By.name('input_1');

  Promise.all(['input_0', 'input_1'].map(inputName => driver.wait(until.elementsLocated(By.name(inputName)), TIMEOUT)));

  await driver.findElement(inputLeft).sendKeys('1');
  await driver.findElement(inputRight).sendKeys('2', Key.ENTER);

  // Wait for the result to load
  await driver.wait(until.urlIs(URL + '#'));

  var elem = await driver.findElement(By.css('body'), TIMEOUT);
  var output = await elem.getText();

  expect(output.trim()).toBe('3');
});
