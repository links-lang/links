const { Builder, By, Key, until } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');


const URL = 'http://localhost:8080/';
const TIMEOUT = 50000;

jest.setTimeout(TIMEOUT);

let driver;
let process;

async function startServer() {
  const command = "../../links ../../examples/webserver/buttons.links --debug";

  process = require('child_process').spawn(command, {
    detached: true,
    stdio: 'inherit', // print the child process stdoutinto the Nodes stdout
    shell: true
  });
  process.unref();

  // TODO: Find workaround to wait for the server to start.
  // The following line produces an uncondtiional timeout.
  return new Promise(resolve => setTimeout(resolve, 5000));
}

beforeAll(async () => {
  // Make browser headless
  const options = new Options().headless();

  require('geckodriver');
  driver = await new Builder()
    .forBrowser('firefox')
    .setFirefoxOptions(options)
    .build();

  await startServer();
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

test("Google", async () => {
  await driver.get('http://google.com');
  expect(await driver.getTitle()).toBe("Google");
});

test("True", () => {
  expect(1).toBe(1);
})