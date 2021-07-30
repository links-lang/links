const { Builder, By, Key, until } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');


const URL = 'http://localhost:8080/';
const TIMEOUT = 10000 * 2;

jest.setTimeout(TIMEOUT);

let driver;

function execShellCommand(cmd) {
 const exec = require('child_process').exec;
 return new Promise((resolve, reject) => {
  exec(cmd, (error, stdout, stderr) => {
   if (error) {
    console.warn(error);
   }
   resolve(stdout? stdout : stderr);
  });
 });
}

async function startServer() {
  const command = "../../links ../../examples/webserver/buttons.links & ";
  const exec= require('util').promisify(require('child_process').exec);
  
  exec(command, {detached: true}).then( (p) => {
    console.log('error', process.error);
    console.log('stdout ', process.stdout);
    console.log('stderr ', process.stderr);
  });

  // TODO: Find workaround to wait for the server to start.
  // The following line produces an uncondtiional timeout.
  return new Promise(resolve => setTimeout(resolve, 10000));
}

beforeAll(async () => {
  require('geckodriver');
  await startServer();
  
  // Make browser headless
  const options = new Options().headless();

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

test("Google", async () => {
  await driver.get('http://google.com');
  expect(await driver.getTitle()).toBe("Google");
});

test("True", () => {
  expect(1).toBe(1);
})