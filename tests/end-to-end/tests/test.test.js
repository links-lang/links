const { Builder } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');
const assert = require('assert');

let driver;

beforeAll(async () => {
  require("geckodriver");
  const options = new Options().headless();
  driver = await new Builder()
    .forBrowser('firefox')
    .setFirefoxOptions(options)
    .build();
});

afterAll(async () => {
  console.log("== Driver: " + driver);
  await driver.quit()
});

it("Google", async () => {
  await driver.get('http://localhost:8125');
  assert.equal(await driver.getTitle(), "HelloWorld");
});