
const { Builder } = require('selenium-webdriver');
const { Options } = require('selenium-webdriver/firefox');
const assert = require('assert');
// const path = require('path');

let driver;

beforeAll(async () => {
  // let capabilities;
  // switch (process.env.BROWSER || "chrome") {
  //   // case "safari": {
  //   //   capabilities = Capabilities.safari();
  //   //   break;
  //   // }
  //   case "firefox": {

      require("geckodriver");
      // Make browser headless
      const options = new Options().headless();

      // Build a firefox driver
      // capabilities = Capabilities.firefox();
      // break;
    // }
    // case "chrome": {
    //   require("chromedriver");
    //   capabilities = Capabilities.chrome();
    //   capabilities.set("chromeOptions", {
    //     args: [
    //       "--headless",
    //       "--no-sandbox",
    //       "--disable-gpu",
    //       "--window-size=1980,1200"
    //     ]
    //   });
    //   break;
    // }
    // default:
    //   throw Error('Unknown browser' + process.env.BROWSER)
  // }
  // driver = await new Builder()
  //   .forBrowser(process.env.BROWSER)
  //   .withCapabilities(capabilities)
  //   .build();

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

