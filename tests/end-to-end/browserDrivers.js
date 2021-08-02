
const { Builder } = require('selenium-webdriver');

module.exports = {
  loadFirefox: () => {
    const { Options } = require('selenium-webdriver/firefox');

    // Load Firefox browser engine
    require('geckodriver');

    // Make browser headless
    const options = new Options().headless();

    return new Builder()
      .forBrowser('firefox')
      .setFirefoxOptions(options)
      .build();
  }
}