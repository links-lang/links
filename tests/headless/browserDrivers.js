const BROWSER = process.env.LINKS_BROWSER || 'chrome';
const { Builder } = require('selenium-webdriver');

let Options;

module.exports = {
  loadBrowser: () => {
    switch (BROWSER) {
      case 'firefox':
        Options = require(`selenium-webdriver/firefox`).Options;
        require('geckodriver'); // Load Firefox engine
        return new Builder()
          .forBrowser(BROWSER)
          .setFirefoxOptions(new Options().headless())
          .build();
      case 'chrome':
        Options = require(`selenium-webdriver/chrome`).Options;
        return new Builder()
          .forBrowser(BROWSER)
          .setChromeOptions(new Options().headless())
          .build();
      default:
        throw new Error(`Browser name ${BROWSER} not recognised`);
    }
  }
};