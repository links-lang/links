
const { Builder } = require('selenium-webdriver');
const { Options } = require(`selenium-webdriver/${process.env.BROWSER}`);

module.exports = {
  loadBrowser: () => {
    const options = new Options().headless();

    switch (process.env.BROWSER) {
      case 'firefox':
        require('geckodriver');
        return new Builder()
          .forBrowser(process.env.BROWSER)
          .setFirefoxOptions(options)
          .build();
      case 'chrome':
        return new Builder()
          .forBrowser(process.env.BROWSER)
          .setChromeOptions(options)
          .build();

    }

  }
}