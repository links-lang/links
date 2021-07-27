const { Builder, By, Key, until } = require('selenium-webdriver')
require('selenium-webdriver/chrome')
require('selenium-webdriver/firefox')

const rootURL = 'https://www.mozilla.org/en-US/'
let driver

beforeAll(async () => {
  driver = await new Builder().forBrowser('firefox').build()
})

afterAll(async () => driver.quit())

it('initialises the context', async () => {
  await driver.get(rootURL)
})
