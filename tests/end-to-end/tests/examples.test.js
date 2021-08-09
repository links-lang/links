const { By, Key, until } = require('selenium-webdriver');
const { loadBrowser } = require('../browserDrivers');
const { startServer, DEFAULT_BASE_URL, LINKS_ROOT } = require('../linksServerRunner');

let driver, linksServer;

beforeAll(async () => {
  // Instantiate browser driver
  driver = await loadBrowser();

  // Start Links server
  linksServer = await startServer(`${LINKS_ROOT}/examples/webserver/examples.links`,
    `${LINKS_ROOT}/examples:${LINKS_ROOT}/examples/games:${LINKS_ROOT}/examples/handlers:${LINKS_ROOT}/examples/dictionary`
  );

  return linksServer;
});

afterAll(async () => {
  await driver.quit();
  linksServer.kill('SIGINT');
});

test('something', async () => {
  await driver.get(`${DEFAULT_BASE_URL}/examples/factorial.links`);
});

// test("Google", async () => {
//     await driver.get('http://google.com');
//     expect(await driver.getTitle()).toBe("Google");
// });

// test("True", () => {
//     expect(1).toBe(1);
// })