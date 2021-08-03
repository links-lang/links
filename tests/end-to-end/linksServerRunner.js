const fetch = require('node-fetch')
const LINKS_ROOT = __dirname + '/../..';
const LINKS_EXEC = `${LINKS_ROOT}/links`
const DEFAULT_BASE_URL = 'http://localhost:8080/';

// Delay function
async function sleep(sec) {
  await new Promise(resolve => {
    setTimeout(resolve, sec);
  });
}

module.exports = {
  startServer: async (linksScriptPath) => {

    linksServer = require('child_process').spawn(`${LINKS_EXEC} ${linksScriptPath} --debug`, {
      detached: true,
      // stdio: 'inherit', // Print the Links stdout into the Node stdout
      stdio: 'ignore',  // Do not print the Links log
      shell: true
    });

    return new Promise(async (resolve, reject) => {

      linksServer.on('exit', (code) => {
        reject(`Links server exited with Code ${code}. Is the given Links script working?`);
        return;
      });

      // Sleep to detect exit
      await sleep(2000);

      linksServer.unref();

      const TRIAL_COUNT = 10
      for (var i = 1; i <= TRIAL_COUNT; i++) {
        // Some delay
        await sleep(2000);

        console.log(`(${i + 1}) request URL`)

        try {
          let response = await fetch(DEFAULT_BASE_URL);
          if (response.ok) {
            console.log(`Request ${i} successful.`)
            resolve();
            return;
          }
        } catch (e) { /* Ignore error and try again */ }

      }

      // else:
      reject();

    });
  },
  DEFAULT_BASE_URL: DEFAULT_BASE_URL,
  LINKS_ROOT: LINKS_ROOT
}