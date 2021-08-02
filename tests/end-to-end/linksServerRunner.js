const { builtinModules } = require('module');

const LINKS_DIR = '../..';
const LINKS_EXEC = `${LINKS_DIR}/links`

module.exports = {
  startServer: (linksScriptPath) => {
    process = require('child_process').spawn(`${LINKS_EXEC} ${linksScriptPath} --debug`, {
      detached: true,
      stdio: 'inherit', // print the child process stdoutinto the Nodes stdout
      shell: true
    });
    process.unref();

    // TODO: Find workaround to wait for the server to start.
    // The following line produces an uncondtiional timeout.
    return new Promise(resolve => {
      setTimeout(() => {
        resolve(process)
      }, 5000)
    });
  },
  DEFAULT_BASE_URL: 'http://localhost:8080/'
}