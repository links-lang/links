# End-to-end Testing
Automated browser testing to test Links.
Designed to run as part of the GitHub Actions pipeline for Links.

## How to run locally
1. Build Links in the root of the repository.
2. Run `npm install` in the current repository
3. (Optional) Set env variable `LINKS_BROWSER` to be either `chrome` (default) or `firefox`
4. (Optional) Modify `linksconfig` file to set the port, hostname, and the database config.
5. Run `npm test`

## Troubleshooting
- To test Links server with database, make sure the database is set up as expected
- The server will fail to start if the port is used by some other process. Some useful shell commands:
  - To check the port `8080`:
    ```
    lsof -l 8080
    ```
  - To kill an existing Links process:
    ```
    killall links.exe
    ```