# End-to-end Testing
Automated browser testing to test Links.
Designed to run as part of the GitHub Actions pipeline for Links.

## How to run locally
### Steps
1. Build Links in the root of the repository.
2. Run `npm install` in the current repository
3. (Optional) Setup the following environment variables
- `BROWSER` to be either `chrome` (default) or `firefox`
- `LINKS_PORT` to match the port number specified in Links config file (default `8080`)
3. Run `npm test`
