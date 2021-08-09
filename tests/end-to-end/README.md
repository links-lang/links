# End-to-end Testing
Automated browser testing to test Links.
Designed to run as part of the GitHub Actions pipeline for Links.

## How to run locally
1. Build Links in the root of the repository.
2. Run `npm install` in the current repository
3. (Optional) Set env variable `LINKS_BROWSER` to be either `chrome` (default) or `firefox`
4. (Optional) MOdify `linksconfig` file to set the port and the hostname. 
5. Run `npm test`
