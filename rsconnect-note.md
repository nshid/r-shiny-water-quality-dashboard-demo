# shinyapps.io deployment notes

This file is a placeholder for environment-specific deployment notes.

Suggested items to capture before production-style demos:

- shinyapps.io application name and account owner
- `rsconnect::setAccountInfo()` setup completed
- deployment region and organization owner
- any package installation warnings observed during deploy
- final app URL used for client review
- iframe host domains approved for prototype validation
- notes on cross-domain embedding tests, authentication flows, and browser behavior

Recommended deployment command:

```r
rsconnect::deployApp(appDir = ".", appFile = "app.R")
```

If this prototype will be embedded into a public government platform, document:

- expected iframe dimensions
- whether anonymous access is allowed
- whether the host platform adds authentication wrappers
- any content-security-policy or frame-ancestor requirements
