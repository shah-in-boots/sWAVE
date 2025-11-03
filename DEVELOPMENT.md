# sWAVE Development Guide

## Quick Start

To run the app in development mode:

```r
# Load the package
devtools::load_all()

# Run the app
run_app()
```

Or use the provided dev script:

```r
source("dev/run_dev.R")
```

## Project Structure

This is a {golem} Shiny application with the following structure:

### Core Files

- `R/run_app.R` - Main function to launch the app
- `R/app_ui.R` - User interface definition
- `R/app_server.R` - Server logic
- `R/app_config.R` - Configuration utilities

### Modules

- `R/mod_file_upload.R` - File upload module

### Configuration

- `inst/golem-config.yml` - App configuration for different environments
- `inst/app/www/` - Static assets (CSS, JS, images)

## Adding New Modules

To add a new module:

```r
golem::add_module(name = "my_module", with_test = TRUE)
```

This creates:
- `R/mod_my_module.R` - Module code
- `tests/testthat/test-mod_my_module.R` - Module tests

## Development Workflow

1. Make changes to your code
2. Document: `devtools::document()`
3. Load: `devtools::load_all()`
4. Test: `devtools::test()`
5. Check: `devtools::check()`
6. Run: `run_app()`

## Testing

Run all tests:

```r
devtools::test()
```

Test coverage:

```r
covr::package_coverage()
```

## File Upload Module

The file upload module supports:
- CSV files (`.csv`)
- Text files (`.txt`)
- R data files (`.rds`, `.RData`)

The module returns a reactive value containing the loaded data, which can be used by other modules.
