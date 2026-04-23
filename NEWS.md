# palettephines 0.1.3

## Documentation and Standards
* **Enhanced Scientific Grounding:** Added canonical links to biological reference monographs for the BBCH-scale and Reef Health Index.
* **Acronym Expansion:** Expanded all technical acronyms in the package description to ensure clarity for non-domain experts.
* **Formal Return Values:** Added `@return` tags to all exported functions, detailing output class and the biological meaning of the resulting data/graphics.

## Internal Improvements
* **State Mapping:** Improved `cite_phines()` to provide more descriptive character strings combining common names and scientific codes.
* **Namespace Safety:** Updated all examples and vignettes to respect user graphical parameters (`par`) by ensuring they are captured and reset after use.

## Bug Fixes
* Resolved length mismatch errors in spatial data processing workflows.
* Updated internal metadata to align perfectly with the latest agricultural maturity stages.

