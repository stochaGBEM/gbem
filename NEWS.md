# gbem (development version)

* gbem no longer supports built-in notion of cross sections; it now operates on cross sections made available through the sxchan package. 
* Almost all functions not related to hydrographs have breaking changes, including `gbem()`, `eroding_flow()`, and `min_stable_channel()`.
* New functions exist for specifying channel cross section features that are needed, depending on the erosion engine being applied: `sx_*()`.

# gbem 0.1.0

* gbem interface applied to a non-spatial cross section.
