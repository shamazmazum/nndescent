# Changelog

## Version 0.2.1

* Improvement: Updated documentation

## Version 0.2

* Incompatible change: `nndescent/point` package is renamed to
  `nndescent/metrics`. The old name is preserved as a nickname though.
* Improvement: Much faster computation of common metrics on SBCL + x86-64.
* `nndescent` now returns the number of iterations and the number of updates
  made on last iteration.
* Improvement: Avoid copying a queue when extracting its elements to a list.
* Improvement: nndescent can now be specialized for metrics which return a
  non-negative single-float number. Add `:nndescent-single` to `*features*`
  before compilation to enable this feature.

## Version 0.1

The first version

* Bruteforce allgorithm for k-NN searches
* nndescent algorithm for approximate k-NN searches
