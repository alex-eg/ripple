(in-package :procedural)

"Special package for macros for generating normal and vertex data"

(defmacro hexagonal-grid (w h height radius)
  "w and h are dimensions of the grid, height and radius - of each individual hexagon"
  
