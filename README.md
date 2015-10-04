# locate-in-region
racket implementation of region mapping
## To run:
- install racket
- set up the path to racket (e.g, in .bash_profile) and activate the path
- racket fileIO.rkt
- The points that you want to find need to be in the file *points.txt*
- The region definitions in the file: *region-definitions.txt*
- The solutions will be placed in the file *solution.txt*

(solve-enclosing-regions points-path-name region-path-name ** solution-path-name ** ... still need to implement this )

###points.txt
- The format is:
- point1: xvalue yvalue
- point2: xvalue yvalue
- ...

###region-definitions.txt
- The format is:
- neighborhood name 1:
- xvalue yvalue
- xvalue yvalue
- ...
- neighborhood name 2:
- ...

###solution.txt
The results will be placed in the file specified by solution-path-name.

###Algorithm - Inclusion of point in polygon
- Winding algorithm
- "But the bottom line is that for both geometric correctness and efficiency reasons, the wn algorithm should always be preferred for determining the inclusion of a point in a polygon."
- http://geomalgorithms.com/a03-_inclusion.html
