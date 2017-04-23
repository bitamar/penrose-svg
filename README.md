Elm SVG Penrose tiler

![](https://bitamar.github.io/penrose-svg/penrose.svg)

Based on Jeff Preshing's python implementation:
http://preshing.com/20110831/penrose-tiling-explained/

The only significant difference is the coloring strategy; Here when a triangle
is divided, the original triangle is kept, and so another pattern is emerged
by the triangles transparency.



Deploy: `elm make Penrose.elm --output docs/index.html`
