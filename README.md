# Parrot

Markdown to HTML templating engine written entirely in Haskell. This tool
will take a template website and a series of markdown articles and produce
a static artefact which can be deployed on a web server. [For an example, my website was created using this tool](https://parsed.dev).

## Dependencies

We use HIP for image processing, which requires zlib and zlib.h to be installed. `apt install zlib1g-dev` on Ubuntu.

## Getting Started

To build the included sample website execute: `cabal run Parrot -- ./examples/sample_site/ ./out/` where ./out/ is the directory that should include the built website.

To automatically rebuild the website whenever a source file or Parrot's source code changes use: `cabal run ParrotWatch -- ./examples/sample_site ./out/'

NOTE: If the output directory does not exist Parrot will create it, but it will not create the directory recursively. For example, given an empty directory `/var/`, `cabal run Parrot -- source/ /var/html` will work correctly but `cabal run Parrot -- source/ /var/html/www/` will not.

NOTE: Parrot will re-use existing directories, which can cause file litter. It is best to create a fresh directory for builds.

## Design

### Templating

### Images, Image Templating, and Resizing

### Photo Gallery

[TODO: Fill this in once the photo gallery extension is finished]
