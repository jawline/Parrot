# Parrot

Markdown to HTML templating engine written entirely in Haskell. This tool
will take a template website and a series of markdown articles and produce
a static artifact which can be deployed on a web server. [For an example, my website was created using this tool](https://parsed.dev).

## Dependencies

We use HIP for image processing, which requires zlib and zlib.h to be installed. `apt install zlib1g-dev` on Ubuntu.

## Getting Started

To build the included sample website execute: `cabal run Parrot -- ./examples/sample_site/ ./out/` where ./out/ is the directory that should include the built website.

To automatically rebuild the website whenever a source file or Parrot's source code changes use: `cabal run ParrotWatch -- ./examples/sample_site ./out/'

__NOTE:__ If the output directory does not exist Parrot will create it, but it will not create the directory recursively. For example, given an empty directory `/var/`, `cabal run Parrot -- source/ /var/html` will work correctly but `cabal run Parrot -- source/ /var/html/www/` will not.

__NOTE:__ Parrot will re-use existing directories, which can cause file litter. It is best to create a fresh directory for builds.

## Design

Parrot is an opinionated static website generation tool that builds a website
from articles (pages of content), and lists (collections of articles). Articles
are expressed in markdown and include metadata in-line using a special
syntax. This meta-data includes the article title, date, tags (the lists to
which the article belongs), and an introduction (a short text at the beginning
of the article that will be used when referrencing the article). Articles
and lists are converted into webpages through substitution into templates,
HTML skeletons for each component, which also give the website its visual feel.

Parrot does not take responsibility for the design of the index page, since for
many websites this is a bespoke piece of content with lots of specific styling,
but it does apply it's template string replacement engine to the index page
which allows for linking to articles and automatic web-optimization of images.

Likewise, the navigation bar is not automatically generated, since developers
often want precise control of the entries, but is split into its own HTML
template which is loaded into every page that requres it. Again template
strings can reference articles, simplifying linking and changing paths.

A parrot website is split up into four component folders:
- `templates/`: the HTML templates which will be used to construct the index page, articles, lists, and the navigation bar.
- `static/`: the static resources which will be copied as-is into the root of every website to allow for things like JavaScript and CSS dependencies.
- `articles/`: The markdown articles for the website.
- `images/`: The images which can be referred to by any template or article using image template strings.

__NOTE:__ The list of lists is automatically generated from the articles, and
is not manually curated.

### Templating

### Images, Image Templating, and Resizing

