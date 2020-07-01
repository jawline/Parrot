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

## Definitions

[TODO]

## Creating New Articles

[TODO]

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

### Template Strings

Content is substituted into templates and articles through template strings
in the form `${{{string}}}`. Template strings can be constants, such as
`${{{ARTICLE_CONTENT}}}` in the article template, or dynamic, such as
`${{{img:expose.png}}}` which instructs Parrot to generate a web-optimized
version of expose.png.

#### Index Template

The index template is used to construct the website index (or landing)
page, the first page users will see when visiting a site. The index template
is largely left to the developer, but supports image template strings and
`${{{NAV_BAR_CONTENT}}}`.

#### Article Template

The article template is used as the skeleton for each article, and template strings are used to include the content as each article is generated. The supported template strings are:
- `NAV_BAR_CONTENT`: For the navigation bar.
- `ARTICLE_TITLE`: The title of the article being rendered.
- `ARTICLE_CONTENT`: The converted content of the article markdown file.
- `ARTICLE_TAGS`: The lists to which this article belongs, extracted from the markdown metadata.
- `ARTICLE_TIME`: The time when this article was created, extracted from the markdown metadata.

#### List Template

List templates form the skeleton of the article lists, which allow viewers to browse through lists of similar content and can be linked to. List templates are split into two components, the overall list template and the list item template.

The overall list template is responsible for the list page theme and the placement of list items within it. It supports the following template strings:
- `NAV_BAR_CONTENT`: The content of the navigation bar.
- `LIST_TITLE`: The title of the list being viewed.
- `LIST_CONTENT`: Each item included in this list will be rendered into a list_item template and then included in the `LIST_CONTENT`.

The list item is the skeleton HTML for a single entry into the list. It includes the following template strings:
- `LI_NAME`: The name of the article being referenced.
- `LI_DESCRIPTION`: The short description of the article extracted from the article metadata.
- `LI_TAGS`: The list of tags (lists) that this article belongs to, extracted from the article metadata.
- `LI_DATE`: The time that this article was created, which is extracted from the article metadata.

#### Linking to lists

[TODO]

#### Linking to articles

[TODO]

#### Images, Image Templating, and Resizing

Parrot can automatically images requested through template strings to desired sizes while building a website. Image template strings come in the form `${{{img:image_name.filetype}}}` and the image is expected to be in a subdirectory of the `images/` path of the source site.

By default, images will be resized to the high resolution setting of Parrot, but this can be overwritten in the image template string. These are the possible overrides:
- `${{{img:filename.filetype:original}}}`: keep the current image settings.
- `${{{img:filename.filetype:high}}}`: Use the high quality Parrot setting.
- `${{{img:filename.filetype:thumb}}}`: Use the thumbnail parrot setting.
