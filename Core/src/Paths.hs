module Paths where
import System.FilePath

templateArticlesPath = "/articles/"

data InputTemplates = InputTemplates {
  templateIndex :: String,
  templateArticle :: String,
  templateList :: String,
  templateListItem :: String,
  templateNav :: String
}

createTemplates templatesRoot = InputTemplates {
  templateIndex = templatesRoot </> "index.html",
  templateArticle = templatesRoot </> "article.html",
  templateList = templatesRoot </> "list.html",
  templateListItem = templatesRoot </> "list_item.html",
  templateNav = templatesRoot </> "nav.html"
}

data InputDirectories = InputDirectories {
  inputRoot :: String,
  inputImages :: String,
  inputStatic :: String,
  inputArticles :: String,
  inputTemplates :: InputTemplates
}

inputDirectories root = InputDirectories {
  inputRoot = root,
  inputImages = root </> "images",
  inputStatic = root </> "static",
  inputArticles = root </> "articles",
  inputTemplates = createTemplates (root </> "templates")
}

data OutputDirectories = OutputDirectories {
  root :: String,
  articles :: String,
  relativeArticles :: String,
  images :: String,
  relativeImages :: String,
  lists :: String,
  relativeLists :: String,
  index :: String
}

outputDirectories root = OutputDirectories {
  root=root,
  articles=root </> "articles",
  relativeArticles="/articles/",
  images=root </> "images",
  relativeImages = "/images",
  lists=root </>  "list",
  relativeLists="/list/",
  index=root </> "index" <.> "html"
}
