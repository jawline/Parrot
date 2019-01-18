module Paths where

articlesDirectory = "articles/"
listsDirectory = "list/"

inputArticles inputDirectory = inputDirectory ++ articlesDirectory
inputTemplates inputDirectory = inputDirectory ++ "/templates/"
inputTemplateArticle inputDirectory = (inputTemplates inputDirectory) ++ "article.html"
inputTemplateList inputDirectory = (inputTemplates inputDirectory) ++ "list.html"
inputTemplateNav inputDirectory = (inputTemplates inputDirectory) ++ "nav.html"
inputTemplateListItem inputDirectory = (inputTemplates inputDirectory) ++ "list_item.html"
inputTemplateIndex inputDirectory = (inputTemplates inputDirectory) ++ "index.html"
inputStatic inputDirectory = inputDirectory ++ "static/"

outputArticles outputDirectory = outputDirectory ++ articlesDirectory
outputLists outputDirectory = outputDirectory ++ listsDirectory
outputIndex outputDirectory = outputDirectory ++ "index.html"


