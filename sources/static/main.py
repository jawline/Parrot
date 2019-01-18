import os
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp import template
import logging

class MainHandler(webapp.RequestHandler):
  def get (self, q):

    logging.debug('Bah')

    if q is None or q == "":
      q = 'index.html'
    
    if q.endswith('.html') == False:
      q += '.html'

    path = os.path.join (os.path.join(os.path.dirname (__file__), 'main/'), q)

    self.response.headers ['Content-Type'] = 'text/html'
    self.response.out.write (template.render (path, {}))

def main ():
  application = webapp.WSGIApplication ([('/(.*?)', MainHandler)], debug=True)
  util.run_wsgi_app (application)

if __name__ == '__main__':
  main ()
