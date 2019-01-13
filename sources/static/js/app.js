'use strict';

/* App Module */

var imcluelessSite = angular.module('imcluelessSite', [ 'ngAnimate', 'ngRoute' ]);

imcluelessSite.config([ '$routeProvider', function($routeProvider) {
	$routeProvider.when('/', {
		redirectTo : '/projects'
	}).when('/error404', {
		templateUrl : 'partials/error404.html'
	}).when('/projects', {
		templateUrl : 'partials/projects.html'
	}).when('/work', {
		templateUrl : 'partials/work.html'
	}).when('/about', {
		templateUrl : 'partials/education.html'
	}).when('/contact', {
		templateUrl : 'partials/contact.html'
	}).when('/research', {
		templateUrl : 'partials/research.html'
	}).otherwise({
		redirectTo : '/error404'
	});
}]).animation('.parsed-body', function() {
  return {
    enter: function(element, done) {
      element.css('display', 'none');
      element.fadeIn(200, done);
      return function() {
        element.stop();
      }
    },
    leave: function(element, done) {
      element.fadeOut(200, done)
      return function() {
        element.stop();
      }
    }
  }
}).run(function ($rootScope) {});
