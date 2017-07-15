"use strict";

window.d3 = require('d3');

var functionPlot = require('function-plot');

//window.functionPlot = functionPlot;

exports.functionPlotImpl = function(options) {
  return function() {
    return window.graph = functionPlot(options);
  };
};

exports.setOptionsImpl = function(graph) {
  return function(options) {
    return function() {
      Object.assign(graph.options, options);
      return {};
    };
  };
};

exports.setDataImpl = function(graph) {
  return function(data) {
    return function() {
      graph.options.data = data;
      return {};
    };
  };
};

exports.draw = function(graph) {
  return function() {
    graph.draw();
    return {};
  };
};
