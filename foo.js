'use strict';

const _ = require('underscore');

let myDo = [
  () => ({out: console.log("hi"), binds: {}}),
  () => ({out: undefined, binds: {foo: "this is foo"}}),
  (binds) => ({out: console.log(binds.foo), binds: {}}),
]

function doDo(theDo, env) {
  if (!theDo.length) return;
  
  let doResult = theDo.shift()(env);
  _.default(doResult.binds, env);
  return doDo(theDo, doResult.binds);
}

doDo(myDo, {});
