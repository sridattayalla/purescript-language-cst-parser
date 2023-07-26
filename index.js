(() => {
  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Main/index.js
  var main = /* @__PURE__ */ log("");

  // <stdin>
  main();
})();
