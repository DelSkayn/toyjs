

// For CommonJS and CommonJS-like environments where a proper `window`
// is present, execute the factory and get jQuery.
  // For environments that do not have a `window` with a `document`
// (such as Node.js), expose a factory as module.exports.
  // This accentuates the need for the creation of a real `window`.
  // e.g. var jQuery = require("jquery")(window);
// See ticket trac-14549 for more info.
  factory( global, true );
  (function( w ) {
  });
