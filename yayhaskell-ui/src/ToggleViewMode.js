"use strict";

// Change the theme on a button click
exports.toggleViewMode = function () {
    var bodyClasses     = document.body.classList
    console.log(bodyClasses);
    var currentViewMode = bodyClasses[0];
    console.log(currentViewMode);
    if (currentViewMode === 'light') {
        bodyClasses.replace('light', 'dark');
    } else {
        bodyClasses.replace('dark', 'light');
    }
    console.log("View mode toggled.");
}
