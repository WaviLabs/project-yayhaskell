// Change the theme on a button click
function toggleViewMode() {
    var bodyClasses     = document.body.classList
    var currentViewMode = bodyClasses[0];
    if (currentViewMode === 'light') {
        bodyClasses.replace('light', 'dark');
    } else {
        bodyClasses.replace('dark', 'light');
    }
}

toggleViewMode();
