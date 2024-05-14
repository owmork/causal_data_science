window.RevealToggleDarkMode = window.RevealToggleDarkMode || {
    id: "RevealToggleDarkMode",
    init: function(deck) {
        initToggleDarkMode(deck);
    }
};

const initToggleDarkMode = function(Reveal) {

  Reveal.addEventListener( 'ready', function( event ) {
    
    // 1. Create Button
    const button = document.createElement("span");
    button.classList.add("theme-icon")
    button.title = "Toggle Dark/Light Mode";
    const icon_light = document.createElement("i");
    icon_light.ariaHidden = "true";
    icon_light.classList.add("fas", "fa-toggle-off");
    const icon_dark = document.createElement("i");
    icon_dark.ariaHidden = "false";
    icon_dark.classList.add("fas", "fa-toggle-on");
    
    button.appendChild(icon_light);
    button.appendChild(icon_dark);
    
    // 2. Style Button depending on the link-color variable
    // Get R, G, B values from HEX
    const hexToRGB = (hex) => {
      var res = hex.substr(1).split(/(..)/).filter(c=>c).map(c => parseInt(c, 16))
      return `rgb(${res})`;
    }
    // Create SVG
    function SvgContainer(svgfill, svgclass, svgpath) {
      // Container
      var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
      obj = {'xmlns': 'http://www.w3.org/2000/svg', 'width':  16,  'height': 16, 'fill': svgfill, 'class': 'bi bi-' + svgclass,'viewBox': '0 0 16 16'}
      for(prop in obj) {
          svg.setAttribute(prop, obj[prop])  
      }
      // Path
      path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttributeNS(null, "d", svgpath);
      
      svg.appendChild(path);
      return svg;
    }
    // svg paths for the icons
    svg_path_toggle_on  = "M5 3a5 5 0 0 0 0 10h6a5 5 0 0 0 0-10H5zm6 9a4 4 0 1 1 0-8 4 4 0 0 1 0 8z"
    svg_path_toggle_off = "M11 4a4 4 0 0 1 0 8H8a4.992 4.992 0 0 0 2-4 4.992 4.992 0 0 0-2-4h3zm-6 8a4 4 0 1 1 0-8 4 4 0 0 1 0 8zM0 8a5 5 0 0 0 5 5h6a5 5 0 0 0 0-10H5a5 5 0 0 0-5 5z"
  
    // Create stylesheet for the pseudo elements
    var LinkColorRGB = hexToRGB(getComputedStyle(document.documentElement).getPropertyValue('--r-link-color'));
    var svg_toggle_off = new SvgContainer(LinkColorRGB, 'fullscreen',      svg_path_toggle_off);
    var svg_toggle_on  = new SvgContainer(LinkColorRGB, 'fullscreen-exit', svg_path_toggle_on);
    var svg_toggle_off_str = window.btoa((new XMLSerializer()).serializeToString(svg_toggle_off))
    var svg_toggle_on_str  = window.btoa((new XMLSerializer()).serializeToString(svg_toggle_on))
    var styleElem = document.head.appendChild(document.createElement("style"));
    styleElem.innerHTML  = '.reveal .slide-chalkboard-buttons .fa-toggle-off::before   {background-image: url(data:image/svg+xml;base64,' + svg_toggle_off_str   + ');}';
    styleElem.innerHTML += '.reveal .slide-chalkboard-buttons .fa-toggle-on::before {background-image: url(data:image/svg+xml;base64,' + svg_toggle_on_str + ');}';
  
    // 3. Add Stylesheet and Button to DOM
    document.querySelector('.slide-chalkboard-buttons').append(styleElem, button);
  
    // 4. Toggle function
    // If the toggle button is clicked, then the theme should be toggled and the setting should be persisted into the local storage.
    // If the page is loaded, then the setting should be read from the local storage and the theme should be set.
    function isLight() {
      return localStorage.getItem("light-mode");
    }
    function toggleRootClass() {
      document.querySelector(".reveal-viewport").classList.toggle("light");
    }
    function toggleLocalStorageItem() {
      if (isLight()) {
        localStorage.removeItem("light-mode");
      } else {
        localStorage.setItem("light-mode", "set");
      }
    }
  
    if (isLight()) {
      toggleRootClass();
    }
    document.querySelector(".theme-icon").addEventListener("click", () => {
      toggleLocalStorageItem();
      toggleRootClass();
    });
  })
}