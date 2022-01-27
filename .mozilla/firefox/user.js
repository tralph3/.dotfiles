// These settings will get applied on Firefox startup, overriding
// settings configured with about:config

// Load userChrome.css and userContent.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Disable about:config warning
user_pref("browser.aboutConfig.showWarning", false);

// Use bookmarks toolbar as default location
user_pref("browser.bookmarks.defaultLocation", "toolbar_____");

// Enable compact mode
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);

// Custom content blocking
user_pref("browser.contentblocking.category", "custom");

// Always ask where to save downloads
user_pref("browser.download.useDownloadDir", false);

// Let Tabliss override the new tab window
user_pref("browser.newtab.extensionControlled", true);

// Show tab title in titlebar
user_pref("browser.tabs.inTitlebar", 1);

// Always show bookmarks toolbar
user_pref("browser.toolbars.bookmarks.visibility", "always");

// Don't teach me how to use PIP
user_pref("media.videocontrols.picture-in-picture.video-toggle.has-used", true);

// Use system's file browser
user_pref("widget.use-xdg-desktop-portal", true);
