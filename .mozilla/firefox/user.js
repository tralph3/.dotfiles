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

// Just stop (no autplay videos)
user_pref("media.autoplay.default", 5);
user_pref("media.autoplay.blocking_policy", 2);
user_pref("media.autoplay.allow-extension-background-pages", false);
user_pref("media.autoplay.block-event.enabled", true);

// Don't allow sites to mess with ma' context menu
user_pref("dom.event.contextmenu.enabled", false);

// Tracking protection
user_pref("privacy.trackingprotection.enabled", true);

// No telemetry
user_pref("toolkit.telemetry.cachedClientID", "blank");

// Don't send pings to other sites
user_pref("browser.send_pings.require_same_host", true);

// Why do you need to know my battery?
user_pref("dom.battery.enabled", false);

// Block third party cookies
user_pref("network.cookie.cookieBehavior", 1);

// No link prefetching
user_pref("network.predictor.enabled", false);
user_pref("network.dns.disablePrefetch", true);
user_pref("network.prefetch-next", false);
    // Also on hover
user_pref("network.http.speculative-parallel-limit", 0);

// Disable pocket
user_pref("extensions.pocket.enabled", false);
user_pref("extensions.pocket.enabled", "blank");
user_pref("extensions.pocket.oAuthConsumerKey", "blank");
user_pref("extensions.pocket.api", "blank");

// Faster animations
user_pref("layout.frame_rate.precise", true);

// Hardware acceleration
user_pref("webgl.force-enabled", true);
user_pref("layers.acceleration.force-enabled", true);
user_pref("layers.offmainthreadcomposition.enabled", true);
user_pref("layers.offmainthreadcomposition.async-animations", true);
user_pref("layers.async-video.enabled", true);
user_pref("html5.offmainthread", true);

// Send "do not track" signal, as if anyone respected it
user_pref("privacy.donottrackheader.enabled", true);

// Enable the titlebar... which will then get removed by Qtile
user_pref("browser.tabs.inTitlebar", 0);
