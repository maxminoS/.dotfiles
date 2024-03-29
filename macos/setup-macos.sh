#!/usr/bin/sh

# Ask Password in Advance
sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Machine Name
# ------------
echo "Set machine name:"
read MACHINE_NAME
sudo scutil --set ComputerName $MACHINE_NAME
sudo scutil --set HostName $MACHINE_NAME
sudo scutil --set LocalHostName $MACHINE_NAME
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string $MACHINE_NAME

# Boot
# ----
# Disable autoboot and boot chime
sudo nvram AutoBoot=%00
sudo nvram SystemAudioVolume=" "
# Lid Wake
sudo pmset -a lidwake 1
# Sleep Display After 15 Minutes
sudo pmset -a displaysleep 15
# Require Password Immediately After Sleep
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0
# Disable Resume (does not open previously open applications on startup)
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

# Globals
# -------
# Show Scrollbars When Scrolling
defaults write NSGlobalDomain AppleShowScrollBars -string "WhenScrolling"
# Disable Focus Ring
defaults write NSGlobalDomain NSUseAnimatedFocusRing -bool false

# Finder
# ------
# Allow Finder to Quit
defaults write com.apple.finder QuitMenuItem -bool true
# Desktop as Default Finder Path
defaults write com.apple.finder NewWindowTarget -string "PfDe"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/Desktop/"
# Show status and path bar
defaults write com.apple.finder ShowStatusBar -bool true
defaults write com.apple.finder ShowPathbar -bool true
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
# Folders First When Sorting by Name
defaults write com.apple.finder _FXSortFoldersFirst -bool true
# Search Scope Within Folder
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
# Instant Spring Loading for Directories (hover on top to open)
defaults write NSGlobalDomain com.apple.springing.enabled -bool true
defaults write NSGlobalDomain com.apple.springing.delay -float 0
# Medium Sidebar Icon Size
defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2
# Instant Toolbar Rollover
defaults write NSGlobalDomain NSToolbarTitleViewRolloverDelay -float 0
# List View by Default
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
# Snap-to-Grid Icon Views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
# Size of Icons: 80
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:iconSize 80" ~/Library/Preferences/com.apple.finder.plist
# Show ~/Library folder
chflags nohidden ~/Library && xattr -d com.apple.FinderInfo ~/Library
# Show /Volumes folder
sudo chflags nohidden /Volumes
# Save to disk (not iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
# Avoid .DS_Store Files on Volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Dialogs
# -------
# Expand Save Panel by Default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
# Expand Print Panel by Default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true
# Remove Duplicates in “Open With”
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user
# Disable “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Menu Bar
# --------
# Menu Bar Clock Format
defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm:ss"
# Set Help Window to Not Always On Top
defaults write com.apple.helpviewer DevMode -bool true
# Disable Notification Center and Remove from Menu Bar
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null

# Dock
# ----
# Disable Dock Hover Effect
defaults write com.apple.dock mouse-over-hilite-stack -bool false
# Spring Loading for Dock Items
defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true
# Dock Icons Size: 36
defaults write com.apple.dock tilesize -int 36
# Set Minimize Window Effect
defaults write com.apple.dock mineffect -string "scale"
# Minimize Windows Into Icon
defaults write com.apple.dock minimize-to-application -bool true
# Wipe Dock Items
defaults write com.apple.dock persistent-apps -array
# Only Open Applications in Dock
defaults write com.apple.dock static-only -bool true
# No Docker App Launch Animations
defaults write com.apple.dock launchanim -bool false
# Fast Mission Control Animations
defaults write com.apple.dock expose-animation-duration -float 0.1
# Instant Auto-hide Dock
defaults write com.apple.dock autohide-delay -float 0
# Fast Dock Animations
defaults write com.apple.dock autohide-time-modifier -float 0.1
# Automatically Hide and Show Dock
defaults write com.apple.dock autohide -bool true
# Remove Recent Apps in Dock
defaults write com.apple.dock show-recents -bool false

# Mission Conrol
# --------------
# No Grouped Windows in Mission Control
defaults write com.apple.dock expose-group-by-app -bool false

# Dashboard
# ---------
# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true
# Don’t Show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true
# Disable Auto-rearranging Spaces
defaults write com.apple.dock mru-spaces -bool false

# Launchpad
# ---------
# Enable Launchpad Pinch Gesture
defaults write com.apple.dock showLaunchpadGestureEnabled -int 1
# Reset Launchpad, But Keep Desktop Wallpaper
find "${HOME}/Library/Application Support/Dock" -name "*-*.db" -maxdepth 1 -delete

# Spotlight
# ---------
# Hide Spotlight Panel Tray Icon
sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search
# Custom Search Results and Search Order
defaults write com.apple.spotlight orderedItems -array \
  '{"enabled" = 1;"name" = "APPLICATIONS";}' \
  '{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
  '{"enabled" = 1;"name" = "DIRECTORIES";}' \
  '{"enabled" = 1;"name" = "MOVIES";}' \
  '{"enabled" = 1;"name" = "IMAGES";}' \
  '{"enabled" = 1;"name" = "MUSIC";}' \
  '{"enabled" = 1;"name" = "PDF";}' \
  '{"enabled" = 1;"name" = "DOCUMENTS";}' \
  '{"enabled" = 1;"name" = "PRESENTATIONS";}' \
  '{"enabled" = 1;"name" = "SPREADSHEETS";}' \
  '{"enabled" = 1;"name" = "SOURCE";}' \
  '{"enabled" = 0;"name" = "MESSAGES";}' \
  '{"enabled" = 0;"name" = "CONTACT";}' \
  '{"enabled" = 0;"name" = "EVENT_TODO";}' \
  '{"enabled" = 0;"name" = "BOOKMARKS";}' \
  '{"enabled" = 0;"name" = "FONTS";}'
# Load New Settings and Rebuild Index
killall mds > /dev/null 2>&1
sudo mdutil -i on / > /dev/null
sudo mdutil -E / > /dev/null

# Screencapture
# -------------
# Screenshots in ~/Pictures
defaults write com.apple.screencapture location -string ~/Pictures
# Disable Screenshot Thumbnail
defaults write com.apple.screencapture show-thumbnail -bool false
# Screenshots in PNG
defaults write com.apple.screencapture type -string "png"

# TextEdit
# --------
# Start With Plain Text Mode
defaults write com.apple.TextEdit RichText -int 0
# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# Photos
# ------
# Disable Auto-opening Photos
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

# Activity Monitor
# ----------------
# Show Main Window
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true
# Visualize CPU Dock Icon
defaults write com.apple.ActivityMonitor IconType -int 5
# Show All Processes
defaults write com.apple.ActivityMonitor ShowCategory -int 0
# Sort by CPU Usage
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

# App Store
# ---------
# Enable WebKit DevTools in App Store
defaults write com.apple.appstore WebKitDeveloperExtras -bool true
# Enable Debug Menu in App Store
defaults write com.apple.appstore ShowDebugMenu -bool true
# Enable the Automatic Update Check
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
# Check Software Updates Daily
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
# Auto-download Updates
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
# Install System Data Files and Security Updates
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1
# Auto-download Purchased Apps
defaults write com.apple.SoftwareUpdate ConfigDataInstall -int 0
# Enable Auto-update
defaults write com.apple.commerce AutoUpdate -bool true
# Disable Rebooting Machine on macOS Updates
defaults write com.apple.commerce AutoUpdateRestartRequired -bool false

# Safari
# ------
# Privacy
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true
# Tab for Web Accessibility
defaults write com.apple.Safari WebKitTabToLinksPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2TabsToLinks -bool true
# Show Full URL
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true
# Blank Home Page
defaults write com.apple.Safari HomePage -string "about:blank"
# Preevent Auto Opening Downloads
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false
# Show Bookmarks
defaults write com.apple.Safari ShowFavoritesBar -bool true
# Hide Sidebar
defaults write com.apple.Safari ShowSidebarInTopSites -bool false
# Debug Menu
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
# Search Banners "Contains", Not "Starts With"
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false
# Remove Blank Bookmarks Icons
defaults write com.apple.Safari ProxiesInBookmarksBar "()"
# DevTools
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true
# Disable Spellchecking
defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool false
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false
# Disable AutoFill
defaults write com.apple.Safari AutoFillFromAddressBook -bool false
defaults write com.apple.Safari AutoFillPasswords -bool false
defaults write com.apple.Safari AutoFillCreditCardData -bool false
defaults write com.apple.Safari AutoFillMiscellaneousForms -bool false
# Warn Fraudulent Websites
defaults write com.apple.Safari WarnAboutFraudulentWebsites -bool true
# Enable “Do Not Track”
defaults write com.apple.Safari SendDoNotTrackHTTPHeader -bool true
# Auto-update Extensions
defaults write com.apple.Safari InstallExtensionUpdatesAutomatically -bool true

# Chrome
# ------
# Use System's Print Dialog
defaults write com.google.Chrome DisablePrintPreview -bool true
defaults write com.google.Chrome PMPrintingExpandedStateForPrint2 -bool true

# Text Automation
# ---------------
# Disable Auto-capitalization
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
# Disable Smart Dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
# Disable Automatic Period Substitution
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
# Disable Smart Quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
# Disable Auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Trackpad
# --------
# Persistent Tap to Click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
# Bottom Right Corner to Right-Click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true
# Full Keyboard Access
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
# Use Metric System
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true


# Finished
for app in "Activity Monitor" "Address Book" "Calendar" "cfprefsd" "Contacts" "Dock" "Finder" "Google Chrome" "Mail" "Messages"  "Photos" "Safari" "SystemUIServer" "Terminal" "iCal"; do
  killall "${app}" &> /dev/null
done
echo "Setup completed"
