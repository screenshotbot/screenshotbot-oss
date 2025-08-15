#!/bin/bash

# Configuration - UPDATE THESE VALUES
APP_PATH="pixel-diff.app"
APPLE_ID="arnstein87@gmail.com"
DEVELOPER_ID="Developer ID Application: Modern Interpreters Inc (HQ25CUJ52L)"
TEAM_ID="HQ25CUJ52L"
APP_PASSWORD="${NOTARIZATION_PASSWORD}"  # Set via: export NOTARIZATION_PASSWORD="your-password"
BUNDLE_ID="io.screenshotbot.pixeldiff"  # Matches Info.plist
ENTITLEMENTS_FILE="src/pixel-diff/lispworks.entitlements"



# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting Mac app signing and notarization process...${NC}"

# Check if password is set
if [ -z "$NOTARIZATION_PASSWORD" ]; then
    echo -e "${RED}Error: NOTARIZATION_PASSWORD environment variable not set${NC}"
    echo -e "${YELLOW}Run: export NOTARIZATION_PASSWORD=\"your-app-specific-password\"${NC}"
    exit 1
fi

# Check if app exists
if [ ! -d "$APP_PATH" ]; then
    echo -e "${RED}Error: $APP_PATH not found${NC}"
    exit 1
fi

# Step 1: Remove quarantine attributes
echo -e "${YELLOW}Step 1: Removing quarantine attributes...${NC}"
xattr -dr com.apple.quarantine "$APP_PATH" 2>/dev/null || true
xattr -c "$APP_PATH" 2>/dev/null || true

# Step 2: Make executable if needed
echo -e "${YELLOW}Step 2: Setting executable permissions...${NC}"
chmod +x "$APP_PATH/Contents/MacOS"/*

# Step 3: Code signing
echo -e "${YELLOW}Step 3: Code signing the app...${NC}"
codesign --force --deep --entitlements $ENTITLEMENTS_FILE --options runtime --sign "$DEVELOPER_ID" "$APP_PATH"

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: Code signing failed${NC}"
    exit 1
fi

# Step 4: Verify code signing
echo -e "${YELLOW}Step 4: Verifying code signature...${NC}"
codesign -dv --verbose=4 "$APP_PATH"
spctl -a -t exec -vv "$APP_PATH"

if [ $? -ne 0 ]; then
    echo -e "${RED}Warning: Code signature verification failed${NC}"
fi

# Step 5: Create zip for notarization
echo -e "${YELLOW}Step 5: Creating zip file for notarization...${NC}"
ZIP_NAME="pixel-diff-notarization.zip"
ditto -c -k --keepParent "$APP_PATH" "$ZIP_NAME"

# Step 6: Submit for notarization
echo -e "${YELLOW}Step 6: Submitting for notarization (this may take several minutes)...${NC}"
xcrun notarytool submit "$ZIP_NAME" \
    --apple-id "$APPLE_ID" \
    --password "$APP_PASSWORD" \
    --team-id "$TEAM_ID" \
    --wait

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: Notarization failed${NC}"
    exit 1
fi

# Step 7: Staple the notarization ticket
echo -e "${YELLOW}Step 7: Stapling notarization ticket...${NC}"
xcrun stapler staple "$APP_PATH"

if [ $? -ne 0 ]; then
    echo -e "${RED}Warning: Stapling failed${NC}"
fi

# Step 8: Final verification
echo -e "${YELLOW}Step 8: Final verification...${NC}"
spctl -a -t exec -vv "$APP_PATH"

# Cleanup
echo -e "${YELLOW}Cleaning up...${NC}"
rm "$ZIP_NAME"

echo -e "${GREEN}✅ App signing and notarization complete!${NC}"
echo -e "${GREEN}Your app is now ready for distribution.${NC}"

# Final check
echo -e "${YELLOW}Final check - trying to run the app...${NC}"
open "$APP_PATH"
