#!/bin/bash

# Configuration
APP_PATH="pixel-diff.app"
DMG_NAME="pixel-diff-installer.dmg"

# Signing configuration
APPLE_ID="arnstein87@gmail.com"
DEVELOPER_ID="Developer ID Application: Modern Interpreters Inc (HQ25CUJ52L)"
TEAM_ID="HQ25CUJ52L"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${YELLOW}Creating signed and notarized DMG installer...${NC}"

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

# Remove any existing DMG
if [ -f "$DMG_NAME" ]; then
    echo -e "${YELLOW}Removing existing DMG...${NC}"
    rm "$DMG_NAME"
fi

# Create a clean temporary directory
TEMP_DIR=$(mktemp -d)
echo -e "${YELLOW}Step 1: Preparing DMG contents in $TEMP_DIR${NC}"

# Copy app to temp directory
cp -R "$APP_PATH" "$TEMP_DIR/"

# Create Applications symlink
ln -s /Applications "$TEMP_DIR/Applications"

# Create DMG directly from temp directory
echo -e "${YELLOW}Step 2: Creating DMG...${NC}"
hdiutil create -volname "Pixel Diff Installer" \
    -srcfolder "$TEMP_DIR" \
    -ov \
    -format UDZO \
    -imagekey zlib-level=9 \
    "$DMG_NAME"

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: DMG creation failed${NC}"
    rm -rf "$TEMP_DIR"
    exit 1
fi

# Clean up temp directory
rm -rf "$TEMP_DIR"

# Step 3: Sign the DMG
echo -e "${YELLOW}Step 3: Code signing the DMG...${NC}"
codesign --force --sign "$DEVELOPER_ID" "$DMG_NAME"

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: DMG code signing failed${NC}"
    exit 1
fi

# Step 4: Notarize the DMG
echo -e "${YELLOW}Step 4: Submitting DMG for notarization (this may take a few minutes)...${NC}"
xcrun notarytool submit "$DMG_NAME" \
  --apple-id "$APPLE_ID" \
  --password "$NOTARIZATION_PASSWORD" \
  --team-id "$TEAM_ID" \
  --wait

if [ $? -ne 0 ]; then
    echo -e "${RED}Error: DMG notarization failed${NC}"
    exit 1
fi

# Step 5: Staple the notarization ticket to DMG
echo -e "${YELLOW}Step 5: Stapling notarization ticket to DMG...${NC}"
xcrun stapler staple "$DMG_NAME"

if [ $? -ne 0 ]; then
    echo -e "${RED}Warning: DMG stapling failed${NC}"
fi

# Verify the DMG
echo -e "${YELLOW}Step 6: Verifying signed DMG...${NC}"
codesign -dv --verbose=4 "$DMG_NAME"
hdiutil verify "$DMG_NAME"

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✅ Signed and notarized DMG created successfully!${NC}"
    echo -e "${GREEN}File: $DMG_NAME${NC}"
    echo -e "${GREEN}Size: $(du -h "$DMG_NAME" | cut -f1)${NC}"
    echo -e "${GREEN}🎉 Ready for public distribution!${NC}"
else
    echo -e "${RED}❌ DMG verification failed${NC}"
    exit 1
fi
