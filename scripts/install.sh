#!/usr/bin/env bash
VERSION="$1"

# Create a temporary directory
TEMP_DIR=$(mktemp -d)

# Make sure the temp directory will be removed on exit.
function finish {
    rm -rf ${TEMP_DIR}
}
trap finish EXIT

# Extract the code
git clone git@github.com:mathmoi/oxide9.git ${TEMP_DIR}
if ! git -C ${TEMP_DIR} checkout ${VERSION}; then
    exit 1    
fi

# Build oxide9
(
    cd ${TEMP_DIR}
    cargo build --release
)

# Install the engine
BIN_DIR="${HOME}/bin"
INSTALL_DIR="${BIN_DIR}/o9-${VERSION}"
if ! mkdir ${INSTALL_DIR}; then
    exit 2
fi
cp ${TEMP_DIR}/target/release/oxide9      ${INSTALL_DIR}
cp ${TEMP_DIR}/assets/config/oxide9.toml  ${INSTALL_DIR}

# Create a launcher script
LAUNCHER="${BIN_DIR}/o9-${VERSION:1}"
echo "#!/usr/bin/env bash
(cd ${INSTALL_DIR} && ./oxide9 \"\$@\")" >> $LAUNCHER
chmod +x $LAUNCHER
