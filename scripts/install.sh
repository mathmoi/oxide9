#!/usr/bin/env bash
NICKNAME="$1"

VERSION=$(grep '^version =' Cargo.toml | cut -d '"' -f2)

# Install the engine
BIN_DIR="${HOME}/bin"
INSTALL_DIR="${BIN_DIR}/oxide9-${VERSION}-${NICKNAME}"
if ! mkdir ${INSTALL_DIR}; then
    exit 2
fi

cargo build --release
BUILD_DIR="/home/mathmoi/Source/m8/build"
cp ./target/release/oxide9     ${INSTALL_DIR}
cp ./assets/config/oxide9.toml ${INSTALL_DIR}
sed -i "s/name\s*=\s*\".*\"/name = \"Oxide9-${NICKNAME}\"/" ${INSTALL_DIR}/oxide9.toml

# Create a launcher script
LAUNCHER="${BIN_DIR}/oxide9-${NICKNAME}"
echo "#!/usr/bin/env bash
(cd ${INSTALL_DIR} && ./oxide9 \$@)" >> $LAUNCHER
chmod +x $LAUNCHER
