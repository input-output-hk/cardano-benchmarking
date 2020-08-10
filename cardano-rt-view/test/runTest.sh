#!/bin/sh

set -e

readonly JSON_WITH_LOG_OBJECTS=logObjects.json
readonly UNIX_SOCKET=/tmp/rt-view-pipe-0
readonly SELENIUM_SERVER_JAR=$1

readonly RT_VIEW_EXE=cardano-rt-view-service
readonly RT_VIEW_CONFIG=rt-view-config.yaml
readonly RT_VIEW_STATIC_DIR=../static
readonly RT_VIEW_WEB_PORT=8024

# readonly EXEC="stack exec"

echo "Remove old UNIX-socket..."
rm -f "${UNIX_SOCKET}"

echo "Launch cardano-rt-view-service..."
nohup stack exec -- "${RT_VIEW_EXE}" --config "${RT_VIEW_CONFIG}" \
                                     --static "${RT_VIEW_STATIC_DIR}" \
                                     --port "${RT_VIEW_WEB_PORT}" &
sleep 2

echo "Launch sender.sh script..."
./sender.sh "${JSON_WITH_LOG_OBJECTS}" "${UNIX_SOCKET}"

## Since analyzer is using webdriver, we must run selenium-standalone-server first.
## By default it will listen 127.0.0.1:4444.
nohup java -jar "${SELENIUM_SERVER_JAR}" &
sleep 2

## Launch analyzer
stack exec -- analyzer "${RT_VIEW_CONFIG}" "${JSON_WITH_LOG_OBJECTS}" "${RT_VIEW_WEB_PORT}"
ANALYZER_STATUS=$?
[ $ANALYZER_STATUS -eq 0 ] && echo "Test passed." || echo "Test failed: analyzer returned an error."
sleep 1

## Stop selenium-standalone-server.
kill "$(pgrep -f selenium-server-standalone)"
sleep 1

## Stop cardano-rt-view-service.
kill "$(pgrep -f ${RT_VIEW_EXE})"
sleep 1

# Remove cardano-rt-view-service' logs.
rm -rf logs

# Remove nohup artifacts
rm -f nohup.out

echo "Done."
