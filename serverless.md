# Serverless bits

Notes:
    `docker pull lambci/lambda:provided` yourself, it's invisible in Offline.

Usage:
    Restart daemon, see: https://github.com/moby/moby/issues/26799
    `npx sls invoke local -f submit-comment`
    Start offline: `npx sls offline start`
    
TODO:
    Find a way not to rebuild stack programs in deploy so we can deploy as user.
    Investigate a way not to use this library and use our own glibc's binaries or even an AppImage.