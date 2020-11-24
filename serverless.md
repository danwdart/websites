# Notes

Yes, the docker image does need to be based on lts-13.30 rather than the actual snapshot in use, as that's the newest version still compatible with AWS's libc. See: https://github.com/seek-oss/serverless-haskell

lts-13.30 was based on 2019-07-29 so it's more than a year out of date.

Guessing lts-14.x won't work, but this was based on the latest ghc-8.6.

I guess it's pointless specifying the small image because the standard image is hardcoded in the library.

TODO: see if this breaks 8.10.x

# Offline

Needs to run as system (rootful) docker as the provided image runs as user with no access to /var/task.

To use offline, you should run `docker pull lambci/lambda:build-provided` if you don't want your first request to hang for a good while. It does this in the background, however it's just a silent nuisance.

Error generated using 8.10:

```
$ stack build --docker --docker-image fpco/stack-build:lts-13.30
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Already downloaded.                 
/home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/configure: line 5952:   454 Aborted                 (core dumped) ./conftest
/home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/configure: line 6140:   471 Aborted                 (core dumped) ./conftest
configure: error: Linker is affected by binutils 22266 but couldn't find another unaffected linker. Please set the SettingsMergeObjectsCommand variable to a functional linker.
Received ExitFailure 1 when running
Raw command: /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/configure --prefix=/home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2/
Run from: /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/

                   
Error: Error encountered while configuring GHC with
         /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/configure --prefix=/home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2/
         run in /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/ghc-8.10.2/
       
       The following directories may now contain files, but won't be used by stack:
         - /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2.temp/
         - /home/dwd/.stack/programs/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/ghc-8.10.2/
       
       For more information consider rerunning with --verbose flag
       
Configuring GHC ...
```


## Running as root

It's also necessary recently to have some of the repo owned by root if running docker as root (TODO find out which).

Probably easiest to temporarily run as user in docker group for now (but not for too long of course!)

Best idea is to sync ~/.stack, ~/.aws and ~/.serverless with /root/

# Usage

Start offline: `sudo sls offline start`

# TODO

Investigate a way not to use this library and use our own glibc's binaries or even an AppImage.