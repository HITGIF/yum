# Yum 😋

A [Discord](https://discord.com/) music player bot.

## Build (Ubuntu 24.04+)
1. `sudo apt install opam`
2. `opam update --all`
3. `make deps-linux`
4. `make install`
5. `make build`

## Install (Ubuntu 24.04+)
1. Install libsodium, libopus
```sh
sudo apt install -y libsodium23 libopus0
```
2. Install [libdave](https://github.com/discord/libdave)
```sh
mkdir tmp
cd tmp
wget -O libdave.zip https://github.com/discord/libdave/releases/download/v1.1.0%2Fcpp/libdave-Linux-X64-boringssl.zip
unzip libdave.zip
sudo cp lib/libdave.so /usr/local/lib
sudo chmod 755 /usr/local/lib/libdave.so
sudo ldconfig
cd ..
rm -rf tmp
```
3. Install [ffmpeg](https://ffmpeg.org/)
```sh
sudo apt install -y ffmpeg
```
4. Download [yt-dlp](https://github.com/yt-dlp/yt-dlp). e.g.
```sh
wget -O yt-dlp https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp_linux
chmod +x yt-dlp
sudo mv yt-dlp /usr/bin # or somewhere else you prefer
```
> Make sure `python` is aliased to `python3`. If not, run `sudo apt install -y python-is-python3`
5. Download [yum](https://github.com/HITGIF/yum)
```sh
wget https://github.com/HITGIF/yum/releases/latest/download/yum
chmod +x yum
```
6. Make a playlist file containing a list of YouTube video IDs, e.g. [mine](https://gist.github.com/HITGIF/bf3ee113f9d86afe717d7fc6a6731b8c)
```sh
wget https://gist.githubusercontent.com/HITGIF/bf3ee113f9d86afe717d7fc6a6731b8c/raw/57df953ab9ff70709825031559b1d5f1562f3ed1/videos.txt
```

## Run
### CLI
```sh
./yum -discord-bot-token YOUR_DISCORD_BOT_TOKEN -youtube-songs YOUR_SONGS_FILE
```
#### Options
```
😋 A Discord music player bot.

  yum

=== flags ===

  -discord-bot-token STRING  . Discord bot auth token
  -youtube-songs FILE        . Youtube songs file
  [-ffmpeg-path PATH]        . Path to the ffmpeg binary (default:
                               /usr/bin/ffmpeg)
  [-log-level LEVEL]         . The log level (can be: Debug, Error, Info)
  [-yt-dlp-path PATH]        . Path to the yt-dlp binary (default:
                               /usr/bin/yt-dlp)
  [-build-info]              . print info about this build and exit
  [-version]                 . print the version of this build and exit
  [-help], -?                . print this help text and exit
```

#### Bot
```
yum start
yum play https://www.youtube.com/watch?v=PLG2Uexyi9s
yum skip
...
```
See `yum help` for available commands