# Yum 😋

A [Discord](https://discord.com/) music player bot.

## Build (Ubuntu 24.04+)
1. `sudo apt install opam`
2. `opam update --all`
3. `make deps-linux`
4. `make build`

## Install (Ubuntu 24.04+)
1. Install [ffmpeg](https://ffmpeg.org/)
```sh
sudo apt install -y ffmpeg
```
2. Download [youtube-dl (ytdl-nightly)](https://github.com/ytdl-org/ytdl-nightly). e.g.
```sh
wget https://github.com/ytdl-org/ytdl-nightly/releases/latest/download/youtube-dl
chmod +x youtube-dl
sudo mv youtube-dl /usr/bin # or somewhere else you prefer
```
> Make sure `python` is aliased to `python3`. If not, run `sudo apt install -y python-is-python3`
3. Download [media-get]([https://github.com/ytdl-org/ytdl-nightly](https://github.com/foamzou/media-get)). e.g.
```sh
wget -O media-get https://github.com/foamzou/media-get/releases/download/v0.2.13/media-get-0.2.13-linux
chmod +x media-get
sudo mv media-get /usr/bin # or somewhere else you prefer
```
4. Download [yum](https://github.com/HITGIF/yum)
```sh
wget https://github.com/HITGIF/yum/releases/latest/download/yum
chmod +x yum
```
5. Make a playlist file containing a list of YouTube video IDs, e.g. [mine](https://gist.github.com/HITGIF/bf3ee113f9d86afe717d7fc6a6731b8c)
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
  [-media-get-path PATH]     . Path to the media-get binary (default:
                               /usr/bin/media-get)
  [-youtube-dl-path PATH]    . Path to the youtube-dl binary (default:
                               /usr/bin/youtube-dl)
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
See `yum help` for available commands.