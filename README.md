# Yum 😋

A [Discord](https://discord.com/) music player bot, based on [discordml](https://github.com/ushitora-anqou/discordml).

## Install (Debian)
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
./yum -discord-bot-token [YOUR_DISCORD_BOT_TOKEN]
```
#### Options
```
😋 A Discord music player bot, based on discordml.

  yum 

=== flags ===

  [-discord-bot-token STRING]
                             . Discord bot token (env: YUM_DISCORD_BOT_TOKEN)
  [-ffmpeg-path FILE]        . Path to the ffmpeg binary (default:
                               /usr/bin/ffmpeg) (env: YUM_FFMPEG_PATH)
  [-media-get-path FILE]     . Path to the media-get binary (default:
                               /usr/bin/media-get) (env: YUM_MEDIA_GET_PATH)
  [-videos-file FILE]        . Path to the file containing the list of YouTube
                               video ids (default: videos.txt) (env:
                               YUM_VIDEOS_FILE)
  [-youtubedl-path FILE]     . Path to the youtube-dl binary (default:
                               /usr/bin/youtube-dl) (env: YUM_YOUTUBEDL_PATH)
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
Available commands:
```
[ start | s ]            : start shuffling songs
[ stop | q ]             : stop playing all songs
[ skip | n ]             : skip the current song
[ play | p ] <url>       : queue a song to play next (LIFO)
[ play! | p! ] <url>     : play a song immediately
[ ping ]                 : ping yum for a pong
[ help | h ]             : print this help text
```
> Supported `<url>` formats:
> - `[...]https://www.youtube.com/watch?v=<id>[...]`
> - `[...]https://youtu.be/<id>[...]`
> - `[...]https://www.bilibili.com/video/<id>[...]`
> - `[...]https://b23.tv/<id>[...]`
