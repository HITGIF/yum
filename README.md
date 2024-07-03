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
3. Download [yum](https://github.com/HITGIF/yum)
```sh
wget https://github.com/HITGIF/yum/releases/latest/download/yum
chmod +x yum
```
4. Make a playlist file containing a list of YouTube video IDs, e.g. [mine](https://gist.github.com/HITGIF/bf3ee113f9d86afe717d7fc6a6731b8c)
```sh
wget https://gist.githubusercontent.com/HITGIF/bf3ee113f9d86afe717d7fc6a6731b8c/raw/57df953ab9ff70709825031559b1d5f1562f3ed1/videos.txt
```

## Run
```sh
./yum -discord-bot-token [YOUR_DISCORD_BOT_TOKEN]
```
### Options
```
  [-discord-bot-token STRING]
                             . Discord bot token (env: YUM_DISCORD_BOT_TOKEN)
  [-ffmpeg-path FILE]        . Path to the ffmpeg binary (default:
                               /usr/bin/youtube-dl) (env: YUM_FFMPEG_PATH)
  [-videos-file FILE]        . Path to the file containing the list of YouTube
                               video ids (default: videos.txt) (env:
                               YUM_VIDEOS_FILE)
  [-youtubedl-path FILE]     . Path to the youtube-dl binary (default:
                               /usr/bin/ffmpeg) (env: YUM_YOUTUBEDL_PATH)
  [-build-info]              . print info about this build and exit
  [-version]                 . print the version of this build and exit
  [-help], -?                . print this help text and exit
```
