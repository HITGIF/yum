import click
import os


@click.group()
def cli():
    pass


@cli.command()
@click.option("--client-secrets-file", default="secret.json")
@click.option("--output", default="videos.txt")
def sync(client_secrets_file, output):
    import google_auth_oauthlib.flow
    import googleapiclient.discovery

    # Disable OAuthlib's HTTPS verification when running locally.
    os.environ["OAUTHLIB_INSECURE_TRANSPORT"] = "1"

    api_service_name = "youtube"
    api_version = "v3"
    scopes = ["https://www.googleapis.com/auth/youtube.readonly"]

    # Get credentials and create an API client
    flow = google_auth_oauthlib.flow.InstalledAppFlow.from_client_secrets_file(
        client_secrets_file, scopes
    )
    credentials = flow.run_local_server(port=8088)
    youtube = googleapiclient.discovery.build(
        api_service_name, api_version, credentials=credentials
    )

    ids = []
    next_page_token = None

    while True:
        request = youtube.playlistItems().list(
            part="snippet", maxResults=50, playlistId="LL", pageToken=next_page_token
        )
        response = request.execute()
        ids += [item["snippet"]["resourceId"]["videoId"] for item in response["items"]]
        next_page_token = response.get("nextPageToken")
        total_results = response["pageInfo"]["totalResults"]
        print(f"Synced {len(ids)}/{total_results} items")
        if not next_page_token:
            break

    with open(output, "w") as f:
        for item in ids:
            f.write(f"{item}\n")

    print("Done")


if __name__ == "__main__":
    cli()
