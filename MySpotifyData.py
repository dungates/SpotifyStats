import pandas as pd
import spotipy
pd.options.display.html.table_schema = True
pd.options.display.max_rows = None

sp = spotipy.Spotify()
from spotipy.oauth2 import SpotifyClientCredentials
cid ="383cfac3d8434244a38c4e279a04ce47"
secret = "486c6f539f8e45a2b82f03a30cfed26f"
client_credentials_manager = SpotifyClientCredentials(client_id = cid, client_secret = secret)
sp = spotipy.Spotify(client_credentials_manager = client_credentials_manager)
sp.trace = False

playlist = sp.user_playlist("dungates", "https://open.spotify.com/playlist/6slH6T3IWAPgHzTJBk8ot9")

songs = playlist["tracks"]["items"]
tracks = playlist["tracks"]

ids = []
while tracks['next']:
    tracks = sp.next(tracks)
    for item in tracks["items"]:
        if (item['track']['id'] is not None):
            ids.append(item['track']['id'])

features = []

for i in range (0, len(ids), 50):
    audio_features = sp.audio_features(ids[i:i+50])
    for track in audio_features:
        features.append(track)
# for i in range(len(songs)):
#     ids.append(songs[i]["track"]["id"])
# features = sp.audio_features(ids)

df = pd.DataFrame(features)
df
