### Mattermost

#### About Mattermost

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/mattermost

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mattermost
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "token": "3jy5kjfruiys3y4dsxbyjs8hoe",
        "team_id": "5shstyr6ti8ztkcppr1imjfnwo"
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch a mapping of team_ids to kazoo_users

> GET /v2/accounts/{ACCOUNT_ID}/mattermost/teams_map

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mattermost/teams_map
```

#### Fetch a mapping of Kazoo users to Mattermost users

> GET /v2/accounts/{ACCOUNT_ID}/mattermost/users_map

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/mattermost/users_map
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "user_id": "518662dd945500a59e047560e842ad63",
            "mattermost_user_id": "i43jw4b6h3rd3mzjd1odi5bsco"
        },
        {
            "user_id": "caee2e7235a828e5668bc3dca17e9481",
            "mattermost_user_id": "ip1bbniqcfrqxnfth88giaqcce"
        },
        {
            "user_id": "3233242aa3977767fa21e699f3652b9f",
            "mattermost_user_id": "at9e7wcdqpb3xy4aurbe6tw8ro"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```
