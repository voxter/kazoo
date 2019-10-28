# Hero Releases

## About Hero Releases

APIs for managing the availability and properties of Hero whitelabel releases.

## Schema



## Fetch a list of versions owned by the account

> GET /v2/accounts/{ACCOUNT_ID}/hero_releases

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{VERSION_ID}",
            "assets": [
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                },
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                }
            ],
            "availability": "2019-10-30T17:39:10.000Z",
            "channel": "beta",
            "created_at": "2019-10-16T17:39:10.000Z",
            "name": "1.0.0-beta.1000",
            "notes": ""
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Change release notes on a version

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_notes

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"notes": "New release notes"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_notes
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{VERSION_ID}",
            "assets": [
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                },
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                }
            ],
            "availability": "2019-10-30T17:39:10.000Z",
            "channel": "beta",
            "created_at": "2019-10-16T17:39:10.000Z",
            "name": "1.0.0-beta.1000",
            "notes": "New release notes"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Change the availability date of a version

Versions that are already past their availability date (have been released) cannot have their availability date changed. There is also a cap on the max time since version creation that the version can be deferred.

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_availability

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"availability": "2019-10-23T17:39:10.000Z"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_availability
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{VERSION_ID}",
            "assets": [
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                },
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                }
            ],
            "availability": "2019-10-23T17:39:10.000Z",
            "channel": "beta",
            "created_at": "2019-10-16T17:39:10.000Z",
            "name": "1.0.0-beta.1000",
            "notes": ""
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Promote/release a version

Will fail if the version has already been released. No data fields are required for this request.

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/promote

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/promote
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{VERSION_ID}",
            "assets": [
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                },
                {
                    "platform": "osx_64",
                    "download_url": "http://localhost:9012/download/flavor/default/1.0.0-beta.1000/osx_64"
                }
            ],
            "availability": "2019-10-16T17:39:10.000Z",
            "channel": "beta",
            "created_at": "2019-10-16T17:39:10.000Z",
            "name": "1.0.0-beta.1000",
            "notes": ""
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
