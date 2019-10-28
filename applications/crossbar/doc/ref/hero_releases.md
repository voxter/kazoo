# Hero Releases

## About Hero Releases

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/hero_releases

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_notes

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_notes
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_availability

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/update_availability
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/promote

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_releases/{VERSION_ID}/promote
```

