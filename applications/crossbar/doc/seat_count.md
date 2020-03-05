# Seat Count

## About Seat Count
This API queries seat type counts, by location, for a given account_id, and all of its descendants.
Whatever account ID you query for, it will go and spider down the account tree for every account beneath it.

## Fetch seat counts for all descendants of an account
This will include children, grandchildren, etc

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/seat_count

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/seat_count
{
    "next_start_key": "g20AAAACYXM",
    "page_size": 3,
    "start_key": "{START_KEY}",
    "data": [
        {
            "id": "{ACCOUNT_ID}",
            "name": "Name1",
            "realm": "Name1.test.sip.voxter.com",
            "tree": [
                "9033f5bb13ebf1927f05b7c3e83b3359",
                "047455305a2faf397b61e30429c02cf1"
            ],
            "flags": [],
            "locations": {}
        },
        {
            "id": "{ACCOUNT_ID}",
            "name": "Name2",
            "realm": "Name2.sip.voxter.com",
            "tree": [
                "9033f5bb13ebf1927f05b7c3e83b3359"
            ],
            "flags": [],
            "locations": {
                "location_1": {
                    "seat_type_a": 4,
                    "seat_type_b": 1,
                    "seat_type_c": 1,
                    "seat_type_d": 1,
                    "seat_type_e": 1
                },
                "location_2": {
                    "seat_type_a": 1,
                    "seat_type_c": 1,
                    "seat_type_f": 1
                },
                "location_3": {
                    "seat_type_b": 1,
                    "seat_type_d": 1,
                    "seat_type_f": 3
                }
            }
        },
        {
            "id": "{ACCOUNT_ID}",
            "name": "Name3",
            "realm": "Name3.test.sip.voxter.com",
            "tree": [
                "9033f5bb13ebf1927f05b7c3e83b3359",
                "047455305a2faf397b61e30429c02cf1"
            ],
            "flags": [],
            "locations": {}
        }
    ],
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```
