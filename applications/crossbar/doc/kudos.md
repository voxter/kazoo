### Kudos

#### About Kudos

This API is used to retrieve Kudos configuration for an account. This configuration is found on the
'kudos' document in the account database.

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/kudos

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/kudos
```

