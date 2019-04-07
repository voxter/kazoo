# Hero Preferences

## About Hero Preferences

#### Schema

Hero user preferences JSON schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_hero_groups` | the user's custom user groupings for Hero | `array()` |   | `false` |  
`locale` | The local that should be used for i18n | `string('de' | 'en-nz' | 'en' | 'mi')` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/hero_preferences

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_preferences
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/hero_preferences

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_preferences
```

