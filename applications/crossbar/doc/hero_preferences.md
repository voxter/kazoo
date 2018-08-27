### Hero Preferences

#### About Hero Preferences

#### Schema

Hero user preferences JSON schema



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`custom_hero_groups` | the user's custom user groupings for Hero | `array()` |   | `false` |  
`locale` | The local that should be used for i18n | `string('de' | 'en-nz' | 'mi')` |   | `false` |  



#### Fetch
When the user has no preferences, crossbar will return the default preferences, but it wont save them against the user.
When the user has created custom preferences via the POST, they will be in the response.

> GET /v2/accounts/{ACCOUNT_ID}/hero_preferences

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_preferences
```

#### Change
When the user has no custom preferences currently, this will set the custom preferences (validated against the schema)
When the user has custom preferences, this will overwrite them (will not merge with old preferences)

> POST /v2/accounts/{ACCOUNT_ID}/hero_preferences

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero_preferences
```

