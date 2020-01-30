# Push Notification Subscriptions

## About Push Notification Subscriptions

#### Schema

Schema for a push notification subscription



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`app_name` | The identifier for the app that will be sending push notifications | `string()` |   | `true` |  
`device_id` | The ID of the Kazoo device for which push notifications are being registered | `string()` |   | `true` |  
`mobile_device_id` | An identifier provided by the mobile device | `string()` |   | `true` |  
`notification_preferences.[]` |   | `string('chat' | 'new_unowned_voicemail' | 'new_voicemail')` |   | `true` |  
`notification_preferences` | The types of notifications that the user wants to receive | `array(string('chat' | 'new_unowned_voicemail' | 'new_voicemail'))` | `["chat", "new_voicemail"]` | `true` |  
`notification_registration_id` | The registration id for the mobile device | `string()` |   | `true` |  
`notification_type` | The type of notification service to use | `string('apns' | 'fcm')` |   | `true` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

