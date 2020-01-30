# Push Notification Subscriptions

## About Push Notification Subscriptions

This is the crossbar module where users can register for push notifications.

A single Kazoo device may only be registered for push notifications to a single mobile device. The schema will enforce this based on the `device_id` property. As well, a single mobile device can only register for push notifications for a single Kazoo device. The schema will not reject requests to associate the mobile device with a different Kazoo device. However, the module will automatically remove registrations from other Kazoo devices with `mobile_device_id` matching the submitted `mobile_device_id`. This could later be changed to support multiple Kazoo device registrations on a single mobile device.

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



#### Other schema properties

The following properties are dynamic, so need to be separated from the auto-generated schema above.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`platform` | The platform that the push notification subscription is for. Used only by fcm | `string('android' | 'ios')` | `'android'` | `true` |  



## Fetch a push notification subscription
Gets the push notification registration for the supplied device for the supplied app

> GET /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "app_name": "{APP}",
        "device_id": "{DEVICE_ID}",
        "id": "{SUBSCRIPTION_ID}",
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_preferences": [
            "chat",
            "new_voicemail"
        ],
        "notification_registration_id": "{TOKEN}",
        "notification_type": "{NOTIFICATION_TYPE}",
        "platform": "{PLATFORM}"
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}
}
```

## Create a new push notification subscription
Creates a push notification registration for the supplied device for the supplied app

> PUT /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"mobile_device_id":"{MOBILE_DEVICE_ID}", "notification_preferences":["chat", "new_voicemail"], "notification_registration_id":"{TOKEN}", "notification_type":"{NOTIFICATION_TYPE}", "platform":"{PLATFORM}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "app_name": "{APP}",
        "device_id": "{DEVICE_ID}",
        "id": "{SUBSCRIPTION_ID}",
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_preferences": [
            "chat",
            "new_voicemail"
        ],
        "notification_registration_id": "{TOKEN}",
        "notification_type": "{NOTIFICATION_TYPE}",
        "platform": "{PLATFORM}"
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}
}
```

## Change a push notification subscription
Updates a push notification registration for the supplied device for the supplied app. Good for changing the notification preferences
or updating the device token.

> POST /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"mobile_device_id":"{MOBILE_DEVICE_ID}", "notification_preferences":["chat", "new_voicemail"], "notification_registration_id":"{TOKEN}", "notification_type":"{NOTIFICATION_TYPE}", "platform":"{PLATFORM}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "app_name": "{APP}",
        "device_id": "{DEVICE_ID}",
        "id": "{SUBSCRIPTION_ID}",
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_preferences": [
            "chat",
            "new_voicemail"
        ],
        "notification_registration_id": "{TOKEN}",
        "notification_type": "{NOTIFICATION_TYPE}",
        "platform": "{PLATFORM}"
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}
}
```

## Remove a push notification subscription
Deletes a push notification registration for the supplied device for the supplied app.

> DELETE /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "app_name": "{APP}",
        "device_id": "{DEVICE_ID}",
        "id": "{SUBSCRIPTION_ID}",
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_preferences": [
            "chat",
            "new_voicemail"
        ],
        "notification_registration_id": "{TOKEN}",
        "notification_type": "{NOTIFICATION_TYPE}",
        "platform": "{PLATFORM}",
        "_read_only": {
            "deleted": true
        }
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}
}
```

