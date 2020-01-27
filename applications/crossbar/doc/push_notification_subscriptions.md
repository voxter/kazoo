# Push Notification Subscriptions

## About Push Notification Subscriptions

This is the crossbar module where users can register for push notifications.

A single Kazoo device may only be registered for push notifications to a single mobile device. The schema will enforce this based on the `mobile_device_id` property. As well, a single mobile device can only register for push notifications for a single Kazoo device. The schema will not reject requests to associate the mobile device with a different Kazoo device. However, the module will automatically remove registrations from other Kazoo devices with `mobile_device_id` matching the submitted `mobile_device_id`. This could later be changed to support multiple Kazoo device registrations on a single mobile device.

#### Schema

Schema for push notification subscriptions



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`app_name` | The identifier for the app that will be sending push notifications | `string()` |   | `true` |  
`mobile_device_id` | An identifier provided by the mobile device | `string()` |   | `true` |  
`notification_registration_ids` | Notification configuration per push notification registration id | `object()` |   | `true` |  
`platform` | The platform that the push notification subscriptions are for | `string('android' | 'ios')` |   | `true` |  
`sip_proxy_server` | The hostname or IP address of the SIP registrar the app will register to upon waking from a push notification | `string()` |   | `true` |  



#### Additional schema properties

The following properties are `additionalProperties`, so need to be separated from the auto-generated schema above.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`notification_registration_ids./.*/.notification_preferences` | The types of notifications that the user wants to receive. The values must be unique amongst all notification registration ids in the subscription | `array(string('chat' | 'incoming_call' | 'new_unowned_voicemail' | 'new_voicemail'))` |   | `true` |  
`notification_registration_ids./.*/.notification_type` | The type of notification service to use | `string('apns' | 'fcm')` |   | `true` |  



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
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_registration_ids": {
            "{APNS_TOKEN}": {
                "notification_preferences": [
                    "incoming_call"
                ],
                "notification_type": "apns"
            },
            "{FCM_TOKEN}": {
                "notification_preferences": [
                    "chat",
                    "new_voicemail"
                ],
                "notification_type": "fcm"
            }
        },
        "platform": "{PLATFORM}",
        "sip_proxy_server": "{KAMAILIO_IP}",
        "app_name": "{APP}"
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
    -d '{"data":{"mobile_device_id":"{MOBILE_DEVICE_ID}", "notification_registration_ids":{"{APNS_TOKEN}":{"notification_preferences":["incoming_call"], "notification_type":"apns"}, "{FCM_TOKEN}":{"notification_preferences":["chat", "new_voicemail"], "notification_type":"fcm"}}, "platform":"{PLATFORM}", "sip_proxy_server":"{KAMAILIO_IP}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_registration_ids": {
            "{APNS_TOKEN}": {
                "notification_preferences": [
                    "incoming_call"
                ],
                "notification_type": "apns"
            },
            "{FCM_TOKEN}": {
                "notification_preferences": [
                    "chat",
                    "new_voicemail"
                ],
                "notification_type": "fcm"
            }
        },
        "platform": "{PLATFORM}",
        "sip_proxy_server": "{KAMAILIO_IP}",
        "app_name": "{APP}"
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
    -d '{"data":{"mobile_device_id":"{MOBILE_DEVICE_ID}", "notification_registration_ids":{"{APNS_TOKEN}":{"notification_preferences":["incoming_call"], "notification_type":"apns"}, "{FCM_TOKEN}":{"notification_preferences":["chat", "new_voicemail"], "notification_type":"fcm"}}, "platform":"{PLATFORM}", "sip_proxy_server":"{KAMAILIO_IP}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "mobile_device_id": "{MOBILE_DEVICE_ID}",
        "notification_registration_ids": {
            "{APNS_TOKEN}": {
                "notification_preferences": [
                    "incoming_call"
                ],
                "notification_type": "apns"
            },
            "{FCM_TOKEN}": {
                "notification_preferences": [
                    "chat",
                    "new_voicemail"
                ],
                "notification_type": "fcm"
            }
        },
        "platform": "{PLATFORM}",
        "sip_proxy_server": "{KAMAILIO_IP}",
        "app_name": "{APP}"
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
        "id": "{DEVICE_ID}",
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

