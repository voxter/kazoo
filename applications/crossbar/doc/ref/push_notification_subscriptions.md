# Push Notification Subscriptions

## About Push Notification Subscriptions

#### Schema

Schema for push notification subscriptions



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`app_name` | The identifier for the app that will be sending push notifications | `string()` |   | `true` |  
`mobile_device_id` | An identifier provided by the mobile device | `string()` |   | `true` |  
`notification_registration_ids` | Notification configuration per push notification registration id | `object()` |   | `true` |  
`platform` | The platform that the push notification subscriptions are for | `string('android' | 'ios')` |   | `true` |  
`sip_proxy_server` | The hostname or IP address of the SIP registrar the app will register to upon waking from a push notification | `string()` |   | `true` |  



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

