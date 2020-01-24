### Push Notification Subscriptions

#### About Push Notification Subscriptions

#### Schema

Schema for push notification subscriptions



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`app_name` | The identifier for the app that will be sending push notifications | `string()` |   | `false` |  
`mobile_device_id` | An identifier provided by the mobile device | `string()` |   | `true` |  
`notification_registration_ids` | Notification configuration per push notification registration id | `object()` |   | `true` |  
`platform` | The platform that the push notification subscriptions are for | `string('android' | 'ios')` |   | `true` |  



#### Fetch
Gets the push notification registration for the supplied device for the supplied app

> GET /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

#### Create
Creates a push notification registration for the supplied device for the supplied app

> PUT /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

#### Change
Updates a push notification registration for the supplied device for the supplied app. Good for changing the notification preferences
or updating the device token.

> POST /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

#### Remove
Deletes a push notification registration for the supplied device for the supplied app.

> DELETE /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{DEVICE_ID}
```

