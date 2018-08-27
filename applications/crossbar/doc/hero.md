### Hero

#### About Hero

This is an API endpoint specifically for Hero, that will allow you to get only data that is different
to that you have cached.
POST an object with the keys being the object types you want, and the values being objects containing
the IDs and revisions of all the documents you know about for that type. The response will contain
all the documents that have been created or changed. If the document is "null" (yes, the string null)
then the document has been deleted (or never existed).

For example:

##### Request
```
{
    "data": {
        "user_statuses": {
        },
        "users": {
            "{USER_1_ID}": "{USER_1_REV}",
            "{USER_2_ID}": "{USER_2_REV}",
            "{USER_3_ID}": "{USER_2_REV}"
        }
    }
}
```

##### Response

```
{
    "data": {
        "user_statuses": {
            "{USER_STATUS_1_ID}": {USER_STATUS_1_DOC}
        },
        "users": {
            "{USER_1_ID}": {USER_1_DOC},
            "{USER_2_ID}": "null",
            "{USER_4_ID}": {USER_4_DOC}
        }
    }
}
```

##### Explanation of response

- USER_STATUS_1 has been created
- USER_1 has been updated
- USER_2 has been deleted
- USER_3 has not been changed
- USER_4 has been created

#### Schema



#### Change

> POST /v2/accounts/{ACCOUNT_ID}/hero

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/hero
```
