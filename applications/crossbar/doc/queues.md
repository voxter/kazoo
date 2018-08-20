### Queues

#### About Queues

When you have more callers than agents to handle those calls, you can create a call queue to put the callers on hold while agents process callers in the order they arrived in.

#### Schema

Call Queues - FIFO call queues for serializing callers connecting to agents



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`agent_ring_timeout` | In seconds, how long to ring an agent before progressing to the next agent available | `integer()` | `15` | `false`
`agent_wrapup_time` | Pre-defined wait period applied after an agent handles a customer call | `integer()` | `0` | `false`
`announce` | Media ID (or appropriate media URI) of media to play when caller is about to be connected. | `string()` |   | `false`
`announcements.interval` | Time between announcements | `integer()` | `30` | `false`
`announcements.media.in_the_queue` | Played after the numeric position | `string()` |   | `true`
`announcements.media.increase_in_call_volume` | Played if the estimated wait time has increased since the previous wait time announcement | `string()` |   | `true`
`announcements.media.the_estimated_wait_time_is` | Played before the estimated wait time media | `string()` |   | `true`
`announcements.media.you_are_at_position` | Played before the numeric position | `string()` |   | `true`
`announcements.media` | Custom prompts to be played for the announcements | `object()` |   | `false`
`announcements.position_announcements_enabled` | Whether announcements of the caller's position in the queue should be played | `boolean()` |   | `false`
`announcements.wait_time_announcements_enabled` | Whether announcements of the estimated wait time in the queue should be played | `boolean()` |   | `false`
`announcements` | Configuration for periodic announcements to callers waiting in the queue | `object()` |   | `false`
`breakout.dtmf` | Digit that puts caller into breakout menu | `string()` |   | `true`
`breakout.media.call_back_at` | You will be called back at: | `string()` |   | `false`
`breakout.media.callback_registered` | Your callback has been registered. Goodbye! | `string()` |   | `false`
`breakout.media.enter_callback_number` | Enter a callback number, followed by the pound sign. | `string()` |   | `false`
`breakout.media.number_correct` | If this is correct, press 1. Otherwise, press 2. | `string()` |   | `false`
`breakout.media.prompt` | Press 1 to receive a callback. | `string()` |   | `false`
`breakout.media` | Media overrides for breakout menu prompts | `object()` |   | `false`
`breakout` |   | `object()` |   | `false`
`caller_exit_key` | Key caller can press while on hold to exit the queue and continue in the callflow | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '*' | '0' | '#')` |   | `false`
`cdr_url` | An optional HTTP URL to POST the CDR | `string()` |   | `false`
`connection_timeout` | In seconds, how long to try to connect the caller before progressing past the queue callflow action | `integer()` | `3600` | `false`
`enter_when_empty` | Allows a caller to enter a queue and wait when no agents are available | `boolean()` | `true` | `false`
`max_priority` | Maximum possible priority level queue will support. Can not be redefined for existing queue. | `integer()` |   | `false`
`max_queue_size` | How many callers are allowed to wait on hold in the queue (0 for no limit) | `integer()` | `0` | `false`
`moh` | Media ID (or appropriate media URI) of media to play while caller is on hold. | `string()` |   | `false`
`name` | A friendly name for the queue | `string(1..128)` |   | `true`
`record_caller` | When enabled, a caller's audio will be recorded | `boolean()` | `false` | `false`
`recording_url` | An optional HTTP URL to PUT the call recording after the call ends (and should respond to GET for retrieving the audio data) | `string()` |   | `false`
`ring_simultaneously` | The number of agents to try in parallel when connecting a caller | `integer()` | `1` | `false`
`strategy` | The queue strategy for connecting agents to callers | `string('round_robin' | 'most_idle' | 'skills_based_round_robin')` | `round_robin` | `false`



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/queues

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "37139638ff5b68f155d8445178524df1",
            "name": "Support Queue"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Create a queue

> PUT /v2/accounts/{ACCOUNT_ID}/queues

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"name":"Support Queue"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "agent_ring_timeout": 15,
        "agent_wrapup_time": 0,
        "caller_exit_key": "#",
        "connection_timeout": 3600,
        "enter_when_empty": true,
        "id": "37139638ff5b68f155d8445178524df1",
        "max_queue_size": 0,
        "name": "Support Queue",
        "record_caller": false,
        "ring_simultaneously": 1,
        "strategy": "round_robin"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Remove a queue

> DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "agent_ring_timeout": 15,
        "agent_wrapup_time": 0,
        "caller_exit_key": "#",
        "connection_timeout": 3600,
        "enter_when_empty": true,
        "id": "{QUEUE_ID}",
        "max_queue_size": 0,
        "name": "Support Queue",
        "record_caller": false,
        "ring_simultaneously": 1,
        "strategy": "round_robin"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Details of a specific queue

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "agent_ring_timeout": 15,
        "agent_wrapup_time": 0,
        "agents": [],
        "caller_exit_key": "#",
        "connection_timeout": 3600,
        "enter_when_empty": true,
        "id": "{QUEUE_ID}",
        "max_queue_size": 0,
        "name": "Support Queue",
        "record_caller": false,
        "ring_simultaneously": 1,
        "strategy": "round_robin"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Update a queue's properties

> POST /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

> PATCH /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"name":"Support Queue", "max_queue_size": 7}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "agent_ring_timeout": 15,
        "agent_wrapup_time": 0,
        "caller_exit_key": "#",
        "connection_timeout": 3600,
        "enter_when_empty": true,
        "id": "93d35ae9f91cf2d5ee4e1bfe59dda029",
        "max_queue_size": 7,
        "name": "Support Queue",
        "record_caller": false,
        "ring_simultaneously": 1,
        "strategy": "round_robin"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List queues stats (processed calls are summarized) (lightweight API)

> GET /v2/accounts/{ACCOUNT_ID}/queues/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/stats_summary
```

Handled/Waiting stat fields:

Key | Type | Description
--- | ---- | -----------
`agent_id` | `string()` | ID of the agent whom was assigned to the call
`average_wait_time_estimation` | `integer()` | Most recent prediction of number of seconds until pickup
`call_id` | `string()` | ID of the call (the original call in case of a callback)
`caller_id_name` | `string()` | Caller ID name of the queue caller
`caller_id_number` | `string()` | Caller ID number of the queue caller
`caller_priority` | `integer()` | Priority of the queue call (higher value =:= higher priority)
`entered_position` | `integer()` | Position in the queue when the call first entered
`entered_timestamp` | `integer()` | Gregorian timestamp of when the call entered the queue
`exited_position` | `integer()` | Position in the queue when the call was picked up
`handled_timestamp` | `integer()` | Gregorian timestamp of when the call was picked up by the agent
`id` | `string()` | ID of the queue call stat
`is_callback` | `boolean()` | True if the call is a callback
`queue_id` | `string()` | ID of the queue the call is in
`required_skills` | `array(string())` | Array of skills required for an agent to be able to ring for the call
`status` | `string()` | Status of the queue call
`wait_time` | `integer()` | Number of seconds of waiting before the call was picked up

Summarized stat fields:

Key | Type | Description
--- | ---- | -----------
`{QUEUE_ID}.abandoned_calls` | `integer()` | Number of abandoned calls in the queue within the stats window
`{QUEUE_ID}.total_calls` | `integer()` | Total number of calls in the queue within the stats window
`{QUEUE_ID}.total_wait_time` | `integer()` | Total seconds of wait for all calls in the queue within the stats window
`{QUEUE_ID}` | `object()` | Summarized queue stats for {QUEUE_ID}

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "current_timestamp": 63642383800,
        "Handled": [
            {
                "agent_id": "c278b28e4e2267b49d64fdca7b0e4ec6",
                "average_wait_time_estimation": 35,
                "call_id": "WVpVIZID4ZWsToxgZXvqtoAarEHKIkXd",
                "caller_id_name": "1003 Guy",
                "caller_id_number": "1003",
                "entered_position": 1,
                "entered_timestamp": 63702004038,
                "exited_position": 1,
                "exited_timestamp": 63702004395,
                "id": "WVpVIZID4ZWsToxgZXvqtoAarEHKIkXd::f55a9c721180cb044c3a28239f47f460",
                "is_callback": false,
                "queue_id": "f55a9c721180cb044c3a28239f47f460",
                "required_skills": [
                    "EN"
                ],
                "status": "waiting",
                "wait_time": 357
            }
        ],
        "Summarized": [
            {
                "f55a9c721180cb044c3a28239f47f460": {
                    "abandoned_calls": 1,
                    "total_calls": 2,
                    "total_wait_time": 1
                }
            }
        ],
        "Waiting": [
            {
                "average_wait_time_estimation": 35,
                "call_id": "WVpVIZID4ZWsToxgZXvqtoAarEHKIkXd",
                "caller_id_name": "1003 Guy",
                "caller_id_number": "1003",
                "entered_position": 1,
                "entered_timestamp": 63702004038,
                "id": "WVpVIZID4ZWsToxgZXvqtoAarEHKIkXd::f55a9c721180cb044c3a28239f47f460",
                "is_callback": false,
                "queue_id": "f55a9c721180cb044c3a28239f47f460",
                "required_skills": [
                    "EN"
                ],
                "status": "waiting"
            }
        ]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List queues stats

> GET /v2/accounts/{ACCOUNT_ID}/queues/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/stats
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "current_timestamp": 63642383800,
        "stats": []
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List waiting members (call IDs) of queue

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/members

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/members
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "OCkFHuCXEcS54Ce2cDFkMkeXIe65EqPK"
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Remove a queue member from queue

> DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/members/{CALL_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/members/{CALL_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "member remove sent",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Clear a queue's roster

> DELETE /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List queue roster (which agents are assigned to the queue)

> GET /v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [],
    "page_size": 0,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Set the queue roster

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster -d '{"data": ["f3ced8ea7bccc352a2124e8a34351e81", "e154a97ec2942599865a1591a477fd19"]}'
