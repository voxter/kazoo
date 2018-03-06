## Acdc Member

### About Acdc Member

Puts the caller into the ACDc queue.

#### Schema

Validator for the acdc_member callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enter_as_callback` | Immediately register the call as a callback and hang up | `boolean()` |   | `false`
`id` | ID of the ACDc Queue | `string()` |   | `false`
`priority` | assign a priority to the caller | `integer()` |   | `false`
`var` | Custom variable to check for queue ID | `string()` |   | `false`



