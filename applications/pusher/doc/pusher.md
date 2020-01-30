
# Pusher
pusher app allows kazoo to send a push message to a device when the device is the target of a bridge call, so that the device can "wake up", register and receive the call.

pusher listens for reg_success messages, checks if the user-agent supports push messages and updates the endpoint_id with a pusher object used in the construction of an endpoint for that device (or a failover endpoint in case of an unregistered device).

freeswitch will send the failover to kamailio, kamailio uses kazoo_query to call pusher to send the real push message, waits for the registration and then completes the call.

if the device is already registered, and the client is alive, kamailio will allow the SIP transaction to continue and the call will be handled as usual

## Configuration

### System Config

* `User-Agents`: list of user agents to check for pusher properties.

```
 "User-Agents": {
       "Linphone": {
           "regex": "^Linphone",
           "properties": {
               "Token-App": "app-id",
               "Token-Type": "pn-type",
               "Token-ID": "pn-tok"
           }
       }
   }
```

### Maintenance

In order for the push services from apple / firebase to work they need to be configured with application secrets / certificates. The app used in the push message is taken from Token-App.

* `sup pusher_maintenance add_firebase_app(AppId, Secret)`
* `sup pusher_maintenance add_apple_app(AppId, CertFile)` (uses the default APNs host: api.push.apple.com)
* `sup pusher_maintenance add_apple_app(AppId, CertFile, Host)` (uses a custom APNs host, i.e. api.development.push.apple.com)

### iOS Certificates and Private Keys

1. Create a new _Apple Push Services_ certificate at [https://developer.apple.com/account/resources/certificates/list](https://developer.apple.com/account/resources/certificates/list). The certificate type should be _Apple Push Notification service SSL (Sandbox & Production)_ under _Services_.
2. Add the certificate to Keychain by double-clicking it.
3. Open _Keychain Access_.
4. View the _login_ keychain, and set the _Category_ on the bottom-left of the window to _Certificates_. The view should list certificates, including the one imported from Apple. It should have the private key from the certificate signing request nested beneath it.
5. Right-click the certificate and choose _Export "${certificateName}"..._.
6. Save as .p12 format.
7. Execute `openssl pkcs12 -in ${CERT_NAME}.p12 -out ${CERT_NAME}.pem -clcerts -nodes` to export the certificate and private key as a .pem file. The private key must be unencrypted, hence the `-nodes` flag.
