# Overview

Mate is a mattermost push notification proxy server. It sits between the mattermost server and its clients. This allows Kazoo to have complete control about the contents and delivery of push notifications.

## Setting up Mate
* To set up mate, the machine running it will need to allow incoming http on the port specified as MATE\_PORT in mate\_proxy.erl.
* Log into the mattermost server as admin and go to the system console (in the drop down burger menu). Choose the Mobile Push option from the side menu.
* In the "Enable Push Notifications:" drop down choose "Manually enter Push Notification Service Location"
* For "Push Notification Server:" enter the url where kazoo is running, prefixed by http:// and include the port that Mate will be running on.
* For "Push Notification Contents:" choose "Send full message snippet"
* Click "Save" at the bottom of the screen

Once the above have been completed you should be ready to receive push notification requests from Mattermost with Mate.

## Dependencies
In order to run Mate, Navi will have to be running in the kazoo cluster as Mate simply intercepts the push notifications and tells Navi to send them
with a request over amqp.
