# RabbitMQ MLS Plugin #

This was an experiment for a Multi Layer Security Plugin for RabbitMQ, curently the plugin does no checks and only logs the information available to the plugin to INFO.

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installation

Binary builds of this plugin can be obtained from
the [Community Plugins page](https://www.rabbitmq.com/community-plugins.html).

See [Plugin Installation](https://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](https://www.rabbitmq.com/plugin-development.html)).

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_message_timestamp
```

The plugin will then hook into the `basic.publish` process in order to
add the current timestamp as seen by the broker.

## LICENSE ##

See the LICENSE file
