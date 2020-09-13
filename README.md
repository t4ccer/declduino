# declduino
declduino is a declarative way to create arudino-based IOT devices. declduino transpiles declarative yaml file that describes your IOT device into c++ arduino code.

Declduino uses mqtt to communication so to use declduino for your IOT devices you must have mqtt server

## Supported boards
- Esp32  

More will be added soon

## Features

- Digital output (LEDs, relays)
- Digital input (Buttons, switches)

## Usage
```
declduino [COMMAND] ... [OPTIONS]

Common flags:
  -h --help                  Take a guess

declduino generate [OPTIONS] [FILES]

OPTIONS:
  -s --ssid=<wifi-ssid>      Overrides WiFI SSID
  -p --pass=<wifi-pass>      Overrides WiFi password
  -m --mqtt=<mqtt-addr>      Overrides MQTT broker address
     --port=<mqtt-port>      Overrides MQTT broker port
  -b --board=<board-type>    Overrides board type
  -n --name=<device-name>    Overrides device name. Note that this option
                             will override name of all devices
  -v --verbosity=<mode>      Sets verbosity. mode =
                             <debug|info|warning|error>. Default: info

declduino hass [OPTIONS] [FILES]

OPTIONS:
  -o --output=<output.yaml>  Configuration output file. Default:
                             configuration.yaml
  -n --name=<device-name>    Overrides device name. Note that this option
                             will override name of all devices
  -v --verbosity=<mode>      Sets verbosity. mode =
                             <debug|info|warning|error>. Default: info
```

## Sample
```yaml
board: esp32
name: led-blinker
ssid: wifi-name
pass: wifi-pass
mqtt: mqtt-broker-server
port: 1883
components:
- type: digital-output
  name: led
  pin: 2
```
Will transform into (formatted):
```c++
#include "WiFi.h"
#include <PubSubClient.h>

WiFiClient espClient;
PubSubClient client(espClient);

void handle_led(byte* message, unsigned int length)
{
    if (length != 1) {
        return;
    }
    if (message[0] == '1') {
        digitalWrite(2, HIGH);
    }
    else if (message[0] == '0') {
        digitalWrite(2, LOW);
    }
}

void callback(char* topic, byte* message, unsigned int length)
{
    if (String(topic) == "declduino/led-blinker/led") {
        handle_led(message, length);
    }
}

void setup()
{
    WiFi.begin("wifi-name", "wifi-pass");
    while (WiFi.status() != WL_CONNECTED) {
        delay(500);
    }
    client.setServer("mqtt-broker-server", 1883);
    client.setCallback(callback);
    client.connect("esp32");
    client.subscribe("declduino/led-blinker/led");
    pinMode(2, OUTPUT);
}

void loop()
{
    client.loop();
}
```
## Contribution
Feel free to add some features, ideas or fix bugs. Tasks that are waiting to complete are listed [here](https://github.com/t4ccer/declduino/projects/1).
