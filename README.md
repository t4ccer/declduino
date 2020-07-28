# declduino
declduino is a declarative way to create arudino-based IOT devices. declduino transpiles declarative yaml file that describes your IOT device into c++ arduino code.

Declduino uses mqtt to communication so to use declduino for your IOT devices you must have mqtt server

## Supported boards
- Esp32  

More will be added soon

## Features

- DigitalOutput (LEDs, switches)

## Usage
```
declduino [OPTIONS] [FILES]

OPTIONS:
  -s --ssid=<wifi-ssid>    Overrides WiFI SSID
  -p --pass=<wifi-pass>    Overrides WiFi password
  -m --mqtt=<mqtt-addr>    Overrides MQTT broker address
  -d --board=<board-type>  Overrides board type
  -n --name=<device-name>  Overrides device name
  -h --help                Take a guess
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
#include <WiFi.h>
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

## TODO
### Features
- Add support for more boards
    - esp8266
    - and more...
- Add support for more components
    - ds18b20 (1-wire thermometer)
    - OTA upload
    - Disconnect detection
    - and more...

### Ecosystem
- All-in-one docker build container (will build yaml to binary)

### Code related
- Use propper eDSL for code generating
- Add support for CLI arguments for board type etc
- Check for name confilcts in component names

## Contribution
Feel free to add some features, ideas or fix bugs
