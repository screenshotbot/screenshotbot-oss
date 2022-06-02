# Katalon Studio sample plugin for beginners

This is a demostrated plugin that listen to `Plugin installation` event and `Katalon Studio execution` event.

## Build

Requirements:
- JDK 1.8
- Maven 3.3+

`mvn clean package`

## Usage
- Install the `Katalon Studio v6.0.3 or later`.
- Go to *Plugin* > *Install Plugin* and select the generated jar file.
- A `hello` message `Event Log` tab after the installation completed. 
- Execute a test suite and wait for a summary message.