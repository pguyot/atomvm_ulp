<!---
  Copyright 2023 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

ULP SHT20 sample code
=====================

This sample code shows how to bit-bang I2C to connect to a
[SHT20](https://sensirion.com/media/documents/CCDE1377/635000A2/Sensirion_Datasheet_Humidity_Sensor_SHT20.pdf)
chip while asleep and wakes up every 16 measures.

Special consideration
---------------------

This sample code requires that the memory reserved to ULP be bumped to at least
700 bytes (instead of default of 512), as the program is large and saves recorded
temperatures every 5 seconds.

Hardware
--------

SHT20 is supposed to be connected to gpios 0 (SDA) and 26 (SCL).

Installation
------------

- Compile and install AtomVM with `atomvm_ulp` as explained [here](../../README.md)
- Install rebar3
- Compile and flash with:

```
rebar3 esp32_flash -p /dev/tty.usbserial-*
```
