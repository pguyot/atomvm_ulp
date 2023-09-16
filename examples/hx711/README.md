<!---
  Copyright 2023 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

ULP HX711 sample code
=====================

This sample code shows how to detect an object using a
[HX711](https://www.digikey.com/htmldatasheets/production/1836471/0/0/1/hx711.html)
and a setup phase.

The HX711 is read using ULP code (with an average of 5 measures), first with
no object, then with an object, and a threshold is computed as the average of
the two.

Then the ESP32 is put into deep sleep. The ULP performs measures every 200ms
and wakes the ESP32 when it's higher than the threshold.

For simplicity, a single ULP program is used.

Hardware
--------

HX711 is supposed to be connected to gpios 32 (clock) and 33 (data).

Installation
------------

- Compile and install AtomVM with `atomvm_ulp` as explained [here](../../README.md)
- Install rebar3
- Compile and flash with:

```
rebar3 esp32_flash -p /dev/tty.usbserial-*
```
