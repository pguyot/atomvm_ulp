<!---
  Copyright 2023 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

ULP interrupt sample code
=========================

This sample code illustrates how to use ULP to send an interrupt to the running
or sleeping ESP32. It tests for GPIO 0 and when it is down, it sends an
interrupt. The ESP32 deep sleeps every other two interrupts. The test occurs
every 200ms, but a real implementation should debounce.

Hardware
--------

Install a button between GPIO 0 and GND.

Installation
------------

- Compile and install AtomVM with `atomvm_ulp` as explained [here](../../README.md)
- Install rebar3
- Compile and flash with:

```
rebar3 esp32_flash -p /dev/tty.usbserial-*
```

Usable GPIOs
------------

It is possible to modify the code to use another GPIO, however, not all GPIOs
are accessible from RTC/ULP.
