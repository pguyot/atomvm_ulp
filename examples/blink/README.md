<!---
  Copyright 2023 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

ULP Blink sample code
=====================

This sample code illustrates how to use ULP to toggle a GPIO on and off,
including during deep sleep.

Hardware
--------

Install a LED between GPIO 0 and GND. It is preferrable to add a resistor.

Installation
------------

- Compile and install AtomVM with `atomvm_ulp` as explained [here](../../../README.md)
- Install rebar3
- Compile and flash with:

```
rebar3 esp32_flash -p /dev/tty.usbserial-*
```

Usable GPIOs
------------

It is possible to modify the code to use another GPIO, however, not all GPIOs
are accessible from RTC/ULP.
