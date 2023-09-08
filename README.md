<!---
  Copyright 2023 Paul Guyot <pguyot@kallisys.net>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# ESP32 ULP Driver for AtomVM

This project brings ESP32 ULP support to AtomVM.
It also includes implementation for `rtc_gpio` APIs for use with ULP.

## Installation

- Install ESP-IDF SDK 4.4 or higher
- Checkout [atomvm](https://github.com/AtomVM/AtomVM)
- Within `src/platform/esp32/components`, checkout this project
- Activate ESP-IDF environment and within `src/platform/esp32/` run `idf.py build`

## Usage

ULP needs to be activated with menuconfig. If it isn't, compilation will fail.
Sufficient memory should be allocated for ULP programs, if it is not the case
loading binary will fail with `badarg`. Default is 512 bytes.

ULP binaries are loaded with `ulp:load_binary/1,2`. Binaries can be generated
using the ULP toolchain or directly with macros from `ulp.hrl`.

Please also refer to [examples](examples/) and
[API documentation](https://pguyot.github.io/atomvm_ulp/).
