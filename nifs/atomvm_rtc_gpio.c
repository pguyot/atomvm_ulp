/*
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_RTC_GPIO_NIFS

#include <stdlib.h>

#include <atom.h>
#include <defaultatoms.h>
#include <esp32_sys.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>
#include <utils.h>

#include <driver/rtc_io.h>

// #define ENABLE_TRACE
#include <trace.h>

static const struct Nif* rtc_gpio_nif_get_nif(const char* nifname);

static const AtomStringIntPair pin_level_table[] = {
    { ATOM_STR("\x3", "low"), 0 },
    { ATOM_STR("\x4", "high"), 1 },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair mode_table[] = {
    { ATOM_STR("\xA", "input_only"), RTC_GPIO_MODE_INPUT_ONLY },
    { ATOM_STR("\xB", "output_only"), RTC_GPIO_MODE_OUTPUT_ONLY },
    { ATOM_STR("\xC", "input_output"), RTC_GPIO_MODE_INPUT_OUTPUT },
    { ATOM_STR("\x8", "disabled"), RTC_GPIO_MODE_DISABLED },
    { ATOM_STR("\x9", "output_od"), RTC_GPIO_MODE_OUTPUT_OD },
    { ATOM_STR("\xF", "input_output_od"), RTC_GPIO_MODE_INPUT_OUTPUT_OD },
    SELECT_INT_DEFAULT(-1)
};

static term nif_rtc_gpio_is_valid_gpio(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    bool result = rtc_gpio_is_valid_gpio(term_to_int(argv[0]));
    return result ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_rtc_gpio_gpio_to_rtc_gpio(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    int result = rtc_io_number_get(term_to_int(argv[0]));
    if (result < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return term_from_int(result);
}

#define RTCGPIO_NIF_GPIO(api_funcname)                                  \
    static term nif_##api_funcname(Context* ctx, int argc, term argv[]) \
    {                                                                   \
        UNUSED(argc);                                                   \
        VALIDATE_VALUE(argv[0], term_is_integer);                       \
        esp_err_t err = api_funcname(term_to_int(argv[0]));             \
        if (UNLIKELY(err != ESP_OK)) {                                  \
            RAISE_ERROR(BADARG_ATOM);                                   \
        }                                                               \
        return OK_ATOM;                                                 \
    }

RTCGPIO_NIF_GPIO(rtc_gpio_init)
RTCGPIO_NIF_GPIO(rtc_gpio_deinit)

static term nif_rtc_gpio_get_level(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    uint32_t level = rtc_gpio_get_level(term_to_int(argv[0]));
    if (UNLIKELY(level != 0 && level != 1)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return term_from_int(level);
}

static term nif_rtc_gpio_set_level(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    uint32_t level;
    if (term_is_integer(argv[1])) {
        level = term_to_int(argv[1]);
    } else if (term_is_atom(argv[1])) {
        int level_i = interop_atom_term_select_int(pin_level_table, argv[1], ctx->global);
        if (UNLIKELY(level_i < 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        level = level_i;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = rtc_gpio_set_level(term_to_int(argv[0]), level);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

static term nif_rtc_gpio_set_direction(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_atom);
    int mode_i = interop_atom_term_select_int(mode_table, argv[1], ctx->global);
    if (UNLIKELY(mode_i < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = rtc_gpio_set_direction(term_to_int(argv[0]), (rtc_gpio_mode_t)mode_i);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

static term nif_rtc_gpio_set_direction_in_sleep(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_atom);
    int mode_i = interop_atom_term_select_int(mode_table, argv[1], ctx->global);
    if (UNLIKELY(mode_i < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = rtc_gpio_set_direction_in_sleep(term_to_int(argv[0]), (rtc_gpio_mode_t)mode_i);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

RTCGPIO_NIF_GPIO(rtc_gpio_pullup_en)
RTCGPIO_NIF_GPIO(rtc_gpio_pulldown_en)
RTCGPIO_NIF_GPIO(rtc_gpio_pullup_dis)
RTCGPIO_NIF_GPIO(rtc_gpio_pulldown_dis)

static const struct Nif rtc_gpio_is_valid_gpio_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_is_valid_gpio
};
static const struct Nif rtc_gpio_gpio_to_rtc_gpio_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_gpio_to_rtc_gpio
};
static const struct Nif rtc_gpio_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_init
};
static const struct Nif rtc_gpio_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_deinit
};
static const struct Nif rtc_gpio_get_level_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_get_level
};
static const struct Nif rtc_gpio_set_level_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_set_level
};
static const struct Nif rtc_gpio_set_direction_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_set_direction
};
static const struct Nif rtc_gpio_set_direction_in_sleep_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_set_direction_in_sleep
};
static const struct Nif rtc_gpio_pullup_en_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_pullup_en
};
static const struct Nif rtc_gpio_pulldown_en_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_pulldown_en
};
static const struct Nif rtc_gpio_pullup_dis_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_pullup_dis
};
static const struct Nif rtc_gpio_pulldown_dis_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtc_gpio_pulldown_dis
};

static const struct Nif* rtc_gpio_nif_get_nif(const char* nifname)
{
    if (strncmp("rtc_gpio:", nifname, 9) != 0) {
        return NULL;
    }
    const char* funcname = nifname + 9;
    if (strcmp("is_valid_gpio/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_is_valid_gpio_nif;
    }
    if (strcmp("gpio_to_rtc_gpio/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_gpio_to_rtc_gpio_nif;
    }
    if (strcmp("init/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_init_nif;
    }
    if (strcmp("deinit/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_deinit_nif;
    }
    if (strcmp("get_level/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_get_level_nif;
    }
    if (strcmp("set_level/2", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_set_level_nif;
    }
    if (strcmp("set_direction/2", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_set_direction_nif;
    }
    if (strcmp("set_direction_in_sleep/2", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_set_direction_in_sleep_nif;
    }
    if (strcmp("pullup_en/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_pullup_en_nif;
    }
    if (strcmp("pulldown_en/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_pulldown_en_nif;
    }
    if (strcmp("pullup_dis/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_pullup_dis_nif;
    }
    if (strcmp("pulldown_dis/1", funcname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtc_gpio_pulldown_dis_nif;
    }

    return NULL;
}

REGISTER_NIF_COLLECTION(rtc_gpio, NULL, NULL, rtc_gpio_nif_get_nif)

#endif
