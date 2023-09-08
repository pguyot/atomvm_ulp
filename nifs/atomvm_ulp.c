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
#ifdef CONFIG_AVM_ENABLE_ULP_NIFS

#ifndef CONFIG_ESP32_ULP_COPROC_ENABLED
#error ULP coprocessor needs to be enabled with menuconfig
#endif

#include <stdlib.h>

#include <atom.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <esp32_sys.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>
#include <utils.h>

#include <ulp.h>

// #define ENABLE_TRACE
#include <trace.h>

// Prototypes
static const struct Nif* ulp_nif_get_nif(const char* nifname);
static void ulp_isr_handler(void* arg);

// Resource
static ErlNifResourceType* ulp_isr_handler_resource_type;

struct ULPISRHandler {
    int32_t registered_process_id;
    ErlNifMonitor registered_process_monitor;
};

static void ulp_isr_handler_dtor(ErlNifEnv* caller_env, void* obj)
{
    UNUSED(caller_env);

    struct ULPISRHandler* handler_obj = (struct ULPISRHandler*)obj;
    if (handler_obj->registered_process_id != INVALID_PROCESS_ID) {
        ulp_isr_deregister(ulp_isr_handler, handler_obj);
        handler_obj->registered_process_id = INVALID_PROCESS_ID;
    }
}

static void ulp_isr_handler_down(ErlNifEnv* caller_env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon)
{
    UNUSED(pid);
    UNUSED(mon);
    // Decrement reference count so it can be garbage collected
    enif_release_resource(obj);
}

const ErlNifResourceTypeInit ulp_isr_handler_resource_type_init = {
    .members = 3,
    .dtor = ulp_isr_handler_dtor,
    .stop = NULL,
    .down = ulp_isr_handler_down,
};

static term nif_ulp_load_binary(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    term code_binary;
    uint32_t load_addr;
    if (argc == 1) {
        code_binary = argv[0];
        load_addr = 0;
    } else {
        VALIDATE_VALUE(argv[0], term_is_integer);
        load_addr = term_from_int(argv[0]);
        code_binary = argv[1];
    }
    VALIDATE_VALUE(code_binary, term_is_binary);
    size_t size = term_binary_size(code_binary);
    const uint8_t* data = (const uint8_t*)term_binary_data(code_binary);
    esp_err_t err = ulp_load_binary(load_addr, data, size / 4);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

// Interrupt handler
static void ulp_isr_handler(void* arg)
{
    struct ULPISRHandler* handler_obj = (struct ULPISRHandler*)arg;
    struct RefcBinary* resource = refc_binary_from_data(handler_obj);
    GlobalContext* global = resource->resource_type->global;

    // 1 header + 2 elements
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE, heap);

    term int_msg = term_alloc_tuple(2, &heap);
    term_put_tuple_element(int_msg, 0, globalcontext_make_atom(global, ATOM_STR("\x3", "ulp")));
    term_put_tuple_element(int_msg, 1, term_from_ref_ticks(handler_obj->registered_process_monitor, &heap));

    globalcontext_send_message(global, handler_obj->registered_process_id, int_msg);

    END_WITH_STACK_HEAP(heap, global);
}

// NIF functions
static term nif_ulp_run(Context* ctx, int argc, term argv[])
{
    uint32_t entry_point;
    if (argc == 1) {
        VALIDATE_VALUE(argv[0], term_is_integer);
        entry_point = term_to_int(argv[0]);
    } else {
        entry_point = 0;
    }
    esp_err_t err = ulp_run(entry_point);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

static term nif_ulp_read_memory(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    avm_int_t offset = term_to_int(argv[0]);
    if (UNLIKELY(offset < 0 || offset * 4 >= CONFIG_ULP_COPROC_RESERVE_MEM)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t* words = (uint32_t*)RTC_SLOW_MEM;
    uint32_t value = words[offset];
    return term_make_maybe_boxed_int64(value, &ctx->heap);
}

static term nif_ulp_write_memory(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_any_integer);
    avm_int_t offset = term_from_int(argv[0]);
    if (UNLIKELY(offset < 0 || offset * 4 >= CONFIG_ULP_COPROC_RESERVE_MEM)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    // ESP32 ULP cannot read high 16 bits, but esp32s2 and esp32s3 can
    avm_int64_t value = term_maybe_unbox_int64(argv[1]);
    if (UNLIKELY(value < 0 || value > UINT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t* words = (uint32_t*)RTC_SLOW_MEM;
    words[offset] = (uint32_t)value;

    return OK_ATOM;
}

static term nif_ulp_isr_register(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    // Create a resource and a monitor to unregister when the process is gone
    struct ULPISRHandler* handler_obj = enif_alloc_resource(ulp_isr_handler_resource_type, sizeof(struct ULPISRHandler));
    if (IS_NULL_PTR(handler_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    ErlNifEnv* env = erl_nif_env_from_context(ctx);
    if (UNLIKELY(enif_monitor_process(env, handler_obj, &ctx->process_id, &handler_obj->registered_process_monitor) != 0)) {
        handler_obj->registered_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }
    handler_obj->registered_process_id = ctx->process_id;
    // Actually register the callback
    esp_err_t err = ulp_isr_register(ulp_isr_handler, handler_obj);
    if (err != ESP_OK) {
        handler_obj->registered_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE + TERM_BOXED_RESOURCE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term obj = term_from_resource(handler_obj, &ctx->heap);
    term obj_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(obj_tuple, 0, obj);
    term_put_tuple_element(obj_tuple, 1, term_from_ref_ticks(handler_obj->registered_process_monitor, &ctx->heap));
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, obj_tuple);

    return result;
}

static term nif_ulp_isr_deregister(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    void* handler_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ulp_isr_handler_resource_type, &handler_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct ULPISRHandler* handler_obj = (struct ULPISRHandler*)handler_obj_ptr;
    if (handler_obj->registered_process_id == INVALID_PROCESS_ID) {
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = ulp_isr_deregister(ulp_isr_handler, handler_obj);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    handler_obj->registered_process_id = INVALID_PROCESS_ID;

    return TRUE_ATOM;
}

static term nif_ulp_set_wakeup_period(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_any_integer);
    avm_int_t period_index = term_to_int(argv[0]);
    if (UNLIKELY(period_index < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int64_t period_us = term_maybe_unbox_int64(argv[1]);
    if (UNLIKELY(period_us < 0 || period_us > UINT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = ulp_set_wakeup_period((size_t)period_index, (uint32_t)period_us);
    if (UNLIKELY(err != ESP_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

static const struct Nif ulp_load_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_load_binary
};
static const struct Nif ulp_run_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_run
};
static const struct Nif ulp_read_memory_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_read_memory
};
static const struct Nif ulp_write_memory_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_write_memory
};
static const struct Nif ulp_isr_register_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_isr_register
};
static const struct Nif ulp_isr_deregister_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_isr_deregister
};
static const struct Nif ulp_set_wakeup_period_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ulp_set_wakeup_period
};

static void ulf_nif_init(GlobalContext* global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    ulp_isr_handler_resource_type = enif_init_resource_type(&env, "ulp_isr_handler", &ulp_isr_handler_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

static void ulf_nif_destroy(GlobalContext* global)
{
    UNUSED(global);
    resource_type_destroy(ulp_isr_handler_resource_type);
}

static const struct Nif* ulp_nif_get_nif(const char* nifname)
{
    if (strcmp("ulp:load_binary/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_load_binary_nif;
    }
    if (strcmp("ulp:load_binary/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_load_binary_nif;
    }
    if (strcmp("ulp:run/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_run_nif;
    }
    if (strcmp("ulp:run/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_run_nif;
    }
    if (strcmp("ulp:read_memory/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_read_memory_nif;
    }
    if (strcmp("ulp:write_memory/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_write_memory_nif;
    }
    if (strcmp("ulp:isr_register/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_isr_register_nif;
    }
    if (strcmp("ulp:isr_deregister/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_isr_deregister_nif;
    }
    if (strcmp("ulp:set_wakeup_period/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ulp_set_wakeup_period_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(ulp, ulf_nif_init, ulf_nif_destroy, ulp_nif_get_nif)

#endif
