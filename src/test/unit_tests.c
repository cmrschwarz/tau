#include "../file_map.h"
#include "../job_queue.h"
#include "../mdg.h"
#include "../tauc.h"
#include "../utils/stack.h"
#include "../utils/debug_utils.h"
#include "../utils/list.h"
#include "../utils/base64.h"
static void print_dash_padded(char* msg, bool err)
{
    ureg len = strlen(msg);
    ureg left = (80 - len) / 2;
    ureg right = 80 - len - left;
    for (ureg i = 0; i < left; i++) {
        fputc('-', err ? stderr : stdout);
    }
    fputs(msg, err ? stderr : stdout);
    for (ureg i = 0; i < right; i++) {
        fputc('-', err ? stderr : stdout);
    }
    fputc('\n', err ? stderr : stdout);
}

#define TEST_IMPL(res, test_name, test_call)                                   \
    do {                                                                       \
        char* msg_succ = STR_CAT("PASSED ", STRINGIFY(test_name));             \
        char* msg_fail = STR_CAT("FAILED ", STRINGIFY(test_name));             \
        int r;                                                                 \
        TIME_MSG_LN(r = test_call, tprintf("%s ", r ? msg_fail : msg_succ););  \
        res |= r;                                                              \
    } while (false)

// MDG TESTS
#define TEST(res, test_name) TEST_IMPL(res, test_name, test_name())
#define TEST_WITH_ARGS(res, test_name, ...)                                    \
    TEST_IMPL(res, test_name, test_name(__VA_ARGS__))

int mdg_test()
{
    tauc t;
    if (tauc_scaffolding_init(&t)) return ERR;
    if (tauc_core_init(&t)) {
        tauc_scaffolding_fin(&t);
        return ERR;
    }
    ureg cc = plattform_get_virt_core_count();
    // prevent spawning of worker threads
    platttform_override_virt_core_count(1);
    int res = ERR;
    module_dependency_graph* m = &t.mdg;
    thread_context* tc = &t.main_thread_context;
    mdg_node* a = mdg_get_node(
        m, m->root_node, string_from_cstr("a"), MS_AWAITING_DEPENDENCIES);
    mdg_node* b = mdg_get_node(
        m, m->root_node, string_from_cstr("b"), MS_AWAITING_DEPENDENCIES);
    mdg_node* c = mdg_get_node(
        m, m->root_node, string_from_cstr("c"), MS_AWAITING_DEPENDENCIES);
    mdg_node* d = mdg_get_node(
        m, m->root_node, string_from_cstr("d"), MS_AWAITING_DEPENDENCIES);
    mdg_node* e = mdg_get_node(
        m, m->root_node, string_from_cstr("e"), MS_AWAITING_DEPENDENCIES);
    mdg_node* f = mdg_get_node(
        m, m->root_node, string_from_cstr("f"), MS_AWAITING_DEPENDENCIES);
    mdg_node* g = mdg_get_node(
        m, m->root_node, string_from_cstr("g"), MS_AWAITING_DEPENDENCIES);
    mdg_node_add_dependency(a, b, tc);
    mdg_node_add_dependency(b, c, tc);
    mdg_node_add_dependency(c, a, tc);
    mdg_node_add_dependency(c, d, tc);
    mdg_node_add_dependency(d, e, tc);
    mdg_node_add_dependency(e, f, tc);
    mdg_node_add_dependency(f, d, tc);
    mdg_node_add_dependency(g, a, tc);
    mdg_node_add_dependency(g, e, tc);
    if (sccd_run(&tc->sccd, g, SCCD_NOTIFY_DEP_PARSED)) goto err;
    if (sccd_run(&tc->sccd, e, SCCD_NOTIFY_DEP_PARSED)) goto err;
    if (sccd_run(&tc->sccd, a, SCCD_NOTIFY_DEP_PARSED)) goto err;
    job j;
    ureg jid = 0;
    while (true) {
        int r = job_queue_try_pop(&t.jobqueue, &j);
        if (r == JQ_NONE) {
            if (jid != 3) goto err;
            break;
        }
        if (r != OK) goto err;
        // TODO: inspect
        switch (jid) {
            case 0:
            case 1:
            case 2: {
                if (j.kind != JOB_RESOLVE) goto err;
                const char* mods[3][3] = {
                    {"d", "e", "f"}, {"a", "b", "c"}, {"g", "", ""}};
                ureg mod_counts[] = {3, 3, 1};
                if (j.concrete.resolve.single_store) {
                    j.concrete.resolve.start = &j.concrete.resolve.single_store;
                    j.concrete.resolve.end = j.concrete.resolve.start + 1;
                }
                if (j.concrete.resolve.end !=
                    j.concrete.resolve.start + mod_counts[jid]) {
                    goto err;
                }

                for (ureg i = 0; i < mod_counts[jid]; i++) {
                    if (strcmp(
                            j.concrete.resolve.start[i]->name, mods[jid][i])) {
                        goto err;
                    }
                }
                r = mdg_nodes_resolved(
                    j.concrete.resolve.start, j.concrete.resolve.end,
                    &t.main_thread_context, false);
                if (!j.concrete.resolve.single_store) {
                    tfree(j.concrete.resolve.start);
                }
                if (r) goto err;

            } break;
        }
        jid++;
    }
    //
    //    B > C > D > E
    //     ^ v     ^ v
    //      A < G > F
    //          ^
    res = OK;
err:
    tauc_core_fin_no_run(&t);
    tauc_scaffolding_fin(&t);
    platttform_override_virt_core_count(cc);
    return res;
}

// FILE MAP TESTS
int file_map_test()
{
    int res = ERR;
    file_map fm;
    if (file_map_init(&fm)) return ERR;
    src_file* f = file_map_get_file_from_path(
        &fm, NULL, string_from_cstr("/foo/bar/baz.tau"));
    src_file* g = file_map_get_file_from_path(
        &fm, NULL, string_from_cstr("/foo/bar/baz.tau"));
    if (f != g) goto err;
    if (string_cmp_cstr(f->head.name, "baz.tau")) goto err;
    src_dir* p = f->head.parent;
    if (string_cmp_cstr(p->head.name, "bar")) goto err;
    src_dir* p2 = p->head.parent;
    if (string_cmp_cstr(p2->head.name, "foo")) goto err;
    if (file_map_get_dir_from_path(&fm, NULL, string_from_cstr("/foo/bar")) !=
        p)
        goto err;
    src_dir* p3 = p2->head.parent;
    if (string_cmp_cstr(p3->head.name, "")) goto err;
    res = OK;
err:
    file_map_fin(&fm);
    return res;
}
int stack_test()
{
    int res = ERR;
    ureg stack_test_size = 77;
    stack st;
    pool p;
    if (pool_init(&p)) return ERR;
    if (stack_init(&st, &p)) return ERR;
    for (ureg i = 0; i < stack_test_size; i++) {
        if (stack_push(&st, (void*)i)) goto err;
    }
    for (ureg i = stack_test_size - 1; i >= stack_test_size / 2; i--) {
        if ((ureg)stack_pop(&st) != i) goto err;
    }
    for (ureg i = stack_test_size / 2; i < stack_test_size; i++) {
        if (stack_push(&st, (void*)i)) goto err;
    }
    for (ureg i = stack_test_size - 1; i > 0; i--) {
        if ((ureg)stack_pop(&st) != i) goto err;
    }
    if ((ureg)stack_pop(&st) != 0) goto err;
    res = OK;
err:
    stack_fin(&st);
    pool_fin(&p);
    return res;
}
job* job_queue_push_raw(job_queue* jq);
int job_queue_test()
{
    int res = ERR;
    job_queue jq;
    if (job_queue_init(&jq)) return ERR;
    job jb;
    ureg waiters, jobs;
    for (int k = 0; k < 1; k++) {
        uregh p = 0;
        uregh q = 0;
        for (int i = 0; i < 20 * k; i++) {
            jb.concrete.parse.requiring_srange = (src_range)p;
            if (job_queue_push(&jq, &jb, &waiters, &jobs)) goto err;
            p++;
            if (i % 5 == 0) {
                job_queue_pop(&jq, &jb, false, 1);
                if ((uregh)jb.concrete.parse.requiring_srange != q) goto err;
                q++;
            }
        }
        while (q < p) {
            job_queue_pop(&jq, &jb, false, 1);
            if ((uregh)jb.concrete.parse.requiring_srange != q) goto err;
            q++;
        }
    }
    res = OK;
err:
    job_queue_fin(&jq);
    return res;
}
int list_builder_test()
{
    // TODO
    return OK;
}

int list_test()
{
    int res = ERR;
    list l;
    if (list_init(&l)) return ERR;
    pool mem;
    if (pool_init(&mem)) return ERR;
    for (int j = 0; j < 100; j++) {
        void** p = NULL;
        const ureg fill_count = 100 * j;
        for (ureg i = 1; i < fill_count; i++) {
            ureg ll = list_length(&l);
            if (ll != i - 1) {
                goto err;
            }
            p++;
            if (list_append(&l, &mem, p)) goto err;
        }
        void** i = NULL;
        list_it it;
        list_it_begin(&it, &l);
        for (void* v = list_it_next(&it, &l); v; v = list_it_next(&it, &l)) {
            i++;
            if (v != i) goto err;
        }
        if (i != p) goto err;
        list_clear(&l);
    }
    res = OK;
err:
    list_fin(&l, false);
    pool_fin(&mem);
    return res;
}

int list_remove_test()
{
    int res = ERR;
    list l;
    if (list_init(&l)) return ERR;
    pool mem;
    if (pool_init(&mem)) return ERR;
    for (ureg i = 1; i <= 100; i++) {
        if (list_append(&l, &mem, (void*)i)) goto err;
    }
    ureg sum = 0;
    list_it it;
    list_it_begin(&it, &l);
    for (void* v = list_it_next(&it, &l); v; v = list_it_next(&it, &l)) {
        sum += (ureg)v;
        if ((ureg)v % 2 == 0) list_remove_swap(&l, &it);
    }

    if (sum != 5050) goto err;
    sum = 0;
    list_it_begin(&it, &l);
    for (void* v = list_it_next(&it, &l); v; v = list_it_next(&it, &l)) {
        sum += (ureg)v;
        if ((ureg)v % 3 == 0) list_remove_swap(&l, &it);
    }
    if (sum != 2500) goto err;
    sum = 0;
    list_it_begin(&it, &l);
    for (void* v = list_it_next(&it, &l); v; v = list_it_next(&it, &l)) {
        sum += (ureg)v;
        list_remove_swap(&l, &it);
    }
    if (sum != 1633) goto err;
    if (!list_empty(&l)) goto err;
    res = OK;
err:
    list_fin(&l, false);
    pool_fin(&mem);
    return res;
}

int ptrlist_test()
{
    int res = ERR;
    ptrlist l;
    if (ptrlist_init(&l, 1)) return ERR;
    for (int j = 1; j < 100; j++) {
        void** p = NULL;
        const ureg fill_count = 100 * j;
        for (ureg i = 1; i < fill_count; i++) {
            if (sbuffer_get_used_size(&l) / sizeof(void*) != i - 1) {
                goto err;
            }
            p++;
            if (ptrlist_append(&l, p)) goto err;
        }
        void** i = NULL;
        pli it = pli_begin(&l);
        for (void* v = pli_next(&it); v; v = pli_next(&it)) {
            i++;
            if (v != i) goto err;
        }
        if (i != p) goto err;
        ptrlist_clear(&l);
    }
    res = OK;
err:
    ptrlist_fin(&l);
    return res;
}

int ptrlist_remove_test()
{
    int res = ERR;
    ptrlist l, l2;
    if (ptrlist_init(&l, 1)) return ERR;
    if (ptrlist_init(&l2, 3)) goto err;
    for (ureg i = 1; i <= 100; i++) {
        if (ptrlist_append(&l, (void*)i)) goto err;
    }
    ureg sum = 0;
    pli it = pli_begin(&l);
    for (void* v = pli_next(&it); v; v = pli_next(&it)) {
        ureg val = (ureg)v;
        sum += val;
        if (val % 2 == 0) {
            ptrlist_remove_prev(&l, &it);
        }
        if (val % 17 == 0) {
            sbuffer_steal_used(&l2, &l, true);
            ptrlist tmp = l;
            l = l2;
            l2 = tmp;
        }
    }
    ptrlist_fin(&l2);
    if (sum != 5050) goto err;
    sum = 0;
    it = pli_rbegin(&l);
    for (void* v = pli_prev(&it); v; v = pli_prev(&it)) {
        ureg val = (ureg)v;
        sum += val;
        if (val % 3 == 0) {
            ptrlist_remove_next(&l, &it);
        }
        if (val % 17 == 0) {
            sbuffer_steal_used(&l2, &l, false);
            ptrlist_fin(&l);
            l = l2;
        }
    }
    if (sum != 2500) goto err;
    sum = 0;
    it = pli_begin(&l);
    if (ptrlist_init(&l2, 17)) goto err;
    for (void* v = pli_next(&it); v; v = pli_next(&it)) {
        ureg val = (ureg)v;
        sum += val;
        ptrlist_remove_prev(&l, &it);
        if (val % 17 == 0) {
            sbuffer_take_and_invalidate(&l2, &l);
            l = l2;
            if (ptrlist_init(&l2, val / 7)) goto err;
        }
    }
    ptrlist_fin(&l2);
    if (sum != 1633) goto err;
    if (!ptrlist_is_empty(&l)) goto err;
    res = OK;
err:
    ptrlist_fin(&l);
    return res;
}

int base64_test()
{
    u8 res[16];
    if (decode_base64("a", 1, '+', '/', (u8*)&res) != UREG_MAX) return ERR;
    res[0] = '$';
    if (decode_base64((char*)res, 10, '+', '/', (u8*)&res) != UREG_MAX) {
        return ERR;
    }
#define BAS64_TEST_COUNT 4
    char* tests[BAS64_TEST_COUNT][2] = {{"%&)(&§$)2~\xFF", "JSYpKCbCpyQpMn7/"},
                                        {"", ""},
                                        {"a", "YQ" /*==*/},
                                        {"ab", "YWI" /*=*/}};

    for (int i = 0; i < BAS64_TEST_COUNT; i++) {
        u8* dec = (u8*)tests[i][0];
        ureg dec_len = strlen((char*)dec);
        char* enc = tests[i][1];
        ureg enc_len = strlen(enc);
        res[enc_len] = 0xFF;
        if (encode_base64(dec, dec_len, '+', '/', (char*)&res) != enc_len) {
            return ERR;
        }
        if (memcmp(res, enc, enc_len)) return ERR;
        if (res[enc_len] != 0xFF) return ERR;
        u8 dec_end = res[dec_len];
        if (decode_base64((char*)res, strlen(enc), '+', '/', res) != dec_len) {
            return ERR;
        }
        if (memcmp(res, dec, dec_len)) return ERR;
        if (res[dec_len] != dec_end) return ERR;
    }

    return OK;
}

#include <utils/debug_utils.h>
int run_unit_tests(int argc, char** argv)
{
    // print_dash_padded("Executing Unit Tests", false);
    UNUSED(print_dash_padded);
    int res = OK;

    TEST(res, stack_test);
    TEST(res, list_builder_test);
    TEST(res, file_map_test);
    TEST(res, job_queue_test);
    TEST(res, mdg_test);
    TEST(res, list_test);
    TEST(res, list_remove_test);
    TEST(res, ptrlist_test);
    TEST(res, ptrlist_remove_test);
    TEST(res, base64_test);
    /*
    if (res) {
        print_dash_padded("FAILED", false);
    }
    else {
        print_dash_padded("PASSED", false);
    }
    */
    debug_utils_free_res();
    return OK;
}
