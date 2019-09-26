#include "../src/file_map.h"
#include "../src/job_queue.h"
#include "../src/mdg.h"
#include "../src/tauc.h"
#include "../src/utils/stack.h"
#include "../src/utils/debug_utils.h"

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
        char* msg = STRINGIFY(test_name);                                      \
        int r;                                                                 \
        TIME(r = test_call; if (r) {                                           \
            tput(msg);                                                         \
            tput(" FAILED");                                                   \
        } else {                                                               \
            tput(msg);                                                         \
            tput(" passed ");                                                  \
        };);                                                                   \
        res |= r;                                                              \
        tflush();                                                              \
    } while (false)

// MDG TESTS
#define TEST(rest, test_name) TEST_IMPL(rest, test_name, test_name())
#define TEST_WITH_ARGS(rest, test_name, ...)                                   \
    TEST_IMPL(rest, test_name, test_name(__VA_ARGS__))

int mdg_test()
{
    /*
    if (tauc_init()) return ERR;
    int res = ERR;
    module_dependency_graph* m = &TAUC.mdg;
    thread_context* tc = &TAUC.main_thread_context;
    mdg_node* a =
        mdg_get_node(m, m->root_node, string_from_cstr("a"), MS_UNNEEDED);
    mdg_node* b =
        mdg_get_node(m, m->root_node, string_from_cstr("b"), MS_UNNEEDED);
    mdg_node* c =
        mdg_get_node(m, m->root_node, string_from_cstr("c"), MS_UNNEEDED);
    mdg_node* d =
        mdg_get_node(m, m->root_node, string_from_cstr("d"), MS_UNNEEDED);
    mdg_node* e =
        mdg_get_node(m, m->root_node, string_from_cstr("e"), MS_UNNEEDED);
    mdg_node* f =
        mdg_get_node(m, m->root_node, string_from_cstr("f"), MS_UNNEEDED);
    mdg_node* g =
        mdg_get_node(m, m->root_node, string_from_cstr("g"), MS_UNNEEDED);
    mdg_node_add_dependency(a, b, tc);
    mdg_node_add_dependency(b, c, tc);
    mdg_node_add_dependency(c, a, tc);
    mdg_node_add_dependency(c, d, tc);
    mdg_node_add_dependency(d, e, tc);
    mdg_node_add_dependency(e, f, tc);
    mdg_node_add_dependency(f, d, tc);
    mdg_node_add_dependency(g, a, tc);
    mdg_node_add_dependency(g, e, tc);
    a->stage = MS_AWAITING_DEPENDENCIES;
    b->stage = MS_AWAITING_DEPENDENCIES;
    c->stage = MS_AWAITING_DEPENDENCIES;
    d->stage = MS_AWAITING_DEPENDENCIES;
    e->stage = MS_AWAITING_DEPENDENCIES;
    f->stage = MS_AWAITING_DEPENDENCIES;
    g->stage = MS_AWAITING_DEPENDENCIES;
    if (scc_detector_run(tc, g)) goto err;
    if (scc_detector_run(tc, e)) goto err;
    if (scc_detector_run(tc, a)) goto err;
    job j;
    while (true) {
        int r = job_queue_try_pop(&TAUC.jobqueue, &j);
        if (r == JQ_NONE) break;
        if (r != OK) goto err;
        r = thread_context_do_job(&TAUC.main_thread_context, &j);
    }
    */
    /*
        B > C > D > F
         ^ v     ^ v
          A < G > E
              ^
    */
    // res = OK;
    // err:
    // tauc_fin();
    // return res;
    return OK;
}

// FILE MAP TESTS
int file_map_test()
{
    int res = ERR;
    file_map fm;
    if (file_map_init(&fm)) return ERR;
    src_file* f =
        file_map_get_file_from_path(&fm, string_from_cstr("/foo/bar/baz.tau"));
    src_file* g =
        file_map_get_file_from_path(&fm, string_from_cstr("/foo/bar/baz.tau"));
    if (f != g) goto err;
    if (string_cmp_cstr(f->head.name, "baz.tau")) goto err;
    src_dir* p = f->head.parent;
    if (string_cmp_cstr(p->head.name, "bar")) goto err;
    src_dir* p2 = p->head.parent;
    if (string_cmp_cstr(p2->head.name, "foo")) goto err;
    if (file_map_get_dir(&fm, p2, string_from_cstr("bar")) != p) goto err;
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
    if (stack_init(&st, &p)) {
        pool_fin(&p);
        return ERR;
    }
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
int release_test(int argc, char** argv)
{
    return tauc_run(argc, argv);
}
int llvmtest_main();
#include <utils/debug_utils.h>
int main_test(int argc, char** argv)
{
    print_dash_padded("Executing Unit Tests", false);
    talloc_init();
    int res = OK;

    // res |= TEST(llvmtest_main);
    TEST(res, stack_test);
    TEST(res, list_builder_test);
    TEST(res, file_map_test);
    TEST(res, job_queue_test);
    // res |= TEST(mdg_test);
    TEST_WITH_ARGS(res, release_test, argc, argv);

    if (res) {
        print_dash_padded("FAILED", false);
    }
    else {
        print_dash_padded("PASSED", false);
    }
    putchar('\n');
    debug_utils_free_res();
    talloc_fin();

    return 0;
}
