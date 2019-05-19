#include "../src/file_map.h"
#include "../src/job_queue.h"
#include "../src/mdg.h"
#include "../src/tauc.h"
#include "../src/utils/stack.h"

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
#define TEST(test_name) print_result(test_name(), STRINGIFY(test_name))
static int print_result(int res, char* msg)
{
    if (res) {
        fputs(msg, stderr);
        fputs(" FAILED\n", stderr);
    }
    else {
        fputs(msg, stdout);
        fputs(" passed\n", stdout);
    }
    return res;
}

// MDG TESTS
int mdg_test()
{
    if (tauc_init()) return ERR;
    int res = ERR;
    mdg* m = &TAUC.mdg;
    thread_context* tc = &TAUC.main_thread_context;
    mdg_node* a = mdg_get_node(m, m->root_node, string_from_cstr("a"));
    mdg_node* b = mdg_get_node(m, m->root_node, string_from_cstr("b"));
    mdg_node* c = mdg_get_node(m, m->root_node, string_from_cstr("c"));
    mdg_node* d = mdg_get_node(m, m->root_node, string_from_cstr("d"));
    mdg_node* e = mdg_get_node(m, m->root_node, string_from_cstr("e"));
    mdg_node* f = mdg_get_node(m, m->root_node, string_from_cstr("f"));
    mdg_node* g = mdg_get_node(m, m->root_node, string_from_cstr("g"));
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
        int r = job_queue_try_pop(&TAUC.job_queue, &j);
        if (r == JQ_NONE) break;
        if (r != OK) goto err;
        r = thread_context_do_job(&TAUC.main_thread_context, &j);
    }
    /*
        B > C > D > F
         ^ v     ^ v
          A < G > E
              ^
    */
    res = OK;
err:
    tauc_fin();
    return res;
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
    ureg _1, _2;
    for (int k = 0; k < 50; k++) {
        ureg p = 0;
        ureg q = 0;
        for (int i = 0; i < 20 * k; i++) {
            jb.concrete.parse.file = (void*)p;
            if (job_queue_push(&jq, &jb, &_1, &_2)) goto err;
            p++;
            if (i % 5 == 0) {
                job_queue_pop(&jq, &jb);
                if (*(ureg*)&jb.concrete.parse.file != q) goto err;
                q++;
            }
        }
        while (q < p) {
            job_queue_pop(&jq, &jb);
            if (*(ureg*)&jb.concrete.parse.file != q) goto err;
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
int release_test()
{
    static char* cli_args[2] = {"", "test/test.tau"};
    int r = master_error_log_init();
    if (!r) {
        r = tauc_init();
        if (!r) {
            r = tauc_run(2, cli_args);
            master_error_log_unwind();
            tauc_fin();
        }
        else {
            master_error_log_unwind();
        }
        master_error_log_fin();
    }
    return r;
}

int main_test(int argc, char** argv)
{
    print_dash_padded("Executing Unit Tests", false);
    talloc_init();
    int res = OK;

    res |= TEST(stack_test);
    res |= TEST(list_builder_test);
    res |= TEST(file_map_test);
    res |= TEST(job_queue_test);
    // res |= TEST(mdg_test);
    res |= TEST(release_test);

    if (res) {
        print_dash_padded("FAILED", false);
    }
    else {
        print_dash_padded("PASSED", false);
    }
    putchar('\n');
    talloc_fin();
    return res;
    return 0;
}
