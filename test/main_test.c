#include "../src/file_map.h"
#include "../src/job_queue.h"
#include "../src/mdg.h"
#include "../src/tauc.h"

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
static int pe(int res, char* msg)
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
mdg_node* mdg_node_create(mdg* m, string ident, mdg_node* parent, scope* tgt);
int mdg_test()
{
    int res = OK;
    mdg m;
    mdg_init(&m);
    mdg_node* a = mdg_node_create(&m, string_from_cstr("a"), NULL, NULL);
    mdg_node* b = mdg_node_create(&m, string_from_cstr("b"), NULL, NULL);
    mdg_node* c = mdg_node_create(&m, string_from_cstr("c"), NULL, NULL);

    mdg_node* d = mdg_node_create(&m, string_from_cstr("d"), NULL, NULL);
    mdg_node* e = mdg_node_create(&m, string_from_cstr("e"), NULL, NULL);
    mdg_node* f = mdg_node_create(&m, string_from_cstr("f"), NULL, NULL);
    mdg_node* g = mdg_node_create(&m, string_from_cstr("g"), NULL, NULL);
    mdg_add_dependency(&m, a, b);
    mdg_add_dependency(&m, b, c);
    mdg_add_dependency(&m, c, a);
    mdg_add_dependency(&m, c, d);
    mdg_add_dependency(&m, d, e);
    mdg_add_dependency(&m, e, f);
    mdg_add_dependency(&m, f, d);
    mdg_add_dependency(&m, g, a);
    mdg_add_dependency(&m, g, e);
    mdg_node_file_parsed(&m, g);
    mdg_group* group = a->group;
    if (!group) res = ERR;
    if (b->group != group) res = ERR;
    if (c->group != group) res = ERR;
    group = d->group;
    if (!g) res = ERR;
    if (e->group != group) res = ERR;
    if (f->group != group) res = ERR;
    mdg_fin(&m);
    return pe(res, "mdg_test");
}

// FILE MAP TESTS
int file_map_test()
{
    int res = OK;
    file_map fm;
    file_map_init(&fm);
    src_file* f =
        file_map_get_file_from_path(&fm, string_from_cstr("/foo/bar/baz.tau"));
    src_file* g =
        file_map_get_file_from_path(&fm, string_from_cstr("/foo/bar/baz.tau"));
    if (f != g) res = ERR;
    if (string_cmp_cstr(f->head.name, "baz.tau")) res = ERR;
    src_dir* p = f->head.parent;
    if (string_cmp_cstr(p->head.name, "bar")) res = ERR;
    src_dir* p2 = p->head.parent;
    if (string_cmp_cstr(p2->head.name, "foo")) res = ERR;
    if (file_map_get_dir(&fm, p2, string_from_cstr("bar")) != p) res = ERR;
    src_dir* p3 = p2->head.parent;
    if (string_cmp_cstr(p3->head.name, "")) res = ERR;
    file_map_fin(&fm);
    return pe(res, "file_map_test");
}
job* job_queue_push_raw(job_queue* jq);
int job_queue_test()
{
    int res = 0;
    job_queue jq;
    job_queue_init(&jq);
    job jb;
    for (int k = 0; k < 50; k++) {
        ureg p = 0;
        ureg q = 0;
        for (int i = 0; i < 20 * k; i++) {
            job_queue_result r = job_queue_request_parse(&jq, (void*)p);
            if (r != JQR_SUCCESS &&
                r != JQR_SUCCESS_WITH_REINFORCEMENTS_REQUEST) {
                res = ERR;
                goto fail;
            }
            p++;
            if (i % 5 == 0) {
                job_queue_pop(&jq, &jb);
                if (*(ureg*)&jb.concrete.parse.file != q) {
                    res = ERR;
                    goto fail;
                }
                q++;
            }
        }
        while (q < p) {
            job_queue_pop(&jq, &jb);
            if (*(ureg*)&jb.concrete.parse.file != q) {
                res = ERR;
                goto fail;
            }
            q++;
        }
    }
fail:
    job_queue_fin(&jq);
    return pe(res, "job_queue_test");
}

int main_release(int argc, char** argv);
int release_test()
{
    char* cli_args[2] = {"", "test/test.tau"};
    int r = main_release(2, cli_args);
    return pe(r, "release_test");
}

int main_test(int argc, char** argv)
{
    print_dash_padded("Executing Unit Tests", false);
    talloc_init();
    int res = OK;

    res |= mdg_test();
    res |= file_map_test();
    res |= job_queue_test();
    res |= release_test();

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