#include "error_log.h"
#include "file_map.h"
#include "math.h"
#include "stdio.h"
#include "tauc.h"
#include "utils/math_utils.h"
#include "utils/zero.h"
#include <assert.h>
#include <unistd.h>

#define FAILURE_NONE ((error*)NULL)
#define FAILURE_FIRST ((error*)UREG_MAX)

#define ANSICOLOR_BLACK "\x1B[30m"
#define ANSICOLOR_RED "\x1B[31m"
#define ANSICOLOR_GREEN "\x1B[32m"
#define ANSICOLOR_YELLOW "\x1B[33m"
#define ANSICOLOR_BLUE "\x1B[34m"
#define ANSICOLOR_MAGENTA "\x1B[35m"
#define ANSICOLOR_CYAN "\x1B[36m"
#define ANSICOLOR_WHITE "\x1B[37m"
#define ANSICOLOR_BOLD "\x1B[1m"
#define ANSICOLOR_CLEAR "\x1B[0m"

#define pec(mel, color)                                                        \
    do {                                                                       \
        if (mel->err_tty) pe(color);                                           \
    } while (false)
#define pect(mel, color, text)                                                 \
    do {                                                                       \
        if (mel->err_tty)                                                      \
            pe(color text);                                                    \
        else                                                                   \
            pe(text);                                                          \
    } while (false)
#define pectc(mel, color, text, color2)                                        \
    do {                                                                       \
        if (mel->err_tty)                                                      \
            pe(color text color2);                                             \
        else                                                                   \
            pe(text);                                                          \
    } while (false)
#define pectct(mel, color, text, color2, text2)                                \
    do {                                                                       \
        if (mel->err_tty)                                                      \
            pe(color text color2 text2);                                       \
        else                                                                   \
            pe(text text2);                                                    \
    } while (false)

int master_error_log_init(master_error_log* mel, file_map* filemap)
{
    int r = aseglist_init(&mel->error_logs);
    if (r) return r;
    r = atomic_pool_init(&mel->error_pool);
    if (r) {
        aseglist_fin(&mel->error_logs);
        return r;
    }
    mel->filemap = filemap;
    mel->global_error_count = 0;
    mel->tab_size = 4; // TODO: make configurablef
    mel->tab_spaces = "    ";
    mel->err_tty = isatty(fileno(stderr));
    mel->max_err_line_length = 80;
    mel->sane_err_line_length = 80;
    mel->alloc_failiure = false;
    return OK;
}
void error_log_fin(error_log* el)
{
}
void master_error_log_fin(master_error_log* mel)
{
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &mel->error_logs);
    for (error_log* el = aseglist_iterator_next(&it); el;
         el = aseglist_iterator_next(&it)) {
        error_log_fin(el);
    }
    atomic_pool_fin(&mel->error_pool);
    aseglist_fin(&mel->error_logs);
}

void* master_error_log_alloc(master_error_log* mel, ureg size)
{
    void* e = atomic_pool_alloc(&mel->error_pool, size);
    if (!e) mel->alloc_failiure = true;
    return e;
}
void* error_log_alloc(error_log* el, ureg size)
{
    void* e = atomic_pool_alloc(&el->mel->error_pool, size);
    if (!e) error_log_report_allocation_failiure(el);
    return e;
}

void master_error_log_report(master_error_log* mel, char* critical_error)
{
    if (mel->global_error_count >= TAUC_MAX_GLOBAL_ERRORS - 1) {
        mel->global_errors[TAUC_MAX_GLOBAL_ERRORS - 1] =
            "global error log memory exhausted";
    }
    else {
        mel->global_errors[mel->global_error_count] = critical_error;
        mel->global_error_count++;
    }
}

error_log* error_log_create(master_error_log* mel)
{
    error_log* el = master_error_log_alloc(mel, sizeof(error_log));
    if (!el) return NULL;
    int r = aseglist_add(&mel->error_logs, el);
    if (r) return NULL;
    el->mel = mel;
    el->errors = NULL;
    el->critical_failiure_point = FAILURE_NONE;
    return el;
}

static inline void error_fill(
    error* e, error_stage stage, bool warn, error_kind type,
    const char* message, src_map* smap, ureg position)
{
    e->stage = stage;
    e->warn = warn;
    e->kind = type;
    e->smap = smap;
    e->position = position;
    e->message = message;
}

void error_log_report_simple(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg position)
{
    error* e = (error*)error_log_alloc(el, sizeof(error));
    if (!e) return;
    error_fill(e, stage, warn, ET_ERROR, message, smap, position);
    error_log_report(el, e);
}

error* error_log_create_error(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annot,
    ureg extra_annot_count)
{
    if (extra_annot_count == 0) {
        error_annotated* e =
            (error_annotated*)error_log_alloc(el, sizeof(error_annotated));
        if (!e) return NULL; // TODO: report this
        error_fill((error*)e, stage, warn, ET_1_ANNOT, message, smap, start);
        e->end = end;
        e->annotation = annot;
        return (error*)e;
    }
    error_multi_annotated* e = (error_multi_annotated*)error_log_alloc(
        el, sizeof(error_multi_annotated) +
                sizeof(error_annotation) * extra_annot_count);
    if (!e) return NULL; // TODO: report this
    error_fill(
        &e->err_annot.err, stage, warn, ET_MULTI_ANNOT, message, smap, start);
    e->annot_count = 0; // will increase with each annotation added
    e->err_annot.annotation = annot;
    e->err_annot.end = end;
    return (error*)e;
}
void error_add_annotation(
    error* e, src_map* smap, ureg start, ureg end, const char* message)
{
    if (!e) return; // save the user the hassle of checking for this
    assert(e->kind == ET_MULTI_ANNOT);
    error_multi_annotated* ema = (error_multi_annotated*)e;
    error_annotation* ea = (error_annotation*)ptradd(
        e, sizeof(error_multi_annotated) +
               sizeof(error_annotation) * ema->annot_count);
    ea->smap = smap;
    ea->start = start;
    ea->end = end;
    ea->annotation = message;
    ema->annot_count++;
}

void error_log_report_annotated(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation)
{
    error* e = error_log_create_error(
        el, stage, warn, message, smap, start, end, annotation, 0);
    error_log_report(el, e);
}
void error_log_report_annotated_twice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation, src_map* smap2,
    ureg start2, ureg end2, const char* annotation2)
{
    error* e = error_log_create_error(
        el, stage, warn, message, smap, start, end, annotation, 1);
    error_add_annotation(e, smap2, start2, end2, annotation2);
    error_log_report(el, e);
}
void error_log_report_annotated_thrice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_map* smap, ureg start, ureg end, const char* annotation, src_map* smap2,
    ureg start2, ureg end2, const char* annotation2, src_map* smap3,
    ureg start3, ureg end3, const char* annotation3)
{
    error* e = error_log_create_error(
        el, stage, warn, message, smap, start, end, annotation, 2);
    error_add_annotation(e, smap2, start2, end2, annotation2);
    error_add_annotation(e, smap3, start3, end3, annotation3);
    error_log_report(el, e);
}

void error_log_report(error_log* el, error* e)
{
    if (!e) return; // save the user the hassle of checking for this
    e->previous = el->errors;
    el->errors = e;
}
void error_log_report_critical_failiure(error_log* el, const char* msg)
{
    if (el->critical_failiure_point != FAILURE_NONE) return;
    if (el->errors != NULL) {
        el->critical_failiure_point = el->errors;
    }
    else {
        el->critical_failiure_point = FAILURE_FIRST;
    }
    el->critical_failiure_msg = msg;
}
void error_log_report_allocation_failiure(error_log* el)
{
    error_log_report_critical_failiure(el, "allocation failiure");
}
void error_log_report_synchronization_failiure(error_log* el)
{
    error_log_report_critical_failiure(el, "synchronization failiure");
}

static int pe(const char* msg)
{
    fputs(msg, stderr);
    return OK;
}
ureg get_line_nr_offset(ureg max_line)
{
    max_line++; // because lines are displayed starting from 1, not 0
    if (max_line >= 10) { // this is to avoid line zero
        return (ureg)floor(log10(max_line)) +
               1; //+1 because log(10, 10) is 1, not 2
    }
    return 1;
}

int print_filepath(
    master_error_log* mel, ureg line_nr_offset, src_pos pos, src_map* smap)
{
    for (ureg r = 0; r < line_nr_offset; r++) pe(" ");
    pectc(mel, ANSICOLOR_BLUE, "==> ", ANSICOLOR_CLEAR);
    src_map_print_path(smap, true);
    // TODO: the column index is currently based on the number of byte,
    // not the number of unicode code points, but tools expect the latter
    fprintf(
        stderr, ":%zu:%zu\n",
        pos.line + 1, // make indices start at one
        pos.column + 1);
    return OK;
}
#define LINE_BUFFER_SIZE 128
// TODO: somehow ensure this doesn't get overrun
#define ERR_POINT_BUFFER_SIZE 16
typedef struct err_point {
    src_map* smap;
    ureg line;
    ureg col_start;
    ureg col_end;
    sreg length_diff_start;
    sreg length_diff_end;
    const char* message;
    const char* text_color;
    const char* squigly_color;
    const char* message_color;
} err_point;
bool escape_char(char c)
{
    return (c > 126 || c < 32);
}
void print_until(
    master_error_log* mel, ureg* bpos, ureg* next, char* buffer,
    ureg* after_tab, sreg* length_diff)
{
    while (*bpos < *next) {
        unsigned char curr = (unsigned char)buffer[*bpos];
        if (escape_char(curr)) {
            if (bpos > after_tab) {
                buffer[*bpos] = '\0';
                pe(&buffer[*after_tab]);
            }
            if (curr < 16) {
                fprintf(stderr, "\\x0%X", (int)curr);
            }
            else {
                fprintf(stderr, "\\x%X", (int)curr);
            }
            *length_diff = *length_diff + 3;
            *after_tab = *bpos + 1;
        }
        else if (curr == '\t') {
            if (*bpos > *after_tab) {
                buffer[*bpos] = '\0';
                pe(&buffer[*after_tab]);
            }
            pe(mel->tab_spaces);
            *after_tab = *bpos + 1;
            *length_diff = *length_diff + (mel->tab_size - 1);
        }
        *bpos = *bpos + 1;
    }
    if (*next > *after_tab) {
        char temp = buffer[*next];
        buffer[*next] = '\0';
        pe(&buffer[*after_tab]);
        buffer[*next] = temp;
    }
}
void print_msg(master_error_log* mel, const char* msg, ureg msg_len)
{
    if (msg_len > mel->max_err_line_length) {
        for (ureg i = 0; i < mel->max_err_line_length - 4 - 3; i++) {
            fputc(msg[i], stderr);
        }
        pe("...");
    }
    else {
        pe(msg);
    }
}
void printCriticalThreadError(master_error_log* mel, const char* msg)
{
    pect(
        mel, ANSICOLOR_RED ANSICOLOR_BOLD, "critical error in worker thread: ");
    pe(msg);
    pect(mel, ANSICOLOR_CLEAR, "\n");
}
void printCriticalError(master_error_log* mel, const char* msg)
{
    pect(mel, ANSICOLOR_RED ANSICOLOR_BOLD, "critical error: ");
    pe(msg);
    pect(mel, ANSICOLOR_CLEAR, "\n");
}
void printFileIOError(master_error_log* mel, src_file* f)
{
    pectc(
        mel, ANSICOLOR_RED ANSICOLOR_BOLD,
        "reporting error: ", ANSICOLOR_CLEAR);
    pe("file IO error prevents giving error context in '");
    file_map_head_print_path(&f->head, true);
    pe("'\n");
}
void printAllocationError(master_error_log* mel)
{
    printCriticalError(
        mel, "memory allocation failiure during error reporting");
}
#define IO_ERR STATUS_1
int open_src_map(master_error_log* mel, src_map* smap)
{
    // TODO: error handling
    if (src_map_is_opened(smap)) return OK;
    return src_map_open(smap);
}
// TODO: allow putting annotation above with  vvv/,,, error here or
// TODO: redo this mess
int print_src_line(
    master_error_log* mel, src_map* smap, ureg line, ureg max_line_length,
    err_point* ep_start, err_point* ep_end)
{
    while (ep_start && ep_end - 1 >= ep_start) {
        if ((ep_end - 1)->message == NULL) {
            ep_end--;
        }
        else {
            break;
        }
    }
    pec(mel, ANSICOLOR_BOLD ANSICOLOR_BLUE);
    fprintf(stderr, "%zu", line + 1);
    ureg space = max_line_length - get_line_nr_offset(line);
    for (ureg i = 0; i < space; i++) pe(" ");
    pe(" |");
    pec(mel, ANSICOLOR_CLEAR);
    static char buffer[LINE_BUFFER_SIZE];
    ureg start, length;
    src_pos_get_line_bounds(smap, line, &start, &length);
    bool has_newline = true;
    if (src_map_seek_set(smap, start)) return ERR;
    bool end_of_line = false;
    ureg pos = 0;
    err_point* ep_pos = ep_start;
    sreg length_diff = 0;
    bool past_whitespace = false;
    while (!end_of_line) {
        ureg end = 0;
        ureg buff_len;
        if (length != 0) {
            ureg tgt = length - 1;
            if (tgt > LINE_BUFFER_SIZE - 1) {
                tgt = LINE_BUFFER_SIZE - 1;
            }
            else {
                end_of_line = true;
            }
            int r = src_map_read(smap, tgt, &buff_len, buffer);
            if (r || buff_len != tgt) {
                pe("\n");
                return ERR;
            }
        }
        else {
            has_newline = false;
            ureg tgt = LINE_BUFFER_SIZE - 1;
            int r = src_map_read(smap, tgt, &buff_len, buffer);
            if (r) return ERR; // TODO: report
            if (buff_len == 0) {
                length = pos;
                break;
            }
            else {
                for (ureg i = 0; i < buff_len; i++) {
                    if (buffer[i] == '\n') {
                        buff_len = i;
                        length = pos + buff_len;
                        end_of_line = true;
                        break;
                    }
                }
                if (!end_of_line && buff_len != tgt) {
                    end_of_line = true;
                    length = pos + buff_len;
                }
            }
        }
        if (pos + buff_len + length_diff - ((!end_of_line) * 3) >
            mel->max_err_line_length) {
            buff_len = mel->max_err_line_length - pos;
            for (ureg i = 0; i < buff_len; i++) {
                if (escape_char(buffer[i])) buff_len -= 2;
            }
            length = pos + buff_len;
            end_of_line = true;
            buffer[buff_len - 3] = '.';
            buffer[buff_len - 2] = '.';
            buffer[buff_len - 1] = '.';
            end = length;
            //        ep_pos->col_end = length;
        }
        if (!past_whitespace) {
            while (pos < length && buffer[pos] == ' ') {
                pos++;
            }
            if (pos != length) {
                for (err_point* ep = ep_pos; ep != ep_end; ep++) {
                    if (ep->col_start < pos) {
                        ep->col_start = pos;
                    }
                    else {
                        break;
                    }
                }
                past_whitespace = true;
            }
            else {
                continue;
            }
        }
        ureg bpos = 0;
        if (!end) end = pos + buff_len;
        while (pos < end && ep_pos != ep_end) {
            int mode;
            ureg next;
            if (ep_pos->col_start >= pos) {
                if (ep_pos->col_start == pos) {
                    ep_pos->length_diff_start = length_diff;
                    pec(mel, ep_pos->squigly_color);
                    next = ep_pos->col_end;
                    mode = 1;
                }
                else {
                    next = ep_pos->col_start;
                    if (ep_pos + 1 != ep_end &&
                        (ep_pos + 1)->col_start == ep_pos->col_start) {
                        mode = 3;
                    }
                    else {
                        mode = 0;
                    }
                }
            }
            else {
                if (ep_pos + 1 != ep_end &&
                    (ep_pos + 1)->col_start < ep_pos->col_end &&
                    next < (ep_pos + 1)->col_start) {
                    mode = 2;
                    next = (ep_pos + 1)->col_start;
                }
                else {
                    mode = 1;
                    next = ep_pos->col_end;
                }
            }
            if (end_of_line && next > length) next = length;
            ureg d = next - pos;
            if (d > buff_len - bpos) break;
            ureg after_tab = bpos;
            print_until(mel, &bpos, &next, buffer, &after_tab, &length_diff);
            switch (mode) {
                case 3: (ep_pos + 1)->length_diff_start = length_diff;
                // fallthrough
                case 0:
                    ep_pos->length_diff_start = length_diff;
                    pec(mel, ep_pos->squigly_color);
                    break;
                case 1:
                    ep_pos->length_diff_end = length_diff;
                    ep_pos++;
                    pec(mel, ANSICOLOR_CLEAR);
                    break;
                case 2:
                    (ep_pos + 1)->length_diff_start = length_diff;
                    pec(mel, (ep_pos + 1)->squigly_color);
                    break;
                default: assert(false);
            }
            pos = next;
        }
        if (end == pos) continue;
        ureg after_tab = bpos;
        print_until(mel, &bpos, &buff_len, buffer, &after_tab, &length_diff);
        pos += buff_len;
    }
    if (ep_end == ep_start) {
        pe("\n");
        return OK;
    }
    ep_pos = ep_end - 1;

    ureg msg_len = strlen(ep_pos->message);
    if (ep_end != ep_start && msg_len > 0 &&
        ep_pos->col_end + has_newline == length &&
        length + length_diff + 4 + msg_len + 4 <= mel->sane_err_line_length) {
        pectc(mel, ANSICOLOR_BLUE ANSICOLOR_BOLD, " <- ", ANSICOLOR_CLEAR);
        pec(mel, ep_pos->message_color);
        pe(ep_pos->message);
        ep_pos++;
        pect(mel, ANSICOLOR_CLEAR, "\n");
        ep_end--;
    }
    else {
        pe("\n");
    }
    ep_pos = ep_start;
    while (ep_pos != ep_end) {
        if (ep_pos->col_end == ep_pos->col_start) {
            ep_pos++;
            continue;
        }
        for (ureg i = 0; i < max_line_length; i++) pe(" ");
        pectc(mel, ANSICOLOR_BOLD ANSICOLOR_BLUE, " |", ANSICOLOR_CLEAR);
        ureg space_before = ep_pos->col_start + ep_pos->length_diff_start;
        msg_len = strlen(ep_pos->message);
        if (space_before > mel->max_err_line_length) {
            if (msg_len < mel->max_err_line_length) {
                for (ureg i = 0; i < mel->max_err_line_length - msg_len - 5;
                     i++)
                    pe(" ");
            }
            pec(mel, ep_pos->message_color);
            print_msg(mel, ep_pos->message, msg_len);
            pec(mel, ANSICOLOR_CLEAR);
            pe("  ...^");
            pect(mel, ANSICOLOR_CLEAR, "\n");
            ep_pos++;
            continue;
        }
        ureg col = 0;
        while (true) {
            ureg squig_len =
                (ep_pos->col_end - ep_pos->col_start) +
                (ep_pos->length_diff_end - ep_pos->length_diff_start);
            bool msg_before = space_before > msg_len + 2;
            if (msg_before) {
                sreg blank_space = (sreg)space_before - msg_len;
                for (sreg i = 0; i < blank_space; i++) pe(" ");
                pec(mel, ep_pos->message_color);
                print_msg(mel, ep_pos->message, msg_len);
                pec(mel, ep_pos->squigly_color);
                if (blank_space > 0) {
                    for (ureg i = 0; i != squig_len; i++) pe("^");
                }
                else {
                    for (ureg i = ep_pos->col_start + blank_space;
                         i != ep_pos->col_end; i++) {
                        pe("^");
                    }
                }
                col = ep_pos->col_end + ep_pos->length_diff_end;
            }
            else {
                for (ureg i = 0; i < space_before; i++) pe(" ");
                pec(mel, ep_pos->squigly_color);
                for (ureg i = 0; i != squig_len; i++) pe("^");
                pect(mel, ANSICOLOR_CLEAR, " ");
                pec(mel, ep_pos->message_color);
                pe(ep_pos->message);
                col = ep_pos->col_end + ep_pos->length_diff_end + 1 + msg_len;
            }
            err_point* next = ep_pos + 1;
            if (next != ep_end) {
                if (col + 1 <= next->col_start + next->length_diff_start) {
                    space_before =
                        next->col_start + next->length_diff_start - col;
                    while (next != ep_end && next->col_end == next->col_start) {
                        next++;
                    }
                    ep_pos = next;
                    if (ep_pos != ep_end) continue;
                }
            }
            ep_pos = next;
            pect(mel, ANSICOLOR_CLEAR, "\n");
            break;
        }
    }
    return OK;
}
static src_map* error_main_smap = NULL;
int cmp_err_point(const err_point* l, const err_point* r)
{
    if (l->smap != r->smap) {
        if (l->smap == error_main_smap) return -1;
        if (r->smap == error_main_smap) return 1;
        return ((ureg)l->smap > (ureg)r->smap) ? 1 : -1;
    }
    if (l->line != r->line) return (l->line > r->line) ? 1 : -1;
    if (r->message == NULL) return -1;
    if (l->message == NULL) return 1;
    return (l->col_start > r->col_start) ? 1 : -1;
}
#define SORT_NAME err_points
#define SORT_TYPE err_point
#define SORT_CMP(x, y) cmp_err_point(&(x), &(y))
#include "sort.h"
ureg extend_em(
    src_map* smap, err_point* err_points, const char* annot, src_pos pos,
    src_pos end)
{
    if (end.column == 0 && end.line != pos.line) {
        ureg lstart, llength;
        src_pos_get_line_bounds(smap, pos.line, &lstart, &llength);
        end.line--;
        end.column = llength - 1;
    }
    if (end.line == pos.line) {
        err_points[0].message = annot;
        err_points[0].col_end = end.column;
        return 1;
    }
    err_points[0].message = "";
    ureg lstart, llength;
    src_pos_get_line_bounds(smap, pos.line, &lstart, &llength);
    err_points[0].col_end = llength - 1;
    err_points[1].message_color = err_points[0].message_color;
    err_points[1].squigly_color = err_points[0].squigly_color;
    err_points[1].smap = err_points[0].smap;
    if (pos.line + 1 == end.line) {
        err_points[1].line = end.line;
        err_points[1].col_start = 0;
        err_points[1].col_end = end.column;
        err_points[1].message = annot;
        return 2;
    }
    if (pos.line + 2 == end.line) {
        src_pos_get_line_bounds(smap, pos.line + 1, &lstart, &llength);
        err_points[1].message = "";
        err_points[1].col_start = 0;
        err_points[1].col_end = llength - 1;
        err_points[1].line = pos.line + 1;
        src_pos_get_line_bounds(smap, pos.line + 2, &lstart, &llength);
        err_points[2].message_color = err_points[0].message_color;
        err_points[2].squigly_color = err_points[0].squigly_color;
        err_points[2].col_start = 0;
        err_points[2].col_end = end.column;
        err_points[2].message = annot;
        err_points[2].line = end.line;
        err_points[2].smap = err_points[1].smap;
        return 3;
    }
    err_points[1].message = annot;
    err_points[1].col_start = 0;
    err_points[1].col_end = end.column;
    err_points[1].line = end.line;
    return 2;
}
int report_error(master_error_log* mel, error* e)
{
    static err_point err_points[ERR_POINT_BUFFER_SIZE];
    pec(mel, ANSICOLOR_BOLD);
    switch (e->stage) {
        case ES_TOKENIZER: pect(mel, ANSICOLOR_GREEN, "lexer "); break;
        case ES_PARSER: pect(mel, ANSICOLOR_CYAN, "parser "); break;
        case ES_RESOLVER: pect(mel, ANSICOLOR_MAGENTA, "resolver "); break;
        default: break;
    }
    if (e->warn) {
        pe("warning: ");
        pec(mel, ANSICOLOR_YELLOW);
    }
    else {
        pe("error: ");
        pec(mel, ANSICOLOR_RED);
    }
    pe(e->message);
    pect(mel, ANSICOLOR_CLEAR, "\n");
    if (!e->smap) return OK;

    int r = open_src_map(mel, e->smap);
    if (r == IO_ERR) return OK;
    if (r != OK) return ERR;

    ureg err_point_count;
    src_pos pos = src_map_get_pos(e->smap, e->position);

    // TODO: multiline errors
    switch (e->kind) {
        case ET_1_ANNOT: {
            error_annotated* ea = (error_annotated*)e;
            err_points[0].smap = e->smap;
            err_points[0].line = pos.line;
            err_points[0].col_start = pos.column;
            err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            src_pos end = src_map_get_pos(e->smap, ea->end);
            err_point_count =
                extend_em(e->smap, err_points, ea->annotation, pos, end);
        } break;
        case ET_MULTI_ANNOT: {
            // TODO: cap at index 3
            static char* msg_colors[] = {
                ANSICOLOR_BOLD ANSICOLOR_RED,
                ANSICOLOR_BOLD ANSICOLOR_MAGENTA,
                ANSICOLOR_BOLD ANSICOLOR_CYAN,
                ANSICOLOR_BOLD ANSICOLOR_GREEN,
                ANSICOLOR_BOLD ANSICOLOR_YELLOW,
            };
            static ureg msg_color_count = sizeof(msg_colors) / sizeof(char*);
            error_multi_annotated* ema = (error_multi_annotated*)e;
            err_points[0].line = pos.line;
            err_points[0].smap = e->smap;
            err_points[0].col_start = pos.column;
            err_points[0].message_color = msg_colors[0];
            err_points[0].squigly_color = msg_colors[0];
            src_pos end = src_map_get_pos(e->smap, ema->err_annot.end);
            err_point_count = extend_em(
                e->smap, err_points, ema->err_annot.annotation, pos, end);
            error_annotation* ea = (error_annotation*)(ema + 1);
            for (ureg i = 0; i < ema->annot_count; i++) {
                int r = open_src_map(mel, ea->smap);
                if (r == IO_ERR) return OK;
                if (r != OK) return ERR;
                src_pos posi = src_map_get_pos(ea->smap, ea->start);
                err_points[err_point_count].smap = ea->smap;
                err_points[err_point_count].line = posi.line;
                err_points[err_point_count].col_start = posi.column;
                err_points[err_point_count].col_end =
                    (posi.column + (ea->end - ea->start));
                err_points[err_point_count].message = ea->annotation;
                err_points[err_point_count].message_color =
                    (msg_colors[(i + 1) % msg_color_count]);
                err_points[err_point_count].squigly_color =
                    (msg_colors[(i + 1) % msg_color_count]);
                end = src_map_get_pos(ea->smap, ea->end);
                err_point_count += extend_em(
                    ea->smap, &err_points[err_point_count], ea->annotation,
                    posi, end);
                ea++;
            }
        } break;
        default: {
            if (print_filepath(mel, get_line_nr_offset(pos.line), pos, e->smap))
                return ERR;
            return OK;
        } break;
    }
    ureg max_line = err_points[0].line;
    for (ureg i = 1; i < err_point_count; i++) {
        if (err_points[i].line > max_line) max_line = err_points[i].line;
    }

    ureg line_nr_offset = get_line_nr_offset(max_line);
    if (print_filepath(mel, line_nr_offset, pos, e->smap)) return ERR;
    error_main_smap = e->smap;
    err_points_grail_sort(err_points, err_point_count);

    ureg start = 0;
    ureg i = 1;
    src_map* smap = e->smap;
    while (true) {
        ureg line = err_points[start].line;
        while (i < err_point_count && err_points[i].line == line &&
               err_points[i].smap == smap) {
            i++;
        }
        if (print_src_line(
                mel, smap, line, line_nr_offset, &err_points[start],
                &err_points[i]))
            return ERR;
        start = i;
        if (start == err_point_count) break;
        if (err_points[start].smap != smap) {
            smap = err_points[start].smap;
            src_pos sp;
            sp.line = err_points[start].line;
            sp.column = err_points[start].col_start;
            print_filepath(mel, 1, sp, smap);
        }
        else {
            if (err_points[start].line > line + 2) {
                pectct(
                    mel, ANSICOLOR_BOLD ANSICOLOR_BLUE, "...", ANSICOLOR_CLEAR,
                    "\n");
            }
            else if (err_points[start].line == line + 2) {
                if (print_src_line(
                        mel, smap, line + 1, line_nr_offset, NULL, NULL))
                    return ERR;
            }
        }
    }
    return OK;
}

int compare_errs(const error* a, const error* b)
{
    // sort by file and then by position in file
    // NULL file will be put at the end
    // this is a really hacky way of sorting by file,
    // abusing the pointer value as an ordering
    // TODO: find a better way to sort these, maybe by include order
    if (a->smap < b->smap) return (a->smap == NULL) ? 1 : -1;
    if (a->smap > b->smap) return (b->smap == NULL) ? -1 : 1;
    if (a->position > b->position) return 1;
    if (b->position > a->position) return -1;
    return 0;
}
#define SORT_NAME errors
#define SORT_TYPE error*
#define SORT_CMP(x, y) compare_errs(x, y)
#include "sort.h"
void master_error_log_unwind(master_error_log* mel)
{
    ureg err_count = 0;
    aseglist_iterator it;
    aseglist_iterator_begin(&it, &mel->error_logs);
    for (error_log* el = aseglist_iterator_next(&it); el != NULL;
         el = aseglist_iterator_next(&it)) {
        error* e = el->errors;
        while (e != NULL) {
            err_count++;
            e = e->previous;
        }
    }
    if (err_count != 0) {
        error** errors = (error**)tmalloc(err_count * sizeof(error*));
        if (errors != NULL) {
            // insert backwards to revert linked list order
            error** pos = errors + err_count - 1;
            aseglist_iterator_begin(&it, &mel->error_logs);
            for (error_log* el = aseglist_iterator_next(&it); el != NULL;
                 el = aseglist_iterator_next(&it)) {
                error* e = el->errors;
                while (e != NULL) {
                    *pos = e;
                    pos--;
                    e = e->previous;
                }
            }
            // stable, in place sorting
            errors_grail_sort(errors, err_count);
            for (error** e = errors; e != errors + err_count; e++) {
                if (report_error(mel, *e)) {
                    break;
                }
                if (e != errors + err_count - 1 ||
                    mel->global_error_count > 0 || errors == NULL) {
                    pe("\n");
                }
            }
            tfree(errors);
            file_map_iterator fmi;
            file_map_iterator_begin(&fmi, mel->filemap);
            while (true) {
                src_file* f = file_map_iterator_next_file(&fmi);
                if (!f) break;
                if (f->file_stream && f->file_stream != (void*)NULL_PTR_PTR) {
                    fclose(f->file_stream);
                }
            }
        }
        else {
            printAllocationError(mel);
        }
    }
    aseglist_iterator_begin(&it, &mel->error_logs);
    for (error_log* el = aseglist_iterator_next(&it); el != NULL;
         el = aseglist_iterator_next(&it)) {
        if (el->critical_failiure_point != FAILURE_NONE) {
            printCriticalThreadError(mel, el->critical_failiure_msg);
        }
    }
    for (ureg i = 0; i < mel->global_error_count; i++) {
        printCriticalError(mel, mel->global_errors[i]);
    }
}

bool error_log_sane_state(error_log* el)
{
    return (el->critical_failiure_point == FAILURE_NONE);
}

char* error_log_cat_strings_2(error_log* e, const char* s1, const char* s2)
{
    const char* strs[2];
    strs[0] = s1;
    strs[1] = s2;
    return error_log_cat_strings(e, 2, strs);
}
char* error_log_cat_strings_3(
    error_log* e, const char* s1, const char* s2, const char* s3)
{
    const char* strs[3];
    strs[0] = s1;
    strs[1] = s2;
    strs[2] = s3;
    return error_log_cat_strings(e, 3, strs);
}
char* error_log_cat_strings(error_log* e, ureg count, const char** strs)
{
    ureg len = 0;
    for (ureg i = 0; i < count; i++) {
        len += strlen(strs[i]);
    }
    // we align this to avoid misaligned error logs
    char* str =
        (char*)error_log_alloc(e, ceil_to_mult_of_pow_two(len + 1, REG_BYTES));
    if (!str) return NULL;
    char* d = str;
    for (ureg i = 0; i < count; i++) {
        ureg len = strlen(strs[i]);
        memcpy(d, strs[i], len);
        d += len;
    }
    *d = '\0';
    return str;
}
