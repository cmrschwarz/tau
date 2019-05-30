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

#define pec(color)                                                             \
    do {                                                                       \
        if (MASTER_ERROR_LOG.err_tty) pe(color);                               \
    } while (false)
#define pect(color, text)                                                      \
    do {                                                                       \
        if (MASTER_ERROR_LOG.err_tty)                                          \
            pe(color text);                                                    \
        else                                                                   \
            pe(text);                                                          \
    } while (false)
#define pectc(color, text, color2)                                             \
    do {                                                                       \
        if (MASTER_ERROR_LOG.err_tty)                                          \
            pe(color text color2);                                             \
        else                                                                   \
            pe(text);                                                          \
    } while (false)
#define pectct(color, text, color2, text2)                                     \
    do {                                                                       \
        if (MASTER_ERROR_LOG.err_tty)                                          \
            pe(color text color2 text2);                                       \
        else                                                                   \
            pe(text text2);                                                    \
    } while (false)
master_error_log MASTER_ERROR_LOG;

int master_error_log_init()
{
    MASTER_ERROR_LOG.global_error_count = 0;
    MASTER_ERROR_LOG.tab_size = 4; // TODO: make configurable
    MASTER_ERROR_LOG.tab_spaces = "    ";
    MASTER_ERROR_LOG.err_tty = isatty(fileno(stderr));
    MASTER_ERROR_LOG.max_err_line_length = 80;
    MASTER_ERROR_LOG.sane_err_line_length = 80;
    return OK;
}
void master_error_log_fin()
{
}

void master_error_log_report(char* critical_error)
{
    if (MASTER_ERROR_LOG.global_error_count >= TAUC_MAX_GLOBAL_ERRORS - 1) {
        MASTER_ERROR_LOG.global_errors[TAUC_MAX_GLOBAL_ERRORS - 1] =
            "global error log memory exhausted";
    }
    else {
        MASTER_ERROR_LOG.global_errors[MASTER_ERROR_LOG.global_error_count] =
            critical_error;
        MASTER_ERROR_LOG.global_error_count++;
    }
}

void error_log_init(error_log* el, pool* error_mem_pool)
{
    el->next = MASTER_ERROR_LOG.error_logs;
    MASTER_ERROR_LOG.error_logs = el;
    el->errors = NULL;
    el->error_mem_pool = error_mem_pool;
    el->next = NULL;
    el->critical_failiure_point = FAILURE_NONE;
}
void error_log_fin(error_log* el)
{
}

void* error_log_alloc(error_log* el, ureg size)
{
    void* e = pool_alloc(el->error_mem_pool, size);
    if (!e) error_log_report_allocation_failiure(el);
    return e;
}
static inline void error_fill(
    error* e, error_stage stage, bool warn, error_kind type,
    const char* message, src_file* file, ureg position)
{
    e->stage = stage;
    e->warn = warn;
    e->kind = type;
    e->file = file;
    e->position = position;
    e->message = message;
}
static inline void error_fill_annot(
    error_annotation* ea, src_file* file, ureg start, ureg end, const char* msg)
{
    ea->file = file;
    ea->start = start;
    ea->end = end;
    ea->annotation = msg;
}

void error_log_report_simple(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg position)
{
    error* e = (error*)error_log_alloc(el, sizeof(error));
    if (!e) return;
    error_fill(e, stage, warn, ET_ERROR, message, file, position);
    error_log_report(el, e);
}

void error_log_report_annotated(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation)
{
    error_annotated* e =
        (error_annotated*)error_log_alloc(el, sizeof(error_annotated));
    if (!e) return;
    error_fill((error*)e, stage, warn, ET_1_ANNOT, message, file, start);
    e->end = end;
    e->annotation = annotation;
    error_log_report(el, (error*)e);
}
void error_log_report_annotated_twice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation,
    src_file* file2, ureg start2, ureg end2, const char* annotation2)
{
    error_multi_annotated* e = (error_multi_annotated*)error_log_alloc(
        el, sizeof(error_multi_annotated) + sizeof(error_annotated));
    if (!e) return;
    error_fill(
        &e->err_annot.error, stage, warn, ET_MULTI_ANNOT, message, file, start);
    e->annot_count = 1;
    e->err_annot.annotation = annotation;
    e->err_annot.end = end;
    error_fill_annot(
        (error_annotation*)(e + 1), file2, start2, end2, annotation2);
    error_log_report(el, (error*)e);
}
void error_log_report_annotated_thrice(
    error_log* el, error_stage stage, bool warn, const char* message,
    src_file* file, ureg start, ureg end, const char* annotation,
    src_file* file2, ureg start2, ureg end2, const char* annotation2,
    src_file* file3, ureg start3, ureg end3, const char* annotation3)
{
    error_multi_annotated* e = (error_multi_annotated*)error_log_alloc(
        el, sizeof(error_multi_annotated) + 2 * sizeof(error_annotated));
    if (!e) return;
    error_fill(
        &e->err_annot.error, stage, warn, ET_MULTI_ANNOT, message, file, start);
    e->annot_count = 2;
    e->err_annot.annotation = annotation;
    e->err_annot.end = end;
    error_annotation* ea = (error_annotation*)(e + 1);
    error_fill_annot(ea, file2, start2, end2, annotation2);
    error_fill_annot(ea + 1, file3, start3, end3, annotation3);
    error_log_report(el, (error*)e);
}

void error_log_report(error_log* el, error* e)
{
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
    else {
        return 1;
    }
}
int print_filepath(ureg line_nr_offset, src_pos pos, src_file* file)
{
    for (ureg r = 0; r < line_nr_offset; r++) pe(" ");
    pectc(ANSICOLOR_BLUE, "==> ", ANSICOLOR_CLEAR);
    src_file_print_path(file, true);
    // TODO: the column index is currently based on the number of byte,
    // not the number of unicode code points, but tools expect the latter
    fprintf(
        stderr, ":%llu:%llu\n",
        pos.line + 1, // make indices start at one
        pos.column + 1);
    return OK;
}
#define LINE_BUFFER_SIZE 128
#define ERR_POINT_BUFFER_SIZE 8
typedef struct err_point {
    src_file* file;
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
void print_until(
    ureg* bpos, ureg* next, char* buffer, ureg* after_tab, sreg* length_diff)
{
    while (*bpos < *next) {
        unsigned char curr = (unsigned char)buffer[*bpos];
        if (curr > 126 || curr < 32) {
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
            pe(MASTER_ERROR_LOG.tab_spaces);
            *after_tab = *bpos + 1;
            *length_diff = *length_diff + (MASTER_ERROR_LOG.tab_size - 1);
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
void print_msg(const char* msg, ureg msg_len)
{
    if (msg_len > MASTER_ERROR_LOG.max_err_line_length) {
        for (ureg i = 0; i < MASTER_ERROR_LOG.max_err_line_length - 4 - 3;
             i++) {
            fputc(msg[i], stderr);
        }
        pe("...");
    }
    else {
        pe(msg);
    }
}
void printCriticalThreadError(const char* msg)
{
    pectc(
        ANSICOLOR_RED ANSICOLOR_BOLD, "critical error in worker thread: ",
        ANSICOLOR_CLEAR);
    pe(msg);
    pe("\n");
}
void printCriticalError(const char* msg)
{
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "critical error: ", ANSICOLOR_CLEAR);
    pe(msg);
    pe("\n");
}
void printFileIOError(src_file* f)
{
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "reporting error: ", ANSICOLOR_CLEAR);
    pe("file IO error prevents giving error context in '");
    src_file_print_path(f, true);
    pe("'\n");
}
void printAllocationError()
{
    printCriticalError("memory allocation failiure during error reporting");
}
#define IO_ERR STATUS_1
int open_src_file(src_file* file)
{
    char pathbuff[256];
    ureg pathlen = src_file_get_path_len(file);
    char* path;
    if (pathlen < 256) {
        src_file_write_path(file, pathbuff);
        path = pathbuff;
    }
    else {
        path = tmalloc(pathlen + 1);
        if (!path) {
            printAllocationError();
            return ERR;
        }
        src_file_write_path(file, pathbuff);
    }
    file->file_stream = fopen(path, "r");
    if (file->file_stream == NULL) {
        file->file_stream = (void*)NULL_PTR_PTR;
        printFileIOError(file);
        return IO_ERR;
    }
    return OK;
}
// TODO: allow putting annotation above with  vvv/,,, error here or
int print_src_line(
    src_file* file, ureg line, ureg max_line_length, err_point* ep_start,
    err_point* ep_end)
{
    while (ep_start && ep_end - 1 > ep_start) {
        if ((ep_end - 1)->message == NULL) {
            ep_end--;
        }
        else {
            break;
        }
    }
    pec(ANSICOLOR_BOLD ANSICOLOR_BLUE);
    fprintf(stderr, "%llu", line + 1);
    ureg space = max_line_length - get_line_nr_offset(line);
    for (ureg i = 0; i < space; i++) pe(" ");
    pe(" |");
    pec(ANSICOLOR_CLEAR);
    static char buffer[LINE_BUFFER_SIZE];
    ureg start, length;
    src_pos_get_line_bounds(&file->src_map, line, &start, &length);
    bool has_newline = true;
    if (fseek(file->file_stream, start, SEEK_SET)) return ERR;
    bool end_of_line = false;
    ureg pos = 0;
    err_point* ep_pos = ep_start;
    sreg length_diff = 0;
    bool past_whitespace = false;
    while (!end_of_line) {
        ureg buff_len;
        if (length != 0) {
            ureg tgt = length - 1;
            if (tgt > LINE_BUFFER_SIZE - 1) {
                tgt = LINE_BUFFER_SIZE - 1;
            }
            else {
                end_of_line = true;
            }
            buff_len = fread(buffer, 1, tgt, file->file_stream);
            if (buff_len != tgt) {
                pe("\n");
                return ERR;
            };
        }
        else {
            has_newline = false;
            ureg tgt = LINE_BUFFER_SIZE - 1;
            buff_len = fread(buffer, 1, tgt, file->file_stream);
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
        if (pos + buff_len - ((!end_of_line) * 3) >
            MASTER_ERROR_LOG.max_err_line_length) {
            buff_len = MASTER_ERROR_LOG.max_err_line_length - pos;
            length = pos + buff_len;
            end_of_line = true;
            buffer[buff_len - 3] = '.';
            buffer[buff_len - 2] = '.';
            buffer[buff_len - 1] = '.';
            ep_pos->col_end = length;
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
        ureg end = pos + buff_len;
        while (pos < end && ep_pos != ep_end) {
            int mode;
            ureg next;
            if (ep_pos->col_start >= pos) {
                if (ep_pos->col_start == pos) {
                    ep_pos->length_diff_start = length_diff;
                    pec(ep_pos->squigly_color);
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
            ureg d = next - pos;
            if (d > buff_len - bpos) break;
            ureg after_tab = bpos;
            print_until(&bpos, &next, buffer, &after_tab, &length_diff);
            switch (mode) {
                case 3:
                    (ep_pos + 1)->length_diff_start = length_diff;
                // fallthrough
                case 0:
                    ep_pos->length_diff_start = length_diff;
                    pec(ep_pos->squigly_color);
                    break;
                case 1:
                    ep_pos->length_diff_end = length_diff;
                    ep_pos++;
                    pec(ANSICOLOR_CLEAR);
                    break;
                case 2:
                    (ep_pos + 1)->length_diff_start = length_diff;
                    pec((ep_pos + 1)->squigly_color);
                    break;
                default: assert(false);
            }
            pos = next;
        }
        if (end == pos) continue;
        ureg after_tab = bpos;
        print_until(&bpos, &buff_len, buffer, &after_tab, &length_diff);
        pos += buff_len;
    }
    if (ep_end == ep_start) {
        pe("\n");
        return OK;
    }
    ep_pos = ep_end - 1;

    ureg msg_len = ep_pos->message ? strlen(ep_pos->message) : 0;
    if (ep_end != ep_start && msg_len > 0 &&
        ep_pos->col_end + has_newline == length &&
        length + length_diff + 4 + msg_len + 4 <=
            MASTER_ERROR_LOG.sane_err_line_length) {
        pectc(ANSICOLOR_BLUE ANSICOLOR_BOLD, " <- ", ANSICOLOR_CLEAR);
        pec(ep_pos->message_color);
        pe(ep_pos->message);
        ep_pos++;
        pect(ANSICOLOR_CLEAR, "\n");
        ep_end--;
    }
    else {
        pe("\n");
    }
    ep_pos = ep_start;
    while (ep_pos < ep_end) {
        if (ep_pos->col_end == ep_pos->col_start) {
            ep_pos++;
            continue;
        }
        for (ureg i = 0; i < max_line_length; i++) pe(" ");
        pectc(ANSICOLOR_BOLD ANSICOLOR_BLUE, " |", ANSICOLOR_CLEAR);
        ureg space_before = ep_pos->col_start + ep_pos->length_diff_start;
        msg_len = strlen(ep_pos->message);
        if (space_before > MASTER_ERROR_LOG.max_err_line_length) {
            if (msg_len < MASTER_ERROR_LOG.max_err_line_length) {
                for (ureg i = 0;
                     i < MASTER_ERROR_LOG.max_err_line_length - msg_len - 5;
                     i++)
                    pe(" ");
            }
            pec(ep_pos->message_color);
            print_msg(ep_pos->message, msg_len);
            pec(ANSICOLOR_CLEAR);
            pe("  ...^");
            pect(ANSICOLOR_CLEAR, "\n");
            ep_pos++;
            continue;
        }
        bool msg_before = space_before > msg_len + 2;
        if (msg_before) {
            for (ureg i = 0; i < space_before - msg_len - 1; i++) pe(" ");
            pec(ep_pos->message_color);
            print_msg(ep_pos->message, msg_len);
            pec(ANSICOLOR_CLEAR);
            pe(" ");
        }
        else {
            for (ureg i = 0; i < space_before; i++) pe(" ");
        }
        pec(ep_pos->squigly_color);
        for (ureg i = ep_pos->col_start + ep_pos->length_diff_start;
             i < ep_pos->col_end + ep_pos->length_diff_end; i++) {
            pe("^");
        }
        if (!msg_before) {
            pect(ANSICOLOR_CLEAR, " ");
            pec(ep_pos->message_color);
            pe(ep_pos->message);
        }
        pect(ANSICOLOR_CLEAR, "\n");
        ep_pos++;
    }
    return OK;
}
static src_file* error_main_file = NULL;
int cmp_err_point(const err_point* l, const err_point* r)
{
    if (l->file != r->file) {
        if (l->file == error_main_file) return -1;
        if (r->file == error_main_file) return 1;
        return ((ureg)l->file > (ureg)r->file) ? 1 : -1;
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
    src_file* file, err_point* err_points, const char* annot, src_pos pos,
    src_pos end)
{
    if (end.column == 0 && end.line != pos.line) {
        ureg lstart, llength;
        src_pos_get_line_bounds(&file->src_map, pos.line, &lstart, &llength);
        end.line--;
        end.column = llength - 1;
    }
    if (end.line == pos.line) {
        err_points[0].message = annot;
        err_points[0].col_end = end.column;
        return 1;
    }
    else {
        err_points[0].message = "";
        ureg lstart, llength;
        src_pos_get_line_bounds(&file->src_map, pos.line, &lstart, &llength);
        err_points[0].col_end = llength - 1;
        err_points[1].message_color = err_points[0].message_color;
        err_points[1].squigly_color = err_points[0].squigly_color;
        err_points[1].file = err_points[0].file;
        if (pos.line + 1 == end.line) {
            err_points[1].line = end.line;
            err_points[1].col_start = 0;
            err_points[1].col_end = end.column;
            err_points[1].message = annot;
            return 2;
        }
        else if (pos.line + 2 == end.line) {
            src_pos_get_line_bounds(
                &file->src_map, pos.line + 1, &lstart, &llength);
            err_points[1].message = "";
            err_points[1].col_start = 0;
            err_points[1].col_end = llength - 1;
            err_points[1].line = pos.line + 1;
            src_pos_get_line_bounds(
                &file->src_map, pos.line + 2, &lstart, &llength);
            err_points[2].message_color = err_points[0].message_color;
            err_points[2].squigly_color = err_points[0].squigly_color;
            err_points[2].col_start = 0;
            err_points[2].col_end = end.column;
            err_points[2].message = annot;
            err_points[2].line = end.line;
            err_points[2].file = err_points[1].file;
            return 3;
        }
        else {
            err_points[1].message = annot;
            err_points[1].col_start = 0;
            err_points[1].col_end = end.column;
            err_points[1].line = end.line;
            return 2;
        }
    }
}
int report_error(error* e)
{
    static err_point err_points[ERR_POINT_BUFFER_SIZE];
    pec(ANSICOLOR_BOLD);
    switch (e->stage) {
        case ES_TOKENIZER: pect(ANSICOLOR_GREEN, "tokenizer "); break;
        case ES_PARSER: pect(ANSICOLOR_CYAN, "parser "); break;
        case ES_RESOLVER: pect(ANSICOLOR_MAGENTA, "resolver "); break;
        default: break;
    }
    if (e->warn) {
        pect(ANSICOLOR_YELLOW, "warning: ");
    }
    else {
        pect(ANSICOLOR_RED, "error: ");
    }
    pe(e->message);
    pect(ANSICOLOR_CLEAR, "\n");
    if (!e->file) return OK;
    if (e->file->file_stream == NULL) {
        int r = open_src_file(e->file);
        if (r == IO_ERR) return OK;
        if (r != OK) return ERR;
    }
    if (e->file->file_stream != (void*)NULL_PTR_PTR) {
        ureg err_point_count;
        src_pos pos = src_map_get_pos(&e->file->src_map, e->position);

        // TODO: multiline errors
        switch (e->kind) {
            case ET_1_ANNOT: {
                error_annotated* ea = (error_annotated*)e;
                err_points[0].file = e->file;
                err_points[0].line = pos.line;
                err_points[0].col_start = pos.column;
                err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                src_pos end = src_map_get_pos(&e->file->src_map, ea->end);
                err_point_count =
                    extend_em(e->file, err_points, ea->annotation, pos, end);
            } break;
            case ET_MULTI_ANNOT: {
                // TODO: cap at index 3
                static char* msg_colors[] = {ANSICOLOR_BOLD ANSICOLOR_RED,
                                             ANSICOLOR_BOLD ANSICOLOR_MAGENTA,
                                             ANSICOLOR_BOLD ANSICOLOR_CYAN};
                error_multi_annotated* ema = (error_multi_annotated*)e;
                err_points[0].line = pos.line;
                err_points[0].file = e->file;
                err_points[0].col_start = pos.column;
                err_points[0].message_color = msg_colors[0];
                err_points[0].squigly_color = msg_colors[0];
                src_pos end =
                    src_map_get_pos(&e->file->src_map, ema->err_annot.end);
                err_point_count = extend_em(
                    e->file, err_points, ema->err_annot.annotation, pos, end);
                error_annotation* ea = (error_annotation*)(ema + 1);
                for (ureg i = 0; i < ema->annot_count; i++) {
                    if (ea->file->file_stream == (void*)NULL_PTR_PTR) return OK;
                    if (ea->file->file_stream == NULL) {
                        int r = open_src_file(ea->file);
                        if (r == IO_ERR) return OK;
                        if (r != OK) return ERR;
                    }
                    src_pos posi =
                        src_map_get_pos(&ea->file->src_map, ea->start);
                    err_points[err_point_count].file = ea->file;
                    err_points[err_point_count].line = posi.line;
                    err_points[err_point_count].col_start = posi.column;
                    err_points[err_point_count].col_end =
                        (posi.column + (ea->end - ea->start));
                    err_points[err_point_count].message = ea->annotation;
                    err_points[err_point_count].message_color =
                        (msg_colors[i + 1]);
                    err_points[err_point_count].squigly_color =
                        (msg_colors[i + 1]);
                    end = src_map_get_pos(&ea->file->src_map, ea->end);
                    err_point_count += extend_em(
                        ea->file, &err_points[err_point_count], ea->annotation,
                        posi, end);
                    ea++;
                }
            } break;
            default: {
                if (print_filepath(get_line_nr_offset(pos.line), pos, e->file))
                    return ERR;
                return OK;
            } break;
        }
        ureg max_line = err_points[0].line;
        for (ureg i = 1; i < err_point_count; i++) {
            if (err_points[i].line > max_line) max_line = err_points[i].line;
        }

        ureg line_nr_offset = get_line_nr_offset(max_line);
        if (print_filepath(line_nr_offset, pos, e->file)) return ERR;
        error_main_file = e->file;
        err_points_grail_sort(err_points, err_point_count);

        ureg start = 0;
        ureg i = 1;
        src_file* file = e->file;
        while (true) {
            ureg line = err_points[start].line;
            while (i < err_point_count && err_points[i].line == line &&
                   err_points[i].file == file) {
                i++;
            }
            if (print_src_line(
                    file, line, line_nr_offset, &err_points[start],
                    &err_points[i]))
                return ERR;
            start = i;
            if (start == err_point_count) break;
            if (err_points[start].file != file) {
                file = err_points[start].file;
                src_pos sp;
                sp.line = err_points[start].line;
                sp.column = err_points[start].col_start;
                print_filepath(1, sp, file);
            }
            else {
                if (err_points[start].line > line + 2) {
                    pectct(
                        ANSICOLOR_BOLD ANSICOLOR_BLUE, "...", ANSICOLOR_CLEAR,
                        "\n");
                }
                else if (err_points[start].line == line + 2) {
                    if (print_src_line(
                            file, line + 1, line_nr_offset, NULL, NULL))
                        return ERR;
                }
            }
        }
    }
    else {
        if (print_filepath(
                1, src_map_get_pos(&e->file->src_map, e->position), e->file))
            return ERR;
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
    if (a->file < b->file) return (a->file == NULL) ? 1 : -1;
    if (a->file > b->file) return (b->file == NULL) ? -1 : 1;
    if (a->position > b->position) return 1;
    if (b->position > a->position) return -1;
    return 0;
}
#define SORT_NAME errors
#define SORT_TYPE error*
#define SORT_CMP(x, y) compare_errs(x, y)
#include "sort.h"
void master_error_log_unwind()
{
    ureg err_count = 0;
    error_log* el = MASTER_ERROR_LOG.error_logs;
    while (el != NULL) {
        error* e = el->errors;
        while (e != NULL) {
            err_count++;
            e = e->previous;
        }
        el = el->next;
    }
    if (err_count != 0) {
        error** errors = (error**)tmalloc(err_count * sizeof(error*));
        if (errors != NULL) {
            // insert backwards to revert linked list order
            error** pos = errors + err_count - 1;
            el = MASTER_ERROR_LOG.error_logs;
            while (el != NULL) {
                error* e = el->errors;
                while (e != NULL) {
                    *pos = e;
                    pos--;
                    e = e->previous;
                }
                el = el->next;
            }
            // stable, in place sorting
            errors_grail_sort(errors, err_count);
            for (error** e = errors; e != errors + err_count; e++) {
                if (report_error(*e)) {
                    break;
                }
                if (e != errors + err_count - 1 ||
                    MASTER_ERROR_LOG.global_error_count > 0 || errors == NULL) {
                    pe("\n");
                }
            }
            tfree(errors);
            file_map_iterator fmi;
            file_map_iterator_begin(&fmi, &TAUC.file_map);
            while (true) {
                src_file* f = file_map_iterator_next_file(&fmi);
                if (!f) break;
                if (f->file_stream && f->file_stream != (void*)NULL_PTR_PTR) {
                    fclose(f->file_stream);
                }
            }
        }
        else {
            printAllocationError();
        }
    }
    el = MASTER_ERROR_LOG.error_logs;
    while (el != NULL) {
        if (el->critical_failiure_point != FAILURE_NONE) {
            printCriticalThreadError(el->critical_failiure_msg);
        }
        el = el->next;
    }
    for (ureg i = 0; i < MASTER_ERROR_LOG.global_error_count; i++) {
        printCriticalError(MASTER_ERROR_LOG.global_errors[i]);
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
