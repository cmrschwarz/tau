#include "error_log.h"
#include "math.h"
#include "stdio.h"
#include "tauc.h"
#include "utils/math_utils.h"
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
    MASTER_ERROR_LOG.max_err_line_length = 120;
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
    el->allocation_failure_point = FAILURE_NONE;
    el->synchronization_failure_point = FAILURE_NONE;
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
    error* e, error_stage stage, bool warn, error_type type, char* message,
    file* file, ureg position)
{
    e->stage = stage;
    e->warn = warn;
    e->type = type;
    e->file = file;
    e->position = position;
    e->message = message;
}
static inline void
error_fill_annot(error_annotation* ea, ureg start, ureg end, char* msg)
{
    ea->start = start;
    ea->end = end;
    ea->annotation = msg;
}

void error_log_report_simple(
    error_log* el, error_stage stage, bool warn, char* message, file* file,
    ureg position)
{
    error* e = (error*)error_log_alloc(el, sizeof(error));
    if (!e) return;
    error_fill(e, stage, warn, ET_ERROR, message, file, position);
    error_log_report(el, e);
}

void error_log_report_annotated(
    error_log* el, error_stage stage, bool warn, char* message, file* file,
    ureg start, ureg end, char* annotation)
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
    error_log* el, error_stage stage, bool warn, char* message, file* file,
    ureg start1, ureg end1, char* annotation1, ureg start2, ureg end2,
    char* annotation2)
{
    error_multi_annotated* e = (error_multi_annotated*)error_log_alloc(
        el, sizeof(error_multi_annotated) + sizeof(error_annotated));
    if (!e) return;
    error_fill(
        &e->err_annot.error, stage, warn, ET_MULTI_ANNOT, message, file,
        start1);
    e->annot_count = 1;
    e->err_annot.annotation = annotation1;
    e->err_annot.end = end1;
    error_fill_annot((error_annotation*)(e + 1), start2, end2, annotation2);
    error_log_report(el, (error*)e);
}
void error_log_report_annotated_thrice(
    error_log* el, error_stage stage, bool warn, char* message, file* file,
    ureg start1, ureg end1, char* annotation1, ureg start2, ureg end2,
    char* annotation2, ureg start3, ureg end3, char* annotation3)
{
    error_multi_annotated* e = (error_multi_annotated*)error_log_alloc(
        el, sizeof(error_multi_annotated) + 2 * sizeof(error_annotated));
    if (!e) return;
    error_fill(
        &e->err_annot.error, stage, warn, ET_MULTI_ANNOT, message, file,
        start1);
    e->annot_count = 2;
    e->err_annot.annotation = annotation1;
    e->err_annot.end = end1;
    error_annotation* ea = (error_annotation*)(e + 1);
    error_fill_annot(ea, start2, end2, annotation2);
    error_fill_annot(ea + 1, start3, end3, annotation3);
    error_log_report(el, (error*)e);
}

void error_log_report(error_log* el, error* e)
{
    e->previous = el->errors;
    el->errors = e;
}
void error_log_report_allocation_failiure(error_log* el)
{
    if (el->errors != NULL) {
        el->allocation_failure_point = el->errors;
    }
    else {
        el->allocation_failure_point = FAILURE_FIRST;
    }
}
void error_log_report_synchronization_failiure(error_log* el)
{
    if (el->errors != NULL) {
        el->synchronization_failure_point = el->errors;
    }
    else {
        el->synchronization_failure_point = FAILURE_FIRST;
    }
}

int pe(char* msg)
{
    fputs(msg, stderr);
    return OK;
}
ureg get_line_nr_offset(ureg max_line)
{
    if (max_line > 10) { // this is to avoid line zero
        return (ureg)ceil(log10(max_line));
    }
    else {
        return 1;
    }
}
int print_filepath(ureg line_nr_offset, src_pos pos, file* file)
{
    for (ureg r = 0; r < line_nr_offset; r++) pe(" ");
    pectc(ANSICOLOR_BLUE, "==>", ANSICOLOR_CLEAR);
    // TODO: the column index is currently based on the number of byte,
    // not the number of unicode code points, but tools expect the latter
    fprintf(
        stderr, " %s:%llu:%llu\n", file->path,
        pos.line + 1, // make indices start at one
        pos.column + 1);
    return OK;
}
#define LINE_BUFFER_SIZE 128
#define ERR_POINT_BUFFER_SIZE 8
typedef struct err_point {
    ureg line; // TODO: handle ranges stretching multiple lines
    ureg col_start;
    ureg col_end;
    sreg length_diff_start;
    sreg length_diff_end;
    char* message;
    char* text_color;
    char* squigly_color;
    char* message_color;
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
int print_src_line(
    FILE* fh, file* file, ureg line, ureg max_line_length, err_point* ep_start,
    err_point* ep_end)
{
    pec(ANSICOLOR_BOLD ANSICOLOR_BLUE);
    fprintf(stderr, "%llu", line + 1);
    ureg space = max_line_length - get_line_nr_offset(line + 1);
    for (ureg i = 0; i < space; i++) pe(" ");
    pe(" |");
    pec(ANSICOLOR_CLEAR);
    static char buffer[LINE_BUFFER_SIZE];
    ureg start, length;
    src_pos_get_line_bounds(&file->src_map, line, &start, &length);
    bool has_newline = true;
    if (fseek(fh, start, SEEK_SET)) return ERR;
    bool end_of_line = false;
    ureg pos = 0;
    err_point* ep_pos = ep_start;
    sreg length_diff = 0;
    while (!end_of_line) {
        ureg r;
        if (length != 0) {
            ureg tgt = length - 1;
            if (tgt > LINE_BUFFER_SIZE - 1) {
                tgt = LINE_BUFFER_SIZE - 1;
            }
            else {
                end_of_line = true;
            }
            r = fread(buffer, 1, tgt, fh);
            if (r != tgt) {
                pe("\n");
                return ERR;
            };
        }
        else {
            has_newline = false;
            ureg tgt = LINE_BUFFER_SIZE - 1;
            r = fread(buffer, 1, tgt, fh);
            if (r == 0) {
                length = pos;
                break;
            }
            else {
                for (ureg i = 0; i < r; i++) {
                    if (buffer[i] == '\n') {
                        r = i;
                        length = pos + r;
                        end_of_line = true;
                        break;
                    }
                }
                if (!end_of_line && r != tgt) {
                    end_of_line = true;
                    length = pos + r;
                }
            }
        }
        if (pos + r > MASTER_ERROR_LOG.max_err_line_length) {
            r = MASTER_ERROR_LOG.max_err_line_length - pos;
            length = pos + r;
            end_of_line = true;
            buffer[r - 3] = '.';
            buffer[r - 2] = '.';
            buffer[r - 1] = '.';
        }
        ureg bpos = 0;
        ureg end = pos + r;
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
            if (d > r - bpos) break;
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
        print_until(&bpos, &r, buffer, &after_tab, &length_diff);
        pos += r;
    }
    if (ep_end == ep_start) {
        pe("\n");
        return OK;
    }
    ep_pos = ep_end - 1;
    ureg msg_len = strlen(ep_pos->message);
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
        bool msg_before = space_before > msg_len + 2 ||
                          space_before > MASTER_ERROR_LOG.max_err_line_length;
        if (msg_before) {
            if (space_before > MASTER_ERROR_LOG.max_err_line_length) {
                if (msg_len < MASTER_ERROR_LOG.max_err_line_length) {
                    for (ureg i = 0;
                         i < MASTER_ERROR_LOG.max_err_line_length - msg_len - 5;
                         i++)
                        pe(" ");
                }
            }
            else {
                for (ureg i = 0; i < space_before - msg_len - 1; i++) pe(" ");
            }
            pec(ep_pos->message_color);
            if (msg_len > MASTER_ERROR_LOG.max_err_line_length) {
                for (ureg i = 0;
                     i < MASTER_ERROR_LOG.max_err_line_length - 4 - 3; i++) {
                    fputc(ep_pos->message[i], stderr);
                }
                pe("...");
            }
            else {
                pe(ep_pos->message);
            }
            if (space_before > MASTER_ERROR_LOG.max_err_line_length) {
                pe("  ...^");
                pect(ANSICOLOR_CLEAR, "\n");
                ep_pos++;
                continue;
            }
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
int cmp_err_point(err_point l, err_point r)
{
    if (l.line != r.line) return l.line - r.line;
    return l.col_start - r.col_start;
}
#define SORT_NAME err_points
#define SORT_TYPE err_point
#define SORT_CMP(x, y) cmp_err_point(x, y)
#include "sort.h"
ureg extend_em(
    error* e, err_point* err_points, char* annot, src_pos pos, src_pos end)
{
    bool end_on_start = (end.column == 0 && end.line != pos.line);
    if (end.line == pos.line + end_on_start) {
        err_points[0].message = annot;
        err_points[0].col_end = end.column;
        return 1;
    }
    else {
        err_points[0].message = "";
        ureg lstart, llength;
        src_pos_get_line_bounds(&e->file->src_map, pos.line, &lstart, &llength);
        err_points[0].col_end = llength - 1;
        if (pos.line + 1 + end_on_start == end.line) {
            err_points[1].message_color = err_points[0].message_color;
            err_points[1].squigly_color = err_points[0].squigly_color;
            err_points[1].line = end.line;
            err_points[1].col_start = 0;
            err_points[1].col_end = end.column;
            err_points[1].message = annot;
            return 2;
        }
        else if (pos.line + 2 + end_on_start == end.line) {
            src_pos_get_line_bounds(
                &e->file->src_map, pos.line + 1, &lstart, &llength);
            err_points[1].message = "";
            err_points[1].message_color = err_points[0].message_color;
            err_points[1].squigly_color = err_points[0].squigly_color;
            err_points[1].col_start = 0;
            err_points[1].col_end = llength - 1;
            err_points[1].line = pos.line + 1;
            src_pos_get_line_bounds(
                &e->file->src_map, pos.line + 2, &lstart, &llength);
            err_points[2].message_color = err_points[0].message_color;
            err_points[2].squigly_color = err_points[0].squigly_color;
            err_points[2].col_start = 0;
            err_points[2].col_end = end.column;
            err_points[2].message = annot;
            err_points[2].line = end.line;
            return 3;
        }
        else {
            err_points[1].message_color = err_points[0].message_color;
            err_points[1].squigly_color = err_points[0].squigly_color;
            err_points[1].message = annot;
            if (!end_on_start) {
                err_points[1].col_start = 0;
                err_points[1].col_end = end.column;
                err_points[1].line = end.line;
            }
            else {
                src_pos_get_line_bounds(
                    &e->file->src_map, end.line - 1, &lstart, &llength);
                err_points[1].col_start = 0;
                err_points[1].col_end = llength;
                err_points[1].line = end.line - 1;
            }

            return 2;
        }
    }
}
int report_error(error* e, FILE* fh, file* file)
{
    static err_point err_points[ERR_POINT_BUFFER_SIZE];
    pec(ANSICOLOR_BOLD);
    switch (e->stage) {
    case ES_TOKENIZER: pect(ANSICOLOR_GREEN, "tokenizer "); break;
    case ES_PARSER: pect(ANSICOLOR_CYAN, "parser "); break;
    case ES_TYPESETTER: pect(ANSICOLOR_MAGENTA, "typededucing "); break;
    default: break;
    }
    if (e->warn) {
        pectc(ANSICOLOR_YELLOW, "warning: ", ANSICOLOR_CLEAR);
    }
    else {
        pectc(ANSICOLOR_RED, "error: ", ANSICOLOR_CLEAR);
    }
    pe(e->message);
    pe("\n");

    if (fh != NULL) {
        ureg err_point_count = 2;
        src_pos pos = src_map_get_pos(&e->file->src_map, e->position);

        // TODO: multiline errors
        switch (e->type) {
        case ET_1_ANNOT: {
            error_annotated* ea = (error_annotated*)e;
            err_points[0].line = pos.line;
            err_points[0].col_start = pos.column;
            err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            src_pos end = src_map_get_pos(&e->file->src_map, ea->end);
            err_point_count =
                extend_em(e, err_points, ea->annotation, pos, end);
        } break;
        case ET_MULTI_ANNOT: {
            error_multi_annotated* ema = (error_multi_annotated*)e;
            err_points[0].line = pos.line;
            err_points[0].col_start = pos.column;
            err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
            src_pos end =
                src_map_get_pos(&e->file->src_map, ema->err_annot.end);
            err_point_count =
                extend_em(e, err_points, ema->err_annot.annotation, pos, end);
            error_annotation* ea = (error_annotation*)(ema + 1);
            for (ureg i = err_point_count; i < ema->annot_count + 1; i++) {
                src_pos posi = src_map_get_pos(&e->file->src_map, ea->start);
                err_points[err_point_count].line = posi.line;
                err_points[err_point_count].col_start = posi.column;
                err_points[err_point_count].col_end =
                    (posi.column + (ea->end - ea->start));
                err_points[err_point_count].message = ea->annotation;
                err_points[err_point_count].message_color =
                    (ANSICOLOR_BOLD ANSICOLOR_MAGENTA);
                err_points[err_point_count].squigly_color =
                    (ANSICOLOR_BOLD ANSICOLOR_MAGENTA);
                end = src_map_get_pos(&e->file->src_map, ea->end);
                err_point_count += extend_em(
                    e, &err_points[err_point_count], ea->annotation, posi, end);
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
        if (print_filepath(line_nr_offset, pos, file)) return ERR;

        err_points_grail_sort(err_points, err_point_count);

        ureg start = 0;
        ureg i = 1;
        while (start < err_point_count) {
            ureg line = err_points[start].line;
            while (i < err_point_count && err_points[i].line == line) i++;
            if (print_src_line(
                    fh, file, line, line_nr_offset, &err_points[start],
                    &err_points[i]))
                return ERR;
            if (start + 1 < err_point_count) {
                if (err_points[start + 1].line > line + 2) {
                    pectct(
                        ANSICOLOR_BOLD ANSICOLOR_BLUE, "...", ANSICOLOR_CLEAR,
                        "\n");
                }
                else if (err_points[start + 1].line == line + 2) {
                    if (print_src_line(
                            fh, file, line + 1, line_nr_offset, NULL, NULL))
                        return ERR;
                }
            }
            start = i;
        }
    }
    else if (file != NULL) {
        if (print_filepath(
                1, src_map_get_pos(&e->file->src_map, e->position), file))
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
int printCriticalError(char* msg)
{
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "critical error: ", ANSICOLOR_CLEAR);
    pe(msg);
    pe("\n");
    return OK;
}
int printFileIOError(char* filepath)
{
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "reporting error: ", ANSICOLOR_CLEAR);
    pe("file IO error prevents giving error context in '");
    pe(filepath);
    pe("'\n");
    return OK;
}
void master_error_log_unwind(pool* p)
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
    if (err_count == 0) return;
    error** errors = (error**)pool_alloc(p, err_count * sizeof(error*));
    if (errors != NULL) {
        // insert backwards revert link list order
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
        file* file = NULL;
        FILE* fh = NULL;
        for (error** e = errors; e != errors + err_count; e++) {
            if (file != (*e)->file) {
                if (fh) {
                    fclose(fh);
                    fh = NULL;
                }
                file = (*e)->file;
                if (file != NULL) {
                    fh = fopen(file->path, "r");
                    if (fh == NULL) {
                        printFileIOError(file->path);
                    }
                }
            }
            if (report_error(*e, fh, file)) {
                printFileIOError(file->path);
                if (fh) {
                    fclose(fh);
                    fh = NULL;
                }
            }
            if (e != errors + err_count - 1 ||
                MASTER_ERROR_LOG.global_error_count > 0 || errors == NULL) {
                pe("\n");
            }
        }
        if (fh) fclose(fh);
    }
    for (ureg i = 0; i < MASTER_ERROR_LOG.global_error_count; i++) {
        printCriticalError(MASTER_ERROR_LOG.global_errors[i]);
    }
    if (errors == NULL) {
        printCriticalError("memory allocation failiure during error reporting");
    }
}

bool error_log_sane_state(error_log* el)
{
    // PERF: maybe cache this
    return (
        el->allocation_failure_point == FAILURE_NONE &&
        el->synchronization_failure_point == FAILURE_NONE);
}

char* error_log_cat_strings_2(error_log* e, char* s1, char* s2)
{
    char* strs[2];
    strs[0] = s1;
    strs[1] = s2;
    return error_log_cat_strings(e, 2, strs);
}
char* error_log_cat_strings_3(error_log* e, char* s1, char* s2, char* s3)
{
    char* strs[3];
    strs[0] = s1;
    strs[1] = s2;
    strs[2] = s3;
    return error_log_cat_strings(e, 3, strs);
}
char* error_log_cat_strings(error_log* e, ureg count, char** strs)
{
    ureg len = 0;
    for (ureg i = 0; i < count; i++) {
        len += strlen(strs[i]);
    }
    char* str = (char*)error_log_alloc(e, align_size(len + 1, REG_BYTES));
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