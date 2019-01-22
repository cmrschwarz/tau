#include "error_log.h"
#include "tauc.h"
#include "stdio.h"
#include "utils/math_utils.h"
#include "math.h"
#include <assert.h>
#include <unistd.h>

#define FAILURE_NONE ((error*)NULL)
#define FAILURE_FIRST ((error*)UREG_MAX)

#define ANSICOLOR_BLACK   "\e[30m"
#define ANSICOLOR_RED     "\e[31m"
#define ANSICOLOR_GREEN   "\e[32m"
#define ANSICOLOR_YELLOW  "\e[33m"
#define ANSICOLOR_BLUE    "\e[34m"
#define ANSICOLOR_MAGENTA "\e[35m"
#define ANSICOLOR_CYAN    "\e[36m"
#define ANSICOLOR_WHITE   "\e[37m"
#define ANSICOLOR_BOLD    "\e[1m"
#define ANSICOLOR_CLEAR   "\e[0m"

#define pec(color) do{if(MASTER_ERROR_LOG.err_tty)pe(color);}while(false)
#define pect(color, text) do{if(MASTER_ERROR_LOG.err_tty)pe(color text); else pe(text);}while(false)
#define pectc(color, text, color2) do{if(MASTER_ERROR_LOG.err_tty)pe(color text color2); else pe(text);}while(false)
#define pectct(color, text, color2, text2) do{if(MASTER_ERROR_LOG.err_tty)pe(color text color2 text2); else pe(text text2);}while(false)
static master_error_log MASTER_ERROR_LOG;

int master_error_log_init(){
    MASTER_ERROR_LOG.global_error_count = 0;
    MASTER_ERROR_LOG.tab_size = 4; //TODO: make configurable
    MASTER_ERROR_LOG.tab_spaces = "    ";
    MASTER_ERROR_LOG.err_tty = isatty(fileno(stderr));
    return OK;
}
void master_error_log_fin(){
}

void master_error_log_report(char* critical_error){
    if(MASTER_ERROR_LOG.global_error_count >= TAUC_MAX_GLOBAL_ERRORS - 1){
        MASTER_ERROR_LOG.global_errors[TAUC_MAX_GLOBAL_ERRORS - 1] = "global error log memory exhausted";
    }
    else{
        MASTER_ERROR_LOG.global_errors[MASTER_ERROR_LOG.global_error_count] = critical_error;
        MASTER_ERROR_LOG.global_error_count++;
    }
}

void error_log_init(error_log* el, pool* error_mem_pool){
    el->next = MASTER_ERROR_LOG.error_logs;
    MASTER_ERROR_LOG.error_logs = el;
    el->errors = NULL;
    el->error_mem_pool = error_mem_pool;
    el->next = NULL;
    el->allocation_failure_point = FAILURE_NONE;
    el->synchronization_failure_point = FAILURE_NONE;
}
void error_log_fin(error_log* el){
}

error* error_log_alloc(error_log* el, ureg size){
    error* e =  (error*)pool_alloc(el->error_mem_pool, size);
    if(!e) error_log_report_allocation_failiure(el);
    return e;
}
void error_fill(
    error* e,
    error_stage stage,
    bool warn,
    error_type type,
    char* message,
    file* file,
    ureg position
){
    e->stage = stage;
    e->warn = warn;
    e->type = type;
    e->file = file;
    e->position = position;
    e->message = message;
}

void error_log_report_error(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position
){
    error* e = (error*)error_log_alloc(el, sizeof(error));
    if(!e) return;
    error_fill(e, stage, warn, ET_ERROR, message, file, position);
    error_log_report(el, e);
}
void error_log_report_error_1_annotation(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position,
    ureg length,
    char* annotation
){
    error_1_annotation* e = (error_1_annotation*)error_log_alloc(el, sizeof(error_1_annotation));
    if(!e) return;
    error_fill((error*)e, stage, warn, ET_1_ANNOT, message, file, position);
    e->length = length;
    e->annotation = annotation;
    error_log_report(el, (error*)e);
}
void error_log_report_error_2_annotations(
    error_log* el,
    error_stage stage,
    bool warn,
    char* message,
    file* file,
    ureg position,
    ureg length1,
    char* annotation1,
    ureg position2,
    ureg length2,
    char* annotation2
){
    error_2_annotations* e = (error_2_annotations*)error_log_alloc(el, sizeof(error_2_annotations));
    if(!e) return;
    error_fill((error*)e, stage, warn, ET_2_ANNOT, message, file, position);
    e->length1 = length1;
    e->annotation1 = annotation1;
    e->position2 = position2;
    e->length2 = length2;
    e->annotation2 = annotation2;
    error_log_report(el, (error*)e);
}

void error_log_report(error_log* el, error* e){
    e->previous = el->errors;
    el->errors = e;
}
void error_log_report_allocation_failiure(error_log* el){
    if(el->errors != NULL){
        el->allocation_failure_point = el->errors;
    }
    else{
        el->allocation_failure_point = FAILURE_FIRST;
    }
}
void error_log_report_synchronization_failiure(error_log* el){
    if(el->errors != NULL){
        el->synchronization_failure_point = el->errors;
    }
    else{
        el->synchronization_failure_point = FAILURE_FIRST;
    }
}

int pe(char* msg){
    fputs(msg, stderr);
}
ureg get_line_nr_offset(ureg max_line){
    if (max_line > 10){//this is to avoid line zero
        return (ureg)ceil(log10(max_line));
    }
    else{
        return 1; 
    }
}
int print_filepath(ureg line_nr_offset, src_pos pos, file* file){
    for(ureg r = 0; r < line_nr_offset; r++)pe(" ");
    pectc(ANSICOLOR_BLUE, "==>", ANSICOLOR_CLEAR); 
    fprintf(
        stderr,
        " %s:%llu:%llu\n",
        file->path,
        pos.line + 1, //make indices start at one
        pos.column +1
    );
    return OK;
}
#define LINE_BUFFER_SIZE 128
#define ERR_POINT_BUFFER_SIZE 8
typedef struct err_point{
    ureg line;
    ureg c_start;
    ureg c_end;
    ureg tabs_start;
    ureg tabs_end;
    char* message;
    char* text_color;
    char* squigly_color;
    char* message_color;
}err_point;
int print_src_line(FILE* fh, file* file, ureg line, ureg max_line_length, err_point* ep_start, err_point* ep_end){
    pec(ANSICOLOR_BOLD ANSICOLOR_BLUE);
    fprintf(stderr, "%llu", line);
    ureg space = max_line_length - get_line_nr_offset(line);
    for(ureg i = 0; i< space; i++)pe(" ");
    pe(" |");
    pec(ANSICOLOR_CLEAR);
    static char buffer[LINE_BUFFER_SIZE];
    ureg start, length;
    src_pos_get_line_bounds(&file->src_map, line, &start, &length);
    if(fseek(fh, start, SEEK_SET)) return ERR;
    bool end = false;
    ureg pos = 0;
    ureg tab_count = 0;
    err_point* ep_pos = ep_start;
    while(!end){
        ureg r;
        if(length != 0){
            ureg tgt = length - 1;
            if(tgt > LINE_BUFFER_SIZE - 1){
                tgt = LINE_BUFFER_SIZE - 1;
            }
            else{
                end = true;
            }
            r = fread(buffer, 1, tgt, fh);
            if(r != tgt){
                pe("\n");
                return ERR;
            };
        }
        else{
            r = fread(buffer, 1, LINE_BUFFER_SIZE - 1, fh);
            if(r == 0){
                length = pos;
                break;
            }
            else{
                for(ureg i = 0; i< r; i++){
                    if(buffer[i] == '\n'){
                        r = i;
                        length = pos + r;
                        end = true;
                        break;
                    }
                }
            }
        }
        if(ep_pos != ep_end){
            ureg end = pos + r;
            ureg bpos = 0;
            while(pos < end && ep_pos != ep_end){
                int  mode;
                ureg next;
                if (ep_pos->c_start > pos){
                    next = ep_pos->c_start;
                    if(ep_pos + 1 != ep_end && (ep_pos + 1)->c_start){
                        mode = 3;
                    }
                    else{
                        mode = 0;
                    }
                }
                else{
                    if(ep_pos + 1 != ep_end && (ep_pos + 1)->c_start < ep_pos->c_end && next < (ep_pos + 1)->c_start){
                        mode = 2;
                        next = (ep_pos + 1)->c_start;
                    }
                    else{
                        mode = 1;
                        next = ep_pos->c_end;
                    }
                }
                ureg d = next - pos;
                if(d >= r - bpos) break;
                ureg after_tab = bpos;
                while(bpos < next){
                    if(buffer[bpos] == '\t'){
                        if(bpos > after_tab){
                            buffer[bpos] = '\0';
                            pe(&buffer[after_tab]);
                        }
                        pe(MASTER_ERROR_LOG.tab_spaces);
                        after_tab = bpos + 1;
                        tab_count++;
                    }
                    bpos++;
                }
                if(next > after_tab){
                    char temp = buffer[next];
                    buffer[next] = '\0';
                    pe(&buffer[after_tab]);
                    buffer[next] = temp;
                }
                switch(mode){
                    case 3:
                        (ep_pos + 1)->tabs_start = tab_count;
                        //fallthrough
                    case 0:
                        ep_pos->tabs_start = tab_count;
                        pec(ep_pos->squigly_color);
                        break;
                    case 1: 
                        ep_pos->tabs_end = tab_count;
                        pec(ANSICOLOR_CLEAR);
                        ep_pos++;
                        break;
                    case 2:
                        (ep_pos + 1)->tabs_start = tab_count;
                        pec((ep_pos + 1)->squigly_color);
                        break;
                    default: assert(false);
                }
                pos = next;
            } 
            ureg after_tab = bpos;
            while(bpos < r){
                if(buffer[bpos] == '\t'){
                    if(bpos > after_tab){
                        buffer[bpos] = '\0';
                        pe(&buffer[after_tab]);
                    }
                    pe(MASTER_ERROR_LOG.tab_spaces);
                    after_tab = bpos + 1;
                    tab_count++;
                }
                bpos++;
            }
            if(r > after_tab){
                buffer[r] = '\0';
                pe(&buffer[after_tab]);
            }
        }
        else{
            pos += r;
            buffer[r] = '\0';
            pe(buffer);
        }
    }
    ep_pos = ep_start;
    if(ep_pos->c_start >= length && length + strlen(ep_pos->message) + 4 <= 80){
        pe(" ");
        pec(ep_pos->message_color);
        pe("<- ");
        pe(ep_pos->message);
        ep_pos++;
        pect(ANSICOLOR_CLEAR, "\n");
    }
    else{
        pe("\n");
    }
    while(ep_pos < ep_end){
        for(ureg i = 0; i< max_line_length; i++)pe(" ");
        pectc(ANSICOLOR_BOLD ANSICOLOR_BLUE, " |", ANSICOLOR_CLEAR);
        for(ureg i=0;i<ep_pos->tabs_start;i++) pe(MASTER_ERROR_LOG.tab_spaces);
        for(ureg i =0;i<ep_pos->c_start - ep_pos->tabs_start;i++)pe(" ");
        pec(ep_pos->squigly_color);
        for(ureg i=ep_pos->tabs_start;i<ep_pos->tabs_end;i++){
            //minus one because each tab is already represented as one character
            for(ureg j = 0; j < MASTER_ERROR_LOG.tab_size - 1; j++)pe("^");
        }
        for(ureg i =ep_pos->c_start;i<ep_pos->c_end;i++)pe("^");
        pe(" ");
        pec(ep_pos->message_color);
        pe(ep_pos->message);
        pect(ANSICOLOR_CLEAR, "\n");
        ep_pos++;
    }
    return OK;
}
int cmp_err_point(err_point l, err_point r){
    if(l.line != r.line) return l.line - r.line;
    return l.c_start - r.c_start;
}
#define SORT_NAME err_points
#define SORT_TYPE err_point
#define SORT_CMP(x, y)cmp_err_point(x, y)
#include "sort.h"
int report_error(error* e, FILE* fh, file* file){
    static err_point err_points[ERR_POINT_BUFFER_SIZE];
    pec(ANSICOLOR_BOLD);
    switch(e->stage){
        case ES_TOKENIZER: pect(ANSICOLOR_GREEN, "tokenizer "); break;
        case ES_PARSER: pect(ANSICOLOR_CYAN, "parser "); break;
        case ES_TYPESETTER: pect(ANSICOLOR_MAGENTA, "typededucing "); break;
        default: break;
    }
    if (e->warn){
        pectc(ANSICOLOR_YELLOW, "warning: ", ANSICOLOR_CLEAR);
    }
    else{
        pectc(ANSICOLOR_RED, "error: ", ANSICOLOR_CLEAR);
    }
    pe(e->message);
    pe("\n");
   
    if(fh != NULL){
        ureg err_point_count = 2;
        src_pos pos = src_map_get_pos(&e->file->src_map, e->position);
        switch(e->type){
            case ET_1_ANNOT:{
                error_1_annotation* e1annot = (error_1_annotation*)e;
                err_points[0].line = pos.line;
                err_points[0].c_start = pos.column;
                err_points[0].c_end = pos.column + e1annot->length;
                err_points[0].message = e1annot->annotation;
                err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                err_point_count = 1;
            }break;
            case ET_2_ANNOT:{
                error_2_annotations* e2annot = (error_2_annotations*)e;
                err_points[0].line = pos.line;
                err_points[0].c_start = pos.column;
                err_points[0].c_end = pos.column + e2annot->length1;
                err_points[0].message = e2annot->annotation1;
                err_points[0].message_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                err_points[0].squigly_color = ANSICOLOR_BOLD ANSICOLOR_RED;
                src_pos pos2 = src_map_get_pos(&e->file->src_map, e2annot->position2);
                err_points[1].line = pos2.line;
                err_points[1].c_start = pos2.column;
                err_points[1].c_end = pos2.column + + e2annot->length2;
                err_points[1].message = e2annot->annotation2;
                err_points[1].message_color = ANSICOLOR_BOLD ANSICOLOR_MAGENTA;
                err_points[1].squigly_color = ANSICOLOR_BOLD ANSICOLOR_MAGENTA;
                err_point_count = 2;
            }break;
            default: {
                if(print_filepath(get_line_nr_offset(pos.line), pos, e->file)) return ERR;
                return OK;
            }break;
        }
        ureg max_line = err_points[0].line;
        for(ureg i = 1; i < err_point_count; i++){
            if(err_points[i].line > max_line)max_line = err_points[i].line;
        }
      
        ureg line_nr_offset = get_line_nr_offset(max_line);
        if(print_filepath(line_nr_offset, pos, file)) return ERR;

        err_points_grail_sort(err_points, err_point_count);
       
        ureg start = 0;
        ureg i = 1;
        while(start < err_point_count){
            ureg line = err_points[start].line;
            while(i < err_point_count && err_points[i].line == line)i++;
            bool dot_dot_on_last = false;
            bool print_following = false;
            if(print_src_line(fh, file, line, line_nr_offset, &err_points[start], &err_points[i])) return ERR;
            if(start + 1 < err_point_count){
                if(err_points[start + 1].line > line + 2){
                    pectct(ANSICOLOR_BOLD ANSICOLOR_BLUE, "...", ANSICOLOR_CLEAR, "\n");
                }
                else if(err_points[start + 1].line == line + 2){
                    if(print_src_line(fh, file, line, line_nr_offset, NULL, NULL))return ERR;
                }
            }
            start = i;
        }
    }
    else if(file != NULL){
        if(print_filepath(1, src_map_get_pos(&e->file->src_map, e->position), file)) return ERR;
    }
    return OK;
}

int compare_errs(const error* a, const error* b){
    //sort by file and then by position in file
    //NULL file will be put at the end
    //this is a really hacky way of sorting by file, abusing the pointer value as an ordering
    //TODO: find a better way to sort these, maybe by include order
    if(a->file < b->file) return (a->file == NULL) ? 1 : -1;
    if(a->file > b->file) return (b->file == NULL) ? -1 : 1;
    if(a->position > b->position) return 1;
    if(b->position > a->position) return -1;
    return 0;
}
#define SORT_NAME errors
#define SORT_TYPE error*
#define SORT_CMP(x, y)compare_errs(x, y)
#include "sort.h"
int printCriticalError(char* msg){
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "critical error: ", ANSICOLOR_CLEAR);
    pe(msg);
    pe("\n");
    return OK;
}
int printFileIOError(char* filepath){
    pectc(ANSICOLOR_RED ANSICOLOR_BOLD, "reporting error: ", ANSICOLOR_CLEAR);
    pe("file IO error prevents giving error context in '");
    pe(filepath);
    pe("'\n");
    return OK;
}
void master_error_log_unwind(pool* p){
    ureg err_count = 0;
    error_log* el = MASTER_ERROR_LOG.error_logs;
    while(el != NULL){
        error* e = el->errors;
        while(e != NULL){
            err_count++;
            e = e->previous;
        }
        el = el->next;
    }
    if(err_count == 0)return;
    error** errors = (error**)pool_alloc(p, err_count * sizeof(error*));
    if(errors != NULL){
        //insert backwards revert link list order
        error** pos = errors + err_count - 1;
        el = MASTER_ERROR_LOG.error_logs;
        while(el != NULL){
            error* e = el->errors;
            while(e != NULL){
                *pos = e;
                pos--;
                e = e->previous;
            }
            el = el->next;
        }
        //stable, in place sorting
        errors_grail_sort(errors, err_count);
        file* file = NULL;
        FILE* fh = NULL;
        for(error** e = errors; e != errors + err_count; e++){
            if(file != (*e)->file){
                if(fh) {
                    fclose(fh);
                    fh = NULL;
                }
                file = (*e)->file;
                if(file != NULL){
                    fh = fopen(file->path, "r");
                    if(fh == NULL){
                        printFileIOError(file->path);
                    }
                }
            }
            if(report_error(*e, fh, file)){
                printFileIOError(file->path);
                if(fh) {
                    fclose(fh);
                    fh = NULL;
                }
            }
            if(e != errors + err_count - 1 ||
               MASTER_ERROR_LOG.global_error_count > 0 ||
               errors == NULL
            ){
                pe("\n");
            }
        }
        if(fh) fclose(fh);
    }
    for(ureg i = 0; i < MASTER_ERROR_LOG.global_error_count; i++){
        printCriticalError(MASTER_ERROR_LOG.global_errors[i]);
    }
    if(errors == NULL){
        printCriticalError("memory allocation failiure during error reporting");
    }
}
