#include "hw7.h"
//helper
static const char* skip_sp(const char *s){//skip space
    while(*s && isspace((unsigned char)*s)) ++s;
    return s;
}

static matrix_sf* alloc_matrix(char name, unsigned int rows, unsigned int cols){
    matrix_sf *m = (matrix_sf*)malloc(sizeof(matrix_sf) + (size_t)rows*cols*sizeof(int));
    if (!m) return NULL;
    m->name = name;
    m->num_rows = rows;
    m->num_cols = cols;
    memset(m->values, 0, (unsigned int)rows*cols*sizeof(int));
    return m;
}

static int is_temp_name(char ch){
    return !isalpha((unsigned char)ch);
}

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL){
        bst_sf *node = (bst_sf*)malloc(sizeof(bst_sf));
        if(node == NULL){
           return NULL; 
        } 
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }
    if(mat->name < root->mat->name){
        root->left_child = insert_bst_sf(mat, root->left_child);
    }
    else{
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
     while(root != NULL){
        if(name == root->mat->name){
            return root->mat;
        }
        root = (name < root->mat->name) ? root->left_child : root->right_child;
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL){
        return;
    }
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

static void free_bst_keep(bst_sf *root, matrix_sf *keep) {
    if (!root) return;
    free_bst_keep(root->left_child, keep);
    free_bst_keep(root->right_child, keep);
    if (root->mat != keep) {
        free(root->mat);
    }
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int r = mat1->num_rows;
    unsigned int c = mat1->num_cols;
    matrix_sf *sum = alloc_matrix(0, r, c);
    if(sum == NULL){
        return NULL;
    } 
    unsigned int n = (unsigned int)r * c;
    for(unsigned int i = 0;i < n; i++){
        sum->values[i] = mat1->values[i] + mat2->values[i];
    }
    return sum;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned int m = mat1->num_rows;
    unsigned int n = mat1->num_cols;
    unsigned int p = mat2->num_cols;
    matrix_sf *product = alloc_matrix(0, m, p);
    if(product == NULL){
        return NULL;
    }
    for(unsigned int i = 0;i < m;i++){
        for(unsigned int k = 0;k < n;k++){
            int current = mat1->values[i*n + k];
            for(unsigned j = 0;j < p;j++){
                product->values[i*p + j] += current * mat2->values[k*p + j];
            }
        }
    }
    return product;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned int r = mat->num_rows;
    unsigned int c = mat->num_cols;
    matrix_sf *t = alloc_matrix(0, c, r);
    if(t == NULL){
        return NULL;
    }
    for(unsigned i = 0;i < r;i++){
        for(unsigned j = 0;j < c;j++){
            t->values[j*r + i] = mat->values[i*c + j];
        }
    }
    return t;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p = expr;
    p = skip_sp(p);
    unsigned rows = (unsigned)strtoul(p, (char**)&p, 10);
    p = skip_sp(p);
    unsigned cols = (unsigned)strtoul(p, (char**)&p, 10);
    p = skip_sp(p);
    if(*p != '['){
        return NULL;
    }
    p++;
    matrix_sf *m = alloc_matrix(name, rows, cols);
    if(m == NULL){
        return NULL;
    }

    for(unsigned int i = 0;i < rows;i++){
        for(unsigned int j = 0;j < cols;j++){
            p = skip_sp(p);
            long v = strtol(p, (char**)&p, 10);
            m->values[i*cols + j] = (int)v;
            p = skip_sp(p);
        }
        p = skip_sp(p);
        if(*p==';'){
            p++;
        }
        p = skip_sp(p);
    }

    if(*p == ']'){
        p++;
    }
    return m;
}
//stack for postfix
typedef struct {
    char *data;
    int top;
    int cap;
} CharStack;
//helper and method for stack
static void cs_init(CharStack *s, int cap){
    s->data = (char*)malloc((unsigned int)cap);
    s->top = -1; s->cap = cap;
}
static int  cs_empty(CharStack *s){
    return s->top < 0;
}
static void cs_push(CharStack *s, char ch){
    s->data[++s->top] = ch;
}
static char cs_pop(CharStack *s){
    return s->data[s->top--];
}
static char cs_peek(CharStack *s){
    return s->data[s->top];
}
static void cs_free(CharStack *s){
    free(s->data);
}

static int is_op(char ch){
    return (ch=='+' || ch=='*' || ch=='\'');
}
static int prec(char op){
    if(op=='\''){
        return 3;
    }
    if(op=='*'){
        return 2;
    }
    if(op=='+'){
        return 1;
    }
    return 0;
}

char* infix2postfix_sf(char *infix){
    unsigned int n = strlen(infix);
    char *out = (char*)malloc(n + 8);
    unsigned int oi = 0;

    CharStack st; cs_init(&st, (int)n + 8);

    for(unsigned int i = 0;i < n;){
        char ch = infix[i];
        if(isspace((unsigned char)ch)){
            i++;
            continue;
        }

        if(ch=='('){
            cs_push(&st, ch); 
            i++;
            continue;
        }

        if(ch==')'){
            while(!cs_empty(&st) && cs_peek(&st)!='('){
                out[oi++] = cs_pop(&st);
            }
            if(!cs_empty(&st) && cs_peek(&st)=='('){
               (void)cs_pop(&st);
            } 
            i++;
            continue;
        }

        if(isupper((unsigned char)ch)){
            out[oi++] = ch;
            i++;
            while(i < n && infix[i] == '\''){
                out[oi++] = '\'';
                i++;
            }
            continue;
        }

        if(is_op(ch)){
            while(!cs_empty(&st) && is_op(cs_peek(&st)) && prec(cs_peek(&st)) >= prec(ch)){
                out[oi++] = cs_pop(&st);
            }
            cs_push(&st, ch);
            i++;
            continue;
        }

        i++;
    }

    while(!cs_empty(&st)){
        char op = cs_pop(&st);
        if(op!='(' && op!=')'){
            out[oi++] = op;
        }
    }
    out[oi] = '\0';
    cs_free(&st);
    return out;
}

typedef struct { matrix_sf **data; int top, cap; } MatStack;
static void ms_init(MatStack *s, int cap){
    s->data = (matrix_sf**)malloc(sizeof(matrix_sf*) * (unsigned int)cap);
    s->top = -1;
    s->cap = cap;
}
static void ms_push(MatStack *s, matrix_sf *m){
    s->data[++s->top] = m;
}
static matrix_sf* ms_pop(MatStack *s){
    return s->data[s->top--];
}
static void ms_free(MatStack *s){
    free(s->data);
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root){
    char *post = infix2postfix_sf(expr);
    unsigned long pn = strlen(post);

    MatStack st; ms_init(&st, (int)pn + 8);

    for(unsigned long i = 0;i < pn;i++){
        char tok = post[i];

        if(isupper((unsigned char)tok)){
            matrix_sf *m = find_bst_sf(tok, root);
            ms_push(&st, m);
            continue;
        }

        if(tok=='\''){
            matrix_sf *a = ms_pop(&st);
            matrix_sf *t = transpose_mat_sf(a);
            if(is_temp_name(a->name)){
                free(a);
            }
            ms_push(&st, t);
            continue;
        }

        if(tok=='+' || tok=='*'){
            matrix_sf *b = ms_pop(&st);
            matrix_sf *a = ms_pop(&st);
            matrix_sf *r = (tok=='+') ? add_mats_sf(a,b) : mult_mats_sf(a,b);

            if(is_temp_name(a->name)){
                free(a);
            }
            if(is_temp_name(b->name)){
                free(b);
            }
            ms_push(&st, r);
            continue;
        }
    }

    matrix_sf *res = ms_pop(&st);
    ms_free(&st);
    free(post);

    res->name = name;
    return res;
}

matrix_sf* execute_script_sf(char *filename){
    FILE *fp = fopen(filename, "r");
    if(!fp) return NULL;

    bst_sf *root = NULL;
    char *line = NULL;
    unsigned long cap = MAX_LINE_LEN;
    unsigned long len;
    matrix_sf *last = NULL;

    while((len = getline(&line, &cap, fp)) != -1){
        
        char *p = line;
        while(*p && isspace((unsigned char)*p)){
            p++;
        }
        if(!*p){
            continue;
        }
        char lhs = *p++;
        
        while(*p && *p != '='){
            p++;
        }
        if(*p=='='){
            p++;
        }
        while(*p && isspace((unsigned char)*p)){
            p++;
        }
        if(strchr(p, '[')){
            matrix_sf *m = create_matrix_sf(lhs, p);
            root = insert_bst_sf(m, root);
            last = m;
        }else{
            matrix_sf *m = evaluate_expr_sf(lhs, p, root);
            root = insert_bst_sf(m, root);
            last = m;
        }
    }

    free(line);
    fclose(fp);

    free_bst_keep(root, last);
    return last;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}