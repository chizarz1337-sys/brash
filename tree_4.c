#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

// ===== Прототипы =====
void expr(const char* str, FILE* out);
void load_prf(const char* expr, FILE* out);
void load_pst(const char* expr, FILE* out);
void save_prf(void);
void save_pst(void);
void eval(const char* str);
void process_commands(FILE* in, FILE* out);

// ===== Структуры данных =====
typedef enum { NODE_OP, NODE_NUM, NODE_VAR } NodeType;

#define MAX_VARS 64
#define VAR_NAME_LEN 64
typedef struct { char name[VAR_NAME_LEN]; double value; int defined; } VarEntry;
VarEntry symTable[MAX_VARS];
int symCount = 0;

typedef struct Node {
    NodeType type;
    char op;            // '+', '-', '*', '/', '%', '@', '!'
    double value;       // Для чисел
    char var[VAR_NAME_LEN]; // Для переменных
    struct Node *left;
    struct Node *right;
} Node;

// ===== Глобальные переменные парсера =====
static char* input = NULL;  // Единственный указатель для парсинга
static Node* g_root = NULL;

// ===== Вспомогательные функции =====
void skipSpaces(void) {
    while (input && (*input == ' ' || *input == '\t')) {
        input++;
    }
}

// Forward-объявления парсера
Node* parseExpression(void);
Node* parseTerm(void);
Node* parseLCM(void);
Node* parseUnary(void);
Node* parseFactor(void);

// Функции работы с переменными и деревом
void setVar(const char* name, double value);
double getVar(const char* name);
void freeTree(Node* node);

Node* createNode(NodeType type, char op, double val, const char var[], Node* l, Node* r) {
    Node* node = (Node*)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Ошибка выделения памяти\n");
        exit(EXIT_FAILURE);
    }
    node->type = type;
    node->op = op;
    node->value = val;
    if (var && var[0] != '\0') {
        strncpy(node->var, var, VAR_NAME_LEN - 1);
        node->var[VAR_NAME_LEN - 1] = '\0';
    } else {
        node->var[0] = '\0';
    }
    node->left = l;
    node->right = r;
    return node;
}

// ===== Рекурсивный спуск =====
Node* parseExpression(void) {
    Node* left = parseTerm();
    skipSpaces();
    while (input && (*input == '+' || *input == '-')) {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseTerm());
        skipSpaces();
    }
    return left;
}

Node* parseTerm(void) {
    Node* left = parseLCM();
    skipSpaces();
    while (input && (*input == '*' || *input == '/' || *input == '%')) {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseLCM());
        skipSpaces();
    }
    return left;
}

Node* parseLCM(void) {
    Node* left = parseUnary();
    skipSpaces();
    while (input && *input == '@') {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseUnary());
        skipSpaces();
    }
    return left;
}

Node* parseUnary(void) {
    Node* node = parseFactor();
    skipSpaces();
    while (input && *input == '!') {
        char op = *input++;
        node = createNode(NODE_OP, op, 0, NULL, node, NULL);
        skipSpaces();
    }
    return node;
}

Node* parseFactor(void) {
    skipSpaces();
    if (!input) return NULL;

    // Скобки
    if (*input == '(') {
        input++;
        Node* node = parseExpression();
        skipSpaces();
        if (input && *input == ')') input++;
        return node;
    }

    // Переменная
    if (isalpha((unsigned char)*input) || *input == '_') {
        char varName[VAR_NAME_LEN];
        int i = 0;
        while (i < VAR_NAME_LEN - 1 && (isalnum((unsigned char)*input) || *input == '_')) {
            varName[i++] = *input++;
        }
        varName[i] = '\0';
        return createNode(NODE_VAR, 0, 0, varName, NULL, NULL);
    }

    // Число
    char* endptr;
    double val = strtod(input, &endptr);
    if (endptr == input) {
        fprintf(stderr, "Ошибка синтаксиса: ожидалось число, переменная или '(' в позиции: '%s'\n", input);
        exit(EXIT_FAILURE);
    }
    input = endptr;
    return createNode(NODE_NUM, 0, val, NULL, NULL, NULL);
}

// ===== Математические операции =====
long long gcd(long long a, long long b) {
    a = llabs(a); b = llabs(b);
    while (b) { long long t = b; b = a % b; a = t; }
    return a;
}

double calculateLCM(double a, double b) {
    if (a == 0 || b == 0) return 0;
    return fabs(a * b) / gcd((long long)a, (long long)b);
}

double calculateFactorial(double n) {
    if (n < 0) return 0;
    if (n != (int)n) return 0;
    if (n > 20) fprintf(stderr, "Предупреждение: факториал >20 может привести к переполнению\n");
    double res = 1;
    for (int i = 2; i <= (int)n; i++) res *= i;
    return res;
}

double calculate(struct Node* node) {
    if (!node) return 0;
    if (node->type == NODE_NUM) return node->value;
    if (node->type == NODE_VAR) return getVar(node->var);

    double l = calculate(node->left);
    double r = calculate(node->right);
    switch (node->op) {
        case '+': return l + r;
        case '-': return l - r;
        case '*': return l * r;
        case '/': return r == 0 ? 0 : l / r;
        case '%': return (double)((long long)l % (long long)r);
        case '@': return calculateLCM(l, r);
        case '!': return calculateFactorial(l);
        default: return 0;
    }
}

// ===== Работа с переменными =====
double getVar(const char* name) {
    if (!name || !*name) { fprintf(stderr, "Ошибка: пустое имя переменной\n"); return 0; }
    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            if (!symTable[i].defined) { fprintf(stderr, "Ошибка: переменная '%s' не имеет значения\n", name); return 0; }
            return symTable[i].value;
        }
    }
    fprintf(stderr, "Предупреждение: переменная '%s' не определена (используется 0)\n", name);
    return 0;
}

void setVar(const char* name, double value) {
    if (!name || !*name) return;
    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            symTable[i].value = value;
            symTable[i].defined = 1;
            return;
        }
    }
    if (symCount >= MAX_VARS) { fprintf(stderr, "Ошибка: лимит переменных исчерпан\n"); return; }
    strncpy(symTable[symCount].name, name, VAR_NAME_LEN - 1);
    symTable[symCount].name[VAR_NAME_LEN - 1] = '\0';
    symTable[symCount].value = value;
    symTable[symCount].defined = 1;
    symCount++;
}

void freeTree(Node* node) {
    if (!node) return;
    freeTree(node->left);
    freeTree(node->right);
    free(node);
}

// ===== Реализация команд =====
void expr(const char* str, FILE* out) {
    if (!str || *str == '\0') {
        fprintf(out, "incorrect\n");
        return;
    }
    
    // Освобождаем предыдущее дерево
    if (g_root != NULL) {
        freeTree(g_root);
        g_root = NULL;
    }
    
    // Инициализируем указатель парсинга
    input = (char*)str;
    
    // Пропускаем ведущие пробелы
    skipSpaces();
    
    // Проверяем, что строка не пустая после пробелов
    if (*input == '\0') {
        fprintf(out, "incorrect\n");
        return;
    }
    
    // Парсим выражение
    Node* result = parseExpression();
    
    // Проверяем, что всё распарсилось корректно
    skipSpaces();
    if (!result || (*input != '\0' && *input != '\n' && *input != '\r')) {
        fprintf(out, "incorrect\n");
        if (result) freeTree(result);
        g_root = NULL;
    } else {
        fprintf(out, "success\n");
        g_root = result;
    }
}

// === Префиксная форма (load_prf) ===
static Node* parse_prf_recursive(const char** ptr, int* error) {
    if (*error) return NULL;
    while (**ptr == ' ' || **ptr == '\t') (*ptr)++;
    if (**ptr == '\0' || **ptr == '\n' || **ptr == '\r') { *error = 1; return NULL; }

    char token[64];
    size_t i = 0;
    while (i < 63 && **ptr != ' ' && **ptr != '\t' && **ptr != '\0' && **ptr != '\n' && **ptr != '\r')
        token[i++] = *(*ptr)++;
    token[i] = '\0';

    if (i == 1 && strchr("+-*/%@!", token[0])) {
        char op = token[0];
        Node* node = createNode(NODE_OP, op, 0, NULL, NULL, NULL);
        if (op == '!') {
            node->left = parse_prf_recursive(ptr, error);
            node->right = NULL;
        } else {
            node->left = parse_prf_recursive(ptr, error);
            if (*error) { free(node); return NULL; }
            node->right = parse_prf_recursive(ptr, error);
        }
        if (*error || !node->left || (op != '!' && !node->right)) { *error = 1; free(node); return NULL; }
        return node;
    } else {
        char* endptr;
        double val = strtod(token, &endptr);
        return (*endptr == '\0') ? createNode(NODE_NUM, 0, val, NULL, NULL, NULL)
                                 : createNode(NODE_VAR, 0, 0, token, NULL, NULL);
    }
}

void load_prf(const char* expr, FILE* out) {
    if (!expr || *expr == '\0') { fprintf(out, "incorrect\n"); return; }
    if (g_root) { freeTree(g_root); g_root = NULL; }

    const char* ptr = expr;
    int error = 0;
    Node* tree = parse_prf_recursive(&ptr, &error);

    while (ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
    if (!error && *ptr != '\0' && *ptr != '\n' && *ptr != '\r') error = 1;

    if (error || !tree) {
        fprintf(out, "incorrect\n");
        if (tree) freeTree(tree);
        g_root = NULL;
    } else {
        fprintf(out, "success\n");
        g_root = tree;
    }
}

// === Постфиксная форма (load_pst) ===
void load_pst(const char* expr, FILE* out) {
    if (!expr || *expr == '\0') { fprintf(out, "incorrect\n"); return; }
    if (g_root) { freeTree(g_root); g_root = NULL; }

    Node* stack[1024];
    int top = -1;
    const char* ptr = expr;
    int error = 0;

    while (1) {
        while (*ptr == ' ' || *ptr == '\t') ptr++;
        if (*ptr == '\0' || *ptr == '\n' || *ptr == '\r') break;

        char token[64];
        size_t i = 0;
        while (i < 63 && *ptr != ' ' && *ptr != '\t' && *ptr != '\0' && *ptr != '\n' && *ptr != '\r')
            token[i++] = *ptr++;
        token[i] = '\0';

        if (i == 1 && strchr("+-*/%@!", token[0])) {
            char op = token[0];
            if (op == '!') {
                if (top < 0) { error = 1; break; }
                Node* left = stack[top--];
                stack[++top] = createNode(NODE_OP, op, 0, NULL, left, NULL);
            } else {
                if (top < 1) { error = 1; break; }
                Node* right = stack[top--];
                Node* left = stack[top--];
                stack[++top] = createNode(NODE_OP, op, 0, NULL, left, right);
            }
        } else {
            char* endptr;
            double val = strtod(token, &endptr);
            Node* node = (*endptr == '\0') ? createNode(NODE_NUM, 0, val, NULL, NULL, NULL)
                                           : createNode(NODE_VAR, 0, 0, token, NULL, NULL);
            if (top >= 1023) { error = 1; free(node); break; }
            stack[++top] = node;
        }
    }

    if (error || top != 0) {
        fprintf(out, "incorrect\n");
        for (int i = 0; i <= top; i++) freeTree(stack[i]);
        g_root = NULL;
    } else {
        fprintf(out, "success\n");
        g_root = stack[0];
    }
}

void save_prf(void){
    return;
}

void save_pst(void){
    return;
}

void eval(const char* str){
    return;
}

// ===== Обработчик команд =====
void process_commands(FILE* in, FILE* out) {
    char line[4096];
    char arg[1024];
    while (fgets(line, sizeof(line), in) != NULL) {
        // Пустые строки
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') continue;
        // Пробелы в начале
        if (isspace((unsigned char)line[0])) {
            fprintf(out, "incorrect\n");
            continue;
        }
        
#define COPY_ARG(start_idx) \
        do { \
            const char* src = line + (start_idx); \
            size_t i = 0; \
            while (i < 1023 && src[i] != '\n' && src[i] != '\r' && src[i] != '\0') { \
                arg[i] = src[i]; i++; \
            } \
            arg[i] = '\0'; \
        } while(0)

        if (strncmp(line, "parse expr", 10) == 0) {
            COPY_ARG(10);
            expr(arg, out);
        }
        else if (strncmp(line, "load_prf", 8) == 0 &&
                 (line[8] == ' ' || line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            COPY_ARG(8);  // <-- ДОБАВЛЕНО: копируем аргумент для load_prf
            load_prf(arg, out);
        }
        else if (strncmp(line, "load_pst", 8) == 0 &&
                 (line[8] == ' ' || line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            COPY_ARG(8);  // <-- ДОБАВЛЕНО: копируем аргумент для load_pst
            load_pst(arg, out);
        }
        else if (strncmp(line, "save_prf", 8) == 0 &&
                 (line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            save_prf();
        }
        else if (strncmp(line, "save_pst", 8) == 0 &&
                 (line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            save_pst();
        }
        else if (strncmp(line, "eval ", 5) == 0) {
            COPY_ARG(5);
            eval(arg);
        }
        else {
            fprintf(out, "incorrect\n");
        }
#undef COPY_ARG
    }
}

// ===== Main =====
int main(void) {
    FILE* input_file = fopen("input.txt", "r");
    if (!input_file) { printf("Error of open input.txt\n"); return 1; }

    FILE* output = fopen("output.txt", "w");
    if (!output) { printf("Error of open output.txt\n"); fclose(input_file); return 1; }

    input = NULL;
    g_root = NULL;
    memset(symTable, 0, sizeof(symTable));
    symCount = 0;

    process_commands(input_file, output);

    if (g_root) { freeTree(g_root); g_root = NULL; }
    fclose(input_file);
    fclose(output);
    return 0;
}