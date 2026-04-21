#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

// ===== Прототипы =====
void expr(const char* str);
void prf(void);
void pst(void);
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
void expr(const char* str) {
    // fprintf(stderr, "[DEBUG] expr() вызвана с: '%s'\n", str ? str : "NULL");
    if (!str || *str == '\0') {
        fprintf(stdout, "incorrect\n");
        // fprintf(stderr, "[DEBUG] Пустая строка, выход\n");
        return;
    }

    if (g_root) { freeTree(g_root); g_root = NULL; }

    input = (char*)str;
    // fprintf(stderr, "[DEBUG] Указатель установлен. Парсинг...\n");
    
    Node* result = parseExpression();
    
    skipSpaces();
    if (input && *input != '\0') {
        fprintf(stderr, "Предупреждение: нераспознанные символы после выражения: '%s'\n", input);
        fprintf(stdout, "incorrect\n");
    }

    g_root = result;
    printf("expr test\n");
    fflush(stdout); // Гарантируем мгновенный вывод в консоль
    // fprintf(stderr, "[DEBUG] expr() завершена успешно\n");
}

void prf(void){
    return;
}

void pst(void){
    return;
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
            fprintf(out, "incorrent\n");
            continue;
        }

        #define COPY_ARG(start_idx) \
        do { \
            const char* src = line + (start_idx); \
            size_t i = 0; \
            while (i < 1023 && src[i] != '\n' && src[i] != '\r' && src[i] != '\0') { \
                arg[i] = src[i]; \
                i++; \
            } \
            arg[i] = '\0'; \
        } while(0)

        if (strncmp(line, "parse expr", 10) == 0) {
            COPY_ARG(10);
            // fprintf(stderr, "[DEBUG] Команда 'parse expr' распознана. Аргумент: '%s'\n", arg);
            expr(arg);
        }
        else if (strncmp(line, "load_prf", 8) == 0 && (line[8] == ' ' || line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            prf();
        }
        else if (strncmp(line, "load_pst", 8) == 0 && (line[8] == ' ' || line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            pst();
        }
        else if (strncmp(line, "save_prf", 8) == 0 && (line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            save_prf();
        }
        else if (strncmp(line, "save_pst", 8) == 0 && (line[8] == '\n' || line[8] == '\r' || line[8] == '\0')) {
            save_pst();
        }
        else if (strncmp(line, "eval ", 5) == 0) {
            COPY_ARG(5);
            eval(arg);
        }
        else {
            fprintf(out, "incorrent\n");
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
    symCount = 0;
    memset(symTable, 0, sizeof(symTable));

    process_commands(input_file, output);

    if (g_root) { freeTree(g_root); g_root = NULL; }

    fclose(input_file);
    fclose(output);
    return 0;
}