#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

// Forward функций команд
void expr(const char* str);
void prf(void);
void pst(void);
void save_prf(void);
void save_pst(void);
void eval(const char* str);

// Функция парсингакоманд
void process_commands(FILE* in, FILE* out);

typedef enum { NODE_OP, NODE_NUM, NODE_VAR } NodeType;

// Таблица переменных
#define MAX_VARS 64
#define VAR_NAME_LEN 64
typedef struct { char name[VAR_NAME_LEN]; double value; int defined; } VarEntry;
VarEntry symTable[MAX_VARS];
int symCount = 0;

typedef struct Node {
    NodeType type;
    char op;            // '+', '-', '*', '/', '%', '@', '!'
    double value;       // Для чисел
    char var[64];       // Для переменных
    struct Node *left;
    struct Node *right;
} Node;

// Глобальный указатель на текущую позицию в строке
char* input;

// Вспомогательная функция для пропуска пробелов
void skipSpaces() {
    while (*input && isspace((unsigned char)*input)) {
        input++;
    }
}

// Forward-объявления функций парсинга
Node* parseExpression();
Node* parseTerm();
Node* parseLCM();
Node* parseUnary();
Node* parseFactor();

// Forward функции для определения переменных
void setVar(const char* name, double value);
double getVar(const char* name);

// Функция создания узла
Node* createNode(NodeType type, char op, double val, char var[], Node* l, Node* r) {
    Node* node = (Node*)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Ошибка выделения памяти\n");
        exit(EXIT_FAILURE);
    }
    node->type = type;
    node->op = op;
    node->value = val;
    
    // Копируем имя переменной (если есть)
    if (var && var[0] != '\0') {
        strncpy(node->var, var, 63);
        node->var[63] = '\0';  // Гарантируем нуль-терминацию
    } else {
        node->var[0] = '\0';
    }
    
    node->left = l;
    node->right = r;
    return node;
}

// 5. Самый слабый приоритет: + и -
Node* parseExpression() {
    Node* left = parseTerm();
    skipSpaces();
    while (*input == '+' || *input == '-') {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseTerm());
        skipSpaces();
    }
    return left;
}

// 4. Приоритет: * / %
Node* parseTerm() {
    Node* left = parseLCM();
    skipSpaces();
    while (*input == '*' || *input == '/' || *input == '%') {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseLCM());
        skipSpaces();
    }
    return left;
}

// 3. Приоритет: @ (НОК)
Node* parseLCM() {
    Node* left = parseUnary();
    skipSpaces();
    while (*input == '@') {
        char op = *input++;
        left = createNode(NODE_OP, op, 0,  NULL, left, parseUnary());
        skipSpaces();
    }
    return left;
}

// 2. Приоритет: ! (Факториал)
Node* parseUnary() {
    Node* node = parseFactor();
    skipSpaces();
    while (*input == '!') {
        char op = *input++;
        node = createNode(NODE_OP, op, 0, NULL, node, NULL);
        skipSpaces();
    }
    return node;
}

// 1. Высший приоритет: скобки, числа и переменные
Node* parseFactor() {
    skipSpaces();
    
    // Обработка скобок
    if (*input == '(') {
        input++; // Пропускаем '('
        Node* node = parseExpression();
        skipSpaces();
        if (*input == ')') input++; // Пропускаем ')'
        return node;
    }
    
    // Проверка на переменную (идентификатор: буква или _ в начале)
    if (isalpha((unsigned char)*input) || *input == '_') {
        char varName[64];
        int i = 0;
        // Читаем имя переменной: буквы, цифры, подчёркивания
        while ((isalnum((unsigned char)*input) || *input == '_') && i < 63) {
            varName[i++] = *input++;
        }
        varName[i] = '\0';
        // Создаём узел типа NODE_VAR с именем переменной
        return createNode(NODE_VAR, 0, 0, varName, NULL, NULL);
    }
    
    // Читаем число
    char* endptr;
    double val = strtod(input, &endptr);
    if (endptr == input) {
        fprintf(stderr, "Ошибка синтаксиса: ожидалось число, переменная или '(' в позиции: %s\n", input);
        exit(EXIT_FAILURE);
    }
    input = endptr;
    return createNode(NODE_NUM, 0, val, NULL, NULL, NULL);
}

// Вспомогательная функция для НОД
long long gcd(long long a, long long b) {
    a = llabs(a); b = llabs(b);
    while (b) {
        long long t = b;
        b = a % b;
        a = t;
    }
    return a;
}

// Операция @: НОК(a, b) = |a*b| / НОД(a, b)
double calculateLCM(double a, double b) {
    if (a == 0 || b == 0) return 0;
    return fabs(a * b) / gcd((long long)a, (long long)b);
}

// Операция !: Факториал
double calculateFactorial(double n) {
    if (n < 0) return 0;
    if (n != (int)n) return 0; // Факториал определён только для целых чисел
    if (n > 20) {
        fprintf(stderr, "Предупреждение: факториал >10 может привести к переполнению\n");
    }
    double res = 1;
    for (int i = 2; i <= (int)n; i++) res *= i;
    return res;
}

// ГЛАВНАЯ ФУНКЦИЯ ОБХОДА ДЕРЕВА
double calculate(struct Node* node) {
    if (node == NULL) return 0;
    
    if (node->type == NODE_NUM) {
        return node->value;
    }

    if (node->type == NODE_VAR) {
        return getVar(node->var);
    }

    double leftVal = calculate(node->left);
    double rightVal = calculate(node->right);
    switch (node->op) {
        case '+': return leftVal + rightVal;
        case '-': return leftVal - rightVal;
        case '*': return leftVal * rightVal;
        case '/':
            if (rightVal == 0) return 0; // Защита от деления на ноль
            return leftVal / rightVal;
        case '%':
            return (double)((long long)leftVal % (long long)rightVal);
        case '@':
            return calculateLCM(leftVal, rightVal);
        case '!':
            return calculateFactorial(leftVal);
        default:
            return 0;
    }
}

double getVar(const char* name) {
    if (!name || !*name) {
        fprintf(stderr, "Ошибка: пустое имя переменной при чтении\n");
        return 0;
    }

    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            if (!symTable[i].defined) {
                fprintf(stderr, "Ошибка: переменная '%s' объявлена, но не имеет значения\n", name);
                return 0;
            }
            return symTable[i].value;
        }
    }

    // Переменная не найдена
    fprintf(stderr, "Ошибка: переменная '%s' не определена. Используйте: eval %s = <значение>\n", name, name);
    return 0;
}

void setVar(const char* name, double value) {
    if (!name || !*name) {
        fprintf(stderr, "Ошибка: пустое имя переменной\n");
        return;
    }

    // Проверяем, существует ли переменная
    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            symTable[i].value = value;
            symTable[i].defined = 1;
            return;
        }
    }

    // Добавляем новую переменную
    if (symCount >= MAX_VARS) {
        fprintf(stderr, "Ошибка: превышен лимит переменных (%d)\n", MAX_VARS);
        return;
    }

    strncpy(symTable[symCount].name, name, VAR_NAME_LEN - 1);
    symTable[symCount].name[VAR_NAME_LEN - 1] = '\0';  // Гарантия нуль-терминации
    symTable[symCount].value = value;
    symTable[symCount].defined = 1;
    symCount++;
}

// Рекурсивное освобождение памяти
void freeTree(Node* node) {
    if (node == NULL) return;
    freeTree(node->left);
    freeTree(node->right);
    free(node); // Дать обертку для подсчета
}

void expr(const char* str){
    return;
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

void process_commands(FILE* in, FILE* out) {
    char line[4096];      // Буфер для чтения строки из файла
    char arg[1024];       // Буфер для аргументов (ровно 1024 байта по условию)

    while (fgets(line, sizeof(line), in) != NULL) {
        // 1. Игнорируем пустые строки
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            continue;
        }

        // 2. Если строка начинается с пробельного символа
        if (isspace((unsigned char)line[0])) {
            fprintf(out, "incorrent\n");
            continue;
        }

        // Вспомогательная логика копирования аргумента (inline для наглядности)
        #define COPY_ARG(start_idx) \
        do { \
            const char* src = line + (start_idx); \
            size_t i = 0; \
            while (i < 1024 && src[i] != '\n' && src[i] != '\r' && src[i] != '\0') { \
                arg[i] = src[i]; \
                i++; \
            } \
            /* Замена последнего символа на \0 при достижении лимита или конце строки */ \
            if (i == 1024) arg[1023] = '\0'; \
            else arg[i] = '\0'; \
        } while(0)

        // 3. Парсинг и вызов команд
        if (strncmp(line, "parse ", 6) == 0) {
            COPY_ARG(6);
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
            // Неизвестная команда
            fprintf(out, "incorrent\n");
        }

        #undef COPY_ARG
    }
}

int main() {
    char expr[1024];
    printf("Введите выражение (поддерживаются: + - * / %% @ !): ");
    if (fgets(expr, sizeof(expr), stdin) == NULL) return 1;

    // Убираем символ новой строки
    expr[strcspn(expr, "\n")] = '\0';

    input = expr;
    Node* root = parseExpression();
    skipSpaces();

    // Проверяем, что вся строка была успешно разобрана
    if (*input != '\0') {
        fprintf(stderr, "Ошибка: неверный синтаксис после позиции '%s'\n", input);
        freeTree(root);
        return 1;
    }

    //добавть EVAL
    // double result = calculate(root); 
    // printf("Результат: %g\n", result);

    freeTree(root);
    return 0;
}