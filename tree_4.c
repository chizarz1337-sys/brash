#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

typedef enum { NODE_OP, NODE_NUM, NODE_VAR } NodeType;

typedef struct Node {
    NodeType type;
    char op;                // '+', '-', '*', '/', '%', '@', '!'
    double value;           // Для NODE_NUM
    char name[64];          // Для NODE_VAR
    struct Node *left;
    struct Node *right;
} Node;

// ================= ТАБЛИЦА ПЕРЕМЕННЫХ =================
#define MAX_VARS 64
typedef struct { char name[64]; double val; int defined; } VarEntry;
VarEntry symTable[MAX_VARS];
int symCount = 0;

void setVar(const char* name, double val) {
    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            symTable[i].val = val;
            symTable[i].defined = 1;
            return;
        }
    }
    if (symCount < MAX_VARS) {
        strncpy(symTable[symCount].name, name, 63);
        symTable[symCount].val = val;
        symTable[symCount].defined = 1;
        symCount++;
    } else {
        fprintf(stderr, "Ошибка: превышено максимальное количество переменных (%d)\n", MAX_VARS);
    }
}

double getVar(const char* name) {
    for (int i = 0; i < symCount; i++) {
        if (strcmp(symTable[i].name, name) == 0) {
            if (!symTable[i].defined) {
                fprintf(stderr, "Ошибка: переменная '%s' не имеет значения\n", name);
                return 0;
            }
            return symTable[i].val;
        }
    }
    fprintf(stderr, "Ошибка: переменная '%s' не определена. Используйте команду eval\n", name);
    return 0;
}

// ================= ПАРСЕР =================
char* input;

void skipSpaces() {
    while (*input && isspace((unsigned char)*input)) input++;
}

// Forward-объявления
Node* parseExpression();
Node* parseTerm();
Node* parseLCM();
Node* parseUnary();
Node* parseFactor();

Node* createNode(NodeType type, char op, double val, const char* name, Node* l, Node* r) {
    Node* node = malloc(sizeof(Node));
    if (!node) { perror("malloc"); exit(1); }
    node->type = type;
    node->op = op;
    node->value = val;
    if (name) strncpy(node->name, name, 63);
    else node->name[0] = '\0';
    node->left = l;
    node->right = r;
    return node;
}

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

Node* parseLCM() {
    Node* left = parseUnary();
    skipSpaces();
    while (*input == '@') {
        char op = *input++;
        left = createNode(NODE_OP, op, 0, NULL, left, parseUnary());
        skipSpaces();
    }
    return left;
}

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

Node* parseFactor() {
    skipSpaces();
    if (*input == '(') {
        input++;
        Node* node = parseExpression();
        skipSpaces();
        if (*input == ')') input++;
        return node;
    }
    // Проверка на переменную (идентификатор)
    if (isalpha(*input) || *input == '_') {
        char buf[64];
        int i = 0;
        while (isalnum(*input) || *input == '_') {
            buf[i++] = *input++;
            if (i >= 63) break;
        }
        buf[i] = '\0';
        return createNode(NODE_VAR, 0, 0, buf, NULL, NULL);
    }
    // Число
    char* endptr;
    double val = strtod(input, &endptr);
    if (endptr == input) {
        fprintf(stderr, "Ошибка синтаксиса: ожидалось число, переменная или '('\n");
        exit(1);
    }
    input = endptr;
    return createNode(NODE_NUM, 0, val, NULL, NULL, NULL);
}

// ================= ВЫЧИСЛЕНИЕ =================
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
    if (n < 0 || n != (int)n) return 0;
    if (n > 20) fprintf(stderr, "Предупреждение: факториал >10 может привести к переполнению double\n");
    double res = 1;
    for (int i = 2; i <= (int)n; i++) res *= i;
    return res;
}

double calculate(Node* node) {
    if (!node) return 0;
    if (node->type == NODE_NUM) return node->value;
    if (node->type == NODE_VAR) return getVar(node->name);

    double l = calculate(node->left);
    double r = calculate(node->right);
    switch (node->op) {
        case '+': return l + r;
        case '-': return l - r;
        case '*': return l * r;
        case '/': return (r == 0) ? 0 : l / r;
        case '%': return (double)((long long)l % (long long)r);
        case '@': return calculateLCM(l, r);
        case '!': return calculateFactorial(l);
        default: return 0;
    }
}

void freeTree(Node* node) {
    if (!node) return;
    freeTree(node->left);
    freeTree(node->right);
    free(node);
}

// ================= КОМАНДА EVAL =================
void processEval(const char* cmd) {
    const char* p = cmd + 4; // пропускаем "eval"
    while (*p && isspace((unsigned char)*p)) p++;

    while (*p) {
        // Читаем имя переменной
        char name[64]; int i = 0;
        while (isalnum(*p) || *p == '_') { name[i++] = *p++; if(i>=63) break; }
        name[i] = '\0';
        if (i == 0) break; // конец строки

        while (*p && isspace((unsigned char)*p)) p++;
        if (*p != '=') { fprintf(stderr, "Ошибка: ожидается '=' после '%s'\n", name); return; }
        p++;
        while (*p && isspace((unsigned char)*p)) p++;

        char* end;
        double val = strtod(p, &end);
        if (end == p) { fprintf(stderr, "Ошибка: ожидается число после '=' для '%s'\n", name); return; }
        p = end;

        setVar(name, val);

        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == ',') p++; // пропускаем запятую
        while (*p && isspace((unsigned char)*p)) p++;
    }
}

// ================= MAIN =================
int main() {
    char line[1024];
    Node* root = NULL;

    printf("Введите выражение с переменными: ");
    if (fgets(line, sizeof(line), stdin) == NULL) return 1;
    line[strcspn(line, "\n")] = '\0';

    input = line;
    root = parseExpression();
    skipSpaces();
    if (*input != '\0') {
        fprintf(stderr, "Ошибка: неверный синтаксис после позиции '%s'\n", input);
        freeTree(root);
        return 1;
    }

    printf("Выражение разобрано. Введите 'eval var1 = val1, var2 = val2, ...' или 'exit'\n");

    while (1) {
        printf("> ");
        if (fgets(line, sizeof(line), stdin) == NULL) break;
        line[strcspn(line, "\n")] = '\0';

        // Убираем пробелы в начале
        char* cmd = line;
        while (*cmd && isspace((unsigned char)*cmd)) cmd++;
        if (!*cmd) continue;

        if (strncmp(cmd, "eval", 4) == 0) {
            processEval(cmd);
            double res = calculate(root);
            printf("Результат: %g\n", res);
        } else if (strcmp(cmd, "exit") == 0 || strcmp(cmd, "quit") == 0) {
            break;
        } else {
            printf("Неизвестная команда. Доступно: eval <assignments>, exit\n");
        }
    }

    freeTree(root);
    return 0;
}