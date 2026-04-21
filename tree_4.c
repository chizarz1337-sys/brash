#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

typedef enum { NODE_OP, NODE_NUM } NodeType;

typedef struct Node {
    NodeType type;
    char op;            // '+', '-', '*', '/', '%', '@', '!'
    double value;       // Для чисел
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

// Функция создания узла
Node* createNode(NodeType type, char op, double val, Node* l, Node* r) {
    Node* node = (Node*)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Ошибка выделения памяти\n");
        exit(EXIT_FAILURE);
    }
    node->type = type;
    node->op = op;
    node->value = val;
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
        left = createNode(NODE_OP, op, 0, left, parseTerm());
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
        left = createNode(NODE_OP, op, 0, left, parseLCM());
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
        left = createNode(NODE_OP, op, 0, left, parseUnary());
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
        node = createNode(NODE_OP, op, 0, node, NULL);
        skipSpaces();
    }
    return node;
}

// 1. Высший приоритет: скобки и сами числа
Node* parseFactor() {
    skipSpaces();
    if (*input == '(') {
        input++; // Пропускаем '('
        Node* node = parseExpression();
        skipSpaces();
        if (*input == ')') input++; // Пропускаем ')'
        return node;
    }
    // Читаем число
    char* endptr;
    double val = strtod(input, &endptr);
    if (endptr == input) {
        fprintf(stderr, "Ошибка синтаксиса: ожидалось число или '(' в позиции: %s\n", input);
        exit(EXIT_FAILURE);
    }
    input = endptr;
    return createNode(NODE_NUM, 0, val, NULL, NULL);
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
double calculate(Node* node) {
    if (node == NULL) return 0;
    if (node->type == NODE_NUM) {
        return node->value;
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

// Рекурсивное освобождение памяти
void freeTree(Node* node) {
    if (node == NULL) return;
    freeTree(node->left);
    freeTree(node->right);
    free(node);
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

    double result = calculate(root);
    printf("Результат: %g\n", result);

    freeTree(root);
    return 0;
}