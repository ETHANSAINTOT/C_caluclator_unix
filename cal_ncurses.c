#include <ncurses.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <complex.h>

/* Définitions de constantes mathématiques */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif

/* ============================= */
/* Partie Parsing                */
/* Utilise la grammaire :
     expression = term { ('+' | '-') term }
     term       = power { ( 'x' | '/' | "//" ) power }
     power      = factor [ '^' power ]          (droite associativité)
     factor     = { '-' } primary { ('!' | '%') }
     primary    = func_call | constant | number | group
     func_call  = ident '(' arglist ')'
     arglist    = expression [ ',' expression ]
     constant   = "pi" | "e" | "i"
     group      = '(' expression ')' | '[' expression ']' | '{' expression '}'
*/
/* ============================= */

const char *expr;  /* pointeur global sur la chaîne à analyser */
int error_flag = 0;

void skip_whitespace(void) {
    while (*expr && isspace(*expr))
        expr++;
}

double complex parse_expression(void);
double complex parse_term(void);
double complex parse_power(void);
double complex parse_factor(void);
double complex parse_primary(void);

double complex parse_expression(void) {
    double complex val = parse_term();
    skip_whitespace();
    while (*expr == '+' || *expr == '-') {
        char op = *expr;
        expr++;
        double complex val2 = parse_term();
        if (error_flag) return 0;
        if (op == '+')
            val += val2;
        else
            val -= val2;
        skip_whitespace();
    }
    return val;
}

double complex parse_term(void) {
    double complex val = parse_power();
    skip_whitespace();
    while (1) {
        if (*expr == 'x') {  /* multiplication */
            expr++;
            double complex val2 = parse_power();
            val *= val2;
        } else if (*expr == '/') {
            expr++;
            if (*expr == '/') { /* division entière : "//" */
                expr++;
                double complex val2 = parse_power();
                if (cabs(val2) < 1e-12) {
                    error_flag = 1;
                    fprintf(stderr, "Erreur : division entière par 0\n");
                    return 0;
                }
                val = trunc(creal(val) / creal(val2));
            } else { /* division classique */
                double complex val2 = parse_power();
                if (cabs(val2) < 1e-12) {
                    error_flag = 1;
                    fprintf(stderr, "Erreur : division par 0\n");
                    return 0;
                }
                val /= val2;
            }
        } else {
            break;
        }
        skip_whitespace();
    }
    return val;
}

double complex parse_power(void) {
    double complex base = parse_factor();
    skip_whitespace();
    if (*expr == '^') {
        expr++; /* sauter '^' */
        double complex exponent = parse_power();
        return cpow(base, exponent);
    }
    return base;
}

double complex parse_factor(void) {
    skip_whitespace();
    int neg = 0;
    while (*expr == '-') {
        neg = !neg;
        expr++;
        skip_whitespace();
    }
    double complex val = parse_primary();
    if (error_flag) return 0;
    skip_whitespace();
    while (*expr == '!' || *expr == '%') {
        if (*expr == '!') {
            expr++;
            /* Factorielle définie uniquement pour les réels non négatifs */
            if (cimag(val) != 0 || creal(val) < 0) {
                error_flag = 1;
                fprintf(stderr, "Erreur : factorielle d'un nombre négatif ou complexe non supportée\n");
                return 0;
            }
            val = tgamma(creal(val) + 1);
        } else if (*expr == '%') {
            expr++;
            val = val / 100.0;
        }
        skip_whitespace();
    }
    if (neg)
        val = -val;
    return val;
}

double complex parse_primary(void) {
    skip_whitespace();
    //double complex val = 0;
    if (isalpha(*expr)) {
        /* Lecture de l'identifiant */
        char ident[32];
        int pos = 0;
        while (isalpha(*expr)) {
            if (pos < 31)
                ident[pos++] = *expr;
            expr++;
        }
        ident[pos] = '\0';
        skip_whitespace();
        if (*expr == '(') {
            /* Appel de fonction */
            expr++; /* sauter '(' */
            double complex arg1 = parse_expression();
            double complex arg2 = 0;
            skip_whitespace();
            int num_args = 1;
            if (*expr == ',') {
                expr++; /* sauter ',' */
                arg2 = parse_expression();
                num_args = 2;
                skip_whitespace();
            }
            if (*expr != ')') {
                error_flag = 1;
                fprintf(stderr, "Erreur : ')' attendue après fonction\n");
                return 0;
            }
            expr++; /* sauter ')' */
            if ((strcmp(ident, "log") == 0 || strcmp(ident, "ln") == 0) && num_args == 1) {
                return clog(arg1);
            } else if (strcmp(ident, "cos") == 0 && num_args == 1) {
                return ccos(arg1);
            } else if (strcmp(ident, "sin") == 0 && num_args == 1) {
                return csin(arg1);
            } else if (strcmp(ident, "tan") == 0 && num_args == 1) {
                return ctan(arg1);
            } else if (strcmp(ident, "arccos") == 0 && num_args == 1) {
                return cacos(arg1);
            } else if (strcmp(ident, "arcsin") == 0 && num_args == 1) {
                return casin(arg1);
            } else if (strcmp(ident, "arctan") == 0 && num_args == 1) {
                return catan(arg1);
            } else if (strcmp(ident, "sqrt") == 0 && num_args == 1) {
                return csqrt(arg1);
            } else if (strcmp(ident, "root") == 0 && num_args == 2) {
                /* root(x,n) = x^(1/n) */
                return cpow(arg1, 1.0/arg2);
            } else {
                error_flag = 1;
                fprintf(stderr, "Erreur : fonction inconnue '%s' ou nombre d'arguments invalide\n", ident);
                return 0;
            }
        } else {
            /* Constante ou identifiant simple */
            if (strcmp(ident, "pi") == 0) {
                return M_PI;
            } else if (strcmp(ident, "e") == 0) {
                return M_E;
            } else if (strcmp(ident, "i") == 0) {
                return I;
            } else {
                error_flag = 1;
                fprintf(stderr, "Erreur : identifiant inconnu '%s'\n", ident);
                return 0;
            }
        }
    } else if (isdigit(*expr) || *expr == '.') {
        char *endptr;
        double real_val = strtod(expr, &endptr);
        expr = endptr;
        return real_val;
    } else if (*expr == '(' || *expr == '[' || *expr == '{') {
        char open = *expr;
        char close;
        if (open == '(') close = ')';
        else if (open == '[') close = ']';
        else /* if (open == '{') */ close = '}';
        expr++; /* sauter le caractère d'ouverture */
        double complex inner = parse_expression();
        skip_whitespace();
        if (*expr != close) {
            error_flag = 1;
            fprintf(stderr, "Erreur : '%c' attendue\n", close);
            return 0;
        }
        expr++; /* sauter le caractère de fermeture */
        return inner;
    } else {
        error_flag = 1;
        fprintf(stderr, "Erreur : caractère inattendu '%c'\n", *expr);
        return 0;
    }
}

/* Fonction d'évaluation : lance le parsing sur l'expression donnée */
double complex evaluate_expression(const char *expression_str) {
    expr = expression_str;
    error_flag = 0;
    double complex res = parse_expression();
    skip_whitespace();
    if (*expr != '\0' && *expr != '\n')
        error_flag = 1;
    return res;
}

/* ============================= */
/* Partie Interface Ncurses      */
/* ============================= */

/* Structure représentant un bouton */
typedef struct {
    int row, col;      /* position dans la grille */
    int x, y;          /* coordonnées sur l'écran */
    int width, height; /* dimensions */
    char label[16];
} Button;

/* Nouvelle grille de boutons : 7 lignes x 6 colonnes (42 cases) */
#define GRID_ROWS 7
#define GRID_COLS 6
#define NUM_BUTTONS (GRID_ROWS * GRID_COLS)

Button buttons[NUM_BUTTONS];
int selected_button = 0;  /* index du bouton sélectionné */

/* Buffer d'expression éditable */
char expression_buf[256] = "";
int expr_cursor = 0;      /* position d'insertion dans le buffer */

/* Mode de focus : 0 = mode boutons, 1 = mode édition */
int focus_mode = 0;

/* Zone de message (résultat ou erreur) */
char message[256] = "";

/* Convertit une expression contenant '^' en affichant les exposants en superscript */
void format_expression(const char *src, char *dest, size_t dest_size) {
    size_t j = 0;
    for (size_t i = 0; src[i] && j < dest_size - 1; i++) {
        if (src[i] == '^') {
            i++;
            while (src[i] && (src[i] == '-' || isdigit(src[i]) || src[i]=='.')) {
                const char *sup = NULL;
                if (src[i] == '-') sup = "⁻";
                else if (src[i] == '0') sup = "⁰";
                else if (src[i] == '1') sup = "¹";
                else if (src[i] == '2') sup = "²";
                else if (src[i] == '3') sup = "³";
                else if (src[i] == '4') sup = "⁴";
                else if (src[i] == '5') sup = "⁵";
                else if (src[i] == '6') sup = "⁶";
                else if (src[i] == '7') sup = "⁷";
                else if (src[i] == '8') sup = "⁸";
                else if (src[i] == '9') sup = "⁹";
                else if (src[i] == '.') { dest[j++] = src[i]; i++; continue; }
                else { dest[j++] = src[i]; i++; continue; }
                size_t len = strlen(sup);
                if (j + len < dest_size) {
                    strcpy(&dest[j], sup);
                    j += len;
                }
                i++;
            }
            i--;
        } else {
            dest[j++] = src[i];
        }
    }
    dest[j] = '\0';
}

/* Insertion de texte dans le buffer d'expression à la position du curseur */
void insert_text(const char *text) {
    int len = strlen(expression_buf);
    int text_len = strlen(text);
    if (len + text_len >= (int)sizeof(expression_buf) - 1)
        return;
    memmove(expression_buf + expr_cursor + text_len,
            expression_buf + expr_cursor,
            len - expr_cursor + 1);
    memcpy(expression_buf + expr_cursor, text, text_len);
    expr_cursor += text_len;
}

/* Suppression d'un caractère avant le curseur */
void delete_char(void) {
    int len = strlen(expression_buf);
    if (expr_cursor > 0) {
        memmove(expression_buf + expr_cursor - 1,
                expression_buf + expr_cursor,
                len - expr_cursor + 1);
        expr_cursor--;
    }
}

/* Initialisation des boutons dans une grille 7x6 */
void init_buttons(void) {
    const char *labels[GRID_ROWS][GRID_COLS] = {
        {"Quit", "C",   "<-", "(",  ")",  "["},
        {"]",    "{",   "}",  "7",  "8",  "9"},
        {"+",    "x",   "-",  "/",  "//", "^"},
        {"pi",   "e",   ".",  "4",  "5",  "6"},
        {"1",    "2",   "3",  "0",  "!",  "%"},
        {"log",  "ln",  "cos", "sin", "tan", "arctan"},
        {"sqrt", "root","=",  "<",  ">",  ""}
    };
    int start_x = 2, start_y = 5;
    int btn_width = 8, btn_height = 3;
    int gap_x = 2, gap_y = 1;
    int index = 0;
    for (int r = 0; r < GRID_ROWS; r++) {
        for (int c = 0; c < GRID_COLS; c++) {
            buttons[index].row = r;
            buttons[index].col = c;
            buttons[index].x = start_x + c * (btn_width + gap_x);
            buttons[index].y = start_y + r * (btn_height + gap_y);
            buttons[index].width = btn_width;
            buttons[index].height = btn_height;
            strncpy(buttons[index].label, labels[r][c], 15);
            buttons[index].label[15] = '\0';
            index++;
        }
    }
}

/* Affiche les boutons sur l'écran */
void draw_buttons(void) {
    for (int i = 0; i < NUM_BUTTONS; i++) {
        if (strlen(buttons[i].label) == 0)
            continue;
        int x = buttons[i].x, y = buttons[i].y;
        int w = buttons[i].width, h = buttons[i].height;
        if (focus_mode == 0 && i == selected_button)
            attron(A_REVERSE);
        for (int j = 0; j < h; j++) {
            for (int k = 0; k < w; k++) {
                if (j == 0 || j == h - 1 || k == 0 || k == w - 1)
                    mvprintw(y + j, x + k, " ");
                else
                    mvprintw(y + j, x + k, " ");
            }
        }
        int label_len = strlen(buttons[i].label);
        int label_x = x + (w - label_len) / 2;
        int label_y = y + h / 2;
        mvprintw(label_y, label_x, "%s", buttons[i].label);
        if (focus_mode == 0 && i == selected_button)
            attroff(A_REVERSE);
    }
}

/* Affiche la zone d'expression et le message.
   En mode édition, le buffer brut est affiché avec un indicateur de curseur. */
void draw_display(void) {
    if (focus_mode == 1) {
        mvprintw(1, 2, "Expression (edit): %-50s", expression_buf);
        int base = strlen("Expression (edit): ");
        mvprintw(2, base + 2 + expr_cursor, "^");
    } else {
        char formatted[512];
        format_expression(expression_buf, formatted, sizeof(formatted));
        mvprintw(1, 2, "Expression: %-50s", formatted);
    }
    mvprintw(4, 2, "Result/Error: %-50s", message);
    if (focus_mode == 0)
        mvprintw(0, 2, "Focus: Boutons (F2: éditer, q: quitter)");
    else
        mvprintw(0, 2, "Focus: Expression (F2: boutons, q: quitter)");
}

int main(void) {
    int ch;
    MEVENT event;

    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    curs_set(0);
    if (has_colors()) {
        start_color();
        init_pair(1, COLOR_BLACK, COLOR_CYAN);
    }

    init_buttons();

    while (1) {
        clear();
        draw_display();
        if (focus_mode == 0)
            draw_buttons();
        refresh();

        ch = getch();
        if (ch == 'q' || ch == 'Q')
            break;
        if (ch == KEY_F(2)) {
            focus_mode = !focus_mode;
            continue;
        }
        if (focus_mode == 0) {  /* Mode Boutons */
            if (ch == KEY_MOUSE) {
                if (getmouse(&event) == OK) {
                    if (event.bstate & BUTTON1_CLICKED) {
                        for (int i = 0; i < NUM_BUTTONS; i++) {
                            if (strlen(buttons[i].label) == 0)
                                continue;
                            if (event.x >= buttons[i].x && event.x < buttons[i].x + buttons[i].width &&
                                event.y >= buttons[i].y && event.y < buttons[i].y + buttons[i].height) {
                                selected_button = i;
                                ch = '\n'; /* simuler Entrée */
                                break;
                            }
                        }
                    }
                }
            } else if (ch == KEY_UP) {
                int cur_row = buttons[selected_button].row;
                int cur_col = buttons[selected_button].col;
                if (cur_row > 0) {
                    for (int i = 0; i < NUM_BUTTONS; i++) {
                        if (buttons[i].row == cur_row - 1 && buttons[i].col == cur_col &&
                            strlen(buttons[i].label) != 0) {
                            selected_button = i;
                            break;
                        }
                    }
                }
            } else if (ch == KEY_DOWN) {
                int cur_row = buttons[selected_button].row;
                int cur_col = buttons[selected_button].col;
                if (cur_row < GRID_ROWS - 1) {
                    for (int i = 0; i < NUM_BUTTONS; i++) {
                        if (buttons[i].row == cur_row + 1 && buttons[i].col == cur_col &&
                            strlen(buttons[i].label) != 0) {
                            selected_button = i;
                            break;
                        }
                    }
                }
            } else if (ch == KEY_LEFT) {
                int cur_row = buttons[selected_button].row;
                int cur_col = buttons[selected_button].col;
                if (cur_col > 0) {
                    for (int i = 0; i < NUM_BUTTONS; i++) {
                        if (buttons[i].row == cur_row && buttons[i].col == cur_col - 1 &&
                            strlen(buttons[i].label) != 0) {
                            selected_button = i;
                            break;
                        }
                    }
                }
            } else if (ch == KEY_RIGHT) {
                int cur_row = buttons[selected_button].row;
                int cur_col = buttons[selected_button].col;
                if (cur_col < GRID_COLS - 1) {
                    for (int i = 0; i < NUM_BUTTONS; i++) {
                        if (buttons[i].row == cur_row && buttons[i].col == cur_col + 1 &&
                            strlen(buttons[i].label) != 0) {
                            selected_button = i;
                            break;
                        }
                    }
                }
            }
            /* Support du clavier physique en mode Boutons :
               les touches '<' et '>' déplacent le curseur dans l'expression */
            else if (ch == '<') {
                if (expr_cursor > 0)
                    expr_cursor--;
            } else if (ch == '>') {
                if (expr_cursor < (int)strlen(expression_buf))
                    expr_cursor++;
            }
            else if (ch == KEY_BACKSPACE || ch == 127) {
                delete_char();
            } else if (isprint(ch)) {
                char s[2] = { (char)ch, '\0' };
                insert_text(s);
            } else if (ch == '\n' || ch == KEY_ENTER) {
                char *label = buttons[selected_button].label;
                if (strcmp(label, "Quit") == 0) {
                    break;
                } else if (strcmp(label, "C") == 0) {
                    expression_buf[0] = '\0';
                    expr_cursor = 0;
                    message[0] = '\0';
                } else if (strcmp(label, "<-") == 0) {
                    delete_char();
                } else if (strcmp(label, "=") == 0) {
                    message[0] = '\0';
                    double complex res = evaluate_expression(expression_buf);
                    if (!error_flag) {
                        if (fabs(cimag(res)) < 1e-12)
                            snprintf(message, sizeof(message), "%g", creal(res));
                        else
                            snprintf(message, sizeof(message), "%g+%gi", creal(res), cimag(res));
                        /* Optionnel : mettre à jour l'expression avec le résultat */
                        snprintf(expression_buf, sizeof(expression_buf), "%s", message);
                        expr_cursor = strlen(expression_buf);
                    }
                } else {
                    insert_text(label);
                }
            }
        } else {  /* Mode Édition */
            if (ch == KEY_LEFT || ch == '<') {
                if (expr_cursor > 0)
                    expr_cursor--;
            } else if (ch == KEY_RIGHT || ch == '>') {
                if (expr_cursor < (int)strlen(expression_buf))
                    expr_cursor++;
            } else if (ch == KEY_BACKSPACE || ch == 127) {
                delete_char();
            } else if (isprint(ch)) {
                char s[2] = { (char)ch, '\0' };
                insert_text(s);
            }
        }
    }
    endwin();
    return 0;
}
