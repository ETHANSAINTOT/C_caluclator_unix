/****************************************************************************************************************\
**                                                                                                              **
** EPITECH PROJECT, 2025                                                                                        **
**                                                                                                              **
** Module Name : B-NCU-200                                                                                      **
** Module description : The ncurses module                                                                      **
**                                                                                                              **
** Project name : The ncurses cal                                                                               **
** Project description : Creation of a calculator in ncurses without coding style and all functions are allowed **
**                                                                                                              **
** File name: cal.c                                                                                             **
** File description : the main file                                                                             **
**                                                                                                              **
** Editor : ethan.saintot@epitech.eu                                                                            **
**                                                                                                              **
** Language : C                                                                                                 **
**                                                                                                              **
\****************************************************************************************************************/


#include <ncurses.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Définitions de constantes mathématiques */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif

/* ============================= */
/* Partie Parsing (modifiée)     */
/* Grammaire utilisée :
   expression = term { ('+' | '-') term }
   term       = power { ( 'x' | '/' | "//" ) power }
   power      = factor [ '^' power ]  (droite associatif)
   factor     = { '-' } primary { ('!' | '%') }
   primary    = number | "pi" | 'e' | '(' expression ')'
*/
/* ============================= */

const char *expr;  /* pointeur global pour l'analyse */
int error_flag = 0;

void skip_whitespace(void) {
    while (*expr && isspace(*expr))
        expr++;
}

double parse_expression(void);
double parse_term(void);
double parse_power(void);
double parse_factor(void);
double parse_primary(void);

double parse_expression(void) {
    double val = parse_term();
    skip_whitespace();
    while (*expr == '+' || *expr == '-') {
        char op = *expr;
        expr++;
        double val2 = parse_term();
        if (error_flag) return 0;
        if (op == '+')
            val += val2;
        else
            val -= val2;
        skip_whitespace();
    }
    return val;
}

double parse_term(void) {
    double val = parse_power();
    skip_whitespace();
    while (1) {
        if (*expr == 'x') { /* multiplication */
            expr++;
            double val2 = parse_power();
            val *= val2;
        } else if (*expr == '/') {
            expr++;
            if (*expr == '/') { /* division entière : "//" */
                expr++;
                double val2 = parse_power();
                if (val2 == 0) {
                    error_flag = 1;
                    fprintf(stderr, "Erreur : division entière par 0\n");
                    return 0;
                }
                val = trunc(val / val2);
            } else { /* division classique */
                double val2 = parse_power();
                if (val2 == 0) {
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

double parse_power(void) {
    double base = parse_factor();
    skip_whitespace();
    if (*expr == '^') {
        expr++; /* on saute '^' */
        double exponent = parse_power(); /* puissance est droite associatif */
        return pow(base, exponent);
    }
    return base;
}

double parse_factor(void) {
    skip_whitespace();
    int neg = 0;
    while (*expr == '-') {
        neg = !neg;
        expr++;
        skip_whitespace();
    }
    double val = parse_primary();
    if (error_flag) return 0;
    skip_whitespace();
    while (*expr == '!' || *expr == '%') {
        if (*expr == '!') {
            expr++;
            if (val < 0) {
                error_flag = 1;
                fprintf(stderr, "Erreur : factorielle d'un nombre négatif\n");
                return 0;
            }
            val = tgamma(val + 1);
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

double parse_primary(void) {
    skip_whitespace();
    double val = 0;
    if (*expr == '(') {
        expr++; /* parenthèse ouvrante */
        double inner = parse_expression();
        skip_whitespace();
        if (*expr != ')') {
            error_flag = 1;
            fprintf(stderr, "Erreur : ')' attendue\n");
            return 0;
        }
        expr++; /* parenthèse fermante */
        return inner;
    } else if (isdigit(*expr) || *expr == '.') {
        char *endptr;
        val = strtod(expr, &endptr);
        expr = endptr;
        return val;
    } else if (strncmp(expr, "pi", 2) == 0) {
        expr += 2;
        return M_PI;
    } else if (*expr == 'e') {
        expr++;
        return M_E;
    } else {
        error_flag = 1;
        fprintf(stderr, "Erreur : caractère inattendu '%c'\n", *expr);
        return 0;
    }
}

/* Fonction d'évaluation : lance l'analyse sur l'expression donnée */
double evaluate_expression(const char *expression_str) {
    expr = expression_str;
    error_flag = 0;
    double res = parse_expression();
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

/* Grille de boutons : ici 5 lignes x 6 colonnes */
#define GRID_ROWS 5
#define GRID_COLS 6
#define NUM_BUTTONS (GRID_ROWS * GRID_COLS)

Button buttons[NUM_BUTTONS];
int selected_button = 0;  /* index du bouton sélectionné */

/* La zone de saisie de l'expression (buffer éditable) */
char expression_buf[256] = "";
int expr_cursor = 0;      /* position d'insertion dans expression_buf */

/* Mode de focus : 0 = boutons, 1 = édition de l'expression */
int focus_mode = 0;

/* Zone de message (résultat ou erreur) */
char message[256] = "";

/* Convertit la portion d'expression après '^' en exposant (superscript)
   et construit une version formatée dans dest. */
void format_expression(const char *src, char *dest, size_t dest_size) {
    size_t j = 0;
    for (size_t i = 0; src[i] && j < dest_size - 1; i++) {
        if (src[i] == '^') {
            /* Au lieu d'afficher '^', on convertit le nombre suivant en exposant */
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
            i--; /* ajuster l'index car la boucle for incrémente */
        } else {
            dest[j++] = src[i];
        }
    }
    dest[j] = '\0';
}

/* Insertion de texte dans le buffer d'expression à la position expr_cursor */
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

/* Initialisation des boutons dans une grille 5x6 */
void init_buttons(void) {
    /* Disposition proposée (les chaînes vides sont des cases inactives) */
    const char *labels[GRID_ROWS][GRID_COLS] = {
        {"Quit", "C",   "←",  "(",  ")",   ""},
        {"7",    "8",   "9",  "+",  "x",   "-"},
        {"4",    "5",   "6",  "/",  "//",  "^"},
        {"1",    "2",   "3",  "pi", "e",   "."},
        {"0",    "!",   "%",  "=",  "",    ""}
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

/* Dessine les boutons sur l'écran */
void draw_buttons(void) {
    for (int i = 0; i < NUM_BUTTONS; i++) {
        if (strlen(buttons[i].label) == 0)
            continue; /* case inactive */
        int x = buttons[i].x, y = buttons[i].y;
        int w = buttons[i].width, h = buttons[i].height;
        if (focus_mode == 0 && i == selected_button)
            attron(A_REVERSE);
        /* Dessiner le contour du bouton */
        for (int j = 0; j < h; j++) {
            for (int k = 0; k < w; k++) {
                if (j == 0 || j == h - 1 || k == 0 || k == w - 1)
                    mvprintw(y + j, x + k, " ");
                else
                    mvprintw(y + j, x + k, " ");
            }
        }
        /* Affichage centré du label */
        int label_len = strlen(buttons[i].label);
        int label_x = x + (w - label_len) / 2;
        int label_y = y + h / 2;
        mvprintw(label_y, label_x, "%s", buttons[i].label);
        if (focus_mode == 0 && i == selected_button)
            attroff(A_REVERSE);
    }
}

/* Affiche la zone d'expression et le message.
   En mode édition, on affiche le buffer brut avec un indicateur de curseur. */
void draw_display(void) {
    if (focus_mode == 1) {
        mvprintw(1, 2, "Expression (edit): %-50s", expression_buf);
        /* Affichage du curseur sous forme d'un '^' à la position correspondante */
        int base = strlen("Expression (edit): ");
        mvprintw(2, base + 2 + expr_cursor, "^");
    } else {
        char formatted[512];
        format_expression(expression_buf, formatted, sizeof(formatted));
        mvprintw(1, 2, "Expression: %-50s", formatted);
    }
    mvprintw(4, 2, "Result/Error: %-50s", message);
    if (focus_mode == 0)
        mvprintw(0, 2, "Focus: Boutons (F2 pour éditer, q pour quitter)");
    else
        mvprintw(0, 2, "Focus: Expression (F2 pour boutons, q pour quitter)");
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
        if (ch == 'q' || ch == 'Q') {  /* quitter avec q */
            break;
        }
        if (ch == KEY_F(2)) {  /* F2 pour changer de mode */
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
            } else if (ch == '\n' || ch == KEY_ENTER) {
                char *label = buttons[selected_button].label;
                if (strcmp(label, "Quit") == 0) {
                    break;
                } else if (strcmp(label, "C") == 0) {
                    expression_buf[0] = '\0';
                    expr_cursor = 0;
                    message[0] = '\0';
                } else if (strcmp(label, "←") == 0) {
                    delete_char();
                } else if (strcmp(label, "=") == 0) {
                    message[0] = '\0';
                    double res = evaluate_expression(expression_buf);
                    if (!error_flag) {
                        snprintf(message, sizeof(message), "%g", res);
                        /* Optionnel : réinitialiser l'expression avec le résultat */
                        snprintf(expression_buf, sizeof(expression_buf), "%g", res);
                        expr_cursor = strlen(expression_buf);
                    }
                } else {
                    insert_text(label);
                }
            }
        } else {  /* Mode Édition d'expression */
            if (ch == KEY_LEFT) {
                if (expr_cursor > 0)
                    expr_cursor--;
            } else if (ch == KEY_RIGHT) {
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
