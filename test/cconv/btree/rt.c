
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <inttypes.h>
#include <setjmp.h>
#include <time.h>

// jmbuf intrinsics
int64_t jmpbuf_size() {
    return sizeof(jmp_buf);
}

typedef struct node {
    int32_t id;
    struct node* left;
    struct node* right;
} node_t;

uint64_t left(uint64_t node) {
    return (uint64_t) ((node_t*) node)->left;
}

uint64_t right(uint64_t node) {
    return (uint64_t) ((node_t*) node)->right;
}

int32_t id(uint64_t node) {
    return ((node_t*) node)->id;
}

static node_t* node(int id, node_t* l, node_t* r) {
    node_t* n = malloc(sizeof(struct node));
    if (!n) { 
        printf("allocation of node failed");
        abort();
    }
    n->id = id;
    n->left = l;
    n->right = r;
    return n;
}

static node_t* make_balanced(int32_t depth, int32_t* id, int32_t tag, node_t** tagged) {
    if (depth <= 0) return NULL;
    int32_t cur_id = (*id)++;
    node_t* l = make_balanced(depth - 1, id, tag, tagged);
    node_t* r = make_balanced(depth - 1, id, tag, tagged);
    node_t* n = node(cur_id, l, r);
    if (cur_id == tag) {
        printf("tagged node <%p : %d>\n", n, n->id);
        assert(!*tagged && "can have only one tagged node");
        *tagged = n;
    }
    return n;
}

static void print_tree(char pos, node_t *t, int32_t indent) {
    for (int i = 0; i < indent; i++)
        printf("   ");
    printf("%c <%p : %d>\n", pos, t, t->id);
    if (t->left) print_tree('L', t->left, indent + 1);
    if (t->right) print_tree('R', t->right, indent + 1);
}

extern uint64_t solve(uint64_t tree, int32_t id);

static const int TEST_DEPTH = 8;
static const unsigned SEED = 42;

int main() {
    srand(time(NULL));
    int32_t counter = 0;
    int32_t tag = rand() % (1 << TEST_DEPTH);
    printf("test tag: %d\n", tag);
    node_t* tagged = NULL;
    node_t* tree = make_balanced(TEST_DEPTH, &counter, tag, &tagged);
    assert(tagged && "no node was tagged");
    node_t* found = (node_t*) solve((uint64_t) tree, tag);
    if (!found) return EXIT_FAILURE;
    print_tree('I', found, 0);
    return (tagged == found) ? EXIT_SUCCESS : EXIT_FAILURE;
}

