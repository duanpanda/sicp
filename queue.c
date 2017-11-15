#define TRUE  (1)
#define FALSE (0)
#define OK (1)
#define ERROR (0)
#define INFEASIBLE (-1)
#define OVERFLOW (-2)

typedef int Status;

typedef int QElemType;

typedef struct QNode {
    QElemType     data;
    struct QNode *next;
} QNode, *QueuePtr;

typedef struct {
    QueuePtr front;
    QueuePtr rear;
} LinkedQueue;

Status InitQueue(LinkedQueue *Q);
Status DestroyQueue(LinkedQueue *Q);
Status ClearQueue(LinkedQueue *Q);
Status QueueEmpty(LinkedQueue Q);
Status QueueLength(LinkedQueue *Q);
Status GetHead(LinkedQueue Q, QElemType **e);
Status EnQueue(LinkedQueue *Q, QElemType e);
Status DeQueue(LinkedQueue *Q, QElemType e);
Status QueueTraverse(LinkedQueue Q, void (*visit)());
