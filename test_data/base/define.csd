
---
Chunk {
    upvalues: 0,
    constants: [
        Number(
            3,
        ),
        String(
            "🤲",
        ),
    ],
    lambdas: [],
    promises: [],
    import_env: RefLock(
        Environment {
            parent: None,
            inner: RefLock(
                EnvironmentInner {
                    values: {},
                },
            ),
            is_frozen: false,
        },
    ),
    code: [
        PushConst {
            index: 0,
        },
        Define {
            symbol: Spur(8),
        },
        PushVoid,
        Reference {
            symbol: Spur(8),
        },
        Reference {
            symbol: Spur(8),
        },
        Reference {
            symbol: Spur(10),
        },
        Call {
            args: 2,
        },
        Define {
            symbol: Spur(9),
        },
        PushVoid,
        PushConst {
            index: 1,
        },
        PushConst {
            index: 1,
        },
        Reference {
            symbol: Spur(12),
        },
        Call {
            args: 2,
        },
        Define {
            symbol: Spur(11),
        },
        PushVoid,
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                37,
                49,
            ),
        },
        9: SourceData {
            source_id: Spur(3),
            range: (
                69,
                107,
            ),
        },
        3: SourceData {
            source_id: Spur(3),
            range: (
                50,
                68,
            ),
        },
    },
    fallback: None,
}
---
(import (only (scheme base) define))
(define x 3)
(define y (+ x x))
(define z (eq? "\x1f932;" "\x1f932;"))
; TODO (define (name vals ...) ...) form
