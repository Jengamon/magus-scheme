
---
Chunk {
    upvalues: 0,
    constants: [
        Number(
            Integer(
                3,
            ),
        ),
        Number(
            Integer(
                5,
            ),
        ),
        Number(
            Integer(
                4,
            ),
        ),
        Number(
            Integer(
                2,
            ),
        ),
        Number(
            Integer(
                6,
            ),
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
        If {
            jump: 2,
        },
        PushConst {
            index: 1,
        },
        Jump {
            jump: 1,
        },
        PushVoid,
        PushBool {
            bool: true,
        },
        If {
            jump: 2,
        },
        PushConst {
            index: 2,
        },
        Jump {
            jump: 1,
        },
        PushConst {
            index: 1,
        },
        PushBool {
            bool: false,
        },
        If {
            jump: 2,
        },
        PushConst {
            index: 2,
        },
        Jump {
            jump: 1,
        },
        PushConst {
            index: 1,
        },
        PushNull,
        If {
            jump: 2,
        },
        PushConst {
            index: 2,
        },
        Jump {
            jump: 1,
        },
        PushConst {
            index: 1,
        },
        PushConst {
            index: 3,
        },
        PushConst {
            index: 0,
        },
        Reference {
            symbol: Spur(9),
        },
        Call {
            args: 2,
        },
        If {
            jump: 4,
        },
        PushConst {
            index: 1,
        },
        Reference {
            symbol: Spur(10),
        },
        Call {
            args: 1,
        },
        Jump {
            jump: 3,
        },
        PushConst {
            index: 4,
        },
        Reference {
            symbol: Spur(11),
        },
        Call {
            args: 1,
        },
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                39,
                47,
            ),
        },
        5: SourceData {
            source_id: Spur(3),
            range: (
                48,
                59,
            ),
        },
        10: SourceData {
            source_id: Spur(3),
            range: (
                60,
                71,
            ),
        },
        15: SourceData {
            source_id: Spur(3),
            range: (
                72,
                84,
            ),
        },
        20: SourceData {
            source_id: Spur(3),
            range: (
                85,
                114,
            ),
        },
    },
    fallback: None,
}
---
(import (only (scheme base) quote if))
(if 3 5)
(if #t 4 5)
(if #f 4 5)
(if '() 4 5)
(if (= 2 3) (jam 5) (bore 6))
