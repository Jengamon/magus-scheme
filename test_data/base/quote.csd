
---
Chunk {
    compiler_id: 1351,
    upvalues: 0,
    constants: [
        Symbol(
            Spur(8),
        ),
        Number(
            Integer(
                3,
            ),
        ),
        String(
            "🍁 leaf",
        ),
        Number(
            Integer(
                4,
            ),
        ),
        Symbol(
            Spur(10),
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
        PushConst {
            index: 1,
        },
        PushBool {
            bool: false,
        },
        PushBool {
            bool: true,
        },
        PushConst {
            index: 2,
        },
        PushNull,
        PushNull,
        PushConst {
            index: 3,
        },
        MakePair,
        PushConst {
            index: 1,
        },
        MakePair,
        PushConst {
            index: 4,
        },
        MakePair,
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                36,
                38,
            ),
        },
        5: SourceData {
            source_id: Spur(3),
            range: (
                67,
                70,
            ),
        },
        2: SourceData {
            source_id: Spur(3),
            range: (
                42,
                45,
            ),
        },
        4: SourceData {
            source_id: Spur(3),
            range: (
                50,
                66,
            ),
        },
        1: SourceData {
            source_id: Spur(3),
            range: (
                39,
                41,
            ),
        },
        6: SourceData {
            source_id: Spur(3),
            range: (
                71,
                79,
            ),
        },
        3: SourceData {
            source_id: Spur(3),
            range: (
                46,
                49,
            ),
        },
    },
    fallback: None,
}
---
(import (only (scheme base) quote))
'x
'3
'#f
'#t
'"\x1f341; leaf"
'()
'(+ 3 4)
; TODO parse vector bytevector labelled label-ref
