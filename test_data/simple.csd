
---
Chunk {
    upvalues: 0,
    constants: [
        Number(
            Integer(
                3,
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
        Reference {
            symbol: Spur(4),
        },
        Reference {
            symbol: Spur(8),
        },
        PushConst {
            index: 0,
        },
        Reference {
            symbol: Spur(7),
        },
        Call {
            args: 2,
        },
        Reference {
            symbol: Spur(8),
        },
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                0,
                4,
            ),
        },
        1: SourceData {
            source_id: Spur(3),
            range: (
                5,
                19,
            ),
        },
        5: SourceData {
            source_id: Spur(3),
            range: (
                20,
                23,
            ),
        },
    },
    fallback: None,
}
---
cowl
(define ram 3)
ram
