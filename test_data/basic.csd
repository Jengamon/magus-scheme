
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
                4,
            ),
        ),
    ],
    lambdas: [
        Compiled(
            CompiledLambda {
                arity: Exact(
                    0,
                ),
                chunk: Chunk {
                    upvalues: 0,
                    constants: [
                        Number(
                            Integer(
                                3,
                            ),
                        ),
                        Number(
                            Integer(
                                4,
                            ),
                        ),
                    ],
                    lambdas: [],
                    promises: [],
                    import_env: RefLock(
                        Environment {
                            parent: Some(
                                RefLock(
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
                            ),
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
                            index: 1,
                        },
                    ],
                    labels: {
                        0: SourceData {
                            source_id: Spur(3),
                            range: (
                                78,
                                79,
                            ),
                        },
                    },
                    fallback: None,
                },
                arg_names: [],
                rest_name: None,
                upvalue_id: None,
                doc_string: RefLock(
                    None,
                ),
            },
        ),
    ],
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
            symbol: Spur(9),
        },
        PushVoid,
        PushLambda {
            index: 0,
        },
        Define {
            symbol: Spur(10),
        },
        PushVoid,
        Reference {
            symbol: Spur(9),
            enable_fallback: true,
        },
        Reference {
            symbol: Spur(11),
            enable_fallback: false,
        },
        Call {
            args: 1,
        },
        Reference {
            symbol: Spur(10),
            enable_fallback: true,
        },
        Call {
            args: 0,
        },
        Reference {
            symbol: Spur(11),
            enable_fallback: false,
        },
        Call {
            args: 1,
        },
        Reference {
            symbol: Spur(10),
            enable_fallback: true,
        },
        Reference {
            symbol: Spur(11),
            enable_fallback: false,
        },
        Call {
            args: 1,
        },
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                44,
                56,
            ),
        },
        13: SourceData {
            source_id: Spur(3),
            range: (
                115,
                120,
            ),
        },
        9: SourceData {
            source_id: Spur(3),
            range: (
                107,
                114,
            ),
        },
        6: SourceData {
            source_id: Spur(3),
            range: (
                101,
                106,
            ),
        },
        3: SourceData {
            source_id: Spur(3),
            range: (
                57,
                81,
            ),
        },
    },
    fallback: None,
}
---
(import (only (scheme base) define lambda))
(define x 3)
(define y (lambda () 4))
#;(define (z x) x)
(z x)
(z (y))
(z y)
