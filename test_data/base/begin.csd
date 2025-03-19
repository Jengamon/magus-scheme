
---
Chunk {
    upvalues: 0,
    constants: [
        Number(
            3,
        ),
        Number(
            4,
        ),
    ],
    lambdas: [
        CompiledLambda {
            arity: Exact(
                0,
            ),
            chunk: Chunk {
                upvalues: 0,
                constants: [
                    Number(
                        3,
                    ),
                    Number(
                        4,
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
                        index: 0,
                    },
                    Reference {
                        symbol: Spur(10),
                    },
                    PushConst {
                        index: 1,
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Call {
                        args: 2,
                    },
                    Reference {
                        symbol: Spur(10),
                    },
                ],
                labels: {
                    0: SourceData {
                        source_id: Spur(3),
                        range: (
                            177,
                            201,
                        ),
                    },
                },
                fallback: None,
            },
            upvalue_id: None,
        },
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
        PushLambda {
            index: 0,
        },
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                166,
                202,
            ),
        },
    },
    fallback: None,
}
---
; begin allows sequencing values in ways that might be
; rejected normally
(import (only (scheme base) lambda begin))
; Normally rejected, but begin makes it alright
(lambda () (begin 3 (define x 4) x))
