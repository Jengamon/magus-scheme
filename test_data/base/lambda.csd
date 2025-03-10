
---
; all the arities are wrong currently
Chunk {
    upvalues: 0,
    constants: [
        Number(
            3,
        ),
        Number(
            2,
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
                        2,
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
                    PushConst {
                        index: 1,
                    },
                    Reference {
                        symbol: Spur(8),
                    },
                    Call {
                        args: 2,
                    },
                ],
                labels: {
                    0: SourceData {
                        source_id: Spur(3),
                        range: (
                            48,
                            55,
                        ),
                    },
                },
            },
        },
        CompiledLambda {
            arity: Exact(
                1,
            ),
            chunk: Chunk {
                upvalues: 0,
                constants: [
                    Number(
                        3,
                    ),
                    Number(
                        2,
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
                                    2,
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
                                PushConst {
                                    index: 1,
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 2,
                                },
                            ],
                            labels: {
                                0: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        48,
                                        55,
                                    ),
                                },
                            },
                        },
                    },
                ],
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
                    FetchArg {
                        index: 0,
                    },
                    Define {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(8),
                    },
                    Call {
                        args: 2,
                    },
                ],
                labels: {
                    2: SourceData {
                        source_id: Spur(3),
                        range: (
                            69,
                            76,
                        ),
                    },
                },
            },
        },
        CompiledLambda {
            arity: AtLeast(
                1,
            ),
            chunk: Chunk {
                upvalues: 0,
                constants: [
                    Number(
                        3,
                    ),
                    Number(
                        2,
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
                                    2,
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
                                PushConst {
                                    index: 1,
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 2,
                                },
                            ],
                            labels: {
                                0: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        48,
                                        55,
                                    ),
                                },
                            },
                        },
                    },
                    CompiledLambda {
                        arity: Exact(
                            1,
                        ),
                        chunk: Chunk {
                            upvalues: 0,
                            constants: [
                                Number(
                                    3,
                                ),
                                Number(
                                    2,
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
                                                2,
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
                                            PushConst {
                                                index: 1,
                                            },
                                            Reference {
                                                symbol: Spur(8),
                                            },
                                            Call {
                                                args: 2,
                                            },
                                        ],
                                        labels: {
                                            0: SourceData {
                                                source_id: Spur(3),
                                                range: (
                                                    48,
                                                    55,
                                                ),
                                            },
                                        },
                                    },
                                },
                            ],
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
                                FetchArg {
                                    index: 0,
                                },
                                Define {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 2,
                                },
                            ],
                            labels: {
                                2: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        69,
                                        76,
                                    ),
                                },
                            },
                        },
                    },
                ],
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
                    FetchArg {
                        index: 0,
                    },
                    Define {
                        symbol: Spur(9),
                    },
                    FetchRest,
                    Define {
                        symbol: Spur(10),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(10),
                    },
                    Reference {
                        symbol: Spur(8),
                    },
                    Call {
                        args: 5,
                    },
                ],
                labels: {
                    4: SourceData {
                        source_id: Spur(3),
                        range: (
                            97,
                            113,
                        ),
                    },
                },
            },
        },
        CompiledLambda {
            arity: Exact(
                3,
            ),
            chunk: Chunk {
                upvalues: 0,
                constants: [
                    Number(
                        3,
                    ),
                    Number(
                        2,
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
                                    2,
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
                                PushConst {
                                    index: 1,
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 2,
                                },
                            ],
                            labels: {
                                0: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        48,
                                        55,
                                    ),
                                },
                            },
                        },
                    },
                    CompiledLambda {
                        arity: Exact(
                            1,
                        ),
                        chunk: Chunk {
                            upvalues: 0,
                            constants: [
                                Number(
                                    3,
                                ),
                                Number(
                                    2,
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
                                                2,
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
                                            PushConst {
                                                index: 1,
                                            },
                                            Reference {
                                                symbol: Spur(8),
                                            },
                                            Call {
                                                args: 2,
                                            },
                                        ],
                                        labels: {
                                            0: SourceData {
                                                source_id: Spur(3),
                                                range: (
                                                    48,
                                                    55,
                                                ),
                                            },
                                        },
                                    },
                                },
                            ],
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
                                FetchArg {
                                    index: 0,
                                },
                                Define {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 2,
                                },
                            ],
                            labels: {
                                2: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        69,
                                        76,
                                    ),
                                },
                            },
                        },
                    },
                    CompiledLambda {
                        arity: AtLeast(
                            1,
                        ),
                        chunk: Chunk {
                            upvalues: 0,
                            constants: [
                                Number(
                                    3,
                                ),
                                Number(
                                    2,
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
                                                2,
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
                                            PushConst {
                                                index: 1,
                                            },
                                            Reference {
                                                symbol: Spur(8),
                                            },
                                            Call {
                                                args: 2,
                                            },
                                        ],
                                        labels: {
                                            0: SourceData {
                                                source_id: Spur(3),
                                                range: (
                                                    48,
                                                    55,
                                                ),
                                            },
                                        },
                                    },
                                },
                                CompiledLambda {
                                    arity: Exact(
                                        1,
                                    ),
                                    chunk: Chunk {
                                        upvalues: 0,
                                        constants: [
                                            Number(
                                                3,
                                            ),
                                            Number(
                                                2,
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
                                                            2,
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
                                                        PushConst {
                                                            index: 1,
                                                        },
                                                        Reference {
                                                            symbol: Spur(8),
                                                        },
                                                        Call {
                                                            args: 2,
                                                        },
                                                    ],
                                                    labels: {
                                                        0: SourceData {
                                                            source_id: Spur(3),
                                                            range: (
                                                                48,
                                                                55,
                                                            ),
                                                        },
                                                    },
                                                },
                                            },
                                        ],
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
                                            FetchArg {
                                                index: 0,
                                            },
                                            Define {
                                                symbol: Spur(9),
                                            },
                                            Reference {
                                                symbol: Spur(9),
                                            },
                                            Reference {
                                                symbol: Spur(9),
                                            },
                                            Reference {
                                                symbol: Spur(8),
                                            },
                                            Call {
                                                args: 2,
                                            },
                                        ],
                                        labels: {
                                            2: SourceData {
                                                source_id: Spur(3),
                                                range: (
                                                    69,
                                                    76,
                                                ),
                                            },
                                        },
                                    },
                                },
                            ],
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
                                FetchArg {
                                    index: 0,
                                },
                                Define {
                                    symbol: Spur(9),
                                },
                                FetchRest,
                                Define {
                                    symbol: Spur(10),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(9),
                                },
                                Reference {
                                    symbol: Spur(10),
                                },
                                Reference {
                                    symbol: Spur(8),
                                },
                                Call {
                                    args: 5,
                                },
                            ],
                            labels: {
                                4: SourceData {
                                    source_id: Spur(3),
                                    range: (
                                        97,
                                        113,
                                    ),
                                },
                            },
                        },
                    },
                ],
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
                    FetchArg {
                        index: 0,
                    },
                    Define {
                        symbol: Spur(9),
                    },
                    FetchArg {
                        index: 1,
                    },
                    Define {
                        symbol: Spur(11),
                    },
                    FetchArg {
                        index: 2,
                    },
                    Define {
                        symbol: Spur(12),
                    },
                    Reference {
                        symbol: Spur(14),
                    },
                    Reference {
                        symbol: Spur(9),
                    },
                    Reference {
                        symbol: Spur(11),
                    },
                    Reference {
                        symbol: Spur(12),
                    },
                    Reference {
                        symbol: Spur(8),
                    },
                    Call {
                        args: 3,
                    },
                    Reference {
                        symbol: Spur(13),
                    },
                    Call {
                        args: 2,
                    },
                    Reference {
                        symbol: Spur(14),
                    },
                ],
                labels: {
                    6: SourceData {
                        source_id: Spur(3),
                        range: (
                            131,
                            151,
                        ),
                    },
                    14: SourceData {
                        source_id: Spur(3),
                        range: (
                            152,
                            153,
                        ),
                    },
                },
            },
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
        PushLambda {
            index: 1,
        },
        PushLambda {
            index: 2,
        },
        PushLambda {
            index: 3,
        },
    ],
    labels: {
        0: SourceData {
            source_id: Spur(3),
            range: (
                37,
                56,
            ),
        },
        2: SourceData {
            source_id: Spur(3),
            range: (
                78,
                114,
            ),
        },
        1: SourceData {
            source_id: Spur(3),
            range: (
                57,
                77,
            ),
        },
        3: SourceData {
            source_id: Spur(3),
            range: (
                115,
                154,
            ),
        },
    },
}
---
(import (only (scheme base) lambda))
(lambda () (+ 3 2))
(lambda (a) (+ a a))
(lambda (a . rest) (+ a a a a rest))
(lambda (a b c) (define z (+ a b c)) z)
