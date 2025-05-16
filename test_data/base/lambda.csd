
---
; all the arities are wrong currently
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
                2,
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
                                2,
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
                            index: 0,
                        },
                        PushConst {
                            index: 1,
                        },
                        Reference {
                            symbol: Spur(8),
                            enable_fallback: false,
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
        Compiled(
            CompiledLambda {
                arity: Exact(
                    1,
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
                                2,
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
                                                2,
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
                                            index: 0,
                                        },
                                        PushConst {
                                            index: 1,
                                        },
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
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
                        FetchArg {
                            index: 0,
                        },
                        Reference {
                            symbol: Spur(8),
                            enable_fallback: false,
                        },
                        Call {
                            args: 2,
                        },
                    ],
                    labels: {
                        0: SourceData {
                            source_id: Spur(3),
                            range: (
                                69,
                                76,
                            ),
                        },
                    },
                    fallback: None,
                },
                arg_names: [
                    Spur(9),
                ],
                rest_name: None,
                upvalue_id: None,
                doc_string: RefLock(
                    None,
                ),
            },
        ),
        Compiled(
            CompiledLambda {
                arity: AtLeast(
                    1,
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
                                2,
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
                                                2,
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
                                            index: 0,
                                        },
                                        PushConst {
                                            index: 1,
                                        },
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
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
                        Compiled(
                            CompiledLambda {
                                arity: Exact(
                                    1,
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
                                                2,
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
                                                                2,
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
                                                            index: 0,
                                                        },
                                                        PushConst {
                                                            index: 1,
                                                        },
                                                        Reference {
                                                            symbol: Spur(8),
                                                            enable_fallback: false,
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
                                        FetchArg {
                                            index: 0,
                                        },
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
                                        },
                                        Call {
                                            args: 2,
                                        },
                                    ],
                                    labels: {
                                        0: SourceData {
                                            source_id: Spur(3),
                                            range: (
                                                69,
                                                76,
                                            ),
                                        },
                                    },
                                    fallback: None,
                                },
                                arg_names: [
                                    Spur(9),
                                ],
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
                        FetchArg {
                            index: 0,
                        },
                        FetchArg {
                            index: 0,
                        },
                        FetchArg {
                            index: 0,
                        },
                        FetchRest,
                        Reference {
                            symbol: Spur(8),
                            enable_fallback: false,
                        },
                        Call {
                            args: 5,
                        },
                    ],
                    labels: {
                        0: SourceData {
                            source_id: Spur(3),
                            range: (
                                97,
                                113,
                            ),
                        },
                    },
                    fallback: None,
                },
                arg_names: [
                    Spur(9),
                ],
                rest_name: Some(
                    Spur(10),
                ),
                upvalue_id: None,
                doc_string: RefLock(
                    None,
                ),
            },
        ),
        Compiled(
            CompiledLambda {
                arity: Exact(
                    3,
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
                                2,
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
                                                2,
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
                                            index: 0,
                                        },
                                        PushConst {
                                            index: 1,
                                        },
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
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
                        Compiled(
                            CompiledLambda {
                                arity: Exact(
                                    1,
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
                                                2,
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
                                                                2,
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
                                                            index: 0,
                                                        },
                                                        PushConst {
                                                            index: 1,
                                                        },
                                                        Reference {
                                                            symbol: Spur(8),
                                                            enable_fallback: false,
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
                                        FetchArg {
                                            index: 0,
                                        },
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
                                        },
                                        Call {
                                            args: 2,
                                        },
                                    ],
                                    labels: {
                                        0: SourceData {
                                            source_id: Spur(3),
                                            range: (
                                                69,
                                                76,
                                            ),
                                        },
                                    },
                                    fallback: None,
                                },
                                arg_names: [
                                    Spur(9),
                                ],
                                rest_name: None,
                                upvalue_id: None,
                                doc_string: RefLock(
                                    None,
                                ),
                            },
                        ),
                        Compiled(
                            CompiledLambda {
                                arity: AtLeast(
                                    1,
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
                                                2,
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
                                                                2,
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
                                                            index: 0,
                                                        },
                                                        PushConst {
                                                            index: 1,
                                                        },
                                                        Reference {
                                                            symbol: Spur(8),
                                                            enable_fallback: false,
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
                                        Compiled(
                                            CompiledLambda {
                                                arity: Exact(
                                                    1,
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
                                                                2,
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
                                                                                2,
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
                                                                            index: 0,
                                                                        },
                                                                        PushConst {
                                                                            index: 1,
                                                                        },
                                                                        Reference {
                                                                            symbol: Spur(8),
                                                                            enable_fallback: false,
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
                                                        FetchArg {
                                                            index: 0,
                                                        },
                                                        Reference {
                                                            symbol: Spur(8),
                                                            enable_fallback: false,
                                                        },
                                                        Call {
                                                            args: 2,
                                                        },
                                                    ],
                                                    labels: {
                                                        0: SourceData {
                                                            source_id: Spur(3),
                                                            range: (
                                                                69,
                                                                76,
                                                            ),
                                                        },
                                                    },
                                                    fallback: None,
                                                },
                                                arg_names: [
                                                    Spur(9),
                                                ],
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
                                        FetchArg {
                                            index: 0,
                                        },
                                        FetchArg {
                                            index: 0,
                                        },
                                        FetchArg {
                                            index: 0,
                                        },
                                        FetchRest,
                                        Reference {
                                            symbol: Spur(8),
                                            enable_fallback: false,
                                        },
                                        Call {
                                            args: 5,
                                        },
                                    ],
                                    labels: {
                                        0: SourceData {
                                            source_id: Spur(3),
                                            range: (
                                                97,
                                                113,
                                            ),
                                        },
                                    },
                                    fallback: None,
                                },
                                arg_names: [
                                    Spur(9),
                                ],
                                rest_name: Some(
                                    Spur(10),
                                ),
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
                        Reference {
                            symbol: Spur(14),
                            enable_fallback: false,
                        },
                        FetchArg {
                            index: 0,
                        },
                        FetchArg {
                            index: 1,
                        },
                        FetchArg {
                            index: 2,
                        },
                        Reference {
                            symbol: Spur(8),
                            enable_fallback: false,
                        },
                        Call {
                            args: 3,
                        },
                        Reference {
                            symbol: Spur(13),
                            enable_fallback: false,
                        },
                        Call {
                            args: 2,
                        },
                        Reference {
                            symbol: Spur(14),
                            enable_fallback: false,
                        },
                    ],
                    labels: {
                        0: SourceData {
                            source_id: Spur(3),
                            range: (
                                131,
                                151,
                            ),
                        },
                        8: SourceData {
                            source_id: Spur(3),
                            range: (
                                152,
                                153,
                            ),
                        },
                    },
                    fallback: None,
                },
                arg_names: [
                    Spur(9),
                    Spur(11),
                    Spur(12),
                ],
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
    fallback: None,
}
---
(import (only (scheme base) lambda))
(lambda () (+ 3 2))
(lambda (a) (+ a a))
(lambda (a . rest) (+ a a a a rest))
(lambda (a b c) (define z (+ a b c)) z)
