use crate::code::{Instructions, InstructionsExt, Opcode, make};

#[test]
fn test_make() {
    #[derive(Debug)]
    struct TestCase {
        op: Opcode,
        operands: Vec<i32>,
        expected: Vec<u8>,
    }

    let tests = vec![TestCase {
        op: Opcode::OpConstant,
        operands: vec![65534],
        expected: vec![Opcode::OpConstant as u8, 255, 254],
    },
    TestCase {
        op: Opcode::OpAdd,
        operands: vec![],
        expected: vec![Opcode::OpAdd as u8]
    }];

    for test in tests {
        let instruction = make(test.op, &test.operands);

        assert_eq!(
            instruction.len(),
            test.expected.len(),
            "instruction has wrong length"
        );

        for (i, byte) in test.expected.iter().enumerate() {
            assert_eq!(
                instruction[i], test.expected[i],
                "wrong byte at pos {}. want {}, got {}",
                i, byte, instruction[i]
            );
        }
    }
}

#[test]
fn test_instructions_string() {
    let instructions = [
        make(Opcode::OpAdd, &[]),
        make(Opcode::OpConstant, &[2]),
        make(Opcode::OpConstant, &[65535]),
    ];

    let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;

    let concatted: Instructions = instructions.into_iter().flatten().collect();

    assert_eq!(
        concatted.string(),
        expected,
        "instructions wrongly formatted"
    );
}

