use crate::{error::Error, ProcedureRecord};
use std::io::BufRead;

/// Parses records.
pub fn parse_raw_records(
    reader: impl BufRead,
) -> impl Iterator<Item = Result<ProcedureRecord, Error>> {
    reader
        .lines()
        .map(|line| -> Result<ProcedureRecord, Error> {
            let mut record = line?.parse::<ProcedureRecord>()?;
            record.stack_mut().reverse_frames();
            Ok(record)
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ProcedureOperation, Stack};
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::io::BufReader;

    #[test]
    fn parse_record() {
        assert_eq!(
            parse_raw_records(BufReader::new(
                indoc!(
                    "
                    call\tfoo;bar;baz\t0
                    return\tfoo;bar;baz\t42
                    "
                )
                .trim()
                .as_bytes()
            ))
            .collect::<Vec<_>>(),
            vec![
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Call,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into())
                    ]),
                    0
                )),
                Ok(ProcedureRecord::new(
                    ProcedureOperation::Return,
                    Stack::new(vec![
                        Some("baz".into()),
                        Some("bar".into()),
                        Some("foo".into())
                    ]),
                    42
                ))
            ]
        );
    }
}
