//! Typo matcher to correct user errors

/// Checks all `possibles` (min 1) for the closest to `target`, returning a string if any are close
pub fn check<'a>(mut possibles: impl Iterator<Item = &'a str>, target: &str) -> String {
    // compare first possible as function needs at least one
    let mut best = compare(possibles.next().unwrap(), target);

    // iterate other possibles and compare, making `best` the max value
    for possible in possibles {
        best = best.max(compare(possible, target))
    }

    todo!()
}

fn compare(possible: &str, target: &str) {
    let matrix = gen_matrix(possible.len(), target.len());

    todo!()
}

/// Generates a matrix with filled first values going `01234..` for distance calculations
fn gen_matrix(possible_len: usize, target_len: usize) -> Vec<Vec<usize>> {
    // initialise detection matrix; x is target, y is possible
    let mut matrix = vec![vec![0; possible_len]; target_len];

    // generate `01234..` parts on either side of the matrix
    let x_len = matrix.len();
    for x in 0..x_len {
        if x == 0 {
            // populate first x column with y values
            let y_len = matrix[x].len();
            for y in 0..y_len {
                matrix[x][y] = y
            }
        } else {
            // populate first y row with x columns
            matrix[x][0] = x
        }
    }

    matrix
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_matrix_gen() {
        // 1x1
        assert_eq!(gen_matrix(1, 1), vec![vec![0]]);
        // 2x2
        assert_eq!(gen_matrix(2, 2), vec![vec![0, 1], vec![1, 0]]);
        // 4x3
        assert_eq!(
            gen_matrix(4, 3),
            vec![vec![0, 1, 2, 3], vec![1, 0, 0, 0], vec![2, 0, 0, 0]],
        );
    }
}
