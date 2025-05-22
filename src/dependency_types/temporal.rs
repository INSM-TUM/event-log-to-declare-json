use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TemporalDependency {
    pub from: String,
    pub to: String,
    pub dependency_type: DependencyType,
    pub direction: Direction,
}

impl TemporalDependency {
    pub fn new(
        from: &str,
        to: &str,
        dependency_type: DependencyType,
        direction: Direction,
    ) -> TemporalDependency {
        TemporalDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type,
            direction,
        }
    }
}

impl std::fmt::Display for TemporalDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let type_char = match self.dependency_type {
            DependencyType::Direct => "d",
            DependencyType::Eventual => "e",
        };
        match &self.direction {
            Direction::Forward => write!(f, "{} ≺{} {}", self.from, type_char, self.to),
            Direction::Backward => write!(f, "{} ≻{} {}", self.from, type_char, self.to),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Direction {
    Forward,
    Backward,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum DependencyType {
    Direct,
    Eventual,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DependencyType::Direct => write!(f, "d"),
            DependencyType::Eventual => write!(f, "e"),
        }
    }
}

/// Checks for temporal dependencies between two activities across multiple traces.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `traces`: A list of traces where each trace is an ordered sequence of activities.
/// - `threshold`: The ratio threshold for considering the dependency direction.
///    (for example, a threshold of 0.8 would mean that the dependency would be considered
///    a Direct dependency if it is found in at least 80% of the traces)
///
/// # Returns
/// An `Option` containing the `TemporalDependency` if a dependency is found; otherwise, `None`.
pub fn check_temporal_dependency(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<TemporalDependency> {
    let mut all_observed_relations: Vec<(DependencyType, Direction)> = Vec::new();

    for trace in traces {
        let trace_relations = check_trace_dependency_pairs(from, to, trace);
        all_observed_relations.extend(trace_relations);
    }
    
    if all_observed_relations.is_empty() {
        return None;
    }

    classify_dependencies(from, to, all_observed_relations, threshold)
}


/// Checks the dependencies between two activities within a single trace
/// by considering all occurrences of `from` and `to`.
/// This version iterates through `from` occurrences and finds the closest `to`.
fn check_trace_dependency_pairs(
    from_activity: &str,
    to_activity: &str,
    trace: &[&str],
) -> Vec<(DependencyType, Direction)> {
    let mut observed_relations = Vec::new();
    let from_indices: Vec<usize> = trace
        .iter()
        .enumerate()
        .filter(|(_, &act)| act == from_activity)
        .map(|(i, _)| i)
        .collect();
    let to_indices: Vec<usize> = trace
        .iter()
        .enumerate()
        .filter(|(_, &act)| act == to_activity)
        .map(|(i, _)| i)
        .collect();

    if from_activity == to_activity {
        // Handle self-loops: A -> A
        for i in 0..from_indices.len() {
            for j in (i + 1)..from_indices.len() {
                let idx1 = from_indices[i];
                let idx2 = from_indices[j];
                // All self-loops are forward for A -> A
                let dep_type = if idx2 == idx1 + 1 {
                    DependencyType::Direct
                } else {
                    DependencyType::Eventual
                };
                observed_relations.push((dep_type, Direction::Forward));
            }
        }
        return observed_relations;
    }

    // For each occurrence of `from_activity`, find its relation to `to_activity`
    for &from_idx in &from_indices {
        let mut best_forward: Option<(DependencyType, usize)> = None; // type, distance
        let mut best_backward: Option<(DependencyType, usize)> = None; // type, distance

        for &to_idx in &to_indices {
            if to_idx > from_idx { // Potential forward dependency
                let distance = to_idx - from_idx;
                let dep_type = if distance == 1 { DependencyType::Direct } else { DependencyType::Eventual };
                if best_forward.map_or(true, |(_, d)| distance < d) {
                    best_forward = Some((dep_type, distance));
                }
            } else if to_idx < from_idx { // Potential backward dependency
                let distance = from_idx - to_idx;
                let dep_type = if distance == 1 { DependencyType::Direct } else { DependencyType::Eventual };
                 if best_backward.map_or(true, |(_, d)| distance < d) {
                    best_backward = Some((dep_type, distance));
                }
            }
        }
    }

    let mut from_ptr = 0;
    let mut to_ptr = 0;

    while from_ptr < from_indices.len() && to_ptr < to_indices.len() {
        let from_pos = from_indices[from_ptr];
        let to_pos = to_indices[to_ptr];

        match from_pos.cmp(&to_pos) {
            Ordering::Less => { // from_pos < to_pos (Forward: from -> to)
                let dep_type = if to_pos - from_pos == 1 {
                    DependencyType::Direct
                } else {
                    DependencyType::Eventual
                };
                observed_relations.push((dep_type, Direction::Forward));
                // Consume both from and to for this pair
                from_ptr += 1;
                to_ptr += 1; 
            }
            Ordering::Greater => { // from_pos > to_pos (Backward: to -> from)
                let dep_type = if from_pos - to_pos == 1 {
                    DependencyType::Direct
                } else {
                    DependencyType::Eventual
                };
                observed_relations.push((dep_type, Direction::Backward));
                // Consume only 'to' as 'from' might pair with a later 'to'
                to_ptr += 1;
            }
            Ordering::Equal => { 
                // This should not happen if from_activity != to_activity
                // If they are the same, self-loop logic above handles it.
                // Advance both to avoid infinite loop, though this state is unexpected.
                from_ptr += 1;
                to_ptr += 1;
            }
        }
    }
    observed_relations
}


/// Classifies the dependencies based on their ratio to determine the overall dependency.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `dependencies`: A vector of dependencies found in the traces.
/// - `threshold`: The ratio threshold for determining the direction of the dependency.
///
/// # Returns
/// An `Option` containing the `TemporalDependency` if a dependency direction meets the threshold; otherwise, `None`.
fn classify_dependencies(
    from: &str,
    to: &str,
    dependencies: Vec<(DependencyType, Direction)>,
    threshold: f64,
) -> Option<TemporalDependency> {
    if dependencies.is_empty() {
        return None;
    }

    let total_observed_relations = dependencies.len() as f64;
    
    let forward_relations_count = dependencies
        .iter()
        .filter(|(_, dir)| *dir == Direction::Forward)
        .count();
    
    let backward_relations_count = dependencies
        .iter()
        .filter(|(_, dir)| *dir == Direction::Backward)
        .count();

    let forward_ratio = forward_relations_count as f64 / total_observed_relations;
    let backward_ratio = backward_relations_count as f64 / total_observed_relations;

    let determined_direction;
    let relevant_dependencies_for_type_check: Vec<(DependencyType, Direction)>;

    if forward_ratio >= threshold && backward_ratio < threshold { // Clear forward
        determined_direction = Direction::Forward;
        relevant_dependencies_for_type_check = dependencies.into_iter().filter(|(_, dir)| *dir == Direction::Forward).collect();
    } else if backward_ratio >= threshold && forward_ratio < threshold { // Clear backward
        determined_direction = Direction::Backward;
        relevant_dependencies_for_type_check = dependencies.into_iter().filter(|(_, dir)| *dir == Direction::Backward).collect();
    } else {
        // Ambiguous or no dominant direction above threshold
        return None; 
    }
    
    // If we have a clear direction, determine type (Direct or Eventual)
    // Based on the relations that support the determined_direction.
    // If any of these are Eventual, the overall is Eventual. Otherwise, it's Direct.
    let is_eventual = relevant_dependencies_for_type_check
        .iter()
        .any(|(dep_type, _)| *dep_type == DependencyType::Eventual);

    let final_dependency_type = if is_eventual {
        DependencyType::Eventual
    } else {
        DependencyType::Direct // All supporting relations must have been Direct
    };

    Some(TemporalDependency::new(
        from,
        to,
        final_dependency_type,
        determined_direction,
    ))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temporal_basic_direct_forward() {
        let traces = vec![vec!["A", "B", "C"]];
        let dep = check_temporal_dependency("A", "B", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "B",
                DependencyType::Direct,
                Direction::Forward
            ))
        );
    }

    #[test]
    fn test_temporal_basic_eventual_forward() {
        let traces = vec![vec!["A", "X", "B", "C"]];
        let dep = check_temporal_dependency("A", "B", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "B",
                DependencyType::Eventual,
                Direction::Forward
            ))
        );
    }
    
    #[test]
    fn test_temporal_basic_direct_backward() {
        let traces = vec![vec!["B", "A", "C"]];
        let dep = check_temporal_dependency("A", "B", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A", // 'from' is still A
                "B", // 'to' is still B
                DependencyType::Direct,
                Direction::Backward // B precedes A
            ))
        );
    }

    #[test]
    fn test_temporal_mixed_direction_dominant_forward() {
        let traces = vec![
            vec!["A", "B"], // Fwd, Direct
            vec!["A", "X", "B"], // Fwd, Eventual
            vec!["B", "A"], // Bwd, Direct
        ];
        // Observed: (D,F), (E,F), (D,B)
        // Forward count: 2, Backward count: 1. Total: 3
        // Fwd ratio: 2/3 = 0.66. Bwd ratio: 1/3 = 0.33
        // With threshold 0.6: Forward wins.
        // Types for forward: Direct, Eventual. So, Eventual.
        let dep = check_temporal_dependency("A", "B", &traces, 0.6);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "B",
                DependencyType::Eventual, // Because one forward relation is Eventual
                Direction::Forward
            ))
        );
    }
    
    #[test]
    fn test_temporal_mixed_direction_no_dominant() {
        let traces = vec![
            vec!["A", "B"], // Fwd
            vec!["B", "A"], // Bwd
        ];
        // Fwd ratio 0.5, Bwd ratio 0.5.
        // With threshold 0.6, neither wins.
        let dep = check_temporal_dependency("A", "B", &traces, 0.6);
        assert_eq!(dep, None);
    }

    #[test]
    fn test_temporal_self_loop_direct() {
        let traces = vec![vec!["A", "A", "B"]];
        let dep = check_temporal_dependency("A", "A", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "A",
                DependencyType::Direct,
                Direction::Forward
            ))
        );
    }

    #[test]
    fn test_temporal_self_loop_eventual() {
        let traces = vec![vec!["A", "X", "A", "B"]];
        let dep = check_temporal_dependency("A", "A", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "A",
                DependencyType::Eventual,
                Direction::Forward
            ))
        );
    }
    
    #[test]
    fn test_temporal_multiple_occurrences_complex_pairing() {
        // Old test: test_with_loop_1
        // vec!["A", "B", "C", "A", "C"] for A, C
        // A at 0, C at 2: (Eventual, Fwd) -> Pair (A0, C2)
        // A at 3, C at 4: (Direct, Fwd)   -> Pair (A3, C4)
        // Result: Eventual, Forward (because one of the pairs is Eventual)
        let traces = vec![vec!["A", "B", "C", "A", "C"]];
        let dep = check_temporal_dependency("A", "C", &traces, 0.9);
        assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "C",
                DependencyType::Eventual, 
                Direction::Forward
            ))
        );
    }

    #[test]
    fn test_temporal_independence_from_old_test() {
        // Old test: test_independence
        // vec!["A", "B", "C", "C", "A"] for A, C
        // A at 0, C at 2: (Eventual, Fwd) -> Pair (A0, C2)
        // C at 3, A at 4: (Direct, Bwd) for (A,C) relation, i.e. C -> A is Direct. -> Pair (A4, C3)
        // Observed relations: (E,F), (D,B)
        // Fwd ratio: 0.5, Bwd ratio: 0.5. Threshold 0.9 (or 1.0 in old) -> None
        let traces = vec![vec!["A", "B", "C", "C", "A"]];
        let dep = check_temporal_dependency("A", "C", &traces, 0.9);
        assert_eq!(dep, None);
    }

     #[test]
    fn test_check_trace_dependency_pairs_forward_backward() {
        let trace = vec!["C", "A", "X", "C", "Y", "A", "C"];
        let relations = check_trace_dependency_pairs("A", "C", &trace);
        assert_eq!(relations, vec![
            (DependencyType::Direct, Direction::Backward),
            (DependencyType::Eventual, Direction::Forward),
            (DependencyType::Direct, Direction::Forward)
        ]);

        let dep = classify_dependencies("A", "C", relations, 0.6);
         assert_eq!(
            dep,
            Some(TemporalDependency::new(
                "A",
                "C",
                DependencyType::Eventual,
                Direction::Forward
            ))
        );
    }
}