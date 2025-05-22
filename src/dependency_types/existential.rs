#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExistentialDependency {
    pub from: String,
    pub to: String,
    pub dependency_type: DependencyType,
    pub direction: Direction,
}

impl ExistentialDependency {
    pub fn new(
        from: &str,
        to: &str,
        dependency_type: DependencyType,
        direction: Direction,
    ) -> Self {
        ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type,
            direction,
        }
    }
}

impl std::fmt::Display for ExistentialDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.dependency_type {
            DependencyType::Implication => match self.direction {
                Direction::Forward => write!(f, "{} => {}", self.from, self.to),
                Direction::Backward => write!(f, "{} <= {}", self.from, self.to),
                Direction::Both => write!(f, "{} <=> {} (Implication-Both?)", self.from, self.to), // Should not happen for implication
            },
            DependencyType::Equivalence => write!(f, "{} <=> {}", self.from, self.to),
            DependencyType::NegatedEquivalence => write!(f, "{} <~> {}", self.from, self.to),
            DependencyType::Nand => write!(f, "{} NAND {}", self.from, self.to),
            DependencyType::Or => write!(f, "{} OR {}", self.from, self.to),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Direction {
    Forward,
    Backward,
    Both,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Copy)]
pub enum DependencyType {
    Implication,
    Equivalence,
    NegatedEquivalence,
    Nand,
    Or,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DependencyType::Implication => write!(f, "⇒"),
            DependencyType::Equivalence => write!(f, "⇔"),
            DependencyType::NegatedEquivalence => write!(f, "⇎"),
            DependencyType::Nand => write!(f, "⊼"),
            DependencyType::Or => write!(f, "∨"),
        }
    }
}

// TODO: NAND and OR dependencies
/// Checks for an existential dependency between two activities within a set of traces.
///
/// This function analyzes the given traces to determine if there is an existential dependency
/// between the `from` and `to` activities based on the specified threshold. It considers
/// implications, equivalences, and negated equivalences to identify the type and direction
/// of the dependency (in that order).
///
/// # Arguments
///
/// * `from` - The name of the starting activity.
/// * `to` - The name of the target activity.
/// * `traces` - A vector of `Trace` objects representing the sequence of events.
/// * `threshold` - A threshold value to determine if the dependency is significant.
///
/// # Returns
///
/// An `Option` containing an `ExistentialDependency` if a dependency is found, otherwise `None`.
pub fn check_existential_dependency(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<ExistentialDependency> {
    assert!(
        (0.0..=1.0).contains(&threshold),
        "Threshold must be between 0 and 1"
    );

    // Prevent issues with empty traces or traces without activities
    let relevant_traces: Vec<_> = traces
        .iter()
        .filter(|trace| trace.contains(&from) || trace.contains(&to))
        .collect();

    if relevant_traces.is_empty() && !traces.is_empty() { // if traces exist, but none are relevant
        return None;
    }

    let from_implies_to = has_implication(from, to, traces, threshold);
    let to_implies_from = has_implication(to, from, traces, threshold);

    if from_implies_to && to_implies_from {
        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: DependencyType::Equivalence,
            direction: Direction::Both, // Equivalence is inherently bidirectional
        });
    } else if from_implies_to {
        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: DependencyType::Implication,
            direction: Direction::Forward,
        });
    } else if to_implies_from {
        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: DependencyType::Implication,
            direction: Direction::Backward,
        });
    }

    // Check for Negated Equivalence if no implication/equivalence was found
    // Negated Equivalence: (A and not B) or (not A and B)
    // This means if A exists, B must not, AND if B exists, A must not, within traces containing either.
    if negated_equivalence(from, to, traces, threshold) {
        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: DependencyType::NegatedEquivalence,
            direction: Direction::Both,
        });
    }
    
    None
}

/// Checks if there is an implication relationship between two events within a set of event traces.
/// `from` => `to`
/// # Parameters
/// - `from`: The event that implies the occurrence of another event.
/// - `to`: The event that is implied by the occurrence of the `from` event.
/// - `event_names`: A vector of vectors, where each inner vector represents a sequence of event names (a trace).
/// - `threshold`: A threshold value between 0 and 1 that determines the minimum proportion of valid traces required to confirm the implication.
///
/// # Returns
/// - `true` if the proportion of valid traces (traces where `from` appears must also contain `to`) is greater than or equal to the threshold.
/// - `false` otherwise.
fn has_implication(from: &str, to: &str, event_names: &[Vec<&str>], threshold: f64) -> bool {
    let traces_with_from: Vec<_> = event_names.iter().filter(|trace| trace.contains(&from)).collect();

    if traces_with_from.is_empty() {
        return true;
    }

    let valid_traces_count = traces_with_from
        .iter()
        .filter(|trace| trace.contains(&to))
        .count();
    
    (valid_traces_count as f64 / traces_with_from.len() as f64) >= threshold
}


/// Checks for negated equivalence.
/// (A and not B) or (not A and B) -- meaning they don't appear together.
/// This is checked over traces that contain *at least one* of A or B.
fn negated_equivalence(from: &str, to: &str, event_names: &[Vec<&str>], threshold: f64) -> bool {
    let relevant_traces: Vec<_> = event_names
        .iter()
        .filter(|trace| trace.contains(&from) || trace.contains(&to))
        .collect();

    if relevant_traces.is_empty() {
        return true;
    }

    let valid_traces_count = relevant_traces
        .iter()
        .filter(|trace| {
            let from_present = trace.contains(&from);
            let to_present = trace.contains(&to);
            // Valid if (from is present AND to is NOT present) OR (from is NOT present AND to IS present)
            (from_present && !to_present) || (!from_present && to_present)
        })
        .count();

    (valid_traces_count as f64 / relevant_traces.len() as f64) >= threshold
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_implication_basic() {
        let traces = vec![
            vec!["A", "B"], // A -> B holds
            vec!["A", "C"], // A -> B fails
            vec!["B", "C"], // A not present, vacuously true by original logic, but `traces_with_from` filters this.
            vec!["A", "B", "D"], // A -> B holds
        ];
        // Traces with A: ["A", "B"], ["A", "C"], ["A", "B", "D"] (3 traces)
        // Traces with A and B: ["A", "B"], ["A", "B", "D"] (2 traces)
        // Ratio: 2/3 = 0.666...
        assert!(has_implication("A", "B", &traces, 0.6));
        assert!(!has_implication("A", "B", &traces, 0.7));
    }
    
    #[test]
    fn test_has_implication_from_not_present() {
        let traces = vec![vec!["X", "Y"], vec!["Z"]];
        // 'A' is never present. traces_with_from is empty.
        assert!(has_implication("A", "B", &traces, 1.0));
    }

    #[test]
    fn test_check_existential_dependency_implication() {
        let traces = vec![vec!["A", "B"], vec!["A"]]; // A exists, B sometimes. B never implies A.
        // A->B : A appears in 2. B appears with A in 1. Ratio 1/2 = 0.5
        // B->A : B appears in 1. A appears with B in 1. Ratio 1/1 = 1.0
        // Expected: B Implies A (Backward for A,B pair)
        let dep_ab = check_existential_dependency("A", "B", &traces, 0.6); // B=>A (0.6 < 1.0)
        assert_eq!(dep_ab, Some(ExistentialDependency::new("A", "B", DependencyType::Implication, Direction::Backward)));

        let dep_ba = check_existential_dependency("B", "A", &traces, 0.6); // B=>A (0.6 < 1.0)
        assert_eq!(dep_ba, Some(ExistentialDependency::new("B", "A", DependencyType::Implication, Direction::Forward)));
    }

    #[test]
    fn test_check_existential_dependency_equivalence() {
        let traces = vec![vec!["A", "B"], vec!["A", "B"], vec!["C"]];
        // A->B: A in 2, A&B in 2. Ratio 1.0
        // B->A: B in 2, B&A in 2. Ratio 1.0
        let dep = check_existential_dependency("A", "B", &traces, 0.9);
        assert_eq!(dep, Some(ExistentialDependency::new("A", "B", DependencyType::Equivalence, Direction::Both)));
    }

    #[test]
    fn test_check_existential_dependency_negated_equivalence() {
        let traces = vec![vec!["A", "C"], vec!["B", "D"], vec!["A", "E"], vec!["B", "F"]];
        // Relevant for A,B: ["A","C"], ["B","D"], ["A","E"], ["B","F"]
        // Trace 1: A yes, B no (ok for NE)
        // Trace 2: A no, B yes (ok for NE)
        // Trace 3: A yes, B no (ok for NE)
        // Trace 4: A no, B yes (ok for NE)
        // All 4 relevant traces satisfy NE. Ratio 4/4 = 1.0
        let dep = check_existential_dependency("A", "B", &traces, 0.9);
        assert_eq!(dep, Some(ExistentialDependency::new("A", "B", DependencyType::NegatedEquivalence, Direction::Both)));
    }
    
    #[test]
    fn test_check_existential_dependency_none() {
        let traces = vec![vec!["A", "B"], vec!["A"], vec!["B"], vec!["C"]];
        // A->B: A in ["A","B"], ["A"]. B with A in ["A","B"]. Ratio 1/2 = 0.5
        // B->A: B in ["A","B"], ["B"]. A with B in ["A","B"]. Ratio 1/2 = 0.5
        // For threshold 0.6, neither implication holds.
        // Negated Equivalence check:
        // Relevant: ["A","B"], ["A"], ["B"]
        // ["A","B"]: A yes, B yes (FAILS NE)
        // ["A"]: A yes, B no (ok for NE)
        // ["B"]: A no, B yes (ok for NE)
        // NE holds for 2/3 relevant traces = 0.66
        let dep = check_existential_dependency("A", "B", &traces, 0.6);
         assert_eq!(dep, Some(ExistentialDependency::new("A", "B", DependencyType::NegatedEquivalence, Direction::Both)));

        // If threshold is higher, e.g., 0.7, NE should fail
        let dep_higher_thresh = check_existential_dependency("A", "B", &traces, 0.7);
        assert_eq!(dep_higher_thresh, None);
    }
}