use std::collections::{HashMap, HashSet};

use crate::dependency_types::{
    dependency::Dependency,
    existential::check_existential_dependency,
    temporal::check_temporal_dependency,
};

pub type Activity = String;
pub type InputMatrix = HashMap<(Activity, Activity), Dependency>;

pub fn generate_dependency_matrix(
    traces: &[Vec<String>],
    existential_threshold: f64,
    temporal_threshold: f64,
) -> InputMatrix {
    let mut matrix: InputMatrix = HashMap::new();

    if traces.is_empty() {
        return matrix;
    }

    let activities_set: HashSet<Activity> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();
    
    let activities_sorted: Vec<&Activity> = {
        let mut sorted: Vec<&Activity> = activities_set.iter().collect();
        sorted.sort();
        sorted
    };

    let traces_str: Vec<Vec<&str>> = traces
        .iter()
        .map(|trace| trace.iter().map(|s| s.as_str()).collect())
        .collect();

    for from_activity_ref in &activities_sorted {
        for to_activity_ref in &activities_sorted {
            let from_activity = (*from_activity_ref).clone();
            let to_activity = (*to_activity_ref).clone();

            // Skip self-loops for now
            if from_activity == to_activity {
                continue;
            }

            let temporal_dependency = check_temporal_dependency(
                &from_activity,
                &to_activity,
                &traces_str,
                temporal_threshold,
            );

            let existential_dependency = check_existential_dependency(
                &from_activity,
                &to_activity,
                &traces_str,
                existential_threshold,
            );

            let dependency_obj = Dependency::new(
                from_activity.clone(),
                to_activity.clone(),
                temporal_dependency,
                existential_dependency,
            );
            
            matrix.insert((from_activity, to_activity), dependency_obj);
        }
    }
    matrix
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dependency_types::{
        temporal::{DependencyType as TempEnum, Direction as TempDir},
        existential::{DependencyType as ExisEnum, Direction as ExisDir},
    };

    #[test]
    fn test_generate_simple_matrix_no_self_loops() {
        let traces = vec![
            vec!["A".to_string(), "B".to_string()],
            vec!["A".to_string(), "B".to_string()],
        ];
        let matrix = generate_dependency_matrix(&traces, 1.0, 1.0);

        assert_eq!(matrix.len(), 2); 

        // Check A->B: Should be Direct Forward Temporal, Equivalence Existential
        let dep_ab = matrix.get(&("A".to_string(), "B".to_string())).unwrap();
        assert_eq!(dep_ab.temporal_dependency.as_ref().unwrap().dependency_type, TempEnum::Direct);
        assert_eq!(dep_ab.temporal_dependency.as_ref().unwrap().direction, TempDir::Forward);
        assert_eq!(dep_ab.existential_dependency.as_ref().unwrap().dependency_type, ExisEnum::Equivalence);
        assert_eq!(dep_ab.existential_dependency.as_ref().unwrap().direction, ExisDir::Both);
        
        // Check B->A:
        // Temporal for B,A on ["A","B"]: A before B. So B->A is Backward Direct.
        // Existential for B,A on ["A","B"]: B appears, A appears with it. A appears, B appears with it. Equivalence.
        let dep_ba = matrix.get(&("B".to_string(), "A".to_string())).unwrap();
        assert_eq!(dep_ba.temporal_dependency.as_ref().unwrap().dependency_type, TempEnum::Direct);
        assert_eq!(dep_ba.temporal_dependency.as_ref().unwrap().direction, TempDir::Backward);
        assert_eq!(dep_ba.existential_dependency.as_ref().unwrap().dependency_type, ExisEnum::Equivalence);
        assert_eq!(dep_ba.existential_dependency.as_ref().unwrap().direction, ExisDir::Both);
    }

    #[test]
    fn test_generate_matrix_with_eventual_and_implication_no_self_loops() {
        let traces = vec![
            vec!["X".to_string(), "Y".to_string(), "Z".to_string()], 
            vec!["X".to_string(), "Z".to_string()],                 
        ];
        let matrix = generate_dependency_matrix(&traces, 1.0, 1.0);
        // Activities: X, Y, Z. Pairs (excluding self-loops):
        // (X,Y), (X,Z)
        // (Y,X), (Y,Z)
        // (Z,X), (Z,Y)
        // Total 6 pairs.
        assert_eq!(matrix.len(), 6); 

        let dep_xz = matrix.get(&("X".to_string(), "Z".to_string())).unwrap();
        assert_eq!(dep_xz.temporal_dependency.as_ref().unwrap().dependency_type, TempEnum::Eventual);
        assert_eq!(dep_xz.temporal_dependency.as_ref().unwrap().direction, TempDir::Forward);
        assert_eq!(dep_xz.existential_dependency.as_ref().unwrap().dependency_type, ExisEnum::Equivalence);
    }

    #[test]
    fn test_empty_traces_no_self_loops() {
        let traces: Vec<Vec<String>> = Vec::new();
        let matrix = generate_dependency_matrix(&traces, 1.0, 1.0);
        assert!(matrix.is_empty());
    }
}