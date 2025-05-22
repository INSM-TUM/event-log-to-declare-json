use std::collections::HashSet;
use serde::Serialize;

use crate::matrix_generation::{Activity, InputMatrix};
use crate::dependency_types::{
    dependency::Dependency,
    temporal::{DependencyType as TempEnum, Direction as TempDir},
    existential::{DependencyType as ExisEnum, Direction as ExisDir},
};

#[derive(Serialize, Debug, Clone)]
pub struct DeclareTask {
    pub name: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct DeclareConstraint {
    pub template: String,
    pub parameters: Vec<Vec<String>>,
}

 #[derive(Serialize, Debug)]
pub struct DeclareModel {
    pub name: String,
    pub tasks: Vec<DeclareTask>,
    pub constraints: Vec<DeclareConstraint>,
}

pub fn matrix_to_declare_model(
    matrix: &InputMatrix,
    all_activities: &HashSet<Activity>,
    model_name: &str,
) -> DeclareModel {
    let mut tasks: Vec<DeclareTask> = all_activities
        .iter()
        .map(|act_name| DeclareTask { name: act_name.clone() })
        .collect();
    tasks.sort_by(|a, b| a.name.cmp(&b.name)); // Consistent ordering for tests

    let mut constraints: Vec<DeclareConstraint> = Vec::new();

    let activity_list: Vec<Activity> = all_activities.iter().cloned().collect();

    // Rule 1: Init(a)
    for act_a in &activity_list {
        let mut is_init = true;
        if all_activities.len() <= 1 { // Init doesn't make sense for 0 or 1 activity
            is_init = false;
        }
        for act_b in &activity_list {
            if act_a == act_b {
                continue;
            }
            match matrix.get(&(act_a.clone(), act_b.clone())) {
                Some(dep) => {
                    if let Some(td) = &dep.temporal_dependency {
                        if !(td.dependency_type == TempEnum::Eventual && td.direction == TempDir::Forward) {
                            is_init = false;
                            break;
                        }
                    } else {
                        is_init = false; // Must have temporal dependency
                        break;
                    }
                }
                None => { // If a pair is missing, it cannot satisfy the condition for all others
                    is_init = false;
                    break;
                }
            }
        }
        if is_init {
            constraints.push(DeclareConstraint {
                template: "Init".to_string(),
                parameters: vec![vec![act_a.clone()]],
            });
        }
    }

    // End(a) - symmetric to Init
    for act_a in &activity_list {
        let mut is_end = true;
        if all_activities.len() <= 1 {
            is_end = false;
        }
        for act_b in &activity_list {
            if act_a == act_b {
                continue;
            }
            // Check (b,a) for b -> a temporal eventual
            match matrix.get(&(act_b.clone(), act_a.clone())) {
                Some(dep) => {
                    if let Some(td) = &dep.temporal_dependency {
                        if !(td.dependency_type == TempEnum::Eventual && td.direction == TempDir::Forward) {
                            is_end = false;
                            break;
                        }
                    } else {
                        is_end = false;
                        break;
                    }
                }
                None => {
                    is_end = false;
                    break;
                }
            }
        }
        if is_end {
            constraints.push(DeclareConstraint {
                template: "End".to_string(),
                parameters: vec![vec![act_a.clone()]],
            });
        }
    }
    
    // Pairwise constraints
    for (pair, dep) in matrix {
        let act_from = &pair.0;
        let act_to = &pair.1;

        let td = &dep.temporal_dependency;
        let ed = &dep.existential_dependency;

        // Rule 2: RespondedExistence(a,b)
        if td.is_none() {
            if let Some(e_dep) = ed {
                if e_dep.dependency_type == ExisEnum::Implication && e_dep.direction == ExisDir::Forward {
                    constraints.push(DeclareConstraint {
                        template: "RespondedExistence".to_string(),
                        parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                    });
                }
            }
        }

        // Rule 3: Coexistence(a,b)
        if td.is_none() {
            if let Some(e_dep) = ed {
                if e_dep.dependency_type == ExisEnum::Equivalence && e_dep.direction == ExisDir::Both {
                    constraints.push(DeclareConstraint {
                        template: "Coexistence".to_string(),
                        parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                    });
                }
            }
        }
        
        // Rule 4: Response(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Eventual && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Implication && e_dep.direction == ExisDir::Forward {
                        constraints.push(DeclareConstraint {
                            template: "Response".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }

        // Rule 5: Precedence(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Eventual && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Implication && e_dep.direction == ExisDir::Backward {
                        constraints.push(DeclareConstraint {
                            template: "Precedence".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }

        // Rule 6: Succession(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Eventual && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Equivalence && e_dep.direction == ExisDir::Both {
                        constraints.push(DeclareConstraint {
                            template: "Succession".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }

        // Rule 7: ChainResponse(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Direct && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Implication && e_dep.direction == ExisDir::Forward {
                        constraints.push(DeclareConstraint {
                            template: "ChainResponse".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }

        // Rule 8: ChainSuccession(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Direct && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Equivalence && e_dep.direction == ExisDir::Both {
                        constraints.push(DeclareConstraint {
                            template: "ChainSuccession".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }
        
        // Rule 9: ChainPrecedence(a,b)
        if let Some(t_dep) = td {
            if t_dep.dependency_type == TempEnum::Direct && t_dep.direction == TempDir::Forward {
                if let Some(e_dep) = ed {
                    if e_dep.dependency_type == ExisEnum::Implication && e_dep.direction == ExisDir::Backward {
                        constraints.push(DeclareConstraint {
                            template: "ChainPrecedence".to_string(),
                            parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                        });
                    }
                }
            }
        }

        // Rule 10: resp_absence(a,b)
        // (a,b)=(-,NAND)
        if td.is_none() {
            if let Some(e_dep) = ed {
                if e_dep.dependency_type == ExisEnum::Nand {
                    constraints.push(DeclareConstraint {
                        template: "resp_absence".to_string(),
                        parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                    });
                }
            }
        }
        
        // Rule 11: NotCoexistence(a,b)
        if td.is_none() {
            if let Some(e_dep) = ed {
                if e_dep.dependency_type == ExisEnum::NegatedEquivalence {
                     // Assuming NegatedEquivalence implies a symmetric relation or direction is Both/irrelevant
                    constraints.push(DeclareConstraint {
                        template: "NotCoexistence".to_string(), 
                        parameters: vec![vec![act_from.clone()], vec![act_to.clone()]],
                    });
                }
            }
        }

        // Rule 12: NotResponse(b,a)
        // (a,b)=(>,-) means temporal for (a,b) is Backward, existential is None.
        // This means b happens before a. Constraint is NotResponse(b,a).
        if let Some(t_dep) = td {
            if t_dep.direction == TempDir::Backward && ed.is_none() {
                 // t_dep.dependency_type can be Eventual or Direct. Rule specifies '>' (eventual backward).
                constraints.push(DeclareConstraint {
                    template: "NotResponse".to_string(),
                    parameters: vec![vec![act_to.clone()], vec![act_from.clone()]], // Parameters (b, a)
                });
            }
        }
    }
    
    // Sort constraints for consistent output, useful for testing
    constraints.sort_by(|a, b| {
        format!("{}{:?}", a.template, a.parameters).cmp(&format!("{}{:?}", b.template, b.parameters))
    });


    DeclareModel {
        name: model_name.to_string(),
        tasks,
        constraints,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    use std::collections::HashMap;

    use crate::dependency_types::temporal::TemporalDependency;
    use crate::dependency_types::existential::ExistentialDependency;

    fn create_temporal(dep_type: TempEnum, dir: TempDir, from: &str, to: &str) -> Option<TemporalDependency> {
        Some(TemporalDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: dep_type,
            direction: dir,
        })
    }

    fn create_existential(dep_type: ExisEnum, dir: ExisDir, from: &str, to: &str) -> Option<ExistentialDependency> {
        Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: dep_type,
            direction: dir,
        })
    }
    
    #[test]
    fn test_empty_matrix_and_activities() {
        let matrix: InputMatrix = HashMap::new();
        let activities: HashSet<Activity> = HashSet::new();
        let model = matrix_to_declare_model(&matrix, &activities, "EmptyModel");

        assert_eq!(model.name, "EmptyModel");
        assert!(model.tasks.is_empty());
        assert!(model.constraints.is_empty());
    }

    #[test]
    fn test_single_activity() {
        let mut activities: HashSet<Activity> = HashSet::new();
        activities.insert("A".to_string());
        let matrix: InputMatrix = HashMap::new(); // No pairs for a single activity
        let model = matrix_to_declare_model(&matrix, &activities, "SingleActivityModel");

        assert_eq!(model.tasks.len(), 1);
        assert_eq!(model.tasks[0].name, "A");
        assert!(model.constraints.is_empty()); // Init/End require >1 activities
    }

    #[test]
    fn test_init_constraint() {
        let mut activities: HashSet<Activity> = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());

        let mut matrix: InputMatrix = HashMap::new();
        // A -> B : Eventual Forward, any existential
        matrix.insert(("A".to_string(), "B".to_string()), Dependency::new("A".to_string(), "B".to_string(), create_temporal(TempEnum::Eventual, TempDir::Forward, "A", "B"), None));
        // For Init(A), all (A,X) must be Eventual Forward.

        let model = matrix_to_declare_model(&matrix, &activities, "InitModel");
        
        assert_eq!(model.tasks.len(), 2);
        let init_constraints: Vec<_> = model.constraints.iter().filter(|c| c.template == "Init").collect();
        assert_eq!(init_constraints.len(), 1, "Expected 1 Init constraint");
        assert_eq!(init_constraints[0].parameters, vec![vec!["A".to_string()]]);
    }

    #[test]
    fn test_end_constraint() {
        let mut activities: HashSet<Activity> = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string()); // B should be End

        let mut matrix: InputMatrix = HashMap::new();
        // A -> B : Eventual Forward for (A,B) implies B is End
        matrix.insert(("A".to_string(), "B".to_string()), Dependency::new("A".to_string(), "B".to_string(), create_temporal(TempEnum::Eventual, TempDir::Forward, "A", "B"), None));
        
        let model = matrix_to_declare_model(&matrix, &activities, "EndModel");
        
        assert_eq!(model.tasks.len(), 2);
        let end_constraints: Vec<_> = model.constraints.iter().filter(|c| c.template == "End").collect();
        assert_eq!(end_constraints.len(), 1, "Expected 1 End constraint");
        assert_eq!(end_constraints[0].parameters, vec![vec!["B".to_string()]]);
    }
    
    #[test]
    fn test_response_constraint() {
        let mut activities: HashSet<Activity> = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity to prevent unintended Init/End
        let mut matrix: InputMatrix = HashMap::new();

        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Eventual, TempDir::Forward, "A", "B"),
                create_existential(ExisEnum::Implication, ExisDir::Forward, "A", "B")
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "ResponseModel");
        assert_eq!(model.constraints.len(), 1, "Expected only one Response constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        assert_eq!(model.constraints[0].template, "Response");
        assert_eq!(model.constraints[0].parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_not_response_constraint() {
        let mut activities: HashSet<Activity> = HashSet::new();
        activities.insert("X".to_string());
        activities.insert("Y".to_string());
        let mut matrix: InputMatrix = HashMap::new();

        // (X,Y) = (>, -) means Y before X, no existential. NotResponse(Y,X)
        matrix.insert(
            ("X".to_string(), "Y".to_string()),
            Dependency::new(
                "X".to_string(), "Y".to_string(),
                create_temporal(TempEnum::Eventual, TempDir::Backward, "X", "Y"), // Y -> X
                None
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "NotResponseModel");
        assert_eq!(model.constraints.len(), 1);
        assert_eq!(model.constraints[0].template, "NotResponse");
        assert_eq!(model.constraints[0].parameters, vec![vec!["Y".to_string()], vec!["X".to_string()]]);
    }
    
    #[test]
    fn test_responded_existence() {
        let mut activities = HashSet::new();
        activities.insert("R".to_string());
        activities.insert("E".to_string());
        let mut matrix = HashMap::new();
        matrix.insert(
            ("R".to_string(), "E".to_string()),
            Dependency::new(
                "R".to_string(), "E".to_string(),
                None,
                create_existential(ExisEnum::Implication, ExisDir::Forward, "R", "E")
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "RespExistModel");
        assert_eq!(model.constraints.len(), 1);
        assert_eq!(model.constraints[0].template, "RespondedExistence");
        assert_eq!(model.constraints[0].parameters, vec![vec!["R".to_string()], vec!["E".to_string()]]);
    }

    #[test]
    fn test_coexistence() {
        let mut activities = HashSet::new();
        activities.insert("C1".to_string());
        activities.insert("C2".to_string());
        let mut matrix = HashMap::new();
        matrix.insert(
            ("C1".to_string(), "C2".to_string()),
            Dependency::new(
                "C1".to_string(), "C2".to_string(),
                None,
                create_existential(ExisEnum::Equivalence, ExisDir::Both, "C1", "C2")
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "CoexistModel");
        // Coexistence is symmetric, so (C1,C2) implies (C2,C1) would also be Coexistence.
        // The current loop processes each entry once. If (C2,C1) is also in matrix with same, it would be duplicate.
        // For now, assume matrix has one entry for the pair or symmetric entries are handled by caller.
        // The test will pass if one constraint is generated.
        // If both (C1,C2) and (C2,C1) are in matrix with this dependency, two identical constraints will be generated.
        // This might be desired or might need deduplication later.
        // The sorting of constraints will make test deterministic if duplicates occur.
        assert!(!model.constraints.is_empty());
        let coex_constraint = model.constraints.iter().find(|c| c.template == "Coexistence");
        assert!(coex_constraint.is_some());
        assert_eq!(coex_constraint.unwrap().parameters, vec![vec!["C1".to_string()], vec!["C2".to_string()]]);
    }
    
    // TODO: Add more tests for Precedence, Succession, Chain*, NotCoexistence, etc.

    #[test]
    fn test_precedence_constraint() {
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Eventual, TempDir::Forward, "A", "B"), // Changed to Eventual for Precedence
                create_existential(ExisEnum::Implication, ExisDir::Backward, "A", "B") // B => A
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestPrecedenceModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 Precedence constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "Precedence");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_succession_constraint() {
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Eventual, TempDir::Forward, "A", "B"), // Changed to Eventual for Succession
                create_existential(ExisEnum::Equivalence, ExisDir::Both, "A", "B") // A <=> B
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestSuccessionModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 Succession constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "Succession");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_chain_response_constraint() {
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Direct, TempDir::Forward, "A", "B"), // Changed to Direct for ChainResponse
                create_existential(ExisEnum::Implication, ExisDir::Forward, "A", "B") // A => B
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestChainResponseModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 ChainResponse constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "ChainResponse");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_chain_succession_constraint() {
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Direct, TempDir::Forward, "A", "B"), // Changed to Direct for ChainSuccession
                create_existential(ExisEnum::Equivalence, ExisDir::Both, "A", "B") // A <=> B
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestChainSuccessionModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 ChainSuccession constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "ChainSuccession");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_chain_precedence_constraint() {
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                create_temporal(TempEnum::Direct, TempDir::Forward, "A", "B"), // Changed to Direct for ChainPrecedence
                create_existential(ExisEnum::Implication, ExisDir::Backward, "A", "B") // B => A
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestChainPrecedenceModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 ChainPrecedence constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "ChainPrecedence");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_responded_absence_constraint() { // For Rule 10: resp_absence(a,b)
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                None, // No temporal dependency
                create_existential(ExisEnum::Nand, ExisDir::Both, "A", "B") // A NAND B
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestRespondedAbsenceModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 resp_absence constraint, found {}. Constraints: {:?}", model.constraints.len(), model.constraints);
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "resp_absence"); // Corrected template name
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }

    #[test]
    fn test_not_coexistence_constraint() { // For Rule 11
        let mut activities = HashSet::new();
        activities.insert("A".to_string());
        activities.insert("B".to_string());
        activities.insert("C".to_string()); // Add a third activity
        let mut matrix = HashMap::new();
        matrix.insert(
            ("A".to_string(), "B".to_string()),
            Dependency::new(
                "A".to_string(), "B".to_string(),
                None, // No temporal dependency
                create_existential(ExisEnum::NegatedEquivalence, ExisDir::Both, "A", "B")
            )
        );
        let model = matrix_to_declare_model(&matrix, &activities, "TestNotCoexistenceModel");
        assert_eq!(model.constraints.len(), 1, "Expected 1 NotCoexistence constraint");
        let constraint = &model.constraints[0];
        assert_eq!(constraint.template, "NotCoexistence");
        assert_eq!(constraint.parameters, vec![vec!["A".to_string()], vec!["B".to_string()]]);
    }
}

