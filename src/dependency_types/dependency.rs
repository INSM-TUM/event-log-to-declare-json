use crate::dependency_types::existential::ExistentialDependency;
use crate::dependency_types::temporal::TemporalDependency;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dependency {
    pub from: String,
    pub to: String,
    pub temporal_dependency: Option<TemporalDependency>,
    pub existential_dependency: Option<ExistentialDependency>,
}

impl Dependency {
    pub fn new(
        from: String,
        to: String,
        temporal_dependency: Option<TemporalDependency>,
        existential_dependency: Option<ExistentialDependency>,
    ) -> Self {
        Self {
            from,
            to,
            temporal_dependency,
            existential_dependency,
        }
    }
}

impl std::fmt::Display for Dependency {
    /// Formats the object using the given formatter.
    ///
    /// This method checks for the presence of `temporal_dependency` and `existential_dependency`
    /// and formats the output accordingly:
    /// - If both dependencies are present, it writes them separated by a comma.
    /// - If only `temporal_dependency` is present, it writes it followed by a comma and a dash.
    /// - If only `existential_dependency` is present, it writes a dash followed by the dependency.
    /// - If neither dependency is present, it writes "None".
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temporal_dep = self.temporal_dependency.as_ref().map(|dep| dep.to_string());
        let existential_dep = self
            .existential_dependency
            .as_ref()
            .map(|dep| dep.to_string());

        match (temporal_dep, existential_dep) {
            (Some(t), Some(e)) => write!(f, "{},{}", t, e),
            (Some(t), None) => write!(f, "{},-", t),
            (None, Some(e)) => write!(f, "-,{}", e),
            (None, None) => write!(f, "None"),
        }
    }
}