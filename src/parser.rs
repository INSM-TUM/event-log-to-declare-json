use chrono::{DateTime, Utc};
use std::collections::HashSet;

use process_mining::{
    event_log::{import_xes::XESParseError, AttributeValue},
    import_xes_file, import_xes_slice, XESImportOptions,
};

#[derive(Debug, Clone)]
struct Event {
    activity: String,
    date: DateTime<Utc>,
}

impl Event {
    fn new(activity: String, date: DateTime<Utc>) -> Event {
        Event { activity, date }
    }
}

// Helper function to extract relevant attributes
fn extract_event_attributes(
    attributes: &[process_mining::event_log::Attribute],
) -> (Option<String>, Option<DateTime<Utc>>) {
    let mut name = None;
    let mut date = None;

    for attribute in attributes {
        match attribute.key.as_str() {
            "concept:name" => {
                if let AttributeValue::String(value) = &attribute.value {
                    name = Some(value.clone());
                }
            }
            "time:timestamp" => {
                if let AttributeValue::Date(value) = &attribute.value {
                    date = Some(value.with_timezone(&Utc));
                }
            }
            _ => continue,
        }
    }

    (name, date)
}

pub fn _get_activities(path: &str) -> Option<HashSet<String>> {
    let event_log = import_xes_file(path, XESImportOptions::default()).ok()?;
    let traces = event_log.traces;
    let mut activities = HashSet::new();

    for trace in traces {
        // Check if there is a lifecycle:transition with value "complete" in the trace
        let has_complete = trace.events.iter().any(|event| {
            event.attributes.iter().any(|a| {
                a.key == "lifecycle:transition"
                    && a.value == AttributeValue::String("complete".to_string())
            })
        });

        for event in trace.events {
            let mut name = None;

            // If "complete" is present, we only consider events with "complete" transition
            if !has_complete
                || event.attributes.iter().any(|a| {
                    a.key == "lifecycle:transition"
                        && a.value == AttributeValue::String("complete".to_string())
                })
            {
                // Extract the "concept:name" (activity name) if it exists
                for attribute in event.attributes {
                    if attribute.key == "concept:name" {
                        if let AttributeValue::String(value) = &attribute.value {
                            name = Some(value.clone());
                        }
                    }
                }

                if let Some(name) = name {
                    activities.insert(name);
                }
            }
        }
    }

    Some(activities)
}

pub fn parse_into_traces(
    path: Option<&str>,
    content: Option<&str>,
) -> Result<Vec<Vec<String>>, XESParseError> {
    let traces = match (path, content) {
        (Some(path), _) => {
            let event_log = import_xes_file(path, XESImportOptions::default())?;
            event_log.traces
        }
        (None, Some(content)) => {
            let event_log =
                import_xes_slice(content.as_bytes(), false, XESImportOptions::default())?;
            event_log.traces
        }
        _ => panic!("Either path or content must be provided, not both"),
    };

    let mut result = Vec::new();

    for trace in traces {
        let mut events: Vec<Event> = Vec::new();

        // First check if there is a lifecycle:transition with value complete anywhere in the trace
        let has_complete = trace.events.iter().any(|event| {
            event.attributes.iter().any(|a| {
                a.key == "lifecycle:transition"
                    && a.value == AttributeValue::String("complete".to_string())
            })
        });

        for event in trace.events {
            let (name, date) = extract_event_attributes(&event.attributes);

            if !has_complete
                || event.attributes.iter().any(|a| {
                    a.key == "lifecycle:transition"
                        && a.value == AttributeValue::String("complete".to_string())
                })
            {
                if let (Some(name), Some(date)) = (name, date) {
                    events.push(Event::new(name, date));
                }
            }
        }

        events.sort_by(|a, b| a.date.cmp(&b.date)); // sort events by date

        let activity_list: Vec<String> = events.into_iter().map(|event| event.activity).collect();
        result.push(activity_list);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    fn variants_of_traces(traces: Vec<Vec<&str>>) -> HashMap<Vec<&str>, usize> {
        traces.into_iter().fold(HashMap::new(), |mut acc, trace| {
            *acc.entry(trace).or_insert(0) += 1;
            acc
        })
    }

    #[test]
    fn test_variants_of_traces() {
        let traces = vec![
            vec!["A", "B", "C"],
            vec!["A", "B", "C"],
            vec!["B", "C", "D"],
            vec!["A", "B", "C"],
            vec!["B", "C", "D"],
            vec!["E", "F", "G"],
        ];

        let result = variants_of_traces(traces);
        assert_eq!(result.len(), 3);
        assert_eq!(result[&vec!["A", "B", "C"]], 3);
        assert_eq!(result[&vec!["B", "C", "D"]], 2);
        assert_eq!(result[&vec!["E", "F", "G"]], 1);
    }
}