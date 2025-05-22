mod dependency_types;
mod matrix_generation;
mod parser;
mod declare_translation;

use matrix_generation::generate_dependency_matrix;
use parser::parse_into_traces;
use crate::declare_translation::{matrix_to_declare_model, DeclareModel};

use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::spawn_local;
use web_sys::{File, HtmlInputElement, MouseEvent, ProgressEvent, FileReader, Event, InputEvent};
use yew::prelude::*;
use clap::Parser;
use gloo_console; // Added for logging

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    file_path: Option<String>,
    
    #[clap(long, default_value_t = 1.0)]
    temporal_threshold: f64,

    #[clap(long, default_value_t = 1.0)]
    existential_threshold: f64,
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
enum AppError {
    #[error("XES parsing error: {0}")]
    XesParseError(String),
    #[error("Classification error: {0}")]
    ClassificationError(String),
}

enum AppMessage {
    FileSelected(Option<File>),
    FileLoaded(Result<String, String>),
    ExistentialThresholdChanged(String),
    TemporalThresholdChanged(String),
    ProcessLog,
    ProcessingComplete(Result<DeclareModel, AppError>), // New message
}

#[derive(Clone, PartialEq)]
struct AppState {
    file_name: Option<String>,
    file_content: Option<String>,
    // xes_file_content_for_display: Option<String>, // Removed
    existential_threshold_str: String, // Store as String
    temporal_threshold_str: String,    // Store as String
    is_processing: bool,
    error_message: Option<String>, // To display errors
    declare_model_json: Option<String>, // To store the resulting JSON
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            file_name: None,
            file_content: None,
            // xes_file_content_for_display: None, // Removed
            existential_threshold_str: "1.0".to_string(), // Default to "1.0" string
            temporal_threshold_str: "1.0".to_string(),    // Default to "1.0" string
            is_processing: false,
            error_message: None,
            declare_model_json: None,
        }
    }
}

fn parse_threshold_str(s: &str) -> Option<f64> {
    s.parse::<f64>().ok().filter(|&val| (0.0..=1.0).contains(&val))
}

#[function_component(App)]
fn app() -> Html {
    let app_state_handle: UseStateHandle<AppState> = use_state(AppState::default);

    let dispatch = {
        let app_state_handle = app_state_handle.clone();
        Rc::new(move |msg: AppMessage| {
            let mut new_state = (*app_state_handle).clone();
            match msg {
                AppMessage::FileSelected(file_opt) => {
                    if let Some(file) = file_opt {
                        new_state.file_name = Some(file.name());
                        new_state.file_content = None;
                        // new_state.xes_file_content_for_display = None; // Removed
                        new_state.declare_model_json = None; // Clear previous results
                        new_state.error_message = None;
                    } else {
                        new_state.file_name = None;
                        new_state.file_content = None;
                        // new_state.xes_file_content_for_display = None; // Removed
                    }
                }
                AppMessage::FileLoaded(result) => {
                    match result {
                        Ok(content) => {
                            new_state.file_content = Some(content.clone());
                            // new_state.xes_file_content_for_display = Some(content); // Removed
                        }
                        Err(err_msg) => {
                            new_state.file_content = None;
                            // new_state.xes_file_content_for_display = None; // Removed
                            new_state.error_message = Some(format!("File loading error: {}", err_msg));
                        }
                    }
                }
                AppMessage::ExistentialThresholdChanged(val_str) => {
                    new_state.existential_threshold_str = val_str; 
                    new_state.declare_model_json = None; // Clear previous results
                    new_state.error_message = None;
                }
                AppMessage::TemporalThresholdChanged(val_str) => {
                    new_state.temporal_threshold_str = val_str; 
                    new_state.declare_model_json = None; // Clear previous results
                    new_state.error_message = None;
                }
                AppMessage::ProcessLog => {
                    new_state.is_processing = true;
                    new_state.error_message = None;
                    new_state.declare_model_json = None; // Clear previous results
                }
                AppMessage::ProcessingComplete(result) => {
                    new_state.is_processing = false;
                    match result {
                        Ok(model) => {
                            match serde_json::to_string_pretty(&model) {
                                Ok(json_string) => {
                                    gloo_console::log!("Processing complete, JSON generated:", json_string.clone());
                                    new_state.declare_model_json = Some(json_string);
                                    new_state.error_message = None;
                                }
                                Err(e) => {
                                    gloo_console::error!("Error serializing Declare model to JSON:", e.to_string());
                                    new_state.error_message = Some(format!("Error serializing model: {}", e));
                                    new_state.declare_model_json = None;
                                }
                            }
                        }
                        Err(e) => {
                            gloo_console::error!("Processing error:", e.to_string());
                            new_state.error_message = Some(e.to_string());
                            new_state.declare_model_json = None;
                        }
                    }
                }
            }
            app_state_handle.set(new_state);
        })
    };

    let on_file_change = {
        let dispatch = dispatch.clone();
        Callback::from(move |e: Event| {
            let input: HtmlInputElement = e.target_unchecked_into();
            if let Some(files) = input.files() {
                if let Some(file) = files.get(0) {
                    dispatch(AppMessage::FileSelected(Some(file.clone())));
                    
                    let reader = FileReader::new().expect("Failed to create FileReader");
                    let dispatch_clone_for_load = dispatch.clone();
                    let onload = Closure::wrap(Box::new(move |event: ProgressEvent| {
                        let reader_res: FileReader = event.target_unchecked_into();
                        match reader_res.result() {
                            Ok(content_val) => {
                                if let Some(content_str) = content_val.as_string() {
                                    dispatch_clone_for_load(AppMessage::FileLoaded(Ok(content_str)));
                                } else {
                                    dispatch_clone_for_load(AppMessage::FileLoaded(Err("Content is not a string".to_string())));
                                }
                            }
                            Err(err) => {
                                dispatch_clone_for_load(AppMessage::FileLoaded(Err(format!("{:?}", err))));
                            }
                        }
                    }) as Box<dyn FnMut(ProgressEvent)>);
                    reader.set_onload(Some(onload.as_ref().unchecked_ref()));
                    if reader.read_as_text(&file).is_err() {
                         dispatch(AppMessage::FileLoaded(Err("Failed to initiate file read".to_string())));
                    }
                    onload.forget();
                } else {
                     dispatch(AppMessage::FileSelected(None));
                }
            }
        })
    };
    
    let on_existential_threshold_change = {
        let dispatch = dispatch.clone();
        Callback::from(move |e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            dispatch(AppMessage::ExistentialThresholdChanged(input.value()));
        })
    };

    let on_temporal_threshold_change = {
        let dispatch = dispatch.clone();
        Callback::from(move |e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            dispatch(AppMessage::TemporalThresholdChanged(input.value()));
        })
    };

    let on_process_log = {
        let app_state_snapshot = (*app_state_handle).clone();
        let dispatch_outer = dispatch.clone(); // Clone for the outer scope
        Callback::from(move |_mouse_event: MouseEvent| {
            let temp_thresh_opt = parse_threshold_str(&app_state_snapshot.temporal_threshold_str);
            let ex_thresh_opt = parse_threshold_str(&app_state_snapshot.existential_threshold_str);
            let file_content_clone = app_state_snapshot.file_content.clone(); // Clone for the async block
            let file_name_clone = app_state_snapshot.file_name.clone().unwrap_or_else(|| "UnnamedLog".to_string());


            if file_content_clone.is_some() && 
               !app_state_snapshot.is_processing &&
               temp_thresh_opt.is_some() &&
               ex_thresh_opt.is_some()
            {
                dispatch_outer(AppMessage::ProcessLog); // Dispatch ProcessLog immediately
                
                let temp_thresh_val = temp_thresh_opt.unwrap();
                let ex_thresh_val = ex_thresh_opt.unwrap();
                let dispatch_for_async = dispatch_outer.clone(); // Clone for the async block

                spawn_local(async move {
                    let processing_result: Result<DeclareModel, AppError> = (|| {
                        let traces = parse_into_traces(None, file_content_clone.as_deref())
                            .map_err(|e| AppError::XesParseError(e.to_string()))?;

                        if traces.is_empty() {
                            return Err(AppError::ClassificationError("No traces found in the log.".to_string()));
                        }
                        
                        let all_activities: std::collections::HashSet<String> = traces
                            .iter()
                            .flat_map(|trace| trace.iter().cloned())
                            .collect();

                        if all_activities.is_empty() {
                             return Err(AppError::ClassificationError("No activities found in the log after parsing traces.".to_string()));
                        }

                        let matrix = generate_dependency_matrix(&traces, ex_thresh_val, temp_thresh_val);
                        let model_name = format!("{}_DeclareModel", file_name_clone.replace(".xes", ""));
                        Ok(matrix_to_declare_model(&matrix, &all_activities, &model_name))
                    })(); // Immediately invoke the closure
                    
                    dispatch_for_async(AppMessage::ProcessingComplete(processing_result));
                });
            }
        })
    };

    let current_app_state_for_view = (*app_state_handle).clone();

    // Determine button disabled state for the view
    let is_temporal_thresh_valid = parse_threshold_str(&current_app_state_for_view.temporal_threshold_str).is_some();
    let is_existential_thresh_valid = parse_threshold_str(&current_app_state_for_view.existential_threshold_str).is_some();
    let is_process_button_disabled = current_app_state_for_view.file_content.is_none() || 
                                     current_app_state_for_view.is_processing ||
                                     !is_temporal_thresh_valid ||
                                     !is_existential_thresh_valid;

    // Logging outside the html! macro for clarity during rendering phase
    if current_app_state_for_view.declare_model_json.is_some() {
        gloo_console::log!("Rendering app component. declare_model_json IS SOME.");
    } else {
        gloo_console::log!("Rendering app component. declare_model_json IS NONE.");
    }

    html! {
        <div class="container" style="padding: 20px; font-family: sans-serif;">
            <h1>{ "Declare JSON Generator" }</h1>

            <div class="controls" style="margin-bottom: 20px; display: flex; gap: 20px; align-items: center;">
                <div>
                    <label for="xes-file" style="margin-right: 5px;">{ "Upload XES File:" }</label>
                    <input type="file" id="xes-file" accept=".xes" onchange={on_file_change} />
                    if let Some(name) = current_app_state_for_view.file_name {
                        <p style="font-size: 0.9em; margin-top: 5px;">{ format!("Selected: {}", name) }</p>
                    }
                </div>
            </div>

            {
                // Removed XES content display block
                // if let Some(xes_content) = &current_app_state_for_view.xes_file_content_for_display {
                //     html! {
                //         <div style="margin-bottom: 20px;">
                //             <h3>{ "XES File Content:" }</h3>
                //             <textarea readonly=true rows="15" style="width: 100%; font-family: monospace; white-space: pre-wrap; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;">
                //                 { xes_content.clone() }
                //             </textarea>
                //         </div>
                //     }
                // } else {
                //     html! {}
                // }
                html! {} // Replaced with an empty html fragment
            }

            <div class="thresholds" style="margin-bottom: 20px; display: flex; gap: 30px;">
                <div>
                    <label for="temporal-threshold" style="margin-right: 5px;">{ "Temporal Threshold (0.0-1.0):" }</label>
                    <input
                        id="temporal-threshold"
                        type="number" 
                        min="0.0" max="1.0" step="0.05"
                        value={current_app_state_for_view.temporal_threshold_str.clone()} // Bind to string state
                        oninput={on_temporal_threshold_change}
                        style={if !is_temporal_thresh_valid && !current_app_state_for_view.temporal_threshold_str.is_empty() {"width: 70px; border-color: red;"} else {"width: 70px;"} }
                    />
                </div>
                <div>
                    <label for="existential-threshold" style="margin-right: 5px;">{ "Existential Threshold (0.0-1.0):" }</label>
                    <input
                        id="existential-threshold"
                        type="number"
                        min="0.0" max="1.0" step="0.05"
                        value={current_app_state_for_view.existential_threshold_str.clone()} // Bind to string state
                        oninput={on_existential_threshold_change}
                        style={if !is_existential_thresh_valid && !current_app_state_for_view.existential_threshold_str.is_empty() {"width: 70px; border-color: red;"} else {"width: 70px;"} }
                    />
                </div>
            </div>

            <button
                onclick={on_process_log}
                disabled={is_process_button_disabled} // Use the new combined condition
                style="padding: 10px 15px; font-size: 1em; cursor: pointer;"
            >
                { if current_app_state_for_view.is_processing { "Processing..." } else { "Process Log" } }
            </button>

            {
                if let Some(err_msg) = &current_app_state_for_view.error_message {
                    html! { <p style="color: red; margin-top: 15px;">{ format!("Error: {}", err_msg) }</p> }
                } else {
                    html! {}
                }
            }

            {
                if let Some(json_output_content) = &current_app_state_for_view.declare_model_json {
                    html! {
                        <div style="margin-top: 20px;">
                            <h2>{ "Generated Declare JSON:" }</h2>
                            <textarea readonly=true rows="20" style="width: 100%; font-family: monospace; white-space: pre; border: 1px solid #ccc; padding: 10px;"
                                value={json_output_content.clone()} >
                                // Content is now set via the value attribute
                            </textarea>
                        </div>
                    }
                } else {
                    html! { <></> } // Render an empty fragment
                }
            }

        </div>
    }
}

fn main() {
    let args = Args::parse();

    if args.file_path.is_some() {
        let file_path = args.file_path.unwrap();
        let temporal_threshold = args.temporal_threshold;
        let existential_threshold = args.existential_threshold;

        if !(0.0..=1.0).contains(&temporal_threshold) {
            eprintln!("Error: Temporal threshold must be between 0.0 and 1.0");
            std::process::exit(1);
        }

        if !(0.0..=1.0).contains(&existential_threshold) {
            eprintln!("Error: Existential threshold must be between 0.0 and 1.0");
            std::process::exit(1);
        }

        match parse_into_traces(Some(&file_path), None) {
            Ok(traces) => {
                let matrix = generate_dependency_matrix(&traces, existential_threshold, temporal_threshold);
                let all_activities: std::collections::HashSet<String> = traces
                    .iter()
                    .flat_map(|trace| trace.iter().cloned())
                    .collect();
                if all_activities.is_empty() {
                    eprintln!("Error: No activities found in the log.");
                    std::process::exit(1);
                }
                let model_name = format!("{}_DeclareModel", file_path.replace(".xes", ""));
                let declare_model = matrix_to_declare_model(&matrix, &all_activities, &model_name);
                match serde_json::to_string_pretty(&declare_model) {
                    Ok(json_string) => println!("{}", json_string),
                    Err(e) => {
                        eprintln!("Error serializing Declare model to JSON: {}", e);
                        std::process::exit(1);
                    }
                }
            }
            Err(e) => {
                eprintln!("Error parsing XES file: {}", e);
                std::process::exit(1);
            }
        }
    } else {
        // Initialize Yew renderer without explicit Tokio runtime
        yew::Renderer::<App>::new().render();
    }
}