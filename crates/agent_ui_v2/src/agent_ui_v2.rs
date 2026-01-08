mod agent_thread_pane;
mod thread_history;

pub mod agents_panel;

use gpui::actions;

actions!(
    agents,
    [
        /// Removes all thread history.
        RemoveHistory,
        /// Removes the currently selected thread.
        RemoveSelectedThread,
    ]
);
