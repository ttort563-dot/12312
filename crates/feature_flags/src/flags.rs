use crate::FeatureFlag;

pub struct NotebookFeatureFlag;
impl FeatureFlag for NotebookFeatureFlag {
    const NAME: &'static str = "notebooks";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct PanicFeatureFlag;
impl FeatureFlag for PanicFeatureFlag {
    const NAME: &'static str = "panic";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct AgentV2FeatureFlag;
impl FeatureFlag for AgentV2FeatureFlag {
    const NAME: &'static str = "agent-v2";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct AcpBetaFeatureFlag;
impl FeatureFlag for AcpBetaFeatureFlag {
    const NAME: &'static str = "acp-beta";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct ToolPermissionsFeatureFlag;
impl FeatureFlag for ToolPermissionsFeatureFlag {
    const NAME: &'static str = "tool-permissions";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct AgentSharingFeatureFlag;
impl FeatureFlag for AgentSharingFeatureFlag {
    const NAME: &'static str = "agent-sharing";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct SubagentsFeatureFlag;
impl FeatureFlag for SubagentsFeatureFlag {
    const NAME: &'static str = "subagents";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct DiffReviewFeatureFlag;
impl FeatureFlag for DiffReviewFeatureFlag {
    const NAME: &'static str = "diff-review";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}

pub struct OpenAiResponsesApiFeatureFlag;
impl FeatureFlag for OpenAiResponsesApiFeatureFlag {
    const NAME: &'static str = "open-ai-responses-api";
    fn enabled_for_staff() -> bool { true }
    fn enabled_for_all() -> bool { true }
}
