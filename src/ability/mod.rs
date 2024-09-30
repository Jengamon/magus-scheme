//! magicflute ability descriptions

#[derive(Debug)]
pub enum Ability {
    Static(StaticAbility),
    Triggered(TriggeredAbility),
    Activated(ActivatedAbility),
    Spell(SpellAbility),
}

#[derive(Debug)]
pub struct StaticAbility;
#[derive(Debug)]
pub struct TriggeredAbility;
#[derive(Debug)]
pub struct ActivatedAbility;
#[derive(Debug)]
pub struct SpellAbility;
