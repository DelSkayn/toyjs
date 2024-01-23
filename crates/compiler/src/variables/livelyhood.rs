/// Struct for running livelyhood analysis on symbols.
pub struct Livelyhood<'a, 'b> {
    ast: &'a Ast,
    variables: &'b mut Variables,
}
